#include "frontend_impl.h"

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#ifdef _WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

typedef enum
{
  C_TY_VOID = 0,
  C_TY_I1,
  C_TY_I8,
  C_TY_U8,
  C_TY_I16,
  C_TY_U16,
  C_TY_I32,
  C_TY_U32,
  C_TY_I64,
  C_TY_U64,
  C_TY_F32,
  C_TY_F64,
  C_TY_PTR,
} CType;

typedef enum
{
  C_BODY_RET_VOID = 0,
  C_BODY_RET_CONST,
  C_BODY_RET_PARAM,
  C_BODY_COMPLEX,
} CBodyKind;

typedef struct
{
  char *name;
  CType type;
} CParam;

typedef struct
{
  char *name;
  CType ret_type;
  CParam *params;
  int param_count;
  int param_cap;
  int is_varargs;
  int is_static;
  int is_extern_decl;
  int is_noreturn;
  int has_body;
  CBodyKind body_kind;
  long long ret_const;
  int ret_param_index;
  char *body_text;
} CFunction;

typedef struct
{
  int parse_ok;
  ChanceCDialect c_dialect;
  CFunction *functions;
  int function_count;
  int function_cap;
  Symbol *externs;
  int extern_count;
  int extern_cap;
  char **visited_headers;
  int visited_header_count;
  int visited_header_cap;
  char **macro_names;
  long long *macro_values;
  int macro_count;
  int macro_cap;
} CFrontendUnit;

static Type g_ty_void = {.kind = TY_VOID};
static Type g_ty_i8 = {.kind = TY_I8};
static Type g_ty_u8 = {.kind = TY_U8};
static Type g_ty_i16 = {.kind = TY_I16};
static Type g_ty_u16 = {.kind = TY_U16};
static Type g_ty_i32 = {.kind = TY_I32};
static Type g_ty_u32 = {.kind = TY_U32};
static Type g_ty_i64 = {.kind = TY_I64};
static Type g_ty_u64 = {.kind = TY_U64};
static Type g_ty_f32 = {.kind = TY_F32};
static Type g_ty_f64 = {.kind = TY_F64};
static Type g_ty_bool = {.kind = TY_BOOL};
static Type g_ty_ptr = {.kind = TY_PTR, .pointee = &g_ty_u8};

static int ends_with_icase(const char *s, const char *suf)
{
  if (!s || !suf)
    return 0;
  size_t ls = strlen(s);
  size_t lu = strlen(suf);
  if (lu > ls)
    return 0;
  const char *p = s + (ls - lu);
  for (size_t i = 0; i < lu; ++i)
  {
    unsigned char a = (unsigned char)p[i];
    unsigned char b = (unsigned char)suf[i];
    if (tolower(a) != tolower(b))
      return 0;
  }
  return 1;
}

static int equals_icase_local(const char *a, const char *b)
{
  if (!a || !b)
    return 0;
  while (*a && *b)
  {
    if (tolower((unsigned char)*a) != tolower((unsigned char)*b))
      return 0;
    ++a;
    ++b;
  }
  return *a == '\0' && *b == '\0';
}

static int c_supports_input_path(const char *input_path)
{
  return input_path &&
         (ends_with_icase(input_path, ".c") || ends_with_icase(input_path, ".h"));
}

static int c_is_header_path(const char *path)
{
  if (!path)
    return 0;
  return ends_with_icase(path, ".h") ||
         ends_with_icase(path, ".hh") ||
         ends_with_icase(path, ".hpp") ||
         ends_with_icase(path, ".hxx") ||
         ends_with_icase(path, ".inc");
}

static char *xstrndup0(const char *s, size_t n)
{
  char *out = (char *)malloc(n + 1);
  if (!out)
    return NULL;
  memcpy(out, s, n);
  out[n] = '\0';
  return out;
}

static char *xstrdup0(const char *s)
{
  if (!s)
    return NULL;
  return xstrndup0(s, strlen(s));
}

static char *trim_dup(const char *s, size_t n)
{
  const char *b = s;
  const char *e = s + n;
  while (b < e && isspace((unsigned char)*b))
    ++b;
  while (e > b && isspace((unsigned char)e[-1]))
    --e;
  return xstrndup0(b, (size_t)(e - b));
}

static int is_ident_start_char(char c)
{
  return (c == '_') || isalpha((unsigned char)c);
}

static int is_ident_char(char c)
{
  return (c == '_') || isalnum((unsigned char)c);
}

static int has_word_icase(const char *text, const char *word)
{
  if (!text || !word || !*word)
    return 0;
  size_t wl = strlen(word);
  for (const char *p = text; *p; ++p)
  {
    if (tolower((unsigned char)*p) != tolower((unsigned char)word[0]))
      continue;
    if (p != text && is_ident_char(p[-1]))
      continue;
    size_t i = 0;
    while (i < wl && p[i] && tolower((unsigned char)p[i]) == tolower((unsigned char)word[i]))
      ++i;
    if (i == wl && !is_ident_char(p[wl]))
      return 1;
  }
  return 0;
}

static const char *ctype_to_ccb(CType t)
{
  switch (t)
  {
  case C_TY_VOID:
    return "void";
  case C_TY_I1:
    return "i1";
  case C_TY_I8:
    return "i8";
  case C_TY_U8:
    return "u8";
  case C_TY_I16:
    return "i16";
  case C_TY_U16:
    return "u16";
  case C_TY_I32:
    return "i32";
  case C_TY_U32:
    return "u32";
  case C_TY_I64:
    return "i64";
  case C_TY_U64:
    return "u64";
  case C_TY_F32:
    return "f32";
  case C_TY_F64:
    return "f64";
  case C_TY_PTR:
  default:
    return "ptr";
  }
}

static Type *ctype_to_ast(CType t)
{
  switch (t)
  {
  case C_TY_VOID:
    return &g_ty_void;
  case C_TY_I1:
    return &g_ty_bool;
  case C_TY_I8:
    return &g_ty_i8;
  case C_TY_U8:
    return &g_ty_u8;
  case C_TY_I16:
    return &g_ty_i16;
  case C_TY_U16:
    return &g_ty_u16;
  case C_TY_I32:
    return &g_ty_i32;
  case C_TY_U32:
    return &g_ty_u32;
  case C_TY_I64:
    return &g_ty_i64;
  case C_TY_U64:
    return &g_ty_u64;
  case C_TY_F32:
    return &g_ty_f32;
  case C_TY_F64:
    return &g_ty_f64;
  case C_TY_PTR:
  default:
    return &g_ty_ptr;
  }
}

static CType ast_to_ctype(const Type *t)
{
  if (!t)
    return C_TY_I32;
  switch (t->kind)
  {
  case TY_VOID:
    return C_TY_VOID;
  case TY_BOOL:
    return C_TY_I1;
  case TY_I8:
    return C_TY_I8;
  case TY_U8:
    return C_TY_U8;
  case TY_I16:
    return C_TY_I16;
  case TY_U16:
    return C_TY_U16;
  case TY_I32:
    return C_TY_I32;
  case TY_U32:
    return C_TY_U32;
  case TY_I64:
    return C_TY_I64;
  case TY_U64:
    return C_TY_U64;
  case TY_F32:
    return C_TY_F32;
  case TY_F64:
    return C_TY_F64;
  case TY_PTR:
  default:
    return C_TY_PTR;
  }
}

static int function_has_definition(const CFrontendUnit *impl, const char *name)
{
  if (!impl || !name)
    return 0;
  for (int i = 0; i < impl->function_count; ++i)
  {
    const CFunction *fn = &impl->functions[i];
    if (fn->name && strcmp(fn->name, name) == 0 && fn->has_body)
      return 1;
  }
  return 0;
}

static int extern_exists(const CFrontendUnit *impl, const char *name)
{
  if (!impl || !name)
    return 0;
  for (int i = 0; i < impl->extern_count; ++i)
  {
    if (impl->externs[i].name && strcmp(impl->externs[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static void cparam_destroy(CParam *p)
{
  if (!p)
    return;
  free(p->name);
  p->name = NULL;
}

static void cfunction_destroy(CFunction *fn)
{
  if (!fn)
    return;
  free(fn->name);
  fn->name = NULL;
  free(fn->body_text);
  fn->body_text = NULL;
  if (fn->params)
  {
    for (int i = 0; i < fn->param_count; ++i)
      cparam_destroy(&fn->params[i]);
    free(fn->params);
  }
  memset(fn, 0, sizeof(*fn));
}

static int function_append(CFrontendUnit *impl, const CFunction *fn)
{
  if (!impl || !fn)
    return 0;
  if (impl->function_count == impl->function_cap)
  {
    int next = impl->function_cap ? impl->function_cap * 2 : 16;
    CFunction *grown = (CFunction *)realloc(impl->functions, sizeof(CFunction) * (size_t)next);
    if (!grown)
      return 0;
    impl->functions = grown;
    impl->function_cap = next;
  }
  impl->functions[impl->function_count++] = *fn;
  return 1;
}

static int function_param_append(CFunction *fn, const char *name, CType type)
{
  if (!fn)
    return 0;
  if (fn->param_count == fn->param_cap)
  {
    int next = fn->param_cap ? fn->param_cap * 2 : 8;
    CParam *grown = (CParam *)realloc(fn->params, sizeof(CParam) * (size_t)next);
    if (!grown)
      return 0;
    fn->params = grown;
    fn->param_cap = next;
  }
  CParam *p = &fn->params[fn->param_count++];
  memset(p, 0, sizeof(*p));
  p->type = type;
  if (name && *name)
  {
    p->name = xstrdup0(name);
    if (!p->name)
      return 0;
  }
  return 1;
}

static int extern_append_from_function(CFrontendUnit *impl, const CFunction *fn)
{
  if (!impl || !fn || !fn->name)
    return 0;
  if (extern_exists(impl, fn->name) || function_has_definition(impl, fn->name))
    return 1;

  if (impl->extern_count == impl->extern_cap)
  {
    int next = impl->extern_cap ? impl->extern_cap * 2 : 32;
    Symbol *grown = (Symbol *)realloc(impl->externs, sizeof(Symbol) * (size_t)next);
    if (!grown)
      return 0;
    impl->externs = grown;
    impl->extern_cap = next;
  }

  Symbol *sym = &impl->externs[impl->extern_count++];
  memset(sym, 0, sizeof(*sym));
  sym->kind = SYM_FUNC;
  sym->name = xstrdup0(fn->name);
  sym->backend_name = sym->name;
  sym->is_extern = 1;
  sym->abi = "C";
  sym->sig.ret = ctype_to_ast(fn->ret_type);
  sym->sig.param_count = fn->param_count;
  sym->sig.is_varargs = fn->is_varargs;
  sym->is_noreturn = fn->is_noreturn;

  if (fn->param_count > 0)
  {
    sym->sig.params = (Type **)calloc((size_t)fn->param_count, sizeof(Type *));
    if (!sym->sig.params)
      return 0;
    for (int i = 0; i < fn->param_count; ++i)
      sym->sig.params[i] = ctype_to_ast(fn->params[i].type);
  }

  return 1;
}

static void destroy_externs(CFrontendUnit *impl)
{
  if (!impl)
    return;
  for (int i = 0; i < impl->extern_count; ++i)
  {
    Symbol *s = &impl->externs[i];
    free((void *)s->name);
    s->name = NULL;
    s->backend_name = NULL;
    free(s->sig.params);
    s->sig.params = NULL;
    s->sig.param_count = 0;
  }
  free(impl->externs);
  impl->externs = NULL;
  impl->extern_count = 0;
  impl->extern_cap = 0;
}

static int macro_find_index(const CFrontendUnit *impl, const char *name)
{
  if (!impl || !name)
    return -1;
  for (int i = 0; i < impl->macro_count; ++i)
  {
    if (impl->macro_names[i] && strcmp(impl->macro_names[i], name) == 0)
      return i;
  }
  return -1;
}

static int macro_lookup_value(const CFrontendUnit *impl, const char *name, long long *out)
{
  if (!out)
    return 0;
  int idx = macro_find_index(impl, name);
  if (idx < 0)
    return 0;
  *out = impl->macro_values[idx];
  return 1;
}

static int macro_set_value(CFrontendUnit *impl, const char *name, long long value)
{
  if (!impl || !name || !*name)
    return 0;

  int idx = macro_find_index(impl, name);
  if (idx >= 0)
  {
    impl->macro_values[idx] = value;
    return 1;
  }

  if (impl->macro_count == impl->macro_cap)
  {
    int next = impl->macro_cap ? impl->macro_cap * 2 : 32;
    char **grown_names = (char **)realloc(impl->macro_names, sizeof(char *) * (size_t)next);
    if (!grown_names)
      return 0;
    long long *grown_values = (long long *)realloc(impl->macro_values, sizeof(long long) * (size_t)next);
    if (!grown_values)
      return 0;
    impl->macro_names = grown_names;
    impl->macro_values = grown_values;
    impl->macro_cap = next;
  }

  impl->macro_names[impl->macro_count] = xstrdup0(name);
  if (!impl->macro_names[impl->macro_count])
    return 0;
  impl->macro_values[impl->macro_count] = value;
  ++impl->macro_count;
  return 1;
}

static int parse_balanced(const char *s, int start, char open_ch, char close_ch)
{
  int depth = 0;
  int in_str = 0;
  char quote = '\0';
  for (int i = start; s[i]; ++i)
  {
    char c = s[i];
    if (!in_str && c == '/' && s[i + 1] == '/')
    {
      i += 2;
      while (s[i] && s[i] != '\n')
        ++i;
      continue;
    }
    if (!in_str && c == '/' && s[i + 1] == '*')
    {
      i += 2;
      while (s[i] && !(s[i] == '*' && s[i + 1] == '/'))
        ++i;
      if (s[i])
        ++i;
      continue;
    }
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
      continue;
    }
    if (in_str)
      continue;
    if (c == open_ch)
      ++depth;
    else if (c == close_ch)
    {
      --depth;
      if (depth == 0)
        return i;
      if (depth < 0)
        return -1;
    }
  }
  return -1;
}

static char *strip_gnu_noise(const char *in)
{
  if (!in)
    return NULL;
  size_t n = strlen(in);
  char *out = (char *)malloc(n + 1);
  if (!out)
    return NULL;

  size_t j = 0;
  for (size_t i = 0; i < n; ++i)
  {
    if (strncmp(in + i, "__extension__", 13) == 0)
    {
      i += 12;
      continue;
    }
    if (strncmp(in + i, "__attribute__", 13) == 0)
    {
      i += 13;
      while (i < n && isspace((unsigned char)in[i]))
        ++i;
      if (i < n && in[i] == '(')
      {
        int end = parse_balanced(in, (int)i, '(', ')');
        if (end >= 0)
        {
          i = (size_t)end;
          continue;
        }
      }
      continue;
    }
    if (strncmp(in + i, "__declspec", 10) == 0)
    {
      i += 10;
      while (i < n && isspace((unsigned char)in[i]))
        ++i;
      if (i < n && in[i] == '(')
      {
        int end = parse_balanced(in, (int)i, '(', ')');
        if (end >= 0)
        {
          i = (size_t)end;
          continue;
        }
      }
      continue;
    }
    if (strncmp(in + i, "__asm__", 7) == 0 || strncmp(in + i, "asm", 3) == 0)
    {
      size_t k = i;
      if (strncmp(in + i, "__asm__", 7) == 0)
        k = i + 7;
      else
        k = i + 3;
      while (k < n && isspace((unsigned char)in[k]))
        ++k;
      if (k < n && in[k] == '(')
      {
        int end = parse_balanced(in, (int)k, '(', ')');
        if (end >= 0)
        {
          i = (size_t)end;
          continue;
        }
      }
    }
    out[j++] = in[i];
  }
  out[j] = '\0';
  return out;
}

static int is_typeish_keyword(const char *w)
{
  if (!w || !*w)
    return 0;
  return equals_icase_local(w, "void") || equals_icase_local(w, "char") ||
         equals_icase_local(w, "short") || equals_icase_local(w, "int") ||
         equals_icase_local(w, "long") || equals_icase_local(w, "float") ||
         equals_icase_local(w, "double") || equals_icase_local(w, "signed") ||
         equals_icase_local(w, "unsigned") || equals_icase_local(w, "const") ||
         equals_icase_local(w, "volatile") || equals_icase_local(w, "restrict") ||
         equals_icase_local(w, "register") || equals_icase_local(w, "static") ||
         equals_icase_local(w, "inline") || equals_icase_local(w, "extern") ||
         equals_icase_local(w, "_Bool") || equals_icase_local(w, "bool") ||
         equals_icase_local(w, "size_t") || equals_icase_local(w, "intptr_t") ||
         equals_icase_local(w, "uintptr_t") || equals_icase_local(w, "int8_t") ||
         equals_icase_local(w, "uint8_t") || equals_icase_local(w, "int16_t") ||
         equals_icase_local(w, "uint16_t") || equals_icase_local(w, "int32_t") ||
         equals_icase_local(w, "uint32_t") || equals_icase_local(w, "int64_t") ||
         equals_icase_local(w, "uint64_t") || equals_icase_local(w, "enum") ||
         equals_icase_local(w, "struct") || equals_icase_local(w, "union") ||
         equals_icase_local(w, "typeof") || equals_icase_local(w, "__typeof__");
}

static CType parse_ctype_from_text(const char *text)
{
  if (!text)
    return C_TY_I32;
  if (strchr(text, '*') || strchr(text, '['))
    return C_TY_PTR;

  if (has_word_icase(text, "_Bool") || has_word_icase(text, "bool"))
    return C_TY_I1;
  if (has_word_icase(text, "void"))
    return C_TY_VOID;
  if (has_word_icase(text, "float"))
    return C_TY_F32;
  if (has_word_icase(text, "double"))
    return C_TY_F64;

  if (has_word_icase(text, "uint64_t") || has_word_icase(text, "uintptr_t"))
    return C_TY_U64;
  if (has_word_icase(text, "int64_t") || has_word_icase(text, "intptr_t"))
    return C_TY_I64;
  if (has_word_icase(text, "uint32_t"))
    return C_TY_U32;
  if (has_word_icase(text, "int32_t"))
    return C_TY_I32;
  if (has_word_icase(text, "uint16_t"))
    return C_TY_U16;
  if (has_word_icase(text, "int16_t"))
    return C_TY_I16;
  if (has_word_icase(text, "uint8_t"))
    return C_TY_U8;
  if (has_word_icase(text, "int8_t"))
    return C_TY_I8;

  if (has_word_icase(text, "size_t"))
    return C_TY_U64;

  int is_unsigned = has_word_icase(text, "unsigned") || has_word_icase(text, "uint");

  if (has_word_icase(text, "char"))
    return is_unsigned ? C_TY_U8 : C_TY_I8;
  if (has_word_icase(text, "short"))
    return is_unsigned ? C_TY_U16 : C_TY_I16;
  if (has_word_icase(text, "long"))
    return is_unsigned ? C_TY_U64 : C_TY_I64;

  if (has_word_icase(text, "enum"))
    return is_unsigned ? C_TY_U32 : C_TY_I32;

  if (has_word_icase(text, "struct") || has_word_icase(text, "union"))
    return C_TY_PTR;

  return is_unsigned ? C_TY_U32 : C_TY_I32;
}

static int split_params(const char *params, char ***out_items, int *out_count)
{
  *out_items = NULL;
  *out_count = 0;
  if (!params)
    return 1;

  int cap = 0;
  int depth_paren = 0;
  int depth_bracket = 0;
  const char *start = params;
  for (const char *p = params;; ++p)
  {
    char c = *p;
    if (c == '\0' || (c == ',' && depth_paren == 0 && depth_bracket == 0))
    {
      char *item = trim_dup(start, (size_t)(p - start));
      if (!item)
        return 0;
      if (*item)
      {
        if (*out_count == cap)
        {
          int next = cap ? cap * 2 : 8;
          char **grown = (char **)realloc(*out_items, sizeof(char *) * (size_t)next);
          if (!grown)
          {
            free(item);
            return 0;
          }
          *out_items = grown;
          cap = next;
        }
        (*out_items)[(*out_count)++] = item;
      }
      else
      {
        free(item);
      }
      if (c == '\0')
        break;
      start = p + 1;
      continue;
    }
    if (c == '(')
      ++depth_paren;
    else if (c == ')' && depth_paren > 0)
      --depth_paren;
    else if (c == '[')
      ++depth_bracket;
    else if (c == ']' && depth_bracket > 0)
      --depth_bracket;
  }
  return 1;
}

static void free_string_list(char **items, int count)
{
  if (!items)
    return;
  for (int i = 0; i < count; ++i)
    free(items[i]);
  free(items);
}

static int parse_param_decl(const char *param_text, CType *out_type, char **out_name)
{
  if (!param_text || !out_type || !out_name)
    return 0;
  *out_name = NULL;

  char *clean = trim_dup(param_text, strlen(param_text));
  if (!clean)
    return 0;

  if (strcmp(clean, "void") == 0)
  {
    free(clean);
    *out_type = C_TY_VOID;
    return 1;
  }

  if (strstr(clean, "..."))
  {
    free(clean);
    return 0;
  }

  size_t n = strlen(clean);
  int end = (int)n - 1;
  while (end >= 0 && isspace((unsigned char)clean[end]))
    --end;

  int ident_end = end;
  while (ident_end >= 0 && is_ident_char(clean[ident_end]))
    --ident_end;

  int took_name = 0;
  if (ident_end < end)
  {
    size_t ns = (size_t)(ident_end + 1);
    size_t nl = (size_t)(end - ident_end);
    char *candidate = xstrndup0(clean + ns, nl);
    if (!candidate)
    {
      free(clean);
      return 0;
    }
    if (!is_typeish_keyword(candidate))
    {
      *out_name = candidate;
      clean[ns] = '\0';
      took_name = 1;
    }
    else
    {
      free(candidate);
    }
  }

  if (!took_name && *out_name)
  {
    free(*out_name);
    *out_name = NULL;
  }

  *out_type = parse_ctype_from_text(clean);
  free(clean);
  return 1;
}

static int parse_function_signature(const char *decl_text, CFunction *out_fn)
{
  if (!decl_text || !out_fn)
    return 0;
  memset(out_fn, 0, sizeof(*out_fn));
  out_fn->ret_param_index = -1;

  char *noise_free = strip_gnu_noise(decl_text);
  if (!noise_free)
    return 0;

  char *decl = trim_dup(noise_free, strlen(noise_free));
  free(noise_free);
  if (!decl)
    return 0;
  if (!*decl)
  {
    free(decl);
    return 0;
  }

  if (decl[0] == '#')
  {
    free(decl);
    return 0;
  }

  if (has_word_icase(decl, "typedef"))
  {
    free(decl);
    return 0;
  }

  char *open = strrchr(decl, '(');
  if (!open)
  {
    free(decl);
    return 0;
  }
  int open_idx = (int)(open - decl);
  int close_idx = parse_balanced(decl, open_idx, '(', ')');
  if (close_idx < 0)
  {
    free(decl);
    return 0;
  }

  int name_end = open_idx - 1;
  while (name_end >= 0 && isspace((unsigned char)decl[name_end]))
    --name_end;
  int name_start = name_end;
  while (name_start >= 0 && is_ident_char(decl[name_start]))
    --name_start;
  ++name_start;
  if (name_start > name_end || !is_ident_start_char(decl[name_start]))
  {
    free(decl);
    return 0;
  }

  out_fn->name = xstrndup0(decl + name_start, (size_t)(name_end - name_start + 1));
  if (!out_fn->name)
  {
    free(decl);
    return 0;
  }

  char *ret_text = trim_dup(decl, (size_t)name_start);
  if (!ret_text)
  {
    cfunction_destroy(out_fn);
    free(decl);
    return 0;
  }
  out_fn->ret_type = parse_ctype_from_text(ret_text);
  out_fn->is_static = has_word_icase(ret_text, "static");
  out_fn->is_extern_decl = has_word_icase(ret_text, "extern");
  out_fn->is_noreturn = has_word_icase(ret_text, "noreturn") ||
                        has_word_icase(ret_text, "_Noreturn");
  free(ret_text);

  char *params_text = xstrndup0(decl + open_idx + 1, (size_t)(close_idx - open_idx - 1));
  free(decl);
  if (!params_text)
  {
    cfunction_destroy(out_fn);
    return 0;
  }

  char **items = NULL;
  int item_count = 0;
  if (!split_params(params_text, &items, &item_count))
  {
    free(params_text);
    cfunction_destroy(out_fn);
    return 0;
  }
  free(params_text);

  for (int i = 0; i < item_count; ++i)
  {
    char *p = items[i];
    if (!p)
      continue;
    if (strcmp(p, "...") == 0)
    {
      out_fn->is_varargs = 1;
      continue;
    }
    if (item_count == 1 && strcmp(p, "void") == 0)
      continue;

    CType pt = C_TY_I32;
    char *pname = NULL;
    if (!parse_param_decl(p, &pt, &pname))
    {
      free_string_list(items, item_count);
      cfunction_destroy(out_fn);
      return 0;
    }
    if (!function_param_append(out_fn, pname, pt))
    {
      free(pname);
      free_string_list(items, item_count);
      cfunction_destroy(out_fn);
      return 0;
    }
    free(pname);
  }

  free_string_list(items, item_count);
  return 1;
}

static int parse_int_literal(const char *text, long long *out)
{
  if (!text || !out)
    return 0;
  while (*text && isspace((unsigned char)*text))
    ++text;
  if (!*text)
    return 0;

  if (*text == '\'' && text[1])
  {
    if (text[1] == '\\' && text[2])
    {
      switch (text[2])
      {
      case 'n':
        *out = '\n';
        return 1;
      case 'r':
        *out = '\r';
        return 1;
      case 't':
        *out = '\t';
        return 1;
      case '0':
        *out = '\0';
        return 1;
      default:
        *out = (unsigned char)text[2];
        return 1;
      }
    }
    *out = (unsigned char)text[1];
    return 1;
  }

  char buf[128];
  size_t j = 0;
  for (const char *p = text; *p && j + 1 < sizeof(buf); ++p)
  {
    if (isspace((unsigned char)*p) || *p == ',' || *p == ')' || *p == ';')
      break;
    if ((unsigned char)*p == 'u' || (unsigned char)*p == 'U' ||
        (unsigned char)*p == 'l' || (unsigned char)*p == 'L')
      continue;
    if (isalpha((unsigned char)*p) && j > 0 && !(buf[0] == '0' && (buf[1] == 'x' || buf[1] == 'X')))
      break;
    buf[j++] = *p;
  }
  buf[j] = '\0';
  if (!buf[0])
    return 0;

  char *end = NULL;
  long long v = strtoll(buf, &end, 0);
  if (!end || *end != '\0')
    return 0;
  *out = v;
  return 1;
}

static int collect_macro_constants(CFrontendUnit *impl, const char *source)
{
  if (!impl || !source)
    return 1;

  const char *p = source;
  while (*p)
  {
    const char *line = p;
    while (*p && *p != '\n')
      ++p;
    size_t len = (size_t)(p - line);
    if (*p == '\n')
      ++p;

    const char *s = line;
    const char *e = line + len;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || *s != '#')
      continue;
    ++s;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if ((size_t)(e - s) < 6 || strncmp(s, "define", 6) != 0)
      continue;
    s += 6;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || !is_ident_start_char(*s))
      continue;

    const char *name_start = s;
    ++s;
    while (s < e && is_ident_char(*s))
      ++s;
    if (s < e && *s == '(')
      continue;

    char *name = xstrndup0(name_start, (size_t)(s - name_start));
    if (!name)
      return 0;

    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e)
    {
      free(name);
      continue;
    }

    char *value_text = xstrndup0(s, (size_t)(e - s));
    if (!value_text)
    {
      free(name);
      return 0;
    }
    char *comment = strstr(value_text, "//");
    if (comment)
      *comment = '\0';
    comment = strstr(value_text, "/*");
    if (comment)
      *comment = '\0';

    char *value_trim = trim_dup(value_text, strlen(value_text));
    free(value_text);
    if (!value_trim)
    {
      free(name);
      return 0;
    }

    long long v = 0;
    int ok = parse_int_literal(value_trim, &v);
    if (!ok)
      ok = macro_lookup_value(impl, value_trim, &v);
    if (ok && !macro_set_value(impl, name, v))
    {
      free(value_trim);
      free(name);
      return 0;
    }

    free(value_trim);
    free(name);
  }

  return 1;
}

static int parse_body_summary(const char *body, CFunction *fn)
{
  if (!body || !fn)
    return 0;

  int depth_paren = 0;
  int in_str = 0;
  char quote = '\0';
  for (const char *p = body; *p; ++p)
  {
    char c = *p;
    if (!in_str && c == '/' && p[1] == '/')
    {
      p += 2;
      while (*p && *p != '\n')
        ++p;
      if (!*p)
        break;
      continue;
    }
    if (!in_str && c == '/' && p[1] == '*')
    {
      p += 2;
      while (*p && !(p[0] == '*' && p[1] == '/'))
        ++p;
      if (!*p)
        break;
      ++p;
      continue;
    }
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
      continue;
    }
    if (in_str)
      continue;
    if (c == '(')
      ++depth_paren;
    else if (c == ')' && depth_paren > 0)
      --depth_paren;
    else if ((c == '{' || c == '}') && depth_paren == 0)
      return 0;
  }

  fn->body_kind = C_BODY_COMPLEX;
  fn->ret_param_index = -1;

  free(fn->body_text);
  fn->body_text = xstrdup0(body);
  if (!fn->body_text)
    return 0;

  int saw_return = 0;
  const char *stmt_start = body;
  depth_paren = 0;
  in_str = 0;
  quote = '\0';
  for (const char *p = body;; ++p)
  {
    char c = *p;
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
    }
    if (!in_str)
    {
      if (c == '(')
        ++depth_paren;
      else if (c == ')' && depth_paren > 0)
        --depth_paren;
    }
    if (c == '\0' || (c == ';' && depth_paren == 0 && !in_str))
    {
      char *stmt = trim_dup(stmt_start, (size_t)(p - stmt_start));
      if (!stmt)
        return 0;
      if (strncmp(stmt, "return", 6) == 0 &&
          (stmt[6] == '\0' || isspace((unsigned char)stmt[6])))
      {
        saw_return = 1;
      }
      free(stmt);
      if (c == '\0')
        break;
      stmt_start = p + 1;
    }
  }

  if (!saw_return && fn->ret_type != C_TY_VOID)
    return 0;

  return 1;
}

static int parse_top_level_chunk(CFrontendUnit *impl, const char *chunk, int has_body,
                                 const char *body_text, const char *input_path)
{
  CFunction fn;
  if (!parse_function_signature(chunk, &fn))
    return 1;

  fn.has_body = has_body;
  if (has_body)
  {
    if (!parse_body_summary(body_text ? body_text : "", &fn))
    {
      if (c_is_header_path(input_path))
      {
        /* Header-only inline/complex bodies are ignored if unsupported. */
        fn.has_body = 0;
        if (!fn.is_static)
        {
          if (!extern_append_from_function(impl, &fn))
          {
            cfunction_destroy(&fn);
            return 0;
          }
        }
        cfunction_destroy(&fn);
        return 1;
      }
      fprintf(stderr,
              "c frontend: unsupported function body in '%s' for function '%s'\n",
              input_path ? input_path : "<input>", fn.name ? fn.name : "<unnamed>");
      cfunction_destroy(&fn);
      return 0;
    }
    if (!function_append(impl, &fn))
    {
      cfunction_destroy(&fn);
      return 0;
    }
    return 1;
  }

  if (!fn.is_static)
  {
    if (!extern_append_from_function(impl, &fn))
    {
      cfunction_destroy(&fn);
      return 0;
    }
  }

  cfunction_destroy(&fn);
  return 1;
}

static int parse_translation_unit(CFrontendUnit *impl, const char *source,
                                  const char *input_path)
{
  if (!impl || !source)
    return 0;

  if (!collect_macro_constants(impl, source))
    return 0;

  size_t n = strlen(source);
  size_t start = 0;
  int depth = 0;
  int in_str = 0;
  char quote = '\0';

  for (size_t i = 0; i <= n; ++i)
  {
    char c = (i < n) ? source[i] : '\0';

    if (!in_str && i + 1 < n && c == '/' && source[i + 1] == '/')
    {
      i += 2;
      while (i < n && source[i] != '\n')
        ++i;
      continue;
    }
    if (!in_str && i + 1 < n && c == '/' && source[i + 1] == '*')
    {
      i += 2;
      while (i + 1 < n && !(source[i] == '*' && source[i + 1] == '/'))
        ++i;
      ++i;
      continue;
    }
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
      continue;
    }
    if (in_str)
      continue;

    if (c == '{' && depth == 0)
    {
      char *decl = trim_dup(source + start, i - start);
      if (!decl)
        return 0;
      int close = parse_balanced(source, (int)i, '{', '}');
      if (close < 0)
      {
        free(decl);
        return 0;
      }
      char *body = xstrndup0(source + i + 1, (size_t)close - i - 1);
      if (!body)
      {
        free(decl);
        return 0;
      }
      int ok = parse_top_level_chunk(impl, decl, 1, body, input_path);
      free(body);
      free(decl);
      if (!ok)
        return 0;

      i = (size_t)close;
      start = i + 1;
      continue;
    }

    if (c == '{')
      ++depth;
    else if (c == '}' && depth > 0)
      --depth;

    if ((c == ';' || c == '\0') && depth == 0)
    {
      char *chunk = trim_dup(source + start, i - start);
      if (!chunk)
        return 0;
      int ok = 1;
      if (*chunk)
        ok = parse_top_level_chunk(impl, chunk, 0, NULL, input_path);
      free(chunk);
      if (!ok)
        return 0;
      start = i + 1;
    }
  }

  return 1;
}

static int path_parent(const char *path, char *out, size_t outsz)
{
  if (!path || !out || outsz == 0)
    return 0;
  const char *slash = strrchr(path, '/');
  if (!slash)
    slash = strrchr(path, '\\');
  if (!slash)
  {
    out[0] = '.';
    out[1] = '\0';
    return 1;
  }
  size_t n = (size_t)(slash - path);
  if (n >= outsz)
    n = outsz - 1;
  memcpy(out, path, n);
  out[n] = '\0';
  return 1;
}

static int read_file_text(const char *path, char **out_buf)
{
  *out_buf = NULL;
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  if (fseek(f, 0, SEEK_END) != 0)
  {
    fclose(f);
    return 0;
  }
  long sz = ftell(f);
  if (sz < 0)
  {
    fclose(f);
    return 0;
  }
  rewind(f);
  char *buf = (char *)malloc((size_t)sz + 1);
  if (!buf)
  {
    fclose(f);
    return 0;
  }
  size_t got = fread(buf, 1, (size_t)sz, f);
  fclose(f);
  if (got != (size_t)sz)
  {
    free(buf);
    return 0;
  }
  buf[sz] = '\0';
  *out_buf = buf;
  return 1;
}

static int file_exists(const char *path)
{
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  fclose(f);
  return 1;
}

static int path_is_absolute_local(const char *path)
{
  if (!path || !*path)
    return 0;
  if (path[0] == '/' || path[0] == '\\')
    return 1;
#ifdef _WIN32
  if (isalpha((unsigned char)path[0]) && path[1] == ':')
    return 1;
#endif
  return 0;
}

static int normalize_existing_path(const char *path, char *out, size_t outsz)
{
  if (!path || !*path || !out || outsz == 0)
    return 0;
#ifdef _WIN32
  if (_fullpath(out, path, outsz) == NULL)
    return 0;
  return 1;
#else
  char *rp = realpath(path, NULL);
  if (!rp)
    return 0;
  size_t n = strlen(rp);
  if (n + 1 > outsz)
  {
    free(rp);
    return 0;
  }
  memcpy(out, rp, n + 1);
  free(rp);
  return 1;
#endif
}

static int resolve_include_candidate(const char *candidate,
                                     char *out,
                                     size_t outsz)
{
  if (!candidate || !*candidate || !out || outsz == 0)
    return 0;
  if (!file_exists(candidate))
    return 0;

  char norm[4096];
  if (normalize_existing_path(candidate, norm, sizeof(norm)))
  {
    snprintf(out, outsz, "%s", norm);
    return 1;
  }

  snprintf(out, outsz, "%s", candidate);
  return 1;
}

static int visited_header_push(CFrontendUnit *impl, const char *path)
{
  if (!impl || !path)
    return 0;
  for (int i = 0; i < impl->visited_header_count; ++i)
  {
    if (strcmp(impl->visited_headers[i], path) == 0)
      return 1;
  }
  if (impl->visited_header_count == impl->visited_header_cap)
  {
    int next = impl->visited_header_cap ? impl->visited_header_cap * 2 : 16;
    char **grown = (char **)realloc(impl->visited_headers, sizeof(char *) * (size_t)next);
    if (!grown)
      return 0;
    impl->visited_headers = grown;
    impl->visited_header_cap = next;
  }
  impl->visited_headers[impl->visited_header_count] = xstrdup0(path);
  if (!impl->visited_headers[impl->visited_header_count])
    return 0;
  ++impl->visited_header_count;
  return 1;
}

static int resolve_include_path(const char *current_path, const char *name,
                                int quoted, char **include_dirs, int include_dir_count,
                                char *out, size_t outsz)
{
  if (!name || !*name || !out || outsz == 0)
    return 0;

  if (path_is_absolute_local(name))
    return resolve_include_candidate(name, out, outsz);

  if (strchr(name, '/') || strchr(name, '\\'))
  {
    if (quoted && current_path && *current_path)
    {
      char dir[1024];
      char candidate[4096];
      if (path_parent(current_path, dir, sizeof(dir)))
      {
        snprintf(candidate, sizeof(candidate), "%s/%s", dir, name);
        if (resolve_include_candidate(candidate, out, outsz))
          return 1;
      }
    }
    return resolve_include_candidate(name, out, outsz);
  }

  if (quoted && current_path && *current_path)
  {
    char dir[1024];
    if (path_parent(current_path, dir, sizeof(dir)))
    {
      char candidate[4096];
      snprintf(candidate, sizeof(candidate), "%s/%s", dir, name);
      if (resolve_include_candidate(candidate, out, outsz))
        return 1;
    }
  }

  for (int i = 0; i < include_dir_count; ++i)
  {
    if (!include_dirs || !include_dirs[i])
      continue;
    char candidate[4096];
    snprintf(candidate, sizeof(candidate), "%s/%s", include_dirs[i], name);
    if (resolve_include_candidate(candidate, out, outsz))
      return 1;
  }

  return 0;
}

static int scan_header_tree(CFrontendUnit *impl, const char *path,
                            char **include_dirs, int include_dir_count,
                            int depth)
{
  if (!impl || !path || depth > 32)
    return 1;
  if (!visited_header_push(impl, path))
    return 0;

  char *src = NULL;
  if (!read_file_text(path, &src))
    return 1;

  if (!parse_translation_unit(impl, src, path))
  {
    free(src);
    return 0;
  }

  const char *p = src;
  while (*p)
  {
    const char *line = p;
    while (*p && *p != '\n')
      ++p;
    size_t len = (size_t)(p - line);
    if (*p == '\n')
      ++p;

    const char *s = line;
    const char *e = line + len;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || *s != '#')
      continue;
    ++s;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if ((size_t)(e - s) < 7 || strncmp(s, "include", 7) != 0)
      continue;
    s += 7;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || (*s != '<' && *s != '"'))
      continue;
    int quoted = (*s == '"');
    char closer = quoted ? '"' : '>';
    ++s;
    const char *name_start = s;
    while (s < e && *s != closer)
      ++s;
    if (s <= name_start)
      continue;

    char *inc_name = xstrndup0(name_start, (size_t)(s - name_start));
    if (!inc_name)
      return 0;

    char resolved[2048];
    int have = resolve_include_path(path, inc_name, quoted, include_dirs,
                                    include_dir_count, resolved,
                                    sizeof(resolved));
    free(inc_name);
    if (!have)
      continue;

    int already = 0;
    for (int vi = 0; vi < impl->visited_header_count; ++vi)
    {
      if (strcmp(impl->visited_headers[vi], resolved) == 0)
      {
        already = 1;
        break;
      }
    }
    if (already)
      continue;

    if (!scan_header_tree(impl, resolved, include_dirs, include_dir_count,
                          depth + 1))
    {
      free(src);
      return 0;
    }
  }

  free(src);
  return 1;
}

static int scan_includes_from_source(CFrontendUnit *impl,
                                     const char *current_path,
                                     const char *source,
                                     char **include_dirs,
                                     int include_dir_count)
{
  if (!impl || !source)
    return 1;

  const char *p = source;
  while (*p)
  {
    const char *line = p;
    while (*p && *p != '\n')
      ++p;
    size_t len = (size_t)(p - line);
    if (*p == '\n')
      ++p;

    const char *s = line;
    const char *e = line + len;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || *s != '#')
      continue;
    ++s;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if ((size_t)(e - s) < 7 || strncmp(s, "include", 7) != 0)
      continue;
    s += 7;
    while (s < e && isspace((unsigned char)*s))
      ++s;
    if (s >= e || (*s != '<' && *s != '"'))
      continue;
    int quoted = (*s == '"');
    char closer = quoted ? '"' : '>';
    ++s;
    const char *name_start = s;
    while (s < e && *s != closer)
      ++s;
    if (s <= name_start)
      continue;

    char *inc_name = xstrndup0(name_start, (size_t)(s - name_start));
    if (!inc_name)
      return 0;

    char resolved[2048];
    int have = resolve_include_path(current_path, inc_name, quoted, include_dirs,
                                    include_dir_count, resolved,
                                    sizeof(resolved));
    free(inc_name);
    if (!have)
      continue;

    int already = 0;
    for (int vi = 0; vi < impl->visited_header_count; ++vi)
    {
      if (strcmp(impl->visited_headers[vi], resolved) == 0)
      {
        already = 1;
        break;
      }
    }
    if (already)
      continue;

    if (!scan_header_tree(impl, resolved, include_dirs, include_dir_count,
                          1))
    {
      return 0;
    }
  }
  return 1;
}

typedef struct
{
  char **items;
  int count;
  int cap;
} CLineBuffer;

typedef struct
{
  char *name;
  CType type;
} CLocalVar;

typedef struct
{
  const CFrontendUnit *impl;
  const CFunction *fn;
  CLineBuffer body;
  CLocalVar *locals;
  int local_count;
  int local_cap;
} CEmitBuilder;

typedef struct
{
  const char *s;
  size_t pos;
  CEmitBuilder *builder;
} CExprParser;

static void c_line_buffer_destroy(CLineBuffer *buf)
{
  if (!buf)
    return;
  for (int i = 0; i < buf->count; ++i)
    free(buf->items[i]);
  free(buf->items);
  buf->items = NULL;
  buf->count = 0;
  buf->cap = 0;
}

static int c_line_buffer_appendf(CLineBuffer *buf, const char *fmt, ...)
{
  if (!buf || !fmt)
    return 0;
  va_list ap;
  va_start(ap, fmt);
  int need = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  if (need < 0)
    return 0;

  char *line = (char *)malloc((size_t)need + 1);
  if (!line)
    return 0;

  va_start(ap, fmt);
  vsnprintf(line, (size_t)need + 1, fmt, ap);
  va_end(ap);

  if (buf->count == buf->cap)
  {
    int next = buf->cap ? buf->cap * 2 : 32;
    char **grown = (char **)realloc(buf->items, sizeof(char *) * (size_t)next);
    if (!grown)
    {
      free(line);
      return 0;
    }
    buf->items = grown;
    buf->cap = next;
  }
  buf->items[buf->count++] = line;
  return 1;
}

static void c_emit_builder_destroy(CEmitBuilder *b)
{
  if (!b)
    return;
  c_line_buffer_destroy(&b->body);
  for (int i = 0; i < b->local_count; ++i)
    free(b->locals[i].name);
  free(b->locals);
  b->locals = NULL;
  b->local_count = 0;
  b->local_cap = 0;
}

static int c_find_param(const CFunction *fn, const char *name, CType *out_ty)
{
  if (!fn || !name)
    return -1;
  for (int i = 0; i < fn->param_count; ++i)
  {
    if (fn->params[i].name && strcmp(fn->params[i].name, name) == 0)
    {
      if (out_ty)
        *out_ty = fn->params[i].type;
      return i;
    }
  }
  return -1;
}

static int c_find_local(const CEmitBuilder *b, const char *name, CType *out_ty)
{
  if (!b || !name)
    return -1;
  for (int i = 0; i < b->local_count; ++i)
  {
    if (b->locals[i].name && strcmp(b->locals[i].name, name) == 0)
    {
      if (out_ty)
        *out_ty = b->locals[i].type;
      return i;
    }
  }
  return -1;
}

static int c_add_local(CEmitBuilder *b, const char *name, CType type, int *out_index)
{
  if (!b || !name || !*name)
    return 0;

  int idx = c_find_local(b, name, NULL);
  if (idx >= 0)
  {
    if (out_index)
      *out_index = idx;
    return 1;
  }

  if (b->local_count == b->local_cap)
  {
    int next = b->local_cap ? b->local_cap * 2 : 8;
    CLocalVar *grown = (CLocalVar *)realloc(b->locals, sizeof(CLocalVar) * (size_t)next);
    if (!grown)
      return 0;
    b->locals = grown;
    b->local_cap = next;
  }

  CLocalVar *v = &b->locals[b->local_count];
  memset(v, 0, sizeof(*v));
  v->name = xstrdup0(name);
  if (!v->name)
    return 0;
  v->type = type;
  if (out_index)
    *out_index = b->local_count;
  ++b->local_count;
  return 1;
}

static int c_lookup_function_signature(const CFrontendUnit *impl,
                                       const char *name,
                                       CType *out_ret,
                                       int *out_varargs)
{
  if (out_ret)
    *out_ret = C_TY_I32;
  if (out_varargs)
    *out_varargs = 0;
  if (!impl || !name)
    return 0;

  for (int i = 0; i < impl->function_count; ++i)
  {
    const CFunction *fn = &impl->functions[i];
    if (fn->name && strcmp(fn->name, name) == 0)
    {
      if (out_ret)
        *out_ret = fn->ret_type;
      if (out_varargs)
        *out_varargs = fn->is_varargs;
      return 1;
    }
  }
  for (int i = 0; i < impl->extern_count; ++i)
  {
    const Symbol *s = &impl->externs[i];
    if (s->name && strcmp(s->name, name) == 0)
    {
      if (out_ret)
        *out_ret = ast_to_ctype(s->sig.ret);
      if (out_varargs)
        *out_varargs = s->sig.is_varargs;
      return 1;
    }
  }
  return 0;
}

static void c_expr_skip_ws(CExprParser *p)
{
  while (p->s[p->pos] && isspace((unsigned char)p->s[p->pos]))
    ++p->pos;
}

static int c_expr_parse_ident(CExprParser *p, char *out, size_t outsz)
{
  c_expr_skip_ws(p);
  if (!is_ident_start_char(p->s[p->pos]))
    return 0;
  size_t i = 0;
  while (is_ident_char(p->s[p->pos]))
  {
    if (i + 1 < outsz)
      out[i++] = p->s[p->pos];
    ++p->pos;
  }
  out[i] = '\0';
  return i > 0;
}

static int c_expr_parse_string_token(CExprParser *p, char **out)
{
  *out = NULL;
  c_expr_skip_ws(p);
  if (p->s[p->pos] != '"')
    return 0;
  size_t start = p->pos;
  ++p->pos;
  while (p->s[p->pos])
  {
    if (p->s[p->pos] == '\\' && p->s[p->pos + 1])
    {
      p->pos += 2;
      continue;
    }
    if (p->s[p->pos] == '"')
    {
      ++p->pos;
      *out = xstrndup0(p->s + start, p->pos - start);
      return *out != NULL;
    }
    ++p->pos;
  }
  return 0;
}

static int c_type_is_numeric(CType t)
{
  return t == C_TY_I1 || t == C_TY_I8 || t == C_TY_U8 ||
         t == C_TY_I16 || t == C_TY_U16 || t == C_TY_I32 ||
         t == C_TY_U32 || t == C_TY_I64 || t == C_TY_U64;
}

static int c_parse_expression(CExprParser *p, CType *out_type);

static int c_parse_primary(CExprParser *p, CType *out_type)
{
  c_expr_skip_ws(p);
  char ch = p->s[p->pos];
  if (!ch)
    return 0;

  if (ch == '(')
  {
    ++p->pos;
    if (!c_parse_expression(p, out_type))
      return 0;
    c_expr_skip_ws(p);
    if (p->s[p->pos] != ')')
      return 0;
    ++p->pos;
    return 1;
  }

  if (ch == '"')
  {
    char *tok = NULL;
    if (!c_expr_parse_string_token(p, &tok) || !tok)
      return 0;
    int ok = c_line_buffer_appendf(&p->builder->body, "  const_str %s", tok);
    free(tok);
    if (!ok)
      return 0;
    *out_type = C_TY_PTR;
    return 1;
  }

  if (isdigit((unsigned char)ch) || ch == '\'')
  {
    size_t start = p->pos;
    if (ch == '\'')
    {
      ++p->pos;
      while (p->s[p->pos])
      {
        if (p->s[p->pos] == '\\' && p->s[p->pos + 1])
        {
          p->pos += 2;
          continue;
        }
        if (p->s[p->pos] == '\'')
        {
          ++p->pos;
          break;
        }
        ++p->pos;
      }
    }
    else
    {
      while (p->s[p->pos] &&
             !isspace((unsigned char)p->s[p->pos]) &&
             p->s[p->pos] != ')' && p->s[p->pos] != ',' &&
             p->s[p->pos] != ';')
      {
        ++p->pos;
      }
    }
    char *tok = xstrndup0(p->s + start, p->pos - start);
    if (!tok)
      return 0;
    long long v = 0;
    int ok = parse_int_literal(tok, &v);
    free(tok);
    if (!ok)
      return 0;
    if (!c_line_buffer_appendf(&p->builder->body, "  const i32 %lld", v))
      return 0;
    *out_type = C_TY_I32;
    return 1;
  }

  if (is_ident_start_char(ch))
  {
    char ident[256];
    if (!c_expr_parse_ident(p, ident, sizeof(ident)))
      return 0;
    c_expr_skip_ws(p);
    if (p->s[p->pos] == '(')
    {
      ++p->pos;
      CType arg_types[128];
      int arg_count = 0;
      c_expr_skip_ws(p);
      if (p->s[p->pos] != ')')
      {
        while (1)
        {
          if (arg_count >= (int)(sizeof(arg_types) / sizeof(arg_types[0])))
            return 0;
          if (!c_parse_expression(p, &arg_types[arg_count]))
            return 0;
          ++arg_count;
          c_expr_skip_ws(p);
          if (p->s[p->pos] == ',')
          {
            ++p->pos;
            continue;
          }
          break;
        }
      }
      c_expr_skip_ws(p);
      if (p->s[p->pos] != ')')
        return 0;
      ++p->pos;

      char arg_sig[1024];
      size_t used = 0;
      arg_sig[used++] = '(';
      for (int i = 0; i < arg_count; ++i)
      {
        const char *tname = ctype_to_ccb(arg_types[i]);
        int n = snprintf(arg_sig + used, sizeof(arg_sig) - used,
                         "%s%s", (i ? "," : ""), tname);
        if (n < 0 || (size_t)n >= sizeof(arg_sig) - used)
          return 0;
        used += (size_t)n;
      }
      if (used + 2 > sizeof(arg_sig))
        return 0;
      arg_sig[used++] = ')';
      arg_sig[used] = '\0';

      CType ret_ty = C_TY_I32;
      int is_varargs = 0;
      (void)c_lookup_function_signature(p->builder->impl, ident, &ret_ty, &is_varargs);
      if (!c_line_buffer_appendf(&p->builder->body,
                                 "  call %s %s %s%s",
                                 ident,
                                 ctype_to_ccb(ret_ty),
                                 arg_sig,
                                 is_varargs ? " varargs" : ""))
      {
        return 0;
      }
      *out_type = ret_ty;
      return 1;
    }

    if (strcmp(ident, "NULL") == 0)
    {
      if (!c_line_buffer_appendf(&p->builder->body, "  const ptr null"))
        return 0;
      *out_type = C_TY_PTR;
      return 1;
    }

    CType vty;
    int local_idx = c_find_local(p->builder, ident, &vty);
    if (local_idx >= 0)
    {
      if (!c_line_buffer_appendf(&p->builder->body, "  load_local %d", local_idx))
        return 0;
      *out_type = vty;
      return 1;
    }

    int param_idx = c_find_param(p->builder->fn, ident, &vty);
    if (param_idx >= 0)
    {
      if (!c_line_buffer_appendf(&p->builder->body, "  load_param %d", param_idx))
        return 0;
      *out_type = vty;
      return 1;
    }

    long long macro_val = 0;
    if (macro_lookup_value(p->builder->impl, ident, &macro_val))
    {
      if (!c_line_buffer_appendf(&p->builder->body, "  const i32 %lld", macro_val))
        return 0;
      *out_type = C_TY_I32;
      return 1;
    }

    return 0;
  }

  return 0;
}

static int c_parse_unary(CExprParser *p, CType *out_type)
{
  c_expr_skip_ws(p);
  if (p->s[p->pos] == '+')
  {
    ++p->pos;
    return c_parse_unary(p, out_type);
  }
  if (p->s[p->pos] == '-')
  {
    ++p->pos;
    c_expr_skip_ws(p);
    size_t start = p->pos;
    while (p->s[p->pos] &&
           !isspace((unsigned char)p->s[p->pos]) &&
           p->s[p->pos] != ')' && p->s[p->pos] != ',' &&
           p->s[p->pos] != ';')
    {
      ++p->pos;
    }
    char *tok = xstrndup0(p->s + start, p->pos - start);
    if (!tok)
      return 0;
    char *signed_tok = (char *)malloc(strlen(tok) + 2);
    if (!signed_tok)
    {
      free(tok);
      return 0;
    }
    signed_tok[0] = '-';
    strcpy(signed_tok + 1, tok);
    free(tok);
    long long v = 0;
    int ok = parse_int_literal(signed_tok, &v);
    free(signed_tok);
    if (!ok)
      return 0;
    if (!c_line_buffer_appendf(&p->builder->body, "  const i32 %lld", v))
      return 0;
    *out_type = C_TY_I32;
    return 1;
  }
  return c_parse_primary(p, out_type);
}

static int c_parse_mul(CExprParser *p, CType *out_type)
{
  if (!c_parse_unary(p, out_type))
    return 0;
  while (1)
  {
    c_expr_skip_ws(p);
    char op = p->s[p->pos];
    const char *op_name = NULL;
    if (op == '*')
      op_name = "mul";
    else if (op == '/')
      op_name = "div";
    else if (op == '%')
      op_name = "mod";
    if (!op_name)
      break;
    ++p->pos;
    CType rhs;
    if (!c_parse_unary(p, &rhs))
      return 0;
    if (!c_type_is_numeric(*out_type) || !c_type_is_numeric(rhs) || rhs != *out_type)
      return 0;
    if (!c_line_buffer_appendf(&p->builder->body, "  binop %s %s", op_name, ctype_to_ccb(*out_type)))
      return 0;
  }
  return 1;
}

static int c_parse_add(CExprParser *p, CType *out_type)
{
  if (!c_parse_mul(p, out_type))
    return 0;
  while (1)
  {
    c_expr_skip_ws(p);
    char op = p->s[p->pos];
    const char *op_name = NULL;
    if (op == '+')
      op_name = "add";
    else if (op == '-')
      op_name = "sub";
    if (!op_name)
      break;
    ++p->pos;
    CType rhs;
    if (!c_parse_mul(p, &rhs))
      return 0;
    if (!c_type_is_numeric(*out_type) || !c_type_is_numeric(rhs) || rhs != *out_type)
      return 0;
    if (!c_line_buffer_appendf(&p->builder->body, "  binop %s %s", op_name, ctype_to_ccb(*out_type)))
      return 0;
  }
  return 1;
}

static int c_parse_expression(CExprParser *p, CType *out_type)
{
  return c_parse_add(p, out_type);
}

static int c_find_top_level_assignment(const char *stmt)
{
  int in_str = 0;
  char quote = '\0';
  int depth_paren = 0;
  for (int i = 0; stmt[i]; ++i)
  {
    char c = stmt[i];
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
      continue;
    }
    if (in_str)
      continue;
    if (c == '(')
      ++depth_paren;
    else if (c == ')' && depth_paren > 0)
      --depth_paren;
    else if (depth_paren == 0 && c == '=')
    {
      char prev = (i > 0) ? stmt[i - 1] : '\0';
      char next = stmt[i + 1];
      if (prev == '=' || prev == '!' || prev == '<' || prev == '>' || next == '=')
        continue;
      return i;
    }
  }
  return -1;
}

static int c_is_decl_prefix(const char *text)
{
  if (!text)
    return 0;
  while (*text && isspace((unsigned char)*text))
    ++text;
  if (!*text)
    return 0;

  char first[64];
  size_t i = 0;
  while (text[i] && is_ident_char(text[i]) && i + 1 < sizeof(first))
  {
    first[i] = text[i];
    ++i;
  }
  first[i] = '\0';
  if (!first[0])
    return 0;

  if (is_typeish_keyword(first))
    return 1;
  return equals_icase_local(first, "const") ||
         equals_icase_local(first, "volatile") ||
         equals_icase_local(first, "signed") ||
         equals_icase_local(first, "unsigned") ||
         equals_icase_local(first, "long") ||
         equals_icase_local(first, "short");
}

static char *c_strip_comments_dup(const char *in)
{
  if (!in)
    return NULL;
  size_t n = strlen(in);
  char *out = (char *)malloc(n + 1);
  if (!out)
    return NULL;
  size_t j = 0;
  int in_str = 0;
  char quote = '\0';
  for (size_t i = 0; i < n; ++i)
  {
    char c = in[i];
    if (!in_str && c == '/' && i + 1 < n && in[i + 1] == '/')
    {
      i += 2;
      while (i < n && in[i] != '\n')
        ++i;
      if (i < n)
        out[j++] = '\n';
      continue;
    }
    if (!in_str && c == '/' && i + 1 < n && in[i + 1] == '*')
    {
      i += 2;
      while (i + 1 < n && !(in[i] == '*' && in[i + 1] == '/'))
        ++i;
      ++i;
      out[j++] = ' ';
      continue;
    }
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
    }
    out[j++] = c;
  }
  out[j] = '\0';
  return out;
}

static int c_emit_statement_expr(CEmitBuilder *b, const char *stmt)
{
  CExprParser parser = {.s = stmt, .pos = 0, .builder = b};
  CType ty = C_TY_VOID;
  if (!c_parse_expression(&parser, &ty))
    return 0;
  c_expr_skip_ws(&parser);
  if (parser.s[parser.pos] != '\0')
    return 0;
  if (ty != C_TY_VOID)
  {
    if (!c_line_buffer_appendf(&b->body, "  drop %s", ctype_to_ccb(ty)))
      return 0;
  }
  return 1;
}

static int c_emit_function_body(CEmitBuilder *b)
{
  if (!b || !b->fn || !b->fn->body_text)
    return 0;

  char *clean = c_strip_comments_dup(b->fn->body_text);
  if (!clean)
    return 0;

  const char *body = clean;
  const char *stmt_start = body;
  int depth_paren = 0;
  int in_str = 0;
  char quote = '\0';
  int saw_return = 0;

  for (const char *p = body;; ++p)
  {
    char c = *p;
    if (c == '\'' || c == '"')
    {
      if (!in_str)
      {
        in_str = 1;
        quote = c;
      }
      else if (quote == c)
      {
        in_str = 0;
      }
    }
    if (!in_str)
    {
      if (c == '(')
        ++depth_paren;
      else if (c == ')' && depth_paren > 0)
        --depth_paren;
    }

    if (c == '\0' || (c == ';' && depth_paren == 0 && !in_str))
    {
      char *stmt = trim_dup(stmt_start, (size_t)(p - stmt_start));
      if (!stmt)
      {
        free(clean);
        return 0;
      }
      if (*stmt)
      {
        if ((strncmp(stmt, "asm", 3) == 0) || (strncmp(stmt, "__asm__", 7) == 0))
        {
          /* Ignored. */
        }
        else if (strncmp(stmt, "return", 6) == 0 &&
                 (stmt[6] == '\0' || isspace((unsigned char)stmt[6])))
        {
          saw_return = 1;
          const char *expr = stmt + 6;
          while (*expr && isspace((unsigned char)*expr))
            ++expr;
          if (!*expr)
          {
            if (b->fn->ret_type != C_TY_VOID)
            {
              free(stmt);
              free(clean);
              return 0;
            }
          }
          else
          {
            if (b->fn->ret_type == C_TY_VOID)
            {
              free(stmt);
              free(clean);
              return 0;
            }
            CExprParser parser = {.s = expr, .pos = 0, .builder = b};
            CType expr_ty = C_TY_VOID;
            if (!c_parse_expression(&parser, &expr_ty))
            {
              free(stmt);
              free(clean);
              return 0;
            }
            c_expr_skip_ws(&parser);
            if (parser.s[parser.pos] != '\0')
            {
              free(stmt);
              free(clean);
              return 0;
            }
            if (expr_ty == C_TY_VOID)
            {
              free(stmt);
              free(clean);
              return 0;
            }
          }
          if (!c_line_buffer_appendf(&b->body, "  ret"))
          {
            free(stmt);
            free(clean);
            return 0;
          }
        }
        else
        {
          int eq = c_find_top_level_assignment(stmt);
          int handled = 0;

          if (c_is_decl_prefix(stmt))
          {
            size_t lhs_len = (eq >= 0) ? (size_t)eq : strlen(stmt);
            char *lhs = trim_dup(stmt, lhs_len);
            if (!lhs)
            {
              free(stmt);
              free(clean);
              return 0;
            }
            CType local_ty = C_TY_I32;
            char *local_name = NULL;
            if (parse_param_decl(lhs, &local_ty, &local_name) && local_name && *local_name)
            {
              int local_idx = -1;
              if (!c_add_local(b, local_name, local_ty, &local_idx))
              {
                free(local_name);
                free(lhs);
                free(stmt);
                free(clean);
                return 0;
              }
              if (eq >= 0)
              {
                const char *rhs = stmt + eq + 1;
                CExprParser parser = {.s = rhs, .pos = 0, .builder = b};
                CType rhs_ty = C_TY_VOID;
                if (!c_parse_expression(&parser, &rhs_ty))
                {
                  free(local_name);
                  free(lhs);
                  free(stmt);
                  free(clean);
                  return 0;
                }
                c_expr_skip_ws(&parser);
                if (parser.s[parser.pos] != '\0' || rhs_ty == C_TY_VOID)
                {
                  free(local_name);
                  free(lhs);
                  free(stmt);
                  free(clean);
                  return 0;
                }
                if (!c_line_buffer_appendf(&b->body, "  store_local %d", local_idx))
                {
                  free(local_name);
                  free(lhs);
                  free(stmt);
                  free(clean);
                  return 0;
                }
              }
              handled = 1;
            }
            free(local_name);
            free(lhs);
          }

          if (!handled && eq >= 0)
          {
            char *lhs = trim_dup(stmt, (size_t)eq);
            if (!lhs)
            {
              free(stmt);
              free(clean);
              return 0;
            }
            CType lhs_ty;
            int local_idx = c_find_local(b, lhs, &lhs_ty);
            free(lhs);
            if (local_idx >= 0)
            {
              const char *rhs = stmt + eq + 1;
              CExprParser parser = {.s = rhs, .pos = 0, .builder = b};
              CType rhs_ty = C_TY_VOID;
              if (!c_parse_expression(&parser, &rhs_ty))
              {
                free(stmt);
                free(clean);
                return 0;
              }
              c_expr_skip_ws(&parser);
              if (parser.s[parser.pos] != '\0' || rhs_ty == C_TY_VOID)
              {
                free(stmt);
                free(clean);
                return 0;
              }
              if (!c_line_buffer_appendf(&b->body, "  store_local %d", local_idx))
              {
                free(stmt);
                free(clean);
                return 0;
              }
              handled = 1;
            }
          }

          if (!handled)
          {
            if (!c_emit_statement_expr(b, stmt))
            {
              free(stmt);
              free(clean);
              return 0;
            }
          }
        }
      }
      free(stmt);
      if (c == '\0')
        break;
      stmt_start = p + 1;
    }
  }

  if (!saw_return)
  {
    if (b->fn->ret_type == C_TY_VOID)
    {
      if (!c_line_buffer_appendf(&b->body, "  ret"))
      {
        free(clean);
        return 0;
      }
    }
    else
    {
      free(clean);
      return 0;
    }
  }

  free(clean);
  return 1;
}

static int c_emit_function(FILE *out, const CFrontendUnit *impl, const CFunction *fn)
{
  if (!out || !impl || !fn || !fn->name || !fn->has_body)
    return 0;

  CEmitBuilder b;
  memset(&b, 0, sizeof(b));
  b.impl = impl;
  b.fn = fn;
  if (!c_emit_function_body(&b))
  {
    c_emit_builder_destroy(&b);
    return 0;
  }

  if (fprintf(out, ".func %s ret=%s params=%d locals=%d\n", fn->name,
              ctype_to_ccb(fn->ret_type), fn->param_count, b.local_count) < 0)
  {
    c_emit_builder_destroy(&b);
    return 0;
  }

  if (fn->param_count > 0)
  {
    if (fputs(".params", out) < 0)
    {
      c_emit_builder_destroy(&b);
      return 0;
    }
    for (int i = 0; i < fn->param_count; ++i)
    {
      if (fprintf(out, " %s", ctype_to_ccb(fn->params[i].type)) < 0)
      {
        c_emit_builder_destroy(&b);
        return 0;
      }
    }
    if (fputc('\n', out) == EOF)
    {
      c_emit_builder_destroy(&b);
      return 0;
    }
  }

  if (b.local_count > 0)
  {
    if (fputs(".locals", out) < 0)
    {
      c_emit_builder_destroy(&b);
      return 0;
    }
    for (int i = 0; i < b.local_count; ++i)
    {
      if (fprintf(out, " %s", ctype_to_ccb(b.locals[i].type)) < 0)
      {
        c_emit_builder_destroy(&b);
        return 0;
      }
    }
    if (fputc('\n', out) == EOF)
    {
      c_emit_builder_destroy(&b);
      return 0;
    }
  }

  for (int i = 0; i < b.body.count; ++i)
  {
    if (fprintf(out, "%s\n", b.body.items[i]) < 0)
    {
      c_emit_builder_destroy(&b);
      return 0;
    }
  }

  if (fputs(".endfunc\n", out) < 0)
  {
    c_emit_builder_destroy(&b);
    return 0;
  }
  if (fprintf(out, ".preserve %s\n", fn->name) < 0)
  {
    c_emit_builder_destroy(&b);
    return 0;
  }

  c_emit_builder_destroy(&b);
  return 1;
}

static CFrontendUnit *c_impl(FrontendUnit *unit)
{
  if (!unit || !unit->impl)
    return NULL;
  return (CFrontendUnit *)unit->impl;
}

static const CFrontendUnit *c_impl_const(const FrontendUnit *unit)
{
  if (!unit || !unit->impl)
    return NULL;
  return (const CFrontendUnit *)unit->impl;
}

static FrontendUnit *c_load_unit(const ChanceFrontend *frontend,
                                 const ChanceFrontendLoadRequest *request,
                                 int keep_semantic_state)
{
  (void)frontend;
  (void)keep_semantic_state;
  if (!request)
    return NULL;

  FrontendUnit *unit = (FrontendUnit *)calloc(1, sizeof(FrontendUnit));
  CFrontendUnit *impl = (CFrontendUnit *)calloc(1, sizeof(CFrontendUnit));
  if (!unit || !impl)
  {
    free(unit);
    free(impl);
    return NULL;
  }

  impl->parse_ok = 1;
  impl->c_dialect = request->c_dialect;

  const char *parse_src = request->source.src ? request->source.src : request->raw_source;
  if (!parse_src || !parse_translation_unit(impl, parse_src, request->input_path))
    impl->parse_ok = 0;

  if (impl->parse_ok && request->raw_source && request->input_path)
  {
    if (!scan_includes_from_source(impl, request->input_path,
                                   request->raw_source,
                                   request->include_dirs,
                                   request->include_dir_count))
      impl->parse_ok = 0;
  }

  if (!impl->parse_ok)
  {
    fprintf(stderr, "c frontend: parse failed for '%s'\n",
            request->input_path ? request->input_path : "<unknown>");
  }

  unit->frontend = chance_frontend_c();
  unit->impl = impl;
  return unit;
}

static void c_discard_semantic_state(FrontendUnit *unit)
{
  (void)unit;
}

static Node *c_unit_ast(FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static const Node *c_unit_ast_const(const FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static SymTable *c_unit_symtab(FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static int c_unit_check(FrontendUnit *unit)
{
  const CFrontendUnit *impl = c_impl_const(unit);
  return (impl && impl->parse_ok) ? 0 : 1;
}

static void c_register_foreign_symbols(FrontendUnit *target,
                                       const FrontendUnit *foreign)
{
  (void)target;
  (void)foreign;
}

static const Symbol *c_unit_externs(FrontendUnit *unit, int *count)
{
  CFrontendUnit *impl = c_impl(unit);
  if (!impl)
  {
    if (count)
      *count = 0;
    return NULL;
  }
  if (count)
    *count = impl->extern_count;
  return impl->externs;
}

static Symbol *c_copy_imported_function_symbols(const FrontendUnit *unit,
                                                int *out_count)
{
  (void)unit;
  if (out_count)
    *out_count = 0;
  return NULL;
}

static Symbol *c_copy_imported_global_symbols(const FrontendUnit *unit,
                                              int *out_count)
{
  (void)unit;
  if (out_count)
    *out_count = 0;
  return NULL;
}

static void c_track_imported_function(FrontendUnit *unit,
                                      const char *name,
                                      const char *module_full,
                                      const Symbol *symbol)
{
  (void)unit;
  (void)name;
  (void)module_full;
  (void)symbol;
}

static int c_supports_direct_ccb_emit(const FrontendUnit *unit)
{
  const CFrontendUnit *impl = c_impl_const(unit);
  return impl && impl->parse_ok;
}

static int c_emit_ccb(FrontendUnit *unit,
                      const ChanceFrontendEmitRequest *request)
{
  CFrontendUnit *impl = c_impl(unit);
  if (!impl || !request || !request->ccb_output_path || !impl->parse_ok)
    return 1;

  FILE *out = fopen(request->ccb_output_path, "wb");
  if (!out)
    return 1;

  int ok = 1;
  int emitted_any_extern = 0;
  if (fputs("ccbytecode 3\n\n", out) < 0)
    ok = 0;

  if (ok)
  {
    for (int i = 0; i < impl->extern_count; ++i)
    {
      const Symbol *s = &impl->externs[i];
      if (!s->name || function_has_definition(impl, s->name))
        continue;
      if (fprintf(out, ".extern %s params=(", s->name) < 0)
      {
        ok = 0;
        break;
      }
      for (int pi = 0; pi < s->sig.param_count; ++pi)
      {
        if (pi > 0 && fputc(',', out) == EOF)
        {
          ok = 0;
          break;
        }
        const char *pt = "i32";
        if (s->sig.params && s->sig.params[pi])
        {
          TypeKind k = s->sig.params[pi]->kind;
          if (k == TY_VOID)
            pt = "void";
          else if (k == TY_I8)
            pt = "i8";
          else if (k == TY_U8)
            pt = "u8";
          else if (k == TY_I16)
            pt = "i16";
          else if (k == TY_U16)
            pt = "u16";
          else if (k == TY_I32)
            pt = "i32";
          else if (k == TY_U32)
            pt = "u32";
          else if (k == TY_I64)
            pt = "i64";
          else if (k == TY_U64)
            pt = "u64";
          else if (k == TY_F32)
            pt = "f32";
          else if (k == TY_F64)
            pt = "f64";
          else if (k == TY_BOOL)
            pt = "i1";
          else if (k == TY_PTR)
            pt = "ptr";
        }
        if (fputs(pt, out) < 0)
        {
          ok = 0;
          break;
        }
      }
      if (!ok)
        break;
      const char *ret = "i32";
      if (s->sig.ret)
      {
        TypeKind k = s->sig.ret->kind;
        if (k == TY_VOID)
          ret = "void";
        else if (k == TY_I8)
          ret = "i8";
        else if (k == TY_U8)
          ret = "u8";
        else if (k == TY_I16)
          ret = "i16";
        else if (k == TY_U16)
          ret = "u16";
        else if (k == TY_I32)
          ret = "i32";
        else if (k == TY_U32)
          ret = "u32";
        else if (k == TY_I64)
          ret = "i64";
        else if (k == TY_U64)
          ret = "u64";
        else if (k == TY_F32)
          ret = "f32";
        else if (k == TY_F64)
          ret = "f64";
        else if (k == TY_BOOL)
          ret = "i1";
        else if (k == TY_PTR)
          ret = "ptr";
      }
      if (fprintf(out, ") returns=%s", ret) < 0)
      {
        ok = 0;
        break;
      }
      if (s->sig.is_varargs && fputs(" varargs", out) < 0)
      {
        ok = 0;
        break;
      }
      if (fputc('\n', out) == EOF)
      {
        ok = 0;
        break;
      }
      emitted_any_extern = 1;
      if (s->is_noreturn)
      {
        if (fprintf(out, ".no-return %s\n", s->name) < 0)
        {
          ok = 0;
          break;
        }
      }
    }
  }

  if (ok && emitted_any_extern)
  {
    if (fputc('\n', out) == EOF)
      ok = 0;
  }

  if (ok)
  {
    for (int i = 0; i < impl->function_count; ++i)
    {
      CFunction *fn = &impl->functions[i];
      if (!fn->has_body)
        continue;
      if (!c_emit_function(out, impl, fn))
      {
        ok = 0;
        break;
      }
    }
  }

  if (fclose(out) != 0)
    ok = 0;

  if (!ok)
  {
    remove(request->ccb_output_path);
    return 1;
  }
  return 0;
}

static void c_destroy_unit(FrontendUnit *unit)
{
  CFrontendUnit *impl = c_impl(unit);
  if (impl)
  {
    for (int i = 0; i < impl->function_count; ++i)
      cfunction_destroy(&impl->functions[i]);
    free(impl->functions);
    impl->functions = NULL;
    impl->function_count = 0;
    impl->function_cap = 0;

    destroy_externs(impl);

    for (int i = 0; i < impl->visited_header_count; ++i)
      free(impl->visited_headers[i]);
    free(impl->visited_headers);

    for (int i = 0; i < impl->macro_count; ++i)
      free(impl->macro_names[i]);
    free(impl->macro_names);
    free(impl->macro_values);

    free(impl);
  }
  free(unit);
}

static const ChanceFrontendOps c_frontend_ops = {
    .load_unit = c_load_unit,
    .discard_semantic_state = c_discard_semantic_state,
    .unit_ast = c_unit_ast,
    .unit_ast_const = c_unit_ast_const,
    .unit_symtab = c_unit_symtab,
    .unit_check = c_unit_check,
    .register_foreign_symbols = c_register_foreign_symbols,
    .unit_externs = c_unit_externs,
    .copy_imported_function_symbols = c_copy_imported_function_symbols,
    .copy_imported_global_symbols = c_copy_imported_global_symbols,
    .track_imported_function = c_track_imported_function,
    .supports_direct_ccb_emit = c_supports_direct_ccb_emit,
    .emit_ccb = c_emit_ccb,
    .destroy_unit = c_destroy_unit,
};

static const ChanceFrontend c_frontend = {
    .name = "c",
    .description = "C frontend (.c/.h), direct CCB emission",
    .supports_input_path = c_supports_input_path,
    .ops = &c_frontend_ops,
};

const ChanceFrontend *chance_frontend_c(void)
{
  return &c_frontend;
}
