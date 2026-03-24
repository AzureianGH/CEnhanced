#include "frontend_impl.h"

#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

typedef enum
{
  CINDER_TYPE_INVALID = 0,
  CINDER_TYPE_VOID,
  CINDER_TYPE_BOOL,
  CINDER_TYPE_BYTE,
  CINDER_TYPE_INT,
  CINDER_TYPE_FLOAT,
  CINDER_TYPE_DOUBLE,
} CinderTypeBase;

typedef struct
{
  CinderTypeBase base;
  int pointer_depth;
} CinderType;

typedef struct
{
  char *text;
} CinderAttr;

typedef struct CinderNode
{
  char *keyword;
  char *name;
  CinderAttr *attrs;
  int attr_count;
  struct CinderNode **children;
  int child_count;
  int has_block;
} CinderNode;

typedef struct
{
  CinderNode **items;
  int count;
  int cap;
} CinderNodeList;

typedef struct
{
  char *name;
  CinderType type;
} CinderParam;

typedef struct
{
  char *namespace_name;
  char *base_name;
  char *symbol_name;
  CinderType return_type;
  CinderParam *params;
  int param_count;
  int is_variadic;
  int is_extern;
  CinderNode *body;
} CinderMethod;

typedef struct
{
  char *namespace_name;
  CinderMethod *methods;
  int method_count;
  int method_cap;
} CinderNamespace;

typedef struct
{
  CinderNamespace *namespaces;
  int namespace_count;
  int namespace_cap;
  CinderNode **roots;
  int root_count;
  int parse_ok;
} CinderProgram;

typedef struct
{
  CinderProgram program;
} CinderFrontendUnit;

typedef struct
{
  char **items;
  int count;
  int cap;
} StringList;

typedef struct
{
  char *name;
  CinderType type;
  int local_index;
} CinderLocal;

typedef struct
{
  CinderLocal *locals;
  int local_count;
  int local_cap;
  StringList body;
  CinderMethod *method;
  CinderNamespace *ns;
  CinderProgram *program;
} EmitFunctionCtx;

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

static char *xstrdup0(const char *s)
{
  if (!s)
    return NULL;
  size_t n = strlen(s);
  char *copy = (char *)malloc(n + 1);
  if (!copy)
    return NULL;
  memcpy(copy, s, n + 1);
  return copy;
}

static char *trim_dup(const char *s)
{
  if (!s)
    return xstrdup0("");
  const char *b = s;
  while (*b && isspace((unsigned char)*b))
    ++b;
  const char *e = s + strlen(s);
  while (e > b && isspace((unsigned char)e[-1]))
    --e;
  size_t n = (size_t)(e - b);
  char *out = (char *)malloc(n + 1);
  if (!out)
    return NULL;
  memcpy(out, b, n);
  out[n] = '\0';
  return out;
}

static int string_list_appendf(StringList *list, const char *fmt, ...)
{
  if (!list || !fmt)
    return 0;
  va_list ap;
  va_start(ap, fmt);
  int needed = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  if (needed < 0)
    return 0;
  char *line = (char *)malloc((size_t)needed + 1);
  if (!line)
    return 0;
  va_start(ap, fmt);
  vsnprintf(line, (size_t)needed + 1, fmt, ap);
  va_end(ap);
  if (list->count == list->cap)
  {
    int next = list->cap ? list->cap * 2 : 16;
    char **grown = (char **)realloc(list->items, (size_t)next * sizeof(char *));
    if (!grown)
    {
      free(line);
      return 0;
    }
    list->items = grown;
    list->cap = next;
  }
  list->items[list->count++] = line;
  return 1;
}

static void string_list_destroy(StringList *list)
{
  if (!list)
    return;
  for (int i = 0; i < list->count; ++i)
    free(list->items[i]);
  free(list->items);
  list->items = NULL;
  list->count = 0;
  list->cap = 0;
}

static int node_list_push(CinderNodeList *list, CinderNode *node)
{
  if (!list || !node)
    return 0;
  if (list->count == list->cap)
  {
    int next = list->cap ? list->cap * 2 : 8;
    CinderNode **grown =
        (CinderNode **)realloc(list->items, (size_t)next * sizeof(CinderNode *));
    if (!grown)
      return 0;
    list->items = grown;
    list->cap = next;
  }
  list->items[list->count++] = node;
  return 1;
}

static void cinder_node_free(CinderNode *node)
{
  if (!node)
    return;
  free(node->keyword);
  free(node->name);
  for (int i = 0; i < node->attr_count; ++i)
    free(node->attrs[i].text);
  free(node->attrs);
  if (node->children)
  {
    for (int i = 0; i < node->child_count; ++i)
      cinder_node_free(node->children[i]);
    free(node->children);
  }
  free(node);
}

static void cinder_program_destroy(CinderProgram *program)
{
  if (!program)
    return;
  if (program->roots)
  {
    for (int i = 0; i < program->root_count; ++i)
      cinder_node_free(program->roots[i]);
    free(program->roots);
  }
  for (int ni = 0; ni < program->namespace_count; ++ni)
  {
    CinderNamespace *ns = &program->namespaces[ni];
    free(ns->namespace_name);
    for (int mi = 0; mi < ns->method_count; ++mi)
    {
      CinderMethod *m = &ns->methods[mi];
      free(m->namespace_name);
      free(m->base_name);
      free(m->symbol_name);
      for (int pi = 0; pi < m->param_count; ++pi)
        free(m->params[pi].name);
      free(m->params);
    }
    free(ns->methods);
  }
  free(program->namespaces);
  memset(program, 0, sizeof(*program));
}

static int is_valid_attr(const char *kw)
{
  if (!kw)
    return 0;
  return strcmp(kw, "extern") == 0 || strcmp(kw, "variadic") == 0;
}

static int is_cinder_type_keyword(const char *kw)
{
  if (!kw)
    return 0;
  char *trim = trim_dup(kw);
  if (!trim)
    return 0;
  size_t n = strlen(trim);
  while (n > 0 && trim[n - 1] == '*')
    trim[--n] = '\0';
  while (n > 0 && isspace((unsigned char)trim[n - 1]))
    trim[--n] = '\0';
  int ok = strcmp(trim, "int") == 0 || strcmp(trim, "float") == 0 ||
           strcmp(trim, "double") == 0 || strcmp(trim, "void") == 0 ||
           strcmp(trim, "byte") == 0 || strcmp(trim, "bool") == 0;
  free(trim);
  return ok;
}

static char *normalize_source(const char *src)
{
  if (!src)
    return xstrdup0("");
  size_t n = strlen(src);
  char *out = (char *)malloc(n + 1);
  if (!out)
    return NULL;
  size_t j = 0;
  int in_string = 0;
  char quote = '\0';
  for (size_t i = 0; i < n; ++i)
  {
    char c = src[i];
    if (!in_string && c == '/' && (i + 1) < n && src[i + 1] == '/')
    {
      while (i < n && src[i] != '\n')
        ++i;
      if (i < n)
        out[j++] = ' ';
      continue;
    }
    if (c == '"' || c == '\'')
    {
      if (!in_string)
      {
        in_string = 1;
        quote = c;
      }
      else if (quote == c && (i == 0 || src[i - 1] != '\\'))
      {
        in_string = 0;
      }
    }
    if (c == '\n' || c == '\r' || c == '\t')
      c = ' ';
    out[j++] = c;
  }
  out[j] = '\0';
  return out;
}

static CinderNode *cinder_node_new(const char *keyword, const char *name,
                                   CinderAttr *attrs, int attr_count,
                                   int has_block)
{
  CinderNode *node = (CinderNode *)calloc(1, sizeof(CinderNode));
  if (!node)
    return NULL;
  node->keyword = trim_dup(keyword ? keyword : "");
  node->name = trim_dup(name ? name : "");
  node->attrs = attrs;
  node->attr_count = attr_count;
  node->has_block = has_block;
  if (!node->keyword || !node->name)
  {
    cinder_node_free(node);
    return NULL;
  }
  return node;
}

static int cinder_parse_nodes(const char *code, CinderNodeList *out);

static int cinder_node_attach_children_from_text(CinderNode *node,
                                                 const char *body)
{
  if (!node)
    return 0;
  CinderNodeList list = {0};
  if (!cinder_parse_nodes(body ? body : "", &list))
    return 0;
  node->children = list.items;
  node->child_count = list.count;
  return 1;
}

static int cinder_push_attr(CinderAttr **attrs, int *count, int *cap,
                            const char *text)
{
  if (!attrs || !count || !cap || !text)
    return 0;
  if (*count == *cap)
  {
    int next = *cap ? *cap * 2 : 4;
    CinderAttr *grown = (CinderAttr *)realloc(*attrs, (size_t)next * sizeof(CinderAttr));
    if (!grown)
      return 0;
    *attrs = grown;
    *cap = next;
  }
  char *copy = trim_dup(text);
  if (!copy)
    return 0;
  (*attrs)[*count].text = copy;
  (*count)++;
  return 1;
}

static void cinder_attrs_destroy(CinderAttr *attrs, int count)
{
  if (!attrs)
    return;
  for (int i = 0; i < count; ++i)
    free(attrs[i].text);
  free(attrs);
}

static int cinder_finish_statement(CinderNodeList *out, const char *keyword,
                                   const char *name, CinderAttr *attrs,
                                   int attr_count)
{
  char *kw = trim_dup(keyword);
  char *nm = trim_dup(name);
  if (!kw || !nm)
  {
    free(kw);
    free(nm);
    cinder_attrs_destroy(attrs, attr_count);
    return 0;
  }

  CinderNode *node = NULL;
  if (strcmp(kw, "if") == 0)
  {
    node = cinder_node_new("if", nm, attrs, attr_count, 1);
    if (!node)
    {
      free(kw);
      free(nm);
      cinder_attrs_destroy(attrs, attr_count);
      return 0;
    }
    const char *scan = nm;
    while (*scan && isspace((unsigned char)*scan))
      ++scan;
    if (*scan != '(')
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
    int depth = 0;
    const char *end = NULL;
    for (const char *p = scan; *p; ++p)
    {
      if (*p == '(')
        ++depth;
      else if (*p == ')')
      {
        --depth;
        if (depth == 0)
        {
          end = p;
          break;
        }
      }
    }
    if (!end)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
    size_t cond_n = (size_t)(end - scan - 1);
    char *cond = (char *)malloc(cond_n + 1);
    if (!cond)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
    memcpy(cond, scan + 1, cond_n);
    cond[cond_n] = '\0';
    free(node->name);
    node->name = trim_dup(cond);
    free(cond);

    const char *tail = end + 1;
    while (*tail && isspace((unsigned char)*tail))
      ++tail;
    char *nested = (char *)malloc(strlen(tail) + 2);
    if (!nested)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
    strcpy(nested, tail);
    strcat(nested, ";");
    int ok = cinder_node_attach_children_from_text(node, nested);
    free(nested);
    if (!ok)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
  }
  else if (strcmp(kw, "else") == 0)
  {
    node = cinder_node_new("else", "", attrs, attr_count, 1);
    if (!node)
    {
      free(kw);
      free(nm);
      cinder_attrs_destroy(attrs, attr_count);
      return 0;
    }
    char *nested = (char *)malloc(strlen(nm) + 2);
    if (!nested)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
    strcpy(nested, nm);
    strcat(nested, ";");
    int ok = cinder_node_attach_children_from_text(node, nested);
    free(nested);
    if (!ok)
    {
      cinder_node_free(node);
      free(kw);
      free(nm);
      return 0;
    }
  }
  else if (strcmp(kw, "return") == 0)
  {
    if (attr_count != 0)
    {
      free(kw);
      free(nm);
      cinder_attrs_destroy(attrs, attr_count);
      return 0;
    }
    node = cinder_node_new("return", nm, attrs, attr_count, 0);
  }
  else if (strcmp(kw, "def") == 0)
  {
    node = cinder_node_new("def", nm, attrs, attr_count, 0);
  }
  else if (is_cinder_type_keyword(kw))
  {
    node = cinder_node_new(kw, nm, attrs, attr_count, 0);
  }
  else
  {
    char *expr = NULL;
    if (kw[0] && nm[0])
    {
      size_t expr_n = strlen(kw) + 1 + strlen(nm);
      expr = (char *)malloc(expr_n + 1);
      if (!expr)
      {
        free(kw);
        free(nm);
        cinder_attrs_destroy(attrs, attr_count);
        return 0;
      }
      snprintf(expr, expr_n + 1, "%s %s", kw, nm);
    }
    else if (kw[0])
    {
      expr = xstrdup0(kw);
    }
    else
    {
      expr = xstrdup0(nm);
    }
    if (!expr)
    {
      free(kw);
      free(nm);
      cinder_attrs_destroy(attrs, attr_count);
      return 0;
    }
    node = cinder_node_new("raw", expr, attrs, attr_count, 0);
    free(expr);
  }

  free(kw);
  free(nm);
  if (!node)
  {
    cinder_attrs_destroy(attrs, attr_count);
    return 0;
  }
  return node_list_push(out, node);
}

static int cinder_finish_container(CinderNodeList *out, const char *keyword,
                                   const char *name, const char *buffer,
                                   CinderAttr *attrs, int attr_count)
{
  char *kw = trim_dup(keyword);
  char *nm = trim_dup(name);
  char *body = trim_dup(buffer ? buffer : "");
  if (!kw || !nm || !body)
  {
    free(kw);
    free(nm);
    free(body);
    cinder_attrs_destroy(attrs, attr_count);
    return 0;
  }

  CinderNode *node = NULL;
  if (strcmp(kw, "namespace") == 0)
  {
    if (attr_count != 0)
      goto fail;
    node = cinder_node_new("namespace", nm, attrs, attr_count, 1);
    if (!node || !cinder_node_attach_children_from_text(node, body))
      goto fail;
  }
  else if (strcmp(kw, "def") == 0)
  {
    node = cinder_node_new("def", nm, attrs, attr_count, 1);
    if (!node || !cinder_node_attach_children_from_text(node, body))
      goto fail;
  }
  else if (strcmp(kw, "if") == 0)
  {
    if (attr_count != 0)
      goto fail;
    const char *scan = nm;
    while (*scan && isspace((unsigned char)*scan))
      ++scan;
    if (*scan != '(')
      goto fail;
    const char *end = nm + strlen(nm);
    while (end > scan && isspace((unsigned char)end[-1]))
      --end;
    if (end <= scan || end[-1] != ')')
      goto fail;
    size_t cond_n = (size_t)(end - scan - 2);
    char *cond = (char *)malloc(cond_n + 1);
    if (!cond)
      goto fail;
    memcpy(cond, scan + 1, cond_n);
    cond[cond_n] = '\0';
    node = cinder_node_new("if", cond, attrs, attr_count, 1);
    free(cond);
    if (!node || !cinder_node_attach_children_from_text(node, body))
      goto fail;
  }
  else if (strcmp(kw, "else") == 0)
  {
    if (attr_count != 0)
      goto fail;
    node = cinder_node_new("else", "", attrs, attr_count, 1);
    if (!node)
      goto fail;
    if (nm[0])
    {
      size_t nested_n = strlen(nm) + strlen(body) + 4;
      char *nested = (char *)malloc(nested_n + 1);
      if (!nested)
        goto fail;
      snprintf(nested, nested_n + 1, "%s {%s}", nm, body);
      if (!cinder_node_attach_children_from_text(node, nested))
      {
        free(nested);
        goto fail;
      }
      free(nested);
    }
    else if (!cinder_node_attach_children_from_text(node, body))
    {
      goto fail;
    }
  }
  else
  {
    goto fail;
  }

  free(kw);
  free(nm);
  free(body);
  return node_list_push(out, node);

fail:
  cinder_node_free(node);
  cinder_attrs_destroy(attrs, attr_count);
  free(kw);
  free(nm);
  free(body);
  return 0;
}

static int cinder_parse_nodes(const char *code, CinderNodeList *out)
{
  if (!out)
    return 0;

  char *keyword = xstrdup0("");
  char *name = xstrdup0("");
  char *buffer = xstrdup0("");
  CinderAttr *attrs = NULL;
  int attr_count = 0;
  int attr_cap = 0;

  if (!keyword || !name || !buffer)
    goto oom;

  int in_string = 0;
  char quote = '\0';
  int char_len = 0;
  int depth = 0;

  for (const char *p = code ? code : ""; *p; ++p)
  {
    char c = *p;
    if (c == '"' || c == '\'')
    {
      if (!in_string)
      {
        in_string = 1;
        quote = c;
        char_len = 0;
      }
      else if (quote == c && (p == code || p[-1] != '\\'))
      {
        if (quote == '\'' && char_len != 2)
          goto fail;
        in_string = 0;
      }
    }
    if (in_string)
      ++char_len;

    if (c == '{' && !in_string)
    {
      if (depth > 0)
      {
        size_t bl = strlen(buffer);
        char *grown = (char *)realloc(buffer, bl + 2);
        if (!grown)
          goto oom;
        buffer = grown;
        buffer[bl] = c;
        buffer[bl + 1] = '\0';
      }
      ++depth;
      continue;
    }

    if (c == '}' && !in_string)
    {
      --depth;
      if (depth < 0)
        goto fail;
      if (depth > 0)
      {
        size_t bl = strlen(buffer);
        char *grown = (char *)realloc(buffer, bl + 2);
        if (!grown)
          goto oom;
        buffer = grown;
        buffer[bl] = c;
        buffer[bl + 1] = '\0';
      }
      if (depth == 0)
      {
        CinderAttr *carry_attrs = attrs;
        int carry_count = attr_count;
        attrs = NULL;
        attr_count = 0;
        attr_cap = 0;
        if (!cinder_finish_container(out, keyword, name, buffer, carry_attrs,
                                     carry_count))
          goto fail;
        free(keyword);
        free(name);
        free(buffer);
        keyword = xstrdup0("");
        name = xstrdup0("");
        buffer = xstrdup0("");
        if (!keyword || !name || !buffer)
          goto oom;
      }
      continue;
    }

    if (depth == 0 && c == ';' && !in_string)
    {
      CinderAttr *carry_attrs = attrs;
      int carry_count = attr_count;
      attrs = NULL;
      attr_count = 0;
      attr_cap = 0;
      if (!cinder_finish_statement(out, keyword, name, carry_attrs, carry_count))
        goto fail;
      free(keyword);
      free(name);
      free(buffer);
      keyword = xstrdup0("");
      name = xstrdup0("");
      buffer = xstrdup0("");
      if (!keyword || !name || !buffer)
        goto oom;
      continue;
    }

    if (depth > 0)
    {
      size_t bl = strlen(buffer);
      char *grown = (char *)realloc(buffer, bl + 2);
      if (!grown)
        goto oom;
      buffer = grown;
      buffer[bl] = c;
      buffer[bl + 1] = '\0';
      continue;
    }

    char *trim_kw = trim_dup(keyword);
    if (!trim_kw)
      goto oom;
    int kw_has_space = 0;
    size_t kw_len = strlen(keyword);
    if (kw_len > 0 && isspace((unsigned char)keyword[kw_len - 1]) && trim_kw[0])
      kw_has_space = 1;

    if (kw_has_space)
    {
      if (is_valid_attr(trim_kw))
      {
        if (!cinder_push_attr(&attrs, &attr_count, &attr_cap, trim_kw))
        {
          free(trim_kw);
          goto oom;
        }
        free(keyword);
        keyword = (char *)malloc(2);
        if (!keyword)
        {
          free(trim_kw);
          goto oom;
        }
        keyword[0] = c;
        keyword[1] = '\0';
      }
      else
      {
        size_t nl = strlen(name);
        char *grown = (char *)realloc(name, nl + 2);
        if (!grown)
        {
          free(trim_kw);
          goto oom;
        }
        name = grown;
        name[nl] = c;
        name[nl + 1] = '\0';
      }
    }
    else
    {
      size_t kl = strlen(keyword);
      char *grown = (char *)realloc(keyword, kl + 2);
      if (!grown)
      {
        free(trim_kw);
        goto oom;
      }
      keyword = grown;
      keyword[kl] = c;
      keyword[kl + 1] = '\0';
    }
    free(trim_kw);
  }

  if (depth != 0)
    goto fail;

  free(keyword);
  free(name);
  free(buffer);
  cinder_attrs_destroy(attrs, attr_count);
  return 1;

oom:
fail:
  free(keyword);
  free(name);
  free(buffer);
  cinder_attrs_destroy(attrs, attr_count);
  return 0;
}

static int cinder_parse_type(const char *text, CinderType *out)
{
  if (!text || !out)
    return 0;
  char *trim = trim_dup(text);
  if (!trim)
    return 0;
  CinderType ty = {0};
  while (trim[0])
  {
    size_t n = strlen(trim);
    if (n == 0)
      break;
    if (trim[n - 1] != '*')
      break;
    trim[n - 1] = '\0';
    while (n > 1 && isspace((unsigned char)trim[n - 2]))
    {
      trim[n - 2] = '\0';
      --n;
    }
    ty.pointer_depth++;
  }
  if (strcmp(trim, "int") == 0)
    ty.base = CINDER_TYPE_INT;
  else if (strcmp(trim, "float") == 0)
    ty.base = CINDER_TYPE_FLOAT;
  else if (strcmp(trim, "double") == 0)
    ty.base = CINDER_TYPE_DOUBLE;
  else if (strcmp(trim, "void") == 0)
    ty.base = CINDER_TYPE_VOID;
  else if (strcmp(trim, "byte") == 0)
    ty.base = CINDER_TYPE_BYTE;
  else if (strcmp(trim, "bool") == 0)
    ty.base = CINDER_TYPE_BOOL;
  else
  {
    free(trim);
    return 0;
  }
  free(trim);
  *out = ty;
  return 1;
}

static const char *cinder_type_to_cc(CinderType ty)
{
  if (ty.pointer_depth > 0)
    return "ptr";
  switch (ty.base)
  {
  case CINDER_TYPE_VOID:
    return "void";
  case CINDER_TYPE_BOOL:
    return "i1";
  case CINDER_TYPE_BYTE:
    return "i8";
  case CINDER_TYPE_INT:
    return "i32";
  case CINDER_TYPE_FLOAT:
    return "f32";
  case CINDER_TYPE_DOUBLE:
    return "f64";
  default:
    return "void";
  }
}

static int cinder_type_equals(CinderType a, CinderType b)
{
  return a.base == b.base && a.pointer_depth == b.pointer_depth;
}

static int cinder_parse_func_sig(const char *sig_text, CinderMethod *method,
                                 int mangle)
{
  if (!sig_text || !method)
    return 0;

  char *sig = trim_dup(sig_text);
  if (!sig)
    return 0;

  char *open = strchr(sig, '(');
  if (!open)
  {
    free(sig);
    return 0;
  }

  char *close = NULL;
  int depth = 0;
  for (char *p = open; *p; ++p)
  {
    if (*p == '(')
      ++depth;
    else if (*p == ')')
    {
      --depth;
      if (depth == 0)
      {
        close = p;
        break;
      }
    }
  }
  if (!close)
  {
    free(sig);
    return 0;
  }

  *open = '\0';
  char *raw_name = trim_dup(sig);
  if (!raw_name)
  {
    free(sig);
    return 0;
  }

  char *args_raw = NULL;
  if (close > open + 1)
  {
    size_t n = (size_t)(close - (open + 1));
    args_raw = (char *)malloc(n + 1);
    if (!args_raw)
    {
      free(raw_name);
      free(sig);
      return 0;
    }
    memcpy(args_raw, open + 1, n);
    args_raw[n] = '\0';
  }
  else
  {
    args_raw = xstrdup0("");
  }

  CinderType ret = {.base = CINDER_TYPE_VOID, .pointer_depth = 0};
  char *tail = close + 1;
  while (*tail && isspace((unsigned char)*tail))
    ++tail;
  if (*tail == ':')
  {
    ++tail;
    if (!cinder_parse_type(tail, &ret))
    {
      free(args_raw);
      free(raw_name);
      free(sig);
      return 0;
    }
  }

  CinderParam *params = NULL;
  int param_count = 0;
  int param_cap = 0;
  char *cursor = args_raw;
  while (*cursor)
  {
    while (*cursor && isspace((unsigned char)*cursor))
      ++cursor;
    if (!*cursor)
      break;
    char *comma = cursor;
    int pdepth = 0;
    while (*comma)
    {
      if (*comma == '(')
        ++pdepth;
      else if (*comma == ')')
        --pdepth;
      else if (*comma == ',' && pdepth == 0)
        break;
      ++comma;
    }
    char saved = *comma;
    *comma = '\0';
    char *pair = trim_dup(cursor);
    if (!pair)
    {
      free(args_raw);
      free(raw_name);
      free(sig);
      free(params);
      return 0;
    }

    char *last_space = strrchr(pair, ' ');
    if (!last_space)
    {
      free(pair);
      free(args_raw);
      free(raw_name);
      free(sig);
      free(params);
      return 0;
    }
    *last_space = '\0';
    char *ty_text = trim_dup(pair);
    char *name_text = trim_dup(last_space + 1);
    free(pair);
    if (!ty_text || !name_text)
    {
      free(ty_text);
      free(name_text);
      free(args_raw);
      free(raw_name);
      free(sig);
      free(params);
      return 0;
    }

    CinderType pty;
    if (!cinder_parse_type(ty_text, &pty))
    {
      free(ty_text);
      free(name_text);
      free(args_raw);
      free(raw_name);
      free(sig);
      free(params);
      return 0;
    }
    free(ty_text);

    if (param_count == param_cap)
    {
      int next = param_cap ? param_cap * 2 : 4;
      CinderParam *grown =
          (CinderParam *)realloc(params, (size_t)next * sizeof(CinderParam));
      if (!grown)
      {
        free(name_text);
        free(args_raw);
        free(raw_name);
        free(sig);
        free(params);
        return 0;
      }
      params = grown;
      param_cap = next;
    }
    params[param_count].name = name_text;
    params[param_count].type = pty;
    ++param_count;

    *comma = saved;
    cursor = (*comma == ',') ? comma + 1 : comma;
  }

  method->base_name = raw_name;
  method->params = params;
  method->param_count = param_count;
  method->return_type = ret;

  if (!mangle || (strcmp(raw_name, "main") == 0 &&
                  ret.pointer_depth == 0 && ret.base == CINDER_TYPE_INT))
  {
    method->symbol_name = xstrdup0(raw_name);
  }
  else
  {
    StringList name_parts = {0};
    if (method->namespace_name && method->namespace_name[0])
      string_list_appendf(&name_parts, "%s", method->namespace_name);
    string_list_appendf(&name_parts, "%s", cinder_type_to_cc(ret));
    string_list_appendf(&name_parts, "%s", raw_name);
    for (int i = 0; i < param_count; ++i)
      string_list_appendf(&name_parts, "%s", cinder_type_to_cc(params[i].type));

    size_t total = 0;
    for (int i = 0; i < name_parts.count; ++i)
      total += strlen(name_parts.items[i]) + 1;
    char *mangled = (char *)malloc(total + 1);
    if (!mangled)
    {
      string_list_destroy(&name_parts);
      free(args_raw);
      free(sig);
      return 0;
    }
    mangled[0] = '\0';
    for (int i = 0; i < name_parts.count; ++i)
    {
      if (i)
        strcat(mangled, ".");
      strcat(mangled, name_parts.items[i]);
    }
    method->symbol_name = mangled;
    string_list_destroy(&name_parts);
  }

  free(args_raw);
  free(sig);
  return method->symbol_name != NULL;
}

static CinderNamespace *cinder_program_find_namespace(CinderProgram *program,
                                                      const char *name)
{
  if (!program || !name)
    return NULL;
  for (int i = 0; i < program->namespace_count; ++i)
  {
    if (strcmp(program->namespaces[i].namespace_name, name) == 0)
      return &program->namespaces[i];
  }
  return NULL;
}

static CinderNamespace *cinder_program_add_namespace(CinderProgram *program,
                                                     const char *name)
{
  if (!program || !name)
    return NULL;
  CinderNamespace *existing = cinder_program_find_namespace(program, name);
  if (existing)
    return existing;

  if (program->namespace_count == program->namespace_cap)
  {
    int next = program->namespace_cap ? program->namespace_cap * 2 : 4;
    CinderNamespace *grown = (CinderNamespace *)realloc(
        program->namespaces, (size_t)next * sizeof(CinderNamespace));
    if (!grown)
      return NULL;
    program->namespaces = grown;
    program->namespace_cap = next;
  }
  CinderNamespace *ns = &program->namespaces[program->namespace_count++];
  memset(ns, 0, sizeof(*ns));
  ns->namespace_name = xstrdup0(name);
  if (!ns->namespace_name)
    return NULL;
  return ns;
}

static int cinder_namespace_add_method(CinderNamespace *ns,
                                       const CinderMethod *method)
{
  if (!ns || !method)
    return 0;
  if (ns->method_count == ns->method_cap)
  {
    int next = ns->method_cap ? ns->method_cap * 2 : 8;
    CinderMethod *grown =
        (CinderMethod *)realloc(ns->methods, (size_t)next * sizeof(CinderMethod));
    if (!grown)
      return 0;
    ns->methods = grown;
    ns->method_cap = next;
  }
  ns->methods[ns->method_count++] = *method;
  return 1;
}

static int cinder_collect_methods_in_namespace(CinderProgram *program,
                                               const CinderNode *ns_node)
{
  if (!program || !ns_node || strcmp(ns_node->keyword, "namespace") != 0)
    return 0;
  CinderNamespace *ns =
      cinder_program_add_namespace(program, ns_node->name ? ns_node->name : "");
  if (!ns)
    return 0;

  for (int i = 0; i < ns_node->child_count; ++i)
  {
    CinderNode *child = ns_node->children[i];
    if (!child)
      continue;
    if (strcmp(child->keyword, "def") != 0)
      continue;

    int is_extern = 0;
    int is_variadic = 0;
    for (int ai = 0; ai < child->attr_count; ++ai)
    {
      if (strcmp(child->attrs[ai].text, "extern") == 0)
        is_extern = 1;
      else if (strcmp(child->attrs[ai].text, "variadic") == 0)
        is_variadic = 1;
      else
        return 0;
    }

    CinderMethod method;
    memset(&method, 0, sizeof(method));
    method.namespace_name = xstrdup0(ns->namespace_name);
    method.is_extern = is_extern;
    method.is_variadic = is_variadic;
    method.body = child->has_block ? child : NULL;

    if (!method.namespace_name ||
        !cinder_parse_func_sig(child->name, &method, !is_extern))
    {
      fprintf(stderr,
              "cinder frontend: invalid method signature '%s' in namespace '%s'\n",
              child->name ? child->name : "", ns->namespace_name);
      free(method.namespace_name);
      free(method.base_name);
      free(method.symbol_name);
      for (int pi = 0; pi < method.param_count; ++pi)
        free(method.params[pi].name);
      free(method.params);
      return 0;
    }

    if (!is_extern && !method.body)
      return 0;
    if (is_extern && method.body)
      return 0;

    if (!cinder_namespace_add_method(ns, &method))
      return 0;
  }

  return 1;
}

static int cinder_parse_program(const char *source, CinderProgram *program)
{
  if (!program)
    return 0;
  memset(program, 0, sizeof(*program));

  char *norm = normalize_source(source);
  if (!norm)
    return 0;

  CinderNodeList roots = {0};
  int ok = cinder_parse_nodes(norm, &roots);
  free(norm);
  if (!ok)
  {
    fprintf(stderr, "cinder frontend: failed while parsing top-level nodes\n");
    for (int i = 0; i < roots.count; ++i)
      cinder_node_free(roots.items[i]);
    free(roots.items);
    return 0;
  }

  for (int i = 0; i < roots.count; ++i)
  {
    CinderNode *node = roots.items[i];
    if (!node || strcmp(node->keyword, "namespace") != 0)
      continue;
    if (!cinder_collect_methods_in_namespace(program, node))
    {
      fprintf(stderr, "cinder frontend: failed collecting namespace '%s'\n",
              node->name ? node->name : "");
      for (int j = 0; j < roots.count; ++j)
        cinder_node_free(roots.items[j]);
      free(roots.items);
      cinder_program_destroy(program);
      return 0;
    }
  }

  program->roots = roots.items;
  program->root_count = roots.count;

  program->parse_ok = 1;
  return 1;
}

static CinderMethod *cinder_find_method(CinderProgram *program,
                                        const char *namespace_name,
                                        const char *name,
                                        CinderType *arg_types,
                                        int arg_count,
                                        const CinderType *expected_ret)
{
  if (!program || !name)
    return NULL;

  CinderMethod *best = NULL;
  for (int ni = 0; ni < program->namespace_count; ++ni)
  {
    CinderNamespace *ns = &program->namespaces[ni];
    for (int mi = 0; mi < ns->method_count; ++mi)
    {
      CinderMethod *m = &ns->methods[mi];
      if (strcmp(m->base_name, name) != 0 && strcmp(m->symbol_name, name) != 0)
        continue;
      if (!m->is_variadic)
      {
        if (m->param_count != arg_count)
          continue;
      }
      else
      {
        if (arg_count < m->param_count)
          continue;
      }

      int types_ok = 1;
      int cmp_count = m->is_variadic ? m->param_count : arg_count;
      for (int ai = 0; ai < cmp_count; ++ai)
      {
        if (!cinder_type_equals(m->params[ai].type, arg_types[ai]))
        {
          types_ok = 0;
          break;
        }
      }
      if (!types_ok)
        continue;

      if (expected_ret && !cinder_type_equals(m->return_type, *expected_ret))
        continue;

      if (!best)
      {
        best = m;
        continue;
      }
      if (namespace_name && namespace_name[0] &&
          strcmp(m->namespace_name, namespace_name) == 0)
      {
        best = m;
      }
    }
  }
  return best;
}

static int emit_lookup_local(EmitFunctionCtx *ctx, const char *name,
                             CinderLocal **out)
{
  if (!ctx || !name || !out)
    return 0;
  for (int i = ctx->local_count - 1; i >= 0; --i)
  {
    if (strcmp(ctx->locals[i].name, name) == 0)
    {
      *out = &ctx->locals[i];
      return 1;
    }
  }
  *out = NULL;
  return 0;
}

static int emit_add_local(EmitFunctionCtx *ctx, const char *name, CinderType ty,
                          int *out_index)
{
  if (!ctx || !name)
    return 0;
  if (ctx->local_count == ctx->local_cap)
  {
    int next = ctx->local_cap ? ctx->local_cap * 2 : 16;
    CinderLocal *grown =
        (CinderLocal *)realloc(ctx->locals, (size_t)next * sizeof(CinderLocal));
    if (!grown)
      return 0;
    ctx->locals = grown;
    ctx->local_cap = next;
  }
  CinderLocal *local = &ctx->locals[ctx->local_count];
  memset(local, 0, sizeof(*local));
  local->name = xstrdup0(name);
  if (!local->name)
    return 0;
  local->type = ty;
  local->local_index = ctx->local_count;
  if (out_index)
    *out_index = local->local_index;
  ++ctx->local_count;
  return 1;
}

static int is_identifier_char(char c)
{
  return isalnum((unsigned char)c) || c == '_' || c == '.';
}

typedef enum
{
  TOK_EOF = 0,
  TOK_IDENT,
  TOK_INT,
  TOK_FLOAT,
  TOK_STRING,
  TOK_CHAR,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_COMMA,
  TOK_PLUS,
  TOK_MINUS,
  TOK_STAR,
  TOK_SLASH,
  TOK_PERCENT,
  TOK_AMP,
  TOK_BANG,
  TOK_EQEQ,
  TOK_NE,
  TOK_LT,
  TOK_LE,
  TOK_GT,
  TOK_GE,
} TokKind;

typedef struct
{
  TokKind kind;
  char *text;
} Tok;

typedef struct
{
  Tok *items;
  int count;
  int cap;
  int pos;
} TokList;

static void tok_list_destroy(TokList *list)
{
  if (!list)
    return;
  for (int i = 0; i < list->count; ++i)
    free(list->items[i].text);
  free(list->items);
  memset(list, 0, sizeof(*list));
}

static int tok_list_push(TokList *list, TokKind kind, const char *start,
                         size_t n)
{
  if (!list)
    return 0;
  if (list->count == list->cap)
  {
    int next = list->cap ? list->cap * 2 : 32;
    Tok *grown = (Tok *)realloc(list->items, (size_t)next * sizeof(Tok));
    if (!grown)
      return 0;
    list->items = grown;
    list->cap = next;
  }
  Tok *t = &list->items[list->count++];
  t->kind = kind;
  t->text = (char *)malloc(n + 1);
  if (!t->text)
    return 0;
  memcpy(t->text, start, n);
  t->text[n] = '\0';
  return 1;
}

static int tokenize_expr(const char *expr, TokList *out)
{
  if (!expr || !out)
    return 0;
  memset(out, 0, sizeof(*out));
  const char *p = expr;
  while (*p)
  {
    if (isspace((unsigned char)*p))
    {
      ++p;
      continue;
    }
    if (*p == '"')
    {
      const char *s = p++;
      while (*p && !(*p == '"' && p[-1] != '\\'))
        ++p;
      if (*p != '"')
        return 0;
      ++p;
      if (!tok_list_push(out, TOK_STRING, s, (size_t)(p - s)))
        return 0;
      continue;
    }
    if (*p == '\'')
    {
      const char *s = p++;
      while (*p && !(*p == '\'' && p[-1] != '\\'))
        ++p;
      if (*p != '\'')
        return 0;
      ++p;
      if (!tok_list_push(out, TOK_CHAR, s, (size_t)(p - s)))
        return 0;
      continue;
    }
    if (is_identifier_char(*p) && !isdigit((unsigned char)*p))
    {
      const char *s = p;
      while (is_identifier_char(*p))
        ++p;
      if (!tok_list_push(out, TOK_IDENT, s, (size_t)(p - s)))
        return 0;
      continue;
    }
    if (isdigit((unsigned char)*p))
    {
      const char *s = p;
      int has_dot = 0;
      while (isdigit((unsigned char)*p) || *p == '.')
      {
        if (*p == '.')
          has_dot = 1;
        ++p;
      }
      if (!tok_list_push(out, has_dot ? TOK_FLOAT : TOK_INT, s,
                         (size_t)(p - s)))
        return 0;
      continue;
    }
    if (*p == '=' && p[1] == '=')
    {
      if (!tok_list_push(out, TOK_EQEQ, p, 2))
        return 0;
      p += 2;
      continue;
    }
    if (*p == '!' && p[1] == '=')
    {
      if (!tok_list_push(out, TOK_NE, p, 2))
        return 0;
      p += 2;
      continue;
    }
    if (*p == '<' && p[1] == '=')
    {
      if (!tok_list_push(out, TOK_LE, p, 2))
        return 0;
      p += 2;
      continue;
    }
    if (*p == '>' && p[1] == '=')
    {
      if (!tok_list_push(out, TOK_GE, p, 2))
        return 0;
      p += 2;
      continue;
    }

    TokKind k = TOK_EOF;
    switch (*p)
    {
    case '(':
      k = TOK_LPAREN;
      break;
    case ')':
      k = TOK_RPAREN;
      break;
    case ',':
      k = TOK_COMMA;
      break;
    case '+':
      k = TOK_PLUS;
      break;
    case '-':
      k = TOK_MINUS;
      break;
    case '*':
      k = TOK_STAR;
      break;
    case '/':
      k = TOK_SLASH;
      break;
    case '%':
      k = TOK_PERCENT;
      break;
    case '&':
      k = TOK_AMP;
      break;
    case '!':
      k = TOK_BANG;
      break;
    case '<':
      k = TOK_LT;
      break;
    case '>':
      k = TOK_GT;
      break;
    default:
      return 0;
    }
    if (!tok_list_push(out, k, p, 1))
      return 0;
    ++p;
  }
  if (!tok_list_push(out, TOK_EOF, "", 0))
    return 0;
  return 1;
}

static Tok *tok_peek(TokList *list)
{
  if (!list || list->pos >= list->count)
    return NULL;
  return &list->items[list->pos];
}

static Tok *tok_next(TokList *list)
{
  Tok *t = tok_peek(list);
  if (t)
    list->pos++;
  return t;
}

static int tok_match(TokList *list, TokKind kind)
{
  Tok *t = tok_peek(list);
  if (!t || t->kind != kind)
    return 0;
  list->pos++;
  return 1;
}

static int emit_expr(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                     const CinderType *expected);
static int emit_block(EmitFunctionCtx *ctx, CinderNode **children, int child_count,
                      int *label_id);

static int emit_call_expr(EmitFunctionCtx *ctx, TokList *tl, const char *name,
                          CinderType *out_type, const CinderType *expected)
{
  CinderType arg_types_buf[64];
  CinderType *arg_types = arg_types_buf;
  int arg_count = 0;
  int arg_cap = 64;

  if (!tok_match(tl, TOK_LPAREN))
    return 0;

  if (!tok_match(tl, TOK_RPAREN))
  {
    while (1)
    {
      if (arg_count == arg_cap)
      {
        int next = arg_cap * 2;
        if (arg_types == arg_types_buf)
        {
          arg_types = (CinderType *)malloc((size_t)next * sizeof(CinderType));
          if (!arg_types)
            return 0;
          memcpy(arg_types, arg_types_buf, (size_t)arg_count * sizeof(CinderType));
        }
        else
        {
          CinderType *grown =
              (CinderType *)realloc(arg_types, (size_t)next * sizeof(CinderType));
          if (!grown)
          {
            free(arg_types);
            return 0;
          }
          arg_types = grown;
        }
        arg_cap = next;
      }

      if (!emit_expr(ctx, tl, &arg_types[arg_count], NULL))
      {
        if (arg_types != arg_types_buf)
          free(arg_types);
        return 0;
      }
      arg_count++;

      if (tok_match(tl, TOK_COMMA))
        continue;
      if (!tok_match(tl, TOK_RPAREN))
      {
        if (arg_types != arg_types_buf)
          free(arg_types);
        return 0;
      }
      break;
    }
  }

  CinderMethod *callee = cinder_find_method(ctx->program,
                                            ctx->ns->namespace_name,
                                            name,
                                            arg_types,
                                            arg_count,
                                            expected);
  if (!callee)
  {
    if (arg_types != arg_types_buf)
      free(arg_types);
    return 0;
  }

  StringList arg_sig = {0};
  for (int i = 0; i < arg_count; ++i)
    string_list_appendf(&arg_sig, "%s", cinder_type_to_cc(arg_types[i]));

  size_t joined_len = 2;
  for (int i = 0; i < arg_sig.count; ++i)
    joined_len += strlen(arg_sig.items[i]) + 1;
  char *joined = (char *)malloc(joined_len + 1);
  if (!joined)
  {
    string_list_destroy(&arg_sig);
    if (arg_types != arg_types_buf)
      free(arg_types);
    return 0;
  }
  strcpy(joined, "(");
  for (int i = 0; i < arg_sig.count; ++i)
  {
    if (i)
      strcat(joined, ",");
    strcat(joined, arg_sig.items[i]);
  }
  strcat(joined, ")");

  if (!string_list_appendf(&ctx->body, "  call %s %s %s%s", callee->symbol_name,
                           cinder_type_to_cc(callee->return_type), joined,
                           callee->is_variadic ? " varargs" : ""))
  {
    free(joined);
    string_list_destroy(&arg_sig);
    if (arg_types != arg_types_buf)
      free(arg_types);
    return 0;
  }
  free(joined);
  string_list_destroy(&arg_sig);

  *out_type = callee->return_type;
  if (arg_types != arg_types_buf)
    free(arg_types);
  return 1;
}

static int emit_primary(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                        const CinderType *expected)
{
  Tok *t = tok_next(tl);
  if (!t)
    return 0;

  if (t->kind == TOK_INT)
  {
    if (!string_list_appendf(&ctx->body, "  const i32 %s", t->text))
      return 0;
    out_type->base = CINDER_TYPE_INT;
    out_type->pointer_depth = 0;
    return 1;
  }
  if (t->kind == TOK_FLOAT)
  {
    if (!string_list_appendf(&ctx->body, "  const f64 %s", t->text))
      return 0;
    out_type->base = CINDER_TYPE_DOUBLE;
    out_type->pointer_depth = 0;
    return 1;
  }
  if (t->kind == TOK_STRING)
  {
    if (!string_list_appendf(&ctx->body, "  const_str %s", t->text))
      return 0;
    out_type->base = CINDER_TYPE_BYTE;
    out_type->pointer_depth = 1;
    return 1;
  }
  if (t->kind == TOK_CHAR)
  {
    unsigned ch = 0;
    if (strlen(t->text) >= 3)
      ch = (unsigned char)t->text[1];
    if (!string_list_appendf(&ctx->body, "  const i8 %u", ch))
      return 0;
    out_type->base = CINDER_TYPE_BYTE;
    out_type->pointer_depth = 0;
    return 1;
  }
  if (t->kind == TOK_IDENT)
  {
    if (strcmp(t->text, "true") == 0 || strcmp(t->text, "false") == 0)
    {
      if (!string_list_appendf(&ctx->body, "  const i1 %d",
                               strcmp(t->text, "true") == 0 ? 1 : 0))
        return 0;
      out_type->base = CINDER_TYPE_BOOL;
      out_type->pointer_depth = 0;
      return 1;
    }

    Tok *peek = tok_peek(tl);
    if (peek && peek->kind == TOK_LPAREN)
      return emit_call_expr(ctx, tl, t->text, out_type, expected);

    CinderLocal *local = NULL;
    if (!emit_lookup_local(ctx, t->text, &local) || !local)
      return 0;
    if (!string_list_appendf(&ctx->body, "  load_local %d", local->local_index))
      return 0;
    *out_type = local->type;
    return 1;
  }
  if (t->kind == TOK_LPAREN)
  {
    if (!emit_expr(ctx, tl, out_type, expected))
      return 0;
    if (!tok_match(tl, TOK_RPAREN))
      return 0;
    return 1;
  }
  return 0;
}

static int emit_unary(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                      const CinderType *expected)
{
  Tok *peek = tok_peek(tl);
  if (!peek)
    return 0;

  if (peek->kind == TOK_MINUS)
  {
    tok_next(tl);
    CinderType rhs;
    if (!emit_unary(ctx, tl, &rhs, expected))
      return 0;
    if (!string_list_appendf(&ctx->body, "  const %s 0", cinder_type_to_cc(rhs)))
      return 0;
    if (!string_list_appendf(&ctx->body, "  binop sub %s", cinder_type_to_cc(rhs)))
      return 0;
    *out_type = rhs;
    return 1;
  }
  if (peek->kind == TOK_BANG)
  {
    tok_next(tl);
    CinderType rhs;
    if (!emit_unary(ctx, tl, &rhs, expected))
      return 0;
    if (!string_list_appendf(&ctx->body, "  unop not i1"))
      return 0;
    out_type->base = CINDER_TYPE_BOOL;
    out_type->pointer_depth = 0;
    return 1;
  }
  if (peek->kind == TOK_AMP)
  {
    tok_next(tl);
    Tok *id = tok_next(tl);
    if (!id || id->kind != TOK_IDENT)
      return 0;
    CinderLocal *local = NULL;
    if (!emit_lookup_local(ctx, id->text, &local) || !local)
      return 0;
    if (!string_list_appendf(&ctx->body, "  addr_local %d", local->local_index))
      return 0;
    *out_type = local->type;
    out_type->pointer_depth++;
    return 1;
  }

  return emit_primary(ctx, tl, out_type, expected);
}

static int emit_muldiv(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                       const CinderType *expected)
{
  if (!emit_unary(ctx, tl, out_type, expected))
    return 0;
  while (1)
  {
    Tok *peek = tok_peek(tl);
    if (!peek)
      return 0;
    const char *op = NULL;
    if (peek->kind == TOK_STAR)
      op = "mul";
    else if (peek->kind == TOK_SLASH)
      op = "div";
    else if (peek->kind == TOK_PERCENT)
      op = "mod";
    else
      break;

    tok_next(tl);
    CinderType rhs;
    if (!emit_unary(ctx, tl, &rhs, expected))
      return 0;
    if (!cinder_type_equals(*out_type, rhs))
      return 0;
    if (!string_list_appendf(&ctx->body, "  binop %s %s", op,
                             cinder_type_to_cc(*out_type)))
      return 0;
  }
  return 1;
}

static int emit_addsub(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                       const CinderType *expected)
{
  if (!emit_muldiv(ctx, tl, out_type, expected))
    return 0;
  while (1)
  {
    Tok *peek = tok_peek(tl);
    if (!peek)
      return 0;
    const char *op = NULL;
    if (peek->kind == TOK_PLUS)
      op = "add";
    else if (peek->kind == TOK_MINUS)
      op = "sub";
    else
      break;

    tok_next(tl);
    CinderType rhs;
    if (!emit_muldiv(ctx, tl, &rhs, expected))
      return 0;
    if (!cinder_type_equals(*out_type, rhs))
      return 0;
    if (!string_list_appendf(&ctx->body, "  binop %s %s", op,
                             cinder_type_to_cc(*out_type)))
      return 0;
  }
  return 1;
}

static int emit_compare_expr(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                             const CinderType *expected)
{
  if (!emit_addsub(ctx, tl, out_type, expected))
    return 0;

  Tok *peek = tok_peek(tl);
  if (!peek)
    return 0;

  const char *cmp = NULL;
  if (peek->kind == TOK_EQEQ)
    cmp = "eq";
  else if (peek->kind == TOK_NE)
    cmp = "ne";
  else if (peek->kind == TOK_LT)
    cmp = "lt";
  else if (peek->kind == TOK_LE)
    cmp = "le";
  else if (peek->kind == TOK_GT)
    cmp = "gt";
  else if (peek->kind == TOK_GE)
    cmp = "ge";
  else
    return 1;

  tok_next(tl);
  CinderType rhs;
  if (!emit_addsub(ctx, tl, &rhs, expected))
    return 0;
  if (!cinder_type_equals(*out_type, rhs))
    return 0;
  if (!string_list_appendf(&ctx->body, "  compare %s %s", cmp,
                           cinder_type_to_cc(*out_type)))
    return 0;
  out_type->base = CINDER_TYPE_BOOL;
  out_type->pointer_depth = 0;
  return 1;
}

static int emit_expr(EmitFunctionCtx *ctx, TokList *tl, CinderType *out_type,
                     const CinderType *expected)
{
  return emit_compare_expr(ctx, tl, out_type, expected);
}

static int emit_expr_from_text(EmitFunctionCtx *ctx, const char *text,
                               CinderType *out_type,
                               const CinderType *expected)
{
  TokList tl;
  if (!tokenize_expr(text, &tl))
    return 0;
  int ok = emit_expr(ctx, &tl, out_type, expected);
  if (ok)
  {
    Tok *tail = tok_peek(&tl);
    if (!tail || tail->kind != TOK_EOF)
      ok = 0;
  }
  tok_list_destroy(&tl);
  return ok;
}

static int emit_statement(EmitFunctionCtx *ctx, CinderNode *node, int *label_id);

static int emit_main_stdout_flush(EmitFunctionCtx *ctx)
{
  if (!ctx || !ctx->method || !ctx->method->base_name)
    return 0;
  if (strcmp(ctx->method->base_name, "main") != 0)
    return 1;
  if (!string_list_appendf(&ctx->body, "  const ptr null"))
    return 0;
  if (!string_list_appendf(&ctx->body, "  call fflush i32 (ptr)"))
    return 0;
  if (!string_list_appendf(&ctx->body, "  drop i32"))
    return 0;
  return 1;
}

static int emit_if_with_optional_else(EmitFunctionCtx *ctx, CinderNode *if_node,
                                      CinderNode *else_node, int *label_id)
{
  if (!ctx || !if_node || !label_id)
    return 0;

  int id = (*label_id)++;
  char l_then[64];
  char l_else[64];
  char l_end[64];
  snprintf(l_then, sizeof(l_then), "Lcin_if_then_%d", id);
  snprintf(l_else, sizeof(l_else), "Lcin_if_else_%d", id);
  snprintf(l_end, sizeof(l_end), "Lcin_if_end_%d", id);

  CinderType cond_ty;
  CinderType expect_bool = {.base = CINDER_TYPE_BOOL, .pointer_depth = 0};
  if (!emit_expr_from_text(ctx, if_node->name, &cond_ty, &expect_bool))
    return 0;
  if (!string_list_appendf(&ctx->body, "  branch %s %s", l_then, l_else))
    return 0;
  if (!string_list_appendf(&ctx->body, "label %s", l_then))
    return 0;
  if (!emit_block(ctx, if_node->children, if_node->child_count, label_id))
    return 0;
  if (!string_list_appendf(&ctx->body, "  jump %s", l_end))
    return 0;
  if (!string_list_appendf(&ctx->body, "label %s", l_else))
    return 0;
  if (else_node)
  {
    if (!emit_block(ctx, else_node->children, else_node->child_count, label_id))
      return 0;
  }
  if (!string_list_appendf(&ctx->body, "  jump %s", l_end))
    return 0;
  if (!string_list_appendf(&ctx->body, "label %s", l_end))
    return 0;
  return 1;
}

static int emit_block(EmitFunctionCtx *ctx, CinderNode **children, int child_count,
                      int *label_id)
{
  for (int i = 0; i < child_count; ++i)
  {
    CinderNode *node = children[i];
    if (!node)
      continue;
    if (strcmp(node->keyword, "if") == 0)
    {
      CinderNode *else_node = NULL;
      if ((i + 1) < child_count && children[i + 1] &&
          strcmp(children[i + 1]->keyword, "else") == 0)
      {
        else_node = children[i + 1];
        ++i;
      }

      /*
       * Parser shape for `if (...) ... else if (...) {...} else ...` is:
       *   if-node, else-node(child: if-node), else-node
       * Fold that so the trailing else binds to the nested if.
       */
      if (else_node && else_node->child_count == 1 && else_node->children[0] &&
          strcmp(else_node->children[0]->keyword, "if") == 0 &&
          (i + 1) < child_count && children[i + 1] &&
          strcmp(children[i + 1]->keyword, "else") == 0)
      {
        CinderNode *nested_if = else_node->children[0];
        CinderNode *nested_else = children[i + 1];

        int id = (*label_id)++;
        char l_then[64];
        char l_else[64];
        char l_end[64];
        snprintf(l_then, sizeof(l_then), "Lcin_if_then_%d", id);
        snprintf(l_else, sizeof(l_else), "Lcin_if_else_%d", id);
        snprintf(l_end, sizeof(l_end), "Lcin_if_end_%d", id);

        CinderType cond_ty;
        CinderType expect_bool = {.base = CINDER_TYPE_BOOL, .pointer_depth = 0};
        if (!emit_expr_from_text(ctx, node->name, &cond_ty, &expect_bool))
          return 0;
        if (!string_list_appendf(&ctx->body, "  branch %s %s", l_then, l_else))
          return 0;
        if (!string_list_appendf(&ctx->body, "label %s", l_then))
          return 0;
        if (!emit_block(ctx, node->children, node->child_count, label_id))
          return 0;
        if (!string_list_appendf(&ctx->body, "  jump %s", l_end))
          return 0;
        if (!string_list_appendf(&ctx->body, "label %s", l_else))
          return 0;
        if (!emit_if_with_optional_else(ctx, nested_if, nested_else, label_id))
          return 0;
        if (!string_list_appendf(&ctx->body, "  jump %s", l_end))
          return 0;
        if (!string_list_appendf(&ctx->body, "label %s", l_end))
          return 0;

        ++i;
        continue;
      }

      if (!emit_if_with_optional_else(ctx, node, else_node, label_id))
        return 0;
      continue;
    }
    if (!emit_statement(ctx, node, label_id))
      return 0;
  }
  return 1;
}

static int split_assignment(const char *text, char **lhs, char **rhs)
{
  if (!text || !lhs || !rhs)
    return 0;
  int depth = 0;
  int in_string = 0;
  char quote = '\0';
  const char *eq = NULL;
  for (const char *p = text; *p; ++p)
  {
    char c = *p;
    if (c == '"' || c == '\'')
    {
      if (!in_string)
      {
        in_string = 1;
        quote = c;
      }
      else if (quote == c && (p == text || p[-1] != '\\'))
      {
        in_string = 0;
      }
    }
    if (in_string)
      continue;
    if (c == '(')
      ++depth;
    else if (c == ')')
      --depth;
    else if (c == '=' && depth == 0)
    {
      if (p[1] == '=')
      {
        ++p;
        continue;
      }
      eq = p;
      break;
    }
  }
  if (!eq)
    return 0;

  size_t ln = (size_t)(eq - text);
  char *l = (char *)malloc(ln + 1);
  if (!l)
    return 0;
  memcpy(l, text, ln);
  l[ln] = '\0';
  char *r = trim_dup(eq + 1);
  if (!r)
  {
    free(l);
    return 0;
  }
  char *lt = trim_dup(l);
  free(l);
  if (!lt)
  {
    free(r);
    return 0;
  }
  *lhs = lt;
  *rhs = r;
  return 1;
}

static int emit_statement(EmitFunctionCtx *ctx, CinderNode *node, int *label_id)
{
  if (!ctx || !node || !label_id)
    return 0;

  if (strcmp(node->keyword, "return") == 0)
  {
    if (ctx->method->return_type.base == CINDER_TYPE_VOID &&
        ctx->method->return_type.pointer_depth == 0)
    {
      char *ret_text = trim_dup(node->name ? node->name : "");
      int has_value = ret_text && ret_text[0];
      free(ret_text);
      if (has_value)
        return 0;
      if (!emit_main_stdout_flush(ctx))
        return 0;
      return string_list_appendf(&ctx->body, "  ret void");
    }
    CinderType ty;
    if (!emit_expr_from_text(ctx, node->name, &ty, &ctx->method->return_type))
      return 0;
    if (!cinder_type_equals(ty, ctx->method->return_type))
      return 0;
    if (!emit_main_stdout_flush(ctx))
      return 0;
    return string_list_appendf(&ctx->body, "  ret");
  }

  if (strcmp(node->keyword, "if") == 0)
  {
    return emit_if_with_optional_else(ctx, node, NULL, label_id);
  }

  if (strcmp(node->keyword, "else") == 0)
  {
    return 0;
  }

  if (is_cinder_type_keyword(node->keyword))
  {
    char *lhs = NULL;
    char *rhs = NULL;
    char *name_only = NULL;
    int name_only_is_lhs = 0;

    if (!cinder_parse_type(node->keyword, &(CinderType){0}))
      return 0;
    CinderType ty;
    if (!cinder_parse_type(node->keyword, &ty))
      return 0;

    if (split_assignment(node->name, &lhs, &rhs))
    {
      name_only = lhs;
      name_only_is_lhs = 1;
    }
    else
    {
      name_only = trim_dup(node->name);
    }

    if (!name_only || !name_only[0])
    {
      free(lhs);
      free(rhs);
      if (!name_only_is_lhs)
        free(name_only);
      return 0;
    }

    CinderLocal *existing = NULL;
    if (emit_lookup_local(ctx, name_only, &existing) && existing)
    {
      free(lhs);
      free(rhs);
      if (!name_only_is_lhs)
        free(name_only);
      return 0;
    }

    int idx = -1;
    if (!emit_add_local(ctx, name_only, ty, &idx))
    {
      free(lhs);
      free(rhs);
      free(name_only);
      return 0;
    }

    if (rhs)
    {
      CinderType expr_ty;
      if (!emit_expr_from_text(ctx, rhs, &expr_ty, &ty))
      {
        free(lhs);
        free(rhs);
        if (!name_only_is_lhs)
          free(name_only);
        return 0;
      }
      if (!cinder_type_equals(expr_ty, ty))
      {
        free(lhs);
        free(rhs);
        if (!name_only_is_lhs)
          free(name_only);
        return 0;
      }
      if (!string_list_appendf(&ctx->body, "  store_local %d", idx))
      {
        free(lhs);
        free(rhs);
        if (!name_only_is_lhs)
          free(name_only);
        return 0;
      }
    }

    free(lhs);
    free(rhs);
    if (!name_only_is_lhs)
      free(name_only);
    return 1;
  }

  if (strcmp(node->keyword, "raw") == 0)
  {
    char *lhs = NULL;
    char *rhs = NULL;
    if (split_assignment(node->name, &lhs, &rhs))
    {
      CinderLocal *local = NULL;
      if (!emit_lookup_local(ctx, lhs, &local) || !local)
      {
        free(lhs);
        free(rhs);
        return 0;
      }
      CinderType expr_ty;
      if (!emit_expr_from_text(ctx, rhs, &expr_ty, &local->type))
      {
        free(lhs);
        free(rhs);
        return 0;
      }
      if (!cinder_type_equals(expr_ty, local->type))
      {
        free(lhs);
        free(rhs);
        return 0;
      }
      int ok = string_list_appendf(&ctx->body, "  store_local %d", local->local_index);
      free(lhs);
      free(rhs);
      return ok;
    }

    CinderType expr_ty;
    if (!emit_expr_from_text(ctx, node->name, &expr_ty, NULL))
      return 0;
    if (!(expr_ty.base == CINDER_TYPE_VOID && expr_ty.pointer_depth == 0))
    {
      if (!string_list_appendf(&ctx->body, "  drop %s", cinder_type_to_cc(expr_ty)))
        return 0;
    }
    return 1;
  }

  return 0;
}

static int emit_method_to_ccb(FILE *out, CinderProgram *program, CinderNamespace *ns,
                              CinderMethod *method)
{
  if (!out || !program || !ns || !method)
    return 0;

  if (method->is_extern)
  {
    if (fprintf(out, ".extern %s params=(", method->symbol_name) < 0)
      return 0;
    for (int i = 0; i < method->param_count; ++i)
    {
      if (i && fputc(',', out) == EOF)
        return 0;
      if (fprintf(out, "%s", cinder_type_to_cc(method->params[i].type)) < 0)
        return 0;
    }
    if (fprintf(out, ") returns=%s%s\n", cinder_type_to_cc(method->return_type),
                method->is_variadic ? " varargs" : "") < 0)
      return 0;
    return 1;
  }

  EmitFunctionCtx ctx;
  memset(&ctx, 0, sizeof(ctx));
  ctx.method = method;
  ctx.ns = ns;
  ctx.program = program;

  for (int i = 0; i < method->param_count; ++i)
  {
    int idx = -1;
    if (!emit_add_local(&ctx, method->params[i].name, method->params[i].type, &idx))
      goto fail;
  }

  for (int i = 0; i < method->param_count; ++i)
  {
    if (!string_list_appendf(&ctx.body, "  load_param %d", i))
      goto fail;
    if (!string_list_appendf(&ctx.body, "  store_local %d", i))
      goto fail;
  }

  int label_id = 0;
  if (!emit_block(&ctx, method->body->children, method->body->child_count, &label_id))
    goto fail;

  if (ctx.body.count == 0 || strcmp(ctx.body.items[ctx.body.count - 1], "  ret") != 0)
  {
    if (method->return_type.base == CINDER_TYPE_VOID &&
        method->return_type.pointer_depth == 0)
    {
      if (!emit_main_stdout_flush(&ctx))
        goto fail;
      if (!string_list_appendf(&ctx.body, "  ret void"))
        goto fail;
    }
  }

  if (fprintf(out, ".func %s ret=%s params=%d locals=%d\n", method->symbol_name,
              cinder_type_to_cc(method->return_type), method->param_count,
              ctx.local_count) < 0)
    goto fail;

  if (ctx.local_count > 0)
  {
    if (fprintf(out, ".locals") < 0)
      goto fail;
    for (int i = 0; i < ctx.local_count; ++i)
    {
      if (fprintf(out, " %s", cinder_type_to_cc(ctx.locals[i].type)) < 0)
        goto fail;
    }
    if (fputc('\n', out) == EOF)
      goto fail;
  }

  if (method->param_count > 0)
  {
    if (fprintf(out, ".params") < 0)
      goto fail;
    for (int i = 0; i < method->param_count; ++i)
    {
      if (fprintf(out, " %s", cinder_type_to_cc(method->params[i].type)) < 0)
        goto fail;
    }
    if (fputc('\n', out) == EOF)
      goto fail;
  }

  for (int i = 0; i < ctx.body.count; ++i)
  {
    if (fprintf(out, "%s\n", ctx.body.items[i]) < 0)
      goto fail;
  }
  if (fprintf(out, ".endfunc\n") < 0)
    goto fail;

  if (strcmp(method->symbol_name, "main") == 0)
  {
    if (fprintf(out, ".preserve main\n") < 0)
      goto fail;
  }

  for (int i = 0; i < ctx.local_count; ++i)
    free(ctx.locals[i].name);
  free(ctx.locals);
  string_list_destroy(&ctx.body);
  return 1;

fail:
  for (int i = 0; i < ctx.local_count; ++i)
    free(ctx.locals[i].name);
  free(ctx.locals);
  string_list_destroy(&ctx.body);
  return 0;
}

static int cinder_supports_input_path(const char *input_path)
{
  return input_path && ends_with_icase(input_path, ".cin");
}

static CinderFrontendUnit *cinder_impl(FrontendUnit *unit)
{
  if (!unit || !unit->impl)
    return NULL;
  return (CinderFrontendUnit *)unit->impl;
}

static const CinderFrontendUnit *cinder_impl_const(const FrontendUnit *unit)
{
  if (!unit || !unit->impl)
    return NULL;
  return (const CinderFrontendUnit *)unit->impl;
}

static FrontendUnit *cinder_load_unit(const ChanceFrontend *frontend,
                                      const ChanceFrontendLoadRequest *request,
                                      int keep_semantic_state)
{
  (void)frontend;
  (void)keep_semantic_state;
  if (!request || !request->source.src)
    return NULL;

  FrontendUnit *unit = (FrontendUnit *)calloc(1, sizeof(FrontendUnit));
  CinderFrontendUnit *impl =
      (CinderFrontendUnit *)calloc(1, sizeof(CinderFrontendUnit));
  if (!unit || !impl)
  {
    free(unit);
    free(impl);
    return NULL;
  }

  if (!cinder_parse_program(request->source.src, &impl->program))
  {
    fprintf(stderr, "cinder frontend: parse failed for '%s'\n",
            request->input_path ? request->input_path : "<unknown>");
    free(unit);
    free(impl);
    return NULL;
  }

  unit->frontend = chance_frontend_cinder();
  unit->impl = impl;
  return unit;
}

static void cinder_discard_semantic_state(FrontendUnit *unit)
{
  (void)unit;
}

static Node *cinder_unit_ast(FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static const Node *cinder_unit_ast_const(const FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static SymTable *cinder_unit_symtab(FrontendUnit *unit)
{
  (void)unit;
  return NULL;
}

static int cinder_unit_check(FrontendUnit *unit)
{
  const CinderFrontendUnit *impl = cinder_impl_const(unit);
  return (impl && impl->program.parse_ok) ? 0 : 1;
}

static void cinder_register_foreign_symbols(FrontendUnit *target,
                                            const FrontendUnit *foreign)
{
  (void)target;
  (void)foreign;
}

static const Symbol *cinder_unit_externs(FrontendUnit *unit, int *count)
{
  (void)unit;
  if (count)
    *count = 0;
  return NULL;
}

static Symbol *cinder_copy_imported_function_symbols(const FrontendUnit *unit,
                                                     int *out_count)
{
  (void)unit;
  if (out_count)
    *out_count = 0;
  return NULL;
}

static Symbol *cinder_copy_imported_global_symbols(const FrontendUnit *unit,
                                                   int *out_count)
{
  (void)unit;
  if (out_count)
    *out_count = 0;
  return NULL;
}

static void cinder_track_imported_function(FrontendUnit *unit,
                                           const char *name,
                                           const char *module_full,
                                           const Symbol *symbol)
{
  (void)unit;
  (void)name;
  (void)module_full;
  (void)symbol;
}

static int cinder_supports_direct_ccb_emit(const FrontendUnit *unit)
{
  const CinderFrontendUnit *impl = cinder_impl_const(unit);
  return impl && impl->program.parse_ok;
}

static int cinder_emit_ccb(FrontendUnit *unit,
                           const ChanceFrontendEmitRequest *request)
{
  CinderFrontendUnit *impl = cinder_impl(unit);
  if (!impl || !request || !request->ccb_output_path)
    return 1;

  FILE *out = fopen(request->ccb_output_path, "wb");
  if (!out)
    return 1;

  int ok = 1;
  if (fprintf(out, "ccbytecode 3\n\n") < 0)
    ok = 0;

  if (ok)
  {
    int has_fflush = 0;
    for (int ni = 0; ni < impl->program.namespace_count && !has_fflush; ++ni)
    {
      CinderNamespace *ns = &impl->program.namespaces[ni];
      for (int mi = 0; mi < ns->method_count; ++mi)
      {
        CinderMethod *m = &ns->methods[mi];
        if (m->is_extern && m->symbol_name && strcmp(m->symbol_name, "fflush") == 0)
        {
          has_fflush = 1;
          break;
        }
      }
    }
    if (!has_fflush)
      ok = (fprintf(out, ".extern fflush params=(ptr) returns=i32\n") > 0);
  }

  for (int ni = 0; ok && ni < impl->program.namespace_count; ++ni)
  {
    CinderNamespace *ns = &impl->program.namespaces[ni];
    for (int mi = 0; ok && mi < ns->method_count; ++mi)
      ok = emit_method_to_ccb(out, &impl->program, ns, &ns->methods[mi]);
    if (ok && ns->method_count > 0)
      ok = (fputc('\n', out) != EOF);
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

static void cinder_destroy_unit(FrontendUnit *unit)
{
  CinderFrontendUnit *impl = cinder_impl(unit);
  if (impl)
  {
    cinder_program_destroy(&impl->program);
    free(impl);
  }
  free(unit);
}

static const ChanceFrontendOps cinder_frontend_ops = {
    .load_unit = cinder_load_unit,
    .discard_semantic_state = cinder_discard_semantic_state,
    .unit_ast = cinder_unit_ast,
    .unit_ast_const = cinder_unit_ast_const,
    .unit_symtab = cinder_unit_symtab,
    .unit_check = cinder_unit_check,
    .register_foreign_symbols = cinder_register_foreign_symbols,
    .unit_externs = cinder_unit_externs,
    .copy_imported_function_symbols = cinder_copy_imported_function_symbols,
    .copy_imported_global_symbols = cinder_copy_imported_global_symbols,
    .track_imported_function = cinder_track_imported_function,
    .supports_direct_ccb_emit = cinder_supports_direct_ccb_emit,
    .emit_ccb = cinder_emit_ccb,
    .destroy_unit = cinder_destroy_unit,
};

static const ChanceFrontend cinder_frontend = {
    .name = "cinder",
    .description = "Cinder language frontend (.cin)",
    .supports_input_path = cinder_supports_input_path,
    .ops = &cinder_frontend_ops,
};

const ChanceFrontend *chance_frontend_cinder(void)
{
  return &cinder_frontend;
}
