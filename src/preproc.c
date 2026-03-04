#include "preproc.h"
#include "ast.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_MACRO_RECURSION 64
#define MAX_CONDITION_RECURSION 32

typedef struct
{
	char *name;
	int param_count; // -1 for object-like macros
	char **params;
	char *body;
} Macro;

typedef struct
{
	int parent_active;
	int current_active;
	int branch_taken;
	int saw_else;
} CondFrame;

typedef struct
{
	const Macro *items[MAX_MACRO_RECURSION];
	int count;
} MacroStack;

typedef struct
{
	Macro *macros;
	int macro_count;
	int macro_cap;
	CondFrame *conds;
	int cond_count;
	int cond_cap;
	const char *path;
	MacroStack expansion_stack;
	char date_literal[32];
	char time_literal[32];
	char *module_name;
	int counter;
	int pointer_width;
	char **interned_strings;
	size_t *interned_string_lengths;
	int interned_string_count;
	int interned_string_cap;
} PreprocState;

typedef struct
{
	char *data;
	size_t len;
	size_t cap;
} StrBuilder;

typedef struct
{
	const char *name;
	const char *value;
} MacroParam;

static const char *detect_host_architecture(void)
{
#if defined(_M_X64) || defined(__x86_64__) || defined(__amd64__)
	return "x86-64";
#elif defined(_M_IX86) || defined(__i386__) || defined(__i386)
	return "x86-32";
#elif defined(_M_I86) || defined(__i86__)
	return "x86-16";
#elif defined(_M_ARM64) || defined(__aarch64__)
	return "ARM64";
#elif defined(_M_ARM) || defined(__arm__)
	return "ARM32";
#elif defined(__ppc64__) || defined(__powerpc64__)
	return "PPC64";
#elif defined(__ppc__) || defined(__powerpc__)
	return "PPC";
#elif defined(__riscv) || defined(__riscv__)
	return "RISC-V";
#else
	return "unknown";
#endif
}

static void sb_init(StrBuilder *sb)
{
	sb->data = NULL;
	sb->len = 0;
	sb->cap = 0;
}

static void sb_reserve(StrBuilder *sb, size_t extra)
{
	size_t need = sb->len + extra + 1;
	if (need <= sb->cap)
		return;
	size_t cap = sb->cap ? sb->cap : 64;
	while (cap < need)
		cap *= 2;
	char *ndata = (char *)realloc(sb->data, cap);
	if (!ndata)
		diag_error("preprocessor: out of memory");
	sb->data = ndata;
	sb->cap = cap;
}

static void sb_append_range(StrBuilder *sb, const char *s, size_t len)
{
	if (!len)
		return;
	sb_reserve(sb, len);
	memcpy(sb->data + sb->len, s, len);
	sb->len += len;
}

static void sb_append_char(StrBuilder *sb, char c)
{
	sb_reserve(sb, 1);
	sb->data[sb->len++] = c;
}

static void sb_append_str(StrBuilder *sb, const char *s)
{
	if (!s)
		return;
	sb_append_range(sb, s, strlen(s));
}

static char *sb_build(StrBuilder *sb)
{
	sb_append_char(sb, '\0');
	char *out = sb->data;
	sb->data = NULL;
	sb->len = 0;
	sb->cap = 0;
	return out ? out : xstrdup("");
}

static void free_macro(Macro *mac)
{
	if (!mac)
		return;
	free(mac->name);
	free(mac->body);
	if (mac->params)
	{
		for (int i = 0; i < mac->param_count; ++i)
			free(mac->params[i]);
		free(mac->params);
	}
	memset(mac, 0, sizeof(*mac));
}

static void macro_stack_push(MacroStack *stack, const Macro *mac)
{
	if (!stack || !mac)
		return;
	if (stack->count >= MAX_MACRO_RECURSION)
		return;
	stack->items[stack->count++] = mac;
}

static void macro_stack_pop(MacroStack *stack, const Macro *mac)
{
	if (!stack || stack->count <= 0)
		return;
	stack->count--;
}

static int macro_stack_contains(const MacroStack *stack, const Macro *mac)
{
	if (!stack || !mac)
		return 0;
	for (int i = 0; i < stack->count; ++i)
	{
		if (stack->items[i] == mac)
			return 1;
	}
	return 0;
}

static int is_ident_start(char c)
{
	return (c == '_') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static int is_ident_char(char c)
{
	return is_ident_start(c) || (c >= '0' && c <= '9');
}

static void trim_range(const char *src, size_t len, size_t *start, size_t *end)
{
	while (*start < *end && (src[*start] == ' ' || src[*start] == '\t' || src[*start] == '\r' || src[*start] == '\n'))
		(*start)++;
	while (*end > *start && (src[*end - 1] == ' ' || src[*end - 1] == '\t' || src[*end - 1] == '\r' || src[*end - 1] == '\n'))
		(*end)--;
}

static char *copy_trimmed(const char *src, size_t start, size_t end)
{
	if (end <= start)
		return xstrdup("");
	char *out = (char *)xmalloc(end - start + 1);
	memcpy(out, src + start, end - start);
	out[end - start] = '\0';
	return out;
}

static char *make_string_literal(const char *s)
{
	StrBuilder sb;
	sb_init(&sb);
	sb_append_char(&sb, '"');
	if (s)
	{
		for (const char *p = s; *p; ++p)
		{
			unsigned char c = (unsigned char)*p;
			switch (c)
			{
			case '\\':
			case '"':
				sb_append_char(&sb, '\\');
				sb_append_char(&sb, (char)c);
				break;
			case '\n':
				sb_append_char(&sb, '\\');
				sb_append_char(&sb, 'n');
				break;
			case '\r':
				sb_append_char(&sb, '\\');
				sb_append_char(&sb, 'r');
				break;
			case '\t':
				sb_append_char(&sb, '\\');
				sb_append_char(&sb, 't');
				break;
			default:
				sb_append_char(&sb, (char)c);
				break;
			}
		}
	}
	sb_append_char(&sb, '"');
	return sb_build(&sb);
}

static long preproc_intern_string_value(PreprocState *st, const char *value, size_t len)
{
	if (!st || !value)
		return 0;
	for (int i = 0; i < st->interned_string_count; ++i)
	{
		if (st->interned_string_lengths[i] == len &&
			(len == 0 || memcmp(st->interned_strings[i], value, len) == 0))
			return (long)(i + 1);
	}
	if (st->interned_string_count == st->interned_string_cap)
	{
		int ncap = st->interned_string_cap ? st->interned_string_cap * 2 : 8;
		char **tmp_str = (char **)realloc(st->interned_strings, (size_t)ncap * sizeof(char *));
		if (!tmp_str)
		{
			diag_error("preprocessor: out of memory interning string");
			return 0;
		}
		size_t *tmp_len = (size_t *)realloc(st->interned_string_lengths, (size_t)ncap * sizeof(size_t));
		if (!tmp_len)
		{
			diag_error("preprocessor: out of memory interning string");
			return 0;
		}
		st->interned_strings = tmp_str;
		st->interned_string_lengths = tmp_len;
		st->interned_string_cap = ncap;
	}
	char *copy = (char *)xmalloc(len + 1);
	memcpy(copy, value, len);
	copy[len] = '\0';
	st->interned_strings[st->interned_string_count] = copy;
	st->interned_string_lengths[st->interned_string_count] = len;
	st->interned_string_count++;
	return (long)st->interned_string_count;
}

static void preproc_error(const PreprocState *st, int line, const char *fmt, ...)
{
	va_list ap;
	fprintf(stderr, "%s:%d: error: ", st && st->path ? st->path : "<input>", line > 0 ? line : 0);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(1);
}

static int macro_find_index(const PreprocState *st, const char *name)
{
	if (!st || !name)
		return -1;
	for (int i = 0; i < st->macro_count; ++i)
	{
		if (strcmp(st->macros[i].name, name) == 0)
			return i;
	}
	return -1;
}

static Macro *macro_find(PreprocState *st, const char *name)
{
	int idx = macro_find_index(st, name);
	if (idx < 0)
		return NULL;
	return &st->macros[idx];
}

static void macro_remove(PreprocState *st, const char *name)
{
	int idx = macro_find_index(st, name);
	if (idx < 0)
		return;
	free_macro(&st->macros[idx]);
	for (int i = idx + 1; i < st->macro_count; ++i)
		st->macros[i - 1] = st->macros[i];
	st->macro_count--;
}

static void macro_add(PreprocState *st, Macro mac)
{
	if (st->macro_count == st->macro_cap)
	{
		int ncap = st->macro_cap ? st->macro_cap * 2 : 16;
		Macro *nm = (Macro *)realloc(st->macros, (size_t)ncap * sizeof(Macro));
		if (!nm)
			diag_error("preprocessor: out of memory adding macro");
		st->macros = nm;
		st->macro_cap = ncap;
	}
	st->macros[st->macro_count++] = mac;
}

static void define_builtin_macro(PreprocState *st, const char *name, const char *value)
{
	if (!st || !name)
		return;
	macro_remove(st, name);
	Macro mac = {0};
	mac.name = xstrdup(name);
	mac.param_count = -1;
	mac.params = NULL;
	mac.body = xstrdup(value ? value : "");
	macro_add(st, mac);
}

static void define_builtin_flag(PreprocState *st, const char *name)
{
	define_builtin_macro(st, name, "1");
}

static char *detect_module_name(const char *src, int len)
{
	if (!src || len <= 0)
		return NULL;
	const char *p = src;
	const char *end = src + len;
	while (p < end)
	{
		if (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')
		{
			p++;
			continue;
		}
		if (*p == '/' && p + 1 < end)
		{
			if (p[1] == '/')
			{
				p += 2;
				while (p < end && *p != '\n')
					p++;
				continue;
			}
			if (p[1] == '*')
			{
				p += 2;
				while (p + 1 < end && !(p[0] == '*' && p[1] == '/'))
					p++;
				if (p + 1 < end)
					p += 2;
				continue;
			}
		}
		break;
	}
	if (p + 6 <= end && strncmp(p, "module", 6) == 0 && !is_ident_char(p[6]))
	{
		p += 6;
		while (p < end && (*p == ' ' || *p == '\t'))
			p++;
		const char *start = p;
		while (p < end && *p != ';' && *p != '\n' && *p != '\r')
			p++;
		const char *finish = p;
		while (finish > start && (finish[-1] == ' ' || finish[-1] == '\t'))
			finish--;
		if (finish > start)
		{
			size_t sidx = (size_t)(start - src);
			size_t eidx = (size_t)(finish - src);
			return copy_trimmed(src, sidx, eidx);
		}
	}
	return NULL;
}

typedef struct
{
	const char *expr;
	size_t len;
	size_t pos;
	PreprocState *state;
	int line_no;
	int depth;
} ExprParser;

static void expr_skip_ws(ExprParser *p)
{
	while (p->pos < p->len && (p->expr[p->pos] == ' ' || p->expr[p->pos] == '\t' || p->expr[p->pos] == '\r' || p->expr[p->pos] == '\n'))
		p->pos++;
}

static int expr_match_keyword(ExprParser *p, const char *kw)
{
	expr_skip_ws(p);
	size_t klen = strlen(kw);
	if (p->pos + klen > p->len)
		return 0;
	if (strncmp(p->expr + p->pos, kw, klen) == 0)
	{
		if (p->pos + klen < p->len && is_ident_char(p->expr[p->pos + klen]))
			return 0;
		p->pos += klen;
		return 1;
	}
	return 0;
}

static long eval_expr_recursive(PreprocState *st, const char *expr, size_t len, int line_no, int depth);

static long eval_identifier_value(PreprocState *st, const char *name, int line_no, int depth)
{
	if (!st || !name)
		return 0;
	if (strcmp(name, "__LINE__") == 0)
		return line_no;
	if (strcmp(name, "__IS64BIT__") == 0)
		return st->pointer_width >= 64 ? 1 : 0;
	if (strcmp(name, "__POINTER_WIDTH__") == 0)
		return st->pointer_width;
	if (strcmp(name, "__COUNTER__") == 0)
		return st->counter++;
	Macro *mac = macro_find(st, name);
	if (!mac)
		return 0;
	if (mac->param_count >= 0)
		return 0;
	const char *body = mac->body ? mac->body : "";
	size_t body_len = strlen(body);
	return eval_expr_recursive(st, body, body_len, line_no, depth + 1);
}

static long parse_primary(ExprParser *p);

static long parse_unary(ExprParser *p)
{
	expr_skip_ws(p);
	if (p->pos >= p->len)
		return 0;
	char c = p->expr[p->pos];
	if (c == '!')
	{
		p->pos++;
		long v = parse_unary(p);
		return !v;
	}
	if (c == '+')
	{
		p->pos++;
		return parse_unary(p);
	}
	if (c == '-')
	{
		p->pos++;
		long v = parse_unary(p);
		return -v;
	}
	if (c == '~')
	{
		p->pos++;
		long v = parse_unary(p);
		return ~v;
	}
	return parse_primary(p);
}

static long parse_mul(ExprParser *p)
{
	long lhs = parse_unary(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos >= p->len)
			return lhs;
		char c = p->expr[p->pos];
		if (c == '*' || c == '/' || c == '%')
		{
			p->pos++;
			long rhs = parse_unary(p);
			if (c == '*')
				lhs *= rhs;
			else if (c == '/')
				lhs = rhs ? lhs / rhs : 0;
			else
				lhs = rhs ? lhs % rhs : 0;
		}
		else
			return lhs;
	}
}

static long parse_add(ExprParser *p)
{
	long lhs = parse_mul(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos >= p->len)
			return lhs;
		char c = p->expr[p->pos];
		if (c == '+' || c == '-')
		{
			p->pos++;
			long rhs = parse_mul(p);
			if (c == '+')
				lhs += rhs;
			else
				lhs -= rhs;
		}
		else
			return lhs;
	}
}

static long parse_rel(ExprParser *p)
{
	long lhs = parse_add(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, "<=", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_add(p);
			lhs = lhs <= rhs;
			continue;
		}
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, ">=", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_add(p);
			lhs = lhs >= rhs;
			continue;
		}
		if (p->pos < p->len && p->expr[p->pos] == '<')
		{
			p->pos++;
			long rhs = parse_add(p);
			lhs = lhs < rhs;
			continue;
		}
		if (p->pos < p->len && p->expr[p->pos] == '>')
		{
			p->pos++;
			long rhs = parse_add(p);
			lhs = lhs > rhs;
			continue;
		}
		break;
	}
	return lhs;
}

static long parse_eq(ExprParser *p)
{
	long lhs = parse_rel(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, "==", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_rel(p);
			lhs = lhs == rhs;
			continue;
		}
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, "!=", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_rel(p);
			lhs = lhs != rhs;
			continue;
		}
		break;
	}
	return lhs;
}

static long parse_and(ExprParser *p)
{
	long lhs = parse_eq(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, "&&", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_eq(p);
			lhs = (lhs && rhs);
			continue;
		}
		break;
	}
	return lhs;
}

static long parse_or(ExprParser *p)
{
	long lhs = parse_and(p);
	while (1)
	{
		expr_skip_ws(p);
		if (p->pos + 1 < p->len && strncmp(p->expr + p->pos, "||", 2) == 0)
		{
			p->pos += 2;
			long rhs = parse_and(p);
			lhs = (lhs || rhs);
			continue;
		}
		break;
	}
	return lhs;
}

static long parse_primary(ExprParser *p)
{
	expr_skip_ws(p);
	if (p->pos >= p->len)
		return 0;
	char c = p->expr[p->pos];
	if (c == '(')
	{
		p->pos++;
		long v = parse_or(p);
		expr_skip_ws(p);
		if (p->pos < p->len && p->expr[p->pos] == ')')
			p->pos++;
		return v;
	}
	if (expr_match_keyword(p, "defined"))
	{
		expr_skip_ws(p);
		int need_paren = 0;
		if (p->pos < p->len && p->expr[p->pos] == '(')
		{
			need_paren = 1;
			p->pos++;
		}
		expr_skip_ws(p);
		size_t start = p->pos;
		while (p->pos < p->len && is_ident_char(p->expr[p->pos]))
			p->pos++;
		size_t end = p->pos;
		if (end <= start)
			return 0;
		char *name = copy_trimmed(p->expr, start, end);
		int res = macro_find(p->state, name) != NULL;
		free(name);
		if (need_paren)
		{
			expr_skip_ws(p);
			if (p->pos < p->len && p->expr[p->pos] == ')')
				p->pos++;
		}
		return res;
	}
	if (c == '"')
	{
		StrBuilder sb;
		sb_init(&sb);
		int had_segment = 0;
		while (p->pos < p->len && p->expr[p->pos] == '"')
		{
			p->pos++;
			int escape = 0;
			int terminated = 0;
			while (p->pos < p->len)
			{
				char ch = p->expr[p->pos++];
				if (escape)
				{
					switch (ch)
					{
					case '\\':
					case '"':
					case '\'':
						sb_append_char(&sb, ch);
						break;
					case 'n':
						sb_append_char(&sb, '\n');
						break;
					case 'r':
						sb_append_char(&sb, '\r');
						break;
					case 't':
						sb_append_char(&sb, '\t');
						break;
					case '0':
						sb_append_char(&sb, '\0');
						break;
					default:
						sb_append_char(&sb, ch);
						break;
					}
					escape = 0;
					continue;
				}
				if (ch == '\\')
				{
					escape = 1;
					continue;
				}
				if (ch == '"')
				{
					terminated = 1;
					break;
				}
				if (ch == '\n' || ch == '\r')
				{
					if (sb.data)
						free(sb.data);
					return 0;
				}
				sb_append_char(&sb, ch);
			}
			if (!terminated)
			{
				if (sb.data)
					free(sb.data);
				return 0;
			}
			had_segment = 1;
			expr_skip_ws(p);
		}
		if (!had_segment)
		{
			if (sb.data)
				free(sb.data);
			return 0;
		}
		size_t str_len = sb.len;
		char *value = sb_build(&sb);
		long id = preproc_intern_string_value(p->state, value, str_len);
		free(value);
		return id;
	}
	if (c == '\'')
	{
		p->pos++;
		int escape = 0;
		int terminated = 0;
		long result = 0;
		int have_value = 0;
		while (p->pos < p->len)
		{
			char ch = p->expr[p->pos++];
			if (escape)
			{
				switch (ch)
				{
				case '\\':
				case '\'':
					result = (unsigned char)ch;
					break;
				case 'n':
					result = '\n';
					break;
				case 'r':
					result = '\r';
					break;
				case 't':
					result = '\t';
					break;
				case '0':
					result = '\0';
					break;
				default:
					result = (unsigned char)ch;
					break;
				}
				escape = 0;
				have_value = 1;
				continue;
			}
			if (ch == '\\')
			{
				escape = 1;
				continue;
			}
			if (ch == '\'')
			{
				terminated = 1;
				break;
			}
			result = (unsigned char)ch;
			have_value = 1;
		}
		if (!terminated || !have_value)
			return 0;
		return result;
	}
	if ((c >= '0' && c <= '9') || c == '.')
	{
		char *endptr = NULL;
		long val = strtol(p->expr + p->pos, &endptr, 0);
		p->pos = (size_t)(endptr - p->expr);
		return val;
	}
	if (is_ident_start(c))
	{
		size_t start = p->pos;
		p->pos++;
		while (p->pos < p->len && is_ident_char(p->expr[p->pos]))
			p->pos++;
		char *name = copy_trimmed(p->expr, start, p->pos);
		long val = eval_identifier_value(p->state, name, p->line_no, p->depth + 1);
		free(name);
		return val;
	}
	return 0;
}

static long eval_expr_recursive(PreprocState *st, const char *expr, size_t len, int line_no, int depth)
{
	if (depth > MAX_CONDITION_RECURSION)
		return 0;
	ExprParser p = {expr, len, 0, st, line_no, depth};
	long v = parse_or(&p);
	return v;
}

static int eval_condition(PreprocState *st, const char *expr, size_t len, int line_no)
{
	long v = eval_expr_recursive(st, expr, len, line_no, 0);
	return v != 0;
}

static char *expand_text(PreprocState *st, const char *text, size_t len, MacroParam *params, int param_count, MacroStack *stack, int depth, int line_no);
static char *expand_directive_argument(PreprocState *st, const char *src, size_t len, int line_no);

static char *try_expand_identifier(PreprocState *st, const char *src, size_t len, size_t start, size_t *out_end,
								   MacroParam *params, int param_count, MacroStack *stack, int depth, int line_no);

static char *expand_text(PreprocState *st, const char *text, size_t len, MacroParam *params, int param_count, MacroStack *stack, int depth, int line_no)
{
	if (!text || len == 0)
		return xstrdup("");
	if (depth > MAX_MACRO_RECURSION)
	{
		char *dup = (char *)xmalloc(len + 1);
		memcpy(dup, text, len);
		dup[len] = '\0';
		return dup;
	}
	StrBuilder sb;
	sb_init(&sb);
	size_t i = 0;
	size_t last_emit = 0;
	int in_string = 0;
	int in_char = 0;
	int escape = 0;
	while (i < len)
	{
		char c = text[i];
		if (in_string)
		{
			if (!escape && c == '"')
				in_string = 0;
			escape = (!escape && c == '\\');
			i++;
			continue;
		}
		if (in_char)
		{
			if (!escape && c == '\'')
				in_char = 0;
			escape = (!escape && c == '\\');
			i++;
			continue;
		}
		if (c == '"')
		{
			i++;
			in_string = 1;
			continue;
		}
		if (c == '\'')
		{
			i++;
			in_char = 1;
			continue;
		}
		if (c == '/' && i + 1 < len)
		{
			if (text[i + 1] == '/')
			{
				i += 2;
				while (i < len && text[i] != '\n')
					i++;
				continue;
			}
			if (text[i + 1] == '*')
			{
				i += 2;
				while (i + 1 < len && !(text[i] == '*' && text[i + 1] == '/'))
					i++;
				if (i + 1 < len)
					i += 2;
				continue;
			}
		}
		if (is_ident_start(c))
		{
			sb_append_range(&sb, text + last_emit, i - last_emit);
			size_t end = i;
			char *expanded = try_expand_identifier(st, text, len, i, &end, params, param_count, stack, depth, line_no);
			if (expanded)
			{
				sb_append_str(&sb, expanded);
				free(expanded);
				i = end;
				last_emit = i;
				continue;
			}
			sb_append_range(&sb, text + i, end - i);
			i = end;
			last_emit = i;
			continue;
		}
		i++;
	}
	if (last_emit < len)
		sb_append_range(&sb, text + last_emit, len - last_emit);
	return sb_build(&sb);
}

static char *expand_directive_argument(PreprocState *st, const char *src, size_t len, int line_no)
{
	if (!src || len == 0)
		return xstrdup("");
	size_t start = 0;
	size_t end = len;
	trim_range(src, len, &start, &end);
	char *raw = copy_trimmed(src, start, end);
	if (!raw)
		return xstrdup("");
	size_t raw_len = strlen(raw);
	char *expanded = expand_text(st, raw, raw_len, NULL, 0, &st->expansion_stack, 0, line_no);
	free(raw);
	return expanded;
}

static int find_param_index(MacroParam *params, int param_count, const char *name)
{
	for (int i = 0; i < param_count; ++i)
	{
		if (strcmp(params[i].name, name) == 0)
			return i;
	}
	return -1;
}

static char *collapse_line_continuations(const char *src, size_t len)
{
	if (!src || len == 0)
		return xstrdup("");
	StrBuilder sb;
	sb_init(&sb);
	size_t i = 0;
	while (i < len)
	{
		if (src[i] == '\\' && i + 1 < len)
		{
			if (src[i + 1] == '\n')
			{
				i += 2;
				continue;
			}
			if (src[i + 1] == '\r')
			{
				i += 2;
				if (i < len && src[i] == '\n')
					i++;
				continue;
			}
		}
		sb_append_char(&sb, src[i]);
		i++;
	}
	return sb_build(&sb);
}

static char *macro_stringize_argument(const char *arg)
{
	const char *src = arg ? arg : "";
	size_t len = strlen(src);
	size_t start = 0;
	size_t end = len;
	trim_range(src, len, &start, &end);
	StrBuilder collapsed;
	sb_init(&collapsed);
	int pending_space = 0;
	for (size_t i = start; i < end; ++i)
	{
		char c = src[i];
		if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
		{
			pending_space = 1;
			continue;
		}
		if (pending_space && collapsed.len > 0)
			sb_append_char(&collapsed, ' ');
		pending_space = 0;
		sb_append_char(&collapsed, c);
	}
	char *normalized = sb_build(&collapsed);
	char *lit = make_string_literal(normalized);
	free(normalized);
	return lit;
}

static char *apply_macro_stringize(const char *body, MacroParam *raw_params, int param_count)
{
	if (!body)
		return xstrdup("");
	size_t len = strlen(body);
	StrBuilder sb;
	sb_init(&sb);
	size_t i = 0;
	int in_string = 0;
	int in_char = 0;
	int escape = 0;
	while (i < len)
	{
		char c = body[i];
		if (in_string)
		{
			sb_append_char(&sb, c);
			if (!escape && c == '"')
				in_string = 0;
			escape = (!escape && c == '\\');
			i++;
			continue;
		}
		if (in_char)
		{
			sb_append_char(&sb, c);
			if (!escape && c == '\'')
				in_char = 0;
			escape = (!escape && c == '\\');
			i++;
			continue;
		}
		if (c == '"')
		{
			sb_append_char(&sb, c);
			in_string = 1;
			i++;
			continue;
		}
		if (c == '\'')
		{
			sb_append_char(&sb, c);
			in_char = 1;
			i++;
			continue;
		}
		if (c == '#')
		{
			size_t j = i + 1;
			while (j < len && (body[j] == ' ' || body[j] == '\t'))
				j++;
			if (j < len && is_ident_start(body[j]))
			{
				size_t id_start = j;
				j++;
				while (j < len && is_ident_char(body[j]))
					j++;
				size_t id_len = j - id_start;
				char ident_buf[128];
				char *ident = ident_buf;
				if (id_len >= sizeof(ident_buf))
					ident = (char *)xmalloc(id_len + 1);
				memcpy(ident, body + id_start, id_len);
				ident[id_len] = '\0';
				int param_idx = find_param_index(raw_params, param_count, ident);
				if (ident != ident_buf)
					free(ident);
				if (param_idx >= 0)
				{
					char *lit = macro_stringize_argument(raw_params[param_idx].value);
					sb_append_str(&sb, lit);
					free(lit);
					i = j;
					continue;
				}
			}
		}
		sb_append_char(&sb, c);
		i++;
	}
	return sb_build(&sb);
}

static int parse_macro_arguments(const char *src, size_t len, size_t lparen, char ***out_args, int *out_count, size_t *out_end,
								 PreprocState *st, int line_no)
{
	size_t pos = lparen + 1;
	int depth = 1;
	size_t arg_start = pos;
	char **args = NULL;
	int arg_count = 0;
	int arg_cap = 0;
	int in_string = 0, in_char = 0, escape = 0;
	while (pos < len)
	{
		char c = src[pos];
		if (in_string)
		{
			if (!escape && c == '"')
				in_string = 0;
			escape = (!escape && c == '\\');
			pos++;
			continue;
		}
		if (in_char)
		{
			if (!escape && c == '\'')
				in_char = 0;
			escape = (!escape && c == '\\');
			pos++;
			continue;
		}
		if (c == '"')
		{
			in_string = 1;
			pos++;
			continue;
		}
		if (c == '\'')
		{
			in_char = 1;
			pos++;
			continue;
		}
		if (c == '/' && pos + 1 < len)
		{
			if (src[pos + 1] == '/')
			{
				pos += 2;
				while (pos < len && src[pos] != '\n')
					pos++;
				continue;
			}
			if (src[pos + 1] == '*')
			{
				pos += 2;
				while (pos + 1 < len && !(src[pos] == '*' && src[pos + 1] == '/'))
					pos++;
				if (pos + 1 < len)
					pos += 2;
				continue;
			}
		}
		if (c == '(')
		{
			depth++;
			pos++;
			continue;
		}
		if (c == ')')
		{
			depth--;
			if (depth == 0)
			{
				size_t start = arg_start;
				size_t end = pos;
				trim_range(src, len, &start, &end);
				if (!(arg_count == 0 && start == end))
				{
					if (arg_count == arg_cap)
					{
						int ncap = arg_cap ? arg_cap * 2 : 4;
						char **tmp = (char **)realloc(args, (size_t)ncap * sizeof(char *));
						if (!tmp)
							diag_error("preprocessor: out of memory parsing macro arguments");
						args = tmp;
						arg_cap = ncap;
					}
					args[arg_count++] = copy_trimmed(src, start, end);
				}
				pos++;
				*out_args = args;
				*out_count = arg_count;
				*out_end = pos;
				return 1;
			}
			pos++;
			continue;
		}
		if (c == ',' && depth == 1)
		{
			size_t start = arg_start;
			size_t end = pos;
			trim_range(src, len, &start, &end);
			if (arg_count == arg_cap)
			{
				int ncap = arg_cap ? arg_cap * 2 : 4;
				char **tmp = (char **)realloc(args, (size_t)ncap * sizeof(char *));
				if (!tmp)
					diag_error("preprocessor: out of memory parsing macro arguments");
				args = tmp;
				arg_cap = ncap;
			}
			args[arg_count++] = copy_trimmed(src, start, end);
			pos++;
			while (pos < len && (src[pos] == ' ' || src[pos] == '\t' || src[pos] == '\r' || src[pos] == '\n'))
				pos++;
			arg_start = pos;
			continue;
		}
		pos++;
	}
	preproc_error(st, line_no, "unterminated macro argument list");
	return 0;
}

static char *expand_function_macro(PreprocState *st, const Macro *mac, const char *src, size_t len, size_t ident_start,
								   size_t *out_end, MacroStack *stack, int depth, int line_no)
{
	size_t lparen = ident_start;
	while (lparen < len && is_ident_char(src[lparen]))
		lparen++;
	if (lparen >= len || src[lparen] != '(')
		return NULL;
	char **raw_args = NULL;
	int arg_count = 0;
	size_t after_args = lparen;
	if (!parse_macro_arguments(src, len, lparen, &raw_args, &arg_count, &after_args, st, line_no))
		return NULL;
	if (arg_count != mac->param_count)
	{
		preproc_error(st, line_no, "macro '%s' expects %d argument(s), got %d", mac->name, mac->param_count, arg_count);
	}
	MacroParam *subs = NULL;
	MacroParam *raw_subs = NULL;
	if (mac->param_count > 0)
	{
		subs = (MacroParam *)xcalloc((size_t)mac->param_count, sizeof(MacroParam));
		raw_subs = (MacroParam *)xcalloc((size_t)mac->param_count, sizeof(MacroParam));
		for (int i = 0; i < mac->param_count; ++i)
		{
			subs[i].name = mac->params[i];
			raw_subs[i].name = mac->params[i];
			raw_subs[i].value = raw_args[i];
			char *expanded = expand_text(st, raw_args[i], strlen(raw_args[i]), NULL, 0, stack, depth + 1, line_no);
			subs[i].value = expanded;
		}
	}
	char *stringized_body = apply_macro_stringize(mac->body ? mac->body : "", raw_subs, mac->param_count);
	macro_stack_push(stack, mac);
	char *result = expand_text(st, stringized_body, strlen(stringized_body), subs, mac->param_count, stack, depth + 1, line_no);
	char *rescanned = expand_text(st, result, strlen(result), NULL, 0, stack, depth + 1, line_no);
	free(result);
	result = rescanned;
	macro_stack_pop(stack, mac);
	free(stringized_body);
	if (subs)
	{
		for (int i = 0; i < mac->param_count; ++i)
			free((char *)subs[i].value);
		free(subs);
	}
	free(raw_subs);
	for (int i = 0; i < arg_count; ++i)
		free(raw_args[i]);
	free(raw_args);
	*out_end = after_args;
	return result;
}

static char *expand_object_macro(PreprocState *st, const Macro *mac, MacroStack *stack, int depth, int line_no)
{
	macro_stack_push(stack, mac);
	const char *body = mac->body ? mac->body : "";
	char *res = expand_text(st, body, strlen(body), NULL, 0, stack, depth + 1, line_no);
	macro_stack_pop(stack, mac);
	return res;
}

static char *try_expand_identifier(PreprocState *st, const char *src, size_t len, size_t start, size_t *out_end,
								   MacroParam *params, int param_count, MacroStack *stack, int depth, int line_no)
{
	size_t end = start;
	while (end < len && is_ident_char(src[end]))
		end++;
	*out_end = end;
	if (end == start)
		return NULL;
	char temp[128];
	size_t name_len = end - start;
	char *name = NULL;
	if (name_len < sizeof(temp))
	{
		memcpy(temp, src + start, name_len);
		temp[name_len] = '\0';
		name = temp;
	}
	else
	{
		name = (char *)xmalloc(name_len + 1);
		memcpy(name, src + start, name_len);
		name[name_len] = '\0';
	}
	if (strcmp(name, "__LINE__") == 0)
	{
		char buf[32];
		snprintf(buf, sizeof(buf), "%d", line_no);
		char *dup = xstrdup(buf);
		if (name != temp)
			free(name);
		return dup;
	}
	if (strcmp(name, "__COUNTER__") == 0)
	{
		char buf[32];
		snprintf(buf, sizeof(buf), "%d", st->counter++);
		char *dup = xstrdup(buf);
		if (name != temp)
			free(name);
		return dup;
	}

	int param_idx = params ? find_param_index(params, param_count, name) : -1;
	if (param_idx >= 0)
	{
		char *dup = xstrdup(params[param_idx].value ? params[param_idx].value : "");
		if (name != temp)
			free(name);
		return dup;
	}

	Macro *mac = macro_find(st, name);
	if (name != temp)
		free(name);
	if (!mac)
		return NULL;
	if (macro_stack_contains(stack, mac))
		return NULL;
	if (mac->param_count >= 0)
	{
		if (src[end] != '(')
			return NULL;
		return expand_function_macro(st, mac, src, len, start, out_end, stack, depth, line_no);
	}
	return expand_object_macro(st, mac, stack, depth, line_no);
}

static int current_active(const PreprocState *st)
{
	if (!st || st->cond_count == 0)
		return 1;
	return st->conds[st->cond_count - 1].current_active;
}

static void push_condition(PreprocState *st, int parent_active, int cond)
{
	if (st->cond_count == st->cond_cap)
	{
		int ncap = st->cond_cap ? st->cond_cap * 2 : 8;
		CondFrame *nc = (CondFrame *)realloc(st->conds, (size_t)ncap * sizeof(CondFrame));
		if (!nc)
			diag_error("preprocessor: out of memory tracking conditionals");
		st->conds = nc;
		st->cond_cap = ncap;
	}
	CondFrame frame;
	frame.parent_active = parent_active;
	frame.current_active = parent_active && cond;
	frame.branch_taken = parent_active && cond;
	frame.saw_else = 0;
	st->conds[st->cond_count++] = frame;
}

static void handle_if(PreprocState *st, const char *expr, size_t len, int line_no)
{
	int parent_active = current_active(st);
	int cond = parent_active ? eval_condition(st, expr, len, line_no) : 0;
	push_condition(st, parent_active, cond);
}

static void handle_else(PreprocState *st, int line_no)
{
	if (st->cond_count == 0)
		preproc_error(st, line_no, "#else without matching #if");
	CondFrame *frame = &st->conds[st->cond_count - 1];
	if (frame->saw_else)
		preproc_error(st, line_no, "multiple #else directives in the same #if");
	frame->saw_else = 1;
	if (!frame->parent_active)
	{
		frame->current_active = 0;
		return;
	}
	frame->current_active = !frame->branch_taken;
	frame->branch_taken = frame->branch_taken || frame->current_active;
}

static void handle_elif(PreprocState *st, const char *expr, size_t len, int line_no)
{
	if (st->cond_count == 0)
		preproc_error(st, line_no, "#elif without matching #if");
	CondFrame *frame = &st->conds[st->cond_count - 1];
	if (frame->saw_else)
		preproc_error(st, line_no, "#elif after #else");
	if (!frame->parent_active || frame->branch_taken)
	{
		frame->current_active = 0;
		return;
	}
	int cond = eval_condition(st, expr, len, line_no);
	frame->current_active = cond;
	if (cond)
		frame->branch_taken = 1;
}

static void handle_endif(PreprocState *st, int line_no)
{
	if (st->cond_count == 0)
		preproc_error(st, line_no, "#endif without matching #if");
	st->cond_count--;
}

static void define_macro(PreprocState *st, const char *line, size_t len, int line_no)
{
	size_t pos = 0;
	while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
		pos++;
	size_t name_start = pos;
	if (pos >= len || !is_ident_start(line[pos]))
		preproc_error(st, line_no, "#define missing macro name");
	pos++;
	while (pos < len && is_ident_char(line[pos]))
		pos++;
	size_t name_end = pos;
	int function_like = 0;
	if (pos < len && line[pos] == '(')
	{
		function_like = 1;
	}
	size_t body_start = pos;
	Macro mac = {0};
	mac.name = copy_trimmed(line, name_start, name_end);
	macro_remove(st, mac.name);
	if (function_like)
	{
		pos++; // skip '('
		char **params = NULL;
		int param_count = 0;
		int param_cap = 0;
		size_t current_start = pos;
		int closed = 0;
		while (pos < len)
		{
			char c = line[pos];
			if (c == ')')
			{
				size_t start = current_start;
				size_t end = pos;
				trim_range(line, len, &start, &end);
				if (end > start)
				{
					if (param_count == param_cap)
					{
						int ncap = param_cap ? param_cap * 2 : 4;
						char **tmp = (char **)realloc(params, (size_t)ncap * sizeof(char *));
						if (!tmp)
							diag_error("preprocessor: out of memory storing macro parameters");
						params = tmp;
						param_cap = ncap;
					}
					params[param_count++] = copy_trimmed(line, start, end);
				}
				pos++;
				closed = 1;
				break;
			}
			if (c == ',')
			{
				size_t start = current_start;
				size_t end = pos;
				trim_range(line, len, &start, &end);
				if (end <= start)
					preproc_error(st, line_no, "empty parameter name in macro");
				if (param_count == param_cap)
				{
					int ncap = param_cap ? param_cap * 2 : 4;
					char **tmp = (char **)realloc(params, (size_t)ncap * sizeof(char *));
					if (!tmp)
						diag_error("preprocessor: out of memory storing macro parameters");
					params = tmp;
					param_cap = ncap;
				}
				params[param_count++] = copy_trimmed(line, start, end);
				pos++;
				while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
					pos++;
				current_start = pos;
				continue;
			}
			pos++;
		}
		if (!closed)
			preproc_error(st, line_no, "unterminated parameter list in macro");
		mac.param_count = param_count;
		mac.params = params;
		body_start = pos;
	}
	else
	{
		mac.param_count = -1;
		mac.params = NULL;
	}
	while (body_start < len && (line[body_start] == ' ' || line[body_start] == '\t'))
		body_start++;
	mac.body = copy_trimmed(line, body_start, len);
	macro_add(st, mac);
}

static void undef_macro(PreprocState *st, const char *line, size_t len)
{
	size_t pos = 0;
	while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
		pos++;
	size_t start = pos;
	while (pos < len && is_ident_char(line[pos]))
		pos++;
	if (pos > start)
	{
		char *name = copy_trimmed(line, start, pos);
		macro_remove(st, name);
		free(name);
	}
}

static int handle_directive(PreprocState *st, const char *src, int len, int *index, StrBuilder *out, int *line_no)
{
	int i = *index;
	int orig = i;
	while (i < len && (src[i] == ' ' || src[i] == '\t'))
		i++;
	if (i >= len || src[i] != '#')
		return 0;
	i++;
	while (i < len && (src[i] == ' ' || src[i] == '\t'))
		i++;
	size_t kw_start = i;
	while (i < len && is_ident_char(src[i]))
		i++;
	size_t kw_end = i;
	while (i < len && (src[i] == ' ' || src[i] == '\t'))
		i++;
	size_t arg_start = i;
	size_t line_end = i;
	int consumed_newlines = 0;
	while (1)
	{
		while (line_end < len && src[line_end] != '\n' && src[line_end] != '\r')
			line_end++;
		size_t probe = line_end;
		while (probe > arg_start && (src[probe - 1] == ' ' || src[probe - 1] == '\t'))
			probe--;
		if (!(probe > arg_start && src[probe - 1] == '\\' && line_end < len))
			break;
		if (src[line_end] == '\r' && line_end + 1 < len && src[line_end + 1] == '\n')
			line_end += 2;
		else
			line_end += 1;
		consumed_newlines++;
	}
	size_t arg_len = (line_end > arg_start) ? (line_end - arg_start) : 0;
	char *directive_arg = collapse_line_continuations(src + arg_start, arg_len);
	size_t directive_arg_len = strlen(directive_arg);
	char keyword[32];
	size_t kw_len = kw_end - kw_start;
	if (kw_len >= sizeof(keyword))
		kw_len = sizeof(keyword) - 1;
	memcpy(keyword, src + kw_start, kw_len);
	keyword[kw_len] = '\0';
	int active = current_active(st);

	if (strcmp(keyword, "define") == 0)
	{
		if (active)
			define_macro(st, directive_arg, directive_arg_len, *line_no);
	}
	else if (strcmp(keyword, "undef") == 0)
	{
		if (active)
			undef_macro(st, directive_arg, directive_arg_len);
	}
	else if (strcmp(keyword, "warn") == 0)
	{
		if (active)
		{
			char *msg = expand_directive_argument(st, directive_arg, directive_arg_len, *line_no);
			const char *path = st && st->path ? st->path : "<input>";
			const char *text = (msg && *msg) ? msg : "#warn triggered";
			diag_warning("%s:%d: warning: %s", path, *line_no, text);
			free(msg);
		}
	}
	else if (strcmp(keyword, "note") == 0)
	{
		if (active)
		{
			char *msg = expand_directive_argument(st, directive_arg, directive_arg_len, *line_no);
			const char *path = st && st->path ? st->path : "<input>";
			const char *text = (msg && *msg) ? msg : "#note";
			diag_note("%s:%d: note: %s", path, *line_no, text);
			free(msg);
		}
	}
	else if (strcmp(keyword, "error") == 0)
	{
		if (active)
		{
			char *msg = expand_directive_argument(st, directive_arg, directive_arg_len, *line_no);
			const char *text = (msg && *msg) ? msg : "#error";
			preproc_error(st, *line_no, "%s", text);
			free(msg);
		}
	}
	else if (strcmp(keyword, "static_assert") == 0)
	{
		if (active)
		{
			char *arg = expand_directive_argument(st, directive_arg, directive_arg_len, *line_no);
			if (arg)
			{
				size_t len = strlen(arg);
				const char *arg_end = arg + len;
				const char *expr_start = arg;
				size_t expr_len = len;
				if (len >= 2 && arg[0] == '(' && arg[len - 1] == ')')
				{
					expr_start = arg + 1;
					expr_len = len - 2;
				}
				while (expr_len > 0 && (expr_start[0] == ' ' || expr_start[0] == '\t'))
				{
					expr_start++;
					expr_len--;
				}
				while (expr_len > 0 && (expr_start[expr_len - 1] == ' ' || expr_start[expr_len - 1] == '\t'))
					expr_len--;
				const char *message_start = NULL;
				size_t message_len = 0;
				int depth = 0;
				int in_string = 0;
				int escape = 0;
				for (size_t i = 0; i < expr_len; ++i)
				{
					char c = expr_start[i];
					if (in_string)
					{
						if (!escape && c == '"')
							in_string = 0;
						escape = (!escape && c == '\\');
						continue;
					}
					if (c == '"')
					{
						in_string = 1;
						continue;
					}
					if (escape)
					{
						escape = 0;
						continue;
					}
					if (c == '(')
					{
						depth++;
						continue;
					}
					if (c == ')')
					{
						if (depth > 0)
							depth--;
						continue;
					}
					if (c == ',' && depth == 0)
					{
						expr_len = i;
						message_start = expr_start + i + 1;
						message_len = (size_t)(arg_end - message_start);
						break;
					}
				}
				while (expr_len > 0 && (expr_start[expr_len - 1] == ' ' || expr_start[expr_len - 1] == '\t'))
					expr_len--;
				if (message_start)
				{
					while (message_len > 0 && (*message_start == ' ' || *message_start == '\t'))
					{
						message_start++;
						message_len--;
					}
					while (message_len > 0 && (message_start[message_len - 1] == ' ' || message_start[message_len - 1] == '\t'))
						message_len--;
				}
				int cond = expr_len > 0 ? eval_condition(st, expr_start, expr_len, *line_no) : 0;
				if (!cond)
				{
					char buffer[512];
					const char *msg = NULL;
					if (message_start && message_len > 0)
					{
						size_t copy_len = message_len < sizeof(buffer) - 1 ? message_len : sizeof(buffer) - 1;
						memcpy(buffer, message_start, copy_len);
						buffer[copy_len] = '\0';
						msg = buffer;
					}
					if (msg && *msg)
						preproc_error(st, *line_no, "static assertion failed: %s", msg);
					else
						preproc_error(st, *line_no, "static assertion failed");
				}
			}
			free(arg);
		}
	}
	else if (strcmp(keyword, "ifdef") == 0)
	{
		size_t start = 0;
		size_t end = directive_arg_len;
		trim_range(directive_arg, directive_arg_len, &start, &end);
		char *name = copy_trimmed(directive_arg, start, end);
		int cond = macro_find(st, name) != NULL;
		free(name);
		push_condition(st, current_active(st), cond);
	}
	else if (strcmp(keyword, "ifndef") == 0)
	{
		size_t start = 0;
		size_t end = directive_arg_len;
		trim_range(directive_arg, directive_arg_len, &start, &end);
		char *name = copy_trimmed(directive_arg, start, end);
		int cond = macro_find(st, name) == NULL;
		free(name);
		push_condition(st, current_active(st), cond);
	}
	if (strcmp(keyword, "hint") == 0)
	{
		if (active)
		{
			(void)directive_arg; (void)line_no; (void)active;
			// parse tokens in directive_arg and emit start markers into output
			size_t pos = 0;
			int emitted = 0;
			while (pos < directive_arg_len)
			{
				// skip separators
				while (pos < directive_arg_len && (directive_arg[pos] == ' ' || directive_arg[pos] == '\t' || directive_arg[pos] == ','))
					pos++;
				if (pos >= directive_arg_len) break;
				size_t start = pos;
				while (pos < directive_arg_len && (is_ident_char(directive_arg[pos]) || directive_arg[pos] == '-')) pos++;
				size_t end = pos;
				if (end <= start) break;
				char *tok = copy_trimmed(directive_arg, start, end);
				if (strcmp(tok, "implicit-void-function") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__");
					emitted = 1;
				}
				else if (strcmp(tok, "implicit-sizeof") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_START_IMPLICIT_SIZEOF__");
					emitted = 1;
				}
				else if (strcmp(tok, "implicit-voidp") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_START_IMPLICIT_VOIDP__");
					emitted = 1;
				}
				free(tok);
			}
			(void)emitted;
		}
	}
	else if (strcmp(keyword, "nohint") == 0)
	{
		if (active)
		{
			(void)directive_arg; (void)line_no; (void)active;
			// Emit end markers for hints so parser can turn off region behavior.
			size_t pos = 0;
			int emitted = 0;
			while (pos < directive_arg_len)
			{
				while (pos < directive_arg_len && (directive_arg[pos] == ' ' || directive_arg[pos] == '\t' || directive_arg[pos] == ',')) pos++;
				if (pos >= directive_arg_len) break;
				size_t start = pos;
				while (pos < directive_arg_len && (is_ident_char(directive_arg[pos]) || directive_arg[pos] == '-')) pos++;
				size_t end = pos;
				if (end <= start) break;
				char *tok = copy_trimmed(directive_arg, start, end);
				if (strcmp(tok, "implicit-void-function") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__");
					emitted = 1;
				}
				else if (strcmp(tok, "implicit-sizeof") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_END_IMPLICIT_SIZEOF__");
					emitted = 1;
				}
				else if (strcmp(tok, "implicit-voidp") == 0)
				{
					sb_append_str(out, "__CHANCE_HINT_END_IMPLICIT_VOIDP__");
					emitted = 1;
				}
				free(tok);
			}
			(void)emitted;
		}
	}
	else if (strcmp(keyword, "if") == 0)
		handle_if(st, directive_arg, directive_arg_len, *line_no);
	else if (strcmp(keyword, "elif") == 0)
		handle_elif(st, directive_arg, directive_arg_len, *line_no);
	else if (strcmp(keyword, "else") == 0)
		handle_else(st, *line_no);
	else if (strcmp(keyword, "endif") == 0)
		handle_endif(st, *line_no);

	free(directive_arg);

	size_t newline_pos = line_end;
	if (newline_pos < len)
	{
		if (src[newline_pos] == '\r' && newline_pos + 1 < len && src[newline_pos + 1] == '\n')
			newline_pos += 2;
		else
			newline_pos += 1;
		consumed_newlines++;
		for (int n = 0; n < consumed_newlines; ++n)
			sb_append_char(out, '\n');
		(*line_no) += consumed_newlines;
	}
	*index = (int)newline_pos;
	return 1;
}

static void process_active_line(PreprocState *st, const char *src, int len, int *index, StrBuilder *out, int *line_no)
{
	int pos = *index;
	int start = pos;
	while (pos < len && src[pos] != '\n' && src[pos] != '\r')
		pos++;
	size_t slice_len = (size_t)(pos - start);
	char *expanded = expand_text(st, src + start, slice_len, NULL, 0, &st->expansion_stack, 0, *line_no);
	sb_append_str(out, expanded);
	free(expanded);
	if (pos < len)
	{
		if (src[pos] == '\r' && pos + 1 < len && src[pos + 1] == '\n')
			pos += 2;
		else
			pos += 1;
		sb_append_char(out, '\n');
		(*line_no)++;
	}
	*index = pos;
}

static void process_inactive_line(const char *src, int len, int *index, StrBuilder *out, int *line_no)
{
	int pos = *index;
	while (pos < len && src[pos] != '\n' && src[pos] != '\r')
		pos++;
	if (pos < len)
	{
		if (src[pos] == '\r' && pos + 1 < len && src[pos + 1] == '\n')
			pos += 2;
		else
			pos += 1;
		sb_append_char(out, '\n');
		(*line_no)++;
	}
	*index = pos;
}

char *chance_preprocess_source(const char *path, const char *src, int len,
							   int *out_len, const char *target_arch_name)
{
	if (!src || len <= 0)
	{
		if (out_len)
			*out_len = 0;
		return xstrdup("");
	}
	PreprocState st;
	memset(&st, 0, sizeof(st));
	st.path = path;
	st.expansion_stack.count = 0;
	st.counter = 0;
	st.pointer_width = (int)(sizeof(void *) * 8);
	st.module_name = detect_module_name(src, len);
	st.date_literal[0] = '\0';
	st.time_literal[0] = '\0';
	time_t now = time(NULL);
	struct tm tm_now;
#if defined(_WIN32)
	if (localtime_s(&tm_now, &now) == 0)
#else
	if (localtime_r(&now, &tm_now) != NULL)
#endif
	{
		if (strftime(st.date_literal, sizeof(st.date_literal), "%b %d %Y", &tm_now) == 0)
			st.date_literal[0] = '\0';
		if (strftime(st.time_literal, sizeof(st.time_literal), "%H:%M:%S", &tm_now) == 0)
			st.time_literal[0] = '\0';
	}
	if (!st.date_literal[0])
		strncpy(st.date_literal, "Jan 01 1970", sizeof(st.date_literal) - 1);
	if (!st.time_literal[0])
		strncpy(st.time_literal, "00:00:00", sizeof(st.time_literal) - 1);
	st.date_literal[sizeof(st.date_literal) - 1] = '\0';
	st.time_literal[sizeof(st.time_literal) - 1] = '\0';
	define_builtin_macro(&st, "__CHANCE__", "1");
	const int version_major = 1;
	const int version_minor = 1;
	const int version_patch = 0;
	char version_compact[16];
	snprintf(version_compact, sizeof(version_compact), "%d", version_major * 10000 + version_minor * 100 + version_patch);
	define_builtin_macro(&st, "__CHANCE_VERSION__", version_compact);
	char version_major_buf[8];
	char version_minor_buf[8];
	char version_patch_buf[8];
	snprintf(version_major_buf, sizeof(version_major_buf), "%d", version_major);
	snprintf(version_minor_buf, sizeof(version_minor_buf), "%d", version_minor);
	snprintf(version_patch_buf, sizeof(version_patch_buf), "%d", version_patch);
	define_builtin_macro(&st, "__CHANCE_VERSION_MAJOR__", version_major_buf);
	define_builtin_macro(&st, "__CHANCE_VERSION_MINOR__", version_minor_buf);
	define_builtin_macro(&st, "__CHANCE_VERSION_PATCH__", version_patch_buf);
	char version_str_literal[32];
	snprintf(version_str_literal, sizeof(version_str_literal), "%d.%d.%d", version_major, version_minor, version_patch);
	char *version_str_macro = make_string_literal(version_str_literal);
	define_builtin_macro(&st, "__CHANCE_VERSION_STR__", version_str_macro);
	free(version_str_macro);
	char pointer_buf[16];
	snprintf(pointer_buf, sizeof(pointer_buf), "%d", st.pointer_width);
	define_builtin_macro(&st, "__POINTER_WIDTH__", pointer_buf);
	define_builtin_macro(&st, "__IS64BIT__", st.pointer_width >= 64 ? "1" : "0");
	const char *arch_name =
		(target_arch_name && *target_arch_name) ? target_arch_name
												: detect_host_architecture();
	char *arch_literal = make_string_literal(arch_name);
	define_builtin_macro(&st, "__TARGET_ARCH__", arch_literal);
	define_builtin_macro(&st, "__ARCH__", arch_literal);
	free(arch_literal);
	define_builtin_macro(&st, "__LINE__", "0");
	define_builtin_macro(&st, "__COUNTER__", "0");
#if defined(_WIN32)
	define_builtin_flag(&st, "__WIN32__");
	define_builtin_flag(&st, "_WIN32");
#elif defined(__APPLE__) || defined(__MACH__)
	define_builtin_flag(&st, "__APPLE__");
	define_builtin_flag(&st, "__MACH__");
#else
	define_builtin_flag(&st, "__LINUX__");
	define_builtin_flag(&st, "__linux__");
#endif
	char *file_literal = make_string_literal(path ? path : "");
	define_builtin_macro(&st, "__FILE__", file_literal);
	free(file_literal);
	const char *module_macro_value = st.module_name ? st.module_name : (path ? path : "");
	char *module_literal = make_string_literal(module_macro_value);
	define_builtin_macro(&st, "__MODULE__", module_literal);
	free(module_literal);
	char *date_literal = make_string_literal(st.date_literal);
	define_builtin_macro(&st, "__DATE__", date_literal);
	free(date_literal);
	char *time_literal = make_string_literal(st.time_literal);
	define_builtin_macro(&st, "__TIME__", time_literal);
	free(time_literal);
#if 0
	Macro *dbg_date = macro_find(&st, "__DATE__");
	Macro *dbg_time = macro_find(&st, "__TIME__");
	fprintf(stderr, "[preproc dbg] __DATE__=%s, __TIME__=%s, counter=%d\n",
			dbg_date && dbg_date->body ? dbg_date->body : "(null)",
			dbg_time && dbg_time->body ? dbg_time->body : "(null)",
			st.counter);
#endif
	StrBuilder out;
	sb_init(&out);
	int index = 0;
	int line_no = 1;
	int at_line_start = 1;
	while (index < len)
	{
		if (at_line_start)
		{
			if (handle_directive(&st, src, len, &index, &out, &line_no))
			{
				at_line_start = 1;
				continue;
			}
		}
		if (current_active(&st))
		{
			process_active_line(&st, src, len, &index, &out, &line_no);
		}
		else
		{
			process_inactive_line(src, len, &index, &out, &line_no);
		}
		at_line_start = 1;
	}
	if (st.cond_count != 0)
		preproc_error(&st, line_no, "unterminated conditional block");
	char *result = sb_build(&out);
	if (out_len)
		*out_len = (int)strlen(result);
	for (int i = 0; i < st.macro_count; ++i)
		free_macro(&st.macros[i]);
	free(st.macros);
	free(st.conds);
	for (int i = 0; i < st.interned_string_count; ++i)
		free(st.interned_strings[i]);
	free(st.interned_strings);
	free(st.interned_string_lengths);
	if (st.module_name)
		free(st.module_name);
	return result;
}
