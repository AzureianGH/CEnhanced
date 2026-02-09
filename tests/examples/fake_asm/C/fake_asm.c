#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_LABELS 32
#define MAX_CODE 256

typedef struct
{
    const char *name;
    int length;
    int addr;
} Label;

typedef struct
{
    const char *src;
} AsmSource;

static int is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' || c == '\f';
}

static char tolower_ascii(char c)
{
    if (c >= 'A' && c <= 'Z')
        return (char)(c + 32);
    return c;
}

static int ident_eq(const char *a, int a_len, const char *b)
{
    int b_len = (int)strlen(b);
    if (a_len != b_len)
        return 0;
    for (int i = 0; i < a_len; ++i)
    {
        if (tolower_ascii(a[i]) != tolower_ascii(b[i]))
            return 0;
    }
    return 1;
}

static int parse_ident(const char *line, int i, int end, const char **out, int *out_len)
{
    if (i >= end)
        return -1;
    char c = line[i];
    if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' ))
        return -1;
    int start = i;
    i++;
    while (i < end)
    {
        c = line[i];
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'))
            break;
        i++;
    }
    *out = line + start;
    *out_len = i - start;
    return i;
}

static int parse_int(const char *line, int i, int end, int *out)
{
    int value = 0;
    int saw = 0;
    while (i < end && line[i] >= '0' && line[i] <= '9')
    {
        value = value * 10 + (line[i] - '0');
        i++;
        saw = 1;
    }
    if (!saw)
        return -1;
    *out = value;
    return i;
}

static int parse_reg(const char *line, int i, int end, int *out)
{
    if (i + 1 >= end)
        return -1;
    char c0 = line[i];
    char c1 = line[i + 1];
    if (tolower_ascii(c0) != 'r')
        return -1;
    if (c1 < '0' || c1 > '7')
        return -1;
    *out = c1 - '0';
    return i + 2;
}

static int parse_imm(const char *line, int i, int end, int *out)
{
    if (i >= end || line[i] != '#')
        return -1;
    return parse_int(line, i + 1, end, out);
}

static int skip_ws(const char *line, int i, int end)
{
    while (i < end && is_space(line[i]))
        i++;
    return i;
}

static void add_label(Label *labels, int *count, const char *name, int len, int addr)
{
    if (*count >= MAX_LABELS)
        return;
    labels[*count].name = name;
    labels[*count].length = len;
    labels[*count].addr = addr;
    (*count)++;
}

static int find_label(const Label *labels, int count, const char *name, int len)
{
    for (int i = 0; i < count; ++i)
    {
        if (labels[i].length == len && ident_eq(labels[i].name, len, name))
            return labels[i].addr;
    }
    return -1;
}

static void first_pass(const char *source, Label *labels, int *label_count)
{
    int addr = 0;
    const char *p = source;
    while (*p)
    {
        const char *line = p;
        const char *line_end = strchr(p, '\n');
        if (!line_end)
            line_end = p + strlen(p);
        int end = (int)(line_end - line);

        const char *comment = memchr(line, ';', (size_t)end);
        if (comment)
            end = (int)(comment - line);

        int i = skip_ws(line, 0, end);
        if (i < end)
        {
            const char *name = NULL;
            int name_len = 0;
            int j = parse_ident(line, i, end, &name, &name_len);
            if (j >= 0)
            {
                j = skip_ws(line, j, end);
                if (j < end && line[j] == ':')
                {
                    add_label(labels, label_count, name, name_len, addr);
                    j = skip_ws(line, j + 1, end);
                    if (j < end)
                    {
                        const char *mn = NULL;
                        int mn_len = 0;
                        int k = parse_ident(line, j, end, &mn, &mn_len);
                        if (k >= 0)
                        {
                            if (ident_eq(mn, mn_len, "mov") || ident_eq(mn, mn_len, "jmp"))
                                addr += 2;
                            else if (ident_eq(mn, mn_len, "add") || ident_eq(mn, mn_len, "sub"))
                                addr += 1;
                        }
                    }
                }
                else
                {
                    if (ident_eq(name, name_len, "mov") || ident_eq(name, name_len, "jmp"))
                        addr += 2;
                    else if (ident_eq(name, name_len, "add") || ident_eq(name, name_len, "sub"))
                        addr += 1;
                }
            }
        }

        if (*line_end == '\0')
            break;
        p = line_end + 1;
    }
}

static void emit_byte(uint8_t *code, int *code_len, uint8_t value)
{
    if (*code_len >= MAX_CODE)
        return;
    code[*code_len] = value;
    (*code_len)++;
}

static void second_pass(const char *source, const Label *labels, int label_count, uint8_t *code, int *code_len)
{
    const char *p = source;
    while (*p)
    {
        const char *line = p;
        const char *line_end = strchr(p, '\n');
        if (!line_end)
            line_end = p + strlen(p);
        int end = (int)(line_end - line);

        const char *comment = memchr(line, ';', (size_t)end);
        if (comment)
            end = (int)(comment - line);

        int i = skip_ws(line, 0, end);
        if (i < end)
        {
            const char *name = NULL;
            int name_len = 0;
            int j = parse_ident(line, i, end, &name, &name_len);
            if (j >= 0)
            {
                j = skip_ws(line, j, end);
                if (j < end && line[j] == ':')
                {
                    j = skip_ws(line, j + 1, end);
                    if (j < end)
                    {
                        name = NULL;
                        name_len = 0;
                        j = parse_ident(line, j, end, &name, &name_len);
                    }
                    else
                    {
                        name = NULL;
                        name_len = 0;
                    }
                }

                if (name && name_len > 0)
                {
                    int k = skip_ws(line, j, end);
                    if (ident_eq(name, name_len, "mov"))
                    {
                        int reg = 0;
                        int imm = 0;
                        k = parse_reg(line, k, end, &reg);
                        k = skip_ws(line, k, end);
                        if (k >= 0 && k < end && line[k] == ',')
                            k = skip_ws(line, k + 1, end);
                        k = parse_imm(line, k, end, &imm);
                        emit_byte(code, code_len, (uint8_t)(0x10 | (reg & 7)));
                        emit_byte(code, code_len, (uint8_t)(imm & 0xFF));
                    }
                    else if (ident_eq(name, name_len, "add") || ident_eq(name, name_len, "sub"))
                    {
                        int reg_a = 0;
                        int reg_b = 0;
                        k = parse_reg(line, k, end, &reg_a);
                        k = skip_ws(line, k, end);
                        if (k >= 0 && k < end && line[k] == ',')
                            k = skip_ws(line, k + 1, end);
                        k = parse_reg(line, k, end, &reg_b);
                        uint8_t base = ident_eq(name, name_len, "add") ? 0x20 : 0x30;
                        emit_byte(code, code_len, (uint8_t)(base | ((reg_a & 7) << 3) | (reg_b & 7)));
                    }
                    else if (ident_eq(name, name_len, "jmp"))
                    {
                        const char *tgt = NULL;
                        int tgt_len = 0;
                        k = parse_ident(line, k, end, &tgt, &tgt_len);
                        int addr = (k >= 0) ? find_label(labels, label_count, tgt, tgt_len) : -1;
                        if (addr < 0)
                            addr = 0;
                        emit_byte(code, code_len, 0x40);
                        emit_byte(code, code_len, (uint8_t)(addr & 0xFF));
                    }
                }
            }
        }

        if (*line_end == '\0')
            break;
        p = line_end + 1;
    }
}

int main(void)
{
    const char *source =
        "; Fake assembly\n"
        "start:\n"
        "  mov r1, #5\n"
        "  mov r2, #10\n"
        "  add r1, r2\n"
        "  jmp end\n"
        "loop:\n"
        "  sub r1, r2\n"
        "end:\n"
        "  mov r0, #0\n";

    Label labels[MAX_LABELS];
    int label_count = 0;
    first_pass(source, labels, &label_count);

    uint8_t code[MAX_CODE];
    int code_len = 0;
    second_pass(source, labels, label_count, code, &code_len);

    printf("labels:\n");
    for (int i = 0; i < label_count; ++i)
        printf("  %.*s = %d\n", labels[i].length, labels[i].name, labels[i].addr);

    printf("bytes(%d):\n", code_len);
    for (int i = 0; i < code_len; ++i)
        printf("%02X%s", code[i], (i + 1 == code_len) ? "" : " ");
    printf("\n");
    return 0;
}
