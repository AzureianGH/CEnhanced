#include <stdio.h>
#include <stdint.h>
#include <string.h>

static void print_bytes(const char *label, const char *s)
{
    size_t len = strlen(s);
    printf("%s\n", label);
    printf("len=%zu\n", len);
    printf("bytes:");
    for (size_t i = 0; i < len; ++i)
    {
        unsigned char b = (unsigned char)s[i];
        printf(" %02X", b);
    }
    printf("\n\n");
}

int main(void)
{
    const char *s1 = "A\nB\tC\\D\"E";
    const char *s2 = "sum = 2 + 3\r\n";
    print_bytes("s1", s1);
    print_bytes("s2", s2);
    return 0;
}
