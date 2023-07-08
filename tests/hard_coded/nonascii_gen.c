// vim: ts=4 sw=4 expandtab ft=c

#include <stdio.h>

size_t
MR_utf8_encode(char s_[], int c)
{
    unsigned char *s = (unsigned char *)s_;
    unsigned int uc = c;

    if (uc <= 0x7f) {
        s[0] = uc;
        return 1;
    }

    if (uc <= 0x7ff) {
        s[0] = 0xC0 | ((uc >> 6) & 0x1F);
        s[1] = 0x80 |  (uc       & 0x3F);
        return 2;
    }

    if (uc <= 0xffff) {
        s[0] = 0xE0 | ((uc >> 12) & 0x0F);
        s[1] = 0x80 | ((uc >>  6) & 0x3F);
        s[2] = 0x80 |  (uc        & 0x3F);
        return 3;
    }

    if (uc <= 0x10ffff) {
        s[0] = 0xF0 | ((uc >> 18) & 0x07);
        s[1] = 0x80 | ((uc >> 12) & 0x3F);
        s[2] = 0x80 | ((uc >>  6) & 0x3F);
        s[3] = 0x80 |  (uc        & 0x3F);
        return 4;
    }

    /* Otherwise is illegal. */
    return 0;
}

void
put_utf8(int c)
{
    char    s[4];
    size_t  n;
    size_t  i;

    n = MR_utf8_encode(s, c);
    for (i = 0; i < n; i++) {
        putchar(s[i]);
    }
}

int
main(void)
{
    int repeat;
    int i;

    for (repeat = 0; repeat < 2; repeat++) {
        for (i = 1; i < 256; i++) {
            if (i != '\n') {
                put_utf8(i);
            }
        }

        put_utf8(0x0007ff);
        put_utf8(0x000800);
        put_utf8(0x00fffd);
        /* U+FFFF is invalid */
        put_utf8(0x010000);
        put_utf8(0x10fffd);
        /* U+10FFFF is invalid */
        put_utf8('\n');
    }
    return 0;
}
