/*
** vim: ft=c ts=4 sw=4 et
*/
/*---------------------------------------------------------------------------*/

/*
** Copyright (C) 2010 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** File: mfiltercc.c
** Author: wangp.
**
** This is a last ditch effort to filter out warning messages from the
** C compiler that we cannot (yet) figure out how to silence in a better way.
*/

#include <string.h>
#include <stdio.h>

#define MAX_LINE_LENGTH 2000

static int
drop_line(const char *line, size_t len);

int
main(void)
{
    char    buf[MAX_LINE_LENGTH];
    size_t  len;
    int     c;

    do {
        len = 0;
        c = getchar();
        while (c != EOF) {
            buf[len++] = c;
            if (c == '\n' || len >= sizeof(buf) - 1) {
                break;
            }
            c = getchar();
        }

        if (len > 0) {
            buf[len] = '\0';
            if (!drop_line(buf, len)) {
                printf("%s", buf);
            }
        }
    } while (c != EOF);

    return 0;
}

static int
drop_line(const char *line, size_t len)
{
    /*
    ** gcc 4.x produces the message (in English locales):
    ** foo.c:42: warning: 'a' used but never defined
    */
    const char      msg[] = " used but never defined\n";
    const size_t    msglen = sizeof(msg) - 1;
    int             skip;

    skip = len - msglen;
    return (skip > 0) && memcmp(line + skip, msg, msglen) == 0;
}

/*---------------------------------------------------------------------------*/
