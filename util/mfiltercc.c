//---------------------------------------------------------------------------//
// vim: ft=c ts=4 sw=4 et
//---------------------------------------------------------------------------//
//
// Copyright (C) 2010, 2012 The University of Melbourne.
// This file may only be copied under the terms of the GNU General
// Public License - see the file COPYING in the Mercury distribution.
//
// File: mfiltercc.c
// Author: wangp.
//
// This is a last ditch effort to filter out warning messages from the
// C compiler that we cannot (yet) figure out how to silence in a better way.
//
// This program must *not* #include any of the header files in the runtime
// directory.
//
//---------------------------------------------------------------------------//

#include <string.h>
#include <stdio.h>

#define MAX_LINE_LENGTH 2000

static int  drop_line(const char *line);

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
            buf[len++] = (char) c;
            if (c == '\n' || len >= sizeof(buf) - 1) {
                break;
            }
            c = getchar();
        }

        if (len > 0) {
            buf[len] = '\0';
            if (!drop_line(buf)) {
                printf("%s", buf);
            }
        }
    } while (c != EOF);

    return 0;
}

static int
drop_line(const char *line)
{
    // gcc 4.x produces the message (in English locales, with varying quotes):
    // foo.c:42: warning: 'mercury__foo__...' used but never defined
    //
    // gcc 4.6 onwards also add " [enabled by default]"
    const char *p;

    p = strstr(line, "mercury__");
    if (p == NULL) {
        return 0;
    }
    return strstr(p, " used but never defined") != NULL;
}

//---------------------------------------------------------------------------//
