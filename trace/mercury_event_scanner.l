%{
// vim: ts=4 sw=4 expandtab ft=lex

// Copyright (C) 2006-2007, 2009 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Scanner for solver event specifications.

#ifndef __USE_SVID
#define __USE_SVID
#endif
#ifndef __USE_POSIX
#define __USE_POSIX
#endif
#ifndef __USE_XOPEN_EXTENDED
#define __USE_XOPEN_EXTENDED
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#ifndef __EXTENSIONS__
#define __EXTENSIONS__
#endif

#include <stdio.h>
#include <string.h>
#include "mercury_conf.h"               // for MR_HAVE_UNISTD_H
#include "mercury_misc.h"               // for MR_fatal_error
#include "mercury_trace_term.h"         // for MR_Cterm etc

#include "mercury_event_spec.h"
#include "mercury_event_spec_missing.h"
#include "mercury_event_parser.h"

#ifndef MR_HAVE_UNISTD_H
#define YY_NO_UNISTD_H
#endif

// We should #include mercury_event_scanner.h as well, but flex puts into the
// header file it creates a #undef for a macro it needs, so #including
// mercury_event_scanner.h leads to compilation failure.

#undef  mercury_event_yywrap
#define YY_NO_UNPUT
#define YY_INPUT(buf, result, max_size)                             \
        do {                                                        \
            result = MR_event_get_input(buf, max_size);             \
        } while (0)

extern  void    mercury_event_init(void);

const char      *mercury_event_filename = "no input file";
int             mercury_event_linenum = 1;

// Add the declarations for local functions that flex is too lazy to add.
//
// These functions should be static, but flex defines them as non-static,
// and we cannot declare them to be static without risking error messages
// from mgnuc about mixing static declarations with non-static definitions.

int     mercury_event_get_lineno(void);
FILE    *mercury_event_get_in(void);
FILE    *mercury_event_get_out(void);
// Flex can generate different return values for the this function
// depending on the platform, so we don't provide a declaration to avoid
// potential compilation errors.
// int  mercury_event_get_leng(void);

char    *mercury_event_get_text(void);
void    mercury_event_set_lineno(int line_number);
void    mercury_event_set_in(FILE *in_str);
void    mercury_event_set_out(FILE *out_str);
int     mercury_event_get_debug(void);
void    mercury_event_set_debug(int bdebug);
int     mercury_event_lex_destroy(void);
%}

alpha   [a-zA-Z_]
digit   [0-9]
alnum   [a-zA-Z_0-9]
sp      [ \t]
nl      [\n\f]
nonl    [^\n\f]

sc      "/*"
ec      "*/"
string  \"[^"]*\"
inside  [^*]|("*"[^/])|{string}
comment {sc}{inside}*{ec}

%pointer
%option noyywrap

%%

"event"             {
                        mercury_event_lval.Uline = mercury_event_linenum;
                        return TOKEN_EVENT;
                    }
"set"               { return TOKEN_SET;                 }
"impure"            { return TOKEN_IMPURE;              }
"function"          { return TOKEN_FUNCTION;            }
"synthesized"       { return TOKEN_SYNTHESIZED;         }
"by"                { return TOKEN_BY;                  }

"("                 { return TOKEN_LPAREN;              }
")"                 { return TOKEN_RPAREN;              }
","                 { return TOKEN_COMMA;               }
":"                 {
                        mercury_event_lval.Uline = mercury_event_linenum;
                        return TOKEN_COLON;
                    }

{alpha}{alnum}*     {
                        mercury_event_lval.Uid = strdup(yytext);
                        if (mercury_event_lval.Uid == NULL) {
                            MR_fatal_error("out of memory");
                        }

                        return TOKEN_ID;
                    }

{alpha}{alnum}*("."{alpha}{alnum}*)* {
                        mercury_event_lval.Uid = strdup(yytext);
                        if (mercury_event_lval.Uid == NULL) {
                            MR_fatal_error("out of memory");
                        }

                        return TOKEN_SYM;
                    }

"%"{nonl}*{nl}      { mercury_event_linenum++;          }
{comment}           {
                        const char  *s;

                        for (s = yytext; *s != '\0'; s++) {
                            if (*s == '\n') {
                                mercury_event_linenum++;
                            }
                        }
                    }

{sp}+               {}
{nl}                { mercury_event_linenum++;          }

{nonl}              { return GARBAGE;                   }

%%

void mercury_event_init(void)
{
}
