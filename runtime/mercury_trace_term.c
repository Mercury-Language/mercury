/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains code to manage terms, for conditional breakpoints.
**
** Author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace_term.h"
#include <stdlib.h>

static  MR_CArgs    MR_create_cargs(char *str, char **rest);
static  void        MR_delete_cargs(MR_CArgs args);

/**************************************************************************/

MR_CTerm
MR_create_cterm(char *str, char **rest)
{
    MR_CTerm    term;
    MR_CArgs    args;
    int         i;

    /*
    ** We don't have to worry about skipping white space, because there
    ** won't be any, except inside strings.
    */

    if (str[0] == '"') {
        i = 1;
        while (str[i] != '\0' && str[i] != '"') {
            i++;
        }

        if (str[i] == '"') {
            term = MR_malloc(sizeof(struct MR_CTerm_Struct));
            term->term_functor = &str[0];
            term->term_args = NULL;
            *rest = &str[i + 1];
            return term;
        }

        return NULL;
    }

    if (! MR_isalnumunder(str[0]) || MR_isupper(str[0])) {
        return NULL;
    }

    i = 0;
    while (MR_isalnumunder(str[i])) {
        i++;
    }

    term = MR_malloc(sizeof(struct MR_CTerm_Struct));

    if ((str[i] == '\0') || str[i] == ',' || str[i] == ')') {
        term->term_functor = str;
        term->term_args = NULL;
        *rest = &str[i];
        return term;
    } else if (str[i] == '(') {
        str[i] = '\0';
        term->term_functor = str;
        args = MR_create_cargs(&str[i + 1], rest);
        if (args == NULL) {
            MR_free(term);
            return NULL;
        }

        term->term_args = args;
        return term;
    }

    MR_free(term);
    return NULL;
}

static MR_CArgs
MR_create_cargs(char *str, char **rest)
{
    char            *more;
    MR_CTerm        term;
    MR_CArgs        args;
    MR_CArgs        tail;
    int             i;

    term = MR_create_cterm(str, &more);
    if (term == NULL) {
        return NULL;
    }

    args = MR_malloc(sizeof(struct MR_CArgs_Struct));
    args->args_head = term;

    if (more[0] == ')') {
        more[0] = '\0'; /* to terminate the just previous functor, if any */
        args->args_tail = NULL;
        *rest = &more[1];
        return args;
    } else if (more[0] == ',') {
        more[0] = '\0'; /* to terminate the just previous functor, if any */
        i = 1;
        while (more[i] != '\0' && MR_isspace(more[i])) {
            i++;
        }

        if (more[i] == '\0') {
            MR_free(args);
            return NULL;
        }

        tail = MR_create_cargs(more + i, rest);
        if (tail == NULL) {
            MR_free(args);
            return NULL;
        }

        args->args_tail = tail;
        return args;
    }

    MR_free(args);
    return NULL;
}

void
MR_print_cterm(FILE *fp, MR_CTerm term)
{
    MR_CArgs    args;
    int         i;

    fprintf(fp, "%s", term->term_functor);
    if (term->term_args != NULL) {
        fprintf(fp, "(");
        i = 0;
        args = term->term_args;
        while (args != NULL) {
            if (i > 0) {
                fprintf(fp, ", ");
            }
            MR_print_cterm(fp, args->args_head);
            args = args->args_tail;
            i++;
        }
        fprintf(fp, ")");
    }
}

void
MR_delete_cterm(MR_CTerm term)
{
    if (term != NULL) {
        MR_delete_cargs(term->term_args);
        MR_free(term);
    }
}

static void
MR_delete_cargs(MR_CArgs args)
{
    if (args != NULL) {
        MR_delete_cargs(args->args_tail);
        MR_delete_cterm(args->args_head);
        MR_free(args);
    }
}
