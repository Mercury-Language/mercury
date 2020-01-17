// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2005-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains code to manage terms, for conditional breakpoints.
//
// Author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_trace_term.h"
#include <stdlib.h>

static  MR_CTerm    MR_parse_cterm(char *str, char endchar, char **rest,
                        MR_bool *mismatch, char **error);
static  MR_CTerm    MR_parse_clist_tail(char *str, char **rest,
                        char *left_bracket, MR_bool *mismatch, char **error);
static  MR_CArgs    MR_parse_cargs(char *str, char endchar, char **rest,
                        char *left_paren, MR_bool *mismatch, char **error);
static  void        MR_delete_cargs(MR_CArgs args);

static  void        MR_print_clist_tail(FILE *fp, MR_CTerm term);

static  MR_bool     MR_cterm_is_nil(MR_CTerm term);
static  MR_bool     MR_cterm_is_cons(MR_CTerm term,
                        MR_CTerm *head_ptr, MR_CTerm *tail_ptr);

static  MR_CTerm    MR_make_nil_cterm(void);
static  MR_CTerm    MR_make_cons_cterm(MR_CTerm head_term, MR_CTerm tail_term);

static  char        *MR_copy_str_fragment(const char *original,
                        int string_length);

////////////////////////////////////////////////////////////////////////////

MR_CTerm
MR_create_cterm(char *str, char **rest, MR_bool *mismatch, char **error)
{
    *mismatch = MR_FALSE;
    *error = NULL;
    return MR_parse_cterm(str, '\0', rest, mismatch, error);
}

MR_CTerm
MR_parse_cterm(char *str, char endchar, char **rest,
    MR_bool *mismatch, char **error)
{
    MR_CTerm    term;
    MR_CArgs    args;
    char        *functor;
    char        *more;
    int         i;

    while (str[0] != '\0' && MR_isspace(str[0])) {
        str++;
    }

    if (str[0] == '[') {
        MR_CTerm    head_term;
        MR_CTerm    tail_term;

        if (str[1] == ']') {
            *rest = &str[2];
            return MR_make_nil_cterm();
        }

        if (str[1] == '\0') {
            *mismatch = MR_TRUE;
            *error = &str[0];
            return NULL;
        }

        // The EBNF grammar for nonempty lists that our code follows is:
        //
        // list ::= "[" term {"," term}* {"|" term}? "]"
        //
        // The initial "[" and the first term are read here; all the rest
        // are read by MR_parse_clist_tail.

        head_term = MR_parse_cterm(&str[1], ']', &more, mismatch, error);
        if (head_term == NULL) {
            // MR_parse_cterm has already set *mismatch and *error.
            return NULL;
        }

        tail_term = MR_parse_clist_tail(&more[0], rest, &str[0],
            mismatch, error);
        if (tail_term == NULL) {
            // MR_parse_clist_tail has already set *mismatch and *error.
            MR_delete_cterm(head_term);
            return NULL;
        }

        return MR_make_cons_cterm(head_term, tail_term);
    }

    if (str[0] == '"') {
        i = 1;
        while (str[i] != '\0' && str[i] != '"') {
            i++;
        }

        if (str[i] == '"') {
            term = MR_malloc(sizeof(struct MR_CTerm_Struct));
            // Include the double quotes in the function symbol.
            term->MR_term_functor = MR_copy_str_fragment(&str[0], i + 1);
            term->MR_term_args = NULL;
            *rest = &str[i + 1];
            return term;
        }

        *mismatch = MR_TRUE;
        *error = &str[0];
        return NULL;
    }

    if (str[0] == '\'') {
        i = 1;
        while (str[i] != '\0' && str[i] != '\'') {
            i++;
        }

        if (str[i] != '\'') {
            *mismatch = MR_TRUE;
            *error = &str[0];
            return NULL;
        }

        // Don't include the single quotes in the function symbol.
        functor = MR_copy_str_fragment(&str[1], i - 1);
        more = &str[i + 1];
    } else if (MR_isalnumunder(str[0]) && !MR_isupper(str[0])) {
        i = 0;
        while (MR_isalnumunder(str[i])) {
            i++;
        }

        functor = MR_copy_str_fragment(&str[0], i);
        more = &str[i];
    } else {
        *mismatch = MR_FALSE;
        *error = &str[0];
        return NULL;
    }

    i = 0;
    while (more[i] != '\0' && MR_isspace(more[i])) {
        i++;
    }

    if ((more[i] == '\0') || (more[i] == ',') || (more[i] == endchar)) {
        term = MR_malloc(sizeof(struct MR_CTerm_Struct));
        term->MR_term_functor = functor;
        term->MR_term_args = NULL;
        *rest = &more[i];
        return term;
    } else if (more[i] == '(') {
        args = MR_parse_cargs(&more[i + 1], ')', rest, &more[i],
            mismatch, error);
        if (args == NULL) {
            // MR_parse_cargs has already set *mismatch and *error.
            MR_free(functor);
            return NULL;
        }

        term = MR_malloc(sizeof(struct MR_CTerm_Struct));
        term->MR_term_functor = functor;
        term->MR_term_args = args;
        return term;
    }

    *mismatch = MR_FALSE;
    *error = &more[i];
    return NULL;
}

static MR_CTerm
MR_parse_clist_tail(char *str, char **rest, char *left_bracket,
    MR_bool *mismatch, char **error)
{
    char        *more;
    int         i;

    while (str[0] != '\0' && MR_isspace(str[0])) {
        str++;
    }

    if (str[0] == ']') {
        *rest = &str[1];
        return MR_make_nil_cterm();
    } else if (str[0] == '|') {
        MR_CTerm    term;

        i = 1;
        while (str[i] != '\0' && MR_isspace(str[i])) {
            i++;
        }

        term = MR_parse_cterm(&str[i], ']', &more, mismatch, error);
        if (term == NULL) {
            // MR_parse_cterm has already set *mismatch and *error.
            return NULL;
        }

        i = 0;
        while (more[i] != '\0' && MR_isspace(more[i])) {
            i++;
        }

        if (more[i] != ']') {
            *mismatch = MR_TRUE;
            *error = left_bracket;
            return NULL;
        }

        *rest = &more[i+1];
        return term;
    } else if (str[0] == ',') {
        MR_CTerm    head_term;
        MR_CTerm    tail_term;

        i = 1;
        while (str[i] != '\0' && MR_isspace(str[i])) {
            i++;
        }

        head_term = MR_parse_cterm(&str[i], ']', &more, mismatch, error);
        if (head_term == NULL) {
            // MR_parse_cterm has already set *mismatch and *error.
            return NULL;
        }

        i = 0;
        while (more[i] != '\0' && MR_isspace(more[i])) {
            i++;
        }

        if (more[i] == '\0') {
            *mismatch = MR_TRUE;
            *error = left_bracket;
            MR_delete_cterm(head_term);
            return NULL;
        }

        tail_term = MR_parse_clist_tail(&more[i], rest, left_bracket,
            mismatch, error);
        if (tail_term == NULL) {
            // MR_parse_clist_tail has already set *mismatch and *error.
            MR_delete_cterm(head_term);
            return NULL;
        }

        return MR_make_cons_cterm(head_term, tail_term);
    }

    *mismatch = MR_TRUE;
    *error = left_bracket;
    return NULL;
}

static MR_CArgs
MR_parse_cargs(char *str, char endchar, char **rest,
    char *left_paren, MR_bool *mismatch, char **error)
{
    char            *more;
    MR_CTerm        term;
    MR_CArgs        args;
    MR_CArgs        tail;
    int             i;

    term = MR_parse_cterm(str, endchar, &more, mismatch, error);
    if (term == NULL) {
        // MR_parse_cterm has already set *mismatch and *error.
        return NULL;
    }

    while (more[0] != '\0' && MR_isspace(more[0])) {
        more++;
    }

    if (more[0] == endchar) {
        args = MR_malloc(sizeof(struct MR_CArgs_Struct));
        args->MR_args_head = term;
        args->MR_args_tail = NULL;
        if (endchar == '\0') {
            *rest = &more[0];
        } else {
            *rest = &more[1];
        }
        return args;
    } else if (more[0] == ',') {
        i = 1;
        while (more[i] != '\0' && MR_isspace(more[i])) {
            i++;
        }

        if (more[i] == '\0') {
            *mismatch = MR_TRUE;
            *error = left_paren;
            MR_delete_cterm(term);
            return NULL;
        }

        tail = MR_parse_cargs(more + i, endchar, rest, left_paren,
            mismatch, error);
        if (tail == NULL) {
            // MR_parse_cargs has already set *mismatch and *error.
            MR_delete_cterm(term);
            return NULL;
        }

        args = MR_malloc(sizeof(struct MR_CArgs_Struct));
        args->MR_args_head = term;
        args->MR_args_tail = tail;
        return args;
    }

    if (more[0] == '\0') {
        *mismatch = MR_TRUE;
        *error = left_paren;
    } else {
        *mismatch = MR_FALSE;
        *error = &more[0];
    }

    MR_delete_cterm(term);
    return NULL;
}

void
MR_print_cterm(FILE *fp, MR_CTerm term)
{
    MR_CTerm    head;
    MR_CTerm    tail;

    if (MR_cterm_is_nil(term)) {
        fprintf(fp, "[]");
    } else if (MR_cterm_is_cons(term, &head, &tail)) {
        fprintf(fp, "[");
        MR_print_cterm(fp, head);
        MR_print_clist_tail(fp, tail);
        fprintf(fp, "]");
    } else {
        MR_CArgs    args;
        int         i;

        fprintf(fp, "%s", term->MR_term_functor);
        if (term->MR_term_args != NULL) {
            fprintf(fp, "(");
            i = 0;
            args = term->MR_term_args;
            while (args != NULL) {
                if (i > 0) {
                    fprintf(fp, ", ");
                }
                MR_print_cterm(fp, args->MR_args_head);
                args = args->MR_args_tail;
                i++;
            }
            fprintf(fp, ")");
        }
    }
}

static void
MR_print_clist_tail(FILE *fp, MR_CTerm term)
{
    MR_CTerm    head;
    MR_CTerm    tail;

    if (MR_cterm_is_nil(term)) {
        return;
    } else if (MR_cterm_is_cons(term, &head, &tail)) {
        fprintf(fp, ", ");
        MR_print_cterm(fp, head);
        MR_print_clist_tail(fp, tail);
    } else {
        fprintf(fp, " | ");
        MR_print_cterm(fp, term);
    }
}

static MR_bool
MR_cterm_is_nil(MR_CTerm term)
{
    if (term == NULL) {
        return MR_FALSE;
    }

    if (MR_strdiff(term->MR_term_functor, "[]")) {
        return MR_FALSE;
    }

    if (term->MR_term_args != NULL) {
        return MR_FALSE;
    }

    return MR_TRUE;
}

static MR_bool
MR_cterm_is_cons(MR_CTerm term, MR_CTerm *head_ptr, MR_CTerm *tail_ptr)
{
    MR_CArgs    args1;
    MR_CArgs    args2;
    MR_CArgs    args3;

    if (term == NULL) {
        return MR_FALSE;
    }

    if (MR_strdiff(term->MR_term_functor, "[|]")) {
        return MR_FALSE;
    }

    args1 = term->MR_term_args;
    if (args1 == NULL) {
        return MR_FALSE;
    }

    args2 = args1->MR_args_tail;
    if (args2 == NULL) {
        return MR_FALSE;
    }

    args3 = args2->MR_args_tail;
    if (args3 != NULL) {
        return MR_FALSE;
    }

    *head_ptr = args1->MR_args_head;
    *tail_ptr = args2->MR_args_head;
    return MR_TRUE;
}

void
MR_delete_cterm(MR_CTerm term)
{
    if (term != NULL) {
        MR_free(term->MR_term_functor);
        MR_delete_cargs(term->MR_term_args);
        MR_free(term);
    }
}

static void
MR_delete_cargs(MR_CArgs args)
{
    if (args != NULL) {
        MR_delete_cargs(args->MR_args_tail);
        MR_delete_cterm(args->MR_args_head);
        MR_free(args);
    }
}

static MR_CTerm
MR_make_nil_cterm(void)
{
    MR_CTerm    term;

    term = MR_malloc(sizeof(struct MR_CTerm_Struct));
    term->MR_term_functor = MR_copy_str_fragment("[]", 2);
    term->MR_term_args = NULL;
    return term;
}

static MR_CTerm
MR_make_cons_cterm(MR_CTerm head_term, MR_CTerm tail_term)
{
    MR_CTerm    term;
    MR_CArgs    args1;
    MR_CArgs    args2;

    args2 = MR_malloc(sizeof(struct MR_CArgs_Struct));
    args2->MR_args_head = tail_term;
    args2->MR_args_tail = NULL;

    args1 = MR_malloc(sizeof(struct MR_CArgs_Struct));
    args1->MR_args_head = head_term;
    args1->MR_args_tail = args2;

    term = MR_malloc(sizeof(struct MR_CTerm_Struct));
    term->MR_term_functor = MR_copy_str_fragment("[|]", 3);
    term->MR_term_args = args1;

    return term;
}

// Make an MR_malloc'd copy of a fragment of a string: string_length bytes
// starting at *original. Don't assume the original string is NULL-terminated.

static char *
MR_copy_str_fragment(const char *original, int string_length)
{
    char    *copy;

    copy = MR_malloc(string_length + 1);
    MR_memcpy(copy, original, string_length);
    copy[string_length] = '\0';
    return copy;
}
