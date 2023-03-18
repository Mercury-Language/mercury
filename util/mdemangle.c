/*
** vim: ft=c ts=4 sw=4 et
*/
/*---------------------------------------------------------------------------*/

/*
** Copyright (C) 1995-2006, 2008 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** File: mdemangle.c
** Author: fjh
**
** A mercury symbol demangler.
** This is used to convert error messages from the linker back
** into a form that users can understand.
**
** This is implemented in C to minimize startup time and memory usage.
**
** BEWARE:
** This code is duplicated in profiler/demangle.m and profiler/mdemangle.m.
** Any changes here will need to be duplicated there and vice versa.
*/

/* mercury_std.h includes mercury_regs.h, and must precede system headers */
#include "mercury_std.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* We used this for the size of fixed-length buffers in a few places <sigh> */
#define MAX_SYMBOL_LENGTH 1000

static void         demangle(const char *name);
static const char   *strip_module_name(char **start_ptr, char *end,
                        const char *special_prefixes[],
                        const char *special_suffixes[]);
static MR_bool      check_for_suffix(char *start, char *position,
                        const char *suffix, int sizeof_suffix, int *mode_num2);
static char         *fix_mangled_ascii(char *str, char **end);
static MR_bool      fix_mangled_special_case(char *str, char **end);
static MR_bool      find_double_underscore(char **str, char *end);
static MR_bool      cut_trailing_integer(char *str, char **end, int *num);
static MR_bool      cut_trailing_underscore_integer(char *str,
                        char **end, int *num);
static MR_bool      strip_prefix(char **str, const char *prefix);
static MR_bool      strip_suffix(const char *str, char **end,
                        const char *suffix);
static MR_bool      strip_leading_integer(char **start_ptr, int *num);

/*
** Bloody SunOS 4.x doesn't have memmove()...
** Using memcpy() may not work, but it doesn't really matter
** if the demangler doesn't work 100% correctly on SunOS 4.x.
*/
#ifndef MR_HAVE_MEMMOVE
#define memmove memcpy
#endif

/*
** This option indicates whether we should output verbose
** explanations of linker error messages.
*/
MR_bool explain_link_errors = MR_FALSE;

/*
** This variable gets set if the symbols MR_grade_* or MR_mercury_grade
** were found.  If it gets set, then we print out the error message below.
*/

char *found_grade_symbol = NULL;
const char probably_grade_error[] =
  "Mercury Linker:\n"
  "\tNote: the symbol `%s' was mentioned.\n"
  "\tAny link errors are most likely due to linking together object\n"
  "\tfiles compiled with different compilation model options.\n"
  "\tTry doing `mmake clean' and then rebuilding.\n";

int
main(int argc, char **argv)
{
    const char *progname = argv[0];

    /* We should use getopt_long(), but for one option, that is overkill. */
    while (argc > 1 && argv[1][0] == '-') {
        if (strcmp(argv[1], "-e") == 0 ||
            strcmp(argv[1], "--explain-link-errors") == 0)
        {
            explain_link_errors = MR_TRUE;
            argc--, argv++;
        } else if (strcmp(argv[1], "--") == 0) {
            argc--, argv++;
            break;
        } else {
            fprintf(stderr, "%s: unknown option `%s'\n", progname, argv[1]);
            exit(1);
        }
    }

    if (argc > 1) {
        int i;

        /*
        ** Invoke demangle() on each command line argument.
        */
        for (i = 1; i < argc; i++) {
            demangle(argv[i]);
            putchar('\n');
        }
    } else {
        /*
        ** Copy stdin to stdout, calling demangle() for every valid
        ** C identifier in the input.
        */

        for (;;) {
            char    buf[MAX_SYMBOL_LENGTH];
            size_t  len;
            int     c;

            len = 0;
            c = getchar();
            while (c != EOF && (isalnum(c) || c == '_')) {
                if (len >= sizeof(buf) - 1) {
                    break;
                }

                buf[len++] = (char) c;
                c = getchar();
            }

            if (len > 0) {
                buf[len] = '\0';
                demangle(buf);
                fflush(stdout);
            }

            if (c == EOF) {
                break;
            }

            putchar(c);
        }
    }

    if (explain_link_errors && found_grade_symbol) {
        printf(probably_grade_error, found_grade_symbol);
        free(found_grade_symbol);
    }

    return 0;
}

/*
** demangle():
** Convert a mangled Mercury identifier into human-readable form
** and then print it to stdout.
*/

static void
demangle(const char *orig_name)
{
    static const char entry[]   = "_entry_";
    static const char mercury[] = "mercury__";
    static const char func_prefix[] = "fn__"; /* added for functions */
    static const char unify1[]   = "__Unify___";
    static const char unify2[]   = "__Unify____";
    static const char compare1[] = "__Compare___";
    static const char compare2[] = "__Compare____";
    static const char index1[]  = "__Index___";
    static const char index2[]  = "__Index____";

    static const char introduced[]  = "IntroducedFrom__";
    static const char deforestation[]  = "DeforestationIn__";
    static const char accumulator[]  = "AccFrom__";
    static const char type_spec[]  = "TypeSpecOf__";
    static const char unused_arg[] = "UnusedArgs__";
    static const char pred[]  = "pred__";
    static const char func[]  = "func__";
    static const char porf[]  = "pred_or_func__";

    /*
    ** XXX This is out-of-date. The compiler now generates names
    ** such as UnusedArgs__p__[1].
    */
    static const char ua_suffix[] = "__ua"; /* added by unused_args.m */
    static const char ua_suffix2[] = "__uab"; /* added by unused_args.m */

    static const char ho_suffix[] = "__ho"; /* added by higher_order.m */

    static const char mercury_common[] = "mercury_common_";
    static const char mercury_data[] = "mercury_data_";
    static const char type_ctor_layout[] = "type_ctor_layout_";
    static const char type_ctor_info[] = "type_ctor_info_";
    static const char type_ctor_functors[] = "type_ctor_functors_";
    static const char base_typeclass_info[] = "base_typeclass_info_";
    static const char underscores_base_typeclass_info[] =
                        "__base_typeclass_info_";
    static const char common[] = "common";
    static const char arity_string[] = "arity";

    static const char MR_grade[] = "MR_grade_";
    static const char MR_runtime_grade[] = "MR_runtime_grade";

    static const char *trailing_context_1[] = {
        introduced,
        deforestation,
        accumulator,
        type_spec,
        unused_arg,
        unify1, compare1, index1,
        NULL
    };

    static const char *trailing_context_1_hl_suffixes[] = {
        ua_suffix,
        ua_suffix2,
        ho_suffix,
        NULL
    };

    static const char *trailing_context_2[] = {
        type_ctor_layout,
        type_ctor_info,
        type_ctor_functors,
        common,
        NULL
    };

    static const char *trailing_context_3[] = {
        arity_string,
        NULL
    };

    char        name[MAX_SYMBOL_LENGTH];
    char        *start = name;
    const char  *module = "";    /* module name */
    char        *end = name + strlen(orig_name);
    char        *position;     /* current position in string */
    int         mode_num;
    int         mode_num2;
    int         arity;
    MR_bool     high_level = MR_TRUE;
    MR_bool     matched = MR_FALSE;
    const char *pred_or_func; /* either "predicate" or "function" */
    MR_bool     unused_args = MR_FALSE;
                /* does this proc have any unused arguments */
    MR_bool     unused_args_extra = MR_FALSE;
                /* __uab suffix rather than __ua */
    int         unused_args_num = 0;
    MR_bool     higher_order = MR_FALSE; /* has this proc been specialized */
    int         higher_order_num = 0;
    int         internal = -1;
    char        *name_before_prefixes = NULL;
    int         lambda_line = 0;
    int         lambda_seq_number = 0;
    char        *lambda_pred_name = NULL;
    char        *end_of_lambda_pred_name = NULL;
    const char  *lambda_kind = NULL;
    enum        { ORDINARY, UNIFY, COMPARE, INDEX,
                LAMBDA, DEFORESTATION, ACCUMULATOR, TYPE_SPEC } category;
    enum        { COMMON, INFO, LAYOUT, FUNCTORS } data_category;
    const char  *class_name;
    int         class_arity;
    char        class_arg_buf[MAX_SYMBOL_LENGTH];
    int         class_arg_num;
    const char  *class_arg;
    const char  *type_spec_sub;

    /*
    ** Copy orig_name to a local buffer which we can modify,
    ** making sure that we don't overflow the buffer.
    */

    if (strlen(orig_name) >= sizeof(name)) {
        goto too_long;
    }
    strcpy(name, orig_name);

    /*
    ** Skip any leading underscore inserted by the C compiler
    ** (but don't skip it if it came from the `_entry_' prefix).
    */
    if (*start == '_' && strncmp(start, entry, strlen(entry)) != 0) {
        start++;
    }

    /* Check for `MR_grade_*' and `MR_runtime_grade'. */
    if (strncmp(start, MR_grade, strlen(MR_grade)) == 0 ||
        strcmp(start, MR_runtime_grade) == 0)
    {
        if (found_grade_symbol == NULL) {
            found_grade_symbol = malloc(strlen(start) + 1);
            if (found_grade_symbol != NULL) {
                strcpy(found_grade_symbol, start);
            }
        }
        goto wrong_format;
    }

    /* Skip the `_entry_' prefix, if any. */
    strip_prefix(&start, entry);

    /* Strip off the `mercury__' prefix, if any. */
    if (strip_prefix(&start, mercury)) {
        matched = MR_TRUE;
    }

/*
** Code for dealing with predicate symbols.
*/

    /*
    ** Get integer from end of string (it might be the mode number,
    ** it might be the internal label number). We'll assume its mode
    ** number for the moment.
    */

    if (!cut_trailing_integer(start, &end, &mode_num)) {
        goto not_plain_mercury;
    }

    if (end == start) {
        goto not_plain_mercury;
    }

    /*
    ** If we got to an `i', that means it is an internal label of the form
    ** `mercury__append_3_0_i1'. In that case, save the internal label number
    ** and then get the mode number.
    */

    if (*--end == 'i') {
        internal = mode_num;
        if (end == start || *--end != '_') {
            goto not_plain_mercury;
        }

        if (!cut_trailing_underscore_integer(start, &end, &mode_num)) {
            goto not_plain_mercury;
        }
    }

    if (end == start) {
        goto not_plain_mercury;
    }

    /* Strip off the `fn__' prefix, if any. */
    if (strip_prefix(&start, func_prefix)) {
        high_level = MR_FALSE;
        pred_or_func = "function";
    } else if (strip_suffix(start, &end, "_f")) {
        high_level = MR_TRUE;
        matched = MR_TRUE;
        pred_or_func = "function";
    } else if (strip_suffix(start, &end, "_p")) {
        high_level = MR_TRUE;
        matched = MR_TRUE;
        pred_or_func = "predicate";
    } else {
        /*
        ** It is not a function. But it could be either an LLDS predicate,
        ** or an MLDS compiler-generated predicate.
        */
        high_level = (strstr(start, unify2) ||
            strstr(start, compare2) ||
            strstr(start, index2));
        pred_or_func = "predicate";
    }

    if (end == start) {
        goto not_plain_mercury;
    }

    /*
    ** Scan back past the arity number and then parse it.
    */

    if (!cut_trailing_underscore_integer(start, &end, &arity)) {
        goto not_plain_mercury;
    }

    if (high_level) {
        module = strip_module_name(&start, end,
            trailing_context_1, trailing_context_1_hl_suffixes);
    }

    /*
    ** Now start processing from the start of the string again. Check whether
    ** the start of the string matches the name of one of the special
    ** compiler-generated predicates; if so, set the `category' to the
    ** appropriate value and then skip past the prefix.
    */

    if (strip_prefix(&start, unify1)) {
        category = UNIFY;
    } else if (strip_prefix(&start, compare1)) {
        category = COMPARE;
        if (mode_num != 0) goto not_plain_mercury;
    } else if (strip_prefix(&start, index1)) {
        category = INDEX;
        if (mode_num != 0) goto not_plain_mercury;
    } else {
        category = ORDINARY;
        /*
        ** For ordinary predicates, we should have matched against something
        ** by now --
        ** either the "mercury__" prefix, for LLDS mangling,
        ** or the "_f" or "_p" suffix, for MLDS mangling.
        */
        if (!matched) {
            goto not_plain_mercury;
        }
    }

    if (category != ORDINARY && start[0] == '_') {
        start++;
    }

    /* Fix any ascii codes mangled in the predicate name. */
    start = fix_mangled_ascii(start, &end);

    /*
    ** Process the mangling introduced by unused_args.m.
    ** This involves stripping off the `__ua<m>' or `__uab<m>' added to
    ** the end of the predicate/function name, where m is the mode number.
    */

    position = end; /* save end of name */

    do {
        if (position == start) {
            goto wrong_format;
        }
        position--;
    } while (MR_isdigit(*position));

    /* get the mode number */
    if (check_for_suffix(start, position, ua_suffix,
        sizeof(ua_suffix), &mode_num2))
    {
        unused_args = MR_TRUE;
        unused_args_extra = MR_FALSE;
        unused_args_num = mode_num;
        end = position + 1 - (sizeof(ua_suffix) - 1);
        mode_num = mode_num2 % 10000;
    } else if (check_for_suffix(start, position, ua_suffix2,
        sizeof(ua_suffix2), &mode_num2))
    {
        unused_args = MR_TRUE;
        unused_args_extra = MR_TRUE;
        unused_args_num = mode_num;
        end = position + 1 - (sizeof(ua_suffix2) - 1);
        mode_num = mode_num2 % 10000;
    }

    /*
    ** Process the mangling introduced by higher_order.m.
    ** This involves stripping off the `__ho<n>' where
    ** n is a unique identifier for this specialized version
    */

    position = end;

    do {
        if (position == start) {
            goto wrong_format;
        }
        position--;
    } while (MR_isdigit(*position));

    if (check_for_suffix(start, position, ho_suffix,
        sizeof(ho_suffix), &higher_order_num))
    {
        end = position + 1 - (sizeof(ho_suffix) - 1);
        higher_order = MR_TRUE;
    }

    /*
    ** Cut off the string before the start of the arity number,
    ** and the unused_args and specialization information,
    ** i.e. at the end of the predicate name or type name.
    */
    *end = '\0';

    /*
    ** Make sure special predicates with unused_args
    ** are reported correctly.
    */

    if (unused_args && category != ORDINARY) {
        if (!cut_trailing_integer(start, &end, &arity)) {
            goto wrong_format;
        }
    }

    if (!high_level) {
        module = strip_module_name(&start, end, trailing_context_1, NULL);
    }

    /*
    ** Look for "IntroducedFrom" or "DeforestationIn" or "AccFrom"
    ** or "TypeSpecOf".
    ** XXX This don't yet handle multiple prefixes. If we get an error after
    ** this point, just treat predicate name as an ordinary predicate.
    */
    name_before_prefixes = start;
    if (category == ORDINARY) {
        if (strip_prefix(&start, introduced)) {
            category = LAMBDA;
        } else if (strip_prefix(&start, deforestation)) {
            category = DEFORESTATION;
        } else if (strip_prefix(&start, accumulator)) {
            category = ACCUMULATOR;
        } else if (strip_prefix(&start, type_spec)) {
            category = TYPE_SPEC;
        }
    }

    if (category == LAMBDA || category == DEFORESTATION ||
        category == ACCUMULATOR || category == TYPE_SPEC)
    {
        if (strip_prefix(&start, pred)) {
            lambda_kind = "pred";
        } else if (strip_prefix(&start, func)) {
            lambda_kind = "func";
        } else if (category == TYPE_SPEC && strip_prefix(&start, porf)) {
            lambda_kind = "";
        } else {
            goto wrong_format;
        }

        lambda_pred_name = start;
        if (!find_double_underscore(&start, end)) {
            category = ORDINARY;
            start = name_before_prefixes;
        } else {
            end_of_lambda_pred_name = start;
            start += 2;
        }
        if (category == TYPE_SPEC) {
            if (start < end && *start == '[') {
                int nest_level;

                nest_level = 1;

                type_spec_sub = start;
                start++;

                /* Handle matched brackets in type names. */
                while (start < end) {
                    if (*start == '[') {
                        nest_level++;
                    }
                    if (*start == ']') {
                        nest_level--;
                    }
                    if (nest_level == 0) {
                        *(start + 1) = '\0';
                        break;
                    }
                    start++;
                }
                if (nest_level != 0) {
                    category = ORDINARY;
                    start = name_before_prefixes;
                } else {
                    /*
                    ** The compiler adds a redundant mode number to the
                    ** predicate name to avoid creating two predicates
                    ** with the same name (deep profiling doesn't like that).
                    ** It isn't used here, so we just ignore it. The compiler
                    ** also adds a version number for the argument order used
                    ** for specialized versions, which can also be ignored.
                    */

                    *end_of_lambda_pred_name = '\0';
                    start = lambda_pred_name;
                }
            } else {
                category = ORDINARY;
                start = name_before_prefixes;
            }
        } else if (category != ORDINARY) {
            lambda_line = 0;

            if (start >= end || !MR_isdigit(*start)) {
                category = ORDINARY;
                start = name_before_prefixes;
            }

            while (start < end && MR_isdigit(*start)) {
                lambda_line = lambda_line * 10 + (*start - '0');
                start++;
            }

            if (strip_prefix(&start, "__")) {
                if (start < end && MR_isdigit(*start)) {
                    lambda_seq_number = 0;
                    while (start < end && MR_isdigit(*start)) {
                        lambda_seq_number =
                            lambda_seq_number * 10 + (*start - '0');
                        start++;
                    }
                    *end_of_lambda_pred_name = '\0';
                } else {
                    category = ORDINARY;
                    start = name_before_prefixes;
                }
            } else {
                category = ORDINARY;
                start = name_before_prefixes;
            }
        }
    }

    /*
    ** Now, finally, we can print the demangled symbol name.
    */

    printf("<");
    switch(category) {
        case UNIFY:
            printf("unification predicate for type '%s.%s'/%d mode %d",
                module, start, arity, mode_num);
            break;

        case COMPARE:
            printf("compare/3 predicate for type '%s.%s'/%d",
                module, start, arity);
            break;

        case INDEX:
            printf("index/2 predicate for type '%s.%s'/%d",
                module, start, arity);
            break;

        case LAMBDA:
            printf("%s goal (#%d) from '%s' in module '%s' line %d",
                lambda_kind, lambda_seq_number,
                lambda_pred_name, module, lambda_line);
            break;

        case ACCUMULATOR:
            printf("accumulator procedure from '%s' in module '%s' line %d",
                lambda_pred_name, module, lambda_line);
            break;

        case DEFORESTATION:
            printf("deforestation procedure (#%d) from '%s' "
                "in module '%s' line %d",
                lambda_seq_number, lambda_pred_name,
                module, lambda_line);
            break;

        case TYPE_SPEC:
        default:
            if (*module == '\0') {
                printf("%s '%s'/%d mode %d",
                    pred_or_func, start, arity, mode_num);
            } else {
                printf("%s '%s.%s'/%d mode %d",
                    pred_or_func, module, start, arity, mode_num);
            }
    }

    if (category == TYPE_SPEC) {
        printf(" (type specialized %s)", type_spec_sub);
    }

    if (higher_order) {
        printf(" (specialized [#%d])", higher_order_num);
    }

    if (unused_args) {
        if (unused_args_extra) {
            printf(" (minus extra unused args [#%d])", unused_args_num);
        } else {
            printf(" (minus unused args [#%d])", unused_args_num);
        }
    }

    if (internal != -1) {
        printf(" label %d", internal);
    }

    printf(">");
    return;

/*
** Code to deal with mercury_data items.
*/

not_plain_mercury:
    /*
    ** Undo any in-place modifications done while trying to demangle
    ** predicate names.
    */

    strcpy(name, orig_name);
    start = name;
    end = name + strlen(name);

    /* Skip any leading underscore inserted by the C compiler. */
    if (*start == '_') {
        start++;
    }

    if (strip_prefix(&start, mercury_common)) {
        if (!strip_leading_integer(&start, &arity)) {
            goto wrong_format;
        }
        printf("<shared constant number %d>", arity);
        return;
    }

    if (strip_prefix(&start, mercury_data)) {
        /* LLDS */
        high_level = MR_FALSE;
        if (strip_prefix(&start, base_typeclass_info)) {
            goto typeclass_info;
        }
        /* Also try the old format, in case we're demangling old files. */
        if (strip_prefix(&start, underscores_base_typeclass_info)) {
            goto typeclass_info;
        }
    } else {
        /* MLDS */
        high_level = MR_TRUE;
        if (strip_prefix(&start, base_typeclass_info)) {
            goto typeclass_info;
        }
        strip_prefix(&start, mercury);
    }

    module = strip_module_name(&start, end, trailing_context_2, NULL);
    if (high_level) {
        /*
        ** For MLDS, the module name gets duplicated (XXX why?)
        ** So here we must replace `foo.foo' with just `foo'.
        */
        size_t half_len;

        half_len = strlen(module) / 2;
        if (strncmp(module, module + half_len + 1, half_len) != 0) {
            goto wrong_format;
        }
        module += half_len + 1;
    }

    if (strip_prefix(&start, type_ctor_info)) {
        data_category = INFO;
        if (!cut_trailing_underscore_integer(start, &end, &arity)) {
            goto wrong_format;
        }
    } else if (strip_prefix(&start, type_ctor_layout)) {
        data_category = LAYOUT;
        if (!cut_trailing_underscore_integer(start, &end, &arity)) {
            goto wrong_format;
        }
    } else if (strip_prefix(&start, type_ctor_functors)) {
        data_category = FUNCTORS;
        if (!cut_trailing_underscore_integer(start, &end, &arity)) {
            goto wrong_format;
        }
    } else if (strip_prefix(&start, common)) {
        data_category = COMMON;
        if (!cut_trailing_underscore_integer(start, &end, &arity)) {
            goto wrong_format;
        }
    } else {
        goto wrong_format;
    }

    start = fix_mangled_ascii(start, &end);

    switch (data_category) {
        case INFO:
            if (*module == '\0') {
                printf("<type_ctor_info for type '%s'/%d>",
                    start, arity);
            } else {
                printf("<type_ctor_info for type '%s.%s'/%d>",
                    module, start, arity);
            }
            break;

        case LAYOUT:
            if (*module == '\0') {
                printf("<type_ctor_layout for type '%s'/%d>",
                    start, arity);
            } else {
                printf("<type_ctor_layout for type '%s.%s'/%d>",
                    module, start, arity);
            }
            break;

        case FUNCTORS:
            if (*module == '\0') {
                printf("<type_ctor_functors for type '%s'/%d>",
                    start, arity);
            } else {
                printf("<type_ctor_functors for type '%s.%s'/%d>",
                    module, start, arity);
            }
            break;

        case COMMON:
            printf("<shared constant number %d for module %s>",
                arity, module);
            break;
        
        default:
            goto wrong_format;
    }

    return;

typeclass_info:
    /*
    ** Parse the class name and class arity, which have the following layout:
    ** <module-qualified class name>__arity<arity>__
    */

    class_name = strip_module_name(&start, end, trailing_context_3, NULL);
    /* XXX fix_mangled_ascii() */
    if (!(strip_prefix(&start, arity_string)
        && strip_leading_integer(&start, &class_arity)
        && strip_prefix(&start, "__")))
    {
        goto wrong_format;
    }

    /*
    ** Parse the class argument types, which each have the following layout:
    ** <module-qualified type name>__arity<arity>__
    **
    ** We store the human-readable formatted output in class_arg_buf as we go.
    */

    fix_mangled_ascii(start, &end);
    strcpy(class_arg_buf, "");
    for (class_arg_num = 0; class_arg_num < class_arity; class_arg_num++) {
        if (class_arg_num != 0) {
            strcat(class_arg_buf, ", ");
        }
        class_arg = strip_module_name(&start, end, trailing_context_3, NULL);
        if (!(strip_prefix(&start, arity_string)
            && strip_leading_integer(&start, &arity)
            && strip_prefix(&start, "__")))
        {
            goto wrong_format;
        }

        sprintf(class_arg_buf + strlen(class_arg_buf), "%s/%d",
            class_arg, arity);
    }

    /* Now print the results. */
    printf("<instance declaration for %s(%s)>", class_name, class_arg_buf);
    return;

wrong_format:
    strcpy(name, orig_name);
    start = name;
    end = name + strlen(name);
    start = fix_mangled_ascii(start, &end);
    fputs(name, stdout);
    return;

too_long:
    fputs(orig_name, stdout);
    return;
} /* end demangle() */

/*
** Remove a module name prefix.
** Just keep munching up double-underscores until we get to something
** that matches the specified trailing context, at which point we stop,
** or until there are no double-underscores left.
*/

static const char *
strip_module_name(char **start_ptr, char *end,
    const char *special_prefixes[], const char *special_suffixes[])
{
    const char  *module;                    /* module name */
    char        *module_end;                /* end of the module name */
    char        *next_double_underscore;
    char        *start;

    start = *start_ptr;

    /*
    ** Strip off the module name
    */
    module = start;
    module_end = start;
    while ((next_double_underscore = strstr(start, "__")) != NULL) {
        int     len;
        int     i;
        MR_bool stop;

        /* Check for special cases. */
        stop = MR_FALSE;
        for (i = 0; special_prefixes[i] != NULL; i++) {
            if (strncmp(start, special_prefixes[i],
                strlen(special_prefixes[i])) == 0)
            {
                stop = MR_TRUE;
            }
        }
        for (i = 0; special_suffixes != NULL && special_suffixes[i] != NULL;
            i++)
        {
            if (strncmp(next_double_underscore, special_suffixes[i],
                strlen(special_suffixes[i])) == 0)
            {
                stop = MR_TRUE;
            }
        }

        if (stop) {
            break;
        }

        len = next_double_underscore - start;
        if (module != module_end) {
            /*
            ** Append a module qualifier, and shift the module name
            ** into the right place.
            */

            *module_end = '.';
            module_end++;
            memmove(module_end, start, len);
        }

        module_end += len;
        start = next_double_underscore + 2;
    }

    if (module == module_end) {
        module = "";
    } else {
        *module_end = '\0';
    }

    *start_ptr = start;
    return module;
}

/*
** Remove the prefix from a string, if it has it.
** Returns MR_TRUE if the string has that prefix, and *str will then point
** to the rest of that string. If the string doesn't have that prefix,
** *str will be unchanged, and the function will return MR_FALSE.
*/

static MR_bool
strip_prefix(char **str, const char *prefix)
{
    size_t len;

    len = strlen(prefix);

    if (strncmp(*str, prefix, len) == 0) {
        *str += len;
        return MR_TRUE;
    }

    return MR_FALSE;
}

/*
** Remove the suffix from a string, if it has it.
** Returns MR_TRUE if the string between start and *end has the specified
** suffix, and sets *end to point to the beginning of the suffix.
*/

static MR_bool
strip_suffix(const char *start, char **end, const char *suffix)
{
    size_t len;

    len = strlen(suffix);

    if (*end - start >= len && strncmp(*end - len, suffix, len) == 0) {
        *end -= len;
        return MR_TRUE;
    }

    return MR_FALSE;
}

/*
** If the string pointed to by *start_ptr starts with an integer,
** then advance *start_ptr past the leading integer, store the value
** of the integer in the int pointed to by `num', and return true;
** otherwise leave *start_ptr unchanged and return false.
** (The string itself is always left unchanged.)
*/

static MR_bool
strip_leading_integer(char **start_ptr, int *num)
{
    char    *start;
    char    save_char;
    MR_bool got_int;

    start = *start_ptr;
    while(MR_isdigit(*start)) {
        start++;
    }

    if (start == *start_ptr) {
        return MR_FALSE;
    }

    save_char = *start;
    *start = '\0';
    got_int = (sscanf(*start_ptr, "%d", num) == 1);
    *start = save_char;

    if (got_int) {
        *start_ptr = start;
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
}

/*
** Remove trailing integer (at the supplied `real_end' of the string),
** and return it in the int pointed to by `num'. We return true if there is
** an integer at the end, and false if there is not. If we return false,
** the string will not be cut. `real_end' is updated with the new end
** of the string.
**
** Requires *str to contain more than just a number; doesn't work
** if the trailing integer starts at the first character of str.
*/

static MR_bool
cut_trailing_integer(char *str, char **real_end, int *num)
{
    char    *end;

    end = *real_end;
    do {
        if (end == str) {
            return MR_FALSE;
        }
        end--;
    } while (MR_isdigit(*end));

    if (sscanf(end + 1, "%d", num) != 1) {
        return MR_FALSE;
    }

    *++end = '\0';
    *real_end = end;

    return MR_TRUE;
}

/*
** Same as cut_trailing_integer, but move end back past the underscore as well.
** If cut_trailing_underscore_integer returns MR_TRUE, the `real_end' will be
** moved back before the underscore and the integer. If it returns MR_FALSE,
** the `real_end' is unchanged.
*/

static MR_bool
cut_trailing_underscore_integer(char *str, char **real_end, int *num)
{
    char    *end;

    end = *real_end;
    if (!cut_trailing_integer(str, &end, num)) {
        return MR_FALSE;
    }

    if (end == str || *(--end) != '_') {
        return MR_FALSE;
    }

    *end = '\0';
    *real_end = end;
    return MR_TRUE;
}

/*
** Scan for `__' and return a pointer to the first `_'.
** Returns MR_TRUE if `__' was found, MR_FALSE otherwise.
*/

static MR_bool
find_double_underscore(char **start, char *end)
{
    char    *str;

    str = *start;
    while (*str != '_' || *(str + 1) != '_') {
        if (str == end) {
            return MR_FALSE;
        }
        str++;
    }

    *start = str;
    return MR_TRUE;
}

/*
** The compiler changes all names starting with `f_' so that they start with
** `f__' instead, and uses names starting with `f_' for mangled names
** which are either descriptions (such as `f_greater_than' for `>')
** or sequences of decimal representations of ASCII codes separated by
** underscores. If the name starts with `f__', we must change it back to
** start with `f_'. Otherwise, if it starts with `f_' we must convert
** the mnemonic or list of ASCII codes back into an identifier.
*/

static char *
fix_mangled_ascii(char *str, char **real_end)
{
    char    *end;

    end = *real_end;

    /*
    ** If it starts with `f__', replace that with `f_'.
    */
    if (strncmp(str, "f__" , 3) == 0) {
        str++;
        *str = 'f';
        return str;
    }

    /*
    ** If it starts with `f_' followed by a mnemonic description,
    ** then replace that with its unmangled version
    */
    if (strncmp(str, "f_", 2) == 0 &&
        fix_mangled_special_case(str, real_end))
    {
        return str;
    }

    /*
    ** Otherwise, if it starts with `f_' we must convert the list of
    ** ASCII codes back into an identifier.
    */
    if (strncmp(str, "f_", 2) == 0) {
        char    buf[MAX_SYMBOL_LENGTH];
        char    *num;
        int     count;

        num = str + 2;
        count = 0;
        while (num < end) {
            char    *next_num;

            next_num = num;
            while (MR_isdigit(*next_num)) {
                next_num++;
            }

            if (*next_num != '_' && *next_num != '\0') {
                break;
            }

            *next_num = '\0';
            buf[count++] = atoi(num);
            num = next_num + 1;
        }

        /* Copy anything after the mangled string. */
        while (num < end) {
            buf[count++] = *num++;
        }

        buf[count] = '\0';
        strcpy(str, buf);
        *real_end = str + count;
    }
    return str;
}

static MR_bool
fix_mangled_special_case(char *str, char **real_end)
{
    static const struct {
        const char *mangled_name;
        const char *unmangled_name;
    } translations[] = {
        /*
        ** Beware: we assume that the unmangled name is always shorter
        ** than the mangled name.
        */
        { "f_not_equal", "\\=" },
        { "f_greater_or_equal", ">=" },
        { "f_less_or_equal", "=<" },
        { "f_equal", "=" },
        { "f_less_than", "<" },
        { "f_greater_than", ">" },
        { "f_plus", "+" },
        { "f_times", "*" },
        { "f_minus", "-" },
        { "f_slash", "/" },
        { "f_comma", "," },
        { "f_semicolon", ";" },
        { "f_cut", "!" },
        { "f_tuple", "{}" },
        { "f_cons", "[|]" },
        { "f_nil", "[]" }
    };
    const int   num_translations =
                    sizeof(translations) / sizeof(translations[0]);

    int         i;

    /*
    ** Check for the special cases listed in the table above.
    */
    for (i = 0; i < num_translations; i++) {
        const char  *mangled;
        size_t      mangled_len;

        mangled = translations[i].mangled_name;
        mangled_len = strlen(mangled);
        if (strncmp(str, mangled, mangled_len) == 0) {
            const char  *unmangled;
            size_t      unmangled_len;
            size_t      leftover_len;

            unmangled = translations[i].unmangled_name;
            unmangled_len = strlen(unmangled);
            leftover_len = strlen(str) - mangled_len;
            assert(unmangled_len <= mangled_len);

            strcpy(str, unmangled);
            memmove(str + unmangled_len, str + mangled_len, leftover_len + 1);

            *real_end = str + unmangled_len + leftover_len;
            return MR_TRUE;
        }
    }
    return MR_FALSE;
}

static MR_bool
check_for_suffix(char *start, char *position, const char *suffix,
    int sizeof_suffix, int *mode_num2)
{
    const int suffix_len = sizeof_suffix - 1;

    return (
        position - suffix_len >= start
        && sscanf(position + 1, "%d", mode_num2) == 1
        && strncmp(position - suffix_len + 1, suffix, suffix_len) == 0
    );
}

/*---------------------------------------------------------------------------*/
