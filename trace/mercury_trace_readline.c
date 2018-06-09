// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2005-2007 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// A simple interface to read a line, normally done using GNU readline.
//
// Main authors: fjh, zs.

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_memory.h"
#include "mercury_std.h"
#include "mercury_wrapper.h"

#include "mercury_trace_readline.h"
#include "mercury_trace_completion.h"

#ifndef MR_NO_USE_READLINE
  #ifdef MR_HAVE_READLINE_READLINE_H
    #include <readline/readline.h>
  #else
    extern FILE         *rl_instream;
    extern FILE         *rl_outstream;
    extern char         (*rl_completion_entry_function)(const char *, int);
    extern const char   *rl_readline_name;
    extern void         (*rl_prep_term_function)(int);
    extern void         (*rl_deprep_term_function)(void);
  #endif
  #ifdef MR_HAVE_READLINE_HISTORY_H
    #include <readline/history.h>
  #endif
  #ifdef MR_HAVE_UNISTD_H
     #include <unistd.h>    // for isatty()
  #endif
#endif

#include <stdio.h>
#include <stdlib.h>

// The initial size of the array of characters used to hold the line.
#define MR_INIT_BUF_LEN     80

static  void    MR_dummy_prep_term_function(int ignored);
static  void    MR_dummy_deprep_term_function(void);

char *
MR_trace_readline(const char *prompt, FILE *in, FILE *out)
{
#if (defined(isatty) || defined(MR_HAVE_ISATTY)) \
 && (defined(fileno) || defined(MR_HAVE_FILENO)) \
 && !defined(MR_NO_USE_READLINE)
    char    *line;
    MR_bool in_isatty;
    char    *last_nl;

    in_isatty = isatty(fileno(in));
    if (in_isatty || MR_force_readline) {
        rl_instream = in;
        rl_outstream = out;

        // The cast to (void *) silences a spurious "assignment from
        // incompatible pointer type" warning (old versions of readline
        // are very sloppy about declaring the types of function pointers).

        rl_completion_entry_function = (void *) &MR_trace_line_completer;
        rl_readline_name = "mdb";

        if (!in_isatty) {
            // This is necessary for tests/debugger/completion, otherwise
            // we get lots of messages about readline not being able to get
            // the terminal settings. This is possibly a bit flaky, but it is
            // only used by our tests.

            rl_prep_term_function = (void *) MR_dummy_prep_term_function;
            rl_deprep_term_function = (void *) MR_dummy_deprep_term_function;
        }

        // If the prompt contains newlines then readline doesn't
        // display it properly.

        last_nl = strrchr(prompt, '\n');
        if (last_nl != NULL) {
            char    *real_prompt;
            char    *pre_prompt;

            real_prompt = (char *) MR_malloc(strlen(last_nl));
            strcpy(real_prompt, last_nl + 1);
            pre_prompt = (char *) MR_malloc(last_nl - prompt + 2);
            strncpy(pre_prompt, prompt, last_nl - prompt + 1);
            pre_prompt[last_nl - prompt + 1] = '\0';
            fputs(pre_prompt, out);
            line = readline((char *) real_prompt);
            MR_free(real_prompt);
            MR_free(pre_prompt);
        } else {
            line = readline((char *) prompt);
        }

        // readline() allocates with malloc(), and we want to return something
        // allocated with MR_malloc(), but that's OK, because MR_malloc() and
        // malloc() are interchangeable.

#if 0
        {
            char *tmp = line;

            line = MR_copy_string(line);
            free(tmp);
        }
#endif

        if (line != NULL && line[0] != '\0') {
            add_history(line);
        }

        return line;
    }
#endif // have isatty && have fileno && !MR_NO_USE_READLINE

    // Otherwise, don't use readline.
    fprintf(out, "%s", prompt);
    fflush(out);
    return MR_trace_readline_raw(in);
}

char *
MR_trace_readline_raw(FILE *fp)
{
    char    *contents;
    int     content_max;
    int     c;
    int     i;

    contents = NULL;
    content_max = 0;

    i = 0;
    while ((c = getc(fp)) != EOF && c != '\n') {
        MR_ensure_big_enough(i, content, char, MR_INIT_BUF_LEN);
        contents[i++] = c;
    }

    if (c == '\n' || i > 0) {
        MR_ensure_big_enough(i, content, char, MR_INIT_BUF_LEN);
        contents[i] = '\0';
        return contents;
    } else {
        MR_free(contents);
        return NULL;
    }
}

char *
MR_trace_readline_from_script(FILE *fp, char **args, int num_args)
{
    char    *line = NULL;
    size_t  line_length;
    int     line_index;
    size_t  expanded_line_length;
    char    *expanded_line;
    int     expanded_line_index;
    int     arg_num;
    size_t  arg_length;
    char    *arg;

    do {
        if (line != NULL) {
            MR_free(line);
        }
        line = MR_trace_readline_raw(fp);
        if (line == NULL) {
            return NULL;
        }
        // Ignore lines starting with '#'.

    } while (*line == '#');

    line_length = strlen(line);

    expanded_line_length = line_length;
    expanded_line = (char*) MR_malloc(line_length + 1);
    expanded_line[0] = '\0';
    expanded_line_index = 0;

    for (line_index = 0; line_index < line_length; line_index++) {
        if ((line[line_index] == '$') &&
            (line_index < (line_length - 1)) &&
            (line[line_index + 1] >= '1') &&
            (line[line_index + 1] <= '9'))
        {
            arg_num = (int)(line[line_index + 1] - '1');
            if (arg_num < num_args) {
                arg = args[arg_num];
                arg_length = strlen(arg);
                // Subtract 2 for the "$n" which will not occur
                // in the expanded string.

                expanded_line_length += arg_length - 2;
                expanded_line = MR_realloc(expanded_line,
                    expanded_line_length + 1);
                expanded_line[expanded_line_index] = '\0';
                strcat(expanded_line, arg);
                expanded_line_index += arg_length;
            }
            // Skip the digit after the '$'.
            line_index++;
        } else {
            expanded_line[expanded_line_index] = line[line_index];
            expanded_line_index++;
        }
    }

    MR_free(line);
    expanded_line[expanded_line_index] = '\0';

    return expanded_line;
}

static void
MR_dummy_prep_term_function(int ignored)
{
}

static void
MR_dummy_deprep_term_function(void)
{
}
