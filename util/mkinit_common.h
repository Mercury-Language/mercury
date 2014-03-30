/*
** vim:sw=4 ts=4 expandtab
*/
/*
** Copyright (C) 1995-2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

#ifndef MKINIT_COMMON_H
#define MKINIT_COMMON_H

#include    <stdio.h>

/*
** mercury_array_macros.h uses the MR_NEW_ARRAY and MR_RESIZE_ARRAY macros.
*/

#define MR_NEW_ARRAY(type, num) \
        ((type *) malloc((num) * sizeof(type)))

#define MR_RESIZE_ARRAY(ptr, type, num) \
        ((type *) realloc((ptr), (num) * sizeof(type)))

/* --- adjustable limits --- */
#define MAXLINE     256 /* maximum number of characters per line */
                        /* (characters after this limit are ignored) */

/* --- used to collect a list of strings --- */

typedef struct String_List_struct {
    char                        *data;
    struct String_List_struct   *next;
} String_List;

/* --- global variables --- */

extern const char   *MR_progname;
extern int          num_errors;
extern int          num_files;
extern char         **files;

/* --- function prototypes --- */

extern  void        process_file_list_file(char *filename);
extern  void        set_output_file(const char *output_file_name);
extern  void        add_init_file_dir(const char *dir_name);
extern  void        do_path_search(char **lfiles, int lnum_files);
extern  char        *read_line(const char *filename, FILE *fp, size_t max);
extern  int         get_line(FILE *file, char *line, int line_max);
extern  void        *checked_malloc(size_t size);
extern  void        *checked_realloc(void *old_ptr, size_t size);
extern  char        *checked_strdup(const char *str);
extern  char        *checked_strdupcat(const char *str, const char *suffix);

#ifndef MR_HAVE_STRERROR
extern  char        *strerror(int errnum);
#endif

#endif /* MKINIT_COMMON_H */
