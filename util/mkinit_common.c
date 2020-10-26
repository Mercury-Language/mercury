/*
** vim:sw=4 ts=4 expandtab
*/
/*
** Copyright (C) 1995-2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** File: mkinit_common.c
** Main authors: zs, fjh
**
** Common code previously shared by mkinit.c and mkinit_erl.c.
**
*/

/* mercury_std.h includes mercury_regs.h, and must precede system headers */
#include    "mercury_conf.h"
#include    "mercury_std.h"
#include    "mercury_array_macros.h"
#include    "mkinit_common.h"

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <ctype.h>
#include    <errno.h>

#ifdef MR_HAVE_SYS_STAT_H
  #include  <sys/stat.h>
#endif

#ifdef MR_HAVE_UNISTD_H
  #include  <unistd.h>
#endif

/* --- global variables --- */

const char          *MR_progname = NULL;
int                 num_errors = 0;
int                 num_files;
char                **files = NULL;

static int          size_of_files;

    /* List of directories to search for init files */
static String_List  *init_file_dirs = NULL;

    /* Pointer to tail of the init_file_dirs list */
static String_List  **init_file_dirs_tail = &init_file_dirs;

/* --- adjustable limits --- */
#define MAXFILENAME 4096    /* maximum file name length           */
#define NUMFILES    128     /* initial size of files array        */
#define FACTOR      2       /* factor to increase files array by  */

/* --- function prototypes --- */

static  char        *find_init_file(const char *base_name);
static  MR_bool     file_exists(const char *filename);

/*---------------------------------------------------------------------------*/

void
process_file_list_file(char *filename)
{
    FILE *fp;
    char *line;

    fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "%s: error opening file `%s': %s\n",
            MR_progname, filename, strerror(errno));
        num_errors++;
        return;
    }
    /* initialize the files structure, if required */
    if (files == NULL) {
        num_files = 0;
        size_of_files = NUMFILES;
        files = (char **) checked_malloc(sizeof(char *) * NUMFILES);
    }

    while ((line = read_line(filename, fp, MAXFILENAME)) != NULL) {
        /* Ignore blank lines */
        if (line[0] != '\0') {
            if (num_files >= size_of_files) {
                size_of_files *= FACTOR;
                files = (char **)
                    checked_realloc(files, size_of_files * sizeof(char *));

                if (files == NULL) {
                    fprintf(stderr, "%s: unable to realloc\n", MR_progname);
                    exit(EXIT_FAILURE);
                }
            }

            files[num_files] = line;
            num_files++;
        }
    }
}

/*---------------------------------------------------------------------------*/

/*
** If the `-o' option was used to specify the output file,
** and the file name specified is not `-' (which we take to mean stdout),
** then reassign stdout to the specified file.
*/

void
set_output_file(const char *output_file_name)
{
    if (output_file_name != NULL) {
        FILE *result = freopen(output_file_name, "w", stdout);
        if (result == NULL) {
            fprintf(stderr,
                "%s: error opening output file `%s': %s\n",
                MR_progname, output_file_name, strerror(errno));
            exit(EXIT_FAILURE);
        }
    }
}

/*---------------------------------------------------------------------------*/

/*
** Add the directory name to the end of the search path for `.init' files.
*/

void
add_init_file_dir(const char *dir_name)
{
    String_List *tmp_slist;

    tmp_slist = (String_List *) checked_malloc(sizeof(String_List));
    tmp_slist->next = NULL;
    tmp_slist->data = (char *) checked_malloc(strlen(dir_name) + 1);
    strcpy(tmp_slist->data, dir_name);
    *init_file_dirs_tail = tmp_slist;
    init_file_dirs_tail = &tmp_slist->next;
}

/*
** Scan the list of files for ones not found in the current directory,
** and replace them with their full path equivalent if they are found
** in the list of search directories.
*/

void
do_path_search(char **lfiles, int lnum_files)
{
    int     filenum;
    char    *init_file;

    for (filenum = 0; filenum < lnum_files; filenum++) {
        init_file = find_init_file(lfiles[filenum]);
        if (init_file != NULL) {
            lfiles[filenum] = init_file;
        }
    }
}

/*
** Search the init file directory list to locate the file.
** If the file is in the current directory or is not in any of the
** search directories, then return NULL.  Otherwise return the full
** path name to the file.
**
** It is the caller's responsibility to free the returned buffer
** holding the full path name when it is no longer needed.
*/

static char *
find_init_file(const char *base_name)
{
    char        *filename;
    char        *dirname;
    String_List *dir_ptr;
    size_t      dirlen;
    size_t      baselen;
    size_t      len;

    if (file_exists(base_name)) {
        /* File is in current directory, so no search required */
        return NULL;
    }

    baselen = strlen(base_name);

    for (dir_ptr = init_file_dirs; dir_ptr != NULL; dir_ptr = dir_ptr->next) {
        dirname = dir_ptr->data;
        dirlen = strlen(dirname);
        len = dirlen + 1 + baselen;

        filename = (char *) checked_malloc(len + 1);
        strcpy(filename, dirname);
        filename[dirlen] = '/';
        strcpy(filename + dirlen + 1, base_name);

        if (file_exists(filename)) {
            return filename;
        }

        free(filename);
    }

    /* Did not find file */
    return NULL;
}

/*
** Check whether a file exists.
*/

static MR_bool
file_exists(const char *filename)
{
#ifdef MR_HAVE_SYS_STAT_H
    struct stat buf;

    return (stat(filename, &buf) == 0);
#else
    FILE        *f;

    f = fopen(filename, "rb");
    if (f != NULL) {
        fclose(f);
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
#endif
}

/*---------------------------------------------------------------------------*/

/*
** Read a line from a file, and return a pointer to a malloc'd buffer
** holding the line (without the final newline). If EOF occurs on a
** nonempty line, treat the EOF as a newline; if EOF occurs on an empty
** line, return NULL.
*/

char *
read_line(const char *filename, FILE *fp, size_t max)
{
    char    *buf;
    int     c;
    size_t  i;

    buf = checked_malloc(max + 1);
    i = 0;
    while ((c = getc(fp)) != EOF && c != '\n') {
        if (i >= max) {
            fprintf(stderr, "%s: line too long in file `%s'\n",
                MR_progname, filename);
            num_errors++;
            return NULL;
        }

        buf[i++] = (char) c;
    }

    if (c == '\n' || i > 0) {
        if (i >= max) {
            fprintf(stderr, "%s: line too long in file `%s'\n",
                MR_progname, filename);
            num_errors++;
            return NULL;
        }

        buf[i] = '\0';
        return buf;
    } else {
        free(buf);
        return NULL;
    }
}

int
get_line(FILE *file, char *line, int line_max)
{
    int c;
    int num_chars;
    int limit;

    num_chars = 0;
    limit = line_max - 2;
    while ((c = getc(file)) != EOF && c != '\n') {
        if (num_chars < limit) {
            line[num_chars++] = (char) c;
        }
    }

    if (c == '\n' || num_chars > 0) {
        line[num_chars++] = '\n';
    }

    line[num_chars] = '\0';

#ifdef  CHECK_GET_LINE
    if (check_fp != NULL) {
        fprintf(check_fp, "%s", line);
    }
#endif

    return num_chars;
}

/*---------------------------------------------------------------------------*/

void *
checked_malloc(size_t size)
{
    void    *ptr;

    ptr = malloc(size);
    if (ptr == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

void *
checked_realloc(void *old_ptr, size_t size)
{
    void    *ptr;

    ptr = realloc(old_ptr, size);
    if (ptr == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

char *
checked_strdup(const char *str)
{
    char    *mem;

    mem = checked_malloc(strlen(str) + 1);
    strcpy(mem, str);
    return mem;
}

char *
checked_strdupcat(const char *str, const char *suffix)
{
    char    *mem;

    mem = checked_malloc(strlen(str) + strlen(suffix) + 1);
    strcpy(mem, str);
    strcat(mem, suffix);
    return mem;
}

/*---------------------------------------------------------------------------*/

#ifndef MR_HAVE_STRERROR

/*
** Apparently SunOS 4.1.3 doesn't have strerror()
** (!%^&!^% non-ANSI systems, grumble...)
**
** This code is duplicated in runtime/mercury_prof.c.
*/

extern int sys_nerr;
extern char *sys_errlist[];

char *
strerror(int errnum)
{
    if (errnum >= 0 && errnum < sys_nerr && sys_errlist[errnum] != NULL) {
        return sys_errlist[errnum];
    } else {
        static char buf[30];

        sprintf(buf, "Error %d", errnum);
        return buf;
    }
}

#endif

/*---------------------------------------------------------------------------*/
