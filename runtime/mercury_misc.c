// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1996-2000,2002, 2006, 2010-2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include    "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include  "mercury_imp.h"
#endif
#include    "mercury_string.h"
#include    "mercury_misc.h"
#include    "mercury_array_macros.h"
#include    "mercury_runtime_util.h"

#include    <stdio.h>
#include    <stdarg.h>
#include    <errno.h>

static void MR_print_warning(const char *prog, const char *fmt, va_list args);
static void MR_do_perror(const char *prog, const char *message);

void
MR_warning(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    MR_print_warning("Mercury runtime", fmt, args);
    va_end(args);
}

void
MR_mdb_warning(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    MR_print_warning("mdb", fmt, args);
    va_end(args);
}

static void
MR_print_warning(const char *prog, const char *fmt, va_list args)
{
    fflush(stdout);     // In case stdout and stderr are the same.

    fprintf(stderr, "%s: ", prog);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

    fflush(stderr);
}

void
MR_perror(const char *message)
{
    MR_do_perror("Mercury runtime", message);
}

void
MR_mdb_perror(const char *message)
{
    MR_do_perror("mdb", message);
}

static void
MR_do_perror(const char *prog, const char *message)
{
    int saved_errno;

    saved_errno = errno;
    fflush(stdout);     // In case stdout and stderr are the same.

    fprintf(stderr, "%s: ", prog);
    errno = saved_errno;
    perror(message);
}

// XXX will need to modify this to kill other threads if MR_THREAD_SAFE
// (and cleanup resources, etc....)

void
MR_fatal_error(const char *fmt, ...)
{
    va_list args;
    int error = errno;
    char errbuf[MR_STRERROR_BUF_SIZE];

    fflush(stdout);     // In case stdout and stderr are the same.

    if (error != 0) {
        fprintf(stderr, "Errno = %d: %s\n", error,
            MR_strerror(error, errbuf, sizeof(errbuf)));
    }
    fprintf(stderr, "Mercury runtime: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");

#ifndef MR_HIGHLEVEL_CODE
    MR_trace_report(stderr);
#endif

    fflush(NULL);       // Flushes all stdio output streams.

    exit(EXIT_FAILURE);
}

void
MR_external_fatal_error(const char *locn, const char *fmt, ...)
{
    va_list args;

    fflush(stdout);     // In case stdout and stderr are the same.

    fprintf(stderr, "%s: ", locn);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");

#ifndef MR_HIGHLEVEL_CODE
    MR_trace_report(stderr);
#endif

    fflush(NULL);       // Flushes all stdio output streams.

    exit(EXIT_FAILURE);
}

typedef struct {
    void    (*func)(void *);
    void    *data;
} MR_cleanup_record;

static  MR_cleanup_record   *MR_cleanup_records = NULL;
static  int                 MR_cleanup_record_next = 0;
static  int                 MR_cleanup_record_max = 0;

#define INIT_CLEANUP_RECORD_ARRAY_SIZE  10

void
MR_register_exception_cleanup(void (*func)(void *), void *data)
{
    MR_ensure_room_for_next(MR_cleanup_record, MR_cleanup_record,
        INIT_CLEANUP_RECORD_ARRAY_SIZE);
    MR_cleanup_records[MR_cleanup_record_next].func = func;
    MR_cleanup_records[MR_cleanup_record_next].data = data;
    MR_cleanup_record_next++;
}

void
MR_perform_registered_exception_cleanups(void)
{
    int i;

    for (i = 0; i < MR_cleanup_record_next; i++) {
        MR_cleanup_records[i].func(MR_cleanup_records[i].data);
    }
}
