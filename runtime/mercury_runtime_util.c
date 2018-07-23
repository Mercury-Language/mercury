// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2002, 2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module contains utility functions for the rest of the Mercury runtime.
//
// Author: petdr

#include    "mercury_imp.h"
#include    "mercury_runtime_util.h"

#include    <stdio.h>
#include    <string.h>

#ifdef MR_HAVE_UNISTD_H
  #include  <unistd.h>
#endif

#include    <errno.h>

static void
generic_strerror(char *buf, size_t buflen, int errnum)
{
    MR_snprintf(buf, buflen, "Error %d", errnum);
}

const char *
MR_strerror(int errnum, char *buf, size_t buflen)
{
#if defined(MR_HAVE_STRERROR_S) && !defined(MR_MINGW)
    // MSVC has strerror_s. It also exists in C11 Annex K and is enabled by
    // defining a preprocessor macro __STDC_WANT_LIB_EXT1__
    //
    // On MinGW-w64, strerror_s results in an undefined reference to strerror_s
    // in MSVCRT.DLL on Windows XP. Avoid it until we drop support for XP.

    if (strerror_s(buf, buflen, errnum) != 0) {
        generic_strerror(buf, buflen, errnum);
    }
    return buf;
#elif defined(MR_HAVE_STRERROR_R)
    // The XSI-compliant and Mac OS X strerror_r populates buf unless it fails.
    // The GNU-specific strerror_r does not always populate buf.

  #if !defined(__GNU_LIBRARY__) || \
       ((_POSIX_C_SOURCE >= 200112L || _XOPEN_SOURCE >= 600) && ! _GNU_SOURCE)
    int x = strerror_r(errnum, buf, buflen);
    if (x != 0) {
        generic_strerror(buf, buflen, errnum);
    }
    return buf;
  #else
    const char *s = strerror_r(errnum, buf, buflen);
    return s;
  #endif
#else
    // Fallback using deprecated variables. This is used on MinGW at least.
    //
    // strerror_l is another thread-safe alternative, specified in POSIX.
    // It is locale-sensitive and takes a locale argument so we don't use it
    // for now.

    if (errnum >= 0 && errnum < sys_nerr && sys_errlist[errnum] != NULL) {
        return sys_errlist[errnum];
    } else {
        generic_strerror(buf, buflen, errnum);
        return buf;
    }
#endif
}

FILE *
MR_checked_fopen(const char *filename, const char *message, const char *mode)
{
    FILE *file;
    char errbuf[MR_STRERROR_BUF_SIZE];

    errno = 0;
    file = fopen(filename, mode);
    if (file == NULL) {
        fprintf(stderr, "Mercury runtime: couldn't %s file `%s': %s\n",
            message, filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
        exit(EXIT_FAILURE);
    }
    return file;
}

void
MR_checked_fclose(FILE *file, const char *filename)
{
    char errbuf[MR_STRERROR_BUF_SIZE];

    errno = 0;
    if (fclose(file) != 0) {
        fprintf(stderr, "Mercury runtime: error closing file `%s': %s\n",
            filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
        exit(EXIT_FAILURE);
    }
}

void
MR_checked_atexit(void (*func)(void))
{
    char errbuf[MR_STRERROR_BUF_SIZE];

    errno = 0;
    if (atexit(func) != 0) {
        fprintf(stderr, "Mercury runtime: error in call to atexit: %s\n",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
        exit(EXIT_FAILURE);
    }
}

#if ! defined(MR_HAVE_PUTENV) && defined(MR_HAVE__PUTENV)
  #define putenv _putenv
#endif

int
MR_setenv(const char *name, const char *value, int overwrite)
{
#if defined(MR_HAVE_SETENV)
    return setenv(name, value, overwrite);
#elif defined(MR_HAVE_PUTENV) || defined(MR_HAVE__PUTENV)
    char *env;
    int length;
    int result;

    if (!overwrite && getenv(name) != NULL) {
        return 0;
    }

    length = strlen(name) + strlen(value) + 2;
    env = MR_NEW_ARRAY(char, length);

    env[0] = '\0';
    strcat(env, name);
    strcat(env, "=");
    strcat(env, value);

    result = putenv(env);

    MR_free(env);

    return result;
#else
  #error "MR_setenv: unable to define"
#endif
}
