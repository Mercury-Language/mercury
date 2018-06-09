// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1994,1997,2000, 2005-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// file: timing.c
// main authors: fjh
//
//  Timing routines.

#include "mercury_imp.h"

#ifdef MR_HAVE_SYS_TIMES_H
  #include <sys/times.h>        // for times() and `struct tms'
#endif

#ifdef MR_HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

#ifdef MR_HAVE_TIME_H
  #include <time.h>
#endif

#ifdef MR_WIN32_GETPROCESSTIMES
  #include "mercury_windows.h"
#endif

#include "mercury_timing.h"

int
MR_get_user_cpu_milliseconds(void)
{
#if defined(MR_WIN32_GETPROCESSTIMES)
    #define FILETIME_TO_MILLISEC(time, msec)                            \
    do                                                                  \
    {                                                                   \
        SYSTEMTIME tmp;                                                 \
        FileTimeToSystemTime(&time, &tmp);                              \
        msec = tmp.wMilliseconds + 1000 *                               \
            (tmp.wSecond + 60 * (tmp.wMinute + 60 * tmp.wHour));        \
    } while (0)

    FILETIME    creation_time;
    FILETIME    exit_time;
    FILETIME    kernel_time;
    FILETIME    user_time;
    int         user_msec;
    int         kernel_msec;

    GetProcessTimes(GetCurrentProcess(),
        &creation_time, &exit_time, &kernel_time, &user_time);
    FILETIME_TO_MILLISEC(user_time, user_msec);
    FILETIME_TO_MILLISEC(kernel_time, kernel_msec);
    return user_msec + kernel_msec;
#elif defined(MR_HAVE_SYS_TIMES_H)
  #ifdef MR_CLOCK_TICKS_PER_SECOND
    const double    ticks_per_millisecond = MR_CLOCK_TICKS_PER_SECOND / 1000.0;
    struct tms t;

    if (times(&t) == -1) {
        return -1;
    }

    return (int) (t.tms_utime / ticks_per_millisecond);
  #else
    return -1;
  #endif
#else
    return -1;
#endif
}

int
MR_get_real_milliseconds(void)
{
#if defined(MR_HAVE_GETTIMEOFDAY)
    struct timeval  tv;

    if (gettimeofday(&tv, NULL) == -1) {
        return -1;
    }

    return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
#else
    return -1;
#endif
}
