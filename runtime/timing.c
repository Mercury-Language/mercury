/*---------------------------------------------------------------------------*/
/* I hacked this from toplev.c in the gcc source - fjh. */
/* further hacked for Solaris 2 by zs */
/*---------------------------------------------------------------------------*/

#include "timing.h"
#include <stdio.h>

/* Copyright (C) 1987, 1988, 1989, 1992, 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef __svr4
#include <sys/times.h>
#include <limits.h>
#else
#ifdef USG
#undef FLOAT
#include <sys/param.h>
/* This is for hpux.  It is a real screw.  They should change hpux.  */
#undef FLOAT
#include <sys/times.h>
#include <time.h>   /* Correct for hpux at least.  Is it good on other USG?  */
#undef FFS  /* Some systems define this in param.h.  */
#else
#ifndef VMS
#include <sys/time.h>
#include <sys/resource.h>
#endif
#endif
#endif

/* Return time used so far, in microseconds.  */

int get_run_time (void)
{
#ifdef __svr4
  struct tms tms;
#else
#ifdef USG
  struct tms tms;
#else
#ifndef VMS
  struct rusage rusage;
#else /* VMS */
  struct
    {
      int proc_user_time;
      int proc_system_time;
      int child_user_time;
      int child_system_time;
    } vms_times;
#endif
#endif
#endif

#ifdef __svr4
  times (&tms);
  return (tms.tms_utime /* + tms.tms_stime */) * (1000000 / _sysconf(3));
#else
#ifdef USG
  times (&tms);
  return (tms.tms_utime /* + tms.tms_stime */) * (1000000 / HZ);
#else
#ifndef VMS
  getrusage (0, &rusage);
  return (rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec
	  /* + rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec */);
#else /* VMS */
  times (&vms_times);
  return (vms_times.proc_user_time /* + vms_times.proc_system_time */) * 10000;
#endif
#endif
#endif
}

void print_time (const char *str, int total)
{
  fprintf (stderr,
	   "%s = %d.%06d\n",
	   str, total / 1000000, total % 1000000);
}
