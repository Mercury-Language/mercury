/*
** Copyright (C) 1993-1994,1997,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: timing.c
** main authors: fjh
**
**	Timing routines.
*/

#include "mercury_imp.h"

#ifdef HAVE_SYS_TIMES_H
  #include <sys/times.h>		/* for times() and `struct tms' */
#endif

#ifdef MR_WIN32_GETPROCESSTIMES
  #include <windows.h>
#endif

#include "mercury_timing.h"

int
MR_get_user_cpu_miliseconds(void)
{
#ifndef MR_CLOCK_TICKS_PER_SECOND
  #ifdef MR_WIN32_GETPROCESSTIMES
    #define FILETIME_TO_MILLISEC(ST, Msec)				\
	do								\
	{								\
	  SYSTEMTIME tmp;						\
	  FileTimeToSystemTime(&ST, &tmp);				\
	  Msec = tmp.wMilliseconds +					\
	    1000 * (tmp.wSecond + 60 * (tmp.wMinute + 60 * tmp.wHour));	\
	} while(0)

	FILETIME CreationTime;
	FILETIME ExitTime;
	FILETIME KernelTime;
	FILETIME UserTime;
	int UserMsec, KernelMsec;
	
	GetProcessTimes(GetCurrentProcess(),
					&CreationTime, &ExitTime,
					&KernelTime, &UserTime);
	FILETIME_TO_MILLISEC(UserTime, UserMsec);
	FILETIME_TO_MILLISEC(KernelTime, KernelMsec);
	return UserMsec + KernelMsec;
  #else
	return -1;
  #endif
#else
	const double ticks_per_milisecond = MR_CLOCK_TICKS_PER_SECOND / 1000.0;
	struct tms t;

	if (times(&t) == -1) {
		return -1;
	}
	return (int) (t.tms_utime / ticks_per_milisecond);
#endif
}
