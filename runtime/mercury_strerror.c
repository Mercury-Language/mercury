/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Apparently SunOS 4.1.3 doesn't have strerror()
**	(!%^&!^% non-ANSI systems, grumble...)
*/

#include <stdio.h>			/* for NULL */
#include "mercury_conf.h"		/* for (possibly) HAVE_STRERROR */
#include "mercury_strerror.h"

#ifndef HAVE_STRERROR

extern	int	sys_nerr;
extern	char	*sys_errlist[];

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
