/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Apparently SunOS 4.1.3 doesn't have strerror()
**	(!%^&!^% non-ANSI systems, grumble...)
*/

#ifndef	MERCURY_STRERROR_H
#define	MERCURY_STRERROR_H

#ifndef HAVE_STRERROR

extern	char	*strerror(int errnum);

#endif	/* HAVE_STRERROR */

#endif	/* MERCURY_STRERROR_H */
