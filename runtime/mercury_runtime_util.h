/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_RUNTIME_UTIL_H
#define	MERCURY_RUNTIME_UTIL_H

#include	<stdio.h>

#ifndef MR_HAVE_STRERROR
extern	char	*strerror(int errnum);
#endif

extern	FILE	*MR_checked_fopen(const char *filename, const char *message,
			const char *mode);
extern	void	MR_checked_fclose(FILE *file, const char *filename);
extern	void	MR_checked_atexit(void (*func)(void));

#endif	/* MERCURY_RUNTIME_UTIL_H */
