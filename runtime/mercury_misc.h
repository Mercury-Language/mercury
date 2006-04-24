/*
** Copyright (C) 1995-2000,2002, 2004, 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_misc.h -	MR_warning(),
**			MR_fatal_error()
*/

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_std.h"	/* for `MR_NO_RETURN' */
#include <stdlib.h>		/* for `size_t' */

extern	void	MR_warning(const char *msg, ...);

	/* For warnings from the debugger */
extern	void	MR_mdb_warning(const char *msg, ...);

extern	void	MR_perror(const char *msg);

	/* For errors from the debugger */
extern	void	MR_mdb_perror(const char *msg);

extern	void	MR_fatal_error(const char *msg, ...) MR_NO_RETURN;

/*
** Register a function to be called (as func(data)) when the program is
** about to be terminated due to an uncaught exception. 
*/

extern	void	MR_register_exception_cleanup(void (*func)(void *),
			void *data);

/*
** Call all the functions registered with MR_register_exception_cleanup.
** Should be invoked only when the program is about to be terminated
** due to an uncaught exception. 
*/

extern	void	MR_perform_registered_exception_cleanups(void);

/*
** These macros are shorthands to allow reductions in the size of compiler
** generated C source files.
*/

#define	MR_COMMON_TYPE(typenum)					\
	MR_PASTE2(mercury_type_, typenum)

#define	MR_COMMON_NAME(cellnum)					\
	MR_PASTE2(mercury_common_, cellnum)

#define	MR_COMMON(typenum, cellnum)				\
	((MR_Word *) &MR_COMMON_NAME(typenum)[cellnum])

#define	MR_XCOMMON(typenum, cellnum)				\
	((MR_Word *) &MR_COMMON_NAME(typenum)[cellnum])

#define	MR_TAG_COMMON(tag, typenum, cellnum)			\
	(MR_mkword(MR_mktag(tag), MR_COMMON(typenum, cellnum)))

#define	MR_TAG_XCOMMON(tag, typenum, cellnum)			\
	(MR_mkword(MR_mktag(tag), MR_XCOMMON(typenum, cellnum)))

#endif /* not MERCURY_MISC_H */
