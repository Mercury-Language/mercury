/*
** Copyright (C) 1995-2000,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_misc.h -	MR_warning(),
**			MR_fatal_error()
*/

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_std.h"	/* for `NO_RETURN' */
#include <stdlib.h>		/* for `size_t' */

extern	void	MR_warning(const char *msg, ...);

	/* For warnings from the debugger */
extern	void	MR_mdb_warning(const char *msg, ...);

extern	void	MR_perror(const char *msg);

	/* For errors from the debugger */
extern	void	MR_mdb_perror(const char *msg);

extern	void	MR_fatal_error(const char *msg, ...) NO_RETURN;

#endif /* not MERCURY_MISC_H */
