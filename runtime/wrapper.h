/*
** Copyright (C) 1994-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** wrapper.h - defines the interface to wrapper.mod.
** See wrapper.mod for documentation.
*/

#ifndef	WRAPPER_H
#define	WRAPPER_H

#include <stddef.h>	/* for `size_t' */
#include "std.h"	/* for `bool' */

extern	void		do_init_modules(void);

extern	const char *	progname;
extern	int		mercury_argc;
extern	char **		mercury_argv;
extern	int		mercury_exit_status;

/* sizes of the data areas, *including* the red zone size */
extern	size_t		heap_size;
extern	size_t		detstack_size;
extern	size_t		nondstack_size;
extern	size_t		solutions_heap_size;
extern	size_t		trail_size;

/* sizes of the red zones */
extern	size_t		heap_zone_size;
extern	size_t		detstack_zone_size;
extern	size_t		nondstack_zone_size;
extern	size_t		solutions_heap_zone_size;
extern	size_t		trail_zone_size;

/* size of the primary cache */
extern	size_t		pcache_size;

extern	int		r1val;
extern	int		r2val;
extern	int		r3val;

extern	bool		check_space;

extern	int		time_at_start;
extern	int		time_at_last_stat;

#endif /* not WRAPPER_H */
