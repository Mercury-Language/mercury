/*
** Copyright (C) 1996-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_prof_mem.h - defines memory allocation functions used to hold
** the tables of profiling counts.
**
** Author: petdr
*/

#ifndef MERCURY_PROF_MEM_H
#define MERCURY_PROF_MEM_H

#include <stddef.h>	/* for size_t */

/*
** prof_malloc() allocates memory in large chunks using newmem(),
** and then doles out portions of these chunks one at a time.
** We use prof_malloc() to reduce the chance of calling newmem()
** from a profiling interrupt that interrupted another call to newmem().
** Doing that is bad news, because newmem() is not re-entrant.
*/
void *prof_malloc(size_t);

#define prof_make(t)	((t *) prof_malloc(sizeof(t)))

#endif /* not MERCURY_PROF_MEM_H */
