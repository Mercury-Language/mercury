/*
** Copyright (C) 1996-1997, 1999-2000, 2002 The University of Melbourne.
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
** MR_prof_malloc() allocates memory in large chunks using MR_GC_malloc(),
** and then doles out portions of these chunks one at a time.
** We use MR_prof_malloc() to reduce the chance of calling MR_GC_malloc()
** from a profiling interrupt that interrupted another call to MR_GC_malloc().
** Doing that is bad news, because MR_GC_malloc() is not guaranteed to be
** re-entrant.
** (If conservative GC is enabled, then MR_GC_malloc() _is_ re-entrant,
** since for profiling grades we compile the conservative collector without
** -DNO_SIGNALS [see boehm_gc/README for documentation about -DNO_SIGNALS].
** But if conservative GC is not enabled, then MR_GC_malloc() just
** calls malloc(), which is not guaranteed to be re-entrant.)
**
** Note that the current implementation of MR_prof_malloc only guarantees
** that the memory will be MR_Dword-aligned; if you want to allocate types
** that contain data types (e.g. `double') which might require stricter
** alignment than that, then you will need to change the implementation of
** MR_prof_malloc().
*/
extern void *MR_prof_malloc(size_t);

/*
** Use the following macros rather than calling MR_prof_malloc() directly.
*/
#define MR_PROF_NEW(t)		((t *) MR_prof_malloc(sizeof(t)))
#define MR_PROF_NEW_ARRAY(t,n)	((t *) MR_prof_malloc(sizeof(t) * (n)))

#endif /* not MERCURY_PROF_MEM_H */
