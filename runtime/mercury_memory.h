/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_memory.h - 	general memory-allocation related stuff for the
**			Mercury runtime.
**
** This defines the different memory areas used by the Mercury runtime,
** including the det & nondet stacks, the heap (and solutions heap),
** and the fake_reg array for holding Mercury virtual registers.
** It also provides interfaces for constructing new memory zones,
** and for allocating (possibly shared) memory.
*/

#ifndef	MERCURY_MEMORY_H
#define	MERCURY_MEMORY_H

#include "mercury_memory_zones.h"

extern MemoryZone	*detstack_zone;
extern MemoryZone	*nondetstack_zone;
#ifndef CONSERVATIVE_GC
extern MemoryZone	*heap_zone;
extern MemoryZone	*solutions_heap_zone;
#endif

#ifdef	MR_LOWLEVEL_DEBUG
extern	MemoryZone	*dumpstack_zone;
extern	int		dumpindex;
#endif

/*
** round_up(amount, align) returns `amount' rounded up to the nearest
** alignment boundary.  `align' must be a power of 2.
*/

#define round_up(amount, align) ((((amount) - 1) | ((align) - 1)) + 1)

/* 
** For these functions, see the comments in mercury_memory.c and 
** mercury_engine.c 
*/
extern	void	init_memory(void);
extern	void	init_heap(void);
extern	void	debug_memory(void);

/*
** allocate_bytes() allocates the given number of bytes.
**
** allocate_object(type) allocates space for an object of the specified type.
**
** allocate_array(type, num) allocates space for an array of objects of the
** specified type.
**
** If shared memory is being used, these allocation routines will allocate
** in shared memory.
*/

extern	void	*allocate_bytes(size_t numbytes);

#define allocate_object(type) \
	((type *)allocate_bytes(sizeof(type)))

#define allocate_array(type, num) \
	((type *)allocate_bytes((num) * sizeof(type)))

/*
** deallocate_memory() deallocates the memory allocated by one of the
** allocate_* functions.
*/

void deallocate_memory(void *);


/*
** checked_malloc() and checked_realloc() are like the standard C
** malloc() and realloc() functions, except that the return values
** are checked.
**
** NOTE: checked_malloc()ed and checked_realloc()ed structures must
** never contain pointers into GCed memory, otherwise those pointers
** will never be traced.
*/

#include <stddef.h>	/* for size_t */
void *checked_malloc(size_t n);
void *checked_realloc(void *old, size_t n);

/*
** `unit' is the size of the minimum unit of memory we allocate (in bytes).
** `page_size' is the size of a single page of memory.
*/

extern size_t          unit;
extern size_t          page_size;


#endif /* not MERCURY_MEMORY_H */
