/*
** Copyright (C) 1994-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** memory.h - general memory-allocation related stuff for the Mercury runtime.
**
** This defines the different memory areas used by the Mercury runtime,
** including the det & nondet stacks, the heap (and solutions heap),
** and the fake_reg array for holding Mercury virtual registers.
** It also provides interfaces for constructing new memory zones,
** and for allocating (possibly shared) memory.
*/

#ifndef	MEMORY_H
#define	MEMORY_H

#include "regs.h"		/* for NUM_REAL_REGS */

#include <stdlib.h>		/* for size_t */

#include "mercury_types.h"	/* for Word */
#include "std.h"		/* for bool */


/* these cannot be changed without lots of modifications elsewhere */
#define MAX_REAL_REG 32		/* r1 .. r32 */
#define NUM_SPECIAL_REG 5	/* succip, sp, hp, maxfr, curfr */

/* this can be changed at will, including by -D options to the C compiler */
#ifndef MAX_VIRTUAL_REG
#define MAX_VIRTUAL_REG	1024
#endif

/* allocate enough fake_regs to hold both the special regs */
/* and all the virtual registers */
#define MAX_FAKE_REG	(NUM_SPECIAL_REG + MAX_VIRTUAL_REG)
				/* mr0 .. mr36, mr(37) ... mr(1028) */

/* reserve MAX_FAKE_REG virtual regs, numbered from 0 to MAX_FAKE_REG-1 */
extern	Word	fake_reg[MAX_FAKE_REG];

/* used to lookup the fake_reg for a given real reg */
extern	Word	virtual_reg_map[MAX_REAL_REG];

/* used for counting register usage */
extern	unsigned long 	num_uses[MAX_RN];

/*
** The Mercury runtime uses a number of memory areas or *zones*. These
** hold the detstack, the nondetstack, the heap, and potentially other
** areas such as a trail, a "solutions"-heap, and so on.
** These memory areas are each represented by a structure that contains
** the following fields:
**	name	- a string constant used to name the allocated area
**	id	- an integer which together with the name should uniquely
**		  identify the allocated area.
**	bottom	- the address of the bottom of the allocated area
**			(should be on a page boundary)
**	top	- the address one word past the top of the allocated area
**			(should be on a page boundary)
**	min	- the address of the lowest part of the allocated that
**			will be used. This may be different to `bottom'
**			so that the use of different memory zones doesn't
**			beat the cache.
**	max	- the highest address in this memory area that has been
**			used so far. This is only defined in debugging grades.
**	hardmax	- the address of the bottom of the last page of the allocated
**			area. This is one higher than the highest address that
**			can be used in this zone. We never unprotect the
**			last page of a zone so that we retain protection
**			against overrunning the end of the zone. This is
**			obviously only available on platforms that have
**			mprotect.
**			(should be on a page boundary)
**	redzone	- the address of the start of the region that has been
**			mprotected as a redzone. Since without SIGINFO
**			it is not possible [portably] to figure out
**			where the fault occured, redzone is only available
**			on platforms that have both mprotect and SIGINFO.
**			(should be on a page boundary)
**	handler - the address of a function to handle accesses in the
**			redzone of this allocated area. This is only
**			specified if mprotect and SIGINFO are available.
*/

typedef struct MEMORY_ZONE	MemoryZone;

typedef bool ZoneHandler(Word *addr, struct MEMORY_ZONE *zone, void *context);

struct MEMORY_ZONE {
	struct MEMORY_ZONE *next; /* the memory zones are organized as a
				  ** linked list of free zones and linked
				  ** list of used zones. The next field
				  ** is NULL or a pointer to the next memory
				  ** zone in the list.
				  */
	const char *name;	/* name identifier */
	int	id;		/* number */
	Word	*bottom;	/* beginning of the allocated area */
	Word	*top;		/* end of the allocated area */
	Word	*min;		/* lowest word of the area to be used */
#ifndef SPEED
	Word	*max;		/* highest word of the area to be used */
#endif
#ifdef HAVE_MPROTECT
	Word	*redzone_base;	/* beginning of the original redzone */
	Word	*redzone;	/* beginning of the current redzone */
	Word	*hardmax;	/* last page of the zone which can't be
				   unprotected */
  #ifdef HAVE_SIGINFO
	ZoneHandler *handler;   /* handler for page faults in the redzone */
  #endif /* HAVE_SIGINFO */
#endif	/* HAVE_MPROTECT */
};

#define MAX_ZONES	16

extern MemoryZone	*zone_table;

	/* A linked list of all the unused zones */
extern MemoryZone	*free_memory_zones;
	/* A linked list of all the used zones */
extern MemoryZone	*used_memory_zones;

extern MemoryZone	*detstack_zone;
extern MemoryZone	*nondetstack_zone;
#ifndef CONSERVATIVE_GC
extern MemoryZone	*heap_zone;
extern MemoryZone	*solutions_heap_zone;
#endif

#ifndef	SPEED
extern	MemoryZone	*dumpstack_zone;
extern	int		dumpindex;
#endif

/*
** create_zone(Name, Id, Size, Offset, RedZoneSize, FaultHandler)
** allocates a new memory zone with name Name, and number Id, size
** Size (in bytes - which gets rounded up to the nearest multiple of
** the page size), an offset Offset from the page boundary at which
** to start using the memory region (used to help avoid beating the cache),
** the amount Redzone of memory (in bytes) to be protected as a redzone
** (must be less than Size), and the address of a function to handle
** memory references in the redzone.
** If it fails to allocate or protect the zone, then it exits.
** If mprotect or SIGINFO are unavailable, then the last two arguments
** are ignored.
*/

MemoryZone	*create_zone(const char *name, int id,
			size_t size, size_t offset, size_t redsize,
			ZoneHandler *handler);

/*
** construct_zone(Name, Id, Base, Size, Offset, RedZoneSize, FaultHandler)
** has the same behaviour as create_zone, except instread of allocating
** the memory, it takes a pointer to a region of memory that must be at
** least Size bytes, or if HAVE_MPROTECT is defined, then it must be at
** least Size + unit[*] bytes.
** If it fails to protect the redzone then it exits
** If mprotect or SIGINFO are unavailable, then the last two arguments
** are ignored.
**
** [*] unit is a global variable containing the page size in bytes
*/

MemoryZone	*construct_zone(const char *name, int Id, Word *base,
			size_t size, size_t offset, size_t redsize,
			ZoneHandler *handler);

/*
** reset_zone(Zone) resets the redzone on the given MemoryZone to the
** original zone specified in the call to {create,construct}_zone() if
** HAVE_MPROTECT and HAVE_SIGINFO. If either HAVE_MPROTECT or HAVE_SIGINFO
** are not defined, it does nothing.
*/
void	reset_zone(MemoryZone *zone);

/*
** default_handler is a function that can be passed to create_zone to
** unprotect enough of the redzone to allow the access to succeed, or
** fail if there is no space left in the zone.
*/
ZoneHandler default_handler;

/*
** null_handler is a function that can be passed to create_zone which always
** fails.
*/
ZoneHandler null_handler;

/* for these functions, see the comments in memory.c and engine.mod */
extern	void	init_memory(void);
extern	void	init_heap(void);
extern	void	debug_memory(void);

/*
** next_offset() returns sucessive offsets across the primary cache. Useful
** when calling {create,construct}_zone().
*/
extern	size_t	next_offset(void);

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

#endif /* not MEMORY_H */
