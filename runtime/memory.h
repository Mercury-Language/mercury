/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MEMORY_H
#define	MEMORY_H

#include "regs.h"
#include "std.h"

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
extern	Word 	num_uses[MAX_RN];

/*
 * The Mercury runtime uses a number of memory areas or *zones*. These
 * hold the detstack, the nondetstack, the heap, and potentially other
 * areas such as a trail, a "solutions"-heap, and so on.
 * These memory areas are each represented by a structure that contains
 * the following fields:
 *	name	- a string constant used to name the allocated area
 *	id	- an integer which together with the name should uniquely
 *		  identify the allocated area.
 *	bottom	- the address of the bottom of the allocated area
 *			(should be on a page boundary)
 *	top	- the address one word past the top of the allocated area
 *			(should be on a page boundary)
 *	min	- the address of the lowest part of the allocated that
 *			will be used. This may be different to `bottom'
 *			so that the use of different memory zones doesn't
 *			beat the cache.
 *	max	- the highest address in this memory area that has been
 *			used so far. This is only defined in debugging grades.
 *	hardmax	- the address of the bottom of the last page of the allocated
 *			area. This is one higher than the highest address that
 *			can be used in this zone. We never unprotect the
 *			last page of a zone so that we retain protection
 *			against overrunning the end of the zone. This is
 *			obviously only available on platforms that have
 *			mprotect.
 *			(should be on a page boundary)
 *	redzone	- the address of the start of the region that has been
 *			mprotected as a redzone. Since without SIGINFO
 *			it is not possible [portably] to figure out
 *			where the fault occured, redzone is only available
 *			on platforms that have both mprotect and SIGINFO.
 *			(should be on a page boundary)
 *	handler - the address of a function to handle accesses in the
 *			redzone of this allocated area. This is only
 *			specified if mprotect and SIGINFO are available.
 */

typedef struct MEMORY_ZONE	MemoryZone;

#if defined(HAVE_MPROTECT) && defined(HAVE_SIGINFO)
typedef bool ZoneHandler(Word *addr, struct MEMORY_ZONE *zone, void *context);
#endif

struct MEMORY_ZONE {
	struct MEMORY_ZONE *next; /* the memory zones are organized as a
				   * linked list of free zones and linked
				   * list of used zones. The next field
				   * is NULL or a pointer to the next memory
				   * zone in the list.
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
/*
	Word	*redzone_base;	beginning of the original redzone
*/
	Word	*redzone;	/* beginning of the current redzone */
	Word	*hardmax;	/* last page of the zone which can't be
				   unprotected */
#ifdef	HAVE_SIGINFO
	ZoneHandler *handler;   /* handler for page faults in the redzone */
#endif	/* HAVE_SIGINFO */
#endif	/* HAVE_MPROTECT */
};

#define MAX_ZONES	16

extern MemoryZone	zone_table[MAX_ZONES];

	/* A linked list of all the unused zones */
extern MemoryZone	*free_memory_zones;
	/* A linked list of all the used zones */
extern MemoryZone	*used_memory_zones;

extern MemoryZone	*detstack_zone;
extern MemoryZone	*nondetstack_zone;
#ifndef CONSERVATIVE_GC
extern MemoryZone	*heap_zone;
extern MemoryZone	*solutions_heap_zone;
extern Word		*solutions_heap_pointer;
#endif

#ifndef	SPEED
extern	MemoryZone	*dumpstack_zone;
extern	int		dumpindex;
#endif

extern	void	init_memory(void);

#endif
