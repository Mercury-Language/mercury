/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MEMORY_H
#define	MEMORY_H

#include "regs.h"

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
 *	redzone	- the address of the start of the region that has been
 *			mprotected as a redzone.
 *			(should be on a page boundary)
 *	hardmax	- the address of the bottom of the last page of the allocated
 *			area. This is one higher than the highest address that
 *			can be used in this zone. We never unprotect the
 *			last page of a zone so that we retain protection
 *			against overrunning the end of the zone.
 *			(should be on a page boundary)
 */

#define MAX_ZONE_NAME	64

typedef struct MEMORY_ZONE {
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
	bool	((*handler)(Word *addr, struct MEMORY_ZONE *zone, void *context));
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
#endif
} MemoryZone;

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
#endif

#ifndef	SPEED
extern	MemoryZone	*dumpstack_zone;
extern	int		dumpindex;
#endif

extern	void	init_memory(void);

/*
** DESCRIPTION
**  The function mprotect() changes the  access  protections  on
**  the mappings specified by the range [addr, addr + len) to be
**  that specified by prot.  Legitimate values for prot are  the
**  same  as  those  permitted  for  mmap  and  are  defined  in
**  <sys/mman.h> as:
**
** PROT_READ    page can be read
** PROT_WRITE   page can be written
** PROT_EXEC    page can be executed
** PROT_NONE    page can not be accessed
*/
#ifdef  HAVE_MPROTECT

#ifdef CONSERVATIVE_GC
	/*
	** The conservative garbage collectors scans through
	** all these areas, so we need to allow reads.
	** XXX This probably causes efficiency problems:
	** too much memory for the GC to scan, and it probably
	** all gets paged in.
	*/
#define MY_PROT PROT_READ
#else
#define MY_PROT PROT_NONE
#endif

/* The BSDI BSD/386 1.1 headers don't define PROT_NONE */
#ifndef PROT_NONE
#define PROT_NONE 0
#endif

#endif

#endif
