/*
** Copyright (C) 1998-2002 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_memory_zones.h - functions and data structures for handling
** 			    memory zones.
**
** This defines a generic memory zone handler, which can be used for
** stacks and heaps in the Mercury runtime.  It provides functions for
** generating offsets so that different memory zones begin at different
** offsets (improves performance with direct mapped caches).  It also
** handles the fake_reg array for holding Mercury virtual registers.
*/

#ifndef	MERCURY_MEMORY_ZONES_H
#define	MERCURY_MEMORY_ZONES_H

#include "mercury_regs.h"		/* for MR_NUM_REAL_REGS */

#include <stdlib.h>		/* for size_t */

#include "mercury_types.h"	/* for MR_Word */
#include "mercury_std.h"		/* for MR_bool */


/* these cannot be changed without lots of modifications elsewhere */
#define MR_MAX_REAL_REG 32		/* MR_r1 .. MR_r32 */

/* this can be changed at will, including by -D options to the C compiler */
#ifndef MR_MAX_VIRTUAL_REG
#define MR_MAX_VIRTUAL_REG	1024
#endif

/* allocate enough fake_regs to hold both the special regs */
/* and all the virtual registers */
#define MR_MAX_FAKE_REG	(MR_NUM_SPECIAL_REG + MR_MAX_VIRTUAL_REG)
			/* MR_mr0 .. MR_mr37, MR_mr(38) ... MR_mr(1000) ... */

/* used to lookup the fake_reg for a given real reg */
extern	MR_Word		MR_virtual_reg_map[MR_MAX_REAL_REG];

/* used for counting register usage */
extern	unsigned long 	MR_num_uses[MR_MAX_RN];

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
**			mprotected as a redzone.  Redzone is only
**			available on platforms where
**			MR_CHECK_OVERFLOW_VIA_MPROTECT is defined.
**			(should be on a page boundary)
**	handler - the address of a function to handle accesses in the
**			redzone of this allocated area. This is only
**			available with MR_CHECK_OVERFLOW_VIA_MPROTECT.
*/

typedef struct MR_MemoryZone_Struct	MR_MemoryZone;

typedef MR_bool	MR_ZoneHandler(MR_Word *addr, MR_MemoryZone *zone,
			void *context);

struct MR_MemoryZone_Struct {
	MR_MemoryZone *next;
				  /* the memory zones are organized as a
				  ** linked list of free zones and linked
				  ** list of used zones. The next field
				  ** is NULL or a pointer to the next memory
				  ** zone in the list.
				  */
	const char *name;	/* name identifier */
	int	id;		/* number */
	MR_Word	*bottom;	/* beginning of the allocated area */
	MR_Word	*top;		/* end of the allocated area */
	MR_Word	*min;		/* lowest word of the area to be used */
	MR_Word	*max;		/* highest word of the area to be used;
				   computed only if MR_LOWLEVEL_DEBUG is
				   enabled */
#ifdef MR_PROTECTPAGE
	MR_Word	*hardmax;	/* last page of the zone which can't be
				   unprotected */
#endif	/* MR_PROTECTPAGE */
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
	MR_Word	*redzone_base;	/* beginning of the original redzone */
	MR_Word	*redzone;	/* beginning of the current redzone */
	MR_ZoneHandler *handler;   /* handler for page faults in the redzone */
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

	/*
	** MR_zone_end specifies the end of the 
	** area accessible without a page fault.
	** It is used by MR_clear_zone_for_GC().
	*/
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
	#define MR_zone_end	redzone
#elif defined(MR_PROTECTPAGE)
	#define MR_zone_end	hardmax
#else
	#define MR_zone_end	top
#endif

#if defined(MR_NATIVE_GC) && defined(MR_HIGHLEVEL_CODE)
	/*
	** This field, which is only used for heap zones,
	** points to MR_heap_margin_size bytes before MR_zone_end.
	** It is used to decide when to do garbage collection.
	*/
	char *gc_threshold;	/* == MR_zone_end - MR_heap_margin_size */
#endif
};

/*
** MR_clear_zone_for_GC(MR_MemoryZone *zone, void *start_address):
**	Zero out the (hopefully unused) portion of the zone
**	from the specified `start_address' to the end of the zone.
**	This is used to avoid unwanted memory retention due to 
**	false hits in the conservative garbage collector.
*/
#define MR_clear_zone_for_GC(zone, start_address) \
	((void) memset((start_address), 0, \
		(char*)((zone)->MR_zone_end) - (char *)(start_address)))

/*
** Rather then using mprotect directly, we call MR_protect_pages which
** is OS independent.
*/
#ifdef MR_PROTECTPAGE

  #ifdef MR_WIN32
    #ifndef PROT_NONE
      #define PROT_NONE  0x0000
    #endif
    #ifndef PROT_READ
      #define PROT_READ  0x0001
    #endif
    #ifndef PROT_WRITE
      #define PROT_WRITE 0x0002
    #endif
  #endif

int MR_protect_pages(void *addr, size_t size, int prot_flags);
#endif

/*
** MR_init_memory_arena() allocates (if necessary) the top-level memory pool
** from which all allocations should come. If PARALLEL is defined, then
** this pool should be shared memory. In the absence of PARALLEL, it
** doesn't need to do anything, since with MR_CONSERVATIVE_GC, the collector
** manages the heap, and without GC, we can allocate memory using memalign
** or malloc.
*/

extern	void		MR_init_memory_arena(void);

/*
** MR_init_zones() initializes the memory zone pool and the offset
** generator.  It should be used before any zones are created or
** offsets requested.
*/

extern	void 		MR_init_zones(void);

/*
** MR_create_zone(Name, Id, Size, Offset, RedZoneSize, FaultHandler)
** allocates a new memory zone with name Name, and number Id, size
** Size (in bytes - which gets rounded up to the nearest multiple of
** the page size), an offset Offset from the page boundary at which
** to start using the memory region (used to help avoid beating the cache),
** the amount Redzone of memory (in bytes) to be protected as a redzone
** (must be less than Size), and the address of a function to handle
** memory references in the redzone.
** If it fails to allocate or protect the zone, then it exits.
** If MR_CHECK_OVERFLOW_VIA_MPROTECT is unavailable, then the last two
** arguments are ignored.
*/

extern	MR_MemoryZone	*MR_create_zone(const char *name, int id,
				size_t size, size_t offset, size_t redsize,
				MR_ZoneHandler *handler);

/*
** MR_construct_zone(Name, Id, Base, Size, Offset, RedZoneSize, FaultHandler)
** has the same behaviour as MR_create_zone, except instread of allocating
** the memory, it takes a pointer to a region of memory that must be at
** least Size + unit[*] bytes, or if MR_PROTECTPAGE is defined, then it
** must be at least Size + 2 * unit[*] bytes.
** If it fails to protect the redzone then it exits.
** If MR_CHECK_OVERFLOW_VIA_MPROTECT is unavailable, then the last two
** arguments are ignored.
**
** [*] unit is a global variable containing the page size in bytes
*/

extern	MR_MemoryZone	*MR_construct_zone(const char *name, int Id,
				MR_Word *base, size_t size, size_t offset,
				size_t redsize, MR_ZoneHandler *handler);

/*
** MR_reset_redzone(Zone) resets the redzone on the given MR_MemoryZone to the
** original zone specified in the call to {create,construct}_zone() if
** MR_CHECK_OVERFLOW_VIA_MPROTECT is defined.  Otherwise it does
** nothing.
*/

extern	void		MR_reset_redzone(MR_MemoryZone *zone);

/*
** MR_get_used_memory_zones() returns a pointer to the linked list of
** used memory zones.
*/

extern	MR_MemoryZone	*MR_get_used_memory_zones(void);

/*
** MR_debug_memory() prints out debugging information about the current
** memory zones.
*/

extern	void		MR_debug_memory(void);

/*
** MR_next_offset() returns sucessive offsets across the primary cache. Useful
** when calling {create,construct}_zone().
*/

extern	size_t		MR_next_offset(void);

#endif /* not MERCURY_MEMORY_ZONES_H */
