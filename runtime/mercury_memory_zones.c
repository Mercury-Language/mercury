/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the MemoryZone data structure and operations
** for managing the memory zones.
**
** The offset of each zone can be supplied to allow us to control how
** they map onto direct mapped caches.  The provided next_offset()
** function can be used to generate these offsets.
**
** We allocate a large arena, preferably aligned on a boundary that
** is a multiple of both the page size and the primary cache size.
**
** If the operating system of the machine supports the mprotect syscall,
** we also protect a chunk at the end of each area against access,
** thus detecting area overflow.
*/

/*---------------------------------------------------------------------------*/

#include "mercury_imp.h"

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_SYS_SIGINFO
  #include <sys/siginfo.h>
#endif 

#ifdef	HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef	HAVE_UCONTEXT
  #include <ucontext.h>
#endif

#ifdef	HAVE_SYS_UCONTEXT
  #include <sys/ucontext.h>
#endif

#include "mercury_trace.h"

#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
#endif

/*---------------------------------------------------------------------------*/

#ifdef	CONSERVATIVE_GC
  #define	memalign(a,s)   GC_MALLOC_UNCOLLECTABLE(s)
#elif defined(HAVE_MEMALIGN)
  extern void	*memalign(size_t, size_t);
#else
  #define	memalign(a,s)	malloc(s)
#endif

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

#endif /* HAVE_MPROTECT */

/*---------------------------------------------------------------------------*/

Word	virtual_reg_map[MAX_REAL_REG] = VIRTUAL_REG_MAP_BODY;

unsigned long	num_uses[MAX_RN];

#define MAX_ZONES	16

static MemoryZone *zone_table = NULL;

static MemoryZone *used_memory_zones = NULL;
static MemoryZone *free_memory_zones = NULL;
#ifdef	MR_THREAD_SAFE
  static MercuryLock *free_memory_zones_lock;
#endif

static void		init_offsets(void);
static MemoryZone	*get_zone(void);
static void		unget_zone(MemoryZone *zone);

	/*
	** We manage the handing out of offets through the cache by
	** computing the offsets once and storing them in an array
	** (in shared memory if necessary). We then maintain a global
	** counter used to index the array which we increment (modulo
	** the size of the array) after handing out each offset.
	*/

#define	CACHE_SLICES	8

static	size_t		*offset_vector;
static	int		offset_counter;
size_t	next_offset(void);

void
init_zones()
{

#ifdef  MR_THREAD_SAFE
	free_memory_zones_lock = make(MercuryLock);
	pthread_mutex_init(free_memory_zones_lock, MR_MUTEX_ATTR);
#endif

	init_offsets();
}

static void 
init_offsets()
{
	int i;
	size_t fake_reg_offset;

	offset_counter = 0;

	offset_vector = allocate_array(size_t, CACHE_SLICES - 1);

	fake_reg_offset = (Unsigned) MR_fake_reg % pcache_size;

	for (i = 0; i < CACHE_SLICES - 1; i++) {
		offset_vector[i] =
			(fake_reg_offset + pcache_size * i / CACHE_SLICES)
			% pcache_size;
	}
} /* end init_offsets() */


MemoryZone *
get_zone(void)
{
	MemoryZone *zone;

	/*
	** unlink the first zone on the free-list,
	** link it onto the used-list and return it.
	*/
	MR_LOCK(free_memory_zones_lock, "get_zone");
	if (free_memory_zones == NULL) {
		zone = (MemoryZone *) make(MemoryZone);
	} else {
		zone = free_memory_zones;
		free_memory_zones = free_memory_zones->next;
	}

	zone->next = used_memory_zones;
	used_memory_zones = zone;
	MR_UNLOCK(free_memory_zones_lock, "get_zone");

	return zone;
}

void 
unget_zone(MemoryZone *zone)
{
	MemoryZone *prev, *tmp;

	/*
	** Find the zone on the used list, and unlink it from
	** the list, then link it onto the start of the free-list.
	*/
	MR_LOCK(free_memory_zones_lock, "unget_zone");
	for(prev = NULL, tmp = used_memory_zones;
		tmp && tmp != zone; prev = tmp, tmp = tmp->next) 
	{
		/* VOID */
	}
	if (tmp == NULL) {
		fatal_error("memory zone not found!");
	}
	if (prev == NULL) {
		used_memory_zones = used_memory_zones->next;
	} else {
		prev->next = tmp->next;
	}

	zone->next = free_memory_zones;
	free_memory_zones = zone;
	MR_UNLOCK(free_memory_zones_lock, "unget_zone");
}

/*
** successive calls to next_offset return offsets modulo the primary
** cache size (carefully avoiding ever giving an offset that clashes
** with fake_reg_array). This is used to give different memory zones
** different starting points across the caches so that it is better
** utilized.
** An alternative implementation would be to increment the offset by
** a fixed amount (eg 2Kb) so that as primary caches get bigger, we
** allocate more offsets across them.
*/
size_t	
next_offset(void)
{
	size_t offset;

	offset = offset_vector[offset_counter];

	offset_counter = (offset_counter + 1) % CACHE_SLICES;

	return offset;
}

MemoryZone *
create_zone(const char *name, int id, size_t size,
		size_t offset, size_t redsize,
		bool ((*handler)(Word *addr, MemoryZone *zone, void *context)))
{
	Word		*base;
	size_t		total_size;

		/*
		** total allocation is:
		**	unit		(roundup to page boundary)
		**	size		(including redzone)
		**	unit		(an extra page for protection if
		**			 mprotect is being used)
		*/
#ifdef	HAVE_MPROTECT
	total_size = size + 2 * unit;
#else
	total_size = size + unit;
#endif

	base = memalign(unit, total_size);
	if (base == NULL) {
		char buf[2560];
		sprintf(buf, "unable allocate memory zone: %s#%d", name, id);
		fatal_error(buf);
	}

	return construct_zone(name, id, base, size, offset, redsize, handler);
} /* end create_zone() */

MemoryZone *
construct_zone(const char *name, int id, Word *base,
		size_t size, size_t offset, size_t redsize,
		ZoneHandler handler)
{
	MemoryZone	*zone;
	size_t		total_size;

	if (base == NULL) {
		fatal_error("construct_zone called with NULL pointer");
	}

	zone = get_zone();

	zone->name = name;
	zone->id = id;

#ifdef	MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->handler = handler;
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

	zone->bottom = base;

#ifdef 	HAVE_MPROTECT
	total_size = size + unit;
#else
	total_size = size;
#endif	/* HAVE_MPROTECT */

	zone->top = (Word *) ((char *)base + total_size);
	zone->min = (Word *) ((char *)base + offset);
#ifdef	MR_LOWLEVEL_DEBUG
	zone->max = zone->min;
#endif	/* MR_LOWLEVEL_DEBUG */

	/*
	** setup the redzone
	*/
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->redzone_base = zone->redzone = (Word *)
			round_up((Unsigned)base + size - redsize, unit);
	if (mprotect((char *)zone->redzone, redsize + unit, MY_PROT) < 0) {
		char buf[2560];
		sprintf(buf, "unable to set %s#%d redzone\n"
			"base=%p, redzone=%p",
			zone->name, zone->id, zone->bottom, zone->redzone);
		fatal_error(buf);
	}
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

	/*
	** setup the hardzone (only if the redzone is unavailable)
	*/
#if	defined(HAVE_MPROTECT) && !defined(MR_CHECK_OVERFLOW_VIA_MPROTECT)
	zone->hardmax = (Word *) round_up((Unsigned)zone->top - unit, unit);
	if (mprotect((char *)zone->hardmax, unit, MY_PROT) < 0) {
		char buf[2560];
		sprintf(buf, "unable to set %s#%d hardmax\n"
			"base=%p, hardmax=%p top=%p",
			zone->name, zone->id, zone->bottom, zone->hardmax,
			zone->top);
		fatal_error(buf);
	}
#endif	/* HAVE_MPROTECT */


	return zone;
} /* end construct_zone() */

void 
reset_redzone(MemoryZone *zone)
{
#ifdef	MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->redzone = zone->redzone_base;

	if (mprotect((char *)zone->redzone,
		((char *)zone->top) - ((char *) zone->redzone), MY_PROT) < 0)
	{
		char buf[2560];
		sprintf(buf, "unable to reset %s#%d redzone\n"
			"base=%p, redzone=%p",
			zone->name, zone->id, zone->bottom, zone->redzone);
		fatal_error(buf);
	}
#endif	/* MR_CHECK_OVERFLOW_VIA_MPROTECT */
}

MemoryZone *
get_used_memory_zones(void)
{
	return used_memory_zones;
}

void
debug_memory(void)
{
	MemoryZone	*zone;

	fprintf(stderr, "\n");
	fprintf(stderr, "pcache_size  = %lu (0x%lx)\n",
		(unsigned long) pcache_size, (unsigned long) pcache_size);
	fprintf(stderr, "page_size    = %lu (0x%lx)\n",
		(unsigned long) page_size, (unsigned long) page_size);
	fprintf(stderr, "unit         = %lu (0x%lx)\n",
		(unsigned long) unit, (unsigned long) unit);

	fprintf(stderr, "\n");
	fprintf(stderr, "fake_reg       = %p (offset %ld)\n",
		(void *) MR_fake_reg, (long) MR_fake_reg & (unit-1));
	fprintf(stderr, "\n");

	for (zone = used_memory_zones; zone; zone = zone->next)
	{
		fprintf(stderr, "%-16s#%d-base	= %p\n",
			zone->name, zone->id, (void *) zone->bottom);
		fprintf(stderr, "%-16s#%d-min		= %p\n",
			zone->name, zone->id, (void *) zone->min);
		fprintf(stderr, "%-16s#%d-top		= %p\n",
			zone->name, zone->id, (void *) zone->top);
#ifdef	MR_CHECK_OVERFLOW_VIA_MPROTECT
		fprintf(stderr, "%-16s#%d-redzone	= %p\n",
			zone->name, zone->id, (void *) zone->redzone);
#endif	/* MR_CHECK_OVERFLOW_VIA_MPROTECT */
#ifdef	HAVE_MPROTECT
		fprintf(stderr, "%-16s#%d-hardmax		= %p\n",
			zone->name, zone->id, (void *) zone->hardmax);
		fprintf(stderr, "%-16s#%d-size		= %lu\n",
			zone->name, zone->id, (unsigned long)
			((char *)zone->hardmax - (char *)zone->min));
#else
		fprintf(stderr, "%-16s#%d-size		= %lu\n",
			zone->name, zone->id, (unsigned long)
			((char *)zone->top - (char *)zone->min));
#endif	/* HAVE_MPROTECT */
		fprintf(stderr, "\n");
	}
}

