/*
** Copyright (C) 1998-2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the MR_MemoryZone data structure and operations
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

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif 

#ifdef MR_HAVE_SYS_SIGNAL_H
  /* on FREEBSD we need to include <sys/signal.h> before <ucontext.h> */
  #include <sys/signal.h>
#endif

#ifdef	MR_HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef	MR_HAVE_UCONTEXT_H
  #include <ucontext.h>
#endif

#ifdef	MR_HAVE_SYS_UCONTEXT_H
  #include <sys/ucontext.h>
#endif

#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
#endif

#ifdef MR_WIN32_VIRTUAL_ALLOC
  #include <windows.h>
#endif

/*---------------------------------------------------------------------------*/

/*
** MR_PROTECTPAGE is now defined if we have some sort of mprotect like
** functionality, all checks for MR_HAVE_MPROTECT should now use MR_PROTECTPAGE.
*/
#if defined(MR_HAVE_MPROTECT)
int
MR_protect_pages(void *addr, size_t size, int prot_flags)
{
	return mprotect((char *) addr, size, prot_flags);
}
#elif defined(MR_WIN32_VIRTUAL_ALLOC)
/*
** Emulate mprotect under Win32.
** Return -1 on failure
*/
int
MR_protect_pages(void *addr, size_t size, int prot_flags)
{
	int rc;
	DWORD flags;

	if (prot_flags & PROT_WRITE) {
		flags = PAGE_READWRITE;
	} else if (prot_flags & PROT_READ) {
		flags = PAGE_READONLY;
	} else {
		flags = PAGE_NOACCESS;
	}
	
	rc = (VirtualAlloc(addr, size, MEM_COMMIT, flags) ? 0 : -1);
	if (rc < 0) {
		fprintf(stderr,
			"Error in VirtualAlloc(addr=0x%08lx, size=0x%08lx):"
			" 0x%08lx\n", (unsigned long) addr,
			(unsigned long) size, (unsigned long) GetLastError());
	}

	return rc;
}
#endif	/* MR_WIN32_VIRTUAL_ALLOC */


/*---------------------------------------------------------------------------*/


#if defined(MR_WIN32_VIRTUAL_ALLOC)
/*
** Under Win32, we use VirtualAlloc instead of the standard malloc,
** since we will have to call VirtualProtect later on the pages
** allocated here.
*/
void *
memalign(size_t unit, size_t size)
{
	void *ptr;

	/*
	** We don't need to use the 'unit' value as the memory is always
	** aligned under Win32.
	*/
	ptr = VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_READWRITE);
	if (ptr == NULL) {
		fprintf(stderr, "Error in VirtualAlloc(size=0x%08lx):"
				" 0x%08lx\n", (unsigned long) size,
				(unsigned long) GetLastError());
	}

  #ifdef MR_CONSERVATIVE_GC
	if (ptr != NULL)
		GC_add_roots((char *)ptr, (char *)ptr + size);
  #endif
	return ptr;
}
#elif defined(MR_CONSERVATIVE_GC)
  #define	memalign(a,s)   GC_MALLOC_UNCOLLECTABLE(s)
#elif defined(MR_HAVE_MEMALIGN)
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
#ifdef MR_PROTECTPAGE

  #ifdef MR_CONSERVATIVE_GC
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

#endif /* MR_PROTECTPAGE */

/*---------------------------------------------------------------------------*/

MR_Word		MR_virtual_reg_map[MR_MAX_REAL_REG] = MR_VIRTUAL_REG_MAP_BODY;

unsigned long	MR_num_uses[MR_MAX_RN];

#define MAX_ZONES	16

static MR_MemoryZone *used_memory_zones = NULL;
static MR_MemoryZone *free_memory_zones = NULL;
#ifdef	MR_THREAD_SAFE
  static MercuryLock *free_memory_zones_lock;
#endif

static void		MR_init_offsets(void);
static MR_MemoryZone	*MR_get_zone(void);
static void		MR_unget_zone(MR_MemoryZone *zone);

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
MR_init_zones()
{

#ifdef  MR_THREAD_SAFE
	free_memory_zones_lock = MR_GC_NEW(MercuryLock);
	pthread_mutex_init(free_memory_zones_lock, MR_MUTEX_ATTR);
#endif

	MR_init_offsets();
}

static void 
MR_init_offsets()
{
	int i;
	size_t fake_reg_offset;

	offset_counter = 0;

	offset_vector = MR_GC_NEW_ARRAY(size_t, CACHE_SLICES - 1);

	fake_reg_offset = (MR_Unsigned) MR_fake_reg % MR_pcache_size;

	for (i = 0; i < CACHE_SLICES - 1; i++) {
		offset_vector[i] =
			(fake_reg_offset + MR_pcache_size * i / CACHE_SLICES)
			% MR_pcache_size;
	}
} /* end init_offsets() */


static MR_MemoryZone *
MR_get_zone(void)
{
	MR_MemoryZone *zone;

	/*
	** unlink the first zone on the free-list,
	** link it onto the used-list and return it.
	*/
	MR_LOCK(free_memory_zones_lock, "get_zone");
	if (free_memory_zones == NULL) {
		zone = MR_GC_NEW(MR_MemoryZone);
	} else {
		zone = free_memory_zones;
		free_memory_zones = free_memory_zones->next;
	}

	zone->next = used_memory_zones;
	used_memory_zones = zone;
	MR_UNLOCK(free_memory_zones_lock, "get_zone");

	return zone;
}

#if 0

/* this function is not currently used */

static void 
MR_unget_zone(MR_MemoryZone *zone)
{
	MR_MemoryZone *prev, *tmp;

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
		MR_fatal_error("memory zone not found!");
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

#endif

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
MR_next_offset(void)
{
	size_t offset;

	offset = offset_vector[offset_counter];

	offset_counter = (offset_counter + 1) % CACHE_SLICES;

	return offset;
}

MR_MemoryZone *
MR_create_zone(const char *name, int id, size_t size,
	size_t offset, size_t redsize,
	MR_bool ((*handler)(MR_Word *addr, MR_MemoryZone *zone, void *context)))
{
	MR_Word		*base;
	size_t		total_size;

		/*
		** total allocation is:
		**	unit		(roundup to page boundary)
		**	size		(including redzone)
		**	unit		(an extra page for protection if
		**			 mprotect is being used)
		*/
#ifdef	MR_PROTECTPAGE
	total_size = size + 2 * MR_unit;
#else
	total_size = size + MR_unit;
#endif

	base = (MR_Word *) memalign(MR_unit, total_size);
	if (base == NULL) {
		char buf[2560];
		sprintf(buf, "unable allocate memory zone: %s#%d", name, id);
		MR_fatal_error(buf);
	}

	return MR_construct_zone(name, id, base, size, offset,
		redsize, handler);
} /* end MR_create_zone() */

MR_MemoryZone *
MR_construct_zone(const char *name, int id, MR_Word *base,
	size_t size, size_t offset, size_t redsize,
	MR_ZoneHandler handler)
{
	MR_MemoryZone	*zone;
	size_t		total_size;

	if (base == NULL) {
		MR_fatal_error("construct_zone called with NULL pointer");
	}

	zone = MR_get_zone();

	zone->name = name;
	zone->id = id;

#ifdef	MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->handler = handler;
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

	zone->bottom = base;

#ifdef 	MR_PROTECTPAGE
	total_size = size + MR_unit;
#else
	total_size = size;
#endif	/* MR_PROTECTPAGE */

	zone->top = (MR_Word *) ((char *)base + total_size);
	zone->min = (MR_Word *) ((char *)base + offset);
#ifdef	MR_LOWLEVEL_DEBUG
	zone->max = zone->min;
#endif	/* MR_LOWLEVEL_DEBUG */

	/*
	** setup the redzone
	*/
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->redzone_base = zone->redzone = (MR_Word *)
			MR_round_up((MR_Unsigned)base + size - redsize,
				MR_unit);
	if (MR_protect_pages((char *)zone->redzone,
			redsize + MR_unit, MY_PROT) < 0)
	{
		char buf[2560];
		sprintf(buf, "unable to set %s#%d redzone\n"
			"base=%p, redzone=%p",
			zone->name, zone->id, zone->bottom, zone->redzone);
		MR_fatal_error(buf);
	}
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

	/*
	** setup the hardzone
	*/
#if	defined(MR_PROTECTPAGE)
	zone->hardmax = (MR_Word *) MR_round_up(
			(MR_Unsigned)zone->top - MR_unit, MR_unit);
	if (MR_protect_pages((char *)zone->hardmax, MR_unit, MY_PROT) < 0) {
		char buf[2560];
		sprintf(buf, "unable to set %s#%d hardmax\n"
			"base=%p, hardmax=%p top=%p",
			zone->name, zone->id, zone->bottom, zone->hardmax,
			zone->top);
		MR_fatal_error(buf);
	}
#endif	/* MR_PROTECTPAGE */

#if defined(MR_NATIVE_GC) && defined(MR_HIGHLEVEL_CODE)
	zone->gc_threshold = (char *) zone->MR_zone_end - MR_heap_margin_size;
#endif

	return zone;
} /* end MR_construct_zone() */

void 
MR_reset_redzone(MR_MemoryZone *zone)
{
#ifdef	MR_CHECK_OVERFLOW_VIA_MPROTECT
	zone->redzone = zone->redzone_base;

	if (MR_protect_pages((char *)zone->redzone,
		((char *)zone->top) - ((char *) zone->redzone), MY_PROT) < 0) {
		char buf[2560];
		sprintf(buf, "unable to reset %s#%d redzone\n"
			"base=%p, redzone=%p",
			zone->name, zone->id, zone->bottom, zone->redzone);
		MR_fatal_error(buf);
	}
#endif	/* MR_CHECK_OVERFLOW_VIA_MPROTECT */
}

MR_MemoryZone *
MR_get_used_memory_zones(void)
{
	return used_memory_zones;
}

void
MR_debug_memory(void)
{
	MR_MemoryZone	*zone;

	fprintf(stderr, "\n");
	fprintf(stderr, "pcache_size  = %lu (0x%lx)\n",
		(unsigned long) MR_pcache_size, (unsigned long) MR_pcache_size);
	fprintf(stderr, "page_size    = %lu (0x%lx)\n",
		(unsigned long) MR_page_size, (unsigned long) MR_page_size);
	fprintf(stderr, "unit         = %lu (0x%lx)\n",
		(unsigned long) MR_unit, (unsigned long) MR_unit);

	fprintf(stderr, "\n");
	fprintf(stderr, "fake_reg       = %p (offset %ld)\n",
		(void *) MR_fake_reg, (long) MR_fake_reg & (MR_unit-1));
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
		fprintf(stderr, "%-16s#%d-redzone_base	= %p\n",
			zone->name, zone->id, (void *) zone->redzone_base);
#endif	/* MR_CHECK_OVERFLOW_VIA_MPROTECT */
#ifdef	MR_PROTECTPAGE
		fprintf(stderr, "%-16s#%d-hardmax		= %p\n",
			zone->name, zone->id, (void *) zone->hardmax);
		fprintf(stderr, "%-16s#%d-size		= %lu\n",
			zone->name, zone->id, (unsigned long)
			((char *)zone->hardmax - (char *)zone->min));
#else
		fprintf(stderr, "%-16s#%d-size		= %lu\n",
			zone->name, zone->id, (unsigned long)
			((char *)zone->top - (char *)zone->min));
#endif	/* MR_PROTECTPAGE */
		fprintf(stderr, "\n");
	}
}
