/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the register array and data regions of the
** execution algorithm.
** They are defined together here to allow us to control how they map
** onto direct mapped caches.
** We allocate a large arena, preferably aligned on a boundary that
** is a multiple of both the page size and the primary cache size.
**
** We then allocate the heap and the stacks in such a way that
**
**	the register array 
**	the bottom of the heap 
**	the bottom of the detstack 
**	the bottom of the nondstack 
**
** all start at different offsets from multiples of the primary cache size.
** This should reduce cache conflicts (especially for small programs).
**
** If the operating system of the machine supports the mprotect syscall,
** we also protect a chunk at the end of each area against access,
** thus detecting area overflow.
**
** The code for handling the allocation and management of different
** memory zones is in mercury_memory_zones.{c,h}.
** The code for handling overflows and memory access errors in general
** is in mercury_memory_handlers.{c,h}.
*/

/*---------------------------------------------------------------------------*/

#include "mercury_imp.h"

#ifdef HAVE_SIGCONTEXT_STRUCT
  /*
  ** Some versions of Linux call it struct sigcontext_struct, some call it
  ** struct sigcontext.  The following #define eliminates the differences.
  */
  #define sigcontext_struct sigcontext /* must be before #include <signal.h> */

  /*
  ** On some systems (e.g. most versions of Linux) we need to #define
  ** __KERNEL__ to get sigcontext_struct from <signal.h>.
  ** This stuff must come before anything else that might include <signal.h>,
  ** otherwise the #define __KERNEL__ may not work.
  */
  #define __KERNEL__
  #include <signal.h>	/* must come third */
  #undef __KERNEL__

  /*
  ** Some versions of Linux define it in <signal.h>, others define it in
  ** <asm/sigcontext.h>.  We try both.
  */
  #ifdef HAVE_ASM_SIGCONTEXT
    #include <asm/sigcontext.h>
  #endif 
#else
  #include <signal.h>
#endif

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

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_memory_handlers.h"

/*---------------------------------------------------------------------------*/

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  #define	getpagesize()	sysconf(_SC_PAGESIZE)
#elif !defined(HAVE_GETPAGESIZE)
  #define	getpagesize()	8192
#endif

/*---------------------------------------------------------------------------*/

static	void	setup_mprotect(void);

#ifdef	HAVE_SIGINFO
  static	bool	try_munprotect(void *address, void *context);
  static	char	*explain_context(void *context);
#endif /* HAVE_SIGINFO */

MemoryZone *detstack_zone;
MemoryZone *nondetstack_zone;
#ifndef CONSERVATIVE_GC
  MemoryZone *heap_zone;
  MemoryZone *solutions_heap_zone;
#endif

#ifdef	MR_LOWLEVEL_DEBUG
  MemoryZone *dumpstack_zone;
  int	dumpindex;
#endif

size_t		unit;
size_t		page_size;

void 
init_memory(void)
{
	static bool already_initialized = FALSE;

	if (already_initialized != FALSE)
		return;
	already_initialized = TRUE;

	/*
	** Convert all the sizes are from kilobytes to bytes and
	** make sure they are multiples of the page and cache sizes.
	*/

	page_size = getpagesize();
	unit = max(page_size, pcache_size);

#ifdef CONSERVATIVE_GC
	heap_zone_size      = 0;
	heap_size	    = 0;
	solutions_heap_zone_size = 0;
	solutions_heap_size = 0;
	global_heap_zone_size = 0;
	global_heap_size    = 0;
#else
	heap_zone_size      = round_up(heap_zone_size * 1024, unit);
	heap_size           = round_up(heap_size * 1024, unit);
	solutions_heap_zone_size = round_up(solutions_heap_zone_size * 1024, 
		unit);
	solutions_heap_size = round_up(solutions_heap_size * 1024, unit);
	global_heap_zone_size = round_up(global_heap_zone_size * 1024, unit);
	global_heap_size    = round_up(global_heap_size * 1024, unit);
#endif

	detstack_size       = round_up(detstack_size * 1024, unit);
	detstack_zone_size  = round_up(detstack_zone_size * 1024, unit);
	nondstack_size      = round_up(nondstack_size * 1024, unit);
	nondstack_zone_size = round_up(nondstack_zone_size * 1024, unit);

#ifdef MR_USE_TRAIL
	trail_size       = round_up(trail_size * 1024, unit);
	trail_zone_size  = round_up(trail_zone_size * 1024, unit);
#else
	trail_size	    = 0;
	trail_zone_size	    = 0;
#endif

	/*
	** If the zone sizes were set to something too big, then
	** set them to a single unit.
	*/

#ifndef CONSERVATIVE_GC
	if (heap_zone_size >= heap_size) {
		heap_zone_size = unit;
	}
	if (solutions_heap_zone_size >= solutions_heap_size) {
		solutions_heap_zone_size = unit;
	}
	if (global_heap_zone_size >= global_heap_size) {
		global_heap_zone_size = unit;
	}
#endif

	if (detstack_zone_size >= detstack_size) {
		detstack_zone_size = unit;
	}

	if (nondstack_zone_size >= nondstack_size) {
		nondstack_zone_size = unit;
	}

#ifdef MR_USE_TRAIL
	if (trail_zone_size >= trail_size) {
		trail_zone_size = unit;
	}
#endif

	init_zones();
	setup_signals();

	if (memdebug) debug_memory();
} /* end init_memory() */

#ifdef	CONSERVATIVE_GC

void *
allocate_bytes(size_t numbytes)
{
	void	*tmp;

	tmp = GC_MALLOC(numbytes);
	
	if (tmp == NULL) {
		fatal_error("could not allocate memory");
	}

	return tmp;
}

#else /* not CONSERVATIVE_GC */

void *
allocate_bytes(size_t numbytes)
{
	void	*tmp;

	tmp = malloc(numbytes);
	
	if (tmp == NULL) {
		fatal_error("could not allocate memory");
	}

	return tmp;
}

#endif

void 
deallocate_memory(void *ptr)
{
#ifdef CONSERVATIVE_GC
	GC_FREE(ptr);

#else
	free(ptr);
#endif
}

		/* Note: checked_malloc()ed structures */
		/* never contain pointers into GCed    */
		/* memory, so we don't need to         */
		/* GC_malloc() them. (cf. newmem())    */
void *
checked_malloc(size_t n)
{
	reg     void    *p;

	p = malloc(n);
	if (p == NULL && n != 0) {
		fatal_error("ran out of memory");
	}

	return p;
}


void *
checked_realloc(void *old, size_t n)
{
	reg     void    *p;

	p = realloc(old, n);
	if (p == NULL && n != 0) {
		fatal_error("ran out of memory");
	}

	return p;
}

