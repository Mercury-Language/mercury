/*
** Copyright (C) 1994-2000 The University of Melbourne.
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

/*
** XXX This code is duplicated in three files:
** mercury_memory.c, mercury_memory_handlers.c, and mercury_signal.c.
*/
#ifdef HAVE_SIGCONTEXT_STRUCT
  /*
  ** Some versions of Linux call it struct sigcontext_struct, some call it
  ** struct sigcontext.  The following #define eliminates the differences.
  */
  #define sigcontext_struct sigcontext /* must be before #include <signal.h> */
  struct sigcontext; /* this forward decl avoids a gcc warning in signal.h */

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

#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif

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
#include "mercury_trace_base.h"
#include "mercury_memory_handlers.h"

/*---------------------------------------------------------------------------*/

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  #define	getpagesize()	sysconf(_SC_PAGESIZE)
#elif !defined(HAVE_GETPAGESIZE)
  #if defined(MR_WIN32_GETSYSTEMINFO)
    #include <windows.h>

    static size_t
    getpagesize(void)
    {
	SYSTEM_INFO SysInfo;
	GetSystemInfo(&SysInfo);
	return (size_t) SysInfo.dwPageSize;
    }
  #else
    #define	getpagesize()	8192
  #endif
#endif

/*---------------------------------------------------------------------------*/

#ifdef	HAVE_SIGINFO
  static	bool	try_munprotect(void *address, void *context);
  static	char	*explain_context(void *context);
#endif /* HAVE_SIGINFO */

/*
** Define the memory zones used by the Mercury runtime.
** (The trail zone is declared in mercury_trail.c.)
*/
MemoryZone *detstack_zone;
MemoryZone *nondetstack_zone;
#ifndef CONSERVATIVE_GC
  MemoryZone *heap_zone;
  MemoryZone *solutions_heap_zone;
#endif
#ifdef	MR_USE_MINIMAL_MODEL
  MemoryZone *generatorstack_zone;
  MemoryZone *cutstack_zone;
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
	heap_size		 = 0;
	heap_zone_size		 = 0;
	solutions_heap_size	 = 0;
	solutions_heap_zone_size = 0;
	global_heap_size	 = 0;
	global_heap_zone_size	 = 0;
	debug_heap_size		 = 0;
	debug_heap_zone_size	 = 0;
#else
	heap_size		 = round_up(heap_size * 1024, unit);
	heap_zone_size		 = round_up(heap_zone_size * 1024, unit);
	solutions_heap_size	 = round_up(solutions_heap_size * 1024, unit);
	solutions_heap_zone_size = round_up(solutions_heap_zone_size * 1024, 
					unit);
	global_heap_size	 = round_up(global_heap_size * 1024, unit);
	global_heap_zone_size	 = round_up(global_heap_zone_size * 1024, unit);
	debug_heap_size		 = round_up(debug_heap_size * 1024, unit);
	debug_heap_zone_size	 = round_up(debug_heap_zone_size * 1024, unit);
#endif
	detstack_size		 = round_up(detstack_size * 1024, unit);
	detstack_zone_size	 = round_up(detstack_zone_size * 1024, unit);
	nondstack_size		 = round_up(nondstack_size * 1024, unit);
	nondstack_zone_size	 = round_up(nondstack_zone_size * 1024, unit);
#ifdef	MR_USE_MINIMAL_MODEL
	generatorstack_size	 = round_up(generatorstack_size * 1024, unit);
	generatorstack_zone_size = round_up(generatorstack_zone_size * 1024,
					unit);
	cutstack_size		 = round_up(cutstack_size * 1024, unit);
	cutstack_zone_size	 = round_up(cutstack_zone_size * 1024, unit);
#endif

#ifdef	MR_USE_TRAIL
	trail_size		 = round_up(trail_size * 1024, unit);
	trail_zone_size		 = round_up(trail_zone_size * 1024, unit);
#else
	trail_size		 = 0;
	trail_zone_size		 = 0;
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

	if (MR_memdebug) debug_memory();
} /* end init_memory() */

/*---------------------------------------------------------------------------*/

/*
** These routines allocate memory that will NOT be scanned by the
** conservative garbage collector.  You MUST NOT uses these to
** store pointers into GC'ed memory.
** 
*/

void *
MR_malloc(size_t n)
{
	void    *ptr;

	ptr = malloc(n);
	if (ptr == NULL && n != 0) {
		MR_fatal_error("ran out of memory");
	}

	return ptr;
}

void *
MR_realloc(void *old_ptr, size_t num_bytes)
{
	void    *ptr;

	ptr = realloc(old_ptr, num_bytes);
	if (ptr == NULL && num_bytes != 0) {
		MR_fatal_error("ran out of memory");
	}

	return ptr;
}

char *
MR_copy_string(const char *s)
{
	int	len;
	char	*copy;

	len = strlen(s);
	copy = MR_malloc(len + 1);
	strcpy(copy, s);
	return copy;
}

/*---------------------------------------------------------------------------*/

/*
** These routines allocate memory that will be scanned by the
** conservative garbage collector.
**
** XXX This is inefficient.  If CONSERVATIVE_GC is enabled,
** we should set `GC_oom_fn' (see boehm_gc/gc.h) rather than
** testing the return value from GC_MALLOC() or GC_MALLOC_UNCOLLECTABLE().
*/

void *
MR_GC_malloc(size_t num_bytes)
{
	void	*ptr;

#ifdef	CONSERVATIVE_GC
	ptr = GC_MALLOC(num_bytes);
#else
	ptr = malloc(num_bytes);
#endif
	
	if (ptr == NULL && num_bytes != 0) {
		MR_fatal_error("could not allocate memory");
	}

	return ptr;
}

void *
MR_GC_malloc_uncollectable(size_t num_bytes)
{
	void	*ptr;

#ifdef	CONSERVATIVE_GC
	ptr = GC_MALLOC_UNCOLLECTABLE(num_bytes);
#else
	ptr = malloc(num_bytes);
#endif
	
	if (ptr == NULL && num_bytes != 0) {
		MR_fatal_error("could not allocate memory");
	}

	return ptr;
}

void *
MR_GC_realloc(void *old_ptr, size_t num_bytes)
{
	void    *ptr;

#ifdef	CONSERVATIVE_GC
	ptr = GC_REALLOC(old_ptr, num_bytes);
#else
	ptr = realloc(old_ptr, num_bytes);
#endif
	if (ptr == NULL && num_bytes != 0) {
		MR_fatal_error("ran out of memory");
	}

	return ptr;
}

/*---------------------------------------------------------------------------*/
