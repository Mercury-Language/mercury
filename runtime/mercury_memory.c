/*
** Copyright (C) 1994-2000,2002 The University of Melbourne.
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
** This include must come before anything else that might include <signal.h>.
** See the commments in mercury_signal.h.
*/
#include "mercury_signal.h"

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
** XXX All the zones should be in mercury_engine.h
*/

#ifdef	MR_USE_MINIMAL_MODEL
  MR_MemoryZone *MR_generatorstack_zone;
  MR_MemoryZone *MR_cutstack_zone;
#endif

size_t		MR_unit;
size_t		MR_page_size;

void 
MR_init_memory(void)
{
	static bool already_initialized = FALSE;

	if (already_initialized != FALSE)
		return;

	already_initialized = TRUE;

	/*
	** Convert all the sizes are from kilobytes to bytes and
	** make sure they are multiples of the page and cache sizes.
	*/

	MR_page_size = getpagesize();
	MR_unit = max(MR_page_size, MR_pcache_size);

#ifdef CONSERVATIVE_GC
	MR_heap_size		 = 0;
	MR_heap_zone_size	 = 0;
	MR_solutions_heap_size	 = 0;
	MR_solutions_heap_zone_size = 0;
	MR_global_heap_size	 = 0;
	MR_global_heap_zone_size = 0;
	MR_debug_heap_size	 = 0;
	MR_debug_heap_zone_size	 = 0;
#else
	MR_heap_size		 = MR_round_up(MR_heap_size * 1024,
					MR_unit);
	MR_heap_zone_size	 = MR_round_up(MR_heap_zone_size * 1024,
					MR_unit);
	MR_solutions_heap_size	 = MR_round_up(MR_solutions_heap_size * 1024,
					MR_unit);
	MR_solutions_heap_zone_size = MR_round_up(
					MR_solutions_heap_zone_size * 1024, 
					MR_unit);
	MR_global_heap_size	 = MR_round_up(MR_global_heap_size * 1024,
					MR_unit);
	MR_global_heap_zone_size = MR_round_up(MR_global_heap_zone_size * 1024,
					MR_unit);
	MR_debug_heap_size	 = MR_round_up(MR_debug_heap_size * 1024,
					MR_unit);
	MR_debug_heap_zone_size	 = MR_round_up(MR_debug_heap_zone_size * 1024,
					MR_unit);
#endif
	MR_detstack_size	 = MR_round_up(MR_detstack_size * 1024,
					MR_unit);
	MR_detstack_zone_size	 = MR_round_up(MR_detstack_zone_size * 1024,
					MR_unit);
	MR_nondstack_size	 = MR_round_up(MR_nondstack_size * 1024,
					MR_unit);
	MR_nondstack_zone_size	 = MR_round_up(MR_nondstack_zone_size * 1024,
					MR_unit);
#ifdef	MR_USE_MINIMAL_MODEL
	MR_generatorstack_size	 = MR_round_up(MR_generatorstack_size * 1024,
					MR_unit);
	MR_generatorstack_zone_size = MR_round_up(
					MR_generatorstack_zone_size * 1024,
					MR_unit);
	MR_cutstack_size	 = MR_round_up(MR_cutstack_size * 1024,
					MR_unit);
	MR_cutstack_zone_size	 = MR_round_up(MR_cutstack_zone_size * 1024,
					MR_unit);
#endif

#ifdef	MR_USE_TRAIL
	MR_trail_size		 = MR_round_up(MR_trail_size * 1024,
					MR_unit);
	MR_trail_zone_size	 = MR_round_up(MR_trail_zone_size * 1024,
					MR_unit);
#else
	MR_trail_size		 = 0;
	MR_trail_zone_size	 = 0;
#endif

	/*
	** If the zone sizes were set to something too big, then
	** set them to a single unit.
	*/

#ifndef CONSERVATIVE_GC
	if (MR_heap_zone_size >= MR_heap_size) {
		MR_heap_zone_size = MR_unit;
	}
	if (MR_solutions_heap_zone_size >= MR_solutions_heap_size) {
		MR_solutions_heap_zone_size = MR_unit;
	}
	if (MR_global_heap_zone_size >= MR_global_heap_size) {
		MR_global_heap_zone_size = MR_unit;
	}
#endif

	if (MR_detstack_zone_size >= MR_detstack_size) {
		MR_detstack_zone_size = MR_unit;
	}

	if (MR_nondstack_zone_size >= MR_nondstack_size) {
		MR_nondstack_zone_size = MR_unit;
	}

#ifdef MR_USE_TRAIL
	if (MR_trail_zone_size >= MR_trail_size) {
		MR_trail_zone_size = MR_unit;
	}
#endif

	MR_init_zones();
	MR_setup_signals();

	if (MR_memdebug) {
		MR_debug_memory();
	}
} /* end MR_init_memory() */

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
