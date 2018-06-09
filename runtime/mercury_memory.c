// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1994-2000,2002-2004, 2006, 2008, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module defines the register array and data regions of the
// execution algorithm.
// They are defined together here to allow us to control how they map
// onto direct mapped caches.
// We allocate a large arena, preferably aligned on a boundary that
// is a multiple of both the page size and the primary cache size.
//
// We then allocate the heap and the stacks in such a way that
//
//  the register array
//  the bottom of the heap
//  the bottom of the detstack
//  the bottom of the nondetstack
//
// all start at different offsets from multiples of the primary cache size.
// This should reduce cache conflicts (especially for small programs).
//
// If the operating system of the machine supports the mprotect syscall,
// we also protect a chunk at the end of each area against access,
// thus detecting area overflow.
//
// The code for handling the allocation and management of different
// memory zones is in mercury_memory_zones.{c,h}.
// The code for handling overflows and memory access errors in general
// is in mercury_memory_handlers.{c,h}.

////////////////////////////////////////////////////////////////////////////

#include "mercury_imp.h"

// This include must come before anything else that might include <signal.h>.
// See the comments in mercury_signal.h.

#include "mercury_signal.h"

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif

#ifdef  MR_HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef  MR_HAVE_UCONTEXT_H
  #include <ucontext.h>
#endif

#ifdef  MR_HAVE_SYS_UCONTEXT_H
  #include <sys/ucontext.h>
#endif

#include "mercury_imp.h"
#include "mercury_trace_base.h"
#include "mercury_memory_handlers.h"

////////////////////////////////////////////////////////////////////////////

// NOTE: getpagesize() is not officially supported on MinGW (there is no
// declaration in the system headers), but one of the supporting libraries
// used by GCC does define a symbol with that name. Consequently on MinGW,
// we need to use GetSystemInfo() even though MR_HAVE_GETPAGESIZE is defined.

#if defined(MR_HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  #define   getpagesize()   sysconf(_SC_PAGESIZE)
#elif !defined(MR_HAVE_GETPAGESIZE) || defined(MR_MINGW)
  #if defined(MR_WIN32_GETSYSTEMINFO)
    #include "mercury_windows.h"

    #define getpagesize() MR_win32_getpagesize()

    // NOTE: we avoid naming the following getpagesize() since that name
    // is already used on MinGW.

    static size_t
    MR_win32_getpagesize(void)
    {
        SYSTEM_INFO SysInfo;
        GetSystemInfo(&SysInfo);
        return (size_t) SysInfo.dwPageSize;
    }
  #else
    #define getpagesize()   8192
  #endif
#endif

////////////////////////////////////////////////////////////////////////////

// Define the memory zones used by the Mercury runtime.
// (The trail zone is declared in mercury_trail.c.)
// XXX All the zones should be in mercury_engine.h

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
  MR_MemoryZone *MR_genstack_zone;
  MR_MemoryZone *MR_cutstack_zone;
  MR_MemoryZone *MR_pnegstack_zone;
#endif

size_t      MR_unit;
size_t      MR_page_size;

void
MR_init_memory(void)
{
    static MR_bool already_initialized = MR_FALSE;

    if (already_initialized != MR_FALSE) {
        return;
    }

    already_initialized = MR_TRUE;

    // Convert all the sizes are from kilobytes to bytes and make sure
    // (a) they are multiples of the page size, and (b) at least as big
    // as the cache size.

    MR_page_size = getpagesize();
    MR_unit = MR_round_up(MR_max(MR_page_size, MR_pcache_size), MR_page_size);

#ifdef MR_CONSERVATIVE_GC
    MR_heap_size                = 0;
    MR_heap_zone_size           = 0;
    MR_solutions_heap_size      = 0;
    MR_solutions_heap_zone_size = 0;
    MR_global_heap_size         = 0;
    MR_global_heap_zone_size    = 0;
    MR_debug_heap_size          = 0;
    MR_debug_heap_zone_size     = 0;
    MR_heap_margin_size         = 0;
#else
    MR_kilobytes_to_bytes_and_round_up(MR_heap_size);
    MR_kilobytes_to_bytes_and_round_up(MR_heap_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_solutions_heap_size);
    MR_kilobytes_to_bytes_and_round_up(MR_solutions_heap_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_global_heap_size);
    MR_kilobytes_to_bytes_and_round_up(MR_global_heap_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_debug_heap_size);
    MR_kilobytes_to_bytes_and_round_up(MR_debug_heap_zone_size);
    // Note that there is no need for the heap margin to be rounded up.
    MR_heap_margin_size  = MR_heap_margin_size * 1024;
#endif
    MR_kilobytes_to_bytes_and_round_up(MR_detstack_size);
#ifndef MR_STACK_SEGMENTS
    MR_kilobytes_to_bytes_and_round_up(MR_small_detstack_size);
#endif
    MR_kilobytes_to_bytes_and_round_up(MR_detstack_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_nondetstack_size);
#ifndef MR_STACK_SEGMENTS
    MR_kilobytes_to_bytes_and_round_up(MR_small_nondetstack_size);
#endif
    MR_kilobytes_to_bytes_and_round_up(MR_nondetstack_zone_size);
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_kilobytes_to_bytes_and_round_up(MR_genstack_size);
    MR_kilobytes_to_bytes_and_round_up(MR_genstack_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_cutstack_size);
    MR_kilobytes_to_bytes_and_round_up(MR_cutstack_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_pnegstack_size);
    MR_kilobytes_to_bytes_and_round_up(MR_pnegstack_zone_size);
#else
    MR_genstack_size       = 0;
    MR_genstack_zone_size  = 0;
    MR_cutstack_size       = 0;
    MR_cutstack_zone_size  = 0;
    MR_pnegstack_size      = 0;
    MR_pnegstack_zone_size = 0;
#endif

#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
    MR_kilobytes_to_bytes_and_round_up(MR_gen_detstack_size);
    MR_kilobytes_to_bytes_and_round_up(MR_gen_nondetstack_size);
    MR_kilobytes_to_bytes_and_round_up(MR_gen_detstack_zone_size);
    MR_kilobytes_to_bytes_and_round_up(MR_gen_nondetstack_zone_size);
#else
    MR_gen_detstack_size         = 0;
    MR_gen_nondetstack_size      = 0;
    MR_gen_detstack_zone_size    = 0;
    MR_gen_nondetstack_zone_size = 0;
#endif

#ifdef  MR_USE_TRAIL
    MR_kilobytes_to_bytes_and_round_up(MR_trail_size);
    MR_kilobytes_to_bytes_and_round_up(MR_trail_zone_size);
#else
    MR_trail_size        = 0;
    MR_trail_zone_size   = 0;
#endif

    // If the zone sizes were set to something too big, then
    // set them to a single unit.

#ifndef MR_CONSERVATIVE_GC
    if (MR_heap_zone_size >= MR_heap_size) {
        MR_heap_zone_size = MR_unit;
    }
    if (MR_solutions_heap_zone_size >= MR_solutions_heap_size) {
        MR_solutions_heap_zone_size = MR_unit;
    }
    if (MR_global_heap_zone_size >= MR_global_heap_size) {
        MR_global_heap_zone_size = MR_unit;
    }
    if (MR_heap_margin_size >= MR_heap_size) {
        MR_heap_margin_size = MR_unit;
    }
#endif

    if (MR_detstack_zone_size >= MR_detstack_size) {
        MR_detstack_zone_size = MR_unit;
    }

    if (MR_nondetstack_zone_size >= MR_nondetstack_size) {
        MR_nondetstack_zone_size = MR_unit;
    }

#ifdef MR_USE_TRAIL
    if (MR_trail_zone_size >= MR_trail_size) {
        MR_trail_zone_size = MR_unit;
    }
#endif

    if (MR_stack_margin_size_words >= (sizeof(MR_Word) * MR_detstack_size)) {
        MR_fatal_error("MR_init_memory: stack margin size far too big");
    }

    MR_init_zones();

} // end MR_init_memory()

////////////////////////////////////////////////////////////////////////////

// These routines allocate memory that will NOT be scanned by the conservative
// garbage collector. You MUST NOT use these to store pointers into GC'ed
// memory.

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
    size_t  len;
    char    *copy;

    if (s == NULL) {
        return NULL;
    } else {
        len = strlen(s);
        copy = MR_malloc(len + 1);
        strcpy(copy, s);
        return copy;
    }
}

void
MR_ensure_big_enough_buffer(char **buffer_ptr, int *buffer_size_ptr,
    int needed_size)
{
    if (*buffer_size_ptr < needed_size) {
        *buffer_size_ptr = 2 * needed_size;
        if (*buffer_ptr == NULL) {
            *buffer_ptr = MR_malloc(*buffer_size_ptr);
        } else {
            *buffer_ptr = MR_realloc((void *) *buffer_ptr, *buffer_size_ptr);
        }
    }
}

////////////////////////////////////////////////////////////////////////////

// These routines allocate memory that will be scanned by the
// conservative garbage collector.
//
// XXX This is inefficient. If MR_BOEHM_GC is enabled, we should set
// `GC_oom_fn' (see boehm_gc/gc.h) rather than testing the return value
// from GC_MALLOC() or GC_MALLOC_UNCOLLECTABLE().

void *
MR_GC_malloc(size_t num_bytes)
{
    void    *ptr;

#ifdef  MR_CONSERVATIVE_GC
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
MR_GC_malloc_atomic(size_t num_bytes)
{
    void    *ptr;

#ifdef  MR_CONSERVATIVE_GC
    ptr = GC_MALLOC_ATOMIC(num_bytes);
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
    void    *ptr;

#ifdef  MR_CONSERVATIVE_GC
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

#ifdef  MR_CONSERVATIVE_GC
    ptr = GC_REALLOC(old_ptr, num_bytes);
#else
    ptr = realloc(old_ptr, num_bytes);
#endif
    if (ptr == NULL && num_bytes != 0) {
        MR_fatal_error("ran out of memory");
    }

    return ptr;
}

#ifdef MR_BOEHM_GC
void*
MR_weak_ptr_read_unsafe(void* weak_ptr_) {
    MR_weak_ptr *weak_ptr = weak_ptr_;

    // Even though we check for NULL in the macro we must re-check here
    // while holding the GC's allocation lock.

    if (MR_NULL_WEAK_PTR != *weak_ptr) {
        return GC_REVEAL_POINTER(*weak_ptr);
    } else {
        return NULL;
    }
}
#endif

////////////////////////////////////////////////////////////////////////////

void *
MR_GC_malloc_attrib(size_t num_bytes, void *attrib)
{
    MR_Word     *ptr;

#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    ptr = MR_GC_malloc(sizeof(MR_Word) + num_bytes);
    *ptr = (MR_Word) attrib;
    ptr++;
#else
    ptr = MR_GC_malloc(num_bytes);
#endif

    return ptr;
}

void *
MR_GC_malloc_uncollectable_attrib(size_t num_bytes, void *attrib)
{
    MR_Word     *ptr;

#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    ptr = MR_GC_malloc_uncollectable(num_bytes + sizeof(MR_Word));
    *ptr = (MR_Word) attrib;
    ptr++;
#else
    ptr = MR_GC_malloc_uncollectable(num_bytes);
#endif

    return ptr;
}

void *
MR_GC_realloc_attrib(void *ptr, size_t num_bytes)
{
    MR_Word     *wptr = ptr;

#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    wptr = MR_GC_realloc(wptr - 1, num_bytes + sizeof(MR_Word));
    wptr = wptr + 1;
#else
    wptr = MR_GC_realloc(wptr, num_bytes);
#endif

    return wptr;
}

void
MR_GC_free_attrib(void *ptr)
{
#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    ptr = (char *) ptr - sizeof(MR_Word);
#endif
    MR_GC_free(ptr);
}

////////////////////////////////////////////////////////////////////////////

void *
MR_new_object_func(size_t num_bytes, MR_AllocSiteInfoPtr alloc_id,
    const char *name)
{
    size_t  num_words;
    MR_Word dest;

    num_words = MR_bytes_to_words(num_bytes);
    MR_incr_hp_msg(dest, num_words, alloc_id, name);
    return (void *) dest;
}

void *
MR_new_object_atomic_func(size_t num_bytes, MR_AllocSiteInfoPtr alloc_id,
    const char *name)
{
    size_t  num_words;
    MR_Word dest;

    num_words = MR_bytes_to_words(num_bytes);
    MR_incr_hp_atomic_msg(dest, num_words, alloc_id, name);
    return (void *) dest;
}

////////////////////////////////////////////////////////////////////////////
