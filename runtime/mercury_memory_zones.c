// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2000, 2002-2007, 2010-2012 The University of Melbourne.
// Copyright (C) 2013-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module defines the MR_MemoryZone data structure and operations
// for managing the memory zones.
//
// The offset of each zone can be supplied to allow us to control how
// they map onto direct mapped caches. The provided next_offset()
// function can be used to generate these offsets.
//
// We allocate a large arena, preferably aligned on a boundary that
// is a multiple of both the page size and the primary cache size.
//
// If the operating system of the machine supports the mprotect syscall,
// we also protect a chunk at the end of each area against access,
// thus detecting area overflow.

////////////////////////////////////////////////////////////////////////////

#include "mercury_imp.h"

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>

#include <stdlib.h>         // for memalign and posix_memalign

#ifdef MR_HAVE_MALLOC_H
    #include <malloc.h>     // for memalign
#endif

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

#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
  #include "mercury_atomic_ops.h"
#endif

#include "mercury_memory_handlers.h"
#include "mercury_runtime_util.h"

// This macro can be used to update a high water mark of a statistic.

#define MR_UPDATE_HIGHWATER(max, cur)                                       \
    do {                                                                    \
        if ((max) < (cur)) {                                                \
            (max) = (cur);                                                  \
        }                                                                   \
    } while (0);

#ifdef MR_PROFILE_ZONES
// These values track the number of zones and the total size.

#ifdef MR_THREAD_SAFE
static  MercuryLock memory_zones_stats_lock;
#endif

static  MR_Integer  MR_num_zones = 0;
static  MR_Integer  MR_total_zone_size_net = 0;
static  MR_Integer  MR_total_zone_size_gross = 0;

static  MR_Integer  MR_max_num_zones = 0;
static  MR_Integer  MR_max_total_zone_size_net = 0;
static  MR_Integer  MR_max_total_zone_size_gross = 0;
#endif

static  void    MR_setup_redzones(MR_MemoryZone *zone);

static  void    *MR_alloc_zone_memory(size_t size);
static  void    *MR_realloc_zone_memory(void *old_base, size_t copy_size,
                    size_t new_size);

////////////////////////////////////////////////////////////////////////////

// MR_PROTECTPAGE is now defined if we have some sort of mprotect like
// functionality, all checks for MR_HAVE_MPROTECT should now use
// MR_PROTECTPAGE.

#if defined(MR_HAVE_MPROTECT)

int
MR_protect_pages(void *addr, size_t size, int prot_flags)
{
    return mprotect((char *) addr, size, prot_flags);
}
#endif

////////////////////////////////////////////////////////////////////////////

#if defined(MR_CONSERVATIVE_GC)

static void *
MR_alloc_zone_memory(size_t size)
{
    MR_Word *ptr;

    ptr = GC_MALLOC_UNCOLLECTABLE(size);
    // Mark the memory zone as being allocated by the Mercury runtime.
    // We do not use a helper function like MR_GC_malloc_uncollectable_attrib
    // as that returns an offset pointer which can interfere with memory page
    // protection. The first word will be within the redzone so it will not be
    // clobbered later.

    *ptr = (MR_Word) MR_ALLOC_SITE_RUNTIME;
    return ptr;
}

static void *
MR_realloc_zone_memory(void *old_base, size_t copy_size, size_t new_size)
{
    void    *ptr;

    ptr = GC_MALLOC_UNCOLLECTABLE(new_size);
    (void) MR_memcpy(ptr, old_base, copy_size);
    GC_free(old_base);
    return ptr;
}

static void
MR_dealloc_zone_memory(void *base, size_t size)
{
    (void) size;
    GC_free(base);
}

#elif defined(MR_HAVE_POSIX_MEMALIGN_XXX)

static void *
MR_alloc_zone_memory(size_t size)
{
    void    *ptr;
    int     res;

    res = posix_memalign(&ptr, MR_unit, size);
    if (res != 0) {
        return NULL;
    }

    return ptr;
}

static void *
MR_realloc_zone_memory(void *old_base, size_t copy_size, size_t new_size)
{
    void    *ptr;
    int     res;

    res = posix_memalign(&ptr, MR_unit, new_size);
    if (res != 0) {
        return NULL;
    }

    (void) MR_memcpy(ptr, old_base, copy_size);
    free(old_base);
    return ptr;
}

static void
MR_dealloc_zone_memory(void *base, size_t size)
{
    (void) size;
    free(base);
}

#elif defined(MR_HAVE_MEMALIGN)

static void *
MR_alloc_zone_memory(size_t size)
{
    return memalign(MR_unit, size);
}

static void *
MR_realloc_zone_memory(void *old_base, size_t copy_size, size_t new_size)
{
    void    *ptr;

    ptr = memalign(MR_unit, new_size);
    (void) MR_memcpy(ptr, old_base, copy_size);
    // We should call free(old_base) here. However, there is no guarantee that
    // the system supports the freeing of memory allocated with memalign via
    // calls to free(). We therefore don't free old_base. This is why we prefer
    // posix_memalign if it is available.

    return ptr;
}

static void
MR_dealloc_zone_memory(void *base, size_t size)
{
    // See above about calling free(base) here.
    (void) base;
    (void) size;
}

#else

static void *
MR_alloc_zone_memory(size_t size)
{
    return malloc(size);
}

static void *
MR_realloc_zone_memory(void *old_base, size_t copy_size, size_t new_size)
{
    // The copying is done by realloc.
    return realloc(old_base, new_size);
}

static void
MR_dealloc_zone_memory(void *base, size_t size)
{
    (void) size;
    free(base);
}

#endif

// DESCRIPTION
//
// The function mprotect() changes the access protections on the mappings
// specified by the range [addr, addr + len) to be that specified by prot.
// Legitimate values for prot are the same as those permitted for mmap
// and are defined in <sys/mman.h> as:
//
// PROT_READ    page can be read
// PROT_WRITE   page can be written
// PROT_EXEC    page can be executed
// PROT_NONE    page can not be accessed

#ifdef MR_PROTECTPAGE

  #define NORMAL_PROT (PROT_READ|PROT_WRITE)

  #ifdef MR_CONSERVATIVE_GC
    // The conservative garbage collectors scans through all these areas,
    // so we need to allow reads.
    // XXX This probably causes efficiency problems: too much memory
    // for the GC to scan, and it probably all gets paged in.

    #define REDZONE_PROT PROT_READ
  #else
    #define REDZONE_PROT PROT_NONE
  #endif

  // The BSDI BSD/386 1.1 headers don't define PROT_NONE.
  #ifndef PROT_NONE
    #define PROT_NONE 0
  #endif

#endif // MR_PROTECTPAGE

////////////////////////////////////////////////////////////////////////////

static void             MR_init_offsets(void);

static MR_MemoryZone    *MR_get_free_zone(size_t size);
static void             MR_add_zone_to_used_list(MR_MemoryZone *zone);
static void             MR_remove_zone_from_used_list(MR_MemoryZone *zone);
static void             MR_return_zone_to_free_list(MR_MemoryZone *zone);
static void             MR_free_zone(MR_MemoryZone *zone);
static size_t           get_zone_alloc_size(MR_MemoryZone *zone);
static void             MR_maybe_gc_zones(void);

#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
static void
MR_configure_redzone_size(MR_MemoryZone *zone, size_t new_redsize);
#endif

static MR_MemoryZone    *MR_create_new_zone(size_t desired_size,
                            size_t redzone_size, size_t offset);

// We manage the handing out of offsets through the cache by computing
// the offsets once and storing them in an array (in shared memory
// if necessary). We then maintain a global counter used to index the array
// which we increment (modulo the size of the array) after handing out
// each offset.
//
// We use this counter ONLY for the first segment of each memory area.
// By the time we need a second, third etc segment in a memory area,
// we are pretty much equally likely to be using all the parts of
// the existing segments of the other memory areas.

#define CACHE_SLICES    8

static  size_t          *offset_vector;
static MR_THREADSAFE_VOLATILE MR_Integer offset_counter;
extern  size_t          next_offset(void);

static MR_THREADSAFE_VOLATILE MR_Unsigned zone_id_counter = 0;

#if ! defined(MR_LL_PARALLEL_CONJ) && defined(MR_THREAD_SAFE)
static MercuryLock  zone_id_counter_lock;
#endif

// This list contains used zones that need a signal handler, for example those
// that have redzones. Other used zones may exist that are not on this list
// because:
//
//   1) They don't have a redzone.
//
//   2) Putting them on this list in a threadsafe grade requires extra
//      synchronisation.
//
// Zones are removed from this list when they're returned to the free list.
// We only attempt to remove the zones that we would have added.

static MR_MemoryZone * MR_THREADSAFE_VOLATILE used_memory_zones = NULL;

#ifdef  MR_THREAD_SAFE
  // You must take this lock to write to either of the zone lists, or to read
  // the complete zone lists. Reading a zone list without taking the lock is
  // also supported _iff_ partial information is okay. Any code that writes
  // the list must guarantee that memory writes occur in the correct order so
  // that the list is always well formed from a reader's perspective.
  //
  // This is necessary so that signal handlers can read the list without taking
  // a lock. They may not take a lock because pthread_mutex_lock cannot be
  // used safely within a signal handler.

  static MercuryLock    memory_zones_lock;
#endif

void
MR_init_zones()
{
#ifdef  MR_THREAD_SAFE
    pthread_mutex_init(&memory_zones_lock, MR_MUTEX_ATTR);
#ifdef MR_PROFILE_ZONES
    pthread_mutex_init(&memory_zones_stats_lock, MR_MUTEX_ATTR);
#endif
#if ! defined(MR_LL_PARALLEL_CONJ)
    pthread_mutex_init(&zone_id_counter_lock, MR_MUTEX_ATTR);
#endif
#endif

    MR_init_offsets();
}

static void
MR_init_offsets()
{
    int     i;
    size_t  fake_reg_offset;

    offset_counter = 0;

    offset_vector = MR_GC_NEW_ARRAY_ATTRIB(size_t, CACHE_SLICES - 1,
        MR_ALLOC_SITE_RUNTIME);

    fake_reg_offset = (MR_Unsigned) MR_fake_reg % MR_pcache_size;

    for (i = 0; i < CACHE_SLICES - 1; i++) {
        offset_vector[i] =
            (fake_reg_offset + MR_pcache_size * i / CACHE_SLICES)
            % MR_pcache_size;
    }
}

static void
MR_add_zone_to_used_list(MR_MemoryZone *zone)
{
    MR_LOCK(&memory_zones_lock, "MR_add_zone_to_used_list");

    zone->MR_zone_next = used_memory_zones;
    // This change must occur before we replace the head of the list.

#ifdef MR_THREAD_SAFE
    MR_CPU_SFENCE;
#endif
    used_memory_zones = zone;

    MR_UNLOCK(&memory_zones_lock, "MR_add_zone_to_used_list");
}

static void
MR_free_zone(MR_MemoryZone *zone)
{
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    size_t          redsize;
    int             res;
    char            errbuf[MR_STRERROR_BUF_SIZE];

    redsize = zone->MR_zone_redzone_size;
    res = MR_protect_pages((char *) zone->MR_zone_redzone,
        redsize + MR_page_size, NORMAL_PROT);
    if (res) {
        MR_fatal_error(
            "Could not unprotect memory pages in MR_free_zone: %s",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
#endif

#ifdef MR_PROFILE_ZONES
    MR_LOCK(&memory_zones_stats_lock, "MR_free_zone");
    MR_num_zones--;
    MR_total_zone_size_net -= zone->MR_zone_desired_size;
    MR_total_zone_size_gross -=
        (MR_Integer) zone->MR_zone_top - (MR_Integer) zone->MR_zone_bottom;
    MR_UNLOCK(&memory_zones_stats_lock, "MR_free_zone");
#endif

    MR_dealloc_zone_memory(zone->MR_zone_bottom,
        ((char *) zone->MR_zone_top) - ((char *) zone->MR_zone_bottom));
}

static void
MR_remove_zone_from_used_list(MR_MemoryZone *zone)
{
    MR_MemoryZone   *prev;
    MR_MemoryZone   *tmp;

    // Find the zone on the used list, and unlink it from the list,
    // then link it onto the start of the free-list.

    MR_LOCK(&memory_zones_lock, "MR_remove_zone_from_used_list");
    prev = NULL;
    tmp = used_memory_zones;
    while (tmp != NULL && tmp != zone) {
        prev = tmp;
        tmp = tmp->MR_zone_next;
    }

    if (tmp == NULL) {
        MR_fatal_error("memory zone not found!");
    }

    if (prev == NULL) {
        used_memory_zones = used_memory_zones->MR_zone_next;
    } else {
        prev->MR_zone_next = tmp->MR_zone_next;
    }
    MR_UNLOCK(&memory_zones_lock, "MR_remove_zone_from_used_list");
}

static size_t
get_zone_alloc_size(MR_MemoryZone *zone)
{
    // XXX: Check this, it seems at odds with the description with the
    // MR_zone_top and MR_zone_bottom fields.

#ifdef  MR_PROTECTPAGE
    return (size_t)
        ((char *) zone->MR_zone_hardmax - (char *) zone->MR_zone_min);
#else
    return (size_t)
        ((char *) zone->MR_zone_top - (char *) zone->MR_zone_min);
#endif
}

// Successive calls to next_offset return offsets modulo the primary
// cache size (carefully avoiding ever giving an offset that clashes
// with fake_reg_array). This is used to give different memory zones
// different starting points across the caches so that it is better utilized.
//
// An alternative implementation would be to increment the offset by
// a fixed amount (eg 2Kb) so that as primary caches get bigger, we allocate
// more offsets across them.

size_t
MR_next_offset(void)
{
    MR_Integer old_counter;
    MR_Integer new_counter;

    old_counter = offset_counter;
    new_counter = (old_counter + 1) % (CACHE_SLICES - 1);
#if defined(MR_THREAD_SAFE)
    // The critical section here is really small, a CAS will work well.
    while (!MR_compare_and_swap_int(&offset_counter, old_counter,
        new_counter))
    {
        MR_ATOMIC_PAUSE;
        old_counter = offset_counter;
        new_counter = (old_counter + 1) % (CACHE_SLICES - 1);
    }
#else
    offset_counter = new_counter;
#endif

    return offset_vector[new_counter];
}

MR_MemoryZone *
MR_create_or_reuse_zone(const char *name, size_t size, size_t offset,
    size_t redzone_size, MR_ZoneHandler *handler)
{
    MR_MemoryZone   *zone;
    MR_bool         is_new_zone;

    zone = MR_get_free_zone(size + redzone_size);
    if (zone != NULL) {
#ifdef MR_DEBUG_STACK_SEGMENTS
        MR_debug_log_message("reusing existing zone");
#endif
        is_new_zone = MR_FALSE;
        zone->MR_zone_desired_size = size;
    } else {
#ifdef  MR_DEBUG_STACK_SEGMENTS
        MR_debug_log_message("allocating new zone");
#endif
        is_new_zone = MR_TRUE;
        zone = MR_create_new_zone(size, redzone_size, offset);
    }

    zone->MR_zone_name = name;
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    zone->MR_zone_handler = handler;
    if (!is_new_zone && (redzone_size != zone->MR_zone_redzone_size)) {
        // The redzone must be reconfigured.

        MR_configure_redzone_size(zone, redzone_size);
        MR_reset_redzone(zone);
    }
#else
    if (!is_new_zone) {
        zone->MR_zone_redzone_size = redzone_size;
    }
#endif

    if (redzone_size > 0 || (handler != MR_null_handler)) {
        // Any zone with a redzone or a non-default handler must be
        // added to the used zones list.

        MR_add_zone_to_used_list(zone);
    }

    return zone;
}

static MR_MemoryZone *
MR_create_new_zone(size_t desired_size, size_t redzone_size, size_t offset)
{
    MR_MemoryZone   *zone;
    MR_Word         *base;
    size_t          total_size;

    // Ignore the offset if it is at least half the desired size of the zone.
    // This should only happen for very small zones.

    if ((offset * 2) > desired_size) {
        offset = 0;
    }

    // The redzone must be page aligned and its size must be a multiple of
    // the page size.

    redzone_size = MR_round_up(redzone_size, MR_page_size);
    // Include an extra page size for the hardzone.

    total_size = desired_size + redzone_size + MR_page_size;
    // The total size must also be rounded to a page boundary, so that it can
    // be allocated from mmap if we are using accurate GC.

    total_size = MR_round_up(total_size, MR_page_size);

#ifdef MR_PROFILE_ZONES
    MR_LOCK(&memory_zones_stats_lock, "MR_create_new_zone");
    MR_num_zones++;
    MR_total_zone_size_net += desired_size;
    MR_total_zone_size_gross += total_size;
    MR_UPDATE_HIGHWATER(MR_max_num_zones, MR_num_zones);
    MR_UPDATE_HIGHWATER(MR_max_total_zone_size_net, MR_total_zone_size_net);
    MR_UPDATE_HIGHWATER(MR_max_total_zone_size_gross, MR_total_zone_size_gross);
    MR_UNLOCK(&memory_zones_stats_lock, "MR_create_new_zone");
#endif

    base = (MR_Word *) MR_alloc_zone_memory(total_size);
    if (base == NULL) {
        MR_fatal_error("Unable to allocate memory for zone");
    }

    zone = MR_GC_NEW_ATTRIB(MR_MemoryZone, MR_ALLOC_SITE_RUNTIME);

    // The name is initialized by our caller.
    zone->MR_zone_name = NULL;
#ifdef MR_LL_PARALLEL_CONJ
    zone->MR_zone_id = MR_atomic_add_and_fetch_uint(&zone_id_counter, 1);
#elif defined(MR_THREAD_SAFE)
    MR_LOCK(&zone_id_counter_lock, "MR_create_new_zone");
    zone->MR_zone_id = ++zone_id_counter;
    MR_UNLOCK(&zone_id_counter_lock, "MR_create_new_zone");
#else
    zone->MR_zone_id = ++zone_id_counter;
#endif
    zone->MR_zone_desired_size = desired_size;
    zone->MR_zone_redzone_size = redzone_size;

#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    // Our caller will set the handler.
    zone->MR_zone_handler = NULL;
#endif // MR_CHECK_OVERFLOW_VIA_MPROTECT

    zone->MR_zone_bottom = base;

    zone->MR_zone_top = (MR_Word *) ((char *) base + total_size);
    zone->MR_zone_min = (MR_Word *) ((char *) base + offset);
#ifdef  MR_LOWLEVEL_DEBUG
    zone->MR_zone_max = zone->MR_zone_min;
#endif  // MR_LOWLEVEL_DEBUG

    MR_setup_redzones(zone);

    return zone;
}

MR_Integer
MR_extend_zone(MR_MemoryZone *zone, size_t new_size)
{
    void            *old_base;
    void            *new_base;
    size_t          offset;
    size_t          copy_size;
    size_t          new_total_size;
    MR_Integer      base_incr;
    int             res;
    char            errbuf[MR_STRERROR_BUF_SIZE];
#ifdef MR_PROFILE_ZONES
    size_t          size_delta;
#endif

    if (zone == NULL) {
        MR_fatal_error("MR_extend_zone called with NULL pointer");
    }

    // XXX: This value is strange for new_total_size, it is a page bigger than
    // it needs to be. However, this allows for a hardzone in some cases.
    // We should fix this in the future.

#ifdef  MR_PROTECTPAGE
    new_total_size = new_size + 2 * MR_unit;
#else
    new_total_size = new_size + MR_unit;
#endif

    old_base = zone->MR_zone_bottom;
    copy_size = zone->MR_zone_end - zone->MR_zone_bottom;
    offset = zone->MR_zone_min - zone->MR_zone_bottom;

#ifdef MR_PROFILE_ZONES
    MR_LOCK(&memory_zones_stats_lock, "MR_extend_zone");
    MR_total_zone_size_net += new_size - zone->MR_zone_desired_size;
    MR_total_zone_size_gross += new_total_size -
        ((MR_Integer) zone->MR_zone_top - (MR_Integer) zone->MR_zone_bottom);
    MR_UNLOCK(&memory_zones_stats_lock, "MR_extend_zone");
#endif

#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    // Unprotect the entire zone area.
    res = MR_protect_pages((char *) zone->MR_zone_bottom,
        ((char *) zone->MR_zone_top) - ((char *) zone->MR_zone_bottom),
        NORMAL_PROT);
    if (res < 0) {
        MR_fatal_error(
            "unable to reset %s#%" MR_INTEGER_LENGTH_MODIFIER
                "d total area\nbase=%p, redzone=%p, errno=%s",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_top,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
#endif  // MR_CHECK_OVERFLOW_VIA_MPROTECT

    new_base = MR_realloc_zone_memory(old_base, copy_size, new_size);
    if (new_base == NULL) {
        MR_fatal_error("unable reallocate memory zone: %s#%"
                MR_INTEGER_LENGTH_MODIFIER "d",
            zone->MR_zone_name, zone->MR_zone_id);
    }

    // XXX The casts to MR_Integer are here because this code was relying
    // on the gcc extension that allows arithmetic on void pointers;
    // this breaks when compiling with Visual C - juliensf.

    base_incr = (MR_Integer) new_base - (MR_Integer) old_base;

    zone->MR_zone_desired_size = new_size;
    zone->MR_zone_bottom = new_base;
    zone->MR_zone_top = (MR_Word *) ((char *) new_base + new_total_size);
    zone->MR_zone_min = (MR_Word *) ((char *) new_base + offset);
#ifdef  MR_LOWLEVEL_DEBUG
    zone->MR_zone_max = zone->MR_zone_min;
#endif  // MR_LOWLEVEL_DEBUG

    MR_setup_redzones(zone);

    return base_incr;
}

void
MR_release_zone(MR_MemoryZone *zone)
{
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    if (zone->MR_zone_redzone_size ||
        (zone->MR_zone_handler != MR_null_handler))
    {
        MR_remove_zone_from_used_list(zone);
    }
#endif
    MR_return_zone_to_free_list(zone);

    MR_maybe_gc_zones();
}

#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
static void
MR_configure_redzone_size(MR_MemoryZone *zone, size_t redsize)
{
    size_t size = zone->MR_zone_desired_size;

    zone->MR_zone_redzone = (MR_Word *)
        MR_round_up((MR_Unsigned) zone->MR_zone_bottom + size - redsize,
            MR_page_size);
    zone->MR_zone_redzone_base = zone->MR_zone_redzone;

    // When using small memory zones, the offset given by MR_next_offset()
    // might have us starting in the middle of the redzone. Don't do that.

    if (zone->MR_zone_min >= zone->MR_zone_redzone) {
        zone->MR_zone_min = zone->MR_zone_bottom;
    }

    MR_assert(zone->MR_zone_redzone < zone->MR_zone_top);
    MR_assert(((MR_Unsigned) zone->MR_zone_redzone + redsize) <
        (MR_Unsigned) zone->MR_zone_top);
}
#endif

static void
MR_setup_redzones(MR_MemoryZone *zone)
{
    size_t      size;
    size_t      redsize;
    int         res;
    char        errbuf[MR_STRERROR_BUF_SIZE];

    size = zone->MR_zone_desired_size;
    redsize = zone->MR_zone_redzone_size;

    assert(size > redsize);

#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("Setting up redzone of size: 0x%x.", redsize);
#endif

    // Setup the redzone.

#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    MR_configure_redzone_size(zone, redsize);

    res = MR_protect_pages((char *) zone->MR_zone_redzone,
        redsize + MR_page_size, REDZONE_PROT);
    if (res < 0) {
        if (zone->MR_zone_name == NULL) {
            zone->MR_zone_name = "unknown";
        }
        MR_fatal_error(
            "unable to set %s#%" MR_INTEGER_LENGTH_MODIFIER
              "d redzone\nbase=%p, redzone=%p, errno=%s",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
#endif // MR_CHECK_OVERFLOW_VIA_MPROTECT

    // Setup the hardzone.

#if defined(MR_PROTECTPAGE)
    // The % MR_page_size is to ensure page alignment.
    zone->MR_zone_hardmax = (MR_Word *)((MR_Unsigned) zone->MR_zone_top -
            MR_page_size - ((MR_Unsigned) zone->MR_zone_top % MR_page_size));
    res = MR_protect_pages((char *) zone->MR_zone_hardmax, MR_page_size,
        REDZONE_PROT);
    if (res < 0) {
        if (zone->MR_zone_name == NULL) {
            zone->MR_zone_name = "unknown";
        }
        MR_fatal_error(
            "unable to set %s#%" MR_INTEGER_LENGTH_MODIFIER
                "d hardmax\nbase=%p, hardmax=%p top=%p, errno=%s",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_hardmax, zone->MR_zone_top,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
#endif  // MR_PROTECTPAGE

#if defined(MR_NATIVE_GC) && defined(MR_HIGHLEVEL_CODE)
    zone->MR_zone_gc_threshold = (char *) zone->MR_zone_end
        - MR_heap_margin_size;
#endif

#if defined(MR_STACK_SEGMENTS) && !defined(MR_HIGHLEVEL_CODE)
    zone->MR_zone_extend_threshold =
        zone->MR_zone_end - MR_stack_margin_size_words;
  #ifdef MR_DEBUG_STACK_SEGMENTS_SET_SIZE
    // Stack segment code is much easier to debug if the segments are small.
    // Since the segments have to be at least a page in size, we can cheat by
    // limiting the size of the part of the segment that we choose to use.
    //
    // Note that since we don't have access to the zone name here, we apply
    // MR_DEBUG_STACK_SEGMENTS_SET_SIZE to all new zones. Ideally, we
    // should apply it only to the zones we are interested in. As it is,
    // setting MR_DEBUG_STACK_SEGMENTS_SET_SIZE to too small a value
    // will also slow down the parts of the program that use zones that
    // you are *not* interested in right now.

    if (zone->MR_zone_extend_threshold >
        (zone->MR_zone_min + MR_DEBUG_STACK_SEGMENTS_SET_SIZE))
    {
        zone->MR_zone_extend_threshold =
            zone->MR_zone_min + MR_DEBUG_STACK_SEGMENTS_SET_SIZE;
    }
  #endif

    assert((MR_Word *) zone->MR_zone_extend_threshold > zone->MR_zone_min);

  #ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    assert((MR_Word *) zone->MR_zone_extend_threshold < zone->MR_zone_redzone);
  #endif
#endif
}

void
MR_reset_redzone(MR_MemoryZone *zone)
{
#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    int     res;
    char    errbuf[MR_STRERROR_BUF_SIZE];

    zone->MR_zone_redzone = zone->MR_zone_redzone_base;

    // Unprotect the non-redzone area.
    res = MR_protect_pages((char *) zone->MR_zone_bottom,
        ((char *) zone->MR_zone_redzone) - ((char *) zone->MR_zone_bottom),
        NORMAL_PROT);
    if (res < 0) {
        MR_fatal_error(
            "unable to reset %s#%" MR_INTEGER_LENGTH_MODIFIER
                "d normal area\nbase=%p, redzone=%p, errno=%s",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
    // Protect the redzone area.
    res = MR_protect_pages((char *) zone->MR_zone_redzone,
        ((char *) zone->MR_zone_top) - ((char *) zone->MR_zone_redzone),
        REDZONE_PROT);
    if (res < 0) {
        MR_fatal_error(
            "unable to reset %s#%" MR_INTEGER_LENGTH_MODIFIER
                "d redzone\nbase=%p, redzone=%p, errno=%s",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
#endif  // MR_CHECK_OVERFLOW_VIA_MPROTECT
}

MR_MemoryZone *
MR_get_used_memory_zones_readonly(void)
{
    return used_memory_zones;
}

MR_bool
MR_in_zone(const MR_Word *ptr, const MR_MemoryZone *zone)
{
    return ((zone->MR_zone_bottom <= ptr) && (ptr < zone->MR_zone_top));
}

////////////////////////////////////////////////////////////////////////////
//
// Caching of memory zones.

// Define this macro to test the performance without caching.
#ifdef MR_DO_NOT_CACHE_FREE_MEMORY_ZONES

static MR_MemoryZone *
MR_get_free_zone(size_t size)
{
    return NULL;
}

static void
MR_return_zone_to_free_list(MR_MemoryZone *zone)
{
    MR_free_zone(zone);
}

static void
MR_maybe_gc_zones(void)
{
    return;
}

#else // ! MR_DO_NOT_CACHE_FREE_MEMORY_ZONES

// Currently we use high and low water marks to manage the cache of free zones,
// collection begins if either the number of zones or total number of pages is
// above their respective high water marks and stops when both are below their
// low water marks.
//
// TODO: Test for optimal values of these settings, however there is probably
// not much to be gained here that can't be more easily gained somewhere else
// first.

// Collection of old zones.
//
// MR_gc_zones()
// Collect zones until MR_should_stop_gc_memory_zones() returns true.
//
// MR_should_gc_memory_zones()
// True if either number and number of pages are above the high water mark.
//
// MR_should_stop_gc_memory_zones()
// True if both the number nad number of pages are below the low water mark.

static void             MR_gc_zones(void);
static MR_bool          MR_should_gc_memory_zones(void);
static MR_bool          MR_should_stop_gc_memory_zones(void);

// TODO: These should be controllable via MERCURY_OPTIONS.

#if defined(MR_THREAD_SAFE) && !defined(MR_HIGHLEVEL_CODE)
    #define THREAD_COUNT    (MR_num_ws_engines + MR_thread_barrier_count)
#else
    #define THREAD_COUNT    (1 + MR_thread_barrier_count)
#endif
// 16 zones per thread
#define MR_FREE_MEMORY_ZONES_NUM_HIGH   (16*THREAD_COUNT)
// 4 zones per thread
#define MR_FREE_MEMORY_ZONES_NUM_LOW    (4*THREAD_COUNT)
// 16MB per thread
#define MR_FREE_MEMORY_ZONES_PAGES_HIGH (((16*1024*1024)/MR_page_size)*THREAD_COUNT)
// 4MB per thread
#define MR_FREE_MEMORY_ZONES_PAGES_LOW  (((4*1024*1024)/MR_page_size)*THREAD_COUNT)

static MR_MemoryZonesFree * MR_THREADSAFE_VOLATILE free_memory_zones = NULL;

// This value is used to maintain a position within the list of free zones.
// If it is null then no position is maintained. Otherwise it points to a
// position within the list, in this case it represents a sub-list of free
// zones. This sub list always contains the least-recently-used zones.
//
// Some actions can invalidate this pointer, in these cases it should be set to
// NULL.
//
// When this value is non-null, it can be used to quickly find the least
// recently used zones. This is used by the garbage collection loop.

static MR_MemoryZonesFree * MR_THREADSAFE_VOLATILE
    lru_free_memory_zones = NULL;

// The number of free zones cached.

static MR_THREADSAFE_VOLATILE MR_Unsigned free_memory_zones_num = 0;

// The number pages used bo the cached free zones.

static MR_THREADSAFE_VOLATILE MR_Unsigned free_memory_zones_pages = 0;

// The next token to be given to a memory zone as it is added to the free list.
// The tokens are used to detect the least recently used zones when freeing
// them. Zones with larger tokens have been used more recently than zones with
// smaller tokens.

static MR_THREADSAFE_VOLATILE MR_Unsigned lru_memory_zone_token = 0;

static MR_MemoryZone *
MR_get_free_zone(size_t size)
{
    MR_MemoryZone       *zone;
    MR_MemoryZonesFree  *zones_list;
    MR_MemoryZonesFree  *zones_list_prev;

    // Before using the lock below see if there is at least one zone on the
    // list.

    if (!free_memory_zones) {
        return NULL;
    }

    // Unlink the first zone on the free-list, link it onto the used-list
    // and return it.

    MR_LOCK(&memory_zones_lock, "MR_get_free_zone");

    zones_list = free_memory_zones;
    zones_list_prev = NULL;
    while (zones_list != NULL) {
        if (zones_list->MR_zonesfree_size <= size) {
            // A zone on this list will fit our needs.
            break;
        }
        zones_list_prev = zones_list;
        zones_list = zones_list->MR_zonesfree_major_next;
    }

    if (zones_list != NULL) {
        zone = zones_list->MR_zonesfree_minor_head;
        if (zone->MR_zone_next != NULL) {
            zones_list->MR_zonesfree_minor_head = zone->MR_zone_next;
        } else {
            // This inner list is now empty, we should remove it from the
            // outer list.

            if (zones_list_prev != NULL) {
                zones_list_prev->MR_zonesfree_major_next = zones_list->MR_zonesfree_major_next;
            } else {
                free_memory_zones = zones_list->MR_zonesfree_major_next;
            }
            if (zones_list->MR_zonesfree_major_next != NULL) {
                zones_list->MR_zonesfree_major_next->MR_zonesfree_major_prev = zones_list_prev;
            }
            if (lru_free_memory_zones == zones_list) {
                // This zone list had the least recently used zone on it,
                // invalidate the lru_free_memory_zones pointer. The garbage
                // collection loop will re-initialise this.

                lru_free_memory_zones = NULL;
            }
        }
    } else {
        zone = NULL;
    }

    if (zone != NULL) {
        free_memory_zones_num--;
        free_memory_zones_pages -= get_zone_alloc_size(zone) / MR_page_size;
    }

    MR_UNLOCK(&memory_zones_lock, "MR_get_free_zone");

    return zone;
}

static void
MR_return_zone_to_free_list(MR_MemoryZone *zone)
{
    // The current list in iterations over the list of free lists.
    MR_MemoryZonesFree      *cur_list;
    size_t                  size;

    size = get_zone_alloc_size(zone);

#ifdef MR_CONSERVATIVE_GC
    // Make sure the GC doesn't find any pointers in this zone.
    MR_clear_zone_for_GC(zone, zone->MR_zone_min);
#endif

    MR_LOCK(&memory_zones_lock, "MR_return_zone_to_free_list");

    free_memory_zones_num++;
    free_memory_zones_pages += size / MR_page_size;

    zone->MR_zone_lru_token = lru_memory_zone_token++;

    cur_list = free_memory_zones;
    while (cur_list) {
        if (cur_list->MR_zonesfree_size == size)
        {
            // We found the correct zone list.
            break;
        }
        // Test to see if we can exit the loop early.
        else if (cur_list->MR_zonesfree_size > size)
        {
            // Set this to null to represent our failure to find a zone list
            // of the right size.

            cur_list = NULL;
            break;
        }
        cur_list = cur_list->MR_zonesfree_major_next;
    }

    if (cur_list == NULL) {
        MR_MemoryZonesFree *new_list;
        MR_MemoryZonesFree *prev_list;

        new_list = MR_GC_NEW_ATTRIB(MR_MemoryZonesFree, MR_ALLOC_SITE_RUNTIME);
        new_list->MR_zonesfree_size = size;
        new_list->MR_zonesfree_minor_head = NULL;
        new_list->MR_zonesfree_minor_tail = NULL;
        cur_list = free_memory_zones;
        prev_list = NULL;
        while (cur_list) {
            if (cur_list->MR_zonesfree_size > size) {
                // We've just passed the position where this list item belongs.
                break;
            }
            prev_list = cur_list;
            cur_list = cur_list->MR_zonesfree_major_next;
        }

        // Insert it between prev_list and cur_list.
        new_list->MR_zonesfree_major_next = cur_list;
        new_list->MR_zonesfree_major_prev = prev_list;
        if (prev_list) {
            prev_list->MR_zonesfree_major_next = new_list;
        } else {
            free_memory_zones = new_list;
        }
        if (cur_list) {
            cur_list->MR_zonesfree_major_prev = new_list;
        }

        // Reset cur_list so that it is pointing at the correct outer list
        // item regardless of whether this branch was executed or not.

        cur_list = new_list;
    }

    zone->MR_zone_next = cur_list->MR_zonesfree_minor_head;
    cur_list->MR_zonesfree_minor_head = zone;
    if (!cur_list->MR_zonesfree_minor_tail) {
        // This is the first zone on this list, so set up the tail pointer.
        cur_list->MR_zonesfree_minor_tail = zone;
    }

    MR_UNLOCK(&memory_zones_lock, "MR_return_zone_to_free_list");
}

static MR_bool
MR_should_gc_memory_zones(void)
{
    return (free_memory_zones_num > MR_FREE_MEMORY_ZONES_NUM_HIGH) ||
        (free_memory_zones_pages > MR_FREE_MEMORY_ZONES_PAGES_HIGH);
}

static MR_bool
MR_should_stop_gc_memory_zones(void)
{
    return (free_memory_zones_num < MR_FREE_MEMORY_ZONES_NUM_LOW) &&
        (free_memory_zones_pages < MR_FREE_MEMORY_ZONES_PAGES_LOW);
}

static void
MR_maybe_gc_zones(void)
{
    if (MR_should_gc_memory_zones()) {
        MR_gc_zones();
    }
}

static void
MR_gc_zones(void)
{
    MR_LOCK(&memory_zones_lock, "MR_gc_zones");
    do {

        MR_MemoryZonesFree  *cur_list;
        MR_Unsigned         oldest_lru_token;
        MR_Unsigned         cur_lru_token;
        MR_MemoryZone       *zone;
        MR_MemoryZone       *prev_zone;

        if (NULL == lru_free_memory_zones) {
            // There is no cached LRU information, find the free list with the
            // oldest zone on it.

            cur_list = free_memory_zones;

            // GCC 3.3 thinks that oldest_lru_token will used uninitialised.
            // But it won't, lru_free_memory_zones will always be NULL and
            // therefore the if branch will be followed to set this variable
            // before it is read in the condition of the else-if branch.
            //
            // To avoid this problem, we initialise oldest_lru_token.

            oldest_lru_token = 0;
            while (cur_list != NULL) {
                cur_lru_token =
                    cur_list->MR_zonesfree_minor_tail->MR_zone_lru_token;
                if (lru_free_memory_zones == NULL) {
                    oldest_lru_token = cur_lru_token;
                    lru_free_memory_zones = cur_list;
                } else if (cur_lru_token < oldest_lru_token) {
                    // The current zone has an older token.
                    oldest_lru_token = cur_lru_token;
                    lru_free_memory_zones = cur_list;
                }

                cur_list = cur_list->MR_zonesfree_major_next;
            }
        }

        if (NULL == lru_free_memory_zones) {
            // There is no memory to collect, perhaps there was a race
            // before we locked mercury_zones_lock.
            MR_UNLOCK(&memory_zones_lock, "MR_gc_zones");
            return;
        }

        zone = lru_free_memory_zones->MR_zonesfree_minor_head;
        prev_zone = NULL;
        MR_assert(NULL != zone);
        while (NULL != zone)
        {
            if (zone == lru_free_memory_zones->MR_zonesfree_minor_tail) {
                break;
            }

            prev_zone = zone;
            zone = zone->MR_zone_next;
        }
        MR_assert(NULL != zone);

        // Unlink zone from the free list.

        if (prev_zone == NULL) {
            // The list that contained zone is now free, unlink it
            // from its list.

            if (NULL != lru_free_memory_zones->MR_zonesfree_major_prev) {
                lru_free_memory_zones->MR_zonesfree_major_prev->
                        MR_zonesfree_major_next =
                    lru_free_memory_zones->MR_zonesfree_major_next;
            } else {
                free_memory_zones =
                    lru_free_memory_zones->MR_zonesfree_major_next;
            }
            if (NULL != lru_free_memory_zones->MR_zonesfree_major_next) {
                lru_free_memory_zones->MR_zonesfree_major_next->
                        MR_zonesfree_major_prev =
                    lru_free_memory_zones->MR_zonesfree_major_prev;
            }
        } else {
            // Simply unlink zone.

            prev_zone->MR_zone_next = NULL;
            lru_free_memory_zones->MR_zonesfree_minor_tail = prev_zone;
        }

        free_memory_zones_num--;
        free_memory_zones_pages -= get_zone_alloc_size(zone) / MR_page_size;
        MR_free_zone(zone);

        // Clear the LRU information.

        lru_free_memory_zones = NULL;

    } while (!MR_should_stop_gc_memory_zones());
    MR_UNLOCK(&memory_zones_lock, "MR_gc_zones");
}

#endif // ! MR_DO_NOT_CACHE_FREE_MEMORY_ZONES

////////////////////////////////////////////////////////////////////////////
//
// Debugging code.

void
MR_debug_memory(FILE *fp)
{
    MR_MemoryZone   *zone;

    fprintf(fp, "\n");
    fprintf(fp, "pcache_size  = %lu (0x%lx)\n",
        (unsigned long) MR_pcache_size, (unsigned long) MR_pcache_size);
    fprintf(fp, "page_size    = %lu (0x%lx)\n",
        (unsigned long) MR_page_size, (unsigned long) MR_page_size);
    fprintf(fp, "unit         = %lu (0x%lx)\n",
        (unsigned long) MR_unit, (unsigned long) MR_unit);

    fprintf(fp, "\n");
    fprintf(fp, "fake_reg       = %p (offset %" MR_INTEGER_LENGTH_MODIFIER "d)\n",
        (void *) MR_fake_reg, (MR_Integer) MR_fake_reg & (MR_unit-1));
    fprintf(fp, "\n");

    MR_LOCK(&memory_zones_lock, "MR_debug_memory");
    for (zone = used_memory_zones; zone; zone = zone->MR_zone_next) {
        MR_debug_memory_zone(fp, zone);
    }
    MR_UNLOCK(&memory_zones_lock, "MR_debug_memory");
}

void
MR_debug_memory_zone(FILE *fp, MR_MemoryZone *zone)
{
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-dessize   = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) zone->MR_zone_desired_size);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-base  = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_bottom);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-min       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_min);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-top       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_top);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-end       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_end);
#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-redsize   = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) zone->MR_zone_redzone_size);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-redzone   = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_redzone);
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-redzone_base  = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_redzone_base);
#endif  // MR_CHECK_OVERFLOW_VIA_MPROTECT
#ifdef  MR_PROTECTPAGE
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-hardmax       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_hardmax);
#endif
    fprintf(fp, "%-16s#%" MR_INTEGER_LENGTH_MODIFIER "d-size      = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) get_zone_alloc_size(zone));
    fprintf(fp, "\n");
}

#ifdef MR_PROFILE_ZONES
void MR_print_zone_stats(void) {
    MR_LOCK(&memory_zones_stats_lock, "MR_print_zone_stats");
    printf("Number of zones allocated: %d\n", MR_num_zones);
    printf("Maximum number of zones allocated: %d\n", MR_max_num_zones);
    printf("Allocated space within zones (KB) Net: %d Gross: %d\n",
        MR_total_zone_size_net / 1024, MR_total_zone_size_gross / 1024);
    printf("Maximum allocated space within zones (KB) Net: %d Gross: %d\n",
        MR_max_total_zone_size_net / 1024,
        MR_max_total_zone_size_gross / 1024);
    MR_UNLOCK(&memory_zones_stats_lock, "MR_print_zone_stats");
}
#endif
