/*
** vim:sw=4 ts=4 expandtab
*/
/*
** Copyright (C) 1998-2000, 2002-2006 The University of Melbourne.
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

#include <stdlib.h>   /* for memalign and posix_memalign */

#ifdef MR_HAVE_MALLOC_H
    #include <malloc.h>     /* for memalign */
#endif

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif

#ifdef MR_HAVE_SYS_SIGNAL_H
  /* on FREEBSD we need to include <sys/signal.h> before <ucontext.h> */
  #include <sys/signal.h>
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
#endif

#ifdef MR_WIN32_VIRTUAL_ALLOC
  #include <windows.h>
#endif

static  void    MR_setup_redzones(MR_MemoryZone *zone);

static  void    *MR_alloc_zone_memory(size_t size);
static  void    *MR_realloc_zone_memory(void *old_base, size_t copy_size,
                    size_t new_size);

/*---------------------------------------------------------------------------*/

/*
** MR_PROTECTPAGE is now defined if we have some sort of mprotect like
** functionality, all checks for MR_HAVE_MPROTECT should now use
** MR_PROTECTPAGE.
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
    int     rc;
    DWORD   flags;

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
            "Error in VirtualAlloc(addr=0x%08lx, size=0x%08lx): 0x%08lx\n",
            (unsigned long) addr, (unsigned long) size,
            (unsigned long) GetLastError());
    }

    return rc;
}

#endif  /* MR_WIN32_VIRTUAL_ALLOC */

/*---------------------------------------------------------------------------*/

#if defined(MR_WIN32_VIRTUAL_ALLOC)

/*
** Under Win32, we use VirtualAlloc instead of the standard malloc,
** since we will have to call VirtualProtect later on the pages
** allocated here.
*/

static void *
MR_alloc_zone_memory(size_t size)
{
    void    *ptr;

    ptr = VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_READWRITE);
    if (ptr == NULL) {
        fprintf(stderr, "Error in VirtualAlloc(size=0x%08lx): 0x%08lx\n",
            (unsigned long) size, (unsigned long) GetLastError());
    }

  #ifdef MR_CONSERVATIVE_GC
    if (ptr != NULL) {
        GC_add_roots((char *) ptr, (char *) ptr + size);
    }
  #endif
    return ptr;
}

static void *
MR_realloc_zone_memory(void *old_base, size_t copy_size, size_t new_size)
{
    void    *ptr;

    ptr = MR_alloc_zone_memory(new_size);
    (void) MR_memcpy(ptr, old_base, copy_size);
    /*
    ** XXX We should of course release old_base's memory, but I don't know
    ** enough about Windows even to implement that. -zs
    */
    return ptr;
}

#elif defined(MR_CONSERVATIVE_GC)

static void *
MR_alloc_zone_memory(size_t size)
{
    return GC_MALLOC_UNCOLLECTABLE(size);
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
    /*
    ** We should call free(old_base) here. However, there is no guarantee that
    ** the system supports the freeing of memory allocated with memalign via
    ** calls to free(). We therefore don't free old_base. This is why we prefer
    ** posix_memalign if it is available.
    */
    return ptr;
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
    /* the copying is done by realloc */
    return realloc(old_base, new_size);
}

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

  #define NORMAL_PROT (PROT_READ|PROT_WRITE)

  #ifdef MR_CONSERVATIVE_GC
    /*
    ** The conservative garbage collectors scans through
    ** all these areas, so we need to allow reads.
    ** XXX This probably causes efficiency problems:
    ** too much memory for the GC to scan, and it probably
    ** all gets paged in.
    */
    #define REDZONE_PROT PROT_READ
  #else
    #define REDZONE_PROT PROT_NONE
  #endif

  /* The BSDI BSD/386 1.1 headers don't define PROT_NONE */
  #ifndef PROT_NONE
    #define PROT_NONE 0
  #endif

#endif /* MR_PROTECTPAGE */

/*---------------------------------------------------------------------------*/

#define MAX_ZONES   16

static MR_MemoryZone    *used_memory_zones = NULL;
static MR_MemoryZone    *free_memory_zones = NULL;
#ifdef  MR_THREAD_SAFE
  static MercuryLock    free_memory_zones_lock;
#endif

static void             MR_init_offsets(void);
static MR_MemoryZone    *MR_get_zone(void);

    /*
    ** We manage the handing out of offsets through the cache by
    ** computing the offsets once and storing them in an array
    ** (in shared memory if necessary). We then maintain a global
    ** counter used to index the array which we increment (modulo
    ** the size of the array) after handing out each offset.
    */

#define CACHE_SLICES    8

static  size_t          *offset_vector;
static  int             offset_counter;
extern  size_t          next_offset(void);

void
MR_init_zones()
{
#ifdef  MR_THREAD_SAFE
    pthread_mutex_init(&free_memory_zones_lock, MR_MUTEX_ATTR);
#endif

    MR_init_offsets();
}

static void
MR_init_offsets()
{
    int     i;
    size_t  fake_reg_offset;

    offset_counter = 0;

    offset_vector = MR_GC_NEW_ARRAY(size_t, CACHE_SLICES - 1);

    fake_reg_offset = (MR_Unsigned) MR_fake_reg % MR_pcache_size;

    for (i = 0; i < CACHE_SLICES - 1; i++) {
        offset_vector[i] =
            (fake_reg_offset + MR_pcache_size * i / CACHE_SLICES)
            % MR_pcache_size;
    }
}

static MR_MemoryZone *
MR_get_zone(void)
{
    MR_MemoryZone   *zone;

    /*
    ** Unlink the first zone on the free-list, link it onto the used-list
    ** and return it.
    */
    MR_LOCK(&free_memory_zones_lock, "get_zone");
    if (free_memory_zones == NULL) {
        zone = MR_GC_NEW(MR_MemoryZone);
    } else {
        zone = free_memory_zones;
        free_memory_zones = free_memory_zones->MR_zone_next;
    }

    zone->MR_zone_next = used_memory_zones;
    used_memory_zones = zone;
    MR_UNLOCK(&free_memory_zones_lock, "get_zone");

    return zone;
}

void
MR_unget_zone(MR_MemoryZone *zone)
{
    MR_MemoryZone   *prev;
    MR_MemoryZone   *tmp;

    /*
    ** Find the zone on the used list, and unlink it from the list,
    ** then link it onto the start of the free-list.
    */

    MR_LOCK(&free_memory_zones_lock, "unget_zone");
    for(prev = NULL, tmp = used_memory_zones; tmp != NULL && tmp != zone;
        prev = tmp, tmp = tmp->MR_zone_next)
    {
        /* VOID */
    }

    if (tmp == NULL) {
        MR_fatal_error("memory zone not found!");
    }

    if (prev == NULL) {
        used_memory_zones = used_memory_zones->MR_zone_next;
    } else {
        prev->MR_zone_next = tmp->MR_zone_next;
    }

    zone->MR_zone_next = free_memory_zones;
    free_memory_zones = zone;
    MR_UNLOCK(&free_memory_zones_lock, "unget_zone");
}

/*
** Successive calls to next_offset return offsets modulo the primary
** cache size (carefully avoiding ever giving an offset that clashes
** with fake_reg_array). This is used to give different memory zones
** different starting points across the caches so that it is better utilized.
**
** An alternative implementation would be to increment the offset by
** a fixed amount (eg 2Kb) so that as primary caches get bigger, we allocate
** more offsets across them.
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
MR_create_zone(const char *name, int id, size_t size, size_t offset,
    size_t redsize, MR_ZoneHandler handler)
{
    MR_Word     *base;
    size_t      total_size;

    /*
    ** total allocation is:
    **  unit        (roundup to page boundary)
    **  size        (including redzone)
    **  unit        (an extra page for protection if mprotect is being used)
    */
#ifdef  MR_PROTECTPAGE
    total_size = size + 2 * MR_unit;
#else
    total_size = size + MR_unit;
#endif

    base = (MR_Word *) MR_alloc_zone_memory(total_size);
    if (base == NULL) {
        char buf[2560];
        sprintf(buf, "unable allocate memory zone: %s#%d", name, id);
        MR_fatal_error(buf);
    }

    return MR_construct_zone(name, id, base, size, offset, redsize, handler);
}

MR_MemoryZone *
MR_construct_zone(const char *name, int id, MR_Word *base,
    size_t size, size_t offset, size_t redsize, MR_ZoneHandler handler)
{
    MR_MemoryZone   *zone;
    size_t          total_size;
    int             res;

    if (base == NULL) {
        MR_fatal_error("MR_construct_zone called with NULL pointer");
    }

    zone = MR_get_zone();

    zone->MR_zone_name = name;
    zone->MR_zone_id = id;
    zone->MR_zone_desired_size = size;
    zone->MR_zone_redzone_size = redsize;

#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    zone->MR_zone_handler = handler;
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

    zone->MR_zone_bottom = base;

#ifdef  MR_PROTECTPAGE
    total_size = size + MR_unit;
#else
    total_size = size;
#endif  /* MR_PROTECTPAGE */

    zone->MR_zone_top = (MR_Word *) ((char *) base + total_size);
    zone->MR_zone_min = (MR_Word *) ((char *) base + offset);
#ifdef  MR_LOWLEVEL_DEBUG
    zone->MR_zone_max = zone->MR_zone_min;
#endif  /* MR_LOWLEVEL_DEBUG */

    MR_setup_redzones(zone);

    return zone;
}

MR_Integer
MR_extend_zone(MR_MemoryZone *zone, size_t new_size)
{
    void            *old_base;
    void            *new_base;
    size_t          offset;
    size_t          redsize;
    size_t          copy_size;
    size_t          new_total_size;
    MR_Integer      base_incr;
    int             res;

    if (zone == NULL) {
        MR_fatal_error("MR_extend_zone called with NULL pointer");
    }

#ifdef  MR_PROTECTPAGE
    new_total_size = new_size + 2 * MR_unit;
#else
    new_total_size = new_size + MR_unit;
#endif

    old_base = zone->MR_zone_bottom;
    copy_size = zone->MR_zone_end - zone->MR_zone_bottom;
    offset = zone->MR_zone_min - zone->MR_zone_bottom;

#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    /* unprotect the entire zone area */
    res = MR_protect_pages((char *) zone->MR_zone_bottom,
        ((char *) zone->MR_zone_top) - ((char *) zone->MR_zone_bottom),
        NORMAL_PROT);
    if (res < 0) {
        char buf[2560];
        sprintf(buf, "unable to reset %s#%d total area\nbase=%p, redzone=%p",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_top);
        MR_fatal_error(buf);
    }
#endif  /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

    new_base = MR_realloc_zone_memory(old_base, copy_size, new_size);
    if (new_base == NULL) {
        char buf[2560];
        sprintf(buf, "unable reallocate memory zone: %s#%d",
            zone->MR_zone_name, zone->MR_zone_id);
        MR_fatal_error(buf);
    }

    /*
    ** XXX the casts to MR_Integer are here because this code was
    ** relying on the gcc extension that allows arithmetic on void
    ** pointers - this breaks when compiling with Visual C - juliensf.
    */
    base_incr = (MR_Integer)new_base - (MR_Integer)old_base;

    zone->MR_zone_desired_size = new_size;
    zone->MR_zone_bottom = new_base;
    zone->MR_zone_top = (MR_Word *) ((char *) new_base + new_total_size);
    zone->MR_zone_min = (MR_Word *) ((char *) new_base + offset);
#ifdef  MR_LOWLEVEL_DEBUG
    zone->MR_zone_max = zone->MR_zone_min;
#endif  /* MR_LOWLEVEL_DEBUG */

    MR_setup_redzones(zone);

    return base_incr;
}

static void
MR_setup_redzones(MR_MemoryZone *zone)
{
    size_t      size;
    size_t      redsize;
    int         res;

    size = zone->MR_zone_desired_size;
    redsize = zone->MR_zone_redzone_size;

    /*
    ** setup the redzone
    */
#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    zone->MR_zone_redzone = (MR_Word *)
        MR_round_up((MR_Unsigned) zone->MR_zone_bottom + size - redsize,
            MR_unit);
    zone->MR_zone_redzone_base = zone->MR_zone_redzone;

    res = MR_protect_pages((char *) zone->MR_zone_redzone, redsize + MR_unit,
        REDZONE_PROT);
    if (res < 0) {
        char buf[2560];
        sprintf(buf, "unable to set %s#%d redzone\nbase=%p, redzone=%p",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone);
        MR_fatal_error(buf);
    }
#endif /* MR_CHECK_OVERFLOW_VIA_MPROTECT */

    /*
    ** setup the hardzone
    */
#if defined(MR_PROTECTPAGE)
    zone->MR_zone_hardmax = (MR_Word *)
        MR_round_up((MR_Unsigned) zone->MR_zone_top - MR_unit, MR_unit);
    res = MR_protect_pages((char *) zone->MR_zone_hardmax, MR_unit,
        REDZONE_PROT);
    if (res < 0) {
        char buf[2560];
        sprintf(buf, "unable to set %s#%d hardmax\nbase=%p, hardmax=%p top=%p",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_hardmax, zone->MR_zone_top);
        MR_fatal_error(buf);
    }
#endif  /* MR_PROTECTPAGE */

#if defined(MR_NATIVE_GC) && defined(MR_HIGHLEVEL_CODE)
    zone->MR_zone_gc_threshold = (char *) zone->MR_zone_end
        - MR_heap_margin_size;
#endif

#if defined(MR_STACK_SEGMENTS) && !defined(MR_HIGHLEVEL_CODE)
    zone->MR_zone_extend_threshold = (char *) zone->MR_zone_end
        - MR_stack_margin_size;
#endif
}

void
MR_reset_redzone(MR_MemoryZone *zone)
{
#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    int res;

    zone->MR_zone_redzone = zone->MR_zone_redzone_base;

    /* unprotect the non-redzone area */
    res = MR_protect_pages((char *) zone->MR_zone_bottom,
        ((char *) zone->MR_zone_redzone) - ((char *) zone->MR_zone_bottom),
        NORMAL_PROT);
    if (res < 0) {
        char buf[2560];
        sprintf(buf, "unable to reset %s#%d normal area\nbase=%p, redzone=%p",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone);
        MR_fatal_error(buf);
    }
    /* protect the redzone area */
    res = MR_protect_pages((char *) zone->MR_zone_redzone,
        ((char *) zone->MR_zone_top) - ((char *) zone->MR_zone_redzone),
        REDZONE_PROT);
    if (res < 0) {
        char buf[2560];
        sprintf(buf, "unable to reset %s#%d redzone\nbase=%p, redzone=%p",
            zone->MR_zone_name, zone->MR_zone_id,
            zone->MR_zone_bottom, zone->MR_zone_redzone);
        MR_fatal_error(buf);
    }
#endif  /* MR_CHECK_OVERFLOW_VIA_MPROTECT */
}

MR_MemoryZone *
MR_get_used_memory_zones(void)
{
    return used_memory_zones;
}

MR_bool
MR_in_zone(const MR_Word *ptr, const MR_MemoryZone *zone)
{
    return (zone->MR_zone_bottom <= ptr && ptr < zone->MR_zone_top);
}

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
    fprintf(fp, "fake_reg       = %p (offset %ld)\n",
        (void *) MR_fake_reg, (long) MR_fake_reg & (MR_unit-1));
    fprintf(fp, "\n");

    for (zone = used_memory_zones; zone; zone = zone->MR_zone_next) {
        MR_debug_memory_zone(fp, zone);
    }
}

void
MR_debug_memory_zone(FILE *fp, MR_MemoryZone *zone)
{
    fprintf(fp, "%-16s#%d-dessize   = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) zone->MR_zone_desired_size);
    fprintf(fp, "%-16s#%d-base  = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_bottom);
    fprintf(fp, "%-16s#%d-min       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_min);
    fprintf(fp, "%-16s#%d-top       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_top);
    fprintf(fp, "%-16s#%d-end       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_end);
#ifdef  MR_CHECK_OVERFLOW_VIA_MPROTECT
    fprintf(fp, "%-16s#%d-redsize   = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) zone->MR_zone_redzone_size);
    fprintf(fp, "%-16s#%d-redzone   = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_redzone);
    fprintf(fp, "%-16s#%d-redzone_base  = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_redzone_base);
#endif  /* MR_CHECK_OVERFLOW_VIA_MPROTECT */
#ifdef  MR_PROTECTPAGE
    fprintf(fp, "%-16s#%d-hardmax       = %p\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (void *) zone->MR_zone_hardmax);
    fprintf(fp, "%-16s#%d-size      = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) ((char *) zone->MR_zone_hardmax
             - (char *) zone->MR_zone_min));
#else
    fprintf(fp, "%-16s#%d-size      = %lu\n",
        zone->MR_zone_name, zone->MR_zone_id,
        (unsigned long) ((char *) zone->MR_zone_top
             - (char *) zone->MR_zone_min));
#endif  /* MR_PROTECTPAGE */
    fprintf(fp, "\n");
}
