// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1994-2000,2002, 2004, 2006, 2008 The University of Melbourne.
// Copyright (C) 2014 The Mercury Team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_memory.h:
// general memory-allocation related stuff for the Mercury runtime.
//
// This defines the different memory areas used by the Mercury runtime,
// including the det & nondet stacks, the heap (and solutions heap),
// and the fake_reg array for holding Mercury virtual registers.
// It also provides interfaces for constructing new memory zones,
// and for allocating (possibly shared) memory.

#ifndef MERCURY_MEMORY_H
#define MERCURY_MEMORY_H

#include "mercury_memory_zones.h"

#include <stddef.h>     // for size_t

#include "mercury_types.h"  // for MR_Word
#include "mercury_std.h"    // for MR_bool
#include "mercury_conf.h"   // for MR_CONSERVATIVE_GC, etc.

#if defined(MR_CONSERVATIVE_GC)
  #if defined(MR_BOEHM_GC)
    #define GC_I_HIDE_POINTERS
    #include "gc.h"     // for GC_FREE
  #endif
#endif

// MR_round_up(amount, align) returns `amount' rounded up to the nearest
// alignment boundary. `align' must be a power of 2.

#define MR_round_up(amount, align)  ((((amount) - 1) | ((align) - 1)) + 1)

// MR_kilobytes_to_bytes_and_round_up(var) takes an original value in var
// which represents an memory size measured in kilobytes, and converts it to
// a memory size measured in bytes, and then rounds it up to be the next
// multiple of MR_unit.

#define MR_kilobytes_to_bytes_and_round_up(var)                         \
    do {                                                                \
        var = MR_round_up(var * 1024, MR_unit);                         \
    } while (0)

// For these functions, see the comments in mercury_memory.c and
// mercury_engine.c

extern  void    MR_init_memory(void);
extern  void    MR_init_heap(void);

#ifdef MR_CONSERVATIVE_GC
  extern void   MR_init_conservative_GC(void);
#endif

////////////////////////////////////////////////////////////////////////////

// MR_malloc() and MR_realloc() are like the standard C malloc() and realloc()
// functions, except that the return values are checked.
//
// Structures allocated with MR_malloc() and MR_realloc() must NOT contain
// pointers into GC'ed memory, because those pointers will never be traced
// by the conservative GC. Use MR_GC_malloc() or MR_GC_malloc_uncollectable()
// for that.
//
// MR_NEW(type):
//  Allocates space for an object of the specified type.
//
// MR_NEW_ARRAY(type, num):
//  Allocates space for an array of objects of the specified type.
//
// MR_RESIZE_ARRAY(ptr, type, num):
//  Resizes the array, as with realloc().
//
// MR_malloc(bytes):
//  Allocates the given number of bytes.
//
// MR_realloc(old, bytes):
//  Allocates the given number of bytes, copies over the old contents of
//  the previously allocated block pointed to by old, and then frees that
//  old block.
//
// MR_ensure_big_enough_buffer(buffer_ptr, buffer_size_ptr, needed_size):
//  Given a character buffer pointed to by buffer_ptr whose is given by
//  *buffer_size_ptr, ensure that the buffer is big enough to hold
//  needed_size characters. If it needs to make the block bigger,
//  this function will update both *buffer_ptr and *buffer_size_ptr.
//
// MR_free(ptr):
//  Deallocates the memory.

extern  void    *MR_malloc(size_t n);
extern  void    *MR_realloc(void *old, size_t n);
extern  void    MR_ensure_big_enough_buffer(char **buffer_ptr,
                    int *buffer_size_ptr, int needed_size);

#define MR_free(ptr) free(ptr)
#define MR_free_func free

#define MR_NEW(type)                                                    \
    ((type *) MR_malloc(sizeof(type)))

#define MR_NEW_ARRAY(type, num)                                         \
    ((type *) MR_malloc((num) * sizeof(type)))

#define MR_RESIZE_ARRAY(ptr, type, num)                                 \
    ((type *) MR_realloc((ptr), (num) * sizeof(type)))

// These routines all allocate memory that will be traced by the
// conservative garbage collector, if conservative GC is enabled.
// (For the native GC, you need to call MR_add_root() to register roots.)
// These routines all check for a null return value themselves,
// so the caller need not check.
//
// MR_GC_NEW(type):
//  Allocates space for an object of the specified type.
//  If conservative GC is enabled, the object will be garbage collected
//  when it is no longer referenced from GC-traced memory.
//  Memory allocated with malloc() (or MR_malloc() or MR_NEW())
//  is not GC-traced. Nor is thread-local storage.
//
// MR_GC_NEW_UNCOLLECTABLE(type):
//  Allocates space for an object of the specified type.
//  The object will not be garbage collected even if it is not referenced,
//  or only referenced from thread-local storage or storage allocated
//  with malloc(). It should be explicitly deallocated with MR_GC_free().
//
// MR_GC_NEW_ARRAY(type, num):
//  Allocates space for an array of objects of the specified type.
//
// MR_GC_RESIZE_ARRAY(ptr, type, num):
//  Resizes the array, as with realloc().
//
// MR_GC_malloc(bytes):
//  Allocates the given number of bytes.
//  If conservative GC is enabled, the memory will be garbage collected
//  when it is no longer referenced from GC-traced memory (see above).
//
// MR_GC_malloc_uncollectable(bytes):
//  Allocates the given number of bytes.
//  The memory will not be garbage collected, and so
//  it should be explicitly deallocated using MR_GC_free().
//
// MR_GC_malloc_atomic(bytes):
//  Allocates the given number of bytes.
//  Pointers to GC objects may not be stored in this object. This allows
//  the GC to optimize it's marking phase.
//
// MR_GC_realloc(ptr, bytes):
//  Reallocates the memory block pointed to by ptr.
//
// MR_GC_free(ptr):
//  Deallocates the memory.
//
// MR_GC_register_finalizer(ptr, finalize_func, data):
//  When ptr is garbage collected invoke (*finalize_func)(ptr, data).
//  ptr must have be a pointer to space allocated by the garbage collector.
//  data is a pointer to some user-defined data.
//  XXX currently this only works with the Boehm collector, i.e. in .gc
//          grades, it is a no-op in non .gc grades.
//
//      XXX this interface is subject to change.
//
// Note: consider using the _attrib variants below.
//
// MR_new_weak_ptr(ptr, object):
//  Create a weak pointer to object and store it in the memory pointed to by
//  ptr (a double pointer). object must have been allocated using one of
//  the MR_GC methods. Weak pointers only work with the Boehm collector
//  (.gc grades). In other grades this is an ordinary pointer.
//
// MR_weak_ptr_read(weak_ptr):
//  Dereference a weak pointer. Returns NULL of the pointed to object has
//  been deallocated. If weak_ptr is NULL then NULL is returned, so the
//  programmer doesn't need to do an extra NULL check in case their pointer
//  is deliberately NULL;

extern  void    *MR_GC_malloc(size_t num_bytes);
extern  void    *MR_GC_malloc_uncollectable(size_t num_bytes);
extern  void    *MR_GC_malloc_atomic(size_t num_bytes);
extern  void    *MR_GC_realloc(void *ptr, size_t num_bytes);

typedef void    (*MR_GC_finalizer)(void *ptr, void *data);

#define MR_GC_NEW(type)                                                 \
    ((type *) MR_GC_malloc(sizeof(type)))

#define MR_GC_NEW_UNCOLLECTABLE(type)                                   \
    ((type *) MR_GC_malloc_uncollectable(sizeof(type)))

#define MR_GC_NEW_ARRAY(type, num)                                      \
    ((type *) MR_GC_malloc((num) * sizeof(type)))

#define MR_GC_RESIZE_ARRAY(ptr, type, num)                              \
    ((type *) MR_GC_realloc((ptr), (num) * sizeof(type)))

#ifdef MR_CONSERVATIVE_GC
  #define MR_GC_free(ptr) GC_FREE(ptr)
#else
  #define MR_GC_free(ptr) free(ptr)
#endif

#if defined(MR_CONSERVATIVE_GC) && defined(MR_BOEHM_GC)
  #define MR_GC_register_finalizer(ptr, finalizer, data)                \
    GC_REGISTER_FINALIZER((ptr), (finalizer), (data), 0, 0)
#else
  #define MR_GC_register_finalizer(ptr, finalizer, data)
#endif

// Don't dereference a weak pointer directly, it won't work as the pointer
// is hidden from the GC by storing its negated bits.

#ifdef  MR_BOEHM_GC
#define MR_NULL_WEAK_PTR    0
typedef GC_hidden_pointer   MR_weak_ptr;
#else
#define MR_NULL_WEAK_PTR    NULL
typedef void*               MR_weak_ptr;
#endif

// Create a weak pointer to obj and store the pointer in the memory pointed
// to by weak_ptr, which must be a pointer to an MR_weak_ptr. obj must not
// be an internal pointer and weak_ptr must be located within a heap object
// managed by the GC.

#ifdef  MR_BOEHM_GC
#define MR_new_weak_ptr(weak_ptr, obj)                                        \
    do {                                                                      \
        int result;                                                           \
                                                                              \
        *(weak_ptr) = GC_HIDE_POINTER((obj));                                 \
        /*                                                                    \
        ** This call takes a double pointer, so it can clear the              \
        ** user's pointer. Recall that *weak_ptr is a hidden pointer          \
        ** a pointer cast to an int.                                          \
        */                                                                    \
                                                                              \
        result =                                                              \
            GC_general_register_disappearing_link((void**)(weak_ptr), (obj)); \
                                                                              \
        if (GC_DUPLICATE == result) {                                         \
            MR_fatal_error(                                                   \
                "Error registering weak pointer: already registered");        \
        } else if (GC_NO_MEMORY == result) {                                  \
            MR_fatal_error(                                                   \
                "Error registering weak pointer: out of memory");             \
        }                                                                     \
    } while (0)
#else
#define MR_new_weak_ptr(weak_ptr, obj)                                      \
    do {                                                                    \
        *(weak_ptr) = (obj);                                                \
    } while (0)
#endif

// Don't call this directly. It must be protected by Boehm's allocation lock,
// see MR_weak_ptr_read below.

#ifdef MR_BOEHM_GC
extern void*
MR_weak_ptr_read_unsafe(void* weak_ptr);
#endif

// Use this before dereferencing a weak pointer. weak_ptr must be a pointer
// to an MR_weak_ptr. This returns the real pointer that the weak pointer
// represents.

#ifdef MR_BOEHM_GC
#define MR_weak_ptr_read(weak_ptr)                                            \
    ((MR_NULL_WEAK_PTR != *(weak_ptr)) ?                                      \
        GC_call_with_alloc_lock(MR_weak_ptr_read_unsafe, (weak_ptr)) :        \
        NULL)
#else
#define MR_weak_ptr_read(weak_ptr)                                      \
    (*(weak_ptr))
#endif

// MR_GC_NEW_ATTRIB(type, attrib):
// MR_GC_NEW_UNCOLLECTABLE_ATTRIB(type, attrib):
// MR_GC_NEW_ARRAY_ATTRIB(type, attrib):
// MR_GC_malloc_attrib(bytes, attrib):
// MR_GC_malloc_uncollectable_attrib(bytes, attrib):
// MR_GC_realloc_attrib(ptr, num_bytes):
//  In grades with memory attribution support, these variants will allocate
//  an extra word before the object. The value stored `attrib' is stored
//  in that extra word.
//
// MR_GC_RESIZE_ARRAY_ATTRIB(ptr, type, num):
// MR_GC_free_attrib(ptr):
//  These variants take into account the extra word before ptr.
//  You must NOT pass pointers which were returned by non-"attrib"
//  functions/macros to these "attrib" variants, and vice versa.

#define MR_GC_NEW_ATTRIB(type, attrib)                                  \
    ((type *) MR_GC_malloc_attrib(sizeof(type), (attrib)))

#define MR_GC_NEW_UNCOLLECTABLE_ATTRIB(type, attrib)                    \
    ((type *) MR_GC_malloc_uncollectable_attrib(sizeof(type), (attrib)))

#define MR_GC_NEW_ARRAY_ATTRIB(type, num, attrib)                       \
    ((type *) MR_GC_malloc_attrib((num) * sizeof(type), (attrib)))

#define MR_GC_RESIZE_ARRAY_ATTRIB(ptr, type, num)                       \
    ((type *) MR_GC_realloc_attrib((ptr), (num) * sizeof(type)))

extern  void    *MR_GC_malloc_attrib(size_t num_bytes, void *attrib);
extern  void    *MR_GC_malloc_uncollectable_attrib(size_t num_bytes,
            void *attrib);
extern  void    *MR_GC_realloc_attrib(void *ptr, size_t num_bytes);
extern  void    MR_GC_free_attrib(void *ptr);

struct MR_AllocSiteInfo_Struct {
    MR_Code *MR_asi_proc;
    const char  *MR_asi_file_name;
    const int   MR_asi_line_number;
    const char  *MR_asi_type;
    const int   MR_asi_words;
};

// Builtin allocation site ids for use in the runtime and other
// hand-written code when context-specific ids are unavailable.
// MR_ALLOC_SITE_RUNTIME is a catch-all for internal runtime structures;
// these are hidden by default in `mprof -s' output.

#define MR_ALLOC_SITE_NONE      ((void *) 0)
#ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    // These must match the entries in mercury_heap_profile.c.
    extern MR_AllocSiteInfo         MR_builtin_alloc_sites[9];
    #define MR_ALLOC_SITE_RUNTIME   ((void *) &MR_builtin_alloc_sites[0])
    #define MR_ALLOC_SITE_FLOAT     ((void *) &MR_builtin_alloc_sites[1])
    #define MR_ALLOC_SITE_STRING    ((void *) &MR_builtin_alloc_sites[2])
    #define MR_ALLOC_SITE_TYPE_INFO ((void *) &MR_builtin_alloc_sites[3])
    #define MR_ALLOC_SITE_FOREIGN   ((void *) &MR_builtin_alloc_sites[4])
    #define MR_ALLOC_SITE_TABLING   ((void *) &MR_builtin_alloc_sites[5])
    #define MR_ALLOC_SITE_STM       ((void *) &MR_builtin_alloc_sites[6])
    #define MR_ALLOC_SITE_INT64     ((void *) &MR_builtin_alloc_sites[7])
    #define MR_ALLOC_SITE_UINT64    ((void *) &MR_builtin_alloc_sites[8])
#else
    #define MR_ALLOC_ID             MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_RUNTIME   MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_FLOAT     MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_STRING    MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_TYPE_INFO MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_FOREIGN   MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_TABLING   MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_STM       MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_INT64     MR_ALLOC_SITE_NONE
    #define MR_ALLOC_SITE_UINT64    MR_ALLOC_SITE_NONE
#endif

extern  void    *MR_new_object_func(size_t num_bytes,
                    MR_AllocSiteInfoPtr alloc_id, const char *name);
extern  void    *MR_new_object_atomic_func(size_t num_bytes,
                    MR_AllocSiteInfoPtr alloc_id, const char *name);

////////////////////////////////////////////////////////////////////////////

// MR_copy_string makes a copy of the given string,
// using memory allocated with MR_malloc().

extern char *MR_copy_string(const char *s);

////////////////////////////////////////////////////////////////////////////

// `MR_unit' is the size of the minimum unit of memory we allocate (in bytes).
// `MR_page_size' is the size of a single page of memory.

extern  size_t          MR_unit;
extern  size_t          MR_page_size;

////////////////////////////////////////////////////////////////////////////

// Users need to call MR_add_root() for any global variable which contains
// pointers to the Mercury heap. This information is only used for agc grades.

#ifdef MR_NATIVE_GC
  #define MR_add_root(root_ptr, type_info)                              \
    MR_agc_add_root((root_ptr), (type_info))
#else
  #define MR_add_root(root_ptr, type_info) // nothing
#endif

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_MEMORY_H
