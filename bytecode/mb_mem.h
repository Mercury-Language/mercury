
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** This file provides basic memory management interfaces
** 
*/

#ifndef MB_MEM_H
#define	MB_MEM_H

#include <stdlib.h>

#include "mb_basetypes.h"

/*
** Do not use MB_malloc() or MB_realloc() directly, unless you want
** to allocate raw memory.  Normally you should use the macros
** MB_NEW(), MB_NEW_ARRAY(), and MB_RESIZE_ARRAY() instead.
**
** None of these are garbage collected and are invisible to the garbage
** collector
*/

/*
** These should not be used for small allocations; guard bytes are added
** for checking. The garbage collected versions are faster for small
** allocations
*/
void* MB_malloc(size_t size);

void* MB_realloc(void* mem, size_t size);

void MB_free(void *mem);

#define MB_NEW(type)		((type *) MB_malloc(sizeof(type)))
#define MB_NEW_ARRAY(type, num)	((type *) MB_malloc((num) * sizeof(type)))
#define MB_RESIZE_ARRAY(array, type, num) \
	((type *) MB_realloc((array), (num) * sizeof(type)))

/*
** Garbage collected versions of the above
** Uses Hans Boehm's conservative garbage collector
*/

/* Atomic == doesn't contain any pointers */

void* MB_GC_malloc(size_t size);

void* MB_GC_malloc_atomic(size_t size);

/* works for both atomic and nonatomic memory */
void* MB_GC_realloc(void* mem, size_t size);

void MB_GC_free(void *mem);

#define MB_GC_NEW(type) \
	((type *) MB_GC_malloc(sizeof(type)))

#define MB_GC_NEW_ATOMIC(type) \
	((type *) MB_GC_malloc_atomic(sizeof(type)))
	
#define MB_GC_NEW_ARRAY(type, num) \
	((type *) MB_GC_malloc((num) * sizeof(type)))
	
#define MB_GC_NEW_ARRAY_ATOMIC(type, num) \
	((type *) MB_GC_malloc((num) * sizeof(type)))

/* works for both atomic and nonatomic memory */
#define MB_GC_RESIZE_ARRAY(array, type, num) \
	((type *) MB_GC_realloc((array), (num) * sizeof(type)))

#endif	/* MB_MEM_H */


