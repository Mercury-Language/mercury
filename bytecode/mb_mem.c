
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	<stdlib.h>
#include	<string.h>
#include	"mercury_tags.h"

#include	"mb_mem.h"
#include	"mb_util.h"

/* Exported definitions */
void	*MB_malloc(size_t size);
void	*MB_realloc(void* mem, size_t size);
void	 MB_free(void *mem);
void	*MB_GC_malloc(size_t size);
void	*MB_GC_malloc_atomic(size_t size);
void	*MB_GC_realloc(void* mem, size_t size);
void	 MB_GC_free(void *mem);

/* Local declarations */

#ifdef MB_MALLOC_CHECK
void	*block_get(void *mem);
void	 block_check(void *block);
#endif

/* Implementation */

#ifdef MB_MALLOC_CHECK
/*
** Memory block layout:
** char guardbytes[]
** size_t size
** memory passed to client
** char guardbytes[]
**
** Make sure the size of guard_bytes is a multiple of 8 to ensure we 
** don't get unaligned accesses, even on 64-bit architectures.
*/
#define GRANULARITY	8	/* Alignment for block elements expect */
#define GUARD_VAL	0xa5
#define GUARD_SIZE	sizeof(guard_bytes)
static const unsigned char
guard_bytes[] = {	GUARD_VAL+0, GUARD_VAL+1, GUARD_VAL+2, GUARD_VAL+3,
			GUARD_VAL+4, GUARD_VAL+5, GUARD_VAL+6, GUARD_VAL+7 };

/* Offset of prologue guard bytes */
#define MEMBLOCK_PREGUARD(block, i)	\
	(((unsigned char *) block)[i])

/* Offset of size field */
#define MEMBLOCK_SIZE(block)				\
	(* (size_t *)					\
	 ((unsigned char *) block			\
	  + MB_MULTIPLEOF(GUARD_SIZE, GRANULARITY)	\
	 )						\
	)

/* Offset of memory block */
/* Offset of size field */
#define MEMBLOCK_MEM(block)				\
	(						\
	 ((unsigned char *) block			\
	  + MB_MULTIPLEOF(GUARD_SIZE, GRANULARITY)	\
	  + MB_MULTIPLEOF(sizeof(size_t), GRANULARITY )	\
	 )						\
	)

/* Offset of epilogue guard bytes */
#define MEMBLOCK_POSTGUARD(block, i)				\
	(							\
	 (MEMBLOCK_MEM(block)					\
	  + MB_MULTIPLEOF(MEMBLOCK_SIZE(block), GRANULARITY)	\
	 )[i]							\
	)

/* Total size of memory block */
#define MEMBLOCK_TOTALSIZE(memsize)				\
	( MB_MULTIPLEOF(GUARD_SIZE, GRANULARITY)		\
	+ MB_MULTIPLEOF(sizeof(size_t), GRANULARITY)		\
	+ MB_MULTIPLEOF(memsize, GRANULARITY)			\
	+ MB_MULTIPLEOF(GUARD_SIZE, GRANULARITY)		\
	)

/* Get the actual memory block start */
void *
block_get(void *mem)
{
	return (unsigned char *) mem 
			- MB_MULTIPLEOF(GUARD_SIZE, GRANULARITY)
			- MB_MULTIPLEOF(sizeof(size_t), GRANULARITY);
}

/* Checks a memory block for corruption and if not corrupt returns its size */
void
block_check(void *block)
{
	int		i;

	/* Check prologue guard bytes */
	for (i = 0; i < GUARD_SIZE; i++) {
		if (MEMBLOCK_PREGUARD(block, i) != guard_bytes[i]) {
			MB_fatal("mb_mem: block_check:"
				" memory corruption detected");
		}
	}

	/* Check epilogue guard bytes */
	for (i = 0; i < GUARD_SIZE; i++) {
		if (MEMBLOCK_POSTGUARD(block, i) != guard_bytes[i]) {
			MB_fatal("mb_mem: block_check:"
				" memory corruption detected");
		}
	}
}

void *
MB_malloc(size_t size)
{
	unsigned char	*block= malloc(MEMBLOCK_TOTALSIZE(size));


	if (block== NULL) {
		MB_fatal("MB_malloc failed");
	}

	MEMBLOCK_SIZE(block) = size;
	memcpy(&(MEMBLOCK_PREGUARD(block, 0)), guard_bytes, GUARD_SIZE);
	memcpy(&(MEMBLOCK_POSTGUARD(block, 0)), guard_bytes, GUARD_SIZE);

	return MEMBLOCK_MEM(block);
}

void
MB_free(void *mem)
{
	void* block = block_get(mem);

	block_check(block);

	/* Free the memory */
	free(block);
}

void *
MB_realloc(void *mem, size_t new_size)
{
	void* block = block_get(mem);

	if (new_size == 0) {
		MB_free(mem);
		return NULL;

	} else if (mem == NULL) {
		return MB_malloc(new_size);
		
	} else if (MEMBLOCK_SIZE(block) != new_size) {
		block_check(block);

		block = realloc(block, MEMBLOCK_TOTALSIZE(new_size)); 

		/* Update the size */
		MEMBLOCK_SIZE(block) = new_size;

		/* Redo the guard bytes at the end */
		memcpy(&MEMBLOCK_POSTGUARD(block, 0), guard_bytes, GUARD_SIZE);

	}

	block_check(block);

	return MEMBLOCK_MEM(block);
}

#else	/* MB_MALLOC_CHECK */

void *
MB_malloc(size_t size)
{
	return malloc(size);
}

void
MB_free(void *mem)
{
	return free(mem);
}

void *
MB_realloc(void *mem, size_t new_size)
{
	return realloc(mem, new_size);
}

#endif	/* MB_MALLOC_CHECK */

/* ------------------------------------------------------------------------- */

#ifndef MB_NO_GC

#include "gc.h"

void *
MB_GC_malloc(size_t size)
{
	return GC_malloc(size);
}

void *
MB_GC_malloc_atomic(size_t size)
{
	return GC_malloc_atomic(size);
}

void
MB_GC_free(void *mem)
{
	GC_free(mem);
}

void *
MB_GC_realloc(void *mem, size_t size)
{
	return GC_realloc(mem, size);
}

#else	/* MB_NO_GC */

void *
MB_GC_malloc(size_t size, MB_Bool atomic)
{
	return MB_malloc(size);
}

void
MB_GC_free(void *mem)
{
	MB_free(mem);
}

void *
MB_GC_realloc(void *mem, size_t size)
{
	return MB_realloc(mem, size);
}

#endif /* MB_NO_GC */

