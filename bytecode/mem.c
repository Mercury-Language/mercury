
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mem.c,v 1.2 1997-03-25 02:15:39 aet Exp $
*/

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>

#include	<util.h>
#include	<mem.h>

/* Exported definitions */

/* Local declarations */

static char
rcs_id[]	= "$Id: mem.c,v 1.2 1997-03-25 02:15:39 aet Exp $";

/* 
 * Make sure the size of guard_bytes is a multiple of 8 to ensure we 
 * don't get unaligned accesses, even on 64-bit architectures.
 */
static uchar
guard_bytes[] = {0xa5,0xa5,0xa5,0xa5,0xa5,0xa5,0xa5,0xa5};

/* Implementation */

void*
mem_malloc(size_t size)
{
	size_t		real_size;
	size_t		guard_size;
	uchar		*real_mem, *mem;

	guard_size = sizeof(guard_bytes) / sizeof(uchar);
	real_size = size + 2 * guard_size;
	
	real_mem = malloc(real_size);
	if (real_mem == NULL)
	{
		fatal("mem.mem_alloc: malloc failed");
	}

	/* 
	 * Now check all allocated memory for corruption.
	 * XXX: Fill this in later...
	 */
	
	mem = real_mem + guard_size;
	return (void*) mem;
}

void
mem_free(void *mem)
{
	size_t		guard_size;
	uchar		*real_mem;

	/*
	 * Check that the memory to be freed was actually allocated.
	 * We can't check for still-remaining references to the
	 * memory without some sort of memory-marking as done in
	 * Hans Boehm's conservative garbage collector.
	 */

	/*
	 * Check all allocated memory for corruption.
	 * XXX: Do this later...
	 */

	guard_size = sizeof(guard_bytes) / sizeof(uchar);
	real_mem = (uchar*) mem - guard_size;
	free((void*)real_mem);

	return;
}

void*
mem_realloc(void *mem, size_t size)
{

	void	*newmem;

	/*
	 * Check all allocated memory for corruption.
	 * XXX: Do this later...
	 */

	newmem = mem_malloc(size);
	memcpy(newmem, mem, size);

	/*
	 * Check mem was actually allocated.
	 * XXX: Do later...
	 */
	mem_free(mem);

	return newmem;
}


