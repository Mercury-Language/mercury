
/*
 *	$Id: mem.c,v 1.1 1997-02-11 09:15:04 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>

#include	<util.h>
#include	<mem.h>

/* Exported definitions */

/* Local declarations */

static char
rcs_id[]	= "$Id: mem.c,v 1.1 1997-02-11 09:15:04 aet Exp $";

static uchar
guard_bytes[] = {0xa5,0xa5,0xa5,0xa5,0xa5,0xa5,0xa5};

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

	guard_size = sizeof(guard_bytes) / sizeof(uchar);
	real_mem = (uchar*) mem - guard_size;
	free((void*)real_mem);

	return;
}

void*
mem_realloc(size_t size, void *mem)
{
	/*
	 * Check mem was actually allocated.
	 */
	mem_free(mem);

	return mem_malloc(size);
}


