
/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id:"
*/

/* Imports */
#include	<stdlib.h>
#include	<string.h>

#include	"mb_mem.h"
#include	"mb_util.h"

/* Exported definitions */

/* Local declarations */

static char
rcs_id[]	= "$Id:";

/* Implementation */

/* 
 * Make sure the size of guard_bytes is a multiple of 8 to ensure we 
 * don't get unaligned accesses, even on 64-bit architectures.
 */
static const unsigned char
guard_bytes[] = {0xa5,0xa5,0xa5,0xa5,0xa5,0xa5,0xa5,0xa5};

/* Implementation */

void*
MB_malloc(size_t size)
{
	size_t		real_size;
	size_t		guard_size;
	unsigned char	*real_mem, *mem;

	guard_size = sizeof(guard_bytes) / sizeof(*guard_bytes);
	real_size = size + 2 * guard_size;
	
	real_mem = malloc(real_size);
	if (real_mem == NULL) {
		MB_fatal("mem.MB_alloc: malloc failed");
	}

	/* 
	 * Now check all allocated memory for corruption.
	 * XXX: Fill this in later...
	 */
	
	mem = real_mem + guard_size;
	return mem;
}

void
MB_free(void *mem)
{
	size_t		guard_size;
	unsigned char	*real_mem;

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

	guard_size = sizeof(guard_bytes) / sizeof(*guard_bytes);
	real_mem = (unsigned char *) mem - guard_size;
	free(real_mem);

	return;
}

void*
MB_realloc(void *mem, size_t size)
{

	void	*new_mem;

	/*
	 * Check all allocated memory for corruption.
	 * XXX: Do this later...
	 */

	new_mem = MB_malloc(size);
	memcpy(new_mem, mem, size);

	/*
	 * Check mem was actually allocated.
	 * XXX: Do later...
	 */
	MB_free(mem);

	return new_mem;
}



