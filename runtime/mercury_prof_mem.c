/*
** Copyright (C) 1996-1997, 1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** prof_mem.c
**
** Author:	petdr
**
** This module is needed to get around the problem that when a signal occurs
** it may be in a malloc.  The handling routine may also do a malloc which 
** stuffs up the internal state of malloc and cause a seg fault.
** To avoid this problem, we use our own version of malloc() which
** allocates memory in large chunks, reducing the chance of this
** problem occurring.
*/

#include <stdio.h>

#include "mercury_prof_mem.h"

#include "mercury_memory.h"	/* for MR_GC_malloc() */
#include "mercury_types.h"	/* for Word */

/*----------------------------------------------------------------------------*/


/*
** Constants
*/
#define	MEMORY_BLOCK	2048		/* Testing on a big input file on */
					/* the compiler normally required */
					/* around 6000 nodes.		  */


/*----------------------------------------------------------------------------*/


/*
** Private Global Variables
*/
static size_t	mem_left = 0;		/* Number of bytes left		*/
static void	*next	 = NULL;	/* Pointer to next data block	*/
					/* we can give away		*/


/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/


void *
MR_prof_malloc(size_t size)
{
	register void *p;

	/*
	** Ensure all allocations are word-aligned, by rounding size
	** up to the nearest multiple of the word size.
	**
	** Note that the current implementation of MR_prof_malloc only
	** guarantees that the memory will be Word-aligned; if you want to
	** allocate types that contain data types (e.g. `double') which might
	** require stricter alignment than that, then you will need to
	** change this to round the size up accordingly.
	*/
	size = ((size + sizeof(Word) - 1) / sizeof(Word)) * sizeof(Word);

	/* Here we waste a bit of space but hopefully not to much */
	if (mem_left < size) {
		next = MR_GC_malloc(MEMORY_BLOCK * size);
		mem_left = MEMORY_BLOCK * size;
	}

	p = next;

	next = (void *) ((char *) next + size);
	mem_left = mem_left - size;

	return p;
}


/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
