/*
** Copyright (C) 1996-1997 The University of Melbourne.
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
** stuff's up the internal state of malloc and cause a seg fault.
**
** NB.	This shouldn't be needed anymore when native gc is implemented
** 	as mercury will no longer use malloc anymore.
*/

#include <stdio.h>

#include "prof_mem.h"

#include "std.h"	/* for newmem() */

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
prof_malloc(size_t size)
{
	register void *p;

	/* Here we waste a bit of space but hopefully not to much */
	if (mem_left < size) {
		next = newmem(MEMORY_BLOCK * size);
		mem_left = MEMORY_BLOCK * size;
	}

	p = next;

	next = (void *) ((char *) next + size);
	mem_left = mem_left - size;

	return p;
}


/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
