/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_AGC_DEBUG_H
#define MERCURY_AGC_DEBUG_H

/*
** mercury_agc_debug.h -
**	Debugging support for accurate garbage collection.
*/

#include "mercury_types.h"		/* for Word */
#include "mercury_label.h"		/* for MR_Internal */
#include "mercury_memory_zones.h"	/* for MemoryZone */
#include "mercury_accurate_gc.h"	/* for MR_RootList */

/*---------------------------------------------------------------------------*/

/*
** MR_agc_dump_stack_frames:
** 	Dump the stack, writing all information available about each
** 	stack frame.
** 	
** 	label is the topmost label on the stack, heap_zone is the zone
** 	which the data is stored upon. 
*/

extern	void	MR_agc_dump_stack_frames(MR_Internal *label, MemoryZone
		*heap_zone, Word *stack_pointer, Word *current_frame);

/*
** MR_agc_dump_roots:
** 	Dump the extra rootset, writing all information about each root.
*/
extern	void	MR_agc_dump_roots(MR_RootList roots);

/*---------------------------------------------------------------------------*/
#endif /* not MERCURY_AGC_DEBUG_H */
