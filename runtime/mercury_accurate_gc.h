/*
** Copyright (C) 1997-1998, 2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_ACCURATE_GC_H
#define MERCURY_ACCURATE_GC_H

/*
** mercury_accurate_gc.h -
**	Definitions for use by the accurate garbage collector (and
**	supporting code).
*/

#include "mercury_types.h"
#include "mercury_type_info.h"

/*---------------------------------------------------------------------------*/

#ifdef MR_HIGHLEVEL_CODE
/* Perform a garbage collection. */
extern	void	MR_garbage_collect(void);
#else
/*
** MR_schedule_agc:
** 	Schedule a garbage collection as soon as possible.  The PC
** 	(program counter) is used to find the procedure that is
** 	executing.  The stack pointer is then used to replace the saved
** 	continuation pointer with the address of the garbage collector
** 	routine.
*/
extern	void	MR_schedule_agc(MR_Code *pc_at_signal, MR_Word *sp_at_signal,
			MR_Word *curfr_at_signal);
#endif

/*
** Roots apart from the stacks are stored in this data structure.
**
** Essentially, this is a list of any pointers into the heap that are
** not stored on the heap or the det/nondet stacks.
**
** Each node stores the address of the root, and its type.  When a
** garbage collection occurs, the root will be modified.
*/

struct	MR_RootNode {
	MR_Word *root;
	MR_TypeInfo type_info;
	struct MR_RootNode* next;
};

typedef	struct MR_RootNode	*MR_RootList;

/*
** MR_agc_add_root:
** 	Adds the root whose address is supplied in root_addr with type
** 	described by type_info to the list of additional roots.
*/

extern	void	MR_agc_add_root(MR_Word *root_addr, MR_TypeInfo type_info);

/*---------------------------------------------------------------------------*/
#endif /* not MERCURY_ACCURATE_GC_H */
