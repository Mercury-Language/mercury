/*
** Copyright (C) 1997-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the deep_copy() functions.
**
** Deep copy is used for a number of different purposes.  Each variant
** has the same basic control structure, but differs in how memory
** is allocated, or whether forwarding pointers are left behind.
*/

#include "mercury_imp.h"
#include "mercury_deep_copy.h"
#include "mercury_type_info.h"
#include "mercury_memory.h"


MR_DECLARE_STRUCT(mercury_data___type_ctor_info_pred_0);
MR_DECLARE_STRUCT(mercury_data___type_ctor_info_func_0);

/*
** deep_copy(): see mercury_deep_copy.h for documentation.
*/

#undef  in_range
#define in_range(X)	(lower_limit == NULL || \
				((X) >= lower_limit && (X) <= upper_limit))

#undef	maybeconst
#define	maybeconst	const

#undef  copy
#define copy		deep_copy

#undef  copy_arg
#define copy_arg	deep_copy_arg

#undef  copy_type_info
#define copy_type_info	deep_copy_type_info

#undef  leave_forwarding_pointer
#define leave_forwarding_pointer(DataPtr, NewData)

#undef	found_forwarding_pointer
#define found_forwarding_pointer(Data)

#include "mercury_deep_copy_body.h"


/*
** agc_deep_copy(): see mercury_deep_copy.h for documentation.
*/

#undef  in_range
#define in_range(X)	((X) >= lower_limit && (X) <= upper_limit)

#undef	maybeconst
#define	maybeconst

#undef  copy
#define copy		agc_deep_copy

#undef  copy_arg
#define copy_arg	agc_deep_copy_arg

#undef  copy_type_info
#define copy_type_info	agc_deep_copy_type_info

#ifdef MR_DEBUG_AGC_FORWARDING
  #define FORWARD_DEBUG_MSG(Msg, Data)	\
		fprintf(stderr, Msg, Data)
#else
  #define FORWARD_DEBUG_MSG(Msg, Data)
#endif

#undef  leave_forwarding_pointer
#define leave_forwarding_pointer(DataPtr, NewData)		\
		if (in_range(DataPtr)) {			\
			FORWARD_DEBUG_MSG("forwarding to %lx\n",\
					(long) NewData);	\
			*DataPtr = NewData;			\
		}

#undef  found_forwarding_pointer
#define found_forwarding_pointer(Data)	\
		FORWARD_DEBUG_MSG("not on this heap: %lx\n", (long) Data);

#include "mercury_deep_copy_body.h"


/*---------------------------------------------------------------------------*/

#define SWAP(val1, val2, type)		\
	do {				\
		type swap_tmp;		\
		swap_tmp = (val1);	\
		(val1) = (val2);	\
		(val2) = swap_tmp;	\
	} while (0)

#ifndef CONSERVATIVE_GC
/*
** MR_make_long_lived(): see mercury_deep_copy.h for documentation.
*/
Word
MR_make_long_lived(Word term, Word *type_info, Word *lower_limit)
{
	Word result;

	restore_transient_hp();	/* Because we play with MR_hp */

	if (lower_limit < MR_heap_zone->bottom ||
			lower_limit > MR_heap_zone->top) {
		lower_limit = MR_heap_zone->bottom;
	}

	/* temporarily swap the heap with the global heap */
	SWAP(MR_heap_zone, MR_global_heap_zone, MemoryZone *);
	SWAP(MR_hp, MR_global_hp, Word *);

	/* copy values from the heap to the global heap */
	save_transient_hp();
	result = deep_copy(&term, type_info, lower_limit,
			MR_global_heap_zone->top);
	restore_transient_hp();

	/* swap the heap and global heap back again */
	SWAP(MR_heap_zone, MR_global_heap_zone, MemoryZone *);
	SWAP(MR_hp, MR_global_hp, Word *);

	save_transient_hp();	/* Because we played with MR_hp */

	return result;
}
#endif	/* not CONSERVATIVE_GC */

