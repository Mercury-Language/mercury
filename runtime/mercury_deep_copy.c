/*
** Copyright (C) 1997-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the MR_deep_copy() functions.
**
** Deep copy is used for a number of different purposes.  Each variant
** has the same basic control structure, but differs in how memory
** is allocated, or whether forwarding pointers are left behind.
*/

#include "mercury_imp.h"
#include "mercury_deep_copy.h"
#include "mercury_type_info.h"
#include "mercury_ho_call.h"
#include "mercury_layout_util.h"
#include "mercury_memory.h"


/*
** MR_deep_copy(): see mercury_deep_copy.h for documentation.
*/

#undef  in_range
#define in_range(X)	(lower_limit == NULL || \
				((X) >= lower_limit && (X) <= upper_limit))

#undef  in_traverse_range
#define in_traverse_range(X)	(MR_FALSE)

#undef	maybeconst
#define	maybeconst	const

#undef  copy
#define copy		MR_deep_copy

#undef  copy_arg
#define copy_arg	MR_deep_copy_arg

#undef  copy_type_info
#define copy_type_info	MR_deep_copy_type_info

#undef  copy_typeclass_info
#define copy_typeclass_info	MR_deep_copy_typeclass_info

#undef  leave_forwarding_pointer
#define leave_forwarding_pointer(DataPtr, NewData)

#undef	found_forwarding_pointer
#define found_forwarding_pointer(Data)

#include "mercury_deep_copy_body.h"


/*
** agc_deep_copy(): see mercury_deep_copy.h for documentation.
*/
#ifdef MR_NATIVE_GC

#undef  in_range
#define in_range(X)	((X) >= lower_limit && (X) <= upper_limit)

#undef  in_traverse_range(X)
#define in_traverse_range(X)	\
		((X) >= MR_ENGINE(MR_eng_solutions_heap_zone)->min && \
			(X) <= MR_ENGINE(MR_eng_solutions_heap_zone)->hardmax)

#undef	maybeconst
#define	maybeconst

#undef  copy
#define copy		MR_agc_deep_copy

#undef  copy_arg
#define copy_arg	MR_agc_deep_copy_arg

#undef  copy_type_info
#define copy_type_info	MR_agc_deep_copy_type_info

#undef  copy_typeclass_info
#define copy_typeclass_info	MR_agc_deep_copy_typeclass_info

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

#endif

/*---------------------------------------------------------------------------*/

#define SWAP(val1, val2, type)		\
	do {				\
		type swap_tmp;		\
		swap_tmp = (val1);	\
		(val1) = (val2);	\
		(val2) = swap_tmp;	\
	} while (0)

#ifndef MR_CONSERVATIVE_GC

/*
** MR_make_long_lived(): see mercury_deep_copy.h for documentation.
*/

MR_Word
MR_make_long_lived(MR_Word term, MR_TypeInfo type_info, MR_Word *lower_limit)
{
	MR_Word result;

	MR_restore_transient_hp();	/* Because we play with MR_hp */

	if (lower_limit < MR_ENGINE(MR_eng_heap_zone)->bottom ||
			lower_limit > MR_ENGINE(MR_eng_heap_zone)->top) {
		lower_limit = MR_ENGINE(MR_eng_heap_zone)->bottom;
	}

	/* temporarily swap the heap with the global heap */
	SWAP(MR_ENGINE(MR_eng_heap_zone), MR_ENGINE(MR_eng_global_heap_zone),
		MR_MemoryZone *);
	SWAP(MR_hp, MR_global_hp, MR_Word *);

	/* copy values from the heap to the global heap */
	MR_save_transient_hp();
	result = MR_deep_copy(&term, type_info, lower_limit,
			MR_ENGINE(MR_eng_global_heap_zone)->top);
	MR_restore_transient_hp();

	/* swap the heap and global heap back again */
	SWAP(MR_ENGINE(MR_eng_heap_zone), MR_ENGINE(MR_eng_global_heap_zone),
		MR_MemoryZone *);
	SWAP(MR_hp, MR_global_hp, MR_Word *);

	MR_save_transient_hp();	/* Because we played with MR_hp */

	return result;
}

#endif	/* not MR_CONSERVATIVE_GC */
