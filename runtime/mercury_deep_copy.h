/*
** Copyright (C) 1997-2000, 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* deepcopy.h - declares the MR_deep_copy() function. */

#ifndef	MERCURY_DEEP_COPY_H
#define	MERCURY_DEEP_COPY_H

#include "mercury_types.h"	/* for `MR_Word' */
#include "mercury_type_info.h"	/* for `MR_TypeInfo' */

/*
** MR_deep_copy:
**
** 	Copy a data item, completely.
**
**	The copying is done depth first. Any part of the data
**	structure that is outside the given upper and lower
**	bounds not be copied, instead a reference to the
**	original data will be used. For conservative gc grades,
**	the entire data structure will be copied, as there is no
**	heap.  
**
**	The caller must provide the type_info describing
**	the type of this data structure. It must also
**	provide the upper and lower limits - if no limits are desired,
**	pass NULL as the lower_limit.
**
**	Deep copy returns the actual data that it copied,
**	which may need to be stored on the heap, or put in
**	a register or stack slot (depending on what you
**	are using deep copy for). This may be a tagged pointer,
**	or if the data is just a simple type like a constant
**	or integer, it will be the constant or integer itself.
**
** 	Please note - MR_deep_copy increments the heap pointer, 
** 	however on some platforms (notably, SPARCs) the 
** 	register-windows mean the transient Mercury registers
** 	may be lost. So before calling MR_deep_copy, call
** 		MR_save_transient_hp();
**
**	MR_deep_copy will use MR_restore_transient_hp()
**	to restore and modify the heap pointer, and
**	then call MR_save_transient_hp() to save it again.
**	(This may also restore/save other registers in the process.)
**
**	After calling MR_deep_copy, be sure to do a 
**		MR_restore_transient_hp();
**	so that the registers are restored.
**
**	If writing a C function that calls MR_deep_copy, make sure
**	you document that around your function,
**	MR_save_transient_hp()/MR_restore_transient_hp()
**	need to be used.
**
**	Deep copy does not preserve sharing of subterms.  Each
**	subterm is copied in full, except for data items that are
**	stored outside the heap limits. 
**	XXX For some applications, sharing is useful.  For others we
**	want a copy that is completely unique.  We should modify
**	MR_deep_copy to do both.
*/

MR_Word MR_deep_copy(MR_Word data, MR_TypeInfo type_info, 
	const MR_Word *lower_limit, const MR_Word *upper_limit);

/*
** MR_agc_deep_copy:
**
**	Just like MR_deep_copy(), but it will leave forwarding pointers
**	in the old data (destructively).  lower_limit and upper_limit
**	give the boundaries for copying data, and the boundaries for
**	leaving forwarding pointers.
**
**	Data will be copied to wherever the heap pointer is pointing.
**
**	A forwarding pointer will be left simply by copying the new
**	value of the data into the old location.  If the data was a
**	tagged pointer, the pointer will now refer to the rest of the
**	data on the new heap.  (If the data wasn't a tagged pointer, it
**	will be a constant anyway). 
**
**	The upper and lower limits allow forwarding pointers to be
**	detected and treated just as if they were pointers off the
**	heap (say to a constant data structure in the data segment of
**	the program).
**
**	Note: You cannot pass NULL as the lower_limit to MR_agc_deep_copy
**	(which is possible with normal MR_deep_copy).
*/

MR_Word MR_agc_deep_copy(MR_Word data, MR_TypeInfo type_info, 
	const MR_Word *lower_limit, const MR_Word *upper_limit);

/*
** This holds a bitmap used by MR_agc_deep_copy() to record which
** objects have already been copied and hence contain forwarding
** pointers.  It gets initialized by MR_garbage_collect().
*/
extern MR_Word *MR_has_forwarding_pointer;

/*
** MR_make_permanent:
**
**	Returns a copy of term that can be accessed safely even after
**	Mercury execution has backtracked past the point at which the
**	term was allocated.
**
**	Note that in conservative GC grades nothing needs to be done, and
**	hence the term is just returned.
**
**	When not using a conservative GC grade, MR_save_transient_hp()
**	and MR_restore_transient_hp() need to be used around this
**	function.  (When using a conservative GC grade, these macros
**	are harmless, so they can be used then too.)
*/

#define MR_make_permanent(term, type_info)			\
	MR_make_long_lived((term), (type_info), NULL)

/*
** MR_make_long_lived:
**
**	This is the same as MR_make_permanent, except that if limit is an
**	address on the heap, parts of term that are "older" than limit will
**	not be copied.  This is useful when you know that the permanent copy
**	of term will not be accessed after the heap pointer has backtracked
**	beyond limit.  Naturally, this always occurs when the permanent term
**	is to be stored in *limit.
**
**	I'd like to describe the limit argument without referring to the
**	"heap," but don't see how to.
*/

#ifdef MR_CONSERVATIVE_GC
  #define MR_make_long_lived(term, type_info, lower_limit) (term)
#else
  extern	MR_Word MR_make_long_lived(MR_Word term, MR_TypeInfo type_info,
		  MR_Word *lower_limit);
#endif

#endif /* not MERCURY_DEEP_COPY_H */
