/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: solutions.mod
** authors: conway, fjh.
**
** this module defines solutions/2 which takes a closure of type
** pred(T) in which the remaining argument is output.
*/

#include "imp.h"
#include "deep_copy.h"

Declare_entry(do_call_nondet_closure);

#ifndef CONSERVATIVE_GC

/*
** Define the base_type_info/layout for list.
**
** Any changes to the representation of base_type_layouts should also
** be reflected here.
**
*/

static const struct 
	mercury_data_solutions__base_type_info_list_1_struct
	mercury_data_solutions__base_type_info_list_1;

static const struct 
	mercury_data_solutions__base_type_layout_list_1_struct
	mercury_data_solutions__base_type_layout_list_1;

static const struct 
	mercury_data_solutions_typeinfo_list_T_struct
	mercury_data_solutions_typeinfo_list_T;

	/* type_info for a list(T) */

static const struct mercury_data_solutions_typeinfo_list_T_struct {
        const Word * f1;
        Integer f2;
} mercury_data_solutions_typeinfo_list_T = {
        (const Word *) &mercury_data_solutions__base_type_info_list_1,
        (Integer) 1
};

	/* base_type_info for lists */

static const struct mercury_data_solutions__base_type_info_list_1_struct {
        Integer f1;
        Code * f2;
        Code * f3;
        Code * f4;
        const Word * f5;
} mercury_data_solutions__base_type_info_list_1 = {
        (Integer) 1,
        (NULL),
        (NULL),
        (NULL),
        (const Word *) &mercury_data_solutions__base_type_layout_list_1
};

	/* data for []/0 */

static const struct mercury_data_solutions_list_nil_struct {
        Integer f1;
        Integer f2;
        const Word * f3;
} mercury_data_solutions_list_nil = {
        (Integer) 0,
        (Integer) 1,
        (const Word *) string_const("[]", 2)
};

	/* data for ./2 */

static const struct mercury_data_solutions_list_cons_struct {
        Integer f1;
        Integer f2;
        const Word * f3;
        const Word * f4;
} mercury_data_solutions_list_cons = {
        (Integer) 2,
        (Integer) 1,
        (const Word *) mkword(mktag(0), (Word) (const Word *) &mercury_data_solutions_typeinfo_list_T),
        (const Word *) string_const(".", 1)
};


static const struct mercury_data_solutions__base_type_layout_list_1_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_solutions__base_type_layout_list_1 = {
	make_typelayout(TYPELAYOUT_SIMPLE_TAG,
        	(Word) (const Word *) &mercury_data_solutions_list_nil),
	make_typelayout(TYPELAYOUT_SIMPLE_TAG,
        	(Word) (const Word *) &mercury_data_solutions_list_cons),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE),
	make_typelayout(TYPELAYOUT_CONST_TAG, TYPELAYOUT_UNUSED_VALUE)
};

#endif		/* ifndef CONSERVATIVE_GC */

BEGIN_MODULE(solutions_module)

BEGIN_CODE

/*
** :- pred builtin_solutions(pred(T), list(T)).
** :- mode builtin_solutions(pred(out) is multi/nondet, out) is det.
**
** Polymorphism will add an extra input parameter, a type_info for T,
** which we don't use at the moment (later it could be used to find
** the address of the deep copy routine).
**
** The type_info structure will be in r1 and the closure will be in r2
** with both caling conventions. The output should go either in r3
** (for the normal parameter convention) or r1 (for the compact parameter
** convention).
*/

#ifdef	COMPACT_ARGS
#define	solutions_output	r1
#else
#define	solutions_output	r3
#endif

mercury__std_util__builtin_solutions_2_0:
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__std_util__builtin_solutions_2_1),
			LABEL(mercury__std_util__builtin_solutions_2_0));
}
#endif
mercury__std_util__builtin_solutions_2_1:

#ifndef CONSERVATIVE_GC

#ifndef USE_TYPE_LAYOUT
	fatal_error("`solutions' not supported with this grade on this "
		    "system.\n"
		"Try using a `.gc' (conservative gc) grade.\n");
#endif

/*
** The following algorithm uses a `solutions heap', and will work with
** non-conservative gc. We create a solution, on the normal heap, then
** copy it to the solutions heap, a part of a solutions list. This list
** list then copied back to the mercury heap.
**
** An improvement to this is that we can copy each solution to the
** solutions heap, but have deep_copy add an offset to the pointers
** (at least, those that would otherwise point to the solutions heap),
** so that, when finished, a block move of the solutions heap back to the
** real heap will leave all the pointers in the correct place.
*/


/*
** Define some framevars we will be using - we need to keep the
** value of hp and the solutions hp (solhp) before we entered 
** solutions, so we can reset the hp after each solution, and
** reset the solhp after all solutions have been found.
** To do a deep copy, we need the type_info of the type of a solution,
** so we save the type_info in type_info_fv.
** Finally, we store the list of solutions so far in list_fv.
*/

#define saved_hp_fv	(framevar(0))
#define saved_solhp_fv	(framevar(1))
#define type_info_fv	(framevar(2))
#define list_fv		(framevar(3))

	/* create a nondet stack frame with four slots,
	   and set the failure continuation */
	
	mkframe("builtin_solutions", 4,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));

	/* setup the framevars */
	saved_solhp_fv = (Word) solutions_heap_pointer; 
	saved_hp_fv = (Word) hp;
	type_info_fv = r1;		
	list_fv = list_empty();

	/* setup for calling the closure */
	r1 = r2;
	r2 = (Word) 0;	/* the higher-order call has 0 extra input arguments */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */

	call(ENTRY(do_call_nondet_closure),
		LABEL(mercury__std_util__builtin_solutions_2_0_i1),
		LABEL(mercury__std_util__builtin_solutions_2_1));

mercury__std_util__builtin_solutions_2_0_i1:
{
	/* we found a solution (in r1) */

	/* save the current heap pointer */
	Word *temp_hp = hp;

	/* set heap to solutions heap */
	hp = (Word) solutions_heap_pointer;

	/* to be safe, save the registers before the call */
	save_transient_registers();

	/* deep copy it to the solutions heap, up to the saved_hp */
	r3 = deep_copy(r1, (Word *) type_info_fv, (Word *) saved_hp_fv, 
		heap_zone->top);

	/* restore the registers */
	restore_transient_registers();

	/* create a cons cell on the solutions heap */
	list_fv = list_cons(r3, list_fv);

	/* save solutions heap pointer */
	solutions_heap_pointer = (Word *) hp;

	/* reset the heap pointer - use the normal mercury heap */
	hp = temp_hp;

	redo();
}
	
mercury__std_util__builtin_solutions_2_0_i2:
	/* no more solutions */

	/* reset heap */
	hp = saved_hp_fv;

	/* copy all solutions to mercury heap */ 

	{  /* create a type_info for list(T), where T is the type
	      of the solutions */

	  Word* new_type_info[2];
	  
	  new_type_info[0] = (Word *) (Word)
	  	&mercury_data_solutions__base_type_info_list_1;
	  new_type_info[1] = (Word *) type_info_fv;

		/* deep_copy the list to the mercury heap, copying
		 * everything between where we started on the solutions
		 * heap, and the top of the solutions heap 
		 */
	  solutions_output = deep_copy(list_fv, (Word *) new_type_info,
		(Word *) saved_solhp_fv, solutions_heap_zone->top);
	}

	/* reset solutions heap to where it was before call to solutions  */
	solutions_heap_pointer = (Word *) saved_solhp_fv;
	
	/* discard the frame we made */
	succeed_discard();

#undef saved_hp_fv
#undef saved_solhp_fv
#undef type_info_fv
#undef list_fv

#else

/*
** The following algorithm is very straight-forward implementation
** but only works with `--gc conservative'.
** Since with conservative gc, we don't reclaim any memory on failure,
** but instead leave it to the garbage collector, there is no need to
** make deep copies of the solutions.  This is a `copy-zero' implementation ;-)
*/

	/* create a nondet stack frame with one slot, to hold the list
	   of solutions, and set the failure continuation */
	mkframe("builtin_solutions", 1,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));
	framevar(0) = list_empty();

	/* we do not (yet) need the type_info we are passed in r1 */
	/* call the higher-order pred closure that we were passed in r2 */
	r1 = r2;
	r2 = (Word) 0;	/* the higher-order call has 0 extra input arguments */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	call(ENTRY(do_call_nondet_closure),
		LABEL(mercury__std_util__builtin_solutions_2_0_i1),
		LABEL(mercury__std_util__builtin_solutions_2_1));

mercury__std_util__builtin_solutions_2_0_i1:
	/* we found a solution */
	/* insert it into the list, and then look for the next one */
	framevar(0) = list_cons(r1, framevar(0));
	redo();

mercury__std_util__builtin_solutions_2_0_i2:
	/* no more solutions */
	/* return the solutions list and discard the frame we made */
	solutions_output = framevar(0);
	succeed_discard();

#endif

END_MODULE
