/*
INIT mercury_sys_init_call
ENDINIT
*/
/*
** Copyright (C) 1995-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module provides much of the functionality for doing
** higher order calls. The rest is provided by code generation of the
** higher_order_call HLDS construct.
**
** The called closure may contain only input arguments. The extra arguments
** provided by the higher-order call may be input or output, and may appear
** in any order.
**
** The input arguments to do_call_*_closure are the closure in r1,
** the number of additional input arguments in r2, the number of output
** arguments to expect in r3, and the additional input arguments themselves
** in r4, r5, etc. The output arguments are returned in registers r1, r2, etc
** for det and nondet calls or registers r2, r3, etc for semidet calls.
**
** The placement of the extra input arguments into r4, r5 etc is done by
** the code generator, as is the movement of the output arguments to their
** eventual destinations.
*/

#include "mercury_imp.h"
#include "mercury_ho_call.h"

	/* 
	** Number of input arguments to do_call_*_closure, 
	** r1 -> closure 
	** r2 -> number of immediate input arguments.
	** r3 -> number of output arguments (unused).
	*/
#define MR_HO_CALL_INPUTS		3

	/*
	** Number of input arguments to do_call_*_class_method,
	** r1 -> typeclass info
	** r2 -> index of method in typeclass info
	** r3 -> number of immediate input arguments.
	** r4 -> number of output arguments (unused).
	*/
#define MR_CLASS_METHOD_CALL_INPUTS	4

/*
** The following entries are obsolete, and are kept for bootstrapping only.
*/

Define_extern_entry(do_call_det_closure);
Define_extern_entry(do_call_semidet_closure);
Define_extern_entry(do_call_nondet_closure);
Define_extern_entry(do_call_old_closure);

Define_extern_entry(do_call_det_class_method);
Define_extern_entry(do_call_semidet_class_method);
Define_extern_entry(do_call_nondet_class_method);

/*
** These are the real implementations of higher order calls and method calls.
*/

Define_extern_entry(mercury__do_call_closure);
Define_extern_entry(mercury__do_call_class_method);

/*
** These are the real implementations of unify, index and compare.
*/

Define_extern_entry(mercury__unify_2_0);
Define_extern_entry(mercury__index_2_0);
Declare_label(mercury__index_2_0_i1);
Define_extern_entry(mercury__compare_3_0);
Define_extern_entry(mercury__compare_3_1);
Define_extern_entry(mercury__compare_3_2);
Define_extern_entry(mercury__compare_3_3);
Declare_label(mercury__compare_3_0_i1);
Define_extern_entry(mercury__solve_equal_2_0);
Define_extern_entry(mercury__init_1_0);

BEGIN_MODULE(call_module)
	init_entry_ai(do_call_det_closure);
	init_entry_ai(do_call_semidet_closure);
	init_entry_ai(do_call_nondet_closure);
	init_entry_ai(do_call_old_closure);

	init_entry_ai(mercury__do_call_closure);

	init_entry_ai(do_call_det_class_method);
	init_entry_ai(do_call_semidet_class_method);
	init_entry_ai(do_call_nondet_class_method);

	init_entry_ai(mercury__do_call_class_method);

	init_entry_ai(mercury__unify_2_0);
	init_entry_ai(mercury__index_2_0);
	init_entry_ai(mercury__compare_3_0);
	init_entry_ai(mercury__compare_3_1);
	init_entry_ai(mercury__compare_3_2);
	init_entry_ai(mercury__compare_3_3);
	init_entry(mercury__solve_equal_2_0);
	init_entry(mercury__init_1_0);
BEGIN_CODE

Define_entry(do_call_det_closure);
	tailcall(ENTRY(mercury__do_call_closure),
		LABEL(do_call_det_closure));
Define_entry(do_call_semidet_closure);
	tailcall(ENTRY(mercury__do_call_closure),
		LABEL(do_call_semidet_closure));
Define_entry(do_call_nondet_closure);
	tailcall(ENTRY(mercury__do_call_closure),
		LABEL(do_call_nondet_closure));

Define_entry(do_call_old_closure);
{
	Word	closure;
	int	i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* number of immediate input args */

	save_registers();

	if (num_in_args < MR_HO_CALL_INPUTS) {
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(i + num_in_args) =
				virtual_reg(i + MR_HO_CALL_INPUTS);
		}
	} else if (num_in_args > MR_HO_CALL_INPUTS) {
		for (i = num_extra_args; i>0; i--) {
			virtual_reg(i + num_in_args) =
				virtual_reg(i + MR_HO_CALL_INPUTS);
		}
	} /* else do nothing because i == MR_HO_CALL_INPUTS */

	for (i = 1; i <= num_in_args; i++) {
		virtual_reg(i) = field(0, closure, i + 1); /* copy args */
	}

	restore_registers();

	tailcall((Code *) field(0, closure, 1), LABEL(do_call_det_closure));
}

Define_entry(mercury__do_call_closure);
{
	MR_Closure	*closure;
	int		num_extra_args;	/* # of args provided by our caller */
	int		num_hidden_args;/* # of args hidden in the closure  */
	int		i;

	closure = (MR_Closure *) r1;

	/* This check is for bootstrapping only. */
	if (((Word) closure->MR_closure_layout) < 1024) {
		/* we found an old-style closure, call the old handler */
		tailcall(ENTRY(do_call_old_closure),
			LABEL(mercury__do_call_closure));
	}

	num_extra_args = r2;
	num_hidden_args = closure->MR_closure_num_hidden_args;

	save_registers();

	if (num_hidden_args < MR_HO_CALL_INPUTS) {
		/* copy to the left, from the left */
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(i + num_hidden_args) =
				virtual_reg(i + MR_HO_CALL_INPUTS);
		}
	} else if (num_hidden_args > MR_HO_CALL_INPUTS) {
		/* copy to the right, from the right */
		for (i = num_extra_args; i > 0; i--) {
			virtual_reg(i + num_hidden_args) =
				virtual_reg(i + MR_HO_CALL_INPUTS);
		}
	} /* else the new args are in the right place */

	for (i = 1; i <= num_hidden_args; i++) {
		virtual_reg(i) = closure->MR_closure_hidden_args(i);
	}

	restore_registers();

	tailcall(closure->MR_closure_code,
		LABEL(mercury__do_call_closure));
}

Define_entry(do_call_det_class_method);
	tailcall(ENTRY(mercury__do_call_class_method),
		LABEL(do_call_det_class_method));
Define_entry(do_call_semidet_class_method);
	tailcall(ENTRY(mercury__do_call_class_method),
		LABEL(do_call_semidet_class_method));
Define_entry(do_call_nondet_class_method);
	tailcall(ENTRY(mercury__do_call_class_method),
		LABEL(do_call_nondet_class_method));

	/*
	** r1: the typeclass_info
	** r2: index of class method
	** r3: number of immediate input arguments
	** r4: number of output arguments
	** r5+:input args
	*/

Define_entry(mercury__do_call_class_method);
{
	Code 	*destination;
	int	num_in_args;
	int	num_arg_typeclass_infos;
	int	i;

	destination = MR_typeclass_info_class_method(r1, r2);
	num_arg_typeclass_infos = (int) MR_typeclass_info_instance_arity(r1);

	num_in_args = r3; /* number of input args */

	save_registers();

	if (num_arg_typeclass_infos < MR_CLASS_METHOD_CALL_INPUTS) {
		/* copy to the left, from the left */
		for (i = 1; i <= num_in_args; i++) {
			virtual_reg(i + num_arg_typeclass_infos) =
				virtual_reg(i + MR_CLASS_METHOD_CALL_INPUTS);
		}
	} else if (num_arg_typeclass_infos > MR_CLASS_METHOD_CALL_INPUTS) {
		/* copy to the right, from the right */
		for (i = num_in_args; i > 0; i--) {
			virtual_reg(i + num_arg_typeclass_infos) =
				virtual_reg(i + MR_CLASS_METHOD_CALL_INPUTS);
		}
	} /* else the new args are in the right place */

	for (i = num_arg_typeclass_infos; i > 0; i--) {
		virtual_reg(i) = 
			MR_typeclass_info_arg_typeclass_info(virtual_reg(1),i);
	}

	restore_registers();

	tailcall(destination, LABEL(mercury__do_call_class_method));
}

/*
** mercury__unify_2_0 is called as `unify(TypeInfo, X, Y)'
** in the mode `unify(in, in, in) is semidet'.
**
** We call the type-specific unification routine as
** `UnifyPred(...ArgTypeInfos..., X, Y)' is semidet, with all arguments input.
*/

Define_entry(mercury__unify_2_0);
{
	Code	*unify_pred;	/* address of the unify pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before the first */
				/* type_info argument */
	Word	x, y;
	int	i;

	Word	type_info;
	Word	type_ctor_info;

	type_info = r1;
	x = r2;
	y = r3;

	type_ctor_info = field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		unify_pred = (Code *) field(0, type_info,
					OFFSET_FOR_UNIFY_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		unify_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_UNIFY_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `UnifyPred(...ArgTypeInfos..., X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;
	virtual_reg(type_arity + 2) = y;

	restore_registers();

	tailcall(unify_pred, LABEL(mercury__unify_2_0));
}

/*
** mercury__index_2_0 is called as `index(TypeInfo, X, Index)'
** in the mode `index(in, in, out) is det'.
**
** We call the type-specific index routine as
** `IndexPred(...ArgTypeInfos..., X, Index)' is det.
** The ArgTypeInfo and X arguments are input, while the Index argument
** is output.
*/

Define_entry(mercury__index_2_0);
{
	Code	*index_pred;	/* address of the index pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before the first */
				/* type_info argument */
	Word	x;
	int	i;

	Word	type_info;
	Word	type_ctor_info;

	type_info = r1;
	x = r2;
	type_ctor_info = field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		index_pred = (Code *) field(0, type_info,
					OFFSET_FOR_INDEX_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		index_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_INDEX_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `IndexPred(...ArgTypeInfos..., X, Index)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

	tailcall(index_pred, LABEL(mercury__index_2_0));
}

/*
** mercury__compare_3_3 is called as `compare(TypeInfo, Result, X, Y)'
** in the mode `compare(in, out, in, in) is det'.
**
** (The additional entry points replace either or both "in"s with "ui"s.)
**
** We call the type-specific compare routine as
** `ComparePred(...ArgTypeInfos..., Result, X, Y)' is det.
** The ArgTypeInfo arguments are input, and are passed in r1, r2, ... rN.
** The X and Y arguments are also input, and are passed in rN+1 and rN+2.
** The Index argument is output.
*/

Define_entry(mercury__compare_3_0);
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__compare_3_3), LABEL(mercury__compare_3_0));
}
#endif
Define_entry(mercury__compare_3_1);
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__compare_3_3), LABEL(mercury__compare_3_1));
}
#endif
Define_entry(mercury__compare_3_2);
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__compare_3_3), LABEL(mercury__compare_3_2));
}
#endif
Define_entry(mercury__compare_3_3);
{
	Code	*compare_pred;	/* address of the compare pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before the first */
				/* type_info argument */
	Word	x, y;
	int	i;

	Word	type_info;
	Word	type_ctor_info;

	type_info = r1;
	x = r2;
	y = r3;

	type_ctor_info = field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		compare_pred = (Code *) field(0, type_info,
						OFFSET_FOR_COMPARE_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		compare_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_COMPARE_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `ComparePred(...ArgTypeInfos..., Result, X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;
	virtual_reg(type_arity + 2) = y;

	restore_registers();

	tailcall(compare_pred, LABEL(mercury__compare_3_3));
}

#ifdef MR_USE_SOLVE_EQUAL
/*
** mercury__solve_equal_2_0 is called as `solve_equal(TypeInfo, X, Y)'
** in the mode `solve_equal(in, in(any), in(any)) is semidet'.
**
** We call the type-specific unification routine as
** `SolveEqualPred(...ArgTypeInfos..., X, Y)' is semidet, with all arguments
** input.
*/

Define_entry(mercury__solve_equal_2_0);
{
	Code	*solve_equal_pred;	/* address of the solve_equal pred */
					/* for this type */
	int	type_arity;		/* number of type_info args */
	Word	args_base;		/* the address of the word before */
					/* the first type_info argument */
	Word	x, y;
	int	i;

	Word	type_info;
	Word	type_ctor_info;

	type_info = r1;
	x = r2;
	y = r3;

	type_ctor_info = field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		solve_equal_pred = (Code *) field(0, type_info,
				OFFSET_FOR_SOLVE_EQUAL_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		solve_equal_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_SOLVE_EQUAL_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `SolveEqualPred(...ArgTypeInfos..., X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;
	virtual_reg(type_arity + 2) = y;

	restore_registers();

	tailcall(solve_equal_pred, LABEL(mercury__solve_equal_2_0));
}
#else /* not MR_USE_SOLVE_EQUAL */
Define_entry(mercury__solve_equal_2_0);
	incr_sp_push_msg(2, "builtin:solve_equal");
	fatal_error("solve_equal not available in this grade");
#endif /* not MR_USE_SOLVE_EQUAL */

#ifdef MR_USE_INIT
/*
** mercury__init_1_0 is called as `init(TypeInfo, X)'
** in the mode `init(in, out(any)) is semidet'.
**
** We call the type-specific unification routine as
** `InitPred(...ArgTypeInfos..., X)' is det, with all arguments
** input.
*/

Define_entry(mercury__init_1_0);
{
	Code	*init_pred;	/* address of the init pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before */
				/* the first type_info argument */
	Word	x;
	int	i;

	Word	type_info;
	Word	type_ctor_info;

	type_info = r1;
	x = r2;

	type_ctor_info = field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		init_pred = (Code *) field(0, type_info,
				OFFSET_FOR_INIT_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		init_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_INIT_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `InitPred(...ArgTypeInfos..., X)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

	tailcall(init_pred, LABEL(mercury__init_1_0));
}
#else /* not MR_USE_INIT */
Define_entry(mercury__init_1_0);
	incr_sp_push_msg(2, "builtin:init");
	fatal_error("init not available in this grade");
#endif /* not MR_USE_INIT */

END_MODULE

void mercury_sys_init_call(void); /* suppress gcc warning */
void mercury_sys_init_call(void) {
	call_module();
}
