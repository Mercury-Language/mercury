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
** The procedure whose address is contained in the closure must use the
** `compact' argument convention.
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

Define_extern_entry(do_call_det_closure);
Define_extern_entry(do_call_semidet_closure);
Define_extern_entry(do_call_nondet_closure);

Define_extern_entry(do_call_det_class_method);
Define_extern_entry(do_call_semidet_class_method);
Define_extern_entry(do_call_nondet_class_method);

Define_extern_entry(mercury__unify_2_0);
Define_extern_entry(mercury__index_2_0);
Declare_label(mercury__index_2_0_i1);
Define_extern_entry(mercury__compare_3_0);
Define_extern_entry(mercury__compare_3_1);
Define_extern_entry(mercury__compare_3_2);
Define_extern_entry(mercury__compare_3_3);
Declare_label(mercury__compare_3_0_i1);

#ifdef	COMPACT_ARGS
  /*
  ** With compact args, all these methods just do some data shuffling
  ** and then a tailcall. They never have stack frames, and therefore
  ** do not participate in stack traces.
  */
#else
  /*
  ** With simple args, some of these procedures make proper calls,
  ** and thus have stack frames.
  */
  MR_MAKE_PROC_LAYOUT(mercury__index_2_0,
	MR_DETISM_DET, 2, MR_LIVE_LVAL_STACKVAR(2),
	MR_PREDICATE, ""builtin"", ""index"", 2, 0);
  MR_MAKE_INTERNAL_LAYOUT(mercury__index_2_0, 1);
  MR_MAKE_PROC_LAYOUT(mercury__compare_3_0,
	MR_DETISM_DET, 2, MR_LIVE_LVAL_STACKVAR(2),
	MR_PREDICATE, ""builtin"", ""compare"", 3, 0);
  MR_MAKE_INTERNAL_LAYOUT(mercury__compare_3_0, 1);
#endif

BEGIN_MODULE(call_module)
	init_entry_ai(do_call_det_closure);
	init_entry_ai(do_call_semidet_closure);
	init_entry_ai(do_call_nondet_closure);

	init_entry_ai(do_call_det_class_method);
	init_entry_ai(do_call_semidet_class_method);
	init_entry_ai(do_call_nondet_class_method);

	init_entry_ai(mercury__unify_2_0);
#ifdef	COMPACT_ARGS
	init_entry_ai(mercury__index_2_0);
	init_entry_ai(mercury__compare_3_0);
#else
	init_entry_sl(mercury__index_2_0);
	init_label_sl(mercury__index_2_0_i1);
	init_entry_sl(mercury__compare_3_0);
	init_label_sl(mercury__compare_3_0_i1);
#endif
	init_entry_ai(mercury__compare_3_1);
	init_entry_ai(mercury__compare_3_2);
	init_entry_ai(mercury__compare_3_3);
BEGIN_CODE

Define_entry(do_call_det_closure);
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

Define_entry(do_call_semidet_closure);
{
	Word	closure;
	int	i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* the number of immediate input args */

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

	tailcall((Code *) field(0, closure, 1), 
		LABEL(do_call_semidet_closure));
}

Define_entry(do_call_nondet_closure);
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
		for (i = num_extra_args; i > 0; i--) {
			virtual_reg(i + num_in_args) =
				virtual_reg(i + MR_HO_CALL_INPUTS);
		}
	} /* else do nothing because i == MR_HO_CALL_INPUTS */

	for (i = 1; i <= num_in_args; i++) {
		virtual_reg(i) = field(0, closure, i + 1); /* copy args */
	}

	restore_registers();

	tailcall((Code *) field(0, closure, 1), LABEL(do_call_nondet_closure));
}

	/*
	** r1: the typeclass_info
	** r2: index of class method
	** r3: number of immediate input arguments
	** r4: number of output arguments
	** r5+:input args
	*/
Define_entry(do_call_det_class_method);
{
	Code 	*destination;
	int	i, num_in_args, num_arg_typeclass_infos;

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
	} /*
	  ** else do nothing because 
	  ** num_arg_typeclass_infos == MR_CLASS_METHOD_CALL_INPUTS
	  */

	for (i = num_arg_typeclass_infos; i > 0; i--) {
		virtual_reg(i) = 
			MR_typeclass_info_arg_typeclass_info(virtual_reg(1),i);
	}

	restore_registers();

	tailcall(destination, LABEL(do_call_det_class_method));
}

Define_entry(do_call_semidet_class_method);
{
	Code 	*destination;
	int	i, num_in_args, num_arg_typeclass_infos;

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
	} /*
	  ** else do nothing because
	  ** num_arg_typeclass_infos == MR_CLASS_METHOD_CALL_INPUTS
	  */

	for (i = num_arg_typeclass_infos; i > 0; i--) {
		virtual_reg(i) = 
			MR_typeclass_info_arg_typeclass_info(virtual_reg(1),i);
	}

	restore_registers();

	tailcall(destination, LABEL(do_call_semidet_class_method));
}

Define_entry(do_call_nondet_class_method);
{
	Code 	*destination;
	int	i, num_in_args, num_arg_typeclass_infos;

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
	} /* 
	  ** else do nothing because
	  ** num_arg_typeclass_infos == MR_CLASS_METHOD_CALL_INPUTS
	  */

	for (i = num_arg_typeclass_infos; i > 0; i--) {
		virtual_reg(i) = 
			MR_typeclass_info_arg_typeclass_info(virtual_reg(1),i);
	}

	restore_registers();

	tailcall(destination, LABEL(do_call_nondet_class_method));
}

/*
** mercury__unify_2_0 is called as `unify(TypeInfo, X, Y)'
** in the mode `unify(in, in, in) is semidet'.
**
** With the simple parameter passing convention, the inputs are in the
** registers r2, r3 and r4. With the compact parameter passing convention,
** the inputs are in the registers r1, r2 and r3.
**
** The only output is the success/failure indication,
** which goes in r1 with both calling conventions.
**
** We call the type-specific unification routine as
** `UnifyPred(...ArgTypeInfos..., X, Y)' is semidet, with all arguments input.
** Again r1 will hold the success/failure continuation; the input arguments
** start either in r1 or r2 depending on the argument passing convention.
*/

Define_entry(mercury__unify_2_0);
{
	Code	*unify_pred;	/* address of the unify pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before the first */
				/* type_info argument */
	Word	x, y;
	int	i;

	Word	type_ctor_info;

	x = mercury__unify__x;
	y = mercury__unify__y;

	type_ctor_info = field(0, mercury__unify__typeinfo, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		unify_pred = (Code *) field(0, mercury__unify__typeinfo,
				OFFSET_FOR_UNIFY_PRED);
		/* args_base will not be needed */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		unify_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_UNIFY_PRED);
		args_base = mercury__unify__typeinfo;
	}

	save_registers();

	/* we call `UnifyPred(...ArgTypeInfos..., X, Y)' */
	/* virtual_reg(1) will hold the success/failure indication */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i + mercury__unify__offset) =
			field(0, args_base, i);
	}
	virtual_reg(type_arity + mercury__unify__offset + 1) = x;
	virtual_reg(type_arity + mercury__unify__offset + 2) = y;

	restore_registers();

	tailcall(unify_pred, LABEL(mercury__unify_2_0));
}

/*
** mercury__index_2_0 is called as `index(TypeInfo, X, Index)'
** in the mode `index(in, in, out) is det'.
**
** With both parameter passing conventions, the inputs are in r1 and r2.
** With the simple parameter passing convention, the output is in r3;
** with the compact parameter passing convention, the output is in r1.
**
** We call the type-specific index routine as
** `IndexPred(...ArgTypeInfos..., X, Index)' is det.
** The ArgTypeInfo and X arguments are input, and are passed in r1, r2, ... rN
** with both conventions. The Index argument is output; it is returned in
** r1 with the compact convention and rN+1 with the simple convention.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the simple convention, we can't.
*/

Define_entry(mercury__index_2_0);
{
	Code	*index_pred;	/* address of the index pred for this type */
	int	type_arity;	/* number of type_info args */
	Word	args_base;	/* the address of the word before the first */
				/* type_info argument */
	Word	x;
	int	i;

	Word	type_ctor_info;

	x = r2;
	type_ctor_info = field(0, r1, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		index_pred = (Code *) field(0, r1, OFFSET_FOR_INDEX_PRED);
		/* args_base will not be needed */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		index_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_INDEX_PRED);
		args_base = r1;
	}

	save_registers();

	/* we call `IndexPred(...ArgTypeInfos..., X, Index)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(index_pred, LABEL(mercury__index_2_0));
#else
	incr_sp_push_msg(2, "mercury__index_2_0");
	MR_stackvar(2) = (Word) MR_succip;
	MR_stackvar(1) = type_arity;
	call(index_pred, LABEL(mercury__index_2_0_i1), 
		LABEL(mercury__index_2_0));
}
Define_label(mercury__index_2_0_i1);
{
	MR_succip = (Code *) MR_stackvar(2);
	save_registers();
	r3 = virtual_reg(MR_stackvar(1) + 2);
	decr_sp_pop_msg(2);
	proceed();
#endif
}

/*
** mercury__compare_3_3 is called as `compare(TypeInfo, Result, X, Y)'
** in the mode `compare(in, out, in, in) is det'.
**
** (The additional entry points replace either or both "in"s with "ui"s.)
**
** With the simple parameter passing convention, the inputs are in r1,
** r3 and r4, while the output is in r2.
**
** With the compact parameter passing convention, the inputs are in r1,
** r2 and r3, while the output is in r1.
**
** We call the type-specific compare routine as
** `ComparePred(...ArgTypeInfos..., Result, X, Y)' is det.
** The ArgTypeInfo arguments are input, and are passed in r1, r2, ... rN
** with both conventions. The X and Y arguments are also input, but are passed
** in different registers (rN+2 and rN+3 with the simple convention and rN+1
** and rN+2 with the compact convention). The Index argument is output; it is
** returned in ** r1 with the compact convention and rN+1 with the simple
** convention.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the simple convention, we can't.
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

	Word	type_ctor_info;

	x = mercury__compare__x;
	y = mercury__compare__y;

	type_ctor_info = field(0, mercury__compare__typeinfo, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		compare_pred = (Code *) field(0, mercury__compare__typeinfo,
				OFFSET_FOR_COMPARE_PRED);
		/* args_base will not be needed */
	} else {
		type_arity = field(0, type_ctor_info, OFFSET_FOR_COUNT);
		compare_pred = (Code *) field(0, type_ctor_info,
				OFFSET_FOR_COMPARE_PRED);
		args_base = mercury__compare__typeinfo;
	}

	save_registers();

	/* we call `ComparePred(...ArgTypeInfos..., Result, X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = field(0, args_base, i);
	}
	virtual_reg(type_arity + mercury__compare__offset + 1) = x;
	virtual_reg(type_arity + mercury__compare__offset + 2) = y;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(compare_pred, LABEL(mercury__compare_3_3));
#else
	incr_sp_push_msg(2, "mercury__index_2_0");
	MR_stackvar(2) = (Word) MR_succip;
	MR_stackvar(1) = type_arity;
	call(compare_pred, LABEL(mercury__compare_3_0_i1),
		LABEL(mercury__compare_3_3));
}
Define_label(mercury__compare_3_0_i1);
{
	MR_succip = (Code *) MR_stackvar(2);
	save_registers();
	r2 = virtual_reg(MR_stackvar(1) + 1);
	decr_sp_pop_msg(2);
	proceed();
#endif
}
END_MODULE
void mercury_sys_init_call(void); /* suppress gcc warning */
void mercury_sys_init_call(void) {
	call_module();
}
