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
	num_in_args = MR_field(0, closure, 0); /* number of input args */
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
		virtual_reg(i) = MR_field(0, closure, i + 1); /* copy args */
	}

	restore_registers();

	tailcall((Code *) MR_field(0, closure, 1), LABEL(do_call_det_closure));
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
*/

Define_entry(mercury__unify_2_0);
{
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP
	Word		type_info;
	MR_TypeCtorInfo	type_ctor_info;
	Word		x, y;

	type_info = r1;
	x = r2;
	y = r3;

unify_start:
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO((Word *) type_info);

#ifdef	MR_CTOR_REP_STATS
	MR_ctor_rep_unify[type_ctor_info->type_ctor_rep]++;
#endif

	switch (type_ctor_info->type_ctor_rep) {

			/*
			** For notag and equiv types, we should probably
			** set type_info to refer to the appropriate type
			** and then goto start. However, the code that we
			** have here now works, even though it could be
			** improved.
			*/

		case MR_TYPECTOR_REP_DU:
		case MR_TYPECTOR_REP_ARRAY:
		case MR_TYPECTOR_REP_NOTAG:
		case MR_TYPECTOR_REP_EQUIV:
		case MR_TYPECTOR_REP_EQUIV_VAR:

			/*
			** We call the type-specific unify routine as
			** `UnifyPred(...ArgTypeInfos..., X, Y)' is semidet.
			** The ArgTypeInfo arguments are input, and are passed
			** in r1, r2, ... rN. The X and Y arguments are also
			** input, and are passed in rN+1 and rN+2.
			** The success indication is output in r1.
			**
			** We specialize the case where the type_ctor arity 
			** is zero, since in this case we don't need the loop.
			** We could also specialize other arities; 1 and 2
			** may be worthwhile.
			*/

			if (type_ctor_info->arity == 0) {
				r1 = x;
				r2 = y;
			}
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_1
			else if (type_ctor_info->arity == 1) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = x;
				r3 = y;
			}
#endif
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_2
			else if (type_ctor_info->arity == 2) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = args_base[2];
				r3 = x;
				r4 = y;
			}
#endif
			else {
				int	i;
				int	type_arity;
				Word	*args_base;

				type_arity = type_ctor_info->arity;
				args_base = (Word *) type_info;
				save_registers();

				/* CompPred(...ArgTypeInfos..., Res, X, Y) * */
				for (i = 1; i <= type_arity; i++) {
					virtual_reg(i) = args_base[i];
				}
				virtual_reg(type_arity + 1) = x;
				virtual_reg(type_arity + 2) = y;

				restore_registers();
			}

			tailcall(type_ctor_info->unify_pred,
				LABEL(mercury__unify_2_0));

		case MR_TYPECTOR_REP_ENUM:
		case MR_TYPECTOR_REP_INT:
		case MR_TYPECTOR_REP_CHAR:
			r1 = ((Integer) x == (Integer) y);
			proceed();

		case MR_TYPECTOR_REP_FLOAT:
			{
				Float	fx, fy;

				fx = word_to_float(x);
				fy = word_to_float(y);
				r1 = (fx == fy);
				proceed();
			}

		case MR_TYPECTOR_REP_STRING:
			r1 = (strcmp((char *) x, (char *) y) == 0);
			proceed();

		case MR_TYPECTOR_REP_UNIV:
			{
				Word	type_info_x, type_info_y;
				int	result;

				/* First compare the type_infos */
				type_info_x = MR_field(MR_mktag(0), x,
						UNIV_OFFSET_FOR_TYPEINFO);
				type_info_y = MR_field(MR_mktag(0), y,
						UNIV_OFFSET_FOR_TYPEINFO);
				save_transient_registers();
				result = MR_compare_type_info(
						type_info_x, type_info_y);
				restore_transient_registers();
				if (result != MR_COMPARE_EQUAL) {
					r1 = FALSE;
					proceed();
				}

				/*
				** If the types are the same, then unify
				** the unwrapped args.
				*/

				type_info = type_info_x;
				x = MR_field(MR_mktag(0), x,
						UNIV_OFFSET_FOR_DATA);
				y = MR_field(MR_mktag(0), y,
						UNIV_OFFSET_FOR_DATA);
				goto unify_start;
			}

		case MR_TYPECTOR_REP_C_POINTER:
			r1 = ((void *) x == (void *) y);
			proceed();

		case MR_TYPECTOR_REP_TYPEINFO:
			{
				int	result;

				save_transient_registers();
				result = MR_compare_type_info(x, y);
				restore_transient_registers();
				r1 = (result == MR_COMPARE_EQUAL);
				proceed();
			}

		case MR_TYPECTOR_REP_VOID:
			fatal_error("attempt to unify terms of type `void'");

		case MR_TYPECTOR_REP_PRED:
			fatal_error("attempt to unify higher-order terms");

		case MR_TYPECTOR_REP_TYPECLASSINFO:
			fatal_error("attempt to unify typeclass_infos");

		case MR_TYPECTOR_REP_UNKNOWN:
			fatal_error("attempt to unify terms of unknown type");

		default:
			fatal_error("attempt to unify terms "
					"of unknown representation");
	}
#else
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

	type_ctor_info = MR_field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		unify_pred = (Code *) MR_field(0, type_info,
					OFFSET_FOR_UNIFY_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = MR_field(0, type_ctor_info, OFFSET_FOR_COUNT);
		unify_pred = (Code *) MR_field(0, type_ctor_info,
				OFFSET_FOR_UNIFY_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `UnifyPred(...ArgTypeInfos..., X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = MR_field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;
	virtual_reg(type_arity + 2) = y;

	restore_registers();

	tailcall(unify_pred, LABEL(mercury__unify_2_0));
#endif
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
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP
	Word		type_info;
	MR_TypeCtorInfo	type_ctor_info;
	Word		x;

	type_info = r1;
	x = r2;

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO((Word *) type_info);

#ifdef	MR_CTOR_REP_STATS
	MR_ctor_rep_index[type_ctor_info->type_ctor_rep]++;
#endif

	switch (type_ctor_info->type_ctor_rep) {

			/*
			** For notag and equiv types, we should probably
			** set type_info to refer to the appropriate type
			** and then goto start. However, the code that we
			** have here now works, even though it could be
			** improved.
			*/

		case MR_TYPECTOR_REP_DU:
		case MR_TYPECTOR_REP_NOTAG:
		case MR_TYPECTOR_REP_EQUIV:
		case MR_TYPECTOR_REP_EQUIV_VAR:
		case MR_TYPECTOR_REP_ARRAY:

			/*
			** We call the type-specific unify routine as
			** `IndexPred(...ArgTypeInfos..., X, Index)' is det.
			** The ArgTypeInfo arguments are input, and are passed
			** in r1, r2, ... rN. The X argument is also input
			** and is passed in rN+1. The index is output in r1.
			**
			** We specialize the case where the type_ctor arity 
			** is zero, since in this case we don't need the loop.
			** We could also specialize other arities; 1 and 2
			** may be worthwhile.
			*/

			if (type_ctor_info->arity == 0) {
				r1 = x;
			}
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_1
			else if (type_ctor_info->arity == 1) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = x;
			}
#endif
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_2
			else if (type_ctor_info->arity == 2) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = args_base[2];
				r3 = x;
			}
#endif
			else {
				int	i;
				int	type_arity;
				Word	*args_base;

				type_arity = type_ctor_info->arity;
				args_base = (Word *) type_info;
				save_registers();

				/* IndexPred(...ArgTypeInfos..., X, Index) */
				for (i = 1; i <= type_arity; i++) {
					virtual_reg(i) = args_base[i];
				}
				virtual_reg(type_arity + 1) = x;

				restore_registers();
			}

			tailcall(type_ctor_info->index_pred,
				LABEL(mercury__index_2_0));

		case MR_TYPECTOR_REP_ENUM:
		case MR_TYPECTOR_REP_INT:
		case MR_TYPECTOR_REP_CHAR:
			r1 = x;
			proceed();

		case MR_TYPECTOR_REP_FLOAT:
			fatal_error("attempt to index a float");

		case MR_TYPECTOR_REP_STRING:
			fatal_error("attempt to index a string");
			proceed();

		case MR_TYPECTOR_REP_UNIV:
			fatal_error("attempt to index a term of type `univ'");

		case MR_TYPECTOR_REP_C_POINTER:
			r1 = x;
			proceed();

		case MR_TYPECTOR_REP_TYPEINFO:
			fatal_error("attempt to index a type_info");

		case MR_TYPECTOR_REP_VOID:
			fatal_error("attempt to index a term of type `void'");

		case MR_TYPECTOR_REP_PRED:
			fatal_error("attempt to index a higher-order term");

		case MR_TYPECTOR_REP_TYPECLASSINFO:
			fatal_error("attempt to index a typeclass_info");

		case MR_TYPECTOR_REP_UNKNOWN:
			fatal_error("attempt to index a term of unknown type");

		default:
			fatal_error("attempt to index a term "
					"of unknown representation");
	}
#else
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
	type_ctor_info = MR_field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		index_pred = (Code *) MR_field(0, type_info,
					OFFSET_FOR_INDEX_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = MR_field(0, type_ctor_info, OFFSET_FOR_COUNT);
		index_pred = (Code *) MR_field(0, type_ctor_info,
				OFFSET_FOR_INDEX_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `IndexPred(...ArgTypeInfos..., X, Index)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = MR_field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

	tailcall(index_pred, LABEL(mercury__index_2_0));
#endif
}

/*
** mercury__compare_3_3 is called as `compare(TypeInfo, Result, X, Y)'
** in the mode `compare(in, out, in, in) is det'.
**
** (The additional entry points replace either or both "in"s with "ui"s.)
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
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP
	Word		type_info;
	MR_TypeCtorInfo	type_ctor_info;
	Word		x, y;

	type_info = r1;
	x = r2;
	y = r3;

compare_start:
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO((Word *) type_info);

#ifdef	MR_CTOR_REP_STATS
	MR_ctor_rep_compare[type_ctor_info->type_ctor_rep]++;
#endif

	switch (type_ctor_info->type_ctor_rep) {

			/*
			** For notag and equiv types, we should probably
			** set type_info to refer to the appropriate type
			** and then goto start. However, the code that we
			** have here now works, even though it could be
			** improved.
			*/

		case MR_TYPECTOR_REP_DU:
		case MR_TYPECTOR_REP_NOTAG:
		case MR_TYPECTOR_REP_EQUIV:
		case MR_TYPECTOR_REP_EQUIV_VAR:
		case MR_TYPECTOR_REP_ARRAY:

			/*
			** We call the type-specific compare routine as
			** `CompPred(...ArgTypeInfos..., Result, X, Y)' is det.
			** The ArgTypeInfo arguments are input, and are passed
			** in r1, r2, ... rN. The X and Y arguments are also
			** input, and are passed in rN+1 and rN+2.
			** The Result argument is output in r1.
			**
			** We specialize the case where the type_ctor arity 
			** is zero, since in this case we don't need the loop.
			** We could also specialize other arities; 1 and 2
			** may be worthwhile.
			*/

			if (type_ctor_info->arity == 0) {
				r1 = x;
				r2 = y;
			}
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_1
			else if (type_ctor_info->arity == 1) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = x;
				r3 = y;
			}
#endif
#ifdef	MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_2
			else if (type_ctor_info->arity == 2) {
				Word	*args_base;

				args_base = (Word *) type_info;
				r1 = args_base[1];
				r2 = args_base[2];
				r3 = x;
				r4 = y;
			}
#endif
			else {
				int	i;
				int	type_arity;
				Word	*args_base;

				type_arity = type_ctor_info->arity;
				args_base = (Word *) type_info;
				save_registers();

				/* CompPred(...ArgTypeInfos..., Res, X, Y) * */
				for (i = 1; i <= type_arity; i++) {
					virtual_reg(i) = args_base[i];
				}
				virtual_reg(type_arity + 1) = x;
				virtual_reg(type_arity + 2) = y;

				restore_registers();
			}

			tailcall(type_ctor_info->compare_pred,
				LABEL(mercury__compare_3_3));

		case MR_TYPECTOR_REP_ENUM:
		case MR_TYPECTOR_REP_INT:
		case MR_TYPECTOR_REP_CHAR:
			if ((Integer) x == (Integer) y) {
				r1 = MR_COMPARE_EQUAL;
			} else if ((Integer) x < (Integer) y) {
				r1 = MR_COMPARE_LESS;
			} else {
				r1 = MR_COMPARE_GREATER;
			}

			proceed();

		case MR_TYPECTOR_REP_FLOAT:
			{
				Float	fx, fy;

				fx = word_to_float(x);
				fy = word_to_float(y);
				if (fx == fy) {
					r1 = MR_COMPARE_EQUAL;
				} else if (fx < fy) {
					r1 = MR_COMPARE_LESS;
				} else {
					r1 = MR_COMPARE_GREATER;
				}

				proceed();
			}

		case MR_TYPECTOR_REP_STRING:
			{
				int	result;

				result = strcmp((char *) x, (char *) y);
				if (result == 0) {
					r1 = MR_COMPARE_EQUAL;
				} else if (result < 0) {
					r1 = MR_COMPARE_LESS;
				} else {
					r1 = MR_COMPARE_GREATER;
				}

				proceed();
			}

		case MR_TYPECTOR_REP_UNIV:
			{
				Word	type_info_x, type_info_y;
				int	result;

				/* First compare the type_infos */
				type_info_x = MR_field(MR_mktag(0), x,
						UNIV_OFFSET_FOR_TYPEINFO);
				type_info_y = MR_field(MR_mktag(0), y,
						UNIV_OFFSET_FOR_TYPEINFO);
				save_transient_registers();
				result = MR_compare_type_info(
						type_info_x, type_info_y);
				restore_transient_registers();
				if (result != MR_COMPARE_EQUAL) {
					r1 = result;
					proceed();
				}

				/*
				** If the types are the same, then compare
				** the unwrapped args.
				*/

				type_info = type_info_x;
				x = MR_field(MR_mktag(0), x,
						UNIV_OFFSET_FOR_DATA);
				y = MR_field(MR_mktag(0), y,
						UNIV_OFFSET_FOR_DATA);
				goto compare_start;
			}

		case MR_TYPECTOR_REP_C_POINTER:
			if ((void *) x == (void *) y) {
				r1 = MR_COMPARE_EQUAL;
			} else if ((void *) x < (void *) y) {
				r1 = MR_COMPARE_LESS;
			} else {
				r1 = MR_COMPARE_GREATER;
			}

			proceed();

		case MR_TYPECTOR_REP_TYPEINFO:
			{
				int	result;

				save_transient_registers();
				result = MR_compare_type_info(x, y);
				restore_transient_registers();
				r1 = result;
				proceed();
			}

		case MR_TYPECTOR_REP_VOID:
			fatal_error("attempt to compare terms of type `void'");

		case MR_TYPECTOR_REP_PRED:
			fatal_error("attempt to compare higher-order terms");

		case MR_TYPECTOR_REP_TYPECLASSINFO:
			fatal_error("attempt to compare typeclass_infos");

		case MR_TYPECTOR_REP_UNKNOWN:
			fatal_error("attempt to compare terms of unknown type");

		default:
			fatal_error("attempt to compare terms "
					"of unknown representation");
	}
#else
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

	type_ctor_info = MR_field(0, type_info, 0);
	if (type_ctor_info == 0) {
		type_arity = 0;
		compare_pred = (Code *) MR_field(0, type_info,
						OFFSET_FOR_COMPARE_PRED);
		/* args_base will not be needed */
		args_base = 0; /* just to supress a gcc warning */
	} else {
		type_arity = MR_field(0, type_ctor_info, OFFSET_FOR_COUNT);
		compare_pred = (Code *) MR_field(0, type_ctor_info,
				OFFSET_FOR_COMPARE_PRED);
		args_base = type_info;
	}

	save_registers();

	/* we call `ComparePred(...ArgTypeInfos..., Result, X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) = MR_field(0, args_base, i);
	}
	virtual_reg(type_arity + 1) = x;
	virtual_reg(type_arity + 2) = y;

	restore_registers();

	tailcall(compare_pred, LABEL(mercury__compare_3_3));
#endif
}
END_MODULE

void mercury_sys_init_call(void); /* suppress gcc warning */
void mercury_sys_init_call(void) {
	call_module();
}
