/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** The call.mod module provides much of the functionality for doing
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

#include "imp.h"
#include "type_info.h"

BEGIN_MODULE(call_module)

BEGIN_CODE

do_call_det_closure:
{
	Word	closure;
	int	i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* number of immediate input args */

	push(r3); /* The number of output args to unpack */
	push(num_in_args + num_extra_args); /* The number of input args */
	push(succip);

	save_registers();

	if (num_in_args < 3) {
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 3) {
		for (i = num_extra_args; i>0; i--) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 3 */

	for (i = 1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */

	restore_registers();

	call((Code *) field(0, closure, 1), LABEL(det_closure_return),
		LABEL(do_call_det_closure));
}
det_closure_return:
{
	int	i, num_in_args, num_out_args;

	succip = pop(); /* restore succip */
	num_in_args = pop(); /* restore the input arg counter */
	num_out_args = pop(); /* restore the ouput arg counter */

#ifdef	COMPACT_ARGS
#else
	save_registers();

	for (i = 1; i <= num_out_args; i++)
		virtual_reg(i) = virtual_reg(i+num_in_args);

	restore_registers();
#endif

	proceed();
}

do_call_semidet_closure:
{
	Word	closure;
	int	i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* the number of immediate input args */

	push(r3); /* The number of output args to unpack */
	push(num_in_args + num_extra_args); /* The number of input args */
	push(succip);

	save_registers();

#ifdef	COMPACT_ARGS
	if (num_in_args < 3) {
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 3) {
		for (i = num_extra_args; i>0; i--) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 3 */

	for (i = 1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */
#else
	if (num_in_args < 2) {
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(1+i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 2) {
		for (i = num_extra_args; i>0; i--) {
			virtual_reg(1+i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 2 */

	for (i = 1; i <= num_in_args; i++) 
		virtual_reg(i+1) = field(0, closure, i+1); /* copy args */
#endif

	restore_registers();

	call((Code *) field(0, closure, 1), LABEL(semidet_closure_return),
		LABEL(do_call_semidet_closure));
}
semidet_closure_return:
{
	int	i, num_in_args, num_out_args;

	succip = pop(); /* restore succip */
	num_in_args = pop(); /* restore the input arg counter */
	num_out_args = pop(); /* restore the ouput arg counter */

#ifdef	COMPACT_ARGS
#else
	save_registers();

	for (i = 1; i <= num_out_args; i++)
		virtual_reg(i+1) = virtual_reg(i+1+num_in_args);

	restore_registers();
#endif

	proceed();
}

do_call_nondet_closure:
{
	Word	closure;
	int	i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* number of immediate input args */

	mkframe("do_call_nondet_closure", 2, ENTRY(do_fail));
	framevar(0) = r3;	/* The number of output args to unpack */
	framevar(1) = num_in_args + num_extra_args;
				/* The number of input args */

	save_registers();

	if (num_in_args < 3) {
		for (i = 1; i <= num_extra_args; i++) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 3) {
		for (i = num_extra_args; i > 0; i--) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 3 */

	for (i = 1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */

	restore_registers();

	call((Code *) field(0, closure, 1), LABEL(nondet_closure_return),
		LABEL(do_call_nondet_closure));
}
nondet_closure_return:
{
	int	i, num_in_args, num_out_args;

	num_in_args = framevar(1); /* restore the input arg counter */
	num_out_args = framevar(0); /* restore the ouput arg counter */

#ifdef	COMPACT_ARGS
#else
	save_registers();

	for (i = 1; i <= num_out_args; i++)
		virtual_reg(i) = virtual_reg(i+num_in_args);

	restore_registers();
#endif

	succeed();
}

/*
** mercury__unify_2_0 is called as `unify(TypeInfo, X, Y)'
** in the mode `unify(in, in, in) is semidet'.
**
** With the normal parameter passing convention, the inputs are in the
** registers r2, r3 and r4. With the compact parameter passing convention,
** the inputs are in the registers r1, r2 and r3.
**
** The only output is the success/failure indication,
** which goes in r1 with both calling conventions.
**
** We call the type-specific unification routine as
** `UnifyPred(...TypeInfos..., X, Y)' is semidet, with all arguments input.
** Again r1 will hold the success/failure continuation; the input arguments
** start either in r1 or r2 depending on the argument passing convention.
*/

mercury__unify_2_0:
{
	Word	type_info;
	Code	*unify_pred;
	Word	x, y;
	int	i, type_arity;

	type_info = mercury__unify__typeinfo;
	x = mercury__unify__x;
	y = mercury__unify__y;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	unify_pred = (Code *) field(0, type_info, OFFSET_FOR_UNIFY_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `UnifyPred(...TypeInfos..., X, Y)' */
	/* virtual_reg(1) will hold the success/failure indication */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i + mercury__unify__offset) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
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
** With the normal parameter passing convention, the output is in r3;
** with the compact parameter passing convention, the output is in r1.
**
** We call the type-specific index routine as
** `IndexPred(...TypeInfos..., X, Index)' is det.
** The TypeInfo and X arguments are input, and are passed in r1, r2, ... rN
** with both conventions. The Index argument is output; it is returned in
** r1 with the compact convention and rN+1 with the normal convention.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the normal convention, we can't.
*/

mercury__index_2_0:
{
	Word	type_info;
	Code	*index_pred;
	Word	x;
	int	i, type_arity;

	/* we get called as `index(TypeInfo, X, Index)' */
	/* in the mode `index(in, in, out) is det'. */
	type_info = r1;
	x = r2;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	index_pred = (Code *) field(0, type_info, OFFSET_FOR_INDEX_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `IndexPred(...TypeInfos..., X, Index)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(index_pred, LABEL(mercury__index_2_0));
#else
	push(succip);
	push(type_arity);
	call(index_pred, LABEL(mercury__index_2_0_i1), 
		LABEL(mercury__index_2_0));
#endif
}
/*
** Since mod2c declares this label, we must define it,
** even though it is not needed with COMPACT_ARGS.
*/
mercury__index_2_0_i1:
{
#ifdef	COMPACT_ARGS
	fatal_error("mercury__index_2_0_i1 reached in COMPACT_ARGS mode");
#else
	int	type_arity;

	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
#endif
}

/*
** mercury__compare_3_3 is called as `compare(TypeInfo, Result, X, Y)'
** in the mode `compare(in, out, in, in) is det'.
**
** (The additional entry points replace either or both "in"s with "ui"s.)
**
** With the normal parameter passing convention, the inputs are in r1,
** r3 and r4, while the output is in r2.
**
** With the compact parameter passing convention, the inputs are in r1,
** r2 and r3, while the output is in r1.
**
** We call the type-specific compare routine as
** `ComparePred(...TypeInfos..., Result, X, Y)' is det.
** The TypeInfo arguments are input, and are passed in r1, r2, ... rN
** with both conventions. The X and Y arguments are also input, but are passed
** in different registers (rN+2 and rN+3 with the normal convention and rN+1
** and rN+2 with the compact convention). The Index argument is output; it is
** returned in ** r1 with the compact convention and rN+1 with the normal
** convention.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the normal convention, we can't.
*/

mercury__compare_3_0:
mercury__compare_3_1:
mercury__compare_3_2:
mercury__compare_3_3:
{
	Word	type_info;
	Code	*compare_pred;
	Word	x, y;
	int	i, type_arity;

	/* we get called as `compare(TypeInfo, Result, X, Y)' */
	/* in the mode `compare(in, out, in, in) is det'. */
	type_info = mercury__compare__typeinfo;
	x = mercury__compare__x;
	y = mercury__compare__y;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	compare_pred = (Code *) field(0, type_info, OFFSET_FOR_COMPARE_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `ComparePred(...TypeInfos..., Result, X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + mercury__compare__offset + 1) = x;
	virtual_reg(type_arity + mercury__compare__offset + 2) = y;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(compare_pred, LABEL(mercury__compare_3_0));
#else
	push(succip);
	push(type_arity);
	call(compare_pred, LABEL(mercury__compare_3_0_i1),
		LABEL(mercury__compare_3_0));
#endif
}
/*
** Since mod2c declares this label, we must define it,
** even though it is not needed with COMPACT_ARGS.
*/
mercury__compare_3_0_i1:
{
#ifdef	COMPACT_ARGS
	fatal_error("mercury__compare_3_0_i1 reached in COMPACT_ARGS mode");
#else
	int	type_arity;

	type_arity = pop();
	succip = pop();
	save_registers();
	r2 = virtual_reg(type_arity + 1);
	proceed();
#endif
}

/*
** mercury__term_to_type_2_0 is called as `term_to_type(TypeInfo, Term, X)'
** in the mode `term_to_type(in, in, out) is semidet'.
**
** With the normal parameter convention, the inputs are in r2 and r3,
** the success/failure indication in r1, and the output in r4.
**
** With the compact parameter convention, the inputs are in r1 and r2,
** the success/failure indication in r1, and the output in r2.
**
** We call the type-specific compare routine as
** `TermToTypePred(...TypeInfos..., Term, X)' is semidet.
**
** With the normal parameter convention, the inputs are in r2, ... rN+2,
** the success/failure indication in r1, and the output in rN+3.
**
** With the compact parameter convention, the inputs are in r1, ... rN+1,
** the success/failure indication in r1, and the output in r2.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the normal convention, we can't.
*/

mercury__term_to_type_2_0:
{
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else

	Word	type_info;
	Code	*term_to_type_pred;
	Word	term;
	int	i, type_arity;

	type_info = mercury__term_to_type__typeinfo;
	term = mercury__term_to_type__term;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	term_to_type_pred =
		(Code *) field(0, type_info, OFFSET_FOR_TERM_TO_TYPE_PRED);
		/* address of the term_to_type pred for this type */

	save_registers();

	/* we call 'TermToTypePred(...TypeInfos..., Term, X)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i + mercury__term_to_type__offset) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + mercury__term_to_type__offset + 1) = term;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(term_to_type_pred, LABEL(mercury__term_to_type_2_0));
#else
	push(succip);
	push(type_arity);
	call(term_to_type_pred, LABEL(mercury__term_to_type_2_0_i1),
		LABEL(mercury__term_to_type_2_0));
#endif
#endif
}
/*
** Since mod2c declares this label, we must define it,
** even though it is not needed with COMPACT_ARGS.
*/
mercury__term_to_type_2_0_i1:
{
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else
#ifdef	COMPACT_ARGS
	fatal_error("mercury__term_to_type_2_0_i1 reached in COMPACT_ARGS mode");
#else
	/* r1 already contains the truth result of the semidet pred */
	/* mercury__term_to_type_2_0 so r1 does not have to be updated. */

	int	type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r4 = virtual_reg(type_arity + 3);
	proceed();
#endif
#endif
}

/*
** mercury__type_to_term_2_0 is called as `type_to_term(TypeInfo, X, Term)'
** in the mode `type_to_term(in, in, out) is det'.
**
** With both conventions, the inputs are in r1 and r2.
** With the normal parameter convention, the output is in r3;
** with the compact parameter convention, the output is in r1.
**
** We call the type-specific compare routine as
** `TypeToTermPred(...TypeInfos..., X, Term)' is det.
**
** With both conventions, the inputs are in r1, ... rN.
** With the normal parameter convention, the output is in rN+1;
** with the compact parameter convention, the output is in r1.
**
** With the compact convention, we can make the call to the type-specific
** routine a tail call, and we do so. With the normal convention, we can't.
*/

mercury__type_to_term_2_0:
{
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else

	Word	type_info;
	Code	*type_to_term_pred;
	Word	x;
	int	i, type_arity;

	/* we get called as 'type_to_term(TypeInfo, X, Term)' */
	/* in the mode 'type_to_term(in, in, out) is det'. */
	type_info = r1;
	x = r2;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	type_to_term_pred =
		(Code *) field(0, type_info, OFFSET_FOR_TYPE_TO_TERM_PRED);
		/* address of the type_to_term pred for this type */

	save_registers();

	/* we call 'TypeToTermPred(...TypeInfos..., X, Term)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 1) = x;

	restore_registers();

#ifdef	COMPACT_ARGS
	tailcall(type_to_term_pred, LABEL(mercury__type_to_term_2_0));
#else
	push(succip);
	push(type_arity);
	call(type_to_term_pred, LABEL(mercury__type_to_term_2_0_i1),
		LABEL(mercury__type_to_term_2_0));
#endif
#endif
}
/*
** Since mod2c declares this label, we must define it,
** even though it is not needed with COMPACT_ARGS.
*/
mercury__type_to_term_2_0_i1:
{
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else
#ifdef	COMPACT_ARGS
	fatal_error("mercury__type_to_term_2_0_i1 reached in COMPACT_ARGS mode");
#else
	int	type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
#endif
#endif
}

END_MODULE
