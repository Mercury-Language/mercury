/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
 * The call.mod module provides the functionality for doing higher order
 * calls. The following constraints apply to higher order calls:
 *
 *	Predicates called from a closure must have all their input
 *	arguments before all their output arguments.
 *
 *	Closures contain only input arguments.
 *
 *	Invocations of call/(1+M+N) consist of a closure giving some of the
 *	input arguments followed M further input arguments, followed by
 *	N output arguments which are returned in registers 1 -- N
 *	or 2 -- N+1 for semidet preds.
 *
 *	The input arguments to do_call_[semidet_]closure are the closure
 *	in r1, the number of additional input arguments in r2, the number
 *	of output arguments to expect in r3, and the additional input arguments
 *	in r4..r(M+3).
 *	The output arguments are returned in registers r1, r2, ...
 *
 *	XXX doesn't work for calling nondet preds! (pushes/pops don't match)
 *
 */

#include "imp.h"
#include "type_info.h"

BEGIN_MODULE(call_module)

BEGIN_CODE

do_call_det_closure:
{
	Word closure;
	int i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* number of immediate input args */

	push(r3); /* The number of output args to unpack */
	push(num_in_args + num_extra_args); /* The number of input args */
	push(succip);

	save_registers();

	if (num_in_args < 3) {
		for (i=1; i<=num_extra_args;i++) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 3) {
		for (i=num_extra_args; i>0; i--) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 3 */

	for(i=1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */

	restore_registers();

	call((Code *)field(0, closure, 1), LABEL(do_det_closure_return),
		LABEL(do_call_det_closure));
}
do_det_closure_return:
{
	int i,num_in_args, num_out_args;

	succip = pop(); /* restore succip */
	num_in_args = pop(); /* restore the input arg counter */
	num_out_args = pop(); /* restore the ouput arg counter */

	save_registers();

	for (i=1; i<= num_out_args; i++)
		virtual_reg(i) = virtual_reg(i+num_in_args);

	restore_registers();

	proceed();
}

do_call_semidet_closure:
{
	Word closure;
	int i,num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* the number of immediate input args */

	push(r3); /* The number of output args to unpack */
	push(num_in_args + num_extra_args); /* The number of input args */
	push(succip);

	save_registers();

	if (num_in_args < 2) {
		for (i=1; i<=num_extra_args;i++) {
			virtual_reg(1+i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 2) {
		for (i=num_extra_args; i>0; i--) {
			virtual_reg(1+i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 2 */

	for(i=1; i <= num_in_args; i++) 
		virtual_reg(i+1) = field(0, closure, i+1); /* copy args */
	restore_registers();
	call((Code *)field(0, closure, 1), LABEL(do_semidet_closure_return),
		LABEL(do_call_semidet_closure));
}
do_semidet_closure_return:
{
	int i,num_in_args, num_out_args;

	succip = pop(); /* restore succip */
	num_in_args = pop(); /* restore the input arg counter */
	num_out_args = pop(); /* restore the ouput arg counter */

	save_registers();

	for (i=1; i<= num_out_args; i++)
		virtual_reg(i+1) = virtual_reg(i+num_in_args);

	restore_registers();

	proceed();
}

do_call_nondet_closure:
{
	Word closure;
	int i, num_in_args, num_extra_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	num_extra_args = r2; /* number of immediate input args */

	mkframe("do_call_nondet_closure", 2, ENTRY(do_fail));
	framevar(0) = r3;	/* The number of output args to unpack */
	framevar(1) = num_in_args + num_extra_args;
				/* The number of input args */

	save_registers();

	if (num_in_args < 3) {
		for (i=1; i<=num_extra_args;i++) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} else if (num_in_args > 3) {
		for (i=num_extra_args; i>0; i--) {
			virtual_reg(i+num_in_args) = virtual_reg(i+3);
		}
	} /* else do nothing because i == 3 */

	for(i=1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */

	restore_registers();

	call((Code *)field(0, closure, 1), LABEL(do_nondet_closure_return),
		LABEL(do_call_nondet_closure));
}
do_nondet_closure_return:
{
	int i,num_in_args, num_out_args;

	num_in_args = framevar(1); /* restore the input arg counter */
	num_out_args = framevar(0); /* restore the ouput arg counter */

	save_registers();

	for (i=1; i<= num_out_args; i++)
		virtual_reg(i) = virtual_reg(i+num_in_args);

	restore_registers();

	succeed();
}

mercury__index_2_0:
{
	Word type_info;
	Code* index_pred;
	Word x;
	int i, type_arity;

	/* we get called as `index(TypeInfo, X, Index)' */
	/* in the mode `index(in, in, out) is det'. */
	type_info = r1;
	x = r2;
	/* r3 will hold the result */
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	index_pred = (Code *)field(0, type_info, OFFSET_FOR_INDEX_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `IndexPred(...TypeInfos..., X, Index)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 1) = x;
	/* virtual_reg(type_arity + 2) will hold the result */

	restore_registers();

	push(succip);
	push(type_arity);
	call(index_pred, LABEL(mercury__index_2_0_i1), 
		LABEL(mercury__index_2_0));
}
mercury__index_2_0_i1:
{
	int type_arity;
	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
}

mercury__compare_3_0:
{
	Word type_info;
	Code *compare_pred;
	Word x, y;
	int i, type_arity;

	/* we get called as `compare(TypeInfo, Result, X, Y)' */
	/* in the mode `compare(in, out, in, in) is det'. */
	type_info = r1;
	/* r2 will hold the result */
	x = r3;
	y = r4;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	compare_pred = (Code *)field(0, type_info, OFFSET_FOR_COMPARE_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `ComparePred(...TypeInfos..., Result, X, Y)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	/* virtual_reg(type_arity + 1) will hold the result */
	virtual_reg(type_arity + 2) = x;
	virtual_reg(type_arity + 3) = y;

	restore_registers();

	push(succip);
	push(type_arity);
	call(compare_pred, LABEL(mercury__compare_3_0_i1),
		LABEL(mercury__compare_3_0));
}
mercury__compare_3_0_i1:
{
	int type_arity;
	type_arity = pop();
	succip = pop();
	save_registers();
	r2 = virtual_reg(type_arity + 1);
	proceed();
}

mercury__unify_2_0:
{
	Word type_info;
	Code *unify_pred;
	Word x, y;
	int i, type_arity;

	/* we get called as `unify(TypeInfo, X, Y)' */
	/* in the mode `unify(in, in, in) is semidet'. */
	/* r1 will hold the success/failure indication */
	type_info = r2;
	x = r3;
	y = r4;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	unify_pred = (Code *)field(0, type_info, OFFSET_FOR_UNIFY_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call `UnifyPred(...TypeInfos..., X, Y)' */
	/* virtual_reg(1) will hold the success/failure indication */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i + 1) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 2) = x;
	virtual_reg(type_arity + 3) = y;

	restore_registers();

	tailcall(unify_pred, LABEL(mercury__unify_2_0));
}


mercury__term_to_type_2_0:
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else
{
	/* we get called as 'term_to_type(TypeInfo, Term, X)' */
	/* in the mode 'term_to_type(in, in, out) is semidet'. */
	/* r1 will hold the success/failure indication */
	/* r2 holds the type_info for term */
	/* r3 holds the term */
	/* r4 will hold the result for X */

	Word type_info;
	Code *term_to_type_pred;
	Word term;
	int i, type_arity;

	type_info = r2;
	term = r3;
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	term_to_type_pred =
		(Code *) field(0, type_info, OFFSET_FOR_TERM_TO_TYPE_PRED);
		/* address of the term_to_type pred for this type */

	save_registers();

	/* we call 'TermToTypePred(...TypeInfos..., Term, X)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i + 1) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 2) = term;
	/* virtual_reg(type_arity + 3) will hold the result */

	restore_registers();

	push(succip);
	push(type_arity);
	call(term_to_type_pred, LABEL(mercury__term_to_type_2_0_i1),
		LABEL(mercury__term_to_type_2_0));
}
#endif
mercury__term_to_type_2_0_i1:
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
{
	/* r1 already contains the truth result of the semidet pred
	** mercury__term_to_type_2_0 so r1 does not have to be updated. */

	int type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r4 = virtual_reg(type_arity + 3);
	proceed();
}
#endif

mercury__type_to_term_2_0:
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
	fatal_error("type_to_term/2 and term_to_type/2 not implemented");
#else
{
	Word type_info;
	Code *type_to_term_pred;
	Word x;
	int i, type_arity;

	/* we get called as 'type_to_term(TypeInfo, X, Term)' */
	/* in the mode 'type_to_term(in, in, out) is det'. */
	type_info = r1;
	x = r2;
	/* r3 will hold the result */
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
	/* virtual_reg(type_arity + 2) will hold the result */

	restore_registers();

	push(succip);
	push(type_arity);
	call(type_to_term_pred, LABEL(mercury__type_to_term_2_0_i1),
		LABEL(mercury__type_to_term_2_0));
}
#endif
mercury__type_to_term_2_0_i1:
#if OFFSET_FOR_ARG_TYPE_INFOS != 6
{
	int type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
}

#endif

END_MODULE
