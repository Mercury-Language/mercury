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

/* See polymorphism.nl for explanation of these offsets and how the
   type_info structure is layed out */

#define OFFSET_FOR_COUNT 0
#define OFFSET_FOR_UNIFY_PRED 1
#define OFFSET_FOR_INDEX_PRED 2
#define OFFSET_FOR_COMPARE_PRED 3
#define	OFFSET_FOR_READ_PRED 4
#define	OFFSET_FOR_WRITE_PRED 5
#define OFFSET_FOR_ARG_TYPE_INFOS 6

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
	call(index_pred, LABEL(mercury__index_3_0_i1), 
		LABEL(mercury__index_2_0));
}
mercury__index_3_0_i1:
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

mercury__read_2_0:
{
	Word type_info;
	Code *read_pred;
	Word term;
	int i, type_arity;

	/* we get called as 'read(TypeInfo, Term, X)' */
	/* in the mode 'read(in, in, out) is det'. */
	type_info = r1;
	term = r2;
	/* r3 will hold the result */
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	read_pred = (Code *) field(0, type_info, OFFSET_FOR_READ_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call 'ReadPred(...TypeInfos..., Term, X)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 1) = term;
	/* virtual_reg(type_arity + 2) will hold the result */

	restore_registers();

	push(succip);
	push(type_arity);
	call(read_pred, LABEL(mercury__read_3_0_i1),
		LABEL(mercury__index_2_0));
}
mercury__read_3_0_i1:
{
	int type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
}

mercury__write_2_0:
{
	Word type_info;
	Code *write_pred;
	Word x;
	int i, type_arity;

	/* we get called as 'write(TypeInfo, X, Term)' */
	/* in the mode 'write(in, in, out) is det'. */
	type_info = r1;
	x = r2;
	/* r3 will hold the result */
	type_arity = field(0, type_info, OFFSET_FOR_COUNT);
		/* number of type_info args */
	write_pred = (Code *) field(0, type_info, OFFSET_FOR_WRITE_PRED);
		/* address of the comparison pred for this type */

	save_registers();

	/* we call 'WritePred(...TypeInfos..., X, Term)' */
	for (i = 1; i <= type_arity; i++) {
		virtual_reg(i) =
			field(0, type_info, i - 1 + OFFSET_FOR_ARG_TYPE_INFOS);
	}
	virtual_reg(type_arity + 1) = x;
	/* virtual_reg(type_arity + 2) will hold the result */

	restore_registers();

	push(succip);
	push(type_arity);
	call(write_pred, LABEL(mercury__write_3_0_i1),
		LABEL(mercury__index_2_0));
}
mercury__write_3_0_i1:
{
	int type_arity;
	
	type_arity = pop();
	succip = pop();
	save_registers();
	r3 = virtual_reg(type_arity + 2);
	proceed();
}

END_MODULE
