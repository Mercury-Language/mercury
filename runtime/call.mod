/*
 * The call.mod module provides the functionality for doing higher order
 * calls. The following constraints apply to higher order calls:
 *
 *	It is assumed that closures contain no output arguments.
 *
 *	Predicates called from a closure must have all their input
 *	arguments before all their output arguments.
 *
 *	Closures contain only input arguments.
 *
 *	Invocations of call/N consist of a closure giving ALL the
 *	input arguments followed by N-1 output arguments which are
 *	returned in registers 1 -- (N-1) or 2 -- N for semidet preds.
 *
 *	The input arguments to do_call_[semidet_]closure are the closure
 *	in r1, and the number of output arguments to expect in r2.
 */

#include "imp.h"

BEGIN_MODULE(call_module)

BEGIN_CODE

do_call_closure:
{
	Word closure;
	int i,num_in_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	push(r2); /* The number of output args to unpack */
	push(num_in_args); /* The number of input args */
	push(succip);
	save_registers();
	for(i=1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */
	restore_registers();
	call(field(0, closure, 1), LABEL(do_closure_return));
}
do_closure_return:
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
	int i,num_in_args;

	closure = r1; /* The closure */
	num_in_args = field(0, closure, 0); /* number of input args */
	push(r2); /* The number of output args to unpack */
	push(num_in_args); /* The number of input args */
	push(succip);
	save_registers();
	for(i=1; i <= num_in_args; i++) 
		virtual_reg(i) = field(0, closure, i+1); /* copy args */
	restore_registers();
	call(field(0, closure, 1), LABEL(do_semidet_closure_return));
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



END_MODULE
