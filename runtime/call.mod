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
 */

#include "imp.h"

BEGIN_MODULE(call_module)

BEGIN_CODE

do_call_closure:
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
