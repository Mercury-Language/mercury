/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	"mercury_imp.h"

#include	"mb_machine.h"

#include	"mb_interface.h" 
#include	"mb_machine_def.h"
#include	"mb_module.h"

/* Exported definitions */

/* Local declarations */

/* Implementation */

/* Get the next instruction pointer */
MB_Bytecode_Addr
MB_ip_get(MB_Machine_State *ms)
{
	return ms->ip;
}

/* set the native code return address */
void
MB_native_return_set(MB_Machine_State *ms, MB_Native_Addr native_return)
{
#if 0
	MB_SAY("Returning to %p", native_return);
	MB_SAY("MB_succip is %p (%s)",
			MB_succip,
			(MB_succip == MB_native_get_return_temp_det()) ?
				"temp det return" :
			(MB_succip == MB_native_get_return_temp_nondet()) ?
				"temp nondet return" :
			(MB_succip == MB_native_get_return_nondet()) ?
				"nondet return" :
			(MB_succip == MB_native_get_return_det()) ?
				"det return" : "unknown");
#endif

	MB_ip_set(ms, MB_CODE_NATIVE_RETURN);
	ms->native_return = native_return;
}

MB_Native_Addr
MB_native_return_get(MB_Machine_State * ms)
{
	assert(ms->ip == MB_CODE_NATIVE_RETURN);

	return ms->native_return;
}

void
MB_ip_set(MB_Machine_State *ms, MB_Bytecode_Addr new_ip)
{
	if (MB_ip_special(new_ip)) {
		switch ((MB_Word) new_ip) {
			case (MB_Word) MB_CODE_DO_FAIL:
			case (MB_Word) MB_CODE_DO_REDO:
			case (MB_Word) MB_CODE_NATIVE_RETURN:
				ms->ip = new_ip;
				break;

			default:
				assert(FALSE);
		}
	} else {
		ms->ip = new_ip;
	}
}

/*
** Check which procedure we are in & set variable stack pointer appropriately
**
** If you don't call this after the ip switches to a new function
** then MB_var_get and MB_var_set will give incorrect results
**
** If det/semidet, set the machine state variable slot pointer to the det stack
** If nondet, set the machine state variable slot pointer to the nondet stack
*/
void
MB_proc_var_init(MB_Machine_State *ms)
{
	MB_Bytecode_Addr ip = MB_ip_get(ms);

	if (!MB_ip_normal(ip)) return;
	
	/* Check that we are actually in a procedure and not just entering one*/
	if (MB_code_get_id(ip) != MB_BC_enter_proc) {

		/* If we are, check the determinism & set vars as appropriate */
		ms->cur_proc.is_det = MB_code_get_det(ip);

		ms->cur_proc.var = (ms->cur_proc.is_det == MB_ISDET_YES)
			? &(MB_stackvar(0))
			: &(MB_framevar(0));
	}
}

MB_Bool
MB_proc_is_det(MB_Machine_State* ms)
{
	return ms->cur_proc.is_det == MB_ISDET_YES;
}

/*
** Get a variable from the appropriate mercury stack
**
** It knows which stack to use because you have of course already
** called MB_proc_var_init to set the current procedure's
** variable pointer to point to variable 0.
*/
MB_Word
MB_var_get(MB_Machine_State *ms, MB_Word idx)
{
	/*
	** idx is negative because variable 0 is the topmost and
	** higher number variables are below it on the stack
	*/
	return ms->cur_proc.var[-idx];
}

/* Set a variable on the mercury stack */
void
MB_var_set(MB_Machine_State *ms, MB_Word idx, MB_Word value)
{
	ms->cur_proc.var[-idx] = value;
}

/* Get/set the initial stack frame (see machine_def.h for use) */
MB_Word *
MB_initialstackframe_get(MB_Machine_State *ms)
{
	return ms->initial_stack;
}

void
MB_initialstackframe_set(MB_Machine_State *ms, MB_Word *frame)
{
	assert(ms->initial_stack == NULL);
	ms->initial_stack = frame;
}

/* Add a temporary stack frame with a redo_ip of do_fail
** Because the do_fail is native code, we can get by with
** a nativecode-type stack frame
*/
void
MB_frame_temp_push_do_fail(MB_Machine_State *ms)
{
	MB_Word *prevfr = MB_maxfr;

	if (ms->cur_proc.is_det) {

		MB_maxfr += MR_DET_TEMP_SIZE;

		MB_fr_prevfr(MB_maxfr) = prevfr;
		MB_fr_redoip(MB_maxfr) = (MB_Word) MB_native_get_do_fail();
		MB_fr_redofr(MB_maxfr) = (MB_Word) MB_curfr;
		MB_fr_detfr(MB_maxfr) = (MB_Word) MB_sp;
	} else {
		MB_maxfr += MR_NONDET_TEMP_SIZE;

		MB_fr_prevfr(MB_maxfr) = prevfr;
		MB_fr_redoip(MB_maxfr) = (MB_Word) MB_native_get_do_fail();
		MB_fr_redofr(MB_maxfr) = (MB_Word) MB_curfr;
	}
}

/* Add a temporary stack frame */
void
MB_frame_temp_push(MB_Machine_State *ms, MB_Bytecode_Addr redoip)
{
	MB_Word *prevfr = MB_maxfr;
	if (ms->cur_proc.is_det) {

		MB_maxfr += MB_FRAME_TEMP_DET_SIZE;

		MB_fr_prevfr(MB_maxfr) = prevfr;
		MB_fr_redoip(MB_maxfr) =
			(MB_Word) MB_native_get_return_temp_det();
		MB_fr_redofr(MB_maxfr) = (MB_Word) MB_curfr;
		MB_fr_detfr(MB_maxfr) = (MB_Word) MB_sp;
		MB_fr_temp_det_bcredoip(MB_maxfr) = (MB_Word) redoip;
		MB_fr_temp_det_bcinitfr(MB_maxfr) =
			(MB_Word) MB_initialstackframe_get(ms);
	} else {
		MB_maxfr += MB_FRAME_TEMP_NONDET_SIZE;

		MB_fr_prevfr(MB_maxfr) = prevfr;
		MB_fr_redoip(MB_maxfr) =
			(MB_Word) MB_native_get_return_temp_nondet();
		MB_fr_redofr(MB_maxfr) = (MB_Word) MB_curfr;
		MB_fr_temp_nondet_bcredoip(MB_maxfr) = (MB_Word) redoip;
	}
}

