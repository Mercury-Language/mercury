
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/


#include "mercury_imp.h"
#include "mercury_regs.h"
#include "mercury_trace.h"
#include "mercury_trace_tables.h"

#include "mb_interface.h"
#include "mb_module.h"
#include "mb_machine.h"

/* Exported definitions */

/* Local declarations */

/* Implementation */

MR_declare_entry(MB_native_return_det_stub);

MB_Native_Addr
MB_native_get_return_det(void)
{
	return (MB_Native_Addr) MR_ENTRY(MB_native_return_det_stub);
}


/* Search for the native code address of a procedure */
MB_Native_Addr
MB_code_find_proc_native(MB_CString_Const module, MB_CString_Const pred,
			MB_Word proc, MB_Word arity, MB_Bool is_func)
{
	MR_Matches_Info matches;
	MR_Proc_Spec spec;

	MR_register_all_modules_and_procs(stderr, TRUE);
	MB_SAY("\n");

	spec.MR_proc_module = module;
	spec.MR_proc_name = pred;
	spec.MR_proc_arity = arity;
	spec.MR_proc_mode = proc;
	spec.MR_proc_pf = (is_func) ? MR_FUNCTION : MR_PREDICATE;

	MB_SAY("Looking for procedures .... ");
	matches = MR_search_for_matching_procedures(&spec);

	{
		MB_Word i;
		for (i = 0; i < matches.match_proc_next; i++) {
			MB_SAY("Match %d: %s %s__%s/%d (%d)",
				i,
				(matches.match_procs[i]
					->MR_sle_proc_id.MR_proc_user
					.MR_user_pred_or_func == MR_PREDICATE) ?
					"pred" : "func",
				matches.match_procs[i]
					->MR_sle_proc_id.MR_proc_user
					.MR_user_def_module,
				matches.match_procs[i]
					->MR_sle_proc_id.MR_proc_user
					.MR_user_name,
				matches.match_procs[i]
					->MR_sle_proc_id.MR_proc_user
					.MR_user_arity,
				matches.match_procs[i]
					->MR_sle_proc_id.MR_proc_user
					.MR_user_mode
			);
		}
	}

	switch (matches.match_proc_next) {
		case 0:
			return NULL;
		case 1:
			{
				MB_Native_Addr addr = (MB_Native_Addr)
				matches.match_procs[0]->
					MR_sle_traversal.MR_trav_code_addr;
				MB_SAY("Adr %08x", addr);
			}
			return (MB_Native_Addr)matches.match_procs[0]->
				MR_sle_traversal.MR_trav_code_addr;
		default:
			MB_fatal("More than one native code entry found!");
			return NULL;
	}
}

/*
** A native code procedure wishes to call a deterministic bytecode procedure
*/

/*
** Needed to instantiate MB_Machine_State. Not #included above because nothing
** else in this module should need to know what is inside an MB_Machine_State
*/
#include "mb_machine_def.h"	

MB_Native_Addr
MB_bytecode_call_entry(MB_Call *bytecode_call)
{

	MB_Native_Addr		native_ip;
	MB_Bytecode_Addr	bc_ip;
	MB_SAY("Det stack is at %08x", MB_sp);

	MB_SAY("\n\nHello from bytecode_entry_det");

	if ((void *) bytecode_call->cached_ip == NULL) {
		bc_ip = MB_code_find_proc(bytecode_call->module_name,
			bytecode_call->pred_name,
			bytecode_call->proc_num,
			bytecode_call->arity,
			bytecode_call->is_func);
	} else {
		bc_ip = bytecode_call->cached_ip;
	}
	if (bc_ip == MB_CODE_INVALID_ADR) {
		MB_util_error("Attempting to call bytecode %s %s__%s/%d (%d):",
			bytecode_call->is_func ? "func" : "pred",
			bytecode_call->module_name,
			bytecode_call->pred_name,
			bytecode_call->arity,
			bytecode_call->proc_num);
		MB_fatal("Unable to find procedure\n"
			"(Is the native code and the bytecode consistent?)");
	}

	MB_SAY(" bytecode addr %08x", bc_ip);

	{
		/* Create a new machine and start executing */
		MB_Machine_State ms;
		MB_machine_create(&ms, bc_ip, NULL);

		MB_SAY("ZZZ ENTERING BYTECODE");
		MB_show_state(&ms, stderr);

		native_ip = MB_machine_exec(&ms);

		MB_SAY("ZZZ RETURNING TO NATIVE1");
		MB_show_state(&ms, stderr);
	}
	
	return native_ip;
}

/*
** This is the reentry point after a det bytecode procedure has called
** native code. See mb_interface.h for a description of how this occurs
*/
MB_Native_Addr
MB_bytecode_return_det(void)
{
	/* Get the bytecode reentry address */
	MB_Bytecode_Addr ip = (MB_Bytecode_Addr)
		MB_stackitem(MB_DETFRAME_INTERFACE_BC_SUCCIP);
	/* Get the initial stack frame */
	MB_Word *initial_frame = (MB_Word *)
		MB_stackitem(MB_DETFRAME_INTERFACE_INITIAL_FRAME);

	MB_Native_Addr ret_ip;

	MB_Machine_State ms;

	MB_decr_sp(MB_DETFRAME_INTERFACE_SIZE);

	MB_machine_create(&ms, ip, initial_frame);

	MB_SAY("ZZZ RETURNING TO BYTECODE");
	MB_show_state(&ms, stderr);

	ret_ip = MB_machine_exec(&ms);

	MB_SAY("ZZZ RETURNING TO NATIVE2");
	MB_show_state(&ms, stderr);

	return ret_ip;

}

