
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/


#include "mercury_imp.h"
#include "mercury_trace_tables.h"

#include "mb_interface.h"

#include "mb_disasm.h"
#include "mb_exec.h"
#include "mb_module.h"

/* Exported definitions */
MB_Native_Addr          MB_bytecode_call_entry(MB_Call *bytecode_call);
MB_Native_Addr          MB_bytecode_return_det(void);
MB_Native_Addr          MB_bytecode_return_temp_det(void);
MB_Native_Addr          MB_bytecode_return_temp_nondet(void);
MB_Native_Addr          MB_bytecode_return_nondet(void);
MB_Native_Addr          MB_native_get_return_det(void);
MB_Native_Addr          MB_native_get_return_temp_det(void);
MB_Native_Addr          MB_native_get_return_temp_nondet(void);
MB_Native_Addr          MB_native_get_return_nondet(void);
MB_Native_Addr          MB_native_get_do_redo(void);
MB_Native_Addr          MB_native_get_do_fail(void);
MB_Native_Addr		MB_native_get_unify_2(void);
MB_Native_Addr		MB_native_get_compare_3(void);
MB_Native_Addr          MB_code_find_proc_native(MB_CString_Const module_name,
					MB_CString_Const pred_name,
					MB_Word mode_num, MB_Word arity,
					MB_Bool is_func);
MB_Native_Addr          MB_code_find_proc_native(MB_CString_Const module_name,
					MB_CString_Const pred_name,
					MB_Word mode_num, MB_Word arity,
					MB_Bool is_func);
MR_TypeCtorInfo         MB_type_find_ctor_info_guaranteed(
					MB_CString_Const module_name,
					MB_CString_Const type_name,
					MB_Word type_arity);
/* Local declarations */

/* Implementation */

/* Reentry stubs */
MR_declare_entry(MB_native_return_det_stub);
MR_declare_entry(MB_native_return_temp_det_stub);
MR_declare_entry(MB_native_return_temp_nondet_stub);
MR_declare_entry(MB_native_return_nondet_stub);

MB_Native_Addr
MB_native_get_return_det(void)
{
	return (MB_Native_Addr) MR_ENTRY(MB_native_return_det_stub);
}

MB_Native_Addr
MB_native_get_return_temp_det(void)
{
	return (MB_Native_Addr) MR_ENTRY(MB_native_return_temp_det_stub);
}

MB_Native_Addr
MB_native_get_return_temp_nondet(void)
{
	return (MB_Native_Addr) MR_ENTRY(MB_native_return_temp_nondet_stub);
}

MB_Native_Addr
MB_native_get_return_nondet(void)
{
	return (MB_Native_Addr) MR_ENTRY(MB_native_return_nondet_stub);
}

MB_Native_Addr
MB_native_get_do_redo(void)
{
	return (MB_Native_Addr) MR_ENTRY(do_redo);
}

MB_Native_Addr
MB_native_get_do_fail(void)
{
	return (MB_Native_Addr) MR_ENTRY(do_fail);
}


MR_declare_entry(mercury__unify_2_0);
MR_declare_entry(mercury__compare_3_0);

MB_Native_Addr
MB_native_get_unify_2(void)
{
	return (MB_Native_Addr) MR_ENTRY(mercury__unify_2_0); 
}

MB_Native_Addr
MB_native_get_compare_3(void)
{
	return (MB_Native_Addr) MR_ENTRY(mercury__compare_3_0); 
}

/* Search for the native code address of a procedure */
MB_Native_Addr
MB_code_find_proc_native(MB_CString_Const module_name,
		MB_CString_Const pred_name, MB_Word mode_num,
		MB_Word arity, MB_Bool is_func)
{
	MR_Matches_Info	matches;
	MR_Proc_Spec	spec;

	MR_register_all_modules_and_procs(stderr, FALSE);

	spec.MR_proc_module = module_name;
	spec.MR_proc_name = pred_name;
	spec.MR_proc_arity = arity;
	spec.MR_proc_mode = mode_num;
	spec.MR_proc_pf = (is_func) ? MR_FUNCTION : MR_PREDICATE;

	matches = MR_search_for_matching_procedures(&spec);

	switch (matches.match_proc_next) {
		case 0:
			if (MB_str_cmp("builtin", module_name) == 0) {
				if (mode_num == 0 && is_func == FALSE) {
					if (arity == 2 &&
						MB_str_cmp("unify", pred_name) == 0)
				       	{
						return MB_native_get_unify_2();
					} else if (arity == 3 && 
						MB_str_cmp("compare", pred_name) == 0)
					{
						return MB_native_get_compare_3();
					}

				}
			}
			return NULL;
		case 1:
			{
				MB_Native_Addr addr = (MB_Native_Addr)
				matches.match_procs[0]->
					MR_sle_traversal.MR_trav_code_addr;
			}
			return (MB_Native_Addr)matches.match_procs[0]->
				MR_sle_traversal.MR_trav_code_addr;
		default:
			MB_fatal("More than one native code entry found!");
			return NULL;
	}
}

/*
** Looks up type constructor info.
** Guaranteed to succeed (aborts program if it fails)
*/
MR_TypeCtorInfo
MB_type_find_ctor_info_guaranteed(MB_CString_Const module_name,
		MB_CString_Const type_name, MB_Word type_arity)
{
	MR_TypeCtorInfo ret_val;
	MR_do_init_modules_type_tables();

	ret_val = MR_lookup_type_ctor_info(module_name, type_name, type_arity);

	if (ret_val == NULL) {
		MB_util_error("Type %s__%s/" MB_FMT_INT " not found",
				module_name, type_name, type_arity);
		MB_fatal("Unable to find type ctor info");
	}

	return ret_val;
}

/*
** A native code procedure wishes to call a deterministic bytecode procedure
*/
MB_Native_Addr
MB_bytecode_call_entry(MB_Call *bytecode_call)
{
	MB_Bytecode_Addr	bc_ip;

	if ((void *) bytecode_call->cached_ip == NULL) {
		bc_ip = MB_code_find_proc(bytecode_call->module_name,
			bytecode_call->pred_name,
			bytecode_call->mode_num,
			bytecode_call->arity,
			bytecode_call->is_func);
	} else {
		bc_ip = bytecode_call->cached_ip;
	}

	if (bc_ip == MB_CODE_INVALID_ADR) {
		MB_util_error("Attempting to call bytecode %s %s__%s/"
					MB_FMT_INT " (" MB_FMT_INT "):",
			bytecode_call->is_func ? "func" : "pred",
			bytecode_call->module_name,
			bytecode_call->pred_name,
			bytecode_call->arity,
			bytecode_call->mode_num);
		MB_fatal("Unable to find procedure\n"
			"(Is the native code and the bytecode consistent?)");
	}

	return MB_machine_exec(bc_ip, NULL);
}

/*
** Reentry point det bytecode procedure has called native code.
*/
MB_Native_Addr
MB_bytecode_return_det(void)
{
	/* Get the bytecode reentry address */
	MB_Bytecode_Addr ip = (MB_Bytecode_Addr)
		MB_stackitem(MB_DETFRAME_INTERFACE_BCRETIP);
	/* Get the initial stack frame */
	MB_Word *initial_frame = (MB_Word *)
		MB_stackitem(MB_DETFRAME_INTERFACE_BCINITFR);

	/* Remove interface frame */
	MB_decr_sp(MB_DETFRAME_INTERFACE_SIZE);

	/* Execute */
	return MB_machine_exec(ip, initial_frame);
}

/*
** Reentry point after a redo/fail was executed and used a temp nondet stack
** frame generated by a det procedure
*/
MB_Native_Addr
MB_bytecode_return_temp_det(void)
{
	/* Get the bytecode reentry address */
	MB_Bytecode_Addr ip = (MB_Bytecode_Addr)
		MB_fr_temp_det_bcredoip(MB_maxfr);
	/* Get the initial stack frame */
	MB_Word *initial_frame = (MB_Word *)
		MB_fr_temp_det_bcinitfr(MB_maxfr);

	/* execute */
	return MB_machine_exec(ip, initial_frame);
}

/*
** Reentry point after a redo/fail was executed and used a temp nondet stack
** frame generated by a nondet procedure
*/
MB_Native_Addr
MB_bytecode_return_temp_nondet(void)
{
	/* Get the bytecode reentry address */
	MB_Bytecode_Addr ip = (MB_Bytecode_Addr)
		MB_fr_temp_nondet_bcredoip(MB_maxfr);

	/* Get the initial stack frame */
	MB_Word *initial_frame = (MB_Word *)
		MB_fr_bcinitfr(MB_curfr);

	/* execute */
	return MB_machine_exec(ip, initial_frame);
}

/*
** Reentry point after a call by a nondet procedure
*/
MB_Native_Addr
MB_bytecode_return_nondet(void)
{
	/* Get the bytecode reentry address */
	MB_Bytecode_Addr ip = (MB_Bytecode_Addr)
		MB_fr_bcretip(MB_curfr);
	/* Get the initial stack frame */
	MB_Word *initial_frame = (MB_Word *)
		MB_fr_bcinitfr(MB_curfr);

	/* execute */
	return MB_machine_exec(ip, initial_frame);
}

