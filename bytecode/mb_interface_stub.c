/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Bytecode entry stubs
**
*/

/* System includes */
#include "mercury_imp.h"
#include "mb_interface.h"

MR_define_extern_entry(MB_native_return_det_stub);
MR_define_extern_entry(MB_native_return_temp_det_stub);
MR_define_extern_entry(MB_native_return_temp_nondet_stub);
MR_define_extern_entry(MB_native_return_nondet_stub);

MR_BEGIN_MODULE(mb_interface_stub)
	MR_init_entry_ai(MB_native_return_det_stub);
	MR_init_entry_ai(MB_native_return_temp_det_stub);
	MR_init_entry_ai(MB_native_return_temp_nondet_stub);
	MR_init_entry_ai(MB_native_return_nondet_stub);
MR_BEGIN_CODE

/* Define the return to deterministic bytecode stub after calling a det proc */
MR_define_entry(MB_native_return_det_stub);
	{
		MR_Code *ret_addr;
		MR_save_registers();
		ret_addr = MB_bytecode_return_det();
		MR_restore_registers();
		MR_GOTO(ret_addr);
	}

/*
** Define the return to deterministic bytecode through a temp nondet stack frame
** pushed by a det procedure
*/
MR_define_entry(MB_native_return_temp_det_stub);
	{
		MR_Code *ret_addr;
		MR_save_registers();
		ret_addr = MB_bytecode_return_temp_det();
		MR_restore_registers();
		MR_GOTO(ret_addr);
	}

/*
** Define the return to deterministic bytecode through a temp nondet stack frame
** pushed by a nondet procedure
*/
MR_define_entry(MB_native_return_temp_nondet_stub);
	{
		MR_Code *ret_addr;
		MR_save_registers();
		ret_addr = MB_bytecode_return_temp_nondet();
		MR_restore_registers();
		MR_GOTO(ret_addr);
	}

/*
** Define the return to deterministic bytecode through a temp nondet stack frame
** pushed by a nondet procedure
*/
MR_define_entry(MB_native_return_nondet_stub);
	{
		MR_Code *ret_addr;
		MR_save_registers();
		ret_addr = MB_bytecode_return_nondet();
		MR_restore_registers();
		MR_GOTO(ret_addr);
	}

MR_END_MODULE


