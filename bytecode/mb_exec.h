/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Execution of bytecode
**
*/

#ifndef MB_EXEC_H
#define MB_EXEC_H

#include "mb_basetypes.h"

#include <stdio.h>
#include "mb_bytecode.h"
#include "mb_machine.h"

/*
** Execute a bytecode machine until native code invocation required.
** Returns address of native code to return to
*/
MB_Native_Addr	MB_machine_exec(MB_Bytecode_Addr new_ip,
				MB_Word *initial_stack);

#endif	/* MB_EXEC_H */

