
/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_disasm.h,v 1.1 2001-01-24 07:42:23 lpcam Exp $
*/

#ifndef MB_DISASM_H
#define	MB_DISASM_H

#include <stdio.h>

#include "mb_bytecode.h"
#include "mb_machine.h"

/* Fills a string buffer with the name of a bytecode
** returns the new indent level after the instruction
*/
int
MB_str_bytecode(MB_Bytecode bc, char* buffer, int buffer_len, int indent_level);

/* displays a code listing (see source file for argument description) */
void
MB_listing(MB_Machine_State* ms, FILE* fp, MB_Word start, MB_Word end);

#endif	/* MB_DISASM_H */


