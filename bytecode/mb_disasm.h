
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

#ifndef MB_DISASM_H
#define	MB_DISASM_H

#include <stdio.h>
#include "mb_basetypes.h"
#include "mb_machine.h"

/*
** Fills a string buffer with the name of a bytecode (if buffer_len > 0)
** Returns the new indent level after the instruction
*/
int	MB_str_bytecode(MB_Bytecode_Addr addr, char *buffer, int buffer_len,
			int indent_level);

/*
** Displays a code listing (see source file for argument description)
** ms may be NULL if desired (you just won't get a little -> at the current ip)
*/
void	MB_listing(MB_Machine_State *ms, FILE *fp, MB_Bytecode_Addr start,
			MB_Bytecode_Addr end, MB_Word line_len);

/* XXX: width fields below may not be correct for all platforms */
/* printf Format string specifiers */
#define MB_FMT_INT	"%" MR_INTEGER_LENGTH_MODIFIER "d"
#define MB_FMT_INTWIDE	"% 11" MR_INTEGER_LENGTH_MODIFIER "d"
#define MB_FMT_HEX	"0x%08" MR_INTEGER_LENGTH_MODIFIER "x"

/* XXX: No string for 'MB_Short's. For now promote to int and use %d */
/* XXX: No string for 'MB_Byte's. For now promote to int and use %d */

#endif	/* MB_DISASM_H */

