/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: disasm.h,v 1.6 1997-04-26 05:56:52 fjh Exp $
*/

#ifndef MB_DISASM_H
#define	MB_DISASM_H


/*
 *	Disassemble a Mercury bytecode file.
 *
 *	`fp' points to the beginning of a Mercury bytecode file. 
 *	The disassembler output is directed to stdout.
 *	XXX: Maybe we should be able to specify the output file pointer?
 */
void
MB_disassemble(FILE* fp);

#endif	/* MB_DISASM_H */
