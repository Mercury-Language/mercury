/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: disasm.h,v 1.4 1997-04-24 05:30:46 aet Exp $
*/

#if	! defined(DISASM_H)
#define	DISASM_H


/*
 *	Disassemble a Mercury bytecode file.
 *
 *	fp points to the beginning of a Mercury bytecode file. 
 *	The disassembler output is directed to stdout.
 *	XXX: Maybe we should be able to specify the output file pointer?
 */
void
disassemble(FILE* fp);

#endif	/* DISASM_H */
