/*
 *	$Id: disasm.h,v 1.2 1997-02-01 09:15:02 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
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
