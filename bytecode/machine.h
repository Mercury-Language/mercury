/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: machine.h,v 1.3 1997-04-24 05:30:52 aet Exp $
*/

#if	! defined(MACHINE_H)
#define	MACHINE_H

#define	MAX_REGISTERS	40

#define	MAX_INT_STACK	10000
#define	MAX_INT_HEAP	10000
#define	MAX_CODE	10000

/*
**	XXX: Currently we store full bloated bytecodes in the
**	code area (text segment). This is extremely inefficient.
**	We should alter design a stripped-down streamlined `machine
**	code' that is a minimalist bytecode with all symbolic information
**	stripped out and and placed in tables (`data segment'), and all 
**	labels replaced with offsets into the text segment
*/

/*
** An Address is an index into code, heap, or stack area.
** This should be identical to a pointer since we will need
** to use the same heap as the compiled Mercury when we interface
** with compiled code.
*/
typedef Word
	Address;

typedef struct Machine {

	Word	reg[MAX_REGISTERS];	/* Machine registers */

	Word	maxfr;		/* Top stack frame pointer */
	Word	curfr;		/* Current stack frame pointer */
	Word	ip;		/* Instruction pointer */

	Word	hp;	/* Heap pointer */
	Word	sp;	/* Stack pointer */

	Word	stack[MAX_INT_STACK];	/* Interpreter stack */
	Word	heap[MAX_INT_HEAP];	/* Interpreter heap */

	Bytecode
		code[MAX_CODE];		/* Data area for storing bytecodes */
} Machine;


#endif	/* MACHINE_H */
