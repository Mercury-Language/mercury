/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: machine.h,v 1.6 1997-07-27 14:59:23 fjh Exp $
*/

#ifndef MB_MACHINE_H
#define	MB_MACHINE_H

#define	MB_MAX_REGISTERS	40

#define	MB_MAX_INT_STACK	10000
#define	MB_MAX_INT_HEAP		10000
#define	MB_MAX_CODE		10000

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
	MB_Address;

typedef struct MB_Machine_struct {

	Word	reg[MB_MAX_REGISTERS];	/* Machine registers */

	Word	maxfr;		/* Top stack frame pointer */
	Word	curfr;		/* Current stack frame pointer */
	Word	ip;		/* Instruction pointer */

	Word	hp;	/* Heap pointer */
	Word	sp;	/* Stack pointer */

	Word	stack[MB_MAX_INT_STACK]; /* Interpreter stack */
	Word	heap[MB_MAX_INT_HEAP];	/* Interpreter heap */

	MB_Bytecode
		code[MB_MAX_CODE];	/* Data area for storing bytecodes */
} MB_Machine;


#endif	/* MB_MACHINE_H */
