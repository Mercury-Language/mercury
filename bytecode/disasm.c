/*
 *	$Id: disasm.c,v 1.2 1997-01-30 01:17:21 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>

#include	<util.h>
#include	<bytecode.h>
#include	<disasm.h>

/* Local declarations */

/* Implementation */

/* XXX: disassemble is still a stub */
void
disassemble(FILE* fp)
{
	int	byte_count = 0;
	ushort	bytecode_version_number = 0;

	/* Read two-byte version number */
	if (read_bytecode_version_number(fp, &bytecode_version_number))
	{
		printf("Bytecode Version number: %d\n",
			bytecode_version_number
		);
	}
	else
	{
		fatal("Failed to read bytecode version number");
	}

	for (;;)
	{
		Byte	cur_byte;
		CString	cur_bytecode_name;

		if (read_byte(fp, &cur_byte))
		{
			byte_count++;
		}
		else
		{
			break;	/* No more bytecodes to read */
		}

		if (bytecode_to_name(cur_byte, &cur_bytecode_name))
		{
			printf("%-10.5d %s", byte_count, cur_bytecode_name);	
		}
		else
		{
			/* No such bytecode instruction */
			/* XXX: Need better error handling here */
			fatal("No such bytecode");
		}

		switch (cur_byte)
		{
		case BC_enter_pred:	/* CString, int */
			{
			CString	byte_pred_id;
			Short	short_arg;

			if (read_cstring(fp, &byte_pred_id) && 
				read_ushort(fp, &short_arg)
			)
			{
				printf(" \"%s\" %d", byte_pred_id, short_arg);
			}
			else
			{
				fatal("Bad args to enter_pred");
			}
			}
			break;
		case BC_endof_pred:	/* No args */
			break;
		case BC_enter_proc:	/* byte byte short */
			{
			Byte	byte_proc_id;
			Byte	determinism;
			Short	list_len;

			if (read_byte(fp, &byte_proc_id) &&
				read_byte(fp, &determinism) &&
				read_ushort(fp, &list_len)
			)
			{
				
			}
			else
			{
				fatal("Bad args to enter_proc");
			}
			}
			break;
		case BC_endof_proc:
			break;
		case BC_label:
			break;
		case BC_enter_disjunction:
			break;
		case BC_endof_disjunction:
			break;
		case BC_enter_disjunct:
			break;
		case BC_endof_disjunct:
			break;
		case BC_enter_switch:
			break;
		case BC_endof_switch:
			break;
		case BC_enter_switch_arm:
			break;
		case BC_endof_switch_arm:
			break;
		case BC_enter_if:
			break;
		case BC_enter_then:
			break;
		case BC_endof_then:
			break;
		case BC_endof_if:
			break;
		case BC_enter_negation:
			break;
		case BC_endof_negation:
			break;
		case BC_enter_commit:
			break;
		case BC_endof_commit:
			break;
		case BC_assign:
			break;
		case BC_test:
			break;
		case BC_construct:
			break;
		case BC_deconstruct:
			break;
		case BC_complex_deconstruct:
			break;
		case BC_place_arg:
			break;
		case BC_call:
			break;
		case BC_pickup_arg:
			break;
		case BC_builtin_binop:
			break;
		case BC_builtin_unop:
			break;
		case BC_builtin_bintest:
			break;
		case BC_builtin_untest:
			break;
		case BC_context:
			break;
		case BC_not_supported:
			break;
		default:
			/* XXX: Provide better error */
			/* util_error("Unknown bytecode"); */
			break;
		} /* switch cur_byte */

		/* Print a newline at the end of each bytecode instruction */
		putchar('\n');

	} /* for(;;) */
} /* disassemble */

