
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

#include	"mercury_imp.h"

/* Imports */
#include	<stdio.h>
#include	<assert.h>

#include	"mb_stack.h"
#include	"mb_disasm.h"
#include	"mb_interface.h"
#include	"mb_machine.h"
#include	"mb_machine_def.h"
#include	"mb_machine_show.h"

/* Exported definitions */

void		MB_show_state(MB_Machine_State *ms, FILE *fp);

/* Local declarations */

#define NREGS	14
#define NSTACK	8


/* Display the current state of the machine */
#define LINE_LEN	78	/* XXX: make this adjustable */
void
MB_show_state(MB_Machine_State *ms, FILE *fp)
{
	char buffer[LINE_LEN];
	MB_Bytecode_Addr ip = MB_ip_get(ms);

	/* Work out what predicate & proc we are in */
	MB_Bytecode_Addr cur_pred;
	MB_Bytecode_Addr cur_proc;

	if (fp == NULL) return;

	fprintf(fp, "----------------------------------------"
			"------------------------------------\n");

	if (MB_code_range_clamp(ip) == ip) {
		/* Show what predicate we are in */
		cur_pred = MB_code_get_pred_addr(ip);
		cur_proc = MB_code_get_proc_addr(ip);

		MB_str_bytecode(ms, cur_pred, buffer, sizeof(buffer), 0);
		fprintf(fp, "%s\n", buffer);

		MB_str_bytecode(ms, cur_proc, buffer, sizeof(buffer), 1);
		fprintf(fp, "%s\n", buffer);

		fprintf(fp, "\n");

		fprintf(fp, "ip: %p\n", ip);

		/* show the surrounding lines of code */
		MB_listing(ms, fp, ip - 2, ip + 4, LINE_LEN);
	} else {
		if (MB_ip_special(ip)) {
			fprintf(fp, " Special execution address (%p)\n", ip);
		} else {
			fprintf(fp, "   Invalid execution address\n");
		}
	}

	fprintf(fp, "\n");

	{
		int i;

		/* Show the registers */
		for (i = 0; i < NREGS; i++) {
			int j = i / 2 +
				((i & 1) ? (NREGS / 2) : 0);
			fprintf(fp, "reg[%02d] = " MB_FMT_INT" ("MB_FMT_HEX ")       ",
					j, MB_reg(j), MB_reg(j));
			if (i & 1) {
				fprintf(fp, "\n");
			}
		}
		if (!(i & 1)) {
			fprintf(fp, "\n");
		}

		fprintf(fp, "\n");
		
		/* Show the machine state */
		fprintf(fp,	" succip     = " MB_FMT_HEX "                "
				" 0          = " MB_FMT_HEX "\n",
				(MB_Unsigned) MB_succip,
				(MB_Unsigned) 0);

		fprintf(fp,	" init_frame = " MB_FMT_HEX "                "
				" natv_retun = " MB_FMT_HEX "\n",
				(MB_Unsigned) ms->initial_stack,
				(MB_Unsigned) ms->native_return);

		/* Show the stack */
		fprintf(fp, "\n");

		fprintf(fp,	" sp         = " MB_FMT_HEX "                "
				" maxfr      = " MB_FMT_HEX "\n",
				(MB_Unsigned) MB_sp,
				(MB_Unsigned) MB_maxfr);
		for (i = 1; i < NSTACK; i++) {
			fprintf(fp,	"%cdet[%02d]    = " MB_FMT_INT
						" (" MB_FMT_HEX ")  "
					"%cnondet[%02d] = " MB_FMT_INT
						" (" MB_FMT_HEX ")\n",
				(&MB_stackitem(i) == ms->cur_proc.var) ?
					'>' : ' ',
				(int) i, MB_stackitem(i), MB_stackitem(i),
				(&MB_frameitem(i) == ms->cur_proc.var) ?
					'>' : ' ',
				(int) i, MB_frameitem(i), MB_frameitem(i));
		}
	}

	fprintf(fp, "\n");

} /* MB_show_state */

