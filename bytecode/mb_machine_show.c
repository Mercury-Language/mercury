
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_machine_show.c,v 1.1 2001-01-24 07:42:26 lpcam Exp $
*/

/* Imports */
#include	<stdio.h>
#include	<assert.h>

#include	"mb_stack.h"
#include	"mb_disasm.h"
#include	"mb_machine_show.h"

/* Exported definitions */


void		MB_show_state(MB_Machine_State* ms, FILE* fp);
void		MB_show_call(MB_Machine_State* ms, FILE* fp);

/* Local declarations */

static char
rcs_id[]	= "$Id: mb_machine_show.c,v 1.1 2001-01-24 07:42:26 lpcam Exp $";

#define NREGS	5
#define NSTACK	10
typedef struct {
	char reg_name[NREGS][10+1];
	char reg[NREGS][8+1];

	char succip[9+1];
	char succip_str[25+1];
	
	char detsp[8+1];
	char det_name[NSTACK][10+1];
	char det[NSTACK][8+1];
	char det_str[NSTACK][16+1];

	char nondetsp[8+1];
	char nondet_name[NSTACK][10+1];
	char nondet[NSTACK][8+1];
	char nondet_str[NSTACK][10+1];
} MB_View_Vars;	/* structure containing variable names & values for output */

static void		pad_space(char* buffer, MB_Word len);
static MB_View_Vars	vars_state(MB_Machine_State* ms);

/* appends spaces to the end of a string so that a buffer is filled (includes null) */
static void
pad_space(char* buffer, MB_Word len)
{
	if (buffer == NULL && len == 0) return;
	
	assert(buffer != NULL);
	
	/* find the null terminator */
	while (len > 0) {
		if (*buffer == 0) break;
		buffer++;
		len--;
	}

	while (len > 1) {
		buffer++[0] = ' ';
		len--;
	}

	*buffer = 0;
}

/* Returns a structure containing formatted strings with
** various state variables in (so it is easier to print
** multiple columns on stdout)
*/

static MB_View_Vars
vars_state(MB_Machine_State* ms)
{
	MB_View_Vars	vars;
	MB_Word		ip = MB_ip_get(ms);
	MB_Bytecode	cur_proc = MB_code_get_proc(ms, ip);

	/* display the succip */
	{
		MB_Word succip = MB_succip_get(ms);
		MB_Bytecode succ_pred = MB_code_get_pred(ms, succip);
		if (succip == MB_CODE_INVALID_ADR) {
			snprintf(vars.succip, sizeof(vars.succip),
					"(invalid)");
			snprintf(vars.succip_str, sizeof(vars.succip_str),
					" ");
		} else {
			snprintf(vars.succip, sizeof(vars.succip),
					"%08x",
					(int)succip);
			snprintf(vars.succip_str, sizeof(vars.succip_str),
					"(%s/%d)",
					succ_pred.opt.enter_pred.pred_name,
					(int)succ_pred.opt.enter_pred.pred_arity);
		}
	}

	/* display the register variables */
	{
		MB_Word j;

		for (j = 0; j < NREGS; j++) {
			snprintf(vars.reg_name[j], sizeof(vars.reg_name[j]),
					"reg[%02x]",
					(int)j);
			snprintf(vars.reg[j], sizeof(vars.reg[j]),
					"%08x",
					(int)MB_reg_get(ms, j));
		}
	}

	/* display the det stack */
	{
		MB_Word j;

		snprintf(vars.detsp, sizeof(vars.detsp),
				"%08x",
				(int)MB_stack_size(&ms->det.stack));
		/* display only the local variables on the stack */
		if (MB_code_get_id(ms, ip) != MB_BC_enter_proc) {
			for (j = 0;
				(j < cur_proc.opt.enter_proc.list_length) &&
					(j < NSTACK);
				j++)
			{
				snprintf(vars.det_name[j], sizeof(vars.det_name[j]),
					"detstack[%1x]",
					(int)j);
				snprintf(vars.det[j], sizeof(vars.det[j]),
					"%08x",
					(int)MB_var_get(ms, j));
				snprintf(vars.det_str[j], sizeof(vars.det_name[j]),
					"%s",
					cur_proc.opt.enter_proc.var_info_list[j]);
			}
		} else {
			j = 0;
		}
		
		/* fill the rest with blanks*/
		for (; j < NSTACK; j++) {
			snprintf(vars.det_name[j],	sizeof(vars.det_name[j]), " ");
			snprintf(vars.det[j],		sizeof(vars.det[j]), " ");
			snprintf(vars.det_str[j],	sizeof(vars.det_name[j]), " ");
		}
	}
	/* display the nondet stack */
	{
		MB_Word j;

		snprintf(vars.nondetsp, sizeof(vars.nondetsp),
				"%08x",
				ms->nondet.curfr);
		for (j = 0;
			(j < NSTACK) &&
				((ms->nondet.curfr+j) < MB_stack_size(&ms->nondet.stack));
			j++)
		{
			snprintf(vars.nondet_name[j], sizeof(vars.nondet_name[j]),
				"nondet[%1x]",
				(int)j);

			snprintf(vars.nondet[j], sizeof(vars.nondet[j]),
				"%08x",
				(int)MB_frame_get(ms, j));

			snprintf(vars.nondet_str[j], sizeof(vars.nondet_name[j]),
				"%s",
				(	(j == MB_FRAME_PREVFR) ? "prevfr" :
					(j == MB_FRAME_REDOIP) ? "redoip" :
					(j == MB_FRAME_REDOFR) ? "redofr" :
					(j == MB_FRAME_SUCCIP) ? "[succip?]" :
					(j == MB_FRAME_SUCCFR) ? "[succfr?]" :
					" ")
				);
		}
		/* fill the rest with blanks */
		for (; j < NSTACK; j++) {
			snprintf(vars.nondet_name[j],	sizeof(vars.nondet_name[j]), " ");
			snprintf(vars.nondet[j],	sizeof(vars.nondet[j]), " ");
			snprintf(vars.nondet_str[j],	sizeof(vars.nondet_name[j]), " ");
		}
	}
	/* pad with spaces */
	{
		#define PAD(x)	pad_space(vars.##x, sizeof(vars.##x))
		MB_Word i;
		
		for (i = 0; i < NREGS; i++) {
			PAD(reg_name[i]);
			PAD(reg[i]);
		}

		PAD(succip);
		PAD(succip_str);
		
		PAD(detsp);
		PAD(nondetsp);
		for (i = 0; i < NSTACK; i++) {
			PAD(det_name[i]);
			PAD(det[i]);
			PAD(det_str[i]);
			PAD(nondet_name[i]);
			PAD(nondet[i]);
			PAD(nondet_str[i]);
		}
	}
	return vars;
}

/* Display the current state of the machine */
void
MB_show_state(MB_Machine_State* ms, FILE* fp)
{
	char buffer[78];
	MB_Word ip = MB_ip_get(ms);

	/* Work out what predicate & proc we are in */
	MB_Bytecode cur_pred = MB_code_get_pred(ms, ip);
	MB_Bytecode cur_proc = MB_code_get_proc(ms, ip);

	/* Show the call stack */
	MB_show_call(ms, fp);

	fprintf(fp, "----------------------------------------------------------------------------\n");
	if (ip >= MB_code_size(ms) || ip < 0) {
		fprintf(fp, "   Invalid execution address\n");
		return;
	}

	/* Show what predicate we are in */
	MB_str_bytecode(cur_pred, buffer, sizeof(buffer), 0);
	fprintf(fp, "%s\n", buffer);

	MB_str_bytecode(cur_proc, buffer, sizeof(buffer), 1);
	fprintf(fp, "%s\n", buffer);

	fprintf(fp, "\n");

	/* show the surrounding lines of code */
	MB_listing(ms, fp, ip-2, ip+4);

	fprintf(fp, "\n");

	/* Print variables */
	{
		MB_View_Vars vars = vars_state(ms);
		MB_Word j;

		fprintf(fp, " succip      %s %s\n", vars.succip, vars.succip_str);

		for (j = 0; j < NREGS; j++) {
			fprintf(fp, " %s  %s \n",
				vars.reg_name[j],
				vars.reg[j]
				);
		}
		

		fprintf(fp, " det.sp      %s		         curfr      %s\n",
				vars.detsp, vars.nondetsp);
		for (j = 0; j < NSTACK; j++) {
			fprintf(fp, " %s %s %s    %s %s %s\n",
				vars.det_name[j],
				vars.det[j],
				vars.det_str[j],
				vars.nondet_name[j],
				vars.nondet[j],
				vars.nondet_str[j]);
		}
	}
	fprintf(fp, "\n");
} /* MB_show_state */

/* Display the call stack of the machine */
void
MB_show_call(MB_Machine_State* ms, FILE* fp)
{
	char buffer[76];
	MB_Word i;
	MB_Word num_calls = MB_stack_size(&ms->call.stack);
	fprintf(fp, "Call stack: \n");
	for (i = 0; i < num_calls; i++) {
		MB_Bytecode bc = MB_code_get_pred(ms, MB_stack_peek(&ms->call.stack, i));
		MB_str_bytecode(bc, buffer, sizeof(buffer), 0);
		fprintf(fp, "%2x %s\n", i, buffer);

		bc = MB_code_get(ms, MB_stack_peek(&ms->call.stack, i));
		MB_str_bytecode(bc, buffer, sizeof(buffer), 0);
		fprintf(fp, "    %s\n", buffer);
	}
	
}


