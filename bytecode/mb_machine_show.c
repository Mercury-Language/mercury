
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	"mercury_imp.h"

#include	"mb_machine_show.h"

#include	"mb_disasm.h"
#include	"mb_interface.h"
#include	"mb_machine_def.h"
#include	"mb_module.h"

/* Exported definitions */
void		MB_show_state(MB_Machine_State *ms, FILE *fp);

/* Local declarations */
static void show_regs(MB_Machine_State *ms, FILE *fp);
static void show_stack(MB_Machine_State *ms, FILE *fp);


/* Number of registers to display */
#define NREGS	8
/* Number of entries in each stack to display */
#define NSTACK	27
/* Maximum line length for disassembly */
#define LINE_LEN	78	/* XXX: make this adjustable */

/* Display the current state of the machine */
static void
show_regs(MB_Machine_State *ms, FILE *fp)
{
	int i;

	/*
	** Show the registers in two columns, like so:
	** r0  r4
	** r1  r5
	** r2  r6
	** r3  r7
	*/
	for (i = 0; i < NREGS; i++) {
		int j = i / 2 +
			((i & 1) ? (NREGS / 2) : 0);
		fprintf(fp,
			"reg[%02d] = " MB_FMT_INTWIDE
			" ("MB_FMT_HEX ")       ",
			j, MB_reg(j), MB_reg(j));
		if (i & 1) {
			fprintf(fp, "\n");
		}
	}
	if (!(i & 1)) {
		fprintf(fp, "\n");
	}

}

static char *
get_var_name(MB_Machine_State* ms, MB_Bytecode_Arg *cur_proc, MB_Word *var)
{
	static char *var_name = NULL;
	if (var_name == NULL) {
		var_name = MB_str_dup("tmp000");
	}

	if (cur_proc != NULL) {
		MB_Integer offset = ms->cur_proc.var - var;

		if (offset >= 0) {
			MB_Integer temp_count = cur_proc->enter_proc.temp_count;
			MB_Integer var_count = cur_proc->enter_proc.list_length;
			if (offset < temp_count) {
				sprintf(var_name+3, "%03d", (int) offset);
				return var_name;
			}  else if ((offset -= temp_count) < var_count) {
				return cur_proc->enter_proc.var_info[offset];
			}
			
		}
	}

	return NULL;
}
/* Display the top stack entries */
static void
show_stack(MB_Machine_State *ms, FILE *fp)
{
	int		i;
	int		frame_index;
	MB_Word		*thisfr;
	char		det_sym[NSTACK];
	char		nondet_sym[NSTACK][2];
	MB_Bytecode_Addr ip = MB_ip_get(ms);
	MB_Bytecode_Arg	*cur_proc = MB_ip_normal(ip)
					? MB_code_get_arg(
						MB_code_get_proc_addr(ip))
					: NULL;

	for (i = 0; i < NSTACK; i++) {
		/* Indicate variable list */
		det_sym[i] =
			(&MB_stackitem(i + 1) == ms->cur_proc.var) ? '>'
			: ' ';
		
		/* Indicate variable list */
		if (&MB_frameitem(MB_maxfr, i) == ms->cur_proc.var) {
			nondet_sym[i][0] = '[';
			nondet_sym[i][1] = '>';
		} else {
			nondet_sym[i][0] = ' ';
			nondet_sym[i][1] = ' ';
		}
	}

	/* Indicate stack frames */
	thisfr = MB_maxfr;
	frame_index = 0;
	i = 0;
	do {
		while (i < frame_index) {
			if (nondet_sym[i][0] == ' ') {
				nondet_sym[i][0] = '|';
			}
			i++;
		}
		i++;

		if ((MB_Word *) MB_fr_prevfr(thisfr) != NULL) {
			nondet_sym[frame_index][0] = '/';
			nondet_sym[frame_index][1] = '=';
		}

		if (frame_index > 0) {
			nondet_sym[frame_index-1][0] = '[';
			nondet_sym[frame_index-1][1] = '_';
		}

		thisfr = (MB_Word *) MB_fr_prevfr(thisfr);
		frame_index = (&MB_frameitem(MB_maxfr, 0) - thisfr);

	} while ((frame_index < NSTACK) && (thisfr != NULL));


	/* Show the stack */
	fprintf(fp, "\n");

	fprintf(fp,	" sp         = " MB_FMT_HEX "                "
			" maxfr      = " MB_FMT_HEX "\n",
			(MB_Unsigned) MB_sp,
			(MB_Unsigned) MB_maxfr);

	for (i = 0; i < NSTACK; i++) {
		char		*var_name;
		var_name = get_var_name(ms, cur_proc, &MB_stackitem(i + 1));
		if (var_name == NULL) {
			var_name = get_var_name(ms, cur_proc,
						&MB_frameitem(MB_maxfr, i));
		}
		if (var_name == NULL) {
			var_name = "";
		}

		fprintf(fp,
			"%cdet[%02d]    = " MB_FMT_INTWIDE " ("MB_FMT_HEX ")  "
			"%c%c%p=" MB_FMT_INTWIDE " (" MB_FMT_HEX ") %s\n",
				
				det_sym[i],
				(int) i + 1,
				MB_stackitem(i + 1),
				MB_stackitem(i + 1),

				nondet_sym[i][0],
				nondet_sym[i][1],
				&(MB_frameitem(MB_maxfr, i)),
				MB_frameitem(MB_maxfr, i),
				MB_frameitem(MB_maxfr, i),
				var_name
			);
	}
}

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

		MB_str_bytecode(cur_pred, buffer, sizeof(buffer), 0);
		fprintf(fp, "%s\n", buffer);

		MB_str_bytecode(cur_proc, buffer, sizeof(buffer), 1);
		fprintf(fp, "%s\n", buffer);

		fprintf(fp, "\n");

		fprintf(fp, "ip: %p\n", ip);

		/* show the surrounding lines of code */
		MB_listing(ms, fp, ip - 2, ip + 4, LINE_LEN);
	} else {
		if (MB_ip_special(ip)) {
			fprintf(fp, " Special execution address (%p)\n", ip);
		} else {
			fprintf(fp, " Invalid execution address (%p)\n", ip);
		}
	}

	fprintf(fp, "\n");

	show_regs(ms, fp);

	fprintf(fp, "\n");

	/* Show the machine state */
	fprintf(fp,	" succip     = " MB_FMT_HEX "                "
			" var_ptr    = " MB_FMT_HEX "\n",
			(MB_Unsigned) MB_succip,
			(MB_Unsigned) ms->cur_proc.var);

	fprintf(fp,	" init_frame = " MB_FMT_HEX "                "
			" natv_retun = " MB_FMT_HEX "\n",
			(MB_Unsigned) ms->initial_stack,
			(MB_Unsigned) ms->native_return);

	fprintf(fp,	" cur_frame  = " MB_FMT_HEX "                "
			"            = " MB_FMT_HEX "\n",
			(MB_Unsigned) MB_curfr,
			(MB_Unsigned) 0);

	show_stack(ms, fp);

	fprintf(fp, "\n");

} /* MB_show_state */

