/*
** Copyright (C) 2000-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains utility functions that can be used by any or all
** of the various kinds of Mercury debuggers.
**
** Author: zs.
*/

#include "mercury_imp.h"
#include "mercury_trace_util.h"
#include "mercury_file.h"

#include <ctype.h>

void
MR_c_file_to_mercury_file(FILE *c_file, MercuryFile *mercury_file)
{
	MR_mercuryfile_init(c_file, 1, mercury_file);
}

bool
MR_trace_is_number(const char *word, int *value)
{
	if (MR_isdigit(*word)) {
		*value = *word - '0';
		word++;
		while (MR_isdigit(*word)) {
			*value = (*value * 10) + *word - '0';
			word++;
		}

		if (*word == '\0') {
			return TRUE;
		}
	}

	return FALSE;
}

void
MR_print_stack_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
	fprintf(fp, "sp = ");
	MR_print_detstackptr(fp, MR_saved_sp(saved_regs));
	fprintf(fp, "\ncurfr = ");
	MR_print_nondstackptr(fp, MR_saved_curfr(saved_regs));
	fprintf(fp, "\nmaxfr = ");
	MR_print_nondstackptr(fp, MR_saved_maxfr(saved_regs));
	fprintf(fp, "\n");
#endif
}

void
MR_print_heap_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_CONSERVATIVE_GC
	fprintf(fp, "hp = ");
	MR_print_heapptr(fp, MR_saved_hp(saved_regs));
	fprintf(fp, "\nsol_hp = ");
	MR_print_heapptr(fp, MR_saved_sol_hp(saved_regs));
	fprintf(fp, "\nmin_hp_rec = ");
	MR_print_heapptr(fp, MR_saved_min_hp_rec(saved_regs));
	fprintf(fp, "\nglobal_hp = ");
	MR_print_heapptr(fp, MR_saved_global_hp(saved_regs));
	fprintf(fp, "\n");
#endif
}

void
MR_print_tabling_regs(FILE *fp, MR_Word *saved_regs)
{
#ifdef MR_USE_MINIMAL_MODEL
	fprintf(fp, "gen_next = %ld\n", (long) MR_saved_gen_next(saved_regs));
	fprintf(fp, "cut_next = %ld\n", (long) MR_saved_cut_next(saved_regs));
#endif
}

void
MR_print_succip_reg(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
	fprintf(fp, "succip = ");
	MR_print_label(fp, MR_saved_succip(saved_regs));
	fprintf(fp, "\n");
#endif
}

void
MR_print_r_regs(FILE *fp, MR_Word *saved_regs)
{
#ifndef MR_HIGHLEVEL_CODE
	fprintf(fp, "r1 = %ld (%lx)\n",
		(long) MR_saved_reg(saved_regs, 1),
		(long) MR_saved_reg(saved_regs, 1));
	fprintf(fp, "r2 = %ld (%lx)\n",
		(long) MR_saved_reg(saved_regs, 2),
		(long) MR_saved_reg(saved_regs, 2));
	fprintf(fp, "r3 = %ld (%lx)\n",
		(long) MR_saved_reg(saved_regs, 3),
		(long) MR_saved_reg(saved_regs, 3));
	fprintf(fp, "r4 = %ld (%lx)\n",
		(long) MR_saved_reg(saved_regs, 4),
		(long) MR_saved_reg(saved_regs, 4));
	fprintf(fp, "r5 = %ld (%lx)\n",
		(long) MR_saved_reg(saved_regs, 5),
		(long) MR_saved_reg(saved_regs, 5));
#endif
}
