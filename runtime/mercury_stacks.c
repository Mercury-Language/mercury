/*
** Copyright (C) 1998-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains code for printing statistics about stack frame sizes,
** and for manipulating the generator stack and the cut stack.
**
** The generator stack has one entry for each call to a minimal model tabled
** procedure that is (a) acting as the generator for its subgoal and (b) is
** in the active state. In systems such as XSB, each choice point has a flag
** saying whether it is an active generator or not, and if yes, where its
** subgoal's tabling information is stored. We achieve the same effect by 
** checking whether a nondet stack frame at a given offset has an entry in
** the generator stack, an approach that minimizes the performance impact
** of tabling on non-tabled procedures.
**
** The cut stack has one entry for each commit goal that execution has entered
** but not yet exited. Each commit stack entry has a list of all the generators
** that have been started inside the corresponding commit goal. When the commit
** goal is exited, it is possible that some of these generators are left
** incomplete; due to the commit, they will in fact never be completed.
** The purpose of the cut stack is to enable us to reset the call table
** entries of such generators to inactive.
**
** All the functions in this file that take MR_TrieNode arguments use
** only the subgoal member of the union.
*/

#include "mercury_imp.h"
#include "mercury_runtime_util.h"
#include <stdio.h>

#ifdef	MR_STACK_FRAME_STATS

#include "mercury_dword.h"

MR_Dword	MR_det_frame_count;
MR_Dword	MR_det_frame_total_size;
MR_Word		*MR_det_frame_max;
MR_Dword	MR_non_frame_count;
MR_Dword	MR_non_frame_total_size;
MR_Word		*MR_non_frame_max;

MR_uint_least32_t MR_old_low_tmp;

void
MR_init_stack_frame_stats(void)
{
	MR_zero_dword(MR_det_frame_count);
	MR_zero_dword(MR_det_frame_total_size);
	MR_zero_dword(MR_non_frame_count);
	MR_zero_dword(MR_non_frame_total_size);

	/*
	** We cannot initialize these to the starts of the their respective
	** memory areas, since those areas may not have been initialized yet.
	*/

	MR_det_frame_max = NULL;
	MR_non_frame_max = NULL;
}

void
MR_print_stack_frame_stats(void)
{
	FILE	*fp;
	double	det_frame_count;
	double	det_frame_total_size;
	double	non_frame_count;
	double	non_frame_total_size;

	fp = MR_checked_fopen(MR_STACK_FRAME_STATS, "open", "a");

	MR_convert_dword_to_double(MR_det_frame_count,
		det_frame_count);
	MR_convert_dword_to_double(MR_det_frame_total_size,
		det_frame_total_size);
	MR_convert_dword_to_double(MR_non_frame_count,
		non_frame_count);
	MR_convert_dword_to_double(MR_non_frame_total_size,
		non_frame_total_size);

	fprintf(fp, "number of det stack frames created:  %.0f\n",
		det_frame_count);
	fprintf(fp, "number of words in det stack frames: %.0f\n",
		det_frame_total_size);
	fprintf(fp, "average size of a det stack frame:   %.3f\n",
		det_frame_total_size / det_frame_count);
	fprintf(fp, "max size of det stack:               %ld\n",
		(long) (MR_det_frame_max
		       - MR_CONTEXT(MR_ctxt_detstack_zone)->min));
	fprintf(fp, "\n");

	fprintf(fp, "number of non stack frames created:  %.0f\n",
		non_frame_count);
	fprintf(fp, "number of words in non stack frames: %.0f\n",
		non_frame_total_size);
	fprintf(fp, "average size of a non stack frame:   %.3f\n",
		non_frame_total_size / non_frame_count);
	fprintf(fp, "max size of non stack:               %ld\n",
		(long) (MR_non_frame_max
			- MR_CONTEXT(MR_ctxt_nondetstack_zone)->min));
	fprintf(fp, "-------------------------------------------\n");

	MR_checked_fclose(fp, MR_STACK_FRAME_STATS);
}

#endif	/* MR_STACK_FRAME_STATS */

#ifdef	MR_USE_MINIMAL_MODEL

static	void	MR_print_gen_stack_entry(FILE *fp, MR_Integer i);
static	void	MR_cleanup_generator_ptr(MR_TrieNode generator_ptr);

void
MR_push_generator(MR_Word *frame_addr, MR_TrieNode table_addr)
{
	MR_gen_stack[MR_gen_next].generator_frame = frame_addr;
	MR_gen_stack[MR_gen_next].generator_table = table_addr;
	MR_gen_next++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("push ");
		MR_print_gen_stack_entry(stdout, MR_gen_next - 1);
	}
#endif
}

MR_Subgoal *
MR_top_generator_table(void)
{
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("top ");
		MR_print_gen_stack_entry(stdout, MR_gen_next - 1);
	}
#endif

	return MR_gen_stack[MR_gen_next - 1].generator_table->MR_subgoal;
}

void
MR_pop_generator(void)
{
	--MR_gen_next;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("pop ");
		MR_print_gen_stack_entry(stdout, MR_gen_next);
	}
#endif
}

void
MR_print_gen_stack(FILE *fp)
{
#ifdef	MR_TABLE_DEBUG
	int	i;

	if (MR_tabledebug) {
		for (i = MR_gen_next - 1; i >= 0; i--) {
			MR_print_gen_stack_entry(fp, i);
		}
	}
#endif
}

static void
MR_print_gen_stack_entry(FILE *fp, MR_Integer i)
{
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		fprintf(fp, "gen %ld = <", (long) i);
		MR_print_nondstackptr(fp, MR_gen_stack[i].generator_frame);
		fprintf(fp, ", %p>\n", MR_gen_stack[i].generator_table);
	}
#endif
}

void
MR_commit_mark(void)
{
	MR_restore_transient_registers();

	MR_cut_stack[MR_cut_next].frame = MR_maxfr;
	MR_cut_stack[MR_cut_next].gen_next = MR_gen_next;
	MR_cut_stack[MR_cut_next].generators = NULL;
	MR_cut_next++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("commit stack next up to %ld\n", (long) MR_cut_next);
	}
#endif

	MR_save_transient_registers();
}

void
MR_commit_cut(void)
{
	MR_CutGeneratorList	g;

	--MR_cut_next;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("commit stack next down to %ld\n",
			(long) MR_cut_next);
		printf("setting generator stack next back to %ld from %ld\n",
			(long) MR_cut_stack[MR_cut_next].gen_next,
			(long) MR_gen_next);

		if (MR_gen_next != MR_cut_stack[MR_cut_next].gen_next) {
			if (MR_gen_next <= MR_cut_stack[MR_cut_next].gen_next)
			{
				printf("MR_gen_next %ld, MR_cut_next %ld, "
					"MR_cut_stack[MR_cut_next].gen_next "
					"%ld\n",
					(long) MR_gen_next,
					(long) MR_cut_next,
					(long) MR_cut_stack[MR_cut_next].
						gen_next);
				MR_fatal_error("GEN_NEXT ASSERTION FAILURE");
			}
		}
	}
#endif

	for (g = MR_cut_stack[MR_cut_next].generators; g != NULL;
		g = g->next_generator)
	{
		MR_cleanup_generator_ptr(g->generator_ptr);
	}

	MR_cut_stack[MR_cut_next].generators = NULL;
	MR_gen_next = MR_cut_stack[MR_cut_next].gen_next;
}

void
MR_register_generator_ptr(MR_TrieNode generator_ptr)
{
	struct MR_CutGeneratorListNode	*node;

	node = MR_GC_NEW(struct MR_CutGeneratorListNode);
	node->generator_ptr = generator_ptr;
	node->next_generator = MR_cut_stack[MR_cut_next - 1].generators;
	MR_cut_stack[MR_cut_next - 1].generators = node;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("registering generator %p -> %p "
			"at commit stack level %d\n",
			generator_ptr, generator_ptr->MR_subgoal,
			MR_cut_next - 1);
	}
#endif
}

static void
MR_cleanup_generator_ptr(MR_TrieNode generator_ptr)
{
	if (generator_ptr->MR_subgoal->status == MR_SUBGOAL_COMPLETE) {
		/* there is nothing to do, everything is OK */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no cleanup: generator %p -> %p is complete\n",
				generator_ptr, generator_ptr->MR_subgoal);
		}
#endif
	} else {
		/* this generator will never complete the subgoal */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("cleanup: generator %p -> %p deleted\n",
				generator_ptr, generator_ptr->MR_subgoal);
		}
#endif

		generator_ptr->MR_subgoal = NULL;
	}
}

#endif	/* MR_USE_MINIMAL_MODEL */
