/*
** Copyright (C) 1998-2001, 2003-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains code for printing statistics about stack frame sizes,
** and for manipulating the generator stack, the cut stack and the pneg stack.
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

/***************************************************************************/

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

/***************************************************************************/

#ifdef	MR_USE_MINIMAL_MODEL

static	int	MR_pneg_cut_depth = 0;

static	void	MR_print_gen_stack_entry(FILE *fp, MR_Integer i,
			MR_GenStackFrame *p);

static	void	MR_cleanup_generator_ptr(MR_SubgoalPtr generator_ptr);
static	void	MR_print_cut_stack_entry(FILE *fp, MR_Integer i,
			MR_CutStackFrame *p);

static	void	MR_cleanup_consumer_ptr(MR_TrieNode consumer_ptr);
static	void	MR_print_pneg_stack_entry(FILE *fp, MR_Integer i,
			MR_PNegStackFrame *p);

/***************************************************************************/

/*
** Record that the nondet stack frame at address frame_addr is now the
** generator for subgoal.
*/

void
MR_push_generator(MR_Word *frame_addr, MR_SubgoalPtr subgoal)
{
	MR_gen_stack[MR_gen_next].MR_gen_frame = frame_addr;
	MR_gen_stack[MR_gen_next].MR_gen_subgoal = subgoal;
	MR_gen_next++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("push ");
		MR_print_gen_stack_entry(stdout, MR_gen_next - 1,
			&MR_gen_stack[MR_gen_next - 1]);
	}
#endif
}

/*
** Return the subgoal of the topmost generator on the nondet stack.
*/

MR_Subgoal *
MR_top_generator_table(void)
{
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("top ");
		MR_print_gen_stack_entry(stdout, MR_gen_next - 1,
			&MR_gen_stack[MR_gen_next - 1]);
	}
#endif

	return MR_gen_stack[MR_gen_next - 1].MR_gen_subgoal;
}

/*
** Record the deletion of the topmost generator on the nondet stack.
*/

void
MR_pop_generator(void)
{
	--MR_gen_next;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("pop ");
		MR_print_gen_stack_entry(stdout, MR_gen_next,
			&MR_gen_stack[MR_gen_next]);
	}
#endif
}

void
MR_print_gen_stack(FILE *fp)
{
	MR_print_any_gen_stack(fp, MR_gen_next, MR_gen_stack);
}

void
MR_print_any_gen_stack(FILE *fp, MR_Integer gen_next,
	MR_GenStackFrame *gen_block)
{
	MR_Integer	i;

	fprintf(fp, "gen stack size: %d\n", (int) gen_next);
	for (i = gen_next - 1; i >= 0; i--) {
		MR_print_gen_stack_entry(fp, i, &MR_gen_stack[i]);
	}
}

static void
MR_print_gen_stack_entry(FILE *fp, MR_Integer i, MR_GenStackFrame *p)
{
	MR_SubgoalDebug	*subgoal_debug;

	fprintf(fp, "gen %ld = <", (long) i);
	MR_print_nondstackptr(fp, p->MR_gen_frame);
	subgoal_debug = MR_lookup_subgoal_debug_addr(p->MR_gen_subgoal);
	fprintf(fp, ", %s>\n", MR_subgoal_debug_name(subgoal_debug));
}

/***************************************************************************/

/*
** Record the entering of a committed choice context.
*/

void
MR_commit_mark(void)
{
	MR_restore_transient_registers();

	MR_cut_stack[MR_cut_next].MR_cut_frame = MR_maxfr;
	MR_cut_stack[MR_cut_next].MR_cut_gen_next = MR_gen_next;
	MR_cut_stack[MR_cut_next].MR_cut_generators = NULL;
	MR_cut_stack[MR_cut_next].MR_cut_depth = MR_pneg_cut_depth;
	MR_cut_next++;
	MR_pneg_cut_depth++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("commit stack next up to %ld\n", (long) MR_cut_next);
	}
#endif

	MR_save_transient_registers();
}

/*
** Record the leaving of a committed choice context, and clean up the
** generators that were created within the context that are still active.
** We need to clean them up because otherwise, consumers will be depend on this
** generator to find all the answers to the generator's subgoal, but the
** generation will never compute any more answers, since it will never be
** backtracked into.
*/

void
MR_commit_cut(void)
{
	MR_CutGeneratorList	g;

	--MR_cut_next;
	--MR_pneg_cut_depth;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("commit stack next down to %ld\n",
			(long) MR_cut_next);
		printf("setting generator stack next back to %ld from %ld\n",
			(long) MR_cut_stack[MR_cut_next].MR_cut_gen_next,
			(long) MR_gen_next);

		if (MR_gen_next < MR_cut_stack[MR_cut_next].MR_cut_gen_next) {
			printf("MR_gen_next %ld, MR_cut_next %ld, "
				"MR_cut_stack[MR_cut_next].gen_next %ld\n",
				(long) MR_gen_next,
				(long) MR_cut_next,
				(long) MR_cut_stack[MR_cut_next].
					MR_cut_gen_next);
			MR_fatal_error("GEN_NEXT ASSERTION FAILURE");
		}
	}
#endif

	for (g = MR_cut_stack[MR_cut_next].MR_cut_generators; g != NULL;
		g = g->MR_cut_next_generator)
	{
		MR_cleanup_generator_ptr(g->MR_cut_generator_ptr);
	}

	MR_cut_stack[MR_cut_next].MR_cut_generators = NULL;
	MR_gen_next = MR_cut_stack[MR_cut_next].MR_cut_gen_next;
}

/*
** Record the creation of a generator, for possible cleanup later by
** MR_commit_cut.
*/

void
MR_register_generator_ptr(MR_SubgoalPtr subgoal)
{
	struct MR_CutGeneratorListNode	*node;

	if (MR_cut_next <= 0) {
		return;
	}

	node = MR_GC_NEW(struct MR_CutGeneratorListNode);
	node->MR_cut_generator_ptr = subgoal;
	node->MR_cut_next_generator =
		MR_cut_stack[MR_cut_next - 1].MR_cut_generators;
	MR_cut_stack[MR_cut_next - 1].MR_cut_generators = node;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("registering generator %p -> %s "
			"at commit stack level %d\n",
			subgoal, MR_subgoal_addr_name(subgoal),
			MR_cut_next - 1);
	}
#endif
}

static void
MR_cleanup_generator_ptr(MR_SubgoalPtr subgoal)
{
	if (subgoal->MR_sg_status == MR_SUBGOAL_COMPLETE) {
		/* there is nothing to do, everything is OK */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no cleanup: generator %p -> %s is complete\n",
				subgoal->MR_sg_back_ptr,
				MR_subgoal_addr_name(subgoal));
		}
#endif
	} else {
		/* this generator will never complete the subgoal */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("cleanup: generator %p -> %s deleted\n",
				subgoal->MR_sg_back_ptr,
				MR_subgoal_addr_name(subgoal));
		}
#endif

		subgoal->MR_sg_back_ptr->MR_subgoal = NULL;
	}
}

void
MR_print_cut_stack(FILE *fp)
{
	MR_print_any_cut_stack(fp, MR_cut_next, MR_cut_stack);
}

void
MR_print_any_cut_stack(FILE *fp, MR_Integer cut_next,
	MR_CutStackFrame *cut_block)
{
	MR_Integer	i;

	fprintf(fp, "cut stack size: %d\n", (int) cut_next);
	for (i = cut_next - 1; i >= 0; i--) {
		MR_print_cut_stack_entry(fp, i, &cut_block[i]);
	}
}

static void
MR_print_cut_stack_entry(FILE *fp, MR_Integer i, MR_CutStackFrame *p)
{
	MR_SubgoalDebug		*subgoal_debug;
	MR_CutGeneratorList	gen_list;

	fprintf(fp, "cut %ld = <", (long) i);
	MR_print_nondstackptr(fp, p->MR_cut_frame);
	fprintf(fp, ">, cut_gen_next %d, ", (int) p->MR_cut_gen_next);
	fprintf(fp, "pneg+cut stack depth %d\n", (int) p->MR_cut_depth);

	fprintf(fp, "registered generators:");
	gen_list = p->MR_cut_generators;
	if (gen_list == NULL) {
		fprintf(fp, " none");
	} else {
		while (gen_list != NULL) {
			if (gen_list->MR_cut_generator_ptr == NULL) {
				fprintf(fp, " <NULL>");
			} else {
				subgoal_debug = MR_lookup_subgoal_debug_addr(
					gen_list->MR_cut_generator_ptr);
				fprintf(fp, " <%s>",
					MR_subgoal_debug_name(subgoal_debug));
			}

			gen_list = gen_list->MR_cut_next_generator;
		}
	}

	fprintf(fp, "\n");
}

/***************************************************************************/

void
MR_register_suspension(MR_SubgoalPtr subgoal)
{
	MR_PNegConsumerList	node_ptr;

	if (MR_pneg_next <= 0) {
		return;
	}

	node_ptr = MR_TABLE_NEW(MR_PNegConsumerListNode);
	node_ptr->MR_pneg_consumer_ptr = subgoal;
	node_ptr->MR_pneg_next_consumer = 
		MR_pneg_stack[MR_pneg_next - 1].MR_pneg_consumers;
	MR_pneg_stack[MR_pneg_next - 1].MR_pneg_consumers = node_ptr;
}

void
MR_pneg_enter_cond(void)
{
	MR_restore_transient_registers();

	MR_pneg_stack[MR_pneg_next].MR_pneg_frame = MR_maxfr;
	MR_pneg_stack[MR_pneg_next].MR_pneg_gen_next = MR_gen_next;
	MR_pneg_stack[MR_pneg_next].MR_pneg_depth = MR_pneg_cut_depth;
	MR_pneg_stack[MR_pneg_next].MR_pneg_consumers = NULL;
	MR_pneg_next++;
	MR_pneg_cut_depth++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("pneg stack next up to %ld\n", (long) MR_pneg_next);
	}
#endif

	MR_save_transient_registers();
}

void
MR_pneg_enter_then(void)
{
	MR_PNegConsumerList	l;
	MR_PNegConsumerList	next;

	MR_restore_transient_registers();

	--MR_pneg_next;
	--MR_pneg_cut_depth;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("pneg stack down up to %ld (then)\n",
			(long) MR_pneg_next);
	}
#endif

	for (l = MR_pneg_stack[MR_pneg_next].MR_pneg_consumers; l != NULL;
		l = next)
	{
		next = l->MR_pneg_next_consumer;
		MR_table_free(l);
	}

	MR_save_transient_registers();
}

void
MR_pneg_enter_else(void)
{
	MR_PNegConsumerList	l;
	MR_PNegConsumerList	next;

	MR_restore_transient_registers();

	--MR_pneg_next;
	--MR_pneg_cut_depth;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("pneg stack down up to %ld (else)\n",
			(long) MR_pneg_next);
	}
#endif

	for (l = MR_pneg_stack[MR_pneg_next].MR_pneg_consumers; l != NULL;
		l = next)
	{
		next = l->MR_pneg_next_consumer;
		if (l->MR_pneg_consumer_ptr->MR_sg_status !=
			MR_SUBGOAL_COMPLETE)
		{
			MR_fatal_error("MR_pneg_enter_else: failing out of "
				"negated context with incomplete consumer");
		}

		MR_table_free(l);
	}

	MR_save_transient_registers();
}

void
MR_print_pneg_stack(FILE *fp)
{
	MR_print_any_pneg_stack(fp, MR_pneg_next, MR_pneg_stack);
}

void
MR_print_any_pneg_stack(FILE *fp, MR_Integer pneg_next,
	MR_PNegStackFrame *pneg_block)
{
	MR_Integer	i;

	fprintf(fp, "pneg stack size: %d\n", (int) pneg_next);
	for (i = MR_pneg_next - 1; i >= 0; i--) {
		MR_print_pneg_stack_entry(fp, i, &pneg_block[i]);
	}
}

static void
MR_print_pneg_stack_entry(FILE *fp, MR_Integer i, MR_PNegStackFrame *p)
{
	MR_PNegConsumerList	l;

	fprintf(fp, "pneg %d = <", (int) i);
	MR_print_nondstackptr(fp, p->MR_pneg_frame);
	fprintf(fp, ">, pneg_gen_next %d, ", (int) p->MR_pneg_gen_next);
	fprintf(fp, "pneg+cut stack depth %d\n", (int) p->MR_pneg_depth);

	fprintf(fp, "registered consumers: ");
	if (p->MR_pneg_consumers == NULL) {
		fprintf(fp, " none");
	} else {
		int	n;

		for (n = 1, l = p->MR_pneg_consumers; l != NULL;
			l = l->MR_pneg_next_consumer, n++)
		{
			fprintf(fp, " <%d: %s>", n,
				MR_subgoal_addr_name(l->MR_pneg_consumer_ptr));
		}
	}

	fprintf(fp, "\n");
}

/***************************************************************************/

#endif	/* MR_USE_MINIMAL_MODEL */
