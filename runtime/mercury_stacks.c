/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"
#include <stdio.h>

#ifdef	MR_USE_MINIMAL_MODEL

static	void	MR_print_gen_stack_entry(FILE *fp, Integer i);
static	void	MR_cleanup_generator_ptr(MR_Subgoal **generator_ptr);

void
MR_push_generator(Word *frame_addr, MR_Subgoal *table_addr)
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

	return MR_gen_stack[MR_gen_next - 1].generator_table;
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
MR_print_gen_stack_entry(FILE *fp, Integer i)
{
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		fprintf(fp, "gen %ld = <", (long) i);
		MR_print_nondstackptr(fp, MR_gen_stack[i].generator_frame);
		fprintf(fp, ", %p>\n",
			(void *) MR_gen_stack[i].generator_table);
	}
#endif
}

void
MR_commit_mark(void)
{
	restore_transient_registers();

	MR_cut_stack[MR_cut_next].frame = MR_maxfr;
	MR_cut_stack[MR_cut_next].gen_next = MR_gen_next;
	MR_cut_stack[MR_cut_next].generators = NULL;
	MR_cut_next++;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("commit stack next up to %ld\n", (long) MR_cut_next);
	}
#endif

	save_transient_registers();
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
				fatal_error("GEN_NEXT ASSERTION FAILURE");
			}
		}

		printf("setting generator stack next back to %ld from %ld\n",
			(long) MR_cut_stack[MR_cut_next].gen_next,
			(long) MR_gen_next);
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
MR_register_generator_ptr(MR_Subgoal **generator_ptr)
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
			generator_ptr, *generator_ptr, MR_cut_next - 1);
	}
#endif
}

static void
MR_cleanup_generator_ptr(MR_Subgoal **generator_ptr)
{
	if ((*generator_ptr)->status == MR_SUBGOAL_COMPLETE) {
		/* there is nothing to do, everything is OK */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no cleanup: generator %p -> %p is complete\n",
				generator_ptr, *generator_ptr);
		}
#endif
	} else {
		/* this generator will never complete the subgoal */
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("cleanup: generator %p -> %p deleted\n",
				generator_ptr, *generator_ptr);
		}
#endif

		*generator_ptr = NULL;
	}
}

#endif
