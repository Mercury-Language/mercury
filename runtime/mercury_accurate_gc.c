/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module contains the accurate garbage collector.
*/

#include "mercury_imp.h"

#ifdef NATIVE_GC

#include "mercury_deep_copy.h"
#include "mercury_layout_util.h"
#include "mercury_agc_debug.h"

/*
** Function prototypes.
*/
static	void	garbage_collect(Code *saved_success, Word *stack_pointer,
			Word *current_frame);
static	void	garbage_collect_roots(void);
static	void	copy_value(MR_Live_Lval locn, Word *type_info, bool copy_regs,
			Word *stack_pointer, Word *current_frame);

/*
** Global variables (only used in this module, however).
*/
static Code	*saved_success = (Code *) NULL;
static Word	*saved_success_location = (Word *) NULL;
static bool	gc_scheduled = FALSE;
static bool	gc_running = FALSE;

/* The list of roots */
static MR_RootList root_list = NULL;

/* The last root on the list */
static MR_RootList last_root = NULL;


Define_extern_entry(mercury__garbage_collect_0_0);


/*
** MR_schedule_agc:
** 	Schedule garbage collection.  
** 	
** 	We do this by replacing the succip that is saved in
** 	the current procedure's stack frame with the address
** 	of the garbage collector.  When the current procedure
** 	returns, it will call the garbage collectior.
**
** 	(We go to this trouble because then the stacks will
** 	be in a known state -- each stack frame is described
** 	by information associated with the continuation label
** 	that the code will return to).
*/
void
MR_schedule_agc(Code *pc_at_signal, Word *sp_at_signal)
{
	MR_Stack_Layout_Label		*layout;
	const MR_Stack_Layout_Entry	*entry_layout;
	MR_Lval_Type			type;
	MR_Live_Lval			location;
	const char			*reason;
	MR_Entry			*entry_label = NULL;
	int				number;
	MR_Determinism			determinism;

	if (gc_running) {
		/*
		** This is bad news, but it can happen if you don't
		** collect any garbage.  We should try to avoid it by
		** resizing the heaps so they don't become too full.
		**
		** It might also be worthwhile eventually turning off
		** the redzone in the destination heap (but only when
		** the large problem of handling collections with little
		** garbage has been solved).
		*/

		fprintf(stderr, "Mercury runtime: Garbage collection scheduled"
				" while collector is already running\n");
		fprintf(stderr, "Mercury_runtime: Trying to continue...\n");
		return;
	}

#ifdef MR_DEBUG_AGC_SCHEDULING
	fprintf(stderr, "PC at signal: %ld (%lx)\n",
		(long) pc_at_signal, (long) pc_at_signal);
	fprintf(stderr, "SP at signal: %ld (%lx)\n",
		(long) sp_at_signal, (long) sp_at_signal);
	fflush(NULL);
#endif

	/* Search for the entry label */

	entry_label = MR_prev_entry_by_addr(pc_at_signal);
	entry_layout = entry_label->e_layout;

	determinism = entry_layout->MR_sle_detism;

	if (determinism < 0) {
		/*
		** This means we have reached some handwritten code that has
		** no further information about the stack frame.
		*/
		fprintf(stderr, "Mercury runtime: the label ");
		if (entry_label->e_name != NULL) {
			fprintf(stderr, "%s has no stack layout info\n",
				entry_label->e_name);
		} else {
			fprintf(stderr, "at address %p "
				"has no stack layout info\n",
				entry_label->e_addr);
		}

		fprintf(stderr, "Mercury runtime: Trying to continue...\n");
		return;
	}

#ifdef MR_DEBUG_AGC_SCHEDULING
	if (entry_label->e_name != NULL) {
		fprintf(stderr, "scheduling called at: %s (%ld %lx)\n",
			entry_label->e_name, (long) entry_label->e_addr,
			(long) entry_label->e_addr);
	} else {
		fprintf(stderr, "scheduling called at: (%ld %lx)\n",
			(long) entry_label->e_addr,
			(long) entry_label->e_addr);
	}
	fflush(NULL);
#endif

	/* 
	** If we have already scheduled a garbage collection, undo the
	** last change, and do a new one.
	*/
	if (gc_scheduled) {
#ifdef MR_DEBUG_AGC_SCHEDULING
		fprintf(stderr, "GC scheduled again. Replacing old scheduling,"
			" and trying to schedule again.\n");
#endif
		*saved_success_location = (Word) saved_success;
	}
	gc_scheduled = TRUE;

	if (MR_DETISM_DET_STACK(determinism)) {
		location = entry_layout->MR_sle_succip_locn;
		type = MR_LIVE_LVAL_TYPE(location);
		number = MR_LIVE_LVAL_NUMBER(location);

		if (type != MR_LVAL_TYPE_STACKVAR) {
			fatal_error("can only handle stackvars");
		}

		/*
		** Save the old succip and its location.
		*/
		saved_success_location = &MR_based_stackvar(sp_at_signal,
			number);
		saved_success = (Code *) *saved_success_location;

#ifdef MR_DEBUG_AGC_SCHEDULING
		fprintf(stderr, "old succip: %ld (%lx) new: %ld (%lx)", 
			(long) saved_success,
			(long) saved_success,
			(long) ENTRY(mercury__garbage_collect_0_0),
			(long) ENTRY(mercury__garbage_collect_0_0));
#endif

		/*
		** Replace the old succip with the address of the
		** garbage collector.
		*/
		*saved_success_location = (Word) mercury__garbage_collect_0_0;

	} else {
		/*
		** XXX we don't support nondet stack frames yet.
		*/
		fatal_error("cannot schedule in nondet stack frame");
	}


#ifdef MR_DEBUG_AGC_SCHEDULING
	fprintf(stderr, "Accurate GC scheduled.\n");
#endif
}

BEGIN_MODULE(native_gc)
BEGIN_CODE

/*
** Our garbage collection entry label.
**
** It saves the registers -- we use the saved registers
** for garbage collection and leave the real ones alone.
*/
Define_entry(mercury__garbage_collect_0_0);

        /* record that the collector is running */
	gc_running = TRUE;

	save_registers();
	garbage_collect(saved_success, MR_sp, MR_curfr);
	restore_registers();
	gc_scheduled = FALSE;
	gc_running = FALSE;

	MR_succip = saved_success;
	proceed();
	fatal_error("Unreachable code reached");

END_MODULE

/*---------------------------------------------------------------------------*/

/*
** garbage_collect:
**
** 	The main garbage collection routine.
**
**  (We use 4 space tabs here because of the depth of indentation).
*/
void
garbage_collect(Code *success_ip, Word *stack_pointer, Word *current_frame)
{
    MR_Internal                     *label, *first_label;
    int                             i, var_count, count;
    const MR_Stack_Layout_Label     *internal_layout;
    const MR_Stack_Layout_Vars      *vars;
    MemoryZone                      *old_heap, *new_heap;
    Word                            *type_params;
    bool                            succeeded;
    bool                            top_frame = TRUE;
    MR_MemoryList                   allocated_memory_cells = NULL;
    Word                            *old_hp;
    MR_Stack_Layout_Entry           *entry_layout;
    Word                            *first_stack_pointer, *first_current_frame;


    old_heap = MR_ENGINE(heap_zone);
    new_heap = MR_ENGINE(heap_zone2);

#ifdef MR_DEBUG_AGC_COLLECTION
    fprintf(stderr, "\ngarbage_collect() called.\n");

    fprintf(stderr, "old_heap->min:  %lx \t old_heap->hardmax:  %lx\n", 
        (long) old_heap->min, (long) old_heap->hardmax);
	fprintf(stderr, "new_heap->min: %lx \t new_heap->hardmax: %lx\n", 
        (long) new_heap->min, (long) new_heap->hardmax);

    fprintf(stderr, "MR_virtual_hp:  %lx\n", (long) MR_virtual_hp);
#endif

    old_hp = MR_virtual_hp;

    /*
    ** The new heap pointer starts at the bottom of the new heap.
    */
    MR_virtual_hp = new_heap->min;

    /*
    ** Swap the two heaps.
    */
    {
        MemoryZone *tmp;

        tmp = MR_ENGINE(heap_zone2);
        MR_ENGINE(heap_zone2) = MR_ENGINE(heap_zone);
        MR_ENGINE(heap_zone) = tmp; 
    }

#ifdef MR_DEBUG_AGC_COLLECTION
    fprintf(stderr, "Swapped heaps\n"); 
    fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
#endif

    label = MR_lookup_internal_by_addr(success_ip);
    internal_layout = label->i_layout;
    entry_layout = internal_layout->MR_sll_entry;

#ifdef MR_DEBUG_AGC_COLLECTION
    first_label = label;
    first_stack_pointer = stack_pointer;
    first_current_frame = current_frame;
    fprintf(stderr, "BEFORE:\n");
    MR_agc_dump_stack_frames(first_label, old_heap, first_stack_pointer,
        first_current_frame);
    MR_agc_dump_roots(root_list);
#endif

    /*
    ** For each stack frame ...
    */
    do {
        MR_Stack_Walk_Step_Result       result;
        const char                      *problem;
        const MR_Stack_Layout_Label     *return_label_layout;

        var_count = internal_layout->MR_sll_var_count;
        vars = &(internal_layout->MR_sll_var_info);

        /* Get the type parameters from the stack frame. */

	/* XXX We must pass NULL since the registers have not been saved */
	/* XXX This is probably a bug; Tyson should look into it */
        type_params = MR_materialize_typeinfos_base(vars,
            NULL, stack_pointer, current_frame);
        
        /* Copy each live variable */

        for (i = 0; i < var_count; i++) {
            MR_Stack_Layout_Var sl_var;
            MR_Live_Type sl_type;
            Word *pseudo_type_info, *type_info;

            sl_var = vars->MR_slvs_pairs[i];
            if (MR_LIVE_TYPE_IS_VAR(sl_var.MR_slv_live_type)) {
                pseudo_type_info = MR_LIVE_TYPE_GET_VAR_TYPE(
                    sl_var.MR_slv_live_type);
                type_info = MR_make_type_info(type_params, pseudo_type_info,
                    &allocated_memory_cells);
                copy_value(sl_var.MR_slv_locn, type_info, top_frame,
                    stack_pointer, current_frame);
                MR_deallocate(allocated_memory_cells);
                allocated_memory_cells = NULL;
            }
        }

        MR_free(type_params);

        result = MR_stack_walk_step(entry_layout, &return_label_layout,
            (Word **) &stack_pointer, &current_frame, &problem);

        if (result == STEP_ERROR_BEFORE || result == STEP_ERROR_AFTER) {
            fatal_error(problem);
        } 

        if (return_label_layout == NULL) {
            break;
        }
        entry_layout = return_label_layout->MR_sll_entry;
        internal_layout = return_label_layout;
        top_frame = FALSE;
    } while (TRUE); /* end for each stack frame... */

    /*
    ** Copy any roots that are not on the stack.
    */
    garbage_collect_roots();

#ifdef MR_DEBUG_AGC_COLLECTION
    fprintf(stderr, "AFTER:\n");

    MR_agc_dump_stack_frames(first_label, new_heap, first_stack_pointer,
        first_current_frame);
    MR_agc_dump_roots(root_list);

    fprintf(stderr, "old heap: %ld bytes, new heap: %ld bytes\n",
        (long) ((char *) old_hp - (char *) old_heap->min),
        (long) ((char *) MR_virtual_hp - (char *) new_heap->min));
    fprintf(stderr, "%ld bytes recovered\n", 
        (long) ((char *) old_hp - (char *) old_heap->min) -
        ((char *) MR_virtual_hp - (char *) new_heap->min));
#endif

    /* Reset the redzone on the old heap */
    reset_redzone(old_heap);

#ifdef MR_DEBUG_AGC_COLLECTION
    fprintf(stderr, "garbage_collect() done.\n\n");
#endif

}

/*
** copy_value:
** 	Copies a value in a register or stack frame,
** 	replacing the original with the new copy.
**
** 	The copying is done using agc_deep_copy, which is
** 	the accurate GC version of deep_copy (it leaves
** 	forwarding pointers in the old copy of the data, if
** 	it is on the old heap).
*/
void
copy_value(MR_Live_Lval locn, Word *type_info, bool copy_regs,
	Word *stack_pointer, Word *current_frame)
{
	int	locn_num;

	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (copy_regs) {
				virtual_reg(locn_num) = agc_deep_copy(
					&virtual_reg(locn_num), type_info,
					MR_ENGINE(heap_zone2->min),
					MR_ENGINE(heap_zone2->hardmax));
			}
			break;

		case MR_LVAL_TYPE_F:
			break;

		case MR_LVAL_TYPE_STACKVAR:
			MR_based_stackvar(stack_pointer, locn_num) =
				agc_deep_copy(&MR_based_stackvar(
						stack_pointer,locn_num),
					type_info, MR_ENGINE(heap_zone2->min),
					MR_ENGINE(heap_zone2->hardmax));
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			MR_based_framevar(current_frame, locn_num) =
				agc_deep_copy(
				&MR_based_framevar(current_frame, locn_num),
				type_info,
				MR_ENGINE(heap_zone2->min),
				MR_ENGINE(heap_zone2->hardmax));
			break;

		case MR_LVAL_TYPE_SUCCIP:
			break;

		case MR_LVAL_TYPE_MAXFR:
			break;

		case MR_LVAL_TYPE_CURFR:
			break;

		case MR_LVAL_TYPE_HP:
			break;

		case MR_LVAL_TYPE_SP:
			break;

		case MR_LVAL_TYPE_INDIRECT:
			/* XXX Tyson will have to write the code for this */
			break;

		case MR_LVAL_TYPE_UNKNOWN:
			break;

		default:
			break;
	}
}

/*
** garbage_collect_roots:
** 
** 	Copies the extra roots.  The roots are overwritten
** 	with the new data.
*/
void
garbage_collect_roots(void) 
{
	MR_RootList current = root_list;

	while (current != NULL) {
		*current->root = agc_deep_copy(current->root,
			current->type_info, MR_ENGINE(heap_zone2->min), 
			MR_ENGINE(heap_zone2->hardmax));
		current = current->next;
	}

}

/*
** MR_agc_add_root_internal:
** 
** 	Adds a new root to the extra roots.
*/
void
MR_agc_add_root(Word *root_addr, Word *type_info)
{
	MR_RootList node;

	node = MR_malloc(sizeof(*node));
	node->root = root_addr;
	node->type_info = type_info;

	if (root_list == NULL) {
		root_list = node;
		last_root = node;
		last_root->next = NULL;
	} else {
		last_root->next = node;
		last_root = node;
	}
}

#endif /* NATIVE_GC */
