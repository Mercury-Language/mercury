/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module contains the accurate garbage collector.
*/

#include "mercury_imp.h"

#ifdef MR_NATIVE_GC

#include "mercury_deep_copy.h"
#include "mercury_layout_util.h"
#include "mercury_agc_debug.h"

/*
** Function prototypes.
*/

#ifdef MR_HIGHLEVEL_CODE

void		MR_garbage_collect(void);
static void	traverse_stack(struct MR_StackChain *top);

#else /* !MR_HIGHLEVEL_CODE */

MR_define_extern_entry(mercury__garbage_collect_0_0);

static	void	garbage_collect(MR_Code *saved_success,
			MR_Word *stack_pointer,
			MR_Word *max_frame, MR_Word *current_frame);
static	void	copy_long_value(MR_Long_Lval locn, MR_TypeInfo type_info, 
			MR_bool copy_regs, MR_Word *stack_pointer,
			MR_Word *current_frame);
static	void	copy_short_value(MR_Short_Lval locn, MR_TypeInfo type_info,
			MR_bool copy_regs, MR_Word *stack_pointer,
			MR_Word *current_frame);

#endif

static	void	garbage_collect_roots(void);

/*
** Global variables (only used in this module, however).
*/

#ifndef MR_HIGHLEVEL_CODE
static MR_Code	*saved_success = (MR_Code *) NULL;
static MR_Word	*saved_success_location = (MR_Word *) NULL;
static MR_bool	gc_scheduled = MR_FALSE;
static MR_bool	gc_running = MR_FALSE;
#endif

/* The list of roots */
static MR_RootList root_list = NULL;

/* The last root on the list */
static MR_RootList last_root = NULL;

#ifdef MR_HIGHLEVEL_CODE

/*
** Perform a garbage collection:
**	swap the two heaps;
**	traverse the roots, copying data from the old heap to the new heap;
**	reset the old heap;
**
** This is the version for the MLDS back-end.  Beware that there is some
** code duplication with the version for the LLDS back-end, which is below.
*/
void
MR_garbage_collect(void)
{
    MR_MemoryZone                   *old_heap, *new_heap;
    MR_Word                         *old_hp, *new_hp;
    size_t			    heap_size_in_words;
    size_t			    num_words_for_bitmap;
    size_t			    num_bytes_for_bitmap;
    static size_t		    prev_num_bytes_for_bitmap;

    old_heap = MR_ENGINE(MR_eng_heap_zone);
    new_heap = MR_ENGINE(MR_eng_heap_zone2);

    /*
    ** Print some debugging messages.
    */
    if (MR_agc_debug) {
        fprintf(stderr, "\ngarbage_collect() called.\n");

        fprintf(stderr, "old_heap->min:  %lx \t old_heap->hardmax:  %lx\n", 
            (long) old_heap->min, (long) old_heap->hardmax);
	fprintf(stderr, "new_heap->min: %lx \t new_heap->hardmax: %lx\n", 
            (long) new_heap->min, (long) new_heap->hardmax);

        fprintf(stderr, "MR_virtual_hp:  %lx\n", (long) MR_virtual_hp);
    }

    old_hp = MR_virtual_hp;

    /*
    ** The new heap pointer starts at the bottom of the new heap.
    */
    MR_virtual_hp = new_heap->min;

    /*
    ** Initialize the forwarding pointer bitmap.
    */
    heap_size_in_words = old_heap->hardmax - old_heap->min;
    num_words_for_bitmap = (heap_size_in_words + MR_WORDBITS - 1) / MR_WORDBITS;
    num_bytes_for_bitmap = num_words_for_bitmap * sizeof(MR_Word);
    if (MR_has_forwarding_pointer == NULL
	|| num_bytes_for_bitmap > prev_num_bytes_for_bitmap)
    {
	MR_has_forwarding_pointer = MR_realloc(MR_has_forwarding_pointer,
					num_bytes_for_bitmap);
	prev_num_bytes_for_bitmap = num_bytes_for_bitmap;
    }
    memset(MR_has_forwarding_pointer, 0, num_bytes_for_bitmap);

    /*
    ** Swap the two heaps.
    */
    {
        MR_MemoryZone *tmp;

        tmp = MR_ENGINE(MR_eng_heap_zone2);
        MR_ENGINE(MR_eng_heap_zone2) = MR_ENGINE(MR_eng_heap_zone);
        MR_ENGINE(MR_eng_heap_zone) = tmp; 
    }

    if (MR_agc_debug) {
        fprintf(stderr, "Swapped heaps\n"); 
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
    }

    /*
    ** Copy any roots on the stack
    */
    traverse_stack(mercury__private_builtin__stack_chain);
    
    /*
    ** Copy any roots that are not on the stack.
    */
    garbage_collect_roots();

    /*
    ** Print some more debugging messages,
    ** and clear out the old heap.
    */
    if (MR_agc_debug) {
        fprintf(stderr, "Clearing old heap:\n");

        {
	    MR_Word *tmp_hp;

	    for (tmp_hp = old_heap->min; tmp_hp <= old_hp; tmp_hp++) {
		*tmp_hp = 0xDEADBEAF;
	    }
        }

        fprintf(stderr, "AFTER:\n");

    	/* XXX save this, it appears to get clobbered */
        new_hp = MR_virtual_hp;

        MR_agc_dump_roots(root_list);

    	/* XXX restore this, it appears to get clobbered */
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
        MR_virtual_hp = new_hp;
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);

        fprintf(stderr, "old heap: %ld bytes, new heap: %ld bytes\n",
            (long) ((char *) old_hp - (char *) old_heap->min),
            (long) ((char *) MR_virtual_hp - (char *) new_heap->min));
        fprintf(stderr, "%ld bytes recovered\n", 
            (long) ((char *) old_hp - (char *) old_heap->min) -
            ((char *) MR_virtual_hp - (char *) new_heap->min));

        fprintf(stderr, "garbage_collect() done.\n\n");
    }
}

static void
traverse_stack(struct MR_StackChain *stack_chain)
{
	/*
	** The trace() routines themselves should not allocate heap space;
	** they should use stack allocation.
	*/
	while (stack_chain != NULL) {
		(*stack_chain->trace)(stack_chain);
		stack_chain = stack_chain->prev;
	}
}

#else

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
MR_schedule_agc(MR_Code *pc_at_signal, MR_Word *sp_at_signal, 
	MR_Word *curfr_at_signal)
{
	MR_Label_Layout		*layout;
	const MR_Proc_Layout	*proc_layout;
	MR_Long_Lval_Type	type;
	MR_Long_Lval		location;
	const char		*reason;
	MR_Entry		*entry_label = NULL;
	int			number;
	MR_Determinism		determinism;

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
	fprintf(stderr, "curfr at signal: %ld (%lx)\n",
		(long) curfr_at_signal, (long) curfr_at_signal);
	fflush(NULL);
#endif

	/* Search for the entry label */

	entry_label = MR_prev_entry_by_addr(pc_at_signal);
	proc_layout = entry_label->e_layout;

	determinism = proc_layout->MR_sle_detism;

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
		*saved_success_location = (MR_Word) saved_success;
	}
	gc_scheduled = MR_TRUE;

	location = proc_layout->MR_sle_succip_locn;
	type = MR_LONG_LVAL_TYPE(location);
	number = MR_LONG_LVAL_NUMBER(location);
	if (MR_DETISM_DET_STACK(determinism)) {

		if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
			MR_fatal_error("can only handle stackvars");
		}

		/*
		** Save the old succip and its location.
		*/
		saved_success_location = &MR_based_stackvar(sp_at_signal,
			number);
		saved_success = (MR_Code *) *saved_success_location;
	} else {
		/*
		** XXX we don't support nondet stack frames yet.
		MR_fatal_error("cannot schedule in nondet stack frame");
		*/

		saved_success_location = &MR_based_framevar(curfr_at_signal,
			number);
		saved_success = (MR_Code *) *saved_success_location;
	}

#ifdef MR_DEBUG_AGC_SCHEDULING
	fprintf(stderr, "old succip: %ld (%lx) new: %ld (%lx)\n", 
		(long) saved_success, (long) saved_success,
		(long) ENTRY(mercury__garbage_collect_0_0),
		(long) ENTRY(mercury__garbage_collect_0_0));
#endif

	/*
	** Replace the old succip with the address of the
	** garbage collector.
	*/
	*saved_success_location = (Word) mercury__garbage_collect_0_0;

#ifdef MR_DEBUG_AGC_SCHEDULING
	fprintf(stderr, "Accurate GC scheduled.\n");
#endif
}

MR_BEGIN_MODULE(native_gc)
MR_BEGIN_CODE

/*
** Our garbage collection entry label.
**
** It saves the registers -- we use the saved registers
** for garbage collection and leave the real ones alone.
*/
MR_define_entry(mercury__garbage_collect_0_0);

        /* record that the collector is running */
	gc_running = MR_TRUE;

	MR_save_registers();
	garbage_collect(saved_success, MR_sp, MR_maxfr, MR_curfr);
	MR_restore_registers();
	gc_scheduled = MR_FALSE;
	gc_running = MR_FALSE;

	MR_succip = saved_success;
	MR_proceed();
	MR_fatal_error("Unreachable code reached");

MR_END_MODULE

/*---------------------------------------------------------------------------*/

/*
** garbage_collect:
**
** 	The main garbage collection routine.
**
**  (We use 4 space tabs here because of the depth of indentation).
*/
static void
garbage_collect(MR_Code *success_ip, MR_Word *stack_pointer, 
	MR_Word *max_frame, MR_Word *current_frame)
{
    MR_Internal                     *label, *first_label;
    int                             i, count;
    const MR_Label_Layout	    *label_layout;
    MR_MemoryZone                   *old_heap, *new_heap;
    MR_TypeInfoParams               type_params;
    MR_bool                            succeeded;
    MR_bool                            top_frame = MR_TRUE;
    MR_MemoryList                   allocated_memory_cells = NULL;
    MR_Word                         *old_hp, *new_hp;
    const MR_Proc_Layout	    *proc_layout;
    MR_Word                         *first_stack_pointer;
    MR_Word                         *first_current_frame;
    MR_Word                         *first_max_frame;

    old_heap = MR_ENGINE(MR_eng_heap_zone);
    new_heap = MR_ENGINE(MR_eng_heap_zone2);

    if (MR_agc_debug) {
        fprintf(stderr, "\ngarbage_collect() called.\n");

        fprintf(stderr, "old_heap->min:  %lx \t old_heap->hardmax:  %lx\n", 
            (long) old_heap->min, (long) old_heap->hardmax);
	fprintf(stderr, "new_heap->min: %lx \t new_heap->hardmax: %lx\n", 
            (long) new_heap->min, (long) new_heap->hardmax);

        fprintf(stderr, "MR_virtual_hp:  %lx\n", (long) MR_virtual_hp);
    }

    old_hp = MR_virtual_hp;

    /*
    ** The new heap pointer starts at the bottom of the new heap.
    */
    MR_virtual_hp = new_heap->min;

    /*
    ** Swap the two heaps.
    */
    {
        MR_MemoryZone *tmp;

        tmp = MR_ENGINE(MR_eng_heap_zone2);
        MR_ENGINE(MR_eng_heap_zone2) = MR_ENGINE(MR_eng_heap_zone);
        MR_ENGINE(MR_eng_heap_zone) = tmp; 
    }

    if (MR_agc_debug) {
        fprintf(stderr, "Swapped heaps\n"); 
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
    }

    label = MR_lookup_internal_by_addr(success_ip);
    label_layout = label->i_layout;

    if (MR_agc_debug) {
        first_label = label;
        first_stack_pointer = stack_pointer;
        first_current_frame = current_frame;
        first_max_frame = max_frame;
        fprintf(stderr, "BEFORE:\n");
        MR_agc_dump_stack_frames(first_label, old_heap, first_stack_pointer,
            first_current_frame);
        MR_agc_dump_nondet_stack_frames(first_label, old_heap,
	    first_stack_pointer, first_current_frame, first_max_frame);
        MR_agc_dump_roots(root_list);

        /* 
        ** XXX the debug code seems to be messing up the heap pointer.
        ** easier to reset it than debug the problem at the moment
        */
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
        MR_virtual_hp = new_heap->min;
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
    }

    /*
    ** For each stack frame ...
    */
    do {
        MR_Stack_Walk_Step_Result       result;
        const char                      *problem;
        const MR_Label_Layout           *return_label_layout;
	int				short_var_count, long_var_count;

	if (MR_agc_debug) {
	    printlabel((MR_Code *) (Word) label->i_addr);
       	    fflush(NULL);
	}

        short_var_count = MR_short_desc_var_count(label_layout);
        long_var_count = MR_long_desc_var_count(label_layout);

        /* Get the type parameters from the stack frame. */

	/* XXX We must pass NULL since the registers have not been saved */
	/* XXX This is probably a bug; Tyson should look into it */
        type_params = MR_materialize_type_params_base(label_layout,
            NULL, stack_pointer, current_frame);
        
        /* Copy each live variable */

        for (i = 0; i < long_var_count; i++) {
            MR_Long_Lval locn;
            MR_PseudoTypeInfo pseudo_type_info;
            MR_TypeInfo type_info;

	    locn = MR_long_desc_var_locn(label_layout, i);
            pseudo_type_info = MR_var_pti(label_layout, i);

            type_info = MR_make_type_info(type_params, pseudo_type_info,
                &allocated_memory_cells);
            copy_long_value(locn, type_info, top_frame,
                stack_pointer, current_frame);
            MR_deallocate(allocated_memory_cells);
            allocated_memory_cells = NULL;
        }

        for (; i < short_var_count; i++) {
            MR_Short_Lval locn;
            MR_PseudoTypeInfo pseudo_type_info;
            MR_TypeInfo type_info;

	    locn = MR_short_desc_var_locn(label_layout, i);
            pseudo_type_info = MR_var_pti(label_layout, i);

            type_info = MR_make_type_info(type_params, pseudo_type_info,
                &allocated_memory_cells);
            copy_short_value(locn, type_info, top_frame,
                stack_pointer, current_frame);
            MR_deallocate(allocated_memory_cells);
            allocated_memory_cells = NULL;
        }

        MR_free(type_params);

        proc_layout = label_layout->MR_sll_entry;

	{
		MR_Long_Lval            location;
		MR_Long_Lval_Type            type;
		int                     number;

		location = proc_layout->MR_sle_succip_locn;
		type = MR_LONG_LVAL_TYPE(location);
		number = MR_LONG_LVAL_NUMBER(location);
		if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
			fatal_error("can only handle stackvars");
			}
		
		success_ip = (MR_Code *) MR_based_stackvar(stack_pointer, number);
		stack_pointer = stack_pointer - 
			proc_layout->MR_sle_stack_slots;
		label = MR_lookup_internal_by_addr(success_ip);
	}

/*
	we should use this code eventually, but it requires a bit of
	a redesign of the code around here.
 
        result = MR_stack_walk_step(proc_layout, &label_layout,
            (MR_Word **) &stack_pointer, &current_frame, &problem);

        if (result == MR_STEP_ERROR_BEFORE || result == MR_STEP_ERROR_AFTER) {
            MR_fatal_error(problem);
        } 
*/

        if (label == NULL) {
            break;
        }
	return_label_layout = label->i_layout;
        label_layout = return_label_layout;
        top_frame = MR_FALSE;
    } while (label_layout != NULL); /* end for each stack frame... */

    /* 
    ** New code for nondet.
    ** XXX Will we need correct value of stack_pointer?
    */ 
    
    while (max_frame > MR_nondet_stack_trace_bottom) {
	MR_bool registers_valid;
	int frame_size;

	registers_valid = (max_frame == current_frame);
	frame_size = max_frame - MR_prevfr_slot(max_frame);

	if (frame_size == MR_NONDET_TEMP_SIZE) {
	    if (MR_agc_debug) {
		printlabel(MR_redoip_slot(max_frame));
		fflush(NULL);
	    }
	} else if (frame_size == MR_DET_TEMP_SIZE) {
	    if (MR_agc_debug) {
		printlabel(MR_redoip_slot(max_frame));
		fflush(NULL);
	    }
	    stack_pointer = MR_tmp_detfr_slot(max_frame); /* XXX ??? */
	} else {
	    if (MR_agc_debug) {
		printlabel(MR_redoip_slot(max_frame));
		fflush(NULL);
	    }
	}
	label = MR_lookup_internal_by_addr(MR_redoip_slot(max_frame));

	if (label != NULL) {
		int short_var_count, long_var_count;

		label_layout = label->i_layout;
		short_var_count = MR_short_desc_var_count(label_layout);
		long_var_count = MR_long_desc_var_count(label_layout);
		/* var_count = label_layout->MR_sll_var_count; */

		/* 
		** XXX We must pass NULL since the registers have not
		** been saved. This is probably a bug; Tyson should look
		** into it
		*/
		type_params = MR_materialize_type_params_base(label_layout,
		    NULL, stack_pointer, MR_redofr_slot(max_frame));
        
		/* Copy each live variable */
		for (i = 0; i < long_var_count; i++) {
		    MR_Long_Lval locn;
		    MR_PseudoTypeInfo pseudo_type_info;
		    MR_TypeInfo type_info;

		    locn = MR_long_desc_var_locn(label_layout, i);
		    pseudo_type_info = MR_var_pti(label_layout, i);

		    type_info = MR_make_type_info(type_params, pseudo_type_info,
			&allocated_memory_cells);
		    copy_long_value(locn, type_info, top_frame,
			stack_pointer, current_frame);
		    MR_deallocate(allocated_memory_cells);
		    allocated_memory_cells = NULL;
		}

		for (; i < short_var_count; i++) {
		    MR_Short_Lval locn;
		    MR_PseudoTypeInfo pseudo_type_info;
		    MR_TypeInfo type_info;

		    locn = MR_short_desc_var_locn(label_layout, i);
		    pseudo_type_info = MR_var_pti(label_layout, i);

		    type_info = MR_make_type_info(type_params, pseudo_type_info,
			&allocated_memory_cells);
		    copy_short_value(locn, type_info, top_frame,
			stack_pointer, current_frame);
		    MR_deallocate(allocated_memory_cells);
		    allocated_memory_cells = NULL;
		}
	}
	max_frame = MR_prevfr_slot(max_frame);
    }
    
    /*
    ** Copy any roots that are not on the stack.
    */
    garbage_collect_roots();

    if (MR_agc_debug) {
        fprintf(stderr, "Clearing old heap:\n");

        {
	    Word *tmp_hp;

	    for (tmp_hp = old_heap->min; tmp_hp <= old_hp; tmp_hp++) {
		*tmp_hp = 0xDEADBEAF;
	    }
        }

        fprintf(stderr, "AFTER:\n");

    	/* XXX save this, it appears to get clobbered */
        new_hp = MR_virtual_hp;

        MR_agc_dump_stack_frames(first_label, new_heap, first_stack_pointer,
            first_current_frame);
        MR_agc_dump_roots(root_list);
        MR_agc_dump_nondet_stack_frames(first_label, new_heap,
	    first_stack_pointer, first_current_frame, first_max_frame);

    	/* XXX restore this, it appears to get clobbered */
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
        MR_virtual_hp = new_hp;
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);

        fprintf(stderr, "old heap: %ld bytes, new heap: %ld bytes\n",
            (long) ((char *) old_hp - (char *) old_heap->min),
            (long) ((char *) MR_virtual_hp - (char *) new_heap->min));
        fprintf(stderr, "%ld bytes recovered\n", 
            (long) ((char *) old_hp - (char *) old_heap->min) -
            ((char *) MR_virtual_hp - (char *) new_heap->min));
    }

    /* Reset the redzone on the old heap */
    MR_reset_redzone(old_heap);

    if (MR_agc_debug) {
        fprintf(stderr, "garbage_collect() done.\n\n");
    }

}

/*
** copy_long_value:
** 	Copies a value in a register or stack frame,
** 	replacing the original with the new copy.
**
** 	The copying is done using MR_agc_deep_copy, which is
** 	the accurate GC version of MR_deep_copy (it leaves
** 	forwarding pointers in the old copy of the data, if
** 	it is on the old heap).
*/

static void
copy_long_value(MR_Long_Lval locn, MR_TypeInfo type_info, MR_bool copy_regs,
	MR_Word *stack_pointer, MR_Word *current_frame)
{
	int	locn_num;

	locn_num = MR_LONG_LVAL_NUMBER(locn);
	switch (MR_LONG_LVAL_TYPE(locn)) {
		case MR_LONG_LVAL_TYPE_R:
			if (copy_regs) {
				MR_virtual_reg(locn_num) = MR_agc_deep_copy(
					MR_virtual_reg(locn_num), type_info,
					MR_ENGINE(MR_eng_heap_zone2->min),
					MR_ENGINE(MR_eng_heap_zone2->hardmax));
			}
			break;

		case MR_LONG_LVAL_TYPE_F:
			break;

		case MR_LONG_LVAL_TYPE_STACKVAR:
			MR_based_stackvar(stack_pointer, locn_num) =
				MR_agc_deep_copy(MR_based_stackvar(
						stack_pointer,locn_num),
					type_info,
					MR_ENGINE(MR_eng_heap_zone2->min),
					MR_ENGINE(MR_eng_heap_zone2->hardmax));
			break;

		case MR_LONG_LVAL_TYPE_FRAMEVAR:
			MR_based_framevar(current_frame, locn_num) =
				MR_agc_deep_copy(
				MR_based_framevar(current_frame, locn_num),
				type_info,
				MR_ENGINE(MR_eng_heap_zone2->min),
				MR_ENGINE(MR_eng_heap_zone2->hardmax));
			break;

		case MR_LONG_LVAL_TYPE_SUCCIP:
			break;

		case MR_LONG_LVAL_TYPE_MAXFR:
			break;

		case MR_LONG_LVAL_TYPE_CURFR:
			break;

		case MR_LONG_LVAL_TYPE_HP:
			break;

		case MR_LONG_LVAL_TYPE_SP:
			break;

		case MR_LONG_LVAL_TYPE_INDIRECT:
			/* XXX Tyson will have to write the code for this */
			break;

		case MR_LONG_LVAL_TYPE_UNKNOWN:
			break;

		default:
			MR_fatal_error("Unknown MR_Long_Lval_Type in copy_long_value");
			break;
	}
}

static void
copy_short_value(MR_Short_Lval locn, MR_TypeInfo type_info, MR_bool copy_regs,
	MR_Word *stack_pointer, MR_Word *current_frame)
{
	int	locn_num;

	switch (MR_SHORT_LVAL_TYPE(locn)) {
		case MR_SHORT_LVAL_TYPE_R:
			if (copy_regs) {
				locn_num = MR_SHORT_LVAL_NUMBER(locn);
				MR_virtual_reg(locn_num) = MR_agc_deep_copy(
					MR_virtual_reg(locn_num), type_info,
					MR_ENGINE(MR_eng_heap_zone2->min),
					MR_ENGINE(MR_eng_heap_zone2->hardmax));
			}
			break;

		case MR_SHORT_LVAL_TYPE_STACKVAR:
			locn_num = MR_SHORT_LVAL_NUMBER(locn);
			MR_based_stackvar(stack_pointer, locn_num) =
				MR_agc_deep_copy(MR_based_stackvar(
						stack_pointer,locn_num),
					type_info,
					MR_ENGINE(MR_eng_heap_zone2->min),
					MR_ENGINE(MR_eng_heap_zone2->hardmax));
			break;

		case MR_SHORT_LVAL_TYPE_FRAMEVAR:
			locn_num = MR_SHORT_LVAL_NUMBER(locn);
			MR_based_framevar(current_frame, locn_num) =
				MR_agc_deep_copy(
					MR_based_framevar(current_frame,
						locn_num),
					type_info,
					MR_ENGINE(MR_eng_heap_zone2->min),
					MR_ENGINE(MR_eng_heap_zone2->hardmax));
				break;

		default:
			MR_fatal_error("Unknown MR_Short_Lval_Type "
				"in copy_short_value");
			break;
	}
}

#endif /* !MR_HIGHLEVEL_CODE */

/*
** garbage_collect_roots:
** 
** 	Copies the extra roots.  The roots are overwritten
** 	with the new data.
*/

static void
garbage_collect_roots(void) 
{
	MR_RootList current = root_list;

	while (current != NULL) {
		*current->root = MR_agc_deep_copy(*current->root,
			current->type_info, MR_ENGINE(MR_eng_heap_zone2->min), 
			MR_ENGINE(MR_eng_heap_zone2->hardmax));
		current = current->next;
	}

}

/*
** MR_agc_add_root_internal:
** 
** 	Adds a new root to the extra roots.
*/

void
MR_agc_add_root(MR_Word *root_addr, MR_TypeInfo type_info)
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

#endif /* MR_NATIVE_GC */
