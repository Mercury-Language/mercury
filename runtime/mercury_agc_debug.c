/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Debugging support for the accurate garbage collector.
*/

#include "mercury_imp.h"
#include "mercury_layout_util.h"
#include "mercury_deep_copy.h"
#include "mercury_agc_debug.h"

#ifdef NATIVE_GC

/*
** Function prototypes.
*/
static	void	dump_live_value(MR_Live_Lval locn, MemoryZone *heap_zone,
			Word * stack_pointer, Word *current_frame,
			bool do_regs);

/*---------------------------------------------------------------------------*/


void
MR_agc_dump_roots(MR_RootList roots)
{
	Word	saved_regs[MAX_FAKE_REG];

	fflush(NULL);
	fprintf(stderr, "Dumping roots\n");

#ifdef MR_DEBUG_AGC_PRINT_VARS
	while (roots != NULL) {


		/*
		** Restore the registers, because we need to save them
		** to a more permanent backing store (we are going to
		** call Mercury soon, and we don't want it messing with
		** the saved registers).
		*/
		restore_registers();
		/* XXX this is unsafe -- should use MAX_FAKE_REG */
		MR_copy_regs_to_saved_regs(MAX_REAL_REG + MR_NUM_SPECIAL_REG,
			saved_regs);

		MR_hp = MR_ENGINE(debug_heap_zone->min);
		MR_virtual_hp = MR_ENGINE(debug_heap_zone->min);

		fflush(NULL);
		MR_write_variable((Word) roots->type_info, *roots->root);
		fflush(NULL);
		fprintf(stderr, "\n");

		/* XXX this is unsafe -- should use MAX_FAKE_REG */
		MR_copy_saved_regs_to_regs(MAX_REAL_REG + MR_NUM_SPECIAL_REG,
			saved_regs);
		save_registers();
		roots = roots->next;
	}
#endif
}

void
MR_agc_dump_stack_frames(MR_Internal *label, MemoryZone *heap_zone,
	Word *stack_pointer, Word *current_frame)
{
	Word saved_regs[MAX_FAKE_REG];
	int i, var_count;
	const MR_Stack_Layout_Vars *vars;
	Word *type_params, type_info, value;
	MR_Stack_Layout_Entry *entry_layout;
	const MR_Stack_Layout_Label *layout;
	const Code *success_ip;
	bool top_frame = TRUE;

	layout = label->i_layout;
	success_ip = label->i_addr;
	entry_layout = layout->MR_sll_entry;

	/* 
	** For each stack frame... 
	*/

	while (MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)) {
		if (label->i_name != NULL) {
			fprintf(stderr, "    label: %s\n", label->i_name);
		} else {
			fprintf(stderr, "    label: unknown\n");
		}

		if (success_ip == MR_stack_trace_bottom) {
			break;
		}

		var_count = layout->MR_sll_var_count;
		vars = &(layout->MR_sll_var_info);

		/*
		** XXX For the top stack frame, we should pass a pointer to
		** a filled-in saved_regs instead of NULL. For other stack
		** frames, passing NULL is fine, since output arguments are
		** not live yet for any call except the top one.
		*/
		type_params = MR_materialize_typeinfos_base(vars,
			NULL, stack_pointer, current_frame);

		for (i = 0; i < var_count; i++) {
			MR_Stack_Layout_Var sl_var;
			MR_Live_Type sl_type;


			fprintf(stderr, "%-12s\t", vars->MR_slvs_names[i]);

			sl_var = vars->MR_slvs_pairs[i];
	
			dump_live_value(sl_var.MR_slv_locn, heap_zone,
				stack_pointer, current_frame, top_frame);
			fprintf(stderr, "\n");
			fflush(NULL);
		
#ifdef MR_DEBUG_AGC_PRINT_VARS
			/*
			** Restore the registers, because we need to
			** save them to a more permanent backing store
			** (we are going to call Mercury soon, and we
			** don't want it messing with the saved
			** registers).
			*/
			restore_registers();
			/* XXX this is unsafe -- should use MAX_FAKE_REG */
			MR_copy_regs_to_saved_regs(MAX_REAL_REG +
				MR_NUM_SPECIAL_REG, saved_regs);

			MR_hp = MR_ENGINE(debug_heap_zone->min);
			MR_virtual_hp = MR_ENGINE(debug_heap_zone->min);

			/*
			** XXX We must pass NULL here because the registers
			** have not been saved. This is probably a bug;
			** Tyson should look into it.
			*/
			if (MR_get_type_and_value_base(&sl_var, 
					NULL, stack_pointer,
					current_frame, type_params,
					&type_info, &value)) {
				printf("\t");
				MR_write_variable(type_info, value);
				printf("\n");
			}

			/* XXX this is unsafe -- should use MAX_FAKE_REG */
			MR_copy_saved_regs_to_regs(MAX_REAL_REG +
				MR_NUM_SPECIAL_REG, saved_regs);
			save_registers();
#endif	/* MR_DEBUG_AGC_PRINT_VARS */

			fflush(NULL);

		}
		MR_free(type_params);

		/* 
		** Move to the next stack frame.
		*/
		{
			MR_Live_Lval            location;
			MR_Lval_Type            type;
			int                     number;

			location = entry_layout->MR_sle_succip_locn;
			type = MR_LIVE_LVAL_TYPE(location);
			number = MR_LIVE_LVAL_NUMBER(location);
			if (type != MR_LVAL_TYPE_STACKVAR) {
				fatal_error("can only handle stackvars");
			}
			                                
			success_ip = (Code *) 
				MR_based_stackvar(stack_pointer, number);
			stack_pointer = stack_pointer - 
				entry_layout->MR_sle_stack_slots;
			label = MR_lookup_internal_by_addr(success_ip);
		}

		top_frame = FALSE;

		layout = label->i_layout;
		entry_layout = layout->MR_sll_entry;
	}
}

static void
dump_live_value(MR_Live_Lval locn, MemoryZone *heap_zone, Word *stack_pointer,
	Word *current_frame, bool do_regs)
{
	int	locn_num;
	Word	value = 0;
	int	difference;
	bool 	have_value = FALSE;

	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (do_regs) {
				value = virtual_reg(locn_num);
				have_value = TRUE;
				fprintf(stderr, "r%d\t", locn_num);
			}
			break;

		case MR_LVAL_TYPE_F:
			fprintf(stderr, "f%d\t", locn_num);
			break;

		case MR_LVAL_TYPE_STACKVAR:
			value = MR_based_stackvar(stack_pointer, locn_num);
			have_value = TRUE;
			fprintf(stderr, "stackvar%d", locn_num);
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			value = MR_based_framevar(current_frame, locn_num);
			have_value = TRUE;
			fprintf(stderr, "framevar%d", locn_num);
			break;

		case MR_LVAL_TYPE_SUCCIP:
			fprintf(stderr, "succip");
			break;

		case MR_LVAL_TYPE_MAXFR:
			fprintf(stderr, "maxfr");
			break;

		case MR_LVAL_TYPE_CURFR:
			fprintf(stderr, "curfr");
			break;

		case MR_LVAL_TYPE_HP:
			fprintf(stderr, "hp");
			break;

		case MR_LVAL_TYPE_SP:
			fprintf(stderr, "sp");
			break;

		case MR_LVAL_TYPE_INDIRECT:
			fprintf(stderr, "offset %d from ",
				MR_LIVE_LVAL_INDIRECT_OFFSET(locn_num));
			/* XXX Tyson will have to complete this */
			/* based on what he wants this function to do */

		case MR_LVAL_TYPE_UNKNOWN:
			fprintf(stderr, "unknown");
			break;

		default:
			fprintf(stderr, "DEFAULT");
			break;
	}
	if (have_value) {
		if (value >= (Word) heap_zone->min && 
				value < (Word) heap_zone->hardmax) {
			difference = (Word *) value - (Word *) heap_zone->min;
			fprintf(stderr, "\thp[%d]\t(%lx)", difference,
				(long) value);
		} else {
			fprintf(stderr, "\t       \t(%lx)", (long) value);
		}
	}
}

#endif /* NATIVE_GC */

