/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file implements utilities that can be useful
** for both the internal and external debuggers.
**
** Main authors: Zoltan Somogyi and Fergus Henderson.
*/

#include "mercury_imp.h"
#include "mercury_stack_layout.h"
#include "mercury_layout_util.h"

static MR_Word MR_lookup_closure_long_lval(MR_Long_Lval locn,
	MR_Closure *closure, MR_bool *succeeded);
static MR_Word MR_lookup_answer_block_long_lval(MR_Long_Lval locn,
	MR_Word *answer_block, int block_size, MR_bool *succeeded);

void
MR_copy_regs_to_saved_regs(int max_mr_num, MR_Word *saved_regs)
{
	/*
	** In the process of browsing within the debugger, we call Mercury,
	** which may clobber the contents of the virtual machine registers,
	** both control and general purpose, and both real and virtual
	** registers. We must therefore save and restore these.
	** We store them in the saved_regs array.
	**
	** The call to MR_trace will clobber the transient registers
	** on architectures that have them. The compiler generated code
	** will therefore call MR_save_transient_registers to save the
	** transient registers in the fake_reg array. We here restore them
	** to the real registers, save them with the other registers back in
	** fake_reg, and then copy all fake_reg entries to saved_regs.
	*/

	int i;

	MR_restore_transient_registers();
	MR_save_registers();

	for (i = 0; i <= max_mr_num; i++) {
		saved_regs[i] = MR_fake_reg[i];
	}
}

void
MR_copy_saved_regs_to_regs(int max_mr_num, MR_Word *saved_regs)
{
	/*
	** We execute the converse procedure to MR_copy_regs_to_saved_regs.
	** The MR_save_transient_registers is there so that a call to the
	** MR_restore_transient_registers macro after MR_trace will do the
	** right thing.
	*/

	int i;

	for (i = 0; i <= max_mr_num; i++) {
		MR_fake_reg[i] = saved_regs[i];
	}

	MR_restore_registers();
	MR_save_transient_registers();
}

MR_TypeInfoParams
MR_materialize_type_params(const MR_Label_Layout *label_layout,
	MR_Word *saved_regs)
{
	return MR_materialize_type_params_base(label_layout, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs));
}

MR_TypeInfoParams
MR_materialize_type_params_base(const MR_Label_Layout *label_layout,
	MR_Word *saved_regs, MR_Word *base_sp, MR_Word *base_curfr)
{
	const MR_Type_Param_Locns *tvar_locns;
	
	tvar_locns = label_layout->MR_sll_tvars;
	if (tvar_locns != NULL) {
		MR_TypeInfoParams	type_params;
		MR_bool			succeeded;
		MR_Integer		count;
		int			i;

		count = tvar_locns->MR_tp_param_count;
		type_params = (MR_TypeInfoParams)
			MR_NEW_ARRAY(MR_Word, count + 1);

		for (i = 0; i < count; i++) {
			if (tvar_locns->MR_tp_param_locns[i] != 0)
			{
				type_params[i + 1] = (MR_TypeInfo)
					MR_lookup_long_lval_base(
						tvar_locns->
							MR_tp_param_locns[i],
						saved_regs, base_sp, base_curfr,
						&succeeded);
				if (! succeeded) {
					MR_fatal_error("missing type param in "
					    "MR_materialize_type_params_base");
				}
			}
		}

		return type_params;

	} else {
		return NULL;
	}
}

MR_TypeInfoParams
MR_materialize_closure_type_params(MR_Closure *closure)
{
	const MR_Type_Param_Locns *tvar_locns;

	tvar_locns = closure->MR_closure_layout->MR_closure_type_params;
	if (tvar_locns != NULL) {
		MR_TypeInfoParams	type_params;
		MR_bool			succeeded;
		MR_Integer		count;
		int			i;

		count = tvar_locns->MR_tp_param_count;
		type_params = (MR_TypeInfoParams)
			MR_NEW_ARRAY(MR_Word, count + 1);

		for (i = 0; i < count; i++) {
			if (tvar_locns->MR_tp_param_locns[i] != 0)
			{
				type_params[i + 1] = (MR_TypeInfo)
					MR_lookup_closure_long_lval(
						tvar_locns->
							MR_tp_param_locns[i],
						closure, &succeeded);
				if (! succeeded) {
					MR_fatal_error("missing type param in "
					  "MR_materialize_closure_type_params");
				}
			}
		}

		return type_params;
	} else {
		return NULL;
	}
}

MR_TypeInfoParams
MR_materialize_answer_block_type_params(const MR_Type_Param_Locns *tvar_locns,
	MR_Word *answer_block, int block_size)
{
	if (tvar_locns != NULL) {
		MR_TypeInfoParams	type_params;
		MR_bool			succeeded;
		MR_Integer		count;
		int			i;

		count = tvar_locns->MR_tp_param_count;
		type_params = (MR_TypeInfoParams)
			MR_NEW_ARRAY(MR_Word, count + 1);

		for (i = 0; i < count; i++) {
			if (tvar_locns->MR_tp_param_locns[i] != 0)
			{
				type_params[i + 1] = (MR_TypeInfo)
					MR_lookup_answer_block_long_lval(
						tvar_locns->
							MR_tp_param_locns[i],
						answer_block, block_size,
						&succeeded);
				if (! succeeded) {
					MR_fatal_error("missing type param in "
				     "MR_materialize_answer_block_type_params");
				}
			}
		}

		return type_params;
	} else {
		return NULL;
	}
}

int
MR_get_register_number_long(MR_Long_Lval locn)
{
	if (MR_LONG_LVAL_TYPE(locn) == MR_LONG_LVAL_TYPE_R) {
		return MR_LONG_LVAL_NUMBER(locn);
	} else {
		return -1;
	}
}

int
MR_get_register_number_short(MR_Short_Lval locn)
{
	if (MR_SHORT_LVAL_TYPE(locn) == MR_SHORT_LVAL_TYPE_R) {
		return locn >> MR_SHORT_LVAL_TAGBITS;
	} else {
		return -1;
	}
}

#ifdef	MR_DEBUG_LVAL_REP
  #define MR_print_locn MR_TRUE
#else
  #define MR_print_locn MR_FALSE
#endif

MR_Word
MR_lookup_long_lval(MR_Long_Lval locn, MR_Word *saved_regs, MR_bool *succeeded)
{
	return MR_lookup_long_lval_base(locn, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		succeeded);
}

static MR_Word
MR_lookup_closure_long_lval(MR_Long_Lval locn, MR_Closure *closure,
	MR_bool *succeeded)
{
	int	locn_num;
	int	offset;
	MR_Word	value;
	MR_Word	baseaddr;
	MR_Word	sublocn;

	*succeeded = MR_FALSE;
	value = 0;

	locn_num = (int) MR_LONG_LVAL_NUMBER(locn);
	switch (MR_LONG_LVAL_TYPE(locn)) {
		case MR_LONG_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("closure r%d\n", locn_num);
			}
			if (locn_num <= closure->MR_closure_num_hidden_args) {
				value = closure->
					MR_closure_hidden_args(locn_num);
				*succeeded = MR_TRUE;
			}
			break;

		case MR_LONG_LVAL_TYPE_F:
			if (MR_print_locn) {
				printf("closure f%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("closure stackvar%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("closure framevar%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_SUCCIP:
			if (MR_print_locn) {
				printf("closure succip\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_MAXFR:
			if (MR_print_locn) {
				printf("closure maxfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_CURFR:
			if (MR_print_locn) {
				printf("closure curfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_HP:
			if (MR_print_locn) {
				printf("closure hp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_SP:
			if (MR_print_locn) {
				printf("closure sp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_INDIRECT:
			offset = MR_LONG_LVAL_INDIRECT_OFFSET(locn_num);
			sublocn = MR_LONG_LVAL_INDIRECT_BASE_LVAL(locn_num);
			if (MR_print_locn) {
				printf("closure offset %d from ", offset);
			}
			baseaddr = MR_lookup_closure_long_lval(sublocn,
					closure, succeeded);
			if (! *succeeded) {
				break;
			}
			value = MR_typeclass_info_type_info(baseaddr, offset);
			*succeeded = MR_TRUE;
			break;

		case MR_LONG_LVAL_TYPE_UNKNOWN:
			if (MR_print_locn) {
				printf("closure unknown\n");
			}
			break;

		default:
			if (MR_print_locn) {
				printf("closure DEFAULT\n");
			}
			break;
	}

	return value;
}

static MR_Word
MR_lookup_answer_block_long_lval(MR_Long_Lval locn, MR_Word *answer_block,
	int block_size, MR_bool *succeeded)
{
	int	locn_num;
	int	offset;
	MR_Word	value;
	MR_Word	baseaddr;
	MR_Word	sublocn;

	*succeeded = MR_FALSE;
	value = 0;

	locn_num = (int) MR_LONG_LVAL_NUMBER(locn);
	switch (MR_LONG_LVAL_TYPE(locn)) {
		case MR_LONG_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("answer_block r%d\n", locn_num);
			}
			if (locn_num <= block_size) {
				value = answer_block[locn_num];
				*succeeded = MR_TRUE;
			}
			break;

		case MR_LONG_LVAL_TYPE_F:
			if (MR_print_locn) {
				printf("answer_block f%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("answer_block stackvar%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("answer_block framevar%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_SUCCIP:
			if (MR_print_locn) {
				printf("answer_block succip\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_MAXFR:
			if (MR_print_locn) {
				printf("answer_block maxfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_CURFR:
			if (MR_print_locn) {
				printf("answer_block curfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_HP:
			if (MR_print_locn) {
				printf("answer_block hp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_SP:
			if (MR_print_locn) {
				printf("answer_block sp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_INDIRECT:
			offset = MR_LONG_LVAL_INDIRECT_OFFSET(locn_num);
			sublocn = MR_LONG_LVAL_INDIRECT_BASE_LVAL(locn_num);
			if (MR_print_locn) {
				printf("answer_block offset %d from ", offset);
			}
			baseaddr = MR_lookup_answer_block_long_lval(sublocn,
					answer_block, block_size, succeeded);
			if (! *succeeded) {
				break;
			}
			value = MR_typeclass_info_type_info(baseaddr, offset);
			*succeeded = MR_TRUE;
			break;

		case MR_LONG_LVAL_TYPE_UNKNOWN:
			if (MR_print_locn) {
				printf("answer_block unknown\n");
			}
			break;

		default:
			if (MR_print_locn) {
				printf("answer_block DEFAULT\n");
			}
			break;
	}

	return value;
}

MR_Word
MR_lookup_long_lval_base(MR_Long_Lval locn, MR_Word *saved_regs,
	MR_Word *base_sp, MR_Word *base_curfr, MR_bool *succeeded)
{
	int	locn_num;
	int	offset;
	MR_Word	value;
	MR_Word	baseaddr;
	MR_Word	sublocn;

	*succeeded = MR_FALSE;
	value = 0;

	locn_num = (int) MR_LONG_LVAL_NUMBER(locn);
	switch (MR_LONG_LVAL_TYPE(locn)) {
		case MR_LONG_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("long r%d\n", locn_num);
			}
			if (saved_regs != NULL) {
				value = MR_saved_reg(saved_regs, locn_num);
				*succeeded = MR_TRUE;
			}
			break;

		case MR_LONG_LVAL_TYPE_F:
			if (MR_print_locn) {
				printf("long f%d\n", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("long stackvar%d\n", locn_num);
			}
			value = MR_based_stackvar(base_sp, locn_num);
			*succeeded = MR_TRUE;
			break;

		case MR_LONG_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("long framevar%d\n", locn_num);
			}
			value = MR_based_framevar(base_curfr, locn_num);
			*succeeded = MR_TRUE;
			break;

		case MR_LONG_LVAL_TYPE_SUCCIP:
			if (MR_print_locn) {
				printf("long succip\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_MAXFR:
			if (MR_print_locn) {
				printf("long maxfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_CURFR:
			if (MR_print_locn) {
				printf("long curfr\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_HP:
			if (MR_print_locn) {
				printf("long hp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_SP:
			if (MR_print_locn) {
				printf("long sp\n");
			}
			break;

		case MR_LONG_LVAL_TYPE_INDIRECT:
			offset = MR_LONG_LVAL_INDIRECT_OFFSET(locn_num);
			sublocn = MR_LONG_LVAL_INDIRECT_BASE_LVAL(locn_num);
			if (MR_print_locn) {
				printf("long offset %d from ", offset);
			}
			baseaddr = MR_lookup_long_lval_base(sublocn,
					saved_regs, base_sp, base_curfr,
					succeeded);
			if (! *succeeded) {
				break;
			}
			value = MR_typeclass_info_type_info(baseaddr,
				offset);
			*succeeded = MR_TRUE;
			break;

		case MR_LONG_LVAL_TYPE_UNKNOWN:
			if (MR_print_locn) {
				printf("long unknown\n");
			}
			break;

		default:
			if (MR_print_locn) {
				printf("long DEFAULT\n");
			}
			break;
	}

	return value;
}

MR_Word
MR_lookup_short_lval(MR_Short_Lval locn, MR_Word *saved_regs,
			MR_bool *succeeded)
{
	return MR_lookup_short_lval_base(locn, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		succeeded);
}

MR_Word
MR_lookup_short_lval_base(MR_Short_Lval locn, MR_Word *saved_regs,
	MR_Word *base_sp, MR_Word *base_curfr, MR_bool *succeeded)
{
	int	locn_num;
	MR_Word	value;

	*succeeded = MR_FALSE;
	value = 0;

	locn_num = (int) locn >> MR_SHORT_LVAL_TAGBITS;
	switch (MR_SHORT_LVAL_TYPE(locn)) {
		case MR_SHORT_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("short r%d\n", locn_num);
			}
			if (saved_regs != NULL) {
				value = MR_saved_reg(saved_regs, locn_num);
				*succeeded = MR_TRUE;
			}
			break;

		case MR_SHORT_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("short stackvar%d\n", locn_num);
			}
			value = MR_based_stackvar(base_sp, locn_num);
			*succeeded = MR_TRUE;
			break;

		case MR_SHORT_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("short framevar%d\n", locn_num);
			}
			value = MR_based_framevar(base_curfr, locn_num);
			*succeeded = MR_TRUE;
			break;

		case MR_SHORT_LVAL_TYPE_SPECIAL:
			switch (locn_num) {
				case MR_LONG_LVAL_TYPE_SUCCIP:
					if (MR_print_locn) {
						printf("short succip\n");
					}
					break;

				case MR_LONG_LVAL_TYPE_MAXFR:
					if (MR_print_locn) {
						printf("short maxfr\n");
					}
					break;

				case MR_LONG_LVAL_TYPE_CURFR:
					if (MR_print_locn) {
						printf("short curfr\n");
					}
					break;

				case MR_LONG_LVAL_TYPE_HP:
					if (MR_print_locn) {
						printf("short hp\n");
					}
					break;

				case MR_LONG_LVAL_TYPE_SP:
					if (MR_print_locn) {
						printf("short sp\n");
					}
					break;

				default:
					if (MR_print_locn) {
						printf("short spec DEFAULT\n");
					}
			}

		default:
			MR_fatal_error("MR_lookup_short_lval_base: bad type");
	}

	return value;
}

MR_bool
MR_get_type_and_value(const MR_Label_Layout *label_layout, int i,
	MR_Word *saved_regs, MR_TypeInfo *type_params, MR_TypeInfo *type_info,
	MR_Word *value)
{
	return MR_get_type_and_value_base(label_layout, i, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info, value);
}

MR_bool
MR_get_type_and_value_base(const MR_Label_Layout *label_layout, int i,
	MR_Word *saved_regs, MR_Word *base_sp, MR_Word *base_curfr,
	MR_TypeInfo *type_params, MR_TypeInfo *type_info, MR_Word *value)
{
	MR_PseudoTypeInfo	pseudo_type_info;
	MR_bool			succeeded;

	pseudo_type_info = MR_var_pti(label_layout, i);
	*type_info = MR_create_type_info(type_params, pseudo_type_info);

	if (i < MR_long_desc_var_count(label_layout)) {
		if (MR_print_locn) {
			printf("looking up long lval\n");
		}

		*value = MR_lookup_long_lval_base(
			MR_long_desc_var_locn(label_layout, i),
			saved_regs, base_sp, base_curfr, &succeeded);
	} else {
		if (MR_print_locn) {
			printf("looking up short lval\n");
		}

		*value = MR_lookup_short_lval_base(
			MR_short_desc_var_locn(label_layout, i),
			saved_regs, base_sp, base_curfr, &succeeded);
	}

	return succeeded;
}

MR_bool
MR_get_type(const MR_Label_Layout *label_layout, int i, MR_Word *saved_regs,
	MR_TypeInfo *type_params, MR_TypeInfo *type_info)
{
	return MR_get_type_base(label_layout, i, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info);
}

MR_bool
MR_get_type_base(const MR_Label_Layout *label_layout, int i,
	MR_Word *saved_regs, MR_Word *base_sp, MR_Word *base_curfr,
	MR_TypeInfo *type_params, MR_TypeInfo *type_info)
{
	MR_PseudoTypeInfo	pseudo_type_info;

	pseudo_type_info = MR_var_pti(label_layout, i);
	*type_info = MR_create_type_info(type_params, pseudo_type_info);
	
	return MR_TRUE;
}

void
MR_write_variable(MR_TypeInfo type_info, MR_Word value)
{
	MR_Word	stdout_stream;

	(*MR_io_stdout_stream)(&stdout_stream);
	(*MR_io_print_to_stream)((MR_Word) type_info, stdout_stream, value);
}

void
MR_generate_proc_name_from_layout(const MR_Proc_Layout *proc_layout,
	MR_ConstString *proc_name_ptr, int *arity_ptr, MR_Word *is_func_ptr)
{
	if (MR_PROC_LAYOUT_COMPILER_GENERATED(proc_layout)) {
		*proc_name_ptr = proc_layout->MR_sle_proc_id.
			MR_proc_comp.MR_comp_pred_name;
		*arity_ptr = proc_layout->MR_sle_proc_id.
			MR_proc_comp.MR_comp_arity;
		*is_func_ptr = MR_BOOL_NO;
	} else {
		*proc_name_ptr = proc_layout->MR_sle_proc_id.
			MR_proc_user.MR_user_name;
		*arity_ptr = proc_layout->MR_sle_proc_id.
			MR_proc_user.MR_user_arity;
		if (proc_layout->MR_sle_proc_id.MR_proc_user.
				MR_user_pred_or_func == MR_FUNCTION)
		{
			*is_func_ptr = MR_BOOL_YES;
		} else {
			*is_func_ptr = MR_BOOL_NO;
		}
	}
}
