/*
** Copyright (C) 1998-1999 The University of Melbourne.
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

void
MR_copy_regs_to_saved_regs(int max_mr_num, Word *saved_regs)
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
	** will therefore call save_transient_registers to save the transient
	** registers in the fake_reg array. We here restore them to the
	** real registers, save them with the other registers back in
	** fake_reg, and then copy all fake_reg entries to saved_regs.
	*/

	int i;

	restore_transient_registers();
	save_registers();

	for (i = 0; i <= max_mr_num; i++) {
		saved_regs[i] = MR_fake_reg[i];
	}
}

void
MR_copy_saved_regs_to_regs(int max_mr_num, Word *saved_regs)
{
	/*
	** We execute the converse procedure to MR_copy_regs_to_saved_regs.
	** The save_transient_registers is there so that a call to the
	** restore_transient_registers macro after MR_trace will do the
	** right thing.
	*/

	int i;

	for (i = 0; i <= max_mr_num; i++) {
		MR_fake_reg[i] = saved_regs[i];
	}

	restore_registers();
	save_transient_registers();
}

Word *
MR_materialize_typeinfos(const MR_Stack_Layout_Vars *vars,
	Word *saved_regs)
{
	return MR_materialize_typeinfos_base(vars, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs));
}

Word *
MR_materialize_typeinfos_base(const MR_Stack_Layout_Vars *vars,
	Word *saved_regs, Word *base_sp, Word *base_curfr)
{
	Word	*type_params;
	bool	succeeded;
	Integer	count;
	int	i;

	if (vars->MR_slvs_tvars != NULL) {
		count = vars->MR_slvs_tvars->MR_tp_param_count;
		type_params = MR_NEW_ARRAY(Word, count + 1);

		/*
		** type_params should look like a typeinfo;
		** type_params[0] is empty and will not be referred to
		*/
		for (i = 0; i < count; i++) {
			if (vars->MR_slvs_tvars->MR_tp_param_locns[i] != 0) {
				type_params[i + 1] = MR_lookup_long_lval_base(
					vars->MR_slvs_tvars->
						MR_tp_param_locns[i],
					saved_regs, base_sp, base_curfr,
					&succeeded);
				if (! succeeded) {
					fatal_error("missing type param in MR_materialize_typeinfos_base");
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

/* if you want to debug this code, you may want to set this var to TRUE */
static	bool	MR_print_locn = FALSE;

Word
MR_lookup_long_lval(MR_Long_Lval locn, Word *saved_regs, bool *succeeded)
{
	return MR_lookup_long_lval_base(locn, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		succeeded);
}

Word
MR_lookup_long_lval_base(MR_Long_Lval locn, Word *saved_regs,
	Word *base_sp, Word *base_curfr, bool *succeeded)
{
	int	locn_num;
	int	offset;
	Word	value;
	Word	baseaddr;
	Word	sublocn;

	*succeeded = FALSE;
	value = 0;

	locn_num = (int) MR_LONG_LVAL_NUMBER(locn);
	switch (MR_LONG_LVAL_TYPE(locn)) {
		case MR_LONG_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("r%d", locn_num);
			}
			if (saved_regs != NULL) {
				value = saved_reg(saved_regs, locn_num);
				*succeeded = TRUE;
			}
			break;

		case MR_LONG_LVAL_TYPE_F:
			if (MR_print_locn) {
				printf("f%d", locn_num);
			}
			break;

		case MR_LONG_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("stackvar%d", locn_num);
			}
			value = MR_based_stackvar(base_sp, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LONG_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("framevar%d", locn_num);
			}
			value = MR_based_framevar(base_curfr, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LONG_LVAL_TYPE_SUCCIP:
			if (MR_print_locn) {
				printf("succip");
			}
			break;

		case MR_LONG_LVAL_TYPE_MAXFR:
			if (MR_print_locn) {
				printf("maxfr");
			}
			break;

		case MR_LONG_LVAL_TYPE_CURFR:
			if (MR_print_locn) {
				printf("curfr");
			}
			break;

		case MR_LONG_LVAL_TYPE_HP:
			if (MR_print_locn) {
				printf("hp");
			}
			break;

		case MR_LONG_LVAL_TYPE_SP:
			if (MR_print_locn) {
				printf("sp");
			}
			break;

		case MR_LONG_LVAL_TYPE_INDIRECT:
			offset = MR_LONG_LVAL_INDIRECT_OFFSET(locn_num);
			sublocn = MR_LONG_LVAL_INDIRECT_BASE_LVAL(locn_num);
			if (MR_print_locn) {
				printf("offset %d from ", offset);
			}
			baseaddr = MR_lookup_long_lval_base(sublocn,
					saved_regs, base_sp, base_curfr,
					succeeded);
			if (! *succeeded) {
				break;
			}
			value = MR_typeclass_info_type_info(baseaddr,
				offset);
			*succeeded = TRUE;
			break;

		case MR_LONG_LVAL_TYPE_UNKNOWN:
			if (MR_print_locn) {
				printf("unknown");
			}
			break;

		default:
			if (MR_print_locn) {
				printf("DEFAULT");
			}
			break;
	}

	return value;
}

Word
MR_lookup_short_lval(MR_Short_Lval locn, Word *saved_regs, bool *succeeded)
{
	return MR_lookup_short_lval_base(locn, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		succeeded);
}

Word
MR_lookup_short_lval_base(MR_Short_Lval locn, Word *saved_regs,
	Word *base_sp, Word *base_curfr, bool *succeeded)
{
	int	locn_num;
	Word	value;

	*succeeded = FALSE;
	value = 0;

	locn_num = (int) locn >> MR_SHORT_LVAL_TAGBITS;
	switch (MR_SHORT_LVAL_TYPE(locn)) {
		case MR_SHORT_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("r%d", locn_num);
			}
			if (saved_regs != NULL) {
				value = saved_reg(saved_regs, locn_num);
				*succeeded = TRUE;
			}
			break;

		case MR_SHORT_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("stackvar%d", locn_num);
			}
			value = MR_based_stackvar(base_sp, locn_num);
			*succeeded = TRUE;
			break;

		case MR_SHORT_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("framevar%d", locn_num);
			}
			value = MR_based_framevar(base_curfr, locn_num);
			*succeeded = TRUE;
			break;

		case MR_SHORT_LVAL_TYPE_SPECIAL:
			switch (locn_num) {
				case MR_LONG_LVAL_TYPE_SUCCIP:
					if (MR_print_locn) {
						printf("succip");
					}
					break;

				case MR_LONG_LVAL_TYPE_MAXFR:
					if (MR_print_locn) {
						printf("maxfr");
					}
					break;

				case MR_LONG_LVAL_TYPE_CURFR:
					if (MR_print_locn) {
						printf("curfr");
					}
					break;

				case MR_LONG_LVAL_TYPE_HP:
					if (MR_print_locn) {
						printf("hp");
					}
					break;

				case MR_LONG_LVAL_TYPE_SP:
					if (MR_print_locn) {
						printf("sp");
					}
					break;

				default:
					if (MR_print_locn) {
						printf("DEFAULT");
					}
			}

		default:
			if (MR_print_locn) {
				printf("DEFAULT");
			}
			break;
	}

	return value;
}

bool
MR_get_type_and_value(const MR_Stack_Layout_Vars *vars, int i,
	Word *saved_regs, Word *type_params, Word *type_info, Word *value)
{
	return MR_get_type_and_value_base(vars, i, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info, value);
}

bool
MR_get_type_and_value_base(const MR_Stack_Layout_Vars *vars, int i,
	Word *saved_regs, Word *base_sp, Word *base_curfr,
	Word *type_params, Word *type_info, Word *value)
{
	bool	succeeded;
	Word	*pseudo_type_info;

	pseudo_type_info = MR_var_pti(vars, i);
	*type_info = (Word) MR_create_type_info(type_params, pseudo_type_info);

	if (i < MR_long_desc_var_count(vars)) {
		*value = MR_lookup_long_lval_base(
			MR_long_desc_var_locn(vars, i),
			saved_regs, base_sp, base_curfr, &succeeded);
	} else {
		*value = MR_lookup_short_lval_base(
			MR_short_desc_var_locn(vars, i),
			saved_regs, base_sp, base_curfr, &succeeded);
	}

	return succeeded;
}

bool
MR_get_type(const MR_Stack_Layout_Vars *vars, int i, Word *saved_regs,
	Word *type_params, Word *type_info)
{
	return MR_get_type_base(vars, i, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info);
}

bool
MR_get_type_base(const MR_Stack_Layout_Vars *vars, int i,
	Word *saved_regs, Word *base_sp, Word *base_curfr,
	Word *type_params, Word *type_info)
{
	Word	*pseudo_type_info;

	pseudo_type_info = MR_var_pti(vars, i);
	*type_info = (Word) MR_create_type_info(type_params, pseudo_type_info);
	
	return TRUE;
}

void
MR_write_variable(Word type_info, Word value)
{
	Word	stdout_stream;

	(*MR_io_stdout_stream)(&stdout_stream);
	(*MR_io_print_to_stream)(type_info, stdout_stream, value);
}
