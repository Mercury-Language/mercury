/*
** Copyright (C) 1998 The University of Melbourne.
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
	int	count;
	int	i;

	if (vars->MR_slvs_tvars != NULL) {
		count = (int) (Integer) vars->MR_slvs_tvars[0];
		type_params = checked_malloc((count + 1) * sizeof(Word));

		/*
		** type_params should look like a typeinfo;
		** type_params[0] is empty and will not be referred to
		*/
		for (i = 1; i <= count; i++) {
			if (vars->MR_slvs_tvars[i] != 0) {
				type_params[i] = MR_lookup_live_lval_base(
					vars->MR_slvs_tvars[i],
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

Word
MR_make_var_list(const MR_Stack_Layout_Label *layout, Word *saved_regs)
{
	int 				var_count;
	const MR_Stack_Layout_Vars 	*vars;
	int				i;
	const char			*name;

	Word				univ_list;
	MR_Stack_Layout_Var*		var;
	Word				univ, value;
	MR_Live_Type			live_type;
	Word				type_info;

	var_count = layout->MR_sll_var_count;
	vars = &layout->MR_sll_var_info;

	/* build up the live variable list, starting from the end */
	restore_transient_registers();
	univ_list = list_empty();
	save_transient_registers();
	for (i = var_count - 1; i >= 0; i--) {
		/*
		** Look up the name, the type and value
		** (XXX we don't include the name or the inst
		** in the list that we return)
		*/

		name = MR_name_if_present(vars, i);
		var = &vars->MR_slvs_pairs[i];

		/*
		** XXX The printing of type_infos is buggy at the moment
		** due to the fake arity of private_builtin:typeinfo/1.
		**
		** XXX The printing of large data structures is painful
		** at the moment due to the lack of a true browser.
		*/

		if (! MR_get_type_and_value_filtered(var, saved_regs,
			name, &type_info, &value))
		{
			continue;
		}

		/*
		** Create a term of type `univ' to hold the type & value,
		** and cons it onto the list.
		** Note that the calls to save/restore transient registers
		** can't be hoisted out of the loop, because
		** MR_get_type_and_value() calls MR_create_type_info()
		** which may allocate memory using incr_saved_hp.
		*/

		restore_transient_registers();
		incr_hp(univ, 2);
		field(mktag(0), univ, UNIV_OFFSET_FOR_TYPEINFO) = type_info;
		field(mktag(0), univ, UNIV_OFFSET_FOR_DATA) = value;
		
		univ_list = list_cons(univ, univ_list);
		save_transient_registers();
	}

	return univ_list;
}

int
MR_get_register_number(MR_Live_Lval locn)
{
	if (MR_LIVE_LVAL_TYPE(locn) == MR_LVAL_TYPE_R) {
		return MR_LIVE_LVAL_NUMBER(locn);
	} else {
		return -1;
	}
}

/* if you want to debug this code, you may want to set this var to TRUE */
static	bool	MR_print_locn = FALSE;

Word
MR_lookup_live_lval(MR_Live_Lval locn, Word *saved_regs, bool *succeeded)
{
	return MR_lookup_live_lval_base(locn, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		succeeded);
}

Word
MR_lookup_live_lval_base(MR_Live_Lval locn, Word *saved_regs,
	Word *base_sp, Word *base_curfr, bool *succeeded)
{
	int	locn_num;
	Word	value;

	*succeeded = FALSE;
	value = 0;

	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (MR_print_locn) {
				printf("r%d", locn_num);
			}
			if (saved_regs != NULL) {
				value = saved_reg(saved_regs, locn_num);
				*succeeded = TRUE;
			}
			break;

		case MR_LVAL_TYPE_F:
			if (MR_print_locn) {
				printf("f%d", locn_num);
			}
			break;

		case MR_LVAL_TYPE_STACKVAR:
			if (MR_print_locn) {
				printf("stackvar%d", locn_num);
			}
			value = MR_based_stackvar(base_sp, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			if (MR_print_locn) {
				printf("framevar%d", locn_num);
			}
			value = MR_based_framevar(base_curfr, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_SUCCIP:
			if (MR_print_locn) {
				printf("succip");
			}
			break;

		case MR_LVAL_TYPE_MAXFR:
			if (MR_print_locn) {
				printf("maxfr");
			}
			break;

		case MR_LVAL_TYPE_CURFR:
			if (MR_print_locn) {
				printf("curfr");
			}
			break;

		case MR_LVAL_TYPE_HP:
			if (MR_print_locn) {
				printf("hp");
			}
			break;

		case MR_LVAL_TYPE_SP:
			if (MR_print_locn) {
				printf("sp");
			}
			break;

		case MR_LVAL_TYPE_UNKNOWN:
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

bool
MR_get_type_and_value(const MR_Stack_Layout_Var *var,
	Word *saved_regs, Word *type_params, Word *type_info, Word *value)
{
	return MR_get_type_and_value_base(var, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info, value);
}

bool
MR_get_type_and_value_base(const MR_Stack_Layout_Var *var,
	Word *saved_regs, Word *base_sp, Word *base_curfr,
	Word *type_params, Word *type_info, Word *value)
{
	bool	succeeded;
	Word	*pseudo_type_info;
	int	i;

	if (!MR_LIVE_TYPE_IS_VAR(var->MR_slv_live_type)) {
		return FALSE;
	}

	pseudo_type_info = MR_LIVE_TYPE_GET_VAR_TYPE(var->MR_slv_live_type);
	*type_info = (Word) MR_create_type_info(type_params, pseudo_type_info);
	*value = MR_lookup_live_lval_base(var->MR_slv_locn,
		saved_regs, base_sp, base_curfr, &succeeded);
	return succeeded;
}

bool
MR_get_type(const MR_Stack_Layout_Var *var, Word *saved_regs,
	Word *type_params, Word *type_info)
{
	return MR_get_type_base(var, saved_regs,
		MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
		type_params, type_info);
}

bool
MR_get_type_base(const MR_Stack_Layout_Var *var,
	Word *saved_regs, Word *base_sp, Word *base_curfr,
	Word *type_params, Word *type_info)
{
	bool	succeeded;
	Word	*pseudo_type_info;
	int	i;

	if (!MR_LIVE_TYPE_IS_VAR(var->MR_slv_live_type)) {
		return FALSE;
	}

	pseudo_type_info = MR_LIVE_TYPE_GET_VAR_TYPE(var->MR_slv_live_type);
	*type_info = (Word) MR_create_type_info(type_params, pseudo_type_info);
	
	return TRUE;
}

/*
** get_type_and_value() and get_type() will succeed to retrieve "variables"
** that we do not want to send to the user; "variables" beginning with
** `ModuleInfo', or `HLDS' may occur when debugging the compiler and are too
** big to be displayed.  "Variables" beginning with `TypeInfo' denote the
** additional parameters introduced by compiler/polymorphism.m that we don't
** want to show neither.
** That's why we define filtered version of get_type_and_value() and get_type()
** that will fail to retrieve such variables.
*/

bool
MR_get_type_and_value_filtered(const MR_Stack_Layout_Var *var, 
	Word *saved_regs, const char *name, Word *type_info, Word *value)
{
	return ((strncmp(name, "TypeInfo", 8) != 0)
	       && (strncmp(name, "ModuleInfo", 10) != 0)
	       && (strncmp(name, "HLDS", 4) != 0)
	       && MR_get_type_and_value(var, saved_regs, NULL,
			type_info, value));
}


bool
MR_get_type_filtered(const MR_Stack_Layout_Var *var, Word *saved_regs,
	const char *name, Word *type_info)
{
	return ((strncmp(name, "TypeInfo", 8) != 0)
	       && (strncmp(name, "ModuleInfo", 10) != 0)
	       && (strncmp(name, "HLDS", 4) != 0)
	       && MR_get_type(var, saved_regs, NULL, type_info));
}

void
MR_write_variable(Word type_info, Word value)
{
	Word	stdout_stream;

	(*MR_io_stdout_stream)(&stdout_stream);
	(*MR_io_print_to_stream)(type_info, stdout_stream, value);
}
