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
#include "mercury_trace_util.h"

Word	MR_saved_regs[MAX_FAKE_REG];

void
MR_copy_regs_to_saved_regs(int max_mr_num)
{
	/*
	** In the process of browsing, we call Mercury code,
	** which may clobber the contents of the virtual machine registers,
	** both control and general purpose, and both real and virtual
	** registers. We must therefore save and restore these.
	** We store them in the MR_saved_regs array.
	**
	** The call to MR_trace will clobber the transient registers
	** on architectures that have them. The compiler generated code
	** will therefore call save_transient_registers to save the transient
	** registers in the fake_reg array. We here restore them to the
	** real registers, save them with the other registers back in
	** fake_reg, and then copy all fake_reg entries to MR_saved_regs.
	**
	** If any code invoked by MR_trace is itself traced,
	** MR_saved_regs will be overwritten, leading to a crash later on.
	** This is one reason (but not the only one) why we turn off
	** tracing when we call back Mercury code from this file.
	*/

	int i;

	restore_transient_registers();
	save_registers();

	for (i = 0; i <= max_mr_num; i++) {
		MR_saved_regs[i] = MR_fake_reg[i];
	}
}

void
MR_copy_saved_regs_to_regs(int max_mr_num)
{
	/*
	** We execute the converse procedure to MR_copy_regs_to_saved_regs.
	** The save_transient_registers is there so that a call to the
	** restore_transient_registers macro after MR_trace will do the
	** right thing.
	*/

	int i;

	for (i = 0; i <= max_mr_num; i++) {
		MR_fake_reg[i] = MR_saved_regs[i];
	}

	restore_registers();
	save_transient_registers();
}

Word
MR_trace_make_var_list(const MR_Stack_Layout_Label *layout)
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
		**
		** "variables" representing the saved values of succip, hp etc,
		** which are the "variables" for which get_type_and_value
		** fails, are not of interest to the trace analyzer.
		*/

		if ((strncmp(name, "TypeInfo", 8) == 0)
		|| (strncmp(name, "ModuleInfo", 10) == 0)
		|| (strncmp(name, "HLDS", 4) == 0)
		|| !MR_trace_get_type_and_value(var, NULL, &type_info, &value))
		{
			continue;
		}

		/*
		** Create a term of type `univ' to hold the type & value,
		** and cons it onto the list.
		** Note that the calls to save/restore transient registers
		** can't be hoisted out of the loop, because
		** MR_trace_get_type_and_value() calls MR_create_type_info()
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

/* if you want to debug this code, you may want to set this var to TRUE */
static	bool	MR_trace_print_locn = FALSE;

Word
MR_trace_lookup_live_lval(MR_Live_Lval locn, bool *succeeded)
{
	int	locn_num;
	Word	value;

	*succeeded = FALSE;
	value = 0;

	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (MR_trace_print_locn)
				printf("r%d", locn_num);
			value = saved_reg(MR_saved_regs, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_F:
			if (MR_trace_print_locn)
				printf("f%d", locn_num);
			break;

		case MR_LVAL_TYPE_STACKVAR:
			if (MR_trace_print_locn)
				printf("stackvar%d", locn_num);
			value = saved_detstackvar(MR_saved_regs, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			if (MR_trace_print_locn)
				printf("framevar%d", locn_num);
			value = saved_framevar(MR_saved_regs, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_SUCCIP:
			if (MR_trace_print_locn)
				printf("succip");
			break;

		case MR_LVAL_TYPE_MAXFR:
			if (MR_trace_print_locn)
				printf("maxfr");
			break;

		case MR_LVAL_TYPE_CURFR:
			if (MR_trace_print_locn)
				printf("curfr");
			break;

		case MR_LVAL_TYPE_HP:
			if (MR_trace_print_locn)
				printf("hp");
			break;

		case MR_LVAL_TYPE_SP:
			if (MR_trace_print_locn)
				printf("sp");
			break;

		case MR_LVAL_TYPE_UNKNOWN:
			if (MR_trace_print_locn)
				printf("unknown");
			break;

		default:
			if (MR_trace_print_locn)
				printf("DEFAULT");
			break;
	}

	return value;
}

bool
MR_trace_get_type_and_value(const MR_Stack_Layout_Var *var,
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
	*value = MR_trace_lookup_live_lval(var->MR_slv_locn, &succeeded);
	return succeeded;
}
