/*
** Copyright (C) 1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the code for managing information about the
** variables of the program being debugged for both the internal
** and external debuggers.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_memory.h"
#include "mercury_layout_util.h"
#include "mercury_stack_layout.h"
#include "mercury_trace_vars.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
** This structure contains all the debugger's information about a variable.
**
** The fullname field obviously contains the variable's full name.
** If this name ends with a sequence of digits, then the basename field will
** contain the name of the variable minus those digits, the num_suffix field
** will contain the numeric value of this sequence of digits, and the
** has_suffix field will be set to true. If the full name does not end with
** a sequence of digits, then the basename field will contain the same string
** as the fullname field, and the has_suffix field will be set to false
** (the num_suffix field will not contain anything meaningful).
**
** The is_headvar field will be set to true iff the basename of the variable
** is HeadVar__; such variables are always listed before other variables.
** The is_ambiguous field will be set iff the full name of the variable
** does not uniquely identify it among all the variables live at the
** current point. What *is* guaranteed to uniquely identify a variable
** is its HLDS number, which will be in the hlds_number field.
**
** The last two fields contain the value of the variable and the typeinfo
** describing the type of this value.
*/

typedef struct {
	char				*MR_var_fullname;
	char				*MR_var_basename;
	int				MR_var_num_suffix;
	bool				MR_var_has_suffix;
	bool				MR_var_is_headvar;
	bool				MR_var_is_ambiguous;
	int				MR_var_hlds_number;
	Word				MR_var_type;
	Word				MR_var_value;
} MR_Var_Details;

/*
** This structure contains all of the debugger's information about
** all the variables that are live at the current program point,
** where a program point is defined as the combination of a debugger
** event and an ancestor level.
**
** The top_layout and top_saved_regs fields together describe the abstract
** machine state at the current debugger event. The problem field points 
** to a string containing an error message describing why the debugger
** can't print any variables at the current point. It will of course be
** NULL if the debugger can do so, which requires not only that the
** debugger have all the information it needs about the current point.
** Since the debugger doesn't allow the setting of the ancestor level
** to a given value if the selected point is missing any of the required
** information, the problem field can only be non-NULL if the ancestor
** level is zero (i.e. the point at the event itself is already missing
** some required info).
**
** The level_entry field contains the proc layout structure of the
** procedure at the selected ancestor level, and the level_base_sp and
** level_base_curfr fields contain the values appropriate for addressing
** the stack frame of the selected invocation of this procedure. This
** information is useful in looking up e.g. the call number of this invocation.
**
** The var_count field says how many variables are live at the current
** point. This many of the elements of the vars array are valid.
** The number of elements of the vars array for which space has been
** reserved is held in var_max.
*/

typedef struct {
	const MR_Stack_Layout_Label	*MR_point_top_layout;
	Word				*MR_point_top_saved_regs;
	const char			*MR_point_problem;
	int				MR_point_level;
	const MR_Stack_Layout_Entry	*MR_point_level_entry;
	Word				*MR_point_level_base_sp;
	Word				*MR_point_level_base_curfr;
	int				MR_point_var_count;
	int				MR_point_var_max;
	MR_Var_Details			*MR_point_vars;
} MR_Point;

static	bool	MR_trace_type_is_ignored(Word type_info);
static	int	MR_trace_compare_var_details(const void *arg1,
			const void *arg2);
static	void	MR_trace_browse_var(FILE *out, MR_Var_Details *var,
			MR_Browser browser);
static	int	MR_trace_print_var_name(FILE *out, MR_Var_Details *var);

#define	MR_INIT_VAR_DETAIL_COUNT	20
#define	MR_TRACE_PADDED_VAR_NAME_LENGTH	23

static	MR_Point			MR_point;

/*
** These extern declarations are necessary because the modules defining
** these structures (some which are in Mercury and some of which are in C)
** do not export them. The types are a lie, but a safe lie.
*/

extern	Word	mercury_data_private_builtin__type_ctor_info_type_info_1;
extern	Word	mercury_data_private_builtin__type_ctor_info_type_ctor_info_1;
extern	Word	mercury_data_private_builtin__type_ctor_info_typeclass_info_1;
extern	Word	mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1;
extern	Word	mercury_data___type_ctor_info_func_0;
extern	Word	mercury_data___type_ctor_info_pred_0;
extern	Word	mercury_data___type_ctor_info_void_0;

#ifdef	NATIVE_GC
extern	Word	mercury_data___type_ctor_info_succip_0;
extern	Word	mercury_data___type_ctor_info_hp_0;
extern	Word	mercury_data___type_ctor_info_curfr_0;
extern	Word	mercury_data___type_ctor_info_maxfr_0;
extern	Word	mercury_data___type_ctor_info_redoip_0;
extern	Word	mercury_data___type_ctor_info_redofr_0;
#endif

static	Word *
MR_trace_ignored_type_ctors[] =
{
	/* we ignore these until the debugger can handle their varying arity */
	(Word *) &mercury_data_private_builtin__type_ctor_info_type_info_1,
	(Word *) &mercury_data_private_builtin__type_ctor_info_type_ctor_info_1,
	(Word *) &mercury_data_private_builtin__type_ctor_info_typeclass_info_1,
	(Word *) &mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1,

	/* we ignore these until the debugger can print higher-order terms */
	(Word *) &mercury_data___type_ctor_info_func_0,
	(Word *) &mercury_data___type_ctor_info_pred_0,

	/* we ignore these because they should never be needed */
	(Word *) &mercury_data___type_ctor_info_void_0,

#ifdef	NATIVE_GC
	/* we ignore these because they are not interesting */
	(Word *) &mercury_data___type_ctor_info_succip_0,
	(Word *) &mercury_data___type_ctor_info_hp_0,
	(Word *) &mercury_data___type_ctor_info_curfr_0,
	(Word *) &mercury_data___type_ctor_info_maxfr_0,
	(Word *) &mercury_data___type_ctor_info_redoip_0,
	(Word *) &mercury_data___type_ctor_info_redofr_0,
#endif
};

static bool
MR_trace_type_is_ignored(Word type_info_as_word)
{
	Word	*type_info;
	Word	*type_ctor_info;
	int	ignore_type_ctor_count;
	int	i;

	type_info = (Word *) type_info_as_word;

	if (type_info[OFFSET_FOR_COUNT] == 0) {
		type_ctor_info = type_info;
	} else {
		type_ctor_info = (Word *) type_info[0];
	}

	ignore_type_ctor_count = sizeof(MR_trace_ignored_type_ctors)
					/ sizeof(Word *);

	for (i = 0; i < ignore_type_ctor_count; i++) {
		if (type_ctor_info == MR_trace_ignored_type_ctors[i]) {
			return TRUE;
		}
	}

	return FALSE;
}

void
MR_trace_init_point_vars(const MR_Stack_Layout_Label *top_layout,
	Word *saved_regs)
{
	MR_point.MR_point_top_layout = top_layout;
	MR_point.MR_point_top_saved_regs = saved_regs;
	MR_point.MR_point_level = 0;
	MR_point.MR_point_problem = MR_trace_set_level(0);
}

const char *
MR_trace_set_level(int ancestor_level)
{
	const char			*problem;
	Word				*base_sp;
	Word				*base_curfr;
	const MR_Stack_Layout_Label	*top_layout;
	const MR_Stack_Layout_Label	*level_layout;
	const MR_Stack_Layout_Entry	*entry;
	const MR_Stack_Layout_Vars	*vars;
	const MR_Var_Name		*var_info;
	Word				*valid_saved_regs;
	int				var_count;
	Word				*type_params;
	Word				value;
	Word				type_info;
	int				i;
	int				slot;
	int				slot_max;
	int				copylen;
	char				*copy;
	char				*s;
	const char			*name;
	const char			*string_table;
	Integer				string_table_size;

	problem = NULL;
	top_layout = MR_point.MR_point_top_layout;
	base_sp = MR_saved_sp(MR_point.MR_point_top_saved_regs);
	base_curfr = MR_saved_curfr(MR_point.MR_point_top_saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
			&base_sp, &base_curfr, &problem);

	if (level_layout != NULL) {
		entry = level_layout->MR_sll_entry;
		if (! MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
			return "this procedure does not have "
				"debugging information";
		}
	} else {
		if (problem == NULL) {
			fatal_error("MR_find_nth_ancestor failed "
					"without reporting a problem");
		}

		return problem;
	}

	vars = &level_layout->MR_sll_var_info;
	if (! MR_has_valid_var_count(vars)) {
		return "there is no information about live variables";
	}

	/*
	** After this point, we cannot find any more problems
	** that would prevent us from assembling an accurate picture
	** of the set of live variables at the given level,
	** so we are free to modify the MR_point structure.
	*/

	MR_point.MR_point_problem = NULL;
	MR_point.MR_point_level = ancestor_level;
	MR_point.MR_point_level_entry = entry;
	MR_point.MR_point_level_base_sp = base_sp;
	MR_point.MR_point_level_base_curfr = base_curfr;

	if (MR_has_valid_var_info(vars)) {
		var_count = MR_all_desc_var_count(vars);
	} else {
		/*
		** If the count of variables is zero, then the rest of the
		** information about the set of live variables (e.g. the
		** type parameter array pointer) is not present. Continuing
		** would therefore lead to a core dump.
		**
		** Instead, we set up the remaining meaningful fields
		** of MR_point.
		*/

		MR_point.MR_point_var_count = 0;
		return NULL;
	}

	if (vars->MR_slvs_names == NULL) {
		return "there are no names for the live variables";
	}

	if (ancestor_level == 0) {
		valid_saved_regs = MR_point.MR_point_top_saved_regs;
	} else {
		valid_saved_regs = NULL;
	}

	type_params = MR_materialize_typeinfos_base(vars,
				valid_saved_regs, base_sp, base_curfr);

	MR_ensure_big_enough(var_count, MR_point.MR_point_var, 
		MR_Var_Details, MR_INIT_VAR_DETAIL_COUNT);

	for (slot = 0; slot < MR_point.MR_point_var_count; slot++) {
		/* free the memory allocated by previous MR_copy_string */
		free(MR_point.MR_point_vars[slot].MR_var_fullname);
		free(MR_point.MR_point_vars[slot].MR_var_basename);
	}

	string_table = entry->MR_sle_module_layout->MR_ml_string_table;
	string_table_size =
		entry->MR_sle_module_layout->MR_ml_string_table_size;

	slot = 0;
	for (i = 0; i < var_count; i++) {
		var_info = &vars->MR_slvs_names[i];

		if (var_info->MR_var_num == 0) {
			/* this value is not a variable */
			continue;
		}

		if (var_info->MR_var_name_offset > string_table_size) {
			fatal_error("array bounds error on string table");
		}

		name = string_table + var_info->MR_var_name_offset;
		if (name == NULL || streq(name, "")) {
			/* this value is a compiler-generated variable */
			continue;
		}

		if (! MR_get_type_and_value_base(vars, i, valid_saved_regs,
			base_sp, base_curfr, type_params, &type_info, &value))
		{
			/* this value is not a variable */
			continue;
		}

		if (MR_trace_type_is_ignored(type_info)) {
			continue;
		}

		MR_point.MR_point_vars[slot].MR_var_hlds_number =
			var_info->MR_var_num;

		copy = MR_copy_string(name);
		MR_point.MR_point_vars[slot].MR_var_fullname = copy;
		MR_point.MR_point_vars[slot].MR_var_type = type_info;
		MR_point.MR_point_vars[slot].MR_var_value = value;

		/* we need another copy we can cut apart */
		copy = MR_copy_string(name);
		copylen = strlen(copy);
		s = copy + copylen - 1;
		while (s > copy && MR_isdigit(*s)) {
			s--;
		}

		if (s == copy + copylen - 1) {
			MR_point.MR_point_vars[slot].MR_var_has_suffix = FALSE;
			/* num_suffix should not be used */
			MR_point.MR_point_vars[slot].MR_var_num_suffix = -1;
			MR_point.MR_point_vars[slot].MR_var_basename = copy;
		} else {
			if (MR_isdigit(*s)) {
				fatal_error("variable name starts with digit");
			}

			MR_point.MR_point_vars[slot].MR_var_has_suffix = TRUE;
			MR_point.MR_point_vars[slot].MR_var_num_suffix
				= atoi(s + 1);
			*(s + 1) = '\0';
			MR_point.MR_point_vars[slot].MR_var_basename = copy;
		}

		if (streq(MR_point.MR_point_vars[slot].MR_var_basename,
			"HeadVar__"))
		{
			MR_point.MR_point_vars[slot].MR_var_is_headvar = TRUE;
		} else {
			MR_point.MR_point_vars[slot].MR_var_is_headvar = FALSE;
		}

		MR_point.MR_point_vars[slot].MR_var_is_ambiguous = FALSE;
		slot++;
	}

	slot_max = slot;
	free(type_params);

	if (slot_max > 0) {
		qsort(MR_point.MR_point_vars, slot_max,
			sizeof(MR_Var_Details),
			MR_trace_compare_var_details);

		slot = 1;
		for (i = 1; i < slot_max; i++) {
			if (MR_point.MR_point_vars[i].MR_var_hlds_number ==
				MR_point.MR_point_vars[i-1].MR_var_hlds_number)
			{
				continue;
			}

			MR_memcpy(&MR_point.MR_point_vars[slot],
				&MR_point.MR_point_vars[i],
				sizeof(MR_Var_Details));

			if (streq(MR_point.MR_point_vars[slot].MR_var_fullname,
				MR_point.MR_point_vars[slot-1].MR_var_fullname))
			{
				MR_point.MR_point_vars[slot - 1].
					MR_var_is_ambiguous = TRUE;
				MR_point.MR_point_vars[slot].
					MR_var_is_ambiguous = TRUE;
			}

			slot++;
		}

		slot_max = slot;
	}

	MR_point.MR_point_var_count = slot_max;
	return NULL;
}

/*
** This comparison function is used to sort variables
**
**	- first on basename,
**	- then on suffix,
**	- and then, if necessary, on HLDS number.
**
** The sorting on basenames is alphabetical except for head variables,
** which always come out first.
**
** The sorting on suffixes orders variables with the same basename
** so that they come out in order of numerically increasing suffix,
** with any variable sharing the same name but without a numeric suffix
** coming out last.
*/

static int
MR_trace_compare_var_details(const void *arg1, const void *arg2)
{
	MR_Var_Details	*var1;
	MR_Var_Details	*var2;
	int		diff;

	var1 = (MR_Var_Details *) arg1;
	var2 = (MR_Var_Details *) arg2;

	if (var1->MR_var_is_headvar && ! var2->MR_var_is_headvar) {
		return -1;
	} else if (! var1->MR_var_is_headvar && var2->MR_var_is_headvar) {
		return 1;
	}

	diff = strcmp(var1->MR_var_basename, var2->MR_var_basename);
	if (diff != 0) {
		return diff;
	}

	if (var1->MR_var_has_suffix && ! var2->MR_var_has_suffix) {
		return -1;
	} else if (! var1->MR_var_has_suffix && var2->MR_var_has_suffix) {
		return 1;
	}

	diff = var1->MR_var_num_suffix - var2->MR_var_num_suffix;
	if (diff != 0) {
		return diff;
	}

	return var1->MR_var_hlds_number - var2->MR_var_hlds_number;
}

int
MR_trace_current_level(void)
{
	return MR_point.MR_point_level;
}

void
MR_trace_current_level_details(const MR_Stack_Layout_Entry **entry_ptr,
	Word **base_sp_ptr, Word **base_curfr_ptr)
{
	if (MR_point.MR_point_problem != NULL) {
		fatal_error("cannot get details about current level");
	}

	if (entry_ptr != NULL) {
		*entry_ptr = MR_point.MR_point_level_entry;
	}

	if (base_sp_ptr != NULL) {
		*base_sp_ptr = MR_point.MR_point_level_base_sp;
	}

	if (base_curfr_ptr != NULL) {
		*base_curfr_ptr = MR_point.MR_point_level_base_curfr;
	}
}

int
MR_trace_var_count(void)
{
	if (MR_point.MR_point_problem != NULL) {
		return -1;
	}

	return MR_point.MR_point_var_count;
}

const char *
MR_trace_list_vars(FILE *out)
{
	int	i;

	if (MR_point.MR_point_problem != NULL) {
		return MR_point.MR_point_problem;
	}

	for (i = 0; i < MR_point.MR_point_var_count; i++) {
		fprintf(out, "%9d ", i + 1);
		MR_trace_print_var_name(out, &MR_point.MR_point_vars[i]);
		fprintf(out, "\n");
	}

	return NULL;
}

const char *
MR_trace_return_var_info(int var_number, const char **name_ptr,
	Word *type_info_ptr, Word *value_ptr)
{
	const MR_Var_Details *details;

	if (MR_point.MR_point_problem != NULL) {
		return MR_point.MR_point_problem;
	}

	if (var_number < 1) {
		return "invalid variable number";
	}
	if (var_number > MR_point.MR_point_var_count) {
		return "there aren't that many variables";
	}

	details = &MR_point.MR_point_vars[var_number - 1];
	
	if (name_ptr != NULL) {
		*name_ptr = details->MR_var_fullname;
	}
	if (type_info_ptr != NULL) {
		*type_info_ptr = details->MR_var_type;
	}
	if (value_ptr != NULL) {
		*value_ptr = details->MR_var_value;
	}

	return NULL;
}

const char *
MR_trace_browse_one(FILE *out, MR_Var_Spec var_spec, MR_Browser browser,
	bool must_be_unique)
{
	int	i;
	bool	found;

	if (MR_point.MR_point_problem != NULL) {
		return MR_point.MR_point_problem;
	}

	if (var_spec.MR_var_spec_kind == MR_VAR_SPEC_NUMBER) {
		if (var_spec.MR_var_spec_number < 1) {
			return "invalid variable number";
		}
		if (var_spec.MR_var_spec_number > MR_point.MR_point_var_count)
		{
			return "there aren't that many variables";
		}
		MR_trace_browse_var(out, &MR_point.MR_point_vars
			[var_spec.MR_var_spec_number - 1], browser);
	} else if (var_spec.MR_var_spec_kind == MR_VAR_SPEC_NAME) {
		found = FALSE;
		for (i = 0; i < MR_point.MR_point_var_count; i++) {
			if (streq(var_spec.MR_var_spec_name,
				MR_point.MR_point_vars[i].MR_var_fullname))
			{
				found = TRUE;
				break;
			}
		}

		if (!found) {
			return "there is no such variable";
		}

		if (MR_point.MR_point_vars[i].MR_var_is_ambiguous) {
			if (must_be_unique) {
				return "variable name is not unique";
			}

			do {
				MR_trace_browse_var(out,
					&MR_point.MR_point_vars[i], browser);
				i++;
			} while (i < MR_point.MR_point_var_count &&
				streq(var_spec.MR_var_spec_name,
				MR_point.MR_point_vars[i].MR_var_fullname));
		} else {
			MR_trace_browse_var(out, &MR_point.MR_point_vars[i],
				browser);
		}
	} else {
		fatal_error("internal error: bad var_spec kind");
	}

	return NULL;
}

const char *
MR_trace_browse_all(FILE *out, MR_Browser browser)
{
	int				i;

	if (MR_point.MR_point_problem != NULL) {
		return MR_point.MR_point_problem;
	}

	if (MR_point.MR_point_var_count == 0 && out != NULL) {
		fprintf(out, "mdb: there are no live variables.\n");
	}

	for (i = 0; i < MR_point.MR_point_var_count; i++) {
		MR_trace_browse_var(out, &MR_point.MR_point_vars[i], browser);
	}

	return NULL;
}

static void
MR_trace_browse_var(FILE *out, MR_Var_Details *var, MR_Browser browser)
{
	int	len;

	if (out != NULL) {
		/*
		** The initial blanks are to visually separate
		** the variable names from the prompt.
		*/

		fprintf(out, "%7s", "");
		len = MR_trace_print_var_name(out, var);
		while (len < MR_TRACE_PADDED_VAR_NAME_LENGTH) {
			fputc(' ', out);
			len++;
		}

		/*
		** We flush the output in case the browser is interactive.
		** XXX we should pass out (and in, and err) to the browser.
		*/

		fflush(out);
	}

	(*browser)(var->MR_var_type, var->MR_var_value);
}

static int
MR_trace_print_var_name(FILE *out, MR_Var_Details *var)
{
	int	len;

	len = strlen(var->MR_var_fullname);
	fputs(var->MR_var_fullname, out);
	if (var->MR_var_is_ambiguous) {
		char	buf[256]; /* this should be plenty big enough */

		sprintf(buf, "(%d)", var->MR_var_hlds_number);
		len += strlen(buf);
		fputs(buf, out);
	}

	return len;
}
