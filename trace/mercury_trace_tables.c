/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file manages a table listing the debuggable modules of the program,
** and subsidiary tables listing the procedures of each of those modules.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_label.h"
#include "mercury_array_macros.h"
#include "mercury_stack_trace.h"

#include "mercury_trace_tables.h"
#include "mercury_trace.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

static	const MR_Module_Layout	**MR_module_infos;
static	int			MR_module_info_next = 0;
static	int			MR_module_info_max  = 0;
static	int			MR_module_info_proc_count = 0;

#define	INIT_MODULE_TABLE_SIZE	10

static	const MR_Module_Layout	*MR_search_module_info(const char *name);
static	void	MR_insert_module_info(const MR_Module_Layout *);
static	void	MR_process_matching_procedures_in_module(
			const MR_Module_Layout *module, MR_Proc_Spec *spec,
			void f(void *, const MR_Proc_Layout *), void *);
static	void	MR_process_line_layouts(const MR_Module_File_Layout
			*file_layout, int line,
			MR_file_line_callback callback_func, int callback_arg);

static MR_bool MR_parse_trailing_number(char *start, char **end, int *number);

void
MR_register_all_modules_and_procs(FILE *fp, MR_bool verbose)
{
	static	MR_bool	done = MR_FALSE;

	if (! done) {
		if (verbose) {
			fprintf(fp, "Registering debuggable procedures... ");
			fflush(fp);
		}

		MR_trace_init_modules();
		done = MR_TRUE;
		if (verbose) {
			fprintf(fp, "done.\n");
			if (MR_module_info_next == 0) {
				fprintf(fp,
					"There are no debuggable modules.\n");
			} else if (MR_module_info_next == 1) {
				fprintf(fp, "There is one debuggable module, "
					"with %d procedures.\n",
					MR_module_info_proc_count);
			} else {
				fprintf(fp, "There are %d debuggable modules, "
					"with a total of %d procedures.\n",
					MR_module_info_next,
					MR_module_info_proc_count);
			}
		}
	}
}

void
MR_register_module_layout_real(const MR_Module_Layout *module)
{
	/*
	** MR_register_module_layout_real should only be called from
	** the initialization function of a module, which should be
	** called only once. The check here whether the module layout
	** already exists in the table is really only for paranoia.
	*/

	if (MR_search_module_info(module->MR_ml_name) == NULL) {
		MR_insert_module_info(module);
	}
}

static const MR_Module_Layout *
MR_search_module_info(const char *name)
{
	int	slot;
	MR_bool	found;

	MR_bsearch(MR_module_info_next, slot, found,
		strcmp(MR_module_infos[slot]->MR_ml_name, name));
	if (found) {
		return MR_module_infos[slot];
	} else {
		return NULL;
	}
}

static void
MR_insert_module_info(const MR_Module_Layout *module)
{
	int	slot;

	MR_ensure_room_for_next(MR_module_info, const MR_Module_Layout *,
		INIT_MODULE_TABLE_SIZE);
	MR_prepare_insert_into_sorted(MR_module_infos, MR_module_info_next,
		slot,
		strcmp(MR_module_infos[slot]->MR_ml_name, module->MR_ml_name));

	MR_module_infos[slot] = module;
	MR_module_info_proc_count += module->MR_ml_proc_count;
}

void
MR_process_file_line_layouts(const char *file, int line,
	MR_file_line_callback callback_func, int callback_arg)
{
	int				i, j;
	const MR_Module_File_Layout	*file_layout;

	for (i = 0; i < MR_module_info_next; i++) {
		for (j = 0; j < MR_module_infos[i]->MR_ml_filename_count; j++)
		{
			file_layout = MR_module_infos[i]->
					MR_ml_module_file_layout[j];
			if (MR_streq(file_layout->MR_mfl_filename, file)) {
				MR_process_line_layouts(file_layout, line,
					callback_func, callback_arg);
			}
		}
	}
}

static void
MR_process_line_layouts(const MR_Module_File_Layout *file_layout, int line,
	MR_file_line_callback callback_func, int callback_arg)
{
	int			k;
	MR_bool			found;

	MR_bsearch(file_layout->MR_mfl_label_count, k, found,
		file_layout->MR_mfl_label_lineno[k] - line);

	if (found) {
		/*
		** The binary search found *one* label with the given
		** linenumber; we now find the *first* such label.
		*/

		while (k > 0
			&& file_layout->MR_mfl_label_lineno[k - 1] == line)
		{
			k--;
		}

		while (k < file_layout->MR_mfl_label_count
			&& file_layout->MR_mfl_label_lineno[k] == line)
		{
			(*callback_func)(file_layout->MR_mfl_label_layout[k],
				callback_arg);
			k++;
		}
	}
}

void
MR_dump_module_tables(FILE *fp)
{
	int	i, j;

	for (i = 0; i < MR_module_info_next; i++) {
		fprintf(fp, "====================\n");
		fprintf(fp, "module %s\n", MR_module_infos[i]->MR_ml_name);
		fprintf(fp, "====================\n");
		for (j = 0; j < MR_module_infos[i]->MR_ml_proc_count; j++) {
			MR_print_proc_id_for_debugger(fp,
				MR_module_infos[i]->MR_ml_procs[j]);
		}
	}
}

void
MR_dump_module_list(FILE *fp)
{
	int			i;

	fprintf(fp, "List of debuggable modules\n\n");
	for (i = 0; i < MR_module_info_next; i++) {
		fprintf(fp, "%s\n", MR_module_infos[i]->MR_ml_name);
	}
}

void
MR_dump_module_procs(FILE *fp, const char *name)
{
	const MR_Module_Layout		*module;
	int				j;

	module = MR_search_module_info(name);
	if (module == NULL) {
		fprintf(fp, "There is no debugging info about module `%s'\n",
				name);
	} else {
		fprintf(fp, "List of procedures in module `%s'\n\n", name);
		for (j = 0; j < module->MR_ml_proc_count; j++) {
			MR_print_proc_id_for_debugger(fp,
				module->MR_ml_procs[j]);
		}
	}
}

MR_bool
MR_parse_proc_spec(char *str, MR_Proc_Spec *spec)
{
	char	*dash;
	char	*start;
	char	*end;
	int	n;
	int	len;
	int	double_underscores;
	MR_bool	found;

	spec->MR_proc_module = NULL;
	spec->MR_proc_name   = NULL;
	spec->MR_proc_arity  = -1;
	spec->MR_proc_mode   = -1;
	spec->MR_proc_pf     = (MR_PredFunc) -1;

	len = strlen(str);

	/*
	** Check for the optional trailing arity and mode number. 
	** This also checks for filename:linenumber breakpoint specifiers.
	*/
	end = str + len - 1;
	if (MR_parse_trailing_number(str, &end, &n)) {
		if (end == str) {
			/* the string contains only a number */
			return MR_FALSE;
		}
		end--;
		if (*end == ':') {
			/* filename:linenumber */
			return MR_FALSE;
		} else if (*end == '-') {
			spec->MR_proc_mode = n;

			/*
			** Avoid modifying the string until we're sure
			** the parse can't fail.
			*/
			dash = end;

			end--;
			if (MR_parse_trailing_number(str, &end, &n)) {
				if (end == str) {
					/* the string contains only a number */
					return MR_FALSE;
				}
				end--;
				if (*end == '/') {
					*end = '\0';
					spec->MR_proc_arity = n;
					end--;
				}
			}
			*dash = '\0';
		} else if (*end == '/') {
			*end = '\0';
			end--;
			spec->MR_proc_arity = n;
		}
	}
	
	if (MR_strneq(str, "pred*", 5)) {
		spec->MR_proc_pf = MR_PREDICATE;
		str += 5;
	} else if (MR_strneq(str, "func*", 5)) {
		spec->MR_proc_pf = MR_FUNCTION;
		str += 5;
	}

	/*
	** Search backwards for the end of the final module qualifier.
	** There must be at least one character before the qualifier.
	*/
	while (end > str) {
		if (*end == ':' || (*end == '_' && *(end + 1) == '_')) {
			if (*end  == ':') {
				spec->MR_proc_name = end + 1;
			} else {
				spec->MR_proc_name = end + 2;
			}

			/*
			** Convert all occurences of '__' to ':'.
			*/
			double_underscores = 0;
			for (start = str; start < end; start++) {
				if (*start == '_' && *(start + 1) == '_') {
					*(start - double_underscores) = ':';
					double_underscores++;
					start++;
				} else {
					*(start - double_underscores) = *start;
				}
			}
			*(end - double_underscores) = '\0';

			spec->MR_proc_module = str;

			return MR_TRUE;
		} else {
			end--;
		}
	}

	/* There was no module qualifier. */
	spec->MR_proc_name = str;
	return MR_TRUE;
}

/* 
** Go backwards over a string starting at `end', stopping at `start',
** parsing the trailing integer and storing it in `*n'.
** On return, `*end' points to the start of the trailing number.
** If no number was found, `*end' is unchanged. 
*/
static MR_bool
MR_parse_trailing_number(char *start, char **end, int *number)
{
	MR_bool found_digit = MR_FALSE;
	int power_of_10 = 1;	
	char c;
	char *tmp_end;

	*number = 0;

	tmp_end = *end + 1;
	while (tmp_end > start && MR_isdigit(*(tmp_end - 1))) {
		found_digit = MR_TRUE;
		*number += power_of_10 * (*(tmp_end - 1) - '0');
		power_of_10 *= 10;
		tmp_end--;
	}
	if (found_digit) {
		*end = tmp_end;
	}
	return found_digit;
}

#define	MR_INIT_MATCH_PROC_SIZE		8

static void
MR_register_matches(void *data, const MR_Proc_Layout *entry)
{
	MR_Matches_Info	*m;

	m = (MR_Matches_Info *) data;
	MR_ensure_room_for_next(m->match_proc, const MR_Proc_Layout *,
		MR_INIT_MATCH_PROC_SIZE);
	m->match_procs[m->match_proc_next] = entry;
	m->match_proc_next++;
}

MR_Matches_Info
MR_search_for_matching_procedures(MR_Proc_Spec *spec)
{
	MR_Matches_Info	m;

	m.match_procs = NULL;
	m.match_proc_max = 0;
	m.match_proc_next = 0;
	MR_process_matching_procedures(spec, MR_register_matches, &m);
	return m;
}

/*
** This struct is for communication between
** MR_register_match and MR_search_for_matching_procedure.
*/

typedef struct {
	const MR_Proc_Layout	*matching_entry;
	MR_bool	 		match_unique;
} MR_Match_Info;

static void
MR_register_match(void *data, const MR_Proc_Layout *entry)
{
	MR_Match_Info	*m;

	m = (MR_Match_Info *) data;
	if (m->matching_entry == NULL) {
		m->matching_entry = entry;
	} else {
		m->match_unique = MR_FALSE;
	}
}

const MR_Proc_Layout *
MR_search_for_matching_procedure(MR_Proc_Spec *spec, MR_bool *unique)
{
	MR_Match_Info	m;

	m.matching_entry = NULL;
	m.match_unique = MR_TRUE;
	MR_process_matching_procedures(spec, MR_register_match, &m);
	*unique = m.match_unique;
	return m.matching_entry;
}

void
MR_process_matching_procedures(MR_Proc_Spec *spec,
	void f(void *, const MR_Proc_Layout *), void *data)
{
	if (spec->MR_proc_module != NULL) {
		const MR_Module_Layout	*module;

		module = MR_search_module_info(spec->MR_proc_module);
		if (module != NULL) {
			MR_process_matching_procedures_in_module(
				module, spec, f, data);
		}
	} else {
		int	i;

		for (i = 0; i < MR_module_info_next; i++) {
			MR_process_matching_procedures_in_module(
				MR_module_infos[i], spec, f, data);
		}
	}
}

#define	match_name(spec, cur)	(((spec)->MR_proc_name == NULL) ||	\
				MR_streq((spec)->MR_proc_name,		\
					cur->MR_sle_user.MR_user_name))

#define	match_arity(spec, cur)	(((spec)->MR_proc_arity < 0) ||		\
    				(spec)->MR_proc_arity ==		\
					MR_sle_user_adjusted_arity(cur))

#define	match_mode(spec, cur)	(((spec)->MR_proc_mode < 0) ||		\
				(spec)->MR_proc_mode ==			\
					cur->MR_sle_user.MR_user_mode)

#define	match_pf(spec, cur)	(((int) (spec)->MR_proc_pf < 0) ||	\
				(spec)->MR_proc_pf ==			\
					cur->MR_sle_user.MR_user_pred_or_func)

static void
MR_process_matching_procedures_in_module(const MR_Module_Layout *module,
	MR_Proc_Spec *spec, void f(void *, const MR_Proc_Layout *), void *data)
{
	const MR_Proc_Layout	*cur_entry;
	int			j;

	for (j = 0; j < module->MR_ml_proc_count; j++) {
		cur_entry = module->MR_ml_procs[j];
		if (match_name(spec, cur_entry) &&
				match_arity(spec, cur_entry) &&
				match_mode(spec, cur_entry) &&
				match_pf(spec, cur_entry))
		{
			f(data, cur_entry);
		}
	}
}

void
MR_print_proc_id_for_debugger(FILE *fp, const MR_Proc_Layout *entry_layout)
{
	MR_print_proc_id(fp, entry_layout);
	fprintf(fp, "\n");
}

void
MR_proc_layout_stats(FILE *fp)
{
	const MR_Module_Layout		*module_layout;
	const MR_Proc_Layout		*proc_layout;
	int				module_num, proc_num;
	MR_Determinism			detism;
	int				total;
	int				histogram[MR_DETISM_MAX + 1];

	total = 0;
	for (detism = 0; detism <= MR_DETISM_MAX; detism++) {
		histogram[detism] = 0;
	}

	for (module_num = 0; module_num < MR_module_info_next; module_num++) {
		module_layout = MR_module_infos[module_num];

		for (proc_num = 0;
			proc_num < module_layout->MR_ml_proc_count;
			proc_num++)
		{
			proc_layout = module_layout->MR_ml_procs[proc_num];

			total++;
			if (0 <= proc_layout->MR_sle_detism &&
				proc_layout->MR_sle_detism <= MR_DETISM_MAX)
			{
				histogram[proc_layout->MR_sle_detism]++;
			}
		}
	}

	for (detism = 0; detism <= MR_DETISM_MAX; detism++) {
		if (histogram[detism] > 0) {
			fprintf(fp, "%-10s %10d (%5.2f%%)\n",
				MR_detism_names[detism],
				histogram[detism],
				((float) 100 * histogram[detism]) / total);
		}
	}
	fprintf(fp, "%-10s %10d\n", "all ", total);
}

void
MR_label_layout_stats(FILE *fp)
{
	const MR_Module_Layout		*module_layout;
	const MR_Module_File_Layout	*file_layout;
	const MR_Label_Layout		*label_layout;
	int				module_num, file_num, label_num;
	MR_Trace_Port			port;
	int				total;
	int				histogram[MR_PORT_NUM_PORTS];

	total = 0;
	for (port = 0; port < MR_PORT_NUM_PORTS; port++) {
		histogram[port] = 0;
	}

	for (module_num = 0; module_num < MR_module_info_next; module_num++) {
		module_layout = MR_module_infos[module_num];

		for (file_num = 0;
			file_num < module_layout->MR_ml_filename_count;
			file_num++)
		{
			file_layout = module_layout->
				MR_ml_module_file_layout[file_num];

			for (label_num = 0;
				label_num < file_layout->MR_mfl_label_count;
				label_num++)
			{
				label_layout = file_layout->
					MR_mfl_label_layout[label_num];

				total++;
				if (0 <= label_layout->MR_sll_port &&
					label_layout->MR_sll_port
					< MR_PORT_NUM_PORTS)
				{
					histogram[label_layout->MR_sll_port]++;
				}
			}
		}
	}

	for (port = 0; port < MR_PORT_NUM_PORTS; port++) {
		fprintf(fp, "%4s %10d (%5.2f%%)\n",
			MR_port_names[port],
			histogram[port],
			((float) 100 * histogram[port]) / total);
	}
	fprintf(fp, "%s %10d\n", "all ", total);
}
