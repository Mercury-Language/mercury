/*
** Copyright (C) 1998-2000 The University of Melbourne.
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
			void f(void *, const MR_Stack_Layout_Entry *),
			void *);
static	void	MR_process_line_layouts(MR_Module_File_Layout *file_layout,
			int line, MR_file_line_callback callback_func,
			int callback_arg);

void
MR_register_all_modules_and_procs(FILE *fp, bool verbose)
{
	static	bool	done = FALSE;

	if (! done) {
		if (verbose) {
			fprintf(fp, "Registering debuggable procedures... ");
			fflush(fp);
		}

		do_init_modules();
		done = TRUE;
		if (verbose) {
			fprintf(fp, "done.\n");
			if (MR_module_info_next == 0) {
				fprintf(fp, "There are no debuggable modules.");
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
	bool	found;

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
	int			i, j;
	MR_Module_File_Layout	*file_layout;

	for (i = 0; i < MR_module_info_next; i++) {
		for (j = 0; j < MR_module_infos[i]->MR_ml_filename_count; j++)
		{
			file_layout = MR_module_infos[i]->
					MR_ml_module_file_layout[j];
			if (streq(file_layout->MR_mfl_filename, file)) {
				MR_process_line_layouts(file_layout, line,
					callback_func, callback_arg);
			}
		}
	}
}

static void
MR_process_line_layouts(MR_Module_File_Layout *file_layout, int line,
	MR_file_line_callback callback_func, int callback_arg)
{
	int			k;
	bool			found;

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

bool
MR_parse_proc_spec(char *str, MR_Proc_Spec *spec)
{
	char	*dash;
	char	*slash;
	char	*s;
	int	n;
	bool	found;

	spec->MR_proc_module = NULL;
	spec->MR_proc_name   = NULL;
	spec->MR_proc_arity  = -1;
	spec->MR_proc_mode   = -1;
	spec->MR_proc_pf     = (MR_PredFunc) -1;

	if (strneq(str, "pred*", 5)) {
		spec->MR_proc_pf = MR_PREDICATE;
		str += 5;
	} else if (strneq(str, "func*", 5)) {
		spec->MR_proc_pf = MR_FUNCTION;
		str += 5;
	}

	if ((dash = strrchr(str, '-')) != NULL) {
		found = FALSE;
		n = 0;
		for (s = dash + 1; *s != '\0'; s++) {
			if (MR_isdigit(*s)) {
				found = TRUE;
				n = n * 10 + *s - '0';
			} else {
				/* a dash followed by a nondigit is an error */
				return FALSE;
			}
		}

		if (! found) {
			/* a dash with no following digit is an error */
			return FALSE;
		}

		spec->MR_proc_mode = n;
		*dash = '\0';
	}

	if ((slash = strrchr(str, '/')) != NULL) {
		found = FALSE;
		n = 0;
		for (s = slash + 1; *s != '\0'; s++) {
			if (MR_isdigit(*s)) {
				found = TRUE;
				n = n * 10 + *s - '0';
			} else {
				/* a slash followed by a nondigit is an error */
				return FALSE;
			}
		}

		if (! found) {
			/* a slash with no following digit is an error */
			return FALSE;
		}

		spec->MR_proc_arity = n;
		*slash = '\0';
	}

	if (MR_isdigit(*str)) {
		/* this looks to be a line number */
		return FALSE;
	}

	for (s = str; *s != '\0'; s++) {
		if (*s == ':' && MR_isdigit(*(s+1))) {
			/* this looks to be filename:linenumber */
			return FALSE;
		}

		if (*s == ':' || (*s == '_' && *(s+1) == '_')) {
			if (*s == ':') {
				spec->MR_proc_name = s+1;
			} else {
				spec->MR_proc_name = s+2;
			}

			*s = '\0';
			spec->MR_proc_module = str;

			return TRUE;
		}
	}

	spec->MR_proc_name = str;
	return TRUE;
}

/*
** This struct is for communication between
** MR_register_match and MR_search_for_matching_procedure.
*/
typedef struct {
	const MR_Stack_Layout_Entry	*matching_entry;
	bool	 			match_unique;
} MR_match_info;

static void
MR_register_match(void *data, const MR_Stack_Layout_Entry *entry)
{
	MR_match_info *m = data;
	if (m->matching_entry == NULL) {
		m->matching_entry = entry;
	} else {
		m->match_unique = FALSE;
	}
}

const MR_Stack_Layout_Entry *
MR_search_for_matching_procedure(MR_Proc_Spec *spec, bool *unique)
{
	MR_match_info m;
	m.matching_entry = NULL;
	m.match_unique = TRUE;
	MR_process_matching_procedures(spec, MR_register_match, &m);
	*unique = m.match_unique;
	return m.matching_entry;
}

void
MR_process_matching_procedures(MR_Proc_Spec *spec,
	void f(void *, const MR_Stack_Layout_Entry *),
	void *data)
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
				streq((spec)->MR_proc_name,		\
					cur->MR_sle_user.MR_user_name))

#define	match_arity(spec, cur)	(((spec)->MR_proc_arity < 0) ||		\
				(spec)->MR_proc_arity ==		\
					cur->MR_sle_user.MR_user_arity)

#define	match_mode(spec, cur)	(((spec)->MR_proc_mode < 0) ||		\
				(spec)->MR_proc_mode ==			\
					cur->MR_sle_user.MR_user_mode)

#define	match_pf(spec, cur)	(((int) (spec)->MR_proc_pf < 0) ||	\
				(spec)->MR_proc_pf ==			\
					cur->MR_sle_user.MR_user_pred_or_func)

static void
MR_process_matching_procedures_in_module(const MR_Module_Layout *module,
	MR_Proc_Spec *spec, void f(void *, const MR_Stack_Layout_Entry *),
	void *data)
{
	const MR_Stack_Layout_Entry	*cur_entry;
	int				j;

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
MR_print_proc_id_for_debugger(FILE *fp,
	const MR_Stack_Layout_Entry *entry_layout)
{
	MR_print_proc_id(fp, entry_layout);
	fprintf(fp, "\n");
}

