/*
** Copyright (C) 1998 The University of Melbourne.
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
#include <stdio.h>
#include <string.h>
#include <ctype.h>

static	MR_Module_Info	*MR_module_infos;
static	int		MR_module_info_next = 0;
static	int		MR_module_info_max  = 0;
static	int		MR_module_info_proc_count = 0;

#define	INIT_MODULE_TABLE_SIZE	10

static	void		MR_register_from_internal_label(const void *info);
static	void		MR_ensure_proc_node_is_present(MR_Module_Info *module,
				const MR_Stack_Layout_Entry *entry);
static	MR_Module_Info	*MR_search_module_info(const char *name);
static	MR_Module_Info	*MR_insert_module_info(const char *name);
static	MR_Module_Info	*MR_ensure_module_info_is_present(const char *name);

static	void		MR_process_matching_procedures_in_module(
				MR_Module_Info *module, MR_Proc_Spec *spec,
				void f(void *, const MR_Stack_Layout_Entry *),
				void *);

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
		MR_process_all_internal_labels(MR_register_from_internal_label);
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

static void
MR_register_from_internal_label(const void *info)
{
	const MR_Stack_Layout_Label	*label;
	const MR_Stack_Layout_Entry	*entry;
	MR_Module_Info			*module;

	label = ((const MR_Internal *) info)->i_layout;

	if (label == NULL) {
		/* some labels have no layout structure */
		return;
	}

	if (label->MR_sll_entry == NULL) {
		/* some hand-crafted label structures have no entry */
		return;
	}

	entry = label->MR_sll_entry;

	if (MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry) &&
			! MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry))
	{
		module = MR_ensure_module_info_is_present(
				entry->MR_sle_user.MR_user_decl_module);
		MR_ensure_proc_node_is_present(module, entry);
	}
}

static void
MR_ensure_proc_node_is_present(MR_Module_Info *module,
	const MR_Stack_Layout_Entry *entry)
{
	MR_Proc_Node	*cur;

	for (cur = module->MR_module_procs; cur != NULL;
			cur = cur->MR_proc_next) {
		if (entry == cur->MR_proc_layout) {
			return;
		}
	}

	cur = checked_malloc(sizeof(MR_Proc_Node));
	cur->MR_proc_layout = entry;
	cur->MR_proc_next   = module->MR_module_procs;
	module->MR_module_procs = cur;
	MR_module_info_proc_count++;
}

static MR_Module_Info *
MR_search_module_info(const char *name)
{
	int	slot;
	bool	found;

	MR_bsearch(MR_module_info_next, slot, found,
		strcmp(MR_module_infos[slot].MR_module_name, name));
	if (found) {
		return &MR_module_infos[slot];
	} else {
		return NULL;
	}
}

static MR_Module_Info *
MR_insert_module_info(const char *name)
{
	int	slot;

	MR_ensure_room_for_next(MR_module_info, MR_Module_Info,
		INIT_MODULE_TABLE_SIZE);
	MR_prepare_insert_into_sorted(MR_module_infos, MR_module_info_next,
		slot, strcmp(MR_module_infos[slot].MR_module_name, name));

	MR_module_infos[slot].MR_module_name = name;
	MR_module_infos[slot].MR_module_procs = NULL;
	return &MR_module_infos[slot];
}

static MR_Module_Info *
MR_ensure_module_info_is_present(const char *name)
{
	MR_Module_Info	*module;

	module = MR_search_module_info(name);
	if (module != NULL) {
		return module;
	} else {
		return MR_insert_module_info(name);
	}
}

void
MR_dump_module_tables(FILE *fp)
{
	const MR_Proc_Node	*cur;
	int			i;

	for (i = 0; i < MR_module_info_next; i++) {
		fprintf(fp, "====================\n");
		fprintf(fp, "module %s\n", MR_module_infos[i].MR_module_name);
		fprintf(fp, "====================\n");
		for (cur = MR_module_infos[i].MR_module_procs; cur != NULL;
				cur = cur->MR_proc_next) {
			MR_print_proc_id(fp, cur->MR_proc_layout, NULL,
				NULL, NULL);
		}
	}
}

void
MR_dump_module_list(FILE *fp)
{
	int			i;

	fprintf(fp, "List of debuggable modules\n\n");
	for (i = 0; i < MR_module_info_next; i++) {
		fprintf(fp, "%s\n", MR_module_infos[i].MR_module_name);
	}
}

void
MR_dump_module_procs(FILE *fp, const char *name)
{
	MR_Module_Info		*module;
	const MR_Proc_Node	*cur;

	module = MR_search_module_info(name);
	if (module == NULL) {
		fprintf(fp, "There is no debugging info about module `%s'\n",
				name);
	} else {
		fprintf(fp, "List of procedures in module `%s'\n\n", name);
		for (cur = module->MR_module_procs; cur != NULL;
				cur = cur->MR_proc_next) {
			MR_print_proc_id(fp, cur->MR_proc_layout, NULL,
				NULL, NULL);
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

	for (s = str; *s != '\0'; s++) {
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
		MR_Module_Info			*module;

		module = MR_search_module_info(spec->MR_proc_module);
		if (module != NULL) {
			MR_process_matching_procedures_in_module(
				module, spec, f, data);
		}
	} else {
		int	i;

		for (i = 0; i < MR_module_info_next; i++) {
			MR_process_matching_procedures_in_module(
				&MR_module_infos[i], spec, f, data);
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
MR_process_matching_procedures_in_module(MR_Module_Info *module,
	MR_Proc_Spec *spec, void f(void *, const MR_Stack_Layout_Entry *),
	void *data)
{
	MR_Proc_Node			*cur;
	const MR_Stack_Layout_Entry	*cur_entry;

	for (cur = module->MR_module_procs; cur != NULL;
			cur = cur->MR_proc_next) {
		cur_entry = cur->MR_proc_layout;
		if (match_name(spec, cur_entry) &&
				match_arity(spec, cur_entry) &&
				match_mode(spec, cur_entry) &&
				match_pf(spec, cur_entry))
		{
			f(data, cur_entry);
		}
	}
}
