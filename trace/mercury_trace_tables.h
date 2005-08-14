/*
** Copyright (C) 1998-2002, 2004-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the declarations of the tables that contain
** the identities of the debuggable modules and their procedures.
**
** Main author: Zoltan Somogyi.
*/

#ifndef	MERCURY_TRACE_TABLES_H
#define	MERCURY_TRACE_TABLES_H

#include	"mercury_stack_layout.h"
#include	"mercury_trace_completion.h"
#include	<stdio.h>

/*
** MR_register_all_modules_and_procs gathers all available debugging info
** about the modules and procedures of the program into the module info table.
** If verbose is MR_TRUE, print progress and summary messages.
*/

extern	void		MR_register_all_modules_and_procs(FILE *fp,
				MR_bool verbose);

/*
** MR_register_module_layout_real registers a module layout structure.
** It is called indirectly, through the function pointer
** MR_register_module_layout, by the module initialization code
** of modules compiled with debugging.
*/

extern	void		MR_register_module_layout_real(const MR_Module_Layout
				*module);

/*
** MR_process_file_line_layouts searches all the module layout structures
** of the program for label layout structures corresponding to the given
** filename/linenumber combination. For all such labels, it calls the supplied
** callback function with a pointer to the label's layout structure and
** with the supplied integer callback argument.
*/

typedef	void		(*MR_file_line_callback)(const MR_Label_Layout *, int);

extern	void		MR_process_file_line_layouts(const char *file,
				int line, MR_file_line_callback callback_func,
				int callback_arg);

/*
** These functions print (parts of) the module info table.
**
** MR_dump_module_tables lists all procedures in all modules, unless
** module is not NULL, in which case lists only procedures in the named module.
** Its output can be very big; it should be used only by developers,
** for debugging the debugger. The components of procedure names will be
** printed in separate fields if the separate argument is true, while the
** names of compiler-created (unify/compare/index etc) predicates will be
** printed if the uci argument is true.
**
** MR_dump_module_list lists the names of all the modules,
** while MR_dump_module_procs lists the names of all the procs in the named
** module. These are intended for ordinary, non-developer users.
*/

extern	void		MR_dump_module_tables(FILE *fp, MR_bool separate,
				MR_bool uci, char *module);

extern	void		MR_dump_module_list(FILE *fp);
extern	void		MR_dump_module_procs(FILE *fp, const char *name);

/*
** A procedure specification has several components, the meaning of which
** depends on whether the procedure is from a user defined procedure (user)
** or from a unify, compare, index or init procedure (uci).
**
** The meanings of the components are
**
**	the name of the module defining the procedure
**	the name of the predicate or function (user)
**		or the name of the type (uci)
**	the arity of the predicate or function (user)
**		or the arity of the type constructor (uci)
**	the mode of the predicate or function
**	whether the procedure is from a predicate or function (user)
**		or is a unify, compare or index procedure (uci)
**
** A NULL pointer for the string fields, and a negative number for the other
** fields signifies the absence of information about that field, which should
** therefore be treated as a wildcard.
*/

typedef enum {
	MR_PREFIX_PRED,
	MR_PREFIX_FUNC,
	MR_PREFIX_UNIF,
	MR_PREFIX_COMP,
	MR_PREFIX_INDX,
	MR_PREFIX_INIT
} MR_Proc_Prefix;

typedef	struct {
	const char			*MR_proc_module;
	const char			*MR_proc_name;
	int				MR_proc_arity;
	int				MR_proc_mode;
	MR_Proc_Prefix			MR_proc_prefix;
} MR_Proc_Spec;

/*
** Given a string containing the specification of a procedure in the form
**
**	[`pred*'|`func*']module:name/arity-mode
**
** in which some of the five components (but not the name) may be missing,
** parse it into the more usable form of a MR_Proc_Spec. The original string
** may be overwritten in the process.
**
** Returns MR_TRUE if the string was correctly formed, and MR_FALSE otherwise.
*/

extern	MR_bool	MR_parse_proc_spec(char *str, MR_Proc_Spec *spec);

/*
** Search the tables for a procedure that matches the given specification.
** If no procedure matches, return NULL.
** If one procedure matches, return its layout structure,
** and set *unique to MR_TRUE.
** If more than one procedure matches, return the layout structure of one
** and set *unique to MR_FALSE.
*/

extern	const MR_Proc_Layout *MR_search_for_matching_procedure(
					MR_Proc_Spec *spec, MR_bool *unique);

/*
** Search the tables for procedures that matches the given specification.
** Return their layout structures in the array in the match_procs field
** of the structure. The match_proc_next field says how many matches there are,
** and the match_proc_max field says how many entries the array has allocated
** for it.
*/

typedef struct {
	const MR_Proc_Layout	**match_procs;
	int	 		match_proc_max;
	int	 		match_proc_next;
} MR_Matches_Info;

extern	MR_Matches_Info	MR_search_for_matching_procedures(MR_Proc_Spec *spec);

/* filter out UCI procs and keep only mode number 0 */
extern	void MR_filter_user_preds(MR_Matches_Info *matches);

/*
** MR_process_matching_procedures(spec, f, data):
**	For each procedure that matches the specification given by `spec',
**	call `f(data, entry)', where `entry' is the entry layout for that
**	procedure.  The argument `data' is a `void *' which can be used
**	to pass any other information needed by the function `f'.
*/

extern	void	MR_process_matching_procedures(MR_Proc_Spec *spec,
			void f(void *, const MR_Proc_Layout *), void *data);

/*
** MR_print_proc_id_and_nl(fp, proc):
** 	Print the id of the procedure identified by proc, followed by a
** 	newline.
*/

extern	void	MR_print_proc_id_and_nl(FILE *fp, const MR_Proc_Layout *proc);

/*
** MR_print_pred_id_and_nl(fp, proc):
** 	Print the id of the predicate/function identified by proc, 
**	followed by a newline.
*/

extern	void	MR_print_pred_id_and_nl(FILE *fp, const MR_Proc_Layout *proc);

/*
** MR_get_proc_decl_module(proc):
** 	Return the module name the procedure is declared in.
*/

extern	MR_ConstString	MR_get_proc_decl_module(const MR_Proc_Layout *proc);

/*
** MR_proc_layout_stats(fp):
**	Prints statistics about the proc layout structures of the program.
*/

extern	void	MR_proc_layout_stats(FILE *fp);

/*
** MR_label_layout_stats(fp):
**	Prints statistics about the label layout structures of the program.
*/

extern	void	MR_label_layout_stats(FILE *fp);

/*
** MR_var_name_stats(fp):
**	Prints statistics about the space occupied by the variable names
**	in the layout structures of the program.
*/

extern	void	MR_var_name_stats(FILE *fp);

/* A Readline completer for module names. */
extern  MR_Completer_List *MR_trace_module_completer(const char *, size_t);

/* A Readline completer for procedure specifications. */
extern  MR_Completer_List *MR_trace_proc_spec_completer(const char *, size_t);

#endif	/* not MERCURY_TRACE_TABLES_H */
