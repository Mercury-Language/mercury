/*
** Copyright (C) 1998 The University of Melbourne.
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
#include	<stdio.h>

/*
** The module info table is an array with one element for each module
** that has procedures with execution tracing information. This element
** gives the module's name and points to a list of the procedure layouts
** of the traceable procedures of the module.
*/

typedef struct MR_Proc_Node_Struct	MR_Proc_Node;

struct MR_Proc_Node_Struct {
	const MR_Stack_Layout_Entry	*MR_proc_layout;
	MR_Proc_Node			*MR_proc_next;
};

typedef struct {
	const char 			*MR_module_name;
	MR_Proc_Node			*MR_module_procs;
} MR_Module_Info;

/*
** MR_register_all_modules_and_procs gathers all available debugging info
** about the modules and procedures of the program into the module info table.
** If verbose is TRUE, print progress and summary messages.
*/

extern	void		MR_register_all_modules_and_procs(FILE *fp,
				bool verbose);

/*
** These functions print (parts of) the module info table.
**
** MR_dump_module_tables lists all procedures in all modules.
** Its output can be very big; it should be used only by developers,
** for debugging the debugger.
**
** MR_dump_module_list lists the names of all the modules,
** while MR_dump_module_procs lists the names of all the procs in the named
** module. These are intended for ordinary, non-developer users.
*/

extern	void		MR_dump_module_tables(FILE *fp);
extern	void		MR_dump_module_list(FILE *fp);
extern	void		MR_dump_module_procs(FILE *fp, const char *name);

/*
** A procedure specification gives some or all of
**
**	the name of the module defining the procedure
**	the name of the predicate or function
**	the arity of the predicate or function
**	the mode of the predicate or function
**	whether the procedure belongs to a predicate or function
**
** A NULL pointer for the string fields, and a negative number for the other
** fields signifies the absence of information about that field, which should
** therefore be treated as a wildcard.
*/

typedef	struct {
	const char			*MR_proc_module;
	const char			*MR_proc_name;
	int				MR_proc_arity;
	int				MR_proc_mode;
	MR_PredFunc			MR_proc_pf;
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
** Returns TRUE if the string was correctly formed, and FALSE otherwise.
*/

extern	bool	MR_parse_proc_spec(char *str, MR_Proc_Spec *spec);

/*
** Search the tables for a procedure that matches the given specification.
** If no procedure matches, return NULL.
** If one procedure matches, return its layout structure,
** and set *unique to TRUE.
** If more than one procedure matches, return the layout structure of one
** and set *unique to FALSE.
*/

extern	const MR_Stack_Layout_Entry *MR_search_for_matching_procedure(
					MR_Proc_Spec *spec, bool *unique);

/*
** Call f(entry) on the layout of every procedure that matches
** the given specification.
*/

extern	void	MR_process_matching_procedures(MR_Proc_Spec *spec,
			void f(const MR_Stack_Layout_Entry *));

#endif	/* not MERCURY_TRACE_TABLES_H */
