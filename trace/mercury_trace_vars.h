/*
** Copyright (C) 1999-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** This module looks after the debugger's information about the variables
** that are live at a given program point.
**
** When execution arrives at an event, the debugger should call the function
** MR_trace_init_point_vars to initialize this module's data structures
** to reflect the variables that are live at that event. During the processing
** of the various debugger commands while at that event, the debugger may
** call MR_trace_set_level zero or more times to change this module's notion
** of the "current" set of variables to refer instead to the variables that
** are live at the return address in a given ancestor. This module maintains
** its own record of what the current ancestor level is; the enquiry function
** MR_trace_current_level returns this information, while enquiry function
** MR_trace_current_level_details returns information about this level.
**
** The six functions MR_trace_var_count, MR_trace_list_vars,
** MR_trace_return_var_info, MR_trace_headvar_num, MR_trace_browse_one
** and MR_trace_browse_all all work in the context established by the
** MR_trace_init_point_vars and possibly MR_trace_set_level.
**
** This context may say that there is no information available about
** the variables live at the current location (this is possible if the
** relevant module was not compiled with the right debugging flags).
** If this is the case, or if some other reason prevents these functions
** from carrying out their assigned tasks, most of these functions return
** a non-NULL string describing the problem; they return NULL if everything
** went OK. (MR_trace_set_level also returns a pointer to an error message
** -and refuses to change levels- if something goes wrong.)
*/

#ifndef	MERCURY_TRACE_VARS_H
#define	MERCURY_TRACE_VARS_H

#include <stdio.h>
#include "mercury_types.h"
#include "mercury_stack_layout.h"

typedef	void	(*MR_Browser)(Word type_info, Word value);

typedef	enum {
	MR_VAR_SPEC_NUMBER,
	MR_VAR_SPEC_NAME
} MR_Var_Spec_Kind;

typedef struct {
	MR_Var_Spec_Kind	MR_var_spec_kind;
	int			MR_var_spec_number; /* valid if NUMBER */
	const char		*MR_var_spec_name;  /* valid if NAME   */
} MR_Var_Spec;

extern	void		MR_trace_init_point_vars(
				const MR_Stack_Layout_Label *top_layout,
				Word *saved_regs, MR_Trace_Port port);
extern	const char	*MR_trace_set_level(int ancestor_level);
extern	int		MR_trace_current_level(void);
extern	void		MR_trace_current_level_details(
				const MR_Stack_Layout_Entry **entry_ptr,
				const char **filename_ptr, int *linenumber_ptr,
				Word **base_sp_ptr, Word **base_curfr_ptr);

/*
** Return the number of live variables at the current point. If the required
** information is missing, return a negative number.
*/

extern	int		MR_trace_var_count(void);

/*
** Print the list of the names of variables live at the current point
** on the given file.
*/

extern	const char	*MR_trace_list_vars(FILE *out);

/*
** Return as a side effect the name, type and value of the specified
** variable in the specified locations, except those which are NULL.
** Variable number n must be in the range 1..MR_trace_var_count().
*/

extern	const char	*MR_trace_return_var_info(int n, const char **name_ptr,
				Word *type_info_ptr, Word *value_ptr);

/*
** If the variable specified by n is a head variable, then store
** its argument position in *num and return NULL, otherwise return
** an error.
*/

extern	const char	*MR_trace_headvar_num(int n, int *num);

/*
** Print the (names and) values of the specified variables.
** The names are printed to the given file if the file pointer is non-NULL.
** The values are printed by giving them to the specified browser.
** The last argument governs whether this function returns an error
** if the given variable specification is ambiguous.
*/

extern	const char	*MR_trace_browse_one(FILE *out, MR_Var_Spec var_spec,
				MR_Browser browser, bool must_be_unique);

/*
** Print the list of the names and values of all variables live at the current
** point. The variables names are printed directly to the given file, but
** only if the given file pointer is not NULL; the variable values are
** printed by calling the given browser function on them.
*/

extern	const char 	*MR_trace_browse_all(FILE *out, MR_Browser browser);

#endif	/* MERCURY_TRACE_VARS_H */
