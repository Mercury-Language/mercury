/*
** Copyright (C) 1998-2002, 2004-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TRACE_DECLARATIVE_H
#define MERCURY_TRACE_DECLARATIVE_H

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"

/*
** When in declarative debugging mode, the internal debugger calls
** MR_trace_decl_debug for each event.  
*/

extern	MR_Code	*MR_trace_decl_debug(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info);

/*
** The internal (interactive) debugger calls this function to enter
** declarative debugging mode.  It returns MR_TRUE if successful, and
** MR_FALSE if there was some problem that prevented this mode from
** being entered.
*/

extern	MR_bool	MR_trace_start_decl_debug(MR_Trace_Mode trace_mode,
			const char *out, MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);

/*
** The declarative debugger may need to perform many retries during one
** diagnosis session. If the goal being debugged can do I/O, these retries
** are safe only if all I/O primitives in the program are tabled. Normally,
** this is guaranteed by the grade being a debugging grade. However, we also
** want to check the functioning of the declarative debugger in non-debug
** grades. In these grades, we only call tabled I/O primitives in the test
** cases themselves, so the existence of non-tabled primitives in the standard
** library doesn't matter. An option of the dd (or dd_dd) command can assert
** that all the I/O primitives being backtracked over are tabled. This global
** variable records the presence or absence of this option on the last dd or
** dd_dd command. It must be stored in a global instead of being passed around
** as a parameter because the front end can cause retries even after the
** initial retry that starts collecting the annotated trace.
*/

extern	MR_bool	MR_trace_decl_assume_all_io_is_tabled;

/*
** These functions add(or remove) a module, pred or func to(or from) the set of 
** trusted objects in the oracle_state inside the current diagnoser_state.
** They will call MR_trace_decl_ensure_init to ensure the diagnoser_state is
** initialised first.
*/

extern	void	MR_decl_add_trusted_module(const char *module_name);
extern	void	MR_decl_add_trusted_pred_or_func(const MR_Proc_Layout *entry);
extern	void	MR_decl_trust_standard_library(void);
extern	MR_bool	MR_decl_remove_trusted(int n);

/*
** MR_trace_decl_set_default_search_mode sets the default search mode for 
** the analyser.
*/

typedef MR_Word MR_Decl_Search_Mode;

extern	void	MR_trace_decl_set_fallback_search_mode(
			MR_Decl_Search_Mode search_mode);

/*
** This function checks to see if the supplied string is a valid
** search mode.  If it is then it returns MR_TRUE and sets
** the value at search_mode to the corresponding search mode.
** If it isn't then it returns MR_FALSE and leaves the value at
** search_mode unchanged.
*/

extern	MR_bool	MR_trace_is_valid_search_mode_string(
			const char *search_mode_string,
			MR_Decl_Search_Mode *search_mode);

/*
** Return the default search mode to use when then --search-mode option for the
** `dd' command is not given.
*/

extern MR_Decl_Search_Mode MR_trace_get_default_search_mode(void);

/*
** Prints a list of the trusted objects.  If mdb_command_format is true it
** prints the list as a series of mdb `trust' commands.  Otherwise it 
** prints the list in a format suitable for display.
*/

extern	void	MR_decl_print_all_trusted(FILE *fp, 
			MR_bool mdb_command_format);
/*
** The following macros are provided to help C code manipulate the
** Mercury data structure.  The values here must match the corresponding
** values in the definitions in browser/declarative_execution.m.
*/

typedef MR_Word MR_Trace_Node;

#define MR_TRACE_STATUS_SUCCEEDED	(MR_Word) 0
#define MR_TRACE_STATUS_FAILED		(MR_Word) 1
#define MR_TRACE_STATUS_UNDECIDED	(MR_Word) 2

/*
** The initial depth step size.  The choice of 3 is quite arbitrary, but
** it does mean the code which generates new portions of the annotated trace
** is exercised more during testing.
*/

#define MR_TRACE_DECL_INITIAL_DEPTH	3

/*
** We only build the annotated trace for events down to a certain depth.
** MR_edt_depth_step_size gives the default depth limit (relative to the
** starting depth).  In future it would be nice to adjust this factor based on
** profiling information.  
*/

extern	MR_Integer	MR_edt_depth_step_size;

#endif	/* MERCURY_TRACE_DECLARATIVE_H */
