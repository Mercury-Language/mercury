/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_browse.h
**
** Defines the interface of the term browser and the interactive query
** facility for the internal and external debuggers.
*/

#ifndef	MERCURY_TRACE_BROWSE_H
#define MERCURY_TRACE_BROWSE_H

#include "mercury_conf.h"	/* for MR_USE_EXTERNAL_DEBUGGER */
#include "mercury_types.h"	/* for MR_Word, MR_String       */
#include "mercury_std.h"	/* for MR_bool                     */
#include "mercury_tags.h"	/* for MR_DEFINE_MERCURY_ENUM_CONST     */

/*
** The following types must correspond with browse_caller_type and
** portray_format in browser/browser_info.m.
*/
typedef enum {
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_CALLER_PRINT),
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_CALLER_BROWSE),
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_CALLER_PRINT_ALL)
} MR_Browse_Caller_Type;

typedef enum {
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_FORMAT_FLAT),
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_FORMAT_RAW_PRETTY),
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_FORMAT_VERBOSE),
	MR_DEFINE_MERCURY_ENUM_CONST(MR_BROWSE_FORMAT_PRETTY)
} MR_Browse_Format;

/*
** This value must be different from any of the MR_BROWSE_FORMAT_* values.
*/
#define MR_BROWSE_DEFAULT_FORMAT	-1

/*
** Interactively browse a term.
*/
extern 	void	MR_trace_browse(MR_Word type_info, MR_Word value,
			MR_Browse_Format format);
extern 	void	MR_trace_browse_goal(MR_ConstString name, MR_Word arg_list,
			MR_Word is_func, MR_Browse_Format format);
#ifdef MR_USE_EXTERNAL_DEBUGGER
extern 	void	MR_trace_browse_external(MR_Word type_info, MR_Word value,
			MR_Browse_Caller_Type caller, MR_Browse_Format format);
#endif

/*
** Display a term non-interactively.
*/
extern	void	MR_trace_print(MR_Word type_info, MR_Word value,
			MR_Browse_Caller_Type caller, MR_Browse_Format format);
extern	void	MR_trace_print_goal(MR_ConstString name, MR_Word arg_list,
			MR_Word is_func, MR_Browse_Caller_Type caller,
			MR_Browse_Format format);

/*
** Set browser parameters.
*/
extern	MR_bool	MR_trace_set_browser_param(MR_Word print, MR_Word browse,
			MR_Word print_all, MR_Word flat, MR_Word raw_pretty,
			MR_Word verbose, MR_Word pretty, const char *param, 
			const char *value);

/*
** Invoke an interactive query.
*/

/* This must kept in sync with query_type in browser/interactive.m. */
typedef enum { 
	MR_DEFINE_MERCURY_ENUM_CONST(MR_NORMAL_QUERY), 
	MR_DEFINE_MERCURY_ENUM_CONST(MR_CC_QUERY), 
	MR_DEFINE_MERCURY_ENUM_CONST(MR_IO_QUERY) 
} MR_Query_Type;

extern	void	MR_trace_query(MR_Query_Type type, const char *options,
			int num_imports, /* const */ char *imports[]);

#ifdef MR_USE_EXTERNAL_DEBUGGER
extern	void	MR_trace_query_external(MR_Query_Type type, MR_String options,
			int num_imports, MR_Word imports_list);
#endif

#endif	/* MERCURY_TRACE_BROWSE_H */
