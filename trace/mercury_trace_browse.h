/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_browse.h
**
** Defines the interface of the term browser for the internal debugger.
*/

#ifndef	MERCURY_TRACE_BROWSE_H
#define MERCURY_TRACE_BROWSE_H

/*
** Interactively browse a term.
*/
extern 	void	MR_trace_browse(Word type_info, Word value);

/*
** Display a term (non-interactively).
*/
extern	void	MR_trace_print(Word type_info, Word value);

#endif	/* MERCURY_TRACE_BROWSE_H */
