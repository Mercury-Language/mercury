/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_UTIL_H
#define	MERCURY_TRACE_UTIL_H

extern	Word    MR_saved_regs[MAX_FAKE_REG];
extern	void	MR_copy_regs_to_saved_regs(int max_mr_num);
extern	void	MR_copy_saved_regs_to_regs(int max_mr_num);

extern	Word	*MR_trace_materialize_typeinfos(const MR_Stack_Layout_Vars
			*vars);

extern	Word	MR_trace_make_var_list(const MR_Stack_Layout_Label *layout);
extern	Word	MR_trace_lookup_live_lval(MR_Live_Lval locn, bool *succeeded);
extern	bool	MR_trace_get_type_and_value(const MR_Stack_Layout_Var *var,
			Word *type_params, Word *type_info, Word *value);

#endif	/* MERCURY_TRACE_UTIL_H */
