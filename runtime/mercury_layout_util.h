/*
** Copyright (C) 1998-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_LAYOUT_UTIL_H
#define	MERCURY_LAYOUT_UTIL_H

#include "mercury_std.h"
#include "mercury_types.h"		/* for MR_Word, etc. */
#include "mercury_stack_layout.h"	/* for MR_Label_Layout, etc. */
#include "mercury_type_info.h"		/* for MR_TypeInfoParams, etc. */
#include "mercury_ho_call.h"		/* for MR_Closure */

/*
** These two functions copy the register state to and from the provided
** saved_regs array, which should have room for MR_MAX_FAKE_REG MR_Words.
*/

extern	void	MR_copy_regs_to_saved_regs(int max_mr_num, MR_Word *saved_regs);
extern	void	MR_copy_saved_regs_to_regs(int max_mr_num, MR_Word *saved_regs);

/*
** A MR_Label_Layout describes the variables that are live at a given
** program point. Some of the types of these variables may contain type
** variables. Since the values of those type variables are not known until
** runtime, the MR_Label_Layout cannot include full typeinfos for the
** variables. Instead, it contains pseudo-typeinfos, in which some parts
** of some typeinfo structures may contain an indication "this data is
** not available at compile time, but at runtime it will be in this location".
**
** MR_materialize_typeinfos takes as input a MR_Label_Layout structure.
** It returns a vector of typeinfos which has one entry for each
** pseudo-typeinfo in the MR_Label_Layout structure, with this typeinfo
** being the pseudo-typeinfo with the runtime-only information substituted in.
** Since type variable numbers start at one, the element of this array at
** index zero will not have a type_info in it.  We store a dummy type_ctor_info
** there, so that the array will itself look like a typeinfo.
** 
** The vector returned by MR_materialize_typeinfos is from MR_malloc;
** it should be MR_freed after last use.
**
** MR_materialize_typeinfos looks up locations in the current
** environment, as indicated by the set of saved registers (including MR_sp
** and MR_curfr). MR_materialize_typeinfos_base does the same job but
** assumes the environment is given by the given values of MR_sp and MR_curfr,
** and does not assume that the registers have valid contents unless saved_regs
** is non-null.
**
** MR_materialize_closure_typeinfos does much the same except that
** it takes an MR_Closure rather than an MR_Label_Layout,
** and it gets the type_infos from a closure using the closure_layout,
** rather than getting them from the registers/stacks using a label_layout.
*/ 

extern	MR_TypeInfoParams	MR_materialize_typeinfos(
					const MR_Label_Layout *label_layout,
					MR_Word *saved_regs);
extern	MR_TypeInfoParams	MR_materialize_typeinfos_base(
					const MR_Label_Layout *label_layout,
					MR_Word *saved_regs,
					MR_Word *base_sp, MR_Word *base_curfr);
extern	MR_TypeInfoParams	MR_materialize_closure_typeinfos(
					const MR_Type_Param_Locns *tvar_locns,
					MR_Closure *closure);


/*
** If the given encoded location refers to a register, return its number.
** If it does not, return -1.
*/

extern	int	MR_get_register_number_long(MR_Long_Lval locn);
extern	int	MR_get_register_number_short(MR_Short_Lval locn);

/*
** Given an location either in a long or short form, return the value
** at that location if possible. *succeeded will say whether the attempt
** was successful.
**
** MR_lookup_{long,short}_lval looks up locations in the current environment,
** as indicated by the set of saved registers (including MR_sp and MR_curfr).
** MR_lookup_{long,short}_lval_base does the same job but assumes the
** environment is given by the given values of MR_sp and MR_curfr, and does
** not assume that the registers have valid contents unless saved_regs is
** non-null.
*/ 

extern	MR_Word	MR_lookup_long_lval(MR_Long_Lval locn,
			MR_Word *saved_regs, bool *succeeded);
extern	MR_Word	MR_lookup_long_lval_base(MR_Long_Lval locn,
			MR_Word *saved_regs, MR_Word *base_sp,
			MR_Word *base_curfr,
			bool *succeeded);
extern	MR_Word	MR_lookup_short_lval(MR_Short_Lval locn,
			MR_Word *saved_regs, bool *succeeded);
extern	MR_Word	MR_lookup_short_lval_base(MR_Short_Lval locn,
			MR_Word *saved_regs, MR_Word *base_sp,
			MR_Word *base_curfr,
			bool *succeeded);

/*
** Given information about the location of a variable (var) and a vector giving
** the typeinfos corresponding to the type variables that may occur in
** the type of that variable (type_params), try to return the value of the
** variable in *value and the typeinfo describing its type in *type_info.
** *succeeded will say whether the attempt was successful.
**
** The type_params array should have the same format as the array returned
** by MR_materialize_typeinfos.
**
** MR_get_type_and_value looks up locations in the current environment,
** as indicated by the set of saved registers (including MR_sp and MR_curfr).
** MR_get_type_and_value_base does the same job but assumes the
** environment is given by the given values of MR_sp and MR_curfr, and does
** not assume that the registers have valid contents unless saved_regs is
** non-null.
**
** MR_get_type and MR_get_type_base are similar but do not
** return the value.
**
** All of these functions may need to allocate memory (to hold the
** type_infos that they return); any memory that they allocate will
** be allocated on the Mercury heap.
*/

extern	bool	MR_get_type_and_value(const MR_Label_Layout *label_layout,
			int var, MR_Word *saved_regs, MR_TypeInfo *type_params,
			MR_TypeInfo *type_info, MR_Word *value);
extern	bool	MR_get_type_and_value_base(const MR_Label_Layout *label_layout,
			int var, MR_Word *saved_regs,
			MR_Word *base_sp, MR_Word *base_curfr,
			MR_TypeInfo *type_params, MR_TypeInfo *type_info,
			MR_Word *value);
extern	bool	MR_get_type(const MR_Label_Layout *label_layout, int var,
			MR_Word *saved_regs, MR_TypeInfo *type_params,
			MR_TypeInfo *type_info);
extern	bool	MR_get_type_base(const MR_Label_Layout *label_layout, int var,
			MR_Word *saved_regs, MR_Word *base_sp,
			MR_Word *base_curfr, MR_TypeInfo *type_params,
			MR_TypeInfo *type_info);

/*
** MR_write_variable:
**	Write a variable to stdout.
**	This uses the fake_reg copies of the registers,
**	and it may also clobber the real registers.
*/

extern	void	MR_write_variable(MR_TypeInfo type_info, MR_Word value);

#endif	/* MERCURY_LAYOUT_UTIL_H */
