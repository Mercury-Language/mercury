/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_LAYOUT_UTIL_H
#define	MERCURY_LAYOUT_UTIL_H

#include "mercury_std.h"
#include "mercury_types.h"
#include "mercury_stack_layout.h"

/*
** These two functions copy the register state to and from the provided
** saved_regs array, which should have room for MAX_FAKE_REG Words.
*/

extern	void	MR_copy_regs_to_saved_regs(int max_mr_num, Word *saved_regs);
extern	void	MR_copy_saved_regs_to_regs(int max_mr_num, Word *saved_regs);

/*
** A MR_Stack_Layout_Vars describes the variables that are live at a given
** program point. Some of the types of these variables may contain type
** variables. Since the values of those type variables are not known until
** runtime, the MR_Stack_Layout_Vars cannot include full typeinfos for the
** variables. Instead, it contains pseudo-typeinfos, in which some parts
** of some typeinfo structures may contain an indication "this data is
** not available at compile time, but at runtime it will be in this location".
**
** MR_materialize_typeinfos takes as input a MR_Stack_Layout_Vars
** structure. It returns a vector of typeinfos which has one entry for each
** pseudo-typeinfo in the MR_Stack_Layout_Vars structure, with this typeinfo
** being the pseudo-typeinfo with the runtime-only information substituted in.
** Since type variable numbers start at one, the element of this array at
** index zero will be unused. This means that the array will itself look
** like a typeinfo.
** 
** The vector returned by MR_materialize_typeinfos is from malloc;
** it should be freed after last use.
**
** MR_materialize_typeinfos looks up locations in the current
** environment, as indicated by the set of saved registers (including MR_sp
** and MR_curfr). MR_materialize_typeinfos_base does the same job but
** assumes the environment is given by the given values of MR_sp and MR_curfr,
** and does not assume that the registers have valid contents unless saved_regs
** is non-null.
*/ 

extern	Word	*MR_materialize_typeinfos(
			const MR_Stack_Layout_Vars *vars, Word *saved_regs);
extern	Word	*MR_materialize_typeinfos_base(
			const MR_Stack_Layout_Vars *vars, Word *saved_regs,
			Word *base_sp, Word *base_curfr);

/*
** Given a stack layout and the saved copy of the registers,
** get the values of the live variables as a list of univs.
** Any memory needed is allocated on the Mercury heap.
*/
extern	Word	MR_make_var_list(const MR_Stack_Layout_Label *layout,
			Word *saved_regs);

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

extern	Word	MR_lookup_long_lval(MR_Long_Lval locn,
			Word *saved_regs, bool *succeeded);
extern	Word	MR_lookup_long_lval_base(MR_Long_Lval locn,
			Word *saved_regs, Word *base_sp, Word *base_curfr,
			bool *succeeded);
extern	Word	MR_lookup_short_lval(MR_Short_Lval locn,
			Word *saved_regs, bool *succeeded);
extern	Word	MR_lookup_short_lval_base(MR_Short_Lval locn,
			Word *saved_regs, Word *base_sp, Word *base_curfr,
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
** return the value, whereas the functions with _filtered deliberately
** do not suceed if the variable's name indicates that the value is likely
** to be too big. This is a temporary measure only, until we get a better
** term printer.
**
** All of these functions may need to allocate memory (to hold the
** type_infos that they return); any memory that they allocate will
** be allocated on the Mercury heap.
*/

extern	bool	MR_get_type_and_value(const MR_Stack_Layout_Vars *vars,
			int var, Word *saved_regs,
			Word *type_params, Word *type_info, Word *value);
extern	bool	MR_get_type_and_value_base(const MR_Stack_Layout_Vars *vars,
			int var, Word *saved_regs,
			Word *base_sp, Word *base_curfr,
			Word *type_params, Word *type_info, Word *value);
extern	bool	MR_get_type(const MR_Stack_Layout_Vars *vars, int var,
			Word *saved_regs, Word *type_params, Word *type_info);
extern	bool	MR_get_type_base(const MR_Stack_Layout_Vars *vars, int var,
			Word *saved_regs, Word *base_sp, Word *base_curfr,
			Word *type_params, Word *type_info);
extern	bool	MR_get_type_and_value_filtered(
			const MR_Stack_Layout_Vars *vars, int var,
			Word *saved_regs, const char *name,
			Word *type_params, Word *type_info, Word *value);
extern	bool	MR_get_type_filtered(const MR_Stack_Layout_Vars *vars, int var, 
			Word *saved_regs, const char *name, Word *type_params, 
			Word *type_info);

/*
** MR_write_variable:
**	Write a variable to stdout.
**	This uses the fake_reg copies of the registers,
**	and it may also clobber the real registers.
*/

extern	void	MR_write_variable(Word type_info, Word value);

#endif	/* MERCURY_LAYOUT_UTIL_H */
