// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2003, 2005-2006, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_LAYOUT_UTIL_H
#define MERCURY_LAYOUT_UTIL_H

#include "mercury_std.h"
#include "mercury_types.h"          // for MR_Word, etc.
#include "mercury_stack_layout.h"   // for MR_LabelLayout, etc.
#include "mercury_type_info.h"      // for MR_TypeInfoParams, etc.
#include "mercury_ho_call.h"        // for MR_Closure

// These two functions copy the register state to and from the provided
// saved_regs and saved_f_regs arrays, which should have room for
// MR_MAX_FAKE_REG MR_Words and MR_MAX_VIRTUAL_F_REG MR_Floats respectively.

extern  void    MR_copy_regs_to_saved_regs(int max_mr_num, MR_Word *saved_regs,
                    int max_f_num, MR_Float *saved_f_regs);
extern  void    MR_copy_saved_regs_to_regs(int max_mr_num, MR_Word *saved_regs,
                    int max_f_num, MR_Float *saved_f_regs);

// A MR_LabelLayout describes the variables that are live at a given
// program point. Some of the types of these variables may contain type
// variables. Since the values of those type variables are not known until
// runtime, the MR_LabelLayout cannot include full typeinfos for the
// variables. Instead, it contains pseudo-typeinfos, in which some parts
// of some typeinfo structures may contain an indication "this data is
// not available at compile time, but at runtime it will be in this location".
//
// MR_materialize_type_params takes as input a MR_LabelLayout structure.
// It returns a vector of typeinfos which has one entry for each
// type variable in the MR_LabelLayout structure, with this typeinfo
// being the value of the corresponding type variable.
// Since type variable numbers start at one, the element of this array at
// index zero will not have a type_info in it. We store a dummy type_ctor_info
// there, so that the array will itself look like a typeinfo.
//
// The vector returned by MR_materialize_type_params is from MR_malloc;
// it should be MR_freed after last use.
//
// MR_materialize_type_params looks up locations in the current
// environment, as indicated by the set of saved registers (including MR_sp
// and MR_curfr). MR_materialize_typeinfos_base does the same job but
// assumes the environment is given by the given values of MR_sp and MR_curfr,
// and does not assume that the registers have valid contents unless saved_regs
// is non-null.
//
// MR_materialize_closure_type_params does much the same except that
// it takes an MR_Closure rather than an MR_LabelLayout,
// and it gets the type_infos from a closure using the closure_layout,
// rather than getting them from the registers/stacks using a label_layout.
//
// MR_materialize_typeclass_info_type_params is the same as
// MR_materialize_closure_type_params except that it takes
// a typeclass_info and a closure layout (for the type class method)
// and it gets the type_infos from the typeclass_info.

extern  MR_TypeInfoParams   MR_materialize_type_params(
                                const MR_LabelLayout *label_layout,
                                MR_Word *saved_regs);
extern  MR_TypeInfoParams   MR_materialize_type_params_base(
                                const MR_LabelLayout *label_layout,
                                MR_Word *saved_regs,
                                MR_Word *base_sp, MR_Word *base_curfr);
extern  MR_TypeInfoParams   MR_materialize_closure_type_params(
                                MR_Closure *closure);
extern  MR_TypeInfoParams   MR_materialize_typeclass_info_type_params(
                                MR_Word typeclass_info,
                                MR_Closure_Layout *closure_layout);
extern  MR_TypeInfoParams   MR_materialize_answer_block_type_params(
                                const MR_TypeParamLocns *tvar_locns,
                                MR_Word *answer_block, int block_size);

// If the given encoded location refers to a register, return its number.
// If it does not, return -1.

extern  int MR_get_register_number_long(MR_LongLval locn);
extern  int MR_get_register_number_short(MR_ShortLval locn);

// Given an location either in a long or short form, return the value
// at that location if possible. *succeeded will say whether the attempt
// was successful.
//
// MR_lookup_{long,short}_lval looks up locations in the current environment,
// as indicated by the set of saved registers (including MR_sp and MR_curfr).
// MR_lookup_{long,short}_lval_base does the same job but assumes the
// environment is given by the given values of MR_sp and MR_curfr, and does
// not assume that the registers have valid contents unless saved_regs is
// non-null.

extern  MR_Word MR_lookup_long_lval(MR_LongLval locn,
                    MR_Word *saved_regs, MR_Float *saved_f_regs,
                    MR_bool *succeeded);
extern  MR_Word MR_lookup_long_lval_base(MR_LongLval locn,
                    MR_Word *saved_regs, MR_Word *base_sp,
                    MR_Word *base_curfr, MR_Float *saved_f_regs,
                    MR_bool *succeeded);
extern  MR_Word MR_lookup_short_lval(MR_ShortLval locn,
                    MR_Word *saved_regs, MR_bool *succeeded);
extern  MR_Word MR_lookup_short_lval_base(MR_ShortLval locn,
                    MR_Word *saved_regs, MR_Word *base_sp,
                    MR_Word *base_curfr, MR_bool *succeeded);

// Given information about the location of a variable (var) and a vector giving
// the typeinfos corresponding to the type variables that may occur in
// the type of that variable (type_params), try to return the value of the
// variable in *value and the typeinfo describing its type in *type_info.
// *succeeded will say whether the attempt was successful.
//
// The type_params array should have the same format as the array returned
// by MR_materialize_type_params.
//
// MR_get_type_and_value looks up locations in the current environment,
// as indicated by the set of saved registers (including MR_sp and MR_curfr).
// MR_get_type_and_value_base does the same job but assumes the
// environment is given by the given values of MR_sp and MR_curfr, and does
// not assume that the registers have valid contents unless saved_regs is
// non-null.
//
// MR_get_type and MR_get_type_base are similar but do not
// return the value.
//
// All of these functions may need to allocate memory (to hold the
// type_infos that they return); any memory that they allocate will
// be allocated on the Mercury heap.

extern  MR_bool MR_get_type_and_value(const MR_LabelLayout *label_layout,
                    int var, MR_Word *saved_regs, MR_Float *saved_f_regs,
                    MR_TypeInfo *type_params, MR_TypeInfo *type_info,
                    MR_Word *value);
extern  MR_bool MR_get_type_and_value_base(const MR_LabelLayout *label_layout,
                    int var, MR_Word *saved_regs, MR_Word *base_sp,
                    MR_Word *base_curfr, MR_Float *saved_f_regs,
                    MR_TypeInfo *type_params, MR_TypeInfo *type_info,
                    MR_Word *value);
extern  MR_bool MR_get_type(const MR_LabelLayout *label_layout, int var,
                    MR_Word *saved_regs, MR_TypeInfo *type_params,
                    MR_TypeInfo *type_info);
extern  MR_bool MR_get_type_base(const MR_LabelLayout *label_layout, int var,
                    MR_Word *saved_regs, MR_Word *base_sp,
                    MR_Word *base_curfr, MR_TypeInfo *type_params,
                    MR_TypeInfo *type_info);

// MR_write_variable: write a variable to stdout.
// This uses the fake_reg copies of the registers,
// and it may also clobber the real registers.

extern  void    MR_write_variable(MR_TypeInfo type_info, MR_Word value);

// Return the name of a procedure specified by the given proc layout in three
// pieces: the name of the procedure in *proc_name_ptr, its arity in
// *arity_ptr, and a boolean that is true iff procedure is a function
// in *is_func_ptr,

extern  void    MR_generate_proc_name_from_layout(const MR_ProcLayout
                    *proc_layout, MR_ConstString *proc_name_ptr,
                    int *arity_ptr, MR_Word *is_func_ptr);

// Return the user-visible arity of the procedure (including the return value
// for functions), the number of typeinfo and/or typeclassinfo arguments added
// by the compiler, and an indication whether the procedure is from a predicate
// or a function.

extern  void    MR_proc_id_arity_addedargs_predfunc(const MR_ProcLayout *proc,
                    int *arity_ptr, int *num_added_args_ptr,
                    MR_PredFunc *pred_or_func_ptr);

#endif  // MERCURY_LAYOUT_UTIL_H
