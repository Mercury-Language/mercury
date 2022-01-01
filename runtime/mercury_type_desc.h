// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2004 The University of Melbourne.
// Copyright (C) 2016, 2018, 2022 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TYPE_DESC_H
#define MERCURY_TYPE_DESC_H

#include "mercury_types.h"
#include "mercury_type_info.h"
#include "mercury_builtin_types.h"  // for the type_ctor_infos of the
                                    // variable arity type constructors

// Values of type `type_ctor.type_desc' are represented the same way as
// values of type `private_builtin.type_info' (this representation is
// documented in compiler/polymorphism.m). Some parts of the library
// (e.g. the gc initialization code) depend on this.
// The C type corresponding to these Mercury types is `MR_TypeInfo'.
//
// Values of type `type_ctor.pseudo_type_desc' are represented the same way as
// values of type `private_builtin.pseudo_type_info' (this representation is
// documented in compiler/polymorphism.m).
// The C type corresponding to these Mercury types is `MR_PseudoTypeInfo'.
//
// Values of type `type_ctor.type_ctor_desc' are not guaranteed to be
// represented the same way as values of type `private_builtin.type_ctor_info'.
// The representations *are* in fact identical for fixed arity types, but they
// differ for higher order and tuple types. Instead of a type_ctor_desc
// being a structure containing a pointer to the type_ctor_info for pred/0
// or func/0 and an arity, we have a single small encoded integer. This
// integer is four times the arity, plus zero, one or two; plus zero encodes
// a predicate, plus one encodes a function, plus two encodes a tuple.
// The maximum arity that can be encoded is given by MR_MAX_VARIABLE_ARITY
// (see below).
// The C type corresponding to type_ctor.type_ctor_desc is `MR_TypeCtorDesc'.

// Declare the MR_TypeCtorDesc ADT.
//
// Note that `struct MR_TypeCtorDesc_Struct' is deliberately left undefined.
// MR_TypeCtorDesc is declared as a pointer to a dummy structure only
// in order to allow the C compiler to catch errors in which things other
// than MR_TypeCtorDescs are given as arguments to macros that depend on their
// arguments being MR_TypeCtorDescs. The actual value is either a small integer
// or a pointer to a MR_TypeCtorInfo_Struct structure, as described above.

typedef struct MR_TypeCtorDesc_Struct *MR_TypeCtorDesc;

// The maximum arity that can be encoded should be set to twice the maximum
// number of general purpose registers, since an predicate or function having
// more arguments that this would run out of registers when passing the input
// arguments, or the output arguments, or both.
//
// XXX When tuples were added this was reduced to be the maximum number
// of general purpose registers, to reduce the probability that the
// `small' integers for higher-order and tuple types are confused with
// type_ctor_info pointers. This still allows higher-order terms with
// 1024 arguments, which is more than ../LIMITATIONS.md promises.

#define MR_MAX_VARIABLE_ARITY       MR_MAX_VIRTUAL_R_REG

// Constructors for the MR_TypeCtorDesc ADT

#define MR_TYPECTOR_DESC_MAKE_PRED(Arity)                               \
    ( (MR_TypeCtorDesc) ((Arity) * 4) )
#define MR_TYPECTOR_DESC_MAKE_FUNC(Arity)                               \
    ( (MR_TypeCtorDesc) ((Arity) * 4 + 1) )
#define MR_TYPECTOR_DESC_MAKE_TUPLE(Arity)                              \
    ( (MR_TypeCtorDesc) ((Arity) * 4 + 2) )
#define MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(type_ctor_info)               \
    ( MR_CHECK_EXPR_TYPE(type_ctor_info, MR_TypeCtorInfo),              \
      (MR_TypeCtorDesc) type_ctor_info )

// Access macros for the MR_TypeCtor ADT.
//
// The MR_TYPECTOR_DESC_GET_VA_* macros should only be called if
// MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns true.
// The MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO() macro
// should only be called if MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns false.

#define MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(T)                           \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (MR_Unsigned) (T) <= (4 * MR_MAX_VARIABLE_ARITY + 2) )
#define MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(T)              \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (MR_TypeCtorInfo) (T) )
#define MR_TYPECTOR_DESC_GET_VA_ARITY(T)                                \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (MR_Unsigned) (T) / 4 )
#define MR_TYPECTOR_DESC_GET_VA_NAME(T)                                 \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (MR_ConstString) (((MR_Unsigned) (T) % 4 == 0)                    \
        ? "pred"                                                        \
        : (((MR_Unsigned) (T) % 4 == 1)                                 \
            ? "func"                                                    \
            : "{}" )) )
#define MR_TYPECTOR_DESC_GET_VA_MODULE_NAME(T)                          \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (MR_ConstString) "builtin" )
#define MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(T)                       \
    ( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),                           \
      (((MR_Unsigned) (T) % 4 == 0)                                     \
        ? &MR_TYPE_CTOR_INFO_NAME(builtin, pred, 0)                     \
        : (((MR_Unsigned) (T) % 4 == 1)                                 \
            ? &MR_TYPE_CTOR_INFO_NAME(builtin, func, 0)                 \
            : &MR_TYPE_CTOR_INFO_NAME(builtin, tuple, 0))) )
#define MR_TYPECTOR_DESC_GET_TYPE_CTOR_INFO(T)                          \
    ( MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(T)                             \
      ? MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(T)                       \
      : MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(T))

// Create and return a MR_TypeCtorDesc that describes the same type as
// type_ctor_info. If type_ctor_info is of variable arity, take the arity
// from type_info, which should be the type_info that type_ctor_info was
// extracted from.

extern  MR_TypeCtorDesc MR_make_type_ctor_desc(MR_TypeInfo type_info,
                            MR_TypeCtorInfo type_ctor_info);

// Create and return a MR_TypeCtorDesc that describes the same type as
// type_ctor_info. If type_ctor_info is of variable arity, take the arity
// from pseudo, which should be the pseudo_type_info that type_ctor_info was
// extracted from.

extern  MR_TypeCtorDesc MR_make_type_ctor_desc_pseudo(MR_PseudoTypeInfo pseudo,
                            MR_TypeCtorInfo type_ctor_info);

// Given type_info, return the MR_TypeCtorDesc describing its outermost type
// constructor in *type_ctor_desc_ptr and a list of the type_infos of its
// argument types in *arg_type_info_list_ptr. If collapse_equivalences is
// MR_TRUE, then expand out the equivalences in type_info first.
//
// You need to wrap MR_{save/restore}_transient_registers() around
// calls to this function.

extern  void            MR_type_ctor_and_args(MR_TypeInfo type_info,
                            MR_bool collapse_equivalences,
                            MR_TypeCtorDesc *type_ctor_desc_ptr,
                            MR_Word *arg_type_info_list_ptr);

// Given pseudo_type_info representing a variable, return MR_FALSE. Given a
// pseudo_type_info representing a nonvariable type, return MR_TRUE, and
// return the MR_TypeCtorDesc describing its outermost type constructor
// in *type_ctor_desc_ptr and a list of the pseudo_type_infos of its argument
// types in *arg_type_info_list_ptr. If collapse_equivalences is MR_TRUE,
// then expand out the equivalences in pseudo_type_info first.
//
// You need to wrap MR_{save/restore}_transient_registers() around
// calls to this function.

extern  MR_bool         MR_pseudo_type_ctor_and_args(MR_PseudoTypeInfo
                            pseudo_type_info,
                            MR_bool collapse_equivalences,
                            MR_TypeCtorDesc *type_ctor_desc_ptr,
                            MR_Word *arg_type_info_list_ptr);

// ML_make_type(arity, type_ctor_info, arg_types_list):
//
// Construct and return a type_info for a type using the specified type_ctor
// for the type constructor, and using the arguments specified in
// arg_types_list for the type arguments (if any).
//
// Assumes that the arity of the type constructor represented by type_ctor_info
// and the length of the arg_types_list are both equal to `arity'.
//
// You need to wrap MR_{save/restore}_transient_registers() around
// calls to this function.

extern  MR_TypeInfo     MR_make_type(int arity, MR_TypeCtorDesc type_ctor_desc,
                            MR_Word arg_type_list);

// Compare two type_ctor_info structures, using an ordering based on the
// module names, type names and arities of the types represented by tcd1/tcd2.
// Return MR_COMPARE_GREATER, MR_COMPARE_EQUAL, or MR_COMPARE_LESS,
// depending on whether tcd1 is greater than, equal to, or less than tcd2.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  int             MR_compare_type_ctor_desc(MR_TypeCtorDesc tcd1,
                            MR_TypeCtorDesc tcd2);

// Unify two type_ctor_info structures, using an ordering based on the
// module names, type names and arities of the types represented by tcd1/tcd2.
// Return MR_TRUE iff tcd1 and tcd2 represent the same type constructor,
// and MR_FALSE otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool         MR_unify_type_ctor_desc(MR_TypeCtorDesc tcd1,
                            MR_TypeCtorDesc tcd2);

#endif  // MERCURY_TYPE_DESC_H
