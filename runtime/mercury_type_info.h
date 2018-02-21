// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2007, 2009, 2011-2012 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// Definitions of the types defining the type_ctor_infos, type_infos,
// pseudo_type_infos, base_typeclass_infos and typeclass_infos generated
// by the Mercury compiler, together with the macros and functions for
// manipulating them.
//
// There is a newer and better design for representing typeclass_infos
// in mercury_typeclass_info.h, but it is not used yet.
//
// Changes to the structures of type_infos and pseudo_type_infos
// may also require changes in:
//
//      compiler/polymorphism.m
//      compiler/pseudo_type_info.m
//      compiler/higher_order.m
//
// Changes to the structures of type_ctor_infos may require changes in:
//
//      compiler/type_ctor_info.m
//      compiler/rtti.m
//      compiler/rtti_out.m
//      compiler/rtti_to_mlds.m
//          (for updating the compiler-generated RTTI structures)
//
//      library/array.m
//      library/builtin.m
//      library/private_builtin.m
//      library/type_desc.m
//      runtime/mercury_bootstrap.c
//          (for updating the hand-written RTTI structures)
//
// Both kinds of changes will of course also require changes to the code
// that traverses type_infos and type_ctor_infos:
//
//      runtime/mercury_deep_copy_body.h
//      runtime/mercury_table_type_body.h
//      runtime/mercury_type_info.c
//      library/type_desc.m
//
//      java/ *.java
//          (for updating the Java backend RTTI structures)
//
// XXX Document what files changes may be required in after changes to the
// structures of base_typeclass_infos and typeclass_infos.

#ifndef MERCURY_TYPE_INFO_H
#define MERCURY_TYPE_INFO_H

#include "mercury_std.h"    // for `MR_STRINGIFY', `MR_PASTEn' and MR_CALL
#include "mercury_types.h"      // for `MR_Word'
#include "mercury_tags.h"       // for `MR_DEFINE_BUILTIN_ENUM_CONST'
#include "mercury_hlc_types.h"  // for `MR_Mercury_Type_Info'
#include <stdio.h>              // for FILE

////////////////////////////////////////////////////////////////////////////

// The version of the RTTI data structures -- useful for bootstrapping.
// MR_RTTI_VERSION sets the version number in the handwritten type_ctor_infos.
// If you write runtime code that checks this version number and
// can at least handle the previous version of the data
// structure, it makes it easier to bootstrap changes to the data
// structures used for RTTI.
//
// This number should be kept in sync with type_ctor_info_rtti_version in
// compiler/type_ctor_info.m.

#define MR_RTTI_VERSION                     MR_RTTI_VERSION__UINT
#define MR_RTTI_VERSION__INITIAL            2
#define MR_RTTI_VERSION__USEREQ             3
#define MR_RTTI_VERSION__CLEAN_LAYOUT       4
#define MR_RTTI_VERSION__VERSION_NO         5
#define MR_RTTI_VERSION__COMPACT            6
#define MR_RTTI_VERSION__REP                7
#define MR_RTTI_VERSION__FLAG               8
#define MR_RTTI_VERSION__STABLE_FOREIGN     9
#define MR_RTTI_VERSION__TYPE_INFO_ZERO     10
#define MR_RTTI_VERSION__DUMMY              11
#define MR_RTTI_VERSION__FUNCTOR_NUMBERS    12
#define MR_RTTI_VERSION__BITMAP             13
#define MR_RTTI_VERSION__DIRECT_ARG         14
#define MR_RTTI_VERSION__ARG_WIDTHS         15
#define MR_RTTI_VERSION__FUNCTOR_SUBTYPE    16
#define MR_RTTI_VERSION__UINT               17

// Check that the RTTI version is in a sensible range.
// The lower bound should be the lowest currently supported version number.
// The upper bound is the current version number.
// If you increase the lower bound, you should also increase the binary
// compatibility version number in runtime/mercury_grade.h (MR_GRADE_PART_0).
//
// Note that the definition of this macro matters only if it used, and (for
// efficiency) it shouldn't be used except if a period of transition between
// different versions requires different treatment of RTTI structures generated
// by different compiler versions.

#define MR_TYPE_CTOR_INFO_CHECK_RTTI_VERSION_RANGE(typector)            \
    assert((typector)->MR_type_ctor_version >= MR_RTTI_VERSION__ARG_WIDTHS)

////////////////////////////////////////////////////////////////////////////

// The C structures of typeinfos and pseudotypeinfos are sort of lies,
// for two reasons. First, we want one C type that can describe both first
// order, and higher order and tuple (pseudo-) typeinfos, and they have
// different structures (higher order and tuple (pseudo-) typeinfos have
// an extra word, the arity, between the type_ctor_info and the argument
// (pseudo-) typeinfos). Second, we can't rely on the C compiler having a
// mechanism for the declaration of dynamically sized vectors embedded in
// structures.
//
// Instead, the types MR_TypeInfo and MR_PseudoTypeInfo are designed as
// error-detection devices. Values of these types should be manipulated
// only through the macros defined below; the fields of the structures should
// not be accessed directly, and there should be no casts involving such
// values, except in the interface between code written in Mercury and code
// written in C, in which case casts to MR_(Pseudo)TypeInfo and back to MR_Word
// may be required. If this discipline is followed, the macros should catch
// most errors, such as passing pseudo typeinfos where typeinfos are expected.
//
// A pseudo typeinfo is the same as a typeinfo (see polymorphism.m) but
// may also store free type variables, represented as small integers:
// 1 to 512 represent universally quantified type variables
// and 513 to 1024 represent existentially quantified type variables.
// We do not use zero to represent any type variable, for two reasons.
// First, variable numbering starts at one inside the compiler. Second,
// starting at one allows us to use universally quantified type variable
// numbers directly as an offset into a (non-higher-order) typeinfo.
//
// This scheme relies on the bit patterns of these integers corresponding
// to memory that is either inaccessible (due to the first page of virtual
// memory being invalid) or is guaranteed to contain something other than
// type_ctor_info structures (such as the code of the program).

// First define generic macro versions of these struct types;
// these are used in the code that the compiler generates
// for static constant typeinfos and pseudotypeinfos.

#define MR_FIXED_ARITY_TYPEINFO_STRUCT(NAME, ARITY)                     \
    struct NAME {                                                       \
        MR_TypeCtorInfo     MR_ti_type_ctor_info;                       \
        MR_TypeInfo         MR_ti_fixed_arity_arg_typeinfos[ARITY];     \
    }

// Tuple types also use the higher-order type-info structure.
#define MR_VAR_ARITY_TYPEINFO_STRUCT(NAME, ARITY)                       \
    struct NAME {                                                       \
        MR_TypeCtorInfo     MR_ti_type_ctor_info;                       \
        MR_Integer          MR_ti_var_arity_arity;                      \
        MR_TypeInfo         MR_ti_var_arity_arg_typeinfos[ARITY];       \
    }

#define MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(NAME, ARITY)                   \
    struct NAME {                                                           \
        MR_TypeCtorInfo     MR_pti_type_ctor_info;                          \
        MR_PseudoTypeInfo   MR_pti_fixed_arity_arg_pseudo_typeinfos[ARITY]; \
    }

// Tuple types also use the higher-order pseudo-type-info structure.
#define MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(NAME, ARITY)                   \
    struct NAME {                                                         \
        MR_TypeCtorInfo     MR_pti_type_ctor_info;                        \
        MR_Integer          MR_pti_var_arity_arity;                       \
        MR_PseudoTypeInfo   MR_pti_var_arity_arg_pseudo_typeinfos[ARITY]; \
    }

// Now define specific versions of these struct types, which are used by
// the MR_TypeInfo and MR_PseudoTypeInfo typedefs in mercury_types.h.

MR_VAR_ARITY_TYPEINFO_STRUCT(MR_TypeInfo_Almost_Struct,
        MR_VARIABLE_SIZED);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_PseudoTypeInfo_Almost_Struct,
        MR_VARIABLE_SIZED);

// Define the C structures and types of all the type_info and pseudo_type_info
// structures generated by the compiler for types of a given arity.
//
// Since standard C doesn't support zero-sized arrays, we use the same
// definitions for arity zero as for arity one.

#define MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(Arity)          \
    typedef MR_FIXED_ARITY_TYPEINFO_STRUCT(                             \
        MR_PASTE2(MR_FA_TypeInfo_Struct, Arity), Arity)                 \
        MR_PASTE2(MR_FA_TypeInfo_Struct, Arity);                        \
    typedef MR_VAR_ARITY_TYPEINFO_STRUCT(                               \
        MR_PASTE2(MR_VA_TypeInfo_Struct, Arity), Arity)                 \
        MR_PASTE2(MR_VA_TypeInfo_Struct, Arity);                        \
    typedef MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(                       \
        MR_PASTE2(MR_FA_PseudoTypeInfo_Struct, Arity), Arity)           \
        MR_PASTE2(MR_FA_PseudoTypeInfo_Struct, Arity);                  \
    typedef MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(                         \
        MR_PASTE2(MR_VA_PseudoTypeInfo_Struct, Arity), Arity)           \
        MR_PASTE2(MR_VA_PseudoTypeInfo_Struct, Arity);

#define MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_ZERO                  \
    typedef struct MR_FA_TypeInfo_Struct1 MR_FA_TypeInfo_Struct0;             \
    typedef struct MR_VA_TypeInfo_Struct1 MR_VA_TypeInfo_Struct0;             \
    typedef struct MR_FA_PseudoTypeInfo_Struct1 MR_FA_PseudoTypeInfo_Struct0; \
    typedef struct MR_VA_PseudoTypeInfo_Struct1 MR_VA_PseudoTypeInfo_Struct0;

// We hard-code the declarations of all four structures (fixed and variable
// arity type_infos and pseudo_type_infos) for all arities up to twenty.
// (This number should be kept in sync with max_always_declared_arity in
// rtti_out.m.) The LLDS back end declares the structures for arities beyond
// this as necessary. The MLDS back end doesn't (yet) do so, so this imposes
// a fixed limit on the arities of types. (If this is exceeded, you will get
// a parse error in the generated C code, due to an undeclared type.)
//
// Note that the generic code for compare and unify for the MLDS back end
// also has a fixed limit of five on the arity of types (other than
// higher-order and tuple types, which have no limit). Fortunately types
// with a high arity tend not to be used very often, so this is probably OK
// for now...

MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_ZERO
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(1)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(2)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(3)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(4)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(5)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(6)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(7)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(8)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(9)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(10)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(11)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(12)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(13)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(14)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(15)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(16)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(17)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(18)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(19)
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(20)

// When converting a MR_PseudoTypeInfo to a MR_TypeInfo, we need the
// MR_TypeInfos corresponding to the type variables in the MR_PseudoTypeInfo.
// A MR_TypeInfoParams array serves this purpose. Because type variables
// start at one, MR_TypeInfoParams arrays also start at one.

typedef MR_TypeInfo             *MR_TypeInfoParams;

// When deep copying a MR_PseudoTypeInfo, we need to know the parameters
// of a non-variable pseudo_type_info, which are themselves pseudo_type_infos.
// A MR_PseudoTypeInfoParams array serves this purpose. Because type variables
// start at one, MR_PseudoTypeInfoParams arrays also start at one.

typedef MR_PseudoTypeInfo       *MR_PseudoTypeInfoParams;

// MR_PSEUDOTYPEINFO_EXIST_VAR_BASE should be kept in sync with
// base_type_layout__pseudo_typeinfo_min_exist_var in base_type_layout.m.
//
// MR_PSEUDOTYPEINFO_MAX_VAR should be kept in sync with
// base_type_layout__pseudo_typeinfo_max_var in base_type_layout.m,
// and with the default value of MR_VARIABLE_SIZED in mercury_conf_params.h.

#define MR_PSEUDOTYPEINFO_EXIST_VAR_BASE    512
#define MR_PSEUDOTYPEINFO_MAX_VAR           1024

// Macros for accessing pseudo_type_infos.
//
// The MR_TYPE_VARIABLE_* macros should only be called if
// MR_PSEUDO_TYPEINFO_IS_VARIABLE() returns MR_TRUE.

#define MR_PSEUDO_TYPEINFO_IS_VARIABLE(T)                               \
    ( MR_CHECK_EXPR_TYPE((T), MR_PseudoTypeInfo),                       \
      (MR_Unsigned) (T) <= MR_PSEUDOTYPEINFO_MAX_VAR )

#define MR_TYPE_VARIABLE_IS_EXIST_QUANT(T)                              \
    ( MR_CHECK_EXPR_TYPE((T), MR_PseudoTypeInfo),                       \
      (MR_Word) (T) > MR_PSEUDOTYPEINFO_EXIST_VAR_BASE )
#define MR_TYPE_VARIABLE_IS_UNIV_QUANT(T)                               \
    ( MR_CHECK_EXPR_TYPE((T), MR_PseudoTypeInfo),                       \
      (MR_Word) (T) <= MR_PSEUDOTYPEINFO_EXIST_VAR_BASE )

// This macro converts a pseudo_type_info to a type_info.
// It should only be called if the pseudo_type_info is ground,
// i.e. contains no type variables.

#define MR_pseudo_type_info_is_ground(pseudo_type_info)                 \
    ( MR_CHECK_EXPR_TYPE((pseudo_type_info), MR_PseudoTypeInfo),        \
      (MR_TypeInfo) (pseudo_type_info) )

// Macros for retrieving things from type_infos and pseudo_type_infos.

#define MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)                       \
    (((type_info)->MR_ti_type_ctor_info != NULL)                        \
        ? (type_info)->MR_ti_type_ctor_info                             \
        : (MR_TypeCtorInfo) (type_info))

#define MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo_type_info)         \
    (((pseudo_type_info)->MR_pti_type_ctor_info != NULL)                \
        ? (pseudo_type_info)->MR_pti_type_ctor_info                     \
        : (MR_TypeCtorInfo) (pseudo_type_info))

#define MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info)                      \
    ((type_info)->MR_ti_var_arity_arity)

#define MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo_type_info)        \
    ((pseudo_type_info)->MR_pti_var_arity_arity)

#define MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info)               \
    ((MR_TypeInfoParams) &(type_info)->MR_ti_type_ctor_info)

#define MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pseudo_type_info) \
    ((MR_PseudoTypeInfoParams) &(pseudo_type_info)->MR_pti_type_ctor_info)

#define MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info)                 \
    ((MR_TypeInfoParams) &(type_info)->MR_ti_var_arity_arity)

#define MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pseudo_type_info)   \
    ((MR_PseudoTypeInfoParams) &(pseudo_type_info)->MR_pti_var_arity_arity)

// Macros for creating type_infos and pseudo_type_infos.

#define MR_fixed_arity_type_info_size(arity)                            \
    (1 + (arity))

#define MR_var_arity_type_info_size(arity)                              \
    (2 + (arity))

#define MR_fixed_arity_pseudo_type_info_size(arity)                     \
    (1 + (arity))

#define MR_var_arity_pseudo_type_info_size(arity)                       \
    (2 + (arity))

#define MR_fill_in_fixed_arity_type_info(arena, type_ctor_info, vector) \
    do {                                                                \
        MR_TypeInfo new_ti;                                             \
        new_ti = (MR_TypeInfo) (arena);                                 \
        new_ti->MR_ti_type_ctor_info = (type_ctor_info);                \
        (vector) = (MR_TypeInfoParams) &new_ti->MR_ti_type_ctor_info;   \
    } while (0)

#define MR_fill_in_var_arity_type_info(arena, type_ctor_info, arity, vector)\
    do {                                                                \
        MR_TypeInfo new_ti;                                             \
        new_ti = (MR_TypeInfo) (arena);                                 \
        new_ti->MR_ti_type_ctor_info = (type_ctor_info);                \
        new_ti->MR_ti_var_arity_arity = (arity);                        \
        (vector) = (MR_TypeInfoParams) &new_ti->MR_ti_var_arity_arity;  \
    } while (0)

#define MR_fill_in_fixed_arity_pseudo_type_info(arena, type_ctor_info, vector) \
    do {                                                                \
        MR_NCPseudoTypeInfo new_pti;                                    \
        new_pti = (MR_NCPseudoTypeInfo) (arena);                        \
        new_pti->MR_pti_type_ctor_info = (type_ctor_info);              \
        (vector) = (MR_PseudoTypeInfoParams) &new_pti->MR_pti_type_ctor_info;  \
    } while (0)

#define MR_fill_in_var_arity_pseudo_type_info(arena, type_ctor_info, arity, vector)\
    do {                                                                \
        MR_NCPseudoTypeInfo new_pti;                                    \
        new_pti = (MR_NCPseudoTypeInfo) (arena);                        \
        new_pti->MR_pti_type_ctor_info = (type_ctor_info);              \
        new_pti->MR_pti_var_arity_arity = (arity);                      \
        (vector) = (MR_PseudoTypeInfoParams) &new_pti->MR_pti_var_arity_arity; \
    } while (0)

#define MR_static_type_info_arity_0(name, ctor)                         \
    struct {                                                            \
        MR_TypeCtorInfo field1;                                         \
    } name = {                                                          \
        (MR_TypeCtorInfo) (ctor)                                        \
    };

#define MR_static_type_info_arity_1(name, ctor, ti1)                    \
    struct {                                                            \
        MR_TypeCtorInfo field1;                                         \
        MR_TypeInfo     field2;                                         \
    } name = {                                                          \
        (MR_TypeCtorInfo) (ctor),                                       \
        (MR_TypeInfo)     (ti1)                                         \
    };

#define MR_static_type_info_arity_2(name, ctor, ti1, ti2)               \
    struct {                                                            \
        MR_TypeCtorInfo field1;                                         \
        MR_TypeInfo     field2;                                         \
        MR_TypeInfo     field3;                                         \
    } name = {                                                          \
        (MR_TypeCtorInfo) (ctor),                                       \
        (MR_TypeInfo)     (ti1),                                        \
        (MR_TypeInfo)     (ti2)                                         \
    };

////////////////////////////////////////////////////////////////////////////

// Definitions for handwritten code, mostly for mercury_compare_typeinfo.

#define MR_COMPARE_EQUAL    0
#define MR_COMPARE_LESS     1
#define MR_COMPARE_GREATER  2

#define MR_BOOL_NO          0
#define MR_BOOL_YES         1

#define MR_UNBOUND          0

////////////////////////////////////////////////////////////////////////////

// Definitions for accessing typeclass_infos and base_typeclass_infos.
// Their structure is described type_class_transformation.html in
// compiler/notes.

// Extract the base_typeclass_info from a typeclass_info.

#define MR_typeclass_info_base(tci)                                     \
    (*(MR_Word **)(tci))

// The following macros look up fields of the base_typeclass_info in the given
// typeclass_info. These fields yield information about the instance
// declaration from which the given typeclass_info is constructed, or about the
// type class declaration itself.
//
// MR_typeclass_info_num_instance_type_vars gives the number of type variables
// in the head of the instance declaration that aren't constrained by type
// class constraints on the instance declaration. (Soon, this will change to
// simply the number of type variables in the head of the instance
// declaration.)
//
// MR_typeclass_info_num_instance_constraints gives the number of constraints
// on the instance declaration.
//
// MR_typeclass_info_num_extra_instance_args gives the sum of
// MR_typeclass_info_num_instance_type_vars and
// MR_typeclass_info_num_instance_constraints.
//
// MR_typeclass_info_num_superclasses gives the number of typeclass constraints
// on the typeclass declaration.
//
// MR_typeclass_info_num_params gives the number of parameters of the typeclass
// declaration, which perforce is also the number of parameters of the instance
// declaration; in other words, the arity of the type class.

#define MR_typeclass_info_num_extra_instance_args(tci)                  \
    ((MR_Integer) MR_typeclass_info_base(tci)[0])
#define MR_typeclass_info_num_instance_constraints(tci)                 \
    ((MR_Integer) MR_typeclass_info_base(tci)[1])
#define MR_typeclass_info_num_superclasses(tci)                         \
    ((MR_Integer) MR_typeclass_info_base(tci)[2])
#define MR_typeclass_info_num_params(tci)                               \
    ((MR_Integer) MR_typeclass_info_base(tci)[3])
#define MR_typeclass_info_num_methods(tci)                              \
    ((MR_Integer) MR_typeclass_info_base(tci)[4])
#define MR_typeclass_info_class_method(tci, n)                          \
    ((MR_Code *) MR_typeclass_info_base(tci)[(n+4)])

#define MR_typeclass_info_num_instance_type_vars(tci)                   \
    ( MR_typeclass_info_num_extra_instance_args(tci)                    \
    - MR_typeclass_info_num_instance_constraints(tci))

// MR_typeclass_info_instance_tvar_type_info returns a typeinfo for
// one of the type variables in the instance declaration that isn't constrained
// by a type class constraints on the instance declaration. (Soon, this will
// change, as above.)
//
// MR_typeclass_info_arg_typeclass_info returns a typeclass_info for one of the
// constraints on the instance declaration.
//
// MR_typeclass_info_extra_instance_arg returns either what
// MR_typeclass_info_instance_tvar_type_info or
// MR_typeclass_info_arg_typeclass_info returns, depending on the value of n
// supplied.
//
// Except for the sanity checks, the following macros have the same
// definitions. This is because calls to MR_typeclass_info_arg_typeclass_info
// must already have the number of (unconstrained) type variables in the head
// of the instance declaration added to it.

#ifdef  MR_CHECK_TYPECLASS_REFS
  #define MR_typeclass_info_extra_instance_arg(tci, n)                  \
    ((0 < (n) && (n) <= MR_typeclass_info_num_extra_instance_args(tci)) \
    ? (((MR_Word *)(tci))[(n)])                                         \
    : MR_typeclass_ref_error((tci), (n),                                \
        "MR_typeclass_info_extra_instance_arg"))
  #define MR_typeclass_info_instance_tvar_type_info(tci, n)             \
    ((0 < (n) && (n) <= MR_typeclass_info_num_instance_type_vars(tci))  \
    ? (((MR_Word *)(tci))[(n)])                                         \
    : MR_typeclass_ref_error((tci), (n),                                \
        "MR_typeclass_info_instance_tvar_type_info"))
  #define MR_typeclass_info_arg_typeclass_info(tci, n)                  \
    ((MR_typeclass_info_num_instance_type_vars(tci) < (n)               \
      && (n) <= MR_typeclass_info_num_extra_instance_args(tci))         \
    ? (((MR_Word *)(tci))[(n)])                                         \
    : MR_typeclass_ref_error((tci), (n),                                \
        "MR_typeclass_info_arg_typeclass_info"))
#else
  #define MR_typeclass_info_extra_instance_arg(tci, n)                  \
    (((MR_Word *)(tci))[(n)])
  #define MR_typeclass_info_instance_tvar_type_info(tci, n)             \
    (((MR_Word *)(tci))[(n)])
  #define MR_typeclass_info_arg_typeclass_info(tci, n)                  \
    (((MR_Word *)(tci))[(n)])
#endif

// MR_typeclass_info_superclass_info return a typeclass_info for one of the
// constraints on the typeclass declaration, i.e. for one this class's
// superclasses.
//
// MR_typeclass_info_param_type_info returns a typeinfo for one the types
// to which the type class constraint applies, i.e. for one of the types bound
// to the type variables in the head of the type class declaration.
//
// Except for the sanity checks, the following macros have the same
// definitions. This is because calls to MR_typeclass_info_param_type_info
// must already have the number of superclasses for the class added to it.

#ifdef  MR_CHECK_TYPECLASS_REFS
  #define MR_typeclass_info_superclass_info(tci, n)                     \
    ((0 < (n) && (n) <= MR_typeclass_info_num_superclasses(tci))        \
    ? (((MR_Word *)(tci))[                                              \
        MR_typeclass_info_num_extra_instance_args(tci) + (n)])          \
    : MR_typeclass_ref_error((tci), (n), "MR_typeclass_info_superclass_info"))
  #define MR_typeclass_info_param_type_info(tci, n)                     \
    ((MR_typeclass_info_num_superclasses(tci) < (n)                     \
      && ((n) - MR_typeclass_info_num_superclasses(tci))                \
        <= MR_typeclass_info_num_params(tci))                           \
    ? (((MR_Word *)(tci))[                                              \
        MR_typeclass_info_num_extra_instance_args(tci) + (n)])          \
    : MR_typeclass_ref_error((tci), (n), "MR_typeclass_info_param_type_info"))
#else
  #define MR_typeclass_info_superclass_info(tci, n)                     \
    (((MR_Word *)(tci))[MR_typeclass_info_num_extra_instance_args(tci) + (n)])
  #define MR_typeclass_info_param_type_info(tci, n)                     \
    (((MR_Word *)(tci))[MR_typeclass_info_num_extra_instance_args(tci) + (n)])
#endif

// Report an attempt to access a typeclass_info with incorrect parameters,
// and abort. MR_typeclass_ref_error doesn't return; the return value is there
// only to appease the C typechecker.

extern  MR_Word MR_typeclass_ref_error(MR_Word tci, int n, const char *msg);

////////////////////////////////////////////////////////////////////////////

// Definitions and functions for categorizing data representations.

// MR_TypeCtorRep specifies the representation scheme for a particular type
// constructor.
//
// Any changes in this definition will also require changes in the
// MR_CTOR_REP_NAMES macro below, in library/rtti_implementation.m
// (definitely the list of type_ctor_reps, maybe the bodies of predicates),
// in library/private_builtin.m (in two places), in java/runtime/
// TypeCtorRep.java, and compiler/rtti.m.
//
// Additions to the end of this enum can be handled naturally, but changes
// in the meanings of already assigned values require bootstrapping
// with RTTI-version-dependent code.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_ENUM),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_ENUM_USEREQ),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_DU),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_DU_USEREQ),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_NOTAG),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_NOTAG_USEREQ),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_EQUIV),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_FUNC),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_INT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UINT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_CHAR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_FLOAT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_STRING),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_PRED),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_SUBGOAL),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_VOID),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_C_POINTER),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TYPEINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TYPECLASSINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_ARRAY),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_SUCCIP),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_HP),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_CURFR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_MAXFR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_REDOFR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_REDOIP),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TRAIL_PTR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TICKET),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_NOTAG_GROUND),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_EQUIV_GROUND),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TUPLE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UNUSED1),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UNUSED2),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TYPECTORINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_BASETYPECLASSINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TYPEDESC),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_TYPECTORDESC),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_FOREIGN),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_REFERENCE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_STABLE_C_POINTER),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_STABLE_FOREIGN),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_PSEUDOTYPEDESC),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_DUMMY),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_BITMAP),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_FOREIGN_ENUM),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_INT8),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UINT8),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_INT16),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UINT16),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_INT32),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UINT32),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_INT64),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UINT64),
    // MR_TYPECTOR_REP_UNKNOWN should remain the last alternative;
    // MR_TYPE_CTOR_STATS depends on this.

    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TYPECTOR_REP_UNKNOWN)
} MR_TypeCtorRep;

// We cannot put enums into structures as bit fields. To avoid wasting space,
// we put MR_TypeCtorRepInts into structures instead of MR_TypeCtorReps
// themselves.
//
// We need more than eight bits for a TypeCtorRep. The number of different
// TypeCtorRep values requires six bits to differentiate them, and in .rt
// grades on 64-bit machines we need another three bits for a primary tag
// value.
// XXX We don't have .rt grades anymore.

typedef MR_int_least16_t  MR_TypeCtorRepInt;

// This macro is intended to be used for the initialization of an array
// that converts each MR_TypeCtorRep into a string form. Therefore it
// must be kept synchronized with the definition of MR_TypeCtorRep.

#define MR_CTOR_REP_NAMES                                               \
    "ENUM",                                                             \
    "ENUM_USEREQ",                                                      \
    "DU",                                                               \
    "DU_USEREQ",                                                        \
    "NOTAG",                                                            \
    "NOTAG_USEREQ",                                                     \
    "EQUIV",                                                            \
    "FUNC",                                                             \
    "INT",                                                              \
    "UINT",                                                             \
    "CHAR",                                                             \
    "FLOAT",                                                            \
    "STRING",                                                           \
    "PRED",                                                             \
    "SUBGOAL",                                                          \
    "VOID",                                                             \
    "C_POINTER",                                                        \
    "TYPE_INFO",                                                        \
    "TYPECLASS_INFO",                                                   \
    "ARRAY",                                                            \
    "SUCCIP",                                                           \
    "HP",                                                               \
    "CURFR",                                                            \
    "MAXFR",                                                            \
    "REDOFR",                                                           \
    "REDOIP",                                                           \
    "TRAIL_PTR",                                                        \
    "TICKET",                                                           \
    "NOTAG_GROUND",                                                     \
    "NOTAG_GROUND_USEREQ",                                              \
    "EQUIV_GROUND",                                                     \
    "TUPLE",                                                            \
    "RESERVED_ADDR",                                                    \
    "RESERVED_ADDR_USEREQ",                                             \
    "TYPE_CTOR_INFO",                                                   \
    "BASE_TYPECLASS_INFO",                                              \
    "TYPE_DESC",                                                        \
    "TYPE_CTOR_DESC",                                                   \
    "FOREIGN",                                                          \
    "REFERENCE",                                                        \
    "STABLE_C_POINTER",                                                 \
    "STABLE_FOREIGN",                                                   \
    "PSEUDO_TYPE_DESC",                                                 \
    "DUMMY",                                                            \
    "BITMAP",                                                           \
    "FOREIGN_ENUM",                                                     \
    "FOREIGN_ENUM_USEREQ",                                              \
    "INT8",                                                             \
    "UINT8",                                                            \
    "INT16",                                                            \
    "UINT16",                                                           \
    "INT32",                                                            \
    "UINT32",                                                           \
    "INT64",                                                            \
    "UINT64",                                                           \
    "UNKNOWN"

extern  MR_ConstString  MR_ctor_rep_name[];

////////////////////////////////////////////////////////////////////////////

// A typeclass constraint asserts the membership of a possibly nonground
// vector of types in a type class, as one may find constraining a typeclass
// declaration, an instance declaration, or a predicate/function declaration.
//
// Type class constraints for type classes with arity N will be of type
// MR_TypeClassConstraint_N. Generic code will manipulate them as if they were
// of type MR_TypeClassConstraint, getting the actual number of arguments from
// MR_tc_constr_type_class_info->MR_tc_decl_id->MR_tc_id_arity.
//
// Note that the arity cannot be zero, so we do not have to worry about
// zero-size arrays. On the other hand, type classes with more than even two
// arguments can be expected to be very rare. We define
// MR_TypeClassConstraint_N on demand for all N > 10 in the low-level grades
// but not yet in high-level grades.
//
// We will have to rethink this structure once we start supporting constructor
// classes.

#define MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(NAME, ARITY)                  \
    typedef struct MR_PASTE2(NAME, _Struct) {                               \
        MR_TypeClassDecl    MR_tc_constr_type_class;                        \
        MR_PseudoTypeInfo   MR_tc_constr_arg_ptis[ARITY];                   \
    } MR_PASTE2(NAME, Struct)

MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_1, 1);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_2, 2);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_3, 3);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_4, 4);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_5, 5);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_6, 6);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_7, 7);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_8, 8);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_9, 9);
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_10, 10);

typedef MR_TypeClassConstraint_5Struct          MR_TypeClassConstraintStruct;
typedef const MR_TypeClassConstraintStruct      *MR_TypeClassConstraint;

#define MR_STD_TYPECLASS_CONSTRAINT_ADDR(p)                             \
        ((MR_TypeClassConstraint) &((p).MR_tc_constr_type_class_info))

// The argument number field gives the offset in the cell (in a form in which
// it can be given to the MR_field macro directly) of either of the typeinfo
// itself or of the typeclassinfo containing the typeinfo. If the former,
// the offset field will be negative; otherwise, it will be an integer
// which can be given as a second argument to the MR_typeclass_info_type_info
// macro.

typedef struct {
    MR_int_least16_t    MR_exist_arg_num;
    MR_int_least16_t    MR_exist_offset_in_tci;
} MR_DuExistLocn;

// This structure contains information about the typeinfos of the
// existentially quantified type variables occurring in the types of some
// of the arguments of a functor in a du type.
//
// The MR_exist_typeinfos_plain field gives the number of typeinfos
// directly inserted at the start of the memory cell of the functor, while
// the MR_exist_tcis field gives the number of typeclassinfos
// inserted AFTER them. The arguments visible to the programmer start AFTER
// these two blocks, which means that when accessing them, one must add
// the sum of MR_exist_typeinfos_plain and MR_exist_tcis to
// the visible argument number in order to arrive at an offset in the cell.
//
// It is possible for a typeclassinfo to contain more than one type variable.
// The MR_exist_typeinfos_in_tci field contains the total number of typeinfos
// stored inside the typeclassinfos of the cell.
//
// The MR_exist_typeinfo_locns field points to an array of
// MR_ExistTypeInfoLocns. This array has MR_exist_typeinfos_plain +
// MR_exist_typeinfos_in_tci elements, each one of which describes
// the location (directly in the cell or indirectly inside a typeclassinfo)
// of the typeinfo for an existentially quantified type variable.
// The typeinfo for type variable N will be at the offset
// N - MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1. (The one is subtracted to convert
// from type var numbering, which starts at 1, to array offset numbering).
//
// The MR_exist_constraints field points to an array of type class constraints
// (each of which is a pointer to a type class constraint structure). The array
// contains MR_exist_tci elements, giving the constraint from which each
// typeclass_info in the functor is derived.

typedef struct {
    MR_int_least16_t                MR_exist_typeinfos_plain;
    MR_int_least16_t                MR_exist_typeinfos_in_tci;
    MR_int_least16_t                MR_exist_tcis;
    const MR_DuExistLocn            *MR_exist_typeinfo_locns;
    const MR_TypeClassConstraint    *MR_exist_constraints;
} MR_DuExistInfo;

// This type describes the implementation of a function symbol
// from a (proper) discriminated union type, whether it has standard
// or user-defined-equality.
//
// Functor descriptors are reachable from both the layout and functor tables.
// They all the information one may need about the function symbol, even
// though some of this information may be redundant along some access paths.
//
// The fields that you are likely to be interested in when you arrive at the
// functor descriptor through the functor table are clustered at the front,
// the fields that you are likely to be interested in when you arrive at the
// functor descriptor through the layout table are clustered at the back.
// This is an attempt to optimize cache effectiveness.
//
// The primary and secondary fields give the corresponding tag values, and
// the sectag_locn field gives the location of the secondary tag.
// MR_SECTAG_NONE_DIRECT_ARG is a sub-case of MR_SECTAG_NONE, where the
// function symbol is represented as a tagged pointer to its only argument.
//
// The ordinal field gives the position of the function symbol in the
// list of function symbols of the type; one function symbol compares
// as less than another iff its ordinal number is smaller.
//
// The orig_arity field records the visible arity of the functor, without
// the typeinfos and/or typeclass_infos added for existentially typed
// arguments.
//
// The arg_types field points to an array of pseudo typeinfos, one for each
// visible argument.
//
// The arg_type_contains_var field contains a bit vector which has one bit
// for each of the first N (currently N=15) arguments, and one bit shared
// between all the other arguments. One of the first N bits is set iff
// the type of the corresponding argument contains a type variable, while
// the last bit is set iff the types of any of the remaining arguments
// contains a type variable.
// This field is meant to be used only via the MR_arg_type_may_contain_var
// and MR_any_arg_type_may_contain_var macros below. In the absence of
// compiler-recorded information, these macros return conservative answers
// for any argument whose type is not represented in this bit vector.
//
// The arg_names field points to an array of field names, one for each
// visible argument. If no argument has a name, this field will be NULL.
//
// If the functor has any arguments whose types include existentially
// quantified type variables, the exist_info field will point to information
// about those type variables; otherwise, the exist_info field will be NULL.
//
// If every argument occupies exactly one word each, then the arg_locns
// field will be NULL. Otherwise, it points to an array of MR_DuArgLocn
// structures, describing the location and packing scheme of each visible
// argument.
//
// If any argument contains subtype information (for example, higher order
// mode information) then the subtype_info field will be
// MR_DU_SUBTYPE_INFO_EXISTS, otherwise it will be MR_DU_SUBTYPE_INFO_NONE.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_SECTAG_NONE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_SECTAG_NONE_DIRECT_ARG),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_SECTAG_LOCAL),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_SECTAG_REMOTE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_SECTAG_VARIABLE)
} MR_Sectag_Locn;

typedef struct {
    MR_int_least16_t        MR_arg_offset; // not including extra args
    MR_int_least8_t         MR_arg_shift;
    MR_int_least8_t         MR_arg_bits;
    // If MR_arg_bits is zero then the argument occupies the entire word.
    // If MR_arg_bits is -1 then the argument is a double-precision floating
    // point value occupying two words. Otherwise MR_arg_bits is non-zero and
    // gives the number of bits used by the argument. Storing the bit-mask
    // would be more useful, but would not be as compact.

} MR_DuArgLocn;

// This type describes the subtype constraints on the arguments of a functor.
// Currently, we only record whether any such constraints exist.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_FUNCTOR_SUBTYPE_NONE),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_FUNCTOR_SUBTYPE_EXISTS)
} MR_FunctorSubtype;

typedef struct {
    MR_ConstString          MR_du_functor_name;
    MR_int_least16_t        MR_du_functor_orig_arity;
    MR_int_least16_t        MR_du_functor_arg_type_contains_var;
    MR_Sectag_Locn          MR_du_functor_sectag_locn;
    MR_int_least8_t         MR_du_functor_primary;
    MR_int_least32_t        MR_du_functor_secondary;
    MR_int_least32_t        MR_du_functor_ordinal;
    const MR_PseudoTypeInfo *MR_du_functor_arg_types;
    const MR_ConstString    *MR_du_functor_arg_names;
    const MR_DuArgLocn      *MR_du_functor_arg_locns;
    const MR_DuExistInfo    *MR_du_functor_exist_info;
    MR_FunctorSubtype       MR_du_functor_subtype;
} MR_DuFunctorDesc;

typedef const MR_DuFunctorDesc              *MR_DuFunctorDescPtr;

// This macro represents the number of bits in the
// MR_du_functor_arg_type_contains_var field of a MR_DuFunctorDesc.
// It should be kept in sync with contains_var_bit_vector_size
// in base_type_layout.m.

#define MR_ARG_TYPE_CONTAINS_VAR_BIT_VECTOR_SIZE    16

#define MR_arg_has_own_contain_var_bit(arg_num)                         \
    ((arg_num) < MR_ARG_TYPE_CONTAINS_VAR_BIT_VECTOR_SIZE - 1)

#define MR_initial_arg_type_may_contain_var(functor_desc, arg_num)      \
    (((functor_desc)->MR_du_functor_arg_type_contains_var &             \
        (1 << (arg_num))) != 0)

#define MR_later_arg_type_may_contain_var(functor_desc)                 \
    (((functor_desc)->MR_du_functor_arg_type_contains_var &             \
        (1 << (MR_ARG_TYPE_CONTAINS_VAR_BIT_VECTOR_SIZE - 1))) != 0)

#define MR_arg_type_may_contain_var(functor_desc, arg_num)              \
    ( MR_arg_has_own_contain_var_bit(arg_num)                           \
    ? MR_initial_arg_type_may_contain_var((functor_desc), (arg_num))    \
    : MR_later_arg_type_may_contain_var(functor_desc))

#define MR_some_arg_type_contains_var(functor_desc)                     \
    ((functor_desc)->MR_du_functor_arg_type_contains_var > 0)

#define MR_du_subtype_none(tci, functor_desc)                           \
    ((tci)->MR_type_ctor_version < MR_RTTI_VERSION__FUNCTOR_SUBTYPE ||  \
        (functor_desc)->MR_du_functor_subtype == MR_FUNCTOR_SUBTYPE_NONE)

////////////////////////////////////////////////////////////////////////////

typedef struct {
    MR_ConstString      MR_enum_functor_name;
    MR_int_least32_t    MR_enum_functor_ordinal;
} MR_EnumFunctorDesc;

typedef const MR_EnumFunctorDesc            *MR_EnumFunctorDescPtr;

////////////////////////////////////////////////////////////////////////////

typedef struct {
    MR_ConstString      MR_foreign_enum_functor_name;
    MR_int_least32_t    MR_foreign_enum_functor_ordinal;
    MR_Integer          MR_foreign_enum_functor_value;
} MR_ForeignEnumFunctorDesc;

typedef const MR_ForeignEnumFunctorDesc     *MR_ForeignEnumFunctorDescPtr;

////////////////////////////////////////////////////////////////////////////

typedef struct {
    MR_ConstString      MR_notag_functor_name;
    MR_PseudoTypeInfo   MR_notag_functor_arg_type;
    MR_ConstString      MR_notag_functor_arg_name;
    MR_FunctorSubtype   MR_notag_functor_subtype;
} MR_NotagFunctorDesc;

typedef const MR_NotagFunctorDesc           *MR_NotagFunctorDescPtr;

#define MR_notag_subtype_none(tci, functor_desc)                        \
    ((tci)->MR_type_ctor_version < MR_RTTI_VERSION__FUNCTOR_SUBTYPE ||  \
        (functor_desc)->MR_notag_functor_subtype == MR_FUNCTOR_SUBTYPE_NONE)

////////////////////////////////////////////////////////////////////////////

// This type describes the function symbols that share the same primary tag.
// The sharers field gives their number, and thus also the size
// of the array of pointers to functor descriptors pointed to by the
// alternatives field.
//
// The intention is that if you have a word in a DU type that you want to
// interpret, you compute its primary tag and find its MR_DuPtagLayout.
// You then look at the locn field. If it is MR_SECTAG_NONE{,_DIRECT_ARG}, you
// index the alternatives field with zero; if it is MR_SECTAG_{LOCAL,REMOTE},
// you compute the secondary tag and index the alternatives field with that.
//
// A value of type MR_DuTypeLayout points to an array of MR_DuPtagLayout
// structures. The element at index k gives information about primary tag
// value k. The size of the array is recorded in the num_ptags field of the
// type_ctor_info.

typedef struct {
    MR_int_least32_t                MR_sectag_sharers;
    MR_Sectag_Locn                  MR_sectag_locn;
    const MR_DuFunctorDesc * const *MR_sectag_alternatives;
} MR_DuPtagLayout;

typedef const MR_DuPtagLayout *MR_DuTypeLayout;

////////////////////////////////////////////////////////////////////////////

// This type describes the function symbols in an enum type.
//
// An MR_EnumLayout points to an array of pointers to functor descriptors.
// There is one pointer for each function symbol, and thus the size of
// the array is given by the num_functors field of the type_ctor_info.
// The array is ordered on the integer value by which the functor is
// represented.
//
// The intention is that if you have a word in an enum type that you want to
// interpret, you index into the array with the word.
//

typedef MR_EnumFunctorDesc  **MR_EnumTypeLayout;

////////////////////////////////////////////////////////////////////////////

// This type describes the function symbols in a foreign enum type.
//
// An MR_ForeignEnumLayout points to an array of pointers to functor
// descriptors. There is one pointer for each of the function symbols, and
// thus the size of the array is given by the num_functors field of the
// type_ctor_info. The array is ordered by declaration order.
//
// NOTE: it is not possible to order this array by the integer value by
// which the functor is represented, since for foreign enums, these may be
// #defined constants, and the Mercury compiler won't know their actual values
// when it generates the type's ForeignEnumLayout structures.

typedef MR_ForeignEnumFunctorDesc **MR_ForeignEnumTypeLayout;

////////////////////////////////////////////////////////////////////////////

// This type describes the single function symbol in a notag type.
//
// An MR_NotagLayout points to the one functor descriptor of the type.
//
// The intention is that if you have a word in a notag type that you want to
// interpret, you look at the given functor descriptor.

typedef MR_NotagFunctorDesc *MR_NotagTypeLayout;

////////////////////////////////////////////////////////////////////////////

// This type describes the identity of the type that an equivalence type
// is equivalent to, and hence its layout.
//
// An MR_EquivLayout gives the pseudo typeinfo of the type that this type
// is equivalent to.
//
// The intention is that if you have a word in an equivalence type that you
// want to interpret, you expand the pseudo typeinfo into a real typeinfo,
// and then use that to interpret the word.

typedef MR_PseudoTypeInfo   MR_EquivLayout;

////////////////////////////////////////////////////////////////////////////

// Some types are defined differently for the MLDS back-end.

#ifdef MR_HIGHLEVEL_CODE
  // XXX This should be `MR_Box', but MR_Box is not visible here
  // (due to a cyclic dependency problem), so we use `void *' instead.

  typedef       void        *MR_ProcAddr;
#else
  typedef       MR_Code     *MR_ProcAddr;
#endif

////////////////////////////////////////////////////////////////////////////

// This type describes the layout in any kind of discriminated union
// type: du, enum, foreign_enum, or notag. In an equivalence type,
// it gives the identity of the equivalent-to type.
//
// The layout_init alternative is used only for static initializers,
// because ANSI C89 does not allow you to say which member of a union
// you are initializing, and instead forces you to initialize the first member.
// When we can rely on C99 compilers, layout_init should no longer be needed.

typedef union {
    const void                  *MR_layout_init;
    MR_DuTypeLayout             MR_layout_du;
    MR_EnumTypeLayout           MR_layout_enum;
    MR_ForeignEnumTypeLayout    MR_layout_foreign_enum;
    MR_NotagTypeLayout          MR_layout_notag;
    MR_EquivLayout              MR_layout_equiv;
} MR_TypeLayout;

////////////////////////////////////////////////////////////////////////////

// This type describes the function symbols in any kind of discriminated union
// type: du, enum, foreign_enum, and notag.
//
// The pointer in the union points to either an array of pointers to functor
// descriptors (for du, enum and foreign enum types) or to a single functor
// descriptor (for notag types). There is one functor descriptor
// for each function symbol, and thus the size of the array is given by
// the num_functors field of the type_ctor_info. Arrays are ordered
// on the name of the function symbol, and then on arity.
//
// The intention is that if you have a function symbol you want to represent,
// you can do binary search on the array for the symbol name and arity.
//
// The functors_init alternative is used only for static initializers;
// see the comment for MR_TypeLayout above.

typedef union {
    const void                  *MR_functors_init;
    MR_DuFunctorDesc            **MR_functors_du;
    MR_EnumFunctorDesc          **MR_functors_enum;
    MR_ForeignEnumFunctorDesc   **MR_functors_foreign_enum;
    MR_NotagFunctorDesc         *MR_functors_notag;
} MR_TypeFunctors;

// Map from ordinal (declaration order) functor numbers to lexicographic
// functor numbers which can be passed to construct.construct.

typedef const MR_Integer        *MR_FunctorNumberMap;

////////////////////////////////////////////////////////////////////////////

    // Structs defining the structure of type_ctor_infos.
    // A type_ctor_info describes the structure of a particular
    // type constructor. One of these is generated for every
    // `:- type' declaration.
    //
    // A change in the TypeCtorInfo structure also requires changes in the
    // files listed at the top of this file, as well as in the macros below.

struct MR_TypeCtorInfo_Struct {
    MR_Integer          MR_type_ctor_arity;
    MR_int_least8_t     MR_type_ctor_version;
    MR_int_least8_t     MR_type_ctor_num_ptags;         // if DU
    MR_TypeCtorRepInt   MR_type_ctor_rep_CAST_ME;
    MR_ProcAddr         MR_type_ctor_unify_pred;
    MR_ProcAddr         MR_type_ctor_compare_pred;
    MR_ConstString      MR_type_ctor_module_name;
    MR_ConstString      MR_type_ctor_name;
    MR_TypeFunctors     MR_type_ctor_functors;
    MR_TypeLayout       MR_type_ctor_layout;
    MR_int_least32_t    MR_type_ctor_num_functors;
    MR_int_least16_t    MR_type_ctor_flags;
    MR_FunctorNumberMap MR_type_ctor_functor_number_map;

// The following fields will be added later, once we can exploit them:
//  MR_TrieNodePtr      MR_type_ctor_std_table;
//  MR_ProcAddr         MR_type_ctor_prettyprinter;

};

// Check whether an MR_TypeCtorRepInt is a valid MR_TypeCtorRep value.

#define MR_type_ctor_rep_is_valid(rep_int)                                  \
    ((unsigned) (rep_int) <= (unsigned) MR_TYPECTOR_REP_UNKNOWN)
#define MR_type_ctor_has_valid_rep(type_ctor_info)                          \
    (MR_type_ctor_rep_is_valid(type_ctor_info->MR_type_ctor_rep_CAST_ME))

#define MR_type_ctor_rep(tci)                                               \
    ((MR_TypeCtorRep) ((tci)->MR_type_ctor_rep_CAST_ME))

#define MR_type_ctor_num_ptags(tci)                                         \
    ((tci)->MR_type_ctor_num_ptags)

#define MR_type_ctor_module_name(tci)                                       \
    ((tci)->MR_type_ctor_module_name)

#define MR_type_ctor_name(tci)                                              \
    ((tci)->MR_type_ctor_name)

#define MR_type_ctor_functors(tci)                                          \
    ((tci)->MR_type_ctor_functors)

#define MR_type_ctor_layout(tci)                                            \
    ((tci)->MR_type_ctor_layout)

#define MR_type_ctor_num_functors(tci)                                      \
    ((tci)->MR_type_ctor_num_functors)

// The flag bits here must agree with the ones in encode_type_ctor_flag
// in compiler/rtti.m.
//
// We used to have a "reserve tag" flag whose representation was 0x1,
// but we don't supported reserving tags anymore.
//
// The variable arity flag is set for builtin constructors whose arity is
// variable: at moment, this means functions, predicates and tuples.
//
// The kind of du flag is set for all discriminated union types, even if
// their representation is specialized (as enumerations, notag types etc).
//
// The dummy flag must be set for type constructors whose values are not
// actually passed around.

#define MR_TYPE_CTOR_FLAG_VARIABLE_ARITY        0x2
#define MR_TYPE_CTOR_FLAG_KIND_OF_DU            0x4

#define MR_type_ctor_has_variable_arity(tci)                                \
    ((tci)->MR_type_ctor_flags & MR_TYPE_CTOR_FLAG_VARIABLE_ARITY)
#define MR_type_ctor_is_kind_of_du(tci)                                     \
    ((tci)->MR_type_ctor_flags & MR_TYPE_CTOR_FLAG_KIND_OF_DU)

////////////////////////////////////////////////////////////////////////////

#ifdef MR_HIGHLEVEL_CODE

// Types for the wrapper versions of type-specific unify/compare procedures.

typedef MR_bool MR_CALL MR_UnifyFunc_0(MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_1(MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_2(MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_3(MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                                MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_4(MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_5(MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                                MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                                MR_Box, MR_Box);

typedef void MR_CALL MR_CompareFunc_0(MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_1(MR_Mercury_Type_Info,
                        MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_2(MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Comparison_Result *,
                        MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_3(MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                        MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_4(MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Comparison_Result *,
                        MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_5(MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                        MR_Mercury_Type_Info, MR_Mercury_Type_Info,
                        MR_Comparison_Result *, MR_Box, MR_Box);

#endif  // MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

// Macros to help the runtime and the library create type_ctor_info
// structures for builtin and special types.

#ifdef MR_HIGHLEVEL_CODE

  #define MR_DEFINE_TYPE_CTOR_INFO_TYPE                                 \
    const struct MR_TypeCtorInfo_Struct

  #define MR_NONSTD_TYPE_CTOR_INFO_NAME(m, n, a)                        \
    MR_PASTE2(m,                                                        \
    MR_PASTE2(__,                                                       \
    MR_PASTE2(m,                                                        \
    MR_PASTE2(__type_ctor_info_,                                        \
    MR_PASTE2(n,                                                        \
    MR_PASTE2(_, a))))))

  #define MR_TYPE_CTOR_INFO_NAME(m, n, a)                               \
    MR_PASTE2(mercury__, MR_NONSTD_TYPE_CTOR_INFO_NAME(m, n, a))

  #define MR_TYPE_CTOR_INFO_FUNC_NAME(m, n, a, f)                       \
    MR_PASTE2(mercury__,                                                \
    MR_PASTE2(m,                                                        \
    MR_PASTE2(__,                                                       \
    MR_PASTE2(f,                                                        \
    MR_PASTE2(__,                                                       \
    MR_PASTE2(n,                                                        \
    MR_PASTE2(_,                                                        \
    MR_PASTE2(a, _0))))))))

  #define MR_TYPE_UNIFY_FUNC(m, n, a)                                   \
    MR_TYPE_CTOR_INFO_FUNC_NAME(m, n, a, do_unify)

  #define MR_TYPE_COMPARE_FUNC(m, n, a)                                 \
    MR_TYPE_CTOR_INFO_FUNC_NAME(m, n, a, do_compare)

  #define MR_SPECIAL_FUNC_TYPE(NAME, ARITY)                             \
    MR_PASTE2(MR_, MR_PASTE2(NAME, MR_PASTE2(Func_, ARITY)))

  #define MR_DEFINE_TYPE_CTOR_INFO_DECLARE_ADDRS(u, c, a)               \
    extern MR_PASTE2(MR_UnifyFunc_, a) u;                               \
    extern MR_PASTE2(MR_CompareFunc_, a) c;

  #define MR_DEFINE_TYPE_CTOR_INFO_CODE(p)                              \
        (MR_Box) p

  #define MR_DEFINE_TYPE_CTOR_INFO_STRING(s)                            \
        MR_STRINGIFY(s)

#else // ! MR_HIGHLEVEL_CODE

  #define MR_DEFINE_TYPE_CTOR_INFO_TYPE                                 \
    MR_STATIC_CODE_CONST struct MR_TypeCtorInfo_Struct

  #define MR_NONSTD_TYPE_CTOR_INFO_NAME(m, n, a)                        \
    MR_PASTE2(mercury_data_,                                            \
    MR_PASTE2(m,                                                        \
    MR_PASTE2(__type_ctor_info_,                                        \
    MR_PASTE2(n,                                                        \
    MR_PASTE2(_, a)))))

  #define MR_TYPE_CTOR_INFO_NAME(m, n, a)                               \
    MR_NONSTD_TYPE_CTOR_INFO_NAME(m, n, a)

  #define MR_TYPE_UNIFY_FUNC(m, n, a)                                   \
    MR_PASTE7(mercury____Unify___, m, __, n, _, a, _0)

  #define MR_TYPE_COMPARE_FUNC(m, n, a)                                 \
    MR_PASTE7(mercury____Compare___, m, __, n, _, a, _0)

  #define MR_DEFINE_TYPE_CTOR_INFO_DECLARE_ADDRS(u, c, a)               \
    MR_declare_entry(u);                                                \
    MR_declare_entry(c);

  #define MR_DEFINE_TYPE_CTOR_INFO_CODE(p)                              \
        MR_MAYBE_STATIC_CODE(MR_ENTRY(p))

  #define MR_DEFINE_TYPE_CTOR_INFO_STRING(s)                            \
        MR_string_const(MR_STRINGIFY(s), sizeof(MR_STRINGIFY(s))-1)

#endif // MR_HIGHLEVEL_CODE

#define MR_DEFINE_TYPE_CTOR_INFO_BODY_FLAG(m, n, a, cr, u, c, f, fns)   \
    {                                                                   \
        a,                                                              \
        MR_RTTI_VERSION__ARG_WIDTHS,                                    \
        -1,                                                             \
        MR_PASTE2(MR_TYPECTOR_REP_, cr),                                \
        MR_DEFINE_TYPE_CTOR_INFO_CODE(u),                               \
        MR_DEFINE_TYPE_CTOR_INFO_CODE(c),                               \
        MR_DEFINE_TYPE_CTOR_INFO_STRING(m),                             \
        MR_DEFINE_TYPE_CTOR_INFO_STRING(n),                             \
        { 0 },                                                          \
        { 0 },                                                          \
        -1,                                                             \
        f,                                                              \
        fns                                                             \
    }

#define MR_DEFINE_TYPE_CTOR_INFO_FULL_FLAG(m, n, a, cr, u, c, f, fns)   \
    MR_DEFINE_TYPE_CTOR_INFO_DECLARE_ADDRS(u, c, a)                     \
    MR_DEFINE_TYPE_CTOR_INFO_TYPE                                       \
    MR_TYPE_CTOR_INFO_NAME(m, n, a) =                                   \
    MR_DEFINE_TYPE_CTOR_INFO_BODY_FLAG(m, n, a, cr, u, c, f, fns)

#define MR_DEFINE_TYPE_CTOR_INFO_FLAG(m, n, a, cr, f)                   \
    MR_DEFINE_TYPE_CTOR_INFO_FULL_FLAG(m, n, a, cr,                     \
        MR_TYPE_UNIFY_FUNC(m, n, a), MR_TYPE_COMPARE_FUNC(m, n, a), f, NULL)

#define MR_DEFINE_TYPE_CTOR_INFO_FLAG_FUNCTORS(m, n, a, cr, f, fns)     \
    MR_DEFINE_TYPE_CTOR_INFO_FULL_FLAG(m, n, a, cr,                     \
        MR_TYPE_UNIFY_FUNC(m, n, a), MR_TYPE_COMPARE_FUNC(m, n, a), f, fns)

#define MR_DEFAULT_TYPE_CTOR_INFO_FLAG  0

#define MR_DEFINE_TYPE_CTOR_INFO_FULL(m, n, a, cr, u, c)                \
    MR_DEFINE_TYPE_CTOR_INFO_FULL_FLAG(m, n, a, cr, u, c,               \
        MR_DEFAULT_TYPE_CTOR_INFO_FLAG)

#define MR_DEFINE_TYPE_CTOR_INFO(m, n, a, cr)                           \
    MR_DEFINE_TYPE_CTOR_INFO_FLAG(m, n, a, cr,                          \
        MR_DEFAULT_TYPE_CTOR_INFO_FLAG)

// The macros below are used to reduce the sizes of compiler-generated
// .c files, especially in debug grades.

#define MR_CTOR_ADDR(m, n, a)                                           \
    (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(m, n, a)
#define MR_CTOR0_ADDR(m, n)                                             \
    (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(m, n, 0)
#define MR_CTOR1_ADDR(m, n)                                             \
    (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(m, n, 1)

// `bool' is often defined in other C code and can cause conflicts
// when linking, hence we expand these definitions out fully here.

#ifdef MR_HIGHLEVEL_CODE

  #define MR_INT_CTOR_ADDR                                                \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_int_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0)
  #define MR_UINT_CTOR_ADDR                                               \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_uint_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint, 0)
  #define MR_INT8_CTOR_ADDR                                               \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_int8_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int8, 0)
  #define MR_UINT8_CTOR_ADDR                                              \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_uint8_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint8, 0)
  #define MR_INT16_CTOR_ADDR                                              \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_int16_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int16, 0)
  #define MR_UINT16_CTOR_ADDR                                             \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_uint16_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint16, 0)
  #define MR_INT32_CTOR_ADDR                                              \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_int32_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int32, 0)
  #define MR_UINT32_CTOR_ADDR                                             \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_uint32_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint32, 0)
  #define MR_INT64_CTOR_ADDR                                              \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_int64_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int64, 0)
  #define MR_UINT64_CTOR_ADDR                                             \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_uint64_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint64, 0)
  #define MR_FLOAT_CTOR_ADDR                                              \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_float_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, float, 0)
  #define MR_CHAR_CTOR_ADDR                                               \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_character_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0)
  #define MR_STRING_CTOR_ADDR                                             \
      (MR_Word *) &mercury__builtin__builtin__type_ctor_info_string_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0)
  #define MR_IO_CTOR_ADDR                                                 \
      (MR_Word *) &mercury__io__io__type_ctor_info_state_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(io, state, 0)
  #define MR_BOOL_CTOR_ADDR                                               \
      (MR_Word *) &mercury__bool__bool__type_ctor_info_bool_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(bool, bool, 0)
  #define MR_LIST_CTOR_ADDR                                               \
      (MR_Word *) &mercury__list__list__type_ctor_info_list_1
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(list, list, 1)
  #define MR_TYPE_INFO_CTOR_ADDR                                          \
      (MR_Word *) &mercury__private_builtin__private_builtin__type_ctor_info_type_info_1
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 1)

#else // ! MR_HIGHLEVEL_CODE

  #define MR_INT_CTOR_ADDR                                                \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_int_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0)
  #define MR_UINT_CTOR_ADDR                                               \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_uint_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint, 0)
  #define MR_INT8_CTOR_ADDR                                               \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_int8_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int8, 0)
  #define MR_UINT8_CTOR_ADDR                                              \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_uint8_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint8, 0)
  #define MR_INT16_CTOR_ADDR                                              \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_int16_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int16, 0)
  #define MR_UINT16_CTOR_ADDR                                             \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_uint16_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint16, 0)
  #define MR_INT32_CTOR_ADDR                                              \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_int32_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int32, 0)
  #define MR_UINT32_CTOR_ADDR                                             \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_uint32_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint32, 0)
  #define MR_INT64_CTOR_ADDR                                              \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_int64_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, int64, 0)
  #define MR_UINT64_CTOR_ADDR                                             \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_uint64_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, uint64, 0)
  #define MR_FLOAT_CTOR_ADDR                                              \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_float_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, float, 0)
  #define MR_CHAR_CTOR_ADDR                                               \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_character_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0)
  #define MR_STRING_CTOR_ADDR                                             \
      (MR_Word *) &mercury_data_builtin__type_ctor_info_string_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0)
  #define MR_IO_CTOR_ADDR                                                 \
      (MR_Word *) &mercury_data_io__type_ctor_info_state_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(io, state, 0)
  #define MR_BOOL_CTOR_ADDR                                               \
      (MR_Word *) &mercury_data_bool__type_ctor_info_bool_0
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(bool, bool, 0)
  #define MR_LIST_CTOR_ADDR                                               \
      (MR_Word *) &mercury_data_list__type_ctor_info_list_1
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(list, list, 1)
  #define MR_TYPE_INFO_CTOR_ADDR                                          \
      (MR_Word *) &mercury_data_private_builtin__type_ctor_info_type_info_1
      // (MR_Word *) &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 1)

#endif

////////////////////////////////////////////////////////////////////////////

// Code for dealing with the static code addresses stored in
// type_ctor_infos.

// Definitions for initialization of type_ctor_infos. If
// MR_STATIC_CODE_ADDRESSES are not available, we need to initialize
// the special predicates in the type_ctor_infos.

// Macros are provided here to initialize type_ctor_infos, both for
// builtin types (such as in runtime/mercury_builtin_types.c) and user
// defined C types (like library/array.m). Also, the automatically
// generated code uses these initializers.
//
// MR_INIT_TYPE_CTOR_INFO(
//  mercury_data_group__type_ctor_info_group_1, group__group_1_0);
//
// MR_INIT_TYPE_CTOR_INFO_WITH_PRED(
//  mercury_date__type_ctor_info_void_0, mercury__unused_0_0);
//
// This will initialize a type_ctor_info with a single code address.

#ifndef MR_STATIC_CODE_ADDRESSES

  #define MR_MAYBE_STATIC_CODE(X)   NULL

  #define MR_STATIC_CODE_CONST

  #define   MR_INIT_TYPE_CTOR_INFO(B, T)                                \
  do {                                                                  \
    (B).MR_type_ctor_unify_pred =                                       \
        MR_ENTRY(mercury____##Unify##___##T);                           \
    (B).MR_type_ctor_compare_pred =                                     \
        MR_ENTRY(mercury____##Compare##___##T);                         \
  } while (0)

  #define   MR_INIT_TYPE_CTOR_INFO_MNA(m, n, a)                         \
  do {                                                                  \
    MR_TYPE_CTOR_INFO_NAME(m, n, a).MR_type_ctor_unify_pred =           \
        MR_ENTRY(MR_TYPE_UNIFY_FUNC(m, n, a));                          \
    MR_TYPE_CTOR_INFO_NAME(m, n, a).MR_type_ctor_compare_pred =         \
        MR_ENTRY(MR_TYPE_COMPARE_FUNC(m, n, a));                        \
  } while (0)

  #define   MR_INIT_TYPE_CTOR_INFO_MNA_WITH_PRED(m, n, a, p)            \
  do {                                                                  \
    MR_TYPE_CTOR_INFO_NAME(m, n, a).MR_type_ctor_unify_pred =           \
        MR_ENTRY(p);                                                    \
    MR_TYPE_CTOR_INFO_NAME(m, n, a).MR_type_ctor_compare_pred =         \
        MR_ENTRY(p);                                                    \
  } while (0)

#else   // MR_STATIC_CODE_ADDRESSES

  #define MR_MAYBE_STATIC_CODE(X)   (X)

  #define MR_STATIC_CODE_CONST const

  #define MR_INIT_TYPE_CTOR_INFO(B, T)                                  \
    do { } while (0)

  #define MR_INIT_TYPE_CTOR_INFO_MNA(m, n, a)                           \
    do { } while (0)

  #define MR_INIT_TYPE_CTOR_INFO_MNA_WITH_PRED(m, n, a, p)              \
    do { } while (0)

#endif // MR_STATIC_CODE_ADDRESSES

#define MR_REGISTER_TYPE_CTOR_INFO(m, n, a)                             \
    MR_register_type_ctor_info(&MR_TYPE_CTOR_INFO_NAME(m, n, a))

////////////////////////////////////////////////////////////////////////////

// Declaration for structs.

#define MR_DECLARE_TYPE_CTOR_INFO_STRUCT(T)                             \
    extern MR_STATIC_CODE_CONST struct MR_TypeCtorInfo_Struct T

////////////////////////////////////////////////////////////////////////////

#define MR_TYPE_CTOR_INFO_IS_HO_PRED(T)                                 \
        (MR_type_ctor_rep(T) == MR_TYPECTOR_REP_PRED)
#define MR_TYPE_CTOR_INFO_IS_HO_FUNC(T)                                 \
        (MR_type_ctor_rep(T) == MR_TYPECTOR_REP_FUNC)
#define MR_TYPE_CTOR_INFO_IS_HO(T)                                      \
        (MR_TYPE_CTOR_INFO_IS_HO_FUNC(T) || MR_TYPE_CTOR_INFO_IS_HO_PRED(T))
#define MR_TYPE_CTOR_INFO_IS_TUPLE(T)                                   \
        (MR_type_ctor_rep(T) == MR_TYPECTOR_REP_TUPLE)

////////////////////////////////////////////////////////////////////////////

// Compare two pseudo_type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_COMPARE_GREATER, MR_COMPARE_EQUAL, or MR_COMPARE_LESS,
// depending on whether pti1 is greater than, equal to, or less than pti2.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  int     MR_compare_pseudo_type_info(MR_PseudoTypeInfo pti1,
                    MR_PseudoTypeInfo pti2);

// Unify two pseudo_type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_TRUE if pti1 represents the same type as pti2, and MR_FALSE
// otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_unify_pseudo_type_info(MR_PseudoTypeInfo pti1,
                    MR_PseudoTypeInfo pti2);

// Unify a pseudo_type_info structure with the type_info for float.
// Return MR_TRUE if pti represents the type float, and MR_FALSE
// otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_unify_pseudo_type_info_float(MR_PseudoTypeInfo pti);

// Compare two pseudo_type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_COMPARE_GREATER, MR_COMPARE_EQUAL, or MR_COMPARE_LESS,
// depending on whether pti1 is greater than, equal to, or less than pti2.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  int     MR_compare_pseudo_type_info(MR_PseudoTypeInfo ti1,
                    MR_PseudoTypeInfo ti2);

// Unify two pseudo_type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_TRUE if ti1 represents the same type as ti2, and MR_FALSE
// otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_unify_pseudo_type_info(MR_PseudoTypeInfo ti1,
                    MR_PseudoTypeInfo ti2);

// Compare two type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_COMPARE_GREATER, MR_COMPARE_EQUAL, or MR_COMPARE_LESS,
// depending on whether ti1 is greater than, equal to, or less than ti2.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  int     MR_compare_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2);

// Unify two type_info structures, using an ordering based on the
// module names, type names and arities of the types inside the type_info.
// Return MR_TRUE if ti1 represents the same type as ti2, and MR_FALSE
// otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_unify_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2);

// Compare two type_ctor_info structures, using an ordering based on the
// module names, type names and arities of the types represented by tci1/tci2.
// Return MR_COMPARE_GREATER, MR_COMPARE_EQUAL, or MR_COMPARE_LESS,
// depending on whether tci1 is greater than, equal to, or less than tci2.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  int     MR_compare_type_ctor_info(MR_TypeCtorInfo tci1,
                    MR_TypeCtorInfo tci2);

// Unify two type_ctor_info structures, using an ordering based on the
// module names, type names and arities of the types represented by tci1/tci2.
// Return MR_TRUE if tci1 represents the same type constructor as tci2, and
// MR_FALSE otherwise.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_unify_type_ctor_info(MR_TypeCtorInfo tci1,
                    MR_TypeCtorInfo tci2);

// MR_collapse_equivalences expands out all the top-level equivalences in
// the argument type_info. It guarantees that the returned type_info's
// type_ctor_info will not have a MR_TYPE_CTOR_REP_EQUIV* representation.
// However, since it only works on the top level type constructor,
// this is not guaranteed for the type_infos of the type constructor's
// arguments.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_TypeInfo MR_collapse_equivalences(MR_TypeInfo type_info);

// MR_collapse_equivalences_pseudo expands out all the top-level equivalences
// in the argument pseudo_type_info. It guarantees that the returned
// pseudo_type_info's type_ctor_info, if any, will not have a
// MR_TYPE_CTOR_REP_EQUIV* representation.
// However, since it only works on the top level type constructor,
// this is not guaranteed for the type_infos of the type constructor's
// arguments.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_PseudoTypeInfo MR_collapse_equivalences_pseudo(
                    MR_PseudoTypeInfo pseudo_type_info);

// MR_collapse_equivalences_pseudo expands out all the top-level equivalences
// in the argument pseudo_type_info. It guarantees that the returned
// pseudo_type_info's type_ctor_info, if any, will not have a
// MR_TYPE_CTOR_REP_EQUIV* representation.
// However, since it only works on the top level type constructor,
// this is not guaranteed for the type_infos of the type constructor's
// arguments.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_PseudoTypeInfo MR_collapse_equivalences_pseudo(
                    MR_PseudoTypeInfo pseudo_type_info);

// MR_create_type and MR_make_type_info both turn a pseudo typeinfo into
// a typeinfo, looking up the typeinfos associated with the type variables
// in the pseudointo typeinfo in the supplied vector of type parameters.
//
// The two functions differ in how they allocate memory. MR_create_type_info
// allocates memory for a new type_info on the Mercury heap. Since this
// may modify MR_hp, you need to wrap MR_save_transient_hp() and
// MR_restore_transient_hp() around calls to MR_create_type_info.
// MR_make_type_info allocates memory using MR_GC_malloc, and inserts
// the address of the cells allocated into the list of allocations
// represented by its last argument; it is the caller's responsibility
// to call MR_deallocate() on the list after they have finished using
// the returned typeinfo.
//
// MR_create_type_info and MR_make_type_info both assume that all type
// variables inside the given pseudo typeinfo are universally quantified.
// Their maybe_existq variants do not make this assumption; they also work
// if the pseudo typeinfo contains existentially quantified arguments.
// This can happen only when the pseudo typeinfo describes (part of) the type
// of an argument of a function symbol from a MR_TYPE_CTOR_REP_DU* type.
// These functions also take two extra arguments: the address of the cell,
// which (directly or indirectly) contains the typeinfos of the existentially
// quantified type variables, and the descriptor of the function symbol,
// which describes how those typeinfos can be found in the cell. The cell
// address is supposed to point PAST the remote secondary tag, if any;
// it should point to the first argument, whether it is a user visible argument
// or a typeinfo/typeclass_info inserted into the cell by the compiler.
//
// All these functions guarantee that if the pseudo typeinfo argument refers
// to a type constructor with no arguments, then they return a one-cell
// typeinfo, and do not require any memory allocation.
//
// These functions should only be called if the pseudo typeinfo may have
// some type variables in it. Otherwise, the pseudo typeinfo should be
// cast to a typeinfo directly, using the macro MR_pseudo_type_info_is_ground.

extern  MR_TypeInfo MR_create_type_info(
                        const MR_TypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info);
extern  MR_TypeInfo MR_create_type_info_maybe_existq(
                        const MR_TypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info,
                        const MR_Word *data_value,
                        const MR_DuFunctorDesc *functor_descriptor);

extern  MR_PseudoTypeInfo
                    MR_create_pseudo_type_info(
                        const MR_PseudoTypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info);
extern  MR_PseudoTypeInfo
                    MR_create_pseudo_type_info_maybe_existq(
                        const MR_PseudoTypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info,
                        const MR_Word *data_value,
                        const MR_DuFunctorDesc *functor_descriptor);

struct MR_MemoryCellNode {
    void                        *data;
    struct MR_MemoryCellNode    *next;
};

typedef struct MR_MemoryCellNode *MR_MemoryList;

extern  MR_TypeInfo MR_make_type_info(
                        const MR_TypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info,
                        MR_MemoryList *allocated);
extern  MR_TypeInfo MR_make_type_info_maybe_existq(
                        const MR_TypeInfoParams type_info_params,
                        const MR_PseudoTypeInfo pseudo_type_info,
                        const MR_Word *data_value,
                        const MR_DuFunctorDesc *functor_descriptor,
                        MR_MemoryList *allocated);
extern  void        MR_deallocate(MR_MemoryList allocated_memory_cells);

// MR_type_params_vector_to_list:
//
// Copy `arity' type_infos from the `arg_type_infos' vector, which starts
// at index 1, onto the Mercury heap in a list.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_Word     MR_type_params_vector_to_list(int arity,
                        MR_TypeInfoParams type_params);

// MR_pseudo_type_params_vector_to_list:
//
// Copy `arity' pseudo_type_infos from the `arg_type_infos' vector,
// which starts at index 1, onto the Mercury heap in a list.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_Word     MR_pseudo_type_params_vector_to_list(int arity,
                        MR_PseudoTypeInfoParams type_params);

// ML_arg_name_vector_to_list:
//
// Copy `arity' argument names from the `arg_names' vector, which starts
// at index 0, onto the Mercury heap in a list.
//
// If `arg_names' is NULL (meaning that none of the arguments were named),
// the output list will contain `arity' NULL entries.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_Word     MR_arg_name_vector_to_list(int arity,
                        const MR_ConstString *arg_names);

// MR_pseudo_type_info_vector_to_pseudo_type_info_list:
//
// Take `arity' pseudo_type_infos from the `arg_pseudo_type_infos' vector,
// which starts at index 0, expand them, and copy them onto the heap
// in a list. The elements of the resulting list will be pseudo_type_infos
// which shouldn't contain universally quantified type variables, but may
// contain existentially quantified type variables.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_Word     MR_pseudo_type_info_vector_to_pseudo_type_info_list(
                        int arity, MR_TypeInfoParams type_params,
                        const MR_PseudoTypeInfo *arg_pseudo_type_infos);

// MR_cell_size_for_args:
//
// Return the number of words required to hold the visible arguments of a
// constructor. That is, it does not count extra arguments, an optional
// secondary tag, or the additional slot for term size profiling.

extern  int         MR_cell_size_for_args(int arity,
                        const MR_DuArgLocn *arg_locns);

// MR_print_type:
//
// Print a representation of the type represented by the given typeinfo to the
// given file.

extern  void        MR_print_type(FILE *fp, MR_TypeInfo type_info);

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_TYPE_INFO_H
