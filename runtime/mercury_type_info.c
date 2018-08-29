// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2006, 2012 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_type_info.c -
//  Definitions for dealing with type_infos needed by the Mercury
//  runtime system.

#include "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_misc.h"           // for MR_fatal_error()
#include "mercury_heap.h"           // for MR_incr_saved_hp()
#include "mercury_builtin_types.h"  // for void/0's type_ctor_info

MR_ConstString  MR_ctor_rep_name[] = {
    MR_CTOR_REP_NAMES
};

////////////////////////////////////////////////////////////////////////////

static MR_PseudoTypeInfo    MR_get_arg_pseudo_type_info(
                                const MR_PseudoTypeInfoParams params,
                                const MR_PseudoTypeInfo pseudo_type_info,
                                const MR_Word *data_value,
                                const MR_DuFunctorDesc *functor_desc);

static MR_TypeInfo          MR_get_arg_type_info(
                                const MR_TypeInfoParams params,
                                const MR_PseudoTypeInfo pseudo_type_info,
                                const MR_Word *data_value,
                                const MR_DuFunctorDesc *functor_desc);

////////////////////////////////////////////////////////////////////////////

#define usual_func              MR_make_type_info
#define exist_func              MR_make_type_info_maybe_existq
#define exist_func_string       "MR_make_type_info_maybe_existq"
#define return_type             MR_TypeInfo
#define params_type             MR_TypeInfoParams
#define create_pseudo           MR_FALSE
#define MAYBE_DECLARE_ALLOC_ARG , MR_MemoryList *allocated
#define MAYBE_PASS_ALLOC_ARG    , allocated
#define ALLOCATE_WORDS(target, size)                                \
    do {                                                            \
        MR_Word         *target_word_ptr;                           \
        MR_MemoryList   node;                                       \
                                                                    \
        target_word_ptr = MR_GC_NEW_ARRAY_ATTRIB(MR_Word, (size),   \
            MR_ALLOC_SITE_TYPE_INFO);                               \
        (target) = (MR_Word) target_word_ptr;                       \
        node = MR_GC_malloc_attrib(sizeof(*node),                   \
            MR_ALLOC_SITE_TYPE_INFO);                               \
        node->data = target_word_ptr;                               \
        node->next = *allocated;                                    \
        *allocated = node;                                          \
    } while (0)

#include "mercury_make_type_info_body.h"
#undef  usual_func
#undef  exist_func
#undef  exist_func_string
#undef  return_type
#undef  params_type
#undef  create_pseudo
#undef  MAYBE_DECLARE_ALLOC_ARG
#undef  MAYBE_PASS_ALLOC_ARG
#undef  ALLOCATE_WORDS

#define usual_func              MR_create_type_info
#define exist_func              MR_create_type_info_maybe_existq
#define exist_func_string       "MR_create_type_info_maybe_existq"
#define return_type             MR_TypeInfo
#define params_type             MR_TypeInfoParams
#define create_pseudo           MR_FALSE
#define MAYBE_DECLARE_ALLOC_ARG
#define MAYBE_PASS_ALLOC_ARG
#ifdef MR_NATIVE_GC
  #define ALLOCATE_WORDS(target, size)                              \
    do {                                                            \
        /*                                                          \
        ** Reserve one extra word for GC forwarding pointer         \
        ** (see comments in compiler/mlds_to_c.m for details).      \
        */                                                          \
        MR_offset_incr_saved_hp((target), 0, 1, NULL, NULL);        \
        MR_offset_incr_saved_hp((target), 0, (size), NULL, NULL);   \
    } while (0)
#else // !MR_NATIVE_GC
  #define ALLOCATE_WORDS(target, size)                              \
    do {                                                            \
        MR_offset_incr_saved_hp((target), 0, (size), NULL, NULL);   \
    } while (0)
#endif // !MR_NATIVE_GC

#include "mercury_make_type_info_body.h"
#undef  usual_func
#undef  exist_func
#undef  exist_func_string
#undef  return_type
#undef  params_type
#undef  create_pseudo
#undef  MAYBE_DECLARE_ALLOC_ARG
#undef  MAYBE_PASS_ALLOC_ARG
#undef  ALLOCATE_WORDS

#define usual_func              MR_create_pseudo_type_info
#define exist_func              MR_create_pseudo_type_info_maybe_existq
#define exist_func_string       "MR_create_pseudo_type_info_maybe_existq"
#define return_type             MR_PseudoTypeInfo
#define params_type             MR_PseudoTypeInfoParams
#define create_pseudo           MR_TRUE
#define MAYBE_DECLARE_ALLOC_ARG
#define MAYBE_PASS_ALLOC_ARG
#ifdef MR_NATIVE_GC
  #define ALLOCATE_WORDS(target, size)                              \
    do {                                                            \
        /*                                                          \
        ** Reserve one extra word for GC forwarding pointer         \
        ** (see comments in compiler/mlds_to_c.m for details).      \
        */                                                          \
        MR_offset_incr_saved_hp((target), 0, 1, NULL, NULL);        \
        MR_offset_incr_saved_hp((target), 0, (size), NULL, NULL);   \
    } while (0)
#else // !MR_NATIVE_GC
  #define ALLOCATE_WORDS(target, size)                              \
    do {                                                            \
        MR_offset_incr_saved_hp((target), 0, (size), NULL, NULL);   \
    } while (0)
#endif // !MR_NATIVE_GC

#include "mercury_make_type_info_body.h"
#undef  usual_func
#undef  exist_func
#undef  exist_func_string
#undef  return_type
#undef  params_type
#undef  create_pseudo
#undef  MAYBE_DECLARE_ALLOC_ARG
#undef  MAYBE_PASS_ALLOC_ARG
#undef  ALLOCATE_WORDS

static MR_PseudoTypeInfo
MR_get_arg_pseudo_type_info(const MR_PseudoTypeInfoParams params,
    const MR_PseudoTypeInfo pseudo_type_info, const MR_Word *data_value,
    const MR_DuFunctorDesc *functor_desc)
{
    MR_Unsigned             arg_num;
    const MR_DuExistInfo    *exist_info;
    MR_DuExistLocn          exist_locn;
    int                     exist_varnum;
    int                     slot;
    int                     offset;

    // Most changes here should also be reflected in MR_get_arg_type_info
    // below.

    arg_num = (MR_Unsigned) pseudo_type_info;

    if (MR_TYPE_VARIABLE_IS_UNIV_QUANT(pseudo_type_info)) {
        // This is a universally quantified type variable.
        return params[arg_num];
    }

    // This is an existentially quantified type variable.
    exist_info = functor_desc->MR_du_functor_exist_info;
    if (exist_info == NULL) {
        MR_fatal_error("MR_get_arg_pseudo_type_info: no exist_info");
    }

    exist_varnum = arg_num - MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1;
    exist_locn = exist_info->MR_exist_typeinfo_locns[exist_varnum];
    slot = exist_locn.MR_exist_arg_num;
    offset = exist_locn.MR_exist_offset_in_tci;
    if (offset < 0) {
        return (MR_PseudoTypeInfo) data_value[slot];
    } else {
        return (MR_PseudoTypeInfo) MR_typeclass_info_param_type_info(
            data_value[slot], offset);
    }
}

static MR_TypeInfo
MR_get_arg_type_info(const MR_TypeInfoParams params,
    const MR_PseudoTypeInfo pseudo_type_info, const MR_Word *data_value,
    const MR_DuFunctorDesc *functor_desc)
{
    MR_Unsigned             arg_num;
    const MR_DuExistInfo    *exist_info;
    const MR_DuExistLocn    *exist_locn;
    int                     exist_varnum;
    int                     slot;
    int                     offset;

    // Most changes here should also be reflected in
    // MR_get_arg_pseudo_type_info above.

    arg_num = (MR_Unsigned) pseudo_type_info;

    if (MR_TYPE_VARIABLE_IS_UNIV_QUANT(pseudo_type_info)) {
        // This is a universally quantified type variable.
        return params[arg_num];
    }

    // This is an existentially quantified type variable.
    exist_info = functor_desc->MR_du_functor_exist_info;
    if (exist_info == NULL) {
        MR_fatal_error("MR_get_arg_type_info: no exist_info");
    }

    exist_varnum = arg_num - MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1;
    exist_locn = &exist_info->MR_exist_typeinfo_locns[exist_varnum];
    slot = exist_locn->MR_exist_arg_num;
    offset = exist_locn->MR_exist_offset_in_tci;
    if (offset < 0) {
        return (MR_TypeInfo) data_value[slot];
    } else {
        return (MR_TypeInfo) MR_typeclass_info_param_type_info(
            data_value[slot], offset);
    }
}

int
MR_compare_pseudo_type_info(MR_PseudoTypeInfo pti1, MR_PseudoTypeInfo pti2)
{
    MR_TypeCtorInfo     tci1;
    MR_TypeCtorInfo     tci2;
    MR_PseudoTypeInfo   *arg_vector_1;
    MR_PseudoTypeInfo   *arg_vector_2;
    int                 num_arg_types_1;
    int                 num_arg_types_2;
    int                 i;
    int                 comp;

    // Try to optimize a common case:
    // If type_info addresses are equal, they must represent the same type.

    if (pti1 == pti2) {
        return MR_COMPARE_EQUAL;
    }

    // Otherwise, we need to expand equivalence types, if any.

    pti1 = MR_collapse_equivalences_pseudo(pti1);
    pti2 = MR_collapse_equivalences_pseudo(pti2);

    // Perhaps they are equal now...

    if (pti1 == pti2) {
        return MR_COMPARE_EQUAL;
    }

    // Handle the comparison if either pseudo_type_info is a variable.
    // Any non-variable is greater than a variable.

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti1) &&
        MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti2))
    {
        if ((MR_Integer) pti1 < (MR_Integer) pti2) {
            return MR_COMPARE_LESS;
        } else if ((MR_Integer) pti1 > (MR_Integer) pti2) {
            return MR_COMPARE_GREATER;
        } else {
            return MR_COMPARE_EQUAL;
        }
    }

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti1)) {
        return MR_COMPARE_LESS;
    }

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti2)) {
        return MR_COMPARE_GREATER;
    }

    // Otherwise find the type_ctor_infos, and compare those.

    tci1 = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti1);
    tci2 = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti2);

    comp = MR_compare_type_ctor_info(tci1, tci2);
    if (comp != MR_COMPARE_EQUAL) {
        return comp;
    }

    // If the type_ctor_infos are equal, we don't need to compare
    // the arity of the types - they must be the same - unless they are
    // higher-order (which are all mapped to pred/0 or func/0) or tuples
    // (which are all mapped to tuple/0), in which cases we must compare
    // the arities before we can check the argument types.

    if (MR_type_ctor_has_variable_arity(tci1)) {
        num_arg_types_1 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pti1);
        num_arg_types_2 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pti2);

        // Check the arities.
        if (num_arg_types_1 < num_arg_types_2) {
            return MR_COMPARE_LESS;
        } else if (num_arg_types_1 > num_arg_types_2) {
            return MR_COMPARE_GREATER;
        }

        arg_vector_1 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pti1);
        arg_vector_2 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pti2);
    } else {
        num_arg_types_1 = tci1->MR_type_ctor_arity;
        arg_vector_1 = MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pti1);
        arg_vector_2 = MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pti2);
    }

    // Compare the argument types.
    for (i = 1; i <= num_arg_types_1; i++) {
        comp = MR_compare_pseudo_type_info(arg_vector_1[i], arg_vector_2[i]);
        if (comp != MR_COMPARE_EQUAL) {
            return comp;
        }
    }

    return MR_COMPARE_EQUAL;
}

MR_bool
MR_unify_pseudo_type_info(MR_PseudoTypeInfo pti1, MR_PseudoTypeInfo pti2)
{
    MR_TypeCtorInfo     tci1;
    MR_TypeCtorInfo     tci2;
    MR_PseudoTypeInfo   *arg_vector_1;
    MR_PseudoTypeInfo   *arg_vector_2;
    int                 num_arg_types_1;
    int                 num_arg_types_2;
    int                 i;

    // Try to optimize a common case:
    // If type_info addresses are equal, they must represent the same type.

    if (pti1 == pti2) {
        return MR_TRUE;
    }

    // Otherwise, we need to expand equivalence types, if any.

    pti1 = MR_collapse_equivalences_pseudo(pti1);
    pti2 = MR_collapse_equivalences_pseudo(pti2);

    // Perhaps they are equal now...

    if (pti1 == pti2) {
        return MR_TRUE;
    }

    // Handle the comparison if either pseudo_type_info is a variable.
    // Any non-variable is greater than a variable.

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti1) &&
        MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti2))
    {
        if ((MR_Integer) pti1 != (MR_Integer) pti2) {
            return MR_FALSE;
        } else {
            return MR_TRUE;
        }
    }

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti1)) {
        return MR_FALSE;
    }

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti2)) {
        return MR_FALSE;
    }

    // Otherwise find the type_ctor_infos, and compare those.

    tci1 = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti1);
    tci2 = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti2);

    if (! MR_unify_type_ctor_info(tci1, tci2)) {
        return MR_FALSE;
    }

    // If the type_ctor_infos are equal, we don't need to compare
    // the arity of the types - they must be the same - unless they are
    // higher-order (which are all mapped to pred/0 or func/0) or tuples
    // (which are all mapped to tuple/0), in which cases we must compare
    // the arities before we can check the argument types.

    if (MR_type_ctor_has_variable_arity(tci1)) {
        num_arg_types_1 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pti1);
        num_arg_types_2 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pti2);

        // Check the arities.
        if (num_arg_types_1 != num_arg_types_2) {
            return MR_FALSE;
        }

        arg_vector_1 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pti1);
        arg_vector_2 = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pti2);
    } else {
        num_arg_types_1 = tci1->MR_type_ctor_arity;
        arg_vector_1 = MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pti1);
        arg_vector_2 = MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pti2);
    }

    // Compare the argument types.
    for (i = 1; i <= num_arg_types_1; i++) {
        if (! MR_unify_pseudo_type_info(arg_vector_1[i], arg_vector_2[i])) {
            return MR_FALSE;
        }
    }

    return MR_TRUE;
}

MR_bool
MR_unify_pseudo_type_info_float(MR_PseudoTypeInfo pti)
{
    MR_TypeCtorInfo tci1;
    MR_TypeCtorInfo tci2;

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti)) {
        return MR_FALSE;
    }

    tci1 = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti);
    tci2 = (MR_TypeCtorInfo) MR_FLOAT_CTOR_ADDR;
    return MR_unify_type_ctor_info(tci1, tci2);
}

int
MR_compare_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2)
{
    MR_TypeCtorInfo tci1;
    MR_TypeCtorInfo tci2;
    MR_TypeInfo     *arg_vector_1;
    MR_TypeInfo     *arg_vector_2;
    int             num_arg_types_1;
    int             num_arg_types_2;
    int             i;
    int             comp;

    // Try to optimize a common case:
    // If type_info addresses are equal, they must represent the same type.

    if (ti1 == ti2) {
        return MR_COMPARE_EQUAL;
    }

    // Otherwise, we need to expand equivalence types, if any.

    ti1 = MR_collapse_equivalences(ti1);
    ti2 = MR_collapse_equivalences(ti2);

    // Perhaps they are equal now...

    if (ti1 == ti2) {
        return MR_COMPARE_EQUAL;
    }

    // Otherwise find the type_ctor_infos, and compare those.

    tci1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti1);
    tci2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti2);

    comp = MR_compare_type_ctor_info(tci1, tci2);
    if (comp != MR_COMPARE_EQUAL) {
        return comp;
    }

    // If the type_ctor_infos are equal, we don't need to compare
    // the arity of the types - they must be the same - unless they are
    // higher-order (which are all mapped to pred/0 or func/0) or tuples
    // (which are all mapped to tuple/0), in which cases we must compare
    // the arities before we can check the argument types.

    if (MR_type_ctor_has_variable_arity(tci1)) {
        num_arg_types_1 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti1);
        num_arg_types_2 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti2);

        // Check the arities.
        if (num_arg_types_1 < num_arg_types_2) {
            return MR_COMPARE_LESS;
        } else if (num_arg_types_1 > num_arg_types_2) {
            return MR_COMPARE_GREATER;
        }

        arg_vector_1 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti1);
        arg_vector_2 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti2);
    } else {
        num_arg_types_1 = tci1->MR_type_ctor_arity;
        arg_vector_1 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti1);
        arg_vector_2 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti2);
    }

    // Compare the argument types.
    for (i = 1; i <= num_arg_types_1; i++) {
        comp = MR_compare_type_info(arg_vector_1[i], arg_vector_2[i]);
        if (comp != MR_COMPARE_EQUAL) {
            return comp;
        }
    }

    return MR_COMPARE_EQUAL;
}

MR_bool
MR_unify_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2)
{
    MR_TypeCtorInfo tci1;
    MR_TypeCtorInfo tci2;
    MR_TypeInfo     *arg_vector_1;
    MR_TypeInfo     *arg_vector_2;
    int             num_arg_types_1;
    int             num_arg_types_2;
    int             i;

    // Try to optimize a common case:
    // If type_info addresses are equal, they must represent the same type.

    if (ti1 == ti2) {
        return MR_TRUE;
    }

    // Otherwise, we need to expand equivalence types, if any.

    ti1 = MR_collapse_equivalences(ti1);
    ti2 = MR_collapse_equivalences(ti2);

    // Perhaps they are equal now...

    if (ti1 == ti2) {
        return MR_TRUE;
    }

    // Otherwise find the type_ctor_infos, and compare those.

    tci1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti1);
    tci2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti2);

    if (! MR_unify_type_ctor_info(tci1, tci2)) {
        return MR_FALSE;
    }

    // If the type_ctor_infos are equal, we don't need to compare
    // the arity of the types - they must be the same - unless they are
    // higher-order (which are all mapped to pred/0 or func/0) or tuples
    // (which are all mapped to tuple/0), in which cases we must compare
    // the arities before we can check the argument types.

    if (MR_type_ctor_has_variable_arity(tci1)) {
        num_arg_types_1 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti1);
        num_arg_types_2 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti2);

        // Check the arities.
        if (num_arg_types_1 != num_arg_types_2) {
            return MR_FALSE;
        }

        arg_vector_1 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti1);
        arg_vector_2 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti2);
    } else {
        num_arg_types_1 = tci1->MR_type_ctor_arity;
        arg_vector_1 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti1);
        arg_vector_2 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti2);
    }

    // Compare the argument types.
    for (i = 1; i <= num_arg_types_1; i++) {
        if (! MR_unify_type_info(arg_vector_1[i], arg_vector_2[i])) {
            return MR_FALSE;
        }
    }

    return MR_TRUE;
}

int
MR_compare_type_ctor_info(MR_TypeCtorInfo tci1, MR_TypeCtorInfo tci2)
{
    int             comp;
    MR_ConstString  modulename1;
    MR_ConstString  modulename2;
    MR_ConstString  typename1;
    MR_ConstString  typename2;
    int             arity1;
    int             arity2;

    // We are relying on the fact that type_ctor_infos are always
    // statically allocated to ensure that two type_ctor_infos are
    // for the same type iff their address is the same.
    //
    // The casts to (MR_Unsigned) here are in the hope of increasing
    // the chance that this will work on a segmented architecture.

    if ((MR_Unsigned) tci1 == (MR_Unsigned) tci2) {
        return MR_COMPARE_EQUAL;
    }

    modulename1 = tci1->MR_type_ctor_module_name;
    modulename2 = tci2->MR_type_ctor_module_name;

    comp = strcmp(modulename1, modulename2);
    if (comp < 0) {
        return MR_COMPARE_LESS;
    } else if (comp > 0) {
        return MR_COMPARE_GREATER;
    }

    typename1 = tci1->MR_type_ctor_name;
    typename2 = tci2->MR_type_ctor_name;
    comp = strcmp(typename1, typename2);
    if (comp < 0) {
        return MR_COMPARE_LESS;
    } else if (comp > 0) {
        return MR_COMPARE_GREATER;
    }

    arity1 = tci1->MR_type_ctor_arity;
    arity2 = tci2->MR_type_ctor_arity;
    if (arity1 < arity2) {
        return MR_COMPARE_LESS;
    } else if (arity1 > arity2) {
        return MR_COMPARE_GREATER;
    }

    MR_fatal_error("type_ctor_info match at distinct addresses");
}

MR_bool
MR_unify_type_ctor_info(MR_TypeCtorInfo tci1, MR_TypeCtorInfo tci2)
{
    // We are relying on the fact that type_ctor_infos are always
    // statically allocated to ensure that two type_ctor_infos are
    // for the same type iff their address is the same.
    //
    // The casts to (MR_Unsigned) here are in the hope of increasing
    // the chance that this will work on a segmented architecture.

    if ((MR_Unsigned) tci1 == (MR_Unsigned) tci2) {
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
}

MR_TypeInfo
MR_collapse_equivalences(MR_TypeInfo maybe_equiv_type_info)
{
    MR_TypeCtorInfo type_ctor_info;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(maybe_equiv_type_info);

    // Look past equivalences.
    while (MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV_GROUND
        || MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV)
    {
        maybe_equiv_type_info = MR_create_type_info(
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(maybe_equiv_type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);

        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(maybe_equiv_type_info);
    }

    return maybe_equiv_type_info;
}

MR_PseudoTypeInfo
MR_collapse_equivalences_pseudo(MR_PseudoTypeInfo maybe_equiv_pseudo_type_info)
{
    MR_TypeCtorInfo type_ctor_info;

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(maybe_equiv_pseudo_type_info)) {
        return maybe_equiv_pseudo_type_info;
    }

    type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(
        maybe_equiv_pseudo_type_info);

    // Look past equivalences.
    while (MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV_GROUND
        || MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV)
    {
        maybe_equiv_pseudo_type_info = MR_create_pseudo_type_info(
            MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(
                maybe_equiv_pseudo_type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);

        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(maybe_equiv_pseudo_type_info)) {
            return maybe_equiv_pseudo_type_info;
        }

        type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(
            maybe_equiv_pseudo_type_info);
    }

    return maybe_equiv_pseudo_type_info;
}

// MR_deallocate() frees up a list of memory cells.

void
MR_deallocate(MR_MemoryList allocated)
{
    while (allocated != NULL) {
        MR_MemoryList next = allocated->next;
        // These were allocated with MR_GC_NEW_ARRAY_ATTRIB so we must free
        // using MR_GC_free_attrib.

        MR_GC_free_attrib(allocated->data);
        MR_GC_free_attrib(allocated);
        allocated = next;
    }
}

MR_Word
MR_type_params_vector_to_list(int arity, MR_TypeInfoParams type_params)
{
    MR_Word     type_info_list;

    MR_restore_transient_registers();
    type_info_list = MR_list_empty();
    while (arity > 0) {
        type_info_list = MR_type_info_list_cons((MR_Word) type_params[arity],
            type_info_list);
        --arity;
    }

    MR_save_transient_registers();
    return type_info_list;
}

MR_Word
MR_pseudo_type_params_vector_to_list(int arity,
    MR_PseudoTypeInfoParams type_params)
{
    MR_Word     type_info_list;

    MR_restore_transient_registers();
    type_info_list = MR_list_empty();
    while (arity > 0) {
        type_info_list = MR_pseudo_type_info_list_cons(
            (MR_Word) type_params[arity], type_info_list);
        --arity;
    }

    MR_save_transient_registers();
    return type_info_list;
}

MR_Word
MR_arg_name_vector_to_list(int arity, const MR_ConstString *arg_names)
{
    MR_Word     arg_names_list;

    MR_restore_transient_registers();
    arg_names_list = MR_list_empty();

    if (arg_names == NULL) {
        // No arguments have names.
        while (arity > 0) {
            --arity;
            arg_names_list = MR_string_list_cons((MR_Word) NULL,
                arg_names_list);
        }
    } else {
        while (arity > 0) {
            --arity;
            arg_names_list = MR_string_list_cons((MR_Word) arg_names[arity],
                arg_names_list);
        }
    }

    MR_save_transient_registers();
    return arg_names_list;
}

MR_Word
MR_pseudo_type_info_vector_to_pseudo_type_info_list(int arity,
    MR_TypeInfoParams type_params,
    const MR_PseudoTypeInfo *arg_pseudo_type_infos)
{
    MR_PseudoTypeInfo   pseudo;
    MR_PseudoTypeInfo   arg_pseudo_type_info;
    MR_Word             pseudo_type_info_list;

    MR_restore_transient_registers();
    pseudo_type_info_list = MR_list_empty();

    while (--arity >= 0) {
        // Get the argument type_info.

        pseudo = arg_pseudo_type_infos[arity];
        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo) &&
            MR_TYPE_VARIABLE_IS_EXIST_QUANT(pseudo))
        {
            arg_pseudo_type_info = pseudo;
        } else {
            MR_save_transient_registers();
            arg_pseudo_type_info =
                MR_create_pseudo_type_info(
                    (MR_PseudoTypeInfoParams) type_params, pseudo);
            MR_restore_transient_registers();

            MR_save_transient_registers();
            arg_pseudo_type_info =
                MR_collapse_equivalences_pseudo(arg_pseudo_type_info);
            MR_restore_transient_registers();
        }

        pseudo_type_info_list = MR_pseudo_type_info_list_cons(
            (MR_Word) arg_pseudo_type_info, pseudo_type_info_list);
    }

    MR_save_transient_registers();
    return pseudo_type_info_list;
}

MR_Word
MR_typeclass_ref_error(MR_Word tci, int n, const char *msg)
{
    fprintf(stderr,
        "n1: # of extra instance args:   %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        MR_typeclass_info_num_extra_instance_args(tci));
    fprintf(stderr,
        "n1-n2: # of instance type vars: %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        MR_typeclass_info_num_instance_type_vars(tci));
    fprintf(stderr,
        "n2: # of instance constraints:  %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        MR_typeclass_info_num_instance_constraints(tci));
    fprintf(stderr,
        "n3: # of superclasses:          %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        MR_typeclass_info_num_superclasses(tci));
    fprintf(stderr,
        "n4: # of parameters:            %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        MR_typeclass_info_num_params(tci));
    fprintf(stderr, "access parameters: %s, %d\n", msg, n);
    MR_fatal_error("typeclass_info reference error");

    /*NOTREACHED*/
    return 0;
}

int
MR_cell_size_for_args(int arity, const MR_DuArgLocn *arg_locns)
{
    int                 last_arg_num;
    const MR_DuArgLocn  *last_arg;

    if (arg_locns == NULL) {
        return arity;
    }

    // The meanings of the various special values of MR_arg_{offset,bits}
    // are documented next to the definition of the MR_DuArgLocn type
    // in mercury_type_info.h.

    // We are looking for the last argument that occupies any storage.
    // Since we loop backwards, we stop at the *first* argument we find
    // that occupies any storage, and decide what to return based on
    // where that argument ends.
    last_arg_num = arity - 1;
    while (last_arg_num >= 0) {
        last_arg = &arg_locns[last_arg_num];

        if (last_arg->MR_arg_offset < 0) {
            // We reached the tagword; nothing but dummy args follow.
            return 1;
        }

        if (last_arg->MR_arg_bits >= 0) {
            // This means that argument #last_arg_num takes either
            // one full word, or part of a word. In the latter case,
            // the rest of the bits in the word (if any are left) are padding.
            return last_arg->MR_arg_offset + 1;
        } else {
            switch (last_arg->MR_arg_bits) {
                case -1:
                case -2:
                case -3:
                    // This means that argument #last_arg_num takes two words;
                    // it can be a double precision float, int64 or uint64.
                    return last_arg->MR_arg_offset + 2;

                case -4:
                case -5:
                case -6:
                case -7:
                case -8:
                case -9:
                    // This means that argument #last_arg_num is a
                    // sub-word-sized integer. Treat this case as we treat
                    // enums.
                    return last_arg->MR_arg_offset + 1;

                case -10:
                    // This indicates that argument #last_arg_num is a dummy,
                    // which means that its offset field is not meaningful.
                    // Try again with the previous argument (if there is one).
                    last_arg_num = last_arg_num - 1;
                    break;

                default:
                    MR_fatal_error("unknown code value in MR_arg_bits");
            }
        }
    }

    // We get here for cells that contain nothing but dummy values.
    // We can generate them now, but we should be able to optimize them away.
    // We return 1 because memory allocators should not be asked to allocate
    // zero words.
    return 1;
}

void
MR_print_type(FILE *fp, MR_TypeInfo type_info)
{
    MR_TypeCtorInfo tci;
    MR_TypeInfo     *arg_vector;
    int             arity;
    int             i;

    tci = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    if (MR_type_ctor_has_variable_arity(tci)) {
        arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
        arg_vector = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
    } else {
        arity = tci->MR_type_ctor_arity;
        arg_vector = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
    }

    fprintf(fp, "%s.%s",
        tci->MR_type_ctor_module_name, tci->MR_type_ctor_name);
    if (arity > 0) {
        fprintf(fp, "(");

        for (i = 1; i <= arity; i++) {
            MR_print_type(fp, arg_vector[i]);
            if (i < arity) {
                fprintf(fp, ", ");
            }
        }

        fprintf(fp, ")");
    }
}

////////////////////////////////////////////////////////////////////////////
