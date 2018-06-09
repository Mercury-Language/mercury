// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000-2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file is intended to be #included in mercury_type_info.c to provide
// the definitions of
//
//  MR_create_type_info
//  MR_create_pseudo_type_info
//  MR_make_type_info
//
// and their helper functions
//
//  MR_create_type_info_maybe_existq
//  MR_create_pseudo_type_info_maybe_existq
//  MR_make_type_info_maybe_existq.

return_type
usual_func(const params_type params, const MR_PseudoTypeInfo pseudo_type_info
    MAYBE_DECLARE_ALLOC_ARG)
{
    return exist_func(params, pseudo_type_info, NULL, NULL
        MAYBE_PASS_ALLOC_ARG);
}

return_type
exist_func(const params_type params, const MR_PseudoTypeInfo pseudo_type_info,
    const MR_Word *data_value, const MR_DuFunctorDesc *functor_desc
    MAYBE_DECLARE_ALLOC_ARG)
{
    MR_TypeCtorInfo     type_ctor_info;
    return_type         expanded;
    MR_Word             *type_info_arena;
    MR_Word             type_info_arena_word;
    MR_PseudoTypeInfo   *pseudo_type_info_arena;
    int                 arity;
    int                 start_region_size;
    int                 i;

    // The pseudo_type_info might be a polymorphic variable.
    // If so, substitute its value, and we are done.

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
#if create_pseudo
        if (MR_TYPE_VARIABLE_IS_EXIST_QUANT(pseudo_type_info)) {
            return pseudo_type_info;
        }

        expanded = MR_get_arg_pseudo_type_info(params, pseudo_type_info,
            data_value, functor_desc);

        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(expanded))
        {
            MR_fatal_error(exist_func_string ": unbound type variable");
        }

        return expanded;
#else
        expanded = MR_get_arg_type_info(params, pseudo_type_info,
            data_value, functor_desc);

        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE((MR_PseudoTypeInfo) expanded)) {
            MR_fatal_error(exist_func_string ": unbound type variable");
        }

        return expanded;
#endif
    }

    type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo_type_info);

    // No arguments - optimise common case.
    if ((MR_Word) type_ctor_info == (MR_Word) pseudo_type_info) {
#if create_pseudo
        return pseudo_type_info;
#else
        return MR_pseudo_type_info_is_ground(pseudo_type_info);
#endif
    }

    if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
        arity = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo_type_info);
        start_region_size = 2;
    } else {
        arity = type_ctor_info->MR_type_ctor_arity;
        start_region_size = 1;
    }

    // Iterate over the arguments, figuring out whether we need to make
    // any substitutions. If so, copy the resulting argument type_infos into
    // a new type_info.

    type_info_arena = NULL;
    pseudo_type_info_arena = (MR_PseudoTypeInfo *) pseudo_type_info;
    for (i = start_region_size; i < arity + start_region_size; i++) {
        expanded = exist_func(params, pseudo_type_info_arena[i],
            data_value, functor_desc MAYBE_PASS_ALLOC_ARG);

#if create_pseudo
        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE((MR_PseudoTypeInfo) expanded) &&
            ! MR_TYPE_VARIABLE_IS_EXIST_QUANT(pseudo_type_info))
        {
            MR_fatal_error(exist_func_string ": univ type variable");
        }
#else
        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE((MR_PseudoTypeInfo) expanded)) {
            MR_fatal_error(exist_func_string ": unbound type variable");
        }
#endif

        if (expanded != (return_type) pseudo_type_info_arena[i]) {
            // We made a substitution.
            // We need to allocate a new type_info,
            // if we haven't done so already.

            if (type_info_arena == NULL) {
                ALLOCATE_WORDS(type_info_arena_word,
                    arity + start_region_size);
                type_info_arena = (MR_Word *) type_info_arena_word;
                MR_memcpy(type_info_arena, (MR_Word *) pseudo_type_info,
                    (arity + start_region_size) * sizeof(MR_Word));
            }

            type_info_arena[i] = (MR_Word) expanded;
        }
    }

    if (type_info_arena == NULL) {
        return (return_type) pseudo_type_info;
    } else {
        return (return_type) type_info_arena;
    }
}
