// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This files defines the bodies of the variants of the
// MR_type_info_lookup_or_add() function.

    MR_TypeCtorInfo     type_ctor_info;
    MR_TrieNode         node;
    MR_TypeInfo         *arg_vector;
    int                 arity;
    int                 i;

    // XXX memory allocation here should be optimized
    type_info = MR_collapse_equivalences(type_info);

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    node = tci_call(table, (MR_Integer) type_ctor_info);

    // All calls to MR_type_info_lookup_or_add that have the same value
    // of node at this point agree on the type_ctor_info of the type
    // being tabled. They must therefore also agree on its arity.
    // This is why looping over all the arguments works.
    //
    // If type_info has a zero-arity type_ctor, then it may be stored
    // using a one-cell type_info, and type_info_args does not make sense.
    // This is OK, because in that case it will never be used.

    if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
        arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
        arg_vector = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
        node = MR_int_hash_lookup_or_add(node, arity);
    } else {
        arity = type_ctor_info->MR_type_ctor_arity;
        arg_vector = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
    }

    for (i = 1; i <= arity; i++) {
        node = rec_call(node, arg_vector[i]);
    }

    return node;
