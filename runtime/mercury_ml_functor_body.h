// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2004, 2007 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_ml_functor_body.h
//
// This file is included several times in library/deconstruct.m. Each inclusion
// defines the body of one of several variants of the `functor' function.
//
// The code including this file must define these macros:
//
// TYPEINFO_ARG         Gives the name of the argument that contains the
//                      typeinfo of the term being deconstructed.
//
// TERM_ARG             Gives the name of the argument that contains the
//                      value of the term being deconstructed.
//
// FUNCTOR_ARG          Gives the name of the argument to which we assign
//                      the function symbol of the term.
//
// FUNCTOR_NUMBER_ARG   Gives the name of the argument to which we assign
//                      the function symbol number of the term.
//
// ARITY_ARG            Gives the name of the argument to which we assign
//                      the arity of the term.
//
// NONCANON             Gives a value of type MR_noncanon_handling; its value
//                      will govern the handling of values of noncanonical
//                      types.

    MR_TypeInfo                 type_info;
    MR_ExpandFunctorOnlyInfo    expand_info;
    MR_ConstString              conststring_functor;

    type_info = (MR_TypeInfo) TYPEINFO_ARG;

    MR_save_transient_registers();
    MR_expand_functor_only(type_info, &TERM_ARG, NONCANON, &expand_info);
    MR_restore_transient_registers();

#ifdef FUNCTOR_ARG
    MR_deconstruct_get_functor(expand_info, functor_only, conststring_functor);
    // Cast away const without getting a warning for it.
    FUNCTOR_ARG = (MR_String) (MR_Integer) conststring_functor;
#endif
    MR_deconstruct_get_arity(expand_info, ARITY_ARG);

#ifdef FUNCTOR_NUMBER_ARG
    MR_deconstruct_get_functor_number(expand_info, FUNCTOR_NUMBER_ARG);
#endif

