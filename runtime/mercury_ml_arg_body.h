// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_ml_arg_body.h
//
// This file is included several times in library/deconstruct.m. Each inclusion
// defines the body of one of several variants of the `arg' function.
//
// The code including this file must define these macros:
//
// TYPEINFO_ARG         Gives the name of the argument that contains the
//                      typeinfo of the term being deconstructed.
//
// TERM_ARG             Gives the name of the argument that contains the
//                      value of the term being deconstructed.
//
// SELECTOR_ARG         Gives the C expression that selects one field of the
//                      term.
//
// SELECTED_ARG         Gives the name of the argument to which the value of
//                      the selected field should be assigned.
//
// SELECTED_TYPE_INFO   Gives the name of the argument to which the typeinfo of
//                      the selected field should be assigned.
//
// NONCANON             Gives a value of type MR_noncanon_handling; its value
//                      will govern the handling of values of noncanonical
//                      types.
//
// The code including this file may define these macros:
//
// SELECT_BY_NAME       If defined, the argument is selected by name; if it is
//                      not defined, the argument is selected by position.
//
// SAVE_SUCCESS         If defined, success is saved into SUCCESS_INDICATOR.

#ifdef  SELECT_BY_NAME
  #define arg_func  MR_named_arg
#else
  #define arg_func  MR_arg
#endif

    MR_TypeInfo         type_info;
    MR_TypeInfo         arg_type_info;
    MR_Word             arg_term;
    MR_Word             *word_sized_arg_ptr;    // unused here
    MR_bool             success;

    type_info = (MR_TypeInfo) TYPEINFO_ARG;

    MR_save_transient_registers();
    success = arg_func(type_info, &TERM_ARG, NONCANON, SELECTOR_ARG,
        &arg_type_info, &arg_term, &word_sized_arg_ptr);
    MR_restore_transient_registers();
    if (success) {
        // The following code is what *should* be here. The commit message
        // of commit fcccbd166f5fe555baffde1b7a644ef5ef14f4dd says it should be
        // reinstated when "the typechecker has been fixed to allow different
        // clauses to return existentially typed values". This refers to
        // predicates such as deconstruct.arg, which has (or at least had)
        // three separate modes, each of which is for a specific value
        // of its NONCANON argument, and which has (or at least had) separate
        // foreign_procs for each mode. See the discussion on m-rev starting
        // around 2002 Jan 30.
        //
        // SELECTED_ARG = value;
        // SELECTED_TYPE_INFO = arg_type_info;

        MR_new_univ_on_hp(SELECTED_ARG, arg_type_info, arg_term);
    }

#ifdef SAVE_SUCCESS
    SUCCESS_INDICATOR = success;
#endif

#undef  arg_func
