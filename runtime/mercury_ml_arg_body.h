/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_ml_arg_body.h
**
** This file is included several times in library/std_util.m. Each inclusion
** defines the body of one of several variants of `arg' function.
**
** The code including this file must define these macros:
**
** PREDNAME             Gives the name of the function or predicate being
**                      defined.
**
** NONCANON_HANDLING    Gives the desired handling of non-canonical types
**                      as a value of C type MR_noncanon_handling.
**
** TYPEINFO_ARG         Gives the name of the argument that contains the
**                      typeinfo of the term being deconstructed.
**
** TERM_ARG             Gives the name of the argument that contains the
**                      value of the term being deconstructed.
**
** SELECTOR_ARG         Gives the C expression that selects one field of the
**                      term.
**
** SELECTED_ARG         Gives the name of the argument to which the value of
**                      the selected field should be assigned.
**
** The code including this file may define these macros:
**
** SELECT_BY_NAME       If defined, the argument is selected by name; if it is
**                      not defined, the argument is selected by position.
**
** EXPECTED_TYPE_INFO   If defined, gives a C expression containing the
**                      typeinfo of the expected type
*/

#ifdef  SELECT_BY_NAME
  #define arg_func  MR_named_arg
#else
  #define arg_func  MR_arg
#endif

    MR_TypeInfo type_info;
    MR_TypeInfo arg_type_info;
    MR_Word     *argument_ptr;
    bool        success;

    type_info = (MR_TypeInfo) TYPEINFO_ARG;

    MR_save_transient_registers();
    success = arg_func(type_info, &TERM_ARG, SELECTOR_ARG, &arg_type_info,
        &argument_ptr, NONCANON_HANDLING, MR_noncanon_msg(PREDNAME));
#ifdef EXPECTED_TYPE_INFO
    if (success) {                                                          \
        /* compare the actual type of the argument with its expected type */\
        int         comparison_result;                                      \
        comparison_result = MR_compare_type_info(arg_type_info,             \
            (MR_TypeInfo) EXPECTED_TYPE_INFO);                              \
        success = (comparison_result == MR_COMPARE_EQUAL);                  \
                                                                            \
        if (success) {                                                      \
            SELECTED_ARG = *argument_ptr;                                   \
        }                                                                   \
    }                                                                       \
                                                                            \
    MR_restore_transient_registers();                                       \
    SUCCESS_INDICATOR = success;
#else
    MR_restore_transient_registers();                                       \
    if (success) {                                                          \
        MR_new_univ_on_hp(SELECTED_ARG, arg_type_info, *argument_ptr);      \
    }                                                                       \
                                                                            \
    SUCCESS_INDICATOR = success;
#endif

#undef  arg_func
