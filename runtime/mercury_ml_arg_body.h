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
** This file is included several times in library/deconstruct.m. Each inclusion
** defines the body of one of several variants of the `arg' function.
**
** The code including this file must define these macros:
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
** SELECTED_TYPE_INFO   Gives the name of the argument to which the typeinfo of
**                      the selected field should be assigned.
**
** NONCANON             Gives a value of type MR_noncanon_handling; its value
**                      will govern the handling of values of noncanonical
**                      types.
**
** The code including this file may define these macros:
**
** SELECT_BY_NAME       If defined, the argument is selected by name; if it is
**                      not defined, the argument is selected by position.
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
        &argument_ptr, NONCANON);
    MR_restore_transient_registers();
    if (success) {
        /*
        ** The following code is what *should* be here. The reason it is
        ** commented out, and the code to create a univ used instead, is
        ** the typechecking bug reported on 30 Jan, 2002.
        **
        ** SELECTED_ARG = *argument_ptr;                               
        ** SELECTED_TYPE_INFO = arg_type_info;
        */

        MR_new_univ_on_hp(SELECTED_ARG, arg_type_info, *argument_ptr);
    }

    SUCCESS_INDICATOR = success;

#undef  arg_func
