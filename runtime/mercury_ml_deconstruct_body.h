/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_ml_deconstruct_body.h
**
** This file is included several times in library/std_util.m. Each inclusion
** defines the body of one of several variants of `deconstruct' function.
**
** The code including this file must define these macros:
**
** PREDNAME             Gives the name of the function or predicate being
**                      defined.
**
** EXPAND_INFO_CALL     Gives the name of the MR_expand_functor_* variant that 
**                      we want to use.
**
** EXPAND_INFO_TYPE     Gives the type of the expand_info argument of
**                      EXPAND_INFO_CALL.
**
** TYPEINFO_ARG         Gives the name of the argument that contains the
**                      typeinfo of the term being deconstructed.
**
** TERM_ARG             Gives the name of the argument that contains the
**                      value of the term being deconstructed.
**
** FUNCTOR_ARG          Gives the name of the argument to which we assign
**                      the function symbol of the term.
**
** ARITY_ARG            Gives the name of the argument to which the value of
**                      the arity field should be assigned.
**
** ARGUMENTS_ARG        Gives the name of the argument to which the list of
**                      univs representing the arguments of the term should
**                      be assigned.
**
** The code including this file may define these macros:
**
** ALLOW_NONCANONICAL   If defined, allow the deconstruction of non-canonical
**                      types. If not defined, abort if the type being
**                      deconstructed is non-canonical.
**
** MAX_ARITY_ARG        If defined, gives the name of the argument whose value
**                      gives the maximum number of arguments we want to
**                      succeed for.
*/

#ifdef	ALLOW_NONCANONICAL
  #define maybe_abort_if_noncanonical(expand_info, msg)            \
	((void) 0)
#else
  #define maybe_abort_if_noncanonical(expand_info, msg)            \
	MR_abort_if_type_is_noncanonical(expand_info, msg)
#endif

#ifdef  MAX_ARITY_ARG
  #define   maybe_max_arity_arg     MAX_ARITY_ARG,
  #define   max_arity_check_start                                       \
                                    if (expand_info.limit_reached) {    \
                                        SUCCESS_INDICATOR = FALSE;      \
                                    } else {                            \
                                        SUCCESS_INDICATOR = TRUE;
  #define   max_arity_check_end     }
#else
  #define   maybe_max_arity_arg
  #define   max_arity_check_start
  #define   max_arity_check_end
#endif

    EXPAND_INFO_TYPE	expand_info;
    MR_TypeInfo    		type_info;

    type_info = (MR_TypeInfo) TYPEINFO_ARG;

    MR_save_transient_registers();
    EXPAND_INFO_CALL(type_info, &TERM_ARG, maybe_max_arity_arg &expand_info);
    MR_restore_transient_registers();

    maybe_abort_if_noncanonical(expand_info, MR_noncanon_msg(PREDNAME));

    max_arity_check_start
        MR_deconstruct_get_functor(expand_info, functor, FUNCTOR_ARG);
        MR_deconstruct_get_arity(expand_info, ARITY_ARG);
        MR_deconstruct_get_arg_list(expand_info, args, ARGUMENTS_ARG);
        MR_deconstruct_free_allocated_arg_type_infos(expand_info, args);
    max_arity_check_end

#undef  maybe_abort_if_noncanonical
#undef  maybe_max_arity_arg
#undef  max_arity_check_start
#undef  max_arity_check_end
