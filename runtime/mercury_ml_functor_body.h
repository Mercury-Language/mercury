/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_ml_functor_body.h
**
** This file is included several times in library/std_util.m. Each inclusion
** defines the body of one of several variants of `functor' function.
**
** The code including this file must define these macros:
**
** PREDNAME             Gives the name of the function or predicate being
**                      defined.
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
** ARITY_ARG            Gives the name of the argument to which we assign
**                      the arity of the term.
**
** The code including this file may define these macros:
**
** ALLOW_NONCANONICAL   If defined, allow the deconstruction of non-canonical
**                      types. If not defined, abort if the type being
**                      deconstructed is non-canonical.
*/

#ifdef	ALLOW_NONCANONICAL
  #define maybe_abort_if_noncanonical(expand_info, msg)            \
	((void) 0)
#else
  #define maybe_abort_if_noncanonical(expand_info, msg)            \
	MR_abort_if_type_is_noncanonical(expand_info, msg)
#endif

    MR_TypeInfo                 type_info;
    MR_Expand_Functor_Only_Info expand_info;

    type_info = (MR_TypeInfo) TYPEINFO_ARG;

    MR_save_transient_registers();
    MR_expand_functor_only(type_info, &TERM_ARG, &expand_info);
    MR_restore_transient_registers();

    maybe_abort_if_noncanonical(expand_info, MR_noncanon_msg(PREDNAME));
    MR_deconstruct_get_functor(expand_info, functor_only, FUNCTOR_ARG);
    MR_deconstruct_get_arity(expand_info, ARITY_ARG);

#undef  maybe_abort_if_noncanonical
