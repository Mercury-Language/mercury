/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2003-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_term_size.c
**
** This module defines a function for measuring the sizes of terms.
*/

#include "mercury_imp.h"

#ifdef MR_RECORD_TERM_SIZES

MR_Unsigned
MR_term_size(MR_TypeInfo type_info, MR_Word term)
{
    MR_TypeCtorInfo         type_ctor_info;
    MR_DuTypeLayout         du_type_layout;
    const MR_DuPtagLayout   *ptag_layout;
    int                     ptag;
    int                     sectag;
    int                     arity;
    int                     size;

try_again:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_term_size: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
            /* XXX the code to handle these cases hasn't been written yet */
            MR_fatal_error("MR_term_size: RESERVED_ADDR");

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            du_type_layout = MR_type_ctor_layout(type_ctor_info).MR_layout_du;
            ptag = MR_tag(term);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
                case MR_SECTAG_NONE:
#ifdef MR_DEBUG_TERM_SIZES
                    if (ptag_layout->MR_sectag_alternatives[0]->
                        MR_du_functor_orig_arity <= 0)
                    {
                        MR_fatal_error("MR_term_size: zero arity ptag none");
                    }

                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag none %p -> %d\n",
                            (void *) term,
                            (int) MR_field(MR_mktag(ptag), term, -1));
                        printf("type %s.%s/%d, functor %s\n",
                            type_ctor_info->MR_type_ctor_module_name,
                            type_ctor_info->MR_type_ctor_name,
                            type_ctor_info->MR_type_ctor_arity,
                            ptag_layout->MR_sectag_alternatives[0]->
                                MR_du_functor_name);
                    }
#endif
                    return MR_field(MR_mktag(ptag), term, -1);

                case MR_SECTAG_LOCAL:
#ifdef MR_DEBUG_TERM_SIZES
                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag local %p\n",
                            (void *) term);
                    }
#endif
                    return 0;

                case MR_SECTAG_REMOTE:
#ifdef MR_DEBUG_TERM_SIZES
                    sectag = MR_field(MR_mktag(ptag), term, 0);

                    if (ptag_layout->MR_sectag_alternatives[sectag]->
                        MR_du_functor_orig_arity <= 0)
                    {
                        MR_fatal_error("MR_term_size: zero arity ptag remote");
                    }

                    if (MR_heapdebug && MR_lld_print_enabled) {
                        printf("MR_term_size: du sectag remote %p -> %d\n",
                            (void *) term,
                            (int) MR_field(MR_mktag(ptag), term, -1));
                        printf("type %s.%s/%d, functor %s\n",
                                type_ctor_info->MR_type_ctor_module_name,
                                type_ctor_info->MR_type_ctor_name,
                                type_ctor_info->MR_type_ctor_arity,
                                ptag_layout->MR_sectag_alternatives[sectag]->
                                    MR_du_functor_name);
                    }
#endif
                    return MR_field(MR_mktag(ptag), term, -1);

                case MR_SECTAG_VARIABLE:
                    MR_fatal_error("MR_term_size: VARIABLE");

                default:
                    fprintf(stderr, "sectag_locn: %d\n",
                        (int) ptag_layout->MR_sectag_locn);
                    MR_fatal_error("MR_term_size: sectag_locn");
            }

        case MR_TYPECTOR_REP_EQUIV:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: equiv %p\n", (void *) term);
            }
#endif
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            goto try_again;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: equiv ground %p\n", (void *) term);
            }
#endif
            type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            goto try_again;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: notag (usereq) %p\n", (void *) term);
            }
#endif
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                    MR_notag_functor_arg_type);
            MR_restore_transient_hp();
            goto try_again;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: notag ground (usereq) %p\n",
                    (void *) term);
            }
#endif
            type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag
                ->MR_notag_functor_arg_type);
            goto try_again;

        case MR_TYPECTOR_REP_TUPLE:
            arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
            if (arity == 0) {
                /* term may be a NULL pointer, so don't follow it */
                size = 0;
            } else {
                size = MR_field(MR_mktag(0), term, -1);
            }

#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: tuple %p -> %d\n",
                    (void *) term, size);
            }
#endif
            return size;

        case MR_TYPECTOR_REP_PRED:
        case MR_TYPECTOR_REP_FUNC:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: pred/func %p\n", (void *) term);
            }
#endif
            /* currently we don't collect stats on closure sizes */
            return 0;

        case MR_TYPECTOR_REP_ARRAY:
            /* currently we don't collect stats on array sizes */
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: array %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: enum (usereq) %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_INT:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: int %p %ld\n",
                    (void *) term, (long) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_CHAR:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: char %p %c\n",
                    (void *) term, (char) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_FLOAT:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: float %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_STRING:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: string %p '%s'\n",
                    (void *) term, (char *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_SUCCIP:
        case MR_TYPECTOR_REP_HP:
        case MR_TYPECTOR_REP_CURFR:
        case MR_TYPECTOR_REP_MAXFR:
        case MR_TYPECTOR_REP_REDOFR:
        case MR_TYPECTOR_REP_REDOIP:
        case MR_TYPECTOR_REP_TRAIL_PTR:
        case MR_TYPECTOR_REP_TICKET:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: impl artifact type %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPECLASSINFO:
        case MR_TYPECTOR_REP_TYPECTORINFO:
        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        case MR_TYPECTOR_REP_TYPEDESC:
        case MR_TYPECTOR_REP_TYPECTORDESC:
        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: type_info etc %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_SUBGOAL:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: subgoal %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_STABLE_C_POINTER:
        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: c_pointer/foreign %p\n", (void *) term);
            }
#endif
            return 0;

        case MR_TYPECTOR_REP_REFERENCE:
#ifdef MR_DEBUG_TERM_SIZES
            if (MR_heapdebug && MR_lld_print_enabled) {
                printf("MR_term_size: reference %p\n", (void *) term);
            }
#endif
            return 1;

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error("MR_term_size: VOID");

        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error("MR_term_size: UNKNOWN");

        default:
            fprintf(stderr, "default rep: %d\n",
                    (int) MR_type_ctor_rep(type_ctor_info));
            MR_fatal_error("MR_term_size: default");
    }

    MR_fatal_error("MR_term_size: unexpected fallthrough");
}

#endif  /* MR_RECORD_TERM_SIZES */
