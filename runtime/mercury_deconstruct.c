/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_deconstruct.c
**
** This file provides utility functions for deconstructing terms, for use by
** the standard library.
*/

#include "mercury_imp.h"
#include "mercury_deconstruct.h"
#include "mercury_deconstruct_macros.h"
#include "mercury_type_desc.h"

static  MR_ConstString  MR_expand_type_name(MR_TypeCtorInfo tci, MR_bool);

#define EXPAND_FUNCTION_NAME        MR_expand_functor_args
#define EXPAND_TYPE_NAME            MR_Expand_Functor_Args_Info
#define EXPAND_FUNCTOR_FIELD        functor
#define EXPAND_ARGS_FIELD           args
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD
#undef  EXPAND_ARGS_FIELD

#define EXPAND_FUNCTION_NAME        MR_expand_functor_args_limit
#define EXPAND_TYPE_NAME            MR_Expand_Functor_Args_Limit_Info
#define EXPAND_FUNCTOR_FIELD        functor
#define EXPAND_ARGS_FIELD           args
#define EXPAND_APPLY_LIMIT
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD
#undef  EXPAND_ARGS_FIELD
#undef  EXPAND_APPLY_LIMIT

#define EXPAND_FUNCTION_NAME        MR_expand_functor_only
#define EXPAND_TYPE_NAME            MR_Expand_Functor_Only_Info
#define EXPAND_FUNCTOR_FIELD        functor_only
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_FUNCTOR_FIELD

#define EXPAND_FUNCTION_NAME        MR_expand_args_only
#define EXPAND_TYPE_NAME            MR_Expand_Args_Only_Info
#define EXPAND_ARGS_FIELD           args_only
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_ARGS_FIELD

#define EXPAND_FUNCTION_NAME        MR_expand_chosen_arg_only
#define EXPAND_TYPE_NAME            MR_Expand_Chosen_Arg_Only_Info
#define EXPAND_CHOSEN_ARG
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_CHOSEN_ARG

#define EXPAND_FUNCTION_NAME        MR_expand_named_arg_only
#define EXPAND_TYPE_NAME            MR_Expand_Chosen_Arg_Only_Info
#define EXPAND_NAMED_ARG
#include "mercury_ml_expand_body.h"
#undef  EXPAND_FUNCTION_NAME
#undef  EXPAND_TYPE_NAME
#undef  EXPAND_NAMED_ARG

/*
** N.B. any modifications to the signature of this function will require
** changes not only to library/deconstruct.m, but also to library/store.m
** and extras/trailed_update/tr_store.m.
*/

MR_bool
MR_arg(MR_TypeInfo type_info, MR_Word *term_ptr, int arg_index,
    MR_TypeInfo *arg_type_info_ptr, MR_Word **arg_ptr,
    MR_noncanon_handling noncanon)
{
    MR_Expand_Chosen_Arg_Only_Info  expand_info;

    MR_expand_chosen_arg_only(type_info, term_ptr, noncanon, arg_index,
            &expand_info);

        /* Check range */
    if (expand_info.chosen_index_exists) {
        *arg_type_info_ptr = expand_info.chosen_type_info;
        *arg_ptr = expand_info.chosen_value_ptr;
        return MR_TRUE;
    }

    return MR_FALSE;
}

MR_bool
MR_named_arg(MR_TypeInfo type_info, MR_Word *term_ptr, MR_ConstString arg_name,
    MR_TypeInfo *arg_type_info_ptr, MR_Word **arg_ptr,
    MR_noncanon_handling noncanon)
{
    MR_Expand_Chosen_Arg_Only_Info  expand_info;

    MR_expand_named_arg_only(type_info, term_ptr, noncanon, arg_name,
            &expand_info);

        /* Check range */
    if (expand_info.chosen_index_exists) {
        *arg_type_info_ptr = expand_info.chosen_type_info;
        *arg_ptr = expand_info.chosen_value_ptr;
        return MR_TRUE;
    }

    return MR_FALSE;
}

MR_bool
MR_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
    const char *arg_name, int *arg_num_ptr)
{
    MR_TypeCtorInfo             type_ctor_info;
    MR_DuTypeLayout             du_type_layout;
    const MR_DuPtagLayout       *ptag_layout;
    const MR_DuFunctorDesc      *functor_desc;
    const MR_NotagFunctorDesc   *notag_functor_desc;
    MR_Word                     data;
    int                         ptag;
    MR_Word                     sectag;
    MR_TypeInfo                 eqv_type_info;
    int                         i;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        {
            MR_ReservedAddrTypeLayout ra_layout;
        
            ra_layout = MR_type_ctor_layout(type_ctor_info).
                MR_layout_reserved_addr;
            data = *term_ptr;

            /*
            ** First check if this value is one of
            ** the numeric reserved addresses.
            */
            if ((MR_Unsigned) data <
                (MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
            {
                /*
                ** If so, it must be a constant, and constants never have
                ** any arguments.
                */
                return MR_FALSE;
            }

            /*
            ** Next check if this value is one of the
            ** the symbolic reserved addresses.
            */
            for (i = 0; i < ra_layout->MR_ra_num_res_symbolic_addrs; i++) {
                if (data == (MR_Word) ra_layout->MR_ra_res_symbolic_addrs[i]) {
                    return MR_FALSE;
                }
            }
            
            /*
            ** Otherwise, it is not one of the reserved addresses,
            ** so handle it like a normal DU type.
            */
            du_type_layout = ra_layout->MR_ra_other_functors;
            goto du_type;
        }


        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_DU:
            data = *term_ptr;
            du_type_layout = MR_type_ctor_layout(type_ctor_info).MR_layout_du;
            /* fall through */

        /*
        ** This label handles both the DU case and the second half of the
        ** RESERVED_ADDR case.  `du_type_layout' and `data' must both be
        ** set before this code is entered.
        */
        du_type:
            ptag = MR_tag(data);
            ptag_layout = &du_type_layout[ptag];

            switch (ptag_layout->MR_sectag_locn) {
                case MR_SECTAG_NONE:
                    functor_desc = ptag_layout->MR_sectag_alternatives[0];
                    break;
                case MR_SECTAG_LOCAL:
                    sectag = MR_unmkbody(data);
                    functor_desc = ptag_layout->MR_sectag_alternatives[sectag];
                    break;
                case MR_SECTAG_REMOTE:
                    sectag = MR_field(ptag, data, 0);
                    functor_desc = ptag_layout->MR_sectag_alternatives[sectag];
                    break;
                case MR_SECTAG_VARIABLE:
                    MR_fatal_error("MR_named_arg_num(): unexpected variable");
                default:
                    MR_fatal_error("MR_named_arg_num(): invalid sectag_locn");
            }

            if (functor_desc->MR_du_functor_arg_names == NULL) {
                return MR_FALSE;
            }

            for (i = 0; i < functor_desc->MR_du_functor_orig_arity; i++) {
                if (functor_desc->MR_du_functor_arg_names[i] != NULL
                && MR_streq(arg_name,
                        functor_desc->MR_du_functor_arg_names[i]))
                {
                    *arg_num_ptr = i;
                    return MR_TRUE;
                }
            }

            return MR_FALSE;

        case MR_TYPECTOR_REP_EQUIV:
            eqv_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            return MR_named_arg_num(eqv_type_info, term_ptr, arg_name,
                arg_num_ptr);

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            eqv_type_info = MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            return MR_named_arg_num(eqv_type_info, term_ptr, arg_name,
                arg_num_ptr);

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            notag_functor_desc = MR_type_ctor_functors(type_ctor_info).
                MR_functors_notag;

            if (notag_functor_desc->MR_notag_functor_arg_name != NULL
            && MR_streq(arg_name,
                    notag_functor_desc->MR_notag_functor_arg_name))
            {
                *arg_num_ptr = 0;
                return MR_TRUE;
            }

            return MR_FALSE;

        default:
            return MR_FALSE;
    }
}

static MR_ConstString
MR_expand_type_name(MR_TypeCtorInfo tci, MR_bool wrap)
{
    MR_String   str;
    int         len;

    len = 0;
    len += strlen(tci->MR_type_ctor_module_name);
    len += 1;   /* : */
    len += strlen(tci->MR_type_ctor_name);
    len += 1;   /* / */
    len += 4;   /* arity; we do not support arities above 1024 */
    if (wrap) {
        len += 4;   /* <<>> */
    }
    len += 1;   /* NULL */

    if (tci->MR_type_ctor_arity > 9999) {
        MR_fatal_error("MR_expand_type_name: arity > 9999");
    }

    MR_restore_transient_hp();
    MR_allocate_aligned_string_msg(str, len, "MR_expand_type_name");
    MR_save_transient_hp();

    sprintf(str, wrap? "<<%s:%s/%d>>" : "%s:%s/%d",
        tci->MR_type_ctor_module_name,
        tci->MR_type_ctor_name,
        tci->MR_type_ctor_arity);

    return (MR_ConstString) str;
}
