/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains a piece of code that is included by mercury_ho_call.c
** four times:
** 
** - as the body of the mercury__unify_2_0 Mercury procedure,
** - as the body of the mercury__compare_3_3 Mercury procedure, and
** - as the body of the MR_generic_unify C function.
** - as the body of the MR_generic_compare C function.
**
** The inclusions are surrounded by #defines and #undefs of the macros
** that personalize each copy of the code.
**
** The reason why the unify and compare Mercury procedures share code is
** that unify is mostly just a special case of comparison; it differs only
** by treating "less than" and "greater than" the same way, and returning
** its result slightly differently.
**
** The reason why there is both a Mercury procedure and a C function for
** unifications and comparisons is that the Mercury procedure needs a
** mechanism that allows it to unify or compare each argument of a function
** symbol, and doing it with a loop body that calls C function is
** significantly easier to program, and probably more efficient, than
** using recursion in Mercury. The Mercury procedure and C function share code
** because they implement the same task.
**
** We need separate C functions for unifications and comparison because
** with --no-special-preds, a type with user-defined equality has an
** a non-NULL unify_pred field in its type_ctor_info but a NULL compare_pred
** field. While in principle unification is a special case of comparison,
** we cannot implement unifications by comparisons for such types:
** they support unifications but not comparisons. Since we cannot do it
** for such types, it is simplest not to do it for any types.
*/

    DECLARE_LOCALS
    initialize();

start_label:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

#ifdef  MR_TYPE_CTOR_STATS
    MR_register_type_ctor_stat(&type_stat_struct, type_ctor_info);
#endif

    switch (type_ctor_info->type_ctor_rep) {

#ifdef  MR_COMPARE_BY_RTTI

        case MR_TYPECTOR_REP_EQUIV:
            save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                type_ctor_info->type_layout.layout_equiv);
            restore_transient_hp();
            goto start_label;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            type_info = (MR_TypeInfo) type_ctor_info->type_layout.layout_equiv;
            goto start_label;

        case MR_TYPECTOR_REP_EQUIV_VAR:
            fatal_error("found type_ctor_rep MR_TYPECTOR_REP_EQUIV_VAR");

        case MR_TYPECTOR_REP_NOTAG:
            save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                type_ctor_info->type_layout.layout_notag->
                MR_notag_functor_arg_type);
            restore_transient_hp();
            goto start_label;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
            type_info = (MR_TypeInfo) type_ctor_info->type_layout.
                layout_notag->MR_notag_functor_arg_type;
            goto start_label;

        case MR_TYPECTOR_REP_DU:
            {
                const MR_DuFunctorDesc  *functor_desc;
#ifdef  select_compare_code
                const MR_DuFunctorDesc  *x_functor_desc;
                const MR_DuFunctorDesc  *y_functor_desc;
                MR_DuPtagLayout         *x_ptaglayout;
                MR_DuPtagLayout         *y_ptaglayout;
#else
                Word                    x_ptag;
                Word                    y_ptag;
                Word                    x_sectag;
                Word                    y_sectag;
                MR_DuPtagLayout         *ptaglayout;
#endif
                Word                    *x_data_value;
                Word                    *y_data_value;
                const MR_DuExistInfo    *exist_info;
                int                     result;
                int                     cur_slot;
                int                     arity;
                int                     i;

#ifdef  select_compare_code

#define MR_find_du_functor_desc(data, data_value, functor_desc)               \
                do {                                                          \
                    MR_DuPtagLayout         *ptaglayout;                      \
                    int                     ptag;                             \
                    int                     sectag;                           \
                                                                              \
                    ptag = MR_tag(data);                                      \
                    ptaglayout = &type_ctor_info->type_layout.layout_du[ptag];\
                    data_value = (Word *) MR_body(data, ptag);                \
                                                                              \
                    switch (ptaglayout->MR_sectag_locn) {                     \
                        case MR_SECTAG_LOCAL:                                 \
                            sectag = MR_unmkbody(data_value);                 \
                            break;                                            \
                        case MR_SECTAG_REMOTE:                                \
                            sectag = data_value[0];                           \
                            break;                                            \
                        case MR_SECTAG_NONE:                                  \
                            sectag = 0;                                       \
                            break;                                            \
                    }                                                         \
                                                                              \
                    functor_desc = ptaglayout->MR_sectag_alternatives[sectag];\
                } while (0)

                MR_find_du_functor_desc(x, x_data_value, x_functor_desc);
                MR_find_du_functor_desc(y, y_data_value, y_functor_desc);

#undef MR_find_du_functor_desc

                if (x_functor_desc->MR_du_functor_ordinal !=
                    y_functor_desc->MR_du_functor_ordinal)
                {
                    if (x_functor_desc->MR_du_functor_ordinal <
                        y_functor_desc->MR_du_functor_ordinal)
                    {
                        return_answer(MR_COMPARE_LESS);
                    } else {
                        return_answer(MR_COMPARE_GREATER);
                    }
                }

                functor_desc = x_functor_desc;
#else
                x_ptag = MR_tag(x);
                y_ptag = MR_tag(y);

                if (x_ptag != y_ptag) {
                    return_answer(FALSE);
                }

                ptaglayout = &type_ctor_info->type_layout.layout_du[x_ptag];
                x_data_value = (Word *) MR_body(x, x_ptag);
                y_data_value = (Word *) MR_body(y, y_ptag);

                switch (ptaglayout->MR_sectag_locn) {
                    case MR_SECTAG_LOCAL:
                        x_sectag = MR_unmkbody(x_data_value);
                        y_sectag = MR_unmkbody(y_data_value);

                        if (x_sectag != y_sectag) {
                            return_answer(FALSE);
                        }

                        break;

                    case MR_SECTAG_REMOTE:
                        x_sectag = x_data_value[0];
                        y_sectag = y_data_value[0];

                        if (x_sectag != y_sectag) {
                            return_answer(FALSE);
                        }

                        break;

                    case MR_SECTAG_NONE:
                        x_sectag = 0;
                        break;
                }

                functor_desc = ptaglayout->MR_sectag_alternatives[x_sectag];
#endif

                if (functor_desc->MR_du_functor_sectag_locn ==
                    MR_SECTAG_REMOTE)
                {
                    cur_slot = 1;
                } else {
                    cur_slot = 0;
                }

                arity = functor_desc->MR_du_functor_orig_arity;
                exist_info = functor_desc->MR_du_functor_exist_info;

                if (exist_info != NULL) {
                    int                     num_ti_plain;
                    int                     num_ti_in_tci;
                    int                     num_tci;
                    const MR_DuExistLocn    *locns;
                    MR_TypeInfo             x_ti;
                    MR_TypeInfo             y_ti;

                    num_ti_plain = exist_info->MR_exist_typeinfos_plain;
                    num_ti_in_tci = exist_info->MR_exist_typeinfos_in_tci;
                    num_tci = exist_info->MR_exist_tcis;
                    locns = exist_info->MR_exist_typeinfo_locns;

                    for (i = 0; i < num_ti_plain + num_ti_in_tci; i++) {
                        if (locns[i].MR_exist_offset_in_tci < 0) {
                            x_ti = (MR_TypeInfo)
                                x_data_value[locns[i].MR_exist_arg_num];
                            y_ti = (MR_TypeInfo)
                                y_data_value[locns[i].MR_exist_arg_num];
                        } else {
                            x_ti = (MR_TypeInfo) MR_typeclass_info_type_info(
                                x_data_value[locns[i].MR_exist_arg_num],
                                locns[i].MR_exist_offset_in_tci);
                            y_ti = (MR_TypeInfo) MR_typeclass_info_type_info(
                                y_data_value[locns[i].MR_exist_arg_num],
                                locns[i].MR_exist_offset_in_tci);
                        }
                        result = MR_compare_type_info(x_ti, y_ti);
                        if (result != MR_COMPARE_EQUAL) {
#ifdef  select_compare_code
                            return_answer(result);
#else
                            return_answer(FALSE);
#endif
                        }
                    }

                    cur_slot += num_ti_plain + num_tci;
                }

                for (i = 0; i < arity; i++) {
                    MR_TypeInfo arg_type_info;

                    if (MR_arg_type_may_contain_var(functor_desc, i)) {
                        arg_type_info = MR_create_type_info_maybe_existq(
                            MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                            functor_desc->MR_du_functor_arg_types[i],
                            x_data_value, functor_desc);
                    } else {
                        arg_type_info = (MR_TypeInfo)
                            functor_desc->MR_du_functor_arg_types[i];
                    }
#ifdef  select_compare_code
                    result = MR_generic_compare(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
                    if (result != MR_COMPARE_EQUAL) {
                        return_answer(result);
                    }
#else
                    result = MR_generic_unify(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
                    if (! result) {
                        return_answer(FALSE);
                    }
#endif
                    cur_slot++;
                }

#ifdef  select_compare_code
                return_answer(MR_COMPARE_EQUAL);
#else
                return_answer(TRUE);
#endif
            }

            break;

#else

        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_EQUIV_VAR:
        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_DU:
            /* fall through */

#endif

        case MR_TYPECTOR_REP_ENUM_USEREQ:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_ARRAY:

            /*
            ** We call the type-specific compare routine as
            ** `CompPred(...ArgTypeInfos..., Result, X, Y)' is det.
            ** The ArgTypeInfo arguments are input, and are passed
            ** in r1, r2, ... rN. The X and Y arguments are also
            ** input, and are passed in rN+1 and rN+2.
            ** The Result argument is output in r1.
            **
            ** We specialize the case where the type_ctor arity
            ** is zero, since in this case we don't need the loop.
            ** We could also specialize other arities; 1 and 2
            ** may be worthwhile.
            */

            if (type_ctor_info->arity == 0) {
                r1 = x;
                r2 = y;
            }
#ifdef  MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_1
            else if (type_ctor_info->arity == 1) {
                Word    *args_base;

                args_base = (Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                r1 = args_base[1];
                r2 = x;
                r3 = y;
            }
#endif
#ifdef  MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_2
            else if (type_ctor_info->arity == 2) {
                Word    *args_base;

                args_base = (Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                r1 = args_base[1];
                r2 = args_base[2];
                r3 = x;
                r4 = y;
            }
#endif
            else {
                int     i;
                int     type_arity;
                Word    *args_base;

                type_arity = type_ctor_info->arity;
                args_base = (Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                save_registers();

                /* CompPred(...ArgTypeInfos..., Res, X, Y) * */
                for (i = 1; i <= type_arity; i++) {
                    virtual_reg(i) = args_base[i];
                }
                virtual_reg(type_arity + 1) = x;
                virtual_reg(type_arity + 2) = y;

                restore_registers();
            }

            tailcall_user_pred();

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_CHAR:
#ifdef  select_compare_code
            if ((Integer) x == (Integer) y) {
                return_answer(MR_COMPARE_EQUAL);
            } else if ((Integer) x < (Integer) y) {
                return_answer(MR_COMPARE_LESS);
            } else {
                return_answer(MR_COMPARE_GREATER);
            }
#else
            return_answer((Integer) x == (Integer) y);
#endif

        case MR_TYPECTOR_REP_FLOAT:
            {
                Float   fx, fy;

                fx = word_to_float(x);
                fy = word_to_float(y);
#ifdef  select_compare_code
                if (fx == fy) {
                    return_answer(MR_COMPARE_EQUAL);
                } else if (fx < fy) {
                    return_answer(MR_COMPARE_LESS);
                } else {
                    return_answer(MR_COMPARE_GREATER);
                }
#else
                return_answer(fx == fy);
#endif
            }

        case MR_TYPECTOR_REP_STRING:
            {
                int result;

                result = strcmp((char *) x, (char *) y);

#ifdef  select_compare_code
                if (result == 0) {
                    return_answer(MR_COMPARE_EQUAL);
                } else if (result < 0) {
                    return_answer(MR_COMPARE_LESS);
                } else {
                    return_answer(MR_COMPARE_GREATER);
                }
#else
                return_answer(result == 0);
#endif
            }

        case MR_TYPECTOR_REP_UNIV:
            {
                MR_TypeInfo type_info_x, type_info_y;
                int         result;

                /* First compare the type_infos */
                type_info_x = (MR_TypeInfo) MR_field(MR_mktag(0), x,
                        UNIV_OFFSET_FOR_TYPEINFO);
                type_info_y = (MR_TypeInfo) MR_field(MR_mktag(0), y,
                        UNIV_OFFSET_FOR_TYPEINFO);
                save_transient_registers();
                result = MR_compare_type_info(type_info_x, type_info_y);
                restore_transient_registers();
                if (result != MR_COMPARE_EQUAL) {
#ifdef  select_compare_code
                    return_answer(result);
#else
                    return_answer(FALSE);
#endif
                }

                /*
                ** If the types are the same, then recurse on
                ** the unwrapped args.
                */

                type_info = type_info_x;
                x = MR_field(MR_mktag(0), x, UNIV_OFFSET_FOR_DATA);
                y = MR_field(MR_mktag(0), y, UNIV_OFFSET_FOR_DATA);
                goto start_label;
            }

        case MR_TYPECTOR_REP_C_POINTER:
#ifdef	select_compare_code
            if ((void *) x == (void *) y) {
                return_answer(MR_COMPARE_EQUAL);
            } else if ((void *) x < (void *) y) {
                return_answer(MR_COMPARE_LESS);
            } else {
                return_answer(MR_COMPARE_GREATER);
            }
#else
	        return_answer((void *) x == (void *) y);
#endif

        case MR_TYPECTOR_REP_TYPEINFO:
            {
                int result;

                save_transient_registers();
                result = MR_compare_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                restore_transient_registers();
#ifdef	select_compare_code
                return_answer(result);
#else
                return_answer(result == MR_COMPARE_EQUAL);
#endif
            }

        case MR_TYPECTOR_REP_VOID:
            fatal_error(attempt_msg "terms of type `void'");

        case MR_TYPECTOR_REP_PRED:
            fatal_error(attempt_msg "higher-order terms");

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            fatal_error(attempt_msg "typeclass_infos");

        case MR_TYPECTOR_REP_UNKNOWN:
            fatal_error(attempt_msg "terms of unknown type");

        default:
            fatal_error(attempt_msg "terms of unknown representation");
    }
