/*
** Copyright (C) 2000-2002 The University of Melbourne.
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
** symbol, and doing it with a loop body that calls a C function is
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

    switch (MR_type_ctor_rep(type_ctor_info)) {

#ifdef  MR_COMPARE_BY_RTTI

        case MR_TYPECTOR_REP_EQUIV:
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).layout_equiv);
            MR_restore_transient_hp();
            goto start_label;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            type_info = (MR_TypeInfo) MR_type_ctor_layout(type_ctor_info).layout_equiv;
            goto start_label;

        case MR_TYPECTOR_REP_EQUIV_VAR:
            MR_fatal_error("found type_ctor_rep MR_TYPECTOR_REP_EQUIV_VAR");

        case MR_TYPECTOR_REP_NOTAG:
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).layout_notag->
                MR_notag_functor_arg_type);
            MR_restore_transient_hp();
            goto start_label;

        case MR_TYPECTOR_REP_NOTAG_GROUND:
            type_info = (MR_TypeInfo) MR_type_ctor_layout(type_ctor_info).
                layout_notag->MR_notag_functor_arg_type;
            goto start_label;

        case MR_TYPECTOR_REP_RESERVED_ADDR:
            MR_fatal_error("sorry, not implemented: "
		    	"MR_COMPARE_BY_RTTI for RESERVED_ADDR");

        case MR_TYPECTOR_REP_DU:
            {
                const MR_DuFunctorDesc  *functor_desc;
  #ifdef  select_compare_code
                const MR_DuFunctorDesc  *x_functor_desc;
                const MR_DuFunctorDesc  *y_functor_desc;
                MR_DuPtagLayout         *x_ptaglayout;
                MR_DuPtagLayout         *y_ptaglayout;
  #else
                MR_Word                 x_ptag;
                MR_Word                 y_ptag;
                MR_Word                 x_sectag;
                MR_Word                 y_sectag;
                MR_DuPtagLayout         *ptaglayout;
  #endif
                MR_Word                 *x_data_value;
                MR_Word                 *y_data_value;
                const MR_DuExistInfo    *exist_info;
                int                     result;
                int                     cur_slot;
                int                     arity;
                int                     i;

  #ifdef  select_compare_code

  #define MR_find_du_functor_desc(data, data_value, functor_desc)             \
                do {                                                          \
                    MR_DuPtagLayout         *ptaglayout;                      \
                    int                     ptag;                             \
                    int                     sectag;                           \
                                                                              \
                    ptag = MR_tag(data);                                      \
                    ptaglayout = &MR_type_ctor_layout(type_ctor_info).layout_du[ptag];\
                    data_value = (MR_Word *) MR_body(data, ptag);             \
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
                        case MR_SECTAG_VARIABLE:                              \
                            MR_fatal_error("find_du_functor_desc(): "         \
                                "attempt get functor desc of variable");      \
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
  #else /* ! select_compare_code */
                x_ptag = MR_tag(x);
                y_ptag = MR_tag(y);

                if (x_ptag != y_ptag) {
                    return_answer(MR_FALSE);
                }

                ptaglayout = &MR_type_ctor_layout(type_ctor_info).layout_du[x_ptag];
                x_data_value = (MR_Word *) MR_body(x, x_ptag);
                y_data_value = (MR_Word *) MR_body(y, y_ptag);

                switch (ptaglayout->MR_sectag_locn) {
                    case MR_SECTAG_LOCAL:
                        x_sectag = MR_unmkbody(x_data_value);
                        y_sectag = MR_unmkbody(y_data_value);

                        if (x_sectag != y_sectag) {
                            return_answer(MR_FALSE);
                        }

                        break;

                    case MR_SECTAG_REMOTE:
                        x_sectag = x_data_value[0];
                        y_sectag = y_data_value[0];

                        if (x_sectag != y_sectag) {
                            return_answer(MR_FALSE);
                        }

                        break;

                    case MR_SECTAG_NONE:
                        x_sectag = 0;
                        break;

                    case MR_SECTAG_VARIABLE:
                        MR_fatal_error("find_du_functor_desc(): attempt get functor desc of variable");                                                   \
                }

                functor_desc = ptaglayout->MR_sectag_alternatives[x_sectag];
  #endif /* select_compare_code */

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
			MR_save_transient_registers();
                        result = MR_compare_type_info(x_ti, y_ti);
			MR_restore_transient_registers();
                        if (result != MR_COMPARE_EQUAL) {
  #ifdef  select_compare_code
                            return_answer(result);
  #else
                            return_answer(MR_FALSE);
  #endif
                        }
                    }

                    cur_slot += num_ti_plain + num_tci;
                }

                for (i = 0; i < arity; i++) {
                    MR_TypeInfo arg_type_info;

                    if (MR_arg_type_may_contain_var(functor_desc, i)) {
		        MR_save_transient_hp();
                        arg_type_info = MR_create_type_info_maybe_existq(
                            MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                            functor_desc->MR_du_functor_arg_types[i],
                            x_data_value, functor_desc);
		        MR_restore_transient_hp();
                    } else {
                        arg_type_info = (MR_TypeInfo)
                            functor_desc->MR_du_functor_arg_types[i];
                    }
  #ifdef  select_compare_code
		    MR_save_transient_registers();
                    result = MR_generic_compare(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
		    MR_restore_transient_registers();
                    if (result != MR_COMPARE_EQUAL) {
                        return_answer(result);
                    }
  #else
		    MR_save_transient_registers();
                    result = MR_generic_unify(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
		    MR_restore_transient_registers();
                    if (! result) {
                        return_answer(MR_FALSE);
                    }
  #endif
                    cur_slot++;
                }

  #ifdef  select_compare_code
                return_answer(MR_COMPARE_EQUAL);
  #else
                return_answer(MR_TRUE);
  #endif
            }

            break;

#else /* ! MR_COMPARE_BY_RTTI */

        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_DU:
            /* fall through */

#endif

        case MR_TYPECTOR_REP_ENUM_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_ARRAY:

            /*
            ** We call the type-specific compare routine as
            ** `CompPred(...ArgTypeInfos..., Result, X, Y)' is det.
            ** The ArgTypeInfo arguments are input, and are passed
            ** in MR_r1, MR_r2, ... MR_rN. The X and Y arguments are also
            ** input, and are passed in MR_rN+1 and MR_rN+2.
            ** The Result argument is output in MR_r1.
            **
            ** We specialize the case where the type_ctor arity
            ** is zero, since in this case we don't need the loop.
            ** We could also specialize other arities; 1 and 2
            ** may be worthwhile.
            */

            if (type_ctor_info->MR_type_ctor_arity == 0) {
                MR_r1 = x;
                MR_r2 = y;
            }
#ifdef  MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_1
            else if (type_ctor_info->MR_type_ctor_arity == 1) {
                MR_Word    *args_base;

                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                MR_r1 = args_base[1];
                MR_r2 = x;
                MR_r3 = y;
            }
#endif
#ifdef  MR_UNIFY_COMPARE_BY_CTOR_REP_SPEC_2
            else if (type_ctor_info->MR_type_ctor_arity == 2) {
                MR_Word    *args_base;

                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                MR_r1 = args_base[1];
                MR_r2 = args_base[2];
                MR_r3 = x;
                MR_r4 = y;
            }
#endif
            else {
                int     i;
                int     type_arity;
                MR_Word *args_base;

                type_arity = type_ctor_info->MR_type_ctor_arity;
                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
                MR_save_registers();

                /* CompPred(...ArgTypeInfos..., Res, X, Y) * */
                for (i = 1; i <= type_arity; i++) {
                    MR_virtual_reg(i) = args_base[i];
                }
                MR_virtual_reg(type_arity + 1) = x;
                MR_virtual_reg(type_arity + 2) = y;

                MR_restore_registers();
            }

            tailcall_user_pred();

        case MR_TYPECTOR_REP_TUPLE:
            {
                int     i;
                int     type_arity;
                int     result;

                type_arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);

                for (i = 0; i < type_arity; i++) {
                    MR_TypeInfo arg_type_info;

                    /* type_infos are counted from one */
                    arg_type_info = MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(
                                            type_info)[i + 1];

#ifdef  select_compare_code
		    MR_save_transient_registers();
                    result = MR_generic_compare(arg_type_info,
                                ((MR_Word *) x)[i], ((MR_Word *) y)[i]);
		    MR_restore_transient_registers();
                    if (result != MR_COMPARE_EQUAL) {
                        return_answer(result);
                    }
#else
		    MR_save_transient_registers();
                    result = MR_generic_unify(arg_type_info,
                                ((MR_Word *) x)[i], ((MR_Word *) y)[i]);
		    MR_restore_transient_registers();
                    if (! result) {
                        return_answer(MR_FALSE);
                    }
#endif
                }
#ifdef  select_compare_code
                return_answer(MR_COMPARE_EQUAL);
#else
                return_answer(MR_TRUE);
#endif
            }

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_CHAR:

#ifdef  select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
            compare_call_exit_code(integer_compare);
  #endif
            if ((MR_Integer) x == (MR_Integer) y) {
                return_answer(MR_COMPARE_EQUAL);
            } else if ((MR_Integer) x < (MR_Integer) y) {
                return_answer(MR_COMPARE_LESS);
            } else {
                return_answer(MR_COMPARE_GREATER);
            }
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
            if ((MR_Integer) x == (MR_Integer) y) {
                unify_call_exit_code(integer_unify);
                return_answer(MR_TRUE);
            } else {
                unify_call_fail_code(integer_unify);
                return_answer(MR_FALSE);
            }
  #else
            return_answer((MR_Integer) x == (MR_Integer) y);
  #endif
#endif

        case MR_TYPECTOR_REP_FLOAT:
            {
                MR_Float   fx, fy;

                fx = MR_word_to_float(x);
                fy = MR_word_to_float(y);
#ifdef  select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                compare_call_exit_code(float_compare);
  #endif
                if (fx == fy) {
                    return_answer(MR_COMPARE_EQUAL);
                } else if (fx < fy) {
                    return_answer(MR_COMPARE_LESS);
                } else {
                    return_answer(MR_COMPARE_GREATER);
                }
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                if (fx == fy) {
                    unify_call_exit_code(float_unify);
                    return_answer(MR_TRUE);
                } else {
                    unify_call_fail_code(float_unify);
                    return_answer(MR_FALSE);
                }
  #else
                return_answer(fx == fy);
  #endif
#endif
            }

        case MR_TYPECTOR_REP_STRING:
            {
                int result;

                result = strcmp((char *) x, (char *) y);

#ifdef  select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                compare_call_exit_code(string_compare);
  #endif
                if (result == 0) {
                    return_answer(MR_COMPARE_EQUAL);
                } else if (result < 0) {
                    return_answer(MR_COMPARE_LESS);
                } else {
                    return_answer(MR_COMPARE_GREATER);
                }
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                if (result == 0) {
                    unify_call_exit_code(string_unify);
                    return_answer(MR_TRUE);
                } else {
                    unify_call_fail_code(string_unify);
                    return_answer(MR_FALSE);
                }
  #else
                return_answer(result == 0);
  #endif
#endif
            }

        case MR_TYPECTOR_REP_C_POINTER:
#ifdef	select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
            compare_call_exit_code(c_pointer_compare);
  #endif
            if ((void *) x == (void *) y) {
                return_answer(MR_COMPARE_EQUAL);
            } else if ((void *) x < (void *) y) {
                return_answer(MR_COMPARE_LESS);
            } else {
                return_answer(MR_COMPARE_GREATER);
            }
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
            if ((void *) x == (void *) y) {
                unify_call_exit_code(c_pointer_unify);
                return_answer(MR_TRUE);
            } else {
                unify_call_fail_code(c_pointer_unify);
                return_answer(MR_FALSE);
            }
  #else
            return_answer((void *) x == (void *) y);
  #endif
#endif

        case MR_TYPECTOR_REP_TYPEINFO:
            {
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                MR_restore_transient_registers();
#ifdef	select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                compare_call_exit_code(typeinfo_compare);
  #endif
                return_answer(result);
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                if (result == MR_COMPARE_EQUAL) {
                    unify_call_exit_code(typeinfo_unify);
                    return_answer(MR_TRUE);
                } else {
                    unify_call_fail_code(typeinfo_unify);
                    return_answer(MR_FALSE);
                }
  #else
                return_answer(result == MR_COMPARE_EQUAL);
  #endif
#endif
            }

        case MR_TYPECTOR_REP_TYPECTORINFO:
            {
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_ctor_info(
                    (MR_TypeCtorInfo) x, (MR_TypeCtorInfo) y);
                MR_restore_transient_registers();
#ifdef	select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                compare_call_exit_code(typectorinfo_compare);
  #endif
                return_answer(result);
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
                if (result == MR_COMPARE_EQUAL) {
                    unify_call_exit_code(typectorinfo_unify);
                    return_answer(MR_TRUE);
                } else {
                    unify_call_fail_code(typectorinfo_unify);
                    return_answer(MR_FALSE);
                }
  #else
                return_answer(result == MR_COMPARE_EQUAL);
  #endif
#endif
            }

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error(attempt_msg "terms of type `void'");

        case MR_TYPECTOR_REP_FUNC:
        case MR_TYPECTOR_REP_PRED:
            MR_fatal_error(attempt_msg "higher-order terms");

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            MR_fatal_error(attempt_msg "typeclass_infos");

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            MR_fatal_error(attempt_msg "base_typeclass_infos");

        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error(attempt_msg "terms of unknown type");

        default:
            MR_fatal_error(attempt_msg "terms of unknown representation");
    }
