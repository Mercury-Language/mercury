// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2000-2005, 2007, 2011 The University of Melbourne.
// Copyright (C) 2014-2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains a piece of code that is included by mercury_ho_call.c
// six times:
//
// - as the body of the mercury__builtin__unify_2_0 Mercury procedure,
// - as the body of the mercury__builtin__compare_3_3 Mercury procedure,
// - as the body of the mercury__builtin__compare_representation_3_0
//   Mercury procedure,
// - as the body of the MR_generic_unify C function,
// - as the body of the MR_generic_compare C function, and
// - as the body of the MR_generic_compare_representation C function.
//
// The inclusions are surrounded by #defines and #undefs of the macros
// that personalize each copy of the code.
//
// The reason why the unify and compare Mercury procedures share code is
// that unify is mostly just a special case of comparison; it differs only
// by treating "less than" and "greater than" the same way, and returning
// its result slightly differently. Likewise, compare_representation
// is mostly the same as compare.
//
// The reason why there is both a Mercury procedure and a C function for
// unifications and comparisons is that the Mercury procedure needs a
// mechanism that allows it to unify or compare each argument of a function
// symbol, and doing it with a loop body that calls a C function is
// significantly easier to program, and probably more efficient, than
// using recursion in Mercury. The Mercury procedure and C function share code
// because they implement the same task.
//
// XXX does the rationale still hold? Only rarely used code paths still have
// loop bodies in C and they are likely incorrect for deep profiling.
// Also, the Mercury implementation of tuple unify/compare predicates is
// faster than the hand-written version was in asm_fast.gc. --pw
//
// We need separate C functions for unifications and comparison because
// with --no-special-preds, a type with user-defined equality (but not
// comparison) has a non-NULL unify_pred field in its type_ctor_info but a
// NULL compare_pred field. While in principle unification is a special case
// of comparison, we cannot implement unifications by comparisons for such
// types: they support unifications but not comparisons. Since we cannot do
// it for such types, it is simplest not to do it for any types.

#ifdef  select_compare_code
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
    #ifdef include_compare_rep_code
      #define return_compare_answer(mod, type, arity, answer)               \
        do {                                                                \
            compare_call_exit_code(mod, __CompareRep__, type, arity);       \
            raw_return_answer(answer);                                      \
        } while (0)
    #else
      #define return_compare_answer(mod, type, arity, answer)               \
        do {                                                                \
            compare_call_exit_code(mod, __Compare__, type, arity);          \
            raw_return_answer(answer);                                      \
        } while (0)
    #endif
  #else
    #define return_compare_answer(mod, type, arity, answer)                 \
        raw_return_answer(answer)
  #endif
#else
  #if defined(MR_DEEP_PROFILING) && defined(entry_point_is_mercury)
    #define return_unify_answer(mod, type, arity, answer)                   \
        do {                                                                \
            if (answer) {                                                   \
                unify_call_exit_code(mod, __Unify__, type, arity);          \
                raw_return_answer(MR_TRUE);                                 \
            } else {                                                        \
                unify_call_fail_code(mod, __Unify__, type, arity);          \
                raw_return_answer(MR_FALSE);                                \
            }                                                               \
        } while (0)
  #else
    #define return_unify_answer(mod, type, arity, answer)                   \
        raw_return_answer(answer)
  #endif
#endif

    DECLARE_LOCALS
    initialize();

start_label:
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

#ifdef  MR_TYPE_CTOR_STATS
    MR_register_type_ctor_stat(&type_stat_struct, type_ctor_info);
#endif

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error(attempt_msg "terms of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {

#if defined(MR_COMPARE_BY_RTTI) || defined(include_compare_rep_code)

        case MR_TYPECTOR_REP_EQUIV:
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
            MR_restore_transient_hp();
            goto start_label;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            type_info = (MR_TypeInfo)
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv;
            goto start_label;

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            // fall through
  #endif
        case MR_TYPECTOR_REP_NOTAG:
            MR_save_transient_hp();
            type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                MR_notag_functor_arg_type);
            MR_restore_transient_hp();
            goto start_label;

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            // fall through
  #endif
        case MR_TYPECTOR_REP_NOTAG_GROUND:
            type_info = (MR_TypeInfo) MR_type_ctor_layout(type_ctor_info).
                MR_layout_notag->MR_notag_functor_arg_type;
            goto start_label;

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_ARRAY:
            MR_fatal_error("sorry, not implemented: "
                "compare_representation for arrays");
  #endif

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_BITMAP:
            MR_fatal_error("sorry, not implemented: "
                "compare_representation for bitmaps");
  #endif

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_FOREIGN:
            MR_fatal_error("sorry, not implemented: "
                "compare_representation for foreign types");
  #endif

  #ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_DU_USEREQ:
            // fall through
  #endif
        case MR_TYPECTOR_REP_DU:
            // When deep profiling is enabled, we use the call, exit and (for
            // unifications) fail ports of dummy unify, compare and compare_rep
            // predicates for the dummy type_ctor builtin.user_by_rtti/0.
            //
            // XXX The deep profiler invariants are likely broken in the loop.

            {
                const MR_DuFunctorDesc  *functor_desc;
  #ifdef  select_compare_code
                const MR_DuFunctorDesc  *x_functor_desc;
                const MR_DuFunctorDesc  *y_functor_desc;
                const MR_DuPtagLayout   *x_ptag_layout;
                const MR_DuPtagLayout   *y_ptag_layout;
  #else
                MR_Word                 x_ptag;
                MR_Word                 y_ptag;
                MR_Word                 x_sectag;
                MR_Word                 y_sectag;
                const MR_DuPtagLayout   *ptag_layout;
  #endif
                MR_Word                 *x_data_value;
                MR_Word                 *y_data_value;
                const MR_DuExistInfo    *exist_info;
                int                     result;
                int                     cur_slot;
                int                     arity;
                int                     i;

  #ifdef MR_CHECK_DU_EQ
    #ifdef  select_compare_code
                if (x == y) {
                    return_compare_answer(builtin, user_by_rtti, 0,
                        MR_COMPARE_EQUAL);
                }
    #else
                if (x == y) {
                    return_unify_answer(builtin, user_by_rtti, 0, MR_TRUE);
                }
    #endif
  #endif

  #ifdef  select_compare_code

  #define MR_find_du_functor_desc(data, data_value, functor_desc)             \
                do {                                                          \
                    const MR_DuPtagLayout   *ptag_layout;                     \
                    int                     ptag;                             \
                    int                     sectag;                           \
                                                                              \
                    ptag = MR_tag(data);                                      \
                    MR_index_or_search_ptag_layout(ptag, ptag_layout);        \
                    data_value = (MR_Word *) MR_body(data, ptag);             \
                                                                              \
                    switch (ptag_layout->MR_sectag_locn) {                    \
                        case MR_SECTAG_LOCAL_REST_OF_WORD:                    \
                            sectag = MR_unmkbody(data_value);                 \
                            break;                                            \
                        case MR_SECTAG_LOCAL_BITS:                            \
                            sectag = MR_unmkbody(data_value) &                \
                                ((1 << ptag_layout->MR_sectag_numbits) - 1);  \
                            break;                                            \
                        case MR_SECTAG_REMOTE_FULL_WORD:                      \
                            sectag = data_value[0];                           \
                            break;                                            \
                        case MR_SECTAG_REMOTE_BITS:                           \
                            sectag = data_value[0] &                          \
                                ((1 << ptag_layout->MR_sectag_numbits) - 1);  \
                            break;                                            \
                        case MR_SECTAG_NONE:             /* fall-though */    \
                        case MR_SECTAG_NONE_DIRECT_ARG:                       \
                            sectag = 0;                                       \
                            break;                                            \
                        case MR_SECTAG_VARIABLE:                              \
                            sectag = 0;     /* Avoid a warning. */            \
                            MR_fatal_error("find_du_functor_desc(): "         \
                                "attempt get functor desc of variable");      \
                        default:                                              \
                            sectag = 0;     /* Avoid a warning. */            \
                            MR_fatal_error("find_du_functor_desc(): "         \
                                "unrecognised sectag locn");                  \
                    }                                                         \
                                                                              \
                    MR_index_or_search_sectag_functor(ptag_layout, sectag,    \
                        functor_desc);                                        \
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
                        return_compare_answer(builtin, user_by_rtti, 0,
                            MR_COMPARE_LESS);
                    } else {
                        return_compare_answer(builtin, user_by_rtti, 0,
                            MR_COMPARE_GREATER);
                    }
                }

                functor_desc = x_functor_desc;
  #else // ! select_compare_code
                x_ptag = MR_tag(x);
                y_ptag = MR_tag(y);

                if (x_ptag != y_ptag) {
                    return_unify_answer(builtin, user_by_rtti, 0, MR_FALSE);
                }

                MR_index_or_search_ptag_layout(x_ptag, ptag_layout);
                x_data_value = (MR_Word *) MR_body(x, x_ptag);
                y_data_value = (MR_Word *) MR_body(y, y_ptag);

                switch (ptag_layout->MR_sectag_locn) {
                    case MR_SECTAG_LOCAL_REST_OF_WORD:
                        x_sectag = MR_unmkbody(x_data_value);
                        y_sectag = MR_unmkbody(y_data_value);

                        if (x_sectag == y_sectag) {
                            return_unify_answer(builtin, user_by_rtti, 0,
                                MR_TRUE);
                        } else {
                            return_unify_answer(builtin, user_by_rtti, 0,
                                MR_FALSE);
                        }

                        break;

                    case MR_SECTAG_LOCAL_BITS:
                        {
                            MR_Word x_sectag_word;
                            MR_Word y_sectag_word;

                            x_sectag_word = MR_unmkbody(x_data_value);
                            y_sectag_word = MR_unmkbody(y_data_value);

                            if (x_sectag_word != y_sectag_word) {
                                return_unify_answer(builtin, user_by_rtti, 0,
                                    MR_FALSE);
                            }

                            x_sectag = x_sectag_word &
                                ((1 << ptag_layout->MR_sectag_numbits) - 1);
                        }
                        break;

                    case MR_SECTAG_REMOTE_FULL_WORD:
                        x_sectag = x_data_value[0];
                        y_sectag = y_data_value[0];

                        if (x_sectag != y_sectag) {
                            return_unify_answer(builtin, user_by_rtti, 0,
                                MR_FALSE);
                        }

                        break;

                    case MR_SECTAG_REMOTE_BITS:
                        x_sectag = x_data_value[0] &
                            ((1 << ptag_layout->MR_sectag_numbits) - 1);
                        y_sectag = y_data_value[0] &
                            ((1 << ptag_layout->MR_sectag_numbits) - 1);

                        if (x_sectag != y_sectag) {
                            return_unify_answer(builtin, user_by_rtti, 0,
                                MR_FALSE);
                        }

                        break;

                    case MR_SECTAG_NONE:
                    case MR_SECTAG_NONE_DIRECT_ARG:
                        x_sectag = 0;
                        break;

                    case MR_SECTAG_VARIABLE:
                        MR_fatal_error("find_du_functor_desc():"
                            "attempt get functor desc of variable");
                }

                MR_index_or_search_sectag_functor(ptag_layout, x_sectag,
                    functor_desc);
  #endif // select_compare_code

                switch (functor_desc->MR_du_functor_sectag_locn) {

                case MR_SECTAG_NONE_DIRECT_ARG:
                    // The work is done in the switch.
                    {
                        MR_TypeInfo arg_type_info;

                        arg_type_info = (MR_TypeInfo)
                            functor_desc->MR_du_functor_arg_types[0];
                        MR_save_transient_registers();
      #ifdef  select_compare_code
        #ifdef include_compare_rep_code
                        result = MR_generic_compare_representation(
                            arg_type_info,
                            (MR_Word) x_data_value, (MR_Word) y_data_value);
        #else
                        result = MR_generic_compare(arg_type_info,
                            (MR_Word) x_data_value, (MR_Word) y_data_value);
        #endif
      #else
                        result = MR_generic_unify(arg_type_info,
                            (MR_Word) x_data_value, (MR_Word) y_data_value);
      #endif
                        MR_restore_transient_registers();
                    }

      #ifdef  select_compare_code
                    return_compare_answer(builtin, user_by_rtti, 0, result);
      #else
                    return_unify_answer(builtin, user_by_rtti, 0, result);
      #endif
                    break;

                case MR_SECTAG_REMOTE_FULL_WORD:
                    cur_slot = 1;
                    // The work is done after the switch.
                    break;

                case MR_SECTAG_NONE:
                    cur_slot = 0;
                    // The work is done after the switch.
                    break;

                case MR_SECTAG_REMOTE_BITS:     // fall through
                case MR_SECTAG_LOCAL_BITS:
                    MR_fatal_error("packed with sectag in du switch NYI");
                    break;

                case MR_SECTAG_LOCAL_REST_OF_WORD:
                    // This case should have been handled in full above.
                    MR_fatal_error("MR_SECTAG_LOCAL_REST_OF_WORD in du switch");

                default:
                    MR_fatal_error("bad sectag location in du switch");
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
                            x_ti = (MR_TypeInfo)
                                MR_typeclass_info_param_type_info(
                                    x_data_value[locns[i].MR_exist_arg_num],
                                    locns[i].MR_exist_offset_in_tci);
                            y_ti = (MR_TypeInfo)
                                MR_typeclass_info_param_type_info(
                                    y_data_value[locns[i].MR_exist_arg_num],
                                    locns[i].MR_exist_offset_in_tci);
                        }
                        MR_save_transient_registers();
                        result = MR_compare_type_info(x_ti, y_ti);
                        MR_restore_transient_registers();
                        if (result != MR_COMPARE_EQUAL) {
  #ifdef  select_compare_code
                            return_compare_answer(builtin, user_by_rtti, 0,
                                result);
  #else
                            return_unify_answer(builtin, user_by_rtti, 0,
                                MR_FALSE);
  #endif
                        }
                    }

                    cur_slot += num_ti_plain + num_tci;
                }

                for (i = 0; i < arity; i++) {
                    MR_TypeInfo arg_type_info;

                    // XXX This code is like the expansion of
                    // MR_get_arg_type_info(type_info, functor_desc,
                    //     x_data_value, i);
                    // but with save/restore of transient hp.
                    // Either that macro should have the same save/restore,
                    // or this code does not need it.
                    if (MR_arg_type_may_contain_var(functor_desc, i)) {
                        MR_save_transient_hp();
                        arg_type_info = MR_create_type_info_maybe_existq(
                            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                            functor_desc->MR_du_functor_arg_types[i],
                            x_data_value, functor_desc);
                        MR_restore_transient_hp();
                    } else {
                        arg_type_info = (MR_TypeInfo)
                            functor_desc->MR_du_functor_arg_types[i];
                    }
  #ifdef  select_compare_code
                    MR_save_transient_registers();
                    // XXX The code below is wrong for packed args.
    #ifdef include_compare_rep_code
                    result = MR_generic_compare_representation(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
    #else
                    result = MR_generic_compare(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
    #endif
                    MR_restore_transient_registers();
                    if (result != MR_COMPARE_EQUAL) {
                        return_compare_answer(builtin, user_by_rtti, 0,
                            result);
                    }
  #else
                    MR_save_transient_registers();
                    result = MR_generic_unify(arg_type_info,
                        x_data_value[cur_slot], y_data_value[cur_slot]);
                    MR_restore_transient_registers();
                    if (! result) {
                        return_unify_answer(builtin, user_by_rtti, 0,
                            MR_FALSE);
                    }
  #endif
                    cur_slot++;
                }

  #ifdef  select_compare_code
                return_compare_answer(builtin, user_by_rtti, 0,
                    MR_COMPARE_EQUAL);
  #else
                return_unify_answer(builtin, user_by_rtti, 0, MR_TRUE);
  #endif
            }

            MR_fatal_error(MR_STRINGIFY(start_label) ": unexpected fall thru");

#endif  // defined(MR_COMPARE_BY_RTTI) || defined(include_compare_rep_code)

#ifndef include_compare_rep_code
  #ifndef MR_COMPARE_BY_RTTI
        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_DU:
            // fall through
  #endif

        case MR_TYPECTOR_REP_ENUM_USEREQ:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_ARRAY:
        case MR_TYPECTOR_REP_BITMAP:
        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:

            // In deep profiling grades, the caller of builtin.unify or
            // builtin.compare (the predicates this piece of code implements)
            // has prepared for a normal call, which must be followed by
            // the execution of the call port code and then of either the exit
            // or the fail port code.
            //
            // That would be a problem if you wanted to pretest x == y here.
            // First, at the moment there is no simple or fast way
            // to get from the type_ctor_info (which we have) to the proc
            // layout structures of the type's unify or compare predicates
            // (which port codes need). Second, even if we put the addresses
            // of those proc layout structures into the type_ctor_info,
            // incrementing just the call and exit/fail port counts would
            // leave the called predicate's counts inconsistent in cases where
            // the body of that predicate did not have any paths through it
            // without making calls (since those calls are being
            // short-circuited here).
            //
            // Our solution is to check x == y not here, but at the starts of
            // compiler-generated unify and compare predicates. This delays
            // the check until after the extraction of any typeinfos
            // representing the arguments of the main type constructor,
            // but it also ensures that the check is performed not just on the
            // top function symbols of the terms being unified or compared,
            // but on all other function symbols too. Measurements show that
            // the overhead of these extra tests is less than the amount of
            // work that these extra pretests avoid.

#ifndef MR_DEEP_PROFILING
  #if 0
    #ifdef  select_compare_code
            if (x == y) {
                raw_return_answer(MR_COMPARE_EQUAL);
            }
    #else
            if (x == y) {
                raw_return_answer(MR_TRUE);
            }
    #endif
  #endif
#endif

            // We call the type-specific compare routine as
            // `CompPred(...ArgTypeInfos..., Result, X, Y)' is det.
            // The ArgTypeInfo arguments are input, and are passed
            // in MR_r1, MR_r2, ... MR_rN. The X and Y arguments are also
            // input, and are passed in MR_rN+1 and MR_rN+2.
            // The Result argument is output in MR_r1.
            //
            // We specialize the case where the type_ctor arity is 0, 1 or 2,
            // in order to avoid the loop. If type_ctors with higher arities
            // were commonly used, we could specialize them too.

            if (type_ctor_info->MR_type_ctor_arity == 0) {
                MR_r1 = x;
                MR_r2 = y;
            } else if (type_ctor_info->MR_type_ctor_arity == 1) {
                MR_Word    *args_base;

                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
                MR_r1 = args_base[1];
                MR_r2 = x;
                MR_r3 = y;
            } else if (type_ctor_info->MR_type_ctor_arity == 2) {
                MR_Word    *args_base;

                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
                MR_r1 = args_base[1];
                MR_r2 = args_base[2];
                MR_r3 = x;
                MR_r4 = y;
            } else {
                int     i;
                int     type_arity;
                MR_Word *args_base;

                type_arity = type_ctor_info->MR_type_ctor_arity;
                args_base = (MR_Word *)
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
                MR_save_registers();

                // CompPred(...ArgTypeInfos..., Res, X, Y)
                for (i = 1; i <= type_arity; i++) {
                    MR_virtual_reg_assign(i, args_base[i]);
                }
                MR_virtual_reg_assign(type_arity + 1, x);
                MR_virtual_reg_assign(type_arity + 2, y);

                MR_restore_registers();
            }

            tailcall_tci_pred();
#endif  // !include_compare_rep_code

        case MR_TYPECTOR_REP_TUPLE:
            // The tuple unify and compare predicates are implemented in
            // Mercury, mainly so that the compiler can perform the deep
            // profiler transformation on them.

#ifdef select_compare_code
    #ifdef include_compare_rep_code
            if (MR_special_pred_hooks.MR_compare_rep_tuple_pred != NULL) {
                tailcall(MR_special_pred_hooks.MR_compare_rep_tuple_pred);
            } else {
                MR_fatal_error(attempt_msg "tuples");
            }
    #else
            if (MR_special_pred_hooks.MR_compare_tuple_pred != NULL) {
                tailcall(MR_special_pred_hooks.MR_compare_tuple_pred);
            } else {
                tailcall_tci_pred();
            }
    #endif
#else
            if (MR_special_pred_hooks.MR_unify_tuple_pred != NULL) {
                tailcall(MR_special_pred_hooks.MR_unify_tuple_pred);
            } else {
                tailcall_tci_pred();
            }
#endif

#ifdef  include_compare_rep_code
        case MR_TYPECTOR_REP_ENUM_USEREQ:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            // fall through
#endif
        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_FOREIGN_ENUM:
        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_INT8:
        case MR_TYPECTOR_REP_INT16:
        case MR_TYPECTOR_REP_INT32:
        case MR_TYPECTOR_REP_CHAR:

#ifdef  select_compare_code
            if ((MR_Integer) x == (MR_Integer) y) {
                return_compare_answer(builtin, int, 0, MR_COMPARE_EQUAL);
            } else if ((MR_Integer) x < (MR_Integer) y) {
                return_compare_answer(builtin, int, 0, MR_COMPARE_LESS);
            } else {
                return_compare_answer(builtin, int, 0, MR_COMPARE_GREATER);
            }
#else
            return_unify_answer(builtin, int, 0,
                (MR_Integer) x == (MR_Integer) y);
#endif
        case MR_TYPECTOR_REP_UINT:
        case MR_TYPECTOR_REP_UINT8:
        case MR_TYPECTOR_REP_UINT16:
        case MR_TYPECTOR_REP_UINT32:

#ifdef  select_compare_code
            if ((MR_Unsigned) x == (MR_Unsigned) y) {
                return_compare_answer(builtin, uint, 0, MR_COMPARE_EQUAL);
            } else if ((MR_Unsigned) x < (MR_Unsigned) y) {
                return_compare_answer(builtin, uint, 0, MR_COMPARE_LESS);
            } else {
                return_compare_answer(builtin, uint, 0, MR_COMPARE_GREATER);
            }
#else
            return_unify_answer(builtin, uint, 0,
                (MR_Unsigned) x == (MR_Unsigned) y);
#endif

        case MR_TYPECTOR_REP_DUMMY:
            // Since dummy types contain only value, all unifies succeed and
            // all comparisons return "equal".

#ifdef  select_compare_code
            return_compare_answer(builtin, dummy, 0, MR_COMPARE_EQUAL);
#else
            return_unify_answer(builtin, int, 0, MR_TRUE);
#endif

        case MR_TYPECTOR_REP_FLOAT:
            {
                MR_Float   fx, fy;

                fx = MR_word_to_float(x);
                fy = MR_word_to_float(y);
#ifdef  select_compare_code
                if (fx == fy) {
                    return_compare_answer(builtin, float, 0, MR_COMPARE_EQUAL);
                } else if (fx < fy) {
                    return_compare_answer(builtin, float, 0, MR_COMPARE_LESS);
                } else {
                    return_compare_answer(builtin, float, 0,
                        MR_COMPARE_GREATER);
                }
#else
                return_unify_answer(builtin, float, 0, fx == fy);
#endif
            }
        
        case MR_TYPECTOR_REP_INT64:
            {
                int64_t   fx, fy;

                fx = MR_word_to_int64(x);
                fy = MR_word_to_int64(y);
#ifdef  select_compare_code
                if (fx == fy) {
                    return_compare_answer(builtin, int64, 0, MR_COMPARE_EQUAL);
                } else if (fx < fy) {
                    return_compare_answer(builtin, int64, 0, MR_COMPARE_LESS);
                } else {
                    return_compare_answer(builtin, int64, 0,
                        MR_COMPARE_GREATER);
                }
#else
                return_unify_answer(builtin, int64, 0, fx == fy);
#endif
            }
        
        case MR_TYPECTOR_REP_UINT64:
            {
                uint64_t   fx, fy;

                fx = MR_word_to_uint64(x);
                fy = MR_word_to_uint64(y);
#ifdef  select_compare_code
                if (fx == fy) {
                    return_compare_answer(builtin, uint64, 0, MR_COMPARE_EQUAL);
                } else if (fx < fy) {
                    return_compare_answer(builtin, uint64, 0, MR_COMPARE_LESS);
                } else {
                    return_compare_answer(builtin, uint64, 0,
                        MR_COMPARE_GREATER);
                }
#else
                return_unify_answer(builtin, uint64, 0, fx == fy);
#endif
            }

        case MR_TYPECTOR_REP_STRING:
            {
                int result;

                result = strcmp((char *) x, (char *) y);

#ifdef  select_compare_code
                if (result == 0) {
                    return_compare_answer(builtin, string, 0,
                        MR_COMPARE_EQUAL);
                } else if (result < 0) {
                    return_compare_answer(builtin, string, 0,
                        MR_COMPARE_LESS);
                } else {
                    return_compare_answer(builtin, string, 0,
                        MR_COMPARE_GREATER);
                }
#else
                return_unify_answer(builtin, string, 0, result == 0);
#endif
            }

            // We use the c_pointer statistics for stable_c_pointer
            // until the stable_c_pointer type is actually added,
            // which will be *after* the builtin types' handwritten
            // unify and compare preds are replaced by automatically
            // generated code.
            //
            // XXX This is a temporary measure.

#ifdef include_compare_rep_code
        case MR_TYPECTOR_REP_STABLE_FOREIGN: // fallthru
#endif
        case MR_TYPECTOR_REP_STABLE_C_POINTER: // fallthru
        case MR_TYPECTOR_REP_C_POINTER:
#ifdef  select_compare_code
            if ((void *) x == (void *) y) {
                return_compare_answer(builtin, c_pointer, 0,
                    MR_COMPARE_EQUAL);
            } else if ((void *) x < (void *) y) {
                return_compare_answer(builtin, c_pointer, 0,
                    MR_COMPARE_LESS);
            } else {
                return_compare_answer(builtin, c_pointer, 0,
                    MR_COMPARE_GREATER);
            }
#else
            return_unify_answer(builtin, c_pointer, 0,
                (void *) x == (void *) y);
#endif

        case MR_TYPECTOR_REP_TYPEINFO:
            {
#ifdef  select_compare_code
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                MR_restore_transient_registers();
                return_compare_answer(private_builtin, type_info, 0, result);
#else
                MR_bool result;

                MR_save_transient_registers();
                result = MR_unify_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                MR_restore_transient_registers();
                return_unify_answer(private_builtin, type_info, 0, result);
#endif
            }

        case MR_TYPECTOR_REP_TYPEDESC:
            // Differs from the code for MR_TYPECTOR_REP_TYPEINFO
            // only in recording profiling information elsewhere.

            {
#ifdef  select_compare_code
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                MR_restore_transient_registers();
                return_compare_answer(type_desc, type_desc, 0, result);
#else
                MR_bool result;

                MR_save_transient_registers();
                result = MR_unify_type_info(
                    (MR_TypeInfo) x, (MR_TypeInfo) y);
                MR_restore_transient_registers();
                return_unify_answer(type_desc, type_desc, 0, result);
#endif
            }

        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
            {
#ifdef  select_compare_code
                int result;

                MR_save_transient_registers();
                result = MR_compare_pseudo_type_info(
                    (MR_PseudoTypeInfo) x, (MR_PseudoTypeInfo) y);
                MR_restore_transient_registers();
                return_compare_answer(type_desc, pseudo_type_desc, 0, result);
#else
                MR_bool result;

                MR_save_transient_registers();
                result = MR_unify_pseudo_type_info(
                    (MR_PseudoTypeInfo) x, (MR_PseudoTypeInfo) y);
                MR_restore_transient_registers();
                return_unify_answer(type_desc, pseudo_type_desc, 0, result);
#endif
            }

        case MR_TYPECTOR_REP_TYPECTORINFO:
            {
#ifdef  select_compare_code
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_ctor_info(
                    (MR_TypeCtorInfo) x, (MR_TypeCtorInfo) y);
                MR_restore_transient_registers();
                return_compare_answer(private_builtin, type_ctor_info, 0,
                    result);
#else
                MR_bool result;

                MR_save_transient_registers();
                result = MR_unify_type_ctor_info(
                    (MR_TypeCtorInfo) x, (MR_TypeCtorInfo) y);
                MR_restore_transient_registers();
                return_unify_answer(private_builtin, type_ctor_info, 0,
                    result);
#endif
            }

        case MR_TYPECTOR_REP_TYPECTORDESC:
            {
#ifdef  select_compare_code
                int result;

                MR_save_transient_registers();
                result = MR_compare_type_ctor_desc(
                    (MR_TypeCtorDesc) x, (MR_TypeCtorDesc) y);
                MR_restore_transient_registers();
                return_compare_answer(type_desc, type_ctor_desc, 0, result);
#else
                MR_bool result;

                MR_save_transient_registers();
                result = MR_unify_type_ctor_desc(
                    (MR_TypeCtorDesc) x, (MR_TypeCtorDesc) y);
                MR_restore_transient_registers();
                return_unify_answer(type_desc, type_ctor_desc, 0, result);
#endif
            }

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error(attempt_msg "terms of type `void'");

        case MR_TYPECTOR_REP_FUNC:
        case MR_TYPECTOR_REP_PRED:
            {
#ifdef  include_compare_rep_code
                int     result;

                MR_save_transient_registers();
                result = MR_compare_closures_representation((MR_Closure *) x,
                            (MR_Closure *) y);
                MR_restore_transient_registers();

                if (MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_FUNC) {
                    return_compare_answer(builtin, func, 0, result);
                } else {
                    return_compare_answer(builtin, pred, 0, result);
                }
#else
                MR_fatal_error(attempt_msg "higher-order terms");
#endif
            }

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            MR_fatal_error(attempt_msg "typeclass_infos");

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            MR_fatal_error(attempt_msg "base_typeclass_infos");

        case MR_TYPECTOR_REP_SUBGOAL:
            MR_fatal_error(attempt_msg "subgoal");

        case MR_TYPECTOR_REP_HP:
            MR_fatal_error(attempt_msg "hp");

        case MR_TYPECTOR_REP_SUCCIP:
            MR_fatal_error(attempt_msg "succip");

        case MR_TYPECTOR_REP_CURFR:
            MR_fatal_error(attempt_msg "curfr");

        case MR_TYPECTOR_REP_MAXFR:
            MR_fatal_error(attempt_msg "maxfr");

        case MR_TYPECTOR_REP_REDOFR:
            MR_fatal_error(attempt_msg "redofr");

        case MR_TYPECTOR_REP_REDOIP:
            MR_fatal_error(attempt_msg "redoip");

        case MR_TYPECTOR_REP_TICKET:
            MR_fatal_error(attempt_msg "ticket");

        case MR_TYPECTOR_REP_TRAIL_PTR:
            MR_fatal_error(attempt_msg "trail_ptr");

        case MR_TYPECTOR_REP_REFERENCE:
#ifdef  select_compare_code
            // This is not permitted, because keeping the order of references
            // consistent would cause significant difficulty for a copying
            // garbage collector.

            MR_fatal_error(attempt_msg "terms of a reference type");
#else
            return_unify_answer(private_builtin, ref, 1,
                (void *) x == (void *) y);
#endif

        case MR_TYPECTOR_REP_UNUSED1:
        case MR_TYPECTOR_REP_UNUSED2:
        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error(attempt_msg "terms of unknown type");
    }

    MR_fatal_error("got to the end of " MR_STRINGIFY(start_label));

#ifdef  select_compare_code
  #undef    return_compare_answer
#else
  #undef    return_unify_answer
#endif
