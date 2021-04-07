// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module provides much of the functionality for doing higher order
// calls (with the rest provided by code generation of the generic_call
// HLDS construct), and most of the functionality for doing generic
// unifications and comparisons (with the rest provided by the
// compiler-generated unify, index and compare predicates).

// Note that the routines here don't need any special handling for accurate GC,
// since they only do tail-calls (or equivalent); their stack stack frames
// will never be live on the stack when a garbage collection occurs (or if they
// are, will never contain any live variables that might contain pointers to
// the Mercury heap).

/*
INIT mercury_sys_init_call
ENDINIT
*/

#include "mercury_imp.h"
#include "mercury_ho_call.h"
#include "mercury_type_desc.h"
#include "mercury_deconstruct_macros.h"
#include "mercury_deep_profiling.h"
#include "mercury_deep_profiling_hand.h"
#include "mercury_layout_util.h"
#include "mercury_builtin_types.h"
#include "mercury_builtin_types_proc_layouts.h"
         // for unify/compare of pred/func and for proc_layout structures
#include "mercury_types.h"
#include "mercury_bitmap.h"

MR_SpecialPredHooks MR_special_pred_hooks;

#ifdef  MR_DEEP_PROFILING
  #ifdef MR_DEEP_PROFILING_STATISTICS
    #define maybe_incr_prof_call_builtin_new()                              \
            do { MR_deep_prof_call_builtin_new++; } while (0)
    #define maybe_incr_prof_call_builtin_old()                              \
            do { MR_deep_prof_call_builtin_old++; } while (0)
  #else
    #define maybe_incr_prof_call_builtin_new()      ((void) 0)
    #define maybe_incr_prof_call_builtin_old()      ((void) 0)
  #endif

  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    #define maybe_incr_call_count(csd)                                      \
            do { csd->MR_csd_own.MR_own_calls++; } while (0)
  #else
    #define maybe_incr_call_count(csd)              ((void) 0)
  #endif

  #define   special_pred_call_leave_code(pl, field)                         \
    do {                                                                    \
        MR_CallSiteDynamic  *csd;                                           \
        MR_ProcDynamic      *pd;                                            \
                                                                            \
        csd = MR_next_call_site_dynamic;                                    \
        pd = csd->MR_csd_callee_ptr;                                        \
        if (pd == NULL) {                                                   \
            MR_new_proc_dynamic(pd, (MR_ProcLayout *) &pl);                 \
            csd->MR_csd_callee_ptr = pd;                                    \
            maybe_incr_prof_call_builtin_new();                             \
        } else {                                                            \
            maybe_incr_prof_call_builtin_old();                             \
        }                                                                   \
        maybe_incr_call_count(csd);                                         \
        csd->MR_csd_own.field++;                                            \
    } while (0)

  #define   unify_call_exit_code(mod, pred, type, a)                        \
    special_pred_call_leave_code(                                           \
        MR_proc_layout_uci_name(mod, pred, type, a, 0), MR_own_exits)

  #define   unify_call_fail_code(mod, pred, type, a)                        \
    special_pred_call_leave_code(                                           \
        MR_proc_layout_uci_name(mod, pred, type, a, 0), MR_own_fails)

  #define   compare_call_exit_code(mod, pred, type, a)                      \
    special_pred_call_leave_code(                                           \
        MR_proc_layout_uci_name(mod, pred, type, a, 0), MR_own_exits)

#endif

#ifdef MR_HIGHLEVEL_CODE

// Define the generic unify/2 and compare/3 functions.

MR_bool MR_CALL
mercury__builtin__unify_2_p_0(MR_Mercury_Type_Info ti, MR_Box x, MR_Box y)
{
    MR_TypeInfo             type_info;
    MR_TypeCtorInfo         type_ctor_info;
    MR_TypeCtorRep          type_ctor_rep;
    int                     arity;
    MR_TypeInfoParams       params;
    MR_Mercury_Type_Info    *args;
    MR_ProcAddr             unify_pred;

    type_info = (MR_TypeInfo) ti;
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    // Tuple and higher-order types do not have a fixed arity,
    // so they need to be special cased here.

    type_ctor_rep = MR_type_ctor_rep(type_ctor_info);
    if (type_ctor_rep == MR_TYPECTOR_REP_TUPLE) {
        if (MR_special_pred_hooks.MR_unify_tuple_pred != NULL) {
            return MR_special_pred_hooks.MR_unify_tuple_pred(ti,
                (MR_Word) x, (MR_Word) y);
        }
    } else if (type_ctor_rep == MR_TYPECTOR_REP_PRED) {
        return mercury__builtin____Unify____pred_0_0((MR_Pred) x, (MR_Pred) y);
    } else if (type_ctor_rep == MR_TYPECTOR_REP_FUNC) {
        return mercury__builtin____Unify____pred_0_0((MR_Pred) x, (MR_Pred) y);
    }

    arity = type_ctor_info->MR_type_ctor_arity;
    params = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
    args = (MR_Mercury_Type_Info *) params;

    unify_pred = type_ctor_info->MR_type_ctor_unify_pred;

    // Cast unify_pred to the right type and then call it,
    // passing the right number of type_info arguments.

    switch (arity) {
        case 0:
            return ((MR_UnifyFunc_0 *) unify_pred)(x, y);

        case 1:
            return ((MR_UnifyFunc_1 *) unify_pred)(args[1], x, y);

        case 2:
            return ((MR_UnifyFunc_2 *) unify_pred)(args[1], args[2], x, y);

        case 3:
            return ((MR_UnifyFunc_3 *) unify_pred)(args[1], args[2], args[3],
                x, y);

        case 4:
            return ((MR_UnifyFunc_4 *) unify_pred)(args[1], args[2], args[3],
                args[4], x, y);

        case 5:
            return ((MR_UnifyFunc_5 *) unify_pred)(args[1], args[2], args[3],
                args[4], args[5], x, y);

        default:
            MR_fatal_error("unify/2: type arity > 5 not supported");
    }
}

void MR_CALL
mercury__builtin__compare_3_p_0(MR_Mercury_Type_Info ti,
    MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
    MR_TypeInfo             type_info;
    MR_TypeCtorInfo         type_ctor_info;
    MR_TypeCtorRep          type_ctor_rep;
    int                     arity;
    MR_TypeInfoParams       params;
    MR_Mercury_Type_Info    *args;
    MR_ProcAddr             compare_pred;

    type_info = (MR_TypeInfo) ti;
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    // Tuple and higher-order types do not have a fixed arity,
    // so they need to be special cased here.

    type_ctor_rep = MR_type_ctor_rep(type_ctor_info);
    if (type_ctor_rep == MR_TYPECTOR_REP_TUPLE) {
        if (MR_special_pred_hooks.MR_compare_tuple_pred != NULL) {
            MR_special_pred_hooks.MR_compare_tuple_pred(ti, res,
                (MR_Word) x, (MR_Word) y);
            return;
        }
    } else if (type_ctor_rep == MR_TYPECTOR_REP_PRED) {
        mercury__builtin____Compare____pred_0_0(res, (MR_Pred) x, (MR_Pred) y);
        return;
    } else if (type_ctor_rep == MR_TYPECTOR_REP_FUNC) {
        mercury__builtin____Compare____pred_0_0(res, (MR_Pred) x, (MR_Pred) y);
        return;
    }

    arity = type_ctor_info->MR_type_ctor_arity;
    params = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
    args = (MR_Mercury_Type_Info *) params;

    compare_pred = type_ctor_info->MR_type_ctor_compare_pred;

    // Cast compare_pre to the right type and then call it,
    // passing the right number of type_info arguments.

    switch (arity) {
        case 0:
            ((MR_CompareFunc_0 *) compare_pred)(res, x, y);
            break;

        case 1:
            ((MR_CompareFunc_1 *) compare_pred)(args[1], res, x, y);
            break;

        case 2:
            ((MR_CompareFunc_2 *) compare_pred)(args[1], args[2], res, x, y);
            break;

        case 3:
            ((MR_CompareFunc_3 *) compare_pred)(args[1], args[2], args[3],
                res, x, y);
            break;

        case 4:
            ((MR_CompareFunc_4 *) compare_pred)(args[1], args[2], args[3],
                args[4], res, x, y);
            break;

        case 5:
            ((MR_CompareFunc_5 *) compare_pred)(args[1], args[2], args[3],
                args[4], args[5], res, x, y);
            break;

        default:
            MR_fatal_error("compare/3: type arity > 5 not supported");
    }
}

void MR_CALL
mercury__builtin__compare_3_p_1(
    MR_Mercury_Type_Info type_info, MR_Comparison_Result *res,
    MR_Box x, MR_Box y)
{
    mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void MR_CALL
mercury__builtin__compare_3_p_2(
    MR_Mercury_Type_Info type_info, MR_Comparison_Result *res,
    MR_Box x, MR_Box y)
{
    mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void MR_CALL
mercury__builtin__compare_3_p_3(
    MR_Mercury_Type_Info type_info, MR_Comparison_Result *res,
    MR_Box x, MR_Box y)
{
    mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void MR_CALL
mercury__builtin__compare_representation_3_p_0(MR_Mercury_Type_Info ti,
    MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
    MR_SORRY("compare_representation/3 for HIGHLEVEL_CODE");
}

#else   // ! MR_HIGHLEVEL_CODE

static  MR_Word MR_generic_compare(MR_TypeInfo type_info, MR_Word x, MR_Word y);
static  MR_Word MR_generic_unify(MR_TypeInfo type_info, MR_Word x, MR_Word y);
static  MR_Word MR_generic_compare_representation(MR_TypeInfo type_info,
            MR_Word x, MR_Word y);
static  MR_Word MR_compare_closures_representation(MR_Closure *x,
            MR_Closure *y);

// The called closure may contain only input arguments. The extra arguments
// provided by the higher-order call may be input or output, and may appear
// in any order.
//
// The input arguments to do_call_closure_compact are the closure in MR_r1,
// the number of additional input arguments in MR_r2, and the additional input
// arguments themselves in MR_r3, MR_r4, etc. The output arguments are
// returned in registers MR_r1, MR_r2, etc for det and nondet calls or
// registers MR_r2, MR_r3, etc for semidet calls.
//
// When float registers are enabled, float input arguments are passed
// in MR_f1, MR_f2, etc. Float output arguments are returned in registers
// MR_f1, MR_f2, etc.
//
// The placement of the extra input arguments into MR_rN and MR_fN etc. is done
// by the code generator, as is the movement of the output arguments to their
// eventual destinations.
//
// Each do_call_closure_N variant is specialized for the case where the number
// of additional input arguments is N. When calling them, the code generator
// puts the additional arguments (if any) in MR_r2, MR_r3 etc.

    // Number of input arguments to do_call_*_closure_compact,
    // MR_r1 -> closure
    // MR_r2 -> number of immediate input arguments: (R | F<<16)
    //
    // R is the number of regular register input arguments
    // F is the number of float register input arguments.

#define MR_HO_CALL_INPUTS_COMPACT   2

    // Number of input arguments to do_call_*_class_method_compact,
    // MR_r1 -> typeclass info
    // MR_r2 -> index of method in typeclass info
    // MR_r3 -> number of immediate input arguments.
    //
    // Method calls do not yet use float registers for input or output.

#define MR_CLASS_METHOD_CALL_INPUTS_COMPACT 3

#ifdef MR_DO_CALL_STATS
#define MR_MAX_STATS_ARG 100

static unsigned int    MR_explicit_closure_arg_histogram[MR_MAX_STATS_ARG];
static unsigned int    MR_explicit_method_arg_histogram[MR_MAX_STATS_ARG];
static unsigned int    MR_hidden_closure_arg_histogram[MR_MAX_STATS_ARG];
static unsigned int    MR_hidden_method_arg_histogram[MR_MAX_STATS_ARG];

#define MR_maybe_record_closure_histogram(exp, hid)                         \
    do {                                                                    \
        if (exp < MR_MAX_STATS_ARG) {                                       \
            MR_explicit_closure_arg_histogram[exp]++;                       \
        }                                                                   \
        if (hid < MR_MAX_STATS_ARG) {                                       \
            MR_hidden_closure_arg_histogram[hid]++;                         \
        }                                                                   \
    } while (0)

#define MR_maybe_record_method_histogram(exp, hid)                          \
    do {                                                                    \
        if (exp < MR_MAX_STATS_ARG) {                                       \
            MR_explicit_method_arg_histogram[exp]++;                        \
        }                                                                   \
        if (hid < MR_MAX_STATS_ARG) {                                       \
            MR_hidden_method_arg_histogram[hid]++;                          \
        }                                                                   \
    } while (0)

void
MR_print_hidden_arg_stats(FILE *fp)
{
    int i;

    for (i = 0; i < MR_MAX_STATS_ARG; i++) {
        if (MR_explicit_closure_arg_histogram[i] > 0) {
            fprintf(fp, "closure invocations with %d explicit args: %d\n",
                i, MR_explicit_closure_arg_histogram[i]);
        }
    }

    for (i = 0; i < MR_MAX_STATS_ARG; i++) {
        if (MR_explicit_method_arg_histogram[i] > 0) {
            fprintf(fp, "method invocations with %d explicit args: %d\n",
                i, MR_explicit_method_arg_histogram[i]);
        }
    }

    for (i = 0; i < MR_MAX_STATS_ARG; i++) {
        if (MR_hidden_closure_arg_histogram[i] > 0) {
            fprintf(fp, "closure invocations with %d hidden args: %d\n",
                i, MR_hidden_closure_arg_histogram[i]);
        }
    }

    for (i = 0; i < MR_MAX_STATS_ARG; i++) {
        if (MR_hidden_method_arg_histogram[i] > 0) {
            fprintf(fp, "method invocations with %d hidden args: %d\n",
                i, MR_hidden_method_arg_histogram[i]);
        }
    }
}

#else

#define MR_maybe_record_closure_histogram(exp, hid)     ((void) 0)
#define MR_maybe_record_method_histogram(exp, hid)      ((void) 0)

#endif

// These are the real implementations of higher order calls and method calls.

#include "mercury_ho_call_declares.i"
#include "mercury_method_call_declares.i"

// These are the real implementations of unify and compare.

MR_define_extern_entry(mercury__builtin__unify_2_0);
MR_define_extern_entry(mercury__builtin__compare_3_0);
MR_define_extern_entry(mercury__builtin__compare_3_1);
MR_define_extern_entry(mercury__builtin__compare_3_2);
MR_define_extern_entry(mercury__builtin__compare_3_3);
MR_declare_label(mercury__builtin__compare_3_0_i1);
MR_define_extern_entry(mercury__builtin__compare_representation_3_0);

MR_BEGIN_MODULE(call_module)
#include "mercury_ho_call_inits.i"
#include "mercury_method_call_inits.i"

    MR_init_entry_an(mercury__builtin__unify_2_0);
    MR_init_entry_an(mercury__builtin__compare_3_0);
    MR_init_entry_an(mercury__builtin__compare_3_1);
    MR_init_entry_an(mercury__builtin__compare_3_2);
    MR_init_entry_an(mercury__builtin__compare_3_3);
    MR_init_entry_an(mercury__builtin__compare_representation_3_0);
MR_BEGIN_CODE

// The various variants of do_call_closure and do_call_class_method are
// created by tools/make_spec_ho_call and tools/make_spec_method_call
// respectively.
//
// Each of routine starts by picking up its input arguments from the relevant
// Mercury abstract machine registers and putting them in local variables.
// This allows the values of these arguments to be printed in gdb without
// worrying about which real machine registers, if any, hold them.
//
// Also note that in each case, when we invoke the higher order value,
// we pass MR_prof_ho_caller_proc as the second argument of MR_tailcall
// instead of the name of the routine itself, so that the call gets recorded
// as having come from our caller.
//
// Each of these routines get ignored for profiling. That means they should be
// called using noprof_call() rather than call(). See the comment in
// output_call in compiler/llds_out for the explanation.

#include "mercury_ho_call_codes.i"
#include "mercury_method_call_codes.i"

// mercury__builtin__unify_2_0 is called as `unify(TypeInfo, X, Y)'
// in the mode `unify(in, in, in) is semidet'.

MR_define_entry(mercury__builtin__unify_2_0);
{

#define DECLARE_LOCALS                                                      \
    MR_TypeCtorInfo type_ctor_info;                                         \
    MR_TypeInfo     type_info;                                              \
    MR_Word         x, y;                                                   \
    MR_Word         saved_succip_word;

#define initialize()                                                        \
    do {                                                                    \
        type_info = (MR_TypeInfo) MR_r1;                                    \
        x = MR_r2;                                                          \
        y = MR_r3;                                                          \
        saved_succip_word = MR_succip_word;                                 \
    } while (0)

#define raw_return_answer(answer)                                           \
    do {                                                                    \
        MR_r1 = (answer);                                                   \
        MR_succip_word = saved_succip_word;                                 \
        MR_proceed();                                                       \
    } while (0)

#define tailcall_tci_pred()                                                 \
    tailcall(type_ctor_info->MR_type_ctor_unify_pred)

#define tailcall(label)                                                     \
    MR_tailcall(label, MR_LABEL(mercury__builtin__unify_2_0))

#define start_label                 unify_start
#define call_user_code_label        call_unify_in_proc
#define type_stat_struct            MR_type_stat_mer_unify
#define attempt_msg                 "attempt to unify "
#define entry_point_is_mercury

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall_tci_pred
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
#undef  entry_point_is_mercury

}

// mercury__builtin__compare_3_3 is called as `compare(TypeInfo, Result, X, Y)'
// in the mode `compare(in, out, in, in) is det'.
//
// (The additional entry points replace either or both "in"s with "ui"s.)

MR_define_entry(mercury__builtin__compare_3_0);
#ifdef MR_MPROF_PROFILE_CALLS
{
    MR_tailcall(MR_ENTRY(mercury__builtin__compare_3_3),
        MR_LABEL(mercury__builtin__compare_3_0));
}
#endif
MR_define_entry(mercury__builtin__compare_3_1);
#ifdef MR_MPROF_PROFILE_CALLS
{
    MR_tailcall(MR_ENTRY(mercury__builtin__compare_3_3),
        MR_LABEL(mercury__builtin__compare_3_1));
}
#endif
MR_define_entry(mercury__builtin__compare_3_2);
#ifdef MR_MPROF_PROFILE_CALLS
{
    MR_tailcall(MR_ENTRY(mercury__builtin__compare_3_3),
        MR_LABEL(mercury__builtin__compare_3_2));
}
#endif
MR_define_entry(mercury__builtin__compare_3_3);
{

#define DECLARE_LOCALS                                                      \
    MR_TypeCtorInfo type_ctor_info;                                         \
    MR_TypeInfo     type_info;                                              \
    MR_Word         x, y;                                                   \
    MR_Word         saved_succip_word;

#define initialize()                                                        \
    do {                                                                    \
        type_info = (MR_TypeInfo) MR_r1;                                    \
        x = MR_r2;                                                          \
        y = MR_r3;                                                          \
        saved_succip_word = MR_succip_word;                                 \
    } while (0)

#define raw_return_answer(answer)                                           \
    do {                                                                    \
        MR_r1 = (answer);                                                   \
        MR_succip_word = saved_succip_word;                                 \
        MR_proceed();                                                       \
    } while (0)

#define tailcall_tci_pred()                                                 \
    tailcall(type_ctor_info->MR_type_ctor_compare_pred)

#define tailcall(label)                                                     \
    MR_tailcall(label, MR_LABEL(mercury__builtin__compare_3_3))

#define start_label                 compare_start
#define call_user_code_label        call_compare_in_proc
#define type_stat_struct            MR_type_stat_mer_compare
#define attempt_msg                 "attempt to compare "
#define select_compare_code
#define entry_point_is_mercury

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall_tci_pred
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
#undef  select_compare_code
#undef  entry_point_is_mercury

}

// mercury__builtin__compare_representation_3_0 is called as
// `compare_representation(TypeInfo, Result, X, Y)' in the mode
// `compare_representation(in, uo, in, in) is cc_multi'.

MR_define_entry(mercury__builtin__compare_representation_3_0);
{

#define DECLARE_LOCALS                                                      \
    MR_TypeCtorInfo type_ctor_info;                                         \
    MR_TypeInfo     type_info;                                              \
    MR_Word         x, y;                                                   \
    MR_Word         saved_succip_word;

#define initialize()                                                        \
    do {                                                                    \
        type_info = (MR_TypeInfo) MR_r1;                                    \
        x = MR_r2;                                                          \
        y = MR_r3;                                                          \
        saved_succip_word = MR_succip_word;                                 \
    } while (0)

#define raw_return_answer(answer)                                           \
    do {                                                                    \
        MR_r1 = (answer);                                                   \
        MR_succip_word = saved_succip_word;                                 \
        MR_proceed();                                                       \
    } while (0)

#define tailcall(label)                                                     \
    MR_tailcall(label, MR_LABEL(mercury__builtin__compare_representation_3_0))

#define start_label                 compare_rep_start
#define call_user_code_label        call_compare_rep_in_proc
#define type_stat_struct            MR_type_stat_mer_compare
#define attempt_msg                 "attempt to compare representation "
#define select_compare_code
#define include_compare_rep_code
#define entry_point_is_mercury

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
#undef  select_compare_code
#undef  include_compare_rep_code
#undef  entry_point_is_mercury

}

MR_END_MODULE

static MR_Word
MR_generic_unify(MR_TypeInfo type_info, MR_Word x, MR_Word y)
{

#define DECLARE_LOCALS                                                        \
    MR_TypeCtorInfo type_ctor_info;

#define initialize()                                                          \
    do {                                                                      \
        MR_restore_transient_registers();                                     \
    } while (0)

#define raw_return_answer(answer)                                             \
    do {                                                                      \
        MR_save_transient_registers();                                        \
        return (answer);                                                      \
    } while (0)

#define tailcall_tci_pred()                                                   \
    tailcall(type_ctor_info->MR_type_ctor_unify_pred)

#define tailcall(label)                                                       \
    do {                                                                      \
        MR_save_transient_registers();                                        \
        (void) MR_call_engine(label, MR_FALSE);                               \
        MR_restore_transient_registers();                                     \
        return (MR_r1);                                                       \
    } while (0)

#define start_label             unify_func_start
#define call_user_code_label    call_unify_in_func
#define type_stat_struct        MR_type_stat_c_unify
#define attempt_msg             "attempt to unify "

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall_tci_pred
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
}

static MR_Word
MR_generic_compare(MR_TypeInfo type_info, MR_Word x, MR_Word y)
{
#define DECLARE_LOCALS                                                      \
    MR_TypeCtorInfo type_ctor_info;

#define initialize()                                                        \
    do {                                                                    \
        MR_restore_transient_registers();                                   \
    } while (0)

#define raw_return_answer(answer)                                           \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        return (answer);                                                    \
    } while (0)

#define tailcall_tci_pred()                                                 \
    tailcall(type_ctor_info->MR_type_ctor_compare_pred)

#define tailcall(label)                                                     \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        (void) MR_call_engine(label, MR_FALSE);                             \
        MR_restore_transient_registers();                                   \
        return (MR_r1);                                                     \
    } while (0)

#define start_label             compare_func_start
#define call_user_code_label    call_compare_in_func
#define type_stat_struct        MR_type_stat_c_compare
#define attempt_msg             "attempt to compare "
#define select_compare_code

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall_tci_pred
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
#undef  select_compare_code
}

static MR_Word
MR_generic_compare_representation(MR_TypeInfo type_info, MR_Word x, MR_Word y)
{
#define DECLARE_LOCALS                                                      \
    MR_TypeCtorInfo type_ctor_info;

#define initialize()                                                        \
    do {                                                                    \
        MR_restore_transient_registers();                                   \
    } while (0)

#define raw_return_answer(answer)                                           \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        return (answer);                                                    \
    } while (0)

#define tailcall(label)                                                     \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        (void) MR_call_engine(label, MR_FALSE);                             \
        MR_restore_transient_registers();                                   \
        return (MR_r1);                                                     \
    } while (0)

#define start_label                 compare_rep_func_start
#define call_user_code_label        call_compare_rep_in_func
#define type_stat_struct            MR_type_stat_c_compare
#define attempt_msg                 "attempt to compare representation "
#define select_compare_code
#define include_compare_rep_code

#include "mercury_unify_compare_body.h"

#undef  DECLARE_LOCALS
#undef  initialize
#undef  raw_return_answer
#undef  tailcall
#undef  start_label
#undef  call_user_code_label
#undef  type_stat_struct
#undef  attempt_msg
#undef  select_compare_code
#undef  include_compare_rep_code
}

static  MR_Word
MR_compare_closures_representation(MR_Closure *x, MR_Closure *y)
{
    MR_Closure_Layout   *x_layout;
    MR_Closure_Layout   *y_layout;
    MR_ProcId           *x_proc_id;
    MR_ProcId           *y_proc_id;
    MR_ConstString      x_module_name;
    MR_ConstString      y_module_name;
    MR_ConstString      x_pred_name;
    MR_ConstString      y_pred_name;
    MR_TypeInfo         *x_type_params;
    MR_TypeInfo         *y_type_params;
    int                 x_num_args;
    int                 y_num_args;
    int                 num_args;
    int                 r_offset;
    int                 f_offset;
    int                 i;
    int                 result;

    // Optimize the simple case.

    if (x == y) {
        return MR_COMPARE_EQUAL;
    }

    x_layout = x->MR_closure_layout;
    y_layout = y->MR_closure_layout;

    x_proc_id = &x_layout->MR_closure_id->MR_closure_proc_id;
    y_proc_id = &y_layout->MR_closure_id->MR_closure_proc_id;

    if (x_proc_id != y_proc_id) {
        if (MR_PROC_ID_IS_UCI(*x_proc_id)) {
            x_module_name = x_proc_id->MR_proc_uci.MR_uci_def_module;
            x_pred_name = x_proc_id->MR_proc_uci.MR_uci_pred_name;
        } else {
            x_module_name = x_proc_id->MR_proc_user.MR_user_decl_module;
            x_pred_name = x_proc_id->MR_proc_user.MR_user_name;
        }
        if (MR_PROC_ID_IS_UCI(*y_proc_id)) {
            y_module_name = y_proc_id->MR_proc_uci.MR_uci_def_module;
            y_pred_name = y_proc_id->MR_proc_uci.MR_uci_pred_name;
        } else {
            y_module_name = y_proc_id->MR_proc_user.MR_user_decl_module;
            y_pred_name = y_proc_id->MR_proc_user.MR_user_name;
        }

        result = strcmp(x_module_name, y_module_name);
        if (result < 0) {
            return MR_COMPARE_LESS;
        } else if (result > 0) {
            return MR_COMPARE_GREATER;
        }

        result = strcmp(x_pred_name, y_pred_name);
        if (result < 0) {
            return MR_COMPARE_LESS;
        } else if (result > 0) {
            return MR_COMPARE_GREATER;
        }
    }

    x_num_args =
        MR_closure_num_hidden_r_args(x) + MR_closure_num_hidden_f_args(x);
    y_num_args =
        MR_closure_num_hidden_r_args(y) + MR_closure_num_hidden_f_args(y);
    if (x_num_args < y_num_args) {
        return MR_COMPARE_LESS;
    } else if (x_num_args > y_num_args) {
        return MR_COMPARE_GREATER;
    }

    num_args = x_num_args;
    x_type_params = MR_materialize_closure_type_params(x);
    y_type_params = MR_materialize_closure_type_params(y);

    r_offset = 0;
    f_offset = MR_closure_num_hidden_r_args(x);

    for (i = 0; i < num_args; i++) {
        MR_TypeInfo x_arg_type_info;
        MR_TypeInfo y_arg_type_info;
        MR_TypeInfo arg_type_info;
        MR_Unsigned offset;

        x_arg_type_info = MR_create_type_info(x_type_params,
            x_layout->MR_closure_arg_pseudo_type_info[i]);
        y_arg_type_info = MR_create_type_info(y_type_params,
            y_layout->MR_closure_arg_pseudo_type_info[i]);
        result = MR_compare_type_info(x_arg_type_info, y_arg_type_info);
        if (result != MR_COMPARE_EQUAL) {
            goto finish_closure_compare;
        }

        arg_type_info = x_arg_type_info;
#ifdef MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
        if (MR_unify_type_ctor_info((MR_TypeCtorInfo) MR_FLOAT_CTOR_ADDR,
            MR_TYPEINFO_GET_TYPE_CTOR_INFO(arg_type_info)))
        {
            offset = f_offset++;
        } else {
            offset = r_offset++;
        }
#else
        offset = i;
#endif
        result = MR_generic_compare_representation(arg_type_info,
            x->MR_closure_hidden_args_0[offset],
            y->MR_closure_hidden_args_0[offset]);
        if (result != MR_COMPARE_EQUAL) {
            goto finish_closure_compare;
        }
    }

    result = MR_COMPARE_EQUAL;

finish_closure_compare:
    if (x_type_params != NULL) {
        MR_free(x_type_params);
    }

    if (y_type_params != NULL) {
        MR_free(y_type_params);
    }

    return result;
}

#endif // not MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
// Code to construct closures, for use by browser/dl.m.

#ifdef MR_HIGHLEVEL_CODE
extern MR_Box MR_CALL MR_generic_closure_wrapper(void *closure,
    MR_Box arg1, MR_Box arg2, MR_Box arg3, MR_Box arg4, MR_Box arg5,
    MR_Box arg6, MR_Box arg7, MR_Box arg8, MR_Box arg9, MR_Box arg10,
    MR_Box arg11, MR_Box arg12, MR_Box arg13, MR_Box arg14, MR_Box arg15,
    MR_Box arg16, MR_Box arg17, MR_Box arg18, MR_Box arg19, MR_Box arg20);
#endif

struct MR_Closure_Struct *
MR_make_closure(MR_Code *proc_addr)
{
    static  int                 closure_counter = 0;
    MR_Closure                  *closure;
    MR_Word                     closure_word;
    MR_ClosureId                *closure_id;
    MR_Closure_Dyn_Link_Layout  *closure_layout;
    char                        buf[80];
    int                         num_hidden_r_args;

    MR_restore_transient_hp();

    // Create a goal path that encodes a unique id for this closure.
    closure_counter++;
    sprintf(buf, "@%d;", closure_counter);

    // XXX All the allocations in this code should use malloc
    // in deep profiling grades.

    // Construct the MR_ClosureId.

    MR_incr_hp_type_msg(closure_id, MR_ClosureId,
        MR_ALLOC_SITE_RUNTIME, NULL);
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_pred_or_func =
        MR_PREDICATE;
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_decl_module =
        "unknown";
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_def_module = "unknown";
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_name = "unknown";
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_arity = -1;
    closure_id->MR_closure_proc_id.MR_proc_user.MR_user_mode = -1;
    closure_id->MR_closure_module_name = "dl";
    closure_id->MR_closure_file_name = __FILE__;
    closure_id->MR_closure_line_number = __LINE__;
    MR_make_aligned_string_copy_msg(closure_id->MR_closure_goal_path, buf,
        MR_ALLOC_SITE_STRING);

    // Construct the MR_Closure_Layout.

    MR_incr_hp_type_msg(closure_layout, MR_Closure_Dyn_Link_Layout,
        MR_ALLOC_SITE_RUNTIME, NULL);
    closure_layout->MR_closure_dl_id = closure_id;
    closure_layout->MR_closure_dl_type_params = NULL;
    closure_layout->MR_closure_dl_num_all_args = 0;

    // Construct the MR_Closure.

#ifdef MR_HIGHLEVEL_CODE
    num_hidden_r_args = 1;
#else
    num_hidden_r_args = 0;
#endif
    MR_offset_incr_hp_msg(closure_word, 0, 3 + num_hidden_r_args,
        MR_ALLOC_SITE_RUNTIME, NULL);
    closure = (MR_Closure *) closure_word;

    closure->MR_closure_layout = (MR_Closure_Layout *) closure_layout;
    closure->MR_closure_code = proc_addr;
    closure->MR_closure_num_hidden_args_rf = num_hidden_r_args;
#ifdef MR_HIGHLEVEL_CODE
    closure->MR_closure_hidden_args(1) = (MR_Word) &MR_generic_closure_wrapper;
#endif

    MR_save_transient_hp();
    return closure;
}

#ifdef MR_HIGHLEVEL_CODE
// For the --high-level-code grades, the closure will be passed
// as an argument to the wrapper procedure. The wrapper procedure
// then extracts any needed curried arguments from the closure,
// and calls the real procedure. Normally the wrapper procedure
// knows which real procedure it will call, but for dl.m we use
// a generic wrapper procedure, and treat the real procedure
// as a curried argument of the generic wrapper. That is always
// the only curried argument, so all the wrapper needs to do
// is to extract the procedure address from the closure, and
// then call it, passing the same arguments that it was passed,
// except for the closure itself.
//
// XXX Using a single generic wrapper procedure is a nasty hack.
// We play fast and loose with the C type system here. In reality
// this will get called with different return type, different
// argument types, and with fewer than 20 arguments. Likewise, the
// procedure that it calls may actually have different arity, return type
// and argument types than we pass. So we really ought to have lots of
// different wrapper procedures, for each different return type, number
// of arguments, and even for each different set of argument types.
// Doing it right might require run-time code generation!
// But with traditional C calling conventions, using a single wrapper
// like this will work anyway, at least for arguments whose type is the
// same size as MR_Box. It fails for arguments of type `char' or `float'.
//
// XXX This will also fail for calling conventions where the callee pops the
// arguments. To handle that right, we'd need different wrappers for
// each different number of arguments. (Doing that would also be slightly
// more efficient, so it may worth doing...)
//
// There are also a couple of libraries called `ffcall' and `libffi'
// which we might be able use to do this in a more portable manner.

MR_Box MR_CALL
MR_generic_closure_wrapper(void *closure,
    MR_Box arg1, MR_Box arg2, MR_Box arg3, MR_Box arg4, MR_Box arg5,
    MR_Box arg6, MR_Box arg7, MR_Box arg8, MR_Box arg9, MR_Box arg10,
    MR_Box arg11, MR_Box arg12, MR_Box arg13, MR_Box arg14, MR_Box arg15,
    MR_Box arg16, MR_Box arg17, MR_Box arg18, MR_Box arg19, MR_Box arg20)
{
    typedef MR_Box MR_CALL FuncType(
        MR_Box a1, MR_Box a2, MR_Box a3, MR_Box a4, MR_Box a5,
        MR_Box a6, MR_Box a7, MR_Box a8, MR_Box a9, MR_Box a10,
        MR_Box a11, MR_Box a12, MR_Box a13, MR_Box a14, MR_Box a15,
        MR_Box a16, MR_Box a17, MR_Box a18, MR_Box a19, MR_Box a20);
    FuncType *proc = (FuncType *)
        MR_field(MR_mktag(0), closure, (MR_Integer) 3);
    return (*proc)(arg1, arg2, arg3, arg4, arg5,
        arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15,
        arg16, arg17, arg18, arg19, arg20);
}
#endif // MR_HIGHLEVEL_CODE

// The initialization function needs to be defined even when
// MR_HIGHLEVEL_CODE is set, because it will get included
// in the list of initialization functions that get called.
// So for MR_HIGHLEVEL_CODE it just does nothing.

// Forward decls to suppress gcc warnings.
void mercury_sys_init_call_init(void);
void mercury_sys_init_call_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_call_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_call_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    call_module();
#endif // not MR_HIGHLEVEL_CODE
}

void mercury_sys_init_call_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_call_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
}
#endif
