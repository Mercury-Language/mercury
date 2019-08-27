%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2008, 2010 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: profiling_builtin.m.
% Authors: conway, zs.
% Stability: low.
%
% This file is automatically imported into every module when deep profiling
% is enabled. It contains support predicates used for deep profiling.
% The tasks of the support predicates are described in some detail in
% ``Engineering a profiler for a logic programming language'' by Thomas Conway
% and Zoltan Somogyi.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module profiling_builtin.
:- interface.

%---------------------------------------------------------------------------%

:- type proc_layout.
:- type proc_dynamic.
:- type call_site_dynamic.

:- impure pred prepare_for_normal_call(int::in) is det.

:- impure pred prepare_for_special_call(int::in, c_pointer::in) is det.

:- impure pred prepare_for_ho_call(int::in, c_pointer::in) is det.

:- impure pred prepare_for_method_call(int::in, c_pointer::in, int::in) is det.

:- impure pred prepare_for_callback(int::in) is det.

:- impure pred prepare_for_tail_call(int::in) is det.

:- impure pred det_call_port_code_ac(proc_layout::in,
    call_site_dynamic::out, call_site_dynamic::out) is det.

:- impure pred det_call_port_code_sr(proc_layout::in, call_site_dynamic::out,
    call_site_dynamic::out, proc_dynamic::out) is det.

:- impure pred det_exit_port_code_ac(call_site_dynamic::in,
    call_site_dynamic::in) is det.

:- impure pred det_exit_port_code_sr(call_site_dynamic::in,
    call_site_dynamic::in, proc_dynamic::in) is det.

:- impure pred semi_call_port_code_ac(proc_layout::in,
    call_site_dynamic::out, call_site_dynamic::out) is det.

:- impure pred semi_call_port_code_sr(proc_layout::in, call_site_dynamic::out,
    call_site_dynamic::out, proc_dynamic::out) is det.

:- impure pred semi_exit_port_code_ac(call_site_dynamic::in,
    call_site_dynamic::in) is det.

:- impure pred semi_exit_port_code_sr(call_site_dynamic::in,
    call_site_dynamic::in, proc_dynamic::in) is det.

:- impure pred semi_fail_port_code_ac(call_site_dynamic::in,
    call_site_dynamic::in) is failure.

:- impure pred semi_fail_port_code_sr(call_site_dynamic::in,
    call_site_dynamic::in, proc_dynamic::in) is failure.

:- impure pred non_call_port_code_ac(proc_layout::in, call_site_dynamic::out,
    call_site_dynamic::out, proc_dynamic::out) is det.

:- impure pred non_call_port_code_sr(proc_layout::in, call_site_dynamic::out,
    call_site_dynamic::out, proc_dynamic::out, proc_dynamic::out) is det.

:- impure pred non_exit_port_code_ac(call_site_dynamic::in,
    call_site_dynamic::in) is det.

:- impure pred non_exit_port_code_sr(call_site_dynamic::in,
    call_site_dynamic::in, proc_dynamic::in) is det.

:- impure pred non_redo_port_code_ac(call_site_dynamic::in, proc_dynamic::in)
    is failure.

:- impure pred non_redo_port_code_sr(call_site_dynamic::in, proc_dynamic::in)
    is failure.

:- impure pred non_fail_port_code_ac(call_site_dynamic::in,
    call_site_dynamic::in) is failure.

:- impure pred non_fail_port_code_sr(call_site_dynamic::in,
    call_site_dynamic::in, proc_dynamic::in) is failure.

:- impure pred save_and_zero_activation_info_ac(int::out, proc_dynamic::out)
    is det.

:- impure pred save_and_zero_activation_info_sr(proc_dynamic::out) is det.

:- impure pred rezero_activation_info_ac is det.

:- impure pred rezero_activation_info_sr is det.

:- impure pred reset_activation_info_ac(int::in, proc_dynamic::in) is det.

:- impure pred reset_activation_info_sr(proc_dynamic::in) is det.

:- impure pred increment_static_coverage_point_count(proc_layout::in, int::in)
    is det.

:- impure pred increment_dynamic_coverage_point_count(int::in) is det.

:- type call_site_nums_2
    --->    call_site_nums_2(int, int).

:- type call_site_nums_3
    --->    call_site_nums_3(int, int, int).

:- type call_site_nums_4
    --->    call_site_nums_4(int, int, int, int).

:- type call_site_nums_5
    --->    call_site_nums_5(int, int, int, int, int).

:- type call_site_nums_6
    --->    call_site_nums_6(int, int, int, int, int, int).

:- type call_site_nums_7
    --->    call_site_nums_7(int, int, int, int, int, int, int).

:- type call_site_nums_8
    --->    call_site_nums_8(int, int, int, int, int, int, int, int).

:- type call_site_nums_9
    --->    call_site_nums_9(int, int, int, int, int, int, int, int, int).

:- impure pred save_recursion_depth_1(call_site_dynamic::in,
    int::in, int::out) is det.

:- impure pred save_recursion_depth_2(call_site_dynamic::in,
    call_site_nums_2::in, int::out, int::out) is det.

:- impure pred save_recursion_depth_3(call_site_dynamic::in,
    call_site_nums_3::in, int::out, int::out, int::out) is det.

:- impure pred save_recursion_depth_4(call_site_dynamic::in,
    call_site_nums_4::in, int::out, int::out, int::out, int::out) is det.

:- impure pred save_recursion_depth_5(call_site_dynamic::in,
    call_site_nums_5::in, int::out, int::out, int::out, int::out,
    int::out) is det.

:- impure pred save_recursion_depth_6(call_site_dynamic::in,
    call_site_nums_6::in, int::out, int::out, int::out, int::out,
    int::out, int::out) is det.

:- impure pred save_recursion_depth_7(call_site_dynamic::in,
    call_site_nums_7::in, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out) is det.

:- impure pred save_recursion_depth_8(call_site_dynamic::in,
    call_site_nums_8::in, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out) is det.

:- impure pred save_recursion_depth_9(call_site_dynamic::in,
    call_site_nums_9::in, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out) is det.

:- impure pred restore_recursion_depth_exit_1(
    call_site_dynamic::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_2(
    call_site_dynamic::in, call_site_nums_2::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_3(
    call_site_dynamic::in, call_site_nums_3::in, int::in, int::in,
    int::in) is det.

:- impure pred restore_recursion_depth_exit_4(
    call_site_dynamic::in, call_site_nums_4::in, int::in, int::in,
    int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_5(
    call_site_dynamic::in, call_site_nums_5::in, int::in, int::in,
    int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_6(
    call_site_dynamic::in, call_site_nums_6::in, int::in, int::in,
    int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_7(
    call_site_dynamic::in, call_site_nums_7::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_8(
    call_site_dynamic::in, call_site_nums_8::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_exit_9(
    call_site_dynamic::in, call_site_nums_9::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_1(
    call_site_dynamic::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_2(
    call_site_dynamic::in, call_site_nums_2::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_3(
    call_site_dynamic::in, call_site_nums_3::in, int::in, int::in,
    int::in) is det.

:- impure pred restore_recursion_depth_fail_4(
    call_site_dynamic::in, call_site_nums_4::in, int::in, int::in,
    int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_5(
    call_site_dynamic::in, call_site_nums_5::in, int::in, int::in,
    int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_6(
    call_site_dynamic::in, call_site_nums_6::in, int::in, int::in,
    int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_7(
    call_site_dynamic::in, call_site_nums_7::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_8(
    call_site_dynamic::in, call_site_nums_8::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred restore_recursion_depth_fail_9(
    call_site_dynamic::in, call_site_nums_9::in, int::in, int::in,
    int::in, int::in, int::in, int::in, int::in, int::in, int::in) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type proc_layout         ---> proc_layout(c_pointer).
:- type proc_dynamic        ---> proc_dynamic(c_pointer).
:- type call_site_dynamic   ---> call_site_dynamic(c_pointer).

:- pragma foreign_type("C", proc_layout,    "MR_ProcLayout *").
:- pragma foreign_type("C", proc_dynamic,   "MR_ProcDynamic *").
:- pragma foreign_type("C", call_site_dynamic,  "MR_CallSiteDynamic *").

:- pragma foreign_decl("C", "
#ifndef MR_DEEP_PROFILING_GUARD
#define MR_DEEP_PROFILING_GUARD

  #ifdef    MR_DEEP_PROFILING

  #include ""mercury_deep_profiling.h""
  #include ""mercury_deep_rec_depth_actions.h""
  #include ""mercury_ho_call.h""
  #include <stdio.h>

  #endif    // MR_DEEP_PROFILING

#endif  // MR_DEEP_PROFILING_GUARD
").

%---------------------------------------------------------------------------%
% Procedures that prepare for calls.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    prepare_for_normal_call(CSN::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;
    MR_CallSiteDynamic  *child_csd;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_normal_call"");
        printf(""call site number: %d\\n"", CSN);
    }
  #endif

    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

    child_csd = pd->MR_pd_call_site_ptr_ptrs[CSN];

  #ifdef MR_DEEP_PROFILING_STATISTICS
    if (child_csd == NULL) {
        MR_deep_prof_prep_normal_new++;
    } else {
        MR_deep_prof_prep_normal_old++;
    }
  #endif

    if (child_csd == NULL) {
        MR_new_call_site_dynamic(child_csd);
        pd->MR_pd_call_site_ptr_ptrs[CSN] = child_csd;
    }

    MR_next_call_site_dynamic = child_csd;
    MR_leave_instrumentation();
#else
    MR_fatal_error(""prepare_for_normal_call: deep profiling not enabled"");
#endif
}").

prepare_for_normal_call(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_normal_call").

:- pragma foreign_proc("C",
    prepare_for_special_call(CSN::in, TypeInfo::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;
    MR_CallSiteDynList  *csdlist;
  #ifdef MR_DEEP_PROFILING_MOVE_TO_FRONT_LISTS
    MR_CallSiteDynList  *prev = NULL;
  #endif
    MR_TypeCtorInfo     type_ctor_info;
    MR_TypeInfo         type_info;
    void                *void_key;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_special_call"");
        printf(""call site number: %d\\n"", CSN);
        type_info = (MR_TypeInfo) TypeInfo;
        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
        printf(""type constructor: %s:%s/%d\\n"",
            type_ctor_info->MR_type_ctor_module_name,
            type_ctor_info->MR_type_ctor_name,
            type_ctor_info->MR_type_ctor_arity);
    }
  #endif

    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

    type_info = (MR_TypeInfo) TypeInfo;
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    void_key = (void *) type_ctor_info;
    MR_search_csdlist(csdlist, prev, pd, CSN, void_key);
    MR_maybe_deep_profile_update_special_history();

  #ifdef MR_DEEP_PROFILING_STATISTICS
    if (csdlist != NULL) {
        MR_deep_prof_prep_special_old++;
    } else {
        MR_deep_prof_prep_special_new++;
    }
  #endif

    if (csdlist != NULL) {
        MR_next_call_site_dynamic = csdlist->MR_csdlist_call_site;
    } else {
        MR_CallSiteDynamic  *newcsd;

        MR_new_call_site_dynamic(newcsd);
        MR_make_and_link_csdlist(csdlist, newcsd, pd, CSN, void_key);
        MR_next_call_site_dynamic = newcsd;
    }

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""prepare_for_special_call: deep profiling not enabled"");
#endif
}").

prepare_for_special_call(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_special_call").

:- pragma foreign_proc("C",
    prepare_for_ho_call(CSN::in, Closure::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;
    MR_Closure          *closure;
    MR_CallSiteDynList  *csdlist;
    void                *void_key;
  #ifdef MR_DEEP_PROFILING_MOVE_TO_FRONT_LISTS
    MR_CallSiteDynList  *prev = NULL;
  #endif

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_ho_call"");
        printf(""call site number: %d\\n"", CSN);
        closure = (MR_Closure *) Closure;
        printf(""closure: layout %p, code %p\\n"",
            (void *) closure->MR_closure_layout,
            (void *) closure->MR_closure_code);
    }
  #endif

    closure = (MR_Closure *) Closure;
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

  #ifdef MR_DEEP_PROFILING_KEY_USES_ID
    void_key = (void *) (closure->MR_closure_layout);
  #else
    void_key = (void *) (closure->MR_closure_code);
  #endif

    MR_search_csdlist(csdlist, prev, pd, CSN, void_key);
    MR_maybe_deep_profile_update_closure_history();

  #ifdef MR_DEEP_PROFILING_STATISTICS
    if (csdlist != NULL) {
        MR_deep_prof_prep_ho_old++;
    } else {
        MR_deep_prof_prep_ho_new++;
    }
  #endif

    if (csdlist != NULL) {
        MR_next_call_site_dynamic = csdlist->MR_csdlist_call_site;
    } else {
        MR_CallSiteDynamic  *newcsd;

        MR_new_call_site_dynamic(newcsd);
        MR_make_and_link_csdlist(csdlist, newcsd, pd, CSN, void_key);
        MR_next_call_site_dynamic = newcsd;
    }

    MR_leave_instrumentation();
#else
    MR_fatal_error(""prepare_for_ho_call: deep profiling not enabled"");
#endif
}").

prepare_for_ho_call(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_ho_call").

:- pragma foreign_proc("C",
    prepare_for_method_call(CSN::in, TypeClassInfo::in, MethodNum::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;
    MR_CallSiteDynList  *csdlist;
    void                *void_key;
  #ifdef MR_DEEP_PROFILING_MOVE_TO_FRONT_LISTS
    MR_CallSiteDynList  *prev = NULL;
  #endif

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_method_call"");
        printf(""call site number: %d\\n"", CSN);
    }
  #endif

    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

    void_key = (void *)
        MR_typeclass_info_class_method(TypeClassInfo, MethodNum);
    MR_search_csdlist(csdlist, prev, pd, CSN, void_key);
    MR_maybe_deep_profile_update_method_history();

  #ifdef MR_DEEP_PROFILING_STATISTICS
    if (csdlist != NULL) {
        MR_deep_prof_prep_method_old++;
    } else {
        MR_deep_prof_prep_method_new++;
    }
  #endif

    if (csdlist != NULL) {
        MR_next_call_site_dynamic = csdlist->MR_csdlist_call_site;
    } else {
        MR_CallSiteDynamic  *newcsd;

        MR_new_call_site_dynamic(newcsd);
        MR_make_and_link_csdlist(csdlist, newcsd, pd, CSN, void_key);
        MR_next_call_site_dynamic = newcsd;
    }

    MR_leave_instrumentation();
#else
    MR_fatal_error(""prepare_for_method_call: deep profiling not enabled"");
#endif
}").

prepare_for_method_call(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_method_call").

:- pragma foreign_proc("C",
    prepare_for_callback(CSN::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_callback"");
        printf(""call site number: %d\\n"", CSN);
    }
  #endif

    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

    MR_current_callback_site = (MR_CallSiteDynList **)
        &(pd->MR_pd_call_site_ptr_ptrs[CSN]);
    MR_leave_instrumentation();
#else
    MR_fatal_error(""prepare_for_callback: deep profiling not enabled"");
#endif
}").

prepare_for_callback(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_callback").

%---------------------------------------------------------------------------%
% Procedures needed for handling tail recursive procedures.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    prepare_for_tail_call(CSN::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
    MR_CallSiteDynamic  *child_csd;
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""prepare_for_tail_call"");
        printf(""call site number: %d\\n"", CSN);
    }
  #endif

    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);

    child_csd = pd->MR_pd_call_site_ptr_ptrs[CSN];

  #ifdef MR_DEEP_PROFILING_STATISTICS
    if (child_csd == NULL) {
        MR_deep_prof_prep_tail_new++;
    } else {
        MR_deep_prof_prep_tail_old++;
    }
  #endif
    if (child_csd == NULL) {
        MR_new_call_site_dynamic(child_csd);
        child_csd->MR_csd_callee_ptr = pd;
        pd->MR_pd_call_site_ptr_ptrs[CSN] = child_csd;
    }

    child_csd->MR_csd_depth_count++;
    MR_current_call_site_dynamic = child_csd;

    MR_leave_instrumentation();
#else
    MR_fatal_error(""prepare_for_tail_call: deep profiling not enabled"");
#endif
}").

prepare_for_tail_call(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("prepare_for_tail_call").

%---------------------------------------------------------------------------%
% Port procedures.
%---------------------------------------------------------------------------%

% These are all implemented in runtime/mercury_profiling_builtin.c,
% which is generated by tools/make_port_code.

:- pragma external_pred(det_call_port_code_ac/3).
:- pragma external_pred(det_call_port_code_sr/4).
:- pragma external_pred(det_exit_port_code_ac/2).
:- pragma external_pred(det_exit_port_code_sr/3).
:- pragma external_pred(semi_call_port_code_ac/3).
:- pragma external_pred(semi_call_port_code_sr/4).
:- pragma external_pred(semi_exit_port_code_ac/2).
:- pragma external_pred(semi_exit_port_code_sr/3).
:- pragma external_pred(semi_fail_port_code_ac/2).
:- pragma external_pred(semi_fail_port_code_sr/3).
:- pragma external_pred(non_call_port_code_ac/4).
:- pragma external_pred(non_call_port_code_sr/5).
:- pragma external_pred(non_exit_port_code_ac/2).
:- pragma external_pred(non_exit_port_code_sr/3).
:- pragma external_pred(non_redo_port_code_ac/2).
:- pragma external_pred(non_redo_port_code_sr/2).
:- pragma external_pred(non_fail_port_code_ac/2).
:- pragma external_pred(non_fail_port_code_sr/3).

det_call_port_code_ac(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("det_call_port_code_ac").
det_call_port_code_sr(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("det_call_port_code_sr").
det_exit_port_code_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("det_exit_port_code_ac").
det_exit_port_code_sr(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("det_exit_port_code_sr").
semi_call_port_code_ac(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("semi_call_port_code_ac").
semi_call_port_code_sr(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("semi_call_port_code_sr").
semi_exit_port_code_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("semi_exit_port_code_ac").
semi_exit_port_code_sr(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("semi_exit_port_code_sr").
semi_fail_port_code_ac(_, _) :-
    impure private_builtin.imp,
    semidet_succeed,
    private_builtin.sorry("semi_fail_port_code_ac").
semi_fail_port_code_sr(_, _, _) :-
    impure private_builtin.imp,
    semidet_succeed,
    private_builtin.sorry("semi_fail_port_code_sr").
non_call_port_code_ac(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_call_port_code_ac").
non_call_port_code_sr(_, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_call_port_code_sr").
non_exit_port_code_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_exit_port_code_ac").
non_exit_port_code_sr(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_exit_port_code_sr").
non_redo_port_code_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_redo_port_code_ac").
non_redo_port_code_sr(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_redo_port_code_sr").
non_fail_port_code_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_fail_port_code_ac").
non_fail_port_code_sr(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("non_fail_port_code_sr").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    save_and_zero_activation_info_ac(Count::out, Ptr::out),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifdef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    Count = ps->MR_ps_activation_count;
    ps->MR_ps_activation_count = 0;
    Ptr = ps->MR_ps_outermost_activation_ptr;
    ps->MR_ps_outermost_activation_ptr = NULL;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""save_and_zero_activation_info_ac called when not using activation counts!"");
  #endif
#else
    MR_fatal_error(""save_and_zero_activation_info_ac: deep profiling not enabled"");
#endif
}").

save_and_zero_activation_info_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_and_zero_activation_info_ac").

:- pragma foreign_proc("C",
    save_and_zero_activation_info_sr(Ptr::out),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifndef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    Ptr = ps->MR_ps_outermost_activation_ptr;
    ps->MR_ps_outermost_activation_ptr = NULL;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""save_and_zero_activation_info_sr called when using activation counts!"");
  #endif
#else
    MR_fatal_error(""save_and_zero_activation_info_sr: deep profiling not enabled"");
#endif
}").

save_and_zero_activation_info_sr(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_and_zero_activation_info_sr").

:- pragma foreign_proc("C",
    rezero_activation_info_ac,
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifdef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    ps->MR_ps_activation_count = 0;
    ps->MR_ps_outermost_activation_ptr = NULL;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""rezero_activation_info_ac called when not using activation counts!"");
  #endif
#else
    MR_fatal_error(""rezero_activation_info_ac: deep profiling not enabled"");
#endif
}").

rezero_activation_info_ac :-
    impure private_builtin.imp,
    private_builtin.sorry("rezero_activation_info_ac").

:- pragma foreign_proc("C",
    rezero_activation_info_sr,
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifndef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    ps->MR_ps_outermost_activation_ptr = NULL;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""rezero_activation_info_sr called when using activation counts!"");
  #endif
#else
    MR_fatal_error(""rezero_activation_info_sr: deep profiling not enabled"");
#endif
}").

rezero_activation_info_sr :-
    impure private_builtin.imp,
    private_builtin.sorry("rezero_activation_info_sr").

:- pragma foreign_proc("C",
    reset_activation_info_ac(Count::in, Ptr::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifdef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    ps->MR_ps_activation_count = Count;
    ps->MR_ps_outermost_activation_ptr = Ptr;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""reset_activation_info_ac called when not using activation counts!"");
  #endif
#else
    MR_fatal_error(""reset_activation_info_ac: deep profiling not enabled"");
#endif
}").

reset_activation_info_ac(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("reset_activation_info_ac").

:- pragma foreign_proc("C",
    reset_activation_info_sr(Ptr::in),
    [thread_safe, will_not_call_mercury],
"{
#ifdef MR_DEEP_PROFILING
  #ifndef MR_USE_ACTIVATION_COUNTS
    MR_CallSiteDynamic      *csd;
    MR_ProcDynamic          *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

    MR_enter_instrumentation();
    csd = MR_current_call_site_dynamic;
    MR_deep_assert(csd, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    ps->MR_ps_outermost_activation_ptr = Ptr;
    MR_leave_instrumentation();
  #else
    MR_fatal_error(""reset_activation_info_sr called when using activation counts!"");
  #endif
#else
    MR_fatal_error(""reset_activation_info_sr: deep profiling not enabled"");
#endif
}").

reset_activation_info_sr(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("reset_activation_info_sr").

%---------------------------------------------------------------------------%
% Instances of increment_{static,dynamic}_coverage_point_count.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    increment_static_coverage_point_count(ProcLayout::in, CPIndex::in),
    [thread_safe, will_not_call_mercury],
    % The code of this predicate is duplicated bodily in deep_profiling.m
    % in the compiler directory, so any changes here should also be made there.
"
#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
    const MR_ProcLayout *pl;
    MR_ProcStatic       *ps;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""increment_coverage_point_count"");
        printf("", ProcLayout: 0x%x, CPIndex: %d\\n"", ProcLayout, CPIndex);
    }
  #endif

    pl = (const MR_ProcLayout *) ProcLayout;

    MR_deep_assert(NULL, NULL, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(NULL, pl, NULL, ps != NULL);

    MR_deep_assert(NULL, pl, ps, CPIndex >= ps->MR_ps_num_coverage_points);
    MR_deep_assert(NULL, pl, ps, ps->MR_ps_coverage_points != NULL);

    ps->MR_ps_coverage_points[CPIndex]++;

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""increment_static_coverage_point_count:  ""
            ""static coverage profiling not enabled"");
#endif // MR_DEEP_PROFILING_COVERAGE_STATIC
").

increment_static_coverage_point_count(_, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("increment_static_coverage_point_count").

:- pragma foreign_proc("C",
    increment_dynamic_coverage_point_count(CPIndex::in),
    [thread_safe, will_not_call_mercury],
    % The code of this predicate is duplicated bodily in deep_profiling.m
    % in the compiler directory, so any changes here should also be made there.
"
#ifdef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    const MR_CallSiteDynamic *csd;
    const MR_ProcDynamic *pd;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""increment_coverage_point_count"");
        printf("", CallSiteDynamic: 0x%x, CPIndex: %d\\n"",
            MR_current_call_site_dynamic, CPIndex);
    }
  #endif

    csd = MR_current_call_site_dynamic;

    MR_deep_assert(NULL, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;

    MR_deep_assert(csd, NULL, NULL, pd != NULL);

#ifdef MR_DEEP_CHECKS
    // Check that CPIndex is within bounds.
    {
        const MR_ProcLayout *pl;
        const MR_ProcStatic *ps;

        pl = pd->MR_pd_proc_layout;
        MR_deep_assert(csd, NULL, NULL, pl != NULL);
        ps = pl->MR_sle_proc_static;
        MR_deep_assert(csd, pl, NULL, ps != NULL);
        MR_deep_assert(csd, pl, ps, CPIndex >= ps->MR_ps_num_coverage_points);
    }
#endif

    MR_deep_assert(csd, NULL, NULL, pd->MR_pd_coverage_points != NULL);

    pd->MR_pd_coverage_points[CPIndex]++;

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""increment_dynamic_coverage_point_count:  ""
            ""dynamic deep profiling not enabled"");
#endif // MR_DEEP_PROFILING_COVERAGE_DYNAMIC
").

increment_dynamic_coverage_point_count(_) :-
    impure private_builtin.imp,
    private_builtin.sorry("increment_dynamic_coverage_point_count").

%---------------------------------------------------------------------------%
% Instances of save_recursion_depth_N.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    save_recursion_depth_1(CSD::in, CSN::in,
        OuterCount1::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSN, OuterCount1
#define MR_PROCNAME     ""save_recursion_depth_1""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1, CSN);                 \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_1(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_1").

:- pragma foreign_proc("C",
    save_recursion_depth_2(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2
#define MR_PROCNAME     ""save_recursion_depth_2""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_2(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_2").

:- pragma foreign_proc("C",
    save_recursion_depth_3(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
#define MR_PROCNAME     ""save_recursion_depth_3""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_3(_, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_3").

:- pragma foreign_proc("C",
    save_recursion_depth_4(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4
#define MR_PROCNAME     ""save_recursion_depth_4""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_4(_, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_4").

:- pragma foreign_proc("C",
    save_recursion_depth_5(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out, OuterCount5::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5
#define MR_PROCNAME     ""save_recursion_depth_5""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount5,                       \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_5(_, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_5").

:- pragma foreign_proc("C",
    save_recursion_depth_6(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out, OuterCount5::out, OuterCount6::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6
#define MR_PROCNAME     ""save_recursion_depth_6""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount5,                       \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount6,                       \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_6(_, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_6").

:- pragma foreign_proc("C",
    save_recursion_depth_7(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out, OuterCount5::out, OuterCount6::out,
        OuterCount7::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
#define MR_PROCNAME     ""save_recursion_depth_7""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount5,                       \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount6,                       \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount7,                       \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_7(_, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_7").

:- pragma foreign_proc("C",
    save_recursion_depth_8(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out, OuterCount5::out, OuterCount6::out,
        OuterCount7::out, OuterCount8::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8
#define MR_PROCNAME     ""save_recursion_depth_8""
#define MR_REC_DEPTH_BODY   {                                       \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount5,                       \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount6,                       \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount7,                       \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount8,                       \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_8(_, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_8").

:- pragma foreign_proc("C",
    save_recursion_depth_9(CSD::in, CSNsVector::in,
        OuterCount1::out, OuterCount2::out, OuterCount3::out,
        OuterCount4::out, OuterCount5::out, OuterCount6::out,
        OuterCount7::out, OuterCount8::out, OuterCount9::out),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8, OuterCount9
#define MR_PROCNAME     ""save_recursion_depth_9""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_SAVE_DEPTH_ACTION(OuterCount1,                       \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount2,                       \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount3,                       \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount4,                       \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount5,                       \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount6,                       \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount7,                       \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount8,                       \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                MR_SAVE_DEPTH_ACTION(OuterCount9,                       \\
                    MR_csn_vector_field(CSNsVector, 8));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

save_recursion_depth_9(_, _, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("save_recursion_depth_9").

%---------------------------------------------------------------------------%
% Instances of restore_recursion_depth_exit_N.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_1(CSD::in, CSN::in,
        OuterCount1::in),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSN, OuterCount1
#define MR_PROCNAME     ""restore_recursion_depth_exit_1""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1, CSN);                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_1(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_1").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_2(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2
#define MR_PROCNAME     ""restore_recursion_depth_exit_2""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_2(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_2").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_3(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
#define MR_PROCNAME     ""restore_recursion_depth_exit_3""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_3(_, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_3").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_4(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4
#define MR_PROCNAME     ""restore_recursion_depth_exit_4""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_4(_, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_4").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_5(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5
#define MR_PROCNAME     ""restore_recursion_depth_exit_5""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_5(_, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_5").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_6(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6
#define MR_PROCNAME     ""restore_recursion_depth_exit_6""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_6(_, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_6").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_7(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
#define MR_PROCNAME     ""restore_recursion_depth_exit_7""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_7(_, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_7").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_8(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in, OuterCount8::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8
#define MR_PROCNAME     ""restore_recursion_depth_exit_8""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount8,                      \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_8(_, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_8").

:- pragma foreign_proc("C",
    restore_recursion_depth_exit_9(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in, OuterCount8::in, OuterCount9::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8, OuterCount9
#define MR_PROCNAME     ""restore_recursion_depth_exit_9""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_EXIT(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount8,                      \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                MR_RESTORE_DEPTH_EXIT(OuterCount9,                      \\
                    MR_csn_vector_field(CSNsVector, 8));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_exit_9(_, _, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_exith_fail_9").

%---------------------------------------------------------------------------%
% Instances of restore_recursion_depth_fail_N.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_1(CSD::in, CSN::in,
        OuterCount1::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSN, OuterCount1
#define MR_PROCNAME     ""restore_recursion_depth_fail_1""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1, CSN);                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_1(_, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_1").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_2(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2
#define MR_PROCNAME     ""restore_recursion_depth_fail_2""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_2(_, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_2").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_3(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
#define MR_PROCNAME     ""restore_recursion_depth_fail_3""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_3(_, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_3").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_4(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4
#define MR_PROCNAME     ""restore_recursion_depth_fail_4""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_4(_, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_4").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_5(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5
#define MR_PROCNAME     ""restore_recursion_depth_fail_5""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_5(_, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_5").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_6(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6
#define MR_PROCNAME     ""restore_recursion_depth_fail_6""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_6(_, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_6").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_7(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
#define MR_PROCNAME     ""restore_recursion_depth_fail_7""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_7(_, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_7").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_8(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in, OuterCount8::in),
        [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8
#define MR_PROCNAME     ""restore_recursion_depth_fail_8""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount8,                      \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_8(_, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_8").

:- pragma foreign_proc("C",
    restore_recursion_depth_fail_9(CSD::in, CSNsVector::in,
        OuterCount1::in, OuterCount2::in, OuterCount3::in,
        OuterCount4::in, OuterCount5::in, OuterCount6::in,
        OuterCount7::in, OuterCount8::in, OuterCount9::in),
    [thread_safe, will_not_call_mercury],
"{
// shut up warning: CSD, CSNsVector, OuterCount1, OuterCount2, OuterCount3
// shut up warning: OuterCount4, OuterCount5, OuterCount6, OuterCount7
// shut up warning: OuterCount8, OuterCount9
#define MR_PROCNAME     ""restore_recursion_depth_fail_9""
#define MR_REC_DEPTH_BODY   {                                           \\
                MR_RESTORE_DEPTH_FAIL(OuterCount1,                      \\
                    MR_csn_vector_field(CSNsVector, 0));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount2,                      \\
                    MR_csn_vector_field(CSNsVector, 1));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount3,                      \\
                    MR_csn_vector_field(CSNsVector, 2));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount4,                      \\
                    MR_csn_vector_field(CSNsVector, 3));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount5,                      \\
                    MR_csn_vector_field(CSNsVector, 4));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount6,                      \\
                    MR_csn_vector_field(CSNsVector, 5));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount7,                      \\
                    MR_csn_vector_field(CSNsVector, 6));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount8,                      \\
                    MR_csn_vector_field(CSNsVector, 7));                \\
                MR_RESTORE_DEPTH_FAIL(OuterCount9,                      \\
                    MR_csn_vector_field(CSNsVector, 8));                \\
                }
#include ""mercury_deep_rec_depth_body.h""
#undef MR_PROCNAME
#undef MR_REC_DEPTH_BODY
}").

restore_recursion_depth_fail_9(_, _, _, _, _, _, _, _, _, _, _) :-
    impure private_builtin.imp,
    private_builtin.sorry("restore_recursion_depth_fail_9").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Erlang", "
    % These stubs are needed so that the Erlang compiler doesn't complain
    % about missing definitions for exported functions.

    det_call_port_code_ac_3_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    det_call_port_code_sr_4_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    det_exit_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    det_exit_port_code_sr_3_p_0(_, _, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_call_port_code_ac_4_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_call_port_code_sr_5_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_exit_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_exit_port_code_sr_3_p_0(_, _, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_fail_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_fail_port_code_sr_3_p_0(_, _, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_redo_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    non_redo_port_code_sr_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_call_port_code_ac_3_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_call_port_code_sr_4_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_exit_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_exit_port_code_sr_3_p_0(_, _, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_fail_port_code_ac_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
    semi_fail_port_code_sr_3_p_0(_, _, _) ->
        mercury__private_builtin:sorry_1_p_0(
            ""deep profiling in Erlang backend"").
").

%---------------------------------------------------------------------------%
