%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_costs.m
% Authors: pbone, zs.
%
% This module contains the code for computing costs of goals, as well as
% costs up to the time of the production or first consumption of a variable.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_costs.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.
:- import_module measurements.
:- import_module report.
:- import_module var_use_analysis.

:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred conj_calc_cost(list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

:- pred disj_calc_cost(detism_rep::in, list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

:- pred switch_calc_cost(list(case_rep(pard_goal_detail_annotation))::in,
    int::in, goal_cost_csq::out) is det.

:- pred ite_calc_cost(pard_goal_detail::in, pard_goal_detail::in,
    pard_goal_detail::in, goal_cost_csq::out) is det.

%---------------------------------------------------------------------------%

:- pred atomic_goal_build_use_map(atomic_goal_rep::in,
    reverse_goal_path::in, implicit_parallelism_info::in,
    var_use_type::in, var_rep::in,
    map(var_rep, lazy(var_use_info))::in,
    map(var_rep, lazy(var_use_info))::out) is det.

%---------------------------------------------------------------------------%

:- pred implicit_par_info_intermodule_var_use(implicit_parallelism_info::in,
    intermodule_var_use::out) is det.

%---------------------------------------------------------------------------%

:- pred recursion_type_get_interesting_parallelisation_depth(
    recursion_type, maybe(recursion_depth)).
:- mode recursion_type_get_interesting_parallelisation_depth(
    in(recursion_type_known_costs), out(maybe_yes(ground))) is det.
:- mode recursion_type_get_interesting_parallelisation_depth(
    in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module coverage.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module message.
:- import_module profile.
:- import_module program_representation_utils.

:- import_module bool.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

conj_calc_cost([], Calls, simple_goal_cost(Calls)).
conj_calc_cost([Conjunct | Conjuncts], _, Cost) :-
    Coverage = Conjunct ^ goal_annotation ^ pgd_coverage,
    get_coverage_after_det(Coverage, After),
    conj_calc_cost(Conjuncts, After, ConjunctsCost),
    ConjunctCost = Conjunct ^ goal_annotation ^ pgd_cost,
    Cost = add_goal_costs_seq(ConjunctCost, ConjunctsCost).

disj_calc_cost(Detism, Disjs, Calls, Cost) :-
    Solutions = detism_get_solutions(Detism),
    (
        ( Solutions = at_most_zero_rep
        ; Solutions = at_most_one_rep
        ),
        % This is a semidet or committed choice disjunction;
        % it has no backtracking.
        disj_calc_cost_semidet(Disjs, Calls, Cost)
    ;
        Solutions = at_most_many_rep,
        disj_calc_cost_nondet(Disjs, Calls, Cost)
    ).

:- pred disj_calc_cost_semidet(list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

disj_calc_cost_semidet([], Calls, simple_goal_cost(Calls)).
disj_calc_cost_semidet([Disjunct | Disjuncts], _, Cost) :-
    Coverage = Disjunct ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( if Before = 0 then
        % Avoid a divide by zero.
        Cost = dead_goal_cost
    else
        _Successes = After,
        Failures = Before - After,
        disj_calc_cost_semidet(Disjuncts, Failures, FailureCost),
        DisjunctCost = Disjunct ^ goal_annotation ^ pgd_cost,
        SuccessCost = atomic_goal_cost(After),
        BranchCost = add_goal_costs_branch(Before, FailureCost, SuccessCost),
        Cost = add_goal_costs_seq(DisjunctCost, BranchCost)
    ).

:- pred disj_calc_cost_nondet(list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

disj_calc_cost_nondet([], Calls, simple_goal_cost(Calls)).
disj_calc_cost_nondet([Disjunct | Disjuncts], Calls, Cost) :-
    Coverage = Disjunct ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_det(Coverage, Before),
    ( if Before = 0 then
        % Avoid a divide by zero.
        Cost = dead_goal_cost
    else
        % TODO: This is very approximate, it calculates the percall cost.
        % For nondet code we probably want the per-call and per-redo cost.
        disj_calc_cost_nondet(Disjuncts, Calls, DisjunctsCost),
        DisjunctCost = Disjunct ^ goal_annotation ^ pgd_cost,
        Cost = add_goal_costs_seq(DisjunctCost, DisjunctsCost)
    ).

switch_calc_cost([], Calls, simple_goal_cost(Calls)).
switch_calc_cost([Case | Cases], TotalCalls, Cost) :-
    ( if TotalCalls = 0 then
        % Avoid a divide by zero.
        Cost = dead_goal_cost
    else
        Coverage = Case ^ cr_case_goal ^ goal_annotation ^ pgd_coverage,
        get_coverage_before_det(Coverage, CaseCalls),
        switch_calc_cost(Cases, TotalCalls - CaseCalls, CasesCost),
        CaseCost = Case ^ cr_case_goal ^ goal_annotation ^ pgd_cost,
        Cost = add_goal_costs_branch(TotalCalls, CaseCost, CasesCost)
    ).

ite_calc_cost(Cond, Then, Else, Cost) :-
    CondCost = Cond ^ goal_annotation ^ pgd_cost,
    ThenCost = Then ^ goal_annotation ^ pgd_cost,
    ElseCost = Else ^ goal_annotation ^ pgd_cost,
    Coverage = Cond ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_det(Coverage, Before),
    ThenElseCost = add_goal_costs_branch(Before, ThenCost, ElseCost),
    Cost = add_goal_costs_seq(CondCost, ThenElseCost).

:- func simple_goal_cost(int) = goal_cost_csq.

simple_goal_cost(Calls) = Cost :-
    ( if Calls = 0 then
        Cost = dead_goal_cost
    else
        Cost = atomic_goal_cost(Calls)
    ).

%---------------------------------------------------------------------------%

atomic_goal_build_use_map(AtomicGoal, RevGoalPath, Info, VarUseType, Var,
        !Map) :-
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        (
            VarUseType = var_use_consumption,
            CostUntilUse = 0.0
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            CostUntilUse = 1.0
        ),
        LazyUse = val(var_use_info(CostUntilUse, 1.0, VarUseType))
    ;
        IsCall = atomic_goal_is_call(Args),
        LazyUse = delay(
            (func) = compute_var_use_lazy(Info, RevGoalPath, Var,
                Args, VarUseType))
    ),
    map.det_insert(Var, LazyUse, !Map).

:- func compute_var_use_lazy(implicit_parallelism_info, reverse_goal_path,
    var_rep, list(var_rep), var_use_type) = var_use_info.

compute_var_use_lazy(Info, RevGoalPath, Var, Args, VarUseType) = EarliestUse :-
    CliquePtr = Info ^ ipi_clique,
    map.lookup(Info ^ ipi_call_sites, RevGoalPath, CostAndCallee),
    ( if
        cost_and_callees_is_recursive(CliquePtr, CostAndCallee),
        map.search(Info ^ ipi_rec_call_sites, RevGoalPath, RecCost)
    then
        Cost = RecCost
    else
        Cost = CostAndCallee ^ cac_cost
    ),

    compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee,
        Cost, VarUseType, Uses),
    (
        VarUseType = var_use_consumption,
        (
            Uses = [],
            unexpected($pred, "No uses")
        ;
            Uses = [FirstUse | OtherUses],
            find_earliest_use(FirstUse, OtherUses, EarliestUse)
        )
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        (
            Uses = [],
            unexpected($pred, "No uses")
        ;
            Uses = [EarliestUse]
        ;
            Uses = [_, _ | _],
            unexpected($pred, "Too many uses")
        )
    ).

:- pred find_earliest_use(var_use_info::in, list(var_use_info)::in,
    var_use_info::out) is det.

find_earliest_use(CurEarliest, [], CurEarliest).
find_earliest_use(CurEarliest, [HeadVarUse | TailVarUses], Earliest) :-
    TimeCur = CurEarliest ^ vui_cost_until_use,
    TimeHead = HeadVarUse ^ vui_cost_until_use,
    ( if TimeCur < TimeHead then
        NextEarliest = CurEarliest
    else
        NextEarliest = HeadVarUse
    ),
    find_earliest_use(NextEarliest, TailVarUses, Earliest).

:- pred compute_var_use_lazy_arg(implicit_parallelism_info::in, var_rep::in,
    list(var_rep)::in, cost_and_callees::in, cs_cost_csq::in, var_use_type::in,
    list(var_use_info)::out) is det.

compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee, Cost, VarUseType,
        Uses) :-
    ( if 0.0 < cs_cost_get_calls(Cost) then
        CostPercall = cs_cost_get_percall(Cost),
        list.member_indexes0(Var, Args, ArgNums),
        (
            ArgNums = [_ | _],
            HigherOrder = CostAndCallee ^ cac_call_site_is_ho,
            (
                HigherOrder = higher_order_call,
                % We cannot push signals or waits into higher order calls.
                pessimistic_var_use_info(VarUseType, CostPercall, Use),
                Uses = [Use]
            ;
                HigherOrder = first_order_call,
                ( if
                    is_singleton(CostAndCallee ^ cac_callees, CalleePrime)
                then
                    Callee = CalleePrime
                else
                    unexpected($pred,
                        "first-order call site has wrong number of CSDs")
                ),
                CSDPtr = Callee ^ c_csd,
                RecursionType = Info ^ ipi_recursion_type,
                recursion_type_get_interesting_parallelisation_depth(
                    RecursionType, MaybeCurDepth),
                list.map(
                    compute_var_use_2(Info, RecursionType,
                        MaybeCurDepth, VarUseType, CostPercall, CSDPtr),
                    ArgNums, Uses0),
                list.sort_and_remove_dups(Uses0, Uses)
            )
        ;
            ArgNums = [],
            ( if VarUseType = var_use_consumption then
                Uses = [var_use_info(0.0, CostPercall, VarUseType)]
            else
                unexpected($pred,
                    "Var use type most be consumption if " ++
                    "\\+ member(Var, Args)")
            )
        )
    else
        % This call site is never called.
        pessimistic_var_use_info(VarUseType, 0.0, Use),
        Uses = [Use]
    ).

:- pred compute_var_use_2(implicit_parallelism_info::in,
    recursion_type::in, maybe(recursion_depth)::in, var_use_type::in,
    float::in, call_site_dynamic_ptr::in, int::in, var_use_info::out) is det.

compute_var_use_2(Info, RecursionType, MaybeCurDepth, VarUseType, Cost,
        CSDPtr, ArgNum, Use) :-
    Deep = Info ^ ipi_deep,
    CliquePtr = Info ^ ipi_clique,
    implicit_par_info_intermodule_var_use(Info, FollowCallsAcrossModules),
    VarUseOptions = var_use_options(Deep, FollowCallsAcrossModules,
        VarUseType),
    get_call_site_dynamic_var_use_info_rec_level(CliquePtr, CSDPtr, ArgNum,
        RecursionType, MaybeCurDepth, Cost, set.init, VarUseOptions, MaybeUse),
    (
        MaybeUse = ok(Use)
    ;
        MaybeUse = error(Error),
        pessimistic_var_use_info(VarUseType, Cost, Use),
        append_message(pl_csd(CSDPtr),
            warning_cannot_compute_first_use_time(Error),
            cord.empty, Messages),
        trace [io(!IO)] (
            stderr_stream(Stderr, !IO),
            write_out_messages(Stderr, Messages, !IO)
        )
    ).

%---------------------------------------------------------------------------%

implicit_par_info_intermodule_var_use(Info, FollowCallsAcrossModules) :-
    IntermoduleVarUse = Info ^ ipi_opts ^ cpcp_intermodule_var_use,
    (
        IntermoduleVarUse = yes,
        FollowCallsAcrossModules = follow_any_call
    ;
        IntermoduleVarUse = no,
        ProcLabel = Info ^ ipi_proc_label,
        ( ProcLabel = str_ordinary_proc_label(_, _, Module, _, _, _)
        ; ProcLabel = str_special_proc_label(_, _, Module, _, _, _)
        ),
        FollowCallsAcrossModules = follow_calls_into_module(Module)
    ).

%---------------------------------------------------------------------------%

recursion_type_get_interesting_parallelisation_depth(RecursionType,
        MaybeDepth) :-
    (
        RecursionType = rt_not_recursive,
        MaybeDepth = yes(recursion_depth_from_float(0.0))
    ;
        RecursionType = rt_single(_, _, _DepthF, _, _),
        % The interesting recursion depth is at the bottom of the recursion, if
        % we can't parallelise here then there's no point parallelising the
        % loop in general.
        % XXX: Update metrics to understand that this is a loop.
        MaybeDepth = yes(recursion_depth_from_float(2.0))
    ;
        ( RecursionType = rt_divide_and_conquer(_, _)
        ; RecursionType = rt_mutual_recursion(_)
        ; RecursionType = rt_other(_)
        ; RecursionType = rt_errors(_)
        ),
        MaybeDepth = no
    ).

%---------------------------------------------------------------------------%
:- end_module autopar_costs.
%---------------------------------------------------------------------------%
