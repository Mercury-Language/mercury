%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: pbone, zs.
%
% This file implements the coverage propagation algorithm, which attaches
% coverage information to the component goals of a procedure body.
%
%-----------------------------------------------------------------------------%

:- module var_use_analysis.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module profile.
:- import_module report.

:- import_module list.
:- import_module maybe.
:- import_module set.

    % Gives information about the use of a variable measured in average call
    % sequence counts since either the beginning or the end of the procedure.
    %
:- type var_use_info
    --->    var_use_info(
                vui_cost_until_use          :: cost_until_var_use,
                vui_use_type                :: var_use_type
            ).

:- type var_use_type
    --->    var_use_production
            % The variable is produced: free >> ground
    ;       var_use_consumption
            % The variable is consumed: free >> free
    ;       var_use_other.
            % The variable is used in some other way.

:- type cost_until_var_use
    --->    cost_since_proc_start(float)
    ;       cost_before_proc_end(float).

:- func cost_until_to_cost_since_start(cost_until_var_use, float) = float.
:- func cost_until_to_cost_before_end(cost_until_var_use, float) = float.

    % generic_vars_first_use(HeadVarsToVars, Deep, PSPtr, CallStack,
    %   VarUseInfos, ProcAverageCost).
    %
    % Find the first uses of the given variables.
    %
    % CallStack is used to prevent unbound recursion; initialise it to
    % set.init.
    %
:- pred generic_vars_first_use(
    pred(list(head_var_rep), list(var_rep), list(var_use_type))
        ::in(pred(in, out, out) is det),
    deep::in, proc_static_ptr::in, set(proc_static_ptr)::in,
    maybe_error(proc_var_use_dump_info)::out) is det.

:- pred head_vars_all(list(head_var_rep)::in, list(var_rep)::out,
    list(var_use_type)::out) is det.

:- pred var_mode_to_var_use(var_mode_rep::in, var_use_type::out) is det.

:- pred pessimistic_var_use_info(var_use_type::in, var_use_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module coverage.
:- import_module create_report.
:- import_module program_representation_utils.

:- import_module float.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

cost_until_to_cost_since_start(cost_since_proc_start(Cost), _WholeCost) =
    Cost.
cost_until_to_cost_since_start(cost_before_proc_end(Cost), WholeCost) =
    WholeCost - Cost.

cost_until_to_cost_before_end(cost_since_proc_start(Cost), WholeCost) =
    WholeCost - Cost.
cost_until_to_cost_before_end(cost_before_proc_end(Cost), _WholeCost) =
    Cost.

%-----------------------------------------------------------------------------%

    % This type represents whether the first use of a variable has been found
    % or not. If it has then the call sequence counts since it was found is
    % stored in this type also.
    %
:- type found_first_use
    --->    have_not_found_first_use
    ;       found_first_use(
                cost_before_use     :: float
            ).

:- inst found_first_use_found
    --->    found_first_use(ground).

:- type var_first_use_static_info
    --->    var_first_use_static_info(
                fui_deep                :: deep,
                fui_call_site_map       :: map(goal_path, call_site_perf),
                fui_var                 :: var_rep,
                fui_var_use             :: var_use_type,

                % A set of call sites whose analysis has started but not yet
                % completed. We keep this set to prevent infinite recursion
                % in the analysis itself.
                fui_call_stack          :: set(proc_static_ptr)
            ).

    % Find the first use of a variable in a goal.
    % Procedure calls can be resolved via the call site which we'll need to
    % lookup anyway to find cost information, This will callback to the deep
    % profiler as it crosses procedure boundaries.
    %
    % This does not follow higher order or method calls. It may be possible to
    % follow call the calls seen during profiling and aggregate their variable
    % use information based on how often they are called from that call site.
    %
:- pred goal_var_first_use(goal_path::in, goal_rep(coverage_info)::in,
    var_first_use_static_info::in, float::in, float::out, found_first_use::out)
    is det.

goal_var_first_use(GoalPath, Goal, StaticInfo, !CostSoFar, FoundFirstUse) :-
    Goal = goal_rep(GoalExpr, Detism, _Coverage),
    (
        GoalExpr = conj_rep(Conjuncts),
        conj_var_first_use(GoalPath, 1, Conjuncts, StaticInfo, !CostSoFar,
            FoundFirstUse)
    ;
        GoalExpr = disj_rep(Disjuncts),
        disj_var_first_use(GoalPath, Disjuncts, Detism, StaticInfo,
            !CostSoFar, FoundFirstUse)
    ;
        GoalExpr = switch_rep(SwitchedOnVar, _CanFail, Cases),
        switch_var_first_use(GoalPath, SwitchedOnVar, Cases,
            StaticInfo, !CostSoFar, FoundFirstUse)
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ite_var_first_use(GoalPath, Cond, Then, Else, StaticInfo, !CostSoFar,
            FoundFirstUse)
    ;
        (
            GoalExpr = negation_rep(SubGoal),
            SubGoalPath = goal_path_add_at_end(GoalPath, step_neg)
        ;
            GoalExpr = scope_rep(SubGoal, ScopeIsCut),
            SubGoalPath = goal_path_add_at_end(GoalPath, step_scope(ScopeIsCut))
        ),
        goal_var_first_use(SubGoalPath, SubGoal, StaticInfo, !CostSoFar,
            FoundFirstUse)
    ;
        GoalExpr = atomic_goal_rep(_, _, BoundVars, AtomicGoal),
        (
            ( AtomicGoal = plain_call_rep(_, _, _)
            ; AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            call_var_first_use(AtomicGoal, BoundVars, GoalPath, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            ( AtomicGoal = unify_construct_rep(_, _, _)
            ; AtomicGoal = unify_deconstruct_rep(_, _, _)
            ; AtomicGoal = partial_construct_rep(_, _, _)
            ; AtomicGoal = partial_deconstruct_rep(_, _, _)
            ; AtomicGoal = unify_assign_rep(_, _)
            ; AtomicGoal = cast_rep(_, _)
            ; AtomicGoal = unify_simple_test_rep(_, _)
            ; AtomicGoal = pragma_foreign_code_rep(_)
            ; AtomicGoal = event_call_rep(_, _)
            ; AtomicGoal = builtin_call_rep(_, _, _)
            ),
            % trivial goals have a zero cost, so !CostSoFar is not updated.
            atomic_trivial_var_first_use(AtomicGoal, BoundVars, !.CostSoFar,
                StaticInfo, FoundFirstUse)
        )
    ),
    trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
        io.format("Trace: goal_var_first_use: %s\n",
            [s(goal_path_to_string(GoalPath))], !IO)
    ).

:- inst atomic_goal_rep_call
    --->    plain_call_rep(ground, ground, ground)
    ;       higher_order_call_rep(ground, ground)
    ;       method_call_rep(ground, ground, ground).

:- pred call_var_first_use(atomic_goal_rep::in(atomic_goal_rep_call),
    list(var_rep)::in, goal_path::in, var_first_use_static_info::in,
    float::in, float::out, found_first_use::out) is det.

call_var_first_use(AtomicGoal, BoundVars, GoalPath, StaticInfo,
        CostSoFar, NextCostSoFar, FoundFirstUse) :-
    Var = StaticInfo ^ fui_var,
    VarUseType = StaticInfo ^ fui_var_use,
    map.lookup(StaticInfo ^ fui_call_site_map, GoalPath, CallSitePerf),

    % Calculate the cost of this call and add it to the cost so far.
    CallSitePerf ^ csf_summary_perf ^ perf_row_maybe_total =
        MaybeTotal,
    (
        MaybeTotal = yes(RowData)
    ;
        MaybeTotal = no,
        CallSitePerf ^ csf_summary_perf ^ perf_row_self =
            RowData
    ),
    ProcCost = RowData ^ perf_row_callseqs_percall,
    % XXX: this doesn't work for (mutually-)recursive calls, the deep profiler
    % sets their cost to 1.0. For now we just have to hope that the variables
    % we're searching for are used in the recursive call so the trick below
    % works.
    NextCostSoFar = CostSoFar + ProcCost,

    % Determine if the variable we're searching for uses of is involved with
    % this call.
    (
        AtomicGoal = plain_call_rep(_, _, Args),
        (
            nth_member_search(Args, Var, ArgNum),
            (
                VarUseType = var_use_consumption
            ;
                % If we're looking for a production ensure that this call binds
                % this variable.
                VarUseType = var_use_production,
                member(Var, BoundVars)
            ;
                VarUseType = var_use_other
            )
        ->
            (
                CallSitePerf ^ csf_kind =
                    normal_call_and_info(Callee)
            ->
                PSPtr = Callee ^ nci_callee_desc ^ pdesc_ps_ptr,
                CallStack0 = StaticInfo ^ fui_call_stack,
                ( contains(CallStack0, PSPtr) ->
                    % Don't follow recursive or mutually recursive calls.
                    % XXX: I'd like to create the result that is the sum of the
                    % recursive expression: Cost(i) = Cost + Cost(i - 1).
                    % Note: this makes a pessimistic assumption instead. Note:
                    % It doesn't matter what type of variable use we're
                    % searching for either the cost before a consumer is 0.0 or
                    % the cost after a producer is 0.0. So asserting
                    % TimeBeforeUse = 0.0 works in both cases.
                    ProcCostUntilUse = 0.0
                ;
                    CallStack = set.insert(CallStack0, PSPtr),
                    proc_var_first_use(StaticInfo ^ fui_deep, PSPtr, ArgNum,
                        VarUseType, CallStack, ProcVarUseInfo),
                    ProcVarUseInfo = var_use_info(ProcCostUntilUse0, _),
                    (
                        VarUseType = var_use_consumption,
                        (
                            ProcCostUntilUse0 =
                                cost_since_proc_start(ProcCostUntilUse)
                        ;
                            ProcCostUntilUse0 =
                                cost_before_proc_end(ProcCostAfterUse),
                            ProcCostUntilUse = ProcCost - ProcCostAfterUse
                        )
                    ;
                        ( VarUseType = var_use_production
                        ; VarUseType = var_use_other
                        ),
                        (
                            ProcCostUntilUse0 =
                                cost_since_proc_start(ProcCostBeforeUse),
                            ProcCostUntilUse = ProcCost - ProcCostBeforeUse
                        ;
                            ProcCostUntilUse0 =
                                cost_before_proc_end(ProcCostUntilUse)
                        )
                    )
                )
            ;
                % Some builtin calls show up as plain calls in the procedure
                % representation. Namely builtin.compare. In these cases use
                % a pessimistic default.
                ProcCostUntilUse = 0.0
            ),
            FoundFirstUse = found_first_use(CostSoFar + ProcCostUntilUse),
            trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
                io.format(
                    "Trace: Set first use info for variable use in call: %s\n",
                    [s(string(FoundFirstUse))], !IO)
            )
        ;
            FoundFirstUse = have_not_found_first_use
        )
    ;
        ( AtomicGoal = higher_order_call_rep(HOVar, Args)
        % The first argument of this functor is really the type info variable,
        % not a higher order term.
        ; AtomicGoal = method_call_rep(HOVar, _, Args)
        ),
        (
            ( HOVar = Var
            ; member(Var, Args)
            )
        ->
            % XXX: Make a pessimistic default, since we don't bother to perform
            % this analysis recursively for higher order or method calls.
            FoundFirstUse = found_first_use(CostSoFar)
        ;
            FoundFirstUse = have_not_found_first_use
        )
    ).

:- inst atomic_trivial_goal_rep
    --->    unify_construct_rep(ground, ground, ground)
    ;       unify_deconstruct_rep(ground, ground, ground)
    ;       partial_construct_rep(ground, ground, ground)
    ;       partial_deconstruct_rep(ground, ground, ground)
    ;       unify_assign_rep(ground, ground)
    ;       cast_rep(ground, ground)
    ;       unify_simple_test_rep(ground, ground)
    ;       pragma_foreign_code_rep(ground)
    ;       event_call_rep(ground, ground)
    ;       builtin_call_rep(ground, ground, ground).

:- pred atomic_trivial_var_first_use(
    atomic_goal_rep::in(atomic_trivial_goal_rep), list(var_rep)::in, float::in,
    var_first_use_static_info::in, found_first_use::out) is det.

atomic_trivial_var_first_use(AtomicGoal, BoundVars, CostSoFar, StaticInfo,
        FoundFirstUse) :-
    Var = StaticInfo ^ fui_var,
    VarUseType = StaticInfo ^ fui_var_use,
    atomic_goal_get_vars(AtomicGoal, Vars),
    (
        member(Var, Vars),
        (
            VarUseType = var_use_consumption
        ;
            VarUseType = var_use_production,
            member(Var, BoundVars)
        ;
            VarUseType = var_use_other
        )
    ->
        FoundFirstUse = found_first_use(CostSoFar)
    ;
        FoundFirstUse = have_not_found_first_use
    ).

    % Find the first use of a variable within a conjunction. Note that when
    % looking for a production of the variable we search backward and add the
    % time from the end of the goal. Similarly with other goal types that have
    % an execution order, namely disjunctions and if-then-elses.
    %
:- pred conj_var_first_use(goal_path::in, int::in,
    list(goal_rep(coverage_info))::in, var_first_use_static_info::in,
    float::in, float::out, found_first_use::out) is det.

conj_var_first_use(_, _, [], _, !Cost, have_not_found_first_use).
conj_var_first_use(GoalPath, ConjNum, [Conj | Conjs], StaticInfo, !CostSoFar,
        FoundFirstUse) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjNum)),
    VarUseType = StaticInfo ^ fui_var_use,
    (
        VarUseType = var_use_consumption,
        goal_var_first_use(ConjGoalPath, Conj, StaticInfo, !CostSoFar,
            HeadFoundFirstUse),
        conj_var_first_use(GoalPath, ConjNum + 1, Conjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse)
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        conj_var_first_use(GoalPath, ConjNum + 1, Conjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse),
        goal_var_first_use(ConjGoalPath, Conj, StaticInfo, !CostSoFar,
            HeadFoundFirstUse)
    ),
    (
        % XXX: if a variable is bound more than once, because it's used with
        % partial instantiation then we want to use the last time it is bound.
        % Instmaps can be used to track this. This is relevant when searching
        % for the producer of a variable.
        HeadFoundFirstUse = found_first_use(_),
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = have_not_found_first_use,
        FoundFirstUse = TailFoundFirstUse
    ).

:- pred disj_var_first_use(goal_path::in, list(goal_rep(coverage_info))::in,
    detism_rep::in, var_first_use_static_info::in, float::in, float::out,
    found_first_use::out) is det.

disj_var_first_use(GoalPath, Disjuncts, Detism, StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    % We cannot handle nondet/multi disjunctions. So we use pessimistic
    % defaults for FoundFirstUse if this disjunction is nondet or multi.
    % For calculating the cost of the disjunction, assume that is is a semidet
    % disjunction. Doing this will find the incorrect cost for the
    % disjunction, however disjunctions occur rarely, this is not likely to
    % drametically effect anything.
    CostBeforeConsumption = !.CostSoFar,
    CostAfterProduction = !.CostSoFar,
    disj_var_first_use_2(GoalPath, 1, Disjuncts, StaticInfo,
        !CostSoFar, FoundFirstUse0),
    (
        detism_get_solutions(Detism) = at_most_many_rep,
        FoundFirstUse0 = found_first_use(_)
    ->
        VarUseType = StaticInfo ^ fui_var_use,
        (
            VarUseType = var_use_consumption,
            FoundFirstUse = found_first_use(CostBeforeConsumption)
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            FoundFirstUse = found_first_use(CostAfterProduction)
        )
    ;
        FoundFirstUse = FoundFirstUse0
    ).

:- pred disj_var_first_use_2(goal_path::in, int::in,
    list(goal_rep(coverage_info))::in, var_first_use_static_info::in,
    float::in, float::out, found_first_use::out) is det.

disj_var_first_use_2(_, _, [], _, !CostSoFar, have_not_found_first_use).
disj_var_first_use_2(GoalPath, DisjNum, [Disj | Disjs], StaticInfo, !CostSoFar,
        FoundFirstUse) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    VarUseType = StaticInfo ^ fui_var_use,
    (
        VarUseType = var_use_consumption,
        goal_var_first_use(DisjGoalPath, Disj, StaticInfo, !CostSoFar,
            HeadFoundFirstUse),
        disj_var_first_use_2(GoalPath, DisjNum + 1, Disjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse)
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        disj_var_first_use_2(GoalPath, DisjNum + 1, Disjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse),
        goal_var_first_use(DisjGoalPath, Disj, StaticInfo, !CostSoFar,
            HeadFoundFirstUse)
    ),
    (
        HeadFoundFirstUse = have_not_found_first_use,
        TailFoundFirstUse = have_not_found_first_use,
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = have_not_found_first_use,
        TailFoundFirstUse = found_first_use(_),
        FoundFirstUse = TailFoundFirstUse
    ;
        HeadFoundFirstUse = found_first_use(_),
        TailFoundFirstUse = have_not_found_first_use,
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = found_first_use(HeadCost),
        TailFoundFirstUse = found_first_use(TailCost),
        (
            VarUseType = var_use_consumption,
            % The variable is probably consumed in the first disjunct even if
            % it fails. This is also the pessimistic default.
            Cost = HeadCost
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            % Use a weighted average to reflect the likely success of the first
            % disjunct.
            ( get_coverage_before(Disj ^ goal_annotation, HeadCount) ->
                HeadWeight = float(HeadCount)
            ;
                error(this_file ++ " unknown coverage before disjunct")
            ),
            (
                Disjs = [],
                TailWeight = 0.0
            ;
                Disjs = [FirstTailDisj | _],
                FirstTailCoverage = FirstTailDisj ^ goal_annotation,
                ( get_coverage_before(FirstTailCoverage, TailCount) ->
                    TailWeight = float(TailCount)
                ;
                    error(this_file ++ " unknown coverage before disjunct")
                )
            ),
            weighted_average([HeadWeight, TailWeight], [HeadCost, TailCost],
                Cost)
        ),
        FoundFirstUse = found_first_use(Cost)
    ).

:- pred switch_var_first_use(goal_path::in, var_rep::in,
    list(case_rep(coverage_info))::in, var_first_use_static_info::in,
    float::in, float::out, found_first_use::out) is det.

switch_var_first_use(GoalPath, SwitchedOnVar, Cases, StaticInfo,
        CostBeforeSwitch, CostAfterSwitch, FoundFirstUse) :-
    switch_var_first_use_2(GoalPath, 1, StaticInfo, Cases, CaseWeights,
        CostBeforeSwitch, CostCases, FoundFirstUseCases),
    weighted_average(CaseWeights, CostCases, CostAfterSwitch),
    Var = StaticInfo ^ fui_var,
    ( Var = SwitchedOnVar ->
        % This can only possibly be a consumption of this variable.
        FoundFirstUse = found_first_use(CostBeforeSwitch)
    ;
        ( list.all_true(unify(have_not_found_first_use), FoundFirstUseCases) ->
            % No case contained a first-use of this variable.
            FoundFirstUse = have_not_found_first_use
        ;
            VarUseType = StaticInfo ^ fui_var_use,
            % XXX: These are the wrong way around.
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostBeforeSwitch
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostAfterSwitch
            ),
            list.map(ffu_to_float(DefaultCost), FoundFirstUseCases,
                FirstUseTimes),
            weighted_average(CaseWeights, FirstUseTimes, AvgFirstUseTime),
            FoundFirstUse = found_first_use(AvgFirstUseTime)
        )
    ).

:- pred switch_var_first_use_2(goal_path::in, int::in,
    var_first_use_static_info::in, list(case_rep(coverage_info))::in,
    list(float)::out, float::in, list(float)::out, list(found_first_use)::out)
    is det.

switch_var_first_use_2(_, _, _, [], [], _, [], []).
switch_var_first_use_2(GoalPath, CaseNum, StaticInfo, [Case | Cases],
        [Weight | Weights], Cost0, [Cost | Costs],
        [FoundFirstUse | FoundFirstUses]) :-
    switch_var_first_use_2(GoalPath, CaseNum + 1, StaticInfo, Cases, Weights,
        Cost0, Costs, FoundFirstUses),
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),
    Case = case_rep(_, _, Goal),
    goal_var_first_use(CaseGoalPath, Goal, StaticInfo, Cost0, Cost,
        FoundFirstUse),
    Goal = goal_rep(_, _, Coverage),
    ( get_coverage_before(Coverage, BeforeCount) ->
        Weight = float(BeforeCount)
    ;
        error(this_file ++ "unknown coverage before switch case")
    ).

:- pred ite_var_first_use(goal_path::in, goal_rep(coverage_info)::in,
    goal_rep(coverage_info)::in, goal_rep(coverage_info)::in,
    var_first_use_static_info::in, float::in, float::out, found_first_use::out)
    is det.

ite_var_first_use(GoalPath, Cond, Then, Else, StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    (
        get_coverage_before(Then ^ goal_annotation, CountBeforeThen),
        get_coverage_before(Else ^ goal_annotation, CountBeforeElse)
    ->
        Weights = [float(CountBeforeThen), float(CountBeforeElse)]
    ;
        error(this_file ++
            "incomplete coverage information for if then else branches")
    ),
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    VarUseType = StaticInfo ^ fui_var_use,
    (
        VarUseType = var_use_consumption,
        CostBeforeITE = !.CostSoFar,
        goal_var_first_use(CondGoalPath, Cond, StaticInfo,
            CostBeforeITE, CostAfterCond, CondFoundFirstUse),
        goal_var_first_use(ThenGoalPath, Then, StaticInfo,
            CostAfterCond, CostAfterThen, ThenFoundFirstUse),
        goal_var_first_use(ElseGoalPath, Else, StaticInfo,
            CostAfterCond, CostAfterElse, ElseFoundFirstUse),
        weighted_average(Weights, [CostAfterThen, CostAfterElse],
            CostAfterITE),
        !:CostSoFar = CostAfterITE
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        CostAfterITE = !.CostSoFar,
        goal_var_first_use(ThenGoalPath, Then, StaticInfo,
            CostAfterITE, CostAfterThen, ThenFoundFirstUse),
        goal_var_first_use(ElseGoalPath, Else, StaticInfo,
            CostAfterITE, CostAfterElse, ElseFoundFirstUse),
        weighted_average(Weights, [CostAfterThen, CostAfterElse],
            CostAfterCond),
        goal_var_first_use(CondGoalPath, Cond, StaticInfo,
            CostAfterCond, CostBeforeITE, CondFoundFirstUse),
        !:CostSoFar = CostBeforeITE
    ),
    (
        CondFoundFirstUse = found_first_use(_),
        FoundFirstUse = CondFoundFirstUse
    ;
        CondFoundFirstUse = have_not_found_first_use,
        (
            ThenFoundFirstUse = have_not_found_first_use,
            ElseFoundFirstUse = have_not_found_first_use
        ->
            FoundFirstUse = have_not_found_first_use
        ;
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostAfterCond
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostAfterITE
            ),
            ffu_to_float(DefaultCost, ThenFoundFirstUse, ThenVarUseTime),
            ffu_to_float(DefaultCost, ElseFoundFirstUse, ElseVarUseTime),
            weighted_average(Weights, [ThenVarUseTime, ElseVarUseTime],
                VarUseTime),
            FoundFirstUse = found_first_use(VarUseTime),
            trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
                io.format("Trace: ITE: Weights: %s, Then: %f, Else: %f, " ++
                        "VarUseTime: %f\n",
                    [s(string(Weights)), f(ThenVarUseTime), f(ElseVarUseTime),
                        f(VarUseTime)],
                    !IO)
            )
        )
    ).

:- pred weighted_average(list(float)::in, list(float)::in, float::out) is det.

weighted_average(Weights, Values, Average) :-
    list.foldl2_corresponding(
        (pred(Value::in, Weight::in, Sum0::in, Sum::out,
                WeightSum0::in, WeightSum::out) is det :-
            Sum = Sum0 + (Value * Weight),
            WeightSum = WeightSum0 + Weight
        ), Values, Weights, 0.0, Total, 0.0, TotalWeight),
    ( abs(TotalWeight) < epsilon ->
        Average = 0.0
    ;
        Average = Total / TotalWeight
    ).

:- pred ffu_to_float(float::in, found_first_use::in, float::out) is det.

ffu_to_float(Default, have_not_found_first_use, Default).
ffu_to_float(_, found_first_use(CostBeforeUse), CostBeforeUse).

%----------------------------------------------------------------------------%

    % proc_var_first_use(Deep, PSPtr, N, VarUseType, CallStack, VarUseInfo).
    %
    % Find the first use of the Nth argument of the procedure given by PSPtr.
    %
:- pred proc_var_first_use(deep::in, proc_static_ptr::in, int::in,
    var_use_type::in, set(proc_static_ptr)::in, var_use_info::out) is det.

proc_var_first_use(Deep, PSPtr, ArgNum, VarUseType, CallStack, VarUseInfo) :-
    generic_vars_first_use(head_var_by_pos(ArgNum), Deep, PSPtr, CallStack,
        MaybeProcVarUseInfo),
    (
        MaybeProcVarUseInfo = ok(proc_var_use_dump_info(_, VarUseInfos)),
        ( VarUseInfos = [VarUseInfoPrime] ->
            VarUseInfo = VarUseInfoPrime
        ;
            error(this_file ++
                "Expecting exactly one result in proc_var_first_use")
        )
    ;
        MaybeProcVarUseInfo = error(_),
        % Some errors can be caused by trying to look up procedures that can't
        % be found. For example float.round_to_int is defined using foreign
        % code, when it gets inlined into another predicate the proc static
        % pointer points to the foreign code which can't be looked up even
        % though it uses a 'plain_call' call site.
        % Return a pessimistic default here.
        pessimistic_var_use_info(VarUseType, VarUseInfo)
    ),
    trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
        io.format("Trace: prog_var_first_use: %s\n",
            [s(string(PSPtr))], !IO)
    ).

:- pred head_var_by_pos(int::in, list(head_var_rep)::in,
    list(var_rep)::out, list(var_use_type)::out) is det.

head_var_by_pos(ArgPos, HeadVars, [Var], [VarUseType]) :-
    list.index1_det(HeadVars, ArgPos, HeadVar),
    HeadVar = head_var_rep(Var, VarMode),
    var_mode_to_var_use(VarMode, VarUseType).

head_vars_all(HeadVars, Vars, VarUseTypes) :-
    list.map2((pred(HeadVar::in, Var::out, VarUseType::out) is det :-
            HeadVar = head_var_rep(Var, VarMode),
            var_mode_to_var_use(VarMode, VarUseType)
        ), HeadVars, Vars, VarUseTypes).

var_mode_to_var_use(var_mode_rep(InitialInst, FinalInst), VarUseType) :-
    (
        InitialInst = ir_ground_rep,
        FinalInst = ir_ground_rep
    ->
        VarUseType = var_use_consumption
    ;
        InitialInst = ir_free_rep,
        FinalInst = ir_ground_rep
    ->
        VarUseType = var_use_production
    ;
        VarUseType = var_use_other
    ).

pessimistic_var_use_info(VarUseType, VarUseInfo) :-
    (
        VarUseType = var_use_consumption,
        CostUntilUse = cost_since_proc_start(0.0)
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        CostUntilUse = cost_before_proc_end(0.0)
    ),
    VarUseInfo = var_use_info(CostUntilUse, VarUseType).

    % Perform the var_first_use for the vars returned by the closure.
    %
generic_vars_first_use(VarsPred, Deep, PSPtr, CallStack, MaybeResult) :-
    create_proc_report(Deep, PSPtr, MaybeProcReport),
    (
        MaybeProcReport = ok(ProcReport),
        create_procrep_coverage_report(Deep, PSPtr, MaybeProcRepCoverage),
        (
            MaybeProcRepCoverage = ok(ProcRepCoverageInfo),
            ProcRepCoverageInfo = procrep_coverage_info(_, ProcRep),
            ProcDefnRep = ProcRep ^ pr_defn,
            HeadVars = ProcDefnRep ^ pdr_head_vars,
            VarsPred(HeadVars, Vars, VarUseTypes),
            ProcReport = proc_report(ProcSummary, CallSiteSummaries),
            MaybeTotal = ProcSummary ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(RowData)
            ;
                MaybeTotal = no,
                RowData = ProcSummary ^ perf_row_self
            ),
            ProcAverageCost = RowData ^ perf_row_callseqs_percall,
            GoalRep = ProcDefnRep ^ pdr_goal,
            list.foldl((pred(CSS::in, Map0::in, Map::out) is det :-
                    GoalPath = CSS ^ csf_summary_perf ^ perf_row_subject
                        ^ csdesc_goal_path,
                    map.det_insert(Map0, GoalPath, CSS, Map)
                ), CallSiteSummaries, map.init, CallSiteMap),
            list.map_corresponding(goal_var_first_use_wrapper(Deep, CallStack,
                CallSiteMap, GoalRep), Vars, VarUseTypes, VarUseInfos),
            MaybeResult =
                ok(proc_var_use_dump_info(ProcAverageCost, VarUseInfos))
        ;
            MaybeProcRepCoverage = error(Error),
            MaybeResult = error(Error)
        )
    ;
        MaybeProcReport = error(Error),
        MaybeResult = error(Error)
    ).

:- pred goal_var_first_use_wrapper(deep::in, set(proc_static_ptr)::in,
    map(goal_path, call_site_perf)::in, goal_rep(coverage_info)::in,
    var_rep::in, var_use_type::in, var_use_info::out) is det.

goal_var_first_use_wrapper(Deep, CallStack, CallSiteMap, Goal, Var,
        VarUseType, VarUseInfo) :-
    goal_var_first_use(empty_goal_path, Goal,
        var_first_use_static_info(Deep, CallSiteMap, Var, VarUseType,
            CallStack),
        0.0, _Cost, FoundFirstUse),
    (
        FoundFirstUse = found_first_use(CostUntilUseRaw),
        (
            VarUseType = var_use_consumption,
            CostUntilUse = cost_since_proc_start(CostUntilUseRaw)
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            CostUntilUse = cost_before_proc_end(CostUntilUseRaw)
        ),
        VarUseInfo = var_use_info(CostUntilUse, VarUseType)
    ;
        % If the first use has not been found, then use the average cost of the
        % procedure as the cost before the first use, since the variable is
        % never used.
        FoundFirstUse = have_not_found_first_use,
        pessimistic_var_use_info(VarUseType, VarUseInfo)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "var_use_analysis.m".

%-----------------------------------------------------------------------------%
