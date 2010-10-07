%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008, 2010 The University of Melbourne.
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
                vui_cost_until_use          :: float,
                vui_proc_cost               :: float,
                vui_use_type                :: var_use_type
            ).

:- type var_use_type
    --->    var_use_production
            % The variable is produced: free >> ground
    ;       var_use_consumption
            % The variable is consumed: free >> free
    ;       var_use_other.
            % The variable is used in some other way.

%-----------------------------------------------------------------------------%

:- func average_var_use(list(var_use_info)) = var_use_info.

:- pred var_mode_to_var_use_type(var_mode_rep::in, var_use_type::out) is det.

:- pred pessimistic_var_use_info(var_use_type::in, float::in, 
    var_use_info::out) is det.

:- pred pessimistic_var_use_time(var_use_type::in, float::in, float::out) 
    is det.

%-----------------------------------------------------------------------------%

    % call_site_dynamic_var_use_info(Deep, CSDPtr, ArgPos, RT, VarUseType,
    %   MaybeVarUseInfo):
    %
    % Lookup when the call site CSDPtr will produce ArgPos.  RT is the type of
    % recursion in the call site's parent clique and VarUseType is the type of
    % variable use that is expected, VarUseType is used to produce conservative
    % defaults if a callee cannot be analyzed.
    %
:- pred call_site_dynamic_var_use_info(deep::in, call_site_dynamic_ptr::in,
    int::in, recursion_type::in, float::in, var_use_type::in,
    maybe_error(var_use_info)::out) is det.

    % call_site_dynamic_var_use_info(Deep, CliquePtr, CSDPtr, ArgPos, RT,
    %   MaybeCurDepth, Cost, CallStack, VarUseType, MaybeVarUseInfo).
    %
    % As above.  This alternative should be used if searching starts at a
    % different recursion level or from within a current variable use analysis.
    %
    % CliquePtr is the current clique, MaybeCurDepth is the current recursion
    % depth within the clique if known, CallStack is the set of proc dynamics
    % that have already been explored.
    %
    % Cost is the cost of the call.
    %
:- pred call_site_dynamic_var_use_info(deep::in, clique_ptr::in,
    call_site_dynamic_ptr::in, int::in, recursion_type::in, maybe(float)::in,
    float::in, set(proc_dynamic_ptr)::in,
    var_use_type::in, maybe_error(var_use_info)::out) is det.

:- pred clique_var_use_info(deep::in, clique_ptr::in, int::in,
    var_use_type::in, maybe_error(var_use_info)::out) is det.

:- pred proc_dynamic_var_use_info(deep::in, clique_ptr::in,
    proc_dynamic_ptr::in, int::in, recursion_type::in, maybe(float)::in,
    float::in, set(proc_dynamic_ptr)::in, var_use_type::in,
    maybe_error(var_use_info)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module coverage.
:- import_module create_report.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module recursion_patterns.

:- import_module float.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module require.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

average_var_use(Uses) = var_use_info(CostUntilUse, AvgProcCost, Type) :-
    (
        Uses = [],
        error(this_file ++ "average_var_use: Cannot average zero items")
    ;
        Uses = [var_use_info(_, _, Type) | _],
        foldl2(sum_use_info_costs, Uses, 0.0, SumCost, 0.0, SumProcCost),
        Num = float(length(Uses)),
        CostUntilUse = SumCost / Num,
        AvgProcCost = SumProcCost / Num, 
        require(all_true((pred(var_use_info(_, _, TypeI)::in) is semidet :- 
                    Type = TypeI
                ), Uses),
            "average_var_use: Use types do not match")
    ).

:- pred sum_use_info_costs(var_use_info::in, float::in, float::out, 
    float::in, float::out) is det.

sum_use_info_costs(var_use_info(Cost, ProcCost, _), !AccCost, !AccProcCost) :-
    !:AccCost = !.AccCost + Cost,
    !:AccProcCost = !.AccProcCost + ProcCost.

%-----------------------------------------------------------------------------%

var_mode_to_var_use_type(var_mode_rep(InitialInst, FinalInst), VarUseType) :-
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

%-----------------------------------------------------------------------------%

pessimistic_var_use_info(VarUseType, ProcCost, VarUseInfo) :-
    pessimistic_var_use_time(VarUseType, ProcCost, CostUntilUse),
    VarUseInfo = var_use_info(CostUntilUse, ProcCost, VarUseType).

pessimistic_var_use_time(VarUseType, ProcCost, CostUntilUse) :-
    (
        VarUseType = var_use_consumption,
        CostUntilUse = 0.0
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        CostUntilUse = ProcCost
    ).

%-----------------------------------------------------------------------------%

    % XXX: If the CSD represents a closure then the argument position will be
    % incorrect.  This is not currently important as we assume that the
    % compiler will not push signals and waits into higher order calls.
    % Therefore this should never be called for a higher order call site.
    %
call_site_dynamic_var_use_info(Deep, CSDPtr, ArgPos, RT, Cost, VarUseType,
        MaybeVarUseInfo) :-
    deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
    deep_lookup_clique_index(Deep, CSD ^ csd_caller, ParentCliquePtr),
    recursion_type_get_maybe_avg_max_depth(RT, MaybeCurDepth),
    call_site_dynamic_var_use_info(Deep, ParentCliquePtr, CSDPtr, ArgPos, RT, 
        MaybeCurDepth, Cost, set.init, VarUseType, MaybeVarUseInfo).

call_site_dynamic_var_use_info(Deep, ParentCliquePtr, CSDPtr, ArgNum,
        RecursionType, MaybeDepth0, Cost, CallStack, VarUseType,
        MaybeVarUseInfo) :-
    deep_lookup_clique_maybe_child(Deep, CSDPtr, MaybeCalleeCliquePtr),
    ( 
        MaybeCalleeCliquePtr = yes(CalleeCliquePtr),
        % This is a non-recursive call site.
        clique_var_use_info(Deep, CalleeCliquePtr, ArgNum, VarUseType,
            MaybeVarUseInfo)
    ;
        MaybeCalleeCliquePtr = no,
        % This is a recursive call site.
        deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
        CalleePDPtr = CSD ^ csd_callee,
        ( member(CalleePDPtr, CallStack) ->
            % Don't follow this recursive call, doing so would make this
            % analysis recurse forever.  XXX: There should be a way to
            % compute and solve a finite series for this.
            pessimistic_var_use_info(VarUseType, Cost, VarUseInfo),
            MaybeVarUseInfo = ok(VarUseInfo)
        ;
            (
                MaybeDepth0 = yes(Depth0),
                % Depth is measured from the bottom, hence this is -1
                MaybeDepth = yes(Depth0 - 1.0)
            ;
                MaybeDepth0 = no,
                MaybeDepth = no
            ),
            proc_dynamic_var_use_info(Deep, ParentCliquePtr, CalleePDPtr,
                ArgNum, RecursionType, MaybeDepth, Cost, CallStack,
                VarUseType, MaybeVarUseInfo)
        )
    ).

clique_var_use_info(Deep, CliquePtr, ArgNum, VarUseType, MaybeVarUseInfo) :-
    deep_lookup_clique_parents(Deep, CliquePtr, CSDPtr),
    deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
    Cost = float(inherit_callseqs(CSDDesc)),
    find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstProc,
        _OtherProcs),
    (
        MaybeFirstProc = yes(FirstPDPtr),
        create_clique_recursion_costs_report(Deep, CliquePtr,
            MaybeRecursionReport),
        (
            MaybeRecursionReport = ok(RecursionReport),
            RecursionType = RecursionReport ^ crr_recursion_type
        ;
            MaybeRecursionReport = error(Error),
            RecursionType = rt_errors([Error])
        ),
        recursion_type_get_maybe_avg_max_depth(RecursionType, MaybeDepth),
        proc_dynamic_var_use_info(Deep, CliquePtr, FirstPDPtr, ArgNum,
            RecursionType, MaybeDepth, Cost, set.init, VarUseType,
            MaybeVarUseInfo)
    ;
        MaybeFirstProc = no,
        error(this_file ++ "Clique has no first procedure")
    ).

proc_dynamic_var_use_info(Deep, CliquePtr, PDPtr, ArgNum, RecursionType,
        MaybeDepth0, ProcCost, CallStack0, VarUseType, MaybeVarUseInfo) :-
    set.insert(CallStack0, PDPtr, CallStack),
    create_dynamic_procrep_coverage_report(Deep, PDPtr, MaybeProcrepCoverage),
    (
        MaybeProcrepCoverage = ok(ProcrepCoverage),
        ProcDefn = ProcrepCoverage ^ prci_proc_rep ^ pr_defn,
        HeadVars = ProcDefn ^ pdr_head_vars,
        ( index0(HeadVars, ArgNum, head_var_rep(Var, Mode)) ->
            var_mode_to_var_use_type(Mode, ComputedUse),
            ( VarUseType = ComputedUse ->
                true
            ;
                PDPtr = proc_dynamic_ptr(PDNum),
                error(format(
                    "%s: Var uses do not match, passed: %s calculated from "
                    ++ "procrep: %s, Arg %d in proc dynamic %d",
                    [s(this_file), s(string(VarUseType)),
                     s(string(ComputedUse)), i(ArgNum), i(PDNum)]))
            ),

            % Prepare callsite information.
            proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
            foldl(build_call_site_cost_and_callee_map(Deep),
                Slots, map.init, CallSiteCostMap),
            
            % We're following a recursive call, therefore we descend one level.
            (
                MaybeDepth0 = yes(Depth0),
                MaybeDepth = yes(Depth0 - 1.0)
            ;
                MaybeDepth0 = no,
                MaybeDepth = no
            ),
            build_recursive_call_site_cost_map(Deep, CliquePtr, PDPtr,
                RecursionType, MaybeDepth, MaybeRecursiveCallSiteCostMap),
            (
                MaybeRecursiveCallSiteCostMap = ok(RecursiveCallSiteCostMap)
            ;
                MaybeRecursiveCallSiteCostMap = error(_),
                % Try to compute var use information without recursion costs.
                RecursiveCallSiteCostMap = map.init
            ),

            % Do the actual computation.
            Goal = ProcDefn ^ pdr_goal,
            goal_var_first_use_wrapper(Deep, CliquePtr, CallStack,
                CallSiteCostMap, RecursiveCallSiteCostMap, RecursionType,
                MaybeDepth, Goal, ProcCost, Var, VarUseType, VarUseInfo),
            MaybeVarUseInfo = ok(VarUseInfo)
        ;
            PDPtr = proc_dynamic_ptr(PDNum),
            MaybeVarUseInfo = error(format(
                "proc_dynamic_var_use_info: ArgNum %d out of range for PD %d",
                [i(ArgNum), i(PDNum)]))
        )
    ;
        MaybeProcrepCoverage = error(Error),
        MaybeVarUseInfo = error(Error)
    ).

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
                fui_clique              :: clique_ptr,
                fui_call_site_map       :: map(goal_path, cost_and_callees),
                fui_rec_call_site_map   :: map(goal_path, cs_cost_csq),
                fui_var                 :: var_rep,
                fui_var_use             :: var_use_type,

                % A set of call sites whose analysis has started but not yet
                % completed. We keep this set to prevent infinite recursion
                % in the analysis itself.
                fui_call_stack          :: set(proc_dynamic_ptr),

                fui_recursion_type      :: recursion_type,
                fui_maybe_cur_depth     :: maybe(float)
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
    Goal = goal_rep(GoalExpr, Detism, Coverage),
    (
        % Do not bother exploring this goal if it is never entered.  Or never
        % finishes and we're looking for a production.
        (
            get_coverage_before(Coverage, 0)
        ;
            StaticInfo ^ fui_var_use = var_use_production, 
            (
                Detism = erroneous_rep
            ;
                Detism = failure_rep
            ;
                get_coverage_after(Coverage, 0)
            )
        )
    ->
        FoundFirstUse = have_not_found_first_use
    ;
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
            ite_var_first_use(GoalPath, Cond, Then, Else, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            (
                GoalExpr = negation_rep(SubGoal),
                SubGoalPath = goal_path_add_at_end(GoalPath, step_neg)
            ;
                GoalExpr = scope_rep(SubGoal, ScopeIsCut),
                SubGoalPath = goal_path_add_at_end(GoalPath,
                    step_scope(ScopeIsCut))
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
    StaticInfo = var_first_use_static_info(_Deep, CliquePtr, CostMap,
        RecCostMap, Var, VarUseType, _CallStack, _RecursionType,
        _MaybeCurDepth),
    map.lookup(CostMap, GoalPath, CostAndCallees),

    % Get the cost of the call.
    (
        cost_and_callees_is_recursive(CliquePtr, CostAndCallees),
        map.search(RecCostMap, GoalPath, CostPrime)
    ->
        Cost0 = CostPrime
    ;
        Cost0 = CostAndCallees ^ cac_cost
    ),
    ( cs_cost_get_calls(Cost0) = 0.0 ->
        Cost = 0.0
    ;
        Cost = cs_cost_get_percall(Cost0)
    ),
    NextCostSoFar = CostSoFar + Cost,
    
    % Determine if the variable we're searching for uses of is involved with
    % this call.
    (
        AtomicGoal = plain_call_rep(_, _, Args),
        Vars = Args
    ;
        ( AtomicGoal = higher_order_call_rep(HOVar, Args)
        ; AtomicGoal = method_call_rep(HOVar, _, Args)
        ),
        Vars = [HOVar | Args]
    ),
    ( member(Var, Vars) ->
        
        solutions((pred(TimeI::out) is nondet :-
                (
                    consume_ho_arg(AtomicGoal, Var, TimeI)
                ;
                    call_args_first_use(Args, Cost, StaticInfo, 
                        CostAndCallees, TimeI)
                )
            ), Times),
        (
            Times = [],
            error(this_file ++ ": No solutions for variable first use time")
        ;
            Times = [FirstTime | OtherTimes],
            FoundFirstUse = found_first_use(FirstTime + CostSoFar),
            (
                VarUseType = var_use_production
            =>
                OtherTimes = []
            ->
                true
            ;
                error(this_file ++ 
                    ": Multiple solutions for variable production time")
            )
        ),
        
        % Assertion
        (
            VarUseType = var_use_production
        =>
            member(Var, BoundVars)
        ->
            true
        ;
            error(this_file ++ 
                ": A bound var must be produced by a call if it's an argument.")
        ),
        (
            VarUseType = var_use_consumption
        =>
            not member(Var, BoundVars)
        ->
            true
        ;
            error(this_file ++
                ": A consumed var must not be mentioned in BoundVars.")
        ),
        (
            VarUseType = var_use_production
        =>
            not (
                ( AtomicGoal = higher_order_call_rep(Var, _)
                ; AtomicGoal = method_call_rep(Var, _, _)
                )
            )
        ->
            true
        ;
            error(this_file ++ 
                ": A HO call site cannot produce it's own HO value.")
        )
    ;
        FoundFirstUse = have_not_found_first_use
    ).

:- pred consume_ho_arg(atomic_goal_rep::in(atomic_goal_rep_call),
    var_rep::in, float::out) is semidet.

consume_ho_arg(higher_order_call_rep(Var, _), Var, 0.0).
consume_ho_arg(method_call_rep(Var, _, _), Var, 0.0).

:- pred call_args_first_use(list(var_rep)::in, float::in, 
    var_first_use_static_info::in, cost_and_callees::in, float::out) is nondet.

call_args_first_use(Args, Cost, StaticInfo, CostAndCallees, Time) :-
    StaticInfo = var_first_use_static_info(Deep, CliquePtr, _CostMap,
        _RecCostMap, Var, VarUseType, CallStack, RecursionType, MaybeCurDepth),
    HigherOrder = CostAndCallees ^ cac_call_site_is_ho,
    Callees = CostAndCallees ^ cac_callees,
    member_index0(Var, Args, ArgNum), 
    (
        HigherOrder = first_order_call,
        ( empty(Callees) ->
            % There are no callees, this code is never called.
            pessimistic_var_use_time(VarUseType, Cost, Time)
        ; singleton_set(Callees, SingletonCallee) ->
            CSDPtr = SingletonCallee ^ c_csd,
            call_site_dynamic_var_use_info(Deep, CliquePtr, CSDPtr,
                ArgNum, RecursionType, MaybeCurDepth, Cost, CallStack,
                VarUseType, MaybeVarUseInfo),
            (
                MaybeVarUseInfo = ok(VarUseInfo),
                VarUseInfo = var_use_info(Time, _, _)
            ;
                MaybeVarUseInfo = error(_),
                pessimistic_var_use_time(VarUseType, Cost, Time)
            )
        ;
            error(this_file ++ 
                "Wrong number of callees for normal call site")
        )
    ;
        HigherOrder = higher_order_call,
        % The compiler cannot push signals or waits into higher order
        % calls.  Therefore we assume a pessimistic default here.
        pessimistic_var_use_time(VarUseType, Cost, Time)
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
    goal_var_first_use(ConjGoalPath, Conj, StaticInfo, !CostSoFar,
        HeadFoundFirstUse),
    (
        % XXX: if a variable is bound more than once, because it's used with
        % partial instantiation then we want to use the last time it is bound.
        % Instmaps can be used to track this. This is relevant when searching
        % for the producer of a variable.
        HeadFoundFirstUse = found_first_use(_),
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = have_not_found_first_use,
        conj_var_first_use(GoalPath, ConjNum + 1, Conjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse),
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
    goal_var_first_use(DisjGoalPath, Disj, StaticInfo, !CostSoFar,
        HeadFoundFirstUse),
    disj_var_first_use_2(GoalPath, DisjNum + 1, Disjs, StaticInfo,
        !CostSoFar, TailFoundFirstUse),
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
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostAfterSwitch
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostBeforeSwitch
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
    CostBeforeITE = !.CostSoFar,
    goal_var_first_use(CondGoalPath, Cond, StaticInfo,
        CostBeforeITE, CostAfterCond, CondFoundFirstUse),
    goal_var_first_use(ThenGoalPath, Then, StaticInfo,
        CostAfterCond, CostAfterThen, ThenFoundFirstUse),
    goal_var_first_use(ElseGoalPath, Else, StaticInfo,
        CostAfterCond, CostAfterElse, ElseFoundFirstUse),
    weighted_average(Weights, [CostAfterThen, CostAfterElse],
        CostAfterITE),
    !:CostSoFar = CostAfterITE,
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

:- pred ffu_to_float(float::in, found_first_use::in, float::out) is det.

ffu_to_float(Default, have_not_found_first_use, Default).
ffu_to_float(_, found_first_use(CostBeforeUse), CostBeforeUse).

%----------------------------------------------------------------------------%

:- pred goal_var_first_use_wrapper(deep::in, clique_ptr::in,
    set(proc_dynamic_ptr)::in, 
    map(goal_path, cost_and_callees)::in, map(goal_path, cs_cost_csq)::in, 
    recursion_type::in, maybe(float)::in,
    goal_rep(coverage_info)::in, float::in, var_rep::in, 
    var_use_type::in, var_use_info::out) is det.

goal_var_first_use_wrapper(Deep, CliquePtr, CallStack, CallSiteMap,
        RecursiveCallSiteMap, RT, MaybeCurDepth, Goal, ProcCost, Var,
        VarUseType, VarUseInfo) :-
    goal_var_first_use(empty_goal_path, Goal,
        var_first_use_static_info(Deep, CliquePtr, CallSiteMap,
            RecursiveCallSiteMap, Var, VarUseType, CallStack, RT,
            MaybeCurDepth),
        0.0, _Cost, FoundFirstUse),
    (
        FoundFirstUse = found_first_use(CostUntilUse),
        VarUseInfo = var_use_info(CostUntilUse, ProcCost, VarUseType)
    ;
        FoundFirstUse = have_not_found_first_use,
        % If the first use has not been found then:
        %  a) for productions, they must have been produced, this is an error.
        %  b) For consumptions. the compiler will insert a wait so any calls
        %     following this one can assume that a wait has already been
        %     performed.
        (
            VarUseType = var_use_production,
            error(this_file ++ 
                ": Goal did not produce a variable that it should have")
        ;
            VarUseType = var_use_consumption,
            VarUseInfo = var_use_info(ProcCost, ProcCost, VarUseType)
        ;
            VarUseType = var_use_other,
            pessimistic_var_use_info(VarUseType, ProcCost, VarUseInfo)
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "var_use_analysis.m".

%-----------------------------------------------------------------------------%
