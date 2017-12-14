%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pre_quantification.m.
% Main author: zs.
%
% This module adds implicit quantifications of some variables just inside
% trace goal scopes in order to avoid a minor but annoying problem, which can
% also be confusing for people who come across it the first time.
%
% Consider the following code:
%
%       :- pred p(int::in, int::in, int::out) is det.
%    
%       p(A, B, X) :-
%           trace [io(!IO)] (
%               ToPrint = A,
%               io.format("A = %d\n", [i(ToPrint)], !IO)
%           ),
%           trace [io(!IO)] (
%               ToPrint = B,
%               io.format("B = %d\n", [i(ToPrint)], !IO)
%           ),
%           X = A + B,
%           trace [io(!IO)] (
%               ToPrint = X,
%               io.format("X = %d\n", [i(ToPrint)], !IO)
%           ).
%
% Since the variable ToPrint is used inside more than one trace goal,
% the compiler's usual scope rules consider it to be a nonlocal variable
% in each trace goal. This is a problem, because the compiler does not allow
% trace goals to bind variables that are not local to the trace goal.
% It does this to preserve the ability of the program to work whether or not
% the trace goal is there, since it can be deleted if either its compile_time
% or run_time condition turns out to be false.
%
% However, in cases like this, where the nonlocal variable occurs *only*
% inside trace goals, leaving ToPrint unbound if a trace goal is deleted
% will not affect the execution of the program. Having it bound more than once
% if the trace goals *aren't* deleted would be a problem, but this is easily
% fixed by making ToPrint existentially quantified inside each trace goal;
% that way, each trace goal will bind and use its own copy of ToPrint.
%
% The job of this module is to find out which variables in a clause body
% are used only inside trace goals, and to insert the quantifications
% into trace goals as necessary to ensure that all such variables
% will end up being local to each trace goal scope. The part of the compiler
% that ensures this is the quantification pass, which is invoked on the
% goal we generate pretty much immediately after we return it.
%
% The overall effect of putting this pass before the quantification pass
% is a minor tweak of the usual rules of quantification, a tweak that
% makes the rules enforced by the compiler resemble the rules expected
% by programmers more closely, helping the compiler approach closer to the
% principle of least astonishment.
%
%-----------------------------------------------------------------------------%

:- module hlds.pre_quantification.
:- interface.

:- import_module hlds.hlds_goal.

%-----------------------------------------------------------------------------%

:- pred separate_trace_goal_only_locals(hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%
%
% The pre-quantification algorithm is based on dividing the clause body
% into one or more zones, each identified by a non-negative integer.
% The zones are based on viewing the clause body as a tree:
%
% - with each goal being a node,
% - atomic goals being leaf nodes, and
% - each compound goal having a subtree for each subgoal.
%
% If lambda goals didn't exist, each zone would be identified by its top node.
%
% The top node of zone 0 is the root of the tree, while the top node of
% every other zone is a trace goal, i.e. a scope goal whose scope_reason
% is trace_goal. Every trace goal is the top node of a zone, with the
% exception of trace goals that are contained inside other trace goals.
% (We have no rule against nesting trace goals, though they don't make
% much sense, and I (zs) have never seen such nesting.)
%
% The zone that each node belongs to is the zone whose top node you first find
% when you traverse the tree upwards from the node. In other words, each zone
% claims the nodes beneath it, except the ones that are claimed by another
% zone whose top node is lower down.
%
% Since lambda goals do exist, we have to handle them. We do so by considering
% the lambda goal to be effectively a separate predicate body inside this
% clause (since the compiler will eventually turn it into that). So the
% top level of a rhs_lambda_goal is also considered to be in zone 0.

:- type zone == int.
% We could make a zone a uint, but there are issues around the uint type's
% membership of the enum typeclass, which is needed for the use of
% sparse_bitset, as discussed on m-rev on 2017 july 3.

:- func top_zone = zone.
:- pragma inline(top_zone/0).

top_zone = 0.

%-----------------------------------------------------------------------------%
%
% The pre-quantification algorithm has three main steps.
%
% - The first step is to find out which variables are used in which zones.
%   This is done by build_vars_to_zones_in_*, which build the VarsToZones map.
%
% - The second step is finding out which variables occur only inside
%   trace goals, not outside them. These variables should be local to the
%   trace goals they appear in. If they occur in only one trace goal,
%   ordinary quantification will ensure this. We need to add explicit
%   existential quantifications for them only if they occur inside
%   more than one trace goal. In terms of zones, this means variables
%   that don't occur in zone 0 but do appear in more than one other zone.
%   (We call these variables the "duplicated" variables.) The job of
%   build_zones_to_dup_vars is to map each trace goal zone to the list
%   of variables that needs to have an existential quantifier added for it
%   in the scope of that trace goal, and to return that as ZonesToDupVars.
%
% - The third step is to go through the clause body and insert those
%   existential quantification scopes inside every trace goal scope
%   that contains one or more of these "duplicated" variables.
%

    % Our parent ensures that we don't get called on clauses that
    % don't contain any trace goals. Most clauses that have trace goals
    % will have only a few of them, so the sparse_bitset(zone) will
    % typically contain only a single node.
    %
:- type vars_to_zones == map(prog_var, sparse_bitset(zone)).

:- type zones_to_dup_vars == map(zone, list(prog_var)).

separate_trace_goal_only_locals(Goal0, Goal) :-
    map.init(VarsToZones0),
    counter.init(1, TraceCounter0),
    build_vars_to_zones_in_goal(top_zone, Goal0, TraceCounter0, _,
        VarsToZones0, VarsToZones),
    map.init(ZonesToDupVars0),
    map.foldl(build_zones_to_dup_vars, VarsToZones,
        ZonesToDupVars0, ZonesToDupVars),
    ( if map.is_empty(ZonesToDupVars) then
        Goal = Goal0
    else
        add_exist_scopes_for_dup_vars_in_goal(top_zone, ZonesToDupVars,
            Goal0, Goal, TraceCounter0, _)
    ).

%-----------------------------------------------------------------------------%

    % The first step described in the comment above
    % separate_trace_goal_only_locals.
    %
:- pred build_vars_to_zones_in_goal(zone::in, hlds_goal::in,
    counter::in, counter::out, vars_to_zones::in, vars_to_zones::out) is det.

build_vars_to_zones_in_goal(CurZone, Goal, !TraceCounter, !VarsToZones) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = plain_call(_PredId, _ProcId, ArgVars, _Builtin,
            _MaybeUnifyContext, _SymName),
        record_vars_in_zone(CurZone, ArgVars, !VarsToZones)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _Modes,
            _RegTypes, _Detism),
        record_vars_in_zone(CurZone, ArgVars, !VarsToZones),
        (
            GenericCall = higher_order(ClosureVar, _Purity, _CallKind, _Arity),
            record_var_in_zone(CurZone, ClosureVar, !VarsToZones)
        ;
            GenericCall = class_method(TypeClassInfoVar, _MethodNum,
                _ClassId, _SimpleCallId),
            record_var_in_zone(CurZone, TypeClassInfoVar, !VarsToZones)
        ;
            GenericCall = event_call(_)
        ;
            GenericCall = cast(_)
        )
    ;
        GoalExpr = call_foreign_proc(_Attrs, _PredId, _ProcId,
            ForeignArgs, ExtraArgs, _TraceCond, _Impl),
        record_vars_in_zone(CurZone, list.map(foreign_arg_var, ForeignArgs),
            !VarsToZones),
        record_vars_in_zone(CurZone, list.map(foreign_arg_var, ExtraArgs),
            !VarsToZones)
    ;
        GoalExpr = unify(LHSVar, RHS, _Mode, _Kind, _Context),
        record_var_in_zone(CurZone, LHSVar, !VarsToZones),
        (
            RHS = rhs_var(RHSVar),
            record_var_in_zone(CurZone, RHSVar, !VarsToZones)
        ;
            RHS = rhs_functor(_ConsId, _IsExistConstr, RHSArgVars),
            record_vars_in_zone(CurZone, RHSArgVars, !VarsToZones)
        ;
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PredOrFunc,
                _EvalMethod, _NonLocals, LambdaVars, _LambdaModes, _Detism,
                LambdaGoal),
            % The lambda goal is the one clause of the procedure that
            % we will construct from LambdaGoal. We therefore treat it
            % as we treat the top level goal.
            LambdaZone = top_zone,
            record_vars_in_zone(LambdaZone, LambdaVars, !VarsToZones),
            build_vars_to_zones_in_goal(LambdaZone, LambdaGoal,
                !TraceCounter, !VarsToZones)
        )
    ;
        ( GoalExpr = conj(_ConjType, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        list.foldl2(build_vars_to_zones_in_goal(CurZone), SubGoals,
            !TraceCounter, !VarsToZones)
    ;
        GoalExpr = switch(Var, _CanFail, Cases),
        record_var_in_zone(CurZone, Var, !VarsToZones),
        list.foldl2(build_vars_to_zones_in_case(CurZone), Cases,
            !TraceCounter, !VarsToZones)
    ;
        GoalExpr = negation(SubGoal),
        build_vars_to_zones_in_goal(CurZone, SubGoal,
            !TraceCounter, !VarsToZones)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = trace_goal(_, _, _, _, _),
            CurZone = top_zone
        then
            counter.allocate(NewZone, !TraceCounter),
            build_vars_to_zones_in_goal(NewZone, SubGoal,
                !TraceCounter, !VarsToZones)
        else
            build_vars_to_zones_in_goal(CurZone, SubGoal,
                !TraceCounter, !VarsToZones)
        )
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        record_vars_in_zone(CurZone, Vars, !VarsToZones),
        build_vars_to_zones_in_goal(CurZone, Cond,
            !TraceCounter, !VarsToZones),
        build_vars_to_zones_in_goal(CurZone, Then,
            !TraceCounter, !VarsToZones),
        build_vars_to_zones_in_goal(CurZone, Else,
            !TraceCounter, !VarsToZones)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = bi_implication(SubGoalA, SubGoalB),
            build_vars_to_zones_in_goal(CurZone, SubGoalA,
                !TraceCounter, !VarsToZones),
            build_vars_to_zones_in_goal(CurZone, SubGoalB,
                !TraceCounter, !VarsToZones)
        ;
            ShortHand = atomic_goal(_GoalType, OuterVars, InnerVars,
                MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            OuterVars = atomic_interface_vars(OVA, OVB),
            InnerVars = atomic_interface_vars(IVA, IVB),
            record_vars_in_zone(CurZone, [OVA, OVB, IVA, IVB], !VarsToZones),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                record_vars_in_zone(CurZone, OutputVars, !VarsToZones)
            ),
            build_vars_to_zones_in_goal(CurZone, MainGoal,
                !TraceCounter, !VarsToZones),
            list.foldl2(build_vars_to_zones_in_goal(CurZone), OrElseGoals,
                !TraceCounter, !VarsToZones)
        ;
            ShortHand = try_goal(MaybeIOStateVars, ResultVar, SubGoal),
            (
                MaybeIOStateVars = no
            ;
                MaybeIOStateVars = yes(IOStateVars),
                IOStateVars = try_io_state_vars(ISVA, ISVB),
                record_vars_in_zone(CurZone, [ISVA, ISVB], !VarsToZones)
            ),
            record_var_in_zone(CurZone, ResultVar, !VarsToZones),
            build_vars_to_zones_in_goal(CurZone, SubGoal,
                !TraceCounter, !VarsToZones)
        )
    ).

:- pred build_vars_to_zones_in_case(zone::in, case::in,
    counter::in, counter::out, vars_to_zones::in, vars_to_zones::out) is det.

build_vars_to_zones_in_case(CurZone, Case, !TraceCounter, !VarsToZones) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    build_vars_to_zones_in_goal(CurZone, Goal, !TraceCounter, !VarsToZones).

:- pred record_vars_in_zone(zone::in, list(prog_var)::in,
    vars_to_zones::in, vars_to_zones::out) is det.

record_vars_in_zone(_Zone, [], !VarsToZones).
record_vars_in_zone(Zone, [Var | Vars], !VarsToZones) :-
    record_var_in_zone(Zone, Var, !VarsToZones),
    record_vars_in_zone(Zone, Vars, !VarsToZones).

:- pred record_var_in_zone(zone::in, prog_var::in,
    vars_to_zones::in, vars_to_zones::out) is det.

record_var_in_zone(Zone, Var, !VarsToZones) :-
    ( if map.search(!.VarsToZones, Var, Zones0) then
        % Most variables will occur only in one zone, typically zone 0.
        % However, they will typically occur there more than once.
        % We don't want to set the bit of the zone more than once.
        ( if sparse_bitset.contains(Zones0, Zone) then
            true
        else
            sparse_bitset.insert(Zone, Zones0, Zones),
            map.det_update(Var, Zones, !VarsToZones)
        )
    else
        Zones = sparse_bitset.make_singleton_set(Zone),
        map.det_insert(Var, Zones, !VarsToZones)
    ).

%-----------------------------------------------------------------------------%

    % The second step described in the comment above
    % separate_trace_goal_only_locals.
    %
:- pred build_zones_to_dup_vars(prog_var::in, sparse_bitset(zone)::in,
    zones_to_dup_vars::in, zones_to_dup_vars::out) is det.

build_zones_to_dup_vars(Var, Zones, !ZonesToDupVars) :-
    ZoneList = sparse_bitset.to_sorted_list(Zones),
    ( if
        ZoneList = [FirstZone, _SecondZone | _],
        FirstZone \= top_zone
    then
        list.foldl(add_var_to_zone(Var), ZoneList, !ZonesToDupVars)
    else
        true
    ).

:- pred add_var_to_zone(prog_var::in, zone::in,
    zones_to_dup_vars::in, zones_to_dup_vars::out) is det.

add_var_to_zone(Var, Zone, !ZonesToDupVars) :-
    ( if map.search(!.ZonesToDupVars, Zone, DupVars0) then
        DupVars = [Var | DupVars0],
        map.det_update(Zone, DupVars, !ZonesToDupVars)
    else
        map.det_insert(Zone, [Var], !ZonesToDupVars)
    ).

%-----------------------------------------------------------------------------%

    % The third step described in the comment above
    % separate_trace_goal_only_locals.
    %
:- pred add_exist_scopes_for_dup_vars_in_goal(zone::in, zones_to_dup_vars::in,
    hlds_goal::in, hlds_goal::out, counter::in, counter::out) is det.

add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
        !Goal, !TraceCounter) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHSVar, RHS0, Mode, Kind, Context),
        (
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            GoalExpr = GoalExpr0
        ;
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
                EvalMethod, NonLocals, LambdaVars, LambdaModes, Detism,
                LambdaGoal0),
            LambdaZone = top_zone,
            add_exist_scopes_for_dup_vars_in_goal(LambdaZone, ZonesToDupVars,
                LambdaGoal0, LambdaGoal, !TraceCounter),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
                EvalMethod, NonLocals, LambdaVars, LambdaModes, Detism,
                LambdaGoal),
            GoalExpr = unify(LHSVar, RHS, Mode, Kind, Context)
        )
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        list.map_foldl(
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars),
            SubGoals0, SubGoals, !TraceCounter),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        list.map_foldl(
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars),
            SubGoals0, SubGoals, !TraceCounter),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map_foldl(
            add_exist_scopes_for_dup_vars_in_case(CurZone, ZonesToDupVars),
            Cases0, Cases, !TraceCounter),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
            SubGoal0, SubGoal, !TraceCounter),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
            SubGoal0, SubGoal1, !TraceCounter),
        ( if
            Reason = trace_goal(_, _, _, _, _),
            CurZone = top_zone
        then
            counter.allocate(NewZone, !TraceCounter),
            ( if map.search(ZonesToDupVars, NewZone, DupVars) then
                list.sort(DupVars, SortedDupVars),
                QuantReason = exist_quant(SortedDupVars),
                QuantExpr = scope(QuantReason, SubGoal1),
                SubGoal1 = hlds_goal(_, SubGoalInfo1),
                SubGoal = hlds_goal(QuantExpr, SubGoalInfo1)
            else
                SubGoal = SubGoal1
            )
        else
            SubGoal = SubGoal1
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
            Cond0, Cond, !TraceCounter),
        add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
            Then0, Then, !TraceCounter),
        add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
            Else0, Else, !TraceCounter),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(SubGoalA0, SubGoalB0),
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
                SubGoalA0, SubGoalA, !TraceCounter),
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
                SubGoalB0, SubGoalB, !TraceCounter),
            ShortHand = bi_implication(SubGoalA, SubGoalB)
        ;
            ShortHand0 = atomic_goal(GoalType, OuterVars, InnerVars,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
                MainGoal0, MainGoal, !TraceCounter),
            list.map_foldl(
                add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars),
                OrElseGoals0, OrElseGoals, !TraceCounter),
            ShortHand = atomic_goal(GoalType, OuterVars, InnerVars,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIOStateVars, ResultVar, SubGoal0),
            add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
                SubGoal0, SubGoal, !TraceCounter),
            ShortHand = try_goal(MaybeIOStateVars, ResultVar, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred add_exist_scopes_for_dup_vars_in_case(zone::in, zones_to_dup_vars::in,
    case::in, case::out, counter::in, counter::out) is det.

add_exist_scopes_for_dup_vars_in_case(CurZone, ZonesToDupVars,
        !Case, !TraceCounter) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    add_exist_scopes_for_dup_vars_in_goal(CurZone, ZonesToDupVars,
        Goal0, Goal, !TraceCounter),
    !:Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%
:- end_module hlds.pre_quantification.
%-----------------------------------------------------------------------------%
