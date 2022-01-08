%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_annotate.m.
% Author: pbone.
%
% This module contains code to annotate goals.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_annotate.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.
:- import_module program_representation_utils.

:- import_module set.

    % Note: It may be useful to add other annotations such as goal path or cost
    % information.
    %
    % SeenDuplicateInstantiation is used to assert that we are analysing single
    % assignment code only.
    %
    % Vars is the set of variables used by this goal, both consumed and
    % produced.
    %
:- pred goal_annotate_with_instmap(goal_rep(goal_id)::in,
    seen_duplicate_instantiation::out,
    set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Annotate a goal with instantiation information.
%

goal_annotate_with_instmap(Goal, SeenDuplicateInstantiation, ConsumedVars,
        BoundVars, !InstMap, !InstMapArray) :-
    Goal = goal_rep(GoalExpr, _, GoalId),
    InstMapBefore = !.InstMap,
    (
        GoalExpr = conj_rep(Conjs),
        conj_annotate_with_instmap(Conjs, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = disj_rep(Disjs),
        disj_annotate_with_instmap(Disjs, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = switch_rep(Var, _CanFail, Cases),
        switch_annotate_with_instmap(Cases, SeenDuplicateInstantiation,
            ConsumedVars0, BoundVars, !InstMap, !InstMapArray),
        set.insert(Var, ConsumedVars0, ConsumedVars)
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ite_annotate_with_instmap(Cond, Then, Else,
            SeenDuplicateInstantiation, ConsumedVars, BoundVars,
            !InstMap, !InstMapArray)
    ;
        % XXX: Not all scope goals can produce variables, in fact some are used
        % to isolate variables that aren't named apart. But other scope goals
        % can bind variables. We don't know which we are looking at here.
        GoalExpr = scope_rep(Subgoal, _MaybeCut),
        goal_annotate_with_instmap(Subgoal, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = negation_rep(Subgoal),
        % A negated goal cannot affect instantiation.
        goal_annotate_with_instmap(Subgoal, SeenDuplicateInstantiation,
            ConsumedVars, _, !.InstMap, _InstMap, !InstMapArray),
        BoundVars = set.init
    ;
        GoalExpr = atomic_goal_rep(_File, _Line, BoundVarsList, AtomicGoal),
        % The binding of a variable may depend on any number of other
        % variables, and recursively the variables that those depended-on
        % variables depend upon.
        % XXX: This doesn't include variables that can affect control flow and
        % therefore the values of other variables, this includes variables
        % referenced from conditions in ITE goals, and variables switched-on.
        % We may get away with this as our new system for determining
        % goal-dependence takes these into account.
        atomic_goal_get_vars(AtomicGoal, Vars),
        BoundVars = set.list_to_set(BoundVarsList),
        set.difference(Vars, BoundVars, ConsumedVars),
        inst_map_ground_vars(BoundVarsList, ConsumedVars, !InstMap,
            SeenDuplicateInstantiation)
    ),
    InstMapAfter = !.InstMap,
    InstMapInfo = inst_map_info(InstMapBefore, InstMapAfter, ConsumedVars,
        BoundVars),
    update_goal_attribute(GoalId, InstMapInfo, !InstMapArray).

:- pred conj_annotate_with_instmap(list(goal_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

conj_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
conj_annotate_with_instmap([Conj | Conjs], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, !InstMap, !InstMapArray) :-
    goal_annotate_with_instmap(Conj, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, !InstMap, !InstMapArray),
    conj_annotate_with_instmap(Conjs, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, !InstMap, !InstMapArray),
    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),
    set.union(BoundVarsTail, BoundVarsHead, BoundVars),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead, SeenDuplicateInstantiationTail).

:- pred disj_annotate_with_instmap(list(goal_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

disj_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
disj_annotate_with_instmap([Disj | Disjs], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    HeadDetism = Disj ^ goal_detism_rep,
    goal_annotate_with_instmap(Disj, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, InstMap0, InstMapHead, !InstMapArray),
    disj_annotate_with_instmap(Disjs, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, InstMap0, InstMapTail, !InstMapArray),

    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),

    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    % XXX: Consider inferring determinism as another simple analysis.
    % A disjunction may only bind a variable if all disjuncts bind that
    % variable. We respect that here and handle the special case of this being
    % the last disjunct in a disjunction.
    (
        Disjs = [],
        TailDetism = failure_rep,
        BoundVars = BoundVarsHead
    ;
        Disjs = [_ | _],
        TailDetism = det_rep,
        set.intersect(BoundVarsTail, BoundVarsHead, BoundVars)
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead, SeenDuplicateInstantiationTail).

:- pred switch_annotate_with_instmap(list(case_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

switch_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
switch_annotate_with_instmap([Case | Cases], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    Case = case_rep(_, _, Goal),
    HeadDetism = Goal ^ goal_detism_rep,
    goal_annotate_with_instmap(Goal, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, InstMap0, InstMapHead, !InstMapArray),
    switch_annotate_with_instmap(Cases, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, InstMap0, InstMapTail, !InstMapArray),
    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Cases = [],
        TailDetism = failure_rep,
        BoundVars = BoundVarsHead
    ;
        Cases = [_ | _],
        TailDetism = det_rep,
        set.intersect(BoundVarsTail, BoundVarsHead, BoundVars)
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead, SeenDuplicateInstantiationTail).

:- pred ite_annotate_with_instmap(goal_rep(goal_id)::in,
    goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

ite_annotate_with_instmap(Cond, Then, Else, SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    goal_annotate_with_instmap(Cond, SeenDuplicateInstantiationCond,
        ConsumedVarsCond, _BoundVarsCond, InstMap0, InstMapAfterCond,
        !InstMapArray),
    goal_annotate_with_instmap(Then, SeenDuplicateInstantiationThen,
        ConsumedVarsThen, BoundVarsThen, InstMapAfterCond, InstMapAfterThen,
        !InstMapArray),
    goal_annotate_with_instmap(Else, SeenDuplicateInstantiationElse,
        ConsumedVarsElse, BoundVarsElse, InstMap0, InstMapAfterElse,
        !InstMapArray),
    ( if
        SeenDuplicateInstantiationCond = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationThen = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationElse = have_not_seen_duplicate_instantiation
    then
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    else
        SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    set.union(ConsumedVarsCond, ConsumedVarsThen, ConsumedVarsCondThen),
    set.union(ConsumedVarsCondThen, ConsumedVarsElse, ConsumedVars),
    % Cond is only allowed to bind variables for then. The variables bound by
    % the ITE are only those that both Then and Else bind.
    set.intersect(BoundVarsThen, BoundVarsElse, BoundVars),
    ThenDetism = Then ^ goal_detism_rep,
    ElseDetism = Else ^ goal_detism_rep,
    InstMap = merge_inst_map(InstMapAfterThen, ThenDetism,
        InstMapAfterElse, ElseDetism).

%---------------------------------------------------------------------------%
:- end_module autopar_annotate.
%---------------------------------------------------------------------------%
