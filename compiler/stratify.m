%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: stratify.m.
% Main authors: ohutch, conway.
%
% This module performs stratification analysis.
% It works by processing the call graph 1 scc at a time. It traverses
% the goal for each procedure in the scc and reports an error or
% warning (depending on the context) for any negated call to another member
% of the scc. If it encounters a higher order call or a call to an
% outside module it will also emit a message.
%
% It has a second pass which is not currently enabled.
%
% The second pass looks for possible non stratified code by looking at
% higher order calls. This second pass works by rebuilding the call
% graph with any possible arcs that can arise though higher order calls
% and then traversing the new sccs looking for negative loops.
%
% The second pass is necessary because the rebuilt call graph does not
% allow the detection of definite non-stratification.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.stratify.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

    % Perform stratification analysis for the given module. If the
    % "warn-non-stratification" option is set, this predicate will check
    % the entire module for stratification, otherwise it will only check
    % the predicates in the stratified_preds set of the module_info structure.
    %
:- pred check_module_for_stratification(module_info::in, module_info::out,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module digraph.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

check_module_for_stratification(!ModuleInfo, Specs) :-
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    FOSCCs = dependency_info_get_bottom_up_sccs(DepInfo),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_non_stratification, Warn),
    module_info_get_must_be_stratified_preds(!.ModuleInfo,
        MustBeStratifiedPreds),
    first_order_check_sccs(FOSCCs, MustBeStratifiedPreds, Warn, !.ModuleInfo,
        [], Specs).

    % The following code was used for the second pass of this module but
    % as that pass is disabled, so is this code. The higher order code
    % is disabled because it is currently unable to detect cases where a
    % higher order proc is hidden in some complex data structure.
    %
    % gen_conservative_graph(!.ModuleInfo, DepGraph0, DepGraph, HOInfo),
    % digraph.atsort(DepGraph, HOSCCs1),
    % dep_sets_to_lists_and_sets(HOSCCs1, [], HOSCCs),
    % higher_order_check_sccs(HOSCCs, HOInfo, ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred get_pred_id(pred_proc_id::in, pred_id::out) is det.

get_pred_id(proc(PredId, _), PredId).

    % Check the first order SCCs for stratification.
    %
:- pred first_order_check_sccs(list(set(pred_proc_id))::in,
    set(pred_id)::in, bool::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_sccs([], _, _, _, !Specs).
first_order_check_sccs([HeadSCC | TailSCCs], MustBeStratifiedPreds, Warn,
        ModuleInfo, !Specs) :-
    set.map(get_pred_id, HeadSCC, HeadSCCPreds),
    set.intersect(HeadSCCPreds, MustBeStratifiedPreds,
        MustBeStratifiedPredsInScc),
    ( if
        ( Warn = yes
        ; set.is_non_empty(MustBeStratifiedPredsInScc)
        )
    then
        first_order_check_scc(HeadSCC, is_warning, ModuleInfo, !Specs)
    else
        true
    ),
    first_order_check_sccs(TailSCCs, MustBeStratifiedPreds, Warn,
        ModuleInfo, !Specs).

:- pred first_order_check_scc(set(pred_proc_id)::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_scc(Scc, ErrorOrWarning, ModuleInfo, !Specs) :-
    first_order_check_scc_loop(set.to_sorted_list(Scc), Scc,
        ErrorOrWarning, ModuleInfo, !Specs).

:- pred first_order_check_scc_loop(list(pred_proc_id)::in,
    set(pred_proc_id)::in, error_or_warning::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_scc_loop([], _WholeScc, _, _, !Specs).
first_order_check_scc_loop([PredProcId | PredProcIds], WholeScc,
        ErrorOrWarning, ModuleInfo, !Specs) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, Proc),
    proc_info_get_goal(Proc, Goal),
    first_order_check_goal(Goal, no, WholeScc,
        PredProcId, ErrorOrWarning, ModuleInfo, !Specs),
    first_order_check_scc_loop(PredProcIds, WholeScc, ErrorOrWarning,
        ModuleInfo, !Specs).

:- pred first_order_check_goal(hlds_goal::in, bool::in, set(pred_proc_id)::in,
    pred_proc_id::in, error_or_warning::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_goal(Goal, Negated, WholeScc, ThisPredProcId, ErrorOrWarning,
        ModuleInfo, !Specs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        first_order_check_goals(Goals, Negated, WholeScc, ThisPredProcId,
            ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = switch(_Var, _Fail, Cases),
        first_order_check_cases(Cases, Negated, WholeScc, ThisPredProcId,
            ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        first_order_check_goal(Cond, yes, WholeScc,
            ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs),
        first_order_check_goal(Then, Negated, WholeScc,
            ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs),
        first_order_check_goal(Else, Negated, WholeScc,
            ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = negation(SubGoal),
        first_order_check_goal(SubGoal, yes, WholeScc,
            ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes cannot contain any calls.
            true
        else
            first_order_check_goal(SubGoal, Negated, WholeScc,
                ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs)
        )
    ;
        ( GoalExpr = plain_call(CPred, CProc, _Args, _BuiltinState, _UC, _Sym)
        ; GoalExpr = call_foreign_proc(_Attributes, CPred, CProc, _, _, _, _)
        ),
        Callee = proc(CPred, CProc),
        ( if
            Negated = yes,
            set.member(Callee, WholeScc)
        then
            Context = goal_info_get_context(GoalInfo),
            ErrorMsg = "call introduces a non-stratified loop.",
            Spec = generate_stratify_error(ModuleInfo, ThisPredProcId, Context,
                ErrorMsg, ErrorOrWarning),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ;
        GoalExpr = generic_call(_Var, _Args, _Modes, _MaybeArgRegs, _Det)
        % Do nothing.
    ;
        GoalExpr = unify(_LHS, _RHS, _Mode, _Unification, _UnifyContext)
        % Do nothing.
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            first_order_check_goal(MainGoal, Negated, WholeScc,
                ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs),
            first_order_check_goals(OrElseGoals, Negated, WholeScc,
                ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            first_order_check_goal(SubGoal, Negated, WholeScc,
                ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred first_order_check_goals(list(hlds_goal)::in, bool::in,
    set(pred_proc_id)::in, pred_proc_id::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_goals([], _, _, _, _, _, !Specs).
first_order_check_goals([Goal | Goals], Negated,
        WholeScc, ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs) :-
    first_order_check_goal(Goal, Negated, WholeScc, ThisPredProcId,
        ErrorOrWarning, ModuleInfo, !Specs),
    first_order_check_goals(Goals, Negated, WholeScc, ThisPredProcId,
        ErrorOrWarning, ModuleInfo, !Specs).

:- pred first_order_check_cases(list(case)::in, bool::in,
    set(pred_proc_id)::in, pred_proc_id::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

first_order_check_cases([], _, _, _, _, _, !Specs).
first_order_check_cases([Case | Goals], Negated, WholeScc, ThisPredProcId,
        ErrorOrWarning, ModuleInfo, !Specs) :-
    Case = case(_, _, Goal),
    first_order_check_goal(Goal, Negated, WholeScc,
        ThisPredProcId, ErrorOrWarning, ModuleInfo, !Specs),
    first_order_check_cases(Goals, Negated, WholeScc, ThisPredProcId,
        ErrorOrWarning, ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%
%
% XXX Currently we don't allow the higher order case so this code is disabled.

    % Check the higher order SCCs for stratification.
    %
:- pred higher_order_check_sccs(
    assoc_list(list(pred_proc_id), set(pred_proc_id))::in, ho_map::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.
:- pragma consider_used(pred(higher_order_check_sccs/5)).

higher_order_check_sccs([], _HOInfo, _ModuleInfo, !Specs).
higher_order_check_sccs([HeadSCC | TailSCCs], HOInfo, ModuleInfo, !Specs) :-
    HeadSCC = HeadSCCProcs - HeadSCCPreds,
    higher_order_check_scc(HeadSCCProcs, HeadSCCPreds, HOInfo, ModuleInfo,
        !Specs),
    higher_order_check_sccs(TailSCCs, HOInfo, ModuleInfo, !Specs).

:- pred higher_order_check_scc(list(pred_proc_id)::in, set(pred_proc_id)::in,
    ho_map::in, module_info::in, list(error_spec)::in, list(error_spec)::out)
    is det.

higher_order_check_scc([], _WholeScc, _HOInfo, _ModuleInfo, !Specs).
higher_order_check_scc([PredProcId | Remaining], WholeScc, HOInfo,
        ModuleInfo, !Specs) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_non_stratification, Warn),
    ErrorOrWarning = is_warning,
    ( if
        Warn = yes,
        map.search(HOInfo, PredProcId, HigherOrderInfo)
    then
        HigherOrderInfo = strat_ho_info(HOCalls, _),
        set.intersect(HOCalls, WholeScc, HOLoops),
        ( if set.is_empty(HOLoops) then
            HighOrderLoops = no
        else
            HighOrderLoops = yes
        ),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, Proc),
        proc_info_get_goal(Proc, Goal),
        higher_order_check_goal(Goal, no, WholeScc, PredProcId, HighOrderLoops,
            ErrorOrWarning, ModuleInfo, !Specs)
    else
        true
    ),
    higher_order_check_scc(Remaining, WholeScc, HOInfo, ModuleInfo, !Specs).

:- pred higher_order_check_goal(hlds_goal::in, bool::in, set(pred_proc_id)::in,
    pred_proc_id::in, bool::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

higher_order_check_goal(Goal, Negated, WholeScc, ThisPredProcId,
        HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        higher_order_check_goals(Goals, Negated, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = switch(_Var, _Fail, Cases),
        higher_order_check_cases(Cases, Negated, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        higher_order_check_goal(Cond, yes, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs),
        higher_order_check_goal(Then, Negated, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs),
        higher_order_check_goal(Else, Negated, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = negation(SubGoal),
        higher_order_check_goal(SubGoal, yes, WholeScc, ThisPredProcId,
            HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These scopes cannot contain any calls.
            true
        else
            higher_order_check_goal(SubGoal, Negated, WholeScc,
                ThisPredProcId, HighOrderLoops, ErrorOrWarning,
                ModuleInfo, !Specs)
        )
    ;
        GoalExpr = plain_call(_CPred, _CProc, _Args, _Builtin, _UC, Sym),
        ( if
            % XXX Is this good enough to detect all calls to solutions ?
            HighOrderLoops = yes,
            ( Sym = unqualified(Name)
            ; Sym = qualified(_, Name)
            ),
            Name = "solutions"
        then
            Context = goal_info_get_context(GoalInfo),
            ErrorMsg = "call to solutions/2 introduces a non-stratified loop.",
            Spec = generate_stratify_error(ModuleInfo, ThisPredProcId, Context,
                ErrorMsg, ErrorOrWarning),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ;
        GoalExpr = generic_call(GenericCall, _Vars, _Modes, _MaybeArgRegs,
            _Det),
        ( if
            Negated = yes,
            HighOrderLoops = yes,
            ( GenericCall = higher_order(_, _, _, _), Msg = "higher order"
            ; GenericCall = class_method(_, _, _, _), Msg = "class method"
            )
        then
            Context = goal_info_get_context(GoalInfo),
            ErrorMsg = Msg ++ " call may introduce a non-stratified loop.",
            Spec = generate_stratify_error(ModuleInfo, ThisPredProcId, Context,
                ErrorMsg, ErrorOrWarning),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        % Do nothing.
    ;
        GoalExpr = unify(_LHS, _RHS, _Mode, _Unification, _UnifyContext)
        % Do nothing.
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            higher_order_check_goal(MainGoal, Negated, WholeScc,
                ThisPredProcId, HighOrderLoops, ErrorOrWarning,
                ModuleInfo, !Specs),
            higher_order_check_goals(OrElseGoals, Negated, WholeScc,
                ThisPredProcId, HighOrderLoops, ErrorOrWarning,
                ModuleInfo, !Specs)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            higher_order_check_goal(SubGoal, Negated, WholeScc,
                ThisPredProcId, HighOrderLoops, ErrorOrWarning,
                ModuleInfo, !Specs)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred higher_order_check_goals(list(hlds_goal)::in, bool::in,
    set(pred_proc_id)::in, pred_proc_id::in, bool::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

higher_order_check_goals([], _, _, _, _, _, _, !Specs).
higher_order_check_goals([Goal | Goals], Negated, WholeScc, ThisPredProcId,
        HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs) :-
    higher_order_check_goal(Goal, Negated, WholeScc,
        ThisPredProcId, HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs),
    higher_order_check_goals(Goals, Negated, WholeScc, ThisPredProcId,
        HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs).

:- pred higher_order_check_cases(list(case)::in, bool::in,
    set(pred_proc_id)::in, pred_proc_id::in, bool::in, error_or_warning::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

higher_order_check_cases([], _, _, _, _, _, _, !Specs).
higher_order_check_cases([Case | Goals], Negated, WholeScc, ThisPredProcId,
        HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs) :-
    Case = case(_, _, Goal),
    higher_order_check_goal(Goal, Negated, WholeScc,
        ThisPredProcId, HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs),
    higher_order_check_cases(Goals, Negated, WholeScc, ThisPredProcId,
        HighOrderLoops, ErrorOrWarning, ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Direction higher order params can flow in a procedure.
:- type ho_in_out
    --->    ho_in
    ;       ho_out
    ;       ho_in_out
    ;       ho_none.

    % This structure is used to hold the higher order characteristics of a
    % procedure.
:- type strat_ho_info
    --->    strat_ho_info(
                % Possible higher order addresses that can reach the procedure.
                set(pred_proc_id),

                % Possible paths the address can take in and out
                % of the procedure.
                ho_in_out
            ).

    % A map from all non imported procedures to there higher order info.
:- type ho_map   == map(pred_proc_id, strat_ho_info).

    % A map from all non imported procs to all the procedures they can call.
:- type call_map == map(pred_proc_id, set(pred_proc_id)).

    % Given a module and a dependency graph, this predicate builds
    % a new dependency graph with all possible higher order calls added.
    % It also returns a map of all the higher order info it collects.
    %
:- pred gen_conservative_graph(module_info::in,
    hlds_dependency_graph::in, hlds_dependency_graph::out, ho_map::out) is det.
:- pragma consider_used(pred(gen_conservative_graph/4)).

gen_conservative_graph(ModuleInfo, !DepGraph, HOInfo) :-
    get_call_info(ModuleInfo, ProcCalls, HOInfo0, CallsHO),
    map.keys(ProcCalls, Callers),
    iterate_solution(Callers, ProcCalls, CallsHO, HOInfo0, HOInfo),
    map.to_assoc_list(HOInfo, HOInfoL),
    add_new_arcs(HOInfoL, CallsHO, !DepGraph).

    % For a given module, collects for each non imported procedure
    % a set of called procedures and a higher order info structure.
    % This pred also returns a set of all non imported procedures that
    % make a higher order call.
    %
:- pred get_call_info(module_info::in, call_map::out,
    ho_map::out, set(pred_proc_id)::out) is det.

get_call_info(ModuleInfo, !:ProcCalls, !:HOInfo, !:CallsHO) :-
    map.init(!:ProcCalls),
    map.init(!:HOInfo),
    set.init(!:CallsHO),
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    expand_predids(PredIds, ModuleInfo, !ProcCalls, !HOInfo, !CallsHO).

    % Finds the transitive closure of a given list of procedures.
    % This pred is used to see how face(???) a higher order address
    % can reach though procedure calls.
    %
:- pred iterate_solution(list(pred_proc_id)::in, call_map::in,
    set(pred_proc_id)::in, ho_map::in, ho_map::out) is det.

iterate_solution(PredProcs, ProcCalls, CallsHO, !HOInfo) :-
    stratify_tc(PredProcs, ProcCalls, CallsHO, !HOInfo, no, Changed),
    (
        Changed = no
    ;
        Changed = yes,
        disable_warning [suspicious_recursion] (
            iterate_solution(PredProcs, ProcCalls, CallsHO, !HOInfo)
        )
    ).

    % For each caller, merge any higher order addresses it takes with all of
    % its callees, and return if any change has occurred.
    %
:- pred stratify_tc(list(pred_proc_id)::in, call_map::in,
    set(pred_proc_id)::in, ho_map::in, ho_map::out, bool::in, bool::out)
    is det.

stratify_tc([], _, _, !HOInfo, !Changed).
stratify_tc([PredProcId | PredProcIds], ProcCalls, CallsHO, !HOInfo,
        !Changed) :-
    map.lookup(ProcCalls, PredProcId, PCalls),
    set.to_sorted_list(PCalls, PCallsL),
    merge_calls(PCallsL, PredProcId, CallsHO, yes, !HOInfo, !Changed),
    stratify_tc(PredProcIds, ProcCalls, CallsHO, !HOInfo, !Changed).

    % Merge any higher order addresses that can pass between the given caller
    % and callees. This code also merges any possible addresses that can pass
    % in and out of higher order calls.
    %
:- pred merge_calls(list(pred_proc_id)::in, pred_proc_id::in,
    set(pred_proc_id)::in, bool::in, ho_map::in, ho_map::out,
    bool::in, bool::out) is det.

merge_calls([], _, _, _, !HOInfo, !Changed).
merge_calls([C | Cs], P, CallsHO, DoingFirstOrder, !HOInfo, !Changed) :-
    ( if map.search(!.HOInfo, C, CInfo) then
        map.lookup(!.HOInfo, P, PInfo),
        CInfo = strat_ho_info(CHaveAT0, CHOInOut),
        PInfo = strat_ho_info(PHaveAT0, PHOInOut),
        % First merge the first order info, if we need to.
        ( if CHOInOut = ho_none then
            true
        else
            (
                CHOInOut = ho_in,
                ( if set.subset(PHaveAT0, CHaveAT0) then
                    CHaveAT = CHaveAT0
                else
                    set.union(PHaveAT0, CHaveAT0, CHaveAT),
                    !:Changed = yes
                ),
                PHaveAT = PHaveAT0
            ;
                CHOInOut = ho_out,
                ( if set.subset(CHaveAT0, PHaveAT0) then
                    PHaveAT = PHaveAT0
                else
                    set.union(CHaveAT0, PHaveAT0, PHaveAT),
                    !:Changed = yes
                ),
                CHaveAT = CHaveAT0
            ;
                CHOInOut = ho_in_out,
                ( if CHaveAT0 = PHaveAT0 then
                    CHaveAT = CHaveAT0,
                    PHaveAT = PHaveAT0
                else
                    set.union(CHaveAT0, PHaveAT0, NewHaveAT),
                    CHaveAT = NewHaveAT,
                    PHaveAT = NewHaveAT,
                    !:Changed = yes
                )
            ;
                CHOInOut = ho_none,
                % XXX What is a good message for this?
                unexpected($pred, "ho_none")
            ),
            NewCInfo = strat_ho_info(CHaveAT, CHOInOut),
            NewPInfo = strat_ho_info(PHaveAT, PHOInOut),
            map.det_update(C, NewCInfo, !HOInfo),
            map.det_update(P, NewPInfo, !HOInfo)
        ),
        % Then, if we need to, merge the higher order info.
        ( if
            DoingFirstOrder = yes,
            set.member(P, CallsHO)
        then
            map.lookup(!.HOInfo, P, PHOInfo),
            PHOInfo = strat_ho_info(PossibleCalls, _),
            set.to_sorted_list(PossibleCalls, PossibleCallsL),
            merge_calls(PossibleCallsL, P, CallsHO, no, !HOInfo, !Changed)
        else
            true
        )
    else
        true
    ),
    merge_calls(Cs, P, CallsHO, DoingFirstOrder, !HOInfo, !Changed).

    % Given the set of procedures that make higher order calls and a
    % list of procedures and higher order call info, this predicate rebuilds
    % the given call graph with new arcs for every possible higher order call.
    %
:- pred add_new_arcs(assoc_list(pred_proc_id, strat_ho_info)::in,
    set(pred_proc_id)::in,
    hlds_dependency_graph::in, hlds_dependency_graph::out) is det.

add_new_arcs([], _, !DepGraph).
add_new_arcs([Caller - CallerInfo | Cs], CallsHO, !DepGraph) :-
    % Only add arcs for callers who call higher order procs.
    ( if set.member(Caller, CallsHO) then
        CallerInfo = strat_ho_info(PossibleCallees0, _),
        set.to_sorted_list(PossibleCallees0, PossibleCallees),
        digraph.lookup_key(!.DepGraph, Caller, CallerKey),
        add_new_arcs2(PossibleCallees, CallerKey, !DepGraph)
    else
        true
    ),
    add_new_arcs(Cs, CallsHO, !DepGraph).

:- pred add_new_arcs2(list(pred_proc_id)::in, hlds_dependency_graph_key::in,
    hlds_dependency_graph::in, hlds_dependency_graph::out) is det.

add_new_arcs2([], _, !DepGraph).
add_new_arcs2([Callee | Cs], CallerKey, !DepGraph) :-
    digraph.lookup_key(!.DepGraph, Callee, CalleeKey),
    digraph.add_edge(CallerKey, CalleeKey, !DepGraph),
    add_new_arcs2(Cs, CallerKey, !DepGraph).

    % For each given pred id, pass all non imported procs onto the
    % stratify_process_procs predicate.
    %
:- pred expand_predids(list(pred_id)::in, module_info::in,
    call_map::in, call_map::out, ho_map::in, ho_map::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

expand_predids([], _, !ProcCalls, !HOInfo, !CallsHO).
expand_predids([PredId | PredIds], ModuleInfo, !ProcCalls, !HOInfo,
        !CallsHO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Procs = pred_info_valid_non_imported_procids(PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    stratify_process_procs(Procs, ModuleInfo, PredId, ArgTypes, ProcTable,
        !ProcCalls, !HOInfo, !CallsHO),
    expand_predids(PredIds, ModuleInfo, !ProcCalls, !HOInfo, !CallsHO).

    % For each given proc id, generate the set of procedures it calls
    % and its higher order info structure.
    %
:- pred stratify_process_procs(list(proc_id)::in, module_info::in, pred_id::in,
    list(mer_type)::in, proc_table::in, call_map::in, call_map::out,
    ho_map::in, ho_map::out, set(pred_proc_id)::in, set(pred_proc_id)::out)
    is det.

stratify_process_procs([], _, _, _, _, !ProcCalls, !HOInfo, !CallsHO).
stratify_process_procs([ProcId | ProcIds], ModuleInfo, PredId, ArgTypes,
        ProcTable, !ProcCalls, !HOInfo, !CallsHO) :-
    stratify_process_proc(ProcId, ModuleInfo, PredId, ArgTypes, ProcTable,
        !ProcCalls, !HOInfo, !CallsHO),
    stratify_process_procs(ProcIds, ModuleInfo, PredId, ArgTypes, ProcTable,
        !ProcCalls, !HOInfo, !CallsHO).

:- pred stratify_process_proc(proc_id::in, module_info::in, pred_id::in,
    list(mer_type)::in, proc_table::in, call_map::in, call_map::out,
    ho_map::in, ho_map::out, set(pred_proc_id)::in, set(pred_proc_id)::out)
    is det.

stratify_process_proc(ProcId, ModuleInfo, PredId, ArgTypes, ProcTable,
        !ProcCalls, !HOInfo, !CallsHO) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_goal(ProcInfo, Goal),
    PredProcId = proc(PredId, ProcId),
    stratify_analyze_proc_body(Goal, Calls, HaveAT, CallsHigherOrder),
    map.det_insert(PredProcId, Calls, !ProcCalls),
    higherorder_in_out(ArgTypes, ArgModes, ModuleInfo, HOInOut),
    map.det_insert(PredProcId, strat_ho_info(HaveAT, HOInOut), !HOInfo),
    (
        CallsHigherOrder = calls_higher_order,
        set.insert(PredProcId, !CallsHO)
    ;
        CallsHigherOrder = does_not_calls_higher_order
    ).

    % Determine if a given set of modes and types indicates that
    % higher order values can be passed into and/or out of a procedure.
    %
:- pred higherorder_in_out(list(mer_type)::in, list(mer_mode)::in,
    module_info::in, ho_in_out::out) is det.

higherorder_in_out(Types, Modes, ModuleInfo, HOInOut) :-
    higherorder_in_out1(Types, Modes, ModuleInfo, no, HOIn, no, HOOut),
    bool_2_ho_in_out(HOIn, HOOut, HOInOut).

:- pred bool_2_ho_in_out(bool::in, bool::in, ho_in_out::out) is det.

bool_2_ho_in_out(yes, no, ho_in).
bool_2_ho_in_out(no, yes, ho_out).
bool_2_ho_in_out(yes, yes, ho_in_out).
bool_2_ho_in_out(no, no, ho_none).

:- pred higherorder_in_out1(list(mer_type)::in, list(mer_mode)::in,
    module_info::in, bool::in, bool::out, bool::in, bool::out) is det.

higherorder_in_out1([], [], _ModuleInfo, !HOIn, !HOOut).
higherorder_in_out1([], [_ | _], _, !HOIn, !HOOut) :-
    unexpected($pred, "mismatched lists").
higherorder_in_out1([_ | _], [], _, !HOIn, !HOOut) :-
    unexpected($pred, "mismatched lists").
higherorder_in_out1([Type | Types], [Mode | Modes], ModuleInfo,
        !HOIn, !HOOut) :-
    ( if
        % XXX We should use a more general check for higher order constants
        % in parameters; users could hide higher order constants in data
        % structures.
        type_is_higher_order(Type)
    then
        ( if mode_is_input(ModuleInfo, Mode) then
            !:HOIn = yes
        else if mode_is_output(ModuleInfo, Mode) then
            !:HOOut = yes
        else
            true
        )
    else
        true
    ),
    higherorder_in_out1(Types, Modes, ModuleInfo, !HOIn, !HOOut).

:- type calls_higher_order
    --->    does_not_calls_higher_order
    ;       calls_higher_order.

    % Return the set of all procedures called in the given goal
    % and all addresses taken in the given goal.
    %
:- pred stratify_analyze_proc_body(hlds_goal::in, set(pred_proc_id)::out,
    set(pred_proc_id)::out, calls_higher_order::out) is det.

stratify_analyze_proc_body(Goal, Calls, TakenAddrs, CallsHO) :-
    set.init(Calls0),
    set.init(TakenAddrs0),
    stratify_analyze_goal(Goal, Calls0, Calls, TakenAddrs0, TakenAddrs,
        does_not_calls_higher_order, CallsHO).

:- pred stratify_analyze_goal(hlds_goal::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    calls_higher_order::in, calls_higher_order::out) is det.

stratify_analyze_goal(Goal, !Calls, !HasAT, !CallsHO) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_Var, RHS, _Mode, Unification, _Context),
        % See if a goal has its address taken.
        (
            % Currently this code assumes that all procs called in a lambda
            % goal have addresses taken. This is not always to case, but
            % should be a suitable approximation for the stratification
            % analysis.
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PredOrFunc,
                _EvalMethod, _NonLocals, _ArgVarsModes, _Determinism,
                LambdaGoal),
            get_called_procs(LambdaGoal, [], CalledProcs),
            set.insert_list(CalledProcs, !HasAT)
        ;
            RHS = rhs_var(_)
        ;
            RHS = rhs_functor(_, _, _)
        ),
        (
            % Currently when this pass is run the construct/4 case will not
            % happen as higher order constants have been transformed to
            % lambda goals. See above.
            Unification = construct(_, ConsId, _, _, _, _, _),
            ( if ConsId = closure_cons(ShroudedPredProcId, _) then
                PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
                set.insert(PredProcId, !HasAT)
            else
                % Do nothing.
                true
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
            % Do nothing.
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated_unify")
        )
    ;
        GoalExpr = plain_call(CPred, CProc, _Args, _Builtin, _UC, _Sym),
        % Add this call to the call list.
        set.insert(proc(CPred, CProc), !Calls)
    ;
        GoalExpr = call_foreign_proc(_Attrib, _CPred, _CProc, _, _, _, _)
        % Do nothing.
        % XXX If the foreign proc may_call_mercury, then we may be missing
        % some calls.
    ;
        GoalExpr = generic_call(_Var, _Vars, _Modes, _MaybeArgRegs, _Det),
        % Record that the higher order call was made.
        !:CallsHO = calls_higher_order
    ;
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        stratify_analyze_goals(Goals, !Calls, !HasAT, !CallsHO)
    ;
        GoalExpr = switch(_Var, _Fail, Cases),
        stratify_analyze_cases(Cases, !Calls, !HasAT, !CallsHO)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        stratify_analyze_goal(Cond, !Calls, !HasAT, !CallsHO),
        stratify_analyze_goal(Then, !Calls, !HasAT, !CallsHO),
        stratify_analyze_goal(Else, !Calls, !HasAT, !CallsHO)
    ;
        GoalExpr = negation(SubGoal),
        stratify_analyze_goal(SubGoal, !Calls, !HasAT, !CallsHO)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The code in these scopes does not make calls (either first order
            % or higher order), and it does not take addresses.
            true
        else
            stratify_analyze_goal(SubGoal, !Calls, !HasAT, !CallsHO)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            stratify_analyze_goal(MainGoal, !Calls, !HasAT, !CallsHO),
            stratify_analyze_goals(OrElseGoals, !Calls, !HasAT, !CallsHO)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            stratify_analyze_goal(SubGoal, !Calls, !HasAT, !CallsHO)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred stratify_analyze_goals(list(hlds_goal)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    calls_higher_order::in, calls_higher_order::out) is det.

stratify_analyze_goals([], !Calls, !HasAT, !CallsHO).
stratify_analyze_goals([Goal | Goals], !Calls, !HasAT, !CallsHO) :-
    stratify_analyze_goal(Goal, !Calls, !HasAT, !CallsHO),
    stratify_analyze_goals(Goals, !Calls, !HasAT, !CallsHO).

:- pred stratify_analyze_cases(list(case)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    calls_higher_order::in, calls_higher_order::out) is det.

stratify_analyze_cases([], !Calls, !HasAT, !CallsHO).
stratify_analyze_cases([Case | Goals], !Calls, !HasAT, !CallsHO) :-
    Case = case(_, _, Goal),
    stratify_analyze_goal(Goal, !Calls, !HasAT, !CallsHO),
    stratify_analyze_cases(Goals, !Calls, !HasAT, !CallsHO).

    % This pred returns a list of all the calls in a given set of goals,
    % including calls in unification lambda functions and pred_proc_id's
    % in constructs.
    %
:- pred get_called_procs(hlds_goal::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

get_called_procs(Goal, !Calls) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = unify(_Var, RHS, _Mode, Unification, _Context),
        (
            % Currently this code assumes that all procs called in a lambda
            % goal have addresses taken. This is not always to case, but
            % should be a suitable approximation for the stratification
            % analysis.
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PredOrFunc,
                _EvalMethod, _NonLocals, _ArgVarsModes, _Determinism,
                LambdaGoal),
            get_called_procs(LambdaGoal, !Calls)
        ;
            RHS = rhs_var(_)
        ;
            RHS = rhs_functor(_, _, _)
        ),
        (
            % Currently when this pass is run the construct/4 case will not
            % happen as higher order constants have been transformed to lambda
            % goals. See above.
            Unification = construct(_, ConsId, _, _, _, _, _),
            ( if ConsId = closure_cons(ShroudedPredProcId, _) then
                PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
                !:Calls = [PredProcId | !.Calls]
            else
                % Do nothing.
                true
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
            % Do nothing.
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated_unify")
        )
    ;
        GoalExpr = plain_call(CPred, CProc, _Args, _Builtin, _UC, _Sym),
        % Add this call to the call list.
        !:Calls = [proc(CPred, CProc) | !.Calls]
    ;
        GoalExpr = call_foreign_proc(_Attrib, _CPred, _CProc, _, _, _, _)
        % Do nothing.
    ;
        GoalExpr = generic_call(_Var, _Vars, _Modes, _MaybeArgRegs, _Det)
        % Do nothing.
    ;
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        get_called_procs_goals(Goals, !Calls)
    ;
        GoalExpr = switch(_Var, _Fail, Cases),
        get_called_procs_cases(Cases, !Calls)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        get_called_procs(Cond, !Calls),
        get_called_procs(Then, !Calls),
        get_called_procs(Else, !Calls)
    ;
        GoalExpr = negation(SubGoal),
        get_called_procs(SubGoal, !Calls)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The code in these scopes does not make calls.
            true
        else
            get_called_procs(SubGoal, !Calls)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            get_called_procs(MainGoal, !Calls),
            get_called_procs_goals(OrElseGoals, !Calls)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            get_called_procs(SubGoal, !Calls)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred get_called_procs_goals(list(hlds_goal)::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

get_called_procs_goals([], !Calls).
get_called_procs_goals([Goal | Goals], !Calls) :-
    get_called_procs(Goal, !Calls),
    get_called_procs_goals(Goals, !Calls).

:- pred get_called_procs_cases(list(case)::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

get_called_procs_cases([], !Calls).
get_called_procs_cases([Case | Cases], !Calls) :-
    Case = case(_, _, Goal),
    get_called_procs(Goal, !Calls),
    get_called_procs_cases(Cases, !Calls).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type error_or_warning
    --->    is_error
    ;       is_warning.

:- func generate_stratify_error(module_info, pred_proc_id, prog_context,
    string, error_or_warning) = error_spec.

generate_stratify_error(ModuleInfo, PPId, Context, Message, ErrorOrWarning)
        = Spec :-
    PPIdDescription = describe_one_proc_name_mode(ModuleInfo, output_mercury,
        should_not_module_qualify, PPId),
    Preamble = [words("In")] ++ PPIdDescription ++ [suffix(":"), nl],
    (
        ErrorOrWarning = is_warning,
        ErrOrWarnMsg = words("warning:"),
        Severity = severity_warning
    ;
        ErrorOrWarning = is_error,
        ErrOrWarnMsg = words("error:"),
        Severity = severity_error
    ),
    MainPieces = [ErrOrWarnMsg, words(Message), nl],
    VerbosePieces =
        [words("A non-stratified loop is a loop in the call graph"),
        words("of the given predicate/function that allows it to call"),
        words("itself in a negated context. This can cause problems for"),
        words("bottom-up evaluation of the predicate/function."), nl],
    Msg = simple_msg(Context,
        [always(Preamble ++ MainPieces),
        verbose_only(verbose_once, VerbosePieces)]),
    Spec = error_spec($pred, Severity, phase_code_gen, [Msg]).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.stratify.
%-----------------------------------------------------------------------------%
