%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: par_loop_control.m.
% Author: pbone.
%
% This module implements the parallel loop control transformation.  Parallel
% conjunctions spawn off their second operand, execute the first and then block
% waiting for the completion of the second.  Therefore, when the second operand
% contains a recursive call the blocked resource consumes memory that orght to
% be avoided.
%
% This can be avoided by spawning off the first operand and continuing with the
% second.  This is acheived by the loop control transformation which provides
% further optimization by using a different structure for synchronization.
% There is one barrier in a loop rather than N (for N recursions) and the
% maximum number of contexts the loop may used is fixed.
%
% Consider this loop:
%
%   map(M, [], []).
%   map(M, [X | Xs], [Y | Ys]) :-
%       (
%           M(X, Y)
%       &
%           map(M, Xs, Ys)
%       ).
%
% It would be transformed to:
%
%   map(M, Xs, Ys) :-
%       create_loop_control(LC, P), % P is the number of contexts to use.
%       map_lc(LC, M, Xs, Ys).
%
%   map(LC, _, [], []) :-
%       finish_loop_control(LC).
%   map(LC, M, [X | Xs], [Y | Ys) :-
%       wait_free_slot(LC, LCS) ->
%       spawn_off(LCS, (
%           M(X, Y),
%           join_and_terminate(LC, LCS)
%       ),
%       map(LC, M, Xs, Ys). % May not use tail recursion.
%
% The parallel conjunction is replaced with a wait_free_slot and spawn_off goals
% for each conjunct except for the last, which is re-written to call the loop
% control version of the predicate.
%
% Rules:
%
% 1. This transformation works when there are multiple parallel conjunctions in
%    different branches.  It also works when the parallel conjunction has more
%    than two conjuncts, in which case all but the right most branch are
%    replaced with the call to spawn_off.
%
% 2. There may be code _after_ the recursive call that consumes variables
%    produced in the first conjunct.  This is safe because the barrier in the
%    base case has been executed.  Any consumption before the recursive call
%    will already be using a future and is safe.
%
% 3. There _may not_ be more than one recursive call along any code-path.  That
%    is to say, the code must be singly recursive so that the base case (and
%    the barrier within) is executed exactly once.
%
% 4. Multiple parallel conjunctions may exist within the body, but due to rule
%    3, only one of them may contain a recursive call.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.par_loop_control.
:- interface.

:- import_module hlds.hlds_module.

%----------------------------------------------------------------------------%

:- pred maybe_par_loop_control_module(module_info::in, module_info::out)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module digraph.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%----------------------------------------------------------------------------%

maybe_par_loop_control_module(!ModuleInfo) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    process_all_nonimported_procs(
        update_module(maybe_par_loop_control_proc(DepInfo)),
        !ModuleInfo).

:- pred maybe_par_loop_control_proc(dependency_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

maybe_par_loop_control_proc(DepInfo, PredProcId, !ProcInfo, !ModuleInfo) :-
    ( loop_control_is_applicable(DepInfo, PredProcId, !.ProcInfo) ->
        proc_info_get_goal(!.ProcInfo, Body0),

        % Re-calculate goal ids.
        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        fill_goal_id_slots_in_proc_body(!.ModuleInfo, VarTypes,
            ContainingGoalMap, Body0, Body),
        proc_info_set_goal(Body, !ProcInfo),
        goal_get_loop_control_par_conjs(Body, PredProcId,
            RecursiveParConjIds),
        (
            ( RecursiveParConjIds = have_not_seen_recursive_call
            ; RecursiveParConjIds = seen_one_recursive_call_on_every_branch
            ; RecursiveParConjIds = seen_unusable_recursion
            )
        ;
            RecursiveParConjIds = seen_usable_recursion_in_par_conj(GoalIds),

            % Go ahead and perform the transformation.
            create_inner_proc(GoalIds, PredProcId, !.ProcInfo,
                ContainingGoalMap,  InnerPredProcId, InnerPredName,
                !ModuleInfo),
            update_outer_proc(PredProcId, InnerPredProcId, InnerPredName,
                !.ModuleInfo, !ProcInfo)
        )
    ;
        true
    ).

%----------------------------------------------------------------------------%

    % Loop control is applicable if the procedure contains a parallel
    % conjunction with exactly two conjuncts whose right conjunct contains a
    % recursive call.
    %
:- pred loop_control_is_applicable(dependency_info::in, pred_proc_id::in,
    proc_info::in) is semidet.

loop_control_is_applicable(DepInfo, PredProcId, ProcInfo) :-
    proc_info_get_has_parallel_conj(ProcInfo, yes),
    proc_info_get_inferred_determinism(ProcInfo, Detism),
    % If the predicate itself is not deterministic then its recursive call
    % will not be deterministic and therefore will not be found in a parallel
    % conjunction.
    ( Detism = detism_det
    ; Detism = detism_cc_multi
    ),
    proc_is_self_recursive(DepInfo, PredProcId).

:- pred proc_is_self_recursive(dependency_info::in, pred_proc_id::in)
    is semidet.

proc_is_self_recursive(DepInfo, PredProcId) :-
    hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),

    % There must be a directly recursive call.
    digraph.lookup_key(DepGraph, PredProcId, SelfKey),
    digraph.is_edge(DepGraph, SelfKey, SelfKey),

    % There must not be a indirectly recursive call.
    % Note: we could handle this in the future by inlining one call within
    % another, but recursion analysis in the deep profiler should support this
    % first.
    digraph.delete_edge(SelfKey, SelfKey, DepGraph, DepGraphWOSelfEdge),
    digraph.tc(DepGraphWOSelfEdge, TCDepGraphWOSelfEdge),
    not digraph.is_edge(TCDepGraphWOSelfEdge, SelfKey, SelfKey).

%----------------------------------------------------------------------------%

:- type seen_usable_recursion
    --->    have_not_seen_recursive_call
                % There is no reachable recursive call in this goal.

    ;       seen_one_recursive_call_on_every_branch
                % There is exactly one recursive call on every reachable
                % branch, Therefore this single recursion can be used if it is
                % within a parallel conjunction.

    ;       seen_unusable_recursion
                % There is recursion but we cannot use it.  This is caused
                % by a number of different reasons, some are:
                %   + Multiple recursion.
                %   + Recursion on some but not all branches or in code that is
                %     not det/cc_multi.
                %   + Usable recursion inside a parallel conjunction that is
                %     inside _another_ parallel conjunction.

    ;       seen_usable_recursion_in_par_conj(list(goal_id)).
                % There is recursion within the right-most conjunct of a
                % parallel conjunction.  There may be multiple cases of this
                % (different parallel conjunctions in different branches).

    % This subtype of seen usable recursion is the set of values for which we
    % should keep searching.
    %
:- inst seen_usable_recursion_continue
    --->    have_not_seen_recursive_call
    ;       seen_one_recursive_call_on_every_branch
    ;       seen_usable_recursion_in_par_conj(ground).

:- pred goal_get_loop_control_par_conjs(hlds_goal::in, pred_proc_id::in,
    seen_usable_recursion::out) is det.

goal_get_loop_control_par_conjs(Goal, SelfPredProcId, SeenUsableRecursion) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Detism = goal_info_get_determinism(GoalInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( instmap_delta_is_reachable(InstmapDelta) ->
        (
            GoalExpr = unify(_, _, _, _, _),
            SeenUsableRecursion0 = have_not_seen_recursive_call
        ;
            GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
            ( SelfPredProcId = proc(PredId, ProcId) ->
                SeenUsableRecursion0 =
                    seen_one_recursive_call_on_every_branch
            ;
                SeenUsableRecursion0 = have_not_seen_recursive_call
            )
        ;
            GoalExpr = generic_call(_, _, _, _),
            % We cannot determine if a generic call is recursive or not,
            % however it most likely is not.  In either case we cannot perform
            % the loop control transformation.
            SeenUsableRecursion0 = have_not_seen_recursive_call
        ;
            GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
            SeenUsableRecursion0 = have_not_seen_recursive_call
        ;
            GoalExpr = conj(ConjType, Conjs),
            (
                ConjType = plain_conj,
                conj_get_loop_control_par_conjs(Conjs, SelfPredProcId,
                    have_not_seen_recursive_call, SeenUsableRecursion0)
            ;
                ConjType = parallel_conj,
                GoalId = goal_info_get_goal_id(GoalInfo),
                par_conj_get_loop_control_par_conjs(Conjs, SelfPredProcId,
                    GoalId, SeenUsableRecursion0)
            )
        ;
            GoalExpr = disj(Disjs),
            % If the disjunction contains a recursive call at all then the
            % recursive call is in an unusable context.
            (
                member(Disj, Disjs),
                goal_calls(Disj, SelfPredProcId)
            ->
                SeenUsableRecursion0 = seen_unusable_recursion
            ;
                SeenUsableRecursion0 = have_not_seen_recursive_call
            )
        ;
            GoalExpr = switch(_, _CanFail, Cases),
            map(case_get_loop_control_par_conjs(SelfPredProcId), Cases,
                SeenUsableRecursionCases),
            % If the switch can fail then there is effectively another branch
            % that has no recursive call.  However, we don not need to test for
            % it here as checking the determinism of the goal will detect such
            % a case.
            merge_loop_control_par_conjs_between_branches_list(
                SeenUsableRecursionCases, SeenUsableRecursion0)
        ;
            GoalExpr = negation(SubGoal),
            goal_get_loop_control_par_conjs(SubGoal, SelfPredProcId,
                SeenUsableRecursion0)
            % If the negation can fail (I don't see how it could possibly be
            % 'det'); then code that checks the determinism below will ensure
            % that any recursion found here is unusable (Like for can-fail
            % switches).
        ;
            GoalExpr = scope(_, SubGoal),
            goal_get_loop_control_par_conjs(SubGoal, SelfPredProcId,
                SeenUsableRecursion0)
        ;
            GoalExpr = if_then_else(_, Cond, Then, Else),
            goal_get_loop_control_par_conjs(Cond, SelfPredProcId,
                SeenUsableRecursionCond),
            (
                SeenUsableRecursionCond = have_not_seen_recursive_call,
                goal_get_loop_control_par_conjs(Then, SelfPredProcId,
                    SeenUsableRecursionThen),
                goal_get_loop_control_par_conjs(Else, SelfPredProcId,
                    SeenUsableRecursionElse),
                merge_loop_control_par_conjs_between_branches(
                    SeenUsableRecursionThen, SeenUsableRecursionElse,
                    SeenUsableRecursion0)
            ;
                % We can't make use of any recursion found in the condition of
                % an if-then-else.
                ( SeenUsableRecursionCond =
                        seen_one_recursive_call_on_every_branch
                ; SeenUsableRecursionCond = seen_unusable_recursion
                ; SeenUsableRecursionCond = seen_usable_recursion_in_par_conj(_)
                ),
                SeenUsableRecursion0 = seen_unusable_recursion
            )
        ;
            GoalExpr = shorthand(_),
            unexpected($module, $pred, "shorthand")
        ),

        % If the goal might fail or might succeed more than once then the
        % recursion is unusable for loop control.
        (
            ( SeenUsableRecursion0 = have_not_seen_recursive_call
            ; SeenUsableRecursion0 = seen_unusable_recursion
            ),
            SeenUsableRecursion = SeenUsableRecursion0
        ;
            ( SeenUsableRecursion0 = seen_one_recursive_call_on_every_branch
            ; SeenUsableRecursion0 = seen_usable_recursion_in_par_conj(_)
            ),
            (
                ( Detism = detism_det
                ; Detism = detism_cc_multi
                ),
                SeenUsableRecursion0 = SeenUsableRecursion
            ;
                ( Detism = detism_semi
                ; Detism = detism_multi
                ; Detism = detism_non
                ; Detism = detism_cc_non
                ; Detism = detism_erroneous
                ; Detism = detism_failure
                ),
                SeenUsableRecursion = seen_unusable_recursion
            )
        )
    ;
        % InstmapDelta is unreachable.
        SeenUsableRecursion = have_not_seen_recursive_call
    ).

    % Analyze the parallel conjunction for a usable recursive call.
    %
    % If any but the last conjunct contain a recursive call then that call is
    % unusable.  If only the last conjunct contains a recursive call then it is
    % usable.
    %
:- pred par_conj_get_loop_control_par_conjs(list(hlds_goal)::in,
    pred_proc_id::in, goal_id::in, seen_usable_recursion::out) is det.

par_conj_get_loop_control_par_conjs(Conjs, SelfPredProcId,
        GoalId, SeenUsableRecursion) :-
    (
        Conjs = [],
        unexpected($module, $pred, "Empty parallel conjunction")
    ;
        Conjs = [_ | _],
        par_conj_get_loop_control_par_conjs_2(Conjs, SelfPredProcId,
            SeenUsableRecursion0),
        (
            SeenUsableRecursion0 = have_not_seen_recursive_call,
            SeenUsableRecursion = SeenUsableRecursion0
        ;
            SeenUsableRecursion0 = seen_one_recursive_call_on_every_branch,
            SeenUsableRecursion = seen_usable_recursion_in_par_conj([GoalId])
        ;
            ( SeenUsableRecursion0 = seen_unusable_recursion
            ; SeenUsableRecursion0 = seen_usable_recursion_in_par_conj(_)
            ),
            SeenUsableRecursion = seen_unusable_recursion
        )
    ).

:- pred par_conj_get_loop_control_par_conjs_2(
    list(hlds_goal)::in(non_empty_list), pred_proc_id::in,
    seen_usable_recursion::out) is det.

par_conj_get_loop_control_par_conjs_2([Conj | Conjs], SelfPredProcId,
        SeenUsableRecursion) :-
    goal_get_loop_control_par_conjs(Conj, SelfPredProcId,
        SeenUsableRecursion0),
    (
        % This is the last conjunct.  Therefore, if it contains a recursive
        % call it is a the recursion we're looking for.
        Conjs = [],
        SeenUsableRecursion = SeenUsableRecursion0
    ;
        Conjs = [_ | _],
        % This is not the last conjunct.  Therefore any recursion it contains
        % is unusable.
        (
            ( SeenUsableRecursion0 = seen_one_recursive_call_on_every_branch
            ; SeenUsableRecursion0 = seen_unusable_recursion
            ; SeenUsableRecursion0 = seen_usable_recursion_in_par_conj(_)
            ),
            SeenUsableRecursion = seen_unusable_recursion
        ;
            SeenUsableRecursion0 = have_not_seen_recursive_call,
            % Analyze the rest of the conjunction.
            par_conj_get_loop_control_par_conjs_2(Conjs, SelfPredProcId,
                SeenUsableRecursion)
        )
    ).

:- pred conj_get_loop_control_par_conjs(hlds_goals::in, pred_proc_id::in,
    seen_usable_recursion::in(seen_usable_recursion_continue),
    seen_usable_recursion::out) is det.

conj_get_loop_control_par_conjs([], _, !SeenUsableRecursion).
conj_get_loop_control_par_conjs([Conj | Conjs], SelfPredProcId,
        !SeenUsableRecursion) :-
    goal_get_loop_control_par_conjs(Conj, SelfPredProcId,
        SeenUsableRecursionConj),
    merge_loop_control_par_conjs_sequential(SeenUsableRecursionConj,
        !SeenUsableRecursion),
    (
        !.SeenUsableRecursion = seen_unusable_recursion
    ;
        ( !.SeenUsableRecursion = seen_one_recursive_call_on_every_branch
        ; !.SeenUsableRecursion = seen_usable_recursion_in_par_conj(_)
        ; !.SeenUsableRecursion = have_not_seen_recursive_call
        ),
        conj_get_loop_control_par_conjs(Conjs, SelfPredProcId,
            !SeenUsableRecursion)
    ).

:- pred case_get_loop_control_par_conjs(pred_proc_id::in, case::in,
        seen_usable_recursion::out) is det.

case_get_loop_control_par_conjs(SelfPredProcId, case(_, _, Goal),
        SeenUsableRecursion) :-
    goal_get_loop_control_par_conjs(Goal, SelfPredProcId,
        SeenUsableRecursion).

:- pred merge_loop_control_par_conjs_sequential(seen_usable_recursion::in,
    seen_usable_recursion::in, seen_usable_recursion::out) is det.

merge_loop_control_par_conjs_sequential(have_not_seen_recursive_call,
        Seen, Seen).
merge_loop_control_par_conjs_sequential(seen_unusable_recursion,
        _, seen_unusable_recursion).
merge_loop_control_par_conjs_sequential(
        seen_one_recursive_call_on_every_branch, Seen0, Seen) :-
    (
        Seen0 = have_not_seen_recursive_call,
        Seen = seen_one_recursive_call_on_every_branch
    ;
        ( Seen0 = seen_one_recursive_call_on_every_branch
        ; Seen0 = seen_unusable_recursion
        ; Seen0 = seen_usable_recursion_in_par_conj(_)
        ),
        Seen = seen_unusable_recursion
    ).
merge_loop_control_par_conjs_sequential(
        seen_usable_recursion_in_par_conj(GoalIds), Seen0, Seen) :-
    (
        Seen0 = have_not_seen_recursive_call,
        Seen = seen_usable_recursion_in_par_conj(GoalIds)
    ;
        ( Seen0 = seen_one_recursive_call_on_every_branch
        ; Seen0 = seen_unusable_recursion
        ; Seen0 = seen_usable_recursion_in_par_conj(_)
        ),
        Seen = seen_unusable_recursion
    ).

:- pred merge_loop_control_par_conjs_between_branches_list(
    list(seen_usable_recursion)::in, seen_usable_recursion::out) is det.

merge_loop_control_par_conjs_between_branches_list([],
        have_not_seen_recursive_call).
merge_loop_control_par_conjs_between_branches_list([Seen | Seens], Result) :-
    foldl(merge_loop_control_par_conjs_between_branches, Seens, Seen, Result).

:- pred merge_loop_control_par_conjs_between_branches(
    seen_usable_recursion::in, seen_usable_recursion::in,
    seen_usable_recursion::out) is det.

merge_loop_control_par_conjs_between_branches(have_not_seen_recursive_call,
        Seen0, Seen) :-
    (
        Seen0 = have_not_seen_recursive_call,
        Seen = have_not_seen_recursive_call
    ;
        ( Seen0 = seen_one_recursive_call_on_every_branch
        ; Seen0 = seen_unusable_recursion
        ),
        Seen = seen_unusable_recursion
    ;
        Seen0 = seen_usable_recursion_in_par_conj(_),
        Seen = Seen0
    ).
merge_loop_control_par_conjs_between_branches(
        seen_one_recursive_call_on_every_branch, Seen0, Seen) :-
    (
        Seen0 = seen_one_recursive_call_on_every_branch,
        Seen = Seen0
    ;
        ( Seen0 = have_not_seen_recursive_call
        ; Seen0 = seen_unusable_recursion
        ; Seen0 = seen_usable_recursion_in_par_conj(_)
        ),
        Seen = seen_unusable_recursion
    ).
merge_loop_control_par_conjs_between_branches(seen_unusable_recursion, _,
        seen_unusable_recursion).
merge_loop_control_par_conjs_between_branches(
        seen_usable_recursion_in_par_conj(GoalIdsA), Seen0, Seen) :-
    (
        Seen0 = have_not_seen_recursive_call,
        Seen = seen_usable_recursion_in_par_conj(GoalIdsA)
    ;
        ( Seen0 = seen_one_recursive_call_on_every_branch
        ; Seen0 = seen_unusable_recursion
        ),
        Seen = seen_unusable_recursion
    ;
        Seen0 = seen_usable_recursion_in_par_conj(GoalIdsB),
        % We do the concatenation in this order so that it is not quadratic
        % when called from merge_loop_control_par_conjs_between_branches_list.
        GoalIds = GoalIdsA ++ GoalIdsB,
        Seen = seen_usable_recursion_in_par_conj(GoalIds)
    ).

%----------------------------------------------------------------------------%

:- pred create_inner_proc(list(goal_id)::in, pred_proc_id::in, proc_info::in,
    containing_goal_map::in, pred_proc_id::out, sym_name::out,
    module_info::in, module_info::out) is det.

create_inner_proc(RecParConjIds, OldPredProcId, OldProcInfo,
        ContainingGoalMap, PredProcId, PredSym, !ModuleInfo) :-
    proc(OldPredId, OldProcId) = OldPredProcId,
    module_info_pred_info(!.ModuleInfo, OldPredId, OldPredInfo),

    % Gather data to build the new pred/proc.
    module_info_get_name(!.ModuleInfo, ModuleName),
    PredOrFunc = pred_info_is_pred_or_func(OldPredInfo),
    make_pred_name(ModuleName, "LoopControl", yes(PredOrFunc),
        pred_info_name(OldPredInfo), newpred_parallel_loop_control, PredSym0),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is parallelised. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    proc_id_to_int(OldProcId, OldProcInt),
    add_sym_name_suffix(PredSym0, "_" ++ int_to_string(OldProcInt), PredSym),
    pred_info_get_context(OldPredInfo, Context),
    pred_info_get_origin(OldPredInfo, OldOrigin),
    Origin = origin_transformed(transform_parallel_loop_control, OldOrigin,
        OldPredId),
    some [!Markers] (
        init_markers(!:Markers),
        add_marker(marker_is_impure, !Markers),
        add_marker(marker_calls_are_fully_qualified, !Markers),
        Markers = !.Markers
    ),
    pred_info_get_typevarset(OldPredInfo, TypeVarSet),
    pred_info_get_exist_quant_tvars(OldPredInfo, ExistQVars),
    pred_info_get_class_context(OldPredInfo, ClassConstraints),
    pred_info_get_arg_types(OldPredInfo, ArgTypes0),

    some [!PredInfo] (
        % Construct the pred info structure.  We initially construct it with
        % the old proc info which will be replaced below.
        pred_info_create(ModuleName, PredSym, PredOrFunc, Context, Origin,
            status_local, Markers, ArgTypes0, TypeVarSet, ExistQVars,
            ClassConstraints, set.init, map.init, OldProcInfo, ProcId,
            !:PredInfo),

        % Add the new predicate to the module.
        some [!PredTable] (
            module_info_get_predicate_table(!.ModuleInfo, !:PredTable),
            predicate_table_insert(!.PredInfo, PredId, !PredTable),
            module_info_set_predicate_table(!.PredTable, !ModuleInfo)
        ),
        PredProcId = proc(PredId, ProcId),

        % Now transform the predicate, this could not be done earlier because
        % we needed to know the knew PredProcId to re-write the recursive calls
        % in the body.
        proc_info_get_argmodes(OldProcInfo, ArgModes0),
        proc_info_get_headvars(OldProcInfo, HeadVars0),
        proc_info_get_varset(OldProcInfo, VarSet0),
        proc_info_get_vartypes(OldProcInfo, VarTypes0),
        proc_info_get_goal(OldProcInfo, Body0),

        varset.new_named_var("LC", LCVar, VarSet0, VarSet1),
        map.det_insert(LCVar, loop_control_var_type, VarTypes0, VarTypes1),
        should_preserve_tail_recursion(!.ModuleInfo, PreserveTailRecursion),
        get_wait_free_slot_proc(!.ModuleInfo, WaitFreeSlotProc),

        Info = loop_control_info(LCVar, OldPredProcId, PredProcId, PredSym,
            PreserveTailRecursion, WaitFreeSlotProc, lc_wait_free_slot_name),
        goal_loop_control_all_paths(Info, RecParConjIds,
            ContainingGoalMap, Body0, Body, VarSet1, VarSet,
            VarTypes1, VarTypes),

        % Now create the new proc_info structure.
        HeadVars = [LCVar | HeadVars0],
        ArgTypes = [loop_control_var_type | ArgTypes0],
        Ground = ground(shared, none),
        In = (Ground -> Ground),
        ArgModes = [In | ArgModes0],

        proc_info_get_inst_varset(OldProcInfo, InstVarSet),
        proc_info_get_rtti_varmaps(OldProcInfo, RttiVarMaps),
        proc_info_get_inferred_determinism(OldProcInfo, Detism),
        proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet,
            ArgModes, detism_decl_none, Detism, Body, RttiVarMaps,
            address_is_not_taken, map.init, ProcInfo),

        % Update the other structures
        pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes, !PredInfo),
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo),
        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
    ).

:- pred should_preserve_tail_recursion(module_info::in,
    preserve_tail_recursion::out) is det.

should_preserve_tail_recursion(ModuleInfo, PreserveTailRecursion) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals,
        par_loop_control_preserve_tail_recursion, PreserveTailRecursionBool),
    (
        PreserveTailRecursionBool = yes,
        PreserveTailRecursion = preserve_tail_recursion
    ;
        PreserveTailRecursionBool = no,
        PreserveTailRecursion = do_not_preserve_tail_recursion
    ).

:- type loop_control_info
    --->    loop_control_info(
                lci_lc_var                      :: prog_var,
                lci_rec_pred_proc_id            :: pred_proc_id,
                lci_inner_pred_proc_id          :: pred_proc_id,
                lci_inner_pred_name             :: sym_name,
                lci_preserve_tail_recursion     :: preserve_tail_recursion,
                lci_wait_free_slot_proc         :: pred_proc_id,
                lci_wait_free_slot_proc_name    :: sym_name
            ).

:- type preserve_tail_recursion
    --->    preserve_tail_recursion
    ;       do_not_preserve_tail_recursion.

:- pred goal_loop_control_all_paths(loop_control_info::in, list(goal_id)::in,
    containing_goal_map::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

goal_loop_control_all_paths(Info, GoalIds, ContainingGoalMap, !Goal,
        !VarSet, !VarTypes) :-
    GoalPaths = map(goal_id_to_forward_path(ContainingGoalMap), GoalIds),
    foldl3(goal_loop_control(Info), GoalPaths, !Goal, !VarSet,
        !VarTypes).

:- pred goal_loop_control(loop_control_info::in, forward_goal_path::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

goal_loop_control(Info, GoalPath0, !Goal, !VarSet, !VarTypes) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    ( goal_path_remove_first(GoalPath0, GoalPath, Step) ->
        format("Couldn't follow goal path step: \"%s\"", [s(string(Step))],
            ErrorString),
        (
            Step = step_conj(N),
            (
                GoalExpr0 = conj(plain_conj, Conjs0),
                list.index1(Conjs0, N, Conj0)
            ->
                goal_loop_control(Info, GoalPath, Conj0, Conj,
                    !VarSet, !VarTypes),
                det_replace_nth(Conjs0, N, Conj, Conjs),
                GoalExpr = conj(plain_conj, Conjs)
            ;
                unexpected($module, $pred, ErrorString)
            )
        ;
            Step = step_switch(N, _),
            (
                GoalExpr0 = switch(Var, CanFail, Cases0),
                list.index1(Cases0, N, Case0)
            ->
                Goal0 = Case0 ^ case_goal,
                goal_loop_control(Info, GoalPath, Goal0, Goal,
                    !VarSet, !VarTypes),
                Case = Case0 ^ case_goal := Goal,
                det_replace_nth(Cases0, N, Case, Cases),
                GoalExpr = switch(Var, CanFail, Cases)
            ;
                unexpected($module, $pred, ErrorString)
            )
        ;
            Step = step_ite_then,
            ( GoalExpr0 = if_then_else(Vars, Cond, Then0, Else) ->
                goal_loop_control(Info, GoalPath, Then0, Then,
                    !VarSet, !VarTypes),
                GoalExpr = if_then_else(Vars, Cond, Then, Else)
            ;
                unexpected($module, $pred, ErrorString)
            )
        ;
            Step = step_ite_else,
            ( GoalExpr0 = if_then_else(Vars, Cond, Then, Else0) ->
                goal_loop_control(Info, GoalPath, Else0, Else,
                    !VarSet, !VarTypes),
                GoalExpr = if_then_else(Vars, Cond, Then, Else)
            ;
                unexpected($module, $pred, ErrorString)
            )
        ;
            Step = step_scope(_),
            ( GoalExpr0 = scope(Reason, SubGoal0) ->
                goal_loop_control(Info, GoalPath, SubGoal0, SubGoal,
                    !VarSet, !VarTypes),
                GoalExpr = scope(Reason, SubGoal)
            ;
                unexpected($module, $pred, ErrorString)
            )
        ;
            ( Step = step_ite_cond
            ; Step = step_disj(_)
            ; Step = step_neg
            ; Step = step_lambda
            ; Step = step_try
            ; Step = step_atomic_main
            ; Step = step_atomic_orelse(_)
            ),
            unexpected($module, $pred,
                format("Unexpected step in goal path \"%s\"",
                [s(string(Step))]))
        ),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        fixup_goal_info(Info, !Goal)
    ;
        ( GoalExpr0 = conj(parallel_conj, Conjs) ->
            par_conj_loop_control(Info, reverse(Conjs), GoalInfo, !:Goal,
                !VarSet, !VarTypes)
        ;
            unexpected($module, $pred, "expected parallel conjunction")
        )
    ).

:- pred par_conj_loop_control(loop_control_info::in, list(hlds_goal)::in,
    hlds_goal_info::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

par_conj_loop_control(_, [], _, _, !VarSet, !VarTypes) :-
    unexpected($module, $pred, "empty parallel conjunction").
par_conj_loop_control(Info, [LastConj0 | RevConjs], GoalInfo, Goal, !VarSet,
        !VarTypes) :-
    % Re-write the recursive call in the last conjunct.
    goal_rewrite_recursive_call(Info, LastConj0, LastConj, _),
    expand_plain_conj(LastConj, LastConjGoals),

    % Process the remaining conjuncts, building up the nested set of ITEs from
    % inside to outside.
    par_conj_loop_control2(Info, RevConjs, LastConjGoals, Goals, !VarSet,
        !VarTypes),
    create_conj_from_list(Goals, plain_conj, Goal0),
    Goal1 = Goal0 ^ hlds_goal_info := GoalInfo,
    fixup_goal_info(Info, Goal1, Goal).

    % Process each of the conjuncts in reverse order, building the new
    % expression from them.
    %
:- pred par_conj_loop_control2(loop_control_info::in, list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

par_conj_loop_control2(_, [], LaterGoals, LaterGoals, !VarSet, !VarTypes).
par_conj_loop_control2(Info, [Conj | RevConjs], LaterGoals, Goals, !VarSet,
        !VarTypes) :-
    % Create the "get free slot" call..
    create_get_free_slot_goal(Info, LCSVar, GetFreeSlotGoal, !VarSet,
        !VarTypes),

    % Wrap Conj in the loop control scope.
    LCVar = Info ^ lci_lc_var,
    ConjGoalInfo = Conj ^ hlds_goal_info,
    some [!NonLocals] (
        !:NonLocals = goal_info_get_nonlocals(ConjGoalInfo),
        insert(LCSVar, !NonLocals),
        insert(LCVar, !NonLocals),
        goal_info_set_nonlocals(!.NonLocals, ConjGoalInfo, ScopeGoalInfo)
    ),
    ScopeGoal = hlds_goal(scope(loop_control(LCVar, LCSVar), Conj),
        ScopeGoalInfo),

    % Process earlier conjuncts.
    Goals0 = [GetFreeSlotGoal, ScopeGoal | LaterGoals],
    par_conj_loop_control2(Info, RevConjs, Goals0, Goals, !VarSet, !VarTypes).

    % Re-write any recursive calls in this goal.
    %
    % This predicate's argument order does not conform to the Mercury coding
    % standards, this is deliberate as it makes it easier to call from
    % list.map2.
    %
:- pred goal_rewrite_recursive_call(loop_control_info::in,
    hlds_goal::in, hlds_goal::out, fixup_goal_info::out) is det.

goal_rewrite_recursive_call(Info, !Goal, FixupGoalInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        FixupGoalInfo = do_not_fixup_goal_info
    ;
        GoalExpr0 = plain_call(CallPredId0, CallProcId0, Args0, Builtin,
            MaybeUnify, _Name0),
        RecPredProcId = Info ^ lci_rec_pred_proc_id,
        ( RecPredProcId = proc(CallPredId0, CallProcId0) ->
            NewPredProcId = Info ^ lci_inner_pred_proc_id,
            proc(CallPredId, CallProcId) = NewPredProcId,
            LCVar = Info ^ lci_lc_var,
            Args = [LCVar | Args0],
            Name = Info ^ lci_inner_pred_name,
            GoalExpr = plain_call(CallPredId, CallProcId, Args, Builtin,
                MaybeUnify, Name),
            FixupGoalInfo = fixup_goal_info
        ;
            GoalExpr = GoalExpr0,
            FixupGoalInfo = do_not_fixup_goal_info
        )
    ;
        GoalExpr0 = conj(ConjType, Conjs0),
        map2(goal_rewrite_recursive_call(Info), Conjs0, Conjs,
            FixupGoalInfoConjs),
        goals_fixup_goal_info(FixupGoalInfoConjs, FixupGoalInfo),
        GoalExpr = conj(ConjType, Conjs)
    ;
        GoalExpr0 = disj(Disjs0),
        map2(goal_rewrite_recursive_call(Info), Disjs0, Disjs,
            FixupGoalInfoDisjs),
        goals_fixup_goal_info(FixupGoalInfoDisjs, FixupGoalInfo),
        GoalExpr = disj(Disjs)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        map2(case_rewrite_recursive_call(Info), Cases0, Cases,
            FixupGoalInfoCases),
        goals_fixup_goal_info(FixupGoalInfoCases, FixupGoalInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        goal_rewrite_recursive_call(Info, SubGoal0, SubGoal,
            FixupGoalInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        goal_rewrite_recursive_call(Info, SubGoal0, SubGoal,
            FixupGoalInfo),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        goal_rewrite_recursive_call(Info, Cond0, Cond, FixupGoalInfoCond),
        goal_rewrite_recursive_call(Info, Then0, Then, FixupGoalInfoThen),
        goal_rewrite_recursive_call(Info, Else0, Else, FixupGoalInfoElse),
        goals_fixup_goal_info([FixupGoalInfoCond, FixupGoalInfoThen,
                FixupGoalInfoElse], FixupGoalInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($module, $pred, "shorthand")
    ),
    !:Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        FixupGoalInfo = fixup_goal_info,
        fixup_goal_info(Info, !Goal),
        ( GoalExpr = plain_call(_, _, _, _, _, _) ->
            goal_add_feature(feature_do_not_tailcall, !Goal)
        ;
            true
        )
    ;
        FixupGoalInfo = do_not_fixup_goal_info
    ).

:- pred case_rewrite_recursive_call(loop_control_info::in,
    case::in, case::out, fixup_goal_info::out) is det.

case_rewrite_recursive_call(Info, !Case, FixupGoalInfo) :-
    some [!Goal] (
        !:Goal = !.Case ^ case_goal,
        goal_rewrite_recursive_call(Info, !Goal, FixupGoalInfo),
        !Case ^ case_goal := !.Goal
    ).

:- pred goals_fixup_goal_info(list(fixup_goal_info)::in, fixup_goal_info::out)
    is det.

goals_fixup_goal_info(List, Fixup) :-
    ( list.contains(List, fixup_goal_info) ->
        Fixup = fixup_goal_info
    ;
        Fixup = do_not_fixup_goal_info
    ).

%----------------------------------------------------------------------------%

:- pred create_get_free_slot_goal(loop_control_info::in, prog_var::out,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

create_get_free_slot_goal(Info, LCSVar, Goal, !VarSet,
        !VarTypes) :-
    varset.new_named_var("LCS", LCSVar, !VarSet),
    map.det_insert(LCSVar, loop_control_slot_var_type, !VarTypes),
    LCVar = Info ^ lci_lc_var,
    proc(PredId, ProcId) = Info ^ lci_wait_free_slot_proc,
    SymName = Info ^ lci_wait_free_slot_proc_name,

    GoalExpr = plain_call(PredId, ProcId, [LCVar, LCSVar], not_builtin, no,
        SymName),
    NonLocals = list_to_set([LCVar, LCSVar]),
    InstmapDelta = instmap_delta_bind_var(LCSVar),
    GoalInfo = impure_init_goal_info(NonLocals, InstmapDelta, detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- type fixup_goal_info
    --->    fixup_goal_info
    ;       do_not_fixup_goal_info.

    % Fixup goalinfo after performing the loop control transformation.
    %
:- pred fixup_goal_info(loop_control_info::in, hlds_goal::in, hlds_goal::out)
    is det.

fixup_goal_info(Info, hlds_goal(GoalExpr, !.GoalInfo),
        hlds_goal(GoalExpr, !:GoalInfo)) :-
    LCVar = Info ^ lci_lc_var,
    some [!NonLocals] (
        !:NonLocals = goal_info_get_nonlocals(!.GoalInfo),
        insert(LCVar, !NonLocals),
        goal_info_set_nonlocals(!.NonLocals, !GoalInfo)
    ),
    goal_info_set_purity(purity_impure, !GoalInfo).

%----------------------------------------------------------------------------%

:- pred update_outer_proc(pred_proc_id::in, pred_proc_id::in, sym_name::in,
    module_info::in, proc_info::in, proc_info::out) is det.

update_outer_proc(PredProcId, InnerPredProcId, InnerPredName, ModuleInfo,
        !ProcInfo) :-
    proc(PredId, _) = PredProcId,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, HeadVarTypes),
    proc_info_get_headvars(!.ProcInfo, HeadVars0),
    proc_info_get_inferred_determinism(!.ProcInfo, Detism),
    proc_info_get_goal(!.ProcInfo, OrigGoal),
    OrigInstmapDelta = goal_info_get_instmap_delta(OrigGoal ^ hlds_goal_info),
    some [!VarSet, !VarTypes] (
        % Re-build the variables in the procedure with smaller sets.
        varset.init(!:VarSet),
        map.init(!:VarTypes),
        proc_info_get_varset(!.ProcInfo, OldVarSet),
        foldl3_corresponding(add_old_var_to_sets(OldVarSet), HeadVars0,
            HeadVarTypes, !VarSet, !VarTypes, map.init, Remap),
        map(map.lookup(Remap), HeadVars0, HeadVars),
        proc_info_set_headvars(HeadVars, !ProcInfo),

        % Fix rtti varmaps.
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarmaps0),
        apply_substitutions_to_rtti_varmaps(map.init, map.init, Remap,
            RttiVarmaps0, RttiVarmaps),
        proc_info_set_rtti_varmaps(RttiVarmaps, !ProcInfo),

        % Create a variable for the number of worker contexts, we control this
        % in the compiler so that it can be adjusted using profiler feedback
        % (for auto-parallelisation), but for now we just set it using a runtime
        % call so that it can be tuned.
        varset.new_named_var("NumContexts", NumContextsVar, !VarSet),
        map.det_insert(NumContextsVar, builtin_type(builtin_type_int),
            !VarTypes),
        get_lc_default_num_contexts_proc(ModuleInfo, LCDefaultNumContextsPredId,
            LCDefaultNumContextsProcId),
        goal_info_init(list_to_set([NumContextsVar]),
            instmap_delta_bind_var(NumContextsVar),
            detism_det, purity_pure, GetNumContextsGoalInfo),
        GetNumContextsGoal = hlds_goal(plain_call(LCDefaultNumContextsPredId,
                LCDefaultNumContextsProcId, [NumContextsVar],
                not_builtin, no, lc_default_num_contexts_name),
            GetNumContextsGoalInfo),

        % Create the call to lc_create
        varset.new_named_var("LC", LCVar, !VarSet),
        map.det_insert(LCVar, loop_control_var_type, !VarTypes),
        get_lc_create_proc(ModuleInfo, LCCreatePredId, LCCreateProcId),
        goal_info_init(list_to_set([NumContextsVar, LCVar]),
            instmap_delta_bind_var(LCVar), detism_det, purity_pure,
            LCCreateGoalInfo),
        LCCreateGoal = hlds_goal(plain_call(LCCreatePredId,
                LCCreateProcId, [NumContextsVar, LCVar], not_builtin, no,
                lc_create_name),
            LCCreateGoalInfo),

        % Create the inner call.
        InnerCallArgs = [LCVar | HeadVars],
        NonLocals = list_to_set(InnerCallArgs),
        % The instmap of the call to the transformed body has the same instmap
        % delta as the original body.
        remap_instmap(Remap, OrigInstmapDelta, InstmapDelta),
        goal_info_init(NonLocals, InstmapDelta, Detism, purity_impure,
            InnerProcCallGoalInfo),
        proc(InnerPredId, InnerProcId) = InnerPredProcId,
        InnerProcCallGoal = hlds_goal(plain_call(InnerPredId, InnerProcId,
            InnerCallArgs, not_builtin, no, InnerPredName),
            InnerProcCallGoalInfo),

        % Build a conjunction of these goals.
        goal_info_init(list_to_set(HeadVars), InstmapDelta, Detism,
            purity_impure, ConjGoalInfo),
        ConjGoal = hlds_goal(conj(plain_conj,
                [GetNumContextsGoal, LCCreateGoal, InnerProcCallGoal]),
            ConjGoalInfo),

        OrigPurity = goal_info_get_purity(OrigGoal ^ hlds_goal_info),
        (
            OrigPurity = purity_impure,
            % The impurity introduced by this transformation does not need
            % to be promised away.
            Body = ConjGoal
        ;
            ( OrigPurity = purity_pure
            ; OrigPurity = purity_semipure
            ),
            % Wrap the body in a scope to promise away the impurity.
            goal_info_set_purity(purity_pure, ConjGoalInfo, ScopeGoalInfo),
            Body = hlds_goal(scope(promise_purity(OrigPurity), ConjGoal),
                ScopeGoalInfo)
        ),

        proc_info_set_goal(Body, !ProcInfo),
        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo)
    ).

:- pred add_old_var_to_sets(prog_varset::in, prog_var::in, mer_type::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

add_old_var_to_sets(OldVarSet, OldVar, VarType, !VarSet, !VarTypes,
        !Remap) :-
    ( varset.search_name(OldVarSet, OldVar, Name) ->
        varset.new_named_var(Name, Var, !VarSet)
    ;
        varset.new_var(Var, !VarSet)
    ),
    map.det_insert(Var, VarType, !VarTypes),
    map.det_insert(OldVar, Var, !Remap).

:- pred remap_instmap(map(prog_var, prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

remap_instmap(Remap, OldInstmapDelta, !:InstmapDelta) :-
    instmap_delta_to_assoc_list(OldInstmapDelta, VarInsts),
    instmap_delta_init_reachable(!:InstmapDelta),
    foldl((pred((OldVar - Inst)::in, IMD0::in, IMD::out) is det :-
            map.lookup(Remap, OldVar, Var),
            instmap_delta_set_var(Var, Inst, IMD0, IMD)
        ), VarInsts, !InstmapDelta).

%--------------------------------------------------------------------%

:- func loop_control_var_type = mer_type.

loop_control_var_type = defined_type(Sym, [], kind_star) :-
    Sym = qualified(par_builtin_module_sym, "loop_control").

:- func loop_control_slot_var_type = mer_type.

loop_control_slot_var_type = defined_type(Sym, [], kind_star) :-
    Sym = qualified(par_builtin_module_sym, "loop_control_slot").

:- func lc_wait_free_slot_name = sym_name.

lc_wait_free_slot_name =
    qualified(par_builtin_module_sym, lc_wait_free_slot_name_unqualified).

:- func lc_wait_free_slot_name_unqualified = string.

lc_wait_free_slot_name_unqualified = "lc_wait_free_slot".

:- pred get_wait_free_slot_proc(module_info::in, pred_proc_id::out) is det.

get_wait_free_slot_proc(ModuleInfo, proc(PredId, ProcId)) :-
    lookup_lc_pred_proc(ModuleInfo, lc_wait_free_slot_name_unqualified, 2, PredId,
        ProcId).

:- func lc_default_num_contexts_name_unqualified = string.

lc_default_num_contexts_name_unqualified = "lc_default_num_contexts".

:- func lc_default_num_contexts_name = sym_name.

lc_default_num_contexts_name =
    qualified(par_builtin_module_sym,
        lc_default_num_contexts_name_unqualified).

:- pred get_lc_default_num_contexts_proc(module_info::in, pred_id::out,
    proc_id::out) is det.

get_lc_default_num_contexts_proc(ModuleInfo, PredId, ProcId) :-
    lookup_lc_pred_proc(ModuleInfo, lc_default_num_contexts_name_unqualified,
        1, PredId, ProcId).

:- func lc_create_name_unqualified = string.

lc_create_name_unqualified = "lc_create".

:- func lc_create_name = sym_name.

lc_create_name =
    qualified(par_builtin_module_sym, lc_create_name_unqualified).

:- pred get_lc_create_proc(module_info::in, pred_id::out, proc_id::out) is det.

get_lc_create_proc(ModuleInfo, PredId, ProcId) :-
    lookup_lc_pred_proc(ModuleInfo, lc_create_name_unqualified, 2, PredId,
        ProcId).

:- pred lookup_lc_pred_proc(module_info::in, string::in, arity::in,
    pred_id::out, proc_id::out) is det.

lookup_lc_pred_proc(ModuleInfo, Sym, Arity, PredId, ProcId) :-
    lookup_builtin_pred_proc_id(ModuleInfo, par_builtin_module_sym,
        Sym, pf_predicate, Arity, only_mode, PredId, ProcId).

:- func par_builtin_module_sym = sym_name.

par_builtin_module_sym = unqualified("par_builtin").

%----------------------------------------------------------------------------%
:- end_module transform_hlds.par_loop_control.
%----------------------------------------------------------------------------%
