%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: par_loop_control.m.
% Author: pbone.
%
% This module implements the parallel loop control transformation.
% This transformation operates on procedure bodies that contain exactly one
% recursive call which occurs in the second and last conjunct of a parallel
% conjunction.
%
% Normally, this parallel conjunction would spawn off their second conjunct,
% execute the first conjunct, and then block waiting for the completion of
% the second. When the second conjunct contains a recursive call, the blocked
% first computation will be a context, and will thus have a high memory
% footprint due to its stacks. The objective of our transformation is to reduce
% this memory footprint.
%
% The way we do this is by spawning off the first parallel conjunct, and
% continuing execution of the second. Since the second conjunct is a recursive
% call, we will continue spawning off first conjuncts until we reach a limit
% that is imposed by a loop control structure. This limit prevents us from
% swamping the available CPUs with too much work. It also allows us to
% use one barrier for ALL the loop iterations, rather than one barrier
% for EACH loop iteration.
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
%       wait_free_slot(LC, LCS),
%       spawn_off(LCS, (
%           M(X, Y),
%           join_and_terminate(LC, LCS)
%       )),
%       map(LC, M, Xs, Ys). % May not use tail recursion.
%
% The parallel conjunction is replaced with a wait_free_slot goal and a
% spawn_off goal for each conjunct except for the last. The last is rewritten
% to call the loop control version of the predicate.
%
% Rules:
%
% 1. This transformation works when there are multiple parallel conjunctions in
%    different branches. It also works when the parallel conjunction has more
%    than two conjuncts, in which case all but the right most branch are
%    replaced with the call to spawn_off.
%
% 2. There may be code _after_ the recursive call that consumes variables
%    produced in the first conjunct. This is safe because we get to such code
%    only *after* the barrier in the base case has been executed. Any
%    consumption before the recursive call will already be using a future,
%    and is therefore safe.
%
% 3. The predicate must be singly recursive, i.e. its body cannot have
%    more than one recursive call along any execution path. We need this
%    to ensure that the base case (and its barrier) is executed exactly once.
%
% 4. Multiple parallel conjunctions may exist within the body, but due to rule
%    3, only one of them may contain a recursive call.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.par_loop_control.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%----------------------------------------------------------------------------%

:- pred maybe_par_loop_control_module(module_info::in, module_info::out)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

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

:- pred maybe_par_loop_control_proc(hlds_dependency_info::in,
    pred_proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

maybe_par_loop_control_proc(DepInfo, PredProcId, !ProcInfo, !ModuleInfo) :-
    ( if loop_control_is_applicable(DepInfo, PredProcId, !.ProcInfo) then
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
    else
        true
    ).

%----------------------------------------------------------------------------%

    % Loop control is applicable if the procedure contains a parallel
    % conjunction with exactly two conjuncts whose right conjunct contains a
    % recursive call.
    %
:- pred loop_control_is_applicable(hlds_dependency_info::in,
    pred_proc_id::in, proc_info::in) is semidet.

loop_control_is_applicable(DepInfo, PredProcId, ProcInfo) :-
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    HasParallelConj = has_parallel_conj,
    proc_info_get_inferred_determinism(ProcInfo, Detism),
    % If the predicate itself is not deterministic then its recursive call
    % will not be deterministic and therefore will not be found in a parallel
    % conjunction.
    ( Detism = detism_det
    ; Detism = detism_cc_multi
    ),
    proc_is_self_recursive(DepInfo, PredProcId).

:- pred proc_is_self_recursive(hlds_dependency_info::in, pred_proc_id::in)
    is semidet.

proc_is_self_recursive(DepInfo, PredProcId) :-
    DepGraph = dependency_info_get_graph(DepInfo),

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
            % There is exactly one recursive call on every reachable branch.
            % Therefore this single recursion can be used if it is within
            % a parallel conjunction.

    ;       seen_unusable_recursion
            % There is recursion, but we cannot use it. There may be several
            % reasons for why we cannot use the transformation, including
            %   + Multiple recursion.
            %   + Recursion on some but not all branches or in code that is
            %     not det/cc_multi.
            %   + Usable recursion inside a parallel conjunction that is
            %     inside _another_ parallel conjunction.

    ;       seen_usable_recursion_in_par_conj(list(goal_id)).
            % There is recursion within the right-most conjunct of a
            % parallel conjunction. There may be multiple cases of this
            % (different parallel conjunctions in different branches).

    % This subtype of seen usable recursion is the set of values for which we
    % should keep searching.
    %
:- inst seen_usable_recursion_continue for seen_usable_recursion/0
    --->    have_not_seen_recursive_call
    ;       seen_one_recursive_call_on_every_branch
    ;       seen_usable_recursion_in_par_conj(ground).

:- pred goal_get_loop_control_par_conjs(hlds_goal::in, pred_proc_id::in,
    seen_usable_recursion::out) is det.

goal_get_loop_control_par_conjs(Goal, SelfPredProcId, SeenUsableRecursion) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Detism = goal_info_get_determinism(GoalInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_reachable(InstmapDelta) then
        (
            GoalExpr = unify(_, _, _, _, _),
            SeenUsableRecursion0 = have_not_seen_recursive_call
        ;
            GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
            ( if SelfPredProcId = proc(PredId, ProcId) then
                SeenUsableRecursion0 =
                    seen_one_recursive_call_on_every_branch
            else
                SeenUsableRecursion0 = have_not_seen_recursive_call
            )
        ;
            GoalExpr = generic_call(_, _, _, _, _),
            % We cannot determine if a generic call is recursive or not,
            % however it most likely is not. In either case, we cannot perform
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
            GoalExpr = disj(_),
            % If the disjunction contains a recursive call at all, then the
            % recursive call is in an unusable context.
            ( if goal_calls(Goal, SelfPredProcId) then
                SeenUsableRecursion0 = seen_unusable_recursion
            else
                SeenUsableRecursion0 = have_not_seen_recursive_call
            )
        ;
            GoalExpr = switch(_, _CanFail, Cases),
            list.map(case_get_loop_control_par_conjs(SelfPredProcId), Cases,
                SeenUsableRecursionCases),
            % If the switch can fail, then there is effectively another branch
            % that has no recursive call. However, we do not need to test for
            % this here, as checking the determinism of the goal will detect
            % such cases.
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
            % If the scope does a cut, then any recursion inside SubGoal
            % is unusable, but the determinism check below will catch that.
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
                % We cannot make use of any recursion found in the condition
                % of an if-then-else.
                ( SeenUsableRecursionCond =
                    seen_one_recursive_call_on_every_branch
                ; SeenUsableRecursionCond = seen_unusable_recursion
                ; SeenUsableRecursionCond =
                    seen_usable_recursion_in_par_conj(_)
                ),
                SeenUsableRecursion0 = seen_unusable_recursion
            )
        ;
            GoalExpr = shorthand(_),
            unexpected($pred, "shorthand")
        ),

        % If the goal might fail or might succeed more than once, then any
        % recursion in it is unusable for loop control.
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
                SeenUsableRecursion = SeenUsableRecursion0
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
    else
        % InstmapDelta is unreachable.
        SeenUsableRecursion = have_not_seen_recursive_call
    ).

    % Analyze the parallel conjunction for a usable recursive call.
    %
    % If any but the last conjunct contains a recursive call, then that
    % recursive call is unusable. If only the last conjunct contains
    % a recursive call, then that recursion is usable.
    %
:- pred par_conj_get_loop_control_par_conjs(list(hlds_goal)::in,
    pred_proc_id::in, goal_id::in, seen_usable_recursion::out) is det.

par_conj_get_loop_control_par_conjs(Conjs, SelfPredProcId,
        GoalId, SeenUsableRecursion) :-
    (
        Conjs = [],
        unexpected($pred, "Empty parallel conjunction")
    ;
        Conjs = [Head | Tail],
        par_conj_get_loop_control_par_conjs_lag(Head, Tail, SelfPredProcId,
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

:- pred par_conj_get_loop_control_par_conjs_lag(hlds_goal::in,
    list(hlds_goal)::in, pred_proc_id::in, seen_usable_recursion::out) is det.

par_conj_get_loop_control_par_conjs_lag(Conj, Conjs, SelfPredProcId,
        SeenUsableRecursion) :-
    goal_get_loop_control_par_conjs(Conj, SelfPredProcId,
        SeenUsableRecursion0),
    (
        % This is the last conjunct. Therefore, if it contains a recursive
        % call it is a the recursion we're looking for.
        Conjs = [],
        SeenUsableRecursion = SeenUsableRecursion0
    ;
        Conjs = [Head | Tail],
        % This is not the last conjunct. Therefore any recursion it contains
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
            par_conj_get_loop_control_par_conjs_lag(Head, Tail, SelfPredProcId,
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
    list.foldl(merge_loop_control_par_conjs_between_branches, Seens,
        Seen, Result).

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

    some [!PredInfo, !Body, !VarSet, !VarTypes] (
        % Construct the pred info structure. We initially construct it with
        % the old proc info which will be replaced below.
        pred_info_create(ModuleName, PredSym, PredOrFunc, Context, Origin,
            pred_status(status_local), Markers, ArgTypes0, TypeVarSet,
            ExistQVars, ClassConstraints, set.init, map.init, OldProcInfo,
            ProcId, !:PredInfo),

        % Add the new predicate to the module.
        some [!PredTable] (
            module_info_get_predicate_table(!.ModuleInfo, !:PredTable),
            predicate_table_insert(!.PredInfo, PredId, !PredTable),
            module_info_set_predicate_table(!.PredTable, !ModuleInfo)
        ),
        PredProcId = proc(PredId, ProcId),

        % Now transform the predicate. This could not be done earlier because
        % we needed to know the new PredProcId to rewrite the recursive calls
        % in the body.
        proc_info_get_argmodes(OldProcInfo, ArgModes0),
        proc_info_get_headvars(OldProcInfo, HeadVars0),
        proc_info_get_varset(OldProcInfo, !:VarSet),
        proc_info_get_vartypes(OldProcInfo, !:VarTypes),
        proc_info_get_goal(OldProcInfo, !:Body),

        varset.new_named_var("LC", LCVar, !VarSet),
        add_var_type(LCVar, loop_control_var_type, !VarTypes),
        should_preserve_tail_recursion(!.ModuleInfo, PreserveTailRecursion),
        get_lc_wait_free_slot_proc(!.ModuleInfo, WaitFreeSlotProc),
        get_lc_join_and_terminate_proc(!.ModuleInfo, JoinAndTerminateProc),

        Info = loop_control_info(!.ModuleInfo, LCVar, OldPredProcId,
            PredProcId, PredSym, PreserveTailRecursion, WaitFreeSlotProc,
            lc_wait_free_slot_name, JoinAndTerminateProc,
            lc_join_and_terminate_name),
        goal_loop_control_all_recursive_paths(Info, RecParConjIds,
            ContainingGoalMap, !Body, !VarSet, !VarTypes),

        % Fixup the remaining recursive calls, and add barriers in the base
        % cases.
        goal_update_non_loop_control_paths(Info, RecParConjIds, _, !Body),

        % Now create the new proc_info structure.
        ItemNumber = -1,
        HeadVars = [LCVar | HeadVars0],
        ArgTypes = [loop_control_var_type | ArgTypes0],
        Ground = ground(shared, none_or_default_func),
        In = from_to_mode(Ground, Ground),
        ArgModes = [In | ArgModes0],

        proc_info_get_inst_varset(OldProcInfo, InstVarSet),
        proc_info_get_rtti_varmaps(OldProcInfo, RttiVarMaps),
        proc_info_get_inferred_determinism(OldProcInfo, Detism),
        proc_info_create(Context, ItemNumber, !.VarSet, !.VarTypes, HeadVars,
            InstVarSet, ArgModes, detism_decl_none, Detism, !.Body,
            RttiVarMaps, address_is_not_taken, has_parallel_conj, map.init,
            ProcInfo),

        % Update the other structures.
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
                lci_module_info                     :: module_info,
                lci_lc_var                          :: prog_var,
                lci_rec_pred_proc_id                :: pred_proc_id,
                lci_inner_pred_proc_id              :: pred_proc_id,
                lci_inner_pred_name                 :: sym_name,
                lci_preserve_tail_recursion         :: preserve_tail_recursion,
                lci_wait_free_slot_proc             :: pred_proc_id,
                lci_wait_free_slot_proc_name        :: sym_name,
                lci_join_and_terminate_proc         :: pred_proc_id,
                lci_join_and_terminate_proc_name    :: sym_name
            ).

:- type preserve_tail_recursion
    --->    preserve_tail_recursion
    ;       do_not_preserve_tail_recursion.

    % Is the current goal the last goal on an execution path through the
    % procedure. In other words, can the last goal within the current goal use
    % a tailcall?
    %
:- type goal_is_last_goal_on_path
    --->    goal_is_last_goal_on_path
    ;       goal_is_not_last_goal_on_path.

:- pred goal_loop_control_all_recursive_paths(loop_control_info::in,
    list(goal_id)::in, containing_goal_map::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

goal_loop_control_all_recursive_paths(Info, GoalIds, ContainingGoalMap, !Goal,
        !VarSet, !VarTypes) :-
    GoalPaths = list.map(goal_id_to_forward_path(ContainingGoalMap), GoalIds),
    list.foldl3(goal_loop_control_one_recursive_path(Info,
            goal_is_last_goal_on_path),
        GoalPaths, !Goal, !VarSet, !VarTypes).

:- pred goal_loop_control_one_recursive_path(loop_control_info::in,
    goal_is_last_goal_on_path::in, forward_goal_path::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

goal_loop_control_one_recursive_path(Info, IsLastGoal, GoalPath0, !Goal,
        !VarSet, !VarTypes) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    ( if goal_path_remove_first(GoalPath0, GoalPath, Step) then
        format("Couldn't follow goal path step: \"%s\"", [s(string(Step))],
            ErrorString),
        (
            Step = step_conj(N),
            ( if
                GoalExpr0 = conj(plain_conj, Conjs0),
                list.index1(Conjs0, N, Conj0)
            then
                (
                    IsLastGoal = goal_is_last_goal_on_path,
                    ( if N = length(Conjs0) then
                        IsLastGoalConj = goal_is_last_goal_on_path
                    else
                        IsLastGoalConj = goal_is_not_last_goal_on_path
                    )
                ;
                    IsLastGoal = goal_is_not_last_goal_on_path,
                    IsLastGoalConj = IsLastGoal
                ),
                goal_loop_control_one_recursive_path(Info, IsLastGoalConj,
                    GoalPath, Conj0, Conj, !VarSet, !VarTypes),
                det_replace_nth(Conjs0, N, Conj, Conjs),
                GoalExpr = conj(plain_conj, Conjs)
            else
                unexpected($pred, ErrorString)
            )
        ;
            Step = step_switch(N, _),
            ( if
                GoalExpr0 = switch(Var, CanFail, Cases0),
                list.index1(Cases0, N, Case0)
            then
                Goal0 = Case0 ^ case_goal,
                goal_loop_control_one_recursive_path(Info, IsLastGoal,
                    GoalPath, Goal0, Goal, !VarSet, !VarTypes),
                Case = Case0 ^ case_goal := Goal,
                det_replace_nth(Cases0, N, Case, Cases),
                GoalExpr = switch(Var, CanFail, Cases)
            else
                unexpected($pred, ErrorString)
            )
        ;
            Step = step_ite_then,
            ( if GoalExpr0 = if_then_else(Vars, Cond, Then0, Else) then
                goal_loop_control_one_recursive_path(Info, IsLastGoal,
                    GoalPath, Then0, Then, !VarSet, !VarTypes),
                GoalExpr = if_then_else(Vars, Cond, Then, Else)
            else
                unexpected($pred, ErrorString)
            )
        ;
            Step = step_ite_else,
            ( if GoalExpr0 = if_then_else(Vars, Cond, Then, Else0) then
                goal_loop_control_one_recursive_path(Info, IsLastGoal,
                    GoalPath, Else0, Else, !VarSet, !VarTypes),
                GoalExpr = if_then_else(Vars, Cond, Then, Else)
            else
                unexpected($pred, ErrorString)
            )
        ;
            Step = step_scope(_),
            ( if GoalExpr0 = scope(Reason, SubGoal0) then
                goal_loop_control_one_recursive_path(Info, IsLastGoal,
                    GoalPath, SubGoal0, SubGoal, !VarSet, !VarTypes),
                GoalExpr = scope(Reason, SubGoal)
            else
                unexpected($pred, ErrorString)
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
            unexpected($pred,
                string.format("Unexpected step in goal path \"%s\"",
                [s(string(Step))]))
        ),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        fixup_goal_info(Info, !Goal)
    else
        ( if GoalExpr0 = conj(parallel_conj, Conjs) then
            par_conj_loop_control(Info, Conjs, IsLastGoal, GoalInfo, !:Goal,
                !VarSet, !VarTypes)
        else
            unexpected($pred, "expected parallel conjunction")
        )
    ).

:- pred par_conj_loop_control(loop_control_info::in, list(hlds_goal)::in,
    goal_is_last_goal_on_path::in, hlds_goal_info::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

par_conj_loop_control(Info, Conjuncts0, IsLastGoal, GoalInfo, Goal, !VarSet,
        !VarTypes) :-
    list.det_split_last(Conjuncts0, EarlierConjuncts0, LastConjunct0),
    % Re-write the recursive call in the last conjunct.
    goal_rewrite_recursive_call(Info, IsLastGoal, LastConjunct0, LastConjunct,
        UseParentStack, _),
    goal_to_conj_list(LastConjunct, LastConjGoals),

    % Process the remaining conjuncts.
    rewrite_nonrecursive_par_conjuncts(Info, UseParentStack,
        EarlierConjuncts0, EarlierConjuncts, !VarSet, !VarTypes),
    Conjuncts = EarlierConjuncts ++ LastConjGoals,
    % XXX The point of calling create_conj_from_list is that it sets up
    % the goal_info of Goal0 appropriately. Why call it if we then immediately
    % overwrite the goal_info?
    create_conj_from_list(Conjuncts, plain_conj, Goal0),
    Goal1 = Goal0 ^ hg_info := GoalInfo,
    fixup_goal_info(Info, Goal1, Goal).

    % Process each of the conjuncts, building the new expression from them.
    %
:- pred rewrite_nonrecursive_par_conjuncts(loop_control_info::in,
    lc_use_parent_stack::in, list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rewrite_nonrecursive_par_conjuncts(_, _, [], [], !VarSet, !VarTypes).
rewrite_nonrecursive_par_conjuncts(Info, UseParentStack,
        [Conjunct0 | Conjuncts0], Goals, !VarSet, !VarTypes) :-
    % Create the "get free slot" call.
    create_get_free_slot_goal(Info, LCSVar, GetFreeSlotGoal,
        !VarSet, !VarTypes),

    % Add a join_and_terminate goal to the end of Conjunct0 forming Conjunct.
    create_join_and_terminate_goal(Info, LCVar, LCSVar, JoinAndTerminateGoal),
    Conjunct0GoalInfo = Conjunct0 ^ hg_info,
    goal_to_conj_list(Conjunct0, Conjunct0Goals),
    ConjunctGoals = Conjunct0Goals ++ [JoinAndTerminateGoal],
    some [!NonLocals] (
        !:NonLocals = goal_info_get_nonlocals(Conjunct0GoalInfo),
        set_of_var.insert(LCSVar, !NonLocals),
        set_of_var.insert(LCVar, !NonLocals),
        goal_info_set_nonlocals(!.NonLocals,
            Conjunct0GoalInfo, ConjunctGoalInfo)
    ),
    conj_list_to_goal(ConjunctGoals, ConjunctGoalInfo, Conjunct),

    % Wrap Conjunct in the loop control scope.
    LCVar = Info ^ lci_lc_var,
    ScopeGoalInfo = ConjunctGoalInfo,
    ScopeGoalExpr = scope(
        loop_control(LCVar, LCSVar, UseParentStack), Conjunct),
    ScopeGoal = hlds_goal(ScopeGoalExpr, ScopeGoalInfo),

    rewrite_nonrecursive_par_conjuncts(Info, UseParentStack, Conjuncts0,
        TailGoals, !VarSet, !VarTypes),
    Goals = [GetFreeSlotGoal, ScopeGoal | TailGoals].

    % Rewrite any recursive calls in this goal.
    %
    % This predicate's argument order does not conform to the Mercury coding
    % standards, this is deliberate as it makes it easier to call from
    % list.map2.
    %
    % UseParentStack is lc_use_parent_stack_frame if, from this goal's
    % perspective it is save to use the parent stack in any spawned off code
    % running in parallel with this goal. Otherwise it is
    % lc_create_frame_on_child_stack.
    %
:- pred goal_rewrite_recursive_call(loop_control_info::in,
    goal_is_last_goal_on_path::in, hlds_goal::in, hlds_goal::out,
    lc_use_parent_stack::out, fixup_goal_info::out) is det.

goal_rewrite_recursive_call(Info, IsLastGoal, !Goal, UseParentStack,
        FixupGoalInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = plain_call(CallPredId0, CallProcId0, Args0, Builtin,
            MaybeUnify, _Name0),
        RecPredProcId = Info ^ lci_rec_pred_proc_id,
        ( if RecPredProcId = proc(CallPredId0, CallProcId0) then
            NewPredProcId = Info ^ lci_inner_pred_proc_id,
            proc(CallPredId, CallProcId) = NewPredProcId,
            LCVar = Info ^ lci_lc_var,
            Args = [LCVar | Args0],
            Name = Info ^ lci_inner_pred_name,
            GoalExpr = plain_call(CallPredId, CallProcId, Args, Builtin,
                MaybeUnify, Name),
            PreserveTailRecursion = Info ^ lci_preserve_tail_recursion,
            !:Goal = hlds_goal(GoalExpr, GoalInfo),
            ( if
                IsLastGoal = goal_is_last_goal_on_path,
                PreserveTailRecursion = preserve_tail_recursion
            then
                % Create a frame on the child's stack so that the parent can
                % tail-call.
                UseParentStack = lc_create_frame_on_child_stack
            else
                UseParentStack = lc_use_parent_stack_frame,
                % Inform the code generator that this call may not be a tail
                % call.
                goal_add_feature(feature_do_not_tailcall, !Goal)
            ),
            fixup_goal_info(Info, !Goal),
            FixupGoalInfo = fixup_goal_info
        else
            UseParentStack = lc_use_parent_stack_frame,
            FixupGoalInfo = do_not_fixup_goal_info
        )
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = conj(_, _)
        ; GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        ; GoalExpr0 = negation(_)
        ; GoalExpr0 = scope(_, _)
        ; GoalExpr0 = if_then_else(_, _, _, _)
        ),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            GoalExpr = GoalExpr0,
            % lc_use_parent_stack_frame is the most indifferent option.
            UseParentStack = lc_use_parent_stack_frame,
            FixupGoalInfo = do_not_fixup_goal_info
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            list.det_split_last(Conjs0, EarlierConjs0, LastConj0),
            goal_rewrite_recursive_call(Info, IsLastGoal, LastConj0, LastConj,
                UseParentStackLastConj, FixupGoalInfoLastConj),
            list.map3(goal_rewrite_recursive_call(Info,
                    goal_is_not_last_goal_on_path),
                EarlierConjs0, EarlierConjs, UseParentStackEarlierConjs,
                FixupGoalInfoEarlierConjs),
            FixupGoalInfoConjs =
                [FixupGoalInfoLastConj | FixupGoalInfoEarlierConjs],
            goals_fixup_goal_info(FixupGoalInfoConjs, FixupGoalInfo),
            goals_use_parent_stack(UseParentStackEarlierConjs,
                UseParentStack0),
            combine_use_parent_stack(UseParentStackLastConj, UseParentStack0,
                UseParentStack),
            Conjs = EarlierConjs ++ [LastConj],
            GoalExpr = conj(ConjType, Conjs)
        ;
            GoalExpr0 = disj(Disjs0),
            % I don't care about disjunctions enough to try to preserve tail
            % calls in them,
            list.map3(goal_rewrite_recursive_call(Info,
                    goal_is_not_last_goal_on_path),
                Disjs0, Disjs, UseParentStackDisjs, FixupGoalInfoDisjs),
            goals_use_parent_stack(UseParentStackDisjs, UseParentStack),
            goals_fixup_goal_info(FixupGoalInfoDisjs, FixupGoalInfo),
            GoalExpr = disj(Disjs)
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            list.map3(case_rewrite_recursive_call(Info, IsLastGoal),
                Cases0, Cases, UseParentStackCases, FixupGoalInfoCases),
            goals_use_parent_stack(UseParentStackCases, UseParentStack),
            goals_fixup_goal_info(FixupGoalInfoCases, FixupGoalInfo),
            GoalExpr = switch(Var, CanFail, Cases)
        ;
            GoalExpr0 = negation(SubGoal0),
            goal_rewrite_recursive_call(Info, IsLastGoal, SubGoal0, SubGoal,
                UseParentStack, FixupGoalInfo),
            GoalExpr = negation(SubGoal)
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            goal_rewrite_recursive_call(Info, IsLastGoal, SubGoal0, SubGoal,
                UseParentStack, FixupGoalInfo),
            GoalExpr = scope(Reason, SubGoal)
        ;
            GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
            goal_rewrite_recursive_call(Info, goal_is_last_goal_on_path,
                Cond0, Cond, UseParentStackCond, FixupGoalInfoCond),
            goal_rewrite_recursive_call(Info, IsLastGoal, Then0, Then,
                UseParentStackThen, FixupGoalInfoThen),
            goal_rewrite_recursive_call(Info, IsLastGoal, Else0, Else,
                UseParentStackElse, FixupGoalInfoElse),
            goals_fixup_goal_info([FixupGoalInfoCond, FixupGoalInfoThen,
                FixupGoalInfoElse], FixupGoalInfo),
            goals_use_parent_stack([UseParentStackCond, UseParentStackThen,
                UseParentStackElse], UseParentStack),
            GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ),
        !:Goal = hlds_goal(GoalExpr, GoalInfo),
        (
            FixupGoalInfo = fixup_goal_info,
            fixup_goal_info(Info, !Goal)
        ;
            FixupGoalInfo = do_not_fixup_goal_info
        )
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred case_rewrite_recursive_call(loop_control_info::in,
    goal_is_last_goal_on_path::in, case::in, case::out,
    lc_use_parent_stack::out, fixup_goal_info::out) is det.

case_rewrite_recursive_call(Info, IsLastGoal, !Case, UseParentStack,
        FixupGoalInfo) :-
    some [!Goal] (
        !:Goal = !.Case ^ case_goal,
        goal_rewrite_recursive_call(Info, IsLastGoal, !Goal, UseParentStack,
            FixupGoalInfo),
        !Case ^ case_goal := !.Goal
    ).

:- pred goals_fixup_goal_info(list(fixup_goal_info)::in, fixup_goal_info::out)
    is det.

goals_fixup_goal_info(List, Fixup) :-
    ( if list.contains(List, fixup_goal_info) then
        Fixup = fixup_goal_info
    else
        Fixup = do_not_fixup_goal_info
    ).

:- pred goals_use_parent_stack(list(lc_use_parent_stack)::in,
    lc_use_parent_stack::out) is det.

goals_use_parent_stack([], lc_use_parent_stack_frame).
goals_use_parent_stack([X | Xs], UseParentStack) :-
    goals_use_parent_stack(Xs, UseParentStack0),
    combine_use_parent_stack(X, UseParentStack0, UseParentStack).

:- pred combine_use_parent_stack(lc_use_parent_stack::in,
    lc_use_parent_stack::in, lc_use_parent_stack::out) is det.

combine_use_parent_stack(lc_use_parent_stack_frame,
    lc_use_parent_stack_frame, lc_use_parent_stack_frame).
combine_use_parent_stack(lc_use_parent_stack_frame,
    lc_create_frame_on_child_stack, lc_create_frame_on_child_stack).
combine_use_parent_stack(lc_create_frame_on_child_stack,
    lc_use_parent_stack_frame, lc_create_frame_on_child_stack).
combine_use_parent_stack(lc_create_frame_on_child_stack,
    lc_create_frame_on_child_stack, lc_create_frame_on_child_stack).

%----------------------------------------------------------------------------%

    % This predicate does two things:
    %
    % 1 It inserts a loop control barrier into the base case(s) of the
    %   predicate.
    %
    % 2 It rewrites the recursive calls that aren't part of parallel
    %   conjunctions so that they call the inner procedure and pass the loop
    %   control variable.
    %
:- pred goal_update_non_loop_control_paths(loop_control_info::in,
    list(goal_id)::in, fixup_goal_info::out,
    hlds_goal::in, hlds_goal::out) is det.

goal_update_non_loop_control_paths(Info, RecParConjIds, FixupGoalInfo,
        !Goal) :-
    GoalInfo0 = !.Goal ^ hg_info,
    GoalId = goal_info_get_goal_id(GoalInfo0),
    ( if
        % This goal is one of the transformed parallel conjunctions,
        % nothing needs to be done.
        % The last conjunct always recurses, this is inforced by
        % merge_loop_control_par_conjs_between_branches, but we should check
        % to see how often this happens and if we should handle it.
        % XXX This may not work, I don't know if the goal ID is maintained.
        list.member(GoalId, RecParConjIds)
    then
        FixupGoalInfo = do_not_fixup_goal_info
    else if
        % This goal is a base case, insert the barrier.
        not (
            some [Callee] (
                goal_calls(!.Goal, Callee),
                (
                    Callee = Info ^ lci_rec_pred_proc_id
                ;
                    Callee = Info ^ lci_inner_pred_proc_id
                )
            )
        )
    then
        goal_to_conj_list(!.Goal, Conjs0),
        create_finish_loop_control_goal(Info, FinishLCGoal),
        Conjs = Conjs0 ++ [FinishLCGoal],
        conj_list_to_goal(Conjs, GoalInfo0, !:Goal),
        fixup_goal_info(Info, !Goal),
        FixupGoalInfo = fixup_goal_info
    else
        !.Goal = hlds_goal(GoalExpr0, _),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            % These cannot be a recursive call and they cannot be a base case
            % since base cases are detected above.
            unexpected($pred, "Non-recursive atomic goal")
        ;
            GoalExpr0 = plain_call(PredId, ProcId, Args0, Builtin,
                MaybeContext, _SymName0),
            % This can only be a recursive call, it must be rewritten
            RecPredProcId = Info ^ lci_rec_pred_proc_id,
            expect(unify(RecPredProcId, proc(PredId, ProcId)), $pred,
                "Expected recursive call"),
            proc(InnerPredId, InnerProcId) = Info ^ lci_inner_pred_proc_id,
            LCVar = Info ^ lci_lc_var,
            Args = [LCVar | Args0],
            SymName = Info ^ lci_inner_pred_name,
            GoalExpr = plain_call(InnerPredId, InnerProcId, Args, Builtin,
                MaybeContext, SymName),
            FixupGoalInfo = fixup_goal_info
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            expect(unify(ConjType, plain_conj), $pred, "parallel conjunction"),
            conj_update_non_loop_control_paths(Info, RecParConjIds,
                FixupGoalInfo, Conjs0, Conjs),
            GoalExpr = conj(ConjType, Conjs)
        ;
            GoalExpr0 = disj(_),
            sorry($pred, "disjunction")
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            list.map2(case_update_non_loop_control_paths(Info, RecParConjIds),
                Cases0, Cases, FixupGoalInfos),
            goals_fixup_goal_info(FixupGoalInfos, FixupGoalInfo),
            GoalExpr = switch(Var, CanFail, Cases)
        ;
            GoalExpr0 = negation(_),
            sorry($pred, "negation")
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            goal_update_non_loop_control_paths(Info, RecParConjIds,
                FixupGoalInfo, SubGoal0, SubGoal),
            GoalExpr = scope(Reason, SubGoal)
        ;
            GoalExpr0 = if_then_else(ExistVars, Cond, Then0, Else0),
            % There may not be any recursive calls in Cond; if there are,
            % we don not apply the transformation.
            goal_update_non_loop_control_paths(Info, RecParConjIds,
                FixupGoalInfoThen, Then0, Then),
            goal_update_non_loop_control_paths(Info, RecParConjIds,
                FixupGoalInfoElse, Else0, Else),
            goals_fixup_goal_info([FixupGoalInfoThen, FixupGoalInfoElse],
                FixupGoalInfo),
            GoalExpr = if_then_else(ExistVars, Cond, Then, Else)
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
        ),
        !Goal ^ hg_expr := GoalExpr,
        (
            FixupGoalInfo = fixup_goal_info,
            some [!NonLocals, !GoalInfo] (
                !:GoalInfo = !.Goal ^ hg_info,
                !:NonLocals = goal_info_get_nonlocals(!.GoalInfo),
                set_of_var.insert(Info ^ lci_lc_var, !NonLocals),
                goal_info_set_nonlocals(!.NonLocals, !GoalInfo),
                goal_info_set_purity(purity_impure, !GoalInfo),
                !Goal ^ hg_info := !.GoalInfo
            )
        ;
            FixupGoalInfo = do_not_fixup_goal_info
        )
    ).

    % As goal_update_non_loop_control_paths, but for a conjunction.
    %
:- pred conj_update_non_loop_control_paths(loop_control_info::in,
    list(goal_id)::in, fixup_goal_info::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

conj_update_non_loop_control_paths(_Info, _RecGoalIds, do_not_fixup_goal_info,
        [], []).
conj_update_non_loop_control_paths(Info, RecGoalIds, FixupGoalInfo,
        [Conj0 | Conjs0], [Conj | Conjs]) :-
    ( if
        not goal_calls(Conj0, Callee),
        (
            % XXX At the moment, we require that all recursive calls be
            % inside parallel conjunctions, and all those recursive calls
            % have by now been transformed to call the inner procedure instead.
            % So the first part of this disjunction cannot succeed.
            Callee = Info ^ lci_rec_pred_proc_id
        ;
            Callee = Info ^ lci_inner_pred_proc_id
        )
    then
        % Conj0 does not make a recursive call or contain a recursive
        % parallel conjunction. We don't need to transform it.
        Conj = Conj0,
        conj_update_non_loop_control_paths(Info, RecGoalIds, FixupGoalInfo,
            Conjs0, Conjs)
    else
        % This Conj has something that needs to be transformed.
        goal_update_non_loop_control_paths(Info, RecGoalIds, FixupGoalInfo,
            Conj0, Conj),
        % There is not going to be anything else in this conjunct
        % that needs to be transformed, we don't make a recursive call.
        Conjs = Conjs0
    ).

    % As goal_update_non_loop_control_paths, but for a case.
    % Note that this argument order is needed by a higher order call above.
    %
:- pred case_update_non_loop_control_paths(loop_control_info::in,
    list(goal_id)::in, case::in, case::out, fixup_goal_info::out) is det.

case_update_non_loop_control_paths(Info, RecParConjIds, !Case,
        FixupGoalInfo) :-
    some [!Goal] (
        !:Goal = !.Case ^ case_goal,
        goal_update_non_loop_control_paths(Info, RecParConjIds,
            FixupGoalInfo, !Goal),
        !Case ^ case_goal := !.Goal
    ).

%----------------------------------------------------------------------------%

:- pred create_get_free_slot_goal(loop_control_info::in, prog_var::out,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

create_get_free_slot_goal(Info, LCSVar, Goal, !VarSet, !VarTypes) :-
    varset.new_named_var("LCS", LCSVar, !VarSet),
    add_var_type(LCSVar, loop_control_slot_var_type, !VarTypes),
    LCVar = Info ^ lci_lc_var,
    proc(PredId, ProcId) = Info ^ lci_wait_free_slot_proc,
    SymName = Info ^ lci_wait_free_slot_proc_name,

    GoalExpr = plain_call(PredId, ProcId, [LCVar, LCSVar], not_builtin,
        maybe.no, SymName),
    NonLocals = set_of_var.list_to_set([LCVar, LCSVar]),
    InstmapDelta = instmap_delta_bind_var(LCSVar),
    GoalInfo = impure_init_goal_info(NonLocals, InstmapDelta, detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- pred create_create_loop_control_goal(module_info::in, prog_var::in,
    prog_var::out, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

create_create_loop_control_goal(ModuleInfo, NumContextsVar, LCVar, Goal,
        !VarSet, !VarTypes) :-
    varset.new_named_var("LC", LCVar, !VarSet),
    add_var_type(LCVar, loop_control_var_type, !VarTypes),
    get_lc_create_proc(ModuleInfo, LCCreatePredId, LCCreateProcId),
    GoalExpr = plain_call(LCCreatePredId, LCCreateProcId,
        [NumContextsVar, LCVar], not_builtin, no, lc_create_name),
    goal_info_init(set_of_var.list_to_set([NumContextsVar, LCVar]),
        instmap_delta_bind_var(LCVar), detism_det, purity_pure, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- pred create_join_and_terminate_goal(loop_control_info::in, prog_var::in,
    prog_var::in, hlds_goal::out) is det.

create_join_and_terminate_goal(Info, LCVar, LCSVar, Goal) :-
    proc(PredId, ProcId) = Info ^ lci_join_and_terminate_proc,
    SymName = Info ^ lci_join_and_terminate_proc_name,

    GoalExpr = plain_call(PredId, ProcId, [LCVar, LCSVar], not_builtin,
        maybe.no, SymName),
    NonLocals = set_of_var.list_to_set([LCVar, LCSVar]),
    instmap_delta_init_reachable(InstmapDelta),
    GoalInfo = impure_init_goal_info(NonLocals, InstmapDelta, detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%----------------------------------------------------------------------------%

:- pred create_finish_loop_control_goal(loop_control_info::in, hlds_goal::out)
    is det.

create_finish_loop_control_goal(Info, Goal) :-
    get_lc_finish_loop_control_proc(Info ^ lci_module_info, PredId, ProcId),
    LCVar = Info ^ lci_lc_var,

    GoalExpr = plain_call(PredId, ProcId, [LCVar], not_builtin, maybe.no,
        lc_finish_loop_control_name),
    NonLocals = set_of_var.list_to_set([LCVar]),
    instmap_delta_init_reachable(InstmapDelta),
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

fixup_goal_info(Info, Goal0, Goal) :-
    some [!GoalInfo, !NonLocals] (
        Goal0 = hlds_goal(GoalExpr, !:GoalInfo),

        LCVar = Info ^ lci_lc_var,
        !:NonLocals = goal_info_get_nonlocals(!.GoalInfo),
        set_of_var.insert(LCVar, !NonLocals),
        goal_info_set_nonlocals(!.NonLocals, !GoalInfo),

        goal_info_set_purity(purity_impure, !GoalInfo),

        Goal = hlds_goal(GoalExpr, !.GoalInfo)
    ).

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
    OrigInstmapDelta = goal_info_get_instmap_delta(OrigGoal ^ hg_info),
    some [!VarSet, !VarTypes] (
        % Re-build the variables in the procedure with smaller sets.
        varset.init(!:VarSet),
        init_vartypes(!:VarTypes),
        proc_info_get_varset(!.ProcInfo, OldVarSet),
        list.foldl3_corresponding(add_old_var_to_sets(OldVarSet), HeadVars0,
            HeadVarTypes, !VarSet, !VarTypes, map.init, Remap),
        list.map(map.lookup(Remap), HeadVars0, HeadVars),
        proc_info_set_headvars(HeadVars, !ProcInfo),

        % Fix rtti varmaps.
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarmaps0),
        apply_substitutions_to_rtti_varmaps(map.init, map.init, Remap,
            RttiVarmaps0, RttiVarmaps),
        proc_info_set_rtti_varmaps(RttiVarmaps, !ProcInfo),

        % Create a variable for the number of worker contexts, we control this
        % in the compiler so that it can be adjusted using profiler feedback
        % (for auto-parallelisation), but for now we just set it using
        % a runtime call so that it can be tuned.
        varset.new_named_var("NumContexts", NumContextsVar, !VarSet),
        add_var_type(NumContextsVar,
            builtin_type(builtin_type_int(int_type_int)), !VarTypes),
        get_lc_default_num_contexts_proc(ModuleInfo,
            LCDefaultNumContextsPredId, LCDefaultNumContextsProcId),
        goal_info_init(set_of_var.list_to_set([NumContextsVar]),
            instmap_delta_bind_var(NumContextsVar),
            detism_det, purity_pure, GetNumContextsGoalInfo),
        GetNumContextsGoal = hlds_goal(plain_call(LCDefaultNumContextsPredId,
                LCDefaultNumContextsProcId, [NumContextsVar],
                not_builtin, no, lc_default_num_contexts_name),
            GetNumContextsGoalInfo),

        % Create the call to lc_create
        create_create_loop_control_goal(ModuleInfo, NumContextsVar, LCVar,
            LCCreateGoal, !VarSet, !VarTypes),

        % Create the inner call.
        InnerCallArgs = [LCVar | HeadVars],
        NonLocals = set_of_var.list_to_set(InnerCallArgs),
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
        goal_info_init(set_of_var.list_to_set(HeadVars), InstmapDelta, Detism,
            purity_impure, ConjGoalInfo),
        ConjGoal = hlds_goal(conj(plain_conj,
                [GetNumContextsGoal, LCCreateGoal, InnerProcCallGoal]),
            ConjGoalInfo),

        OrigPurity = goal_info_get_purity(OrigGoal ^ hg_info),
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
    ( if varset.search_name(OldVarSet, OldVar, Name) then
        varset.new_named_var(Name, Var, !VarSet)
    else
        varset.new_var(Var, !VarSet)
    ),
    add_var_type(Var, VarType, !VarTypes),
    map.det_insert(OldVar, Var, !Remap).

:- pred remap_instmap(map(prog_var, prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

remap_instmap(Remap, OldInstmapDelta, !:InstmapDelta) :-
    instmap_delta_to_assoc_list(OldInstmapDelta, VarInsts),
    instmap_delta_init_reachable(!:InstmapDelta),
    list.foldl(
        (pred((OldVar - Inst)::in, IMD0::in, IMD::out) is det :-
            map.lookup(Remap, OldVar, Var),
            instmap_delta_set_var(Var, Inst, IMD0, IMD)
        ), VarInsts, !InstmapDelta).

%--------------------------------------------------------------------%

:- func loop_control_var_type = mer_type.

loop_control_var_type = defined_type(Sym, [], kind_star) :-
    Sym = qualified(par_builtin_module_sym, "loop_control").

:- func loop_control_slot_var_type = mer_type.

loop_control_slot_var_type = builtin_type(builtin_type_int(int_type_int)).

%----------------------------------------------------------------------------%

:- func lc_wait_free_slot_name = sym_name.

lc_wait_free_slot_name =
    qualified(par_builtin_module_sym, lc_wait_free_slot_name_unqualified).

:- func lc_wait_free_slot_name_unqualified = string.

lc_wait_free_slot_name_unqualified = "lc_wait_free_slot".

:- pred get_lc_wait_free_slot_proc(module_info::in, pred_proc_id::out) is det.

get_lc_wait_free_slot_proc(ModuleInfo, proc(PredId, ProcId)) :-
    lookup_lc_pred_proc(ModuleInfo, lc_wait_free_slot_name_unqualified, 2,
        PredId, ProcId).

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

:- func lc_join_and_terminate_name_unqualified = string.

lc_join_and_terminate_name_unqualified = "lc_join_and_terminate".

:- func lc_join_and_terminate_name = sym_name.

lc_join_and_terminate_name =
    qualified(par_builtin_module_sym, lc_join_and_terminate_name_unqualified).

:- pred get_lc_join_and_terminate_proc(module_info::in, pred_proc_id::out)
    is det.

get_lc_join_and_terminate_proc(ModuleInfo, proc(PredId, ProcId)) :-
    lookup_lc_pred_proc(ModuleInfo, lc_join_and_terminate_name_unqualified, 2,
        PredId, ProcId).

:- func lc_finish_loop_control_name_unqualified = string.

lc_finish_loop_control_name_unqualified = "lc_finish".

:- func lc_finish_loop_control_name = sym_name.

lc_finish_loop_control_name =
    qualified(par_builtin_module_sym, lc_finish_loop_control_name_unqualified).

:- pred get_lc_finish_loop_control_proc(module_info::in,
    pred_id::out, proc_id::out) is det.

get_lc_finish_loop_control_proc(ModuleInfo, PredId, ProcId) :-
    lookup_lc_pred_proc(ModuleInfo, lc_finish_loop_control_name_unqualified, 1,
        PredId, ProcId).

%----------------------------------------------------------------------------%

:- pred lookup_lc_pred_proc(module_info::in, string::in, arity::in,
    pred_id::out, proc_id::out) is det.

lookup_lc_pred_proc(ModuleInfo, Sym, Arity, PredId, ProcId) :-
    lookup_builtin_pred_proc_id(ModuleInfo, par_builtin_module_sym,
        Sym, pf_predicate, Arity, only_mode, PredId, ProcId).

%----------------------------------------------------------------------------%

:- func par_builtin_module_sym = sym_name.

par_builtin_module_sym = unqualified("par_builtin").

%----------------------------------------------------------------------------%
:- end_module transform_hlds.par_loop_control.
%----------------------------------------------------------------------------%
