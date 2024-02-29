%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module constructs the call trees of the predicates defined in the
% given module. (For the purposes of documentation in this module,
% "predicates" includes functions as well.)
%
% The basic data structure we construct associates a given predicate with
% the list of other predicates it makes a reference to. Most of the time,
% this reference is a call, but it can also be a reference to a closure
% containing a pred_id to be callee, because the usual reason for
% constructing a closure is that we want it to be called, though the call
% may be done by another predicate (such as list.map, map.foldl etc).
%
% We write out the info in this call tree to module.local_call_tree files
% in the form of entries like this:
%
%   pred polymorphism_process_module/5
%       pred maybe_polymorphism_process_pred/7
%       pred polymorphism_update_arg_types/5
%
%   pred maybe_polymorphism_process_pred/7
%       pred polymorphism_process_pred_msg/7
%
%   pred polymorphism_process_pred_msg/7
%       pred polymorphism_process_pred/6
%
%   ...
%
% Each entry gives the name of a predicate (or function), and then lists
% its local callees (i.e. the ones defined in the same module) in the order
% in which a depth-first left-to-right traversal of the first valid procedure
% of the predicate first encounters them. (The data structure records
% references to predicates defined in other modules as well, but the intended
% use case of .local_call_tree files is finding good ways to group the
% predicates of the module, and for this purpose, references to nonlocal
% predicates are irrelevant.)
%
% We start at the first exported predicate, write out the local predicates
% in the first layer of its call tree (i.e. the local predicates it has
% direct references to) in the same depth-first left-to-right order.
% We then print out the second exported predicate and its call tree,
% and third, and so on.
%
% We never repeat the entry for a given predicate. If a predicate p1
% is called from two or more predicates (say p2 and p3) in the call trees
% of an exported predicate (say p4), then it will be printed as part of
% the call tree of p4 only once.
%
% Likewise, if a predicate is part of the call tree of more than one
% exported predicate, it will be printed as part of the call tree
% of only the first of those.
%
% This local call tree is one of three outputs we generate. The second output,
% the .local_call_tree_order file, is derived from the first: it is a list of
% just the predicates of the module in the order in which the first output
% first encounters them, like this:
%
%   pred polymorphism_process_module/5
%   pred maybe_polymorphism_process_pred/7
%   pred polymorphism_process_pred_msg/7
%   pred polymorphism_process_pred/6
%   pred polymorphism_process_clause_info/6
%   ...
%
% This is the order that a strict top-down decomposition of the exported
% predicates of the module would yield. It is a good idea for the
% predicates in the source code to follow this order, though this order
% may be trumped by other considerations, such as putting
%
% - mutually recursive predicates, or
% - predicates that are almost copies of each other and therefore must be
%   maintained together,
%
% next to each other.
%
% The third output, the .local_call_tree_full file, contains entries
% for each predicate defined in the current module module in the same order,
% but each entry lists the full visible call tree of the predicate.
% This means that
%
% - in addition to listing only the directly referenced predicates,
%   it also lists those that are indirectly referenced
%   (meaning that if p calls q and q calls r, then it includes r
%   in the call tree of p, even if p never references r directly), and
%
% - it lists referenced predicates defined in other modules, as well as
%   those defined in other modules.
%
% The "visible" part of the phase "visible call tree" above acknowledges
% the fact that the predicates defined in other modules will in general
% have their own callees, but these are in effect beyond the visibility horizon
% of the compiler when it has access only to the HLDS of the current module.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_call_tree.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------%

:- type call_tree_info.

    % Compute a representation of the local call tree.
    % The caller can give this representation to either or both of two
    % predicates below: write_local_call_tree and generate_movability_report.
    %
:- pred compute_local_call_tree(module_info::in, call_tree_info::out) is det.

%---------------------%

    % Construct and return strings containing
    %
    % - the proposed contents of the file containing the depth-first
    %   left-to-right traversal (such as the first example above)
    %   of the given module, with each predicate's entry containing
    %   only its direct, local callees;
    %
    % - the proposed contents of the file containing the depth-first
    %   left-to-right traversal (such as the first example above)
    %   of the given module, with an entry for each local predicate containing
    %   all predicates in its call tree, whether they are called directly
    %   or indirectly, and whether they are local or not.
    %
    % - the proposed contents of the file containing the flattened
    %   order of the module's predicates.
    %
:- pred construct_local_call_tree_file_contents(module_info::in,
    call_tree_info::in, string::out, string::out, string::out) is det.

%---------------------%

:- pred generate_movability_report(module_info::in, call_tree_info::in,
    list(string)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module string.builder.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type call_tree_info
    --->    call_tree_info(
                % The set of pred_ids defined in the current module.
                cti_local_pred_set          :: set_tree234(pred_id),

                % The pred_ids of the exported predicates and functions
                % of the current module, listed in the order in which
                % they appear in the interface. (Technically, they are
                % ordered by line number, which can be confused by the
                % presence of pragmas that change the filename, and/or by
                % the presence of more than one declaration on a single line,
                % but such interfaces are vanishingly rare.)
                cti_exported_preds          :: list(pred_id),

                % The list of local predicates in the module, with the direct
                % callees of each.
                %
                % The callees of each predicate are listed in the order
                % in which a top-down left-to-right traversal of the first
                % (valid) procedure of the predicate would encounter them.
                %
                % The pred_callee structures themselves are ordered in the same
                % way, though the top level is not the body of a single
                % predicate, but the list of the exported predicates, as given
                % by cti_exported_preds.
                cti_pred_callee_list        :: list(pred_callees),

                % A map that returns, for each pred_id, its position in the
                % cti_pred_callee_list field. Position numbers start at 1.
                cti_pred_order_map          :: map(pred_id, int),

                % A map that represents the same info as cti_pred_callee_list,
                % but organized for random access by pred_id.
                cti_direct_callee_map       :: pred_callees_map,

                % A map that is effectively the transitive closure of the
                % contents of the cti_direct_callee_map field. It maps each
                % local pred_id to a pred_callees structure whose callees
                % field contains not just the preds called *directly*
                % by that pred, but also *its* callees, *their* callees,
                % and so on.
                cti_indirect_callee_map     :: pred_callees_map
            ).

:- type pred_callees_map == map(pred_id, pred_callees).

:- type pred_callees
    --->    pred_callees(
                % The local predicate described by these two fields ...
                pred_id,
                pred_info,

                % ... calls these predicates. Each callee is present
                % in the list just once. The order of the list is given
                % by the order in which the first occurrence of each callee
                % is encountered in a depth-first left-to-right traversal
                % of the first valid procedure of the predicate.
                %
                % For each callee, we record whether the callee is local or
                % not, because several users of these structures want to
                % process only local callees, and it is more efficient
                % to determine whether the callee is local or not just once,
                % rather than separately at each point of use.
                %
                % Whether this field contains just the direct callees of the
                % predicate identified by the first two args, or the callees
                % of callees of callees ... as well, depends on which of the
                % fields of call_tree_info contains this structure.
                list(callee)
            ).

:- type callee
    --->    callee(pred_id, maybe_local).

:- type maybe_local
    --->    is_not_local
    ;       is_local.

%---------------------------------------------------------------------------%

compute_local_call_tree(ModuleInfo, CallTreeInfo) :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_sorted_assoc_list(PredIdTable, PredIdsInfos),
    find_local_preds_exports(PredIdsInfos,
        set_tree234.init, LocalPredIds, one_or_more_map.init, ExportLineMap),
    one_or_more_map.to_flat_assoc_list(ExportLineMap, ExportLineList),
    assoc_list.values(ExportLineList, ExportedPredIds),

    build_direct_pred_callee_map(PredIdTable, LocalPredIds, ExportedPredIds,
        set_tree234.init, cord.init, PredCalleesCord,
        map.init, DirectPredCalleeMap),
    PredCalleesList = cord.list(PredCalleesCord),

    list.foldl(add_pred_and_callees_to_digraph, PredCalleesList,
        digraph.init, Graph),
    BottomUpSccs = digraph.return_sccs_in_to_from_order(Graph),
    record_pred_order(PredCalleesList, 0, map.init, PredOrderMap),
    build_indirect_map_sccs(DirectPredCalleeMap, PredOrderMap, BottomUpSccs,
        map.init, IndirectPredCalleesMap),

    CallTreeInfo = call_tree_info(LocalPredIds, ExportedPredIds,
        PredCalleesList, PredOrderMap,
        DirectPredCalleeMap, IndirectPredCalleesMap).

%---------------------------------------------------------------------------%

:- pred find_local_preds_exports(assoc_list(pred_id, pred_info)::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out,
    one_or_more_map(int, pred_id)::in, one_or_more_map(int, pred_id)::out)
    is det.

find_local_preds_exports([], !LocalPredIds, !ExportMap).
find_local_preds_exports([PredId - PredInfo | PredIdsInfos],
        !LocalPredIds, !ExportMap) :-
    pred_info_get_status(PredInfo, PredStatus),
    IsLocal = pred_status_defined_in_this_module(PredStatus),
    (
        IsLocal = no
    ;
        IsLocal = yes,
        pred_info_get_origin(PredInfo, Origin),
        (
            ( Origin = origin_compiler(_)
            ; Origin = origin_pred_transform(_, _, _)
            ; Origin = origin_proc_transform(_, _, _, _)
            )
        ;
            Origin = origin_user(_),
            set_tree234.insert(PredId, !LocalPredIds),
            IsExported = pred_status_is_exported_to_non_submodules(PredStatus),
            (
                IsExported = no
            ;
                IsExported = yes,
                pred_info_get_context(PredInfo, Context),
                LineNumber = term_context.context_line(Context),
                one_or_more_map.det_insert(LineNumber, PredId, !ExportMap)
            )
        )
    ),
    find_local_preds_exports(PredIdsInfos, !LocalPredIds, !ExportMap).

%---------------------------------------------------------------------------%

:- pred build_direct_pred_callee_map(pred_id_table::in,
    set_tree234(pred_id)::in, list(pred_id)::in, set_tree234(pred_id)::in,
    cord(pred_callees)::in, cord(pred_callees)::out,
    map(pred_id, pred_callees)::in, map(pred_id, pred_callees)::out) is det.

build_direct_pred_callee_map(_PredIdTable, _LocalPredIds, [],
        _HandledPredIds, !PredCalleesCord, !DirectPredCalleeMap).
build_direct_pred_callee_map(PredIdTable, LocalPredIds,
        [HeadPredId | TailPredIds], !.HandledPredIds,
        !PredCalleesCord, !DirectPredCalleeMap) :-
    ( if set_tree234.insert_new(HeadPredId, !HandledPredIds) then
        map.lookup(PredIdTable, HeadPredId, PredInfo),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.to_assoc_list(ProcTable, ProcIdsInfos),
        (
            ProcIdsInfos = [_ProcId - ProcInfo | _],
            proc_info_get_goal(ProcInfo, Goal),
            acc_pred_ids_in_goal(Goal, cord.init, ReferencedPredIdsCord),
            ReferencedPredIds = cord.list(ReferencedPredIdsCord),
            list.map(pred_id_to_callee(LocalPredIds),
                ReferencedPredIds, Callees0),
            Callees = keep_only_first_calls(Callees0),
            PredCallees = pred_callees(HeadPredId, PredInfo, Callees),
            cord.snoc(PredCallees, !PredCalleesCord),
            map.det_insert(HeadPredId, PredCallees, !DirectPredCalleeMap),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.filter_map(
                callee_is_local_has_not_been_handled(!.HandledPredIds),
                Callees, NotYethandledLocalPredIds),
            NextPredIds = NotYethandledLocalPredIds ++ TailPredIds
        ;
            ProcIdsInfos = [],
            % Builtin predicates have no procedures in the HLDS.
            % It is also possible for non-builtin predicates to *start* with
            % one or more procedures, but mode analysis deletes any procedure
            % it finds to be invalid, and it is possible for it to delete
            % all of a predicate's initial procedures.
            NextPredIds = TailPredIds
        )
    else
        NextPredIds = TailPredIds
    ),
    build_direct_pred_callee_map(PredIdTable, LocalPredIds, NextPredIds,
        !.HandledPredIds, !PredCalleesCord, !DirectPredCalleeMap).

:- pred pred_id_to_callee(set_tree234(pred_id)::in, pred_id::in, callee::out)
    is det.

pred_id_to_callee(LocalPredIds, PredId, Callee) :-
    ( if set_tree234.contains(LocalPredIds, PredId) then
        MaybeLocal = is_local
    else
        MaybeLocal = is_not_local
    ),
    Callee = callee(PredId, MaybeLocal).

:- pred callee_is_local_has_not_been_handled(set_tree234(pred_id)::in,
    callee::in, pred_id::out) is semidet.

callee_is_local_has_not_been_handled(HandledPredIds, Callee, PredId) :-
    Callee = callee(PredId, MaybeLocal),
    ( if set_tree234.contains(HandledPredIds, PredId) then
        fail
    else
        MaybeLocal = is_local
    ).

%---------------------%

:- pred acc_pred_ids_in_goal(hlds_goal::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

acc_pred_ids_in_goal(Goal, !CalleeCord) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, RHS, _, Unification, _),
        (
            Unification = construct(_, UnifyConsId, _, _, _, _, _),
            acc_pred_ids_in_cons_id(UnifyConsId, !CalleeCord)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
            % These cannot refer to predicates.
        ;
            Unification = complicated_unify(_, _, _)
            % The simplification pass will turn this into a call.
            % The callee of that call will be either
            %
            % - the applicable type's user-declared unification predicate,
            %   if it has one, or
            %
            % - the type's compiler-generated unification predicate.
            %
            % In the latter case, we definitely don't want the callee,
            % and I (zs) guess that most users won't want the callee
            % in the former case either, since unification predicates
            % are below the threshold of interest even if user-defined.
            %
            % If we want to change this decision, the execution of this pass
            % should be delayed after simplification, which will replace
            % all complicated_unify goals with other code, probably a call.
        ),
        (
            RHS = rhs_var(_)
        ;
            RHS = rhs_functor(RHSConsId, _, _),
            acc_pred_ids_in_cons_id(RHSConsId, !CalleeCord)
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, SubGoal),
            acc_pred_ids_in_goal(SubGoal, !CalleeCord)
        )
    ;
        GoalExpr = plain_call(PredId, _, _, _, _, _),
        cord.snoc(PredId, !CalleeCord)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, _, _, _, _),
        cord.snoc(PredId, !CalleeCord)
    ;
        GoalExpr = generic_call(_, _, _, _, _)
        % We don't know the identity of the callee.
    ;
        GoalExpr = conj(_Kind, SubGoals),
        list.foldl(acc_pred_ids_in_goal, SubGoals, !CalleeCord)
    ;
        GoalExpr = disj(SubGoals),
        list.foldl(acc_pred_ids_in_goal, SubGoals, !CalleeCord)
    ;
        GoalExpr = switch(_, _, Cases),
        % The cons_ids that can occur in switch cases cannot contain pred_ids.
        SubGoals = list.map((func(C) = C ^ case_goal), Cases),
        list.foldl(acc_pred_ids_in_goal, SubGoals, !CalleeCord)
    ;
        GoalExpr = negation(SubGoal),
        acc_pred_ids_in_goal(SubGoal, !CalleeCord)
    ;
        GoalExpr = scope(_Reason, SubGoal),
        acc_pred_ids_in_goal(SubGoal, !CalleeCord)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        acc_pred_ids_in_goal(Cond, !CalleeCord),
        acc_pred_ids_in_goal(Then, !CalleeCord),
        acc_pred_ids_in_goal(Else, !CalleeCord)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        ;
            Shorthand = atomic_goal(_Type, _Outer, _Inner, _OutputVars,
                MainGoal, OrElseGoals, _Inners),
            acc_pred_ids_in_goal(MainGoal, !CalleeCord),
            list.foldl(acc_pred_ids_in_goal, OrElseGoals, !CalleeCord)
        ;
            Shorthand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            acc_pred_ids_in_goal(SubGoal, !CalleeCord)
        )
    ).

:- pred acc_pred_ids_in_cons_id(cons_id::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

acc_pred_ids_in_cons_id(ConsId, !CalleeCord) :-
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        ShroudedPredProcId = shrouded_pred_proc_id(PredIdInt, _),
        ShroudedPredId = shrouded_pred_id(PredIdInt),
        PredId = unshroud_pred_id(ShroudedPredId),
        cord.snoc(PredId, !CalleeCord)
    else
        true
    ).

%---------------------%

:- func keep_only_first_calls(list(callee)) = list(callee).

keep_only_first_calls(CalleeListWithDuplicates) = CalleeListWithoutDuplicates :-
    SeenCalleess0 = set_tree234.init,
    keep_only_first_calls_loop(CalleeListWithDuplicates, SeenCalleess0,
        cord.init, CalleeCordWithoutDuplicates),
    CalleeListWithoutDuplicates = cord.list(CalleeCordWithoutDuplicates).

:- pred keep_only_first_calls_loop(list(callee)::in, set_tree234(callee)::in,
    cord(callee)::in, cord(callee)::out) is det.

keep_only_first_calls_loop([], _, !CalleeCordWithoutDuplicates).
keep_only_first_calls_loop([Callee | Callees], !.SeenCallees,
        !CalleeCordWithoutDuplicates) :-
    ( if set_tree234.insert_new(Callee, !SeenCallees) then
        cord.snoc(Callee, !CalleeCordWithoutDuplicates)
    else
        true
    ),
    keep_only_first_calls_loop(Callees, !.SeenCallees,
        !CalleeCordWithoutDuplicates).

%---------------------%

:- pred add_pred_and_callees_to_digraph(pred_callees::in,
    digraph(pred_id)::in, digraph(pred_id)::out) is det.

add_pred_and_callees_to_digraph(PredCallee, !Graph) :-
    PredCallee = pred_callees(CallerPredId, _, Callees),
    add_vertex(CallerPredId, CallerKey, !Graph),
    add_caller_callees_to_digraph(CallerKey, Callees, !Graph).

:- pred add_caller_callees_to_digraph(digraph_key(pred_id)::in,
    list(callee)::in, digraph(pred_id)::in, digraph(pred_id)::out) is det.

add_caller_callees_to_digraph(_, [], !Graph).
add_caller_callees_to_digraph(CallerKey, [Callee | Callees], !Graph) :-
    Callee = callee(CalleePredId, _),
    add_vertex(CalleePredId, CalleeKey, !Graph),
    add_edge(CallerKey, CalleeKey, !Graph),
    add_caller_callees_to_digraph(CallerKey, Callees, !Graph).

%---------------------%

:- pred record_pred_order(list(pred_callees)::in, int::in,
    map(pred_id, int)::in, map(pred_id, int)::out) is det. 

record_pred_order([], _, !PredOrderMap).
record_pred_order([HeadPredCallee | TailPredCalles], CurNum, !PredOrderMap) :-
    HeadPredCallee = pred_callees(CallerPredId, _, _),
    map.det_insert(CallerPredId, CurNum, !PredOrderMap),
    record_pred_order(TailPredCalles, CurNum + 1, !PredOrderMap).

%---------------------%

:- pred build_indirect_map_sccs(pred_callees_map::in, map(pred_id, int)::in,
    list(set(pred_id))::in,
    pred_callees_map::in, pred_callees_map::out) is det.

build_indirect_map_sccs(_, _, [], !IndirectCalleeMap).
build_indirect_map_sccs(DirectCalleeMap, PredOrderMap, [SccSet | SccSets],
        !IndirectCalleeMap) :-
    SccPredIds = set.to_sorted_list(SccSet),
    % Construct IncompleteSccIndirectCalleeMap, which maps each pred in the SCC
    % to a pred_callees structure in which the last field contains
    %
    % - the full call trees of every callee that is in a lower SCC, and
    % - just the pred_id of every callee that is the same SCC,
    %
    % all in the order given by a depth-first left-to-right traversal.
    % This list may (and often will) contain duplicates.
    LowerSccIndirectCalleeMap = !.IndirectCalleeMap,
    list.foldl(
        build_incomplete_indirect_map_pred(DirectCalleeMap, SccSet,
            LowerSccIndirectCalleeMap),
        SccPredIds, map.init, IncompleteSccIndirectCalleeMap),
    % Complete the incomplete map constructed above by replacing each callee
    % that is in this SCC by its call tree, *and* delete any duplicates.
    % Add the completed entries to !IndirectCalleeMap.
    list.foldl(
        complete_and_add_indirect_map_pred(IncompleteSccIndirectCalleeMap),
        SccPredIds, !IndirectCalleeMap),
    build_indirect_map_sccs(DirectCalleeMap, PredOrderMap, SccSets,
        !IndirectCalleeMap).

:- pred build_incomplete_indirect_map_pred(pred_callees_map::in,
    set(pred_id)::in, pred_callees_map::in, pred_id::in,
    pred_callees_map::in, pred_callees_map::out) is det.

build_incomplete_indirect_map_pred(DirectCalleeMap, SccSet,
        LowerSccIndirectCalleeMap, PredId,
        !IncompleteSccIndirectCalleeMap) :-
    expect(set.contains(SccSet, PredId), $pred, "PredId not in SccSet"),
    ( if map.search(DirectCalleeMap, PredId, PredCallees) then
        PredCallees = pred_callees(PredCalleesPredId, PredInfo, DirectCallees),
        expect(unify(PredId, PredCalleesPredId), $pred,
            "PredId != PredCalleesPredId"),
        build_incomplete_indirect_map_callees(LowerSccIndirectCalleeMap,
            SccSet, DirectCallees, cord.init, IncompleteIndirectCallees),
        IncompleteIndirectPredCallees = pred_callees(PredId, PredInfo,
            cord.list(IncompleteIndirectCallees)),
        map.det_insert(PredId, IncompleteIndirectPredCallees,
            !IncompleteSccIndirectCalleeMap)
    else
        % It is possible for the map.search to fail, in the presence of errors.
        % This happens for tests/invalid/mode_inf.m.
        true
    ).

:- pred build_incomplete_indirect_map_callees(pred_callees_map::in,
    set(pred_id)::in, list(callee)::in,
    cord(callee)::in, cord(callee)::out) is det.

build_incomplete_indirect_map_callees(_, _, [],
        !IncompleteIndirectCallees).
build_incomplete_indirect_map_callees(LowerSccIndirectCalleeMap, SccSet,
        [Callee | Callees], !IncompleteIndirectCallees) :-
    Callee = callee(CalleePredId, _IsLocal),
    ( if
        map.search(LowerSccIndirectCalleeMap, CalleePredId, IndirectPredCallees)
    then
        IndirectPredCallees = pred_callees(_, _, IndirectCallees),
        !:IncompleteIndirectCallees = !.IncompleteIndirectCallees ++
            cord.from_list([Callee | IndirectCallees])
    else
        % This can be a predicate in the current SCC (whose info is NOT YET
        % in LowerSccIndirectCalleeMap).
        %
        % or it could be a nonlocal predicate that (because it is nonlocal)
        % build_direct_pred_callee_map did not add to DirectCalleeMap, and
        % which therefore build_incomplete_indirect_map_pred did not add to
        % IncompleteSccIndirectCalleeMap.
        %
        % This action is the appropriate action in both cases.
        cord.snoc(Callee, !IncompleteIndirectCallees)
    ),
    build_incomplete_indirect_map_callees(LowerSccIndirectCalleeMap, SccSet,
        Callees, !IncompleteIndirectCallees).

:- pred complete_and_add_indirect_map_pred(pred_callees_map::in, pred_id::in,
    pred_callees_map::in, pred_callees_map::out) is det.

complete_and_add_indirect_map_pred(IncompleteSccIndirectPredCalleeMap, PredId,
        !IndirectPredCalleeMap) :-
    ( if
        map.search(IncompleteSccIndirectPredCalleeMap, PredId,
            IncompletePredCallees)
    then
        IncompletePredCallees = pred_callees(PredCalleesPredId, PredInfo,
            IncompleteIndirectCallees),
        expect(unify(PredId, PredCalleesPredId), $pred,
            "PredId != PredCalleesPredId"),
        complete_callees(IncompleteSccIndirectPredCalleeMap,
            IncompleteIndirectCallees,
            cord.init, CompleteIndirectCalleesCord0),
        CompleteIndirectCallees0 = cord.list(CompleteIndirectCalleesCord0),
        CompleteIndirectCallees =
            keep_only_first_calls(CompleteIndirectCallees0),
        PredCallees = pred_callees(PredId, PredInfo, CompleteIndirectCallees),
        map.det_insert(PredId, PredCallees, !IndirectPredCalleeMap)
    else
        % Again, it is possible for the map.search to fail,
        % in the presence of errors. This happens for tests/invalid/mode_inf.m.
        true
    ).

:- pred complete_callees(pred_callees_map::in, list(callee)::in,
    cord(callee)::in, cord(callee)::out) is det.

complete_callees(_, [], !CompleteCalleesCord).
complete_callees(IncompleteSccIndirectPredCalleeMap, [Callee | Callees],
        !CompleteCalleesCord) :-
    cord.snoc(Callee, !CompleteCalleesCord),
    Callee = callee(PredId, _IsLocal),
    ( if
        map.search(IncompleteSccIndirectPredCalleeMap, PredId, PredCallees)
    then
        PredCallees = pred_callees(_, _, SccPredCallees),
        !:CompleteCalleesCord = !.CompleteCalleesCord ++
            cord.from_list([Callee | SccPredCallees])
    else
        true
    ),
    complete_callees(IncompleteSccIndirectPredCalleeMap, Callees,
        !CompleteCalleesCord).

%---------------------%

:- pred construct_depth_first_left_right_order(map(pred_id, pred_callees)::in,
    list(pred_callees)::in, set_tree234(pred_id)::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

construct_depth_first_left_right_order(_, [], _, !PredIdCord).
construct_depth_first_left_right_order(PredCalleeMap,
        [HeadPredCallees | TailPredCallees], !.HandledPredIds, !PredIdCord) :-
    HeadPredCallees = pred_callees(PredId, _PredInfo, Callees),
    ( if set_tree234.insert_new(PredId, !HandledPredIds) then
        cord.snoc(PredId, !PredIdCord),
        keep_only_new_local_callees(!.HandledPredIds, Callees,
            cord.init, NewLocalPredIdCord),
        NewLocalPredIds = cord.list(NewLocalPredIdCord),
        % Some predicates in NewCallees may not be in PredCalleeMap,
        % because they have no valid procedures.
        list.filter_map(map.search(PredCalleeMap),
            NewLocalPredIds, NewPredCallees),
        NextPredCallees = NewPredCallees ++ TailPredCallees
    else
        NextPredCallees = TailPredCallees
    ),
    construct_depth_first_left_right_order(PredCalleeMap,
        NextPredCallees, !.HandledPredIds, !PredIdCord).

:- pred keep_only_new_local_callees(set_tree234(pred_id)::in, list(callee)::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

keep_only_new_local_callees(_HandledPredIds, [], !NewLocalPredIdCord).
keep_only_new_local_callees(HandledPredIds, [Callee | Callees],
        !NewLocalPredIdCord) :-
    Callee = callee(PredId, IsLocal),
    (
        IsLocal = is_local,
        ( if set_tree234.contains(HandledPredIds, PredId) then
            true
        else
            cord.snoc(PredId, !NewLocalPredIdCord)
        )
    ;
        IsLocal = is_not_local
    ),
    keep_only_new_local_callees(HandledPredIds, Callees, !NewLocalPredIdCord).

%---------------------%

:- pred compare_pred_callees_by_order(map(pred_id, int)::in,
    pred_callees::in, pred_callees::in, comparison_result::out) is det.

compare_pred_callees_by_order(PredOrderMap, PredCalleesA, PredCalleesB,
        Result) :-
    PredCalleesA = pred_callees(PredIdA, _, _),
    PredCalleesB = pred_callees(PredIdB, _, _),
    map.lookup(PredOrderMap, PredIdA, OrderA),
    map.lookup(PredOrderMap, PredIdB, OrderB),
    compare(Result, OrderA, OrderB).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

construct_local_call_tree_file_contents(ModuleInfo, CallTreeInfo,
        DirectTreeFileStr, IndirectTreeFileStr, OrderFileStr) :-
    CallTreeInfo = call_tree_info(_LocalPredIds, _ExportList,
        PredCalleesList, PredOrderMap,
        DirectPredCalleeMap, IndirectPredCalleesMap),

    DirectTreeState0 = string.builder.init,
    list.foldl2(construct_direct_pred_callees_entry(ModuleInfo),
        PredCalleesList, "", _MaybeNlD, DirectTreeState0, DirectTreeState),
    DirectTreeFileStr = string.builder.to_string(DirectTreeState),

    map.values(IndirectPredCalleesMap, IndirectPredCalleesList0),
    list.sort(compare_pred_callees_by_order(PredOrderMap),
        IndirectPredCalleesList0, IndirectPredCalleesList),
    IndirectTreeState0 = string.builder.init,
    list.foldl2(construct_indirect_pred_callees_entry(ModuleInfo),
        IndirectPredCalleesList, "", _MaybeNlI,
        IndirectTreeState0, IndirectTreeState),
    IndirectTreeFileStr = string.builder.to_string(IndirectTreeState),

    construct_depth_first_left_right_order(DirectPredCalleeMap,
        PredCalleesList, set_tree234.init, cord.init, PredIdCord),
    PredIdList = cord.list(PredIdCord),
    OrderState0 = string.builder.init,
    list.foldl(construct_pred_order_entry(ModuleInfo), PredIdList,
        OrderState0, OrderState),
    OrderFileStr = string.builder.to_string(OrderState).

%---------------------------------------------------------------------------%

    % List a predicate's direct callees.
    %
:- pred construct_direct_pred_callees_entry(module_info::in,
    pred_callees::in, string::in, string::out,
    string.builder.state::di, string.builder.state::uo) is det.

construct_direct_pred_callees_entry(ModuleInfo, PredCallees,
        !MaybeNl, !State) :-
    PredCallees = pred_callees(_PredId, PredInfo, Callees),
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    list.filter_map(callee_get_local_pred_id, Callees, LocalCalleePredIds),
    list.map(lookup_callee_and_construct_direct_entry(ModuleInfo),
        LocalCalleePredIds, LocalCalleeDescEntries),
    % Print a blank line before every entry except the first.
    string.builder.append_string(!.MaybeNl, !State),
    !:MaybeNl = "\n",
    string.builder.format("%s\n", [s(PredDesc)], !State),
    string.builder.append_strings(LocalCalleeDescEntries, !State).

:- pred callee_get_local_pred_id(callee::in, pred_id::out) is semidet.

callee_get_local_pred_id(callee(PredId, is_local), PredId).

:- pred lookup_callee_and_construct_direct_entry(module_info::in, pred_id::in,
    string::out) is det.

lookup_callee_and_construct_direct_entry(ModuleInfo, PredId, PredDescEntry) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    string.format("    %s\n", [s(PredDesc)], PredDescEntry).

%---------------------------------------------------------------------------%

    % List a predicate's direct and indirect callees.
    %
    % This list can be considerably longer than the list of
    % just the direct callees. We therefore print it twice:
    %
    % - once in the order they are first encountered by a
    %   depth-first left-to-right traversal of the predicate's body, and
    %
    % - once in alphabetical order.
    %
    % The latter makes it easier to see whether a searched-for predicate
    % is in the call tree of this predicate, or not.
    %
:- pred construct_indirect_pred_callees_entry(module_info::in,
    pred_callees::in, string::in, string::out,
    string.builder.state::di, string.builder.state::uo) is det.

construct_indirect_pred_callees_entry(ModuleInfo, PredCallees,
        !MaybeNl, !State) :-
    PredCallees = pred_callees(_PredId, PredInfo, Callees),
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    list.map(lookup_callee_and_construct_indirect_entry(ModuleInfo),
        Callees, CalleeDescEntries),
    % Print a blank line before every entry except the first.
    string.builder.append_string(!.MaybeNl, !State),
    !:MaybeNl = "\n",
    string.builder.format("%s\n", [s(PredDesc)], !State),
    list.sort(CalleeDescEntries, SortedCalleeDescEntries),
    ( if CalleeDescEntries = SortedCalleeDescEntries then
        (
            CalleeDescEntries = []
            % There is no point in printing CalleeDescEntries twice,
            % and there is no point in even printing the heading.
        ;
            CalleeDescEntries = [_ | _],
            % There is no point in printing CalleeDescEntries twice.
            string.builder.append_string(
                "    <call and lexicographic order>\n", !State),
            string.builder.append_strings(CalleeDescEntries, !State)
        )
    else
        string.builder.append_string("    <call order>\n", !State),
        string.builder.append_strings(CalleeDescEntries, !State),
        string.builder.append_string("\n", !State),
        string.builder.append_string("    <lexicographic order>\n", !State),
        string.builder.append_strings(SortedCalleeDescEntries, !State)
    ).

:- pred lookup_callee_and_construct_indirect_entry(module_info::in, callee::in,
    string::out) is det.

lookup_callee_and_construct_indirect_entry(ModuleInfo, Callee, PredDescEntry) :-
    Callee = callee(PredId, _IsLocal),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    % XXX Should we pass include_module_name here if _IsLocal = is_not_local?
    % The extra info may be welcome, but some module names are quite long,
    % and the resulting overlong lines could be harder to read.
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    string.format("    %s\n", [s(PredDesc)], PredDescEntry).

%---------------------------------------------------------------------------%

:- pred construct_pred_order_entry(module_info::in, pred_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

construct_pred_order_entry(ModuleInfo, PredId, !State) :-
    PredDesc = describe_pred_from_id(do_not_include_module_name,
        ModuleInfo, PredId),
    string.builder.format("%s\n", [s(PredDesc)], !State).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type movability_report
    --->    movability_report(
                % The ids of the predicates that the user wants to move
                % to a new module.
                mr_want_to_move         :: set_tree234(pred_id),

                % The set of exported predicates that are not in the
                % want_to_move set that are reachable from the want_to_move
                % predicates.
                %
                % If this set is not empty, then the new module would
                % have to import the old. This would represent unwanted
                % coupling.
                mr_new_coupling         :: set_tree234(pred_id),

                % The set of predicates reachable from the want_to_move
                % predicates, which we would need to move to the new module.
                % This set will contain the want_to_move set.
                mr_all_to_move          :: set_tree234(pred_id),

                % The set of predicates that are reachable BOTH
                %
                % - from the want_to_move predicates, AND
                % - from the non-want_to_move exported predicates.
                %
                % The first says that the predicate should be moved;
                % the second says that it should stay.
                %
                % The main use case for the --show-movability option
                % is the automated computation of this set, because
                % if this set is not empty, then the programmer should
                %
                % - either reconsidet the set of predicates that should be
                %   moved to the new module being carved out of this one,
                % - or abandon the carving-out process altogether.
                mr_moving_staying       :: set_tree234(pred_id),

                % The set of locally-defined type_ctors that are used
                % in the interface of the current module.
                mr_ltcs_in_interface    :: set_tree234(name_arity),

                % The set of locally-defined type_ctors that are used
                % in moving predicates.
                mr_ltcs_moving_pred     :: set_tree234(name_arity),

                % The set of locally-defined type_ctors that are used
                % in staying predicates.
                mr_ltcs_staying_pred    :: set_tree234(name_arity)
            ).

generate_movability_report(ModuleInfo, CallTreeInfo, WantToMovePredNames,
        Specs) :-
    list.foldl3(acc_moving_pred_name(ModuleInfo), WantToMovePredNames,
        set_tree234.init, WantToMovePredIdSet,
        set_tree234.init, UnknownNameSet, set_tree234.init, AmbigNameSet),
    set_tree234.to_sorted_list(UnknownNameSet, UnknownNames),
    set_tree234.to_sorted_list(AmbigNameSet, AmbigNames),
    (
        UnknownNames = [],
        UnknownSpecs = []
    ;
        UnknownNames = [_ | _],
        UnknownPieces = [words("Error in the arguments"),
            words("of the --show-movability option: the"),
            words(choose_number(UnknownNames, "name", "names"))] ++
            list_to_pieces(UnknownNames) ++
            [words(choose_number(UnknownNames, "does not", "do not")),
            words("name any predicate or function in this module."), nl],
        UnknownSpec = simplest_no_context_spec($pred, severity_error,
            phase_style, UnknownPieces),
        UnknownSpecs = [UnknownSpec]
    ),
    (
        AmbigNames = [],
        AmbigSpecs = []
    ;
        AmbigNames = [_ | _],
        AmbigPieces = [words("Error in the arguments"),
            words("of the --show-movability option: the"),
            words(choose_number(AmbigNames,
                "name", "names"))] ++
            list_to_pieces(AmbigNames) ++
            [words(choose_number(AmbigNames,
                "is ambiguous.", "are ambiguous.")), nl],
        AmbigSpec = simplest_no_context_spec($pred, severity_error,
            phase_style, AmbigPieces),
        AmbigSpecs = [AmbigSpec]
    ),
    ( if
        UnknownSpecs = [],
        AmbigSpecs = []
    then
        CallTreeInfo = call_tree_info(_LocalPredIds, ExportPredIds,
            _PredCalleesList, _PredOrderMap,
            PredCalleeMap, _IndirectPredCalleeMap),
        set_tree234.list_to_set(ExportPredIds, ExportPredIdSet),
        set_tree234.difference(ExportPredIdSet, WantToMovePredIdSet,
            NonMovingExportPredIdSet),
        set_tree234.to_sorted_list(WantToMovePredIdSet, WantToMovePredIds),
        find_moving_pred_ids(PredCalleeMap,
            NonMovingExportPredIdSet, WantToMovePredIds,
            set_tree234.init, MovingPredIdSet),
        set_tree234.intersect(NonMovingExportPredIdSet, MovingPredIdSet,
            ConflictExportedPredIdSet),

        set_tree234.to_sorted_list(NonMovingExportPredIdSet,
            NonMovingExportPredIds),
        find_staying_pred_ids(PredCalleeMap, WantToMovePredIdSet,
            NonMovingExportPredIds, set_tree234.init, StayingPredIdSet),
        set_tree234.intersect(MovingPredIdSet, StayingPredIdSet,
            MovingStayingPredIdSet),

        set_tree234.foldl(acc_local_type_ctors_in_pred_arg_list(ModuleInfo),
            ExportPredIdSet, set_tree234.init, InInterfaceTypeCtorSet),
        set_tree234.foldl(acc_local_type_ctors_in_pred(ModuleInfo),
            StayingPredIdSet, set_tree234.init, StayingPredTypeCtorSet),
        set_tree234.foldl(acc_local_type_ctors_in_pred(ModuleInfo),
            MovingPredIdSet, set_tree234.init, MovingPredTypeCtorSet),

        Report = movability_report(WantToMovePredIdSet,
            ConflictExportedPredIdSet, MovingPredIdSet,
            MovingStayingPredIdSet,
            InInterfaceTypeCtorSet,
            MovingPredTypeCtorSet, StayingPredTypeCtorSet),
        construct_movability_report(ModuleInfo, Report, InfoSpec),
        Specs = [InfoSpec]
    else
        Specs = UnknownSpecs ++ AmbigSpecs
    ).

:- pred acc_moving_pred_name(module_info::in, string::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out,
    set_tree234(string)::in, set_tree234(string)::out,
    set_tree234(string)::in, set_tree234(string)::out) is det.

acc_moving_pred_name(ModuleInfo, PredName,
        !MovingPredIdSet, !UnknownNameSet, !AmbigNameSet) :-
    module_info_get_predicate_table(ModuleInfo, PredTable),
    module_info_get_name(ModuleInfo, ModuleName),
    SymName = qualified(ModuleName, PredName),
    predicate_table_lookup_sym(PredTable, is_fully_qualified,
        SymName, PredIds),
    (
        PredIds = [],
        set_tree234.insert(PredName, !UnknownNameSet)
    ;
        PredIds = [PredId],
        set_tree234.insert(PredId, !MovingPredIdSet)
    ;
        PredIds = [_, _ | _],
        set_tree234.insert(PredName, !AmbigNameSet)
    ).

:- pred find_moving_pred_ids(pred_callees_map::in,
    set_tree234(pred_id)::in, list(pred_id)::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out) is det.

find_moving_pred_ids(_, _, [], !ReachablePredIdSet).
find_moving_pred_ids(PredCalleeMap, NonMovingExportPredIdSet,
        [HeadPredId | TailPredIds], !ReachablePredIdSet) :-
    ( if set_tree234.insert_new(HeadPredId, !ReachablePredIdSet) then
        ( if set_tree234.contains(NonMovingExportPredIdSet, HeadPredId) then
            % If any of the NonMovingExportPredIdSet is reachable
            % from a pred_id that we want to move to a new module,
            % this would requre the both of the old and the new modules
            % to import each other, which is presumably what the user
            % of the --show-movability option is trying to avoid.
            % So just stop here, but only *after* adding HeadPredId
            % to !ReachablePredIdSet, thereby signaling the problem
            % to our caller.
            NextPredIds = TailPredIds
        else
            map.lookup(PredCalleeMap, HeadPredId, PredCallees),
            PredCallees = pred_callees(_, _, Callees),
            list.filter_map(callee_get_local_pred_id,
                Callees, LocalCalleePredIds),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.negated_filter(set_tree234.contains(!.ReachablePredIdSet),
                LocalCalleePredIds, NewLocalCalleePredIds),
            NextPredIds = NewLocalCalleePredIds ++ TailPredIds
        )
    else
        NextPredIds = TailPredIds
    ),
    find_moving_pred_ids(PredCalleeMap, NonMovingExportPredIdSet,
        NextPredIds, !ReachablePredIdSet).

:- pred find_staying_pred_ids(pred_callees_map::in,
    set_tree234(pred_id)::in, list(pred_id)::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out) is det.

find_staying_pred_ids(_, _, [], !StayingPredIdSet).
find_staying_pred_ids(PredCalleeMap, WantToMovePredIdSet,
        [HeadPredId | TailPredIds], !StayingPredIdSet) :-
    ( if set_tree234.contains(WantToMovePredIdSet, HeadPredId) then
        % HeadPredId is moving to a new module.
        NextPredIds = TailPredIds
    else
        ( if set_tree234.insert_new(HeadPredId, !StayingPredIdSet) then
            map.lookup(PredCalleeMap, HeadPredId, PredCallees),
            PredCallees = pred_callees(_, _, Callees),
            list.filter_map(callee_get_local_pred_id,
                Callees, LocalCalleePredIds),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.negated_filter(set_tree234.contains(!.StayingPredIdSet),
                LocalCalleePredIds, NewLocalCalleePredIds),
            NextPredIds = NewLocalCalleePredIds ++ TailPredIds
        else
            NextPredIds = TailPredIds
        )
    ),
    find_staying_pred_ids(PredCalleeMap, WantToMovePredIdSet, NextPredIds,
        !StayingPredIdSet).

%---------------------------------------------------------------------------%

:- pred acc_local_type_ctors_in_pred_arg_list(module_info::in, pred_id::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_pred_arg_list(ModuleInfo, PredId,
        !TypeCtorNameArities) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    list.foldl(acc_local_type_ctors_in_type(ModuleName), ArgTypes,
        !TypeCtorNameArities).

:- pred acc_local_type_ctors_in_pred(module_info::in, pred_id::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_pred(ModuleInfo, PredId, !TypeCtorNameArities) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.values(ProcTable, ProcInfos),
    list.foldl(acc_local_type_ctors_in_proc(ModuleName), ProcInfos,
        !TypeCtorNameArities).

:- pred acc_local_type_ctors_in_proc(module_name::in, proc_info::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_proc(ModuleName, ProcInfo, !TypeCtorNameArities) :-
    proc_info_get_var_table(ProcInfo, VarTable),
    var_table_entries(VarTable, VarTableEntries),
    list.foldl(acc_local_type_ctors_in_var_table_entry(ModuleName),
        VarTableEntries, !TypeCtorNameArities).

:- pred acc_local_type_ctors_in_var_table_entry(module_name::in,
    var_table_entry::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_var_table_entry(ModuleName, VarTableEntry,
        !TypeCtorNameArities) :-
    VarTableEntry = vte(_, Type, _),
    acc_local_type_ctors_in_type(ModuleName, Type, !TypeCtorNameArities).

%---------------------%

:- pred acc_local_type_ctors_in_type(module_name::in, mer_type::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_type(ModuleName, Type, !TypeCtorNameArities) :-
     (
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(SymName, ArgTypes, _),
        ( if SymName = qualified(ModuleName, Name) then
            list.length(ArgTypes, Arity),
            NameArity = name_arity(Name, Arity),
            set_tree234.insert(NameArity, !TypeCtorNameArities)
        else
            true
        ),
        acc_local_type_ctors_in_types(ModuleName, ArgTypes,
            !TypeCtorNameArities)
    ;
        ( Type = apply_n_type(_, ArgTypes, _)
        ; Type = higher_order_type(_, ArgTypes, _, _, _)
        ; Type = tuple_type(ArgTypes, _)
        ),
        acc_local_type_ctors_in_types(ModuleName, ArgTypes,
            !TypeCtorNameArities)
    ;
        Type = kinded_type(SubType, _),
        acc_local_type_ctors_in_type(ModuleName, SubType, !TypeCtorNameArities)
    ).

:- pred acc_local_type_ctors_in_types(module_name::in, list(mer_type)::in,
    set_tree234(name_arity)::in, set_tree234(name_arity)::out) is det.

acc_local_type_ctors_in_types(_, [], !TypeCtorNameArities).
acc_local_type_ctors_in_types(ModuleName, [Type | Types],
        !TypeCtorNameArities) :-
    acc_local_type_ctors_in_type(ModuleName, Type, !TypeCtorNameArities),
    acc_local_type_ctors_in_types(ModuleName, Types, !TypeCtorNameArities).

%---------------------------------------------------------------------------%

:- pred construct_movability_report(module_info::in, movability_report::in,
    error_spec::out) is det.

construct_movability_report(ModuleInfo, Report, InfoSpec) :-
    Report = movability_report(WantToMovePredIdSet, ConflictExportedPredIdSet,
        MovingPredIdSet, MovingStayingPredIdSet,
        InInterfaceTypeCtorSet, MovingPredTypeCtorSet, StayingPredTypeCtorSet),

    WantToMovePredPieces = pred_name_set_to_line_pieces(ModuleInfo,
        WantToMovePredIdSet),
    WantToMovePieces =
        [words("Report for the proposed move of")] ++
            WantToMovePredPieces ++
        [words("to a new module:"), nl, blank_line],

    MovingPredLinesAndDescs = set_tree234.map(
        make_line_number_and_desc_for_pred(ModuleInfo), MovingPredIdSet),
    MovingTypeLinesAndDescs = set_tree234.map(
        make_line_number_and_desc_for_type(ModuleInfo), MovingPredTypeCtorSet),
    MovingPredTypePieces = line_number_and_descs_to_format_pieces(
        set_tree234.union(MovingPredLinesAndDescs, MovingTypeLinesAndDescs)),

    MovingPieces =
        [words("The set of predicates, functions and/or types reachable from"),
        words("the proposed-to-be-moved predicates and/or functions,"),
        words("which should therefore be moved to the new module,"),
        words("would be")] ++
            MovingPredTypePieces,

    ( if set_tree234.is_empty(ConflictExportedPredIdSet) then
        ConflictExportedPieces = []
    else
        ConflictExportedPredPieces = pred_name_set_to_line_pieces(ModuleInfo,
            ConflictExportedPredIdSet),
        ConflictExportedPieces =
            [words("Moving these predicates and/or functions to a new module"),
            words("would require the new module to import the current module"),
            words("to get access to")] ++
                ConflictExportedPredPieces
    ),

    ( if set_tree234.is_empty(InInterfaceTypeCtorSet) then
        set_tree234.intersect(MovingPredTypeCtorSet, StayingPredTypeCtorSet,
            MovingStayingPredTypeCtorSet),
        ( if set_tree234.is_empty(MovingStayingPredTypeCtorSet) then
            MovingTypePieces =
                [words("All of the moved types can be private"),
                words("in the new module."), nl, blank_line]
        else
            ExportedTypeCtorPieces =
                type_name_set_to_line_pieces(MovingStayingPredTypeCtorSet),
            MovingTypePieces =
                [words("The following moved types are used by code"),
                words("that is staying in the current module,"),
                words("and would therefore need to be exported"),
                words("from the new module:")] ++
                    ExportedTypeCtorPieces
        )
    else
        InInterfaceTypeCtorPieces =
            type_name_set_to_line_pieces(InInterfaceTypeCtorSet),
        MovingTypePieces =
            [words("Moving these types to a new module would require"),
            words("current module to import the new module in its interface"),
            words("to get access to")] ++
                InInterfaceTypeCtorPieces
    ),

    ( if set_tree234.is_empty(MovingStayingPredIdSet) then
        MovingStayingPieces = []
    else
        MovingStayingPredPieces = pred_name_set_to_line_pieces(ModuleInfo,
            MovingStayingPredIdSet),
        MovingStayingPieces =
            [words("However, the following local predicates and/or functions"),
            words("are reachable both from code being moved and"),
            words("code that is staying, which means that they would"),
            words("need to be either duplicated, or, if included in only"),
            words("one of the two modules, old and new, they would"),
            words("need to be exported from the module they end up in"),
            words("to be accessible from the other module."),
            words("Neither option is usually a good idea.")] ++
                MovingStayingPredPieces
    ),

    InfoPieces = WantToMovePieces ++ ConflictExportedPieces ++ MovingPieces ++
        MovingTypePieces ++ MovingStayingPieces,
    InfoSpec = simplest_no_context_spec($pred, severity_informational,
        phase_style, InfoPieces).

%---------------------------------------------------------------------------%

:- type line_number_and_desc
    --->    line_number_and_desc(
                % The line number and ...
                int,
                % ... the description of the entity at that line.
                string
            ).

:- func make_line_number_and_desc_for_pred(module_info, pred_id)
    = line_number_and_desc.

make_line_number_and_desc_for_pred(ModuleInfo, PredId) = LineNumberDesc :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_context(PredInfo, Context),
    Context = context(_FileName, LineNumber),
    Desc = describe_pred_from_id(do_not_include_module_name,
        ModuleInfo, PredId),
    LineNumberDesc = line_number_and_desc(LineNumber, Desc).

:- func make_line_number_and_desc_for_type(module_info, name_arity)
    = line_number_and_desc.

make_line_number_and_desc_for_type(ModuleInfo, NameArity) = LineNumberDesc :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    module_info_get_name(ModuleInfo, ModuleName),
    NameArity = name_arity(Name, Arity),
    TypeCtor = type_ctor(qualified(ModuleName, Name), Arity),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    get_type_defn_context(TypeDefn, Context),
    Context = context(_FileName, LineNumber),
    string.format("type constructor %s/%d", [s(Name), i(Arity)], Desc),
    LineNumberDesc = line_number_and_desc(LineNumber, Desc).

:- func line_number_and_descs_to_format_pieces(
    set_tree234(line_number_and_desc)) = list(format_piece).

line_number_and_descs_to_format_pieces(LineNumberDescSet) = Pieces :-
    set_tree234.to_sorted_list(LineNumberDescSet, LineNumberDescs),
    % LineNumberDescs is sorted in line number.
    % It cannot be empty, because it contains the description
    % of at least one want-to-move predicate or function.
    list.det_split_last(LineNumberDescs, _, LastLineNumberDesc),
    LastLineNumberDesc = line_number_and_desc(LastLineNumber, _),
    ( if LastLineNumber >= 100_000 then
        % People don't usually create modules which contain lines
        % whose line numbers contain seven digits. If they don't create them,
        % they can't want to split them up.
        NumDigits = digits_6
    else if LastLineNumber >= 10_000 then
        NumDigits = digits_5
    else if LastLineNumber >= 1_000 then
        NumDigits = digits_4
    else
        % People don't usually want to split up a module in which
        % all the entities mentioned in a report like this
        % all have one- or two-digit line numbers.
        NumDigits = digits_3
    ),
    list.map(line_number_and_desc_to_string(NumDigits), LineNumberDescs,
        LineNumberDescStrs),
    Pieces = line_list_to_line_pieces(LineNumberDescStrs).

:- type num_digits
    --->    digits_3
    ;       digits_4
    ;       digits_5
    ;       digits_6.

:- pred line_number_and_desc_to_string(num_digits::in,
    line_number_and_desc::in, string::out) is det.

line_number_and_desc_to_string(NumDigits, LineNumberDesc, LineNumberDescStr) :-
    LineNumberDesc = line_number_and_desc(LineNumber, Desc),
    % It would be nice if string.format allowed the specification
    % of the width to be supplied by a parameter, as e.g. printf in C does.
    %
    % We could construct the format string here, but format_call.m
    % does not (yet) know how to handle a situation where part of a
    % format specification is constructed dynamically using any operation
    % other than appending strings. (This is because string.append and
    % string.append_list are the only two operations it knows how to evaluate
    % statically.)
    (
        NumDigits = digits_3,
        string.format("line %3d: %s", [i(LineNumber), s(Desc)],
            LineNumberDescStr)
    ;
        NumDigits = digits_4,
        string.format("line %4d: %s", [i(LineNumber), s(Desc)],
            LineNumberDescStr)
    ;
        NumDigits = digits_5,
        string.format("line %5d: %s", [i(LineNumber), s(Desc)],
            LineNumberDescStr)
    ;
        NumDigits = digits_6,
        string.format("line %6d: %s", [i(LineNumber), s(Desc)],
            LineNumberDescStr)
    ).

%---------------------%

:- func pred_name_set_to_line_pieces(module_info,
    set_tree234(pred_id)) = list(format_piece).

pred_name_set_to_line_pieces(ModuleInfo, PredIdSet) = Pieces :-
    PredDescSet = set_tree234.map(
        describe_pred_from_id(do_not_include_module_name, ModuleInfo),
        PredIdSet),
    set_tree234.to_sorted_list(PredDescSet, PredDescs),
    Pieces = line_list_to_line_pieces(PredDescs).

:- func type_name_set_to_line_pieces(set_tree234(name_arity))
    = list(format_piece).

type_name_set_to_line_pieces(NameAritySet) = Pieces :-
    PredDescSet = set_tree234.map( name_arity_to_string, NameAritySet),
    set_tree234.to_sorted_list(PredDescSet, PredDescs),
    Pieces = line_list_to_line_pieces(PredDescs).

:- func name_arity_to_string(name_arity) = string.

name_arity_to_string(name_arity(Name, Arity)) = Str :-
    string.format("%s/%d", [s(Name), i(Arity)], Str).

%---------------------%

:- func line_list_to_line_pieces(list(string)) = list(format_piece).

line_list_to_line_pieces(Lines) = Pieces :-
    list.det_split_last(Lines, AllButLastLines, LastLine),
    AllButLastLinePieceLists =
        list.map((func(PD) = [fixed(PD), nl]), AllButLastLines),
    list.condense(AllButLastLinePieceLists, AllButLastLinePieces),

    Pieces = [nl_indent_delta(1), blank_line] ++
        AllButLastLinePieces ++
        [fixed(LastLine), nl_indent_delta(-1), blank_line].

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_call_tree.
%---------------------------------------------------------------------------%
