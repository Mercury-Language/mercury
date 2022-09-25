%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module constructs a local call tree of the given module,
% which is the call tree restricted to just the predicates of the module,
% ignoring any references to predicates defined in other modules.
% It does so in a piecewise fashion. Each piece maps a local predicate
% to the list of other local predicates it calls, but since we can also
% look up the pieces of the callees, we can (and do) traverse this
% data structure as it were a fullly materialized tree.
%
% We consider a reference to a closure containing a pred_id to be callee
% just like a plain_call containing a pred_id, because the usual reason
% for constructing a closure is that we want it to be called, though the call
% may be done by other predicate (such as list.map, map.foldl etc).
%
% We write out the info in this call tree in the form of entries like this:
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
% its callees, in the order in which a depth-first left-to-right traversal
% of the first valid procedure of the predicate first encounters them.
%
% We start at the first exported predicate, write out the predicates
% in its call tree in the same depth-first left-to-right order.
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
% This local call tree is one of two outputs we generate. The second output
% is derived from the first: it is a list of just the predicates of the module
% in the order in which the first output first encounters them, like this:
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
%---------------------------------------------------------------------------%

:- module hlds.hlds_call_tree.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

    % Write out the pieces of the depth-first left-to-right traversal
    % (such as the first example above) of the given module to the first
    % output stream, and write the flatted predicate order to the second
    % output stream.
    %
:- pred write_local_call_tree(io.text_output_stream::in,
    io.text_output_stream::in, module_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

write_local_call_tree(TreeStream, OrderStream, ModuleInfo, !IO) :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_sorted_assoc_list(PredIdTable, PredIdsInfos),
    find_local_preds_exports(PredIdsInfos,
        set_tree234.init, LocalPredIds, one_or_more_map.init, ExportLineMap),
    one_or_more_map.to_flat_assoc_list(ExportLineMap, ExportLineList),
    assoc_list.values(ExportLineList, ExportList),

    gather_pred_callees(PredIdTable, LocalPredIds, ExportList,
        set_tree234.init, cord.init, PredCallesCord, map.init, PredCalleeMap),
    PredCallesList = cord.list(PredCallesCord),
    list.foldl2(write_pred_callees_entry(TreeStream, ModuleInfo),
        PredCallesList, yes, _First, !IO),

    construct_depth_first_left_right_order(PredCalleeMap, PredCallesList,
        set_tree234.init, cord.init, PredIdCord),
    PredIdList = cord.list(PredIdCord),
    list.foldl(write_pred_order_entry(OrderStream, ModuleInfo),
        PredIdList, !IO).

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

:- type pred_callees
    --->    pred_callees(
                % The local predicate described by these two fields ...
                pred_id,
                pred_info,

                % ... calls these local predicates. Each callee is present
                % in the list just once. The order of the list is given
                % by the order in which the first occurrence of each callee
                % is encountered in a depth-first left-to-right traversal
                % of the first valid procedure of the predicate.
                list(pred_id)
            ).

:- pred gather_pred_callees(pred_id_table::in, set_tree234(pred_id)::in,
    list(pred_id)::in, set_tree234(pred_id)::in,
    cord(pred_callees)::in, cord(pred_callees)::out,
    map(pred_id, pred_callees)::in, map(pred_id, pred_callees)::out) is det.

gather_pred_callees(_PredIdTable, _LocalPredIds, [],
        _HandledPredIds, !PredCalleesCord, !PredCalleeMap).
gather_pred_callees(PredIdTable, LocalPredIds, [HeadPredId | TailPredIds],
        !.HandledPredIds, !PredCalleesCord, !PredCalleeMap) :-
    ( if set_tree234.insert_new(HeadPredId, !HandledPredIds) then
        map.lookup(PredIdTable, HeadPredId, PredInfo),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.to_assoc_list(ProcTable, ProcIdsInfos),
        ( if find_first_valid_proc(ProcIdsInfos, ValidProcInfo) then
            proc_info_get_goal(ValidProcInfo, Goal),
            acc_goal_callees(Goal, cord.init, AllCalleesCord),
            AllCalleesList = cord.list(AllCalleesCord),
            list.filter(set_tree234.contains(LocalPredIds),
                AllCalleesList, LocalCalleesList0),
            LocalCalleesList = keep_only_first_calls(LocalCalleesList0),
            PredCallees = pred_callees(HeadPredId, PredInfo, LocalCalleesList),
            cord.snoc(PredCallees, !PredCalleesCord),
            map.det_insert(HeadPredId, PredCallees, !PredCalleeMap),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.filter(set_tree234.contains(!.HandledPredIds),
                LocalCalleesList, _OldLocalCalleesList, NewLocalCalleesList),
            NextPredIds = NewLocalCalleesList ++ TailPredIds
        else
            % Builtin predicates have no procedures in the HLDS,
            % and other predicates may have only procedures that mode analysis
            % has found to be invalid.
            NextPredIds = TailPredIds
        )
    else
        NextPredIds = TailPredIds
    ),
    gather_pred_callees(PredIdTable, LocalPredIds, NextPredIds,
        !.HandledPredIds, !PredCalleesCord, !PredCalleeMap).

:- pred find_first_valid_proc(assoc_list(proc_id, proc_info)::in,
    proc_info::out) is semidet.

find_first_valid_proc([], _) :-
    fail.
find_first_valid_proc([_ProcId - ProcInfo | ProcIdsInfos],
        FirstValidProcInfo) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
        FirstValidProcInfo = ProcInfo
    else
        find_first_valid_proc(ProcIdsInfos, FirstValidProcInfo)
    ).

%---------------------%

:- pred acc_goal_callees(hlds_goal::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

acc_goal_callees(Goal, !CalleeCord) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, RHS, _, Unification, _),
        (
            Unification = construct(_, UnifyConsId, _, _, _, _, _),
            acc_cons_id_callees(UnifyConsId, !CalleeCord)
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
            acc_cons_id_callees(RHSConsId, !CalleeCord)
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, SubGoal),
            acc_goal_callees(SubGoal, !CalleeCord)
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
        list.foldl(acc_goal_callees, SubGoals, !CalleeCord)
    ;
        GoalExpr = disj(SubGoals),
        list.foldl(acc_goal_callees, SubGoals, !CalleeCord)
    ;
        GoalExpr = switch(_, _, Cases),
        SubGoals = list.map((func(C) = C ^ case_goal), Cases),
        list.foldl(acc_goal_callees, SubGoals, !CalleeCord)
    ;
        GoalExpr = negation(SubGoal),
        acc_goal_callees(SubGoal, !CalleeCord)
    ;
        GoalExpr = scope(_Reason, SubGoal),
        acc_goal_callees(SubGoal, !CalleeCord)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        acc_goal_callees(Cond, !CalleeCord),
        acc_goal_callees(Then, !CalleeCord),
        acc_goal_callees(Else, !CalleeCord)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        ;
            Shorthand = atomic_goal(_Type, _Outer, _Inner, _OutputVars,
                MainGoal, OrElseGoals, _Inners),
            acc_goal_callees(MainGoal, !CalleeCord),
            list.foldl(acc_goal_callees, OrElseGoals, !CalleeCord)
        ;
            Shorthand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            acc_goal_callees(SubGoal, !CalleeCord)
        )
    ).

:- pred acc_cons_id_callees(cons_id::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

acc_cons_id_callees(ConsId, !CalleeCord) :-
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        ShroudedPredProcId = shrouded_pred_proc_id(PredIdInt, _),
        ShroudedPredId = shrouded_pred_id(PredIdInt),
        PredId = unshroud_pred_id(ShroudedPredId),
        cord.snoc(PredId, !CalleeCord)
    else
        true
    ).

%---------------------%

:- func keep_only_first_calls(list(pred_id)) = list(pred_id).

keep_only_first_calls(ListWithDuplicates) = ListWithoutDuplicates :-
    SeenPredIds0 = set_tree234.init,
    keep_only_first_calls_loop(ListWithDuplicates, SeenPredIds0,
        cord.init, CordWithoutDuplicates),
    ListWithoutDuplicates = cord.list(CordWithoutDuplicates).

:- pred keep_only_first_calls_loop(list(pred_id)::in, set_tree234(pred_id)::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

keep_only_first_calls_loop([], _, !CordWithoutDuplicates).
keep_only_first_calls_loop([PredId | PredIds], !.SeenPredIds,
        !CordWithoutDuplicates) :-
    ( if set_tree234.insert_new(PredId, !SeenPredIds) then
        cord.snoc(PredId, !CordWithoutDuplicates)
    else
        true
    ),
    keep_only_first_calls_loop(PredIds, !.SeenPredIds, !CordWithoutDuplicates).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred construct_depth_first_left_right_order(map(pred_id, pred_callees)::in,
    list(pred_callees)::in, set_tree234(pred_id)::in,
    cord(pred_id)::in, cord(pred_id)::out) is det.

construct_depth_first_left_right_order(_, [], _, !PredIdCord).
construct_depth_first_left_right_order(PredCalleeMap,
        [HeadPredCallees | TailPredCallees], !.HandledPredIds, !PredIdCord) :-
    HeadPredCallees = pred_callees(PredId, _PredInfo, Callees),
    ( if set_tree234.insert_new(PredId, !HandledPredIds) then
        cord.snoc(PredId, !PredIdCord),
        list.filter(set_tree234.contains(!.HandledPredIds), Callees,
            _OldCallees, NewCallees),
        % Some predicates in NewCallees may not be in PredCalleeMap,
        % because they have no valid procedures.
        list.filter_map(map.search(PredCalleeMap), NewCallees, NewPredCallees),
        NextPredCallees = NewPredCallees ++ TailPredCallees
    else
        NextPredCallees = TailPredCallees
    ),
    construct_depth_first_left_right_order(PredCalleeMap,
        NextPredCallees, !.HandledPredIds, !PredIdCord).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred write_pred_callees_entry(io.text_output_stream::in, module_info::in,
    pred_callees::in, bool::in, bool::out, io::di, io::uo) is det.

write_pred_callees_entry(Stream, ModuleInfo, PredCallees, !IsFirst, !IO) :-
    PredCallees = pred_callees(_PredId, PredInfo, Callees),
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    % Print a newline before every entry except the first.
    (
        !.IsFirst = yes,
        !:IsFirst = no
    ;
        !.IsFirst = no,
        io.nl(Stream, !IO)
    ),
    io.format(Stream, "%s\n", [s(PredDesc)], !IO),
    list.foldl(lookup_and_write_callee(Stream, ModuleInfo), Callees, !IO).

:- pred lookup_and_write_callee(io.text_output_stream::in, module_info::in,
    pred_id::in, io::di, io::uo) is det.

lookup_and_write_callee(Stream, ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredDesc = describe_pred(do_not_include_module_name, PredInfo),
    io.format(Stream, "    %s\n", [s(PredDesc)], !IO).

%---------------------------------------------------------------------------%

:- pred write_pred_order_entry(io.text_output_stream::in, module_info::in,
    pred_id::in, io::di, io::uo) is det.

write_pred_order_entry(Stream, ModuleInfo, PredId, !IO) :-
    PredDesc = describe_pred_from_id(do_not_include_module_name,
        ModuleInfo, PredId),
    io.format(Stream, "%s\n", [s(PredDesc)], !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_call_tree.
%---------------------------------------------------------------------------%
