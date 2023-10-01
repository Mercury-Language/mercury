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
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------%

:- type call_tree_info.

    % Compute a representation of the local call tree.
    % The caller can give this representation to either or both of two
    % predicates below: write_local_call_tree and generate_movability_report.
    %
:- pred compute_local_call_tree(module_info::in, call_tree_info::out) is det.

    % Write out the pieces of the depth-first left-to-right traversal
    % (such as the first example above) of the given module to the first
    % output stream, and write the flatted predicate order to the second
    % output stream.
    %
:- pred write_local_call_tree(io.text_output_stream::in,
    io.text_output_stream::in, module_info::in, call_tree_info::in,
    io::di, io::uo) is det.

%---------------------%

:- pred generate_movability_report(module_info::in, call_tree_info::in,
    list(string)::in, list(error_spec)::out) is det.

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
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

:- type call_tree_info
    --->    call_tree_info(
                cti_local_pred_set      :: set_tree234(pred_id),
                cti_exported_preds      :: list(pred_id),
                cti_pred_callee_list    :: list(pred_callees),
                cti_pred_callee_map     :: pred_callees_map
            ).

:- type pred_callees_map == map(pred_id, pred_callees).

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

%---------------------------------------------------------------------------%

compute_local_call_tree(ModuleInfo, CallTreeInfo) :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_sorted_assoc_list(PredIdTable, PredIdsInfos),
    find_local_preds_exports(PredIdsInfos,
        set_tree234.init, LocalPredIds, one_or_more_map.init, ExportLineMap),
    one_or_more_map.to_flat_assoc_list(ExportLineMap, ExportLineList),
    assoc_list.values(ExportLineList, ExportList),

    gather_pred_callees(PredIdTable, LocalPredIds, ExportList,
        set_tree234.init, cord.init, PredCalleesCord, map.init, PredCalleeMap),
    PredCalleesList = cord.list(PredCalleesCord),

    CallTreeInfo = call_tree_info(LocalPredIds, ExportList,
        PredCalleesList, PredCalleeMap).

write_local_call_tree(TreeStream, OrderStream, ModuleInfo,
        CallTreeInfo, !IO) :-
    CallTreeInfo = call_tree_info(_LocalPredIds, _ExportList,
        PredCalleesList, PredCalleeMap),

    list.foldl2(write_pred_callees_entry(TreeStream, ModuleInfo),
        PredCalleesList, yes, _First, !IO),

    construct_depth_first_left_right_order(PredCalleeMap, PredCalleesList,
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
find_first_valid_proc([_ProcId - ProcInfo | _ProcIdsInfos], ProcInfo).

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
%---------------------------------------------------------------------------%

:- type movability_report
    --->    movability_report(
                % The ids of the predicates that the user wants to move
                % to a new module.
                mr_want_to_move     :: set_tree234(pred_id),

                % The set of exported predicates that are not in the
                % want_to_move set that are reachable from the want_to_move
                % predicates.
                %
                % If this set is not empty, then the new module would
                % have to import the old. This would represent unwanted
                % coupling.
                mr_new_coupling     :: set_tree234(pred_id),

                % The set of predicates reachable from the want_to_move
                % predicates, which we would need to move to the new module.
                % This set will contain the want_to_move set.
                mr_all_to_move      :: set_tree234(pred_id),

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
                mr_moving_staying   :: set_tree234(pred_id)
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
            words(choose_number(UnknownNames,
                "name", "names"))] ++
            list_to_pieces(UnknownNames) ++
            [words(choose_number(UnknownNames,
                "does not", "do not")),
            words("don't name any predicate or function."), nl],
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
            _PredCalleesList, PredCalleeMap),
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
        Report = movability_report(WantToMovePredIdSet,
            ConflictExportedPredIdSet, MovingPredIdSet,
            MovingStayingPredIdSet),
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
            PredCallees = pred_callees(_, _, LocalCalleesList),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.filter(set_tree234.contains(!.ReachablePredIdSet),
                LocalCalleesList, _OldLocalCalleesList, NewLocalCalleesList),
            NextPredIds = NewLocalCalleesList ++ TailPredIds
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
            PredCallees = pred_callees(_, _, LocalCalleesList),
            % Depth-first traversal: traverse the callees of HeadPredId
            % before traversing TailPredIds.
            %
            % We have to filter out the callees that have already been handled.
            % We don't have to do it *here*; we could leave it for the
            % recursive call. However, doing it here substantially reduces
            % the maximum depth of the recursion.
            list.filter(set_tree234.contains(!.StayingPredIdSet),
                LocalCalleesList, _OldLocalCalleesList, NewLocalCalleesList),
            NextPredIds = NewLocalCalleesList ++ TailPredIds
        else
            NextPredIds = TailPredIds
        )
    ),
    find_staying_pred_ids(PredCalleeMap, WantToMovePredIdSet, NextPredIds,
        !StayingPredIdSet).

%---------------------------------------------------------------------------%

:- pred construct_movability_report(module_info::in, movability_report::in,
    error_spec::out) is det.

construct_movability_report(ModuleInfo, Report, InfoSpec) :-
    Report = movability_report(WantToMovePredIdSet, ConflictExportedPredIdSet,
        MovingPredIdSet, MovingStayingPredIdSet),

    WantToMovePredPieces = name_set_to_line_pieces(ModuleInfo,
        WantToMovePredIdSet),
    MovingPredPieces = name_set_to_line_pieces(ModuleInfo,
        MovingPredIdSet),

    WantToMovePieces =
        [words("Report for the proposed move of")] ++
            WantToMovePredPieces ++
        [words("to a new module:"), nl, blank_line],

    ( if set_tree234.is_empty(ConflictExportedPredIdSet) then
        ConflictExportedPieces = []
    else
        ConflictExportedPredPieces = name_set_to_line_pieces(ModuleInfo,
            ConflictExportedPredIdSet),
        ConflictExportedPieces =
            [words("Moving these predicates and/or functions to a new module"),
            words("would require the new module to import the current module"),
            words("to get access to")] ++
                ConflictExportedPredPieces
    ),

    MovingPieces =
        [words("The set of predicates and/or functions reachable from"),
        words("the proposed-to-be-moved predicates and/or functions,"),
        words("which should therefore be moved to the new module,"),
        words("would be")] ++
            MovingPredPieces,

    ( if set_tree234.is_empty(MovingStayingPredIdSet) then
        MovingStayingPieces = []
    else
        MovingStayingPredPieces = name_set_to_line_pieces(ModuleInfo,
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

    InfoPieces = WantToMovePieces ++ ConflictExportedPieces ++
        MovingPieces ++ MovingStayingPieces,
    InfoSpec = simplest_no_context_spec($pred, severity_informational,
        phase_style, InfoPieces).

:- func name_set_to_line_pieces(module_info, set_tree234(pred_id)) =
    list(format_piece).

name_set_to_line_pieces(ModuleInfo, PredIdSet) = Pieces :-
    PredDescSet = set_tree234.map(
        describe_pred_from_id(do_not_include_module_name, ModuleInfo),
        PredIdSet),
    set_tree234.to_sorted_list(PredDescSet, PredDescs),
    list.det_split_last(PredDescs, AllButLastPredDescs, LastPredDesc),
    AllButLastPredDescPieceLists =
        list.map((func(PD) = [fixed(PD), nl]), AllButLastPredDescs),
    list.condense(AllButLastPredDescPieceLists, AllButLastPredDescPieces),

    Pieces = [nl_indent_delta(1), blank_line] ++
        AllButLastPredDescPieces ++
        [fixed(LastPredDesc), nl_indent_delta(-1), blank_line].

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_call_tree.
%---------------------------------------------------------------------------%
