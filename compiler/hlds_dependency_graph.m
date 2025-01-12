%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2017-2021, 2023-2024 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_dependency_graph.m.
% Main authors: bromage, conway, stayl, zs.
%
% The HLDS dependency graph is an instance of dependency_graph in which
% the entities are the HLDS procedures in the module being compiled.
% The criterion for inclusion in the dependency graph is "do we have access
% to the body of this procedure?", which means that imported procedures
% are *not* included, but opt_imported procedures *are*.
%
% The reason why we build the dependency graph is because from it,
% dependency_graph.m can compute the list of the SCCs (strongly-connected
% components) of this graph. This is very handy for doing fixpoint iterations.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_dependency_graph.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.dependency_graph.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type hlds_dependency_info        == dependency_info(pred_proc_id).

:- type hlds_dependency_graph       == dependency_graph(pred_proc_id).
:- type hlds_dependency_graph_key   == dependency_graph_key(pred_proc_id).

:- type hlds_bottom_up_dependency_sccs
    == bottom_up_dependency_sccs(pred_proc_id).

%---------------------------------------------------------------------------%

    % Ensure that the module_info contains a version of the dependency_info
    % which only contains arcs between procedures for which there are clauses
    % defined (everything that is not imported, plus opt_imported).
    % Return this dependency_info.
    %
    % There is no guarantee that the dependency_info is current.
    %
:- pred module_info_ensure_dependency_info(module_info::in, module_info::out,
    hlds_dependency_info::out) is det.

    % Ensure that the module_info contains a version of the dependency_info
    % which only contains arcs between procedures for which there are clauses
    % defined (everything that is not imported, plus opt_imported).
    % Return this dependency_info.
    %
    % The dependency_info will be up-to-date.
    %
:- pred module_info_rebuild_dependency_info(module_info::in, module_info::out,
    hlds_dependency_info::out) is det.

%---------------------%

:- type include_imported
    --->    include_imported
    ;       do_not_include_imported.

    % Should the dependency graph include an edge from p to q
    % only if p calls q in a tail call (only_tail_calls calls for this),
    % or if p calls q in any call, and if p references q in a unification
    % (all_calls_and_unifies).
    %
    % Note that only_tail_calls requires recursive calls to be marked by
    % mark_tail_calls.m, and mark_tail_calls.m requires a previously built
    % dependency graph, which therefore must have been built with
    % all_calls_and_unifies.
    %
:- type what_dependency_edges
    --->    only_tail_calls
    ;       only_all_calls
    ;       all_calls_and_unifies.

    % Build the dependency graph for the given set of predicates,
    % after filtering out imported predicates if the last argument
    % is do_not_include_imported.
    %
    % Predicates without mode information have no idea what calls will
    % end up being tail calls, so for their dependency graphs, we cannot
    % restrict the edges to tail calls.
    %
:- func build_pred_dependency_graph(module_info, list(pred_id),
    include_imported) = dependency_info(pred_id).

    % Build the dependency graph for the given set of procedures.
    %
:- func build_proc_dependency_graph(module_info, set(pred_proc_id),
    what_dependency_edges) = dependency_info(pred_proc_id).

%---------------------%

:- type scc_with_entry_points
    --->    scc_with_entry_points(
                % The set of procedures in the SCC.
                swep_scc_procs                  :: set(pred_proc_id),
                swep_called_from_higher_sccs    :: set(pred_proc_id),
                swep_exported_procs             :: set(pred_proc_id)
            ).

:- pred get_bottom_up_sccs_with_entry_points(module_info::in,
    hlds_dependency_info::in, list(scc_with_entry_points)::out) is det.

%---------------------------------------------------------------------------%

    % Return a form of the static call graph as a string, in a format suitable
    % for use in .dependency_info files. After the heading, the format of
    % each line is
    %
    %   CallerModeDecl \t CalleeModeDecl
    %
:- pred dependency_graph_to_string(string::out,
    module_info::in, module_info::out) is det.

    % Return a form of the static call graph as a file for use by the profiler.
    % There is no heading, and the format of each line is
    %
    %   CallerLabel \t CalleeLabel
    %
:- pred prof_dependency_graph_to_string(string::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module std_util.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

module_info_ensure_dependency_info(!ModuleInfo, DepInfo) :-
    module_info_get_maybe_dependency_info(!.ModuleInfo, MaybeDepInfo),
    (
        MaybeDepInfo = yes(DepInfo)
    ;
        MaybeDepInfo = no,
        module_info_rebuild_dependency_info(!ModuleInfo, DepInfo)
    ).

module_info_rebuild_dependency_info(!ModuleInfo, DepInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(gather_pred_proc_ids(!.ModuleInfo, do_not_include_imported),
        PredIds, [], GatheredPredProcIds),
    DepInfo = build_proc_dependency_graph(!.ModuleInfo,
        set.list_to_set(GatheredPredProcIds), all_calls_and_unifies),
    module_info_set_dependency_info(DepInfo, !ModuleInfo).

:- pred gather_pred_proc_ids(module_info::in, include_imported::in,
    pred_id::in, list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

gather_pred_proc_ids(ModuleInfo, Imported, PredId, !PredProcIds) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    (
        % Don't bother to add imported procedures, since we don't have
        % their bodies.
        % XXX We do have bodies for opt_imported procedures.
        Imported = do_not_include_imported,
        ProcIds = pred_info_all_non_imported_procids(PredInfo)
    ;
        Imported = include_imported,
        ProcIds = pred_info_all_procids(PredInfo)
    ),
    list.foldl(gather_pred_proc_id(PredId), ProcIds, !PredProcIds).

:- pred gather_pred_proc_id(pred_id::in, proc_id::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

gather_pred_proc_id(PredId, ProcId, !PredProcIds) :-
    !:PredProcIds = [proc(PredId, ProcId) | !.PredProcIds].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

build_pred_dependency_graph(ModuleInfo, PredIds, Imported) = DepInfo :-
    list.foldl(gather_pred_ids(ModuleInfo, Imported), PredIds,
        [], GatheredPredIds),

    digraph.init(DepGraph0),
    list.map_foldl(add_vertex, GatheredPredIds, _VertexKeys,
        DepGraph0, DepGraph1),

    list.foldl(
        maybe_add_pred_arcs(DepGraph1, all_calls_and_unifies, ModuleInfo),
        PredIds, [], DepArcs),
    digraph.add_assoc_list(DepArcs, DepGraph1, DepGraph),
    DepInfo = make_dependency_info(DepGraph, DepArcs).

:- pred gather_pred_ids(module_info::in, include_imported::in, pred_id::in,
    list(pred_id)::in, list(pred_id)::out) is det.

gather_pred_ids(ModuleInfo, IncludeImported, PredId, !PredIds) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if
        IncludeImported = do_not_include_imported,
        pred_info_is_imported(PredInfo)
    then
        % Don't bother adding nodes (or arcs) for predicates
        % which are imported (i.e. which we don't have any `clauses' for).
        % XXX This is slightly wrong: if a predicate is opt_imported,
        % then pred_info_is_imported will succeed for it, but we *will* have
        % its clauses.
        true
    else
        !:PredIds = [PredId | !.PredIds]
    ).

:- pred maybe_add_pred_arcs(dependency_graph(pred_id)::in,
    what_dependency_edges::in, module_info::in, pred_id::in,
    dep_arcs(pred_id)::in, dep_arcs(pred_id)::out) is det.

maybe_add_pred_arcs(DepGraph, WhatEdges, ModuleInfo, PredId, !DepArcs) :-
    ( if digraph.search_key(DepGraph, PredId, Caller) then
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
        get_clause_list_maybe_repeated(ClausesRep, Clauses),
        Goals = list.map(clause_body, Clauses),
        add_dependency_arcs_in_goals(DepGraph, WhatEdges, Caller, Goals,
            !DepArcs)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

build_proc_dependency_graph(ModuleInfo, PredProcIds, WhatEdges) = DepInfo :-
    digraph.init(DepGraph0),
    set.map_fold(add_vertex, PredProcIds, _VertexKeys, DepGraph0, DepGraph1),
    PredIds = set.map(pred_proc_id_get_pred_id, PredProcIds),
    set.foldl(maybe_add_pred_proc_arcs(DepGraph1, WhatEdges, ModuleInfo),
        PredIds, [], DepArcs),
    digraph.add_assoc_list(DepArcs, DepGraph1, DepGraph),
    DepInfo = make_dependency_info(DepGraph, DepArcs).

:- func pred_proc_id_get_pred_id(pred_proc_id) = pred_id.

pred_proc_id_get_pred_id(proc(PredId, _ProcId)) = PredId.

:- pred maybe_add_pred_proc_arcs(dependency_graph(pred_proc_id)::in,
    what_dependency_edges::in, module_info::in, pred_id::in,
    dep_arcs(pred_proc_id)::in, dep_arcs(pred_proc_id)::out) is det.

maybe_add_pred_proc_arcs(DepGraph, WhatEdges, ModuleInfo, PredId, !DepArcs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(maybe_add_proc_arcs(DepGraph, WhatEdges, PredId), ProcTable,
        !DepArcs).

:- pred maybe_add_proc_arcs(dependency_graph(pred_proc_id)::in,
    what_dependency_edges::in, pred_id::in, proc_id::in, proc_info::in,
    dep_arcs(pred_proc_id)::in, dep_arcs(pred_proc_id)::out) is det.

maybe_add_proc_arcs(DepGraph, WhatEdges, PredId, ProcId, ProcInfo, !DepArcs) :-
    ( if digraph.search_key(DepGraph, proc(PredId, ProcId), Caller) then
        proc_info_get_goal(ProcInfo, Goal),
        add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Goal,
            !DepArcs)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- typeclass dependency_node(T) where [
    func dependency_node(pred_proc_id) = T
].

:- instance dependency_node(pred_proc_id) where [
    func(dependency_node/1) is id
].

:- instance dependency_node(pred_id) where [
    func(dependency_node/1) is pred_proc_id_get_pred_id
].

%---------------------%

:- type dep_arcs(T) == assoc_list(dependency_graph_key(T)).

%---------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_goal(dependency_graph(T)::in,
    what_dependency_edges::in, digraph_key(T)::in, hlds_goal::in,
    dep_arcs(T)::in, dep_arcs(T)::out) is det <= dependency_node(T).

add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Goal, !DepArcs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        add_dependency_arcs_in_goals(DepGraph, WhatEdges, Caller, Goals,
            !DepArcs)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        add_dependency_arcs_in_cases(DepGraph, WhatEdges, Caller, Cases,
            !DepArcs)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Cond,
            !DepArcs),
        add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Then,
            !DepArcs),
        add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Else,
            !DepArcs)
    ;
        GoalExpr = negation(SubGoal),
        add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, SubGoal,
            !DepArcs)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope references no predicates or procedures.
            true
        else
            add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, SubGoal,
            !DepArcs)
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, Builtin, _, _),
        (
            Builtin = inline_builtin
        ;
            Builtin = not_builtin,
            ( if
                goal_info_has_feature(GoalInfo,
                    feature_self_or_mutual_tail_rec_call)
            then
                EdgeKind = edge_tail_call
            else
                EdgeKind = edge_non_tail_call
            ),
            maybe_add_dependency_arc(DepGraph, WhatEdges, EdgeKind,
                Caller, proc(PredId, ProcId), !DepArcs)
        )
    ;
        GoalExpr = unify(_,_,_,Unify,_),
        (
            ( Unify = construct(_, ConsId, _, _, _, _, _)
            ; Unify = deconstruct(_, ConsId, _, _, _, _)
            ),
            add_dependency_arcs_in_cons(DepGraph, WhatEdges, Caller, ConsId,
                !DepArcs)
        ;
            ( Unify = assign(_, _)
            ; Unify = simple_test(_, _)
            ; Unify = complicated_unify(_, _, _)
            )
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner, _Vars,
                MainGoal, OrElseGoals, _OrElseInners),
            add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller,
                MainGoal, !DepArcs),
            add_dependency_arcs_in_goals(DepGraph, WhatEdges, Caller,
                OrElseGoals, !DepArcs)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, SubGoal,
                !DepArcs)
        ;
            ShortHand = bi_implication(LHS, RHS),
            add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, LHS,
                !DepArcs),
            add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, RHS,
                !DepArcs)
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_goals(dependency_graph(T)::in,
    what_dependency_edges::in, digraph_key(T)::in, list(hlds_goal)::in,
    dep_arcs(T)::in, dep_arcs(T)::out) is det <= dependency_node(T).

add_dependency_arcs_in_goals(_DepGraph, _WhatEdges, _Caller, [], !DepArcs).
add_dependency_arcs_in_goals(DepGraph, WhatEdges, Caller, [Goal | Goals],
        !DepArcs) :-
    add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Goal, !DepArcs),
    add_dependency_arcs_in_goals(DepGraph, WhatEdges, Caller, Goals, !DepArcs).

:- pred add_dependency_arcs_in_cases(dependency_graph(T)::in,
    what_dependency_edges::in, digraph_key(T)::in, list(case)::in,
    dep_arcs(T)::in, dep_arcs(T)::out) is det <= dependency_node(T).

add_dependency_arcs_in_cases(_DepGraph, _WhatEdges, _Caller, [], !DepArcs).
add_dependency_arcs_in_cases(DepGraph, WhatEdges, Caller, [Case | Cases],
        !DepArcs) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    add_dependency_arcs_in_cons(DepGraph, WhatEdges, Caller,
        MainConsId, !DepArcs),
    list.foldl(add_dependency_arcs_in_cons(DepGraph, WhatEdges, Caller),
        OtherConsIds, !DepArcs),
    add_dependency_arcs_in_goal(DepGraph, WhatEdges, Caller, Goal, !DepArcs),
    add_dependency_arcs_in_cases(DepGraph, WhatEdges, Caller, Cases, !DepArcs).

%---------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_cons(dependency_graph(T)::in,
    what_dependency_edges::in, digraph_key(T)::in, cons_id::in,
    dep_arcs(T)::in, dep_arcs(T)::out) is det <= dependency_node(T).

add_dependency_arcs_in_cons(DepGraph, WhatEdges, Caller, ConsId, !DepArcs) :-
    (
        ConsId = closure_cons(ShroudedPredProcId),
        PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        maybe_add_dependency_arc(DepGraph, WhatEdges, edge_unify,
            Caller, PredProcId, !DepArcs)
    ;
        ( ConsId = du_data_ctor(_)
        ; ConsId = tuple_cons(_)
        ; ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ).

%---------------------------------------------------------------------------%

:- type edge_kind
    --->    edge_non_tail_call
    ;       edge_tail_call
    ;       edge_unify.

:- pred maybe_add_dependency_arc(dependency_graph(T)::in,
    what_dependency_edges::in, edge_kind::in,
    digraph_key(T)::in, pred_proc_id::in,
    dep_arcs(T)::in, dep_arcs(T)::out) is det <= dependency_node(T).

maybe_add_dependency_arc(DepGraph, WhatEdges, EdgeKind, Caller, PredProcId,
        !DepArcs) :-
    % If the callee isn't in the graph, then we didn't create a node for it.
    % If we didn't create a node for it, then we are not interested in calls
    % to it.
    ( if
        digraph.search_key(DepGraph, dependency_node(PredProcId), Callee),
        require_complete_switch [WhatEdges]
        (
            WhatEdges = only_tail_calls,
            EdgeKind = edge_tail_call
        ;
            WhatEdges = only_all_calls,
            ( EdgeKind = edge_tail_call
            ; EdgeKind = edge_non_tail_call
            )
        ;
            WhatEdges = all_calls_and_unifies
        )
    then
        !:DepArcs = [Caller - Callee | !.DepArcs]
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type scc_id == int.

    % An SCC cannot be merged into its parents if one of its procedures
    % is called as an aggregate query.
    %
    % XXX This predicate is not called from anywhere. Maybe it should be;
    % maybe not.
    %
:- pred handle_higher_order_args(list(prog_var)::in, bool::in, scc_id::in,
    multi_map(prog_var, pred_proc_id)::in, map(pred_proc_id, scc_id)::in,
    digraph(scc_id)::in, digraph(scc_id)::out,
    set(scc_id)::in, set(scc_id)::out) is det.
:- pragma consider_used(pred(handle_higher_order_args/9)).

handle_higher_order_args([], _, _, _, _, !SCCRel, !NoMerge).
handle_higher_order_args([Arg | Args], IsAgg, SCCid, Map, PredSCC,
        !SCCGraph, !NoMerge) :-
    ( if multi_map.search(Map, Arg, PredProcIds) then
        list.foldl2(handle_higher_order_arg(PredSCC, IsAgg, SCCid),
            PredProcIds, !SCCGraph, !NoMerge)
    else
        true
    ),
    handle_higher_order_args(Args, IsAgg, SCCid, Map, PredSCC,
        !SCCGraph, !NoMerge).

:- pred handle_higher_order_arg(map(pred_proc_id, scc_id)::in, bool::in,
    scc_id::in, pred_proc_id::in,
    digraph(scc_id)::in, digraph(scc_id)::out,
    set(scc_id)::in, set(scc_id)::out) is det.

handle_higher_order_arg(PredSCC, IsAgg, SCCid, PredProcId,
        !SCCGraph, !NoMerge) :-
    ( if map.search(PredSCC, PredProcId, CalledSCCid) then
        % Make sure anything called through an aggregate
        % is not merged into the current sub-module.
        (
            IsAgg = yes,
            set.insert(CalledSCCid, !NoMerge)
        ;
            IsAgg = no
        ),
        ( if CalledSCCid = SCCid then
            true
        else
            digraph.add_vertices_and_edge(SCCid, CalledSCCid, !SCCGraph)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

get_bottom_up_sccs_with_entry_points(ModuleInfo, DepInfo,
        BottomUpSCCsEntryPoints) :-
    DepGraph = dependency_info_get_graph(DepInfo),
    BottomUpSCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.reverse(BottomUpSCCs, TopDownSCCs),
    find_scc_entry_points(ModuleInfo, DepGraph, TopDownSCCs, set.init,
        TopDownSCCsEntryPoints),
    list.reverse(TopDownSCCsEntryPoints, BottomUpSCCsEntryPoints).

:- pred find_scc_entry_points(module_info::in,
    dependency_graph(pred_proc_id)::in, list(scc)::in,
    set(pred_proc_id)::in, list(scc_with_entry_points)::out) is det.

find_scc_entry_points(_, _, [], _, []).
find_scc_entry_points(ModuleInfo, DepGraph, [SCC | SCCs],
        !.CalledFromHigherSCC, [SCCEntryPoints | SCCsEntryPoints]) :-
    set.intersect(!.CalledFromHigherSCC, SCC, SCCProcsCalledFromHigherSCCs),
    set.filter(proc_is_exported(ModuleInfo), SCC, ExportedSCCProcs),
    SCCEntryPoints = scc_with_entry_points(SCC,
        SCCProcsCalledFromHigherSCCs, ExportedSCCProcs),

    % The set of procedures called from SCCs at or above this SCC is
    % the set of procedures called from SCCs above this SCC, plus
    % the set of procedures called from this SCC.
    set.map(find_callee_keys(DepGraph), SCC, CalleeKeySets),
    CalleeKeys = set.power_union(CalleeKeySets),
    set.map(lookup_vertex(DepGraph), CalleeKeys, Callees),
    set.union(Callees, !CalledFromHigherSCC),

    % When we process the lower SCCs, we won't care whether the procedures
    % of *this* SCC get called or not. Deleting them should reduce the
    % growth of CalledFromHigherSCC; for many modules, its size should remain
    % roughly constant, instead of growing linearly in the number of SCCs
    % processed so far. This is good, because the cost of the operations
    % on CalledFromHigherSCC would then remain roughly constant as well.
    set.difference(!.CalledFromHigherSCC, SCC, !:CalledFromHigherSCC),

    find_scc_entry_points(ModuleInfo, DepGraph, SCCs,
        !.CalledFromHigherSCC, SCCsEntryPoints).

:- pred find_callee_keys(dependency_graph(pred_proc_id)::in, pred_proc_id::in,
    set(dependency_graph_key(pred_proc_id))::out) is det.

find_callee_keys(DepGraph, ParentId, ChildKeys) :-
    digraph.lookup_key(DepGraph, ParentId, ParentKey),
    digraph.lookup_from(DepGraph, ParentKey, ChildKeys).

:- pred proc_is_exported(module_info::in, pred_proc_id::in) is semidet.

proc_is_exported(ModuleInfo, PredProcId) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    procedure_is_exported(ModuleInfo, PredInfo, ProcId).

%---------------------------------------------------------------------------%

:- pred append_sccs(module_info::in,
    int::in, list(list(pred_proc_id))::in,
    string.builder.state::di, string.builder.state::uo) is det.
:- pragma consider_used(pred(append_sccs/5)).

append_sccs(_ModuleInfo, _CurSCCNum, [], !SB) :-
    append_string("\n", !SB).
append_sccs(ModuleInfo, CurSCCNum, [SCC | SCCs], !SB) :-
    string.builder.format("%% SCC %d\n", [i(CurSCCNum)], !SB),
    append_scc(ModuleInfo, SCC, !SB),
    append_sccs(ModuleInfo, CurSCCNum + 1, SCCs, !SB).

:- pred append_scc(module_info::in, list(pred_proc_id)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_scc(_ModuleInfo, [], !SB).
append_scc(ModuleInfo, [PredProcId | PredProcIds], !SB) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    Name = pred_info_name(PredInfo),
    proc_info_get_declared_determinism(ProcInfo, Det),
    proc_info_get_argmodes(ProcInfo, Modes),
    varset.init(ModeVarSet),

    MaybeWithInst = maybe.no,
    append_string("% ", !SB),
    mercury_format_pred_or_func_mode_subdecl(output_mercury, ModeVarSet,
        unqualified(Name), Modes, MaybeWithInst, Det,
        string.builder.handle, !SB),
    append_string("\n", !SB),
    append_scc(ModuleInfo, PredProcIds, !SB).

%---------------------------------------------------------------------------%

dependency_graph_to_string(DepGraphStr, !ModuleInfo) :-
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    some [!SB] (
        !:SB = string.builder.init,
        append_string("% Dependency graph\n", !SB),
        append_string("\n\n% Dependency ordering\n", !SB),
        digraph.traverse(dependency_info_get_graph(DepInfo),
            append_empty_node,
            append_dep_graph_link(!.ModuleInfo),
            !SB),
        DepGraphStr = string.builder.to_string(!.SB)
    ).

prof_dependency_graph_to_string(DepGraphStr, !ModuleInfo) :-
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    some [!SB] (
        !:SB = string.builder.init,
        digraph.traverse(dependency_info_get_graph(DepInfo),
            append_empty_node,
            append_prof_dep_graph_link(!.ModuleInfo),
            !SB),
        DepGraphStr = string.builder.to_string(!.SB)
    ).

%---------------------------------------------------------------------------%

:- pred append_empty_node(pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_empty_node(_, !SB).

%---------------------------------------------------------------------------%

:- pred append_prof_dep_graph_link(module_info::in,
    pred_proc_id::in, pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_prof_dep_graph_link(ModuleInfo, Parent, Child, !SB) :-
    Parent = proc(PPredId, PProcId),    % Caller
    Child = proc(CPredId, CProcId),     % Callee
    append_label_dependency(ModuleInfo, PPredId, PProcId, !SB),
    append_string("\t", !SB),
    append_label_dependency(ModuleInfo, CPredId, CProcId, !SB),
    append_string("\n", !SB).

    % Print out the label corresponding to the given pred_id and proc_id.
    %
:- pred append_label_dependency(module_info::in, pred_id::in, proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_label_dependency(ModuleInfo, PredId, ProcId, !SB) :-
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    append_string(proc_label_to_c_string(add_label_prefix, ProcLabel), !SB).

:- pred append_dep_graph_link(module_info::in,
    pred_proc_id::in, pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_dep_graph_link(ModuleInfo, Parent, Child, !SB) :-
    Parent = proc(PPredId, PProcId),    % Caller
    Child = proc(CPredId, CProcId),     % Callee
    module_info_pred_proc_info(ModuleInfo, PPredId, PProcId,
        PPredInfo, PProcInfo),
    module_info_pred_proc_info(ModuleInfo, CPredId, CProcId,
        CPredInfo, CProcInfo),
    PName = pred_info_name(PPredInfo),
    proc_info_get_declared_determinism(PProcInfo, PDet),
    proc_info_get_argmodes(PProcInfo, PModes),
    CName = pred_info_name(CPredInfo),
    proc_info_get_declared_determinism(CProcInfo, CDet),
    proc_info_get_argmodes(CProcInfo, CModes),
    varset.init(ModeVarSet),
    MaybeWithInst = maybe.no,
    mercury_format_pred_or_func_mode_subdecl(output_mercury, ModeVarSet,
        unqualified(PName), PModes, MaybeWithInst, PDet,
        string.builder.handle, !SB),
    append_string(" -> ", !SB),
    mercury_format_pred_or_func_mode_subdecl(output_mercury, ModeVarSet,
        unqualified(CName), CModes, MaybeWithInst, CDet,
        string.builder.handle, !SB),
    append_string("\n", !SB).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_dependency_graph.
%---------------------------------------------------------------------------%
