%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dependency_graph.m.
% Main authors: bromage, conway, stayl.
%
% The dependency_graph records which procedures depend on which other
% procedures. It is defined as a relation (see hlds_module.m) R where xRy
% means that the definition of x depends on the definition of y.
% Note that imported procedures are not included in the dependency_graph
% (although opt_imported procedures are included).
%
% The other important structure is the dependency_ordering which is
% a list of the cliques (strongly-connected components) of this relation,
% in topological order. This is very handy for doing fixpoint iterations.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.dependency_graph.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Ensure that the module_info contains a version of the dependency_info
    % which only contains arcs between procedures for which there are clauses
    % defined (everything that is not imported, plus opt_imported). There is
    % no guarantee that the dependency_info is current.
    %
:- pred module_info_ensure_dependency_info(module_info::in, module_info::out)
    is det.

    % Ensure that the module_info contains a version of the dependency_info
    % which only contains arcs between procedures for which there are clauses
    % defined (everything that is not imported, plus opt_imported). The
    % dependency_info will be up-to-date.
    %
:- pred module_info_rebuild_dependency_info(module_info::in, module_info::out,
    dependency_info(pred_proc_id)::out) is det.

:- type include_imported
    --->    include_imported
    ;       do_not_include_imported.

    % Build the dependency graph of procedures.
    %
:- pred build_pred_dependency_graph(module_info::in, include_imported::in,
    dependency_info(pred_id)::out) is det.

    % Build the dependency graph of predicates.
    %
:- pred build_proc_dependency_graph(module_info::in, include_imported::in,
    dependency_info(pred_proc_id)::out) is det.

    % Output a form of the static call graph to a file, in a format suitable
    % for use in .dependency_info files. After the heading, the format of
    % each line is
    %
    %   CallerModeDecl \t CalleeModeDecl
    %
:- pred write_dependency_graph(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Output a form of the static call graph to a file for use by the profiler.
    % There is no heading, and the format of each line is
    %
    %   CallerLabel \t CalleeLabel
    %
:- pred write_prof_dependency_graph(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Given the list of predicates in a strongly connected component
    % of the dependency graph, a list of the higher SCCs in the module
    % and a module_info, find out which members of the SCC can be
    % called from outside the SCC.
    %
:- pred get_scc_entry_points(list(pred_proc_id)::in, dependency_ordering::in,
    module_info::in, list(pred_proc_id)::out) is det.

    % write_graph(Graph, WriteNode, WriteEdge):
    %
    % Write out the dependency graph using WriteNode to decide what to output
    % for a node in the dependency graph and WriteEdge for an edge.
    %
:- pred write_graph(dependency_info::in,
    pred(pred_proc_id, io, io)::pred(in, di, uo) is det,
    pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
    io::di, io::uo) is det.

    % write_graph_nodes(Nodes, Graph, WriteNode, WriteEdge)
    %
    % Write out each of the Nodes in the Graph using WriteNode and
    % any edges originating in Nodes, using WriteEdge.
    %
:- pred write_graph_nodes(list(pred_proc_id)::in, dependency_graph::in,
    pred(pred_proc_id, io, io)::pred(in, di, uo) is det,
    pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module eqvclass.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module relation.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

module_info_ensure_dependency_info(!ModuleInfo) :-
    module_info_get_maybe_dependency_info(!.ModuleInfo, MaybeDepInfo),
    (
        MaybeDepInfo = yes(_)
    ;
        MaybeDepInfo = no,
        build_dependency_graph(!.ModuleInfo, do_not_include_imported, DepInfo),
        module_info_set_dependency_info(DepInfo, !ModuleInfo)
    ).

module_info_rebuild_dependency_info(!ModuleInfo, DepInfo) :-
    build_dependency_graph(!.ModuleInfo, do_not_include_imported, DepInfo),
    module_info_set_dependency_info(DepInfo, !ModuleInfo).

build_proc_dependency_graph(ModuleInfo, Imported, DepInfo) :-
    build_dependency_graph(ModuleInfo, Imported, DepInfo).

build_pred_dependency_graph(ModuleInfo, Imported, DepInfo) :-
    build_dependency_graph(ModuleInfo, Imported, DepInfo).

    % Traverse the module structure, calling `add_dependency_arcs'
    % for each procedure body.
    %
:- pred build_dependency_graph(module_info::in, include_imported::in,
    dependency_info(T)::out) is det <= dependency_node(T).

build_dependency_graph(ModuleInfo, Imported, !:DepInfo) :-
    module_info_predids(ModuleInfo, PredIds),
    relation.init(DepGraph0),
    add_dependency_nodes(PredIds, ModuleInfo, Imported, DepGraph0, DepGraph1),
    add_dependency_arcs(PredIds, ModuleInfo, Imported, DepGraph1, DepGraph),
    hlds_dependency_info_init(!:DepInfo),
    hlds_dependency_info_set_dependency_graph(DepGraph, !DepInfo),
    relation.atsort(DepGraph, DepOrd0),
    sets_to_lists(DepOrd0, [], DepOrd),
    hlds_dependency_info_set_dependency_ordering(DepOrd, !DepInfo).

:- pred sets_to_lists(list(set(T))::in, list(list(T))::in,
    list(list(T))::out) is det.

sets_to_lists([], Xs, Xs).
sets_to_lists([X | Xs], Ys, Zs) :-
    set.to_sorted_list(X, Y),
    sets_to_lists(Xs, [Y | Ys], Zs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- typeclass dependency_node(T) where [
    pred add_dependency_nodes(list(pred_id)::in, module_info::in,
        include_imported::in,
        dependency_graph(T)::in, dependency_graph(T)::out) is det,

    pred add_dependency_arcs(list(pred_id)::in, module_info::in,
        include_imported::in,
        dependency_graph(T)::in, dependency_graph(T)::out) is det,

    func dependency_node(pred_proc_id) = T
].

:- instance dependency_node(pred_proc_id) where [
    pred(add_dependency_nodes/5) is add_pred_proc_nodes,
    pred(add_dependency_arcs/5) is add_pred_proc_arcs,
    func(dependency_node/1) is id
].

:- instance dependency_node(pred_id) where [
    pred(add_dependency_nodes/5) is add_pred_nodes,
    pred(add_dependency_arcs/5) is add_pred_arcs,
    func(dependency_node/1) is pred_proc_id_get_pred_id
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_pred_proc_nodes(list(pred_id)::in, module_info::in,
    include_imported::in, dependency_graph::in, dependency_graph::out) is det.

add_pred_proc_nodes([], _ModuleInfo, _, !DepGraph).
add_pred_proc_nodes([PredId | PredIds], ModuleInfo, Imported, !DepGraph) :-
    module_info_preds(ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    (
        % Don't bother adding nodes (or arcs) for procedures which are imported
        % (i.e. which we don't have any `clauses' for).
        Imported = do_not_include_imported,
        ProcIds = pred_info_non_imported_procids(PredInfo)
    ;
        Imported = include_imported,
        ProcIds = pred_info_procids(PredInfo)
    ),
    add_proc_nodes(ProcIds, PredId, ModuleInfo, !DepGraph),
    add_pred_proc_nodes(PredIds, ModuleInfo, Imported, !DepGraph).

:- pred add_proc_nodes(list(proc_id)::in, pred_id::in,
    module_info::in, dependency_graph::in, dependency_graph::out) is det.

add_proc_nodes([], _PredId, _ModuleInfo, !DepGraph).
add_proc_nodes([ProcId | ProcIds], PredId, ModuleInfo, !DepGraph) :-
    relation.add_element(!.DepGraph, proc(PredId, ProcId), _, !:DepGraph),
    add_proc_nodes(ProcIds, PredId, ModuleInfo, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred add_pred_nodes(list(pred_id)::in, module_info::in,
    include_imported::in,
    dependency_graph(pred_id)::in, dependency_graph(pred_id)::out) is det.

add_pred_nodes([], _ModuleInfo, _, DepGraph, DepGraph).
add_pred_nodes([PredId | PredIds], ModuleInfo, IncludeImported, !DepGraph) :-
    module_info_preds(ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    % Don't bother adding nodes (or arcs) for predicates
    % which are imported (i.e. which we don't have any `clauses' for).
    (
        IncludeImported = do_not_include_imported,
        pred_info_is_imported(PredInfo)
    ->
        true
    ;
        relation.add_element(!.DepGraph, PredId, _, !:DepGraph)
    ),
    add_pred_nodes(PredIds, ModuleInfo, IncludeImported, !DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_pred_proc_arcs(list(pred_id)::in, module_info::in,
    include_imported::in, dependency_graph::in, dependency_graph::out) is det.

add_pred_proc_arcs([], _ModuleInfo, _, !DepGraph).
add_pred_proc_arcs([PredId | PredIds], ModuleInfo, Imported, !DepGraph) :-
    module_info_preds(ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    (
        % Don't bother adding nodes (or arcs) for procedures which are imported
        % (i.e. which we don't have any `clauses' for).
        Imported = do_not_include_imported,
        ProcIds = pred_info_non_imported_procids(PredInfo)
    ;
        Imported = include_imported,
        ProcIds = pred_info_procids(PredInfo)
    ),
    add_proc_arcs(ProcIds, PredId, ModuleInfo, Imported, !DepGraph),
    add_pred_proc_arcs(PredIds, ModuleInfo, Imported, !DepGraph).

:- pred add_proc_arcs(list(proc_id)::in, pred_id::in, module_info::in,
    include_imported::in, dependency_graph::in, dependency_graph::out) is det.

add_proc_arcs([], _PredId, _ModuleInfo, _, !DepGraph).
add_proc_arcs([ProcId | ProcIds], PredId, ModuleInfo, IncludeImported,
        !DepGraph) :-
    module_info_preds(ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    (
        IncludeImported = do_not_include_imported,
        proc_info_get_goal(ProcInfo0, Goal),

        relation.lookup_element(!.DepGraph, proc(PredId, ProcId), Caller),
        add_dependency_arcs_in_goal(Goal, Caller, !DepGraph)
    ;
        IncludeImported = include_imported,
        pred_info_get_import_status(PredInfo0, ImportStatus),
        Imported = status_is_imported(ImportStatus),
        (
            Imported = yes
        ;
            Imported = no,
            proc_info_get_goal(ProcInfo0, Goal),
            relation.lookup_element(!.DepGraph, proc(PredId, ProcId), Caller),
            add_dependency_arcs_in_goal(Goal, Caller, !DepGraph)
        )
    ),
    add_proc_arcs(ProcIds, PredId, ModuleInfo, IncludeImported, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred add_pred_arcs(list(pred_id)::in, module_info::in, include_imported::in,
    dependency_graph(pred_id)::in, dependency_graph(pred_id)::out) is det.

add_pred_arcs([], _ModuleInfo, _, !DepGraph).
add_pred_arcs([PredId | PredIds], ModuleInfo, IncludeImported, !DepGraph) :-
    module_info_preds(ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    (
        IncludeImported = do_not_include_imported,
        pred_info_is_imported(PredInfo)
    ->
        true
    ;
        pred_info_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_clauses_rep(ClausesInfo, ClausesRep),
        get_clause_list_any_order(ClausesRep, Clauses),
        Goals = list.map(func(clause(_, Goal, _, _)) = Goal, Clauses),
        relation.lookup_element(!.DepGraph, PredId, Caller),
        add_dependency_arcs_in_list(Goals, Caller, !DepGraph)
    ),
    add_pred_arcs(PredIds, ModuleInfo, IncludeImported, !DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func pred_proc_id_get_pred_id(pred_proc_id) = pred_id.

pred_proc_id_get_pred_id(proc(PredId, _)) = PredId.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_goal(hlds_goal::in, relation_key::in,
    dependency_graph(T)::in, dependency_graph(T)::out) is det
    <= dependency_node(T).

add_dependency_arcs_in_goal(GoalExpr - _GoalInfo, Caller, !DepGraph) :-
    (
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        add_dependency_arcs_in_list(Goals, Caller, !DepGraph)
    ;
        GoalExpr = switch(_Var, _Det, Cases),
        add_dependency_arcs_in_cases(Cases, Caller, !DepGraph)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        add_dependency_arcs_in_goal(Cond, Caller, !DepGraph),
        add_dependency_arcs_in_goal(Then, Caller, !DepGraph),
        add_dependency_arcs_in_goal(Else, Caller, !DepGraph)
    ;
        ( GoalExpr = negation(Goal)
        ; GoalExpr = scope(_, Goal)
        ),
        add_dependency_arcs_in_goal(Goal, Caller, !DepGraph)
    ;
        GoalExpr = generic_call(_, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, Builtin, _, _),
        ( Builtin = inline_builtin ->
            true
        ;
            (
                % If the node isn't in the relation, then we didn't insert it
                % because is was imported, and we don't consider it.
                relation.search_element(!.DepGraph,
                    dependency_node(proc(PredId, ProcId)), Callee)
            ->
                relation.add(!.DepGraph, Caller, Callee, !:DepGraph)
            ;
                true
            )
        )
    ;
        GoalExpr = unify(_,_,_,Unify,_),
        (
            Unify = assign(_, _)
        ;
            Unify = simple_test(_, _)
        ;
            Unify = construct(_, Cons, _, _, _, _, _),
            add_dependency_arcs_in_cons(Cons, Caller, !DepGraph)
        ;
            Unify = deconstruct(_, Cons, _, _, _, _),
            add_dependency_arcs_in_cons(Cons, Caller, !DepGraph)
        ;
            Unify = complicated_unify(_, _, _)
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = shorthand(ShorthandGoal),
        ShorthandGoal = bi_implication(LHS, RHS),
        add_dependency_arcs_in_list([LHS, RHS], Caller, !DepGraph)
    ).

%-----------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_list(list(hlds_goal)::in,
    relation_key::in, dependency_graph(T)::in, dependency_graph(T)::out) is det
    <= dependency_node(T).

add_dependency_arcs_in_list([], _Caller, !DepGraph).
add_dependency_arcs_in_list([Goal|Goals], Caller, !DepGraph) :-
    add_dependency_arcs_in_goal(Goal, Caller, !DepGraph),
    add_dependency_arcs_in_list(Goals, Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_cases(list(case)::in, relation_key::in,
    dependency_graph(T)::in, dependency_graph(T)::out) is det
    <= dependency_node(T).

add_dependency_arcs_in_cases([], _Caller, !DepGraph).
add_dependency_arcs_in_cases([case(Cons, Goal) | Goals], Caller, !DepGraph) :-
    add_dependency_arcs_in_cons(Cons, Caller, !DepGraph),
    add_dependency_arcs_in_goal(Goal, Caller, !DepGraph),
    add_dependency_arcs_in_cases(Goals, Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred add_dependency_arcs_in_cons(cons_id::in, relation_key::in,
    dependency_graph(T)::in, dependency_graph(T)::out) is det
    <= dependency_node(T).

add_dependency_arcs_in_cons(cons(_, _), _Caller, !DepGraph).
add_dependency_arcs_in_cons(int_const(_), _Caller, !DepGraph).
add_dependency_arcs_in_cons(string_const(_), _Caller, !DepGraph).
add_dependency_arcs_in_cons(float_const(_), _Caller, !DepGraph).
add_dependency_arcs_in_cons(pred_const(ShroudedPredProcId, _), Caller,
        !DepGraph) :-
    PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
    (
        % If the node isn't in the relation, then we didn't insert it
        % because it was imported, and we don't consider it.
        relation.search_element(!.DepGraph, dependency_node(PredProcId),
            Callee)
    ->
        relation.add(!.DepGraph, Caller, Callee, !:DepGraph)
    ;
        true
    ).
add_dependency_arcs_in_cons(type_ctor_info_const(_, _, _), _, !DepGraph).
add_dependency_arcs_in_cons(base_typeclass_info_const(_, _, _, _), _,
    !DepGraph).
add_dependency_arcs_in_cons(type_info_cell_constructor(_), _, !DepGraph).
add_dependency_arcs_in_cons(typeclass_info_cell_constructor, _,
    !DepGraph).
add_dependency_arcs_in_cons(tabling_info_const(_), _Caller, !DepGraph).
add_dependency_arcs_in_cons(deep_profiling_proc_layout(_), _, !DepGraph).
add_dependency_arcs_in_cons(table_io_decl(_), _Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred write_dependency_ordering( list(list(pred_proc_id))::in,
    module_info::in, int::in, io::di, io::uo) is det.

write_dependency_ordering([], _ModuleInfo, _N, !IO) :-
    io.write_string("\n", !IO).
write_dependency_ordering([Clique | Rest], ModuleInfo, N, !IO) :-
    io.write_string("% Clique ", !IO),
    io.write_int(N, !IO),
    io.write_string("\n", !IO),
    write_clique(Clique, ModuleInfo, !IO),
    N1 = N + 1,
    write_dependency_ordering(Rest, ModuleInfo, N1, !IO).

:- pred write_clique(list(pred_proc_id)::in, module_info::in, io::di, io::uo)
    is det.

write_clique([], _ModuleInfo, !IO).
write_clique([proc(PredId, ProcId) | Rest], ModuleInfo, !IO) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    Name = pred_info_name(PredInfo),
    proc_info_get_declared_determinism(ProcInfo, Det),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_context(ProcInfo, Context),
    varset.init(ModeVarSet),

    io.write_string("% ", !IO),
    mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(Name),
        Modes, Det, Context, !IO),
    io.write_string("\n", !IO),
    write_clique(Rest, ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

write_prof_dependency_graph(!ModuleInfo, !IO) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    write_graph(DepInfo, write_empty_node,
        write_prof_dep_graph_link(!.ModuleInfo), !IO).

write_dependency_graph(!ModuleInfo, !IO) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    io.write_string("% Dependency graph\n", !IO),
    io.write_string("\n\n% Dependency ordering\n", !IO),
    write_graph(DepInfo, write_empty_node,
        write_dep_graph_link(!.ModuleInfo), !IO).

:- pred write_empty_node(pred_proc_id::in, io::di, io::uo) is det.

write_empty_node(_, !IO).

:- pred write_prof_dep_graph_link(module_info::in,
    pred_proc_id::in, pred_proc_id::in, io::di, io::uo) is det.

write_prof_dep_graph_link(ModuleInfo, Parent, Child, !IO) :-
    Parent = proc(PPredId, PProcId),    % Caller
    Child = proc(CPredId, CProcId),     % Callee
    output_label_dependency(ModuleInfo, PPredId, PProcId, !IO),
    io.write_string("\t", !IO),
    output_label_dependency(ModuleInfo, CPredId, CProcId, !IO),
    io.write_string("\n", !IO).

:- pred write_dep_graph_link(module_info::in,
    pred_proc_id::in, pred_proc_id::in, io::di, io::uo) is det.

write_dep_graph_link(ModuleInfo, Parent, Child, !IO) :-
    Parent = proc(PPredId, PProcId),    % Caller
    Child = proc(CPredId, CProcId),     % Callee
    module_info_pred_proc_info(ModuleInfo, PPredId, PProcId,
        PPredInfo, PProcInfo),
    module_info_pred_proc_info(ModuleInfo, CPredId, CProcId,
        CPredInfo, CProcInfo),
    PName = pred_info_name(PPredInfo),
    proc_info_get_declared_determinism(PProcInfo, PDet),
    proc_info_get_argmodes(PProcInfo, PModes),
    proc_info_get_context(PProcInfo, PContext),
    CName = pred_info_name(CPredInfo),
    proc_info_get_declared_determinism(CProcInfo, CDet),
    proc_info_get_argmodes(CProcInfo, CModes),
    proc_info_get_context(CProcInfo, CContext),
    varset.init(ModeVarSet),
    mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(PName),
        PModes, PDet, PContext, !IO),
    io.write_string(" -> ", !IO),
    mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(CName),
        CModes, CDet, CContext, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

write_graph(DepInfo, WriteNode, WriteLink, !IO) :-
    hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),
    relation.domain(DepGraph, DomSet),
    set.to_sorted_list(DomSet, DomList),
    write_graph_nodes(DomList, DepGraph, WriteNode, WriteLink, !IO).

write_graph_nodes([], _Graph, _WriteNode, _WriteLink, !IO).
write_graph_nodes([Node | Nodes], Graph, WriteNode, WriteLink, !IO) :-
    WriteNode(Node, !IO),
    relation.lookup_element(Graph, Node, NodeKey),
    relation.lookup_from(Graph, NodeKey, ChildrenSet),
    set.to_sorted_list(ChildrenSet, Children),
    write_graph_children(Children, Node, Graph, WriteLink, !IO),
    write_graph_nodes(Nodes, Graph, WriteNode, WriteLink, !IO).

:- pred write_graph_children(list(relation_key)::in, pred_proc_id::in,
    dependency_graph::in,
    pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
    io::di, io::uo) is det.

write_graph_children([], _Parent, _Graph, _WriteLink, !IO).
write_graph_children([ChildKey | Children], Parent, Graph, WriteLink, !IO) :-
    relation.lookup_key(Graph, ChildKey, Child),
    WriteLink(Parent, Child, !IO),
    write_graph_children(Children, Parent, Graph, WriteLink, !IO).

%-----------------------------------------------------------------------------%

    % Print out the label corresponding to the given pred_id and proc_id.
    %
:- pred output_label_dependency(module_info::in, pred_id::in, proc_id::in,
    io::di, io::uo) is det.

output_label_dependency(ModuleInfo, PredId, ProcId, !IO) :-
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    output_proc_label(ProcLabel, !IO).

%-----------------------------------------------------------------------------%

get_scc_entry_points(SCC, HigherSCCs, ModuleInfo, EntryPoints) :-
    list.filter(is_entry_point(HigherSCCs, ModuleInfo), SCC, EntryPoints).

:- pred is_entry_point(list(list(pred_proc_id))::in, module_info::in,
    pred_proc_id::in) is semidet.

is_entry_point(HigherSCCs, ModuleInfo, PredProcId) :-
    (
        % Is the predicate exported?
        PredProcId = proc(PredId, _ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_is_exported(PredInfo)
    ;
        % Is the predicate called from a higher SCC?
        module_info_dependency_info(ModuleInfo, DepInfo),
        hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),

        relation.lookup_element(DepGraph, PredProcId, PredProcIdKey),
        relation.lookup_to(DepGraph, PredProcIdKey, CallingKeys),
        set.member(CallingKey, CallingKeys),
        relation.lookup_key(DepGraph, CallingKey, CallingPred),
        list.member(HigherSCC, HigherSCCs),
        list.member(CallingPred, HigherSCC)
    ).

%-----------------------------------------------------------------------------%

    % Find the SCCs called from a given SCC.
    %
:- pred get_called_scc_ids(scc_id::in, relation(scc_id)::in, set(scc_id)::out)
    is det.

get_called_scc_ids(SCCid, SCCRel, CalledSCCSet) :-
    relation.lookup_element(SCCRel, SCCid, SCCidKey),
    relation.lookup_from(SCCRel, SCCidKey, CalledSCCKeys),
    set.to_sorted_list(CalledSCCKeys, CalledSCCKeyList),
    list.map(relation.lookup_key(SCCRel), CalledSCCKeyList, CalledSCCs),
    set.list_to_set(CalledSCCs, CalledSCCSet).

%-----------------------------------------------------------------------------%

    % For each SCC, a list of all preds in SCC, and a list
    % of entry-points of the SCC.
:- type scc_pred_map == map(scc_id, pair(list(pred_proc_id))).

:- type scc_id == int.

:- type scc == list(pred_proc_id).

    % An SCC cannot be merged into its parents if one of its procedures
    % is called as an aggregate query.
    %
:- pred handle_higher_order_args(list(prog_var)::in, bool::in, scc_id::in,
    multi_map(prog_var, pred_proc_id)::in, map(pred_proc_id, scc_id)::in,
    relation(scc_id)::in, relation(scc_id)::out,
    set(scc_id)::in, set(scc_id)::out) is det.

handle_higher_order_args([], _, _, _, _, !SCCRel, !NoMerge).
handle_higher_order_args([Arg | Args], IsAgg, SCCid, Map, PredSCC,
        !SCCRel, !NoMerge) :-
    ( multi_map.search(Map, Arg, PredProcIds) ->
        list.foldl2(handle_higher_order_arg(PredSCC, IsAgg, SCCid),
            PredProcIds, !SCCRel, !NoMerge)
    ;
        true
    ),
    handle_higher_order_args(Args, IsAgg, SCCid, Map, PredSCC,
        !SCCRel, !NoMerge).

:- pred handle_higher_order_arg(map(pred_proc_id, scc_id)::in, bool::in,
    scc_id::in, pred_proc_id::in,
    relation(scc_id)::in, relation(scc_id)::out,
    set(scc_id)::in, set(scc_id)::out) is det.

handle_higher_order_arg(PredSCC, IsAgg, SCCid, PredProcId,
        !SCCRel, !NoMerge) :-
    ( map.search(PredSCC, PredProcId, CalledSCCid) ->
        % Make sure anything called through an aggregate
        % is not merged into the current sub-module.
        (
            IsAgg = yes,
            set.insert(!.NoMerge, CalledSCCid, !:NoMerge)
        ;
            IsAgg = no
        ),
        ( CalledSCCid = SCCid ->
            true
        ;
            relation.add_values(!.SCCRel, SCCid, CalledSCCid, !:SCCRel)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "dependency_graph.m".

%-----------------------------------------------------------------------------%
:- end_module dependency_graph.
%-----------------------------------------------------------------------------%
