%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main authors: bromage, conway, stayl.

% The dependency_graph records which procedures depend on which other
% procedures. It is defined as a relation (see hlds_module.m) R where xRy
% means that the definition of x depends on the definition of y.
% Note that imported procedures are not included in the dependency_graph
% (although opt_imported procedures are included).
%
% The other important structure is the dependency_ordering which is
% a list of the cliques (strongly-connected components) of this relation,
% in topological order. This is very handy for doing fixpoint iterations.

%-----------------------------------------------------------------------------%

:- module transform_hlds__dependency_graph.

:- interface.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module bool.
:- import_module io.
:- import_module list.

	% Ensure that the module_info contains a version of the
	% dependency_info which only contains arcs between procedures
	% for which there are clauses defined (ie not imported except
	% for opt_imported).
	%
:- pred module_info_ensure_dependency_info(module_info::in, module_info::out)
	is det.

	% Build the dependency graph, if the bool is yes then
	% imported procedures are included in the dependency graph,
	% otherwise they aren't.
	%
:- pred dependency_graph__build_pred_dependency_graph(module_info::in,
	bool::in, dependency_info(pred_id)::out) is det.

	% Output a form of the static call graph to a file, in a format
	% suitable for use in .dependency_info files.
:- pred dependency_graph__write_dependency_graph(module_info::in,
	module_info::out, io::di, io::uo) is det.

	% Output a form of the static call graph to a file for use by the
	% profiler.
:- pred dependency_graph__write_prof_dependency_graph(module_info::in,
	module_info::out, io::di, io::uo) is det.

	% Given the list of predicates in a strongly connected component
	% of the dependency graph, a list of the higher SCCs in the module
	% and a module_info, find out which members of the SCC can be
	% called from outside the SCC.
:- pred dependency_graph__get_scc_entry_points(list(pred_proc_id)::in,
	dependency_ordering::in, module_info::in, list(pred_proc_id)::out)
	is det.

	% Create the Aditi dependency ordering. This contains all the Aditi
	% SCCs in the original program. The difference is that SCCs which
	% are only called from one other SCC and are not called through
	% negation or aggregation are merged into the parent SCC. This makes
	% the low-level RL optimizations more effective while maintaining
	% stratification.
	% dead_proc_elim.m should be be run before this is called
	% to avoid missing some opportunities for merging where
	% a procedure is called from a dead procedure.
:- pred module_info_ensure_aditi_dependency_info(module_info::in,
	module_info::out) is det.

	% write_graph(Graph, WriteNode, WriteEdge)
	%
	% Write out the dependency graph using WriteNode to decide what
	% to output for a node in the dependency graph and WriteEdge for
	% an edge.
	%
:- pred dependency_graph__write_graph(dependency_info::in,
	pred(pred_proc_id, io, io)::pred(in, di, uo) is det,
	pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
	io::di, io::uo) is det.

	% write_graph_nodes(Nodes, Graph, WriteNode, WriteEdge)
	%
	% Write out each of the Nodes in the Graph using WriteNode and
	% any edges originating in Nodes, using WriteEdge.
	%
:- pred dependency_graph__write_graph_nodes(list(pred_proc_id)::in,
	dependency_graph::in,
	pred(pred_proc_id, io, io)::pred(in, di, uo) is det,
	pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module check_hlds__mode_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module eqvclass.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module relation.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module term.
:- import_module varset.
:- import_module varset.

%-----------------------------------------------------------------------------%

	% Ensure that the dependency graph has been built by building
	% it if necessary.

module_info_ensure_dependency_info(!ModuleInfo) :-
	module_info_get_maybe_dependency_info(!.ModuleInfo, MaybeDepInfo),
	( MaybeDepInfo = yes(_) ->
		true
	;
		dependency_graph__build_dependency_graph(!.ModuleInfo, no,
			DepInfo),
		module_info_set_dependency_info(DepInfo, !ModuleInfo)
	).

	% Traverse the module structure, calling `dependency_graph__add_arcs'
	% for each procedure body.

dependency_graph__build_pred_dependency_graph(ModuleInfo, Imported, DepInfo) :-
	dependency_graph__build_dependency_graph(ModuleInfo, Imported, DepInfo).

:- pred dependency_graph__build_dependency_graph(module_info::in, bool::in,
	dependency_info(T)::out) is det <= dependency_node(T).

dependency_graph__build_dependency_graph(ModuleInfo, Imported, DepInfo) :-
	module_info_predids(ModuleInfo, PredIds),
	relation__init(DepGraph0),
	dependency_graph__add_nodes(PredIds, ModuleInfo, Imported,
		DepGraph0, DepGraph1),
	dependency_graph__add_arcs(PredIds, ModuleInfo, Imported,
		DepGraph1, DepGraph),
	hlds_dependency_info_init(DepInfo0),
	hlds_dependency_info_set_dependency_graph(DepGraph, DepInfo0, DepInfo1),
	relation__atsort(DepGraph, DepOrd0),
	dependency_graph__sets_to_lists(DepOrd0, [], DepOrd),
	hlds_dependency_info_set_dependency_ordering(DepOrd, DepInfo1, DepInfo).

:- pred dependency_graph__sets_to_lists(list(set(T))::in, list(list(T))::in,
	list(list(T))::out) is det.

dependency_graph__sets_to_lists([], Xs, Xs).
dependency_graph__sets_to_lists([X | Xs], Ys, Zs) :-
	set__to_sorted_list(X, Y),
	dependency_graph__sets_to_lists(Xs, [Y | Ys], Zs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- typeclass dependency_node(T) where [
	pred dependency_graph__add_nodes(list(pred_id)::in, module_info::in,
		bool::in, dependency_graph(T)::in, dependency_graph(T)::out)
		is det,

	pred dependency_graph__add_arcs(list(pred_id)::in, module_info::in,
		bool::in, dependency_graph(T)::in, dependency_graph(T)::out)
		is det,

	func dependency_node(pred_proc_id) = T
].

:- instance dependency_node(pred_proc_id) where [
	pred(dependency_graph__add_nodes/5) is
		dependency_graph__add_pred_proc_nodes,
	pred(dependency_graph__add_arcs/5) is
		dependency_graph__add_pred_proc_arcs,
	func(dependency_node/1) is id
].

:- instance dependency_node(pred_id) where [
	pred(dependency_graph__add_nodes/5) is
		dependency_graph__add_pred_nodes,
	pred(dependency_graph__add_arcs/5) is
		dependency_graph__add_pred_arcs,
	func(dependency_node/1) is pred_proc_id_get_pred_id
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_proc_nodes(list(pred_id)::in,
	module_info::in, bool::in,
	dependency_graph::in, dependency_graph::out) is det.

dependency_graph__add_pred_proc_nodes([], _ModuleInfo, _, !DepGraph).
dependency_graph__add_pred_proc_nodes([PredId | PredIds], ModuleInfo, Imported,
		!DepGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	(
		% Don't bother adding nodes (or arcs) for procedures
		% which are imported (i.e. which we don't have any
		% `clauses' for).
		Imported = no,
		ProcIds = pred_info_non_imported_procids(PredInfo)
	;
		Imported = yes,
		ProcIds = pred_info_procids(PredInfo)
	),
	dependency_graph__add_proc_nodes(ProcIds, PredId, ModuleInfo,
		!DepGraph),
	dependency_graph__add_pred_proc_nodes(PredIds, ModuleInfo, Imported,
		!DepGraph).

:- pred dependency_graph__add_proc_nodes(list(proc_id)::in, pred_id::in,
	module_info::in, dependency_graph::in, dependency_graph::out) is det.

dependency_graph__add_proc_nodes([], _PredId, _ModuleInfo, !DepGraph).
dependency_graph__add_proc_nodes([ProcId | ProcIds], PredId, ModuleInfo,
		!DepGraph) :-
	relation__add_element(!.DepGraph, proc(PredId, ProcId), _, !:DepGraph),
	dependency_graph__add_proc_nodes(ProcIds, PredId, ModuleInfo,
		!DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_nodes(list(pred_id)::in, module_info::in,
	bool::in,
	dependency_graph(pred_id)::in, dependency_graph(pred_id)::out) is det.

dependency_graph__add_pred_nodes([], _ModuleInfo, _, DepGraph, DepGraph).
dependency_graph__add_pred_nodes([PredId | PredIds], ModuleInfo,
		IncludeImported, !DepGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	% Don't bother adding nodes (or arcs) for predicates
	% which are imported (i.e. which we don't have any `clauses' for).
	(
		IncludeImported = no,
		pred_info_is_imported(PredInfo)
	->
		true
	;
		relation__add_element(!.DepGraph, PredId, _, !:DepGraph)
	),
	dependency_graph__add_pred_nodes(PredIds, ModuleInfo, IncludeImported,
		!DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_proc_arcs(list(pred_id)::in,
	module_info::in, bool::in,
	dependency_graph::in, dependency_graph::out) is det.

dependency_graph__add_pred_proc_arcs([], _ModuleInfo, _, !DepGraph).
dependency_graph__add_pred_proc_arcs([PredId | PredIds], ModuleInfo, Imported,
		!DepGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	(
		% Don't bother adding nodes (or arcs) for procedures
		% which are imported (i.e. which we don't have any
		% `clauses' for).
		Imported = no,
		ProcIds = pred_info_non_imported_procids(PredInfo)
	;
		Imported = yes,
		ProcIds = pred_info_procids(PredInfo)
	),
	dependency_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo, Imported,
		!DepGraph),
	dependency_graph__add_pred_proc_arcs(PredIds, ModuleInfo, Imported,
		!DepGraph).

:- pred dependency_graph__add_proc_arcs(list(proc_id)::in, pred_id::in,
	module_info::in, bool::in,
	dependency_graph::in, dependency_graph::out) is det.

dependency_graph__add_proc_arcs([], _PredId, _ModuleInfo, _, !DepGraph).
dependency_graph__add_proc_arcs([ProcId | ProcIds], PredId, ModuleInfo,
		IncludeImported, !DepGraph) :-
	module_info_preds(ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	(
		IncludeImported = no,
		proc_info_goal(ProcInfo0, Goal),

		relation__lookup_element(!.DepGraph,
			proc(PredId, ProcId), Caller),
		dependency_graph__add_arcs_in_goal(Goal, Caller, !DepGraph)
	;
		IncludeImported = yes,
		pred_info_import_status(PredInfo0, ImportStatus),
		status_is_imported(ImportStatus, Imported),
		(
			Imported = yes
		;
			Imported = no,
			proc_info_goal(ProcInfo0, Goal),
			relation__lookup_element(!.DepGraph,
				proc(PredId, ProcId), Caller),
			dependency_graph__add_arcs_in_goal(Goal, Caller,
				!DepGraph)
		)
	),
	dependency_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo,
		IncludeImported, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_arcs(list(pred_id)::in, module_info::in,
	bool::in,
	dependency_graph(pred_id)::in, dependency_graph(pred_id)::out) is det.

dependency_graph__add_pred_arcs([], _ModuleInfo, _, !DepGraph).
dependency_graph__add_pred_arcs([PredId | PredIds], ModuleInfo,
		IncludeImported, !DepGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	(
		IncludeImported = no,
		pred_info_is_imported(PredInfo)
	->
		true
	;
		pred_info_clauses_info(PredInfo, ClausesInfo),
		clauses_info_clauses(ClausesInfo, Clauses),
		Goals = list__map(func(clause(_, Goal, _, _)) = Goal, Clauses),
		relation__lookup_element(!.DepGraph, PredId, Caller),
		dependency_graph__add_arcs_in_list(Goals, Caller, !DepGraph)
	),
	dependency_graph__add_pred_arcs(PredIds, ModuleInfo, IncludeImported,
		!DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func pred_proc_id_get_pred_id(pred_proc_id) = pred_id.

pred_proc_id_get_pred_id(proc(PredId, _)) = PredId.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_goal(hlds_goal::in, relation_key::in,
	dependency_graph(T)::in, dependency_graph(T)::out) is det
	<= dependency_node(T).

dependency_graph__add_arcs_in_goal(Goal - _GoalInfo, PPId, !DepGraph) :-
	dependency_graph__add_arcs_in_goal_2(Goal, PPId, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_goal_2(hlds_goal_expr::in,
	relation_key::in,
	dependency_graph(T)::in, dependency_graph(T)::out) is det
	<= dependency_node(T).

dependency_graph__add_arcs_in_goal_2(conj(Goals), Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(par_conj(Goals), Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(disj(Goals), Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(switch(_Var, _Det, Cases), Caller,
		!DepGraph) :-
	dependency_graph__add_arcs_in_cases(Cases, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(if_then_else(_Vars, Cond, Then, Else),
			Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_goal(Cond, Caller, !DepGraph),
	dependency_graph__add_arcs_in_goal(Then, Caller, !DepGraph),
	dependency_graph__add_arcs_in_goal(Else, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(not(Goal), Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(scope(_, Goal), Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, !DepGraph).

dependency_graph__add_arcs_in_goal_2(generic_call(_, _, _, _), _, !DepGraph).

dependency_graph__add_arcs_in_goal_2(call(PredId, ProcId, _, Builtin, _, _),
		Caller, !DepGraph) :-
	( Builtin = inline_builtin ->
		true
	;
		(
			% If the node isn't in the relation, then
			% we didn't insert it because is was imported,
			% and we don't consider it.
			relation__search_element(!.DepGraph,
				dependency_node(proc(PredId, ProcId)), Callee)
		->
			relation__add(!.DepGraph, Caller, Callee, !:DepGraph)
		;
			true
		)
	).

dependency_graph__add_arcs_in_goal_2(unify(_,_,_,Unify,_), Caller,
		!DepGraph) :-
	(
		Unify = assign(_, _)
	;
		Unify = simple_test(_, _)
	;
		Unify = construct(_, Cons, _, _, _, _, _),
		dependency_graph__add_arcs_in_cons(Cons, Caller,
			!DepGraph)
	;
		Unify = deconstruct(_, Cons, _, _, _, _),
		dependency_graph__add_arcs_in_cons(Cons, Caller,
			!DepGraph)
	;
		Unify = complicated_unify(_, _, _)
	).

% There can be no dependencies within a foreign_proc
dependency_graph__add_arcs_in_goal_2(
	foreign_proc(_, _, _, _, _, _), _, !DepGraph).

dependency_graph__add_arcs_in_goal_2(shorthand(ShorthandGoal), Caller,
		!DepGraph) :-
	dependency_graph__add_arcs_in_goal_2_shorthand(ShorthandGoal, Caller,
		!DepGraph).

:- pred dependency_graph__add_arcs_in_goal_2_shorthand(shorthand_goal_expr::in,
	relation_key::in, dependency_graph(T)::in, dependency_graph(T)::out)
	is det <= dependency_node(T).

dependency_graph__add_arcs_in_goal_2_shorthand(bi_implication(LHS, RHS),
		Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_list([LHS, RHS], Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_list(list(hlds_goal)::in,
	relation_key::in,
	dependency_graph(T)::in, dependency_graph(T)::out) is det
	<= dependency_node(T).

dependency_graph__add_arcs_in_list([], _Caller, !DepGraph).
dependency_graph__add_arcs_in_list([Goal|Goals], Caller, !DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, !DepGraph),
	dependency_graph__add_arcs_in_list(Goals, Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_cases(list(case)::in, relation_key::in,
	dependency_graph(T)::in, dependency_graph(T)::out) is det
	<= dependency_node(T).

dependency_graph__add_arcs_in_cases([], _Caller, !DepGraph).
dependency_graph__add_arcs_in_cases([case(Cons, Goal) | Goals], Caller,
		!DepGraph) :-
	dependency_graph__add_arcs_in_cons(Cons, Caller, !DepGraph),
	dependency_graph__add_arcs_in_goal(Goal, Caller, !DepGraph),
	dependency_graph__add_arcs_in_cases(Goals, Caller, !DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_cons(cons_id::in, relation_key::in,
	dependency_graph(T)::in, dependency_graph(T)::out) is det
	<= dependency_node(T).

dependency_graph__add_arcs_in_cons(cons(_, _), _Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(int_const(_), _Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(string_const(_), _Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(float_const(_), _Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(pred_const(ShroudedPredProcId, _), Caller,
		!DepGraph) :-
	PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
	(
			% If the node isn't in the relation, then
			% we didn't insert it because is was imported,
			% and we don't consider it.
		relation__search_element(!.DepGraph,
			dependency_node(PredProcId), Callee)
	->
		relation__add(!.DepGraph, Caller, Callee, !:DepGraph)
	;
		true
	).
dependency_graph__add_arcs_in_cons(type_ctor_info_const(_, _, _),
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(base_typeclass_info_const(_, _, _, _),
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(type_info_cell_constructor(_),
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(typeclass_info_cell_constructor,
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(tabling_pointer_const(_),
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(deep_profiling_proc_layout(_),
		_Caller, !DepGraph).
dependency_graph__add_arcs_in_cons(table_io_decl(_),
		_Caller, !DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

:- pred dependency_graph__write_dependency_ordering(
	list(list(pred_proc_id))::in, module_info::in, int::in,
	io::di, io::uo) is det.

dependency_graph__write_dependency_ordering([], _ModuleInfo, _N, !IO) :-
	io__write_string("\n", !IO).
dependency_graph__write_dependency_ordering([Clique | Rest], ModuleInfo, N,
		!IO) :-
	io__write_string("% Clique ", !IO),
	io__write_int(N, !IO),
	io__write_string("\n", !IO),
	dependency_graph__write_clique(Clique, ModuleInfo, !IO),
	N1 = N + 1,
	dependency_graph__write_dependency_ordering(Rest, ModuleInfo, N1, !IO).

:- pred dependency_graph__write_clique(list(pred_proc_id)::in, module_info::in,
	io::di, io::uo) is det.

dependency_graph__write_clique([], _ModuleInfo, !IO).
dependency_graph__write_clique([proc(PredId, ProcId) | Rest], ModuleInfo,
		!IO) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	Name = pred_info_name(PredInfo),
	proc_info_declared_determinism(ProcInfo, Det),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_context(ProcInfo, Context),
	varset__init(ModeVarSet),

	io__write_string("% ", !IO),
	mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(Name),
		Modes, Det, Context, !IO),
	io__write_string("\n", !IO),
	dependency_graph__write_clique(Rest, ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

% dependency_graph__write_prof_dependency_graph:
%	Output the static call graph of the current module in the form of
%		CallerLabel (\t) CalleeLabel
%
dependency_graph__write_prof_dependency_graph(!ModuleInfo, !IO) :-
	module_info_ensure_dependency_info(!ModuleInfo),
	module_info_dependency_info(!.ModuleInfo, DepInfo),
	write_graph(DepInfo, write_empty_node,
		write_prof_dep_graph_link(!.ModuleInfo), !IO).

% dependency_graph__write_dependency_graph:
%	Output the static call graph of the current module in the form of
%		CallerModeDecl (\t) CalleeModeDecl
%	with a heading.
%
dependency_graph__write_dependency_graph(!ModuleInfo, !IO) :-
	module_info_ensure_dependency_info(!ModuleInfo),
	module_info_dependency_info(!.ModuleInfo, DepInfo),
	io__write_string("% Dependency graph\n", !IO),
	io__write_string("\n\n% Dependency ordering\n", !IO),
	write_graph(DepInfo, write_empty_node,
		write_dep_graph_link(!.ModuleInfo), !IO).

:- pred write_empty_node(pred_proc_id::in, io::di, io::uo) is det.

write_empty_node(_, !IO).

:- pred write_prof_dep_graph_link(module_info::in,
	pred_proc_id::in, pred_proc_id::in, io::di, io::uo) is det.

write_prof_dep_graph_link(ModuleInfo, Parent, Child, !IO) :-
	Parent = proc(PPredId, PProcId),	% Caller
	Child = proc(CPredId, CProcId),		% Callee
	dependency_graph__output_label(ModuleInfo, PPredId, PProcId, !IO),
	io__write_string("\t", !IO),
	dependency_graph__output_label(ModuleInfo, CPredId, CProcId, !IO),
	io__write_string("\n", !IO).

:- pred write_dep_graph_link(module_info::in,
	pred_proc_id::in, pred_proc_id::in, io::di, io::uo) is det.

write_dep_graph_link(ModuleInfo, Parent, Child, !IO) :-
	Parent = proc(PPredId, PProcId),	% Caller
	Child = proc(CPredId, CProcId),		% Callee
	module_info_pred_proc_info(ModuleInfo, PPredId, PProcId,
		PPredInfo, PProcInfo),
	module_info_pred_proc_info(ModuleInfo, CPredId, CProcId,
		CPredInfo, CProcInfo),
	PName = pred_info_name(PPredInfo),
	proc_info_declared_determinism(PProcInfo, PDet),
	proc_info_argmodes(PProcInfo, PModes),
	proc_info_context(PProcInfo, PContext),
	CName = pred_info_name(CPredInfo),
	proc_info_declared_determinism(CProcInfo, CDet),
	proc_info_argmodes(CProcInfo, CModes),
	proc_info_context(CProcInfo, CContext),
	varset__init(ModeVarSet),
	mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(PName),
		PModes, PDet, PContext, !IO),
	io__write_string(" -> ", !IO),
	mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(CName),
		CModes, CDet, CContext, !IO),
	io__write_string("\n", !IO).

%-----------------------------------------------------------------------------%

write_graph(DepInfo, WriteNode, WriteLink, !IO) :-
	hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph),
	relation__domain(DepGraph, DomSet),
	set__to_sorted_list(DomSet, DomList),
	write_graph_nodes(DomList, DepGraph, WriteNode, WriteLink, !IO).

write_graph_nodes([], _Graph, _WriteNode, _WriteLink, !IO).
write_graph_nodes([Node | Nodes], Graph, WriteNode, WriteLink, !IO) :-
	WriteNode(Node, !IO),
	relation__lookup_element(Graph, Node, NodeKey),
	relation__lookup_from(Graph, NodeKey, ChildrenSet),
	set__to_sorted_list(ChildrenSet, Children),
	write_graph_children(Children, Node, Graph, WriteLink, !IO),
	write_graph_nodes(Nodes, Graph, WriteNode, WriteLink, !IO).

:- pred write_graph_children(list(relation_key)::in, pred_proc_id::in,
	dependency_graph::in,
	pred(pred_proc_id, pred_proc_id, io, io)::pred(in, in, di, uo) is det,
	io::di, io::uo) is det.

write_graph_children([], _Parent, _Graph, _WriteLink, !IO).
write_graph_children([ChildKey | Children], Parent, Graph, WriteLink, !IO) :-
	relation__lookup_key(Graph, ChildKey, Child),
	WriteLink(Parent, Child, !IO),
	write_graph_children(Children, Parent, Graph, WriteLink, !IO).

%-----------------------------------------------------------------------------%

% dependency_graph__output_label:
%	Prints out the label corresponding to PredId and ProcId.
%

:- pred dependency_graph__output_label(module_info::in,
	pred_id::in, proc_id::in, io::di, io::uo) is det.

dependency_graph__output_label(ModuleInfo, PredId, ProcId, !IO) :-
	ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
	output_proc_label(ProcLabel, !IO).

%-----------------------------------------------------------------------------%

dependency_graph__get_scc_entry_points(SCC, HigherSCCs,
		ModuleInfo, EntryPoints) :-
	list__filter(dependency_graph__is_entry_point(HigherSCCs, ModuleInfo),
		SCC, EntryPoints).

:- pred dependency_graph__is_entry_point(list(list(pred_proc_id))::in,
	module_info::in, pred_proc_id::in) is semidet.

dependency_graph__is_entry_point(HigherSCCs, ModuleInfo, PredProcId) :-
	(
		% Is the predicate exported?
		PredProcId = proc(PredId, _ProcId),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_is_exported(PredInfo)
	;
		% Is the predicate called from a higher SCC?
		module_info_dependency_info(ModuleInfo, DepInfo),
		hlds_dependency_info_get_dependency_graph(DepInfo,
			DepGraph),

		relation__lookup_element(DepGraph, PredProcId, PredProcIdKey),
		relation__lookup_to(DepGraph, PredProcIdKey, CallingKeys),
		set__member(CallingKey, CallingKeys),
		relation__lookup_key(DepGraph, CallingKey,
			CallingPred),
		list__member(HigherSCC, HigherSCCs),
		list__member(CallingPred, HigherSCC)
	).

%-----------------------------------------------------------------------------%

module_info_ensure_aditi_dependency_info(!ModuleInfo) :-
	module_info_ensure_dependency_info(!ModuleInfo),
	module_info_dependency_info(!.ModuleInfo, DepInfo0),
	hlds_dependency_info_get_maybe_aditi_dependency_ordering(DepInfo0,
		MaybeAditiInfo),
	( MaybeAditiInfo = yes(_) ->
		true
	;
		hlds_dependency_info_get_dependency_ordering(DepInfo0,
			DepOrdering),
		aditi_scc_info_init(!.ModuleInfo, AditiInfo0),
		dependency_graph__build_aditi_scc_info(DepOrdering,
			AditiInfo0, AditiInfo),
		dependency_graph__merge_aditi_sccs(AditiInfo, AditiOrdering),
		hlds_dependency_info_set_aditi_dependency_ordering(
			AditiOrdering, DepInfo0, DepInfo),
		module_info_set_dependency_info(DepInfo, !ModuleInfo)
	).

:- pred dependency_graph__build_aditi_scc_info(dependency_ordering::in,
	aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__build_aditi_scc_info([], !Info).
dependency_graph__build_aditi_scc_info([SCC | SCCs], !Info) :-
	aditi_scc_info_get_module_info(ModuleInfo, !Info),
	(
		list__member(PredProcId, SCC),
		PredProcId = proc(PredId, _),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		hlds_pred__pred_info_is_aditi_relation(PredInfo),
		\+ hlds_pred__pred_info_is_base_relation(PredInfo)
	->
		aditi_scc_info_add_scc(SCC, SCCs, SCCid, !Info),
		list__foldl(
			dependency_graph__process_aditi_pred_proc_id(SCCid),
			SCC, !Info)
	;
		true
	),
	dependency_graph__build_aditi_scc_info(SCCs, !Info).

:- pred dependency_graph__process_aditi_pred_proc_id(scc_id::in,
	pred_proc_id::in, aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__process_aditi_pred_proc_id(SCCid, PredProcId, !Info) :-
	aditi_scc_info_get_module_info(ModuleInfo, !Info),
	module_info_pred_proc_info(ModuleInfo, PredProcId,
		PredInfo, ProcInfo),
	dependency_graph__process_aditi_proc_info(SCCid, PredInfo, ProcInfo,
		!Info).

:- pred dependency_graph__process_aditi_proc_info(scc_id::in, pred_info::in,
	proc_info::in, aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__process_aditi_proc_info(CurrSCC, PredInfo, ProcInfo,
		!Info) :-
	(
		pred_info_is_exported(PredInfo)
	->
		aditi_scc_info_add_no_merge_scc(CurrSCC, !Info)
	;
		pred_info_get_markers(PredInfo, Markers),
		check_marker(Markers, context)
	->
		% The context transformation can only be applied
		% to a single predicate SCC, so don't merge
		% other SCCs with a context-transformed SCC.
		aditi_scc_info_add_no_merge_scc(CurrSCC, !Info)
	;
		true
	),
	proc_info_goal(ProcInfo, Goal),
	process_aditi_goal(Goal, !Info).

%-----------------------------------------------------------------------------%

	% Go over the goal finding predicates called through negation
	% or aggregation. The SCCs containing those predicates cannot
	% be merged into the current SCC.
:- pred process_aditi_goal(hlds_goal::in, aditi_scc_info::in,
	aditi_scc_info::out) is det.

process_aditi_goal(Goal, !Info) :-
	multi_map__init(MMap0),
	process_aditi_goal(no, Goal, MMap0, _, !Info).

:- pred process_aditi_goal(bool::in, hlds_goal::in,
	multi_map(prog_var, pred_proc_id)::in,
	multi_map(prog_var, pred_proc_id)::out,
	aditi_scc_info::in, aditi_scc_info::out) is det.

process_aditi_goal(IsNeg, conj(Goals) - _, !Map, !Info) :-
	list__foldl2(process_aditi_goal(IsNeg), Goals, !Map, !Info).
process_aditi_goal(_IsNeg, par_conj(_) - _, _, _, !Info) :-
	error("process_aditi_goal - par_conj").
process_aditi_goal(IsNeg, disj(Goals) - _, !Map, !Info) :-
	list__foldl2(process_aditi_goal(IsNeg), Goals, !Map, !Info).
process_aditi_goal(IsNeg, switch(_, _, Cases) - _, !Map, !Info) :-
	NegCallsInCases = (
		pred(Case::in, M0::in, M::out, AInfo0::in, AInfo::out) is det :-
			Case = case(_ConsId, Goal),
			process_aditi_goal(IsNeg, Goal, M0, M, AInfo0, AInfo)
	),
	list__foldl2(NegCallsInCases, Cases, !Map, !Info).
process_aditi_goal(IsNeg, if_then_else(_, Cond, Then, Else) - _,
		!Map, !Info) :-
	process_aditi_goal(yes, Cond, !Map, !Info),
	process_aditi_goal(IsNeg, Then, !Map, !Info),
	process_aditi_goal(IsNeg, Else, !Map, !Info).
process_aditi_goal(IsNeg, scope(_, Goal) - _, !Map, !Info) :-
	process_aditi_goal(IsNeg, Goal, !Map, !Info).
process_aditi_goal(_IsNeg, not(Goal) - _, !Map, !Info) :-
	process_aditi_goal(yes, Goal, !Map, !Info).
process_aditi_goal(IsNeg, call(PredId, ProcId, Args, _, _, _) - _,
		!Map, !Info) :-
	aditi_scc_info_handle_call(IsNeg, PredId, ProcId, Args, !.Map, !Info).
process_aditi_goal(_IsNeg, unify(Var, _, _, Unify, _) - _, !Map, !Info) :-
	(
		Unify = construct(_, pred_const(ShroudedPredProcId, _),
			_, _, _, _, _)
	->
		PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
		aditi_scc_info_add_closure(Var, PredProcId, !Map, !Info)
	;
		true
	).
process_aditi_goal(_IsNeg, generic_call(_, _, _, _) - _, !Map, !Info).
process_aditi_goal(_IsNeg, foreign_proc(_, _, _, _, _, _) - _, !Map, !Info).
process_aditi_goal(_, shorthand(_) - _, _, _, _, _) :-
	% these should have been expanded out by now
	error("process_aditi_goal: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred dependency_graph__merge_aditi_sccs(aditi_scc_info::in,
	aditi_dependency_ordering::out) is det.

dependency_graph__merge_aditi_sccs(Info, Ordering) :-
	Info = aditi_scc_info(ModuleInfo, _PredSCC, SCCPred,
		_, SCCRel, NoMerge, _),
	( relation__tsort(SCCRel, SCCTsort) ->
		eqvclass__init(EqvSCCs0),
		set__init(MergedSCCs),
		% Make all the SCCs known to the equivalence class.
		AddElement = (pred(Elem::in, Eqv0::in, Eqv::out) is det :-
				eqvclass__new_element(Eqv0, Elem, Eqv)
			),
		list__foldl(AddElement, SCCTsort, EqvSCCs0, EqvSCCs),
		dependency_graph__merge_aditi_sccs_2(SCCTsort, ModuleInfo,
			EqvSCCs, MergedSCCs, NoMerge, SCCRel,
			SCCPred, [], Ordering)
	;
		error("dependency_graph__merge_aditi_sccs: " ++
			"SCC dependency relation is cyclic")
	).

:- pred dependency_graph__merge_aditi_sccs_2(list(scc_id)::in,
	module_info::in, eqvclass(scc_id)::in, set(scc_id)::in,
	set(scc_id)::in, relation(scc_id)::in, scc_pred_map::in,
	aditi_dependency_ordering::in, aditi_dependency_ordering::out) is det.

dependency_graph__merge_aditi_sccs_2([], _, _, _, _, _, _, !Ordering).
dependency_graph__merge_aditi_sccs_2([SCCid | SCCs0], ModuleInfo, EqvSCCs0,
		MergedSCCs0, NoMergeSCCs, SCCRel, SCCPreds, !Ordering) :-
	( set__member(SCCid, MergedSCCs0) ->
			% This SCC has been merged into its parent.
		EqvSCCs = EqvSCCs0,
		SCCs = SCCs0
	;
		map__lookup(SCCPreds, SCCid, SCC0 - EntryPoints),
		some [PredProcId] (
			list__member(proc(PredId, _), SCC0),
			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			pred_info_get_markers(PredInfo, Markers),
			check_marker(Markers, context)
		)
	->
		% Don't merge predicates for which the context
		% transformation has been requested with other SCCs --
		% their magic predicates are incompatible.
		!:Ordering = [aditi_scc([SCC0], EntryPoints) | !.Ordering],
		EqvSCCs = EqvSCCs0,
		SCCs = SCCs0
	;
		dependency_graph__get_called_scc_ids(SCCid, SCCRel,
			CalledSCCs),
		map__lookup(SCCPreds, SCCid, SCC0 - EntryPoints),
		dependency_graph__do_merge_aditi_sccs(SCCid, CalledSCCs,
			NoMergeSCCs, SCCs0, SCCs, SCCPreds, SCCRel,
			EqvSCCs0, EqvSCCs,
			aditi_scc([SCC0], EntryPoints), SCC),
		!:Ordering = [SCC | !.Ordering]
	),
	dependency_graph__merge_aditi_sccs_2(SCCs, ModuleInfo, EqvSCCs,
		MergedSCCs0, NoMergeSCCs, SCCRel, SCCPreds, !Ordering).

	% Find the SCCs called from a given SCC.
:- pred dependency_graph__get_called_scc_ids(scc_id::in, relation(scc_id)::in,
	set(scc_id)::out) is det.

dependency_graph__get_called_scc_ids(SCCid, SCCRel, CalledSCCSet) :-
	relation__lookup_element(SCCRel, SCCid, SCCidKey),
	relation__lookup_from(SCCRel, SCCidKey, CalledSCCKeys),
	set__to_sorted_list(CalledSCCKeys, CalledSCCKeyList),
	list__map(relation__lookup_key(SCCRel), CalledSCCKeyList, CalledSCCs),
	set__list_to_set(CalledSCCs, CalledSCCSet).

	% Go over the list of SCCs finding all those which
	% can be merged into a given SCC.
:- pred dependency_graph__do_merge_aditi_sccs(scc_id::in, set(scc_id)::in,
	set(scc_id)::in, list(scc_id)::in, list(scc_id)::out,
	scc_pred_map::in, relation(scc_id)::in,
	eqvclass(scc_id)::in, eqvclass(scc_id)::out,
	aditi_scc::in, aditi_scc::out) is det.

dependency_graph__do_merge_aditi_sccs(_, _, _, [], [], _, _, !Eqv, !SubModule).
dependency_graph__do_merge_aditi_sccs(CurrSCCid, CalledSCCs, NoMergeSCCs,
		[LowerSCCid | LowerSCCs0], LowerSCCs, SCCPreds, SCCRel,
		!EqvSCCs, !SubModule) :-
	(
		set__member(LowerSCCid, CalledSCCs),
		\+ set__member(LowerSCCid, NoMergeSCCs)
	->
		relation__lookup_element(SCCRel, LowerSCCid, LowerSCCKey),
		relation__lookup_to(SCCRel, LowerSCCKey, CallingSCCKeys),
		set__to_sorted_list(CallingSCCKeys, CallingSCCKeyList),
		list__map(relation__lookup_key(SCCRel),
			CallingSCCKeyList, CallingSCCs),
		( eqvclass__same_eqvclass_list(!.EqvSCCs, CallingSCCs) ->

			%
			% All the calling SCCs have been merged (or
			% there was only one to start with) so we
			% can safely merge this one in as well.
			%
			eqvclass__new_equivalence(!.EqvSCCs, CurrSCCid,
				LowerSCCid, !:EqvSCCs),
			map__lookup(SCCPreds, LowerSCCid, LowerSCC),
			LowerSCC = LowerSCCPreds - _,

			%
			% The entry-points of the combined SCC cannot include
			% the entry-points of the lower SCC, since that
			% would mean that the lower SCC was called from
			% multiple places and could not be merged.
			%
			!.SubModule = aditi_scc(CurrSCCPreds0, EntryPoints),
			!:SubModule =
				aditi_scc([LowerSCCPreds | CurrSCCPreds0],
					EntryPoints),

			%
			% Add the SCCs called by the newly merged SCC
			% to those we are attempting to merge.
			%
			dependency_graph__get_called_scc_ids(LowerSCCid,
				SCCRel, LowerCalledSCCs),
			set__union(CalledSCCs, LowerCalledSCCs, CalledSCCs1),

			dependency_graph__do_merge_aditi_sccs(CurrSCCid,
				CalledSCCs1, NoMergeSCCs, LowerSCCs0,
				LowerSCCs, SCCPreds, SCCRel, !EqvSCCs,
				!SubModule)
		;
			dependency_graph__do_merge_aditi_sccs(CurrSCCid,
				CalledSCCs, NoMergeSCCs, LowerSCCs0,
				LowerSCCs1, SCCPreds, SCCRel,
				!EqvSCCs, !SubModule),
			LowerSCCs = [LowerSCCid | LowerSCCs1]
		)
	;
		dependency_graph__do_merge_aditi_sccs(CurrSCCid, CalledSCCs,
			NoMergeSCCs, LowerSCCs0, LowerSCCs1, SCCPreds, SCCRel,
			!EqvSCCs, !SubModule),
		LowerSCCs = [LowerSCCid | LowerSCCs1]
	).

%-----------------------------------------------------------------------------%

:- type aditi_scc_info --->
	aditi_scc_info(
		aditi_scc_module_info	:: module_info,
		aditi_scc_proc_to_scc	:: map(pred_proc_id, scc_id),
		aditi_scc_scc_to_procs	:: scc_pred_map,
		aditi_scc_local_procs	:: set(pred_proc_id),
					% all local Aditi preds
		aditi_scc_dependencies	:: relation(scc_id),
		aditi_scc_no_merge_sccs	:: set(scc_id),
					% SCCs which can't be merged
					% into their parents.
		aditi_scc_cur_scc	:: scc_id
					% current SCC.
	).

		% For each SCC, a list of all preds in SCC, and a list
		% of entry-points of the SCC.
:- type scc_pred_map == map(scc_id, pair(list(pred_proc_id))).

:- type scc_id == int.

:- type scc == list(pred_proc_id).

:- pred aditi_scc_info_init(module_info::in, aditi_scc_info::out) is det.

aditi_scc_info_init(ModuleInfo, AditiInfo) :-
	map__init(PredSCC),
	map__init(SCCPred),
	set__init(AditiPreds),
	relation__init(SCCDep),
	set__init(NoMergeSCCs),
	AditiInfo = aditi_scc_info(ModuleInfo, PredSCC, SCCPred,
		AditiPreds, SCCDep, NoMergeSCCs, 0).

:- pred aditi_scc_info_get_module_info(module_info::out,
	aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_get_module_info(Module, Info, Info) :-
	Module = Info ^ aditi_scc_module_info.

:- pred aditi_scc_info_add_no_merge_scc(scc_id::in,
	aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_no_merge_scc(SCCid, Info0, Info) :-
	NoMerge0 = Info0 ^ aditi_scc_no_merge_sccs,
	set__insert(NoMerge0, SCCid, NoMerge),
	Info = Info0 ^ aditi_scc_no_merge_sccs := NoMerge.

:- pred aditi_scc_info_add_scc(list(pred_proc_id)::in,
	dependency_ordering::in, scc_id::out,
	aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_scc(SCC, HigherSCCs, SCCid, Info0, Info) :-
	Info0 = aditi_scc_info(ModuleInfo, PredSCC0, SCCPred0, AditiPreds0,
		SCCRel0, NoMerge, LastSCC),
	SCCid = LastSCC + 1,
	dependency_graph__get_scc_entry_points(SCC, HigherSCCs,
		ModuleInfo, EntryPoints),
	map__det_insert(SCCPred0, SCCid, SCC - EntryPoints, SCCPred),
	AddToMap = (pred(PredProcId::in, PS0::in, PS::out) is det :-
		map__det_insert(PS0, PredProcId, SCCid, PS)
	),
	list__foldl(AddToMap, SCC, PredSCC0, PredSCC),
	relation__add_element(SCCRel0, SCCid, _, SCCRel),
	set__insert_list(AditiPreds0, SCC, AditiPreds),
	Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred, AditiPreds,
		SCCRel, NoMerge, SCCid).

:- pred aditi_scc_info_handle_call(bool::in, pred_id::in, proc_id::in,
	list(prog_var)::in, multi_map(prog_var, pred_proc_id)::in,
	aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_handle_call(IsNeg, PredId, ProcId, Args, Map, !Info) :-
	!.Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred,
		AditiPreds, SCCRel0, NoMerge0, SCCid),
	PredProcId = proc(PredId, ProcId),
	( set__member(PredProcId, AditiPreds) ->
		map__lookup(PredSCC, PredProcId, CalledSCCid),
		( CalledSCCid = SCCid ->
			SCCRel1 = SCCRel0,
			NoMerge1 = NoMerge0
		;
			relation__add_values(SCCRel0, SCCid, CalledSCCid,
				SCCRel1),
			( IsNeg = yes ->
				set__insert(NoMerge0, CalledSCCid, NoMerge1)
			;
				NoMerge1 = NoMerge0
			)
		),
		handle_higher_order_args(Args, no, SCCid, Map, PredSCC,
			SCCRel1, SCCRel, NoMerge1, NoMerge),
		!:Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred,
			AditiPreds, SCCRel, NoMerge, SCCid)
	;
		( hlds_pred__is_aditi_aggregate(ModuleInfo, PredId) ->
			handle_higher_order_args(Args, yes, SCCid, Map,
				PredSCC, SCCRel0, SCCRel, NoMerge0, NoMerge),
			!:Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred,
				AditiPreds, SCCRel, NoMerge, SCCid)
		;
			true
		)
	).

	% An SCC cannot be merged into its parents if one of its
	% procedures is called as an aggregate query.
:- pred handle_higher_order_args(list(prog_var)::in, bool::in, scc_id::in,
	multi_map(prog_var, pred_proc_id)::in, map(pred_proc_id, scc_id)::in,
	relation(scc_id)::in, relation(scc_id)::out,
	set(scc_id)::in, set(scc_id)::out) is det.

handle_higher_order_args([], _, _, _, _, !SCCRel, !NoMerge).
handle_higher_order_args([Arg | Args], IsAgg, SCCid, Map, PredSCC,
		!SCCRel, !NoMerge) :-
	( multi_map__search(Map, Arg, PredProcIds) ->
		list__foldl2(handle_higher_order_arg(PredSCC, IsAgg, SCCid),
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
	( map__search(PredSCC, PredProcId, CalledSCCid) ->
		% Make sure anything called through an aggregate
		% is not merged into the current sub-module.
		( IsAgg = yes ->
			set__insert(!.NoMerge, CalledSCCid, !:NoMerge)
		;
			true
		),
		( CalledSCCid = SCCid ->
			true
		;
			relation__add_values(!.SCCRel, SCCid, CalledSCCid,
				!:SCCRel)
		)
	;
		true
	).

:- pred aditi_scc_info_add_closure(prog_var::in, pred_proc_id::in,
	multi_map(prog_var, pred_proc_id)::in,
	multi_map(prog_var, pred_proc_id)::out,
	aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_closure(Var, PredProcId, Map0, Map, Info, Info) :-
	AditiPreds = Info ^ aditi_scc_local_procs,
	( set__member(PredProcId, AditiPreds) ->
		multi_map__set(Map0, Var, PredProcId, Map)
	;
		Map = Map0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
