%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module transform_hlds__dependency_graph.
% Main author: bromage, conway, stayl.

% The dependency_graph records which procedures depend on which other
% procedures.  It is defined as a relation (see hlds_module.m) R where xRy
% means that the definition of x depends on the definition of y.
% Note that imported procedures are not included in the dependency_graph
% (although opt_imported procedures are included).
%
% The other important structure is the dependency_ordering which is
% a list of the cliques (strongly-connected components) of this relation,
% in topological order.  This is very handy for doing fixpoint iterations.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module bool, list, io.

	% Ensure that the module_info contains a version of the
	% dependency_info which only contains arcs between procedures
	% for which there are clauses defined (ie not imported except
	% for opt_imported).
	%
:- pred module_info_ensure_dependency_info(module_info, module_info).
:- mode module_info_ensure_dependency_info(in, out) is det.

	% Build the dependency graph, if the bool is yes then
	% imported procedures are included in the dependency graph,
	% otherwise they aren't.
	%
:- pred dependency_graph__build_dependency_graph(module_info, bool,
		dependency_info).
:- mode dependency_graph__build_dependency_graph(in, in, out) is det.

:- pred dependency_graph__write_dependency_graph(module_info, module_info,
						io__state, io__state).
:- mode dependency_graph__write_dependency_graph(in, out, di, uo) is det.

	% Output a form of the static call graph to a file for use by the
	% profiler.
:- pred dependency_graph__write_prof_dependency_graph(module_info, module_info,
						io__state, io__state).
:- mode dependency_graph__write_prof_dependency_graph(in, out, di, uo) is det.

	% Given the list of predicates in a strongly connected component
	% of the dependency graph, a list of the higher SCCs in the module
	% and a module_info, find out which members of the SCC can be
	% called from outside the SCC.
:- pred dependency_graph__get_scc_entry_points(list(pred_proc_id), 
		dependency_ordering, module_info, list(pred_proc_id)).
:- mode dependency_graph__get_scc_entry_points(in, in, in, out) is det.

	% Create the Aditi dependency ordering. This contains all the Aditi
	% SCCs in the original program. The difference is that SCCs which 
	% are only called from one other SCC and are not called through
	% negation or aggregation are merged into the parent SCC. This makes
	% the low-level RL optimizations more effective while maintaining
	% stratification. 
	% dead_proc_elim.m should be be run before this is called
	% to avoid missing some opportunities for merging where
	% a procedure is called from a dead procedure.
:- pred module_info_ensure_aditi_dependency_info(module_info, module_info).
:- mode module_info_ensure_aditi_dependency_info(in, out) is det.

	% write_graph(Graph, WriteNode, WriteEdge)
	%
	% Write out the dependency graph using WriteNode to decide what
	% to output for a node in the dependency graph and WriteEdge for
	% an edge.
	%
:- pred dependency_graph__write_graph(dependency_info::in,
	pred(pred_proc_id, io__state, io__state)::pred(in, di, uo) is det,
	pred(pred_proc_id, pred_proc_id, io__state, io__state)::
			pred(in, in, di, uo) is det,
	io__state::di, io__state::uo) is det.

	% write_graph_nodes(Nodes, Graph, WriteNode, WriteEdge)
	%
	% Write out each of the Nodes in the Graph using WriteNode and
	% any edges originating in Nodes, using WriteEdge.
	%
:- pred dependency_graph__write_graph_nodes(list(pred_proc_id)::in,
	dependency_graph::in,
	pred(pred_proc_id, io__state, io__state)::pred(in, di, uo) is det,
	pred(pred_proc_id, pred_proc_id, io__state, io__state)::
			pred(in, in, di, uo) is det,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_data, parse_tree__prog_data.
:- import_module check_hlds__mode_util, libs__globals, libs__options.
:- import_module ll_backend__code_util, hlds__goal_util.
:- import_module parse_tree__mercury_to_mercury.

% XXX we should not import llds here -- this should depend only on the HLDS,
% not on the LLDS.  But the LLDS stuff is unfortunately needed for producing
% the LLDS labels used for dependency_graph__write_prof_dependency_graph.
:- import_module ll_backend__llds, ll_backend__llds_out.

:- import_module term, varset.
:- import_module int, bool, term, require, string.
:- import_module map, multi_map, set, std_util.
:- import_module varset, relation, eqvclass.

%-----------------------------------------------------------------------------%

	% Ensure that the dependency graph has been built by building
	% it if necessary.

module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo) :-
	module_info_get_maybe_dependency_info(ModuleInfo0, MaybeDepInfo),
	( MaybeDepInfo = yes(_) ->
	    ModuleInfo = ModuleInfo0
	;
	    dependency_graph__build_dependency_graph(ModuleInfo0, no, DepInfo),
	    module_info_set_dependency_info(ModuleInfo0, DepInfo, ModuleInfo)
	).

	% Traverse the module structure, calling `dependency_graph__add_arcs'
	% for each procedure body.

dependency_graph__build_dependency_graph(ModuleInfo0, Imported, DepInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	relation__init(DepGraph0),
	dependency_graph__add_pred_nodes(PredIds, ModuleInfo0, Imported,
				DepGraph0, DepGraph1),
	dependency_graph__add_pred_arcs(PredIds, ModuleInfo0, Imported,
				DepGraph1, DepGraph),
	hlds_dependency_info_init(DepInfo0),
	hlds_dependency_info_set_dependency_graph(DepInfo0, DepGraph,
				DepInfo1),
	relation__atsort(DepGraph, DepOrd0),
	dependency_graph__sets_to_lists(DepOrd0, [], DepOrd),
	hlds_dependency_info_set_dependency_ordering(DepInfo1, DepOrd,
				DepInfo).

:- pred dependency_graph__sets_to_lists( list(set(pred_proc_id)),
			list(list(pred_proc_id)), list(list(pred_proc_id))).
:- mode dependency_graph__sets_to_lists(in, in, out) is det.

dependency_graph__sets_to_lists([], Xs, Xs).
dependency_graph__sets_to_lists([X | Xs], Ys, Zs) :-
	set__to_sorted_list(X, Y),
	dependency_graph__sets_to_lists(Xs, [Y | Ys], Zs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_nodes(list(pred_id), module_info,
		bool, dependency_graph, dependency_graph).
:- mode dependency_graph__add_pred_nodes(in, in, in, in, out) is det.

dependency_graph__add_pred_nodes([], _ModuleInfo, _, DepGraph, DepGraph).
dependency_graph__add_pred_nodes([PredId | PredIds], ModuleInfo, Imported,
                                        DepGraph0, DepGraph) :-
        module_info_preds(ModuleInfo, PredTable),
        map__lookup(PredTable, PredId, PredInfo),
	(
		% Don't bother adding nodes (or arcs) for procedures
		% which which are imported (ie we don't have any
		% `clauses' for).
		Imported = no,
		pred_info_non_imported_procids(PredInfo, ProcIds)
	;
		Imported = yes,
		pred_info_procids(PredInfo, ProcIds)
	),

	dependency_graph__add_proc_nodes(ProcIds, PredId, ModuleInfo,
		DepGraph0, DepGraph1),
        dependency_graph__add_pred_nodes(PredIds, ModuleInfo, Imported,
		DepGraph1, DepGraph).

:- pred dependency_graph__add_proc_nodes(list(proc_id), pred_id, module_info,
                        dependency_graph, dependency_graph).
:- mode dependency_graph__add_proc_nodes(in, in, in, in, out) is det.

dependency_graph__add_proc_nodes([], _PredId, _ModuleInfo, DepGraph, DepGraph).
dependency_graph__add_proc_nodes([ProcId | ProcIds], PredId, ModuleInfo,
                                                DepGraph0, DepGraph) :-
	relation__add_element(DepGraph0, proc(PredId, ProcId), _, DepGraph1),
        dependency_graph__add_proc_nodes(ProcIds, PredId, ModuleInfo,
                                                DepGraph1, DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_pred_arcs(list(pred_id), module_info, bool,
			dependency_graph, dependency_graph).
:- mode dependency_graph__add_pred_arcs(in, in, in, in, out) is det.

dependency_graph__add_pred_arcs([], _ModuleInfo, _, DepGraph, DepGraph).
dependency_graph__add_pred_arcs([PredId | PredIds], ModuleInfo, Imported,
					DepGraph0, DepGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	(
		% Don't bother adding nodes (or arcs) for procedures
		% which which are imported (ie we don't have any
		% `clauses' for).
		Imported = no,
		pred_info_non_imported_procids(PredInfo, ProcIds)
	;
		Imported = yes,
		pred_info_procids(PredInfo, ProcIds)
	),
	dependency_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo, Imported,
			DepGraph0, DepGraph1),
	dependency_graph__add_pred_arcs(PredIds, ModuleInfo, Imported,
			DepGraph1, DepGraph).

:- pred dependency_graph__add_proc_arcs(list(proc_id), pred_id, module_info,
			bool, dependency_graph, dependency_graph).
:- mode dependency_graph__add_proc_arcs(in, in, in, in, in, out) is det.

dependency_graph__add_proc_arcs([], _PredId, _ModuleInfo, _,
		DepGraph, DepGraph).
dependency_graph__add_proc_arcs([ProcId | ProcIds], PredId, ModuleInfo,
		IncludeImported, DepGraph0, DepGraph) :-

	module_info_preds(ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	(
		IncludeImported = no,
		proc_info_goal(ProcInfo0, Goal),

		relation__lookup_element(DepGraph0,
				proc(PredId, ProcId), Caller),
		dependency_graph__add_arcs_in_goal(Goal, Caller,
				DepGraph0, DepGraph1)
	;
		IncludeImported = yes,
		pred_info_import_status(PredInfo0, ImportStatus),
		status_is_imported(ImportStatus, Imported),
		(
			Imported = yes,
			DepGraph1 = DepGraph0
		;
			Imported = no,
			proc_info_goal(ProcInfo0, Goal),

			relation__lookup_element(DepGraph0,
					proc(PredId, ProcId), Caller),
			dependency_graph__add_arcs_in_goal(Goal, Caller,
					DepGraph0, DepGraph1)
		)
	),
	dependency_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo,
			IncludeImported, DepGraph1, DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_goal(hlds_goal, relation_key,
					dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_goal(in, in, in, out) is det.

dependency_graph__add_arcs_in_goal(Goal - _GoalInfo, PPId, 
					DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal_2(Goal, PPId, DepGraph0, DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_goal_2(hlds_goal_expr, relation_key,
					dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_goal_2(in, in, in, out) is det.

dependency_graph__add_arcs_in_goal_2(conj(Goals), Caller, 
					DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(par_conj(Goals), Caller, 
					DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(disj(Goals), Caller, 
					DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_list(Goals, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(switch(_Var, _Det, Cases),
					Caller, DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_cases(Cases, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(if_then_else(_Vars, Cond, Then, Else),
			Caller, DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal(Cond, Caller, DepGraph0, DepGraph1),
	dependency_graph__add_arcs_in_goal(Then, Caller, DepGraph1, DepGraph2),
	dependency_graph__add_arcs_in_goal(Else, Caller, DepGraph2, DepGraph).

dependency_graph__add_arcs_in_goal_2(not(Goal), Caller, DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(some(_Vars, _, Goal), Caller, 
					DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, DepGraph0, DepGraph).

dependency_graph__add_arcs_in_goal_2(generic_call(_, _, _, _),
		_Caller, DepGraph, DepGraph).

dependency_graph__add_arcs_in_goal_2(call(PredId, ProcId, _, Builtin, _, _),
			Caller, DepGraph0, DepGraph) :-
	(
		Builtin = inline_builtin
	->
		DepGraph = DepGraph0
	;
		(
			% If the node isn't in the relation, then
			% we didn't insert it because is was imported,
			% and we don't consider it.
			relation__search_element(DepGraph0,
				proc(PredId, ProcId), Callee)
		->
			relation__add(DepGraph0, Caller, Callee, DepGraph)
		;
			DepGraph = DepGraph0
		)
	).

dependency_graph__add_arcs_in_goal_2(unify(_,_,_,Unify,_), Caller,
				DepGraph0, DepGraph) :-
	( Unify = assign(_, _),
	    DepGraph0 = DepGraph
	; Unify = simple_test(_, _),
	    DepGraph0 = DepGraph
	; Unify = construct(_, Cons, _, _, _, _, _),
	    dependency_graph__add_arcs_in_cons(Cons, Caller,
				DepGraph0, DepGraph)
	; Unify = deconstruct(_, Cons, _, _, _, _),
	    dependency_graph__add_arcs_in_cons(Cons, Caller,
				DepGraph0, DepGraph)
	; Unify = complicated_unify(_, _, _),
	    DepGraph0 = DepGraph
	).

% There can be no dependencies within a foreign_proc
dependency_graph__add_arcs_in_goal_2(
	foreign_proc(_, _, _, _, _, _, _), _, DepGraph, DepGraph).

dependency_graph__add_arcs_in_goal_2(shorthand(ShorthandGoal), Caller, 
		DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal_2_shorthand(ShorthandGoal, Caller,
			DepGraph0, DepGraph).


:- pred dependency_graph__add_arcs_in_goal_2_shorthand(shorthand_goal_expr,
		relation_key, dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_goal_2_shorthand(in, in, in, out) 
		is det.
		
dependency_graph__add_arcs_in_goal_2_shorthand(bi_implication(LHS, RHS),
		Caller, DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_list([LHS, RHS], Caller,
			DepGraph0, DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_list(list(hlds_goal), relation_key,
			dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_list(in, in, in, out) is det.

dependency_graph__add_arcs_in_list([], _Caller, DepGraph, DepGraph).
dependency_graph__add_arcs_in_list([Goal|Goals], Caller, DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_goal(Goal, Caller, DepGraph0, DepGraph1),
	dependency_graph__add_arcs_in_list(Goals, Caller, DepGraph1, DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_cases(list(case), relation_key,
			dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_cases(in, in, in, out) is det.

dependency_graph__add_arcs_in_cases([], _Caller, DepGraph, DepGraph).
dependency_graph__add_arcs_in_cases([case(Cons, Goal) | Goals], Caller,
						DepGraph0, DepGraph) :-
	dependency_graph__add_arcs_in_cons(Cons, Caller, DepGraph0, DepGraph1),
	dependency_graph__add_arcs_in_goal(Goal, Caller, DepGraph1, DepGraph2),
	dependency_graph__add_arcs_in_cases(Goals, Caller, DepGraph2, DepGraph).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__add_arcs_in_cons(cons_id, relation_key,
			dependency_graph, dependency_graph).
:- mode dependency_graph__add_arcs_in_cons(in, in, in, out) is det.

dependency_graph__add_arcs_in_cons(cons(_, _), _Caller,
				DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(int_const(_), _Caller,
				DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(string_const(_), _Caller,
				DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(float_const(_), _Caller,
				DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(pred_const(Pred, Proc, _), Caller,
				DepGraph0, DepGraph) :-
	(
			% If the node isn't in the relation, then
			% we didn't insert it because is was imported,
			% and we don't consider it.
		relation__search_element(DepGraph0, proc(Pred, Proc), Callee)
	->
		relation__add(DepGraph0, Caller, Callee, DepGraph)
	;
		DepGraph = DepGraph0
	).
dependency_graph__add_arcs_in_cons(code_addr_const(Pred, Proc), Caller,
				DepGraph0, DepGraph) :-
	(
			% If the node isn't in the relation, then
			% we didn't insert it because is was imported,
			% and we don't consider it.
		relation__search_element(DepGraph0, proc(Pred, Proc), Callee)
	->
		relation__add(DepGraph0, Caller, Callee, DepGraph)
	;
		DepGraph = DepGraph0
	).
dependency_graph__add_arcs_in_cons(type_ctor_info_const(_, _, _), _Caller,
				DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(base_typeclass_info_const(_, _, _, _),
				_Caller, DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(tabling_pointer_const(_, _),
				_Caller, DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(deep_profiling_proc_static(_),
				_Caller, DepGraph, DepGraph).
dependency_graph__add_arcs_in_cons(table_io_decl(_),
				_Caller, DepGraph, DepGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

dependency_graph__write_dependency_graph(ModuleInfo0, ModuleInfo) -->
	io__write_string("% Dependency graph\n"),
	{ module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo) },
	{ module_info_dependency_info(ModuleInfo, DepInfo) },
	io__write_string("\n\n% Dependency ordering\n"),
	write_graph(DepInfo, (pred(_::in, di, uo) is det --> []),
		(pred(Parent::in, Child::in, di, uo) is det -->
			{ Parent = proc(PPredId, PProcId) }, % Caller
			{ Child = proc(CPredId, CProcId) }, % Callee
			{ module_info_pred_proc_info(ModuleInfo, PPredId,
					PProcId, PPredInfo, PProcInfo) },
			{ module_info_pred_proc_info(ModuleInfo, CPredId,
					CProcId, CPredInfo, CProcInfo) },
			{ pred_info_name(PPredInfo, PName) },
			{ proc_info_declared_determinism(PProcInfo, PDet) },
			{ proc_info_argmodes(PProcInfo, PModes) },
			{ proc_info_context(PProcInfo, PContext) },

			{ pred_info_name(CPredInfo, CName) },
			{ proc_info_declared_determinism(CProcInfo, CDet) },
			{ proc_info_argmodes(CProcInfo, CModes) },
			{ proc_info_context(CProcInfo, CContext) },

			{ varset__init(ModeVarSet) },

			mercury_output_pred_mode_subdecl(ModeVarSet,
					unqualified(PName), PModes, PDet,
					PContext),
			io__write_string(" -> "),
			mercury_output_pred_mode_subdecl(ModeVarSet,
					unqualified(CName), CModes, CDet,
					CContext),
			io__write_string("\n")
		)).

%-----------------------------------------------------------------------------%

:- pred dependency_graph__write_dependency_ordering(list(list(pred_proc_id)),
				module_info, int, io__state, io__state).
:- mode dependency_graph__write_dependency_ordering(in, in, in, di, uo) is det.
dependency_graph__write_dependency_ordering([], _ModuleInfo, _N) -->
	io__write_string("\n").
dependency_graph__write_dependency_ordering([Clique | Rest], ModuleInfo, N) -->
	io__write_string("% Clique "),
	io__write_int(N),
	io__write_string("\n"),
	dependency_graph__write_clique(Clique, ModuleInfo),
	{ N1 is N + 1 },
	dependency_graph__write_dependency_ordering(Rest, ModuleInfo, N1).

:- pred dependency_graph__write_clique(list(pred_proc_id),
				module_info, io__state, io__state).
:- mode dependency_graph__write_clique(in, in, di, uo) is det.
dependency_graph__write_clique([], _ModuleInfo) --> [].
dependency_graph__write_clique([proc(PredId, ProcId) | Rest], ModuleInfo) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
						PredInfo, ProcInfo) },
	{ pred_info_name(PredInfo, Name) },
	{ proc_info_declared_determinism(ProcInfo, Det) },
	{ proc_info_argmodes(ProcInfo, Modes) },
	{ proc_info_context(ProcInfo, Context) },	
	{ varset__init(ModeVarSet) },

	io__write_string("% "),
	mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(Name),
						Modes, Det, Context),
	io__write_string("\n"),
	dependency_graph__write_clique(Rest, ModuleInfo).

%-----------------------------------------------------------------------------%

% dependency_graph__write_prof_dependency_graph:
%	Output's the static call graph of the current module in the form of
%		CallerLabel (\t) CalleeLabel
%
dependency_graph__write_prof_dependency_graph(ModuleInfo0, ModuleInfo) -->
	{ module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo) },
	{ module_info_dependency_info(ModuleInfo, DepInfo) },
	write_graph(DepInfo, (pred(_::in, di, uo) is det --> []),
		(pred(Parent::in, Child::in, di, uo) is det -->
			{ Parent = proc(PPredId, PProcId) }, % Caller
			{ Child = proc(CPredId, CProcId) }, % Callee
			dependency_graph__output_label(ModuleInfo,
					PPredId, PProcId),
			io__write_string("\t"),
			dependency_graph__output_label(ModuleInfo,
					CPredId, CProcId),
			io__write_string("\n")
		)).

%-----------------------------------------------------------------------------%

write_graph(DepInfo, WriteNode, WriteLink) -->
	{ hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph) },
	{ relation__domain(DepGraph, DomSet) },
	{ set__to_sorted_list(DomSet, DomList) },
	write_graph_nodes(DomList, DepGraph, WriteNode, WriteLink).

write_graph_nodes([], _Graph, _WriteNode, _WriteLink) --> [].
write_graph_nodes([Node | Nodes], Graph, WriteNode, WriteLink) -->
	WriteNode(Node),

	{ relation__lookup_element(Graph, Node, NodeKey) },
	{ relation__lookup_from(Graph, NodeKey, ChildrenSet) },
	{ set__to_sorted_list(ChildrenSet, Children) },

	write_graph_children(Children, Node, Graph, WriteLink),

	write_graph_nodes(Nodes, Graph, WriteNode, WriteLink).

:- pred write_graph_children(list(relation_key)::in, pred_proc_id::in,
	dependency_graph::in,
	pred(pred_proc_id, pred_proc_id, io__state, io__state)::
			pred(in, in, di, uo) is det,
	io__state::di, io__state::uo) is det.

write_graph_children([], _Parent, _Graph, _WriteLink) --> [].
write_graph_children([ChildKey | Children], Parent, Graph, WriteLink) -->
	{ relation__lookup_key(Graph, ChildKey, Child) },
	WriteLink(Parent, Child),
	write_graph_children(Children, Parent, Graph, WriteLink).

%-----------------------------------------------------------------------------%

% dependency_graph__output_label:
%	Prints out the label corresponding to PredId and ProcId.  
%
:- pred dependency_graph__output_label(module_info, pred_id, proc_id,
				io__state, io__state).
:- mode dependency_graph__output_label(in, in, in, di, uo) is det.

dependency_graph__output_label(ModuleInfo, PredId, ProcId) -->
	{ code_util__make_entry_label(ModuleInfo, PredId, ProcId, no,
		Address) },
        (
                { Address = label(local(ProcLabela)) }
        ->
                output_label(local(ProcLabela))
        ;
                { Address = imported(ProcLabelb) }
        ->
                output_proc_label(ProcLabelb)
        ;
                { Address = label(exported(ProcLabelc)) }
        ->
                output_label(exported(ProcLabelc))
        ;
                { error("dependency_graph__output_label: label not of type local or imported or exported\n") }
        ).

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

module_info_ensure_aditi_dependency_info(ModuleInfo0, ModuleInfo) :-
	module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo1),
	module_info_dependency_info(ModuleInfo1, DepInfo0),
	hlds_dependency_info_get_maybe_aditi_dependency_ordering(DepInfo0, 
		MaybeAditiInfo),
	( MaybeAditiInfo = yes(_) ->
		ModuleInfo = ModuleInfo1
	;	
		hlds_dependency_info_get_dependency_ordering(DepInfo0, 
			DepOrdering),
		aditi_scc_info_init(ModuleInfo1, AditiInfo0),
		dependency_graph__build_aditi_scc_info(DepOrdering, 
			AditiInfo0, AditiInfo),
		dependency_graph__merge_aditi_sccs(AditiInfo, AditiOrdering),
		hlds_dependency_info_set_aditi_dependency_ordering(DepInfo0,
			AditiOrdering, DepInfo),
		module_info_set_dependency_info(ModuleInfo1, 
			DepInfo, ModuleInfo)
	).

:- pred dependency_graph__build_aditi_scc_info(dependency_ordering::in, 
		aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__build_aditi_scc_info([]) --> [].
dependency_graph__build_aditi_scc_info([SCC | SCCs]) -->
	aditi_scc_info_get_module_info(ModuleInfo),
	(
		{ list__member(PredProcId, SCC) },
		{ PredProcId = proc(PredId, _) },
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ hlds_pred__pred_info_is_aditi_relation(PredInfo) },
		{ \+ hlds_pred__pred_info_is_base_relation(PredInfo) }
	->
		aditi_scc_info_add_scc(SCC, SCCs, SCCid),
		list__foldl(dependency_graph__process_aditi_pred_proc_id(SCCid),
			SCC)
	;
		[]
	),
	dependency_graph__build_aditi_scc_info(SCCs).	

:- pred dependency_graph__process_aditi_pred_proc_id(scc_id::in, 
	pred_proc_id::in, aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__process_aditi_pred_proc_id(SCCid, PredProcId) -->
	aditi_scc_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredProcId, 
		PredInfo, ProcInfo) },
	dependency_graph__process_aditi_proc_info(SCCid, PredInfo, ProcInfo).

:- pred dependency_graph__process_aditi_proc_info(scc_id::in, pred_info::in, 
	proc_info::in, aditi_scc_info::in, aditi_scc_info::out) is det.

dependency_graph__process_aditi_proc_info(CurrSCC, PredInfo, ProcInfo) -->
	(
		{ pred_info_is_exported(PredInfo) }
	->
		aditi_scc_info_add_no_merge_scc(CurrSCC)
	;
		{ pred_info_get_markers(PredInfo, Markers) },
		{ check_marker(Markers, context) }
	->
		% The context transformation can only be applied
		% to a single predicate SCC, so don't merge
		% other SCCs with a context-transformed SCC.
		aditi_scc_info_add_no_merge_scc(CurrSCC)
	;
		[]
	),
	{ proc_info_goal(ProcInfo, Goal) },
	process_aditi_goal(Goal).

%-----------------------------------------------------------------------------%

	% Go over the goal finding predicates called through negation
	% or aggregation. The SCCs containing those predicates cannot
	% be merged into the current SCC.
:- pred process_aditi_goal(hlds_goal::in, aditi_scc_info::in,
		aditi_scc_info::out) is det.

process_aditi_goal(Goal) -->
	 { multi_map__init(MMap0) },
	process_aditi_goal(no, Goal, MMap0, _).

:- pred process_aditi_goal(bool::in, hlds_goal::in,
	multi_map(prog_var, pred_proc_id)::in, 
	multi_map(prog_var, pred_proc_id)::out,
	aditi_scc_info::in, aditi_scc_info::out) is det.

process_aditi_goal(IsNeg, conj(Goals) - _, Map0, Map) -->
	list__foldl2(process_aditi_goal(IsNeg), Goals, Map0, Map).
process_aditi_goal(_IsNeg, par_conj(_) - _, _, _) -->
	{ error("process_aditi_goal - par_conj") }.
process_aditi_goal(IsNeg, disj(Goals) - _, Map0, Map) -->
	list__foldl2(process_aditi_goal(IsNeg), Goals, Map0, Map).
process_aditi_goal(IsNeg, switch(_, _, Cases) - _, Map0, Map) -->
	{ NegCallsInCases = 
	    lambda([Case::in, M0::in, M::out, AInfo0::in, AInfo::out] is det, (
		Case = case(_ConsId, Goal),
		process_aditi_goal(IsNeg, Goal, M0, M, AInfo0, AInfo)
	    )) },
	list__foldl2(NegCallsInCases, Cases, Map0, Map).
process_aditi_goal(IsNeg, if_then_else(_, Cond, Then, Else) - _, 
		Map0, Map) -->
	process_aditi_goal(yes, Cond, Map0, Map1),
	process_aditi_goal(IsNeg, Then, Map1, Map2),
	process_aditi_goal(IsNeg, Else, Map2, Map).
process_aditi_goal(IsNeg, some(_, _, Goal) - _, Map0, Map) -->
	process_aditi_goal(IsNeg, Goal, Map0, Map).
process_aditi_goal(_IsNeg, not(Goal) - _, Map0, Map) -->
	process_aditi_goal(yes, Goal, Map0, Map).
process_aditi_goal(IsNeg, call(PredId, ProcId, Args, _, _, _) - _, 
		Map0, Map) -->
	aditi_scc_info_handle_call(IsNeg, PredId, ProcId, Args, Map0, Map).

process_aditi_goal(_IsNeg, unify(Var, _, _, Unify, _) - _, 
		Map0, Map) -->
	(
		{ Unify = construct(_, pred_const(PredId, ProcId, _),
			_, _, _, _, _) }
	->
		aditi_scc_info_add_closure(Var, 
			proc(PredId, ProcId), Map0, Map)
	;
		{ Map = Map0 }
	).
process_aditi_goal(_IsNeg, generic_call(_, _, _, _) - _, 
		Map, Map) --> [].
process_aditi_goal(_IsNeg, foreign_proc(_, _, _, _, _, _, _) - _,
		Map, Map) --> [].
process_aditi_goal(_, shorthand(_) - _, _, _) -->
	% these should have been expanded out by now
	{ error("process_aditi_goal: unexpected shorthand") }.

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
		AddElement = lambda([Elem::in, Eqv0::in, Eqv::out] is det, (
				eqvclass__new_element(Eqv0, Elem, Eqv)	
			)),
		list__foldl(AddElement, SCCTsort, EqvSCCs0, EqvSCCs),
		dependency_graph__merge_aditi_sccs_2(SCCTsort, ModuleInfo,
			EqvSCCs, MergedSCCs, NoMerge, SCCRel,
			SCCPred, [], Ordering)
	;
		error("dependency_graph__merge_aditi_sccs: SCC dependency relation is cyclic")
	).

:- pred dependency_graph__merge_aditi_sccs_2(list(scc_id)::in,
	module_info::in, eqvclass(scc_id)::in, set(scc_id)::in,
	set(scc_id)::in, relation(scc_id)::in, scc_pred_map::in, 
	aditi_dependency_ordering::in, aditi_dependency_ordering::out) is det.

dependency_graph__merge_aditi_sccs_2([], _, _, _, _, _, _, Ordering, Ordering).
dependency_graph__merge_aditi_sccs_2([SCCid | SCCs0], ModuleInfo, EqvSCCs0, 
		MergedSCCs0, NoMergeSCCs, SCCRel, 
		SCCPreds, Ordering0, Ordering) :-
	(
		set__member(SCCid, MergedSCCs0)
	->
			% This SCC has been merged into its parent.
		Ordering1 = Ordering0,
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
		Ordering1 = [aditi_scc([SCC0], EntryPoints) | Ordering0],
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
		Ordering1 = [SCC | Ordering0]
	),
	dependency_graph__merge_aditi_sccs_2(SCCs, ModuleInfo, EqvSCCs,
		MergedSCCs0, NoMergeSCCs, SCCRel, SCCPreds,
		Ordering1, Ordering).

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

dependency_graph__do_merge_aditi_sccs(_, _, _, [], [], 
		_, _, Eqv, Eqv, SubModule, SubModule).
dependency_graph__do_merge_aditi_sccs(CurrSCCid, CalledSCCs, NoMergeSCCs,
		[LowerSCCid | LowerSCCs0], LowerSCCs, SCCPreds, SCCRel, 
		EqvSCCs0, EqvSCCs, SubModule0, SubModule) :-
	(
		set__member(LowerSCCid, CalledSCCs), 
		\+ set__member(LowerSCCid, NoMergeSCCs) 
	->
		relation__lookup_element(SCCRel, LowerSCCid, LowerSCCKey),
		relation__lookup_to(SCCRel, LowerSCCKey, CallingSCCKeys),
		set__to_sorted_list(CallingSCCKeys, CallingSCCKeyList),
		list__map(relation__lookup_key(SCCRel), 
			CallingSCCKeyList, CallingSCCs),
		( eqvclass__same_eqvclass_list(EqvSCCs0, CallingSCCs) ->

			%
			% All the calling SCCs have been merged (or 
			% there was only one to start with) so we
			% can safely merge this one in as well.
			%
			eqvclass__new_equivalence(EqvSCCs0, CurrSCCid, 
				LowerSCCid, EqvSCCs1),
			map__lookup(SCCPreds, LowerSCCid, LowerSCC),
			LowerSCC = LowerSCCPreds - _,

			%
			% The entry-points of the combined SCC cannot include
			% the entry-points of the lower SCC, since that 
			% would mean that the lower SCC was called from
			% multiple places and could not be merged.
			%
			SubModule0 = aditi_scc(CurrSCCPreds0, EntryPoints),
			SubModule1 = aditi_scc([LowerSCCPreds | CurrSCCPreds0], 
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
				LowerSCCs, SCCPreds, SCCRel, EqvSCCs1, EqvSCCs, 
				SubModule1, SubModule)
		;	
			dependency_graph__do_merge_aditi_sccs(CurrSCCid,
				CalledSCCs, NoMergeSCCs, LowerSCCs0, 
				LowerSCCs1, SCCPreds, SCCRel, 
				EqvSCCs0, EqvSCCs, SubModule0, SubModule),
			LowerSCCs = [LowerSCCid | LowerSCCs1]	
		)
	;
		dependency_graph__do_merge_aditi_sccs(CurrSCCid, CalledSCCs, 
			NoMergeSCCs, LowerSCCs0, LowerSCCs1, SCCPreds, SCCRel, 
			EqvSCCs0, EqvSCCs, SubModule0, SubModule),
		LowerSCCs = [LowerSCCid | LowerSCCs1]
	).

%-----------------------------------------------------------------------------%

:- type aditi_scc_info
	---> aditi_scc_info(
		module_info,
		map(pred_proc_id, scc_id),
		scc_pred_map,
		set(pred_proc_id),		% all local Aditi preds
		relation(scc_id),
		set(scc_id),			% SCCs which can't be merged
						% into their parents.
		scc_id				% current SCC.
	).

		% For each SCC, a list of all preds in SCC, and a list
		% of entry-points of the SCC.
:- type scc_pred_map ==	map(scc_id, pair(list(pred_proc_id))).

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
	Info = aditi_scc_info(Module, _, _, _, _, _, _).

:- pred aditi_scc_info_add_no_merge_scc(scc_id::in,
		aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_no_merge_scc(SCCid, Info0, Info) :-
	Info0 = aditi_scc_info(A, B, C, D, E, NoMerge0, G),
	set__insert(NoMerge0, SCCid, NoMerge),
	Info = aditi_scc_info(A, B, C, D, E, NoMerge, G).

:- pred aditi_scc_info_add_scc(list(pred_proc_id)::in, 
		dependency_ordering::in, scc_id::out,
		aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_scc(SCC, HigherSCCs, SCCid, Info0, Info) :-
	Info0 = aditi_scc_info(ModuleInfo, PredSCC0, SCCPred0, AditiPreds0, 
			SCCRel0, NoMerge, LastSCC),
	SCCid is LastSCC + 1,
	dependency_graph__get_scc_entry_points(SCC, HigherSCCs,
		ModuleInfo, EntryPoints),
	map__det_insert(SCCPred0, SCCid, SCC - EntryPoints, SCCPred),
	AddToMap = 
	    lambda([PredProcId::in, PS0::in, PS::out] is det, (
		map__det_insert(PS0, PredProcId, SCCid, PS)
	    )),
	list__foldl(AddToMap, SCC, PredSCC0, PredSCC),
	relation__add_element(SCCRel0, SCCid, _, SCCRel),
	set__insert_list(AditiPreds0, SCC, AditiPreds),	
	Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred, AditiPreds, 
			SCCRel, NoMerge, SCCid).

:- pred aditi_scc_info_handle_call(bool::in, pred_id::in, proc_id::in,
		list(prog_var)::in, multi_map(prog_var, pred_proc_id)::in, 
		multi_map(prog_var, pred_proc_id)::out, 
		aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_handle_call(IsNeg, PredId, ProcId, Args, 
		Map, Map, Info0, Info) :-
	Info0 = aditi_scc_info(ModuleInfo, PredSCC, SCCPred, AditiPreds, 
			SCCRel0, NoMerge0, SCCid),
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
		Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred, AditiPreds, 
			SCCRel, NoMerge, SCCid)
	;
		( hlds_pred__is_aditi_aggregate(ModuleInfo, PredId) ->
			handle_higher_order_args(Args, yes, SCCid, Map,
				PredSCC, SCCRel0, SCCRel, NoMerge0, NoMerge),
			Info = aditi_scc_info(ModuleInfo, PredSCC, SCCPred,
				AditiPreds, SCCRel, NoMerge, SCCid)
		;
			Info = Info0
		)
	).

	% An SCC cannot be merged into its parents if one of its
	% procedures is called as an aggregate query.
:- pred handle_higher_order_args(list(prog_var)::in, bool::in, scc_id::in,
	multi_map(prog_var, pred_proc_id)::in, map(pred_proc_id, scc_id)::in,
	relation(scc_id)::in, relation(scc_id)::out,
	set(scc_id)::in, set(scc_id)::out) is det.
	
handle_higher_order_args([], _, _, _, _, SCCRel, SCCRel, NoMerge, NoMerge).
handle_higher_order_args([Arg | Args], IsAgg, SCCid, Map, PredSCC, 
		SCCRel0, SCCRel, NoMerge0, NoMerge) :-
	( multi_map__search(Map, Arg, PredProcIds) ->
		list__foldl2(handle_higher_order_arg(PredSCC, IsAgg, SCCid),
			PredProcIds, SCCRel0, SCCRel1, NoMerge0, NoMerge1)
	;
		SCCRel1 = SCCRel0, 
		NoMerge1 = NoMerge0
	),
	handle_higher_order_args(Args, IsAgg, SCCid, Map, PredSCC, 
		SCCRel1, SCCRel, NoMerge1, NoMerge).

:- pred handle_higher_order_arg(map(pred_proc_id, scc_id)::in, bool::in,
		scc_id::in, pred_proc_id::in,
		relation(scc_id)::in, relation(scc_id)::out,
		set(scc_id)::in, set(scc_id)::out) is det.

handle_higher_order_arg(PredSCC, IsAgg, SCCid, PredProcId,
		SCCRel0, SCCRel, NoMerge0, NoMerge) :-
	( map__search(PredSCC, PredProcId, CalledSCCid) ->
		% Make sure anything called through an
		% aggregate is not merged into the current 
		% sub-module.
		( IsAgg = yes ->
			set__insert(NoMerge0, CalledSCCid, NoMerge)
		;
			NoMerge = NoMerge0
		),
		( CalledSCCid = SCCid ->
			SCCRel = SCCRel0
		;	
			relation__add_values(SCCRel0, SCCid,
				CalledSCCid, SCCRel)
		)
	;
		NoMerge = NoMerge0, 
		SCCRel = SCCRel0
	).

:- pred aditi_scc_info_add_closure(prog_var::in, pred_proc_id::in, 
		multi_map(prog_var, pred_proc_id)::in, 
		multi_map(prog_var, pred_proc_id)::out,
		aditi_scc_info::in, aditi_scc_info::out) is det.

aditi_scc_info_add_closure(Var, PredProcId, Map0, Map, Info, Info) :-
	Info = aditi_scc_info(_, _, _, AditiPreds, _, _, _),
	( set__member(PredProcId, AditiPreds) ->
		multi_map__set(Map0, Var, PredProcId, Map)
	;
		Map = Map0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
