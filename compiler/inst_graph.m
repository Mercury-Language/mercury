%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
:- module inst_graph.
:- interface.

:- import_module list, map, io.
:- import_module prog_data, hlds_data.

:- type inst_graph == map(prog_var, inst_graph__node).

:- type inst_graph__node
	--->	node(
			map(cons_id, list(prog_var)),
			maybe_parent
		).

:- type maybe_parent ---> top_level ; parent(prog_var).

	% Initialise an inst_graph.
	% Add a node for each variable.  Each node is initialised to have
	% no parents and no children.
:- pred inst_graph__init(list(prog_var), inst_graph).
:- mode inst_graph__init(in, out) is det.

	% inst_graph__set_parent(Parent, Child, Graph0, Graph)
	%	Set Parent as the parent node of Child.  Abort if
	%	Child already has a parent.
:- pred inst_graph__set_parent(prog_var, prog_var, inst_graph, inst_graph).
:- mode inst_graph__set_parent(in, in, in, out) is det.

	% top_level_node(InstGraph, VarA, VarB)
	%	Succeeds iff VarB is the top_level node reachable
	%	from VarA in InstGraph.
:- pred inst_graph__top_level_node(inst_graph, prog_var, prog_var).
:- mode inst_graph__top_level_node(in, in, out) is det.

:- pred inst_graph__decendant(inst_graph, prog_var, prog_var).
:- mode inst_graph__decendant(in, in, out) is nondet.

:- pred inst_graph__reachable(inst_graph, prog_var, prog_var).
:- mode inst_graph__reachable(in, in, out) is multi.

:- pred inst_graph__reachable_from_list(inst_graph, list(prog_var), prog_var).
:- mode inst_graph__reachable_from_list(in, in, out) is nondet.

:- pred inst_graph__corresponding_nodes(inst_graph, prog_var, prog_var,
		prog_var, prog_var).
:- mode inst_graph__corresponding_nodes(in, in, in, out, out) is multi.

:- pred inst_graph__corresponding_nodes(inst_graph, inst_graph, prog_var,
		prog_var, prog_var, prog_var).
:- mode inst_graph__corresponding_nodes(in, in, in, in, out, out) is multi.

:- pred inst_graph__corresponding_nodes_from_lists(inst_graph, inst_graph,
		list(prog_var), list(prog_var), prog_var, prog_var).
:- mode inst_graph__corresponding_nodes_from_lists(in, in, in, in, out, out)
		is nondet.

	% Merge two inst_graphs by renaming the variables in the second
	% inst_graph.  Also return the variable substitution map.
:- pred inst_graph__merge(inst_graph, prog_varset, inst_graph, prog_varset,
		inst_graph, prog_varset, map(prog_var, prog_var)).
:- mode inst_graph__merge(in, in, in, in, out, out, out) is det.

/*
	% Join two inst_graphs together by taking the maximum unrolling
	% of the type tree of each variable from the two graphs.
:- pred inst_graph__join(inst_graph, prog_varset, inst_graph, prog_varset,
		inst_graph, prog_varset).
:- mode inst_graph__join(in, in, in, in, out, out) is det.
*/

:- pred inst_graph__dump(inst_graph, prog_varset, io__state, io__state).
:- mode inst_graph__dump(in, in, di, uo) is det.

	% XXX this should probably go in list.m.
:- pred corresponding_members(list(T), list(U), T, U).
:- mode corresponding_members(in, in, out, out) is nondet.

	% inst_graph_info contains all the inst_graph-related info for a
	% predicate that needs to be stored in the pred_info.
:- type inst_graph_info.

:- func interface_inst_graph(inst_graph_info) = inst_graph.
:- func 'interface_inst_graph :='(inst_graph_info, inst_graph) = inst_graph_info.

:- func interface_vars(inst_graph_info) = list(prog_var).
:- func 'interface_vars :='(inst_graph_info, list(prog_var)) =
		inst_graph_info.

:- func interface_varset(inst_graph_info) = prog_varset.
:- func 'interface_varset :='(inst_graph_info, prog_varset) = inst_graph_info.

:- func implementation_inst_graph(inst_graph_info) = inst_graph.
:- func 'implementation_inst_graph :='(inst_graph_info, inst_graph) =
		inst_graph_info.

:- func inst_graph_info_init = inst_graph_info.

:- implementation.

:- import_module require, set, varset, term, std_util.
:- import_module hlds_out, term_io.

inst_graph__init(Vars, Graph) :-
	map__init(Graph0),
	map__init(Empty),
	list__foldl((pred(V::in, G0::in, G::out) is det :-
			map__det_insert(G0, V, node(Empty, top_level), G)),
		Vars, Graph0, Graph).

inst_graph__set_parent(Parent, Child, InstGraph0, InstGraph) :-
	map__lookup(InstGraph0, Child, node(Functors, MaybeParent0)),
	( MaybeParent0 = top_level ->
		map__det_update(InstGraph0, Child,
			node(Functors, parent(Parent)), InstGraph)
	;
		error("inst_graph__set_parent: node already has parent")
	).

inst_graph__top_level_node(InstGraph, Var, TopLevel) :-
	map__lookup(InstGraph, Var, node(_, MaybeParent)),
	(
		MaybeParent = parent(Parent),
		inst_graph__top_level_node(InstGraph, Parent, TopLevel)
	;
		MaybeParent = top_level,
		TopLevel = Var
	).

inst_graph__decendant(InstGraph, Var, Descendant) :-
	set__init(Seen),
	inst_graph__decendant_2(InstGraph, Seen, Var, Descendant).

:- pred inst_graph__decendant_2(inst_graph, set(prog_var), prog_var, prog_var).
:- mode inst_graph__decendant_2(in, in, in, out) is nondet.

inst_graph__decendant_2(InstGraph, Seen, Var, Descendant) :-
	map__lookup(InstGraph, Var, node(Functors, _)),
	map__member(Functors, _ConsId, Args),
	list__member(Arg, Args),
	(
		Descendant = Arg
	;
		( Arg `member` Seen ->
			fail
		;
			inst_graph__decendant_2(InstGraph, Seen `insert` Arg,
				Arg, Descendant)
		)
	).

inst_graph__reachable(_InstGraph, Var, Var).
inst_graph__reachable(InstGraph, Var, Reachable) :-
	inst_graph__decendant(InstGraph, Var, Reachable).

inst_graph__reachable_from_list(InstGraph, Vars, Reachable) :-
	list__member(Var, Vars),
	inst_graph__reachable(InstGraph, Var, Reachable).

inst_graph__corresponding_nodes(InstGraph, A, B, V, W) :-
	inst_graph__corresponding_nodes(InstGraph, InstGraph, A, B, V, W).

inst_graph__corresponding_nodes(InstGraphA, InstGraphB, A, B, V, W) :-
	inst_graph__corresponding_nodes_2(InstGraphA, InstGraphB,
		set__init, set__init, A, B, V, W).

:- pred inst_graph__corresponding_nodes_2(inst_graph, inst_graph, set(prog_var),
		set(prog_var), prog_var, prog_var, prog_var, prog_var).
:- mode inst_graph__corresponding_nodes_2(in, in, in, in, in, in, out, out)
		is multi.

inst_graph__corresponding_nodes_2(_, _, _, _, A, B, A, B).
inst_graph__corresponding_nodes_2(InstGraphA, InstGraphB, SeenA0, SeenB0,
		A, B, V, W) :-
	not ( A `member` SeenA0, B `member` SeenB0 ),

	map__lookup(InstGraphA, A, node(FunctorsA, _)),
	map__lookup(InstGraphB, B, node(FunctorsB, _)),

	SeenA = SeenA0 `insert` A,
	SeenB = SeenB0 `insert` B,

	( map__member(FunctorsA, ConsId, ArgsA) ->
		( map__is_empty(FunctorsB) ->
			list__member(V0, ArgsA),
			inst_graph__corresponding_nodes_2(InstGraphA,
				InstGraphB, SeenA, SeenB, V0, B, V, W)
		;
			map__search(FunctorsB, ConsId, ArgsB),
			corresponding_members(ArgsA, ArgsB, V0, W0),
			inst_graph__corresponding_nodes_2(InstGraphA,
				InstGraphB, SeenA, SeenB, V0, W0, V, W)
		)
	;
		map__member(FunctorsB, _ConsId, ArgsB),
		list__member(W0, ArgsB),
		inst_graph__corresponding_nodes_2(InstGraphA, InstGraphB,
			SeenA, SeenB, A, W0, V, W)
	).

inst_graph__corresponding_nodes_from_lists(InstGraphA, InstGraphB,
		VarsA, VarsB, V, W) :-
	corresponding_members(VarsA, VarsB, A, B),
	inst_graph__corresponding_nodes(InstGraphA, InstGraphB, A, B, V, W).

corresponding_members([A | _], [B | _], A, B).
corresponding_members([_ | As], [_ | Bs], A, B) :-
	corresponding_members(As, Bs, A, B).


inst_graph__merge(InstGraph0, VarSet0, NewInstGraph, NewVarSet, InstGraph,
		VarSet, Sub) :-
	varset__merge_subst_without_names(VarSet0, NewVarSet, VarSet, Sub0),
	(
	    map__map_values(
		pred(_::in, term__variable(V)::in, V::out) is semidet,
		Sub0, Sub1)
	->
	    Sub = Sub1
	;
	    error("inst_graph__merge: non-variable terms in substitution")
	),
	map__foldl((pred(Var0::in, Node0::in, IG0::in, IG::out) is det :-
		Node0 = node(Functors0, MaybeParent),
		map__map_values((pred(_::in, Args0::in, Args::out) is det :-
		    map__apply_to_list(Args0, Sub, Args)), Functors0, Functors),
		Node = node(Functors, MaybeParent),
		map__lookup(Sub, Var0, Var),
		map__det_insert(IG0, Var, Node, IG)
	    ), NewInstGraph, InstGraph0, InstGraph).

/*
inst_graph__join(InstGraphA, VarSetA, InstGraphB, VarSetB,
		InstGraph, VarSet) :-
	solutions((pred(V::out) is nondet :-
			map__member(InstGraphB, V, node(_, top_level))
		), VarsB),
	list__foldl2(join_nodes(InstGraphB, VarSetB), VarsB, InstGraphA,
		InstGraph, VarSetA, VarSet).

:- pred join_nodes(inst_graph, prog_varset, prog_var, inst_graph, inst_graph,
		prog_varset, prog_varset).
:- mode join_nodes(in, in, in, in, out, in, out) is det.

join_nodes(_, _, _, _, _, _, _) :- error("join_nodes: NYI").
*/


inst_graph__dump(InstGraph, VarSet) -->
	map__foldl((pred(Var::in, Node::in, di, uo) is det -->
		{ Node = node(Functors, MaybeParent) },
		io__write_string("%% "),
		term_io__write_variable(Var, VarSet),
		io__write_string(": "),
		( { MaybeParent = parent(Parent) } ->
		    term_io__write_variable(Parent, VarSet)
		;
		    []
		),
		io__nl,

		map__foldl((pred(ConsId::in, Args::in, di, uo) is det -->
			io__write_string("%%\t"),
			hlds_out__write_cons_id(ConsId),
			( { Args \= [] } ->
			    io__write_char('('),
			    io__write_list(Args, ", ", 
				(pred(Arg::in, di, uo) is det -->
				    term_io__write_variable(Arg, VarSet))),
			    io__write_char(')')
			;
			    []
			),
			io__nl
		    ), Functors)
	    ), InstGraph).

:- type inst_graph_info
	--->	inst_graph_info(
			interface_inst_graph		:: inst_graph,
					% Inst graph derived from the mode
					% declarations, if there are any.
					% If there are no mode declarations
					% for the pred, this is the same as
					% the implementation_inst_graph.
			interface_vars			:: list(prog_var),
					% Vars that appear in the head of the
					% mode declaration constraint.
			interface_varset		:: prog_varset,
					% Varset used for inteface_inst_graph.
			implementation_inst_graph	:: inst_graph
					% Inst graph derived from the body of
					% the predicate.
		).

inst_graph_info_init = inst_graph_info(IG, [], VS, IG) :-
	varset__init(VS),
	map__init(IG).
