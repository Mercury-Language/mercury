%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_tree.m
% Author: Mark Brown
%
% This module defines an instance of mercury_edt/2, the debugging tree.
%
%-----------------------------------------------------------------------------%

:- module mdb__declarative_tree.
:- interface.
:- import_module mdb__declarative_analyser, mdb__declarative_execution.

	% The type of nodes in our implementation of EDTs.  The parameter
	% is meant to be the type of references to trace nodes.  In
	% particular, the references should be to trace nodes that could
	% be considered nodes in the EDT, namely those for exit, fail
	% and exception events.
	% 
:- type edt_node(R)
	--->	dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R).

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(S) ---> wrap(S).

:- pred edt_subtree_details(S, edt_node(R), event_number, sequence_number)
		<= annotated_trace(S, R).
:- mode edt_subtree_details(in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_debugger, mdb__io_action.
:- import_module mdb__program_representation.
:- import_module assoc_list, bool, exception, int, list, map, std_util.

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R)
	where [
		pred(edt_root_question/4) is trace_root_question,
		pred(edt_root_e_bug/4) is trace_root_e_bug,
		pred(edt_children/3) is trace_children,
		pred(edt_dependency/6) is trace_dependency
	].

%-----------------------------------------------------------------------------%

:- func exit_node_decl_atom(io_action_map::in, S::in,
	trace_node(R)::in(trace_node_exit)) = (final_decl_atom::out) is det
	<= annotated_trace(S, R).

exit_node_decl_atom(IoActionMap, Store, ExitNode) = DeclAtom :-
	ExitAtom = ExitNode ^ exit_atom,
	CallId = ExitNode ^ exit_call,
	call_node_from_id(Store, CallId, Call),
	CallIoSeq = Call ^ call_io_seq_num,
	ExitIoSeq = ExitNode ^ exit_io_seq_num,
	IoActions = make_io_actions(IoActionMap, CallIoSeq, ExitIoSeq),
	DeclAtom = final_decl_atom(ExitAtom, IoActions).

:- func call_node_decl_atom(S, R) = init_decl_atom <= annotated_trace(S, R).

call_node_decl_atom(Store, CallId) = DeclAtom :-
	call_node_from_id(Store, CallId, CallNode),
	CallAtom = CallNode ^ call_atom,
	DeclAtom = init_decl_atom(CallAtom).

:- func make_io_actions(io_action_map, int, int) = list(io_action).

make_io_actions(IoActionMap, InitIoSeq, ExitIoSeq) =
	( InitIoSeq = ExitIoSeq ->
		[]
	;
		[map__lookup(IoActionMap, InitIoSeq) |
			make_io_actions(IoActionMap, InitIoSeq + 1, ExitIoSeq)]
	).

%-----------------------------------------------------------------------------%

:- pred trace_root_question(io_action_map::in, wrap(S)::in, edt_node(R)::in,
	decl_question(edt_node(R))::out) is det <= annotated_trace(S, R).

trace_root_question(IoActionMap, wrap(Store), dynamic(Ref), Root) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = fail(_, CallId, RedoId, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		get_answers(IoActionMap, Store, RedoId, [], Answers),
		Root = missing_answer(dynamic(Ref), DeclAtom, Answers)
	;
		Node = exit(_, _, _, _, _, _),
		DeclAtom = exit_node_decl_atom(IoActionMap, Store, Node),
		Root = wrong_answer(dynamic(Ref), DeclAtom)
	;
		Node = excp(_, CallId, _, Exception, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Root = unexpected_exception(dynamic(Ref), DeclAtom, Exception)
	).

:- pred get_answers(io_action_map::in, S::in, R::in,
	list(final_decl_atom)::in, list(final_decl_atom)::out) is det
	<= annotated_trace(S, R).

get_answers(IoActionMap, Store, RedoId, DeclAtoms0, DeclAtoms) :-
	(
		maybe_redo_node_from_id(Store, RedoId, redo(_, ExitId))
	->
		exit_node_from_id(Store, ExitId, ExitNode),
		NextId = ExitNode ^ exit_prev_redo,
		DeclAtom = exit_node_decl_atom(IoActionMap, Store, ExitNode),
		get_answers(IoActionMap, Store, NextId,
			[DeclAtom | DeclAtoms0], DeclAtoms)
	;
		DeclAtoms = DeclAtoms0
	).

:- pred trace_root_e_bug(io_action_map::in, wrap(S)::in, edt_node(R)::in,
	decl_e_bug::out) is det <= annotated_trace(S, R).

trace_root_e_bug(IoActionMap, wrap(Store), dynamic(Ref), Bug) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, _, _, _, Event, _),
		DeclAtom = exit_node_decl_atom(IoActionMap, Store, Node),
		Bug = incorrect_contour(DeclAtom, unit, Event)
	;
		Node = fail(_, CallId, _, Event),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Bug = partially_uncovered_atom(DeclAtom, Event)
	;
		Node = excp(_, CallId, _, Exception, Event),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Bug = unhandled_exception(DeclAtom, Exception, Event)
	).

:- pred trace_children(wrap(S), edt_node(R), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode trace_children(in, in, out) is semidet.

trace_children(wrap(Store), dynamic(Ref), Children) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = fail(PrecId, CallId, _, _),
		not_at_depth_limit(Store, CallId),
		missing_answer_children(Store, PrecId, CallId, [], Children)
	;
		Node = exit(PrecId, CallId, _, _, _, _),
		not_at_depth_limit(Store, CallId),
		wrong_answer_children(Store, PrecId, CallId, [], Children)
	;
		Node = excp(PrecId, CallId, _, _, _),
		not_at_depth_limit(Store, CallId),
		unexpected_exception_children(Store, PrecId, CallId, [],
			Children)
	).

:- pred not_at_depth_limit(S, R) <= annotated_trace(S, R).
:- mode not_at_depth_limit(in, in) is semidet.

not_at_depth_limit(Store, Ref) :-
	call_node_from_id(Store, Ref, CallNode),
	CallNode ^ call_at_max_depth = no.

:- pred wrong_answer_children(S, R, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode wrong_answer_children(in, in, in, in, out) is det.

wrong_answer_children(Store, NodeId, StartId, Ns0, Ns) :-
	(
		NodeId = StartId
	->
		Ns = Ns0
	;
		wrong_answer_children_2(Store, NodeId, StartId, Ns0, Ns)
	).

:- pred wrong_answer_children_2(S, R, R, list(edt_node(R)),
	list(edt_node(R))) <= annotated_trace(S, R).
:- mode wrong_answer_children_2(in, in, in, in, out) is det.

wrong_answer_children_2(Store, NodeId, StartId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
		throw(internal_error("wrong_answer_children_2",
			"unexpected start of contour"))
	;
		Node = excp(_, _, _, _, _)
	->
		throw(unimplemented_feature("code that catches exceptions"))
	;
		Node = exit(_, _, _, _, _, _)
	->
			%
			% Add a child for this node.
			%
		Ns1 = [dynamic(NodeId) | Ns0]
	;
		Node = fail(_, CallId, _, _)
	->
			%
			% Fail events can be reached here if there
			% were events missing due to a parent being
			% shallow traced.  In this case, we can't tell
			% whether the call was in a negated context
			% or backtracked over, so we have to assume
			% the former.
			%
			% Fail events can also be reached here if the
			% parent was a variant of solutions/2.
			%
			% If this really is in a negated context, the start of
			% the context would be just before the entry to this
			% failed call, modulo any det/semidet code which
			% succeeded.
			%
		call_node_from_id(Store, CallId, Call),
		NestedStartId = Call ^ call_preceding,
		missing_answer_children(Store, NodeId, NestedStartId, Ns0, Ns1)
	;
		Node = neg_fail(Prec, NestedStartId)
	->
			%
			% There is a nested context.  Neg_fail events can be
			% reached here if there were events missing due to a
			% parent being shallow traced.  In this case, we can't
			% tell whether the call was in a negated context or
			% backtracked over, so we have to assume the former.
			%
		wrong_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
		( Node = else(Prec, NestedStartId)
		; Node = neg_succ(Prec, NestedStartId)
		)
	->
			%
			% There is a nested context.
			%
		missing_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
			%
			% This handles the following cases:
			% redo, switch, first_disj, later_disj, and
			% then.  Also handles cond when the status is
			% anything other than failed.
			%
			% Redo events can be reached here if there
			% were missing events due to a shallow tracing.
			% In this case, we have to scan over the entire
			% previous contour, since there is no way to
			% tell how much of it was backtracked over.
			%
		Ns1 = Ns0
	),
	Next = step_left_in_contour(Store, Node),
	wrong_answer_children(Store, Next, StartId, Ns1, Ns).

:- pred missing_answer_children(S, R, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode missing_answer_children(in, in, in, in, out) is det.

missing_answer_children(Store, NodeId, StartId, Ns0, Ns) :-
	(
		NodeId = StartId
	->
		Ns = Ns0
	;
		missing_answer_children_2(Store, NodeId, StartId, Ns0, Ns)
	).

:- pred missing_answer_children_2(S, R, R, list(edt_node(R)), list(edt_node(R)))
	<= annotated_trace(S, R).
:- mode missing_answer_children_2(in, in, in, in, out) is det.

missing_answer_children_2(Store, NodeId, StartId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
		throw(internal_error("missing_answer_children_2",
			"unexpected start of contour"))
	;
		Node = excp(_, _, _, _, _)
	->
		throw(unimplemented_feature("code that catches exceptions"))
	;
		( Node = exit(_, _, _, _, _, _)
		; Node = fail(_, _, _, _)
		)
	->
			%
			% Add a child for this node.
			%
		Ns1 = [dynamic(NodeId) | Ns0]
	;
		Node = neg_fail(Prec, NestedStartId)
	->
			%
			% There is a nested successful context.
			%
		wrong_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
		( Node = else(Prec, NestedStartId)
		; Node = neg_succ(Prec, NestedStartId)
		)
	->
			%
			% There is a nested failed context.
			%
		missing_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
			%
			% This handles the following cases:
			% redo, switch, first_disj, later_disj and
			% then.  Also handles cond when the status
			% is anything other than failed.
			%
		Ns1 = Ns0
	),
	Next = step_in_stratum(Store, Node),
	missing_answer_children(Store, Next, StartId, Ns1, Ns).

:- pred unexpected_exception_children(S, R, R, list(edt_node(R)),
		list(edt_node(R))) <= annotated_trace(S, R).
:- mode unexpected_exception_children(in, in, in, in, out) is det.

unexpected_exception_children(Store, NodeId, StartId, Ns0, Ns) :-
	(
		NodeId = StartId
	->
		Ns = Ns0
	;
		unexpected_exception_children_2(Store, NodeId, StartId, Ns0, Ns)
	).

:- pred unexpected_exception_children_2(S, R, R, list(edt_node(R)),
	list(edt_node(R))) <= annotated_trace(S, R).
:- mode unexpected_exception_children_2(in, in, in, in, out) is det.

unexpected_exception_children_2(Store, NodeId, StartId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, failed)
		; Node = cond(_, _, failed)
		)
	->
		throw(internal_error("unexpected_exception_children_2",
			"unexpected start of contour"))
	;
		( Node = exit(_, _, _, _, _, _)
		; Node = excp(_, _, _, _, _)
		)
	->
			%
			% Add a child for this node.
			%
		Ns1 = [dynamic(NodeId) | Ns0]
	;
		Node = fail(_, CallId, _, _)
	->
			%
			% Fail events can be reached here if there
			% were events missing due to a parent being
			% shallow traced.  In this case, we can't tell
			% whether the call was in a negated context
			% or backtracked over, so we have to assume
			% the former.
			%
			% Fail events can also be reached here if the
			% parent was a variant of solutions/2.
			%
			% If this really is in a negated context, the start of
			% the context would be just before the entry to this
			% failed call, modulo any det/semidet code which
			% succeeded.
			%
		call_node_from_id(Store, CallId, Call),
		NestedStartId = Call ^ call_preceding,
		missing_answer_children(Store, NodeId, NestedStartId, Ns0, Ns1)
	;
		Node = neg_fail(Prec, NestedStartId)
	->
			%
			% There is a nested context.  Neg_fail events can be
			% reached here if there were events missing due to a
			% parent being shallow traced.  In this case, we can't
			% tell whether the call was in a negated context or
			% backtracked over, so we have to assume the former.
			%
		wrong_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
		( Node = else(Prec, NestedStartId)
		; Node = neg_succ(Prec, NestedStartId)
		)
	->
			%
			% There is a nested context.
			%
		missing_answer_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
			%
			% This handles the following cases:
			% redo, switch, first_disj, later_disj, and
			% then.  Also handles neg and cond when the
			% status is anything other than failed.
			%
			% Redo events can be reached here if there
			% were missing events due to a shallow tracing.
			% In this case, we have to scan over the entire
			% previous contour, since there is no way to
			% tell how much of it was backtracked over.
			%
		Ns1 = Ns0
	),
	Next = step_left_in_contour(Store, Node),
	unexpected_exception_children(Store, Next, StartId, Ns1, Ns).

%-----------------------------------------------------------------------------%
%
% Tracking a subterm dependency.
%
% We are given an EDT node, an argument position, and a path to the selected
% subterm.  We wish to find the origin of that subterm within the body of the
% given node, or within the body of its parent.  We can figure out the mode of
% the top of the selected subterm.
%
% If the mode is `in', the origin could be:
%	- a primitive (unification of foreign_proc) within the body of the
%	  parent,
%	- an output subterm in a sibling node, or
%	- an input subterm of the parent node.
% In this case we look at the contour leading up to the call event associated
% with the given node. This contour will be wholly within the parent call.
%
% If the mode is `out', the origin could be:
%	- a primitive (unification or foreign_proc) within the body of the
%	  call,
%	- an output subterm of a child of the node, or
%	- an input subterm of the node itself.
% In this case we look at the contour leading up to the exit or exception event
% associated with the given node. This contour will be wholly within the
% current call.
%
% Our algorithm for finding the origin has three phases.
%
% In the first phase, we materialize a list of the nodes in the contour.
%
% In the second phase, we use this list of nodes to construct a list of the
% primitive goals along that contour in the body of the relevant procedure,
% leading up to either the call event (if subterm_mode is `in') or the exit
% event (if subterm_mode is `out').
%
% In the third phase, we traverse the list of primitive goals backwards, from
% the most recently executed primitive to the earliest one, keeping track of
% the variable which contains the selected subterm, and the location within
% this variable.

:- type dependency_chain_start(R)
	--->	chain_start(
			start_loc(R),
			int,		% The argument number of the selected
					% position in the full list of
					% arguments, including the
					% compiler-generated ones.
			R,		% The id of the node preceding the exit
					% node, if start_loc is cur_goal
					% and the id of the node preceding the
					% call node if start_loc is
					% parent_goal.
			maybe(goal_path),
					% No if start_loc is cur_goal;
					% and yes wrapped around the goal path
					% of the call in the parent procedure
					% if start_loc is parent_goal.
			maybe(proc_rep)	% The body of the procedure indicated
					% by start_loc.
		).

:- type start_loc(R)
	--->	cur_goal
	;	parent_goal(R, trace_node(R)).

:- type goal_and_path	--->	goal_and_path(goal_rep, goal_path).

:- type goal_and_path_list ==	list(goal_and_path).

:- type annotated_primitive(R)
	--->	primitive(
			string,		% filename
			int,		% line number
			list(var_rep),	% vars bound by the atomic goal
			atomic_goal_rep,% the atomic goal itself
			goal_path,	% its goal path
			maybe(R)
					% if the atomic goal is a call,
					% the id of the call's exit event
		).

:- pred trace_dependency(wrap(S)::in, edt_node(R)::in,
	arg_pos::in, term_path::in, subterm_mode::out,
	subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

trace_dependency(wrap(Store), dynamic(Ref), ArgPos, TermPath, Mode, Origin) :-
	find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart),
	ChainStart = chain_start(StartLoc, ArgNum, NodeId, StartPath,
		MaybeProcRep),
	Mode = start_loc_to_subterm_mode(StartLoc),
	(
		MaybeProcRep = no,
		Origin = not_found
	;
		MaybeProcRep = yes(ProcRep),
		det_trace_node_from_id(Store, NodeId, Node),
		materialize_contour(Store, NodeId, Node, [], Contour0),
		(
			StartLoc = parent_goal(CallId, CallNode),
			Contour = list__append(Contour0, [CallId - CallNode])
		;
			StartLoc = cur_goal,
			Contour = Contour0
		),
		ProcRep = proc_rep(HeadVars, GoalRep),
		make_primitive_list(Store, [goal_and_path(GoalRep, [])],
			Contour, StartPath, ArgNum, HeadVars, Var,
			[], Primitives),
		traverse_primitives(Primitives, Var, TermPath,
			Store, ProcRep, Origin)
	).

:- pred find_chain_start(S::in, R::in, arg_pos::in, term_path::in,
	dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, CallId, _, ExitAtom, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = CallNode ^ call_atom,
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		; trace_atom_subterm_is_ground(ExitAtom, ArgPos, TermPath) ->
			find_chain_start_outside(CallNode, Node, ArgPos,
				ChainStart)
		;
			throw(internal_error("find_chain_start",
				"unbound wrong answer term"))
		)
	;
		Node = fail(_, CallId, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = CallNode ^ call_atom,
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		;
			throw(internal_error("find_chain_start",
				"unbound missing answer term"))
		)
	;
		Node = excp(_, CallId, _, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = CallNode ^ call_atom,
		%
		% XXX we don't yet handle tracking of the exception value.
		%
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		;
			throw(internal_error("find_chain_start",
				"unbound exception term"))
		)
	).

:- pred find_chain_start_inside(S::in, R::in,
	trace_node(R)::in(trace_node_call), arg_pos::in,
	dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start_inside(Store, CallId, CallNode, ArgPos, ChainStart) :-
	CallPrecId = CallNode ^ call_preceding,
	CallAtom = CallNode ^ call_atom,
	CallPathStr = CallNode ^ call_goal_path,
	path_from_string_det(CallPathStr, CallPath),
	StartLoc = parent_goal(CallId, CallNode),
	absolute_arg_num(ArgPos, CallAtom, ArgNum),
	StartId = CallPrecId,
	StartPath = yes(CallPath),
	parent_proc_rep(Store, CallId, StartRep),
	ChainStart = chain_start(StartLoc, ArgNum, StartId, StartPath,
		StartRep).

:- pred find_chain_start_outside(trace_node(R)::in(trace_node_call),
	trace_node(R)::in(trace_node_exit), arg_pos::in,
	dependency_chain_start(R)::out) is det.

find_chain_start_outside(CallNode, ExitNode, ArgPos, ChainStart) :-
	StartLoc = cur_goal,
	ExitAtom = ExitNode ^ exit_atom,
	absolute_arg_num(ArgPos, ExitAtom, ArgNum),
	StartId = ExitNode ^ exit_preceding,
	StartPath = no,
	StartRep = CallNode ^ call_proc_rep,
	ChainStart = chain_start(StartLoc, ArgNum, StartId,
		StartPath, StartRep).

:- pred parent_proc_rep(S::in, R::in, maybe(proc_rep)::out)
	is det <= annotated_trace(S, R).

parent_proc_rep(Store, CallId, ProcRep) :-
	call_node_from_id(Store, CallId, Call),
	CallPrecId = Call ^ call_preceding,
	( trace_node_from_id(Store, CallPrecId, CallPrecNode) ->
		step_left_to_call(Store, CallPrecNode, ParentCallNode),
		ProcRep = ParentCallNode ^ call_proc_rep
	;
		% The parent call is outside the annotated trace.
		ProcRep = no
	).

:- pred step_left_to_call(S::in, trace_node(R)::in,
	trace_node(R)::out(trace_node_call)) is det <= annotated_trace(S, R).

step_left_to_call(Store, Node, ParentCallNode) :-
	( Node = call(_, _, _, _, _, _, _, _, _) ->
		ParentCallNode = Node
	;
		( Node = neg(NegPrec, _, _) ->
			PrevNodeId = NegPrec
		;
			PrevNodeId = step_left_in_contour(Store, Node)
		),
		det_trace_node_from_id(Store, PrevNodeId, PrevNode),
		step_left_to_call(Store, PrevNode, ParentCallNode)
	).

:- pred materialize_contour(S::in, R::in, trace_node(R)::in,
	assoc_list(R, trace_node(R))::in, assoc_list(R, trace_node(R))::out)
	is det <= annotated_trace(S, R).

materialize_contour(Store, NodeId, Node, Nodes0, Nodes) :-
	( Node = call(_, _, _, _, _, _, _, _, _) ->
		Nodes = Nodes0
	;
		( Node = neg(NegPrec, _, _) ->
			PrevNodeId = NegPrec
		;
			PrevNodeId = step_left_in_contour(Store, Node)
		),
		det_trace_node_from_id(Store, PrevNodeId, PrevNode),
		( Node = then(_, _) ->
			% The cond node is enough to tell us which way the
			% if-then-else went; the then node would just
			% complicate the job of make_primitive_list.
			Nodes1 = Nodes0
		;
			Nodes1 = [NodeId - Node | Nodes0]
		),
		materialize_contour(Store, PrevNodeId, PrevNode,
			Nodes1, Nodes)
	).

:- pred make_primitive_list(S::in, goal_and_path_list::in,
	assoc_list(R, trace_node(R))::in, maybe(goal_path)::in,
	int::in, list(var_rep)::in, var_rep::out,
	list(annotated_primitive(R))::in, list(annotated_primitive(R))::out)
	is det <= annotated_trace(S, R).

make_primitive_list(Store, [goal_and_path(Goal, Path) | GoalPaths],
		Contour, MaybeEnd, ArgNum, HeadVars, Var,
		Primitives0, Primitives) :-
	(
		Goal = conj_rep(Conjs),
		add_paths_to_conjuncts(Conjs, Path, 1, ConjPaths),
		make_primitive_list(Store, list__append(ConjPaths, GoalPaths),
			Contour, MaybeEnd, ArgNum, HeadVars, Var,
			Primitives0, Primitives)
	;
		Goal = disj_rep(Disjs),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			( ContourHeadNode = first_disj(_, DisjPathStr)
			; ContourHeadNode = later_disj(_, DisjPathStr, _)
			),
			path_from_string_det(DisjPathStr, DisjPath),
			list__append(Path, PathTail, DisjPath),
			PathTail = [disj(N)]
		->
			list__index1_det(Disjs, N, Disj),
			DisjAndPath = goal_and_path(Disj, DisjPath),
			make_primitive_list(Store, [DisjAndPath | GoalPaths],
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			throw(internal_error("make_primitive_list",
				"mismatch on disj"))
		)
	;
		Goal = switch_rep(Arms),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = switch(_, ArmPathStr),
			path_from_string_det(ArmPathStr, ArmPath),
			list__append(Path, PathTail, ArmPath),
			PathTail = [switch(N)]
		->
			list__index1_det(Arms, N, Arm),
			ArmAndPath = goal_and_path(Arm, ArmPath),
			make_primitive_list(Store, [ArmAndPath | GoalPaths],
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			throw(internal_error("make_primitive_list",
				"mismatch on switch"))
		)
	;
		Goal = ite_rep(Cond, Then, Else),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = cond(_, CondPathStr, _),
			path_from_string_det(CondPathStr, CondPath),
			list__append(Path, PathTail, CondPath),
			PathTail = [ite_cond]
		->
			ThenPath = list__append(Path, [ite_then]),
			CondAndPath = goal_and_path(Cond, CondPath),
			ThenAndPath = goal_and_path(Then, ThenPath),
			make_primitive_list(Store,
				[CondAndPath, ThenAndPath | GoalPaths],
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = else(_, ElseCondId),
			cond_node_from_id(Store, ElseCondId, CondNode),
			CondNode = cond(_, CondPathStr, _),
			path_from_string_det(CondPathStr, CondPath),
			list__append(Path, PathTail, CondPath),
			PathTail = [ite_cond]
		->
			ElsePath = list__append(Path, [ite_else]),
			ElseAndPath = goal_and_path(Else, ElsePath),
			make_primitive_list(Store, [ElseAndPath | GoalPaths],
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			throw(internal_error("make_primitive_list",
				"mismatch on if-then-else"))
		)
	;
		Goal = negation_rep(NegGoal),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = neg_succ(_, _)
		->
			% The negated goal cannot contribute any bindings.
			make_primitive_list(Store, GoalPaths,
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = neg(_, _, _)
		->
			% The end of the primitive list is somewhere inside
			% NegGoal.
			NegPath = list__append(Path, [neg]),
			NegAndPath = goal_and_path(NegGoal, NegPath),
			make_primitive_list(Store, [NegAndPath],
				ContourTail, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives0, Primitives)
		;
			throw(internal_error("make_primitive_list",
				"mismatch on negation"))
		)
	;
		Goal = some_rep(InnerGoal, MaybeCut),
		InnerPath = list__append(Path, [exist(MaybeCut)]),
		InnerAndPath = goal_and_path(InnerGoal, InnerPath),
		make_primitive_list(Store, [InnerAndPath | GoalPaths],
			Contour, MaybeEnd, ArgNum, HeadVars, Var,
			Primitives0, Primitives)
	;
		Goal = atomic_goal_rep(_, File, Line, BoundVars, AtomicGoal),
		GeneratesEvent = atomic_goal_generates_event(AtomicGoal),
		(
			GeneratesEvent = yes(Args),
			(
				Contour = [ContourHeadId - ContourHeadNode
					| ContourTail],
				CallId = ContourHeadNode ^ exit_call,
				call_node_from_id(Store, CallId, CallNode),
				CallPathStr = CallNode ^ call_goal_path,
				path_from_string_det(CallPathStr, CallPath),
				CallPath = Path,
				\+ (
					MaybeEnd = yes(EndPath),
					EndPath = Path
				)
			->
				Primitive = primitive(File, Line, BoundVars,
					AtomicGoal, Path, yes(ContourHeadId)),
				Primitives1 = [Primitive | Primitives0],
				make_primitive_list(Store, GoalPaths,
					ContourTail, MaybeEnd, ArgNum,
					HeadVars, Var, Primitives1, Primitives)
			;
				Contour = [_ContourHeadId - ContourHeadNode],
				CallPathStr = ContourHeadNode ^ call_goal_path,
				path_from_string_det(CallPathStr, CallPath),
				CallPath = Path,
				MaybeEnd = yes(EndPath),
				EndPath = Path
			->
				list__index1_det(Args, ArgNum, Var),
				Primitives = Primitives0
			;
				throw(internal_error("make_primitive_list",
					"mismatch on call"))
			)
		;
			GeneratesEvent = no,
			Primitive = primitive(File, Line, BoundVars,
				AtomicGoal, Path, no),
			Primitives1 = [Primitive | Primitives0],
			make_primitive_list(Store, GoalPaths,
				Contour, MaybeEnd, ArgNum, HeadVars, Var,
				Primitives1, Primitives)
		)
	).
make_primitive_list(_, [], Contour, MaybeEnd, ArgNum, HeadVars, Var,
		Primitives, Primitives) :-
	decl_require(unify(Contour, []),
		"make_primitive_list", "nonempty contour at end"),
	decl_require(unify(MaybeEnd, no),
		"make_primitive_list", "found end when looking for call"),
	list__index1_det(HeadVars, ArgNum, Var).

:- pred traverse_primitives(list(annotated_primitive(R))::in,
	var_rep::in, term_path::in, S::in, proc_rep::in,
	subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

traverse_primitives([], Var0, TermPath0, _, ProcRep, Origin) :-
	ProcRep = proc_rep(HeadVars, _),
	ArgPos = find_arg_pos(HeadVars, Var0),
	Origin = input(ArgPos, TermPath0).
traverse_primitives([Prim | Prims], Var0, TermPath0, Store, ProcRep,
		Origin) :-
	Prim = primitive(File, Line, BoundVars, AtomicGoal, _GoalPath,
		MaybeNodeId),
	(
		AtomicGoal = unify_construct_rep(_CellVar, _Cons, FieldVars),
		( list__member(Var0, BoundVars) ->
			(
				TermPath0 = [],
				Origin = primitive_op(File, Line)
			;
				TermPath0 = [TermPathStep0 | TermPath],
				list__index1_det(FieldVars, TermPathStep0,
					Var),
				traverse_primitives(Prims, Var, TermPath,
					Store, ProcRep, Origin)
			)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_deconstruct_rep(CellVar, _Cons, FieldVars),
		( list__member(Var0, BoundVars) ->
			( list__nth_member_search(FieldVars, Var0, Pos) ->
				traverse_primitives(Prims,
					CellVar, [Pos | TermPath0],
					Store, ProcRep, Origin)
			;
				throw(internal_error("traverse_primitives",
					"bad deconstruct"))
			)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_assign_rep(ToVar, FromVar),
		( list__member(Var0, BoundVars) ->
			decl_require(unify(Var0, ToVar),
				"traverse_primitives", "bad assign"),
			traverse_primitives(Prims, FromVar, TermPath0,
				Store, ProcRep, Origin)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = pragma_foreign_code_rep(_Args),
		( list__member(Var0, BoundVars) ->
			Origin = primitive_op(File, Line)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_simple_test_rep(_LVar, _RVar),
		( list__member(Var0, BoundVars) ->
			throw(internal_error("traverse_primitives", "bad test"))
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = higher_order_call_rep(_, Args),
		traverse_call(BoundVars, no, Args, MaybeNodeId, Prims,
			Var0, TermPath0, Store, ProcRep, Origin)
	;
		AtomicGoal = method_call_rep(_, _, Args),
		traverse_call(BoundVars, no, Args, MaybeNodeId, Prims,
			Var0, TermPath0, Store, ProcRep, Origin)
	;
		AtomicGoal = plain_call_rep(ModuleName, PredName, Args),
		PlainCallInfo = plain_call_info(File, Line,
			ModuleName, PredName),
		traverse_call(BoundVars, yes(PlainCallInfo), Args, MaybeNodeId,
			Prims, Var0, TermPath0, Store, ProcRep, Origin)
	).

:- type plain_call_info
	--->	plain_call_info(
			file_name	:: string,
			line_number	:: int,
			module_name	:: string,
			pred_name	:: string
		).

:- pred traverse_call(list(var_rep)::in, maybe(plain_call_info)::in,
	list(var_rep)::in, maybe(R)::in,
	list(annotated_primitive(R))::in, var_rep::in, term_path::in,
	S::in, proc_rep::in, subterm_origin(edt_node(R))::out) is det
	<= annotated_trace(S, R).

traverse_call(BoundVars, MaybePlainCallInfo, Args, MaybeNodeId,
		Prims, Var, TermPath, Store, ProcRep, Origin) :-
	( list__member(Var, BoundVars) ->
		Pos = find_arg_pos(Args, Var),
		(
			MaybeNodeId = yes(NodeId),
			Origin = output(dynamic(NodeId), Pos, TermPath)
		;
			MaybeNodeId = no,
			(
				MaybePlainCallInfo = yes(PlainCallInfo),
				PlainCallInfo = plain_call_info(File, Line,
					ModuleName, PredName),
				call_is_primitive(ModuleName, PredName)
			->
				Origin = primitive_op(File, Line)
			;
				throw(internal_error("traverse_call",
					"no node id"))
			)
		)
	;
		traverse_primitives(Prims, Var, TermPath, Store, ProcRep,
			Origin)
	).

%-----------------------------------------------------------------------------%

:- pred add_paths_to_conjuncts(list(goal_rep)::in, goal_path::in, int::in,
	goal_and_path_list::out) is det.

add_paths_to_conjuncts([], _, _, []).
add_paths_to_conjuncts([Goal | Goals], ParentPath, N,
		[goal_and_path(Goal, Path) | GoalAndPaths]) :-
	list__append(ParentPath, [conj(N)], Path),
	add_paths_to_conjuncts(Goals, ParentPath, N + 1, GoalAndPaths).

%-----------------------------------------------------------------------------%

:- func start_loc_to_subterm_mode(start_loc(R)) = subterm_mode.

start_loc_to_subterm_mode(cur_goal) = subterm_out.
start_loc_to_subterm_mode(parent_goal(_, _)) = subterm_in.

%-----------------------------------------------------------------------------%

:- func find_arg_pos(list(var_rep), var_rep) = arg_pos.

find_arg_pos(HeadVars, Var) = ArgPos :-
	find_arg_pos_2(HeadVars, Var, 1, ArgPos).

:- pred find_arg_pos_2(list(var_rep)::in, var_rep::in, int::in, arg_pos::out)
	is det.

find_arg_pos_2([], _, _, _) :-
	throw(internal_error("find_arg_pos_2", "empty list")).
find_arg_pos_2([HeadVar | HeadVars], Var, Pos, ArgPos) :-
	( HeadVar = Var ->
		ArgPos = any_head_var(Pos)
	;
		find_arg_pos_2(HeadVars, Var, Pos + 1, ArgPos)
	).

%-----------------------------------------------------------------------------%

edt_subtree_details(Store, dynamic(Ref), Event, SeqNo) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, Call, _, _, Event, _)
	;
		Node = fail(_, Call, _, Event)
	;
		Node = excp(_, Call, _, _, Event)
	),
	call_node_from_id(Store, Call, CallNode),
	SeqNo = CallNode ^ call_seq.

:- inst edt_return_node =
		bound(	exit(ground, ground, ground, ground, ground, ground)
		;	fail(ground, ground, ground, ground)
		;	excp(ground, ground, ground, ground, ground)).

:- pred det_edt_return_node_from_id(S::in, R::in,
	trace_node(R)::out(edt_return_node)) is det <= annotated_trace(S, R).

det_edt_return_node_from_id(Store, Ref, Node) :-
	(
		trace_node_from_id(Store, Ref, Node0),
		(
			Node0 = exit(_, _, _, _, _, _)
		;
			Node0 = fail(_, _, _, _)
		;
			Node0 = excp(_, _, _, _, _)
		)
	->
		Node = Node0
	;
		throw(internal_error("det_edt_return_node_from_id",
			"not a return node"))
	).

%-----------------------------------------------------------------------------%

:- pred trace_atom_subterm_is_ground(trace_atom, arg_pos, term_path).
:- mode trace_atom_subterm_is_ground(in, in, in) is semidet.

trace_atom_subterm_is_ground(atom(_, _, Args), ArgPos, _) :-
	select_arg_at_pos(ArgPos, Args, ArgInfo),
	ArgInfo = arg_info(_, _, MaybeArg),
	MaybeArg = yes(_).

%-----------------------------------------------------------------------------%

:- pred decl_require(pred, string, string).
:- mode decl_require((pred) is semidet, in, in) is det.

decl_require(Goal, Loc, Msg) :-
	(
		call(Goal)
	->
		true
	;
		throw(internal_error(Loc, Msg))
	).

