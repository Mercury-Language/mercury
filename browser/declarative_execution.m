%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_execution.m
% Author: Mark Brown
%
% This module defines a Mercury representation of Mercury program
% execution, the annotated trace.  This structure is described in
% papers/decl_debug.  The declarative debugging infrastructure in the
% trace directory builds an annotated trace, using predicates exported
% from this module.  Once built, the structure is passed to the front
% end (in browser/declarative_debugger.m) where it is analysed
% to produce a bug diagnosis.

:- module mdb__declarative_execution.
:- interface.
:- import_module list, std_util, string, io.
:- import_module mdb__util.

	% This type represents a port in the annotated trace.
	% The type R is the type of references to other nodes
	% in the store.
	%
	% If this type is modified, some of the macros in
	% trace/mercury_trace_declarative.h may also need to be
	% updated.
	%
:- type trace_node(R)
	--->	call(
			R,			% Preceding event.
			R,			% Last EXIT or REDO event.
			trace_atom,		% Atom that was called.
			sequence_number		% Call sequence number.
		)
	;	exit(
			R,			% Preceding event.
			R,			% CALL event.
			R,			% Previous REDO event, if any.
			trace_atom		% Atom in its final state.
		)
	;	redo(
			R,			% Preceding event.
			R			% EXIT event.
		)
	;	fail(
			R,			% Preceding event.
			R,			% CALL event.
			R			% Previous REDO event, if any.
		)
	;	switch(
			R,			% Preceding event.
			goal_path		% Path for this event.
		)
	;	first_disj(
			R,			% Preceding event.
			goal_path		% Path for this event.
		)
	;	later_disj(
			R,			% Preceding event.
			goal_path,		% Path for this event.
			R			% Event of the first DISJ.
		)
	;	cond(
			R,			% Preceding event.
			goal_path,		% Path for this event.
			goal_status		% Whether we have reached
						% a THEN or ELSE event.
		)
	;	then(
			R,			% Preceding event.
			R			% COND event.
		)
	;	else(
			R,			% Preceding event.
			R			% COND event.
		)
	;	neg(
			R,			% Preceding event.
			goal_path,		% Path for this event.
			goal_status		% Whether we have reached
						% a NEGS or NEGF event.
		)
	;	neg_succ(
			R,			% Preceding event.
			R			% NEGE event.
		)
	;	neg_fail(
			R,			% Preceding event.
			R			% NEGE event.
		)
	.

	% If either of the following two types are modified, some of
	% the macros in trace/mercury_trace_declarative.h may need
	% to be updated.
	%
:- type trace_atom
	--->	atom(
			string,			% Procedure name.
			list(univ)		% Arguments.
			% XXX we also need to store some information about
			% where the arguments come from, since they will
			% not necessarily be in the right order or all
			% present (we do not store unbound variables).
		).

:- type goal_status
	--->	succeeded
	;	failed
	;	undecided.

:- type goal_path == goal_path_string.

:- type sequence_number == int.

	% Members of this typeclass represent an entire annotated
	% trace.  The second parameter is the type of identifiers
	% for trace nodes, and the first parameter is the type of
	% an abstract mapping from the identfiers to the nodes they
	% identify.
	%
:- typeclass execution_tree(S, R) where [

		% Dereference the identifier.  This fails if the
		% identifier does not refer to any trace_node (ie.
		% it is a NULL pointer).
		%
	pred trace_node_from_id(S, R, trace_node(R)),
	mode trace_node_from_id(in, in, out) is semidet
].


	% The following procedures also dereference the identifiers,
	% but they give an error if the node is not of the expected type.
	%
:- pred det_trace_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode det_trace_node_from_id(in, in, out) is det.

:- inst trace_node_call = bound(call(ground, ground, ground, ground)).

:- pred call_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode call_node_from_id(in, in, out(trace_node_call)) is det.

:- inst trace_node_redo = bound(redo(ground, ground)).

	% maybe_redo_node_from_id/3 fails if the argument is a
	% NULL reference.
	% 
:- pred maybe_redo_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode maybe_redo_node_from_id(in, in, out(trace_node_redo)) is semidet.

:- inst trace_node_exit = bound(exit(ground, ground, ground, ground)).

:- pred exit_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode exit_node_from_id(in, in, out(trace_node_exit)) is det.

:- inst trace_node_cond = bound(cond(ground, ground, ground)).

:- pred cond_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode cond_node_from_id(in, in, out(trace_node_cond)) is det.

:- inst trace_node_neg = bound(neg(ground, ground, ground)).

:- pred neg_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode neg_node_from_id(in, in, out(trace_node_neg)) is det.

:- inst trace_node_first_disj = bound(first_disj(ground, ground)).

:- pred first_disj_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode first_disj_node_from_id(in, in, out(trace_node_first_disj)) is det.

:- inst trace_node_disj = bound(first_disj(ground, ground);
				later_disj(ground, ground, ground)).

:- pred disj_node_from_id(S, R, trace_node(R)) <= execution_tree(S, R).
:- mode disj_node_from_id(in, in, out(trace_node_disj)) is det.

	% Load an execution tree which was previously saved by
	% the back end.
	%
:- pred load_trace_node_map(io__input_stream, trace_node_map,
		trace_node_key, io__state, io__state).
:- mode load_trace_node_map(in, out, out, di, uo) is det.

	% Save an execution tree generated by the back end.  It is
	% first converted into a trace_node_map/trace_node_key pair.
	%
:- pred save_trace_node_store(io__output_stream, trace_node_store,
		trace_node_id, io__state, io__state).
:- mode save_trace_node_store(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% This instance is used when the declarative debugger is in
	% normal mode.  Values of this instance are produced by the
	% back end and passed directly to the front end.
	%
:- type trace_node_store.
:- type trace_node_id.
:- instance execution_tree(trace_node_store, trace_node_id).

	% This instance is used when the declarative debugger is in
	% test mode.  Values of this instance are produced by copying
	% values of the previous instance.  Unlike the previous
	% instance, values of this one can be fed through a stream.
	% 
:- type trace_node_map.
:- type trace_node_key.
:- instance execution_tree(trace_node_map, trace_node_key).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, require.

det_trace_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0)
	->
		Node = Node0
	;
		error("det_trace_node_from_id: NULL node id")
	).

call_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = call(_, _, _, _)
	->
		Node = Node0
	;
		error("call_node_from_id: not a CALL node")
	).

maybe_redo_node_from_id(Store, NodeId, Node) :-
	trace_node_from_id(Store, NodeId, Node0),
	(
		Node0 = redo(_, _)
	->
		Node = Node0
	;
		error("maybe_redo_node_from_id: not a REDO node or NULL")
	).

exit_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = exit(_, _, _, _)
	->
		Node = Node0
	;
		error("exit_node_from_id: not an EXIT node")
	).

cond_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = cond(_, _, _)
	->
		Node = Node0
	;
		error("cond_node_from_id: not a COND node")
	).

neg_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = neg(_, _, _)
	->
		Node = Node0
	;
		error("neg_node_from_id: not a NEG node")
	).

first_disj_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		Node0 = first_disj(_, _)
	->
		Node = Node0
	;
		error("first_disj_node_from_id: not a first DISJ node")
	).

disj_node_from_id(Store, NodeId, Node) :-
	(
		trace_node_from_id(Store, NodeId, Node0),
		( Node0 = first_disj(_, _)
		; Node0 = later_disj(_, _, _)
		)
	->
		Node = Node0
	;
		error("disj_node_from_id: not a DISJ node")
	).

%-----------------------------------------------------------------------------%

:- instance execution_tree(trace_node_store, trace_node_id) where [
	pred(trace_node_from_id/3) is search_trace_node_store
].

	% The "map" is actually just an integer representing the version
	% of the map.  The empty map should be given the value 0, and
	% each time the map is destructively modified (by C code), the
	% value should be incremented.
	%
:- type trace_node_store ---> store(int).

	% The implementation of the identifiers is the same as what
	% is identified.  This fact is hidden, however, to force the
	% abstract map to be explicitly used whenever a new node is
	% accessed.
	%
:- type trace_node_id ---> id(c_pointer).

:- pred search_trace_node_store(trace_node_store, trace_node_id,
		trace_node(trace_node_id)).
:- mode search_trace_node_store(in, in, out) is semidet.

:- pragma c_code(
	search_trace_node_store(_Store::in, Id::in, Node::out),
	[will_not_call_mercury, thread_safe],
	"
		Node = Id;
		SUCCESS_INDICATOR = (Id != (Word) NULL);
	"
).

	%
	% Following are some predicates that are useful for
	% manipulating the above instance in C code.
	%

:- func trace_node_port(trace_node(trace_node_id)) = trace_port_type.
:- pragma export(trace_node_port(in) = out,
		"MR_DD_trace_node_port").

trace_node_port(call(_, _, _, _))	= call.
trace_node_port(exit(_, _, _, _))	= exit.
trace_node_port(redo(_, _))		= redo.
trace_node_port(fail(_, _, _))		= fail.
trace_node_port(switch(_, _))		= switch.
trace_node_port(first_disj(_, _))	= disj.
trace_node_port(later_disj(_, _, _))	= disj.
trace_node_port(cond(_, _, _))		= ite_cond.
trace_node_port(then(_, _))		= ite_then.
trace_node_port(else(_, _))		= ite_else.
trace_node_port(neg(_, _, _))		= neg_enter.
trace_node_port(neg_succ(_, _))		= neg_success.
trace_node_port(neg_fail(_, _))		= neg_failure.

:- func trace_node_path(trace_node_store, trace_node(trace_node_id))
		= goal_path_string.
:- pragma export(trace_node_path(in, in) = out,
		"MR_DD_trace_node_path").

trace_node_path(_, call(_, _, _, _)) = "".
trace_node_path(_, exit(_, _, _, _)) = "".
trace_node_path(_, redo(_, _)) = "".
trace_node_path(_, fail(_, _, _)) = "".
trace_node_path(_, switch(_, P)) = P.
trace_node_path(_, first_disj(_, P)) = P.
trace_node_path(_, later_disj(_, P, _)) = P.
trace_node_path(_, cond(_, P, _)) = P.
trace_node_path(S, then(_, Cond)) = P :-
	cond_node_from_id(S, Cond, cond(_, P, _)).
trace_node_path(S, else(_, Cond)) = P :-
	cond_node_from_id(S, Cond, cond(_, P, _)).
trace_node_path(_, neg(_, P, _)) = P.
trace_node_path(S, neg_succ(_, Neg)) = P :-
	neg_node_from_id(S, Neg, neg(_, P, _)).
trace_node_path(S, neg_fail(_, Neg)) = P :-
	neg_node_from_id(S, Neg, neg(_, P, _)).

:- pred trace_node_seqno(trace_node_store, trace_node(trace_node_id),
		sequence_number).
:- mode trace_node_seqno(in, in, out) is semidet.

:- pragma export(trace_node_seqno(in, in, out), "MR_DD_trace_node_seqno").

trace_node_seqno(S, Node, SeqNo) :-
	(
		Node = call(_, _, _, SeqNo0)
	->
		SeqNo = SeqNo0
	;
		trace_node_call(S, Node, Call),
		call_node_from_id(S, Call, call(_, _, _, SeqNo))
	).

:- pred trace_node_call(trace_node_store, trace_node(trace_node_id),
		trace_node_id).
:- mode trace_node_call(in, in, out) is semidet.

:- pragma export(trace_node_call(in, in, out), "MR_DD_trace_node_call").

trace_node_call(_, exit(_, Call, _, _), Call).
trace_node_call(S, redo(_, Exit), Call) :-
	exit_node_from_id(S, Exit, exit(_, Call, _, _)).
trace_node_call(_, fail(_, Call, _), Call).

:- pred trace_node_first_disj(trace_node(trace_node_id), trace_node_id).
:- mode trace_node_first_disj(in, out) is semidet.

:- pragma export(trace_node_first_disj(in, out),
		"MR_DD_trace_node_first_disj").

trace_node_first_disj(first_disj(_, _), NULL) :-
	null_trace_node_id(NULL).
trace_node_first_disj(later_disj(_, _, FirstDisj), FirstDisj).	

	% Given any node in an annotated trace, find the most recent
	% node in the same context which has not been backtracked over,
	% skipping negations, conditions, the bodies of calls, and
	% alternative disjuncts.  Return the NULL reference if there
	% is no such node (eg. if we are at the start of a negation,
	% condition, or call).
	%
:- func step_left_in_context(trace_node_store, trace_node(trace_node_id))
		= trace_node_id.
:- pragma export(step_left_in_context(in, in) = out,
		"MR_DD_step_left_in_context").

step_left_in_context(_, call(_, _, _, _)) = _ :-
	error("step_left_in_context: unexpected CALL node").
step_left_in_context(_, cond(Prec, _, Status)) = Node :-
	(
		Status = succeeded
	->
		Node = Prec
	;
		null_trace_node_id(Node)
	).
step_left_in_context(_, neg(_, _, _)) = _ :-
	error("step_left_in_context: unexpected NEGE node").
step_left_in_context(Store, exit(_, Call, _, _)) = Prec :-
	call_node_from_id(Store, Call, call(Prec, _, _, _)).
step_left_in_context(Store, fail(_, Call, _)) = Prec :-
	call_node_from_id(Store, Call, call(Prec, _, _, _)).
step_left_in_context(_, redo(_, _)) = _ :-
	error("step_left_in_context: unexpected REDO node").
step_left_in_context(_, switch(Prec, _)) = Prec.
step_left_in_context(_, first_disj(Prec, _)) = Prec.
step_left_in_context(Store, later_disj(_, _, FirstDisj)) = Prec :-
	first_disj_node_from_id(Store, FirstDisj, first_disj(Prec, _)).
step_left_in_context(_, then(Prec, _)) = Prec.
step_left_in_context(Store, else(_, Cond)) = Prec :-
	cond_node_from_id(Store, Cond, cond(Prec, _, _)).
step_left_in_context(Store, neg_succ(_, Neg)) = Prec :-
	neg_node_from_id(Store, Neg, neg(Prec, _, _)).
step_left_in_context(Store, neg_fail(_, Neg)) = Prec :-
	neg_node_from_id(Store, Neg, neg(Prec, _, _)).

	% Given any node in an annotated trace, find a node in
	% the previous contour.
	%
:- func find_prev_contour(trace_node_store, trace_node_id)
		= trace_node_id.
:- pragma export(find_prev_contour(in, in) = out,
		"MR_DD_find_prev_contour").

find_prev_contour(Store, NodeId) = OnContour :-
	det_trace_node_from_id(Store, NodeId, Node),
	find_prev_contour_1(Store, NodeId, Node, OnContour).

:- pred find_prev_contour_1(trace_node_store, trace_node_id,
		trace_node(trace_node_id), trace_node_id).
:- mode find_prev_contour_1(in, in, in, out) is det.

find_prev_contour_1(_, _, call(_, _, _, _), _) :-
	error("find_prev_contour: reached CALL node").
find_prev_contour_1(_, Exit, exit(_, _, _, _), Exit).
find_prev_contour_1(Store, _, redo(_, Exit), OnContour) :-
	exit_node_from_id(Store, Exit, exit(OnContour, _, _, _)).
find_prev_contour_1(Store, _, fail(_, Call, _), OnContour) :-
	call_node_from_id(Store, Call, call(OnContour, _, _, _)).
find_prev_contour_1(_, _, cond(_, _, _), _) :-
	error("find_prev_contour: reached COND node").
find_prev_contour_1(_, Then, then(_, _), Then).
find_prev_contour_1(_, Else, else(_, _), Else).
find_prev_contour_1(_, _, neg(_, _, _), _) :-
	error("find_prev_contour: reached NEGE node").
find_prev_contour_1(_, NegS, neg_succ(_, _), NegS).
find_prev_contour_1(Store, _, neg_fail(_, Neg), OnContour) :-
	neg_node_from_id(Store, Neg, neg(OnContour, _, _)).
find_prev_contour_1(_, Swtc, switch(_, _), Swtc).
find_prev_contour_1(_, FirstDisj, first_disj(_, _), FirstDisj).
find_prev_contour_1(_, LaterDisj, later_disj(_, _, _), LaterDisj).

	% Print a text representation of a trace node, useful
	% for debugging purposes.
	%
:- pred print_trace_node(io__output_stream, trace_node(trace_node_id),
		io__state, io__state).
:- mode print_trace_node(in, in, di, uo) is det.
:- pragma export(print_trace_node(in, in, di, uo), "MR_DD_print_trace_node").

print_trace_node(OutStr, Node) -->
	{ convert_node(Node, CNode) },
	io__write(OutStr, CNode).

%-----------------------------------------------------------------------------%

	%
	% Each node type has a Mercury function which constructs
	% a node of that type.  The functions are exported to C so
	% that the back end can build an execution tree.
	%

:- func construct_call_node(trace_node_id, trace_atom, sequence_number)
		= trace_node(trace_node_id).
:- pragma export(construct_call_node(in, in, in) = out,
		"MR_DD_construct_call_node").

construct_call_node(Preceding, Atom, SeqNo) = Call :-
	Call = call(Preceding, Answer, Atom, SeqNo),
	null_trace_node_id(Answer).


:- func construct_exit_node(trace_node_id, trace_node_id, trace_node_id,
		trace_atom) = trace_node(trace_node_id).
:- pragma export(construct_exit_node(in, in, in, in) = out,
		"MR_DD_construct_exit_node").

construct_exit_node(Preceding, Call, MaybeRedo, Atom)
		= exit(Preceding, Call, MaybeRedo, Atom).


:- func construct_redo_node(trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_redo_node(in, in) = out,
		"MR_DD_construct_redo_node").

construct_redo_node(Preceding, Exit) = redo(Preceding, Exit).


:- func construct_fail_node(trace_node_id, trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_fail_node(in, in, in) = out,
		"MR_DD_construct_fail_node").

construct_fail_node(Preceding, Call, Redo) = fail(Preceding, Call, Redo).


:- func construct_switch_node(trace_node_id, goal_path_string)
		= trace_node(trace_node_id).
:- pragma export(construct_switch_node(in, in) = out,
		"MR_DD_construct_switch_node").

construct_switch_node(Preceding, Path) =
		switch(Preceding, Path).

:- func construct_first_disj_node(trace_node_id, goal_path_string)
		= trace_node(trace_node_id).
:- pragma export(construct_first_disj_node(in, in) = out,
		"MR_DD_construct_first_disj_node").

construct_first_disj_node(Preceding, Path) =
		first_disj(Preceding, Path).


:- func construct_later_disj_node(trace_node_store, trace_node_id,
		goal_path_string, trace_node_id) = trace_node(trace_node_id).
:- pragma export(construct_later_disj_node(in, in, in, in) = out,
		"MR_DD_construct_later_disj_node").

construct_later_disj_node(Store, Preceding, Path, PrevDisj)
		= later_disj(Preceding, Path, FirstDisj) :-
	disj_node_from_id(Store, PrevDisj, PrevDisjNode),
	(
		PrevDisjNode = first_disj(_, _),
		FirstDisj = PrevDisj
	;
		PrevDisjNode = later_disj(_, _, FirstDisj)
	).


:- func construct_cond_node(trace_node_id, goal_path_string)
		= trace_node(trace_node_id).
:- pragma export(construct_cond_node(in, in) = out,
		"MR_DD_construct_cond_node").

construct_cond_node(Preceding, Path) = cond(Preceding, Path, undecided).


:- func construct_then_node(trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_then_node(in, in) = out,
		"MR_DD_construct_then_node").

construct_then_node(Preceding, Cond) = then(Preceding, Cond).


:- func construct_else_node(trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_else_node(in, in) = out,
		"MR_DD_construct_else_node").

construct_else_node(Preceding, Cond) = else(Preceding, Cond).


:- func construct_neg_node(trace_node_id, goal_path_string)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_node(in, in) = out,
		"MR_DD_construct_neg_node").

construct_neg_node(Preceding, Path) = neg(Preceding, Path, undecided).


:- func construct_neg_succ_node(trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_succ_node(in, in) = out,
		"MR_DD_construct_neg_succ_node").

construct_neg_succ_node(Preceding, Neg) = neg_succ(Preceding, Neg).


:- func construct_neg_fail_node(trace_node_id, trace_node_id)
		= trace_node(trace_node_id).
:- pragma export(construct_neg_fail_node(in, in) = out,
		"MR_DD_construct_neg_fail_node").

construct_neg_fail_node(Preceding, Neg) = neg_fail(Preceding, Neg).


:- pred null_trace_node_id(trace_node_id).
:- mode null_trace_node_id(out) is det.

:- pragma c_code(
	null_trace_node_id(Id::out),
	[will_not_call_mercury, thread_safe],
	"Id = (Word) NULL;"
).

%-----------------------------------------------------------------------------%

	% The most important property of this instance is that it
	% can be written to or read in from a stream easily.  It
	% is not as efficient to use as the earlier instance, though.
	%
:- instance execution_tree(trace_node_map, trace_node_key) where [
	pred(trace_node_from_id/3) is search_trace_node_map
].

:- type trace_node_map
	--->	map(map(trace_node_key, trace_node(trace_node_key))).

	% Values of this type are represented in the same way (in the
	% underlying C code) as corresponding values of the other
	% instance.
	%
:- type trace_node_key
	--->	key(int).

:- pred search_trace_node_map(trace_node_map, trace_node_key,
		trace_node(trace_node_key)).
:- mode search_trace_node_map(in, in, out) is semidet.

search_trace_node_map(map(Map), Key, Node) :-
	map__search(Map, Key, Node).

load_trace_node_map(Stream, Map, Key) -->
	io__read(Stream, ResKey),
	{
		ResKey = ok(Key)
	;
		ResKey = eof,
		error("load_trace_node_map: unexpected EOF")
	;
		ResKey = error(Msg, _),
		error(Msg)
	},
	io__read(Stream, ResMap),
	{
		ResMap = ok(Map)
	;
		ResMap = eof,
		error("load_trace_node_map: unexpected EOF")
	;
		ResMap = error(Msg, _),
		error(Msg)
	}.

:- pragma export(save_trace_node_store(in, in, in, di, uo),
		"MR_DD_save_trace").

save_trace_node_store(Stream, Store, NodeId) -->
	{ map__init(Map0) },
	{ node_id_to_key(NodeId, Key) },
	{ node_map(Store, NodeId, map(Map0), Map) },
	io__write(Stream, Key),
	io__write_string(Stream, ".\n"),
	io__write(Stream, Map),
	io__write_string(Stream, ".\n").

:- pred node_map(trace_node_store, trace_node_id, trace_node_map,
		trace_node_map).
:- mode node_map(in, in, in, out) is det.

node_map(Store, NodeId, map(Map0), Map) :-
	(
		search_trace_node_store(Store, NodeId, Node1)
	->
		node_id_to_key(NodeId, Key),
		convert_node(Node1, Node2),
		map__det_insert(Map0, Key, Node2, Map1),
		Next = preceding_node(Node1),
		node_map(Store, Next, map(Map1), Map)
	;
		Map = map(Map0)
	).

:- pred node_id_to_key(trace_node_id, trace_node_key).
:- mode node_id_to_key(in, out) is det.

:- pragma c_code(node_id_to_key(Id::in, Key::out),
		[will_not_call_mercury, thread_safe],
		"Key = (Integer) Id;").

:- pred convert_node(trace_node(trace_node_id), trace_node(trace_node_key)).
:- mode convert_node(in, out) is det.

:- pragma c_code(convert_node(N1::in, N2::out),
		[will_not_call_mercury, thread_safe],
		"N2 = N1;").

	% Given a node in an annotated trace, return a reference to
	% the preceding node in the trace, or a NULL reference if
	% it is the first.
	%
:- func preceding_node(trace_node(T)) = T.

preceding_node(call(P, _, _, _))	= P.
preceding_node(exit(P, _, _, _))	= P.
preceding_node(redo(P, _))		= P.
preceding_node(fail(P, _, _))		= P.
preceding_node(switch(P, _))		= P.
preceding_node(first_disj(P, _))	= P.
preceding_node(later_disj(P, _, _))	= P.
preceding_node(cond(P, _, _))		= P.
preceding_node(then(P, _))		= P.
preceding_node(else(P, _))		= P.
preceding_node(neg(P, _, _))		= P.
preceding_node(neg_succ(P, _))		= P.
preceding_node(neg_fail(P, _))		= P.

