%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_debugger.m
% Author: Mark Brown
%
% This module has two main purposes:
% 	- to define the interface between the front and back ends of
% 	  a Mercury declarative debugger, and
% 	- to implement a front end.
%
% The interface is defined by a procedure that can be called from
% the back end to perform diagnosis, and a typeclass which represents
% a declarative view of execution used by the front end.
%
% The front end implemented in this module analyses the EDT it is
% passed to diagnose a bug.  It does this by a simple top-down search.
%

:- module mdb__declarative_debugger.
:- interface.
:- import_module mdb__declarative_execution, mdb__program_representation.
:- import_module mdb__io_action.
:- import_module io, bool, list, std_util.

	% This type represents the possible truth values for nodes
	% in the EDT.
	%
:- type decl_truth == bool.

	% This type represents the possible responses to being
	% asked to confirm that a node is a bug.
	%
:- type decl_confirmation
	--->	confirm_bug
	;	overrule_bug
	;	abort_diagnosis.

	% This type represents the bugs which can be diagnosed.
	% The parameter of the constructor is the type of EDT nodes.
	%
:- type decl_bug
			% An EDT whose root node is incorrect,
			% but whose children are all correct.
			%
	--->	e_bug(decl_e_bug)

			% An EDT whose root node is incorrect, and
			% which has no incorrect children but at
			% least one inadmissible one.
			%
	;	i_bug(decl_i_bug).

:- type decl_e_bug
	--->	incorrect_contour(
			final_decl_atom,% The head of the clause, in its
					% final state of instantiation.
			decl_contour,	% The path taken through the body.
			event_number	% The exit event.
		)
	;	partially_uncovered_atom(
			init_decl_atom,	% The called atom, in its initial
					% state.
			event_number	% The fail event.
		)
	;	unhandled_exception(
			init_decl_atom,	% The called atom, in its initial
					% state.
			decl_exception, % The exception thrown.
			event_number	% The excp event.
		).

:- type decl_i_bug
	--->	inadmissible_call(
			init_decl_atom,	% The parent atom, in its initial
					% state.
			decl_position,	% The location of the call in the
					% parent's body.
			init_decl_atom,	% The inadmissible child, in its
					% initial state.
			event_number	% The call event.
		).

	% XXX not yet implemented.
	%
:- type decl_contour == unit.
:- type decl_position == unit.

	% Values of the following two types represent questions from the
	% analyser to the oracle about some aspect of program behaviour,
	% and responses from the oracle, respectively.  In both cases the
	% type parameter is for the type of EDT nodes -- each question and
	% answer keeps a reference to the node which generated it, so that
	% the analyser is able to figure out what to do when the answer
	% arrives back from the oracle.
	%
:- type decl_question(T)
			% The node is a suspected wrong answer.  The first
			% argument is the EDT node the question came from.
			% The second argument is the atom in its final
			% state of instantiatedness (ie. at the EXIT event).
			%
	--->	wrong_answer(T, final_decl_atom)

			% The node is a suspected missing answer.  The
			% first argument is the EDT node the question came
			% from. The second argument is the atom in its
			% initial state of instantiatedness (ie. at the
			% CALL event), and the third argument is the list
			% of solutions.
			%
	;	missing_answer(T, init_decl_atom, list(final_decl_atom))

			% The node is a possibly unexpected exception.
			% The first argument is the EDT node the question
			% came from.  The second argument is the atom in
			% its initial state of instantiation, and the third
			% argument is the exception thrown.
			%
	;	unexpected_exception(T, init_decl_atom, decl_exception).

:- type decl_answer(T)
			% The oracle knows the truth value of this node.
			%
	--->	truth_value(T, decl_truth)

			% The oracle does not say anything about the truth
			% value, but is suspicious of the subterm at the
			% given term_path and arg_pos.
			%
	;	suspicious_subterm(T, arg_pos, term_path).

	% Extract the EDT node from a question.
	%
:- func get_decl_question_node(decl_question(T)) = T.

:- type some_decl_atom
	--->	init(init_decl_atom)
	;	final(final_decl_atom).

:- type init_decl_atom
	--->	init_decl_atom(
			init_atom		:: trace_atom
		).

:- type final_decl_atom
	--->	final_decl_atom(
			final_atom		:: trace_atom,
			final_io_actions	:: list(io_action)
		).

:- type decl_exception == univ.

	% The diagnoser eventually responds with a value of this type
	% after it is called.
	%
:- type diagnoser_response

			% There was a bug found and confirmed.  The
			% event number is for a call port (inadmissible
			% call), an exit port (incorrect contour),
			% or a fail port (partially uncovered atom).
			%
	--->	bug_found(event_number)

			% There was no symptom found, or the diagnoser
			% aborted before finding a bug.
			%
	;	no_bug_found

			% The analyser requires the back end to reproduce
			% part of the annotated trace, with a greater
			% depth bound.  The event number and sequence
			% number are for the final event required (the
			% first event required is the call event with
			% the same sequence number).
			%
	;	require_subtree(event_number, sequence_number).

:- type diagnoser_state(R).

:- pred diagnoser_state_init(io_action_map::in, io__input_stream::in,
	io__output_stream::in, diagnoser_state(R)::out) is det.

:- pred diagnosis(S::in, R::in, int::in, int::in, int::in,
	diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

:- pred unravel_decl_atom(some_decl_atom::in, trace_atom::out,
	list(io_action)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_analyser, mdb__declarative_oracle.
:- import_module require, int, char, string, assoc_list, map.

unravel_decl_atom(DeclAtom, TraceAtom, IoActions) :-
	(
		DeclAtom = init(init_decl_atom(TraceAtom)),
		IoActions = []
	;
		DeclAtom = final(final_decl_atom(TraceAtom, IoActions))
	).

get_decl_question_node(wrong_answer(Node, _)) = Node.
get_decl_question_node(missing_answer(Node, _, _)) = Node.
get_decl_question_node(unexpected_exception(Node, _, _)) = Node.

%-----------------------------------------------------------------------------%

:- type diagnoser_state(R)
	--->	diagnoser(
			analyser_state	:: analyser_state(edt_node(R)),
			oracle_state	:: oracle_state
		).

:- pred diagnoser_get_analyser(diagnoser_state(R),
		analyser_state(edt_node(R))).
:- mode diagnoser_get_analyser(in, out) is det.

diagnoser_get_analyser(diagnoser(Analyser, _), Analyser).

:- pred diagnoser_set_analyser(diagnoser_state(R), analyser_state(edt_node(R)),
		diagnoser_state(R)).
:- mode diagnoser_set_analyser(in, in, out) is det.

diagnoser_set_analyser(diagnoser(_, B), A, diagnoser(A, B)).

:- pred diagnoser_get_oracle(diagnoser_state(R), oracle_state).
:- mode diagnoser_get_oracle(in, out) is det.

diagnoser_get_oracle(diagnoser(_, Oracle), Oracle).

:- pred diagnoser_set_oracle(diagnoser_state(R), oracle_state,
		diagnoser_state(R)).
:- mode diagnoser_set_oracle(in, in, out) is det.

diagnoser_set_oracle(diagnoser(A, _), B, diagnoser(A, B)).

diagnoser_state_init(IoActionMap, InStr, OutStr, Diagnoser) :-
	analyser_state_init(IoActionMap, Analyser),
	oracle_state_init(InStr, OutStr, Oracle),
	Diagnoser = diagnoser(Analyser, Oracle).

diagnosis(Store, NodeId, UseOldIoActionMap, IoActionStart, IoActionEnd,
		Response, Diagnoser0, Diagnoser) -->
	( { UseOldIoActionMap > 0 } ->
		{ Diagnoser1 = Diagnoser0 },
		{ diagnoser_get_analyser(Diagnoser1, Analyser1) }
	;
		make_io_action_map(IoActionStart, IoActionEnd, IoActionMap),
		{ Analyser0 = Diagnoser0 ^ analyser_state },
		{ analyser_state_replace_io_map(IoActionMap,
			Analyser0, Analyser1) },
		{ Diagnoser1 = Diagnoser0 ^ analyser_state := Analyser1 }
	),
	{ start_analysis(wrap(Store), dynamic(NodeId), AnalyserResponse,
		Analyser1, Analyser) },
	{ diagnoser_set_analyser(Diagnoser1, Analyser, Diagnoser2) },
	{ debug_analyser_state(Analyser, MaybeOrigin) },
	handle_analyser_response(Store, AnalyserResponse, MaybeOrigin,
		Response, Diagnoser2, Diagnoser).

:- pred handle_analyser_response(S::in, analyser_response(edt_node(R))::in,
	maybe(subterm_origin(edt_node(R)))::in, diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

handle_analyser_response(_, no_suspects, _, no_bug_found, D, D) -->
	io__write_string("No bug found.\n").

handle_analyser_response(_, bug_found(Bug), _, Response, Diagnoser0,
		Diagnoser) -->

	confirm_bug(Bug, Response, Diagnoser0, Diagnoser).

handle_analyser_response(Store, oracle_queries(Queries), MaybeOrigin, Response,
		Diagnoser0, Diagnoser) -->
	{ diagnoser_get_oracle(Diagnoser0, Oracle0) },
	debug_origin(Flag),
	(
		{ MaybeOrigin = yes(Origin) },
		{ Flag > 0 }
	->
		io__write_string("Origin: "),
		write_origin(wrap(Store), Origin),
		io__nl
	;
		[]
	),
	query_oracle(Queries, OracleResponse, Oracle0, Oracle),
	{ diagnoser_set_oracle(Diagnoser0, Oracle, Diagnoser1) },
	handle_oracle_response(Store, OracleResponse, Response, Diagnoser1,
			Diagnoser).

handle_analyser_response(Store, require_explicit(Tree), _, Response,
		Diagnoser, Diagnoser) -->
	{
		edt_subtree_details(Store, Tree, Event, Seqno),
		Response = require_subtree(Event, Seqno)
	}.

:- pred handle_oracle_response(S::in, oracle_response(edt_node(R))::in,
	diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

handle_oracle_response(Store, oracle_answers(Answers), Response, Diagnoser0,
		Diagnoser) -->
	{ diagnoser_get_analyser(Diagnoser0, Analyser0) },
	{ continue_analysis(wrap(Store), Answers, AnalyserResponse,
		Analyser0, Analyser) },
	{ diagnoser_set_analyser(Diagnoser0, Analyser, Diagnoser1) },
	{ debug_analyser_state(Analyser, MaybeOrigin) },
	handle_analyser_response(Store, AnalyserResponse, MaybeOrigin,
		Response, Diagnoser1, Diagnoser).

handle_oracle_response(_, no_oracle_answers, no_bug_found, D, D) -->
	[].

handle_oracle_response(_, abort_diagnosis, no_bug_found, D, D) -->
	io__write_string("Diagnosis aborted.\n").

:- pred confirm_bug(decl_bug::in, diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi.

confirm_bug(Bug, Response, Diagnoser0, Diagnoser) -->
	{ diagnoser_get_oracle(Diagnoser0, Oracle0) },
	oracle_confirm_bug(Bug, Confirmation, Oracle0, Oracle),
	{ diagnoser_set_oracle(Diagnoser0, Oracle, Diagnoser) },
	{
		Confirmation = confirm_bug,
		decl_bug_get_event_number(Bug, Event),
		Response = bug_found(Event)
	;
		Confirmation = overrule_bug,
		Response = no_bug_found
	;
		Confirmation = abort_diagnosis,
		Response = no_bug_found
	}.

	% Export a monomorphic version of diagnosis_state_init/4, to
	% make it easier to call from C code.
	%
:- pred diagnoser_state_init_store(io__input_stream, io__output_stream,
		diagnoser_state(trace_node_id)).
:- mode diagnoser_state_init_store(in, in, out) is det.

:- pragma export(diagnoser_state_init_store(in, in, out),
		"MR_DD_decl_diagnosis_state_init").

diagnoser_state_init_store(InStr, OutStr, Diagnoser) :-
	diagnoser_state_init(map__init, InStr, OutStr, Diagnoser).

	% Export a monomorphic version of diagnosis/10, to make it
	% easier to call from C code.
	%
:- pred diagnosis_store(trace_node_store::in, trace_node_id::in,
	int::in, int::in, int::in, diagnoser_response::out,
	diagnoser_state(trace_node_id)::in,
	diagnoser_state(trace_node_id)::out, io__state::di, io__state::uo)
	is cc_multi.

:- pragma export(diagnosis_store(in, in, in, in, in, out, in, out, di, uo),
		"MR_DD_decl_diagnosis").

diagnosis_store(Store, Node, UseOldIoActionMap, IoActionStart, IoActionEnd,
		Response, State0, State) -->
	diagnosis(Store, Node, UseOldIoActionMap, IoActionStart, IoActionEnd,
		Response, State0, State).

	% Export some predicates so that C code can interpret the
	% diagnoser response.
	%
:- pred diagnoser_bug_found(diagnoser_response, event_number).
:- mode diagnoser_bug_found(in, out) is semidet.

:- pragma export(diagnoser_bug_found(in, out), "MR_DD_diagnoser_bug_found").

diagnoser_bug_found(bug_found(Event), Event).

:- pred diagnoser_require_subtree(diagnoser_response, event_number,
		sequence_number).
:- mode diagnoser_require_subtree(in, out, out) is semidet.

:- pragma export(diagnoser_require_subtree(in, out, out),
		"MR_DD_diagnoser_require_subtree").

diagnoser_require_subtree(require_subtree(Event, SeqNo), Event, SeqNo).

%-----------------------------------------------------------------------------%

	%
	% This section defines an instance of the EDT in terms of
	% any instance of execution tree.
	%

	% The type of nodes in our implementation of EDTs.  The parameter
	% is meant to be the type of references to trace nodes.  In
	% particular, the references should be to trace nodes that could
	% be considered nodes in the EDT, namely those for exit, fail
	% and exception events.
	% 
:- type edt_node(R)
	--->	dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R)
	where [
		pred(edt_root_question/4) is trace_root_question,
		pred(edt_root_e_bug/4) is trace_root_e_bug,
		pred(edt_children/3) is trace_children,
		pred(edt_dependency/6) is trace_dependency
	].

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(S) ---> wrap(S).

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
		missing_answer_children(Store, PrecId, [], Children)
	;
		Node = exit(PrecId, CallId, _, _, _, _),
		not_at_depth_limit(Store, CallId),
		wrong_answer_children(Store, PrecId, [], Children)
	;
		Node = excp(PrecId, CallId, _, _, _),
		not_at_depth_limit(Store, CallId),
		unexpected_exception_children(Store, PrecId, [], Children)
	).

:- pred not_at_depth_limit(S, R) <= annotated_trace(S, R).
:- mode not_at_depth_limit(in, in) is semidet.

not_at_depth_limit(Store, Ref) :-
	call_node_from_id(Store, Ref, CallNode),
	CallNode ^ call_at_max_depth = no.

:- pred wrong_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode wrong_answer_children(in, in, in, out) is det.

wrong_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
			%
			% We have reached the end of the contour.
			%
		Ns = Ns0
	;
		Node = excp(_, _, _, _, _)
	->
		error("wrong_answer_children: exception handling not supported")
	;
		(
			Node = exit(_, _, _, _, _, _)
		->
				%
				% Add a child for this node.
				%
			Ns1 = [dynamic(NodeId) | Ns0]
		;
			( Node = else(Prec, _)
			; Node = neg_succ(Prec, _)
			)
		->
				%
				% There is a nested context.
				%
			missing_answer_children(Store, Prec, Ns0, Ns1)
		;
			Ns1 = Ns0
		),
		Next = step_left_in_contour(Store, Node),
		wrong_answer_children(Store, Next, Ns1, Ns)
	).

:- pred missing_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode missing_answer_children(in, in, in, out) is det.

missing_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
			%
			% We have reached the boundary of the stratum.
			%
		Ns = Ns0
	;
		Node = excp(_, _, _, _, _)
	->
		error(
		    "missing_answer_children: exception handling not supported")
	;
		(
			( Node = exit(_, _, _, _, _, _)
			; Node = fail(_, _, _, _)
			)
		->
				%
				% Add a child for this node.
				%
			Ns1 = [dynamic(NodeId) | Ns0]
		;
			Node = neg_fail(Prec, _)
		->
				%
				% There is a nested successful context.
				%
			wrong_answer_children(Store, Prec, Ns0, Ns1)
		;
			( Node = else(Prec, _)
			; Node = neg_succ(Prec, _)
			)
		->
				%
				% There is a nested failed context.
				%
			missing_answer_children(Store, Prec, Ns0, Ns1)
		;
			Ns1 = Ns0
		),
		Next = step_in_stratum(Store, Node),
		missing_answer_children(Store, Next, Ns1, Ns)
	).

:- pred unexpected_exception_children(S, R, list(edt_node(R)),
		list(edt_node(R))) <= annotated_trace(S, R).
:- mode unexpected_exception_children(in, in, in, out) is det.

unexpected_exception_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, failed)
		; Node = cond(_, _, failed)
		)
	->
			%
			% We have reached the end of the contour.
			%
		Ns = Ns0
	;
		(
			( Node = exit(_, _, _, _, _, _)
			; Node = excp(_, _, _, _, _)
			)
		->
				%
				% Add a child for this node.
				%
			Ns1 = [dynamic(NodeId) | Ns0]
		;
			( Node = else(Prec, _)
			; Node = neg_succ(Prec, _)
			)
		->
				%
				% There is a nested context.
				%
			missing_answer_children(Store, Prec, Ns0, Ns1)
		;
			Ns1 = Ns0
		),
		Next = step_left_in_contour(Store, Node),
		unexpected_exception_children(Store, Next, Ns1, Ns)
	).

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
			error("find_chain_start: unbound wrong answer term")
		)
	;
		Node = fail(_, CallId, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = CallNode ^ call_atom,
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		;
			error("find_chain_start: unbound missing answer term")
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
			error("find_chain_start: unbound exception term")
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
			error("make_primitive_list: mismatch on disj")
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
			error("make_primitive_list: mismatch on switch")
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
			error("make_primitive_list: mismatch on if-then-else")
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
			error("make_primitive_list: mismatch on negation")
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
				error("make_primitive_list: mismatch on call")
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
	require(unify(Contour, []),
		"make_primitive_list: nonempty contour at end"),
	require(unify(MaybeEnd, no),
		"make_primitive_list: found end when looking for call"),
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
				error("traverse_primitives: bad deconstruct")
			)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_assign_rep(ToVar, FromVar),
		( list__member(Var0, BoundVars) ->
			require(unify(Var0, ToVar),
				"traverse_primitives: bad assign"),
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
			error("traverse_primitives: bad test")
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
				error("traverse_call: no node id")
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
	error("find_arg_pos_2: empty list").
find_arg_pos_2([HeadVar | HeadVars], Var, Pos, ArgPos) :-
	( HeadVar = Var ->
		ArgPos = any_head_var(Pos)
	;
		find_arg_pos_2(HeadVars, Var, Pos + 1, ArgPos)
	).

%-----------------------------------------------------------------------------%

:- pred edt_subtree_details(S, edt_node(R), event_number, sequence_number)
		<= annotated_trace(S, R).
:- mode edt_subtree_details(in, in, out, out) is det.

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
		error("det_edt_return_node_from_id: not a return node")
	).

%-----------------------------------------------------------------------------%

:- pred trace_atom_subterm_is_ground(trace_atom, arg_pos, term_path).
:- mode trace_atom_subterm_is_ground(in, in, in) is semidet.

trace_atom_subterm_is_ground(atom(_, _, Args), ArgPos, _) :-
	select_arg_at_pos(ArgPos, Args, ArgInfo),
	ArgInfo = arg_info(_, _, MaybeArg),
	MaybeArg = yes(_).

:- pred decl_bug_get_event_number(decl_bug, event_number).
:- mode decl_bug_get_event_number(in, out) is det.

decl_bug_get_event_number(e_bug(EBug), Event) :-
	(
		EBug = incorrect_contour(_, _, Event)
	;
		EBug = partially_uncovered_atom(_, Event)
	;
		EBug = unhandled_exception(_, _, Event)
	).
decl_bug_get_event_number(i_bug(IBug), Event) :-
	IBug = inadmissible_call(_, _, _, Event).

%-----------------------------------------------------------------------------%

:- pred write_origin(wrap(S)::in, subterm_origin(edt_node(R))::in,
	io__state::di, io__state::uo) is det <= annotated_trace(S, R).

write_origin(wrap(Store), Origin) -->
	( { Origin = output(dynamic(NodeId), ArgPos, TermPath) } ->
		{ exit_node_from_id(Store, NodeId, ExitNode) },
		{ ProcName = ExitNode ^ exit_atom ^ proc_name },
		io__write_string("output("),
		io__write_string(ProcName),
		io__write_string(", "),
		io__write(ArgPos),
		io__write_string(", "),
		io__write(TermPath),
		io__write_string(")")
	;
		io__write(Origin)
	).

:- pragma foreign_code("C",
"

/*
** The declarative debugger will print diagnostic information about the origins
** computed by dependency tracking if this flag has a positive value.
*/

int	MR_DD_debug_origin = 0;

").

:- pragma foreign_decl("C",
"
extern	int	MR_DD_debug_origin;
").

:- pred debug_origin(int::out, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	debug_origin(Flag::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Flag = MR_DD_debug_origin;
	IO = IO0;
").
