%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
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
:- import_module io, list, bool, std_util.
:- import_module mdb__declarative_execution.

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
			decl_atom,	% The head of the clause, in its
					% final state of instantiation.
			decl_contour,	% The path taken through the body.
			event_number	% The exit event.
		)
	;	partially_uncovered_atom(
			decl_atom,	% The called atom, in its initial
					% state.
			event_number	% The fail event.
		).

:- type decl_i_bug
	--->	inadmissible_call(
			decl_atom,	% The parent atom, in its initial
					% state.
			decl_position,	% The location of the call in the
					% parent's body.
			decl_atom,	% The inadmissible child, in its
					% initial state.
			event_number	% The call event.
		).

	% XXX not yet implemented.
	%
:- type decl_contour == unit.
:- type decl_position == unit.

	% Values of this type represent goal behaviour.  This representation
	% is used by the front end (in this module), as well as the
	% oracle and user interface.
	%
:- type decl_question
			% The node is a suspected wrong answer.  The
			% argument is the atom in its final state of
			% instantiatedness (ie. at the EXIT event).
			%
	--->	wrong_answer(decl_atom)

			% The node is a suspected missing answer.  The
			% first argument is the atom in its initial state
			% of instantiatedness (ie. at the CALL event),
			% and the second argument is the list of solutions.
			% 
	;	missing_answer(decl_atom, list(decl_atom)).

:- type decl_answer == pair(decl_question, decl_truth).

:- type decl_atom == trace_atom.

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

:- pred diagnoser_state_init(io__input_stream, io__output_stream,
		diagnoser_state(R)).
:- mode diagnoser_state_init(in, in, out) is det.

:- pred diagnosis(S, R, diagnoser_response, diagnoser_state(R),
		diagnoser_state(R), io__state, io__state)
			<= annotated_trace(S, R).
:- mode diagnosis(in, in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, char.
:- import_module mdb__declarative_analyser, mdb__declarative_oracle.

:- type diagnoser_state(R)
	--->	diagnoser(
			analyser_state(edt_node(R)),
			oracle_state
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

diagnoser_state_init(InStr, OutStr, Diagnoser) :-
	analyser_state_init(Analyser),
	oracle_state_init(InStr, OutStr, Oracle),
	Diagnoser = diagnoser(Analyser, Oracle).

diagnosis(Store, NodeId, Response, Diagnoser0, Diagnoser) -->
	{ diagnoser_get_analyser(Diagnoser0, Analyser0) },
	{ start_analysis(wrap(Store), dynamic(NodeId), AnalyserResponse,
			Analyser0, Analyser) },
	{ diagnoser_set_analyser(Diagnoser0, Analyser, Diagnoser1) },
	handle_analyser_response(Store, AnalyserResponse, Response,
			Diagnoser1, Diagnoser).

:- pred handle_analyser_response(S, analyser_response(edt_node(R)),
		diagnoser_response, diagnoser_state(R), diagnoser_state(R),
		io__state, io__state) <= annotated_trace(S, R).
:- mode handle_analyser_response(in, in, out, in, out, di, uo) is det.

handle_analyser_response(_, no_suspects, no_bug_found, D, D) -->
	[].

handle_analyser_response(_, bug_found(Bug), Response, Diagnoser0,
		Diagnoser) -->

	confirm_bug(Bug, Response, Diagnoser0, Diagnoser).

handle_analyser_response(Store, oracle_queries(Queries), Response,
		Diagnoser0, Diagnoser) -->
	
	{ diagnoser_get_oracle(Diagnoser0, Oracle0) },
	query_oracle(Queries, OracleResponse, Oracle0, Oracle),
	{ diagnoser_set_oracle(Diagnoser0, Oracle, Diagnoser1) },
	handle_oracle_response(Store, OracleResponse, Response, Diagnoser1,
			Diagnoser).

handle_analyser_response(Store, require_explicit(Tree), Response,
		Diagnoser, Diagnoser) -->

	{
		edt_subtree_details(Store, Tree, Event, Seqno),
		Response = require_subtree(Event, Seqno)
	}.

:- pred handle_oracle_response(S, oracle_response, diagnoser_response,
		diagnoser_state(R), diagnoser_state(R), io__state, io__state)
			<= annotated_trace(S, R).
:- mode handle_oracle_response(in, in, out, in, out, di, uo) is det.

handle_oracle_response(Store, oracle_answers(Answers), Response, Diagnoser0,
		Diagnoser) -->
	
	{ diagnoser_get_analyser(Diagnoser0, Analyser0) },
	{ continue_analysis(wrap(Store), Answers, AnalyserResponse,
			Analyser0, Analyser) },
	{ diagnoser_set_analyser(Diagnoser0, Analyser, Diagnoser1) },
	handle_analyser_response(Store, AnalyserResponse, Response,
			Diagnoser1, Diagnoser).

handle_oracle_response(_, no_oracle_answers, no_bug_found, D, D) -->
	[].

handle_oracle_response(_, abort_diagnosis, no_bug_found, D, D) -->
	io__write_string("Diagnosis aborted.\n").

:- pred confirm_bug(decl_bug, diagnoser_response, diagnoser_state(R),
		diagnoser_state(R), io__state, io__state).
:- mode confirm_bug(in, out, in, out, di, uo) is det.

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
	diagnoser_state_init(InStr, OutStr, Diagnoser).
		
	% Export a monomorphic version of diagnosis/9, to make it
	% easier to call from C code.
	%
:- pred diagnosis_store(trace_node_store, trace_node_id, diagnoser_response,
		diagnoser_state(trace_node_id), diagnoser_state(trace_node_id),
		io__state, io__state).
:- mode diagnosis_store(in, in, out, in, out, di, uo) is det.

:- pragma export(diagnosis_store(in, in, out, in, out, di, uo),
		"MR_DD_decl_diagnosis").
	
diagnosis_store(Store, Node, Response, State0, State) -->
	diagnosis(Store, Node, Response, State0, State).

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

:- type edt_node(R)
	--->	dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R)
	where [
		pred(edt_root_question/3) is trace_root_question,
		pred(edt_root_e_bug/3) is trace_root_e_bug,
		pred(edt_children/3) is trace_children
	].

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(S) ---> wrap(S).

:- pred trace_root_question(wrap(S), edt_node(R), decl_question)
		<= annotated_trace(S, R).
:- mode trace_root_question(in, in, out) is det.

trace_root_question(wrap(Store), dynamic(Ref), Root) :-
	det_edt_node_from_id(Store, Ref, Node),
	(
		Node = fail(_, CallId, RedoId, _),
		call_node_from_id(Store, CallId, Call),
		Call = call(_, _, CallAtom, _, _, _),
		get_answers(Store, RedoId, [], Answers),
		Root = missing_answer(CallAtom, Answers)
	;
		Node = exit(_, _, _, ExitAtom, _),
		Root = wrong_answer(ExitAtom)
	).

:- pred get_answers(S, R, list(decl_atom), list(decl_atom))
		<= annotated_trace(S, R).
:- mode get_answers(in, in, in, out) is det.

get_answers(Store, RedoId, As0, As) :-
	(
		maybe_redo_node_from_id(Store, RedoId, redo(_, ExitId))
	->
		exit_node_from_id(Store, ExitId, exit(_, _, NextId, Atom, _)),
		get_answers(Store, NextId, [Atom | As0], As)
	;
		As = As0
	).

:- pred trace_root_e_bug(wrap(S), edt_node(R), decl_e_bug)
		<= annotated_trace(S, R).
:- mode trace_root_e_bug(in, in, out) is det.

trace_root_e_bug(wrap(S), dynamic(Ref), Bug) :-
	det_edt_node_from_id(S, Ref, Node),
	(
		Node = exit(_, _, _, Atom, Event),
		Bug = incorrect_contour(Atom, unit, Event)
	;
		Node = fail(_, CallId, _, Event),
		call_node_from_id(S, CallId, Call),
		Call = call(_, _, CallAtom, _, _, _),
		Bug = partially_uncovered_atom(CallAtom, Event)
	).

:- pred trace_children(wrap(S), edt_node(R), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode trace_children(in, in, out) is semidet.

trace_children(wrap(Store), dynamic(Ref), Children) :-
	det_edt_node_from_id(Store, Ref, Node),
	(
		Node = fail(PrecId, CallId, _, _),
		not_at_depth_limit(Store, CallId),
		missing_answer_children(Store, PrecId, [], Children)
	;
		Node = exit(PrecId, CallId, _, _, _),
		not_at_depth_limit(Store, CallId),
		wrong_answer_children(Store, PrecId, [], Children)
	).

:- pred not_at_depth_limit(S, R) <= annotated_trace(S, R).
:- mode not_at_depth_limit(in, in) is semidet.

not_at_depth_limit(Store, Ref) :-
	call_node_from_id(Store, Ref, call(_, _, _, _, _, no)).

:- pred wrong_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode wrong_answer_children(in, in, in, out) is det.

wrong_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		Node = call(_, _, _, _, _, _),
		Ns = Ns0
	;
		Node = neg(_, _, _),
		Ns = Ns0
	;
		Node = exit(_, Call, _, _, _),
		call_node_from_id(Store, Call, call(Prec, _, _, _, _, _)),
		wrong_answer_children(Store, Prec, [dynamic(NodeId) | Ns0], Ns)
	;
		Node = redo(_, _),
		error("wrong_answer_children: unexpected REDO node")
	;
		Node = fail(_, Call, _, _),
		call_node_from_id(Store, Call, call(Prec, _, _, _, _, _)),
		wrong_answer_children(Store, Prec, [dynamic(NodeId) | Ns0], Ns)
	;
		Node = cond(Prec, _, Flag),
		(
			Flag = succeeded
		->
			wrong_answer_children(Store, Prec, Ns0, Ns)
		;
			Ns = Ns0
		)
	;
		Node = switch(Back, _),
		wrong_answer_children(Store, Back, Ns0, Ns)
	;
		Node = first_disj(Back, _),
		wrong_answer_children(Store, Back, Ns0, Ns)
	;
		Node = later_disj(_, _, FirstDisj),
		first_disj_node_from_id(Store, FirstDisj, first_disj(Back, _)),
		wrong_answer_children(Store, Back, Ns0, Ns)
	;
		Node = then(Back, _),
		wrong_answer_children(Store, Back, Ns0, Ns)
	;
		Node = else(Prec, Cond),
		missing_answer_children(Store, Prec, Ns0, Ns1),
		cond_node_from_id(Store, Cond, cond(Back, _, _)),
		wrong_answer_children(Store, Back, Ns1, Ns)
	;
		Node = neg_succ(Prec, Neg),
		missing_answer_children(Store, Prec, Ns0, Ns1),
		neg_node_from_id(Store, Neg, neg(Back, _, _)),
		wrong_answer_children(Store, Back, Ns1, Ns)
	;
		Node = neg_fail(Prec, Neg),
		wrong_answer_children(Store, Prec, Ns0, Ns1),
		neg_node_from_id(Store, Neg, neg(Back, _, _)),
		wrong_answer_children(Store, Back, Ns1, Ns)
	).

:- pred missing_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode missing_answer_children(in, in, in, out) is det.

missing_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		Node = call(_, _, _, _, _, _),
		Ns = Ns0
	;
		Node = neg(_, _, _),
		Ns = Ns0
	;
		Node = exit(_, Call, Redo, _, _),
		(
			maybe_redo_node_from_id(Store, Redo, redo(Prec0, _))
		->
			Prec = Prec0
		;
			call_node_from_id(Store, Call, CallNode),
			CallNode = call(Prec, _, _, _, _, _)
		),
		missing_answer_children(Store, Prec, [dynamic(NodeId) | Ns0],
				Ns)
	;
		Node = redo(_, Exit),
		exit_node_from_id(Store, Exit, exit(Prec, _, _, _, _)),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = fail(_, CallId, MaybeRedo, _),
		(
			maybe_redo_node_from_id(Store, MaybeRedo, Redo)
		->
			Redo = redo(Prec, _),
			Next = Prec
		;
			call_node_from_id(Store, CallId, Call),
			Call = call(Back, _, _, _, _, _),
			Next = Back
		),
		missing_answer_children(Store, Next, [dynamic(NodeId) | Ns0],
				Ns)
	;
		Node = cond(Prec, _, Flag),
		(
			Flag = succeeded
		->
			missing_answer_children(Store, Prec, Ns0, Ns)
		;
			Ns = Ns0
		)
	;
		Node = switch(Prec, _),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = first_disj(Prec, _),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = later_disj(Prec, _, _),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = then(Prec, _),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = else(Prec, Cond),
		missing_answer_children(Store, Prec, Ns0, Ns1),
		cond_node_from_id(Store, Cond, cond(Back, _, _)),
		missing_answer_children(Store, Back, Ns1, Ns)
	;
		Node = neg_succ(Prec, Neg),
		missing_answer_children(Store, Prec, Ns0, Ns1),
		neg_node_from_id(Store, Neg, neg(Back, _, _)),
		missing_answer_children(Store, Back, Ns1, Ns)
	;
		Node = neg_fail(Prec, Neg),
		wrong_answer_children(Store, Prec, Ns0, Ns1),
		neg_node_from_id(Store, Neg, neg(Back, _, _)),
		missing_answer_children(Store, Back, Ns1, Ns)
	).

:- pred edt_subtree_details(S, edt_node(R), event_number, sequence_number)
		<= annotated_trace(S, R).
:- mode edt_subtree_details(in, in, out, out) is det.

edt_subtree_details(Store, dynamic(Ref), Event, SeqNo) :-
	det_edt_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, Call, _, _, Event)
	;
		Node = fail(_, Call, _, Event)
	),
	call_node_from_id(Store, Call, call(_, _, _, SeqNo, _, _)).

:- inst trace_node_edt_node =
		bound(	exit(ground, ground, ground, ground, ground)
		;	fail(ground, ground, ground, ground)).

:- pred det_edt_node_from_id(S, R, trace_node(R)) <= annotated_trace(S, R).
:- mode det_edt_node_from_id(in, in, out(trace_node_edt_node)) is det.

det_edt_node_from_id(Store, Ref, Node) :-
	(
		trace_node_from_id(Store, Ref, Node0),
		(
			Node0 = exit(_, _, _, _, _)
		;
			Node0 = fail(_, _, _, _)
		)
	->
		Node = Node0
	;
		error("det_edt_node_from_id: not an EXIT or FAIL node")
	).

%-----------------------------------------------------------------------------%

:- pred decl_bug_get_event_number(decl_bug, event_number).
:- mode decl_bug_get_event_number(in, out) is det.

decl_bug_get_event_number(e_bug(EBug), Event) :-
	(
		EBug = incorrect_contour(_, _, Event)
	;
		EBug = partially_uncovered_atom(_, Event)
	).
decl_bug_get_event_number(i_bug(IBug), Event) :-
	IBug = inadmissible_call(_, _, _, Event).

