%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
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

:- module declarative_debugger.
:- interface.
:- import_module io, list, bool, std_util.
:- import_module declarative_execution.

	% This type represents the possible truth values for nodes
	% in the EDT.
	%
:- type decl_truth == bool.

	% This type represents the bugs which can be diagnosed.
	% The parameter of the constructor is the type of EDT nodes.
	%
:- type decl_bug(T)
	--->	e_bug(T)	% An EDT whose root node is incorrect,
				% but whose children are all correct.

	;	i_bug(T).	% An EDT whose root node is incorrect, and
				% which has no incorrect children but at
				% least one inadmissible one.

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
	% XXX need to have a case for expanding an implicit tree.
	%
:- type diagnoser_response
	--->	bug_found
	;	no_bug_found.

:- type diagnoser_state(R).

:- pred diagnoser_state_init(io__input_stream, io__output_stream,
		diagnoser_state(R)).
:- mode diagnoser_state_init(in, in, out) is det.

:- pred diagnosis(S, R, diagnoser_response, diagnoser_state(R),
		diagnoser_state(R), io__state, io__state)
			<= execution_tree(S, R).
:- mode diagnosis(in, in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, char.
:- import_module declarative_analyser, declarative_oracle.

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
		io__state, io__state) <= execution_tree(S, R).
:- mode handle_analyser_response(in, in, out, in, out, di, uo) is det.

handle_analyser_response(_, no_suspects, no_bug_found, D, D) -->
	[].

handle_analyser_response(Store, bug_found(Bug), Response, D, D) -->
	confirm_bug(Store, Bug, Confirmed),
	{
		Confirmed = yes,
		Response = bug_found
	;
		Confirmed = no,
		Response = no_bug_found
	}.

handle_analyser_response(Store, oracle_queries(Queries), Response,
		Diagnoser0, Diagnoser) -->
	
	{ diagnoser_get_oracle(Diagnoser0, Oracle0) },
	query_oracle(Queries, OracleResponse, Oracle0, Oracle),
	{ diagnoser_set_oracle(Diagnoser0, Oracle, Diagnoser1) },
	handle_oracle_response(Store, OracleResponse, Response, Diagnoser1,
			Diagnoser).

handle_analyser_response(_, require_explicit(_), _, _, _) -->
	{ error("diagnosis: implicit representation not yet implemented") }.

:- pred handle_oracle_response(S, oracle_response, diagnoser_response,
		diagnoser_state(R), diagnoser_state(R), io__state, io__state)
			<= execution_tree(S, R).
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

:- pred confirm_bug(S, decl_bug(edt_node(R)), bool, io__state, io__state)
		<= execution_tree(S, R).
:- mode confirm_bug(in, in, out, di, uo) is det.

confirm_bug(_, Bug, yes) -->
	io__write(Bug),		% XXX this doesn't work very well.
	io__nl.

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

%-----------------------------------------------------------------------------%

	%
	% This section defines an instance of the EDT in terms of
	% any instance of execution tree.
	%

:- type edt_node(R)
	--->	dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= execution_tree(S, R)
	where [
		pred(edt_root/3) is trace_root,
		pred(edt_children/3) is trace_children
	].

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(S) ---> wrap(S).

:- pred trace_root(wrap(S), edt_node(R), decl_question) <= execution_tree(S, R).
:- mode trace_root(in, in, out) is det.

trace_root(wrap(Store), dynamic(Ref), Root) :-
	det_trace_node_from_id(Store, Ref, Node),
	(
		Node = fail(_, CallId)
	->
		call_node_from_id(Store, CallId, Call),
		Call = call(_, RedoId, CallAtom),
		get_answers(Store, RedoId, [], Answers),
		Root = missing_answer(CallAtom, Answers)
	;
		Node = exit(_, _, _, ExitAtom)
	->
		Root = wrong_answer(ExitAtom)
	;
		error("trace_root: not an EXIT or FAIL node")
	).

:- pred get_answers(S, R, list(decl_atom), list(decl_atom))
		<= execution_tree(S, R).
:- mode get_answers(in, in, in, out) is det.

get_answers(Store, RedoId, As0, As) :-
	(
		maybe_redo_node_from_id(Store, RedoId, redo(_, ExitId))
	->
		exit_node_from_id(Store, ExitId, exit(_, _, NextId, Atom)),
		get_answers(Store, NextId, [Atom | As0], As)
	;
		As = As0
	).

:- pred trace_children(wrap(S), edt_node(R), list(edt_node(R)))
		<= execution_tree(S, R).
:- mode trace_children(in, in, out) is semidet.

trace_children(wrap(Store), dynamic(Ref), Children) :-
		
		% This is meant to fail if the children are implicit,
		% but this is not yet implemented.
		%
	semidet_succeed,

	det_trace_node_from_id(Store, Ref, Node),
	(
		Node = fail(PrecId, _)
	->
		missing_answer_children(Store, PrecId, [], Children)
	;
		Node = exit(PrecId, _, _, _)
	->
		wrong_answer_children(Store, PrecId, [], Children)
	;
		error("trace_children: not an EXIT or FAIL node")
	).

:- pred wrong_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= execution_tree(S, R).
:- mode wrong_answer_children(in, in, in, out) is det.

wrong_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		Node = call(_, _, _),
		Ns = Ns0
	;
		Node = neg(_, _, _),
		Ns = Ns0
	;
		Node = exit(_, Call, _, _),
		call_node_from_id(Store, Call, call(Prec, _, _)),
		wrong_answer_children(Store, Prec, [dynamic(NodeId) | Ns0], Ns)
	;
		Node = redo(_, _),
		error("wrong_answer_children: unexpected REDO node")
	;
		Node = fail(_, Call),
		call_node_from_id(Store, Call, call(Prec, _, _)),
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
		Node = first_disj(Back, _, _),
		wrong_answer_children(Store, Back, Ns0, Ns)
	;
		Node = later_disj(_, Back, _),
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
		<= execution_tree(S, R).
:- mode missing_answer_children(in, in, in, out) is det.

missing_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		Node = call(_, _, _),
		Ns = Ns0
	;
		Node = neg(_, _, _),
		Ns = Ns0
	;
		Node = exit(_, Call, Redo, _),
		(
			maybe_redo_node_from_id(Store, Redo, redo(Prec0, _))
		->
			Prec = Prec0
		;
			call_node_from_id(Store, Call, call(Prec, _, _))
		),
		wrong_answer_children(Store, Prec, [dynamic(NodeId) | Ns0], Ns)
	;
		Node = redo(_, Exit),
		exit_node_from_id(Store, Exit, exit(Prec, _, _, _)),
		wrong_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = fail(_, Call),
		call_node_from_id(Store, Call, call(Back, Answer, _)),
		(
			maybe_redo_node_from_id(Store, Answer, redo(Prec, _))
		->
			Next = Prec
		;
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
		Node = first_disj(Prec, _, _),
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
		Node = neg_succ(_, Neg),
		neg_node_from_id(Store, Neg, neg(Prec, _, _)),
		missing_answer_children(Store, Prec, Ns0, Ns)
	;
		Node = neg_fail(Prec, Neg),
		wrong_answer_children(Store, Prec, Ns0, Ns1),
		neg_node_from_id(Store, Neg, neg(Back, _, _)),
		missing_answer_children(Store, Back, Ns1, Ns)
	).

