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
:- import_module io, list, bool, std_util.
:- import_module mdb__declarative_execution, mdb__program_representation.

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
		)
	;	unhandled_exception(
			decl_atom,	% The called atom, in its initial
					% state.
			decl_exception, % The exception thrown.
			event_number	% The excp event.
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
	;	missing_answer(decl_atom, list(decl_atom))

			% The node is a possibly unexpected exception.
			% The first argument is the atom in its initial
			% state of instantiation, and the second argument
			% is the exception thrown.
			%
	;	unexpected_exception(decl_atom, decl_exception).

	% These are the possible answers that the oracle can give.
	%
:- type decl_answer
			% The oracle knows the truth value of this node.
			%
	--->	truth_value(decl_question, decl_truth)

			% The oracle does not say anything about the truth
			% value, but is suspicious of the subterm at the
			% given term_path and arg_pos.
			%
	;	suspicious_subterm(decl_question, arg_pos, term_path).

:- type decl_atom == trace_atom.

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

:- pred diagnoser_state_init(io__input_stream, io__output_stream,
		diagnoser_state(R)).
:- mode diagnoser_state_init(in, in, out) is det.

:- pred diagnosis(S::in, R::in, diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, char, string.
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

:- pred handle_analyser_response(S::in, analyser_response(edt_node(R))::in,
	diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

handle_analyser_response(_, no_suspects, no_bug_found, D, D) -->
	io__write_string("No bug found.\n").

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

:- pred handle_oracle_response(S::in, oracle_response::in,
	diagnoser_response::out,
	diagnoser_state(R)::in, diagnoser_state(R)::out,
	io__state::di, io__state::uo) is cc_multi <= annotated_trace(S, R).

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
	diagnoser_state_init(InStr, OutStr, Diagnoser).
		
	% Export a monomorphic version of diagnosis/9, to make it
	% easier to call from C code.
	%
:- pred diagnosis_store(trace_node_store::in, trace_node_id::in,
	diagnoser_response::out, diagnoser_state(trace_node_id)::in,
	diagnoser_state(trace_node_id)::out, io__state::di, io__state::uo)
	is cc_multi.

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
		pred(edt_children/3) is trace_children,
		pred(edt_dependency/6) is trace_dependency
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
		Call = call(_, _, CallAtom, _, _, _, _),
		get_answers(Store, RedoId, [], Answers),
		Root = missing_answer(CallAtom, Answers)
	;
		Node = exit(_, _, _, ExitAtom, _),
		Root = wrong_answer(ExitAtom)
	;
		Node = excp(_, CallId, _, Exception, _),
		call_node_from_id(Store, CallId, Call),
		Call = call(_, _, CallAtom, _, _, _, _),
		Root = unexpected_exception(CallAtom, Exception)
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
		Call = call(_, _, CallAtom, _, _, _, _),
		Bug = partially_uncovered_atom(CallAtom, Event)
	;
		Node = excp(_, CallId, _, Exception, Event),
		call_node_from_id(S, CallId, Call),
		Call = call(_, _, CallAtom, _, _, _, _),
		Bug = unhandled_exception(CallAtom, Exception, Event)
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
	;
		Node = excp(PrecId, CallId, _, _, _),
		not_at_depth_limit(Store, CallId),
		unexpected_exception_children(Store, PrecId, [], Children)
	).

:- pred not_at_depth_limit(S, R) <= annotated_trace(S, R).
:- mode not_at_depth_limit(in, in) is semidet.

not_at_depth_limit(Store, Ref) :-
	call_node_from_id(Store, Ref, call(_, _, _, _, _, no, _)).

:- pred wrong_answer_children(S, R, list(edt_node(R)), list(edt_node(R)))
		<= annotated_trace(S, R).
:- mode wrong_answer_children(in, in, in, out) is det.

wrong_answer_children(Store, NodeId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _)
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
			Node = exit(_, _, _, _, _)
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
		( Node = call(_, _, _, _, _, _, _)
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
			( Node = exit(_, _, _, _, _)
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
		( Node = call(_, _, _, _, _, _, _)
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
			( Node = exit(_, _, _, _, _)
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
% the top of the selected subterm; if the mode is `in', the origin could be:
%	- a unification within the body of the parent,
%	- an output subterm in a sibling node, or
%	- an input subterm of the parent node.
% In this case we look at the contour leading up to the call event associated
% with the given node.  If the mode is `out', the origin could be:
%	- a unification within the body of the call,
%	- an output subterm of a child of the node, or
%	- an input subterm of the node itself.
% In the case we look at the contour leading up to the exit or exception event
% associated with the given node.
%
% If the contour starts with a neg or cond event, then we also look at the
% contour leading up to that event (and so on, recursively).  We eventually
% stop when a call event is reached.  The goal representation used comes from
% this call event.
%
% We first make a full pass of the contour(s), matching up the contour events
% with atomic events in the goal representation, and constructing a list of
% `atom_info's, information about atomic goals in the contour(s).  We then
% traverse this list, keeping track of the variable which contains the
% selected subterm, and the location within this variable.

:- pred trace_dependency(wrap(S), edt_node(R), arg_pos, term_path,
		subterm_mode, subterm_origin(edt_node(R)))
		<= annotated_trace(S, R).
:- mode trace_dependency(in, in, in, in, out, out) is det.

trace_dependency(wrap(Store), dynamic(Ref), ArgPos, TermPath, Mode, Origin) :-
	det_edt_node_from_id(Store, Ref, Node),
	(
		Node = exit(ExitPrec, CallId, _, ExitAtom, _),
		call_node_from_id(Store, CallId, Call),
		Call = call(CallPrec, _, CallAtom, _, _, _, _),
		(
			trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath)
		->
			Mode = subterm_in,
			Start = CallPrec
		;
			trace_atom_subterm_is_ground(ExitAtom, ArgPos, TermPath)
		->
			Mode = subterm_out,
			Start = ExitPrec
		;
			error("trace_dependency: wrong answer subterm unbound")
		)
	;
		Node = fail(_, CallId, _, _),
		call_node_from_id(Store, CallId, Call),
		Call = call(CallPrec, _, CallAtom, _, _, _, _),
		(
			trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath)
		->
			Mode = subterm_in,
			Start = CallPrec
		;
			error(
			    "trace_dependency: missing answer subterm unbound")
		)
	;
		Node = excp(_, CallId, _, _, _),
		call_node_from_id(Store, CallId, Call),
		Call = call(CallPrec, _, CallAtom, _, _, _, _),
		%
		% XXX we don't yet handle tracking of the exception value.
		%
		(
			trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath)
		->
			Mode = subterm_in,
			Start = CallPrec
		;
			error("trace_dependency: exception subterm unbound")
		)
	),

	contour_foldl2(Store, process_trace_event, Start, next_contour(Store),
			GoalCont, AtomInfo0),
	(
		GoalCont = unknown_goal
	->
		%
		% There was no goal_rep to match the contour up with, so the
		% origin cannot be found.
		%
		Origin = not_found
	;
		%
		% Use up any remaining goals which are not associated with
		% any events (e.g. unifications).
		%
		process_non_event_goals(GoalCont, MaybeCallArgs, AtomInfo,
				AtomInfo0),
		(
			Mode = subterm_in,
			MaybeCallArgs = yes(CallArgs)
		->
			list__index1_det(CallArgs, ArgPos, VarRep)
		;
			Mode = subterm_out,
			MaybeCallArgs = no
		->
			%
			% Headvars have the same number as their argument
			% position.
			%
			VarRep = ArgPos
		;
			error("trace_dependency: contour mismatch")
		),
		Origin = find_subterm_origin(AtomInfo, VarRep, TermPath)
	).

	% contour_foldl2(Store, Pred, Right, Init, A, B) is analogous to
	% other foldl2 predicates which keep track of two accumulators
	% over a sequence.  In this case the sequence is the contour defined
	% by Right, the rightmost event of the contour.  The main difference
	% is that instead of supplying the initial accumulator values, the
	% Init closure calculates them from the event at the left boundary
	% of the contour.
	%
	% The mode that we have chosen has the last two arguments of the
	% accumulator predicate (second argument) with the opposite modes to
	% normal.  This is so the accumulator predicate can construct a
	% list using the DCG syntax.
	%
:- pred contour_foldl2(S, pred(R, trace_node(R), A, A, B, B), R,
		pred(trace_node(R), A, B), A, B) <= annotated_trace(S, R).
:- mode contour_foldl2(in, pred(in, in, in, out, out, in) is det, in,
		pred(in, out, out) is det, out, out) is det.

contour_foldl2(Store, ProcessEvent, Ref, Init, A, B) :-
	det_trace_node_from_id(Store, Ref, Node),
	(
		( Node = call(_, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
		Init(Node, A, B)
	;
		Next = step_left_in_contour(Store, Node),
		contour_foldl2(Store, ProcessEvent, Next, Init, A0, B0),
		ProcessEvent(Ref, Node, A0, A, B, B0)
	).

	% This type represents the remainder of a goal after some of it
	% has been executed, like a continuation.  We don't actually
	% execute this code, but match it up with the remainder of a contour
	% after some events have been processed.
	%
:- type goal_cont
	--->	subgoal_cont(
			goal_rep,	% A subgoal to execute.
			goal_cont	% Code after the subgoal.
		)
	;	conj_cont(
			list(goal_rep), % The rest of a conjunction to execute.
			goal_cont	% Code after the conjunction.
		)
	;	ite_cont(
			goal_rep,	% Then branch.
			goal_rep,	% Else branch.
			goal_cont	% Code after the if-then-else.
		)
	;	neg_cont(
			goal_cont	% Code after the negation.
		)
	;	return			% End of the procedure.
	;	unknown_goal.		% We don't have access to the
					% program representation.

:- type atom_info(R)
	--->	call_info(R, goal_rep)
	;	unify_info(goal_rep).

:- pred next_contour(S, trace_node(R), goal_cont, list(atom_info(R)))
		<= annotated_trace(S, R).
:- mode next_contour(in, in, out, out) is det.

next_contour(Store, Node, Cont, AtomInfo) :-
	(
		Node = call(_, _, _, _, _, _, MaybeGoal)
	->
		AtomInfo = [],
		(
			MaybeGoal = yes(Goal)
		->
			Cont = subgoal_cont(Goal, return)
		;
			Cont = unknown_goal
		)
	;
		( Node = neg(Prec, _, _)
		; Node = cond(Prec, _, _)
		)
	->
		%
		% We continue into the next contour up, since the subterm
		% could have come from there.
		%
		contour_foldl2(Store, process_trace_event, Prec,
				next_contour(Store), Cont, AtomInfo)
	;
		error("next_contour: not a contour boundary")
	).

	% Match the goal_cont up with one trace event, leaving a new
	% goal_cont.
	%
:- pred process_trace_event(R, trace_node(R), goal_cont, goal_cont,
		list(atom_info(R)), list(atom_info(R))).
:- mode process_trace_event(in, in, in, out, out, in) is det.

process_trace_event(Ref, Event, subgoal_cont(Goal, Cont0), Cont) -->
	process_trace_event_goal(Ref, Event, Goal, Cont0, Cont).
process_trace_event(Ref, Event, conj_cont([], Cont0), Cont) -->
	process_trace_event(Ref, Event, Cont0, Cont).
process_trace_event(Ref, Event, conj_cont([G | Gs], Cont0), Cont) -->
	process_trace_event_goal(Ref, Event, G, conj_cont(Gs, Cont0), Cont).
process_trace_event(_, Event, ite_cont(Then, Else, Cont0), Cont) -->
	{
		Event = then(_, _)
	->
		Cont = subgoal_cont(Then, Cont0)
	;
		Event = else(_, _)
	->
		Cont = subgoal_cont(Else, Cont0)
	;
		error("process_trace_event: ite mismatch")
	}.
process_trace_event(_, _, neg_cont(_), _) -->
	{ error("process_trace_event: unexpected end of negation") }.
process_trace_event(_, _, return, _) -->
	{ error("process_trace_event: unexpected end of goal") }.
process_trace_event(_, _, unknown_goal, unknown_goal) -->
	[].

:- pred process_trace_event_goal(R, trace_node(R), goal_rep, goal_cont,
		goal_cont, list(atom_info(R)), list(atom_info(R))).
:- mode process_trace_event_goal(in, in, in, in, out, out, in) is det.

process_trace_event_goal(Ref, Event, conj_rep([]), Cont0, Cont) -->
	process_trace_event(Ref, Event, Cont0, Cont).
process_trace_event_goal(Ref, Event, conj_rep([G | Gs]), Cont0, Cont) -->
	process_trace_event_goal(Ref, Event, G, conj_cont(Gs, Cont0), Cont).
process_trace_event_goal(_, Event, disj_rep(Ds), Cont0, Cont) -->
	{ list__index1_det(Ds, disj_event_branch_number(Event), D) },
	{ Cont = subgoal_cont(D, Cont0) }.
process_trace_event_goal(_, Event, switch_rep(As), Cont0, Cont) -->
	{ list__index1_det(As, switch_event_branch_number(Event), A) },
	{ Cont = subgoal_cont(A, Cont0) }.
process_trace_event_goal(_, Event, ite_rep(Cond, Then, Else), Cont0, Cont) -->
	{
		Event = cond(_, _, _)
	->
		Cont = subgoal_cont(Cond, ite_cont(Then, Else, Cont0))
	;
		Event = else(_, _)
	->
		%
		% The contour stepped over the (failed) condition.
		%
		Cont = subgoal_cont(Else, Cont0)
	;
		error("process_trace_event_goal: ite mismatch")
	}.
process_trace_event_goal(Ref, Event, negation_rep(Goal), Cont0, Cont) -->
	(
		{ Event = neg_succ(_, _) }
	->
		{ Cont = Cont0 }
	;
		process_trace_event_goal(Ref, Event, Goal, neg_cont(Cont0),
				Cont)
	).
process_trace_event_goal(Ref, Event, some_rep(Goal), Cont0, Cont) -->
	process_trace_event_goal(Ref, Event, Goal, Cont0, Cont).
process_trace_event_goal(Ref, Event, GoalRep, Cont0, Cont) -->
	{ GoalRep = atomic_goal_rep(_, _, _, _, AtomicGoal) },
	(
		{ atomic_goal_rep_is_call(AtomicGoal, _) }
	->
		{
			Event = exit(_, _, _, _, _)
		->
			Cont = Cont0
		;
			error("process_trace_event_goal: exit mismatch")
		},
		[ call_info(Ref, GoalRep) ]
	;
		[ unify_info(GoalRep) ],
		process_trace_event(Ref, Event, Cont0, Cont)
	).

:- pred process_non_event_goals(goal_cont, maybe(list(var_rep)),
		list(atom_info(R)), list(atom_info(R))).
:- mode process_non_event_goals(in, out, out, in) is det.

process_non_event_goals(subgoal_cont(Goal, Cont), MaybeArgs) -->
	process_non_event_goals_2(Goal, Cont, MaybeArgs).
process_non_event_goals(conj_cont([], Cont), MaybeArgs) -->
	process_non_event_goals(Cont, MaybeArgs).
process_non_event_goals(conj_cont([G | Gs], Cont), MaybeArgs) -->
	process_non_event_goals_2(G, conj_cont(Gs, Cont), MaybeArgs).
process_non_event_goals(ite_cont(_, _, _), _) -->
	{ error("process_non_event_goals: ite event expected") }.
process_non_event_goals(neg_cont(_), _) -->
	{ error("process_non_event_goals: neg event expected") }.
process_non_event_goals(return, no) -->
	[].
process_non_event_goals(unknown_goal, _) -->
	{ error("process_non_event_goals: goal is unknown") }.

:- pred process_non_event_goals_2(goal_rep, goal_cont, maybe(list(var_rep)),
		list(atom_info(R)), list(atom_info(R))).
:- mode process_non_event_goals_2(in, in, out, out, in) is det.

process_non_event_goals_2(conj_rep([]), Cont, MaybeArgs) -->
	process_non_event_goals(Cont, MaybeArgs).
process_non_event_goals_2(conj_rep([G | Gs]), Cont, MaybeArgs) -->
	process_non_event_goals_2(G, conj_cont(Gs, Cont), MaybeArgs).
process_non_event_goals_2(disj_rep(_), _, _) -->
	{ error("process_non_event_goals_2: disj event expected") }.
process_non_event_goals_2(switch_rep(_), _, _) -->
	{ error("process_non_event_goals_2: swtc event expected") }.
process_non_event_goals_2(ite_rep(_, _, _), _, _) -->
	{ error("process_non_event_goals_2: cond event expected") }.
process_non_event_goals_2(negation_rep(Goal), Cont, MaybeArgs) -->
	process_non_event_goals_2(Goal, neg_cont(Cont), MaybeArgs).
process_non_event_goals_2(some_rep(Goal), Cont, MaybeArgs) -->
	process_non_event_goals_2(Goal, Cont, MaybeArgs).
process_non_event_goals_2(Goal, Cont, MaybeArgs) -->
	{ Goal = atomic_goal_rep(_, _, _, _, AtomicGoal) },
	(
		{ atomic_goal_rep_is_call(AtomicGoal, Args) }
	->
		{ MaybeArgs = yes(Args) }
	;
		process_non_event_goals(Cont, MaybeArgs),
		[ unify_info(Goal) ]
	).

	% Scan through the information derived from the contour, and
	% track the location of the selected subterm.
	%
:- func find_subterm_origin(list(atom_info(R)), var_rep, term_path)
		= subterm_origin(edt_node(R)).

find_subterm_origin([], VarRep, TermPath) = input(VarRep, TermPath).
find_subterm_origin([unify_info(Goal) | AtomInfo], VarRep, TermPath)
		= Origin :-
	(
		Goal = atomic_goal_rep(_, File, Line, BoundVars, AtomicGoal),
		list__member(VarRep, BoundVars)
	->
		Origin = find_subterm_origin_unify(File, Line, AtomicGoal,
				AtomInfo, VarRep, TermPath)
	;
		Origin = find_subterm_origin(AtomInfo, VarRep, TermPath)
	).
find_subterm_origin([call_info(Ref, Goal) | AtomInfo], VarRep, TermPath)
		= Origin :-
	(
		Goal = atomic_goal_rep(_, _, _, BoundVars, AtomicGoal),
		list__member(VarRep, BoundVars)
	->
		Origin = find_subterm_origin_call(Ref, AtomicGoal, VarRep,
				TermPath)
	;
		Origin = find_subterm_origin(AtomInfo, VarRep, TermPath)
	).

:- func find_subterm_origin_unify(string, int, atomic_goal_rep,
		list(atom_info(R)), var_rep, term_path)
		= subterm_origin(edt_node(R)).

find_subterm_origin_unify(File, Line, unify_construct_rep(_, _, Args),
		AtomInfo, _, TermPath0) = Origin :-
	(
		TermPath0 = [ArgPos | TermPath],
		list__index1_det(Args, ArgPos, VarRep),
		Origin = find_subterm_origin(AtomInfo, VarRep, TermPath)
	;
		TermPath0 = [],
		Origin = unification(File, Line)
	).
find_subterm_origin_unify(_, _, unify_deconstruct_rep(VarRep, _, Args),
		AtomInfo, VarRep0, TermPath0) = Origin :-
	(
		list__nth_member_search(Args, VarRep0, ArgPos)
	->
		TermPath = [ArgPos | TermPath0],
		Origin = find_subterm_origin(AtomInfo, VarRep, TermPath)
	;
		error("find_subterm_origin_unify: arg not found")
	).
find_subterm_origin_unify(_, _, unify_assign_rep(_, Source), AtomInfo, _,
		TermPath) = find_subterm_origin(AtomInfo, Source, TermPath).
find_subterm_origin_unify(_, _, unify_simple_test_rep(_, _), _, _, _) = _ :-
	error("find_subterm_origin_unify: unexpected test").
find_subterm_origin_unify(_, _, pragma_foreign_code_rep(_), _, _, _) = _ :-
	error("find_subterm_origin_unify: unexpected pragma call").
find_subterm_origin_unify(_, _, higher_order_call_rep(_, _), _, _, _) = _ :-
	error("find_subterm_origin_unify: unexpected ho call").
find_subterm_origin_unify(_, _, method_call_rep(_, _, _), _, _, _) = _ :-
	error("find_subterm_origin_unify: unexpected method call").
find_subterm_origin_unify(_, _, plain_call_rep(_, _), _, _, _) = _ :-
	error("find_subterm_origin_unify: unexpected call").

:- func find_subterm_origin_call(R, atomic_goal_rep, var_rep, term_path)
		= subterm_origin(edt_node(R)).

find_subterm_origin_call(Ref, Call, VarRep, TermPath) = Origin :-
	(
		atomic_goal_rep_is_call(Call, Args),
		list__nth_member_search(Args, VarRep, ArgPos)
	->
		Origin = output(dynamic(Ref), ArgPos, TermPath)
	;
		error("find_subterm_origin_call: arg not found")
	).

%-----------------------------------------------------------------------------%

:- pred edt_subtree_details(S, edt_node(R), event_number, sequence_number)
		<= annotated_trace(S, R).
:- mode edt_subtree_details(in, in, out, out) is det.

edt_subtree_details(Store, dynamic(Ref), Event, SeqNo) :-
	det_edt_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, Call, _, _, Event)
	;
		Node = fail(_, Call, _, Event)
	;
		Node = excp(_, Call, _, _, Event)
	),
	call_node_from_id(Store, Call, call(_, _, _, SeqNo, _, _, _)).

:- inst trace_node_edt_node =
		bound(	exit(ground, ground, ground, ground, ground)
		;	fail(ground, ground, ground, ground)
		;	excp(ground, ground, ground, ground, ground)).

:- pred det_edt_node_from_id(S, R, trace_node(R)) <= annotated_trace(S, R).
:- mode det_edt_node_from_id(in, in, out(trace_node_edt_node)) is det.

det_edt_node_from_id(Store, Ref, Node) :-
	(
		trace_node_from_id(Store, Ref, Node0),
		(
			Node0 = exit(_, _, _, _, _)
		;
			Node0 = fail(_, _, _, _)
		;
			Node0 = excp(_, _, _, _, _)
		)
	->
		Node = Node0
	;
		error("det_edt_node_from_id: not an EXIT, FAIL or EXCP node")
	).

:- pred trace_atom_subterm_is_ground(trace_atom, arg_pos, term_path).
:- mode trace_atom_subterm_is_ground(in, in, in) is semidet.

trace_atom_subterm_is_ground(atom(_, _, Args), ArgPos, _) :-
	list__index1_det(Args, ArgPos, yes(_)).

:- func disj_event_branch_number(trace_node(R)) = int.

disj_event_branch_number(Node) = N :-
	(
		(
			Node = first_disj(_, Str)
		;
			Node = later_disj(_, Str, _)
		),
		list__last(string__words(is_semicolon, Str), LastStepStr),
		path_step_from_string(LastStepStr, disj(N0))
	->
		N = N0
	;
		error("disj_event_branch_number: not a DISJ event")
	).

:- func switch_event_branch_number(trace_node(R)) = int.

switch_event_branch_number(Node) = N :-
	(
		Node = switch(_, Str),
		list__last(string__words(is_semicolon, Str), LastStepStr),
		path_step_from_string(LastStepStr, switch(N0))
	->
		N = N0
	;
		error("switch_event_branch_number: not a SWTC event")
	).

:- pred is_semicolon(char).
:- mode is_semicolon(in) is semidet.

is_semicolon(';').

%-----------------------------------------------------------------------------%

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

