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
:- import_module io, list, bool.
:- import_module declarative_execution.

	% This type represents the possible truth values for nodes
	% in the EDT.
	%
:- type edt_truth == bool.

	% Values of this type represent EDT nodes.  This representation
	% is used by the front end (in this module), as well as the
	% oracle and user interface.
	%
:- type edt_node
			% The node is a suspected wrong answer.  The
			% argument is the atom in its final state of
			% instantiatedness (ie. at the EXIT event).
			%
	--->	wrong_answer(edt_atom)

			% The node is a suspected missing answer.  The
			% first argument is the atom in its initial state
			% of instantiatedness (ie. at the CALL event),
			% and the second argument is the list of solutions.
			% 
	;	missing_answer(edt_atom, list(edt_atom)).

:- type edt_atom == trace_atom.

	% This typeclass represents a declarative view of execution.
	%
:- typeclass mercury_edt(S, T) where [
	pred edt_root(S, T, edt_node),
	mode edt_root(in, in, out) is det,

	pred edt_children(S, T, list(T)),
	mode edt_children(in, in, out) is det
].

	% The diagnoser eventually responds with a value of this type
	% when it is called.
	%
	% XXX need to have a case for expanding an implicit tree.
	%
:- type diagnoser_response
	--->	bug_found(edt_node)
	;	no_bug_found.

:- type diagnoser_state.

:- pred diagnoser_state_init(diagnoser_state).
:- mode diagnoser_state_init(out) is det.

:- pred diagnosis(io__input_stream, io__output_stream, S, trace_node(R),
		diagnoser_response, diagnoser_state, diagnoser_state,
		io__state, io__state) <= execution_tree(S, R).
:- mode diagnosis(in, in, in, in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, char.
:- import_module declarative_oracle, declarative_user.

:- type diagnoser_state == oracle_state.

:- pragma export(diagnoser_state_init(out),
		"MR_DD_diagnoser_state_init").

diagnoser_state_init(Oracle) :-
	oracle_state_init(Oracle).

diagnosis(MdbIn, MdbOut, Store, Node, Response, State0, State) -->
	io__set_input_stream(MdbIn, OldIn),
	io__set_output_stream(MdbOut, OldOut),
	analyse_edt(wrap(Store), Node, Response, State0, State),
	io__set_input_stream(OldIn, _),
	io__set_output_stream(OldOut, _).

	% Export a monomorphic version of diagnosis/9, to make it
	% easier to call from C code.
	%
:- pred diagnosis_store(io__input_stream, io__output_stream,
		trace_node_store, trace_node(trace_node_id),
		diagnoser_response, diagnoser_state, diagnoser_state,
		io__state, io__state).
:- mode diagnosis_store(in, in, in, in, out, in, out, di, uo) is det.

:- pragma export(diagnosis_store(in, in, in, in, out, in, out, di, uo),
		"MR_DD_decl_diagnosis").
	
diagnosis_store(In, Out, Store, Node, Response, State0, State) -->
	diagnosis(In, Out, Store, Node, Response, State0, State).

%-----------------------------------------------------------------------------%

	%
	% This section defines an instance of the EDT in terms of
	% any instance of execution tree.
	%

:- instance mercury_edt(wrap(S), trace_node(R)) <= execution_tree(S, R)
	where [
		pred(edt_root/3) is trace_root,
		pred(edt_children/3) is trace_children
	].

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(T) ---> wrap(T).

:- pred trace_root(wrap(S), trace_node(R), edt_node) <= execution_tree(S, R).
:- mode trace_root(in, in, out) is det.

trace_root(wrap(Store), Node, Root) :-
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

:- pred get_answers(S, R, list(edt_atom), list(edt_atom))
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

:- pred trace_children(wrap(S), trace_node(R), list(trace_node(R)))
		<= execution_tree(S, R).
:- mode trace_children(in, in, out) is det.

trace_children(wrap(Store), Node, Children) :-
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

:- pred wrong_answer_children(S, R, list(trace_node(R)), list(trace_node(R)))
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
		wrong_answer_children(Store, Prec, [Node | Ns0], Ns)
	;
		Node = redo(_, _),
		error("wrong_answer_children: unexpected REDO node")
	;
		Node = fail(_, Call),
		call_node_from_id(Store, Call, call(Prec, _, _)),
		wrong_answer_children(Store, Prec, [Node | Ns0], Ns)
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

:- pred missing_answer_children(S, R, list(trace_node(R)), list(trace_node(R)))
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
		wrong_answer_children(Store, Prec, [Node | Ns0], Ns)
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
		missing_answer_children(Store, Next, [Node | Ns0], Ns)
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


%-----------------------------------------------------------------------------%

	%
	% This section implements the analysis.
	% It is passed an EDT, which is analysed to find a cause of the bug,
	% and this bug is then presented to the user.
	%
	% The current implementation uses a simple top-down strategy to
	% analyse the EDT.
	%

	%
	% This is what the analysis can currently find.
	%

:- type declarative_bug(T)		% <= evaluation_tree(T)
	--->	not_found
		%
		% An e_bug is an EDT whose root node is incorrect, but
		% whose children are all correct.
		%
	;	e_bug(T).


:- pred analyse_edt(S, T, diagnoser_response, oracle_state,
		oracle_state, io__state, io__state) <= mercury_edt(S, T).
:- mode analyse_edt(in, in, out, in, out, di, uo) is det.

analyse_edt(Store, EDT, no_bug_found, Oracle0, Oracle) -->
	{ edt_root(Store, EDT, RootNode) },
	query_oracle(RootNode, Answer, Oracle0, Oracle1),
	(
		{ Answer = truth_value(yes) },
		{ Bug = not_found },
		{ Oracle = Oracle1 }
	;
		{ Answer = truth_value(no) },
		analyse_edt_2(Store, EDT, Bug, Oracle1, Oracle)
	;
		{ Answer = deferred(_) },
		{ Bug = not_found },
		{ Oracle = Oracle1 }
	),
	report_bug(Store, Bug).


	%
	% Assumes the root note is not valid.
	%
:- pred analyse_edt_2(S, T, declarative_bug(T), oracle_state, oracle_state,
		io__state, io__state) <= mercury_edt(S, T).
:- mode analyse_edt_2(in, in, out, in, out, di, uo) is det.

analyse_edt_2(Store, EDT, Bug, Oracle0, Oracle) -->
	{ edt_children(Store, EDT, Children) },
	analyse_children(Store, Children, e_bug(EDT), Bug, Oracle0, Oracle).


:- pred analyse_children(S, list(T), declarative_bug(T), declarative_bug(T),
		oracle_state, oracle_state, io__state, io__state)
				<= mercury_edt(S, T).
:- mode analyse_children(in, in, in, out, in, out, di, uo) is det.

analyse_children(_, [], Bug, Bug, Oracle, Oracle) -->
	[].
analyse_children(Store, [Child | Children], Bug0, Bug, Oracle0, Oracle) -->
	{ edt_root(Store, Child, ChildNode) },
	query_oracle(ChildNode, Answer, Oracle0, Oracle1),
	(
		{ Answer = truth_value(yes) },
		analyse_children(Store, Children, Bug0, Bug, Oracle1, Oracle)
	;
		{ Answer = truth_value(no) },
		analyse_edt_2(Store, Child, Bug, Oracle1, Oracle)
	;
		{ Answer = deferred(_) },
		{ append(Children, [Child], NewChildren) },
		analyse_children(Store, NewChildren, Bug0, Bug, Oracle1,
				Oracle)
	).


:- pred report_bug(S, declarative_bug(T), io__state, io__state)
		<= mercury_edt(S, T).
:- mode report_bug(in, in, di, uo) is det.

report_bug(_, not_found) -->
	io__write_string("Bug not found.\n").
report_bug(Store, e_bug(EDT)) -->
	io__write_string("Incorrect instance found:\n\n"),
	write_root_node(Store, EDT),
	{ edt_children(Store, EDT, Children0) },
	(
		{ Children0 = [Child | Children1] }
	->
		io__write_string(" :-\n"),
		{ list__reverse(Children1, Children) },
		write_children(Store, Children),
		io__write_char('\t'),
		write_root_node(Store, Child)
	;
		[]
	),
	io__write_string(".\n\n").


:- pred write_children(S, list(T), io__state, io__state) <= mercury_edt(S, T).
:- mode write_children(in, in, di, uo) is det.

write_children(_, []) -->
	[].
write_children(Store, [Child | Children]) -->
	io__write_char('\t'),
	write_root_node(Store, Child),
	io__write_string(",\n"),
	write_children(Store, Children).


:- pred write_root_node(S, T, io__state, io__state) <= mercury_edt(S, T).
:- mode write_root_node(in, in, di, uo) is det.

write_root_node(Store, EDT) -->
	{ edt_root(Store, EDT, RootNode) },
	write_edt_node(RootNode).

