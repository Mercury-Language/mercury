%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_oracle.m
% Author: Mark Brown
% Purpose:
%	This module implements the oracle for a Mercury declarative debugger.
% It is called by the front end of the declarative debugger to provide 
% information about the intended interpretation of the program being
% debugged.
%
% The module has a knowledge base as a sub-component.  This is a cache
% for all the assumptions that the oracle is currently making.  When
% the oracle is queried, it first checks the KB to see if an answer
% is available there.
%
% If no answer is available in the KB, then the oracle uses the UI 
% (in browser/declarative_user.m) to get the required answer from the
% user.  If any new knowledge is obtained, it is added to the KB so
% the user will not be asked the same question twice.
%

:- module mdb__declarative_oracle.
:- interface.
:- import_module mdb__declarative_debugger.
:- import_module list, io.

	% A response that the oracle gives to a query about the
	% truth of an EDT node.
	%
:- type oracle_response
	--->	oracle_answers(list(decl_answer))
	;	no_oracle_answers
	;	abort_diagnosis.

	% The oracle state.  This is threaded around the declarative
	% debugger.
	%
:- type oracle_state.

	% Produce a new oracle state.
	%
:- pred oracle_state_init(io__input_stream, io__output_stream, oracle_state).
:- mode oracle_state_init(in, in, out) is det.

	% Query the oracle about the program being debugged.  The first
	% argument is a queue of nodes in the evaluation tree, the second
	% argument is the oracle response to any of these.  The oracle
	% state is threaded through so its contents can be updated after
	% user responses.
	%
:- pred query_oracle(list(decl_question)::in, oracle_response::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

	% Confirm that the node found is indeed an e_bug or an i_bug.
	%
:- pred oracle_confirm_bug(decl_bug::in, decl_confirmation::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_user, mdb__util.
:- import_module bool, std_util, map, set, require.

query_oracle(Queries, Response, Oracle0, Oracle) -->
	{ get_oracle_kb(Oracle0, KB0) },
	{ list__filter_map(query_oracle_kb(KB0), Queries, Answers) },
	(
		{ Answers = [] }
	->
		{ get_oracle_user(Oracle0, User0) },
		query_user(Queries, UserResponse, User0, User),
		{
			UserResponse = user_answer(Answer),
			assert_oracle_kb(Answer, KB0, KB),
			Response = oracle_answers([Answer])
		;
			UserResponse = no_user_answer,
			Response = no_oracle_answers,
			KB = KB0
		;
			UserResponse = abort_diagnosis,
			Response = abort_diagnosis,
			KB = KB0
		},
		{ set_oracle_kb(Oracle0, KB, Oracle1) },
		{ set_oracle_user(Oracle1, User, Oracle) }
	;
		{ Response = oracle_answers(Answers) },
		{ Oracle = Oracle0 }
	).

oracle_confirm_bug(Bug, Confirmation, Oracle0, Oracle) -->
	{ get_oracle_user(Oracle0, User0) },
	user_confirm_bug(Bug, Confirmation, User0, User),
	{ set_oracle_user(Oracle0, User, Oracle) }.

%-----------------------------------------------------------------------------%
		
:- type oracle_state
	--->	oracle(
			oracle_kb,		% Knowledge base.
			user_state		% User interface.
		).

oracle_state_init(InStr, OutStr, Oracle) :-
	user_state_init(InStr, OutStr, User),
	oracle_kb_init(KB),
	Oracle = oracle(KB, User).

:- pred get_oracle_kb(oracle_state, oracle_kb).
:- mode get_oracle_kb(in, out) is det.

get_oracle_kb(oracle(KB, _), KB).

:- pred set_oracle_kb(oracle_state, oracle_kb, oracle_state).
:- mode set_oracle_kb(in, in, out) is det.

set_oracle_kb(oracle(_, UI), KB, oracle(KB, UI)).

:- pred get_oracle_user(oracle_state, user_state).
:- mode get_oracle_user(in, out) is det.

get_oracle_user(oracle(_, UI), UI).

:- pred set_oracle_user(oracle_state, user_state, oracle_state).
:- mode set_oracle_user(in, in, out) is det.

set_oracle_user(oracle(KB, _), UI, oracle(KB, UI)).

%-----------------------------------------------------------------------------%

	%
	% This section implements the oracle knowledge base, which
	% stores anything that the debugger knows about the intended
	% interpretation.  This can be used to check the correctness
	% of an EDT node.
	%

	% The type of the knowledge base.  Other fields may be added in
	% the future, such as for assertions made on-the-fly by the user,
	% or assertions in the program text.
	%
:- type oracle_kb
	---> oracle_kb(

		% For ground atoms, the knowledge is represented directly
		% with a map.  This is used, for example, in the common
		% case that the user supplies a truth value for a
		% "wrong answer" node.
		%
		map(decl_atom, decl_truth),

		% Mapping from call atoms to their solution sets.
		% The sets in this map are all complete---but they may
		% contain wrong answers.
		%
		map(decl_atom, set(decl_atom)),

		% Mapping from call atoms to their solution sets.
		% The sets in this map are all incomplete---there
		% exists a correct solution which is not in the set.
		%
		map(decl_atom, set(decl_atom)),

		% Mapping from call atoms to information about which
		% exceptions are possible or impossible.
		%
		map(decl_atom, known_exceptions)
	).

:- type known_exceptions
	--->	known_excp(
			set(univ),		% Possible exceptions.
			set(univ)		% Impossible exceptions.
		).

:- pred oracle_kb_init(oracle_kb).
:- mode oracle_kb_init(out) is det.

oracle_kb_init(oracle_kb(G, Y, N, X)) :-
	map__init(G),
	map__init(Y),
	map__init(N),
	map__init(X).

:- pred get_kb_ground_map(oracle_kb, map(decl_atom, decl_truth)).
:- mode get_kb_ground_map(in, out) is det.

get_kb_ground_map(oracle_kb(Map, _, _, _), Map).

:- pred set_kb_ground_map(oracle_kb, map(decl_atom, decl_truth), oracle_kb).
:- mode set_kb_ground_map(in, in, out) is det.

set_kb_ground_map(oracle_kb(_, Y, N, X), G, oracle_kb(G, Y, N, X)).

:- pred get_kb_complete_map(oracle_kb, map(decl_atom, set(decl_atom))).
:- mode get_kb_complete_map(in, out) is det.

get_kb_complete_map(oracle_kb(_, Map, _, _), Map).

:- pred set_kb_complete_map(oracle_kb, map(decl_atom, set(decl_atom)),
		oracle_kb).
:- mode set_kb_complete_map(in, in, out) is det.

set_kb_complete_map(oracle_kb(G, _, N, X), Y, oracle_kb(G, Y, N, X)).

:- pred get_kb_incomplete_map(oracle_kb, map(decl_atom, set(decl_atom))).
:- mode get_kb_incomplete_map(in, out) is det.

get_kb_incomplete_map(oracle_kb(_, _, Map, _), Map).

:- pred set_kb_incomplete_map(oracle_kb, map(decl_atom, set(decl_atom)),
		oracle_kb).
:- mode set_kb_incomplete_map(in, in, out) is det.

set_kb_incomplete_map(oracle_kb(G, Y, _, X), N, oracle_kb(G, Y, N, X)).

:- pred get_kb_exceptions_map(oracle_kb, map(decl_atom, known_exceptions)).
:- mode get_kb_exceptions_map(in, out) is det.

get_kb_exceptions_map(oracle_kb(_, _, _, Map), Map).

:- pred set_kb_exceptions_map(oracle_kb, map(decl_atom, known_exceptions),
		oracle_kb).
:- mode set_kb_exceptions_map(in, in, out) is det.

set_kb_exceptions_map(oracle_kb(G, Y, N, _), X, oracle_kb(G, Y, N, X)).

%-----------------------------------------------------------------------------%

:- pred query_oracle_kb(oracle_kb, decl_question, decl_answer).
:- mode query_oracle_kb(in, in, out) is semidet.

query_oracle_kb(KB, Node, truth_value(Node, Truth)) :-
	Node = wrong_answer(Atom),
	get_kb_ground_map(KB, Map),
	map__search(Map, Atom, Truth).

query_oracle_kb(KB, Node, truth_value(Node, Truth)) :-
	Node = missing_answer(Call, Solns),
	set__list_to_set(Solns, Ss),
	get_kb_complete_map(KB, CMap),
	(
		map__search(CMap, Call, CSs),
		set__subset(CSs, Ss)
	->
		Truth = yes
	;
		get_kb_incomplete_map(KB, IMap),
		map__search(IMap, Call, ISs),
		set__subset(Ss, ISs),
		Truth = no
	).

query_oracle_kb(KB, Node, truth_value(Node, Truth)) :-
	Node = unexpected_exception(Call, Exception),
	get_kb_exceptions_map(KB, XMap),
	map__search(XMap, Call, known_excp(Possible, Impossible)),
	(
		set__member(Exception, Possible)
	->
		Truth = yes
	;
		set__member(Exception, Impossible),
		Truth = no
	).

	% assert_oracle_kb/3 assumes that the asserted fact is consistent
	% with the current knowledge base.  This will generally be the
	% case, since the user will never be asked questions which
	% the knowledge base knows anything about.
	%
:- pred assert_oracle_kb(decl_answer, oracle_kb, oracle_kb).
:- mode assert_oracle_kb(in, in, out) is det.

assert_oracle_kb(suspicious_subterm(_, _, _), KB, KB).

assert_oracle_kb(truth_value(wrong_answer(Atom), Truth), KB0, KB) :-
	get_kb_ground_map(KB0, Map0),
	map__det_insert(Map0, Atom, Truth, Map),
	set_kb_ground_map(KB0, Map, KB).

assert_oracle_kb(truth_value(missing_answer(Call, Solns), yes), KB0, KB) :-
	get_kb_complete_map(KB0, Map0),
	set__list_to_set(Solns, Ss0),
	(
		map__search(Map0, Call, OldSs)
	->
			% The sets are both complete, so their
			% intersection must be also.
			%
		set__intersect(OldSs, Ss0, Ss),
		map__set(Map0, Call, Ss, Map)
	;
		map__det_insert(Map0, Call, Ss0, Map)
	),
	set_kb_complete_map(KB0, Map, KB).

assert_oracle_kb(truth_value(missing_answer(Call, Solns), no), KB0, KB) :-
	get_kb_incomplete_map(KB0, Map0),
	set__list_to_set(Solns, Ss),
		%
		% XXX should also keep the old incomplete set around, too.
		% It can still give us information that the new one can't.
		%
	map__set(Map0, Call, Ss, Map),
	set_kb_incomplete_map(KB0, Map, KB).

assert_oracle_kb(truth_value(unexpected_exception(Call, Exception), Truth),
		KB0, KB) :-

	get_kb_exceptions_map(KB0, Map0),
	(
		map__search(Map0, Call, known_excp(Possible0, Impossible0))
	->
		Possible1 = Possible0,
		Impossible1 = Impossible0
	;
		set__init(Possible1),
		set__init(Impossible1)
	),
	(
		Truth = yes,
		set__insert(Possible1, Exception, Possible),
		Impossible = Impossible1
	;
		Truth = no,
		Possible = Possible1,
		set__insert(Impossible1, Exception, Impossible)
	),
	map__set(Map0, Call, known_excp(Possible, Impossible), Map),
	set_kb_exceptions_map(KB0, Map, KB).

