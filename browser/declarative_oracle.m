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
:- type oracle_response(T)
	--->	oracle_answers(list(decl_answer(T)))
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
:- pred query_oracle(list(decl_question(T))::in, oracle_response(T)::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

	% Confirm that the node found is indeed an e_bug or an i_bug.
	%
:- pred oracle_confirm_bug(decl_bug::in, decl_confirmation::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mdb__declarative_user, mdb__tree234_cc, mdb__util.
:- import_module bool, std_util, set, require.

query_oracle(Questions, Response, Oracle0, Oracle) -->
	{ get_oracle_kb(Oracle0, KB0) },
	{ query_oracle_kb_list(KB0, Questions, Answers) },
	(
		{ Answers = [] }
	->
		{ get_oracle_user(Oracle0, User0) },
		query_user(Questions, UserResponse, User0, User),
		{
			UserResponse = user_answer(Question, Answer),
			assert_oracle_kb(Question, Answer, KB0, KB),
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
		map_cc(decl_atom, decl_truth),

		% Mapping from call atoms to their solution sets.
		% The sets in this map are all complete---but they may
		% contain wrong answers.
		%
		map_cc(decl_atom, set(decl_atom)),

		% Mapping from call atoms to their solution sets.
		% The sets in this map are all incomplete---there
		% exists a correct solution which is not in the set.
		%
		map_cc(decl_atom, set(decl_atom)),

		% Mapping from call atoms to information about which
		% exceptions are possible or impossible.
		%
		map_cc(decl_atom, known_exceptions)
	).

:- type map_cc(K, V) == tree234_cc(K, V).

:- type known_exceptions
	--->	known_excp(
			set(univ),		% Possible exceptions.
			set(univ)		% Impossible exceptions.
		).

:- pred oracle_kb_init(oracle_kb).
:- mode oracle_kb_init(out) is det.

oracle_kb_init(oracle_kb(G, Y, N, X)) :-
	tree234_cc__init(G),
	tree234_cc__init(Y),
	tree234_cc__init(N),
	tree234_cc__init(X).

:- pred get_kb_ground_map(oracle_kb, map_cc(decl_atom, decl_truth)).
:- mode get_kb_ground_map(in, out) is det.

get_kb_ground_map(oracle_kb(Map, _, _, _), Map).

:- pred set_kb_ground_map(oracle_kb, map_cc(decl_atom, decl_truth), oracle_kb).
:- mode set_kb_ground_map(in, in, out) is det.

set_kb_ground_map(oracle_kb(_, Y, N, X), G, oracle_kb(G, Y, N, X)).

:- pred get_kb_complete_map(oracle_kb, map_cc(decl_atom, set(decl_atom))).
:- mode get_kb_complete_map(in, out) is det.

get_kb_complete_map(oracle_kb(_, Map, _, _), Map).

:- pred set_kb_complete_map(oracle_kb, map_cc(decl_atom, set(decl_atom)),
		oracle_kb).
:- mode set_kb_complete_map(in, in, out) is det.

set_kb_complete_map(oracle_kb(G, _, N, X), Y, oracle_kb(G, Y, N, X)).

:- pred get_kb_incomplete_map(oracle_kb, map_cc(decl_atom, set(decl_atom))).
:- mode get_kb_incomplete_map(in, out) is det.

get_kb_incomplete_map(oracle_kb(_, _, Map, _), Map).

:- pred set_kb_incomplete_map(oracle_kb, map_cc(decl_atom, set(decl_atom)),
		oracle_kb).
:- mode set_kb_incomplete_map(in, in, out) is det.

set_kb_incomplete_map(oracle_kb(G, Y, _, X), N, oracle_kb(G, Y, N, X)).

:- pred get_kb_exceptions_map(oracle_kb, map_cc(decl_atom, known_exceptions)).
:- mode get_kb_exceptions_map(in, out) is det.

get_kb_exceptions_map(oracle_kb(_, _, _, Map), Map).

:- pred set_kb_exceptions_map(oracle_kb, map_cc(decl_atom, known_exceptions),
		oracle_kb).
:- mode set_kb_exceptions_map(in, in, out) is det.

set_kb_exceptions_map(oracle_kb(G, Y, N, _), X, oracle_kb(G, Y, N, X)).

%-----------------------------------------------------------------------------%

:- pred query_oracle_kb_list(oracle_kb, list(decl_question(T)),
		list(decl_answer(T))).
:- mode query_oracle_kb_list(in, in, out) is cc_multi.

query_oracle_kb_list(_, [], []).
query_oracle_kb_list(KB, [Q | Qs0], As) :-
	query_oracle_kb_list(KB, Qs0, As0),
	query_oracle_kb(KB, Q, MaybeA),
	(
		MaybeA = yes(A),
		As = [A | As0]
	;
		MaybeA = no,
		As = As0
	).

:- pred query_oracle_kb(oracle_kb, decl_question(T), maybe(decl_answer(T))).
:- mode query_oracle_kb(in, in, out) is cc_multi.

query_oracle_kb(KB, Question, Result) :-
	Question = wrong_answer(Node, Atom),
	get_kb_ground_map(KB, Map),
	tree234_cc__search(Map, Atom, MaybeTruth),
	(
		MaybeTruth = yes(Truth),
		Result = yes(truth_value(Node, Truth))
	;
		MaybeTruth = no,
		Result = no
	).

query_oracle_kb(KB, Question, Result) :-
	Question = missing_answer(Node, Call, Solns),
	set__list_to_set(Solns, Ss),
	get_kb_complete_map(KB, CMap),
	tree234_cc__search(CMap, Call, MaybeCSs),
	(
		MaybeCSs = yes(CSs),
		set__subset(CSs, Ss)
	->
		Result = yes(truth_value(Node, yes))
	;
		get_kb_incomplete_map(KB, IMap),
		tree234_cc__search(IMap, Call, MaybeISs),
		(
			MaybeISs = yes(ISs),
			(
				set__subset(Ss, ISs)
			->
				Result = yes(truth_value(Node, no))
			;
				Result = no
			)
		;
			MaybeISs = no,
			Result = no
		)
	).

query_oracle_kb(KB, Question, Result) :-
	Question = unexpected_exception(Node, Call, Exception),
	get_kb_exceptions_map(KB, XMap),
	tree234_cc__search(XMap, Call, MaybeX),
	(
		MaybeX = no,
		Result = no
	;
		MaybeX = yes(known_excp(Possible, Impossible)),
		(
			set__member(Exception, Possible)
		->
			Result = yes(truth_value(Node, yes))
		;
			set__member(Exception, Impossible)
		->
			Result = yes(truth_value(Node, no))
		;
			Result = no
		)
	).

	% assert_oracle_kb/3 assumes that the asserted fact is consistent
	% with the current knowledge base.  This will generally be the
	% case, since the user will never be asked questions which
	% the knowledge base knows anything about.
	%
:- pred assert_oracle_kb(decl_question(T), decl_answer(T), oracle_kb,
		oracle_kb).
:- mode assert_oracle_kb(in, in, in, out) is cc_multi.

assert_oracle_kb(_, suspicious_subterm(_, _, _), KB, KB).

assert_oracle_kb(wrong_answer(_, Atom), truth_value(_, Truth), KB0, KB) :-
	get_kb_ground_map(KB0, Map0),
	tree234_cc__set(Map0, Atom, Truth, Map),
	set_kb_ground_map(KB0, Map, KB).

assert_oracle_kb(missing_answer(_, Call, Solns), truth_value(_, yes),
		KB0, KB) :-
	get_kb_complete_map(KB0, Map0),
	set__list_to_set(Solns, Ss0),
	tree234_cc__search(Map0, Call, MaybeOldSs),
	(
		MaybeOldSs = yes(OldSs),
			%
			% The sets are both complete, so their
			% intersection must be also.
			%
		set__intersect(OldSs, Ss0, Ss)
	;
		MaybeOldSs = no,
		Ss = Ss0
	),
	tree234_cc__set(Map0, Call, Ss, Map),
	set_kb_complete_map(KB0, Map, KB).

assert_oracle_kb(missing_answer(_, Call, Solns), truth_value(_, no), KB0, KB) :-
	get_kb_incomplete_map(KB0, Map0),
	set__list_to_set(Solns, Ss),
		%
		% XXX should also keep the old incomplete set around, too.
		% It can still give us information that the new one can't.
		%
	tree234_cc__set(Map0, Call, Ss, Map),
	set_kb_incomplete_map(KB0, Map, KB).

assert_oracle_kb(unexpected_exception(_, Call, Exception),
		truth_value(_, Truth), KB0, KB) :-
	get_kb_exceptions_map(KB0, Map0),
	tree234_cc__search(Map0, Call, MaybeX),
	(
		MaybeX = yes(known_excp(Possible0, Impossible0))
	;
		MaybeX = no,
		set__init(Possible0),
		set__init(Impossible0)
	),
	(
		Truth = yes,
		set__insert(Possible0, Exception, Possible),
		Impossible = Impossible0
	;
		Truth = no,
		Possible = Possible0,
		set__insert(Impossible0, Exception, Impossible)
	),
	tree234_cc__set(Map0, Call, known_excp(Possible, Impossible), Map),
	set_kb_exceptions_map(KB0, Map, KB).

