%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2004 The University of Melbourne.
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
	;	exit_diagnosis(T)
	;	abort_diagnosis.

	% The oracle state.  This is threaded around the declarative
	% debugger.
	%
:- type oracle_state.

	% Produce a new oracle state.
	%
:- pred oracle_state_init(io__input_stream, io__output_stream, oracle_state).
:- mode oracle_state_init(in, in, out) is det.

	% Add a module to the set of modules trusted by the oracle
	%
:- func add_trusted_module(string, oracle_state) = oracle_state. 

	% Query the oracle about the program being debugged.  The first
	% argument is a queue of nodes in the evaluation tree, the second
	% argument is the oracle response to any of these.  The oracle
	% state is threaded through so its contents can be updated after
	% user responses.
	%
:- pred query_oracle(list(decl_question(T))::in, oracle_response(T)::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

	% Confirm that the node found is indeed an e_bug or an i_bug.  If
	% the bug is overruled, force the oracle to forget everything
	% it knows about the evidence that led to that bug.
	%
:- pred oracle_confirm_bug(decl_bug::in, decl_evidence(T)::in,
	decl_confirmation::out, oracle_state::in, oracle_state::out,
	io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb__declarative_execution.
:- import_module mdb__declarative_user.
:- import_module mdb__tree234_cc.
:- import_module mdb__set_cc.
:- import_module mdb__util.

:- import_module bool, std_util, set.

query_oracle(Questions, Response, Oracle0, Oracle) -->
	{ query_oracle_list(Oracle0, Questions, Answers) },
	(
		{ Answers = [] }
	->
		{ list__map(make_user_question(Oracle0 ^ kb_revised),
			Questions, UserQuestions) },
		query_oracle_user(UserQuestions, Response, Oracle0, Oracle)
	;
		{ Response = oracle_answers(Answers) },
		{ Oracle = Oracle0 }
	).

:- pred make_user_question(oracle_kb::in, decl_question(T)::in,
	user_question(T)::out) is cc_multi.

make_user_question(Revised, DeclQuestion, UserQuestion) :-
	query_oracle_kb(Revised, DeclQuestion, MaybeDeclAnswer),
	(
		MaybeDeclAnswer = yes(truth_value(_, DeclTruth))
	->
		UserQuestion = question_with_default(DeclQuestion, DeclTruth)
	;
		UserQuestion = plain_question(DeclQuestion)
	).

:- pred query_oracle_user(list(user_question(T))::in, oracle_response(T)::out,
	oracle_state::in, oracle_state::out, io__state::di, io__state::uo)
	is cc_multi.

query_oracle_user(Questions, OracleResponse, Oracle0, Oracle) -->
	{ User0 = Oracle0 ^ user_state },
	query_user(Questions, UserResponse, User0, User),
	{
		UserResponse = user_answer(Question, Answer),
		OracleResponse = oracle_answers([Answer]),
		Current0 = Oracle0 ^ kb_current,
		Revised0 = Oracle0 ^ kb_revised,
		retract_oracle_kb(Question, Revised0, Revised),
		assert_oracle_kb(Question, Answer, Current0, Current),
		Oracle1 = (Oracle0
				^ kb_current := Current)
				^ kb_revised := Revised
	;
		UserResponse = no_user_answer,
		OracleResponse = no_oracle_answers,
		Oracle1 = Oracle0
	;
		UserResponse = exit_diagnosis(Node),
		OracleResponse = exit_diagnosis(Node),
		Oracle1 = Oracle0
	;
		UserResponse = abort_diagnosis,
		OracleResponse = abort_diagnosis,
		Oracle1 = Oracle0
	},
	{ Oracle = Oracle1 ^ user_state := User }.

oracle_confirm_bug(Bug, Evidence, Confirmation, Oracle0, Oracle) -->
	{ User0 = Oracle0 ^ user_state },
	user_confirm_bug(Bug, Confirmation, User0, User),
	{ Oracle1 = Oracle0 ^ user_state := User },
	{
		Confirmation = overrule_bug
	->
		list__foldl(revise_oracle, Evidence, Oracle1, Oracle)
	;
		Oracle = Oracle1
	}.

:- pred revise_oracle(decl_question(T)::in, oracle_state::in, oracle_state::out)
	is cc_multi.

revise_oracle(Question, Oracle0, Oracle) :-
	Current0 = Oracle0 ^ kb_current,
	query_oracle_kb(Current0, Question, MaybeAnswer),
	(
		MaybeAnswer = yes(Answer),
		retract_oracle_kb(Question, Current0, Current),
		Revised0 = Oracle0 ^ kb_revised,
		assert_oracle_kb(Question, Answer, Revised0, Revised),
		Oracle = (Oracle0
				^ kb_revised := Revised)
				^ kb_current := Current
	;
		MaybeAnswer = no,
		Oracle = Oracle0
	).

%-----------------------------------------------------------------------------%
		
:- type oracle_state
	--->	oracle(
			kb_current	:: oracle_kb,
				% Current information about the intended
				% interpretation.  These answers have been
				% given, but have not since been revised.

			kb_revised	:: oracle_kb,
				% Old information about the intended
				% interpretation.  These answers were given
				% and subsequently revised, but new answers
				% to the questions have not yet been given.

			user_state	:: user_state,
				% User interface.
				
			trusted_modules :: set(string)
				% If a module name is in this set then the 
				% oracle will report any calls to predicates 
				% or functions in that module as valid.
		
		).

oracle_state_init(InStr, OutStr, Oracle) :-
	oracle_kb_init(Current),
	oracle_kb_init(Old),
	user_state_init(InStr, OutStr, User),
	set.init(TrustedModules),
	Oracle = oracle(Current, Old, User, TrustedModules).
	
add_trusted_module(ModuleName, OracleState) = 
	OracleState ^ trusted_modules := 
		insert(OracleState ^ trusted_modules, ModuleName). 

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
		kb_ground_map :: map_cc(final_decl_atom, decl_truth),

		% This map stores knowledge about the completeness of the
		% set of solutions generated by calling the given initial
		% atom.  This is used, for example, in the common case that
		% the user supplies a truth value for a "missing answer"
		% node.
		%
		kb_complete_map :: map_cc(init_decl_atom, decl_truth),

		% Mapping from call atoms to information about which
		% exceptions are possible or impossible.
		%
		kb_exceptions_map :: map_cc(init_decl_atom, known_exceptions)
	).

:- type map_cc(K, V) == tree234_cc(K, V).

:- type known_exceptions
	--->	known_excp(
			set_cc(decl_exception),	% Possible exceptions.
			set_cc(decl_exception)	% Impossible exceptions.
		).

:- pred oracle_kb_init(oracle_kb).
:- mode oracle_kb_init(out) is det.

oracle_kb_init(oracle_kb(G, C, X)) :-
	tree234_cc__init(G),
	tree234_cc__init(C),
	tree234_cc__init(X).

:- pred get_kb_ground_map(oracle_kb, map_cc(final_decl_atom, decl_truth)).
:- mode get_kb_ground_map(in, out) is det.

get_kb_ground_map(KB, KB ^ kb_ground_map).

:- pred set_kb_ground_map(oracle_kb, map_cc(final_decl_atom, decl_truth),
	oracle_kb).
:- mode set_kb_ground_map(in, in, out) is det.

set_kb_ground_map(KB, M, KB ^ kb_ground_map := M).

:- pred get_kb_complete_map(oracle_kb,
	map_cc(init_decl_atom, decl_truth)).
:- mode get_kb_complete_map(in, out) is det.

get_kb_complete_map(KB, KB ^ kb_complete_map).

:- pred set_kb_complete_map(oracle_kb,
	map_cc(init_decl_atom, decl_truth), oracle_kb).
:- mode set_kb_complete_map(in, in, out) is det.

set_kb_complete_map(KB, M, KB ^ kb_complete_map := M).

:- pred get_kb_exceptions_map(oracle_kb,
	map_cc(init_decl_atom, known_exceptions)).
:- mode get_kb_exceptions_map(in, out) is det.

get_kb_exceptions_map(KB, KB ^ kb_exceptions_map).

:- pred set_kb_exceptions_map(oracle_kb,
	map_cc(init_decl_atom, known_exceptions), oracle_kb).
:- mode set_kb_exceptions_map(in, in, out) is det.

set_kb_exceptions_map(KB, M, KB ^ kb_exceptions_map := M).

%-----------------------------------------------------------------------------%

:- pred query_oracle_list(oracle_state::in, list(decl_question(T))::in,
		list(decl_answer(T))::out) is cc_multi.

query_oracle_list(_, [], []).
query_oracle_list(OS, [Q | Qs0], As) :-
	query_oracle_list(OS, Qs0, As0),
	Atom = get_decl_question_atom(Q),
	(	
		% is the atom in a trusted module?
		member(Atom ^ module_name, OS ^ trusted_modules)
	->
		As = [truth_value(get_decl_question_node(Q), yes) | As0]
	;
		query_oracle_kb(OS ^ kb_current, Q, MaybeA),
		(
			MaybeA = yes(A),
			As = [A | As0]
		;
			MaybeA = no,
			As = As0
		)
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
	Question = missing_answer(Node, Call, _Solns),
	get_kb_complete_map(KB, CMap),
	tree234_cc__search(CMap, Call, MaybeTruth),
	(
		MaybeTruth = yes(Truth),
		Result = yes(truth_value(Node, Truth))
	;
		MaybeTruth = no,
		Result = no
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
		set_cc__member(Exception, Possible, PossibleBool),
		(
			PossibleBool = yes,
			Result = yes(truth_value(Node, yes))
		;
			PossibleBool = no,
			set_cc__member(Exception, Impossible, ImpossibleBool),
			(
				ImpossibleBool = yes,
				Result = yes(truth_value(Node, no))
			;
				ImpossibleBool = no,
				Result = no
			)
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

assert_oracle_kb(missing_answer(_, Call, _), truth_value(_, Truth), KB0, KB) :-
	get_kb_complete_map(KB0, Map0),
	tree234_cc__set(Map0, Call, Truth, Map),
	set_kb_complete_map(KB0, Map, KB).

assert_oracle_kb(unexpected_exception(_, Call, Exception),
		truth_value(_, Truth), KB0, KB) :-
	get_kb_exceptions_map(KB0, Map0),
	tree234_cc__search(Map0, Call, MaybeX),
	(
		MaybeX = yes(known_excp(Possible0, Impossible0))
	;
		MaybeX = no,
		set_cc__init(Possible0),
		set_cc__init(Impossible0)
	),
	(
		Truth = yes,
		set_cc__insert(Possible0, Exception, Possible),
		Impossible = Impossible0
	;
		Truth = no,
		Possible = Possible0,
		set_cc__insert(Impossible0, Exception, Impossible)
	),
	tree234_cc__set(Map0, Call, known_excp(Possible, Impossible), Map),
	set_kb_exceptions_map(KB0, Map, KB).

:- pred retract_oracle_kb(decl_question(T), oracle_kb, oracle_kb).
:- mode retract_oracle_kb(in, in, out) is cc_multi.

retract_oracle_kb(wrong_answer(_, FinalAtom), KB0, KB) :-
	Map0 = KB0 ^ kb_ground_map,
	tree234_cc__delete(Map0, FinalAtom, Map),
	KB = KB0 ^ kb_ground_map := Map.

retract_oracle_kb(missing_answer(_, InitAtom, _), KB0, KB) :-
	CompleteMap0 = KB0 ^ kb_complete_map,
	tree234_cc__delete(CompleteMap0, InitAtom, CompleteMap),
	KB = KB0 ^ kb_complete_map := CompleteMap.

retract_oracle_kb(unexpected_exception(_, InitAtom, Exception), KB0, KB) :-
	ExceptionsMap0 = KB0 ^ kb_exceptions_map,
	tree234_cc__search(ExceptionsMap0, InitAtom, MaybeKnownExceptions0),
	(
		MaybeKnownExceptions0 = yes(known_excp(Possible0, Impossible0))
	->
		set_cc__delete(Possible0, Exception, Possible),
		set_cc__delete(Impossible0, Exception, Impossible),
		KnownExceptions = known_excp(Possible, Impossible),
		tree234_cc__set(ExceptionsMap0, InitAtom, KnownExceptions,
			ExceptionsMap)
	;
		ExceptionsMap = ExceptionsMap0
	),
	KB = KB0 ^ kb_exceptions_map := ExceptionsMap.

