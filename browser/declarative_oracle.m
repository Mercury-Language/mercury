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
:- import_module mdb__declarative_execution.

:- import_module list, io, bool, string.

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
:- pred add_trusted_module(string::in, oracle_state::in, oracle_state::out) 
	is det. 

	% Add a predicate/function to the set of predicates/functions trusted 
	% by the oracle.
	%
:- pred add_trusted_pred_or_func(proc_layout::in, oracle_state::in, 
	oracle_state::out) is det. 

	% remove_trusted(N, !Oracle).
	% Removes the (N-1)th trusted object from the set of trusted objects.
	% Fails if there are fewer than N-1 trusted modules (or N < 0).
	% The trusted set is turned into a sorted list before finding the
	% (N-1)th element.
	%
:- pred remove_trusted(int::in, oracle_state::in, oracle_state::out)
	is semidet.

	% get_trusted_list(Oracle, MDBCommandFormat, String).
	% Return a string listing the trusted objects.
	% If MDBCommandFormat is true then returns the list so that it can be
	% run as a series of mdb `trust' commands.  Otherwise returns them
	% in a format suitable for display only.
	%
:- pred get_trusted_list(oracle_state::in, bool::in, string::out) is det.

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

:- import_module mdb__declarative_user.
:- import_module mdb__tree234_cc.
:- import_module mdb__set_cc.
:- import_module mdb__util.

:- import_module bool, std_util, set, int.

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
				
			trusted :: set(trusted_module_or_predicate)
				% Modules and predicates/functions trusted
				% by the oracle.
		).

oracle_state_init(InStr, OutStr, Oracle) :-
	oracle_kb_init(Current),
	oracle_kb_init(Old),
	user_state_init(InStr, OutStr, User),
	set.init(TrustedModules),
	Oracle = oracle(Current, Old, User, TrustedModules).
	
%-----------------------------------------------------------------------------%

:- type trusted_module_or_predicate
	--->	all(string) % all predicates/functions in a module
	;	specific(
			pred_or_func,	
			string,		% module name
			string,		% pred or func name
			int		% arity
		).

add_trusted_module(ModuleName, !Oracle) :-
	insert(!.Oracle ^ trusted, all(ModuleName), Trusted),
	!:Oracle = !.Oracle ^ trusted := Trusted.

add_trusted_pred_or_func(ProcLayout, !Oracle) :-
	ProcId = get_proc_id_from_layout(ProcLayout),
	(
		ProcId = proc(ModuleName, PredOrFunc, _, Name, Arity, _)
	;
		ProcId = uci_proc(ModuleName, _, _, Name, Arity, _),
		PredOrFunc = predicate
	),
	insert(!.Oracle ^ trusted, specific(PredOrFunc, ModuleName, Name, 
		Arity), Trusted),
	!:Oracle = !.Oracle ^ trusted := Trusted.

remove_trusted(N, !Oracle) :-
	TrustedList = to_sorted_list(!.Oracle ^ trusted),
	index0(TrustedList, N, ObjectToDelete),
	delete_all(TrustedList, ObjectToDelete, NewTrustedList),
	!:Oracle = !.Oracle ^ trusted := sorted_list_to_set(NewTrustedList). 

get_trusted_list(Oracle, CommandFormat, List) :-
	Trusted = to_sorted_list(Oracle ^ trusted),
	(
		CommandFormat = yes,
		foldl(format_trust_command, Trusted, "", List)
	;
		CommandFormat = no,
		foldl(format_trust_display, Trusted, {0, "Trusted Objects:\n"}, 
			{I, List0}),
		(
			I = 0
		->
			List = "There are no trusted modules, predicates "++
				"or functions.\n"
		;
			List = List0
		)
	).

:- pred format_trust_command(trusted_module_or_predicate::in, string::in,
	string::out) is det.

format_trust_command(all(ModuleName), S, S++"trust "++ModuleName++"\n").
format_trust_command(specific(PredOrFunc, ModuleName, Name, Arity), S, 
		S++Command) :-
	(
		PredOrFunc = predicate,
		PredOrFuncStr = "pred*",
		ArityStr = int_to_string(Arity)
	;
		PredOrFunc = function,
		PredOrFuncStr = "func*",
		ArityStr = int_to_string(Arity - 1)
	),
	Command = "trust "++PredOrFuncStr++ModuleName++"."++Name++"/"++
		ArityStr++ "\n".
		
:- pred format_trust_display(trusted_module_or_predicate::in, {int,string}::in,
	{int,string}::out) is det.

format_trust_display(all(ModuleName), {I, S}, 
	{I + 1, S++int_to_string(I)++": module "++ModuleName++"\n"}).
format_trust_display(specific(PredOrFunc, ModuleName, Name, Arity), {I, S}, 
		{I + 1, S++Display}) :-
	(
		PredOrFunc = predicate,
		PredOrFuncStr = "pred",
		ArityStr = int_to_string(Arity)
	;
		PredOrFunc = function,
		PredOrFuncStr = "func",
		ArityStr = int_to_string(Arity - 1)
	),
	Display = int_to_string(I)++": "++PredOrFuncStr++" "++ModuleName++"."++
		Name++"/"++ArityStr++"\n".
		
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
		trusted(Atom ^ proc_layout, OS)
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

:- pred trusted(proc_layout::in, oracle_state::in) is semidet.

trusted(ProcLayout, Oracle) :-
	ProcId = get_proc_id_from_layout(ProcLayout),
	(
		ProcId = proc(Module, PredOrFunc, _, Name, Arity, _),
		(
			set.member(all(Module), Oracle ^ trusted)
		;
			set.member(specific(PredOrFunc, Module, Name, Arity),
				Oracle ^ trusted)
		)
	;
		ProcId = uci_proc(_, _, _, _, _, _)
	).

:- pred query_oracle_kb(oracle_kb::in, decl_question(T)::in,
	maybe(decl_answer(T))::out) is cc_multi.

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
	% insert all modes of the predicate/function
	foldl(add_atom_to_ground_map(Truth, Atom),
		get_all_modes_for_layout(Atom ^ final_atom ^ proc_layout),
		Map0, Map),
	set_kb_ground_map(KB0, Map, KB).

:- pred add_atom_to_ground_map(decl_truth::in, final_decl_atom::in, 
	proc_layout::in, map_cc(final_decl_atom, decl_truth)::in,
	map_cc(final_decl_atom, decl_truth)::out) is cc_multi.

add_atom_to_ground_map(Truth, FinalAtom, ProcLayout, Map0, Map) :-
	tree234_cc.set(Map0, final_decl_atom(
		atom(ProcLayout, FinalAtom ^ final_atom ^ atom_args),
		FinalAtom ^ final_io_actions), Truth, Map).

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

