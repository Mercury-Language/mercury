%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_analyser.m
% Author: Mark Brown
%
% This module defines Evaluation Dependency Trees (EDTs), and
% implements an analysis algorithm which finds bugs in such trees.
%

:- module mdb__declarative_analyser.
:- interface.
:- import_module list.
:- import_module mdb__declarative_debugger.

	% This typeclass defines how EDTs may be accessed by this module.
	% An EDT is a tree of nodes, each of which contains a question
	% about the truth of an assertion.  The children of a node may
	% not be immediately accessible if the sub-tree beneath that
	% node is represented implicitly.  In this case, the analyser
	% must request that it be made explicit before continuing.
	%
:- typeclass mercury_edt(S, T) where [
		
		% Gives the root node of an EDT.
		%
	pred edt_root_question(S, T, decl_question),
	mode edt_root_question(in, in, out) is det,
	
		% If this node is an e_bug, then find the bug.
		%
	pred edt_root_e_bug(S, T, decl_e_bug),
	mode edt_root_e_bug(in, in, out) is det,

		% Gives the list of children of a tree.  If the tree is
		% represented implicitly, then the procedure fails.
		%
	pred edt_children(S, T, list(T)),
	mode edt_children(in, in, out) is semidet
].

:- type analyser_response(T)

			% There are no suspects left, and no incorrect
			% nodes have been found.
			%
	--->	no_suspects
	
			% A suspect who is guilty.
			%
	;	bug_found(decl_bug)

			% The analyser desires answers to any of a list
			% of queries.
			%
	;	oracle_queries(list(decl_question))

			% The analyser requires the given implicit sub-tree
			% to be made explicit.
			%
	;	require_explicit(T).

:- type analyser_state(T).

:- pred analyser_state_init(analyser_state(T)).
:- mode analyser_state_init(out) is det.

	% Perform analysis on the given EDT, which may be a new tree
	% to diagnose, or a sub-tree that was required to be made
	% explicit.
	%
:- pred start_analysis(S, T, analyser_response(T), analyser_state(T),
		analyser_state(T)) <= mercury_edt(S, T).
:- mode start_analysis(in, in, out, in, out) is det.

	% Continue analysis after the oracle has responded with some
	% answers.
	%
:- pred continue_analysis(S, list(decl_answer), analyser_response(T),
		analyser_state(T), analyser_state(T)) <= mercury_edt(S, T).
:- mode continue_analysis(in, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, bool, require.

	% The analyser state represents a set of suspects.  We
	% consider one incorrect node at a time, and store its suspect
	% children.
	%
:- type analyser_state(T)
	--->	analyser(
				% Current incorrect node (initially `no').
				% This is the most recent node that the
				% oracle has said is incorrect.
				%
			maybe(prime_suspect(T)),

				% Current suspects.
				%
			list(suspect(T))
	).

analyser_state_init(analyser(no, [])).

start_analysis(Store, Tree, Response, _, Analyser) :-
	edt_root_question(Store, Tree, Question),
	Response = oracle_queries([Question]),
	create_suspect(Store, Tree, Suspect),
	Analyser = analyser(no, [Suspect]).

continue_analysis(Store, Answers, Response, Analyser0, Analyser) :-
	(
		find_incorrect_suspect(Answers, Analyser0, Suspect)
	->
		make_new_prime_suspect(Store, Suspect, Response, Analyser0,
				Analyser)
	;
		remove_suspects(Store, Answers, Response, Analyser0, Analyser)
	).


	% Find an answer which is `no' and find the suspect that
	% corresponds to it, or else fail.
	%
:- pred find_incorrect_suspect(list(decl_answer), analyser_state(T),
		suspect(T)).
:- mode find_incorrect_suspect(in, in, out) is semidet.

find_incorrect_suspect([Answer | Answers], Analyser, Child) :-
	Analyser = analyser(_, Suspects),
	(
		Answer = _ - no,
		find_matching_suspects(Answer, Suspects, [Match | _], _)
	->
		Match = Child
	;
		find_incorrect_suspect(Answers, Analyser, Child)
	).

	% Create a new prime suspect from the given suspect, which is
	% assumed to be incorrect.
	%
:- pred make_new_prime_suspect(S, suspect(T), analyser_response(T),
		analyser_state(T), analyser_state(T)) <= mercury_edt(S, T).
:- mode make_new_prime_suspect(in, in, out, in, out) is det.

make_new_prime_suspect(Store, Suspect, Response, Analyser0, Analyser) :-
	Analyser0 = analyser(MaybeOldPrime, _),
	suspect_get_edt_node(Suspect, Tree),
	create_prime_suspect(Suspect, MaybeOldPrime, Prime),
	(
		edt_children(Store, Tree, Children)
	->
		make_suspects(Store, Children, Suspects, Queries),
		(
			Queries = []
		->
			edt_root_e_bug(Store, Tree, EBug),
			Response = bug_found(e_bug(EBug))
		;
			Response = oracle_queries(Queries)
		)
	;
			% The real suspects cannot be found, so we
			% just use the empty list.
			%
		Suspects = [],
		Response = require_explicit(Tree)
	),
	Analyser = analyser(yes(Prime), Suspects).

:- pred make_suspects(S, list(T), list(suspect(T)), list(decl_question))
		<= mercury_edt(S, T).
:- mode make_suspects(in, in, out, out) is det.

make_suspects(_, [], [], []).
make_suspects(Store, [Tree | Trees], [Suspect | Ss], [Query | Qs]) :-
	create_suspect(Store, Tree, Suspect),
	Suspect = suspect(_, Query),
	make_suspects(Store, Trees, Ss, Qs).

	% Go through the answers (none of which should be `no') and
	% remove the corresponding children from the suspect list.
	%
:- pred remove_suspects(S, list(decl_answer), analyser_response(T),
		analyser_state(T), analyser_state(T)) <= mercury_edt(S, T).
:- mode remove_suspects(in, in, out, in, out) is det.

remove_suspects(Store, [], Response, Analyser, Analyser) :-
	Analyser = analyser(MaybePrime, Suspects),
	(
		Suspects = []
	->
		(
			MaybePrime = yes(Prime)
		->
			prime_suspect_get_edt_node(Prime, Tree),
			edt_root_e_bug(Store, Tree, EBug),
			Response = bug_found(e_bug(EBug))
		;
			Response = no_suspects
		)
	;
		list__map(suspect_get_question, Suspects, Queries),
		Response = oracle_queries(Queries)
	).

remove_suspects(Store, [Answer | Answers], Response, Analyser0,
		Analyser) :-

	(
		Answer = _ - yes
	->
		Analyser0 = analyser(MaybeTree, Suspects0),
		find_matching_suspects(Answer, Suspects0, _, Suspects),
		Analyser1 = analyser(MaybeTree, Suspects),
		remove_suspects(Store, Answers, Response, Analyser1, Analyser)
	;
		error("remove_suspects: unexpected incorrect node")
	).

%-----------------------------------------------------------------------------%

:- type suspect(T)
	--->	suspect(T, decl_question).

:- pred create_suspect(S, T, suspect(T)) <= mercury_edt(S, T).
:- mode create_suspect(in, in, out) is det.

create_suspect(S, T, Suspect) :-
	edt_root_question(S, T, Question),
	Suspect = suspect(T, Question).

:- pred suspect_get_edt_node(suspect(T), T).
:- mode suspect_get_edt_node(in, out) is det.

suspect_get_edt_node(suspect(Node, _), Node).

:- pred suspect_get_question(suspect(T), decl_question).
:- mode suspect_get_question(in, out) is det.

suspect_get_question(suspect(_, Question), Question).

:- pred suspect_answer_match(suspect(T), decl_answer, decl_truth).
:- mode suspect_answer_match(in, in, out) is semidet.

suspect_answer_match(suspect(_, Question), Question - Truth, Truth).

:- pred find_matching_suspects(decl_answer, list(suspect(T)),
		list(suspect(T)), list(suspect(T))).
:- mode find_matching_suspects(in, in, out, out) is det.

find_matching_suspects(Answer, Suspects, Matches, NoMatches) :-
	P = (pred(S::in) is semidet :- suspect_answer_match(S, Answer, _)),
	list__filter(P, Suspects, Matches, NoMatches).

%-----------------------------------------------------------------------------%

:- type prime_suspect(T)
	--->	prime_suspect(
				% Incorrect node.
				%
			suspect(T),

				% Evidence: the oracle said these nodes
				% were either correct or inadmissible.
				%
			list(suspect(T)),

				% Earliest inadmissible child, if there
				% have been any at all.  This child
				% is also included in the list of
				% evidence.
				%
			maybe(suspect(T)),

				% Previous prime suspects.
				%
			list(suspect(T))
		).

	% Create a prime suspect from a suspect, and maybe the previous
	% prime suspect (if there was one).
	%
:- pred create_prime_suspect(suspect(T), maybe(prime_suspect(T)),
		prime_suspect(T)).
:- mode create_prime_suspect(in, in, out) is det.

create_prime_suspect(Suspect, MaybeOldPrime, Prime) :-
	(
		MaybeOldPrime = yes(OldPrime)
	->
		OldPrime = prime_suspect(OldSuspect, _, _, Previous0),
		PreviousPrimes = [OldSuspect | Previous0]
	;
		PreviousPrimes = []
	),
	Prime = prime_suspect(Suspect, [], no, PreviousPrimes).

:- pred prime_suspect_get_edt_node(prime_suspect(T), T).
:- mode prime_suspect_get_edt_node(in, out) is det.

prime_suspect_get_edt_node(prime_suspect(Suspect, _, _, _), EDT) :-
	suspect_get_edt_node(Suspect, EDT).

	% Get all the suspects who are children of the prime suspect,
	% and who are deemed correct or inadmissible.  Maybe get
	% the earliest inadmissible child (if there was one).
	%
:- pred prime_suspect_get_evidence(prime_suspect(T), list(suspect(T)),
		maybe(suspect(T))).
:- mode prime_suspect_get_evidence(in, out, out) is det.

prime_suspect_get_evidence(prime_suspect(_, E, M, _), E, M).

	% Add to the evidence against the prime suspect a child who
	% is deemed correct or inadmissible.
	% This predicate will be more interesting when decl_truth
	% has three values.
	%
:- pred prime_suspect_add_evidence(prime_suspect(T), suspect(T), decl_truth,
		prime_suspect(T)).
:- mode prime_suspect_add_evidence(in, in, in, out) is det.

prime_suspect_add_evidence(Prime0, Suspect, yes, Prime) :-
	Prime0 = prime_suspect(S, Evidence0, M, P),
	Evidence = [Suspect | Evidence0],
	Prime = prime_suspect(S, Evidence, M, P).

prime_suspect_add_evidence(_, _, no, _) :-
	error("prime_suspect_add_evidence: not evidence").

