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

:- module declarative_analyser.
:- interface.
:- import_module list.
:- import_module declarative_debugger.

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
	pred edt_root(S, T, decl_question),
	mode edt_root(in, in, out) is det,

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
	;	bug_found(decl_bug(T))

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
				%
			maybe(T),	

				% Current suspects.
				%
			list(suspect(T))
	).

	% A suspect is a suspect tree along with its corresponding
	% root node.
	%
:- type suspect(T) == pair(T, decl_question).

analyser_state_init(analyser(no, [])).

start_analysis(Store, Tree, Response, _, Analyser) :-
	edt_root(Store, Tree, Root),
	Response = oracle_queries([Root]),
	Analyser = analyser(no, [Tree - Root]).

continue_analysis(Store, Answers, Response, Analyser0, Analyser) :-
	(
		find_incorrect_suspect(Answers, Analyser0, Node)
	->
		make_new_suspects(Store, Node, Response, Analyser)
	;
		remove_suspects(Answers, Response, Analyser0, Analyser)
	).


	% Find an answer which is `no' and find the suspect that
	% corresponds to it, or else fail.
	%
:- pred find_incorrect_suspect(list(decl_answer), analyser_state(T), T).
:- mode find_incorrect_suspect(in, in, out) is semidet.

find_incorrect_suspect([Answer | Answers], Analyser, Child) :-
	Analyser = analyser(_, Suspects),
	(
		Answer = Node - no,
		find_matching_suspects(Node, Suspects, [Match | _], _)
	->
		Match = Child - _
	;
		find_incorrect_suspect(Answers, Analyser, Child)
	).

	% Create a new suspect list from the given tree, which is
	% assumed to have an incorrect root.
	%
:- pred make_new_suspects(S, T, analyser_response(T), analyser_state(T))
		<= mercury_edt(S, T).
:- mode make_new_suspects(in, in, out, out) is det.

make_new_suspects(Store, Tree, Response, Analyser) :-
	(
		edt_children(Store, Tree, Children)
	->
		make_suspects(Store, Children, Suspects, Queries),
		Analyser = analyser(yes(Tree), Suspects),
		(
			Queries = []
		->
			Response = bug_found(e_bug(Tree))
		;
			Response = oracle_queries(Queries)
		)
	;
		Response = require_explicit(Tree),
		Analyser = analyser(yes(Tree), [])
	).

:- pred make_suspects(S, list(T), list(suspect(T)), list(decl_question))
		<= mercury_edt(S, T).
:- mode make_suspects(in, in, out, out) is det.

make_suspects(_, [], [], []).
make_suspects(Store, [Tree | Trees], [Tree - Root | Ss], [Root | Qs]) :-
	edt_root(Store, Tree, Root),
	make_suspects(Store, Trees, Ss, Qs).

	% Go through the answers (none of which should be `no') and
	% remove the corresponding children from the suspect list.
	%
:- pred remove_suspects(list(decl_answer), analyser_response(T),
		analyser_state(T), analyser_state(T)).
:- mode remove_suspects(in, out, in, out) is det.

remove_suspects([], Response, Analyser, Analyser) :-
	Analyser = analyser(MaybeTree, Suspects),
	(
		Suspects = []
	->
		(
			MaybeTree = yes(Tree)
		->
			Response = bug_found(e_bug(Tree))
		;
			Response = no_suspects
		)
	;
		list__map(snd, Suspects, Queries),
		Response = oracle_queries(Queries)
	).

remove_suspects([Node - yes | Answers], Response, Analyser0, Analyser) :-
	Analyser0 = analyser(MaybeTree, Suspects0),
	find_matching_suspects(Node, Suspects0, _, Suspects),
	Analyser1 = analyser(MaybeTree, Suspects),
	remove_suspects(Answers, Response, Analyser1, Analyser).

remove_suspects([_ - no | _], _, _, _) :-
	error("remove_suspects: unexpected incorrect node").

:- pred find_matching_suspects(decl_question, list(suspect(T)),
		list(suspect(T)), list(suspect(T))).
:- mode find_matching_suspects(in, in, out, out) is det.

find_matching_suspects(Node, Suspects, Matches, NoMatches) :-
	P = (pred(A::in) is semidet :- A = _ - Node),
	list__filter(P, Suspects, Matches, NoMatches).

