%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
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
:- import_module mdb__declarative_debugger, mdb__program_representation.
:- import_module list, std_util.

	% This typeclass defines how EDTs may be accessed by this module.
	% An EDT is a tree of nodes, each of which contains a question
	% about the truth of an assertion.  The children of a node may
	% not be immediately accessible if the sub-tree beneath that
	% node is represented implicitly.  In this case, the analyser
	% must request that it be made explicit before continuing.
	%
	% The first argument is intuitively a "store", which maps
	% references to the things they reference.  The second argument
	% is the type of trees themselves.  By convention, we use the
	% names S and T for type variables which are constrained by
	% mercury_edt.
	%
	% By convention, we also use the names S and T in type declarations
	% where it is *intended* that the type variables be constrained by
	% mercury_edt.
	%
	% (Compare with the similar conventions for annotated_trace/2.)
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
	mode edt_children(in, in, out) is semidet,

		% Given a subterm of a tree, find the mode of that subterm
		% and the origin of it amongst the parent, siblings or
		% children.
		%
	pred edt_dependency(S, T, arg_pos, term_path, subterm_mode,
			subterm_origin(T)),
	mode edt_dependency(in, in, in, in, out, out) is det
].

:- type subterm_mode
	--->	subterm_in
	;	subterm_out.

:- type subterm_origin(T)

			% Subterm came from an output of a child or sibling
			% call. The first argument records the child or sibling
			% edt node. The second and third arguments state which
			% part of which argument is the origin.
			%
	--->	output(T, arg_pos, term_path)

			% Subterm came from an input of the parent. The
			% arguments identify which part of which argument of
			% the clause head is the origin.
			%
	;	input(arg_pos, term_path)

			% Subterm was constructed in the body.  We record
			% the filename and line number of the primitive
			% operation (unification or inlined foreign_proc)
			% that constructed it.
			%
	;	primitive_op(string, int)

			% The origin could not be found due to missing
			% information.
			%
	;	not_found.

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

	% Return information within the analyser state that is intended for
	% debugging the declarative debugger itself.
	%
:- pred debug_analyser_state(analyser_state(T)::in,
	maybe(subterm_origin(T))::out) is det.

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
			maybe_prime	:: maybe(prime_suspect(T)),

				% Current suspects.
				%
			suspects	:: list(suspect(T)),

				% Previous prime suspects.
				%
			previous	:: list(suspect(T)),

				% This field is present only to make it easier
				% to debug the dependency tracking algorithm;
				% if bound to yes, it records the result of
				% the invocation of that algorithm on the last
				% analysis step.
				%
			debug_origin	:: maybe(subterm_origin(T))
	).

analyser_state_init(analyser(no, [], [], no)).

debug_analyser_state(Analyser, Analyser ^ debug_origin).

start_analysis(Store, Tree, Response, Analyser0, Analyser) :-
	make_suspects(Store, [Tree], Suspects, Queries),
	get_all_prime_suspects(Analyser0, OldPrimes),
	Analyser = analyser(no, Suspects, OldPrimes, no),
	Response = oracle_queries(Queries).

continue_analysis(Store, Answers, Response, Analyser0, Analyser) :-
	%
	% Check for suspicious subterms before anything else.  The oracle
	% is unlikely to answer with one of these unless it thinks it is
	% particularly significant, which is why we check these first.
	%
	% After that check for incorrect suspects.  Leave the correct
	% suspects until last, since these generally prune the search space
	% by the least amount.
	%
	Suspects = Analyser0 ^ suspects,
	(
		find_suspicious_subterm(Answers, Suspects,
			Suspect, ArgPos, TermPath)
	->
		follow_suspicious_subterm(Store, Suspect, ArgPos, TermPath,
			Response, Analyser0, Analyser)
	;
		find_incorrect_suspect(Answers, Suspects, Suspect)
	->
		make_new_prime_suspect(Store, Suspect, Response,
			Analyser0, Analyser)
	;
		remove_suspects(Store, Answers, Response,
			Analyser0, Analyser)
	).

	% Find an answer which is a suspicious subterm, and find the
	% suspect that corresponds to it, or else fail.
	%
:- pred find_suspicious_subterm(list(decl_answer), list(suspect(T)),
	suspect(T), arg_pos, term_path).
:- mode find_suspicious_subterm(in, in, out, out, out) is semidet.

find_suspicious_subterm([Answer | Answers], Suspects, Suspect, ArgPos,
		TermPath) :-
	(
		Answer = suspicious_subterm(Question, ArgPos0, TermPath0),
		find_matching_suspects(Question, Suspects, [Match | _], _)
	->
		Suspect = Match,
		ArgPos = ArgPos0,
		TermPath = TermPath0
	;
		find_suspicious_subterm(Answers, Suspects, Suspect, ArgPos,
				TermPath)
	).

:- pred follow_suspicious_subterm(S, suspect(R), arg_pos, term_path,
	analyser_response(R), analyser_state(R), analyser_state(R))
	<= mercury_edt(S, R).
:- mode follow_suspicious_subterm(in, in, in, in, out, in, out) is det.

follow_suspicious_subterm(Store, Suspect, ArgPos, TermPath, Response,
		Analyser0, Analyser) :-

	Suspect = suspect(Tree, Query),
	edt_dependency(Store, Tree, ArgPos, TermPath, SubtermMode, Origin),
	%
	% If the selected subterm has mode `in' then we infer that the node
	% is correct, otherwise we infer that it is wrong.
	%
	(
		SubtermMode = subterm_in,
		remove_suspects(Store, [truth_value(Query, yes)], Response0,
			Analyser0, Analyser1)
	;
		SubtermMode = subterm_out,
		make_new_prime_suspect(Store, Suspect, Response0,
			Analyser0, Analyser1)
	),
	Analyser = Analyser1 ^ debug_origin := yes(Origin),
	(
		Origin = output(Node, _, _),
		Response0 = oracle_queries(_)
	->
		%
		% Replace all of the queries with just the one which output
		% the subterm.  We may wind up asking the full list later,
		% including this query, but the oracle should have the
		% previous answer available.
		%
		create_suspect(Store, Node, suspect(_, NodeQuery)),
		Response = oracle_queries([NodeQuery])
	;
		Response = Response0
	).

	% Find an answer which is `no' and find the suspect that
	% corresponds to it from the given list, or else fail.
	%
:- pred find_incorrect_suspect(list(decl_answer), list(suspect(T)),
		suspect(T)).
:- mode find_incorrect_suspect(in, in, out) is semidet.

find_incorrect_suspect([Answer | Answers], Suspects, Child) :-
	(
		Answer = truth_value(Question, no),
		find_matching_suspects(Question, Suspects, [Match | _], _)
	->
		Match = Child
	;
		find_incorrect_suspect(Answers, Suspects, Child)
	).

	% Create a new prime suspect from the given suspect, which is
	% assumed to be incorrect.
	%
:- pred make_new_prime_suspect(S::in, suspect(T)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

make_new_prime_suspect(Store, Suspect, Response,
		Analyser0, Analyser) :-
	get_all_prime_suspects(Analyser0, OldPrimes),
	suspect_get_edt_node(Suspect, Tree),
	(
		edt_children(Store, Tree, Children)
	->
		create_prime_suspect(Suspect, Prime),
		MaybePrime = yes(Prime),
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
		MaybePrime = no,
		Response = require_explicit(Tree)
	),
	Analyser = analyser(MaybePrime, Suspects, OldPrimes, no).

	% Make a list of previous prime suspects, and include the current
	% one if it exists.
	%
:- pred get_all_prime_suspects(analyser_state(T), list(suspect(T))).
:- mode get_all_prime_suspects(in, out) is det.

get_all_prime_suspects(Analyser, OldPrimes) :-
	(
		Analyser ^ maybe_prime = yes(Prime)
	->
		prime_suspect_get_suspect(Prime, Suspect),
		OldPrimes = [Suspect | Analyser ^ previous]
	;
		OldPrimes = Analyser ^ previous
	).

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
:- pred remove_suspects(S::in, list(decl_answer)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

remove_suspects(Store, [], Response, Analyser, Analyser) :-
	(
		Analyser ^ suspects = []
	->
		(
			Analyser ^ maybe_prime = yes(Prime)
		->
			prime_suspect_get_edt_node(Prime, Tree),
			edt_root_e_bug(Store, Tree, EBug),
			Response = bug_found(e_bug(EBug))
		;
			Response = no_suspects
		)
	;
		list__map(suspect_get_question, Analyser ^ suspects, Queries),
		Response = oracle_queries(Queries)
	).

remove_suspects(Store, [Answer | Answers], Response, Analyser0, Analyser) :-
	(
		Answer = truth_value(_, yes)
	->
		find_matching_suspects(get_decl_question(Answer),
			Analyser0 ^ suspects, _, Suspects),
		Analyser1 = Analyser0 ^ suspects := Suspects,
		remove_suspects(Store, Answers, Response,
			Analyser1, Analyser)
	;
		error("remove_suspects: unexpected incorrect node")
	).

:- func get_decl_question(decl_answer) = decl_question.

get_decl_question(truth_value(Q, _)) = Q.
get_decl_question(suspicious_subterm(Q, _, _)) = Q.

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

:- pred find_matching_suspects(decl_question, list(suspect(T)),
		list(suspect(T)), list(suspect(T))).
:- mode find_matching_suspects(in, in, out, out) is det.

find_matching_suspects(Question, Suspects, Matches, NoMatches) :-
	P = (pred(S::in) is semidet :-
		suspect_get_question(S, Question)
	),
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
			maybe(suspect(T))
		).

	% Create a prime suspect from a suspect.
	%
:- pred create_prime_suspect(suspect(T), prime_suspect(T)).
:- mode create_prime_suspect(in, out) is det.

create_prime_suspect(Suspect, Prime) :-
	Prime = prime_suspect(Suspect, [], no).

:- pred prime_suspect_get_suspect(prime_suspect(T), suspect(T)).
:- mode prime_suspect_get_suspect(in, out) is det.

prime_suspect_get_suspect(prime_suspect(Suspect, _, _), Suspect).

:- pred prime_suspect_get_edt_node(prime_suspect(T), T).
:- mode prime_suspect_get_edt_node(in, out) is det.

prime_suspect_get_edt_node(prime_suspect(Suspect, _, _), EDT) :-
	suspect_get_edt_node(Suspect, EDT).

	% Get all the suspects who are children of the prime suspect,
	% and who are deemed correct or inadmissible.  Maybe get
	% the earliest inadmissible child (if there was one).
	%
:- pred prime_suspect_get_evidence(prime_suspect(T), list(suspect(T)),
		maybe(suspect(T))).
:- mode prime_suspect_get_evidence(in, out, out) is det.

prime_suspect_get_evidence(prime_suspect(_, E, M), E, M).

	% Add to the evidence against the prime suspect a child who
	% is deemed correct or inadmissible.
	% This predicate will be more interesting when decl_truth
	% has three values.
	%
:- pred prime_suspect_add_evidence(prime_suspect(T), suspect(T), decl_truth,
		prime_suspect(T)).
:- mode prime_suspect_add_evidence(in, in, in, out) is det.

prime_suspect_add_evidence(Prime0, Suspect, yes, Prime) :-
	Prime0 = prime_suspect(S, Evidence0, M),
	Evidence = [Suspect | Evidence0],
	Prime = prime_suspect(S, Evidence, M).

prime_suspect_add_evidence(_, _, no, _) :-
	error("prime_suspect_add_evidence: not evidence").

