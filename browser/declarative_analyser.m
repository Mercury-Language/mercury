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
	pred edt_root_question(S, T, decl_question(T)),
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
	;	oracle_queries(list(decl_question(T)))

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
:- pred continue_analysis(S, list(decl_answer(T)), analyser_response(T),
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

	% The analyser state records all of the information that needs
	% to be remembered across multiple invocations of the analyser.
	% This includes information about the current set of suspects
	% in the EDT, that is, the smallest set of EDT nodes which,
	% together with the prime suspect, is known to contain at least
	% one bug.
	%
	% Note that sometimes we represent a suspect by the question
	% generated from it.  We can extract the actual suspect from
	% this question.  We do this in order to avoid recreating the
	% question repreatedly, for each call to the oracle.
	%
:- type analyser_state(T)
	--->	analyser(
				% Current incorrect node (initially `no').
				% This is the most recent node that the
				% oracle has said is incorrect.
				%
			maybe_prime		:: maybe(prime_suspect(T)),

				% Previous prime suspects.
				%
			previous		:: list(T),

				% Nodes in the EDT which are the roots of
				% subtrees which contain suspects.  Every
				% suspect in the EDT is either in one of
				% these lists, or is the descendent of a
				% node in one of these lists.
				%
				% Nodes whose descendents are suspects
				% which are represented implicitly in the
				% EDT are in the second list.
				%
			suspect_roots		:: list(decl_question(T)),
			suspect_parents		:: list(T),

				% Suspects which, for whatever reason, are
				% deemed to be particularly suspicious.
				% For example, the node which is the origin
				% of a suspicious subterm.
				%
			priority_suspects	:: list(decl_question(T)),

				% This field is present only to make it easier
				% to debug the dependency tracking algorithm;
				% if bound to yes, it records the result of
				% the invocation of that algorithm on the last
				% analysis step.
				%
			debug_origin		:: maybe(subterm_origin(T))
	).

analyser_state_init(analyser(no, [], [], [], [], no)).

debug_analyser_state(Analyser, Analyser ^ debug_origin).

start_analysis(Store, Tree, Response, Analyser0, Analyser) :-
	get_all_prime_suspects(Analyser0, OldPrimes),
	edt_root_question(Store, Tree, Question),
	Analyser = analyser(no, OldPrimes, [Question], [], [], no),
	decide_analyser_response(Store, Analyser, Response).

continue_analysis(Store, Answers, Response, Analyser0, Analyser) :-
	list__foldl(process_answer(Store), Answers, Analyser0, Analyser),
	decide_analyser_response(Store, Analyser, Response).

:- pred process_answer(S::in, decl_answer(T)::in, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

process_answer(Store, truth_value(Suspect, yes), Analyser0, Analyser) :-
	assert_suspect_is_correct(Store, Suspect, Analyser0, Analyser).

process_answer(Store, truth_value(Suspect, no), Analyser0, Analyser) :-
	assert_suspect_is_wrong(Store, Suspect, Analyser0, Analyser).

process_answer(Store, Answer, Analyser0, Analyser) :-
	Answer = suspicious_subterm(Suspect, ArgPos, TermPath),
	edt_dependency(Store, Suspect, ArgPos, TermPath, SubtermMode, Origin),
	%
	% If the selected subterm has mode `in' then we infer that the node
	% is correct, otherwise we infer that it is wrong.
	%
	(
		SubtermMode = subterm_in,
		assert_suspect_is_correct(Store, Suspect, Analyser0, Analyser1)
	;
		SubtermMode = subterm_out,
		assert_suspect_is_wrong(Store, Suspect, Analyser0, Analyser1)
	),
	Analyser2 = Analyser1 ^ debug_origin := yes(Origin),
	%
	% If the origin of the subterm was an output of one of the children,
	% we flag that child as a priority suspect.  At the moment, we only
	% follow the suspicious subterm down one level, and we first make
	% sure that the origin is one of the existing suspects.  In future,
	% we intend to implement more sophisticated search strategies which
	% make more use of the term dependencies.
	%
	% If the origin of the subterm was an input of the parent, we can't
	% do anything useful yet.  This is because, since we step down one
	% level at a time, the parent node is the prime suspect and is thus
	% known to be wrong.  Therefore we can't infer anything useful from
	% a suspicious input.
	%
	(
		Origin = output(OriginSuspect, _, _),
		some [S] (
			list__member(S, Analyser2 ^ suspect_roots),
			OriginSuspect = get_decl_question_node(S)
		)
	->
		edt_root_question(Store, OriginSuspect, OriginQuestion),
		Analyser = Analyser2 ^ priority_suspects := [OriginQuestion]
	;
		Analyser = Analyser2
	).

%-----------------------------------------------------------------------------%

:- pred assert_suspect_is_correct(S::in, T::in, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

assert_suspect_is_correct(_Store, Suspect, Analyser0, Analyser) :-
	Suspects0 = Analyser0 ^ suspect_roots,
	delete_suspect(Suspects0, Suspect, Suspects),
	Analyser1 = Analyser0 ^ suspect_roots := Suspects,
	PrioritySuspects0 = Analyser1 ^ priority_suspects,
	delete_suspect(PrioritySuspects0, Suspect, PrioritySuspects),
	Analyser = Analyser1 ^ priority_suspects := PrioritySuspects.

:- pred assert_suspect_is_wrong(S::in, T::in, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

assert_suspect_is_wrong(Store, Suspect, Analyser0, Analyser) :-
	get_all_prime_suspects(Analyser0, OldPrimes),
	(
		edt_children(Store, Suspect, Children)
	->
		create_prime_suspect(Suspect, Prime),
		MaybePrime = yes(Prime),
		list__map(edt_root_question(Store), Children, SuspectRoots),
		SuspectParents = []
	;
			% The real suspects cannot be found, so we are
			% going to need to request a subtree.  In the
			% meantime, we leave the prime suspect field empty.
			% The root of the requested subtree will become the
			% prime suspect when the analyser is next called.
			%
		MaybePrime = no,
		SuspectRoots = [],
		SuspectParents = [Suspect]
	),
	Analyser = analyser(MaybePrime, OldPrimes, SuspectRoots,
			SuspectParents, [], no).

:- pred decide_analyser_response(S::in, analyser_state(T)::in,
	analyser_response(T)::out) is det <= mercury_edt(S, T).

decide_analyser_response(Store, Analyser, Response) :-
	%
	% If any subtrees need to be made explicit, then request this
	% for the first one.
	%
	% Otherwise, check whether there are any suspects at all.  If not,
	% we may have found a bug.
	%
	% Otherwise, ask the oracle about the priority suspects and the
	% ordinary suspects, in that order.
	%
	(
		Analyser ^ suspect_parents = [RequiredTree | _]
	->
		Response = require_explicit(RequiredTree)
	;
		Analyser ^ suspect_roots = []
	->
		%
		% If there is a prime suspect, it is the bug.  Otherwise,
		% we throw up our hands and end the analysis.
		%
		(
			Analyser ^ maybe_prime = yes(Prime)
		->
			prime_suspect_get_e_bug(Store, Prime, EBug),
			Response = bug_found(e_bug(EBug))
		;
			Response = no_suspects
		)
	;
		list__append(Analyser ^ priority_suspects,
				Analyser ^ suspect_roots, Questions),
		Response = oracle_queries(Questions)
	).

	% Make a list of previous prime suspects, and include the current
	% one if it exists.
	%
:- pred get_all_prime_suspects(analyser_state(T), list(T)).
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

:- pred delete_suspect(list(decl_question(T)), T, list(decl_question(T))).
:- mode delete_suspect(in, in, out) is det.

delete_suspect(Suspects0, Target, Suspects) :-
	Filter = (pred(S::in) is semidet :-
			Target \= get_decl_question_node(S)
		),
	list__filter(Filter, Suspects0, Suspects).

%-----------------------------------------------------------------------------%

:- type prime_suspect(T)
	--->	prime_suspect(
				% Incorrect node.
				%
			T,

				% Evidence: the oracle said these nodes
				% were either correct or inadmissible.
				%
			list(T),

				% Earliest inadmissible child, if there
				% have been any at all.  This child
				% is also included in the list of
				% evidence.
				%
			maybe(T)
		).

	% Create a prime suspect from a suspect.
	%
:- pred create_prime_suspect(T, prime_suspect(T)).
:- mode create_prime_suspect(in, out) is det.

create_prime_suspect(Suspect, Prime) :-
	Prime = prime_suspect(Suspect, [], no).

:- pred prime_suspect_get_suspect(prime_suspect(T), T).
:- mode prime_suspect_get_suspect(in, out) is det.

prime_suspect_get_suspect(prime_suspect(Suspect, _, _), Suspect).

:- pred prime_suspect_get_e_bug(S, prime_suspect(T), decl_e_bug)
	<= mercury_edt(S, T).
:- mode prime_suspect_get_e_bug(in, in, out) is det.

prime_suspect_get_e_bug(Store, Prime, EBug) :-
	prime_suspect_get_suspect(Prime, Suspect),
	edt_root_e_bug(Store, Suspect, EBug).

	% Get all the suspects who are children of the prime suspect,
	% and who are deemed correct or inadmissible.  Maybe get
	% the earliest inadmissible child (if there was one).
	%
:- pred prime_suspect_get_evidence(prime_suspect(T), list(T), maybe(T)).
:- mode prime_suspect_get_evidence(in, out, out) is det.

prime_suspect_get_evidence(prime_suspect(_, E, M), E, M).

	% Add to the evidence against the prime suspect a child who
	% is deemed correct or inadmissible.
	% This predicate will be more interesting when decl_truth
	% has three values.
	%
:- pred prime_suspect_add_evidence(prime_suspect(T), T, decl_truth,
		prime_suspect(T)).
:- mode prime_suspect_add_evidence(in, in, in, out) is det.

prime_suspect_add_evidence(Prime0, Suspect, yes, Prime) :-
	Prime0 = prime_suspect(S, Evidence0, M),
	Evidence = [Suspect | Evidence0],
	Prime = prime_suspect(S, Evidence, M).

prime_suspect_add_evidence(_, _, no, _) :-
	error("prime_suspect_add_evidence: not evidence").

