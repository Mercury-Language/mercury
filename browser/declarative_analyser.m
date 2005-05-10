%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_analyser.m
% Authors: Mark Brown, Ian MacLarty
%
% This module implements some analysis algorithms that search for bugs in
% Evaluation Dependency Trees (EDTs).  The search algorithms use information
% provided by the search_space data type which acts as a layer on top of the
% EDT, storing information relevant to the bug search.  Throughout this module
% the type variables T and S refer to the types of nodes in the EDT and the
% store of EDT nodes respectively.
%

:- module mdb.declarative_analyser.

:- interface.

:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_edt.
:- import_module mdb.declarative_oracle.
:- import_module mdb.io_action.

:- import_module std_util, io.

:- type analyser_response(T)

			% There are no suspects left, and no incorrect
			% nodes have been found.
	--->	no_suspects

			% A suspect who is guilty, along with the evidence
			% against the suspect.
	;	bug_found(decl_bug, decl_evidence(T))

			% The analyser desires an answer to the question.
	;	oracle_question(decl_question(T))

			% The analyser requires the given implicit sub-tree
			% to be made explicit.
	;	require_explicit_subtree(T)

			% The analyser requires an explicit tree above the 
			% root of an existing explicit tree.
	;	require_explicit_supertree(T)

			% The analyser would like the oracle to re-ask the user
			% this question and then for analysis to continue.
	;	revise(decl_question(T)).

:- func reason_to_string(reason_for_question) = string.

:- type analyser_state(T).

:- type search_mode.

:- func divide_and_query_search_mode = search_mode.

:- func top_down_search_mode = search_mode.

:- pred analyser_state_init(io_action_map::in, analyser_state(T)::out) is det.

	% Resets the state of the analyser except for the io_action_map.
	%
:- pred reset_analyser(analyser_state(T)::in, analyser_state(T)::out) is det.

	% Make the given search mode the fallback search mode
	% and the current search mode for the analyser.
	%
:- pred set_fallback_search_mode(search_mode::in, 
	analyser_state(T)::in, analyser_state(T)::out) is det.

:- pred analyser_state_replace_io_map(io_action_map::in,
	analyser_state(T)::in, analyser_state(T)::out) is det.

:- type analysis_type(T)
			% Use the given tree to do analysis.  The tree will be
			% a new explicitly generated portion of the annotated
			% trace.  start_or_resume_analysis should be called
			% with this type of analysis when a new declarative
			% debugging session has been started or a requested
			% subtree or supertree has been generated.
	--->	new_tree(T)
			% Continue the previous analysis.  This will happen
			% when the user suspends a declarative debugging
			% session with a `pd' or `abort' command and now wants
			% to continue the suspended session.
	;	resume_previous.

	% Perform analysis on the given EDT, which may be a new tree
	% to diagnose, or a sub-tree that was required to be made
	% explicit.
	%
:- pred start_or_resume_analysis(S::in, oracle_state::in, analysis_type(T)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

	% Continue analysis after the oracle has responded with an
	% answer.
	%
:- pred continue_analysis(S::in, oracle_state::in, decl_answer(T)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

	% Display information about the current question and the state
	% of the search to the supplied output stream.
	%
:- pred show_info(S::in, io.output_stream::in, analyser_state(T)::in,
	analyser_response(T)::out, io::di, io::uo) is det <= mercury_edt(S, T).

	% Revise the current analysis.  This is done when a bug determined
	% by the analyser has been overruled by the oracle.
	%
:- pred revise_analysis(S::in, analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

	% Return information within the analyser state that is intended for
	% debugging the declarative debugger itself.
	%
:- pred debug_analyser_state(analyser_state(T)::in,
	maybe(subterm_origin(T))::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_edt.
:- import_module mdb.declarative_execution.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.

:- import_module bool, exception, counter, array, list, float.
:- import_module math, string, map, int. 

	% Describes what search strategy is being used by the analyser and the
	% state of the search.
	%
:- type search_mode
			% Look for the first unknown suspect in a top-down 
			% fashion, starting at the root.  If no unknown
			% suspects are found then choose a skipped suspect
			% to requery.
	--->	top_down

			%
			% Follow the subterm all the way to where it's bound or
			% until it can't be followed any further (for example
			% when there is a call to a module with no tracing),
			% and ask a question about the nearest unknown suspect
			% on the subterm dependency chain.  Then proceed to do
			% a binary search between this node and the root of the
			% search space (the binary search will only come into
			% effect if the oracle asserts the suspect is correct
			% or inadmissible).  			
			%
	;	follow_subterm_end(
				%
				% The following 3 args give the position the
				% sub-term tracking algorithm has got up to if
				% it needs to stop to wait for an explicit
				% sub/super-tree to be generated.
				%
			suspect_id, 
			arg_pos, 
			term_path, 

				% The last suspect on the dependency chain
				% whose status was unknown.  Initially this is
				% no, but as the sub-term is tracked to where
				% it was initially bound (which could be above
				% or below the node where it was marked
				% incorrect), the most recent node through
				% which the sub-term was tracked that has a
				% status of `unknown' is stored in this field.
				% This is then used as the next question if the
				% node that bound the sub-term is trusted or in
				% an excluded part of the search tree.
			maybe(suspect_id)
		)

			%
			% Perform a binary search on a path in the search space
			% between a suspect and an ancestor of the suspect.
			% The path is represented as an array (the 1st
			% argument) with the deeper suspect at the end of the
			% array and its ancestor at the beginning.
			% The range field gives the inclusive subrange of the
			% array to search.  last_tested is the index into the
			% array of the last suspect about which a question was
			% asked.   
			%
	;	binary(
			suspects	:: array(suspect_id),
			range		:: pair(int, int),
			last_tested	:: int
		)
	;
			% 
			% An adapted version of the divide and query approach
			% proposed by Shapiro.  We weight each node with the
			% number of events executed by descendent children
			% plus the internal body events of the call.  This is
			% mainly because it's hard to work out how many
			% descendents are in unmaterialized portions of the EDT
			% without using memory proportional to the
			% unmaterialized portion.
			%
		divide_and_query.

divide_and_query_search_mode = divide_and_query.

top_down_search_mode = top_down.

	% Each search algorithm should respond with either a question
	% or a request for an explicit subtree to be generated for a suspect 
	% which is the root of an implicit subtree.
	% 
:- type search_response
	--->	question(suspect_id, reason_for_question)
	;	require_explicit_subtree(suspect_id)
	;	require_explicit_supertree
	;	no_suspects
	;	found_bug(suspect_id, list(suspect_id), list(suspect_id)).

	% The reason the declarative debugger asked a question.
	%
:- type reason_for_question
	--->	start 			% The first question.
	;	top_down
	;	binding_node(
			binding_prim_op		:: primitive_op_type,
			binding_filename	:: string,	
			binding_line_no		:: int, 	
				% The path of the subterm in the binding node,
				% if it appears in the binding node's atom.
			maybe_atom_path		:: maybe(term_path),
			binding_proc		:: proc_label,
			binding_node_eliminated	:: bool
		)
	;	subterm_no_proc_rep	% No proc rep when tracking subterm.
	;	binding_node_eliminated
	;	binary(
			binary_reason_bottom	:: int, 
			binary_reason_top	:: int, 
			binary_reason_split	:: int
		)
	;	divide_and_query(
			old_weight		:: int,
			choosen_subtree_weight 	:: int
		)
	;	skipped
	;	revise.

	% The analyser state records all of the information that needs
	% to be remembered across multiple invocations of the analyser.
	%
:- type analyser_state(T)
	--->	analyser(
				
				% Information about the EDT nodes relevent to 
				% the bug search.
			search_space		:: search_space(T),
			
				% This is set to yes when an explicit tree
				% needs to be generated.  
				% The maybe argument says what type of explicit
				% tree needs to be generated.
			require_explicit :: maybe(explicit_tree_type),

				% The method currently being employed to search
				% the search space for questions for the 
				% oracle.
			search_mode		:: search_mode,
				
				% The search mode to use by default. 
				% Only non-parametrized search modes should
				% be used as the fallback search mode.
			fallback_search_mode	:: search_mode,

				% Everytime a search finds a suspect to
				% ask the oracle about it is put in this field
				% before asking the oracle, so the analyser
				% knows how to modify the search space when 
				% it gets an answer.
			last_search_question	:: maybe(suspect_and_reason),

				% This field allows us to map I/O action
				% numbers to the actions themselves.
			io_action_map		:: io_action_map,
			
				% This field is present only to make it easier
				% to debug the dependency tracking algorithm;
				% if bound to yes, it records the result of
				% the invocation of that algorithm on the last
				% analysis step.
			debug_origin		:: maybe(subterm_origin(T))
	).

:- type suspect_and_reason
	--->	suspect_and_reason(suspect_id, reason_for_question).

:- type explicit_tree_type

			% Generate an explicit subtree for the implicit root
			% referenced by the suspect_id.
	--->	explicit_subtree(suspect_id)
	
			% Generate a new explicit tree above the current 
			% explicit tree.
	;	explicit_supertree.

analyser_state_init(IoActionMap, Analyser) :-
	Analyser = analyser(empty_search_space, no, top_down, 
		top_down, no, IoActionMap, no).

reset_analyser(!Analyser) :-
	FallBack = !.Analyser ^ fallback_search_mode,
	!:Analyser = analyser(empty_search_space, no, FallBack, 
		FallBack, no, !.Analyser ^ io_action_map, no).

set_fallback_search_mode(FallBackSearchMode, !Analyser) :-
	!:Analyser = !.Analyser ^ fallback_search_mode := FallBackSearchMode,
	!:Analyser = !.Analyser ^ search_mode := FallBackSearchMode.

analyser_state_replace_io_map(IoActionMap, !Analyser) :-
	!:Analyser = !.Analyser ^ io_action_map := IoActionMap.

debug_analyser_state(Analyser, Analyser ^ debug_origin).

start_or_resume_analysis(Store, Oracle, AnalysisType, Response, !Analyser) :-
	(
		AnalysisType = new_tree(Node),
		MaybeRequireExplicit = !.Analyser ^ require_explicit,
		(
			MaybeRequireExplicit = yes(TreeType),
			SearchSpace0 = !.Analyser ^ search_space,
			(
				TreeType = explicit_supertree,
				incorporate_explicit_supertree(
					!.Analyser ^ io_action_map, Store, 
					Oracle, Node, SearchSpace0,
					SearchSpace)
			;
				TreeType = explicit_subtree(SuspectId),
				incorporate_explicit_subtree(SuspectId, Node, 
					SearchSpace0, SearchSpace)
			),
			!:Analyser = !.Analyser ^ search_space := SearchSpace,
			!:Analyser = !.Analyser ^ require_explicit := no,
			decide_analyser_response(Store, Oracle, Response, 
				!Analyser)
		;
			MaybeRequireExplicit = no,
			%
			% An explicit subtree was not requested, so this is the 
			% start of a new declarative debugging session.
			%
			reset_analyser(!Analyser),
			initialise_search_space(Store, Node, SearchSpace),
			!:Analyser = !.Analyser ^ search_space := SearchSpace,
			topmost_det(SearchSpace, TopMostId),
			!:Analyser = !.Analyser ^ last_search_question := 
				yes(suspect_and_reason(TopMostId, start)),
			edt_question(!.Analyser ^ io_action_map, Store, Node, 
				Question),
			Response = revise(Question)
		)
	;
		AnalysisType = resume_previous,
		decide_analyser_response(Store, Oracle, Response, !Analyser)
	).

continue_analysis(Store, Oracle, Answer, Response, !Analyser) :-
	(
		!.Analyser ^ last_search_question = yes(
			suspect_and_reason(SuspectId, _)),
		process_answer(Store, Answer, SuspectId, !Analyser)
	;
		!.Analyser ^ last_search_question = no,
		throw(internal_error("continue_analysis", 
			"received answer to unasked question"))
	),
	!:Analyser = !.Analyser ^ last_search_question := no,
	decide_analyser_response(Store, Oracle, Response, !Analyser).

:- pred process_answer(S::in, decl_answer(T)::in, suspect_id::in, 
	analyser_state(T)::in, analyser_state(T)::out) 
	is det <= mercury_edt(S, T).

process_answer(_, skip(_), SuspectId, !Analyser) :-
	skip_suspect(SuspectId, !.Analyser ^ search_space, SearchSpace),
	!:Analyser = !.Analyser ^ search_space := SearchSpace.

process_answer(Store, ignore(_), SuspectId, !Analyser) :-
	ignore_suspect(Store, SuspectId, !.Analyser ^ search_space, 
		SearchSpace),
	!:Analyser = !.Analyser ^ search_space := SearchSpace.

process_answer(_, truth_value(_, correct), SuspectId, !Analyser) :-
	assert_suspect_is_correct(SuspectId, !.Analyser ^ search_space, 
		SearchSpace),
	!:Analyser = !.Analyser ^ search_space := SearchSpace.

process_answer(_, truth_value(_, inadmissible), SuspectId, !Analyser) :-
	assert_suspect_is_inadmissible(SuspectId, !.Analyser ^ search_space, 
		SearchSpace),
	!:Analyser = !.Analyser ^ search_space := SearchSpace.

process_answer(_, truth_value(_, erroneous), SuspectId, !Analyser) :-
	assert_suspect_is_erroneous(SuspectId, !.Analyser ^ search_space, 
		SearchSpace),
	!:Analyser = !.Analyser ^ search_space := SearchSpace.

	% process_answer shouldn't be called with a show_info oracle response.
	%
process_answer(_, show_info(_), _, _, _) :-
	throw(internal_error("process_answer", "called with show_info/1")).


process_answer(Store, suspicious_subterm(Node, ArgPos, TermPath), SuspectId, 
		!Analyser) :-
	% 
	% XXX The following 2 lines just done so that debugging info can be
	% printed for tests run when declarative_analyser.m not compiled with
	% tracing (so can't use dd_dd command in mdb).  Should be removed when
	% edt_dependency becomes stable enough.
	%
	edt_dependency(Store, Node, ArgPos, TermPath, _, DebugOrigin),
	!:Analyser = !.Analyser ^ debug_origin := yes(DebugOrigin),

	edt_subterm_mode(Store, Node, ArgPos, TermPath, Mode),
	(
		Mode = subterm_in,
		assert_suspect_is_inadmissible(SuspectId, 
			!.Analyser ^ search_space, SearchSpace)
	;
		Mode = subterm_out,
		assert_suspect_is_erroneous(SuspectId, 
			!.Analyser ^ search_space, SearchSpace)
	),
	!:Analyser = !.Analyser ^ search_space := SearchSpace,
	!:Analyser = !.Analyser ^ search_mode := follow_subterm_end(SuspectId,
		ArgPos, TermPath, no).

revise_analysis(Store, Response, !Analyser) :-
	SearchSpace = !.Analyser ^ search_space,
	(
		root(SearchSpace, RootId)
	->
		Node = get_edt_node(!.Analyser ^ search_space, RootId),
		edt_question(!.Analyser ^ io_action_map, Store, Node,
			Question),
		Response = revise(Question),
		revise_root(Store, SearchSpace, SearchSpace1),
		!:Analyser = !.Analyser ^ search_space := SearchSpace1,
		!:Analyser = !.Analyser ^ last_search_question := 
			yes(suspect_and_reason(RootId, revise)),
		!:Analyser = !.Analyser ^ search_mode := 
			!.Analyser ^ fallback_search_mode
	;
		% There must be a root, since a bug was found (and is now
		% being revised).
		throw(internal_error("revise_analysis", "no root"))
	).

:- pred decide_analyser_response(S::in, oracle_state::in, 
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

decide_analyser_response(Store, Oracle, Response, !Analyser) :-
	maybe_check_search_space_consistency(Store, !.Analyser ^ search_space,
		"Start of decide_analyser_response"),
	some [!SearchSpace] (
		!:SearchSpace = !.Analyser ^ search_space,
		search(!.Analyser ^ io_action_map, Store, Oracle, !SearchSpace, 
			!.Analyser ^ search_mode,
			!.Analyser ^ fallback_search_mode, NewMode, 
			SearchResponse),
		!:Analyser = !.Analyser ^ search_space := !.SearchSpace,
		!:Analyser = !.Analyser ^ search_mode := NewMode,
		handle_search_response(Store, SearchResponse, 
			!Analyser, Response)
	),
	maybe_check_search_space_consistency(Store, !.Analyser ^ search_space,
		"End of decide_analyser_response").

:- pred handle_search_response(S::in, search_response::in, 
	analyser_state(T)::in, analyser_state(T)::out, 
	analyser_response(T)::out) is det <= mercury_edt(S, T).

handle_search_response(Store, question(SuspectId, Reason), !Analyser,
		Response) :-
	SearchSpace = !.Analyser ^ search_space,
	Node = get_edt_node(SearchSpace, SuspectId),
	edt_question(!.Analyser ^ io_action_map, Store, Node,
		OracleQuestion),
	(
		(
			suspect_unknown(SearchSpace, SuspectId) 
		; 
			suspect_skipped(SearchSpace, SuspectId)
		)
	->
		Response = oracle_question(OracleQuestion)
	;
		suspect_ignored(SearchSpace, SuspectId)
	->
		% Searches should not respond with questions about suspects we
		% already know to be trusted.
		throw(internal_error("handle_search_response",
			"search responded with query about ignored suspect"))
	;
		% We already known something about this suspect, but the search
		% wants the oracle to be requeried.  This may happen if the
		% search thinks the user might have answered the question
		% incorrectly before.
		Response = revise(OracleQuestion)
	),
	!:Analyser = !.Analyser ^ last_search_question := 
		yes(suspect_and_reason(SuspectId, Reason)).

handle_search_response(_, require_explicit_subtree(SuspectId), !Analyser, 
		Response) :-
	!:Analyser = !.Analyser ^ require_explicit := yes(explicit_subtree(
		SuspectId)),
	Node = get_edt_node(!.Analyser ^ search_space, SuspectId),
	Response = require_explicit_subtree(Node).

handle_search_response(_, require_explicit_supertree, !Analyser, Response) :-
	!:Analyser = !.Analyser ^ require_explicit := yes(explicit_supertree),
	SearchSpace = !.Analyser ^ search_space,
	topmost_det(SearchSpace, TopMostId),
	TopMost = get_edt_node(SearchSpace, TopMostId),
	Response = require_explicit_supertree(TopMost).

handle_search_response(_, no_suspects, !Analyser, no_suspects).

handle_search_response(Store, found_bug(BugId, CorrectDescendents, 
		InadmissibleChildren), !Analyser, Response) :-
	bug_response(Store, !.Analyser ^ io_action_map, 
		!.Analyser ^ search_space, BugId, [BugId | CorrectDescendents],
		InadmissibleChildren, Response).

	% bug_response(Store, IoActionMap, SearchSpace, BugId, Evidence, 
	%	InadmissibleChildren, Response)
	% Create a bug analyser-response using the given Evidence.  If 
	% InadmissibleChildren isn't empty then an i_bug will be created,
	% otherwise an e_bug will be created.
	%
:- pred bug_response(S::in, io_action_map::in, search_space(T)::in, 
	suspect_id::in, list(suspect_id)::in, list(suspect_id)::in,
	analyser_response(T)::out) is det <= mercury_edt(S, T).

bug_response(Store, IoActionMap, SearchSpace, BugId, Evidence, 
		InadmissibleChildren, Response) :-
	BugNode = get_edt_node(SearchSpace, BugId),
	(
		InadmissibleChildren = [InadmissibleChild | _],
		edt_get_i_bug(Store, BugNode, 
			get_edt_node(SearchSpace, InadmissibleChild), IBug),
		Bug = i_bug(IBug)
	;
		InadmissibleChildren = [],
		edt_get_e_bug(IoActionMap, Store, BugNode, EBug),
		Bug = e_bug(EBug)
	),
	EDTNodes = list.map(get_edt_node(SearchSpace), Evidence),
	list.map(edt_question(IoActionMap, Store), EDTNodes,
		EvidenceAsQuestions),
	Response = bug_found(Bug, EvidenceAsQuestions).

%-----------------------------------------------------------------------------%

	% Search the search space for a question for the oracle.  The search
	% should respond with a question about a suspect, or a request for an
	% explicit subree to be generated.  A new search mode is returned so
	% that the search algorithm being used can remember its current state
	% next time round.
	% 
:- pred search(io_action_map::in, S::in, oracle_state::in, 
	search_space(T)::in, search_space(T)::out, 
	search_mode::in, search_mode::in, 
	search_mode::out, search_response::out) is det <= mercury_edt(S, T).

search(IoActionMap, Store, Oracle, !SearchSpace, top_down, FallBackSearchMode, 
		NewMode, Response) :-
	top_down_search(IoActionMap, Store, Oracle, !SearchSpace, Response),
	% We always go back to the fallback search mode after a top-down
	% search, because some fallback searches (such as divide and query)
	% use top-down as a fail safe and we want the fallback search to 
	% resume after the top-down search.
	NewMode = FallBackSearchMode.

search(IoActionMap, Store, Oracle, !SearchSpace, SearchMode, FallBackSearchMode,
		NewMode, Response) :-
	SearchMode = follow_subterm_end(SuspectId, ArgPos, TermPath, LastUnknown), 
	follow_subterm_end_search(IoActionMap, Store, Oracle, !SearchSpace,
		LastUnknown, SuspectId, ArgPos, TermPath, FallBackSearchMode,
		NewMode, Response).

search(IoActionMap, Store, Oracle, !SearchSpace, SearchMode,
		FallBackSearchMode, NewMode, Response) :-
	SearchMode = binary(PathArray, Top - Bottom, LastTested),
	binary_search(IoActionMap, Store, Oracle, PathArray, Top, Bottom,
		LastTested, !SearchSpace, FallBackSearchMode, NewMode,
		Response).

search(IoActionMap, Store, Oracle, !SearchSpace, divide_and_query, _, NewMode, 
		Response) :-
	divide_and_query_search(IoActionMap, Store, Oracle, !SearchSpace, 
		Response, NewMode).

:- pred top_down_search(io_action_map::in, S::in, oracle_state::in, 
	search_space(T)::in, search_space(T)::out,
	search_response::out) is det <= mercury_edt(S, T).

top_down_search(IoActionMap, Store, Oracle, !SearchSpace, Response) :-
	%
	% If there's no root yet (because the oracle hasn't asserted any nodes
	% are erroneous yet) then use the topmost suspect as a starting point.
	%
	(
		root(!.SearchSpace, RootId)
	->
		Start = RootId
	;
		topmost_det(!.SearchSpace, Start)
	),
	first_unknown_descendent(IoActionMap, Store, Oracle, Start,
		!SearchSpace, MaybeUnknownDescendent),
	(
		MaybeUnknownDescendent = found(Unknown),
		Response = question(Unknown, top_down)
	;
		MaybeUnknownDescendent = not_found,
		(
			choose_skipped_suspect(!.SearchSpace,
				SkippedSuspect)
		->
			Response = question(SkippedSuspect, skipped)
		;
			% Since the are no skipped suspects and no unknown
			% suspects in the search space, if there is a root
			% (i.e. an erroneous suspect), then it must be a bug.
			% Note that only top down search actually checks if a bug was
			% found.  This is okay, since all the other search algorithms 
			% call top down search if they can't find an unknown suspect.
			root(!.SearchSpace, BugId)
		->
			(
				children(IoActionMap, Store, Oracle, BugId,
					!SearchSpace, BugChildren),
				non_ignored_descendents(IoActionMap,
					Store, Oracle, BugChildren, 
					!SearchSpace, NonIgnoredDescendents),
				list.filter(suspect_correct_or_inadmissible(
					!.SearchSpace), NonIgnoredDescendents,
					CorrectDescendents, [])
			->
				list.filter(suspect_inadmissible(
					!.SearchSpace), BugChildren,
					InadmissibleChildren),
				Response = found_bug(BugId, CorrectDescendents,
					InadmissibleChildren)
			;
				throw(internal_error("top_down_search",
					"bug has unexplored or unknown " ++
					"children"))
			)
		;
			%
			% Try to extend the search space upwards.  If
			% this fails and we're not at the topmost
			% traced node, then request that an explicit
			% supertree be generated.
			%
			(
				extend_search_space_upwards(
					IoActionMap, Store, Oracle,
					!.SearchSpace,
					ExtendedSearchSpace)
			->
				top_down_search(IoActionMap, Store,
					Oracle, ExtendedSearchSpace,
					!:SearchSpace, Response)
			;
				topmost_det(!.SearchSpace, TopMostId),
				TopMostNode =
					get_edt_node(!.SearchSpace,
					TopMostId),
				(
					edt_topmost_node(Store, TopMostNode)
				->
					% We can't look any higher.
					Response = no_suspects
				;
					Response = require_explicit_supertree
				)
			)
		)
	;
		MaybeUnknownDescendent = require_explicit_subtree(
			RequireExplicitId),
		Response = require_explicit_subtree(RequireExplicitId)
	).

:- pred follow_subterm_end_search(io_action_map::in, S::in, oracle_state::in, 
	search_space(T)::in, search_space(T)::out, 
	maybe(suspect_id)::in, suspect_id::in,
	arg_pos::in, term_path::in, search_mode::in, search_mode::out,
	search_response::out) is det <= mercury_edt(S, T).

follow_subterm_end_search(IoActionMap, Store, Oracle, !SearchSpace, 
		LastUnknown, SuspectId, ArgPos, TermPath, FallBackSearchMode,
		NewMode, SearchResponse) :-
	find_subterm_origin(IoActionMap, Store, Oracle, SuspectId, ArgPos, 
		TermPath, !SearchSpace, FindOriginResponse),
	(
		FindOriginResponse = primitive_op(BindingSuspectId, FileName, 
			LineNo, PrimOpType, Output),
		ProcLabel = get_proc_label_for_suspect(Store, !.SearchSpace,
			BindingSuspectId),
		( 
			Output = yes,
			% BindingSuspectId = SuspectId since the
			% subterm is an output of SuspectId.
			BindingNode = get_edt_node(!.SearchSpace, SuspectId),
			ArgNum = edt_arg_pos_to_user_arg_num(Store, 
				BindingNode, ArgPos),
			MaybePath = yes([ArgNum | TermPath])
		;
			Output = no,
			% Since the subterm is not an output of the 
			% binding node, it will not appear in any of the
			% arguments of the binding node (it can't be an 
			% input, because then it would have been bound outside
			% the node).
			MaybePath = no
		),
		(
			% We ask about the binding node even if it was 
			% previously skipped, since this behaviour is
			% more predictable from the user's perspective.
			%
			( suspect_unknown(!.SearchSpace, BindingSuspectId)
			; suspect_skipped(!.SearchSpace, BindingSuspectId)
			)
		->
			SearchResponse = question(BindingSuspectId, 
				binding_node(PrimOpType, FileName, LineNo,
					MaybePath, ProcLabel, no)),
			setup_binary_search(!.SearchSpace, BindingSuspectId,
				NewMode)
		;
			(
				LastUnknown = yes(Unknown),
				suspect_still_unknown(!.SearchSpace, Unknown)
			->
				Reason = binding_node(PrimOpType, 
					FileName, LineNo, MaybePath, ProcLabel,
					yes),
				SearchResponse = question(Unknown, Reason),
				setup_binary_search(!.SearchSpace, 
					Unknown, NewMode)
			;
				search(IoActionMap, Store, Oracle, 
					!SearchSpace, FallBackSearchMode, 
					FallBackSearchMode, NewMode, 
					SearchResponse)
			)
		)
	;
		FindOriginResponse = not_found,
		(
			LastUnknown = yes(Unknown),
			suspect_still_unknown(!.SearchSpace, Unknown)
		->
			SearchResponse = question(Unknown, 
				subterm_no_proc_rep),
			setup_binary_search(!.SearchSpace, Unknown, NewMode)
		;
			search(IoActionMap, Store, Oracle, !SearchSpace, 
				FallBackSearchMode, FallBackSearchMode,
				NewMode, SearchResponse)
		)
	;
		FindOriginResponse = require_explicit_subtree,
		SearchResponse = require_explicit_subtree(SuspectId),
		%
		% Record the current position of the search so 
		% we can continue where we left off once the explicit
		% subtree has been generated.
		%
		NewMode = follow_subterm_end(SuspectId, ArgPos, TermPath,
			LastUnknown)
	;
		FindOriginResponse = require_explicit_supertree,
		SearchResponse = require_explicit_supertree,
		NewMode = follow_subterm_end(SuspectId, ArgPos, TermPath,
			LastUnknown)
	;
		FindOriginResponse = origin(OriginId, OriginArgPos, 
			OriginTermPath, SubtermMode),
		(
			suspect_unknown(!.SearchSpace, OriginId)
		->
			NewLastUnknown = yes(OriginId)
		;
			NewLastUnknown = LastUnknown
		),
		(
			%
			% Check if it's worth continuing tracking the sub-term.
			% We want to stop if we enter a portion of the search
			% space known not to contain the bug from which we 
			% can't return (for example if we come across an
			% erroneous node where the sub-term is an input).
			%
			give_up_subterm_tracking(!.SearchSpace, OriginId,
				SubtermMode)
		->
			(
				LastUnknown = yes(Unknown),
				suspect_still_unknown(!.SearchSpace, Unknown)
			->
				SearchResponse = question(Unknown, 
					binding_node_eliminated),
				setup_binary_search(!.SearchSpace,
					Unknown, NewMode)
			;
				search(IoActionMap, Store, Oracle, 
					!SearchSpace, FallBackSearchMode, 
					FallBackSearchMode, NewMode, 
					SearchResponse)
			)
		;
			%
			% This recursive call will not lead to an infinite loop
			% because eventually either the sub-term will be bound
			% (and find_subterm_origin will respond with
			% primitive_op/3) or there will be insufficient tracing
			% information to continue (and find_subterm_origin will
			% respond with not_found).
			%
			follow_subterm_end_search(IoActionMap, Store, Oracle,
				!SearchSpace, NewLastUnknown, OriginId,
				OriginArgPos, OriginTermPath,
				FallBackSearchMode, NewMode, SearchResponse)
		)
	).

	% setup_binary_search(SearchSpace, SuspectId, Response, SearchMode).
	% Sets up the search mode to do a binary search between SuspectId
	% and either the root of the search space if a suspect has 
	% previously been marked erroneous, or the topmost node if no suspect
	% has yet been marked erroneous.
	%
:- pred setup_binary_search(search_space(T)::in, suspect_id::in,
	search_mode::out) is det.

setup_binary_search(SearchSpace, SuspectId, SearchMode) :-
	(
		root(SearchSpace, RootId) 
	->
		TopId = RootId,
		BottomId = SuspectId
	;
		topmost_det(SearchSpace, TopId),
		BottomId = SuspectId
	),
	(
		get_path(SearchSpace, BottomId, TopId, Path)
	->
		PathArray = array.from_list(Path),
		array.bounds(PathArray, Top, Bottom),
		SearchMode = binary(PathArray, Top - Bottom, Bottom)
	;
		throw(internal_error("setup_binary_search", 
			"TopId not an ancestor of BottomId"))
	).

:- pred binary_search(io_action_map::in, S::in, oracle_state::in, 
	array(suspect_id)::in, int::in, int::in, int::in,
	search_space(T)::in, search_space(T)::out, search_mode::in,
	search_mode::out, search_response::out) is det <= mercury_edt(S, T).

binary_search(IoActionMap, Store, Oracle, PathArray, Top, Bottom, LastTested,
		!SearchSpace, FallBackSearchMode, NewMode, Response) :-
	SuspectId = PathArray ^ elem(LastTested),
	%
	% Check what the result of the query about LastTested was and adjust
	% the range appropriately.
	%
	(
		% The oracle answered `erroneous'.
		suspect_in_excluded_complement(!.SearchSpace, SuspectId)
	->
		NewTop = LastTested + 1,
		NewBottom = Bottom
	;
		% The oracle answered `correct' or `inadmissible'
		suspect_in_excluded_subtree(!.SearchSpace, SuspectId)
	->
		NewTop = Top,
		NewBottom = LastTested - 1
	;
		% The suspect is trusted(ignored) or was skipped.
		NewTop = Top,
		NewBottom = Bottom
	),
	(
		NewTop > NewBottom
	->
		% Revert to the fallback search mode when binary search is 
		% over.
		search(IoActionMap, Store, Oracle, !SearchSpace, 
			FallBackSearchMode, FallBackSearchMode, NewMode,
			Response)
	;
		(
			find_unknown_closest_to_middle(!.SearchSpace, 
				PathArray, NewTop, NewBottom,
				UnknownClosestToMiddle)
		->
			NewMode = binary(PathArray, NewTop - NewBottom, 
				UnknownClosestToMiddle),
			Response = question(PathArray ^ elem(
				UnknownClosestToMiddle), binary(NewBottom, 
					NewTop, UnknownClosestToMiddle))
		;
			% No unknown suspects on the path, so revert to
			% the fallback search mode.
			search(IoActionMap, Store, Oracle, !SearchSpace, 
				FallBackSearchMode, FallBackSearchMode,
				NewMode, Response)
		)
	).

	% find_unknown_closest_to_middle(SearchSpace, PathArray, Top, Bottom, 
	%	Unknown).
	% Unknown is the position in PathArray of the suspect which has status
	% unknown and is closest to halfway between From and To which are 
	% also indexes into PathArray.  Fails if there are no unknown suspects
	% between From and To (inclusive).
	%
:- pred find_unknown_closest_to_middle(search_space(T)::in, 
	array(suspect_id)::in, int::in, int::in, int::out) is semidet.

find_unknown_closest_to_middle(SearchSpace, PathArray, Top, Bottom, Unknown) :-
	Middle = Top + ((Bottom - Top) // 2),
	find_unknown_closest_to_range(SearchSpace, PathArray, Top, Bottom, 
		Middle, Middle, Unknown).

	% find_unknown_closest_to_range(SearchSpace, PathArray, OuterTop, 
	%	OuterBottom, InnerTop, InnerBottom, Unknown)
	% Unknown is a position in PathArray between OuterTop and OuterBottom
	% (inclusive) where the status of the suspect is unknown. The preferred
	% position to return is as close as possible to InnerTop and
	% InnerBottom, with the proviso that elements between InnerTop and
	% InnerBottom (exclusive) aren't tested, since the caller has already
	% found they were not unknown.
	%
:- pred find_unknown_closest_to_range(search_space(T)::in, 
	array(suspect_id)::in, int::in, int::in, int::in, int::in, int::out) 
	is semidet.

find_unknown_closest_to_range(SearchSpace, PathArray, OuterTop, OuterBottom,
		InnerTop, InnerBottom, Unknown) :-
	InnerTop =< InnerBottom,
	( OuterTop =< InnerTop ; InnerBottom =< OuterBottom ),
	(
		OuterTop =< InnerTop, 
		suspect_unknown(SearchSpace, PathArray ^ elem(InnerTop))
	->
		Unknown = InnerTop
	;
		InnerBottom =< OuterBottom,
		suspect_unknown(SearchSpace, PathArray ^ elem(InnerBottom))
	->
		Unknown = InnerBottom
	;
		find_unknown_closest_to_range(SearchSpace, PathArray,
			OuterTop, OuterBottom, InnerTop - 1, InnerBottom + 1,
			Unknown)
	).	

:- pred divide_and_query_search(io_action_map::in, S::in, oracle_state::in,
	search_space(T)::in, search_space(T)::out, search_response::out,
	search_mode::out) is det <= mercury_edt(S, T).

divide_and_query_search(IoActionMap, Store, Oracle, !SearchSpace, Response, 
		NewMode) :-
	%
	% If there's no root yet (because the oracle hasn't asserted any nodes
	% are erroneous yet), then use top-down search.
	%
	(
		root(!.SearchSpace, RootId)
	->
		NewMode = divide_and_query,
		(
			children(IoActionMap, Store, Oracle, RootId, 
				!SearchSpace, Children)
		->
			find_middle_weight(IoActionMap, Store, Oracle, 
				Children, RootId, no, !SearchSpace, Response)
		;
			Response = require_explicit_subtree(RootId)
		)
	;
		top_down_search(IoActionMap, Store, Oracle, !SearchSpace, 
			Response),
		NewMode = divide_and_query
	).

	% Call find_middle_weight/9 if we are able to find the children of the
	% given suspect id, otherwise return a require_explicit_subtree
	% search response in the last argument.
	%
:- pred find_middle_weight_if_children(io_action_map::in, S::in,
	oracle_state::in, suspect_id::in, suspect_id::in,
	maybe(suspect_id)::in, search_space(T)::in, search_space(T)::out,
	search_response::out) is det <= mercury_edt(S, T).

find_middle_weight_if_children(IoActionMap, Store, Oracle, SuspectId, TopId, 
		MaybeLastUnknown, !SearchSpace, Response) :-
	(
		children(IoActionMap, Store, Oracle, SuspectId, !SearchSpace, 
			Children)
	->
		find_middle_weight(IoActionMap, Store, Oracle, Children, TopId,
			MaybeLastUnknown, !SearchSpace, Response)
	;
		Response = require_explicit_subtree(SuspectId)
	).

	% find_middle_weight(IoActionMap, Store, Oracle, SuspectIds, TopId, 
	%	MaybeLastUnknown, !SearchSpace, Response).
	% Find the unknown suspect whose weight is closest to half the weight
	% of TopId, considering only the heaviest suspect in SuspectIds, the
	% heaviest child of the heaviest suspect in SuspectIds and so on.
	% MaybeLastUnknown is the last node that was unknown in the search (if
	% any).  
	%
:- pred find_middle_weight(io_action_map::in, S::in, oracle_state::in, 
	list(suspect_id)::in, suspect_id::in,
	maybe(suspect_id)::in, search_space(T)::in, 
	search_space(T)::out, search_response::out)
	is det <= mercury_edt(S, T).

find_middle_weight(IoActionMap, Store, Oracle, [], TopId, MaybeLastUnknown,
		!SearchSpace, Response) :-
	(
		MaybeLastUnknown = yes(LastUnknown),
		suspect_still_unknown(!.SearchSpace, LastUnknown)
	->
		Response = question(LastUnknown, divide_and_query(
			get_weight(!.SearchSpace, TopId), 
			get_weight(!.SearchSpace, LastUnknown)))
	;
		% This could happen when there were no unknown suspects 
		% encountered during the search, in which case we revert 
		% to top-down search.
		top_down_search(IoActionMap, Store, Oracle, !SearchSpace, 
			Response)
	).
find_middle_weight(IoActionMap, Store, Oracle, [SuspectId | SuspectIds], TopId,
		MaybeLastUnknown, !SearchSpace, Response) :-
	TopWeight = get_weight(!.SearchSpace, TopId),
	Target = TopWeight // 2,
	%
	% Find the heaviest suspect:
	%
	Weight = get_weight(!.SearchSpace, SuspectId),
	list.foldl(max_weight(!.SearchSpace), SuspectIds, 
		{Weight, SuspectId}, {MaxWeight, Heaviest}),
	(
		MaxWeight > Target
	->
		(
			suspect_unknown(!.SearchSpace, Heaviest)
		->
			NewMaybeLastUnknown = yes(Heaviest)
		;
			NewMaybeLastUnknown = MaybeLastUnknown
		),
		find_middle_weight_if_children(IoActionMap, Store, Oracle, 
			Heaviest, TopId, NewMaybeLastUnknown, !SearchSpace,
			Response)
	;
		(
			suspect_unknown(!.SearchSpace, Heaviest)
		->
			(
				MaybeLastUnknown = yes(LastUnknown),
				suspect_still_unknown(!.SearchSpace, LastUnknown)
			->
				LastUnknownWeight = get_weight(!.SearchSpace, 
					LastUnknown),
				%
				% If the last unknown suspect was closer to
				% the target weight then ask about it.
				%
				( 
					LastUnknownWeight - Target <
						Target - MaxWeight
				->
					Response = question(LastUnknown, 
						divide_and_query(TopWeight,
							LastUnknownWeight))
				;					
					Response = question(Heaviest,
						divide_and_query(TopWeight,
							MaxWeight))
				)
			;
				Response = question(Heaviest, 
					divide_and_query(TopWeight, MaxWeight))
			)
		;
			(
				MaybeLastUnknown = yes(LastUnknown),
				suspect_still_unknown(!.SearchSpace, LastUnknown)
			->
				LastUnknownWeight = get_weight(!.SearchSpace, 
					LastUnknown),
				Response = question(LastUnknown,
					divide_and_query(TopWeight,
						LastUnknownWeight))
			;
				% Look deeper until we find an unknown:
				find_middle_weight_if_children(IoActionMap,
					Store, Oracle, Heaviest, TopId, no,
					!SearchSpace, Response)
			)
		)		
	).

:- pred max_weight(search_space(T)::in, suspect_id::in, 
	{int, suspect_id}::in, {int, suspect_id}::out) 
	is det.

max_weight(SearchSpace, SuspectId, {PrevMax, PrevSuspectId},
	{NewMax, NewSuspectId}) :-
	Weight = get_weight(SearchSpace, SuspectId),
	(
		Weight > PrevMax
	->
		NewMax = Weight,
		NewSuspectId = SuspectId
	;
		NewMax = PrevMax,
		NewSuspectId = PrevSuspectId
	).

%-----------------------------------------------------------------------------%

	% Check that a suspect is still unknown.  This is called by the search
	% algorithms to make double sure that a suspect is still unknown (it
	% might not be unknown if, for example, an erroneous suspect was added
	% to the search space during the search).
	%
:- pred suspect_still_unknown(search_space(T)::in, suspect_id::in) is semidet.

suspect_still_unknown(SearchSpace, SuspectId) :-
	suspect_unknown(SearchSpace, SuspectId).

%-----------------------------------------------------------------------------%

reason_to_string(start) = "this is the node where the `dd' command "
	++ "was issued.".

reason_to_string(binding_node(PrimOpType, FileName, LineNo,
		MaybePath, ProcLabel, Eliminated)) = Str :-
	PrimOpStr = primitive_op_type_to_string(PrimOpType),
	LineNoStr = int_to_string(LineNo),
	get_pred_attributes(ProcLabel, SymModule, Name, Arity, 
		PredOrFunc),
	(
		PredOrFunc = function,
		PredOrFuncStr = "function"
	;
		PredOrFunc = predicate,
		PredOrFuncStr = "predicate"
	),
	Module = sym_name_to_string(SymModule),
	ArityStr = int_to_string(Arity),
	(
		Eliminated = yes,
		EliminatedSent = " That node was, however, previously "
			++ "eliminated from the bug search."
	;
		Eliminated = no,
		EliminatedSent = ""
	),
	(
		MaybePath = yes(Path),
		PathStrings = list.map(int_to_string, Path),
		PathStr = string.join_list("/", PathStrings),
		PathSent = "The path to the subterm in the atom is " ++
			PathStr ++ "."
	;
		MaybePath = no,
		PathSent = ""
	),
	Str = "the marked subterm was bound by the " ++ 
		PrimOpStr ++ " inside the " ++ PredOrFuncStr ++
		" " ++ Module ++ "." ++ Name ++ "/" ++ ArityStr ++
		" (" ++ FileName ++ ":" ++ LineNoStr ++ "). " ++
		PathSent ++ EliminatedSent.
		
reason_to_string(top_down) = "this is the next node in the top-down " 
	++ "search.".

reason_to_string(subterm_no_proc_rep) = 
	"tracking of the marked subterm had to be aborted here, because of "
	++ "missing tracing information.".

reason_to_string(binding_node_eliminated) = 
	"tracking of the marked subterm was stopped here, because the binding "
	++ "node lies in a portion of the tree which has been eliminated.".

reason_to_string(binary(Bottom, Top, Split)) = Str :-
	PathLengthStr = int_to_string_thousands(Bottom - Top + 1),
	SubPath1LengthStr = int_to_string_thousands(Bottom - Split),
	SubPath2LengthStr = int_to_string_thousands(Split - Top + 1),
	Str = "this node divides a path of length " ++ PathLengthStr
		++ " into two paths of length " ++
		SubPath1LengthStr ++ " and " ++ SubPath2LengthStr ++ ".".

reason_to_string(divide_and_query(OldWeight, SubtreeWeight)) = Str :-
	Weight1Str = int_to_string_thousands(OldWeight - SubtreeWeight),
	Weight2Str = int_to_string_thousands(SubtreeWeight),
	Str = "this node divides the suspect area into " ++
		"two regions of " ++ Weight1Str ++ " and " ++ Weight2Str ++
		" events each.".

reason_to_string(skipped) = "there are no more non-skipped questions "
	++ "left.".

reason_to_string(revise) = "this node is being revised, because of "
	++ "an unsuccessful previous bug search.".

show_info(Store, OutStream, Analyser, Response, !IO) :-
	SearchSpace = Analyser ^ search_space,
	some [!FieldNames, !Data] (
		!:FieldNames = [], 
		!:Data = [],
		%
		% Get the context of the current question.
		%
		(
			Analyser ^ last_search_question = 
				yes(suspect_and_reason(LastId, Reason)),
			(
				edt_context(Store, get_edt_node(SearchSpace, 
					LastId), FileName -  LineNo, 
					MaybeReturnContext)
			->
				(
					MaybeReturnContext = 
						yes(ReturnFileName - 
						ReturnLineNo),
					ContextStr = FileName ++ ":" ++ 
						int_to_string(LineNo) ++
						" (" ++ ReturnFileName ++ ":" 
						++ int_to_string(ReturnLineNo) 
						++ ")"
				;
					MaybeReturnContext = no,
					ContextStr = FileName ++ ":" ++ 
						int_to_string(LineNo)
				),
				list.append(!.FieldNames, 
					["Context of current question"], 
					!:FieldNames),
				list.append(!.Data, [ContextStr], !:Data)
			;
				true
			)
		;
			Analyser ^ last_search_question = no,
			throw(internal_error("show_info", "no last question"))
		),

		list.append(!.FieldNames, ["Search mode"], 
			!:FieldNames),
		list.append(!.Data, [search_mode_to_string(
			Analyser ^ search_mode)], !:Data),

		(
			Analyser ^ search_mode = divide_and_query
		->
			list.append(!.FieldNames, 
				["Estimated questions remaining"], 
				!:FieldNames),
			EstimatedQuestions = float.ceiling_to_int(
				math.log2(float(Weight))),
			list.append(!.Data, 
				[int_to_string(EstimatedQuestions)], !:Data)
		;
			true
		),
		
		list.append(!.FieldNames, ["Number of suspect events"], 
			!:FieldNames),
		(
			root(SearchSpace, RootId)
		->
			StartId = RootId
		;
			topmost_det(SearchSpace, StartId)
		),
		Weight = get_weight(SearchSpace, StartId),
		list.append(!.Data, [int_to_string_thousands(Weight)], !:Data),
		
		InfoMessage = string.format_table([left(!.FieldNames),
			left(!.Data)], " : ")
	),
	ReasonStr = reason_to_string(Reason),
	ReasonSent = "The current question was chosen because " ++ ReasonStr,
	WrappedReason = string.word_wrap(ReasonSent, 72),
 	io.format(OutStream, "%s\n%s\n", [s(InfoMessage), s(WrappedReason)], 
		!IO),
	Node = get_edt_node(SearchSpace, LastId),
	edt_question(Analyser ^ io_action_map, Store, Node,
		OracleQuestion),
	Response = oracle_question(OracleQuestion).

:- func search_mode_to_string(search_mode) = string.

search_mode_to_string(top_down) = "top down".
search_mode_to_string(follow_subterm_end(_, _, _, _)) = 
	"tracking marked sub-term".
search_mode_to_string(binary(_, _, _)) = "binary search on path".
search_mode_to_string(divide_and_query) = "divide and query".
