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
:- import_module mdb.io_action.
:- import_module mdb.declarative_edt.

:- import_module std_util.

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
:- pred start_or_resume_analysis(S::in, analysis_type(T)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

	% Continue analysis after the oracle has responded with an
	% answer.
	%
:- pred continue_analysis(S::in, decl_answer(T)::in,
	analyser_response(T)::out, analyser_state(T)::in,
	analyser_state(T)::out) is det <= mercury_edt(S, T).

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
:- import_module mdbcomp.program_representation.

:- import_module exception, string, map, int, counter, array, list.

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
	--->	question(suspect_id)
	;	require_explicit_subtree(suspect_id)
	;	require_explicit_supertree
	;	no_suspects.

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
			last_search_question	:: maybe(suspect_id),

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

start_or_resume_analysis(Store, AnalysisType, Response, !Analyser) :-
	(
		AnalysisType = new_tree(Node),
		MaybeRequireExplicit = !.Analyser ^ require_explicit,
		(
			MaybeRequireExplicit = yes(TreeType),
			SearchSpace0 = !.Analyser ^ search_space,
			(
				TreeType = explicit_supertree,
				incorporate_explicit_supertree(Store, Node, 
					SearchSpace0, SearchSpace)
			;
				TreeType = explicit_subtree(SuspectId),
				incorporate_explicit_subtree(SuspectId, Node, 
					SearchSpace0, SearchSpace)
			),
			!:Analyser = !.Analyser ^ search_space := SearchSpace,
			!:Analyser = !.Analyser ^ require_explicit := no,
			decide_analyser_response(Store, Response, !Analyser)
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
				yes(TopMostId),
			edt_question(!.Analyser ^ io_action_map, Store, Node, 
				Question),
			Response = revise(Question)
		)
	;
		AnalysisType = resume_previous,
		decide_analyser_response(Store, Response, !Analyser)
	).

continue_analysis(Store, Answer, Response, !Analyser) :-
	(
		!.Analyser ^ last_search_question = yes(SuspectId),
		process_answer(Store, Answer, SuspectId, !Analyser)
	;
		!.Analyser ^ last_search_question = no,
		throw(internal_error("continue_analysis", 
			"received answer to unasked question"))
	),
	!:Analyser = !.Analyser ^ last_search_question := no,
	decide_analyser_response(Store, Response, !Analyser).

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
		!:Analyser = !.Analyser ^ last_search_question := yes(RootId),
		!:Analyser = !.Analyser ^ search_mode := 
			!.Analyser ^ fallback_search_mode
	;
		% There must be a root, since a bug was found (and is now
		% being revised).
		throw(internal_error("revise_analysis", "no root"))
	).

:- pred decide_analyser_response(S::in, analyser_response(T)::out,
	analyser_state(T)::in, analyser_state(T)::out) 
	is det <= mercury_edt(S, T).

decide_analyser_response(Store, Response, !Analyser) :-
	% Do a sanity check before the search.  So we can determine afterwards
	% if the search corrupted the search space at all.
	% XXX this should be removed at some stage as it's relatively slow.
	check_search_space_consistency(Store, !.Analyser ^ search_space,
		"Start of decide_analyser_response"),
	some [!SearchSpace] (
		!:SearchSpace = !.Analyser ^ search_space,
		(
			root(!.SearchSpace, RootId),
			suspect_is_bug(Store, RootId, !SearchSpace,
				CorrectDescendents, InadmissibleChildren)
		->
			!:Analyser = !.Analyser ^ search_space :=
				!.SearchSpace,
			bug_response(Store, !.Analyser ^ io_action_map, 
				!.SearchSpace, RootId, 
				[RootId | CorrectDescendents], 
				InadmissibleChildren, Response)
		;
			search(Store, !SearchSpace, !.Analyser ^ search_mode,
				!.Analyser ^ fallback_search_mode, NewMode, 
				SearchResponse),
			!:Analyser = !.Analyser ^ search_space :=
				!.SearchSpace,
			!:Analyser = !.Analyser ^ search_mode := NewMode,
			handle_search_response(Store, SearchResponse, 
				!Analyser, Response)
		)
	),
	% Do a sanity check after the search to determine if the search 
	% corrupted the search space at all. 
	% XXX this should be removed at some stage as it's relatively slow.
	check_search_space_consistency(Store, !.Analyser ^ search_space,
		"End of decide_analyser_response").

:- pred handle_search_response(S::in, search_response::in, 
	analyser_state(T)::in, analyser_state(T)::out, 
	analyser_response(T)::out) is det <= mercury_edt(S, T).

handle_search_response(Store, question(SuspectId), !Analyser, Response) :-
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
	!:Analyser = !.Analyser ^ last_search_question := yes(SuspectId).

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
:- pred search(S::in, search_space(T)::in, search_space(T)::out, 
	search_mode::in, search_mode::in, 
	search_mode::out, search_response::out) is det <= mercury_edt(S, T).

search(Store, !SearchSpace, top_down, FallBackSearchMode, NewMode, Response) :-
	top_down_search(Store, !SearchSpace, Response),
	% We always go back to the fallback search mode after a top-down
	% search, because some fallback searches (such as divide and query)
	% use top-down as a fail safe and we want the fallback search to 
	% resume after the top-down search.
	NewMode = FallBackSearchMode.

search(Store, !SearchSpace, follow_subterm_end(SuspectId, ArgPos, TermPath,	
		LastUnknown), FallBackSearchMode, NewMode, Response) :-
	follow_subterm_end_search(Store, !SearchSpace, LastUnknown, SuspectId, 
		ArgPos, TermPath, FallBackSearchMode, NewMode, Response).

search(Store, !SearchSpace, binary(PathArray, Top - Bottom, LastTested),
		FallBackSearchMode, NewMode, Response) :-
	binary_search(Store, PathArray, Top, Bottom, LastTested, !SearchSpace, 
		FallBackSearchMode, NewMode, Response).

search(Store, !SearchSpace, divide_and_query, _, NewMode, Response) :-
	divide_and_query_search(Store, !SearchSpace, Response, NewMode).

:- pred top_down_search(S::in, search_space(T)::in, search_space(T)::out,
	search_response::out) is det <= mercury_edt(S, T).

top_down_search(Store, !SearchSpace, Response) :-
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
	(
		first_unknown_descendent(Store, Start, 
			!.SearchSpace, SearchSpace1, MaybeDescendent)
	->
		SearchSpace1 = !:SearchSpace,
		(
			MaybeDescendent = yes(Unknown),
			Response = question(Unknown)
		;
			MaybeDescendent = no,
			(
				choose_skipped_suspect(!.SearchSpace,
					SkippedSuspect)
			->
				Response = question(SkippedSuspect)
			;
				%
				% Try to extend the search space upwards.  If
				% this fails and we're not at the topmost
				% traced node, then request that an explicit
				% supertree be generated.
				%
				(
					extend_search_space_upwards(Store,
						!.SearchSpace,
						ExtendedSearchSpace)
				->
					top_down_search(Store,
						ExtendedSearchSpace,
						!:SearchSpace, Response)
				;
					topmost_det(!.SearchSpace, TopMostId),
					TopMostNode =
						get_edt_node(!.SearchSpace,
						TopMostId),
					(
						edt_topmost_node(Store,
							TopMostNode)
					->
						% We can't look any higher.
						Response = no_suspects
					;
						Response =
						    require_explicit_supertree
					)
				)
			)
		)
	;
		%
		% An explicit subtree is required, so pick an implicit root
		% to make explicit.  pick_implicit_root/3 will choose an
		% implicit root that is a descendent of the root id of the 
		% search space.  There is no point in making an implicit root
		% that is not a descendent of the root id explicit, since
		% all suspects above the root id have been excluded from the
		% bug search.  pick_implicit_root will also not choose an
		% implicit root that is a descendent of a correct or
		% inadmissible node, for the same reason.
		% 
		(
			pick_implicit_root(Store, !.SearchSpace, ImplicitRoot)
		->
			Response = require_explicit_subtree(ImplicitRoot)
		;
			throw(internal_error("top_down_search",
				"first_unknown_descendent requires an "
				++ "explicit subtree to be generated, but "
				++ "pick_implicit_root couldn't find an "
				++ "implicit root to generate an "
				++ "explicit subtree from"))
		)
	).

:- pred follow_subterm_end_search(S::in, search_space(T)::in,
	search_space(T)::out, maybe(suspect_id)::in, suspect_id::in,
	arg_pos::in, term_path::in, search_mode::in, search_mode::out,
	search_response::out) is det <= mercury_edt(S, T).

follow_subterm_end_search(Store, !SearchSpace, LastUnknown, SuspectId, ArgPos, 
		TermPath, FallBackSearchMode, NewMode, SearchResponse) :-
	find_subterm_origin(Store, SuspectId, ArgPos, TermPath, !SearchSpace,
		FindOriginResponse),
	(
		FindOriginResponse = primitive_op(PrimitiveOpId, _, _),
		%
		% XXX In future the filename and line number of the primitive
		% operation could be printed out if the node in which the
		% primitive operation occured turned out to be a bug.
		%
		(
			suspect_unknown(!.SearchSpace, PrimitiveOpId)
		->
			SearchResponse = question(PrimitiveOpId),
			setup_binary_search(!.SearchSpace,  PrimitiveOpId,
				NewMode)
		;
			(
				LastUnknown = yes(Unknown),
				SearchResponse = question(Unknown),
				setup_binary_search(!.SearchSpace, 
					Unknown, NewMode)
			;
				LastUnknown = no,
				search(Store, !SearchSpace, FallBackSearchMode, 
					FallBackSearchMode, NewMode, 
					SearchResponse)
				)
		)
	;
		FindOriginResponse = not_found,
		(
			LastUnknown = yes(Unknown),
			SearchResponse = question(Unknown),
			setup_binary_search(!.SearchSpace,  
				Unknown, NewMode)
		;
			LastUnknown = no,
			search(Store, !SearchSpace, FallBackSearchMode, 
				FallBackSearchMode, NewMode, SearchResponse)
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
			OriginTermPath),
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
			give_up_subterm_tracking(!.SearchSpace, OriginId)
		->
			(
				LastUnknown = yes(Unknown),
				SearchResponse = question(Unknown),
				setup_binary_search(!.SearchSpace,
					Unknown, NewMode)
			;
				LastUnknown = no,
				search(Store, !SearchSpace, FallBackSearchMode, 
					FallBackSearchMode, NewMode, 
					SearchResponse)
			)
		;
			%
			% This recursive call will not lead to an infinite loop
			% because eventually either the sub-term will be bound
			% (and find_subterm_origin will respond with
			% primitive_op/2) or there will be insufficient tracing
			% information to continue (and find_subterm_origin will
			% respond with not_found).
			%
			follow_subterm_end_search(Store, !SearchSpace, 
				NewLastUnknown, OriginId, OriginArgPos,
				OriginTermPath, FallBackSearchMode, NewMode, 
				SearchResponse)
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

:- pred binary_search(S::in, array(suspect_id)::in, int::in, int::in, int::in,
	search_space(T)::in, search_space(T)::out, search_mode::in,
	search_mode::out, search_response::out) is det <= mercury_edt(S, T).

binary_search(Store, PathArray, Top, Bottom, LastTested, !SearchSpace, 
		FallBackSearchMode, NewMode, Response) :-
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
		search(Store, !SearchSpace, FallBackSearchMode,
			FallBackSearchMode, NewMode, Response)
	;
		(
			find_unknown_closest_to_middle(!.SearchSpace, 
				PathArray, NewTop, NewBottom,
				UnknownClosestToMiddle)
		->
			NewMode = binary(PathArray, NewTop - NewBottom, 
				UnknownClosestToMiddle),
			Response = question(PathArray ^ elem(
				UnknownClosestToMiddle))
		;
			% No unknown suspects on the path, so revert to
			% the fallback search mode.
			search(Store, !SearchSpace, FallBackSearchMode, 
				FallBackSearchMode, NewMode, Response)
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

:- pred divide_and_query_search(S::in, search_space(T)::in, 
	search_space(T)::out, search_response::out, search_mode::out) is det
	<= mercury_edt(S, T).

divide_and_query_search(Store, !SearchSpace, Response, NewMode) :-
	%
	% If there's no root yet (because the oracle hasn't asserted any nodes
	% are erroneous yet), then use top-down search.
	%
	(
		root(!.SearchSpace, RootId)
	->
		NewMode = divide_and_query,
		(
			children(Store, RootId, !SearchSpace, Children)
		->
			find_middle_weight(Store, Children, RootId, no,
				!SearchSpace, Response)
		;
			Response = require_explicit_subtree(RootId)
		)
	;
		top_down_search(Store, !SearchSpace, Response),
		NewMode = divide_and_query
	).

	% Call find_middle_weight/7 if we are able to find the children of the
	% given suspect id, otherwise return a require_explicit_subtree
	% search response in the last argument.
	%
:- pred find_middle_weight_if_children(S::in, suspect_id::in, suspect_id::in,
	maybe(suspect_id)::in, search_space(T)::in, search_space(T)::out,
	search_response::out) is det <= mercury_edt(S, T).

find_middle_weight_if_children(Store, SuspectId, TopId, MaybeLastUnknown,
		!SearchSpace, Response) :-
	(
		children(Store, SuspectId, !SearchSpace, Children)
	->
		find_middle_weight(Store, Children, TopId,
			MaybeLastUnknown, !SearchSpace, Response)
	;
		Response = require_explicit_subtree(SuspectId)
	).

	% find_middle_weight(Store, SuspectIds, TopId, MaybeLastUnknown,
	%	!SearchSpace, Response).
	% Find the unknown suspect whose weight is closest to half the weight
	% of TopId, considering only the heaviest suspect in SuspectIds, the
	% heaviest child of the heaviest suspect in SuspectIds and so on.
	% MaybeLastUnknown is the last node that was unknown in the search (if
	% any).  
	%
:- pred find_middle_weight(S::in, list(suspect_id)::in, suspect_id::in,
	maybe(suspect_id)::in, search_space(T)::in, 
	search_space(T)::out, search_response::out)
	is det <= mercury_edt(S, T).

find_middle_weight(Store, [], _, MaybeLastUnknown, !SearchSpace, Response) :-
	(
		MaybeLastUnknown = yes(LastUnknown),
		Response = question(LastUnknown)
	;
		MaybeLastUnknown = no,
		% This could happen when there were no unknown suspects 
		% encountered during the search, in which case we revert 
		% to top-down search.
		top_down_search(Store, !SearchSpace, Response)
	).
find_middle_weight(Store, [SuspectId | SuspectIds], TopId, MaybeLastUnknown, 
		!SearchSpace, Response) :-
	Target = get_weight(!.SearchSpace, TopId) // 2,
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
		find_middle_weight_if_children(Store, Heaviest, TopId,
			NewMaybeLastUnknown, !SearchSpace, Response)
	;
		(
			suspect_unknown(!.SearchSpace, Heaviest)
		->
			(
				MaybeLastUnknown = yes(LastUnknown),
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
					Response = question(LastUnknown)
				;					
					Response = question(Heaviest)
				)
			;
				MaybeLastUnknown = no,
				Response = question(Heaviest)
			)
		;
			(
				MaybeLastUnknown = yes(LastUnknown),
				Response = question(LastUnknown)
			;
				MaybeLastUnknown = no,
				% Look deeper until we find an unknown:
				find_middle_weight_if_children(Store, Heaviest,
					TopId, no, !SearchSpace, Response)
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
