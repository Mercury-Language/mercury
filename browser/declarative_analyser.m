%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: declarative_analyser.m
% Authors: Mark Brown, Ian MacLarty
%
% This module implements some analysis algorithms that search for bugs in
% Evaluation Dependency Trees (EDTs). The search algorithms use information
% provided by the search_space data type which acts as a layer on top of the
% EDT, storing information relevant to the bug search. Throughout this module
% the type variables T and S refer to the types of nodes in the EDT and the
% store of EDT nodes respectively.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.declarative_analyser.
:- interface.

:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_edt.
:- import_module mdb.declarative_oracle.
:- import_module mdb.declarative_user.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type analyser_response(T)
    --->    analyser_response_no_suspects
            % There are no suspects left, and no incorrect
            % nodes have been found.

    ;       analyser_response_bug_found(decl_bug, decl_evidence(T))
            % A suspect who is guilty, along with the evidence
            % against the suspect.

    ;       analyser_response_oracle_question(decl_question(T))
            % The analyser desires an answer to the question.

    ;       analyser_response_require_explicit_subtree(T)
            % The analyser requires the given implicit sub-tree
            % to be made explicit.

    ;       analyser_response_require_explicit_supertree(T)
            % The analyser requires an explicit tree above the root
            % of an existing explicit tree.

    ;       analyser_response_revise(decl_question(T)).
            % The analyser would like the oracle to re-ask the user
            % this question and then for analysis to continue.

:- type analyser_state(T).

:- type search_mode.

:- func divide_and_query_search_mode = search_mode.

:- func suspicion_divide_and_query_search_mode = search_mode.

:- func top_down_search_mode = search_mode.

:- pred analyser_state_init(analyser_state(T)::out) is det.

    % Resets the state of the analyser.
    %
:- pred reset_analyser(analyser_state(T)::in, analyser_state(T)::out) is det.

    % Make the given search mode the fallback search mode
    % and the current search mode for the analyser.
    %
:- pred set_fallback_search_mode(S::in, search_mode::in,
    analyser_state(T)::in, analyser_state(T)::out)
    is det <= mercury_edt(S, T).

:- type analysis_type(T)
    --->    new_tree(T)
            % Use the given tree to do analysis. The tree will be
            % a new explicitly generated portion of the annotated trace.
            % start_or_resume_analysis should be called with this type
            % of analysis when a new declarative debugging session has been
            % started or a requested subtree or supertree has been generated.

    ;       resume_previous.
            % Continue the previous analysis. This will happen when the user
            % suspends a declarative debugging session with a `pd' or `abort'
            % command and now wants to continue the suspended session.

    % Perform analysis on the given EDT, which may be a new tree
    % to diagnose, or a sub-tree that was required to be made explicit.
    %
:- pred start_or_resume_analysis(S::in, oracle_state::in, analysis_type(T)::in,
    analyser_response(T)::out, analyser_state(T)::in,
    analyser_state(T)::out) is det <= mercury_edt(S, T).

    % Return a response which will cause the last question to be re-asked.
    %
:- pred reask_last_question(S::in, analyser_state(T)::in,
    analyser_response(T)::out) is semidet <= mercury_edt(S, T).

    % Continue analysis after the oracle has responded with an answer.
    %
:- pred continue_analysis(S::in, oracle_state::in, decl_answer(T)::in,
    analyser_response(T)::out, analyser_state(T)::in,
    analyser_state(T)::out) is det <= mercury_edt(S, T).

    % Change the current search mode of the analyser and return the
    % next question using the new search mode.
    %
:- pred change_search_mode(S::in, oracle_state::in, user_search_mode::in,
    analyser_state(T)::in, analyser_state(T)::out,
    analyser_response(T)::out) is det <= mercury_edt(S, T).

    % Revise the current analysis. This is done when a bug determined
    % by the analyser has been overruled by the oracle.
    %
:- pred revise_analysis(S::in, analyser_response(T)::out,
    analyser_state(T)::in, analyser_state(T)::out) is det <= mercury_edt(S, T).

    % Display information about the current question and the state
    % of the search to the supplied output stream.
    %
:- pred show_info(S::in, io.output_stream::in, analyser_state(T)::in,
    io::di, io::uo) is det <= mercury_edt(S, T).

    % Return information within the analyser state that is intended for
    % debugging the declarative debugger itself.
    %
:- pred debug_analyser_state(analyser_state(T)::in,
    maybe(subterm_origin(T))::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.browser_info.
:- import_module mdb.declarative_execution.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.

:- import_module array.
:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module pair.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

    % Describes what search strategy is being used by the analyser and the
    % state of the search.
    %
:- type search_mode
    --->    analyser_top_down
            % Look for the first unknown suspect in a top-down fashion,
            % starting at the root. If no unknown suspects are found, then
            % choose a skipped suspect to requery.

    ;       analyser_follow_subterm_end(
                % Follow the subterm all the way to where it's bound or
                % until it can't be followed any further (for example
                % when there is a call to a module with no tracing),
                % and ask a question about the nearest unknown suspect
                % on the subterm dependency chain. Then proceed to do
                % a binary search between this node and the root of the
                % search space (the binary search will only come into
                % effect if the oracle asserts the suspect is correct
                % or inadmissible).

                suspect_id,
                arg_pos,
                term_path,
                % The above 3 args give the position the sub-term tracking
                % algorithm has got up to if it needs to stop to wait for
                % an explicit sub/super-tree to be generated.

                maybe(suspect_id),
                % The last suspect on the dependency chain whose status was
                % unknown. Initially this is no, but as the sub-term is tracked
                % to where it was initially bound (which could be above or
                % below the node where it was marked incorrect), the most
                % recent node through which the sub-term was tracked that
                % has a status of `unknown' is stored in this field. This is
                % then used as the next question if the node that bound
                % the sub-term is trusted or in an excluded part of the search
                % tree.

                how_track_subterm
                % This field specifies the algorithm to use when tracking
                % the subterm.
            )

    ;       analyser_binary(
                % Perform a binary search on a path in the search space
                % between a suspect and an ancestor of the suspect.
                % The path is represented as an array (the 1st argument)
                % with the deeper suspect at the end of the array and its
                % ancestor at the beginning. The range field gives the
                % inclusive subrange of the array to search. last_tested
                % is the index into the array of the last suspect about which
                % a question was asked.

                suspects    :: array(suspect_id),
                range       :: pair(int, int),
                last_tested :: int
            )

    ;       analyser_divide_and_query(weighting_heuristic).
            % Divide and query using the given weighting heuristic.

divide_and_query_search_mode = analyser_divide_and_query(number_of_events).

suspicion_divide_and_query_search_mode = analyser_divide_and_query(suspicion).

top_down_search_mode = analyser_top_down.

    % Each search algorithm should respond with either a question
    % or a request for an explicit subtree to be generated for a suspect
    % which is the root of an implicit subtree.
    %
:- type search_response
    --->    search_response_question(suspect_id, reason_for_question)
    ;       search_response_require_explicit_subtree(suspect_id)
    ;       search_response_require_explicit_supertree
    ;       search_response_no_suspects
    ;       search_response_found_bug(suspect_id, list(suspect_id),
                list(suspect_id)).

    % The reason the declarative debugger asked a question.
    %
:- type reason_for_question
    --->    ques_reason_start
            % The first question.

    ;       ques_reason_top_down

    ;       ques_reason_binding_node(
                binding_prim_op         :: primitive_op_type,
                binding_node_eliminated :: bool,
                binding_filename        :: string,
                binding_line_no         :: int,

                % The path of the subterm in the binding node,
                % if it appears in the binding node's atom.
                maybe_atom_path         :: maybe(term_path),

                binding_proc            :: proc_label
            )

    ;       ques_reason_subterm_no_proc_rep
            % No proc rep when tracking subterm.

    ;       ques_reason_binding_node_eliminated

    ;       ques_reason_binary(
                binary_reason_bottom    :: int,
                binary_reason_top       :: int,
                binary_reason_split     :: int
            )

    ;       ques_reason_divide_and_query(
                dq_weighting            :: weighting_heuristic,

                dq_old_weight           :: int,
                                        % The weight of the search space before
                                        % the question was asked.

                dq_chosen_subtree_weight :: int
                                        % The weight the searchspace will be
                                        % if the user answers `no' to the
                                        % current question.
            )

    ;       ques_reason_skipped

    ;       ques_reason_revise.

    % The analyser state records all of the information that needs
    % to be remembered across multiple invocations of the analyser.
    %
:- type analyser_state(T)
    --->    analyser(
                % Information about the EDT nodes relevant to the bug search.
                search_space            :: search_space(T),

                % This is set to yes when an explicit tree needs to be
                % generated. The maybe argument says what type of explicit
                % tree needs to be generated.
                require_explicit        :: maybe(explicit_tree_type),

                % The method currently being employed to search
                % the search space for questions for the
                % oracle.
                search_mode             :: search_mode,

                % The search mode to use by default.
                % Only non-parametrized search modes should
                % be used as the fallback search mode.
                fallback_search_mode    :: search_mode,

                % Everytime a search finds a suspect to ask the oracle about,
                % it is put in this field before asking the oracle, so the
                % analyser knows how to modify the search space when it gets
                % an answer.
                last_search_question    :: maybe(suspect_and_reason),

                % This field is present only to make it easier to debug
                % the dependency tracking algorithm; if bound to yes, it
                % records the result of the invocation of that algorithm
                % on the last analysis step.
                debug_origin            :: maybe(subterm_origin(T))
        ).

:- type suspect_and_reason
    --->    suspect_and_reason(suspect_id, reason_for_question).

:- type explicit_tree_type
    --->    explicit_subtree(suspect_id)
            % Generate an explicit subtree for the implicit root
            % referenced by the suspect_id.

    ;       explicit_supertree.
            % Generate a new explicit tree above the current
            % explicit tree.

analyser_state_init(Analyser) :-
    Analyser = analyser(empty_search_space, no,
        analyser_top_down, analyser_top_down, no, no).

reset_analyser(!Analyser) :-
    FallBack = !.Analyser ^ fallback_search_mode,
    !:Analyser = analyser(empty_search_space, no, FallBack, FallBack, no, no).

set_fallback_search_mode(Store, FallBackSearchMode, !Analyser) :-
    !Analyser ^ fallback_search_mode := FallBackSearchMode,
    !Analyser ^ search_mode := FallBackSearchMode,
    !Analyser ^ last_search_question := no,
    (
        FallBackSearchMode = analyser_divide_and_query(Weighting),
        SearchSpace0 = !.Analyser ^ search_space,
        update_weighting_heuristic(Store, Weighting, SearchSpace0,
            SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        FallBackSearchMode = analyser_follow_subterm_end(_, _, _, _, _)
    ;
        FallBackSearchMode = analyser_binary(_, _, _)
    ;
        FallBackSearchMode = analyser_top_down
    ).

start_or_resume_analysis(Store, Oracle, AnalysisType, Response, !Analyser) :-
    (
        AnalysisType = new_tree(Node),
        MaybeRequireExplicit = !.Analyser ^ require_explicit,
        (
            MaybeRequireExplicit = yes(TreeType),
            SearchSpace0 = !.Analyser ^ search_space,
            (
                TreeType = explicit_supertree,
                incorporate_explicit_supertree(Store, Oracle, Node,
                    SearchSpace0, SearchSpace)
            ;
                TreeType = explicit_subtree(SuspectId),
                incorporate_explicit_subtree(SuspectId, Node,
                    SearchSpace0, SearchSpace)
            ),
            !Analyser ^ search_space := SearchSpace,
            !Analyser ^ require_explicit := no,
            decide_analyser_response(Store, Oracle, Response, !Analyser)
        ;
            MaybeRequireExplicit = no,

            % An explicit subtree was not requested, so this is the start
            % of a new declarative debugging session.

            reset_analyser(!Analyser),
            MaybeWeighting = get_maybe_weighting_from_search_mode(
                !.Analyser ^ search_mode),
            initialise_search_space(Store, MaybeWeighting, Node, SearchSpace),
            !Analyser ^ search_space := SearchSpace,
            topmost_det(SearchSpace, TopMostId),
            !Analyser ^ last_search_question :=
                yes(suspect_and_reason(TopMostId, ques_reason_start)),
            edt_question(Store, Node, Question),
            Response = analyser_response_revise(Question)
        )
    ;
        AnalysisType = resume_previous,
        ( if reask_last_question(Store, !.Analyser, Response0) then
            Response = Response0
        else
            decide_analyser_response(Store, Oracle, Response, !Analyser)
        )
    ).

:- func get_maybe_weighting_from_search_mode(search_mode) =
    maybe(weighting_heuristic).

get_maybe_weighting_from_search_mode(SearchMode) = MaybeHeuristic :-
    (
        SearchMode = analyser_divide_and_query(Weighting),
        MaybeHeuristic = yes(Weighting)
    ;
        ( SearchMode = analyser_top_down
        ; SearchMode = analyser_binary(_, _, _)
        ; SearchMode = analyser_follow_subterm_end(_, _, _, _, _)
        ),
        MaybeHeuristic = no
    ).

reask_last_question(Store, Analyser, Response) :-
    MaybeLastQuestion = Analyser ^ last_search_question,
    MaybeLastQuestion = yes(suspect_and_reason(SuspectId, _)),
    SearchSpace = Analyser ^ search_space,
    Node = get_edt_node(SearchSpace, SuspectId),
    edt_question(Store, Node, OracleQuestion),
    Response = analyser_response_oracle_question(OracleQuestion).

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
    decide_analyser_response(Store, Oracle, Response, !Analyser).

change_search_mode(Store, Oracle, UserMode, !Analyser, Response) :-
    (
        UserMode = user_top_down,
        set_fallback_search_mode(Store, analyser_top_down, !Analyser)
    ;
        UserMode = user_divide_and_query,
        set_fallback_search_mode(Store,
            analyser_divide_and_query(number_of_events), !Analyser)
    ;
        UserMode = user_suspicion_divide_and_query,
        set_fallback_search_mode(Store, analyser_divide_and_query(suspicion),
            !Analyser)
    ;
        UserMode = user_binary,
        (
            !.Analyser ^ last_search_question =
                yes(suspect_and_reason(SuspectId, _)),
            setup_binary_search(!.Analyser ^ search_space, SuspectId,
                SearchMode),
            !Analyser ^ search_mode := SearchMode
        ;
            !.Analyser ^ last_search_question = no,
            throw(internal_error("change_search_mode",
                "binary mode requested, but no last question"))
        )
    ),
    decide_analyser_response(Store, Oracle, Response, !Analyser).

:- pred process_answer(S::in, decl_answer(T)::in, suspect_id::in,
    analyser_state(T)::in, analyser_state(T)::out) is det <= mercury_edt(S, T).

process_answer(Store, Answer, SuspectId, !Analyser) :-
    (
        Answer = skip(_),
        skip_suspect(SuspectId, !.Analyser ^ search_space, SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        Answer = ignore(_),
        ignore_suspect(Store, SuspectId, !.Analyser ^ search_space,
            SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        Answer = truth_value(_, truth_correct),
        assert_suspect_is_correct(SuspectId, !.Analyser ^ search_space,
            SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        Answer = truth_value(_, truth_inadmissible),
        assert_suspect_is_inadmissible(SuspectId, !.Analyser ^ search_space,
            SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        Answer = truth_value(_, truth_erroneous),
        assert_suspect_is_erroneous(SuspectId, !.Analyser ^ search_space,
            SearchSpace),
        !Analyser ^ search_space := SearchSpace
    ;
        Answer = suspicious_subterm(Node, ArgPos, TermPath, HowTrack,
            ShouldAssertInvalid),

        % XXX The following 2 lines just done so that debugging info can be
        % printed for tests run when declarative_analyser.m not compiled with
        % tracing (so can't use dd_dd command in mdb). Should be removed when
        % edt_dependency becomes stable enough.

        edt_dependency(Store, Node, ArgPos, TermPath, _, DebugOrigin),
        !Analyser ^ debug_origin := yes(DebugOrigin),
        (
            ShouldAssertInvalid = assert_invalid,
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
            !Analyser ^ search_space := SearchSpace
        ;
            ShouldAssertInvalid = no_assert_invalid
        ),
        !Analyser ^ search_mode :=
            analyser_follow_subterm_end(SuspectId, ArgPos, TermPath, no,
                HowTrack)
    ).

revise_analysis(Store, Response, !Analyser) :-
    SearchSpace = !.Analyser ^ search_space,
    ( if root(SearchSpace, RootId) then
        Node = get_edt_node(!.Analyser ^ search_space, RootId),
        edt_question(Store, Node, Question),
        Response = analyser_response_revise(Question),
        revise_root(Store, SearchSpace, SearchSpace1),
        !Analyser ^ search_space := SearchSpace1,
        !Analyser ^ last_search_question :=
            yes(suspect_and_reason(RootId, ques_reason_revise)),
        !Analyser ^ search_mode :=
            !.Analyser ^ fallback_search_mode
    else
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
        search_for_question(Store, Oracle, !SearchSpace,
            !.Analyser ^ search_mode, !.Analyser ^ fallback_search_mode,
            NewMode, SearchResponse),
        !Analyser ^ search_space := !.SearchSpace,
        !Analyser ^ search_mode := NewMode,
        handle_search_response(Store, SearchResponse, !Analyser, Response)
    ),
    maybe_check_search_space_consistency(Store, !.Analyser ^ search_space,
        "End of decide_analyser_response").

:- pred handle_search_response(S::in, search_response::in,
    analyser_state(T)::in, analyser_state(T)::out,
    analyser_response(T)::out) is det <= mercury_edt(S, T).

handle_search_response(Store, SearchResponse, !Analyser, AnalyserResponse) :-
    (
        SearchResponse = search_response_question(SuspectId, Reason),
        SearchSpace = !.Analyser ^ search_space,
        Node = get_edt_node(SearchSpace, SuspectId),
        edt_question(Store, Node, OracleQuestion),
        ( if
            (
                suspect_unknown(SearchSpace, SuspectId)
            ;
                suspect_skipped(SearchSpace, SuspectId)
            )
        then
            AnalyserResponse =
                analyser_response_oracle_question(OracleQuestion)
        else if
            suspect_ignored(SearchSpace, SuspectId)
        then
            % Searches should not respond with questions about suspects we
            % already know to be trusted.
            throw(internal_error("handle_search_response",
                "search responded with query about ignored suspect"))
        else
            % We already known something about this suspect, but the search
            % wants the oracle to be requeried. This may happen if the
            % search thinks the user might have answered the question
            % incorrectly before.
            AnalyserResponse = analyser_response_revise(OracleQuestion)
        ),
        !Analyser ^ last_search_question :=
            yes(suspect_and_reason(SuspectId, Reason))
    ;
        SearchResponse = search_response_require_explicit_subtree(SuspectId),
        !Analyser ^ require_explicit := yes(explicit_subtree(
            SuspectId)),
        Node = get_edt_node(!.Analyser ^ search_space, SuspectId),
        AnalyserResponse = analyser_response_require_explicit_subtree(Node)
    ;
        SearchResponse = search_response_require_explicit_supertree,
        !Analyser ^ require_explicit := yes(explicit_supertree),
        SearchSpace = !.Analyser ^ search_space,
        topmost_det(SearchSpace, TopMostId),
        TopMost = get_edt_node(SearchSpace, TopMostId),
        AnalyserResponse =
            analyser_response_require_explicit_supertree(TopMost)
    ;
        SearchResponse = search_response_no_suspects,
        AnalyserResponse = analyser_response_no_suspects
    ;
        SearchResponse = search_response_found_bug(BugId, CorrectDescendants,
            InadmissibleChildren),
        bug_response(Store, !.Analyser ^ search_space, BugId,
            [BugId | CorrectDescendants], InadmissibleChildren,
            AnalyserResponse)
    ).

    % bug_response(Store, SearchSpace, BugId, Evidence, InadmissibleChildren,
    %   Response):
    %
    % Create a bug analyser-response using the given Evidence.
    % If InadmissibleChildren isn't empty then an i_bug will be created,
    % otherwise an e_bug will be created.
    %
:- pred bug_response(S::in, search_space(T)::in,
    suspect_id::in, list(suspect_id)::in, list(suspect_id)::in,
    analyser_response(T)::out) is det <= mercury_edt(S, T).

bug_response(Store, SearchSpace, BugId, Evidence, InadmissibleChildren,
        Response) :-
    BugNode = get_edt_node(SearchSpace, BugId),
    (
        InadmissibleChildren = [InadmissibleChild | _],
        edt_get_i_bug(Store, BugNode,
            get_edt_node(SearchSpace, InadmissibleChild), IBug),
        Bug = i_bug(IBug)
    ;
        InadmissibleChildren = [],
        edt_get_e_bug(Store, BugNode, EBug),
        Bug = e_bug(EBug)
    ),
    EDTNodes = list.map(get_edt_node(SearchSpace), Evidence),
    list.map(edt_question(Store), EDTNodes,
        EvidenceAsQuestions),
    Response = analyser_response_bug_found(Bug, EvidenceAsQuestions).

%---------------------------------------------------------------------------%

    % Search the search space for a question for the oracle. The search
    % should respond with a question about a suspect, or a request for an
    % explicit subtree to be generated. A new search mode is returned so
    % that the search algorithm being used can remember its current state
    % next time round.
    %
:- pred search_for_question(S::in, oracle_state::in,
    search_space(T)::in, search_space(T)::out,
    search_mode::in, search_mode::in,
    search_mode::out, search_response::out) is det <= mercury_edt(S, T).

search_for_question(Store, Oracle, !SearchSpace, OldMode, FallBackSearchMode,
        NewMode, Response) :-
    (
        OldMode = analyser_top_down,
        top_down_search(Store, Oracle, !SearchSpace, Response),
        % We always go back to the fallback search mode after a top-down
        % search, because some fallback searches (such as divide and query)
        % use top-down as a fail safe and we want the fallback search to
        % resume after the top-down search.
        NewMode = FallBackSearchMode
    ;
        OldMode = analyser_follow_subterm_end(SuspectId, ArgPos, TermPath,
            LastUnknown, HowTrack),
        follow_subterm_end_search(Store, Oracle, !SearchSpace, HowTrack,
            LastUnknown, SuspectId, ArgPos, TermPath, FallBackSearchMode,
            NewMode, Response)
    ;
        OldMode = analyser_binary(PathArray, Top - Bottom, LastTested),
        binary_search(Store, Oracle, PathArray, Top, Bottom, LastTested,
            !SearchSpace, FallBackSearchMode, NewMode, Response)
    ;
        OldMode = analyser_divide_and_query(Weighting),
        divide_and_query_search(Store, Oracle, Weighting, !SearchSpace,
            Response, NewMode)
    ).

:- pred top_down_search(S::in, oracle_state::in,
    search_space(T)::in, search_space(T)::out,
    search_response::out) is det <= mercury_edt(S, T).

top_down_search(Store, Oracle, !SearchSpace, Response) :-
    % If there's no root yet (because the oracle hasn't asserted any nodes
    % are erroneous yet) then use the topmost suspect as a starting point.

    ( if root(!.SearchSpace, RootId) then
        Start = RootId
    else
        topmost_det(!.SearchSpace, Start)
    ),
    first_unknown_descendant(Store, Oracle, Start, !SearchSpace,
        MaybeUnknownDescendant),
    (
        MaybeUnknownDescendant = found(Unknown),
        Response = search_response_question(Unknown, ques_reason_top_down)
    ;
        MaybeUnknownDescendant = not_found,
        ( if
            choose_skipped_suspect(!.SearchSpace, SkippedSuspect)
        then
            Response = search_response_question(SkippedSuspect,
                ques_reason_skipped)
        else if
            % Since the are no skipped suspects and no unknown suspects
            % in the search space, if there is a root (i.e. an erroneous
            % suspect), then it must be a bug. Note that only top down search
            % actually checks if a bug was found. This is okay, since all the
            % other search algorithms call top down search if they can't find
            % an unknown suspect.
            root(!.SearchSpace, BugId)
        then
            ( if
                children(Store, Oracle, BugId, !SearchSpace, BugChildren),
                non_ignored_descendants(Store, Oracle, BugChildren,
                    !SearchSpace, NonIgnoredDescendants),
                list.filter(suspect_correct_or_inadmissible(!.SearchSpace),
                    NonIgnoredDescendants, CorrectDescendants, [])
            then
                list.filter(suspect_inadmissible(!.SearchSpace), BugChildren,
                    InadmissibleChildren),
                Response = search_response_found_bug(BugId, CorrectDescendants,
                    InadmissibleChildren)
            else
                throw(internal_error("top_down_search",
                    "bug has unexplored or unknown children"))
            )
        else
            % Try to extend the search space upwards. If this fails
            % and we're not at the topmost traced node, then request that
            % an explicit supertree be generated.
            ( if
                extend_search_space_upwards(Store, Oracle, !.SearchSpace,
                    ExtendedSearchSpace)
            then
                top_down_search(Store, Oracle, ExtendedSearchSpace,
                    !:SearchSpace, Response)
            else
                topmost_det(!.SearchSpace, TopMostId),
                TopMostNode = get_edt_node(!.SearchSpace, TopMostId),
                ( if edt_topmost_node(Store, TopMostNode) then
                    % We can't look any higher.
                    Response = search_response_no_suspects
                else
                    Response = search_response_require_explicit_supertree
                )
            )
        )
    ;
        MaybeUnknownDescendant = require_explicit_subtree(RequireExplicitId),
        Response = search_response_require_explicit_subtree(RequireExplicitId)
    ).

:- pred follow_subterm_end_search(S::in, oracle_state::in,
    search_space(T)::in, search_space(T)::out, how_track_subterm::in,
    maybe(suspect_id)::in, suspect_id::in,
    arg_pos::in, term_path::in, search_mode::in, search_mode::out,
    search_response::out) is det <= mercury_edt(S, T).

follow_subterm_end_search(Store, Oracle, !SearchSpace, HowTrack,
        LastUnknown, SuspectId, ArgPos, TermPath, FallBackSearchMode,
        NewMode, SearchResponse) :-
    follow_subterm_end_search_2(Store, Oracle, !SearchSpace, HowTrack,
        map.init, _, LastUnknown, SuspectId, ArgPos, TermPath,
        FallBackSearchMode, NewMode, SearchResponse).

:- pred follow_subterm_end_search_2(S::in, oracle_state::in,
    search_space(T)::in, search_space(T)::out, how_track_subterm::in,
    map(proc_layout, unit)::in, map(proc_layout, unit)::out,
    maybe(suspect_id)::in, suspect_id::in,
    arg_pos::in, term_path::in, search_mode::in, search_mode::out,
    search_response::out) is det <= mercury_edt(S, T).

follow_subterm_end_search_2(Store, Oracle, !SearchSpace, HowTrack,
        !TriedShortcutProcs, LastUnknown, SuspectId, ArgPos, TermPath,
        FallBackSearchMode, NewMode, SearchResponse) :-
    find_subterm_origin(Store, Oracle, SuspectId, ArgPos, TermPath, HowTrack,
        !TriedShortcutProcs, !SearchSpace, FindOriginResponse),
    (
        FindOriginResponse = primitive_op(BindingSuspectId, FileName,
            LineNo, PrimOpType, Output),
        ProcLabel = get_proc_label_for_suspect(Store, !.SearchSpace,
            BindingSuspectId),
        (
            Output = yes,
            % BindingSuspectId = SuspectId since the subterm is an output
            % of SuspectId.
            BindingNode = get_edt_node(!.SearchSpace, SuspectId),
            ArgNum = edt_arg_pos_to_user_arg_num(Store, BindingNode, ArgPos),
            MaybePath = yes([ArgNum | TermPath])
        ;
            Output = no,
            % Since the subterm is not an output of the binding node,
            % it will not appear in any of the arguments of the binding node
            % (it can't be an input, because then it would have been bound
            % outside the node).
            MaybePath = no
        ),
        ( if
            % We ask about the binding node even if it was previously skipped,
            % since this behaviour is more predictable from the user's
            % perspective.

            ( suspect_unknown(!.SearchSpace, BindingSuspectId)
            ; suspect_skipped(!.SearchSpace, BindingSuspectId)
            )
        then
            SearchResponse = search_response_question(BindingSuspectId,
                ques_reason_binding_node(PrimOpType, no, FileName, LineNo,
                    MaybePath, ProcLabel)),
            NewMode = FallBackSearchMode
        else
            ( if
                LastUnknown = yes(Unknown),
                suspect_still_unknown(!.SearchSpace, Unknown)
            then
                Reason = ques_reason_binding_node(PrimOpType, yes,
                    FileName, LineNo, MaybePath, ProcLabel),
                SearchResponse = search_response_question(Unknown, Reason),
                NewMode = FallBackSearchMode
            else
                search_for_question(Store, Oracle, !SearchSpace,
                    FallBackSearchMode, FallBackSearchMode, NewMode,
                    SearchResponse)
            )
        )
    ;
        FindOriginResponse = not_found,
        ( if
            LastUnknown = yes(Unknown),
            suspect_still_unknown(!.SearchSpace, Unknown)
        then
            SearchResponse = search_response_question(Unknown,
                ques_reason_subterm_no_proc_rep),
            NewMode = FallBackSearchMode
        else
            search_for_question(Store, Oracle, !SearchSpace,
                FallBackSearchMode, FallBackSearchMode,
                NewMode, SearchResponse)
        )
    ;
        FindOriginResponse = require_explicit_subtree,
        SearchResponse = search_response_require_explicit_subtree(SuspectId),

        % Record the current position of the search so we can continue
        % where we left off once the explicit subtree has been generated.
        NewMode = analyser_follow_subterm_end(SuspectId, ArgPos, TermPath,
            LastUnknown, HowTrack)
    ;
        FindOriginResponse = require_explicit_supertree,
        SearchResponse = search_response_require_explicit_supertree,
        NewMode = analyser_follow_subterm_end(SuspectId, ArgPos, TermPath,
            LastUnknown, HowTrack)
    ;
        FindOriginResponse = origin(OriginId, OriginArgPos,
            OriginTermPath, SubtermMode),
        ( if
            suspect_unknown(!.SearchSpace, OriginId)
        then
            NewLastUnknown = yes(OriginId)
        else
            NewLastUnknown = LastUnknown
        ),
        ( if
            % Check if it's worth continuing tracking the sub-term.
            % We want to stop if we enter a portion of the search space
            % known not to contain the bug from which we can't return
            % (for example if we come across an erroneous node where
            % the sub-term is an input).

            give_up_subterm_tracking(!.SearchSpace, OriginId, SubtermMode)
        then
            ( if
                LastUnknown = yes(Unknown),
                suspect_still_unknown(!.SearchSpace, Unknown)
            then
                SearchResponse = search_response_question(Unknown,
                    ques_reason_binding_node_eliminated),
                NewMode = FallBackSearchMode
            else
                search_for_question(Store, Oracle, !SearchSpace,
                    FallBackSearchMode, FallBackSearchMode,
                    NewMode, SearchResponse)
            )
        else
            % This recursive call will not lead to an infinite loop
            % because eventually either the sub-term will be bound
            % (and find_subterm_origin will respond with primitive_op/3)
            % or there will be insufficient tracing information to continue
            % (and find_subterm_origin will respond with not_found).

            follow_subterm_end_search_2(Store, Oracle, !SearchSpace, HowTrack,
                !TriedShortcutProcs, NewLastUnknown, OriginId,
                OriginArgPos, OriginTermPath,
                FallBackSearchMode, NewMode, SearchResponse)
        )
    ).

    % setup_binary_search(SearchSpace, SuspectId, SearchMode):
    %
    % Sets up the search mode to do a binary search between SuspectId
    % and either the root of the search space if a suspect has previously been
    % marked erroneous, or the topmost node if no suspect has yet been marked
    % erroneous.
    %
:- pred setup_binary_search(search_space(T)::in, suspect_id::in,
    search_mode::out) is det.

setup_binary_search(SearchSpace, SuspectId, SearchMode) :-
    ( if root(SearchSpace, RootId) then
        TopId = RootId,
        BottomId = SuspectId
    else
        topmost_det(SearchSpace, TopId),
        BottomId = SuspectId
    ),
    ( if get_path(SearchSpace, BottomId, TopId, Path) then
        PathArray = array.from_list(Path),
        array.bounds(PathArray, Top, Bottom),
        SearchMode = analyser_binary(PathArray, Top - Bottom, Bottom)
    else
        throw(internal_error("setup_binary_search",
            "TopId not an ancestor of BottomId"))
    ).

:- pred binary_search(S::in, oracle_state::in,
    array(suspect_id)::in, int::in, int::in, int::in,
    search_space(T)::in, search_space(T)::out, search_mode::in,
    search_mode::out, search_response::out) is det <= mercury_edt(S, T).

binary_search(Store, Oracle, PathArray, Top, Bottom, LastTested,
        !SearchSpace, FallBackSearchMode, NewMode, Response) :-
    SuspectId = PathArray ^ elem(LastTested),

    % Check what the result of the query about LastTested was and adjust
    % the range appropriately.
    ( if
        % The oracle answered `erroneous'.
        suspect_in_excluded_complement(!.SearchSpace, SuspectId)
    then
        NewTop = LastTested + 1,
        NewBottom = Bottom
    else if
        % The oracle answered `correct' or `inadmissible'
        suspect_in_excluded_subtree(!.SearchSpace, SuspectId)
    then
        NewTop = Top,
        NewBottom = LastTested - 1
    else
        % The suspect is trusted(ignored) or was skipped.
        NewTop = Top,
        NewBottom = Bottom
    ),
    ( if
        NewTop > NewBottom
    then
        % Revert to the fallback search mode when binary search is over.
        search_for_question(Store, Oracle, !SearchSpace,
            FallBackSearchMode, FallBackSearchMode, NewMode, Response)
    else
        ( if
            find_unknown_closest_to_middle(!.SearchSpace, PathArray,
                NewTop, NewBottom, UnknownClosestToMiddle)
        then
            NewMode = analyser_binary(PathArray, NewTop - NewBottom,
                UnknownClosestToMiddle),
            Response = search_response_question(
                PathArray ^ elem(UnknownClosestToMiddle),
                ques_reason_binary(NewBottom, NewTop, UnknownClosestToMiddle))
        else
            % No unknown suspects on the path, so revert to the fallback
            % search mode.
            search_for_question(Store, Oracle, !SearchSpace,
                FallBackSearchMode, FallBackSearchMode, NewMode, Response)
        )
    ).

    % find_unknown_closest_to_middle(SearchSpace, PathArray, Top, Bottom,
    %   Unknown):
    %
    % Unknown is the position in PathArray of the suspect which has status
    % unknown and is closest to halfway between From and To which are
    % also indexes into PathArray. Fails if there are no unknown suspects
    % between From and To (inclusive).
    %
:- pred find_unknown_closest_to_middle(search_space(T)::in,
    array(suspect_id)::in, int::in, int::in, int::out) is semidet.

find_unknown_closest_to_middle(SearchSpace, PathArray, Top, Bottom, Unknown) :-
    Middle = Top + ((Bottom - Top) // 2),
    find_unknown_closest_to_range(SearchSpace, PathArray, Top, Bottom,
        Middle, Middle, Unknown).

    % find_unknown_closest_to_range(SearchSpace, PathArray, OuterTop,
    %   OuterBottom, InnerTop, InnerBottom, Unknown):
    %
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
    ( if
        OuterTop =< InnerTop,
        suspect_unknown(SearchSpace, PathArray ^ elem(InnerTop))
    then
        Unknown = InnerTop
    else if
        InnerBottom =< OuterBottom,
        suspect_unknown(SearchSpace, PathArray ^ elem(InnerBottom))
    then
        Unknown = InnerBottom
    else
        find_unknown_closest_to_range(SearchSpace, PathArray,
            OuterTop, OuterBottom, InnerTop - 1, InnerBottom + 1, Unknown)
    ).

:- pred divide_and_query_search(S::in, oracle_state::in,
    weighting_heuristic::in, search_space(T)::in, search_space(T)::out,
    search_response::out, search_mode::out) is det <= mercury_edt(S, T).

divide_and_query_search(Store, Oracle, Weighting, !SearchSpace,
        Response, analyser_divide_and_query(Weighting)) :-
    % If there's no root yet (because the oracle hasn't asserted any nodes
    % are erroneous yet), then use top-down search.
    ( if root(!.SearchSpace, RootId) then
        ( if children(Store, Oracle, RootId, !SearchSpace, Children) then
            find_middle_weight(Store, Oracle, Weighting, Children,
                RootId, no, !SearchSpace, Response)
        else
            Response = search_response_require_explicit_subtree(RootId)
        )
    else
        top_down_search(Store, Oracle, !SearchSpace, Response)
    ).

    % Call find_middle_weight if we are able to find the children of the
    % given suspect id, otherwise return a require_explicit_subtree
    % search response in the last argument.
    %
:- pred find_middle_weight_if_children(S::in,
    oracle_state::in, weighting_heuristic::in, suspect_id::in,
    suspect_id::in, maybe(suspect_id)::in,
    search_space(T)::in, search_space(T)::out, search_response::out)
    is det <= mercury_edt(S, T).

find_middle_weight_if_children(Store, Oracle, Weighting, SuspectId, TopId,
        MaybeLastUnknown, !SearchSpace, Response) :-
    ( if children(Store, Oracle, SuspectId, !SearchSpace, Children) then
        find_middle_weight(Store, Oracle, Weighting, Children, TopId,
            MaybeLastUnknown, !SearchSpace, Response)
    else
        Response = search_response_require_explicit_subtree(SuspectId)
    ).

    % find_middle_weight(Store, Oracle, Weighting, SuspectIds,
    %   TopId, MaybeLastUnknown, !SearchSpace, Response):
    %
    % Find the unknown suspect whose weight is closest to half the weight
    % of TopId, considering only the heaviest suspect in SuspectIds, the
    % heaviest child of the heaviest suspect in SuspectIds and so on.
    % MaybeLastUnknown is the last node that was unknown in the search
    % (if any).
    %
:- pred find_middle_weight(S::in, oracle_state::in,
    weighting_heuristic::in, list(suspect_id)::in, suspect_id::in,
    maybe(suspect_id)::in, search_space(T)::in, search_space(T)::out,
    search_response::out) is det <= mercury_edt(S, T).

find_middle_weight(Store, Oracle, Weighting, [], TopId,
        MaybeLastUnknown, !SearchSpace, Response) :-
    ( if
        MaybeLastUnknown = yes(LastUnknown),
        suspect_still_unknown(!.SearchSpace, LastUnknown)
    then
        Response = search_response_question(LastUnknown,
            ques_reason_divide_and_query(Weighting,
                get_weight(!.SearchSpace, TopId),
                get_weight(!.SearchSpace, LastUnknown)))
    else
        % This could happen when there were no unknown suspects encountered
        % during the search, in which case we revert to top-down search.
        top_down_search(Store, Oracle, !SearchSpace, Response)
    ).
find_middle_weight(Store, Oracle, Weighting, [SuspectId | SuspectIds], TopId,
        MaybeLastUnknown, !SearchSpace, Response) :-
    TopWeight = get_weight(!.SearchSpace, TopId),
    Target = TopWeight // 2,

    % Find the heaviest suspect.
    Weight = get_weight(!.SearchSpace, SuspectId),
    list.foldl2(max_weight(!.SearchSpace), SuspectIds,
        Weight, MaxWeight, SuspectId, Heaviest),
    ( if MaxWeight > Target then
        ( if suspect_unknown(!.SearchSpace, Heaviest) then
            NewMaybeLastUnknown = yes(Heaviest)
        else
            NewMaybeLastUnknown = MaybeLastUnknown
        ),
        find_middle_weight_if_children(Store, Oracle, Weighting, Heaviest,
            TopId, NewMaybeLastUnknown, !SearchSpace, Response)
    else
        ( if suspect_unknown(!.SearchSpace, Heaviest) then
            ( if
                MaybeLastUnknown = yes(LastUnknown),
                suspect_still_unknown(!.SearchSpace, LastUnknown)
            then
                LastUnknownWeight = get_weight(!.SearchSpace, LastUnknown),

                % If the last unknown suspect was closer to the target weight,
                % then ask about it.
                ( if LastUnknownWeight - Target < Target - MaxWeight then
                    Response = search_response_question(LastUnknown,
                        ques_reason_divide_and_query(Weighting, TopWeight,
                            LastUnknownWeight))
                else
                    Response = search_response_question(Heaviest,
                        ques_reason_divide_and_query(Weighting, TopWeight,
                            MaxWeight))
                )
            else
                Response = search_response_question(Heaviest,
                    ques_reason_divide_and_query(Weighting, TopWeight,
                        MaxWeight))
            )
        else
            ( if
                MaybeLastUnknown = yes(LastUnknown),
                suspect_still_unknown(!.SearchSpace, LastUnknown)
            then
                LastUnknownWeight = get_weight(!.SearchSpace, LastUnknown),
                Response = search_response_question(LastUnknown,
                    ques_reason_divide_and_query(Weighting, TopWeight,
                        LastUnknownWeight))
            else
                % Look deeper until we find an unknown.
                find_middle_weight_if_children(Store, Oracle, Weighting,
                    Heaviest, TopId, no, !SearchSpace, Response)
            )
        )
    ).

:- pred max_weight(search_space(T)::in, suspect_id::in,
    int::in, int::out, suspect_id::in, suspect_id::out) is det.

max_weight(SearchSpace, SuspectId, PrevMax, NewMax,
        PrevSuspectId, NewSuspectId) :-
    Weight = get_weight(SearchSpace, SuspectId),
    ( if Weight > PrevMax then
        NewMax = Weight,
        NewSuspectId = SuspectId
    else
        NewMax = PrevMax,
        NewSuspectId = PrevSuspectId
    ).

%---------------------------------------------------------------------------%

    % Check that a suspect is still unknown. This is called by the search
    % algorithms to make double sure that a suspect is still unknown (it
    % might not be unknown if, for example, an erroneous suspect was added
    % to the search space during the search).
    %
:- pred suspect_still_unknown(search_space(T)::in, suspect_id::in) is semidet.

suspect_still_unknown(SearchSpace, SuspectId) :-
    suspect_unknown(SearchSpace, SuspectId).

%---------------------------------------------------------------------------%

:- func reason_to_string(reason_for_question) = string.

reason_to_string(Reason) = Str :-
    (
        Reason = ques_reason_start,
        Str = "this is the node where the `dd' command was issued."
    ;
        Reason = ques_reason_binding_node(PrimOpType, Eliminated,
            FileName, LineNo, MaybePath, ProcLabel),
        PrimOpStr = primitive_op_type_to_string(PrimOpType),
        LineNoStr = int_to_string(LineNo),
        get_pred_attributes(ProcLabel, SymModule, Name, Arity, PredOrFunc),
        (
            PredOrFunc = pf_function,
            PredOrFuncStr = "function"
        ;
            PredOrFunc = pf_predicate,
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
            PathSent = "The path to the subterm in the atom is " ++ PathStr ++ "."
        ;
            MaybePath = no,
            PathSent = ""
        ),
        Str = "the marked subterm was bound by the " ++
            PrimOpStr ++ " inside the " ++ PredOrFuncStr ++
            " " ++ Module ++ "." ++ Name ++ "/" ++ ArityStr ++
            " (" ++ FileName ++ ":" ++ LineNoStr ++ "). " ++
            PathSent ++ EliminatedSent
    ;
        Reason = ques_reason_top_down,
        Str = "this is the next node in the top-down search."
    ;
        Reason = ques_reason_subterm_no_proc_rep,
        Str = "tracking of the marked subterm had to be aborted here, " ++
            "because of missing tracing information."
    ;
        Reason = ques_reason_binding_node_eliminated,
        Str = "tracking of the marked subterm was stopped here, " ++
            "because the binding node lies in a portion of the tree " ++
            "which has been eliminated."
    ;
        Reason = ques_reason_binary(Bottom, Top, Split),
        PathLengthStr = int_to_string_thousands(Bottom - Top + 1),
        SubPath1LengthStr = int_to_string_thousands(Bottom - Split),
        SubPath2LengthStr = int_to_string_thousands(Split - Top + 1),
        Str = "this node divides a path of length " ++ PathLengthStr
            ++ " into two paths of length " ++
            SubPath1LengthStr ++ " and " ++ SubPath2LengthStr ++ "."
    ;
        Reason = ques_reason_divide_and_query(Weighting, OldWeight,
            SubtreeWeight),
        Str = weighting_to_reason_string(Weighting, OldWeight - SubtreeWeight,
            SubtreeWeight)
    ;
        Reason = ques_reason_skipped,
        Str = "there are no more non-skipped questions left."
    ;
        Reason = ques_reason_revise,
        Str = "this question is being revisited, because of " ++
            "an unsuccessful previous bug search."
    ).

:- func weighting_to_reason_string(weighting_heuristic, int, int) = string.

weighting_to_reason_string(number_of_events, Weight1, Weight2) = Str :-
    Weight1Str = int_to_string_thousands(Weight1),
    Weight2Str = int_to_string_thousands(Weight2),
    Str = "this node divides the suspect area into two regions of "
        ++ Weight1Str ++ " and " ++ Weight2Str ++ " events each.".
weighting_to_reason_string(suspicion, Weight1, Weight2) = Str :-
    Weight1Str = int_to_string_thousands(Weight1),
    Weight2Str = int_to_string_thousands(Weight2),
    Str = "this node divides the suspect area into " ++
        "two regions of suspicion " ++ Weight1Str ++ " and
        " ++ Weight2Str ++ ".".

show_info(Store, OutStream, Analyser, !IO) :-
    SearchSpace = Analyser ^ search_space,
    some [!FieldNames, !Data] (
        !:FieldNames = [],
        !:Data = [],

        % Get the context of the current question.
        (
            Analyser ^ last_search_question =
                yes(suspect_and_reason(LastId, Reason)),
            ( if
                edt_context(Store, get_edt_node(SearchSpace,
                    LastId), FileName -  LineNo, MaybeReturnContext)
            then
                (
                    MaybeReturnContext = yes(ReturnFileName - ReturnLineNo),
                    ContextStr = FileName ++ ":" ++ int_to_string(LineNo)
                        ++ " (" ++ ReturnFileName ++ ":"
                        ++ int_to_string(ReturnLineNo) ++ ")"
                ;
                    MaybeReturnContext = no,
                    ContextStr = FileName ++ ":" ++ int_to_string(LineNo)
                ),
                list.append(!.FieldNames, ["Context of current question"],
                    !:FieldNames),
                list.append(!.Data, [ContextStr], !:Data)
            else
                true
            )
        ;
            Analyser ^ last_search_question = no,
            throw(internal_error("show_info", "no last question"))
        ),

        !:FieldNames = !.FieldNames ++ ["Search mode"],
        !:Data = !.Data ++ [search_mode_to_string(Analyser ^ search_mode)],

        MaybeWeighting = get_current_maybe_weighting(SearchSpace),
        ( if MaybeWeighting = yes(number_of_events) then
            ( if root(SearchSpace, RootId) then
                StartId = RootId
            else
                topmost_det(SearchSpace, StartId)
            ),
                Weight = get_weight(SearchSpace, StartId),
            ( if
                Analyser ^ search_mode =
                    analyser_divide_and_query(number_of_events)
            then
                !:FieldNames = !.FieldNames ++
                    ["Estimated questions remaining"],
                EstimatedQuestions = float.ceiling_to_int(
                    math.log2(float(Weight))),
                !:Data = !.Data ++ [int_to_string(EstimatedQuestions)]
            else
                true
            ),
            !:FieldNames = !.FieldNames ++ ["Number of suspect events"],
            !:Data = !.Data ++ [int_to_string_thousands(Weight)]
        else
            true
        ),

        InfoMessage = string.format_table([left(!.FieldNames), left(!.Data)],
            " : ")
    ),
    ReasonStr = reason_to_string(Reason),
    ReasonSent = "The current question was chosen because " ++ ReasonStr,
    WrappedReason = string.word_wrap(ReasonSent, 72),
    io.format(OutStream, "%s\n%s\n", [s(InfoMessage), s(WrappedReason)], !IO).

:- func search_mode_to_string(search_mode) = string.

search_mode_to_string(SearchMode) = Str :-
    (
        SearchMode = analyser_top_down,
        Str = "top down"
    ;
        SearchMode = analyser_follow_subterm_end(_, _, _, _, track_accurate),
        Str = "tracking marked sub-term (using accurate algorithm)"
    ;
        SearchMode = analyser_follow_subterm_end(_, _, _, _, track_fast),
        Str = "tracking marked sub-term (using fast algorithm)"
    ;
        SearchMode = analyser_binary(_, _, _),
        Str = "binary search on path"
    ;
        SearchMode = analyser_divide_and_query(number_of_events),
        Str = "divide and query"
    ;
        SearchMode = analyser_divide_and_query(suspicion),
        Str = "suspicion divide and query"
    ).

%---------------------------------------------------------------------------%

debug_analyser_state(Analyser, Analyser ^ debug_origin).

%---------------------------------------------------------------------------%
