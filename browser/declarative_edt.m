%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2011 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: declarative_edt.m
% Authors: Ian MacLarty, Mark Brown
%
% This module defines Evaluation Dependency Trees (EDTs), which represent the
% dependencies between calls made during the execution of a buggy program.
% Search spaces are also defined as a layer on top of EDTs.
%
% The search space records extra information like which nodes have been
% eliminated from the bug search, which nodes have been skipped or are
% trusted, the weight of each node, and in future might also store information
% like the probability of each node being buggy, based on some heuristic(s).
%
% The search space provides a consistent view of the debug tree - combining
% separately generated EDT subtrees into one tree. Each node in the search
% space corresponds to a node either explicitly represented in a generated EDT
% subtree or the root of an implicitly represented EDT subtree. We maintain
% the invariant that search space nodes always correspond with explicit nodes
% where an explicit version of the EDT subtree containing that node exists, and
% only correspond to implicit roots when an explicit version of the EDT subtree
% rooted at the node has not yet been generated.
%
% Every node in the EDT may not have a corresponding node in the search space:
% nodes are only added to the search space when they are needed by a
% particular search algorithm.
%
% By convention, nodes in the search space are referred to as `suspects', while
% nodes in the EDT are referred to as `EDT nodes', or just `nodes'.
%
% Also we use the term "root" to refer to the root of the smallest subtree in
% the search space that must contain a bug based on the answers received so
% far, and the term "topmost" for the suspect in the search space with the
% lowest depth.
%
% Each suspect in the search space can be assigned a weight to be used for
% divide and query search. Any heuristic can be used, as long as the combined
% weight of all the children of a suspect does not exceed the suspect's own
% weight. Sometimes, the weight may depend on the weights of unmaterialized
% portions of the EDT, resulting in the situation where the combined weight of
% the children of a suspect exceeds the parent's weight. If this happens then
% an "excess weight" may be specified along with the normal weight which will
% be added to all the ancestor's of the overweight suspect. For example if the
% number of events in descendant suspects is being used as a weight, then for a
% FAIL node some events may be repeated in siblings of the FAIL node. In this
% case the duplicate events might not have been included in the ancestor's
% weights, so should be added.
%
% When debugging, a search space consistency check can be turned on by
% compiling with the C macro MR_DD_CHECK_SEARCH_SPACE defined (i.e. by
% putting "EXTRA_CFLAGS=-DMR_DD_CHECK_SEARCH_SPACE" in Mmake.browser.params).
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.declarative_edt.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_oracle.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module unit.

%---------------------------------------------------------------------------%

    % This typeclass defines how EDTs may be accessed by this module.
    % An EDT is a tree of nodes, each of which contains a question about
    % the truth of an assertion. The children of a node may not be immediately
    % accessible if the sub-tree beneath that node is represented implicitly.
    % In this case, the analyser must request that it be made explicit
    % before continuing.
    %
    % The first argument is intuitively a "store", which maps references
    % to the things they reference. The second argument is the type of trees
    % themselves. By convention, we use the names S and T for type variables
    % which are constrained by mercury_edt.
    %
    % By convention, we also use the names S and T in type declarations
    % where it is *intended* that the type variables be constrained by
    % mercury_edt.
    %
    % (Compare with the similar conventions for annotated_trace/2.)
    %
:- typeclass mercury_edt(S, T) where [

        % Return the question corresponding to the given node.
        %
    pred edt_question(S::in, T::in, decl_question(T)::out) is det,

        % If this node is an E-bug, then return the bug.
        % An E-bug is an erroneous node whose children are all correct.
        %
    pred edt_get_e_bug(S::in, T::in, decl_e_bug::out) is det,

        % If this node is an I-bug, then return the bug.
        % An I-bug is an erroneous node whose children are all correct
        % or inadmissible, with at least one child being inadmissible.
        %
    pred edt_get_i_bug(S::in, T::in, T::in, decl_i_bug::out) is det,

        % Return the list of children of the given tree node. If the tree
        % is represented implicitly, then the procedure fails.
        %
    pred edt_children(S::in, T::in, list(T)::out) is semidet,

        % Return a parent of an EDT node. Using the annotated trace to
        % generate the EDT there may be more than one parent of a given node
        % (see the comment above trace_last_parent/3 in declarative_tree.m).
        % This member is required to deterministically pick one if this is
        % the case. Fails if the node is the root of the initial explicit
        % portion of the EDT, or the root of a portion of the EDT generated
        % as an explicit supertree.
        %
    pred edt_parent(S::in, T::in, T::out) is semidet,

        % Given a subterm of a tree, find the mode of that subterm
        % and the origin of it amongst a parent, siblings or children.
        %
    pred edt_dependency(S::in, T::in, arg_pos::in, term_path::in,
        subterm_mode::out, subterm_origin(T)::out) is det,

        % Just find the mode of the subterm.
        %
    pred edt_subterm_mode(S::in, T::in, arg_pos::in, term_path::in,
        subterm_mode::out) is det,

        % Succeed if the Node is the root of an implicit subtree.
        % Otherwise, fail.
        %
    pred edt_is_implicit_root(S::in, T::in) is semidet,

        % Succeed if the two nodes are the same even if one is represented
        % implicitly and the other explicitly.
        %
    pred edt_same_nodes(S::in, T::in, T::in) is semidet,

        % Succeed if it is not possible to materialize any nodes
        % above the given node.
        %
    pred edt_topmost_node(S::in, T::in) is semidet,

        % edt_number_of_events(Store, Node, Events, DuplicateEvents):
        %
        % Find the number of events in the subtree rooted at Node,
        % including the CALL and final event of Node. Return in DuplicateEvents
        % the number of events which will be repeated in siblings to the
        % right of Node, times by the number of repetitions of each event.
        %
    pred edt_number_of_events(S::in, T::in, int::out, int::out) is det,

        % edt_subtree_suspicion(Store, Node, Suspicion, Excess):
        %
        % Find the suspicion of the subtree rooted at Node.
        % Return any suspicion duplicated in siblings of Node in Excess.
        %
    pred edt_subtree_suspicion(S::in, T::in, int::out, int::out) is det,

        % Return the filename and line number of the predicate associated
        % with the given node. Also return the parent context if available.
        % Sometimes the main context may not be available (for example
        % exception nodes); in this case, fail.
        %
    pred edt_context(S::in, T::in, pair(string, int)::out,
        maybe(pair(string, int))::out) is semidet,

        % Return the proc label for the given node.
        %
    func edt_proc_label(S, T) = proc_label,

        % Convert an arg_pos to a user arg number using the atom
        % of the given node.
        %
    func edt_arg_pos_to_user_arg_num(S, T, arg_pos) = int
].

:- type subterm_mode
    --->    subterm_in
    ;       subterm_out.

:- type subterm_origin(T)
    --->    origin_output(T, arg_pos, term_path)
            % Subterm came from an output of a child or sibling call.
            % The first argument records the child or sibling edt node.
            % The second and third arguments state which part of which argument
            % is the origin.

    ;       origin_input(arg_pos, term_path)
            % Subterm came from an input of a parent. The arguments identify
            % which part of which argument of the clause head is the origin.

    ;       origin_primitive_op(string, int, primitive_op_type)
            % Subterm was constructed in the body. We record the filename,
            % line number and type of the primitive operation that
            % constructed it.

    ;       origin_not_found
            % The origin could not be found due to missing information.

    ;       origin_require_explicit_subtree.
            % An explicit subtree is required.

    % The type of primitive operation that bound a subterm that was being
    % tracked.
    %
:- type primitive_op_type
    --->    primop_foreign_proc
    ;       primop_builtin_call
    ;       primop_untraced_call
    ;       primop_unification.

:- func primitive_op_type_to_string(primitive_op_type) = string.

%---------------------------------------------------------------------------%

    % This type defines a search space in which the declarative debugger
    % can look for bugs. The search space keeps track of which nodes in
    % the EDT could contain a bug as well as skipped or ignored nodes.
    % Each suspect in the search space has an identifier (suspect_id)
    % that is independent of the EDT node id and independent of whether the
    % EDT node is represented implicitly or explicitly.
    %
    % Information about each node that is relevant to the bug search is
    % stored in the search space. For example information about the status
    % of the node like whether it was skipped, ignored or marked erroneous,
    % correct or inadmissible and the depth of each node in the EDT is
    % stored here. In future information like the probability that a node
    % contains a bug could also be stored here.
    %
:- type search_space(T).

    % suspect_id is used to lookup suspects in the search space. Each
    % suspect in the search space will have a unique suspect_id. When an
    % explicit subtree is generated, a new EDT node is generated for the
    % root of the explicit subtree, replacing the EDT node that represented
    % the subtree implicitly. However the suspect_id will remain unchanged.
    % Any search algorithms that needs to keep track of EDT nodes can use
    % suspect_ids for this purpose and not have to worry about updating these
    % when an explicit subtree is generated.
    %
:- type suspect_id.

    % Returns a search space with no suspects.
    %
:- func empty_search_space = search_space(T).

    % Creates a new search space containing just the one EDT node with
    % an initial status of unknown.
    %
:- pred initialise_search_space(S::in, maybe(weighting_heuristic)::in, T::in,
    search_space(T)::out) is det <= mercury_edt(S, T).

    % The root of the search space is the root of the subtree of the EDT
    % that we think contains a bug, based on information received so far.
    % Normally the root will be marked erroneous, but it could also be
    % marked unknown, skipped or ignored (for example when the search has
    % just started and the oracle has not asserted any suspects are erroneous
    % or when a bug search is being revised. This pred returns the root,
    % or fails if the search space is empty.
    %
:- pred root(search_space(T)::in, suspect_id::out) is semidet.

    % Return the topmost suspect in the search space and throw an exception
    % if the search space is empty.
    %
:- pred topmost_det(search_space(T)::in, suspect_id::out) is det.

    % parent(SearchSpace, SuspectId, ParentId):
    %
    % Succeeds if ParentId is the Id of the parent of SuspectId in SearchSpace
    % and fails if SuspectId has no parent in SearchSpace.
    %
:- pred parent(search_space(T)::in, suspect_id::in, suspect_id::out)
    is semidet.

    % children(Store, Oracle, SuspectId, !SearchSpace, Children):
    %
    % Children is the list of children of SuspectId in the SearchSpace.
    % If the children were not in the search space then they are added.
    % Fails if SuspectId is the root of an implicit subtree.
    %
:- pred children(S::in, oracle_state::in, suspect_id::in,
    search_space(T)::in, search_space(T)::out, list(suspect_id)::out)
    is semidet <= mercury_edt(S, T).

    % non_ignored_descendants(Store, Oracle, SuspectIds, !SearchSpace,
    %   Descendants):
    %
    % Descendants is the non-ignored children of the suspects in SuspectIds
    % appended together. If a child is ignored then its non-ignored children
    % are added to the list. This is done recursively. Fails if an explicit
    % subtree is required to find the children of an ignored suspect.
    %
:- pred non_ignored_descendants(S::in, oracle_state::in, list(suspect_id)::in,
    search_space(T)::in, search_space(T)::out, list(suspect_id)::out)
    is semidet <= mercury_edt(S, T).

%---------------------%

    % Return the proc_label for the given suspect.
    %
:- func get_proc_label_for_suspect(S, search_space(T), suspect_id) = proc_label
    <= mercury_edt(S, T).

    % Return the EDT node corresponding to the suspect_id.
    %
:- func get_edt_node(search_space(T), suspect_id) = T.

    % Return the weight of the suspect.
    %
:- func get_weight(search_space(T), suspect_id) = int.

    % Returns the depth of the suspect in the EDT.
    %
:- func suspect_depth(search_space(T), suspect_id) = int.

    % Succeeds if the suspect's status is unknown.
    %
:- pred suspect_unknown(search_space(T)::in, suspect_id::in) is semidet.

    % Succeeds if the suspect has been marked correct or inadmissible.
    %
:- pred suspect_correct_or_inadmissible(search_space(T)::in, suspect_id::in)
    is semidet.

    % Succeeds if the suspect has been marked inadmissible.
    %
:- pred suspect_inadmissible(search_space(T)::in, suspect_id::in) is semidet.

    % Succeeds if the suspect's status is erroneous.
    %
:- pred suspect_erroneous(search_space(T)::in, suspect_id::in) is semidet.

    % Succeeds if the suspect's status is skipped.
    %
:- pred suspect_skipped(search_space(T)::in, suspect_id::in) is semidet.

    % Succeeds if the suspect's status is ignored.
    %
:- pred suspect_ignored(search_space(T)::in, suspect_id::in) is semidet.

    % Succeeds if the suspect has been marked correct or inadmissible
    % or is the descendant of a suspect that was marked correct or
    % inadmissible. Fails otherwise.
    %
:- pred suspect_in_excluded_subtree(search_space(T)::in, suspect_id::in)
    is semidet.

    % Succeeds if the suspect has been marked erroneous or is in the
    % complement of a subtree with an erroneous root. Fails otherwise.
    %
:- pred suspect_in_excluded_complement(search_space(T)::in, suspect_id::in)
    is semidet.

%---------------------%

    % travel_up(SearchSpace, SuspectId, N, AncestorId):
    %
    % True iff AncestorId is the Nth ancestor of SuspectId in SearchSpace.
    %
:- pred travel_up(search_space(_)::in, suspect_id::in, int::in,
    suspect_id::out) is det.

    % get_path(SearchSpace, BottomId, TopId, Path):
    %
    % Path is InitialPath appended to the list of suspect_id's between
    % FromId and ToId (inclusive). ToId should be an ancestor of FromId.
    % If it isn't, then the call will fail.
    %
:- pred get_path(search_space(T)::in, suspect_id::in, suspect_id::in,
    list(suspect_id)::out) is semidet.

%---------------------%

    % Marks the suspect correct and all its descendants as pruned.
    %
:- pred assert_suspect_is_correct(suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

    % Marks the suspect erroneous and marks the complement of the subtree
    % rooted at the erroneous suspect as in_erroneous_subtree_complement.
    %
:- pred assert_suspect_is_erroneous(suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

    % Marks the suspect as inadmissible and all its descendants as pruned.
    %
:- pred assert_suspect_is_inadmissible(suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

    % Marks the suspect as ignored.
    %
:- pred ignore_suspect(S::in, suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

    % Marks the suspect as skipped.
    %
:- pred skip_suspect(suspect_id::in, search_space(T)::in, search_space(T)::out)
    is det.

    % When tracking a sub-term, should we give up if we reach the given
    % suspect, because the binding node must lie in a portion of the tree
    % we have already eliminated?
    %
:- pred give_up_subterm_tracking(search_space(T)::in, suspect_id::in,
    subterm_mode::in) is semidet.

    % find_subterm_origin(Store, Oracle, SuspectId, ArgPos, TermPath, HowTrack,
    %   !TriedShortcutProcs, !SearchSpace, Response):
    %
    % Finds the origin of the subterm given by SuspectId, ArgPos and TermPath
    % in its immediate neighbours. If the children of a suspect are required
    % then they will be added to the search space, unless an explicit subtree
    % is required in which case the appropriate response is returned
    % (see definition of find_origin_response type below).
    %
    % find_subterm_origin can use heuristics to avoid materialising subtrees
    % unnecessarily. If the subterm is being tracked through an output
    % argument, and there is an input argument with the same name as the
    % output argument, except for a numerical suffix, then find_subterm_origin
    % will check if the subterm appears in the same position in the input
    % argument. If it does then it will continue tracking the subterm in the
    % input argument, thus bypassing the subtree rooted at the call and so
    % possibly avoiding materialising that subtree. Since dereferencing
    % a subterm in a large structure can be expensive, find_subterm_origin
    % will only try to bypass calls to procedures it has not tried to bypass
    % before. The HowTrack argument specifies whether to use the bypassing
    % heuristics and !TriedShortcutProcs keeps track of which procedures' calls
    % it has already tried to bypass.
    %
:- pred find_subterm_origin(S::in, oracle_state::in,
    suspect_id::in, arg_pos::in, term_path::in, how_track_subterm::in,
    map(proc_layout, unit)::in, map(proc_layout, unit)::out,
    search_space(T)::in, search_space(T)::out, find_origin_response::out)
    is det <= mercury_edt(S, T).

:- type find_origin_response
    --->    not_found
            % The origin could not be found because of insufficient
            % tracing information.

    ;       origin(suspect_id, arg_pos, term_path, subterm_mode)
            % The subterm originated from the suspect referenced by argument 1.
            % The second and third arguments give the position of the subterm
            % in the origin node, while the fourth argument gives the mode
            % of the subterm.

    ;       primitive_op(
                % The subterm was bound by a primitive operation inside
                % the suspect. The other arguments are the filename,
                % line number and type of the primitive operation that
                % bound the subterm.

                suspect_id,         % The node in which the subterm was bound.
                string,             % File name of primitive op.
                int,                % Line number of primitive op.
                primitive_op_type,  % Type of primitive operation.
                bool                % Whether the subterm appears
                                    % as an output of the binding node.
            )

    ;       require_explicit_subtree
            % The suspect is the root of an implicit subtree and
            % the origin lies in one of its children.

    ;       require_explicit_supertree.
            % The suspect is the root of the topmost explicit EDT
            % and the origin lies in an ancestor. A new supertree
            % needs to be generated.

    % incorporate_explicit_subtree(SuspectId, Node, !SearchSpace).
    %
    % Replaces the EDT node referenced by SuspectId with Node.
    %
:- pred incorporate_explicit_subtree(suspect_id::in, T::in,
    search_space(T)::in, search_space(T)::out) is det.

    % incorporate_explicit_supertree(Store, Oracle, Node, !SearchSpace):
    %
    % Node should be the implicit root in a newly generated supertree
    % that represents the topmost node of the current search space.
    % Node's parent will be inserted at the top of the search space.
    %
:- pred incorporate_explicit_supertree(S::in, oracle_state::in, T::in,
    search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

    % extend_search_space_upwards(Store, Oracle, !SearchSpace):
    %
    % Attempts to add a parent of the current topmost node to the search space.
    % Fails if this is not possible because an explicit supertree is required.
    %
:- pred extend_search_space_upwards(S::in, oracle_state::in,
    search_space(T)::in, search_space(T)::out) is semidet <= mercury_edt(S, T).

%---------------------%

    % first_unknown_descendant(Store, Oracle, SuspectId,
    %   !SearchSpace, MaybeDescendant):
    %
    % Search the search space for a suspect with status = unknown in a top down
    % fashion, starting with SuspectId. If no unknown suspect is found then
    % MaybeDescendant will be no. If there are no unknown suspects in the
    % explicit part of the search space and a skipped, ignored or erroneous
    % suspect is the root of an implicit subtree, then the call will fail.
    %
:- pred first_unknown_descendant(S::in, oracle_state::in,
    suspect_id::in, search_space(T)::in, search_space(T)::out,
    maybe_found_descendant::out) is det <= mercury_edt(S, T).

:- type maybe_found_descendant
    --->    found(suspect_id)
    ;       not_found
    ;       require_explicit_subtree(suspect_id).

    % choose_skipped_suspect(SearchSpace, Skipped) is true iff Skipped is the
    % skipped suspect in SearchSpace with the lowest skip order (i.e. it was
    % skipped the longest time ago). Fails if there are no skipped suspects
    % in SearchSpace.
    %
:- pred choose_skipped_suspect(search_space(T)::in, suspect_id::out)
    is semidet.

    % Mark the root and its non-ignored children as unknown.
    % Throws an exception if the search space does not have a root.
    %
:- pred revise_root(S::in, search_space(T)::in, search_space(T)::out)
    is det <= mercury_edt(S, T).

%---------------------%

:- type weighting_heuristic
    --->    number_of_events
    ;       suspicion.

    % Recalculate the value of the `weight' fields for all suspects in the
    % search space if the given weighting heuristic is different from the
    % previous one.
    %
:- pred update_weighting_heuristic(S::in, weighting_heuristic::in,
    search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

    % Return the meaning of the values of the `weight' fields of suspects
    % in the search space.
    %
:- func get_current_maybe_weighting(search_space(T)) =
    maybe(weighting_heuristic).

%---------------------%

    % Check the consistency of the search space if the MR_DD_CHECK_SEARCH_SPACE
    % C macro is defined. Throw an exception if it is not consistent.
    % Used for assertion checking during debugging.
    %
:- pred maybe_check_search_space_consistency(S::in, search_space(T)::in,
    string::in) is det <= mercury_edt(S, T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_execution.
:- import_module mdb.term_rep.

:- import_module bimap.
:- import_module counter.
:- import_module exception.
:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

primitive_op_type_to_string(primop_foreign_proc) = "foreign procedure call".
primitive_op_type_to_string(primop_builtin_call) = "builtin operation".
primitive_op_type_to_string(primop_untraced_call) = "untraced call".
primitive_op_type_to_string(primop_unification) = "unification".

%---------------------------------------------------------------------------%

    % A suspect is an edt node with some additional information relevant
    % to the bug search.
    %
:- type suspect(T)
    --->    suspect(
                % The suspect's parent id in the search space.
                % Is set to `no' if the suspect is at the root of the
                % search space.
                parent          :: maybe(suspect_id),

                % The EDT node.
                edt_node        :: T,

                % What is the status of this node with respect to
                % the bug search.
                status          :: suspect_status,

                % The depth of the suspect in the EDT. Initially the depth
                % of the topmost node will be zero, however if a new explicit
                % supertree is generated and added to the search space,
                % we allow the depth of the new topmost node to be negative.
                depth           :: int,

                % The children of the suspect. If this is no, then
                % the children have not yet been explored. Children are only
                % added to the search space when they are required.
                children        :: maybe(list(suspect_id)),

                % A weighting used for divide and query search.
                weight          :: int
            ).

:- type suspect_status
    --->    suspect_ignored
    ;       suspect_skipped(int) % We record the order nodes were skipped in.
    ;       suspect_correct
    ;       suspect_erroneous
    ;       suspect_inadmissible

    ;       suspect_pruned
            % The suspect is in a subtree with a correct or inadmissible root.

    ;       suspect_in_erroneous_subtree_complement
            % The suspect was in the complement of a subtree with
            % an erroneous root.

    ;       suspect_unknown.

:- type suspect_id == int.

:- type search_space(T)
    --->    search_space(
                % The root of the subtree in the search space that contains
                % a bug, based on the answers received so far. The search space
                % root will be the last suspect marked erroneous, or no if
                % no suspects have been marked erroneous yet.
                root                        :: maybe(suspect_id),

                % The topmost node of all the nodes in the search space.
                % Will be no if the search space is empty.
                topmost                     :: maybe(suspect_id),

                % Counter for generating suspect_ids.
                suspect_id_counter          :: counter,

                % So we can keep the skipped nodes in some kind of order
                % to avoid asking about the same skipped node twice in a row.
                skip_counter                :: counter,

                % The collection of suspects in the search space.
                store                       :: map(suspect_id, suspect(T)),

                % A map of roots of implicit subtrees in the EDT to explicit
                % subtrees. We use a bimap so we can also find the implicit
                % root given an explicit root.
                implicit_to_explicit_roots  :: bimap(T, T),

                % The weighting heuristic (if any) used to calculate
                % the weights of suspects.
                maybe_weighting_heuristic   :: maybe(weighting_heuristic)
    ).

empty_search_space =
    search_space(no, no, counter.init(0), counter.init(0),
        map.init, bimap.init, no).

initialise_search_space(Store, MaybeWeighting, Node, SearchSpace) :-
    (
        MaybeWeighting = yes(Weighting),
        calc_weight(Weighting, Store, Node, Weight, _)
    ;
        MaybeWeighting = no,
        Weight = 0
    ),
    Suspect = suspect(no, Node, suspect_unknown, 0, no, Weight),
    SuspectStore = map.singleton(0, Suspect),
    SearchSpace = search_space(no, yes(0), counter.init(1),
        counter.init(0), SuspectStore, bimap.init, MaybeWeighting).

root(SearchSpace, RootId) :-
    SearchSpace ^ root = yes(RootId).

topmost_det(SearchSpace, TopMostId) :-
    (
        SearchSpace ^ topmost = yes(Id),
        TopMostId = Id
    ;
        SearchSpace ^ topmost = no,
        throw(internal_error("topmost_det", "search space empty"))
    ).

parent(SearchSpace, SuspectId, ParentId) :-
    lookup_suspect(SearchSpace, SuspectId, Parent),
    Parent ^ parent = yes(ParentId).

children(Store, Oracle, SuspectId, !SearchSpace, Children) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    (
        Suspect ^ children = yes(Children)
    ;
        Suspect ^ children = no,
        edt_children(Store, Suspect ^ edt_node, EDTChildren),
        NewStatus = new_child_status(Suspect ^ status),
        add_children(Store, Oracle, EDTChildren, SuspectId, NewStatus,
            !SearchSpace, Children)
    ).

non_ignored_descendants(_, _, [], !SearchSpace, []).
non_ignored_descendants(Store, Oracle, [SuspectId | SuspectIds],
        !SearchSpace, Descendants) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    ( if Suspect ^ status = suspect_ignored then
        children(Store, Oracle, SuspectId, !SearchSpace, Children),
        non_ignored_descendants(Store, Oracle, Children, !SearchSpace,
            Descendants1)
    else
        Descendants1 = [SuspectId]
    ),
    non_ignored_descendants(Store, Oracle, SuspectIds, !SearchSpace,
        Descendants2),
    Descendants = Descendants1 ++ Descendants2.

%---------------------------------------------------------------------------%

    % Return the status that should be assigned to children of a suspect
    % with the given status, when the children are being added to the
    % search space.
    %
:- func new_child_status(suspect_status) = suspect_status.

new_child_status(suspect_ignored) = suspect_unknown.
new_child_status(suspect_skipped(_)) = suspect_unknown.
new_child_status(suspect_correct) = suspect_pruned.
new_child_status(suspect_erroneous) = suspect_unknown.
new_child_status(suspect_inadmissible) = suspect_pruned.
new_child_status(suspect_pruned) = suspect_pruned.
new_child_status(suspect_in_erroneous_subtree_complement) =
    suspect_in_erroneous_subtree_complement.
new_child_status(suspect_unknown) = suspect_unknown.

    % Return the status that should be assigned to the parent of a suspect
    % with the given status, when the parent is being added to the search
    % space.
    %
:- func new_parent_status(suspect_status) = suspect_status.

new_parent_status(suspect_ignored) = suspect_unknown.
new_parent_status(suspect_skipped(_)) = suspect_unknown.
new_parent_status(suspect_correct) = suspect_unknown.
new_parent_status(suspect_erroneous) = suspect_in_erroneous_subtree_complement.
new_parent_status(suspect_inadmissible) = suspect_unknown.
new_parent_status(suspect_pruned) = suspect_pruned.
new_parent_status(suspect_in_erroneous_subtree_complement) =
    suspect_in_erroneous_subtree_complement.
new_parent_status(suspect_unknown) = suspect_unknown.

%---------------------------------------------------------------------------%

get_proc_label_for_suspect(Store, SearchSpace, SuspectId) =
    edt_proc_label(Store, get_edt_node(SearchSpace, SuspectId)).

get_edt_node(SearchSpace, SuspectId) = Node :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Node = Suspect ^ edt_node.

get_weight(SearchSpace, SuspectId) = Weight :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Weight = Suspect ^ weight.

    % Return the status of the suspect.
    %
:- func get_status(search_space(T), suspect_id) = suspect_status.

get_status(SearchSpace, SuspectId) = Status :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Status = Suspect ^ status.

suspect_depth(SearchSpace, SuspectId) = Suspect ^ depth :-
    lookup_suspect(SearchSpace, SuspectId, Suspect).

suspect_unknown(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Suspect ^ status = suspect_unknown.

suspect_correct_or_inadmissible(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Status = Suspect ^ status,
    ( Status = suspect_correct
    ; Status = suspect_inadmissible
    ).

suspect_inadmissible(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Suspect ^ status = suspect_inadmissible.

suspect_erroneous(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Suspect ^ status = suspect_erroneous.

suspect_skipped(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Suspect ^ status = suspect_skipped(_).

suspect_ignored(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Suspect ^ status = suspect_ignored.

suspect_in_excluded_subtree(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    excluded_subtree(Suspect ^ status, yes).

    % Does the given status mean the suspect is in a subtree that was
    % excluded from the bug search (because it was marked correct or
    % inadmissible or is the descendant of such a suspect)?
    %
:- pred excluded_subtree(suspect_status::in, bool::out) is det.

excluded_subtree(suspect_ignored, no).
excluded_subtree(suspect_skipped(_), no).
excluded_subtree(suspect_correct, yes).
excluded_subtree(suspect_erroneous, no).
excluded_subtree(suspect_inadmissible, yes).
excluded_subtree(suspect_pruned, yes).
excluded_subtree(suspect_in_erroneous_subtree_complement, no).
excluded_subtree(suspect_unknown, no).

suspect_in_excluded_complement(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    excluded_complement(Suspect ^ status, yes).

    % Does the given status mean the suspect is in the complement of
    % a subtree whose root was marked erroneous or is erroneous itself.
    %
:- pred excluded_complement(suspect_status::in, bool::out) is det.

excluded_complement(suspect_ignored, no).
excluded_complement(suspect_skipped(_), no).
excluded_complement(suspect_correct, no).
excluded_complement(suspect_erroneous, yes).
excluded_complement(suspect_inadmissible, no).
excluded_complement(suspect_pruned, no).
excluded_complement(suspect_in_erroneous_subtree_complement, yes).
excluded_complement(suspect_unknown, no).

%---------------------------------------------------------------------------%

travel_up(SearchSpace, StartId, Distance, FinishId) :-
    ( if
        Distance > 0,
        lookup_suspect(SearchSpace, StartId, Suspect),
        Suspect ^ parent = yes(ParentId)
    then
        travel_up(SearchSpace, ParentId, Distance - 1, FinishId)
    else
        FinishId = StartId
    ).

get_path(SearchSpace, BottomId, TopId, Path) :-
    get_path(SearchSpace, BottomId, TopId, [], Path).

    % get_path(SearchSpace, BottomId, TopId, PathSoFar, Path):
    %
    % Path = append(RemainingPath, PathSoFar) where RemainingPath is the
    % path in the search space between TopId and BottomId, starting at TopId
    % and ending at BottomId (inclusive). Fails if TopId is not an ancestor
    % of BottomId.
    %
:- pred get_path(search_space(T)::in, suspect_id::in, suspect_id::in,
    list(suspect_id)::in, list(suspect_id)::out) is semidet.

get_path(SearchSpace, BottomId, TopId, PathSoFar, Path) :-
    ( if BottomId = TopId then
        Path = [TopId | PathSoFar]
    else
        lookup_suspect(SearchSpace, BottomId, Bottom),
        Bottom ^ parent = yes(ParentId),
        get_path(SearchSpace, ParentId, TopId, [BottomId | PathSoFar], Path)
    ).

%---------------------------------------------------------------------------%

assert_suspect_is_correct(SuspectId, !SearchSpace) :-
    assert_suspect_is_valid(suspect_correct, SuspectId, !SearchSpace).

    % Mark the suspect as correct or inadmissible.
    %
:- pred assert_suspect_is_valid(suspect_status::in, suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

assert_suspect_is_valid(Status, SuspectId, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    set_suspect(SuspectId, (Suspect ^ status := Status) ^ weight := 0,
        !SearchSpace),
    (
        Suspect ^ children = yes(Children),
        list.foldl(propagate_status_downwards(suspect_pruned,
            [suspect_correct, suspect_inadmissible]), Children, !SearchSpace)
    ;
        Suspect ^ children = no
    ),
    % Remove the suspect's weight from its ancestors, since its weight is
    % now zero.
    add_weight_to_ancestors(SuspectId, - Suspect ^ weight, !SearchSpace),

    % If the suspect was erroneous or excluded because of another erroneous
    % suspect, then we should update the complement of the subtree rooted
    % at the suspect to unknown.
    ( if excluded_complement(Suspect ^ status, yes) then
        propagate_status_upwards(suspect_unknown, [suspect_erroneous],
            SuspectId, Lowest, !SearchSpace),

        % Update the root to the next lowest erroneous suspect.
        ( if suspect_erroneous(!.SearchSpace, Lowest) then
            !SearchSpace ^ root := yes(Lowest)
        else
            !SearchSpace ^ root := no
        )
    else
        true
    ).

assert_suspect_is_erroneous(SuspectId, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    set_suspect(SuspectId, Suspect ^ status := suspect_erroneous,
        !SearchSpace),
    propagate_status_upwards(suspect_in_erroneous_subtree_complement,
        [suspect_erroneous, suspect_correct, suspect_inadmissible],
        SuspectId, _, !SearchSpace),
    !SearchSpace ^ root := yes(SuspectId).

assert_suspect_is_inadmissible(SuspectId, !SearchSpace) :-
    assert_suspect_is_valid(suspect_inadmissible, SuspectId, !SearchSpace).

ignore_suspect(Store, SuspectId, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children,
        suspect_ignored, !.SearchSpace, Weight, _),
    set_suspect(SuspectId,
        (Suspect ^ status := suspect_ignored) ^ weight := Weight,
        !SearchSpace),
    add_weight_to_ancestors(SuspectId, Weight - Suspect ^ weight,
        !SearchSpace).

skip_suspect(SuspectId, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    counter.allocate(N, !.SearchSpace ^ skip_counter, SkipCounter),
    !SearchSpace ^ skip_counter := SkipCounter,
    set_suspect(SuspectId, Suspect ^ status := suspect_skipped(N),
        !SearchSpace).

    % lookup_subterm_node(Store, SuspectId, ArgPos, TermPath, SearchSpace,
    %   Suspect, Mode, Node):
    %
    % Finds the node of the subterm given by SuspectId, ArgPos and TermPath
    % in its immediate neighbours.
    %
:- pred lookup_subterm_node(S::in, suspect_id::in, arg_pos::in, term_path::in,
    search_space(T)::in, suspect(T)::out, subterm_mode::out, T::out)
    is det <= mercury_edt(S, T).

lookup_subterm_node(Store, SuspectId, ArgPos, TermPath, SearchSpace, Suspect,
        Mode, Node) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    % The node in the search space will be the explicit version.
    ExplicitNode = Suspect ^ edt_node,
    edt_subterm_mode(Store, ExplicitNode, ArgPos, TermPath, Mode),

    % If the mode is input then the origin will be in a parent or a
    % sibling. In either case we need access to a parent EDT node, so
    % if the node is at the top of a generated explicit subtree we must use
    % the implicit root instead, so the dependency tracking algorithm
    % has access to the node's parent and siblings in the EDT.
    ( if
        Mode = subterm_in,
        ImplicitToExplicit = SearchSpace ^ implicit_to_explicit_roots,
        bimap.search(ImplicitToExplicit, ImplicitNode, ExplicitNode)
    then
        Node = ImplicitNode
    else
        Node = ExplicitNode
    ).

give_up_subterm_tracking(SearchSpace, SuspectId, subterm_in) :-
    Status = get_status(SearchSpace, SuspectId),
    excluded_complement(Status, yes).

find_subterm_origin(Store, Oracle, SuspectId, ArgPos, TermPath, HowTrack,
        !TriedShortcutProcs, !SearchSpace, Response) :-
    lookup_subterm_node(Store, SuspectId, ArgPos, TermPath,
        !.SearchSpace, Suspect, Mode, Node),
    (
        Mode = subterm_in,
        (
            Suspect ^ parent = yes(ParentId),
            resolve_origin(Store, Oracle, Node, ArgPos, TermPath,
                ParentId, no, !SearchSpace, Response)
        ;
            Suspect ^ parent = no,
            ( if extend_search_space_upwards(Store, Oracle, !SearchSpace) then
                topmost_det(!.SearchSpace, NewRootId),
                resolve_origin(Store, Oracle, Node, ArgPos, TermPath,
                    NewRootId, no, !SearchSpace, Response)
            else
                Response = require_explicit_supertree
            )
        )
    ;
        Mode = subterm_out,
        (
            HowTrack = track_accurate,
            resolve_origin(Store, Oracle, Node, ArgPos, TermPath, SuspectId,
                yes, !SearchSpace, Response)
        ;
            HowTrack = track_fast,
            subterm_is_in_input_with_same_prefix(Store, Node, ArgPos, TermPath,
                !TriedShortcutProcs, MaybeInputArgPos),
            (
                MaybeInputArgPos = yes(InputArgPos),
                Response = origin(SuspectId, InputArgPos, TermPath, subterm_in)
            ;
                MaybeInputArgPos = no,
                resolve_origin(Store, Oracle, Node, ArgPos, TermPath,
                    SuspectId, yes, !SearchSpace, Response)
            )
        )
    ).

    % resolve_origin(Store, Oracle, Node, ArgPos, TermPath,
    %   SuspectId, Output, !SearchSpace, Response):
    %
    % Find the origin of the subterm in Node and report the origin as SuspectId
    % if the origin is a primitive op or an input and as the appropriate child
    % of SuspectId if the origin is an output. SuspectId should point to
    % a parent of Node if the mode of the sub-term is input and should point to
    % Node itself if the mode of the sub-term is output. Output should be yes
    % if the sub-term is an output of SuspectId and no if it is not.
    %
:- pred resolve_origin(S::in, oracle_state::in, T::in,
    arg_pos::in, term_path::in, suspect_id::in, bool::in,
    search_space(T)::in, search_space(T)::out,
    find_origin_response::out) is det <= mercury_edt(S, T).

resolve_origin(Store, Oracle, Node, ArgPos, TermPath, SuspectId,
        Output, !SearchSpace, Response) :-
    edt_dependency(Store, Node, ArgPos, TermPath, _, Origin),
    (
        Origin = origin_primitive_op(FileName, LineNo, PrimOpType),
        Response = primitive_op(SuspectId, FileName, LineNo,
            PrimOpType, Output)
    ;
        Origin = origin_not_found,
        Response = not_found
    ;
        Origin = origin_input(InputArgPos, InputTermPath),
        Response = origin(SuspectId, InputArgPos, InputTermPath, subterm_in)
    ;
        Origin = origin_output(OriginNode, OutputArgPos, OutputTermPath),
        ( if
            bimap.search(!.SearchSpace ^ implicit_to_explicit_roots,
                OriginNode, ExplicitNode)
        then
            ExplicitOrigin = ExplicitNode
        else
            ExplicitOrigin = OriginNode
        ),
        ( if
            children(Store, Oracle, SuspectId, !SearchSpace, Children)
        then
            ( if
                find_edt_node_in_suspect_list(Children, ExplicitOrigin,
                    !.SearchSpace, OriginId)
            then
                Response = origin(OriginId, OutputArgPos, OutputTermPath,
                    subterm_out)
            else
                throw(internal_error("resolve_origin",
                    "output origin for input subterm not in siblings"))
            )
        else
            Response = require_explicit_subtree
        )
    ;
        Origin = origin_require_explicit_subtree,
        Response = require_explicit_subtree
    ).

:- pred subterm_is_in_input_with_same_prefix(S::in, T::in, arg_pos::in,
    term_path::in, map(proc_layout, unit)::in, map(proc_layout, unit)::out,
    maybe(arg_pos)::out) is det <= mercury_edt(S, T).

subterm_is_in_input_with_same_prefix(Store, Node, OutputArgPos, TermPath,
        !TriedProcs, MaybeInitialVersionArgPos) :-
    edt_question(Store, Node, Question),
    ( if
        Question = wrong_answer(_, _, FinalDeclAtom),
        FinalDeclAtom = final_decl_atom(FinalAtom, _),
        FinalAtom = atom(ProcLayout, FinalArgs),
        not map.search(!.TriedProcs, ProcLayout, _)
    then
        map.det_insert(ProcLayout, unit, !TriedProcs),
        ( if
            absolute_arg_num(OutputArgPos, FinalAtom, OutputArgNum),
            select_arg_at_pos(OutputArgPos, FinalArgs, OutputArg),
            OutputArg = arg_info(_, _, yes(OutputTermRep)),
            find_initial_version_arg_num(ProcLayout, OutputArgNum,
                InitialVersionArgNum),
            deref_path(OutputTermRep, TermPath, OutputSubtermRep),
            InitialVersionArgPos = any_head_var(InitialVersionArgNum),
            select_arg_at_pos(InitialVersionArgPos, FinalArgs,
                InitialVersionArg),
            InitialVersionArg = arg_info(_, _, yes(InitialVersionTermRep)),
            deref_path(InitialVersionTermRep, TermPath,
                InitialVersionSubtermRep),
            InitialVersionSubtermRep = OutputSubtermRep
        then
            MaybeInitialVersionArgPos = yes(InitialVersionArgPos)
        else
            MaybeInitialVersionArgPos = no
        )
    else
        MaybeInitialVersionArgPos = no
    ).

    % Returns the suspect id in the given list that refers to the given edt
    % node or fails if it cannot find such a suspect in the list.
    %
:- pred find_edt_node_in_suspect_list(list(suspect_id)::in, T::in,
    search_space(T)::in, suspect_id::out) is semidet.

find_edt_node_in_suspect_list([SuspectId | SuspectIds], Node, SearchSpace,
        FoundId) :-
    ( if
        map.search(SearchSpace ^ store, SuspectId, Suspect),
        Node = Suspect ^ edt_node
    then
        FoundId = SuspectId
    else
        find_edt_node_in_suspect_list(SuspectIds, Node, SearchSpace, FoundId)
    ).

    % Looks up the suspect in the search space and throws an exception if
    % it cannot find the suspect.
    %
:- pred lookup_suspect(search_space(T)::in, suspect_id::in, suspect(T)::out)
    is det.

lookup_suspect(SearchSpace, SuspectId, Suspect) :-
    ( if map.search(SearchSpace ^ store, SuspectId, FoundSuspect) then
        Suspect = FoundSuspect
    else
        throw(internal_error("lookup_suspect", "could not find suspect"))
    ).

    % Update or add the suspect in the search space.
    %
:- pred set_suspect(suspect_id::in, suspect(T)::in, search_space(T)::in,
    search_space(T)::out) is det.

set_suspect(SuspectId, Suspect, !SearchSpace) :-
    Store0 = !.SearchSpace ^ store,
    map.set(SuspectId, Suspect, Store0, Store),
    !SearchSpace ^ store := Store.

    % propagate_status_downwards(Status, StopStatusSet, SuspectId,
    %   StopSuspects, !SearchSpace):
    %
    % Sets the status of SuspectId and all its descendants to Status.
    % If a descendant (including the suspect) already has a status in
    % StopStatusSet then propagate_status_downwards will not update any
    % further descendants. The list of all the children of the lowest
    % updated suspects is returned in StopSuspects.
    %
:- pred propagate_status_downwards(suspect_status::in,
    list(suspect_status)::in, suspect_id::in, list(suspect_id)::out,
    search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, StopSuspects,
    !SearchSpace) :-
    propagate_status_downwards(Status, StopStatusSet, SuspectId, [],
        StopSuspects, !SearchSpace).

    % A version of propagate_status_downwards which does not return leaves.
    %
:- pred propagate_status_downwards(suspect_status::in,
    list(suspect_status)::in, suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, !SearchSpace) :-
    propagate_status_downwards(Status, StopStatusSet, SuspectId, _,
        !SearchSpace).

    % An accumulator version of propagate_status_downwards.
    %
:- pred propagate_status_downwards(suspect_status::in,
    list(suspect_status)::in, suspect_id::in,
    list(suspect_id)::in, list(suspect_id)::out,
    search_space(T)::in, search_space(T)::out) is det.

propagate_status_downwards(Status, StopStatusSet, SuspectId, !StopSuspects,
        !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    ( if list.member(Suspect ^ status, StopStatusSet) then
        list.cons(SuspectId, !StopSuspects)
    else
        set_suspect(SuspectId, Suspect ^ status := Status,
            !SearchSpace),
        (
            Suspect ^ children = yes(Children),
            list.foldl2(propagate_status_downwards(Status, StopStatusSet),
                Children, !StopSuspects, !SearchSpace)
        ;
            Suspect ^ children = no
        )
    ).

    % force_propagate_status_downwards is like propagate_status_downwards,
    % except that the given suspect's status will be changed no matter what
    % its current status.
    %
:- pred force_propagate_status_downwards(suspect_status::in,
    list(suspect_status)::in, suspect_id::in,
    search_space(T)::in, search_space(T)::out) is det.

force_propagate_status_downwards(Status, StopStatusSet, SuspectId,
        !SearchSpace) :-
    force_propagate_status_downwards(Status, StopStatusSet, SuspectId, _,
        !SearchSpace).

:- pred force_propagate_status_downwards(suspect_status::in,
    list(suspect_status)::in, suspect_id::in, list(suspect_id)::out,
    search_space(T)::in, search_space(T)::out) is det.

force_propagate_status_downwards(Status, StopStatusSet, SuspectId,
        StopSuspects, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    set_suspect(SuspectId, Suspect ^ status := Status, !SearchSpace),
    (
        Suspect ^ children = yes(Children),
        list.foldl2(propagate_status_downwards(Status, StopStatusSet),
            Children, [], StopSuspects, !SearchSpace)
    ;
        Suspect ^ children = no,
        StopSuspects = []
    ).

    % Calculate the weight of a suspect based on the weights of its children.
    % If the node is correct or inadmissible then the weight is zero.
    % If the node is ignored then the weight is the sum of the weights
    % of the children plus the sum of the excess weights of the children.
    % Otherwise the weight is the original weight of the node as reported
    % by calc_weight/5 minus the original weights of the children plus the
    % current weight of the children plus any excess weight in the children.
    %
:- pred calc_suspect_weight(S::in, T::in, maybe(list(suspect_id))::in,
    suspect_status::in, search_space(T)::in, int::out, int::out)
    is det <= mercury_edt(S, T).

calc_suspect_weight(Store, Node, MaybeChildren, Status, SearchSpace, Weight,
        ExcessWeight) :-
    (
        SearchSpace ^ maybe_weighting_heuristic = yes(Weighting),
        ( if
            ( Status = suspect_correct
            ; Status = suspect_inadmissible
            )
        then
            Weight = 0,
            ExcessWeight = 0
        else
            calc_weight(Weighting, Store, Node, OriginalWeight, ExcessWeight),
            (
                MaybeChildren = no,
                Weight = OriginalWeight
            ;
                MaybeChildren = yes(Children),
                list.map(lookup_suspect(SearchSpace), Children,
                    ChildrenSuspects),
                ChildrenNodes = list.map(func(S) = N :- N = S ^ edt_node,
                    ChildrenSuspects),
                list.foldl2(add_original_weight(Weighting, Store),
                    ChildrenNodes,
                    0, ChildrenOriginalWeight, 0, ChildrenExcess),
                list.foldl(add_existing_weight, ChildrenSuspects,
                    0, ChildrenWeight),
                (
                    Status = suspect_ignored,
                    Weight = ChildrenWeight + ChildrenExcess
                ;
                    ( Status = suspect_skipped(_)
                    ; Status = suspect_correct
                    ; Status = suspect_erroneous
                    ; Status = suspect_inadmissible
                    ; Status = suspect_pruned
                    ; Status = suspect_in_erroneous_subtree_complement
                    ; Status = suspect_unknown
                    ),
                    Weight = OriginalWeight - ChildrenOriginalWeight
                        + ChildrenWeight + ChildrenExcess
                )
            )
        )
    ;
        SearchSpace ^ maybe_weighting_heuristic = no,
        Weight = 0,
        ExcessWeight = 0
    ).

    % Add the given weight to the ancestors of the given suspect
    % (excluding the given suspect) until an erroneous node is encountered
    % (the erroneous node is also updated).
    %
:- pred add_weight_to_ancestors(suspect_id::in, int::in,
    search_space(T)::in, search_space(T)::out) is det.

add_weight_to_ancestors(SuspectId, Weight, !SearchSpace) :-
    ( if
        % Stop if the weight is 0, if the node is erroneous or
        % if there is no parent.
        Weight \= 0,
        lookup_suspect(!.SearchSpace, SuspectId, Suspect),
        Suspect ^ parent = yes(ParentId)
    then
        lookup_suspect(!.SearchSpace, ParentId, Parent),
        set_suspect(ParentId, Parent ^ weight := Parent ^ weight + Weight,
            !SearchSpace),
        excluded_complement(Parent ^ status, ExcludedComplement),
        (
            ExcludedComplement = yes
        ;
            ExcludedComplement = no,
            add_weight_to_ancestors(ParentId, Weight, !SearchSpace)
        )
    else
        true
    ).

:- pred add_original_weight(weighting_heuristic::in, S::in, T::in, int::in,
    int::out, int::in, int::out) is det <= mercury_edt(S, T).

add_original_weight(Weighting, Store, Node, Prev, Prev + Weight, PrevExcess,
        PrevExcess + Excess) :-
    calc_weight(Weighting, Store, Node, Weight, Excess).

:- pred add_existing_weight(suspect(T)::in, int::in, int::out) is det.

add_existing_weight(Suspect, Prev, Prev + Suspect ^ weight).

    % recalc_weights_upto_ancestor(Store, Ancestor, Suspects, !SearchSpace):
    %
    % Recalculate the weights of the suspects in Suspects and all their
    % ancestors below and including Ancestor. Ancestor must be a common
    % ancestor of all the suspects in Suspects.
    %
:- pred recalc_weights_upto_ancestor(S::in, suspect_id::in,
    list(suspect_id)::in, search_space(T)::in, search_space(T)::out)
    is det <= mercury_edt(S, T).

recalc_weights_upto_ancestor(Store, Ancestor, SuspectIds, !SearchSpace) :-
    recalc_weights_and_get_parents(Store, SuspectIds, [], Parents,
        !SearchSpace),
    list.filter(unify(Ancestor), Parents, _, FilteredParents),
    (
        FilteredParents = [_ | _],
        list.sort_and_remove_dups(FilteredParents, SortedParents),
        recalc_weights_upto_ancestor(Store, Ancestor, SortedParents,
            !SearchSpace)
    ;
        FilteredParents = [],
        recalc_weights_and_get_parents(Store, [Ancestor], [], _, !SearchSpace)
    ).

:- pred recalc_weights_and_get_parents(S::in, list(suspect_id)::in,
    list(suspect_id)::in, list(suspect_id)::out,
    search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

recalc_weights_and_get_parents(_, [], Parents, Parents, !SearchSpace).
recalc_weights_and_get_parents(Store, [SuspectId | SuspectIds], PrevParents,
        Parents, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children,
        Suspect ^ status, !.SearchSpace, Weight, _),
    set_suspect(SuspectId, Suspect ^ weight := Weight, !SearchSpace),
    (
        Suspect ^ parent = yes(ParentId),
        NewPrevParents = [ParentId | PrevParents]
    ;
        Suspect ^ parent = no,
        NewPrevParents = PrevParents
    ),
    recalc_weights_and_get_parents(Store, SuspectIds, NewPrevParents,
        Parents, !SearchSpace).

    % Work out the number of unknown suspects in the search space.
    % Can be used for assertion checking.
    %
:- func calc_num_unknown(search_space(T)) = int.
:- pragma consider_used(func(calc_num_unknown/1)).

calc_num_unknown(SearchSpace) = NumUnknown :-
    Suspects = map.values(SearchSpace ^ store),
    SuspectIsQuestionable =
        ( pred(suspect(_, _, Status, _, _, _)::in) is semidet :-
            questionable(Status, yes)
        ),
    list.filter(SuspectIsQuestionable, Suspects, Questionable),
    NumUnknown = list.length(Questionable).

    % Does the status mean we haven't got an answer from the oracle, or
    % haven't been able to infer anything about this suspect from other
    % oracle answers?
    %
:- pred questionable(suspect_status::in, bool::out) is det.

questionable(suspect_ignored, no).
questionable(suspect_skipped(_), yes).
questionable(suspect_correct, no).
questionable(suspect_erroneous, no).
questionable(suspect_inadmissible, no).
questionable(suspect_pruned, no).
questionable(suspect_in_erroneous_subtree_complement, no).
questionable(suspect_unknown, yes).

    % Work out the number of suspects with unexplored children.
    % Can be used for assertion checking.
    %
:- func calc_num_unexplored(search_space(T)) = int.
:- pragma consider_used(func(calc_num_unexplored/1)).

calc_num_unexplored(SearchSpace) = NumUnexplored :-
    Suspects = map.values(SearchSpace ^ store),
    SuspectIsBuggySubtree =
        ( pred(suspect(_, _, Status, _, no, _)::in) is semidet :-
            in_buggy_subtree(Status, yes)
        ),
    list.filter(SuspectIsBuggySubtree, Suspects, Unexplored),
    NumUnexplored = list.length(Unexplored).

    % Does the given status mean the suspect is in a subtree that could
    % contain a bug.
    %
:- pred in_buggy_subtree(suspect_status::in, bool::out) is det.

in_buggy_subtree(suspect_ignored, yes).
in_buggy_subtree(suspect_skipped(_), yes).
in_buggy_subtree(suspect_correct, no).
in_buggy_subtree(suspect_erroneous, yes).
in_buggy_subtree(suspect_inadmissible, no).
in_buggy_subtree(suspect_pruned, no).
in_buggy_subtree(suspect_in_erroneous_subtree_complement, no).
in_buggy_subtree(suspect_unknown, yes).

    % propagate_status_upwards(Status, StopStatusSet, SuspectId, Lowest,
    %   !SearchSpace):
    %
    % Marks all suspects not in the subtree rooted at SuspectId with Status.
    % If an ancestor of SuspectId has a status in StopStatusSet, then
    % perculation will not progress passed this ancestor. The lowest ancestor
    % of SuspectId with a status in StopStatusSet is returned in Lowest.
    % If there are no ancestors with a status in StopStatusSet then Lowest
    % will be the topmost suspect.
    %
:- pred propagate_status_upwards(suspect_status::in, list(suspect_status)::in,
    suspect_id::in, suspect_id::out,
    search_space(T)::in, search_space(T)::out) is det.

propagate_status_upwards(Status, StopStatusSet, SuspectId, Lowest,
        !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    (
        Suspect ^ parent = yes(ParentId),
        get_siblings(!.SearchSpace, SuspectId, Siblings),
        list.foldl(propagate_status_downwards(Status, StopStatusSet),
            Siblings, !SearchSpace),
        lookup_suspect(!.SearchSpace, ParentId, Parent),
        ( if list.member(Parent ^ status, StopStatusSet) then
            Lowest = ParentId
        else
            propagate_status_upwards(Status, StopStatusSet,
                ParentId, Lowest, !SearchSpace),
            set_suspect(ParentId, Parent ^ status := Status,
                !SearchSpace)
        )
    ;
        Suspect ^ parent = no,
        Lowest = SuspectId
    ).

    % Find the siblings of a suspect in the search space.
    % This does not include the suspect itself.
    %
:- pred get_siblings(search_space(T)::in, suspect_id::in,
    list(suspect_id)::out) is det.

get_siblings(SearchSpace, SuspectId, Siblings) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    (
        Suspect ^ parent = yes(ParentId),
        lookup_suspect(SearchSpace, ParentId, Parent),
        (
            Parent ^ children = yes(Children),
            (
                Children = [_ | _],
                list.filter(unify(SuspectId), Children, _, Siblings)
            ;
                Children = [],
                throw(internal_error("get_siblings", "parent has no children"))
            )
        ;
            Parent ^ children = no,
            throw(internal_error("get_siblings",
                "parent's children unexplored"))
        )
    ;
        Suspect ^ parent = no,
        Siblings = []
    ).

    % Add the list of EDT nodes to the search space as children to the given
    % suspect. The suspect_ids for the new suspects will also be returned.
    %
:- pred add_children(S::in, oracle_state::in, list(T)::in,
    suspect_id::in, suspect_status::in, search_space(T)::in,
    search_space(T)::out, list(suspect_id)::out) is det <= mercury_edt(S, T).

add_children(Store, Oracle, EDTChildren, SuspectId, Status, !SearchSpace,
        Children) :-
    Counter0 = !.SearchSpace ^ suspect_id_counter,
    lookup_suspect(!.SearchSpace, SuspectId, Suspect0),
    Depth = Suspect0 ^ depth + 1,
    add_children_2(Store, Oracle, EDTChildren, SuspectId,
        Status, Depth, !SearchSpace, Counter0, Counter, Children),
    % Lookup the suspect again, since its weight and/or status may have
    % changed.
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    !SearchSpace ^ suspect_id_counter := Counter,
    SuspectWithChildren = Suspect ^ children := yes(Children),
    set_suspect(SuspectId, SuspectWithChildren, !SearchSpace),
    list.foldl(adjust_suspect_status_from_oracle(Store, Oracle), Children,
        !SearchSpace),

    % Recalculate the weight if the suspect is ignored. This would not have
    % been done by ignore_suspect/4, since the children would not have been
    % available.

    ( if Suspect ^ status = suspect_ignored then
        calc_suspect_weight(Store, Suspect ^ edt_node, yes(Children),
            suspect_ignored, !.SearchSpace, Weight, _),
        set_suspect(SuspectId, SuspectWithChildren ^ weight := Weight,
            !SearchSpace),
        add_weight_to_ancestors(SuspectId, Weight - Suspect ^ weight,
            !SearchSpace)
    else
        true
    ).

:- pred add_children_2(S::in, oracle_state::in, list(T)::in,
    suspect_id::in, suspect_status::in, int::in,
    search_space(T)::in, search_space(T)::out, counter::in, counter::out,
    list(suspect_id)::out) is det <= mercury_edt(S, T).

add_children_2(_, _, [], _, _, _, !SearchSpace, !Counter, []).
add_children_2(Store, Oracle, [EDTChild | EDTChildren], ParentId,
        Status, Depth, !SearchSpace, !Counter, Children) :-
    allocate(NextId, !Counter),
    calc_suspect_weight(Store, EDTChild, no, Status, !.SearchSpace, Weight,
        ExcessWeight),
    map.det_insert(NextId,
        suspect(yes(ParentId), EDTChild, Status, Depth, no, Weight),
        !.SearchSpace ^ store, SuspectStore),
    !SearchSpace ^ store := SuspectStore,
    add_weight_to_ancestors(NextId, ExcessWeight, !SearchSpace),
    add_children_2(Store, Oracle, EDTChildren, ParentId,
        Status, Depth, !SearchSpace, !Counter, OtherChildren),
    Children = [NextId | OtherChildren].

:- pred adjust_suspect_status_from_oracle(S::in,
    oracle_state::in, suspect_id::in, search_space(T)::in,
    search_space(T)::out) is det <= mercury_edt(S, T).

adjust_suspect_status_from_oracle(Store, Oracle, SuspectId, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    ( if Suspect ^ status = suspect_unknown then
        edt_question(Store, Suspect ^ edt_node, Question),
        ( if answer_known(Oracle, Question, Answer) then
            (
                Answer = ignore(_),
                ignore_suspect(Store, SuspectId, !SearchSpace)
            ;
                Answer = truth_value(_, Truth),
                (
                    Truth = truth_erroneous,
                    assert_suspect_is_erroneous(SuspectId, !SearchSpace)
                ;
                    Truth = truth_correct,
                    assert_suspect_is_correct(SuspectId, !SearchSpace)
                ;
                    Truth = truth_inadmissible,
                    assert_suspect_is_inadmissible(SuspectId, !SearchSpace)
                )
            )
        else
            true
        )
    else
        true
    ).

incorporate_explicit_subtree(SuspectId, Node, !SearchSpace) :-
    lookup_suspect(!.SearchSpace, SuspectId, Suspect),
    set_suspect(SuspectId, Suspect ^ edt_node := Node, !SearchSpace),
    bimap.set(Suspect ^ edt_node, Node,
        !.SearchSpace ^ implicit_to_explicit_roots, ImplicitToExplicit),
    !SearchSpace ^ implicit_to_explicit_roots := ImplicitToExplicit.

incorporate_explicit_supertree(Store, Oracle, Node, !SearchSpace) :-
    topmost_det(!.SearchSpace, OldTopMostId),
    ( if edt_parent(Store, Node, Parent) then
        insert_new_topmost_node(Store, Oracle, Parent, !SearchSpace),

        % Node implicitly represents the root of the old search space,
        % which we already have an explicit version of, so we link the two
        % by placing an entry in implicit_to_explicit_roots.
        bimap.set(Node, get_edt_node(!.SearchSpace, OldTopMostId),
            !.SearchSpace ^ implicit_to_explicit_roots, ImplicitToExplicit),
        !SearchSpace ^ implicit_to_explicit_roots := ImplicitToExplicit
    else
        throw(internal_error("incorporate_explicit_supertree", "no parent"))
    ).

extend_search_space_upwards(Store, Oracle, !SearchSpace) :-
    topmost_det(!.SearchSpace, OldTopMostId),
    edt_parent(Store, get_edt_node(!.SearchSpace, OldTopMostId), NewTopMost),
    insert_new_topmost_node(Store, Oracle, NewTopMost, !SearchSpace).

    % Add the given EDT node to the top of the search space. The given node
    % should be a parent of the current topmost node in the search space.
    %
:- pred insert_new_topmost_node(S::in, oracle_state::in, T::in,
    search_space(T)::in, search_space(T)::out) is det <= mercury_edt(S, T).

insert_new_topmost_node(Store, Oracle, NewTopMostEDTNode, !SearchSpace) :-
    ( if edt_children(Store, NewTopMostEDTNode, EDTChildren) then
        topmost_det(!.SearchSpace, OldTopMostId),
        lookup_suspect(!.SearchSpace, OldTopMostId, OldTopMost),
        ( if
            % One of the children of the new topmost node will be the old
            % topmost node, so filter it out so it is not added twice.

            find_node_in_list(Store, EDTChildren, OldTopMost ^ edt_node, Pos),
            list.split_list(Pos - 1, EDTChildren, LeftChildren,
                [_ | RightChildren])
        then
            % Insert the new topmost node.
            NewTopMostStatus = new_parent_status( OldTopMost ^ status),
            NewTopMostDepth = OldTopMost ^ depth - 1,

            % We will update the weight below, so for now we just use 0.
            NewTopMost = suspect(no, NewTopMostEDTNode,
                NewTopMostStatus, NewTopMostDepth, no, 0),
            some [!Counter, !SuspectStore] (
                !:Counter = !.SearchSpace ^ suspect_id_counter,
                counter.allocate(NewTopMostId, !Counter),
                !SearchSpace ^ suspect_id_counter := !.Counter,
                !:SuspectStore = !.SearchSpace ^ store,
                map.set(NewTopMostId, NewTopMost, !SuspectStore),
                !SearchSpace ^ store := !.SuspectStore
            ),
            SiblingStatus = new_child_status(NewTopMostStatus),
            add_children(Store, Oracle, append(LeftChildren, RightChildren),
                NewTopMostId, SiblingStatus, !SearchSpace, ChildrenIds),

            % Now add the old topmost node as a child to the new topmost node.
            ( if
                list.split_list(Pos - 1, ChildrenIds,
                    LeftChildrenIds, RightChildrenIds)
            then
                append(LeftChildrenIds, [OldTopMostId | RightChildrenIds],
                    NewTopMostChildrenIds)
            else
                throw(internal_error("insert_new_topmost_node",
                    "invalid position"))
            ),

            calc_suspect_weight(Store, NewTopMostEDTNode,
                yes(NewTopMostChildrenIds), NewTopMostStatus,
                !.SearchSpace, Weight, _),
            some [!SuspectStore] (
                !:SuspectStore = !.SearchSpace ^ store,
                NewTopMostWithCorrectChildren =
                    NewTopMost ^ children := yes(NewTopMostChildrenIds),
                NewTopMostWithCorrectWeight =
                    NewTopMostWithCorrectChildren ^ weight := Weight,
                map.set(NewTopMostId, NewTopMostWithCorrectWeight,
                    !SuspectStore),
                map.set(OldTopMostId,
                    OldTopMost ^ parent := yes(NewTopMostId), !SuspectStore),
                !SearchSpace ^ store := !.SuspectStore
            ),
            !SearchSpace ^ topmost := yes(NewTopMostId),
            adjust_suspect_status_from_oracle(Store, Oracle, NewTopMostId,
                !SearchSpace)
        else
            throw(internal_error("insert_new_topmost_node",
                "couldn't find event number"))
        )
    else
        throw(internal_error("insert_new_topmost_node",
            "couldn't get new topmost node's children"))
    ).

:- pred find_node_in_list(S::in, list(T)::in, T::in, int::out) is semidet
    <= mercury_edt(S, T).

find_node_in_list(Store, [Node | Nodes], NodeToMatch, Pos) :-
    ( if edt_same_nodes(Store, Node, NodeToMatch) then
        Pos = 1
    else
        find_node_in_list(Store, Nodes, NodeToMatch, TailPos),
        Pos = TailPos + 1
    ).

%---------------------------------------------------------------------------%

first_unknown_descendant(Store, Oracle, SuspectId, !SearchSpace, MaybeFound) :-
    first_unknown_descendant_list(Store, Oracle, [SuspectId], !SearchSpace,
        MaybeFound).

    % first_unknown_descendant_list(Store, Oracle, List,
    %   !SearchSpace, MaybeDescendant):
    %
    % Find the first unknown suspect in List. If one is found then it is
    % returned through MaybeDescendant. Otherwise if there are any skipped,
    % ignored or erroneous suspects in List then look in the list of all
    % the children of the skipped, ignored or erroneous nodes in List,
    % recursively. Fails if an explicit subtree is required to get the children
    % of an implicit subtree and there are no other unknown suspects.
    % MaybeDescendant will be no if there are no unknown descendants and
    % no explicit subtrees are required.
    %
:- pred first_unknown_descendant_list(S::in, oracle_state::in,
    list(suspect_id)::in, search_space(T)::in, search_space(T)::out,
    maybe_found_descendant::out) is det <= mercury_edt(S, T).

first_unknown_descendant_list(Store, Oracle, SuspectList, !SearchSpace,
        MaybeFound) :-
    list.filter(suspect_unknown(!.SearchSpace), SuspectList, UnknownList,
        Others),
    (
        UnknownList = [Unknown | _],
        MaybeFound = found(Unknown)
    ;
        UnknownList = [],
        list.filter(suspect_in_buggy_subtree(!.SearchSpace), Others,
            InBuggySubtree),
        get_children_list(Store, Oracle, InBuggySubtree, !SearchSpace,
            ExplicitRequired, Children),
        (
            Children = [],
            (
                ExplicitRequired = no,
                MaybeFound = not_found
            ;
                ExplicitRequired = yes(RequireExplicitId),
                MaybeFound = require_explicit_subtree(RequireExplicitId)
            )
        ;
            Children = [_ | _],
            first_unknown_descendant_list(Store, Oracle, Children,
                !SearchSpace, MaybeFound0),
            (
                MaybeFound0 = not_found,
                (
                    ExplicitRequired = no,
                    MaybeFound = not_found
                ;
                    ExplicitRequired = yes(RequireExplicitId),
                    MaybeFound = require_explicit_subtree(RequireExplicitId)
                )
            ;
                MaybeFound0 = found(_),
                MaybeFound = MaybeFound0
            ;
                MaybeFound0 = require_explicit_subtree(_),
                MaybeFound = MaybeFound0
            )
        )
    ).

    % Succeeds if the suspect is in a part of the search space that could
    % contain a bug.
    %
:- pred suspect_in_buggy_subtree(search_space(T)::in, suspect_id::in)
    is semidet.

suspect_in_buggy_subtree(SearchSpace, SuspectId) :-
    in_buggy_subtree(get_status(SearchSpace, SuspectId), yes).

    % get_children_list(Store, Oracle, SuspectIds, !SearchSpace,
    %   ExplicitRequired, Children):
    %
    % Children is the children of all the suspects in SuspectIds appended
    % together. If an explicit subtree is required to find the children
    % of at least one element of SuspectIds, then ExplicitRequired will be yes,
    % otherwise it will be no. If an explicit subtree is required for a suspect
    % then its children are not included in Children.
    %
:- pred get_children_list(S::in, oracle_state::in,
    list(suspect_id)::in, search_space(T)::in, search_space(T)::out,
    maybe(suspect_id)::out, list(suspect_id)::out) is det <= mercury_edt(S, T).

get_children_list(_, _, [], SearchSpace, SearchSpace, no, []).
get_children_list(Store, Oracle, [SuspectId | SuspectIds],
        !SearchSpace, ExplicitRequired, ChildrenList) :-
    get_children_list(Store, Oracle, SuspectIds, !SearchSpace,
        ExplicitRequired0, ChildrenList0),
    ( if children(Store, Oracle, SuspectId, !SearchSpace, Children) then
        append(Children, ChildrenList0, ChildrenList),
        ExplicitRequired = ExplicitRequired0
    else
        ChildrenList = ChildrenList0,
        ExplicitRequired = yes(SuspectId)
    ).

%---------------------------------------------------------------------------%

choose_skipped_suspect(SearchSpace, Skipped) :-
    SearchSpace ^ topmost = yes(TopMostId),
    % XXX This can be done more efficiently, but I don't think this
    % predicate will be called too often.
    map.foldl(least_skipped(SearchSpace), SearchSpace ^ store, TopMostId,
        Skipped),
    (
        TopMostId = Skipped
    =>
        suspect_skipped(_) = get_status(SearchSpace, TopMostId)
    ).

    % least_skipped(SearchSpace, SuspectId1, Suspect1, SuspectId2,
    %   LeastSkipped):
    %
    % LeastSkipped is whichever of SuspectId1 and SuspectId2 has the lowest
    % skip order? If neither has been skipped then LeastSuspect = SuspectId2.
    % Suspect1 is the suspect referenced by SuspectId1 and is present
    % so we can use this predicate with map.foldl.
    %
:- pred least_skipped(search_space(T)::in, suspect_id::in, suspect(T)::in,
    suspect_id::in, suspect_id::out) is det.

least_skipped(SearchSpace, SuspectId1, Suspect1, SuspectId2, LeastSkipped) :-
    Status1 = Suspect1 ^ status,
    Status2 = get_status(SearchSpace, SuspectId2),
    ( if Status1 = suspect_skipped(N), Status2 = suspect_skipped(M) then
        ( if N > M then
            LeastSkipped = SuspectId2
        else
            LeastSkipped = SuspectId1
        )
    else if Status1 = suspect_skipped(_) then
        LeastSkipped = SuspectId1
    else
        LeastSkipped = SuspectId2
    ).

revise_root(Store, !SearchSpace) :-
    (
        !.SearchSpace ^ root = yes(RootId),
        force_propagate_status_downwards(suspect_unknown,
            [suspect_correct, suspect_inadmissible], RootId, StopSuspects,
            !SearchSpace),
        list.foldl(force_propagate_status_downwards(suspect_unknown,
            [suspect_correct, suspect_inadmissible]), StopSuspects,
            !SearchSpace),
        propagate_status_upwards(suspect_unknown,
            [suspect_erroneous, suspect_correct, suspect_inadmissible],
            RootId, Lowest, !SearchSpace),
        ( if suspect_erroneous(!.SearchSpace, Lowest) then
            !SearchSpace ^ root := yes(Lowest)
        else
            !SearchSpace ^ root := no
        ),

        % Recompute the suspect weights from the bottom up.
        map.keys(!.SearchSpace ^ store, AllSuspects),
        list.filter(suspect_is_leaf(!.SearchSpace), AllSuspects, Leaves),
        recalc_weights_upto_ancestor(Store, Lowest, Leaves, !SearchSpace)
    ;
        !.SearchSpace ^ root = no,
        throw(internal_error("revise_root", "no root"))
    ).

    % True if the suspect is a leaf node in the search space (i.e. it has
    % either `no' or `yes([])' in its children field.
    %
:- pred suspect_is_leaf(search_space(T)::in, suspect_id::in) is semidet.

suspect_is_leaf(SearchSpace, SuspectId) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    ( Suspect ^ children = no
    ; Suspect ^ children = yes([])
    ).

%---------------------------------------------------------------------------%

update_weighting_heuristic(Store, Weighting, !SearchSpace) :-
    MaybePrevWeighting = !.SearchSpace ^ maybe_weighting_heuristic,
    ( if
        MaybePrevWeighting = yes(PrevWeighting),
        PrevWeighting = Weighting
    then
        true
    else
        !SearchSpace ^ maybe_weighting_heuristic := yes(Weighting),
        ( if map.is_empty(!.SearchSpace ^ store) then
            true
        else
            % Recompute the suspect weights from the bottom up.
            map.keys(!.SearchSpace ^ store, AllSuspects),
            list.filter(suspect_is_leaf(!.SearchSpace),
                AllSuspects, Leaves),
            topmost_det(!.SearchSpace, TopId),
            recalc_weights_upto_ancestor(Store, TopId, Leaves, !SearchSpace)
        )
    ).

:- pred calc_weight(weighting_heuristic::in,
    S::in, T::in, int::out, int::out) is det <= mercury_edt(S, T).

calc_weight(number_of_events, Store, Node, Weight, Excess) :-
    edt_number_of_events(Store, Node, Weight, Excess).
calc_weight(suspicion, Store, Node, Weight, Excess) :-
    edt_subtree_suspicion(Store, Node, Weight, Excess).

get_current_maybe_weighting(SearchSpace) =
    SearchSpace ^ maybe_weighting_heuristic.

%---------------------------------------------------------------------------%

maybe_check_search_space_consistency(Store, SearchSpace, Context) :-
    ( if should_check_search_space_consistency then
        check_search_space_consistency(Store, SearchSpace, Context)
    else
        true
    ).

:- pred check_search_space_consistency(S::in, search_space(T)::in,
    string::in) is det <= mercury_edt(S, T).

check_search_space_consistency(Store, SearchSpace, Context) :-
    ( if
        SearchSpace ^ maybe_weighting_heuristic = yes(_),
        find_inconsistency_in_weights(Store, SearchSpace, Message)
    then
        throw(internal_error("check_search_space_consistency",
            Message ++ "\n Context = " ++ Context))
    else
        true
    ).

:- pred should_check_search_space_consistency is semidet.

:- pragma foreign_proc("C",
    should_check_search_space_consistency,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    #ifdef MR_DD_CHECK_SEARCH_SPACE
        SUCCESS_INDICATOR = MR_TRUE;
    #else
        SUCCESS_INDICATOR = MR_FALSE;
    #endif
").

    % Try to find an inconsistency in the weights of the suspects.
    % If one is found, output an error message; otherwise fail.
    %
:- pred find_inconsistency_in_weights(S::in, search_space(T)::in,
    string::out) is semidet <= mercury_edt(S, T).

find_inconsistency_in_weights(Store, SearchSpace, Message) :-
    ( if root(SearchSpace, RootId) then
        find_inconsistency_in_weights_2(Store, SearchSpace, RootId, Message)
    else
        topmost_det(SearchSpace, TopMostId),
        find_inconsistency_in_weights_2(Store, SearchSpace, TopMostId, Message)
    ).

    % Check that the weights are correct from the given suspect down.
    %
:- pred find_inconsistency_in_weights_2(S::in, search_space(T)::in,
    suspect_id::in, string::out) is semidet <= mercury_edt(S, T).

find_inconsistency_in_weights_2(Store, SearchSpace, SuspectId, Message) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    calc_suspect_weight(Store, Suspect ^ edt_node, Suspect ^ children,
        Suspect ^ status, SearchSpace, Weight, _),
    ( if
        Weight = Suspect ^ weight,
        Weight >= 0
    then
        Suspect ^ children = yes(Children),
        in_buggy_subtree(Suspect ^ status, yes),
        list.filter_map(find_inconsistency_in_weights_2(Store, SearchSpace),
            Children, Messages),
        Messages = [Message | _]
    else
        Message = "Weights not consistent for suspect id " ++
            int_to_string(SuspectId) ++ ", Suspect = " ++
            string(Suspect) ++ " Calculated weight = " ++ int_to_string(Weight)
    ).

%---------------------------------------------------------------------------%

    % Look for an implicit root in the descendants of each suspect in
    % the list in a depth first fashion.
    %
:- pred find_first_implicit_root(S::in, search_space(T)::in,
    list(suspect_id)::in, suspect_id::out) is semidet <= mercury_edt(S, T).
:- pragma consider_used(pred(find_first_implicit_root/4)).

find_first_implicit_root(Store, SearchSpace, [SuspectId | SuspectIds],
        ImplicitRoot) :-
    lookup_suspect(SearchSpace, SuspectId, Suspect),
    Status = Suspect ^ status,
    ( if
        % Check whether it might be worthwhile to build
        % an explicit subtree here.
        in_buggy_subtree(Status, yes),
        edt_is_implicit_root(Store, Suspect ^ edt_node)
    then
        ImplicitRoot = SuspectId
    else if
        in_buggy_subtree(Status, yes),
        Suspect ^ children = yes(Children),
        find_first_implicit_root(Store, SearchSpace, Children,
            ImplicitRootInChildren)
    then
        ImplicitRoot = ImplicitRootInChildren
    else
        find_first_implicit_root(Store, SearchSpace, SuspectIds,
            ImplicitRoot)
    ).

%---------------------------------------------------------------------------%
:- end_module mdb.declarative_edt.
%---------------------------------------------------------------------------%
