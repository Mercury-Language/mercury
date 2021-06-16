%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2019 The Mercury Team
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: psqueue.m.
% Main author: Matthias Güdemann.
% Stability: low.
%
% This module implements priority search queues. A priority search queue,
% or psqueue for short, combines in a single ADT the functionality of both
% a map and a priority queue.
%
% Psqueues map from priorities to keys and back. This modules provide functions
% and predicates to lookup the priority of a key, to insert and to remove
% priority-key pairs, to adjust the priority of a given key, and to retrieve
% the priority/key pair with the highest conceptual priority. However,
% since in many applications of psqueues, a low number represents high
% priority; for example, Dijkstra's shortest path algorithm wants to process
% the nearest nodes first. Therefore, given two priorities PrioA and PrioB,
% this module considers priority PrioA to have the higher conceptual priority
% if compare(CMP, PrioA, PrioB) returns CMP = (<). If priorities are numerical,
% which is common but is not required, then higher priorities are represented
% by lower numbers.
%
% The operations in this module are based on the algorithms described in
% Ralf Hinze: A simple implementation technique for priority search queues,
% Proceedings of the International Conference on Functional Programming 2001,
% pages 110-121. They use a weight-balanced tree to store priority/key pairs,
% to allow the following operation complexities:
%
% psqueue.insert        insert new priority/key pair:   O(log n)
% psqueue.lookup        lookup the priority of a key:   O(log n)
% psqueue.adjust        adjust the priority of a key:   O(log n)
% psqueue.peek:         read highest priority pair:     O(1)
% psqueue.remove_least: remove highest priority pair:   O(log n)
% psqueue.remove        remove pair with given key:     O(log n)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module psqueue.
:- interface.

:- import_module assoc_list.

%---------------------------------------------------------------------------%

:- type psqueue(P, K).

    % Create an empty priority search queue.
    %
:- func init = psqueue(P, K).
:- pred init(psqueue(P, K)::out) is det.

    % True iff the priority search queue is empty.
    %
:- pred is_empty(psqueue(P, K)::in) is semidet.

    % Create a singleton psqueue.
    %
:- func singleton(P, K) = psqueue(P, K).
:- pred singleton(P::in, K::in, psqueue(P, K)::out) is det.

    % Insert key K with priority P into the given priority search queue.
    % Fail if the key already exists.
    %
:- pred insert(P::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is semidet.
:- pragma type_spec(pred(insert/4), P = int).

    % Insert key K with priority P into the given priority search queue.
    % Throw an exception if the key already exists.
    %
:- func det_insert(psqueue(P, K), P, K) = psqueue(P, K).
:- pred det_insert(P::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is det.
:- pragma type_spec(func(det_insert/3), P = int).
:- pragma type_spec(pred(det_insert/4), P = int).

    % Return the highest priority priority/key pair in the given queue.
    % Fail if the queue is empty.
    %
:- pred peek(psqueue(P, K)::in, P::out, K::out) is semidet.

    % Return the highest priority priority/key pair in the given queue.
    % Throw an exception if the queue is empty.
    %
:- pred det_peek(psqueue(P, K)::in, P::out, K::out) is det.

    % Remove the element with the top priority. If the queue is empty, fail.
    %
:- pred remove_least(P::out, K::out, psqueue(P, K)::in, psqueue(P, K)::out)
    is semidet.
:- pragma type_spec(pred(remove_least/4), P = int).

    % Remove the element with the top priority. If the queue is empty,
    % throw an exception.
    %
:- pred det_remove_least(P::out, K::out, psqueue(P, K)::in, psqueue(P, K)::out)
    is det.
:- pragma type_spec(pred(det_remove_least/4), P = int).

    % Create an association list from a priority search queue.
    % The returned list will be in ascending order, sorted first on priority,
    % and then on key.
    %
:- func to_assoc_list(psqueue(P, K)) = assoc_list(P, K).
:- pred to_assoc_list(psqueue(P, K)::in, assoc_list(P, K)::out) is det.
:- pragma type_spec(func(to_assoc_list/1), P = int).
:- pragma type_spec(pred(to_assoc_list/2), P = int).

    % Create a priority search queue from an assoc_list of priority/key pairs.
    %
:- func from_assoc_list(assoc_list(P, K)) = psqueue(P, K).
:- pred from_assoc_list(assoc_list(P, K)::in, psqueue(P, K)::out) is det.
:- pragma type_spec(func(from_assoc_list/1), P = int).
:- pragma type_spec(pred(from_assoc_list/2), P = int).

    % Remove the element with the given key from a priority queue.
    % Fail if it is not in the queue.
    %
:- pred remove(P::out, K::in, psqueue(P, K)::in, psqueue(P, K)::out)
    is semidet.
:- pragma type_spec(pred(remove/4), P = int).

    % Remove the element with the given key from a priority queue.
    % Throw an exception if it is not in the queue.
    %
:- pred det_remove(P::out, K::in, psqueue(P, K)::in, psqueue(P, K)::out)
    is det.
:- pragma type_spec(pred(det_remove/4), P = int).

    % Adjust the priority of the specified element; the new priority will be
    % the value returned by the given adjustment function on the old priority.
    % Fail if the element is not in the queue.
    %
:- pred adjust((func(P) = P)::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out)
    is semidet.
:- pragma type_spec(pred(adjust/4), P = int).

    % Search for the priority of the specified key. If it is not in the queue,
    % fail.
    %
:- pred search(psqueue(P, K)::in, K::in, P::out) is semidet.
:- pragma type_spec(pred(search/3), P = int).

    % Search for the priority of the specified key. If it is not in the queue,
    % throw an exception.
    %
:- func lookup(psqueue(P, K), K) = P.
:- pred lookup(psqueue(P, K)::in, K::in, P::out) is det.
:- pragma type_spec(func(lookup/2), P = int).
:- pragma type_spec(pred(lookup/3), P = int).

    % Return all priority/key pairs whose priority is less than or equal to
    % the given priority.
    %
:- func at_most(psqueue(P, K), P) = assoc_list(P, K).
:- pred at_most(psqueue(P, K)::in, P::in, assoc_list(P, K)::out) is det.
:- pragma type_spec(func(at_most/2), P = int).
:- pragma type_spec(pred(at_most/3), P = int).

    % Return the number of priority/key pairs in the given queue.
    %
:- func size(psqueue(P, K)) = int.
:- pred size(psqueue(P, K)::in, int::out) is det.
:- pragma type_spec(func(size/1), P = int).
:- pragma type_spec(pred(size/2), P = int).

%---------------------------------------------------------------------------%

:- implementation.
:- interface.

% The following part of the interface is not for public consumption;
% it is intended only for use by the test suite, e.g. psqueue_test.m
% in tests/hardcoded.
%
% If the implementation is working correctly, then is_semi_heap,
% is_search_tree, has_key_condition and is_finite_map should always succeed.

    % Succeed iff the priority search queue respects the semi heap properties:
    %
    %   1) the top element has the highest priority and
    %   2) for each node of the loser tree, the priority of the loser is higher
    %      or equal to the priorities in the subtree from which the loser
    %      originates.
    %
:- pred is_semi_heap(psqueue(P, K)::in) is semidet.

    % Succeed iff the loser tree in the given priority search queue
    % respects the search tree properties:
    %
    %   1) for each node, the keys in the left subtree are smaller than
    %      or equal to the split key, and
    %   2) the keys in the right subtree are greater than the split key.
    %
:- pred is_search_tree(psqueue(P, K)::in) is semidet.

    % Succeed iff the maximal key in the winner structure and the split keys
    % in loser nodes are all present in the search tree.
    %
:- pred has_key_condition(psqueue(P, K)::in) is semidet.

    % Succeed iff all keys in the queue are present just once.
    %
:- pred is_finite_map(psqueue(P, K)::in) is semidet.

    % Return a string representation of the queue suitable for debugging.
    %
:- func dump_psqueue(psqueue(P, K)) = string.

    % Return a string representation of the queue suitable for debugging,
    % AFTER checking to see that it passes all four of the above integrity
    % tests.
    %
:- func verify_and_dump_psqueue(psqueue(P, K)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

% The psqueue data structure is based on the idea of a knockout tournament
% between the priority/key tuple in the queue. Pairs of priority/key tuples
% play matches, with the loser dropping out of the tournament, while
% the winner plays matches with other winners. Eventually, there is
% only one priority/key tuple left, the champion.
%
% In this view, the champion does not lose any matches, while every other
% tuple loses exactly one match. The representation of psqueues is based
% on this fact. When representing nonempty queues, it stores the champion
% tuple in the w_prio/w_key fields of the winner structure of the
% psqueue type, while it stores all the other tuples in the l_prio/l_key
% fields of loser_node structures in the loser_tree type.
%
% In a binary tree representing a knockout tournament, each winner of a match
% is represented at least twice in the tree: as the winner, and as one of the
% players. The loser_tree type is designed to avoid this redundancy. The idea
% is to have a tree, the loser_tree, whose structure is identical to the
% structure of the binary tree representing the matched of the knockout
% tournament, but to store information about each player in the node
% that corresponds to the match that the player LOST. Since the champion
% does not lose any matches, its details cannot be stored in such a tree,
% which is why they are stored above the tree, in the winner structure.
% The loser_tree type gets its name from the fact that each node stores
% information about the loser of the match it represents (even though
% the loser of that match may have won other matches).
%
% When a psqueue is viewed as a mapping from keys to priorities, the mapping
% must be a function: a key cannot appear in the psqueue more than once.
% When a psqueue is viewed as a mapping from priorities to keys, the mapping
% need not be a function: a priority *may* appear in the psqueue more than
% once.

:- type psqueue(P, K)
    --->    empty_psqueue
    ;       nonempty_psqueue(winner(P, K)).

:- type winner(P, K)
    --->    winner(
                % The w_prio and w_key fields contain the priority/key pair
                % with the lowest numerical priority. If there is more than one
                % pair with the same priority, it will contain the pair with
                % the smaller key.
                w_prio      :: P,
                w_key       :: K,

                % The w_losers field contains all the priority/key pairs
                % in the priority search queue other than the pair in the
                % w_prio/w_key fields.
                w_losers    :: loser_tree(P, K),

                % The w_max_key contains the highest key in the queue;
                % it must be equal to either w_key here, or to l_key
                % in one of the nodes of the tree in the w_losers field.
                % *somewhere* in the entire psqueue. This is first half of
                % the *key condition*.
                w_max_key   :: K
            ).

:- type loser_tree_size == int.

:- type loser_tree(P, K)
    --->    loser_leaf
    ;       loser_node(
                % The number of priority/key pairs in this loser tree.
                % The insertion algorithms use this measure of weight
                % to keep the loser tree weight balanced. In our case,
                % this means that either both subtrees have at most element,
                % or if the ratio of the weights of the two subtrees
                % (weight of the heavier subtree divided by the weight
                % of the lighter subtree) is no more than the limit ratio
                % given the balance_omega function.
                l_size          :: loser_tree_size,

                % The l_prio/l_key pair represents the loser of the match
                % that is represented by this node.
                %
                % The l_prio field must be less than or equal to the priorities
                % of all the priority/key pairs in the subtree from which the
                % l_prio/l_key pair originates. This loser originates from
                % the left subtree (l_left_tree) if l_key is less than or
                % equal to l_sort_key; otherwise, it originates from the right
                % subtree. This is the *semi-heap condition*.

                l_prio          :: P,
                l_key           :: K,

                % The l_left_tree field contains all the priority/key pairs
                % in this loser tree in which the key is less than or equal to
                % the key in the l_sort_key field, while
                % the l_right_tree field contains all the priority/key pairs
                % in this loser tree in which the key is greater than
                % the key in the l_sort_key field. This is the *search tree
                % condition*.
                %
                % The sort key may appear in l_left_tree, or it may be absent
                % from l_left_tree. It can never appear in l_right_tree.
                %
                % The key in l_sort_key must appear as a key (w_key or l_key)
                % *somewhere* in the entire psqueue. This is second half of
                % the *key condition*.
                l_left_tree     :: loser_tree(P, K),
                l_sort_key      :: K,
                l_right_tree    :: loser_tree(P, K)
            ).

%---------------------------------------------------------------------------%
%
% This type defines an alternate ways of looking at psqueues: as a tournament
% between priority/key pairs.
%
% Ralf Hinze's paper also talks about two other views, the min view and
% the tree view. We don't need the min view because the one task that it is
% used for in the paper (implementing at_most) we can accomplish more
% efficiently without it, and we don't need the tree view because it is
% isomorphic to the actual representation of loser trees.

:- type tournament_view(P, K)
    --->    singleton_tournament(P, K)
            % A tournament with one entrant.

    ;       tournament_between(winner(P, K), winner(P, K)).
            % A tournament between two nonempty sets of entrants.
            %
            % For tournament_between(WinnerA, WinnerB), all the keys
            % in WinnerB will be strictly greater than the maximum key
            % in WinnerA.

%---------------------------------------------------------------------------%

    % Get a tournament view of a nonempty priority search queue.
    %
:- func get_tournament_view(winner(P, K)) = tournament_view(P, K).
:- pragma type_spec(func(get_tournament_view/1), P = int).

get_tournament_view(Winner) = TournamentView :-
    Winner = winner(WinnerPrio, WinnerKey, LTree, MaxKey),
    (
        LTree = loser_leaf,
        TournamentView = singleton_tournament(WinnerPrio, WinnerKey)
    ;
        LTree = loser_node(_, LoserPrio, LoserKey,
            SubLTreeL, SplitKey, SubLTreeR),
        ( if LoserKey `leq` SplitKey then
            WinnerA = winner(LoserPrio, LoserKey, SubLTreeL, SplitKey),
            WinnerB = winner(WinnerPrio, WinnerKey, SubLTreeR, MaxKey)
        else
            WinnerA = winner(WinnerPrio, WinnerKey, SubLTreeL, SplitKey),
            WinnerB = winner(LoserPrio, LoserKey, SubLTreeR, MaxKey)
        ),
        TournamentView = tournament_between(WinnerA, WinnerB)
    ),
    trace [compile_time(flag("debug_psqueue")), io(!IO)] (
        TournamentStr = dump_tournament(0, TournamentView),
        io.output_stream(OutStream, !IO),
        io.write_string(OutStream, TournamentStr, !IO),
        io.nl(OutStream, !IO)
    ).

    % Play a tournament to combine two priority search queues, PSQA and PSQB.
    % All the keys in PSQA are guaranteed to be less than or equal to
    % PSQA's max key, while all the keys in LTreeR are guaranteed to be
    % strictly greater than PSQA's max key.
    %
    % See Ralf Hinze's paper for a more detailed explanation.
    %
    % The other combine_*_via_tournament predicates are special cases
    % for situations in which we know that one or both psqueues are
    % nonempty.
    %
:- pred combine_psqueues_via_tournament(psqueue(P, K)::in, psqueue(P, K)::in,
    psqueue(P, K)::out) is det.
:- pragma type_spec(pred(combine_psqueues_via_tournament/3), P = int).

combine_psqueues_via_tournament(PSQA, PSQB, CombinedPSQ) :-
    (
        PSQA = empty_psqueue,
        CombinedPSQ = PSQB
        % has the same effect as
        % (
        %     PSQB = empty_psqueue,
        %     CombinedPSQ = empty_psqueue
        % ;
        %     PSQB = nonempty_psqueue(WinnerB),
        %     CombinedPSQ = PSQB
        % )
    ;
        PSQA = nonempty_psqueue(WinnerA),
        (
            PSQB = empty_psqueue,
            CombinedPSQ = PSQA
        ;
            PSQB = nonempty_psqueue(WinnerB),
            combine_winners_via_tournament(WinnerA, WinnerB, CombinedWinner),
            CombinedPSQ = nonempty_psqueue(CombinedWinner)
        )
    ).

:- pred combine_winner_psqueue_via_tournament(
    winner(P, K)::in, psqueue(P, K)::in, winner(P, K)::out) is det.
:- pragma type_spec(pred(combine_winner_psqueue_via_tournament/3), P = int).

combine_winner_psqueue_via_tournament(WinnerA, PSQB, CombinedWinner) :-
    (
        PSQB = empty_psqueue,
        CombinedWinner = WinnerA
    ;
        PSQB = nonempty_psqueue(WinnerB),
        combine_winners_via_tournament(WinnerA, WinnerB, CombinedWinner)
    ).

:- pred combine_psqueue_winner_via_tournament(
    psqueue(P, K)::in, winner(P, K)::in, winner(P, K)::out) is det.
:- pragma type_spec(pred(combine_psqueue_winner_via_tournament/3), P = int).

combine_psqueue_winner_via_tournament(PSQA, WinnerB, CombinedWinner) :-
    (
        PSQA = empty_psqueue,
        CombinedWinner = WinnerB
    ;
        PSQA = nonempty_psqueue(WinnerA),
        combine_winners_via_tournament(WinnerA, WinnerB, CombinedWinner)
    ).

:- pred combine_winners_via_tournament(winner(P, K)::in, winner(P, K)::in,
    winner(P, K)::out) is det.
:- pragma type_spec(pred(combine_winners_via_tournament/3), P = int).

combine_winners_via_tournament(WinnerA, WinnerB, CombinedWinner) :-
    WinnerA = winner(PrioA, KeyA, LTreeA, MaxKeyA),
    WinnerB = winner(PrioB, KeyB, LTreeB, MaxKeyB),
    ( if PrioA `leq` PrioB then
        % WinnerA wins
        LTree = balance(PrioB, KeyB, LTreeA, MaxKeyA, LTreeB),
        CombinedWinner = winner(PrioA, KeyA, LTree, MaxKeyB)
    else
        % WinnerB wins
        LTree = balance(PrioA, KeyA, LTreeA, MaxKeyA, LTreeB),
        CombinedWinner = winner(PrioB, KeyB, LTree, MaxKeyB)
    ).

%---------------------------------------------------------------------------%
%
% Balancing functions for weight balanced trees.
%

    % balance(Prio, Key, LTreeL, SplitKey, LTreeR) = LTree:
    %
    % Construct LTree, a loser tree that contains:
    %
    % - the priority/key pair Prio/Key,
    % - all the priority/key pairs in LTreeL, and
    % - all the priority/key pairs in LTreeR.
    %
    % All the keys in LTreeL are guaranteed to be less than or equal to
    % SplitKey, while all the keys in LTreeR are guaranteed to be strictly
    % greater than SplitKey.
    %
    % XXX It would be nice to add assertions to the code to enforce this
    % invariant, and see whether they are violated during stress tests
    % of this module. However, right now we don't have any such stress tests.
    %
    % NOTE All the calls to unexpected below when finding loser_leaf are there
    % because there is no way to tell Mercury that finding that e.g. LTreeL
    % is heavier than LTreeR implies that LTreeL cannot be empty.
    %
:- func balance(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(balance/5), P = int).

balance(Prio, Key, LTreeL, SplitKey, LTreeR) = LTree :-
    SizeL = loser_tree_size(LTreeL),
    SizeR = loser_tree_size(LTreeR),
    ( if
        SizeR + SizeL < 2
    then
        LTree = construct_node(Prio, Key, LTreeL, SplitKey, LTreeR)
    else if
        compare(CMPL, SizeR, balance_omega * SizeL),
        CMPL = (>)
    then
        LTree = balance_left(Prio, Key, LTreeL, SplitKey, LTreeR)
    else if
        compare(CMPR, SizeL, balance_omega * SizeR),
        CMPR = (>)
    then
        LTree = balance_right(Prio, Key, LTreeL, SplitKey, LTreeR)
    else
        LTree = construct_node(Prio, Key, LTreeL, SplitKey, LTreeR)
    ).

    % The implementation of balance for the case when
    %   size(LTreeR) > balance_omega * size(LTreeL),
    % so we want to rotate the tree to move weight towards the left.
    %
:- func balance_left(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(balance_left/5), P = int).

balance_left(Prio, Key, LTreeL, SplitKey, LTreeR) = LTree :-
    (
        LTreeR = loser_node(_, _, _, SubLTreeRL, _, SubLTreeRR),
        compare(CMP, loser_tree_size(SubLTreeRL), loser_tree_size(SubLTreeRR)),
        ( if CMP = (<) then
            LTree = single_left(Prio, Key, LTreeL, SplitKey, LTreeR)
        else
            LTree = double_left(Prio, Key, LTreeL, SplitKey, LTreeR)
        )
    ;
        LTreeR = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % The implementation of balance for the case when
    %   size(LTreeL) > balance_omega * size(LTreeR).
    % so we want to rotate the tree to move weight towards the right.
    %
:- func balance_right(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(balance_right/5), P = int).

balance_right(Prio, Key, LTreeL, SplitKey, LTreeR) = LTree :-
    (
        LTreeL = loser_node(_, _, _, SubLTreeLL, _, SubLTreeLR),
        compare(CMP, loser_tree_size(SubLTreeLR), loser_tree_size(SubLTreeLL)),
        ( if CMP = (<) then
            LTree = single_right(Prio, Key, LTreeL, SplitKey, LTreeR)
        else
            LTree = double_right(Prio, Key, LTreeL, SplitKey, LTreeR)
        )
    ;
        LTreeL = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % The implementation of balance for the case when we need a double
    % rotation to the left.
    %
:- func double_left(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(double_left/5), P = int).

double_left(InsertPrio, InsertKey, LTreeA, SplitKeyAB, LTreeBC) = LTree :-
    (
        LTreeBC = loser_node(_, LoserPrio, LoserKey,
            LTreeB, SplitKeyBC, LTreeC),
        LTree = single_left(InsertPrio, InsertKey,
            LTreeA,
            SplitKeyAB,
            single_right(LoserPrio, LoserKey, LTreeB, SplitKeyBC, LTreeC))
    ;
        LTreeBC = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % The implementation of balance for the case when we need a double
    % rotation to the right.
    %
:- func double_right(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(double_right/5), P = int).

double_right(InsertPrio, InsertKey, LTreeAB, SplitKeyBC, LTreeC) = LTree :-
    (
        LTreeAB = loser_node(_, LoserPrio, LoserKey,
            LTreeA, SplitKeyAB, LTreeB),
        LTree = single_right(InsertPrio, InsertKey,
            single_left(LoserPrio, LoserKey, LTreeA, SplitKeyAB, LTreeB),
            SplitKeyBC,
            LTreeC)
    ;
        LTreeAB = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % The implementation of balance for the case when we need a single
    % rotation to the left.
    %
:- func single_left(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(single_left/5), P = int).

single_left(InsertPrio, InsertKey, LTreeA, SplitKeyAB, LTreeBC) = LTree :-
    (
        LTreeBC = loser_node(_, LoserPrio, LoserKey,
            LTreeB, SplitKeyBC, LTreeC),
        ( if
            LoserKey `leq` SplitKeyBC,
            InsertPrio `leq` LoserPrio
        then
            LTree = construct_node(InsertPrio, InsertKey,
                construct_node(LoserPrio, LoserKey,
                    LTreeA, SplitKeyAB, LTreeB),
                SplitKeyBC,
                LTreeC)
        else
            LTree = construct_node(LoserPrio, LoserKey,
                construct_node(InsertPrio, InsertKey,
                    LTreeA, SplitKeyAB, LTreeB),
                SplitKeyBC,
                LTreeC)
        )
    ;
        LTreeBC = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % The implementation of balance for the case when we need a single
    % rotation to the right.
    %
:- func single_right(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma type_spec(func(single_right/5), P = int).

single_right(InsertPrio, InsertKey, LTreeAB, SplitKeyBC, LTreeC) = LTree :-
    (
        LTreeAB = loser_node(_, LoserPrio, LoserKey,
            LTreeA, SplitKeyAB, LTreeB),
        ( if
            compare(CMP0, LoserKey, SplitKeyAB),
            CMP0 = (>),
            InsertPrio `leq` LoserPrio
        then
            LTree = construct_node(InsertPrio, InsertKey,
                LTreeA,
                SplitKeyAB,
                construct_node(LoserPrio, LoserKey,
                    LTreeB, SplitKeyBC, LTreeC))
        else
            LTree = construct_node(LoserPrio, LoserKey,
                LTreeA,
                SplitKeyAB,
                construct_node(InsertPrio, InsertKey,
                    LTreeB, SplitKeyBC, LTreeC))
        )
    ;
        LTreeAB = loser_leaf,
        unexpected($file, $pred, "heavier tree is a leaf")
    ).

    % Balance factor, must be over 3.75 (see Ralf Hinze's paper).
    %
:- func balance_omega = loser_tree_size.

balance_omega = 4.

:- func construct_node(P, K, loser_tree(P, K), K, loser_tree(P, K))
    = loser_tree(P, K).
:- pragma inline(func(construct_node/5)).

construct_node(Prio, Key, SubLTreeL, SplitKey, SubLTreeR) = LTree :-
    Size = 1 + loser_tree_size(SubLTreeL) + loser_tree_size(SubLTreeR),
    LTree = loser_node(Size, Prio, Key, SubLTreeL, SplitKey, SubLTreeR).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init = PSQ :-
    init(PSQ).

init(empty_psqueue).

is_empty(empty_psqueue).

singleton(Prio, Key) = PSQ :-
    singleton(Prio, Key, PSQ).

singleton(Prio, Key, PSQ) :-
    PSQ = nonempty_psqueue(singleton_winner(Prio, Key)).

:- func singleton_winner(P, K) = winner(P, K).
:- pragma inline(func(singleton_winner/2)).

singleton_winner(Prio, Key) =
    winner(Prio, Key, loser_leaf, Key).

%---------------------------------------------------------------------------%

insert(InsertPrio, InsertKey, !PSQ) :-
    (
        !.PSQ = empty_psqueue,
        !:PSQ = singleton(InsertPrio, InsertKey)
    ;
        !.PSQ = nonempty_psqueue(Winner0),
        insert_tv(InsertPrio, InsertKey, get_tournament_view(Winner0), Winner),
        !:PSQ = nonempty_psqueue(Winner)
    ).

det_insert(PSQ0, InsertPrio, InsertKey) = PSQ :-
    det_insert(InsertPrio, InsertKey, PSQ0, PSQ).

det_insert(InsertPrio, InsertKey, !PSQ) :-
    ( if insert(InsertPrio, InsertKey, !PSQ) then
        true
    else
        unexpected($file, $pred, "key being inserted is already present")
    ).

:- pred insert_tv(P::in, K::in,
    tournament_view(P, K)::in, winner(P, K)::out) is semidet.
:- pragma type_spec(pred(insert_tv/4), P = int).

insert_tv(InsertPrio, InsertKey, TV, Winner) :-
    (
        TV = singleton_tournament(Prio, Key),
        compare(CMP, InsertKey, Key),
        (
            CMP = (<),
            WinnerA = singleton_winner(InsertPrio, InsertKey),
            WinnerB = singleton_winner(Prio, Key)
        ;
            CMP = (>),
            WinnerA = singleton_winner(Prio, Key),
            WinnerB = singleton_winner(InsertPrio, InsertKey)
        ),
        % XXX Why call a general-purpose predicate for combining
        % two singletons?
        combine_winners_via_tournament(WinnerA, WinnerB, Winner)
    ;
        TV = tournament_between(WinnerA, WinnerB),
        WinnerA = winner(_, _, _, MaxKeyA),
        WinnerB = winner(_, _, _, _),
        ( if InsertKey `leq` MaxKeyA then
            insert_tv(InsertPrio, InsertKey,
                get_tournament_view(WinnerA), UpdatedWinnerA),
            combine_winners_via_tournament(UpdatedWinnerA, WinnerB, Winner)
        else
            insert_tv(InsertPrio, InsertKey,
                get_tournament_view(WinnerB), UpdatedWinnerB),
            combine_winners_via_tournament(WinnerA, UpdatedWinnerB, Winner)
        )
    ).

%---------------------------------------------------------------------------%

peek(PSQ, MinPrio, MinKey) :-
    PSQ = nonempty_psqueue(winner(MinPrio, MinKey, _, _)).

det_peek(PSQ, MinPrio, MinKey) :-
    ( if peek(PSQ, MinPrioPrime, MinKeyPrime) then
        MinKey = MinKeyPrime,
        MinPrio = MinPrioPrime
    else
        unexpected($file, $pred, "priority search queue is empty")
    ).

remove_least(MinPrio, MinKey, !PSQ) :-
    !.PSQ = nonempty_psqueue(winner(MinPrio, MinKey, LTree, MaxKey)),
    !:PSQ = convert_loser_tree_to_psqueue(LTree, MaxKey).

det_remove_least(MinPrio, MinKey, !PSQ) :-
    ( if remove_least(MinPrioPrime, MinKeyPrime, !PSQ) then
        MinKey = MinKeyPrime,
        MinPrio = MinPrioPrime
    else
        unexpected($file, $pred, "priority search queue is empty")
    ).

    % convert_loser_tree_to_psqueue(LTree, MaxKey):
    %
    % Convert LTree to a psqueue. All the keys in LTree are guaranteed
    % to be less than or equal to MaxKey.
    %
:- func convert_loser_tree_to_psqueue(loser_tree(P, K), K) = psqueue(P, K).
:- pragma type_spec(func(convert_loser_tree_to_psqueue/2), P = int).

convert_loser_tree_to_psqueue(LTree, MaxKey) = PSQ :-
    (
        LTree = loser_leaf,
        PSQ = empty_psqueue
    ;
        LTree = loser_node(_, LoserPrio, LoserKey,
            SubLTreeL, SplitKey, SubLTreeR),
        ( if LoserKey `leq` SplitKey then
            WinnerA = winner(LoserPrio, LoserKey, SubLTreeL, SplitKey),
            PSQA = nonempty_psqueue(WinnerA),
            PSQB = convert_loser_tree_to_psqueue(SubLTreeR, MaxKey)
        else
            PSQA = convert_loser_tree_to_psqueue(SubLTreeL, SplitKey),
            WinnerB = winner(LoserPrio, LoserKey, SubLTreeR, MaxKey),
            PSQB = nonempty_psqueue(WinnerB)
        ),
        combine_psqueues_via_tournament(PSQA, PSQB, PSQ)
    ).

%---------------------------------------------------------------------------%

to_assoc_list(PSQ) = AssocList :-
    to_assoc_list(PSQ, AssocList).

to_assoc_list(PSQ0, AssocList) :-
    ( if remove_least(K, P, PSQ0, PSQ1) then
        to_assoc_list(PSQ1, AssocListTail),
        AssocList = [K - P | AssocListTail]
    else
        AssocList = []
    ).

from_assoc_list(AssocList) = PSQ :-
    from_assoc_list(AssocList, PSQ).

from_assoc_list(AssocList, PSQ) :-
    from_assoc_list_loop(AssocList, init, PSQ).

:- pred from_assoc_list_loop(assoc_list(P, K)::in,
    psqueue(P, K)::in, psqueue(P, K)::out) is det.
:- pragma type_spec(pred(from_assoc_list_loop/3), P = int).

from_assoc_list_loop([], !PSQ).
from_assoc_list_loop([Prio - Key | PriosKeys], !PSQ) :-
    det_insert(Prio, Key, !PSQ),
    from_assoc_list_loop(PriosKeys, !PSQ).

%---------------------------------------------------------------------------%

remove(MatchingPrio, SearchKey, !PSQ) :-
    (
        !.PSQ = empty_psqueue,
        fail
    ;
        !.PSQ = nonempty_psqueue(Winner0),
        remove_tv(MatchingPrio, SearchKey, get_tournament_view(Winner0), !:PSQ)
    ).

det_remove(MatchingPrio, SearchKey, !PSQ) :-
    ( if remove(MatchingPrioPrime, SearchKey, !PSQ) then
        MatchingPrio = MatchingPrioPrime
    else
        unexpected($file, $pred, "element not found")
    ).

:- pred remove_tv(P::out, K::in,
    tournament_view(P, K)::in, psqueue(P, K)::out) is semidet.
:- pragma type_spec(pred(remove_tv/4), P = int).

remove_tv(MatchingPrio, SearchKey, TournamentView, PSQ) :-
    (
        TournamentView = singleton_tournament(Prio, Key),
        ( if Key = SearchKey then
            MatchingPrio = Prio,
            PSQ = empty_psqueue
        else
            fail
        )
    ;
        TournamentView = tournament_between(WinnerA, WinnerB),
        WinnerA = winner(_, _, _, MaxKeyA),
        ( if SearchKey `leq` MaxKeyA then
            remove_tv(MatchingPrio, SearchKey,
                get_tournament_view(WinnerA), UpdatedPSQA),
            combine_psqueue_winner_via_tournament(UpdatedPSQA, WinnerB,
                CombinedWinner)
        else
            remove_tv(MatchingPrio, SearchKey,
                get_tournament_view(WinnerB), UpdatedPSQB),
            combine_winner_psqueue_via_tournament(WinnerA, UpdatedPSQB,
                CombinedWinner)
        ),
        PSQ = nonempty_psqueue(CombinedWinner)
    ).

%---------------------------------------------------------------------------%

adjust(AdjustFunc, SearchKey, !PSQ) :-
    (
        !.PSQ = empty_psqueue,
        fail
    ;
        !.PSQ = nonempty_psqueue(Winner0),
        adjust_tv(AdjustFunc, SearchKey, get_tournament_view(Winner0), Winner),
        !:PSQ = nonempty_psqueue(Winner)
    ).

:- pred adjust_tv(func(P) = P::in(func(in) = out is det),
    K::in, tournament_view(P, K)::in, winner(P, K)::out) is semidet.
:- pragma type_spec(pred(adjust_tv/4), P = int).

adjust_tv(AdjustFunc, SearchKey, TournamentView, Winner) :-
    (
        TournamentView = singleton_tournament(Prio, Key),
        ( if Key = SearchKey then
            Winner = singleton_winner(AdjustFunc(Prio), Key)
        else
            % XXX was Winner = singleton_winner(Prio, Key)
            fail
        )
    ;
        TournamentView = tournament_between(WinnerA, WinnerB),
        WinnerA = winner(_, _, _, MaxKeyA),
        ( if SearchKey `leq` MaxKeyA then
            adjust_tv(AdjustFunc, SearchKey,
                get_tournament_view(WinnerA), UpdatedWinnerA),
            combine_winners_via_tournament(UpdatedWinnerA, WinnerB, Winner)
        else
            adjust_tv(AdjustFunc, SearchKey,
                get_tournament_view(WinnerB), UpdatedWinnerB),
            combine_winners_via_tournament(WinnerA, UpdatedWinnerB, Winner)
        )
    ).

%---------------------------------------------------------------------------%

search(PSQ, SearchKey, MatchingPrio) :-
    (
        PSQ = empty_psqueue,
        fail
    ;
        PSQ = nonempty_psqueue(Winner),
        % XXX Why do we transform PSQ into a tournament view
        % before searching it? Why don't we search it directly?
        % It should be both simpler and faster.
        search_tv(get_tournament_view(Winner), SearchKey, MatchingPrio)
    ).

:- pred search_tv(tournament_view(P, K)::in, K::in, P::out) is semidet.
:- pragma type_spec(pred(search_tv/3), P = int).

search_tv(TournamentView, SearchKey, MatchingPrio) :-
    (
        TournamentView = singleton_tournament(Prio, Key),
        ( if Key = SearchKey then
            MatchingPrio = Prio
        else
            fail
        )
    ;
        TournamentView = tournament_between(WinnerA, WinnerB),
        WinnerA = winner(_, _, _, MaxKeyA),
        ( if SearchKey `leq` MaxKeyA then
            search_tv(get_tournament_view(WinnerA), SearchKey, MatchingPrio)
        else
            search_tv(get_tournament_view(WinnerB), SearchKey, MatchingPrio)
        )
    ).

lookup(PSQ, SearchKey) = MatchingPrio :-
    lookup(PSQ, SearchKey, MatchingPrio).

lookup(PSQ, SearchKey, MatchingPrio) :-
    ( if search(PSQ, SearchKey, MatchingPrioPrime) then
        MatchingPrio = MatchingPrioPrime
    else
        unexpected($file, $pred, "key not found")
    ).

%---------------------------------------------------------------------------%

at_most(PSQ, MaxPrio) = AssocList :-
    at_most(PSQ, MaxPrio, AssocList).

at_most(PSQ, MaxPrio, AssocList) :-
    (
        PSQ = empty_psqueue,
        AssocList = []
    ;
        PSQ = nonempty_psqueue(Winner),
        at_most_in_winner(Winner, MaxPrio, Cord),
        AssocList = cord.list(Cord)
    ).

:- pred at_most_in_winner(winner(P, K)::in, P::in, cord(pair(P, K))::out)
    is det.
:- pragma type_spec(pred(at_most_in_winner/3), P = int).

at_most_in_winner(Winner, MaxPrio, Cord) :-
    Winner = winner(WinnerPrio, _, _, _),
    compare(CMP, WinnerPrio, MaxPrio),
    (
        CMP = (>),
        Cord = cord.init
    ;
        ( CMP = (=)
        ; CMP = (<)
        ),
        TournamentView = get_tournament_view(Winner),
        (
            TournamentView = singleton_tournament(SinglePrio, SingleKey),
            Cord = cord.singleton(SinglePrio - SingleKey)
        ;
            TournamentView = tournament_between(WinnerA, WinnerB),
            at_most_in_winner(WinnerA, MaxPrio, CordA),
            at_most_in_winner(WinnerB, MaxPrio, CordB),
            Cord = CordA ++ CordB
        )
    ).

%---------------------------------------------------------------------------%

size(PSQ) = Size :-
    size(PSQ, Size).

size(PSQ, Size) :-
    (
        PSQ = empty_psqueue,
        Size = 0
    ;
        PSQ = nonempty_psqueue(winner(_, _, LTree, _)),
        % XXX was just loser_tree_size(LTree)
        Size = 1 + loser_tree_size(LTree)
    ).

:- func loser_tree_size(loser_tree(P, K)) = loser_tree_size.

loser_tree_size(LTree) = Size :-
    (
        LTree = loser_leaf,
        Size = 0
    ;
        LTree = loser_node(Size, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%

:- pred leq(V::in, V::in) is semidet.
:- pragma type_spec(pred(leq/2), V = int).

leq(ValLeft, ValRight) :-
    compare(CMP, ValLeft, ValRight),
    ( CMP = (<)
    ; CMP = (=)
    ).

:- func min2(V, V) = V.
:- pragma type_spec(func(min2/2), V = int).

min2(A, B) = Min :-
    ( if A `leq` B then
        Min = A
    else
        Min = B
    ).

:- func max2(V, V) = V.
:- pragma type_spec(func(max2/2), V = int).

max2(A, B) = Max :-
    ( if A `leq` B then
        Max = B
    else
        Max = A
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The integrity test predicates.
%

is_semi_heap(PSQ) :-
    (
        PSQ = empty_psqueue
    ;
        PSQ = nonempty_psqueue(winner(WinnerPrio, _, LTree, _)),
        all_prios_in_loser_tree_at_or_above_prio(WinnerPrio, LTree),
        all_nodes_obey_semi_heap(LTree)
    ).

    % Succeed iff all priorities in the given ltree are numerically
    % at least as high as the given priority.
    %
:- pred all_prios_in_loser_tree_at_or_above_prio(P::in, loser_tree(P, K)::in)
    is semidet.

all_prios_in_loser_tree_at_or_above_prio(WinnerPrio, LTree) :-
    (
        LTree = loser_leaf
    ;
        LTree = loser_node(_, LoserPrio, _, SubLTreeL, _, SubLTreeR),
        WinnerPrio `leq` LoserPrio,
        all_prios_in_loser_tree_at_or_above_prio(WinnerPrio, SubLTreeL),
        all_prios_in_loser_tree_at_or_above_prio(WinnerPrio, SubLTreeR)
    ).

:- pred all_nodes_obey_semi_heap(loser_tree(P, K)::in) is semidet.

all_nodes_obey_semi_heap(LTree) :-
    (
        LTree = loser_leaf
    ;
        LTree = loser_node(_, Prio, Key, SubLTreeL, SplitKey, SubLTreeR),
        ( if Key `leq` SplitKey then
            min_prio_in_loser_tree_acc(SubLTreeL, Prio, MaybeMinPrio)
        else
            min_prio_in_loser_tree_acc(SubLTreeR, Prio, MaybeMinPrio)
        ),
        (
            MaybeMinPrio = no
        ;
            MaybeMinPrio = yes(MinPrio),
            compare(CMP, Prio, MinPrio),
            CMP = (=)
        ),
        all_nodes_obey_semi_heap(SubLTreeL),
        all_nodes_obey_semi_heap(SubLTreeR)
    ).

:- pred min_prio_in_loser_tree_acc(loser_tree(P, K)::in, P::in, maybe(P)::out)
    is det.

min_prio_in_loser_tree_acc(LTree, !.CurMinPrio, MaybeMinPrio) :-
    (
        LTree = loser_leaf,
        MaybeMinPrio = no
    ;
        LTree = loser_node(_, Prio, _, SubLTreeL, _, SubLTreeR),
        compare(CMP, !.CurMinPrio, Prio),
        (
            ( CMP = (<)
            ; CMP = (=)
            )
        ;
            CMP = (>),
            !:CurMinPrio = Prio
        ),
        min_prio_in_loser_tree_acc(SubLTreeL, !.CurMinPrio, MaybeMinPrioL),
        min_prio_in_loser_tree_acc(SubLTreeR, !.CurMinPrio, MaybeMinPrioR),
        take_min_xmxmx(!.CurMinPrio, MaybeMinPrioL, MaybeMinPrioR, MinPrio),
        MaybeMinPrio = yes(MinPrio)
    ).

%---------------------------------------------------------------------------%

is_search_tree(PSQ) :-
    (
        PSQ = empty_psqueue
    ;
        PSQ = nonempty_psqueue(winner(_, _, LTree, _)),
        loser_tree_has_search_property(LTree)
    ).

:- pred loser_tree_has_search_property(loser_tree(P, K)::in) is semidet.

loser_tree_has_search_property(LTree) :-
    (
        LTree = loser_leaf
    ;
        LTree = loser_node(_, _, _, SubLTreeL, SplitKey, SubLTreeR),
        max_key_in_loser_tree(SubLTreeL, MaybeMaxKeyL),
        min_key_in_loser_tree(SubLTreeR, MaybeMinKeyR),
        (
            MaybeMaxKeyL = no
        ;
            MaybeMaxKeyL = yes(MaxKeyL),
            compare(CMPL, MaxKeyL, SplitKey),
            ( CMPL = (<) ; CMPL = (=) ),
            loser_tree_has_search_property(SubLTreeL)
        ),
        (
            MaybeMinKeyR = no
        ;
            MaybeMinKeyR = yes(MinKeyR),
            compare(CMPR, MinKeyR, SplitKey),
            CMPR = (>),
            loser_tree_has_search_property(SubLTreeR)
        )
    ).

%------------------%

:- pred min_key_in_loser_tree(loser_tree(P, K)::in, maybe(K)::out) is det.

min_key_in_loser_tree(LTree, MaybeMinKey) :-
    (
        LTree = loser_leaf,
        MaybeMinKey = no
    ;
        LTree = loser_node(_, _, Key, SubLTreeL, _, SubLTreeR),
        CurMin = Key,
        min_key_in_loser_tree_acc(SubLTreeL, CurMin, MaybeMinKeyL),
        min_key_in_loser_tree_acc(SubLTreeR, CurMin, MaybeMinKeyR),
        take_min_xmxmx(CurMin, MaybeMinKeyL, MaybeMinKeyR, MinKey),
        MaybeMinKey = yes(MinKey)
    ).

:- pred min_key_in_loser_tree_acc(loser_tree(P, K)::in, K::in,
    maybe(K)::out) is det.

min_key_in_loser_tree_acc(LTree, !.CurMin, MaybeMinKey) :-
    (
        LTree = loser_leaf,
        MaybeMinKey = no
    ;
        LTree = loser_node(_, _, Key, SubLTreeL, _, SubLTreeR),
        compare(CMP, !.CurMin, Key),
        (
            ( CMP = (<)
            ; CMP = (=)
            )
        ;
            CMP = (>),
            !:CurMin = Key
        ),
        min_key_in_loser_tree_acc(SubLTreeL, !.CurMin, MaybeMinKeyL),
        min_key_in_loser_tree_acc(SubLTreeR, !.CurMin, MaybeMinKeyR),
        take_min_xmxmx(!.CurMin, MaybeMinKeyL, MaybeMinKeyR, MinKey),
        MaybeMinKey = yes(MinKey)
    ).

%------------------%

:- pred max_key_in_loser_tree(loser_tree(P, K)::in, maybe(K)::out) is det.

max_key_in_loser_tree(LTree, MaybeMaxKey) :-
    (
        LTree = loser_leaf,
        MaybeMaxKey = no
    ;
        LTree = loser_node(_, _, Key, SubLTreeL, _, SubLTreeR),
        CurMax = Key,
        max_key_in_loser_tree_acc(SubLTreeL, CurMax, MaybeMaxKeyL),
        max_key_in_loser_tree_acc(SubLTreeR, CurMax, MaybeMaxKeyR),
        take_max_xmxmx(CurMax, MaybeMaxKeyL, MaybeMaxKeyR, MaxKey),
        MaybeMaxKey = yes(MaxKey)
    ).

:- pred max_key_in_loser_tree_acc(loser_tree(P, K)::in, K::in,
    maybe(K)::out) is det.

max_key_in_loser_tree_acc(LTree, !.CurMax, MaybeMaxKey) :-
    (
        LTree = loser_leaf,
        MaybeMaxKey = no
    ;
        LTree = loser_node(_, _, Key, SubLTreeL, _, SubLTreeR),
        compare(CMP, !.CurMax, Key),
        (
            CMP = (<),
            !:CurMax = Key
        ;
            ( CMP = (=)
            ; CMP = (>)
            )
        ),
        max_key_in_loser_tree_acc(SubLTreeL, !.CurMax, MaybeMaxKeyL),
        max_key_in_loser_tree_acc(SubLTreeR, !.CurMax, MaybeMaxKeyR),
        take_max_xmxmx(!.CurMax, MaybeMaxKeyL, MaybeMaxKeyR, MaxKey),
        MaybeMaxKey = yes(MaxKey)
    ).

%------------------%

:- pred take_min_xmxmx(T::in, maybe(T)::in, maybe(T)::in, T::out) is det.

take_min_xmxmx(X, MaybeL, MaybeR, Min) :-
    (
        MaybeL = no,
        MaybeR = no,
        Min = X
    ;
        MaybeL = yes(L),
        MaybeR = no,
        Min = min2(X, L)
    ;
        MaybeL = no,
        MaybeR = yes(R),
        Min = min2(X, R)
    ;
        MaybeL = yes(L),
        MaybeR = yes(R),
        Min = min2(X, min2(L, R))
    ).

:- pred take_max_xmxmx(T::in, maybe(T)::in, maybe(T)::in, T::out) is det.

take_max_xmxmx(X, MaybeL, MaybeR, Min) :-
    (
        MaybeL = no,
        MaybeR = no,
        Min = X
    ;
        MaybeL = yes(L),
        MaybeR = no,
        Min = max2(X, L)
    ;
        MaybeL = no,
        MaybeR = yes(R),
        Min = max2(X, R)
    ;
        MaybeL = yes(L),
        MaybeR = yes(R),
        Min = max2(X, max2(L, R))
    ).

%---------------------------------------------------------------------------%

has_key_condition(PSQ) :-
    (
        PSQ = empty_psqueue
    ;
        PSQ = nonempty_psqueue(winner(_, _, LTree, MaxKey)),
        search(PSQ, MaxKey, _),
        loser_split_keys_are_present(PSQ, LTree)
    ).

    % loser_split_keys_are_present(PSQ, LTree):
    %
    % Succeed iff all the split keys in LTree are present *somewhere* in PSQ.
    % They do not have to be present in LTree itself, and in fact, they often
    % won't be.
    %
:- pred loser_split_keys_are_present(psqueue(P, K)::in, loser_tree(P, K)::in)
    is semidet.

loser_split_keys_are_present(PSQ, LTree) :-
    (
        LTree = loser_leaf
    ;
        LTree = loser_node(_, _, _, SubLTreeL, SplitKey, SubLTreeR),
        search(PSQ, SplitKey, _),
        loser_split_keys_are_present(PSQ, SubLTreeL),
        loser_split_keys_are_present(PSQ, SubLTreeR)
    ).

%---------------------------------------------------------------------------%

is_finite_map(PSQ) :-
    (
        PSQ = empty_psqueue
    ;
        PSQ = nonempty_psqueue(Winner),
        Winner = winner(_, _, LTree, _),
        Keys = get_keys(LTree),
        list.sort_and_remove_dups(Keys, UniqKeys),
        list.length(Keys, NumKeys),
        list.length(UniqKeys, NumUniqKeys),
        NumKeys = NumUniqKeys
    ).

    % Return a list of the keys in the given loser tree, in no particular
    % order.
    %
:- func get_keys(loser_tree(P, K)) = list(K).

get_keys(LTree) = Keys :-
    (
        LTree = loser_leaf,
        Keys = []
    ;
        LTree = loser_node(_, _, Key, SubLTreeL, _, SubLTreeR),
        Keys = [Key | get_keys(SubLTreeL) ++ get_keys(SubLTreeR)]
    ).

%---------------------------------------------------------------------------%

dump_psqueue(PSQ) =
    dump_psqueue(0, PSQ).

verify_and_dump_psqueue(PSQ) = Str :-
    ( if
        is_semi_heap(PSQ),
        is_search_tree(PSQ),
        has_key_condition(PSQ),
        is_finite_map(PSQ)
    then
        Str = dump_psqueue(PSQ)
    else
        unexpected($pred, "verification failed")
    ).

:- func indent_string(int) = string.

indent_string(Indent) = IndentStr :-
    string.duplicate_char(' ', Indent * 2, IndentStr).

:- func dump_psqueue(int, psqueue(P, K)) = string.

dump_psqueue(Indent, PSQ) = Str :-
    IndentStr = indent_string(Indent),
    (
        PSQ = empty_psqueue,
        Str = IndentStr ++ "void"
    ;
        PSQ = nonempty_psqueue(Winner),
        Str = dump_winner(Indent, Winner)
    ).

:- func dump_winner(int, winner(P, K)) = string.

dump_winner(Indent, Winner) = Str :-
    IndentStr = indent_string(Indent),
    Winner = winner(WinnerPrio, WinnerKey, LTree, MaxKey),
    Str = IndentStr ++ "winner(prio " ++ string(WinnerPrio) ++
        ", key " ++ string(WinnerKey) ++
        ", maxkey " ++ string(MaxKey) ++ ",\n" ++
            dump_loser_tree(Indent + 1, LTree) ++
        IndentStr ++ ")\n".

:- func dump_loser_tree(int, loser_tree(P, K)) = string.

dump_loser_tree(Indent, LTree) = Str :-
    IndentStr = indent_string(Indent),
    (
        LTree = loser_leaf,
        Str = IndentStr ++ "loser_leaf\n"
    ;
        LTree = loser_node(Size, LoserPrio, LoserKey,
            LTreeL, SplitKey, LTreeR),
        Str = IndentStr ++ "loser_node(size " ++ int_to_string(Size) ++
            ", prio " ++ string(LoserPrio) ++
            ", key " ++ string(LoserKey) ++ "\n" ++
                dump_loser_tree(Indent + 1, LTreeL) ++
            IndentStr ++ "split key " ++ string(SplitKey) ++ "\n" ++
                dump_loser_tree(Indent + 1, LTreeR) ++
            IndentStr ++ ")\n"
    ).

:- func dump_tournament(int, tournament_view(P, K)) = string.

dump_tournament(Indent, Tournament) = Str :-
    IndentStr = indent_string(Indent),
    (
        Tournament = singleton_tournament(Prio, Key),
        Str = IndentStr ++ "singleton_tournament(key " ++ string(Key) ++
            ", prio " ++ string(Prio) ++ ")\n"
    ;
        Tournament = tournament_between(WinnerA, WinnerB),
        Str = IndentStr ++ "tournament_between(\n" ++
                dump_winner(Indent + 1, WinnerA) ++
            IndentStr ++ "and\n" ++
                dump_winner(Indent + 1, WinnerB) ++
            IndentStr ++ ")\n"
    ).

%---------------------------------------------------------------------------%
:- end_module psqueue.
%---------------------------------------------------------------------------%
