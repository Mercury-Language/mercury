%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury Team
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: psqueue.m.
% Main author: Matthias Güdemann.
% Stability: low.
%
% This module implements a priority search queue ADT.
%
% A priority search queue (pqueue) provides both map-like and priority queue
% functionality in a single ADT.  This combination is very powerful and
% useful in many situations.
%
% Psqueues map from priorities to keys and back.  They
% provide methods to lookup the priority of a key, insert and delete
% priority-key pairs, adjust the priority of a given key and retrieve the
% priority and key with the highest priority.
%
% The implementation here closely follows the description given in Ralf Hinze's
% paper "A Simple Implementation Technique for Priority Search Queues", ICFP
% 2001, pp. 110-121.
%
% The priority-key pairs are stored in a weight-balanced tree for efficient
% access.
%
% read highest priority element:       O(1)
% remove highest priority element      O(log n)
% delete/insert/ajdust/lookup element: O(log n)
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

    % create singleton psqueue
    %
:- pred singleton(P::in, K::in, psqueue(P, K)::out) is det.
:- func singleton(P, K) = psqueue(P, K).

    % Insert key K with priority P into a priority search queue.
    % Fail if the key already exists.
    %
:- pred insert(P::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is semidet.

    % Insert key K with priority P into a priority search queue.
    % Abort if the key already exists.
    %
:- func det_insert(psqueue(P, K), P, K) = psqueue(P, K) is det.
:- pred det_insert(P::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is det.

    % Peek at highest priority key, do not change the priority search queue.
    %
:- pred peek(psqueue(P, K)::in, P::out, K::out) is semidet.

    % As peek/3, will call error/1 if the psqueue is empty.
    %
:- pred det_peek(psqueue(P, K)::in, P::out, K::out) is det.

    % Remove element with minimal priority.
    %
:- pred remove_least(P::out, K::out, psqueue(P, K)::in, psqueue(P, K)::out)
    is semidet.

    % Remove element with minimal priority, call error/1 if priority search
    % queue is empty.
    %
:- pred det_remove_least(P::out, K::out, psqueue(P, K)::in, psqueue(P, K)::out)
    is det.

    % Create an ordered association list from a priority search queue.
    %
:- func to_assoc_list(psqueue(P, K)) = assoc_list(P, K).
:- pred to_assoc_list(psqueue(P, K)::in, assoc_list(P, K)::out) is det.

    % Create a priority search queue from an assoc_list of priority, key pairs
    %
:- func from_assoc_list(assoc_list(P, K)) = psqueue(P, K).
:- pred from_assoc_list(assoc_list(P, K)::in, psqueue(P, K)::out) is det.

    % Remove element with specific key from a priority queue.
    %
:- pred remove(P::out, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is semidet.

:- pred det_remove(P::out, K::in, psqueue(P, K)::in, psqueue(P, K)::out) is det.

    % Adjust priority of specified element. The old priority is given as an
    % argument to the adjustment function. Fails if the element is not
    % found.
    %
:- pred adjust((func(P) = P)::in, K::in, psqueue(P, K)::in, psqueue(P, K)::out)
    is semidet.

    % Search for the priority of the specified element.
    %
:- pred search(psqueue(P, K)::in, K::in, P::out) is semidet.

    % Lookup the priority of the specified key, calls error/1 if the element is
    % not present.
    %
:- func lookup(psqueue(P, K), K) = P.
:- pred lookup(psqueue(P, K)::in, K::in, P::out) is det.

    % Range query for all priority - key pairs less or equal to a specified
    % priority
    %
:- func at_most(psqueue(P, K), P) = assoc_list(P, K).
:- pred at_most(psqueue(P, K)::in, P::in, assoc_list(P, K)::out) is det.

    % Return the size of the priority search queue as the number of elements.
    %
:- func size(psqueue(P, K)) = int is det.
:- pred size(psqueue(P, K)::in, int::out) is det.

%---------------------------------------------------------------------------%

% These predicates may be used by the test suite to check the correctness of
% the implementation.  They should always be true.

    % True iff the priority search queue respects the semi heap properties:
    %
    %   1) the top element has the highest priority and
    %   2) for each node of the loser tree, the priority of the loser is higher
    %      or equal to the priorities in the subtree from which the loser
    %      originates.
    %
:- pred is_semi_heap(psqueue(P, K)::in) is semidet.

    % True iff the priority search queue respects the search tree properties:
    %
    %   1) for each node the keys in the left subtree are smaller as or equal
    %      to the split key and
    %   2) the keys in the right subtree are larger than the
    %      split key.
    %
:- pred is_search_tree(psqueue(P, K)::in) is semidet.

    % True iff maximal key and all split keys are present
    %
:- pred key_condition(psqueue(P, K)::in) is semidet.

    % True iff keys are unique.
    %
:- pred is_finite_map(psqueue(P, K)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

% The PSQueue data structure uses the 'tournament' metaphore.  Consider
% multiple competitors playing matches, with the winners from each match
% playing one another to find the champion.  The winner is the item with the
% lowest priority.  The data structure here follows a similar tree.  However,
% two modifications are made:
%
% + First, a tournament tree contains the data in the leaves of the tree and
%   repeats the winners within the tree.  To avoid this duplication we do not
%   store any information in the leaves and store the losers internally within
%   the tree.  The champion is stored at the root node of the tree.
%
% + To facilitate sorting by key, sort keys are stored inside each node.  The
%   items in the left subtree have keys less than or equal to the sort key, the
%   keys in the right subtree have keys greater than the sort key.  The loser
%   (stored in this node) is considered to be part of one of the two subtrees,
%   depending how it's key compares with the sort key.

:- type psqueue(P, K)
    --->    void
    ;       winner(
                w_key       :: K,
                w_prio      :: P,
                w_losers    :: ltree(K, P),
                w_max_key   :: K
            ).

:- type t_ltree_size == int.

:- type ltree(K, P)
    --->    start
    ;       loser(
                l_size          :: t_ltree_size,
                l_loser_key     :: K,
                l_loser_prio    :: P,
                l_left_tree     :: ltree(K, P),
                l_sort_key      :: K,
                l_right_tree    :: ltree(K, P)
            ).

%-----------------------------------------------------------------------%

    % Extract maximal (highest priority) key.
    %
:- pred max_key(psqueue(P, K)::in, K::out) is semidet.

max_key(PSQ, MaxKey) :-
    PSQ = winner(_, _, _, MaxKey).

    % Play tournament to combine two priority search queues.
    % See Ralf Hinze's paper for an explanation.
    %
:- pred tournament(psqueue(P, K)::in, psqueue(P, K)::in, psqueue(P, K)::out)
    is det.
:- pragma type_spec(tournament/3, P = int).

tournament(PSQ0, PSQ1, PSQ) :-
    PSQ = tournament(PSQ0, PSQ1).

:- func tournament(psqueue(P, K), psqueue(P, K)) = psqueue(P, K).
:- pragma type_spec(tournament/2, P = int).

tournament(PSQ1, PSQ2) = Res :-
    (
        PSQ1 = void,
        Res = PSQ2
    ;
        PSQ1 = winner(K1, Prio1, L1, MaxKey1),
        (
            PSQ2 = void,
            Res = PSQ1
        ;
            PSQ2 = winner(K2, Prio2, L2, MaxKey2),
            ( Prio1 `leq` Prio2 ->
                % left wins
                Res = winner(K1, Prio1,
                             balance(K2, Prio2, L1, MaxKey1, L2), MaxKey2)
            ;
                % right wins
                Res = winner(K2, Prio2,
                             balance(K1, Prio1, L1, MaxKey1, L2), MaxKey2)
            )
        )
    ).

:- func second_best(ltree(K, P), K) = psqueue(P, K) is det.

second_best(LTree, Key) = Res :-
    (
        LTree = start,
        Res = void
    ;
        LTree = loser(_, LK, LP, T, SplitKey, U),
        ( LK `leq` SplitKey ->
            T1 = winner(LK, LP, T, SplitKey),
            T2 = second_best(U, Key),
            Res = tournament(T1, T2)
        ;
            T1 = second_best(T, SplitKey),
            T2 = winner(LK, LP, U, Key),
            Res = tournament(T1, T2)
        )
    ).

%---------------------------------------------------------------------------%

    % create empty psqueue
    %
init = PSQ :-
    init(PSQ).

init(void).

    % check for empty psqueue
    %
is_empty(void).

singleton(P, K) = Res :-
    singleton(P, K, Res).

singleton(P, K, PSQ) :-
    PSQ = winner(K, P, start, K).

%-----------------------------------------------------------------------%

insert(P, K, !PSQ) :-
    insert_tv(K, P, tournament_view(!.PSQ), !:PSQ).

det_insert(P, K, !PSQ) :-
    ( insert(P, K, !PSQ) ->
        true
    ;
        unexpected($file, $pred,
            "error on inserting element into priority search queue")
    ).

det_insert(PSQ0, P, K) = PSQ :-
    det_insert(P, K, PSQ0, PSQ).

:- pred insert_tv(K::in, P::in,
    t_tournament_view(K, P)::in, psqueue(P, K)::out) is semidet.

insert_tv(IK, IP, TV, Res) :-
    (
        TV = emptySet,
        Res = psqueue.singleton(IP, IK)
    ;
        TV = singleton(Key, Prio),
        compare(CMP, IK, Key),
        (
            CMP = (<),
            Res = tournament(psqueue.singleton(IP, IK),
                psqueue.singleton(Prio, Key))
        ;
            CMP = (>),
            Res = tournament(psqueue.singleton(Prio, Key),
                psqueue.singleton(IP, IK))
        )
    ;
        TV = tournament_between(T1, T2),
        T1 = winner(_, _, _, MaxKey1),
        T2 = winner(_, _, _, _),
        ( IK `leq` MaxKey1 ->
            insert(IP, IK, T1, Left),
            Res = tournament(Left, T2)
        ;
            insert(IP, IK, T2, Right),
            Res = tournament(T1, Right)
        )
    ).

%-----------------------------------------------------------------------%

peek(PSQ, MinPrio, MinKey) :-
    PSQ = winner(MinKey, MinPrio, _, _).

det_peek(PSQ, MinPrio, MinKey) :-
    ( peek(PSQ, MinPrio0, MinKey0) ->
        MinKey = MinKey0,
        MinPrio = MinPrio0
    ;
        unexpected($file, $pred, "priority search queue is empty")
    ).

remove_least(MinPrio, MinKey, PSQ, NewPSQ) :-
    PSQ = winner(MinKey, MinPrio, L, MaxKey),
    NewPSQ = second_best(L, MaxKey).

det_remove_least(MinPrio, MinKey, PSQ, NewPSQ) :-
    ( remove_least(MinPrio0, MinKey0, PSQ, NewPSQ0) ->
        NewPSQ = NewPSQ0,
        MinKey = MinKey0,
        MinPrio = MinPrio0
    ;
        unexpected($file, $pred, "priority search queue is empty")
    ).

%-----------------------------------------------------------------------%

to_assoc_list(PSQ) = Res :-
    to_assoc_list(PSQ, Res).

to_assoc_list(PSQ, AList) :-
    ( remove_least(K, P, PSQ, PSQ0) ->
        to_assoc_list(PSQ0, AList0),
        AList = [K - P | AList0]
    ;
        AList = []
    ).

from_assoc_list(AList) = Res :-
    from_assoc_list(AList, Res).

from_assoc_list(AList, PSQ) :-
    from_assoc_list2(AList, init, PSQ).

:- pred from_assoc_list2(assoc_list(P, K)::in, psqueue(P, K)::in,
                       psqueue(P, K)::out) is det.

from_assoc_list2([], !PSQ).
from_assoc_list2([(Prio - Key) | Rest], !PSQ) :-
    det_insert(Prio, Key, !PSQ),
    from_assoc_list2(Rest, !PSQ).

%-----------------------------------------------------------------------%

remove(P, K, !PSQ) :-
    remove_tv(P, K, tournament_view(!.PSQ), !:PSQ).

det_remove(P, K, !PSQ) :-
    ( remove(PPrime, K, !.PSQ, PSQPrime) ->
        P = PPrime,
        !:PSQ = PSQPrime
    ;
        unexpected($file, $pred, "element not found")
    ).

:- pred remove_tv(P::out, K::in,
    t_tournament_view(K, P)::in, psqueue(P, K)::out) is semidet.

remove_tv(Prio, Key, TV, Res) :-
    (
        TV = emptySet,
        false
    ;
        TV = singleton(Key, Prio),
        Res = void
    ;
        TV = tournament_between(TL, TR),
        TL = winner(_, _, _, MaxKey1),
        ( Key `leq` MaxKey1 ->
            remove(Prio, Key, TL, Left),
            Res = tournament(Left, TR)
        ;
            remove(Prio, Key, TR, Right),
            Res = tournament(TL, Right)
        )
    ).

%---------------------------------------------------------------------------%

:- pred leq(V::in, V::in) is semidet.
:- pragma type_spec(leq/2, V = int).

leq(ValLeft, ValRight) :-
    compare(CMP, ValLeft, ValRight),
    ( CMP = (<)
    ; CMP = (=)
    ).

%-----------------------------------------------------------------------%

adjust(F, K, !PSQ) :-
    adjust_tv(F, K, tournament_view(!.PSQ), !:PSQ).

:- pred adjust_tv(func(P) = P, K, t_tournament_view(K, P), psqueue(P, K)).
:- mode adjust_tv(func(in) = out is det, in, in, out) is semidet.

adjust_tv(Func, K, TV, Res) :-
    (
        TV = emptySet,
        false
    ;
        TV = singleton(Key, Prio),
        ( K = Key ->
            Res = psqueue.singleton(Func(Prio), Key)
        ;
            Res = psqueue.singleton(Prio, Key)
        )
    ;
        TV = tournament_between(TL, TR),
        TL = winner(_, _, _, MaxKey1),
        ( K `leq` MaxKey1 ->
            adjust(Func, K, TL, Left),
            Res = tournament(Left, TR)
        ;
            adjust(Func, K, TR, Right),
            Res = tournament(TL, Right)
        )
    ).

%---------------------------------------------------------------------------%

search(PSQ, K, P) :-
    search_tv(tournament_view(PSQ), K, P).

:- pred search_tv(t_tournament_view(K, P)::in, K::in, P::out) is semidet.

search_tv(TV, K, Res) :-
    (
        TV = singleton(Key, Prio),
        Key = K,
        Res = Prio
    ;
        TV = tournament_between(TL, TR),
        TL = winner(_, _, _, MaxKey1),
        ( K `leq` MaxKey1 ->
            search(TL, K, Res)
        ;
            search(TR, K, Res)
        )
    ).

lookup(PSQ, K, P) :-
    ( search(PSQ, K, PPrime) ->
        P = PPrime
    ;
        unexpected($file, $pred, "element not found")
    ).

lookup(PSQ, K) = P :-
    lookup(PSQ, K, P).

%-----------------------------------------------------------------------%

at_most(PSQ, P) = Res :-
    at_most(PSQ, P, Res).

at_most(PSQ, Pt, AList) :-
    MView = min_view(PSQ),
    (
        MView = empty,
        AList = []
    ;
        MView = min(_, Prio, _),
        compare(CMP, Prio, Pt),
        (
            CMP = (>),
            AList = []
        ;
            ( CMP = (=)
            ; CMP = (<)
            ),
            TView = tournament_view(PSQ),
            (
                TView = emptySet,
                AList = []
            ;
                TView = singleton(Prio0, Key0),
                AList = [Key0 - Prio0]
            ;
                TView = tournament_between(T1, T2),
                at_most(T1, Pt, AL0),
                at_most(T2, Pt, AL1),
                AList = AL0 ++ AL1
            )
        )
    ).

size(PSQ, Size) :-
    (
        PSQ = void,
        Size = 0
    ;
        PSQ = winner(_, _, LTree, _),
        Size = ltree_size(LTree)
    ).

size(PSQ) = Res :-
    size(PSQ, Res).

:- func ltree_size(ltree(K, P)) = t_ltree_size.

ltree_size(LTree) = Res :-
    (
        LTree = start, Res = 0
    ;
        LTree = loser(Res, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%
% view types for min view, tournament view and tree view
%---------------------------------------------------------------------------%

:- type t_min_view(K, P)
    --->        empty
    ;           min(K, P, psqueue(P, K)).

:- type t_tournament_view(K, P)
    --->        emptySet
    ;           singleton(K, P)
    ;           tournament_between(psqueue(P, K), psqueue(P, K)).

:- type t_tree_view(K, P)
    --->        leaf
    ;           node(K, P, ltree(K, P), K, ltree(K, P)).

%---------------------------------------------------------------------------%

    % get min view of priority search queue
    %
:- func min_view(psqueue(P, K)) = t_min_view(K, P).

min_view(PSQ) = Res :-
    (
        PSQ = void,
        Res = empty
    ;
        PSQ = winner(Key, Prio, LTree, MaxKey),
        Res = min(Key, Prio, second_best(LTree, MaxKey))
    ).

    % get tournament view of priority search queue
    %
:- func tournament_view(psqueue(P, K)) = t_tournament_view(K, P).

tournament_view(PSQ) = Res :-
    (
        PSQ = void,
        Res = emptySet
    ;
        PSQ = winner(K, P, LTree, MaxKey),
        (
            LTree = start,
            Res = singleton(K, P)
        ;
            LTree = loser(_, LK, LP, TL, SplitKey, TR),
            ( LK `leq` SplitKey ->
                Res = tournament_between(winner(LK, LP, TL, SplitKey),
                                     winner(K, P, TR, MaxKey))
            ;
                Res = tournament_between(winner(K, P, TL, SplitKey),
                                     winner(LK, LP, TR, MaxKey))
            )
        )
    ).


    % get tree view of priority search queue
    %
:- func tree_view(ltree(K, P)) = t_tree_view(K, P) is det.

tree_view(LTree) = Res :-
    (
        LTree = start,
        Res = leaf
    ;
        LTree = loser(_, LK, LP, LL, SplitKey, LR),
        Res = node(LK, LP, LL, SplitKey, LR)
    ).

%---------------------------------------------------------------------------%
% smart constructors
%---------------------------------------------------------------------------%

:- func construct_leaf = ltree(K, P).
construct_leaf = start.

:- func construct_node(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P).
construct_node(Key, Prio, L, SplitKey, R) = Res :-
    Size = 1 + ltree_size(L) + ltree_size(R),
    Res = loser(Size, Key, Prio, L, SplitKey, R).


%---------------------------------------------------------------------------%
% balancing functions for weight balanced trees
%---------------------------------------------------------------------------%

    % balance factor, must be over 3.75 (see Ralf Hinze's paper)
    %
:- func balance_omega = t_ltree_size.
balance_omega = 4.

:- func balance(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_left(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_right(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func single_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P)
    is det.
:- func single_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P)
    is det.
:- func double_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P)
    is det.
:- func double_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P)
    is det.

balance(Key, Prio, L, SplitKey, R) = Res :-
    SizeL = ltree_size(L),
    SizeR = ltree_size(R),
    ( (SizeR + SizeL) < 2 ->
        Res = construct_node(Key, Prio, L, SplitKey, R)
    ;
        (( compare(CMP, SizeR, balance_omega * SizeL), CMP = (>)) ->
            Res = balance_left(Key, Prio, L, SplitKey, R)
        ;
            (( compare(CMP, SizeL, balance_omega * SizeR), CMP = (>)) ->
                Res = balance_right(Key, Prio, L, SplitKey, R)
            ;
                Res = construct_node(Key, Prio, L, SplitKey, R)
            )
        )
    ).

balance_left(Key, Prio, L, SplitKey, R) = Res :-
    TVR = tree_view(R),
    ( TVR = node(_, _, RL, _, RR) ->
        ( (compare(CMP, ltree_size(RL), ltree_size(RR)), CMP = (<)) ->
            Res = single_left(Key, Prio, L, SplitKey, TVR)
        ;
            Res = double_left(Key, Prio, L, SplitKey, TVR)
        )
    ;
        unexpected($file, $pred, "error in left balance")
    ).

balance_right(Key, Prio, L, SplitKey, R) = Res :-
    TVL = tree_view(L),
    ( TVL = node(_, _, LL, _, LR) ->
        ( (compare(CMP, ltree_size(LR), ltree_size(LL)), CMP = (<)) ->
            Res = single_right(Key, Prio, TVL, SplitKey, R)
        ;
            Res = double_right(Key, Prio, TVL, SplitKey, R)
        )
    ;
        unexpected($file, $pred, "error in right balance")
    ).

single_left(K1, P1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, T2, S2, T3) ->
        ( ( K2 `leq` S2, P1 `leq` P2 ) ->
            Res = construct_node(K1, P1,
                                 construct_node(K2, P2, T1, S1, T2), S2, T3)
        ;
            Res = construct_node(K2, P2,
                                 construct_node(K1, P1, T1, S1, T2), S2, T3)
        )
    ;
        unexpected($file, $pred, "error in single left rotation")
    ).

single_right(K1, P1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, T1, S1, T2) ->
        ( ( compare(CMP0, K2, S1), CMP0 = (>), P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, T1, S1,
                                 construct_node(K2, P2, T2, S2, T3))
        ;
            Res = construct_node(K2, P2, T1, S1,
                                 construct_node(K1, P1, T2, S2, T3))
        )
    ;
        unexpected($file, $pred, "error in single right rotation")
    ).

double_left(K1, P1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, T2, S2, T3) ->
        Res = single_left(K1, P1, T1, S1,
                          tree_view(single_right(K2, P2,
                                                 tree_view(T2), S2, T3)))
    ;
        unexpected($file, $pred, "error in doulbe left rotation")
    ).

double_right(K1, P1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, T1, S1, T2) ->
        Res = single_right(K1, P1,
                           tree_view(single_left(K2, P2, T1, S1,
                                                 tree_view(T2))),
                           S2, T3)
    ;
        unexpected($file, $pred, "error in double right rotation")
    ).

%---------------------------------------------------------------------------%
% test predicates for correct implementation of psqueue
%---------------------------------------------------------------------------%

is_semi_heap(PSQ) :-
    (
        PSQ = void
    ;
        PSQ = winner(_, Prio, LTree, _),
        all_keys_larger_ltree(Prio, LTree),
        all_nodes_loser_prio(LTree)
    ).

:- pred all_keys_larger_ltree(P::in, ltree(K, P)::in) is semidet.

all_keys_larger_ltree(Prio, LTree) :-
    (
        LTree = start
    ;
        LTree = loser(_, _, LP, LT, _, RT),
        Prio `leq` LP,
        all_keys_larger_ltree(Prio, LT),
        all_keys_larger_ltree(Prio, RT)
    ).

:- func min(V, V) = V is det.

min(P1, P2) = Res :-
    ( P1 `leq` P2 ->
        Res = P1
    ;
        Res = P2
    ).

:- func max(V, V) = V is det.

max(P1, P2) = Res :-
    ( P1 `leq` P2 ->
        Res = P2
    ;
        Res = P1
    ).

:- pred min_prio_loser_tree(ltree(K, P)::in, maybe(P)::out) is det.

min_prio_loser_tree(LTree, MinPrio) :-
    (
        LTree = start,
        MinPrio = no
    ;
        LTree = loser(_, _, Prio, TL, _, TR),
        min_prio_loser_tree(TL, Prio, MinPrio1),
        min_prio_loser_tree(TR, Prio, MinPrio2),
        (
            MinPrio1 = no,
            MinPrio2 = no,
            MinPrio = yes(Prio)
        ;
            MinPrio1 = yes(MinPrio1Val),
            MinPrio2 = no,
            MinPrio = yes(min(MinPrio1Val, Prio))
        ;
            MinPrio2 = yes(MinPrio2Val),
            MinPrio1 = no,
            MinPrio = yes(min(MinPrio2Val, Prio))
        ;
            MinPrio1 = yes(MinPrio1Val),
            MinPrio2 = yes(MinPrio2Val),
            MinPrio = yes(min(MinPrio1Val,
                          min(Prio, MinPrio2Val)))
        )
    ).

:- pred min_prio_loser_tree(ltree(K, P)::in, P::in, maybe(P)::out) is det.

min_prio_loser_tree(LTree, CurrMin, MinPrio) :-
    (
        LTree = start,
        MinPrio = no
    ;
        LTree = loser(_, _, Prio, TL, _, TR),
        ( CurrMin `leq` Prio ->
            NewPrio = CurrMin
        ;
            NewPrio = Prio
        ),
        min_prio_loser_tree(TL, NewPrio, MinPrio1),
        min_prio_loser_tree(TR, NewPrio, MinPrio2),
        (
            MinPrio1 = no,
            MinPrio2 = no,
            MinPrio = yes(NewPrio)
        ;
            MinPrio1 = yes(MinPrio1Val),
            MinPrio2 = no,
            MinPrio = yes(min(MinPrio1Val, NewPrio))
        ;
            MinPrio2 = yes(MinPrio2Val),
            MinPrio1 = no,
            MinPrio = yes(min(MinPrio2Val, NewPrio))
        ;
            MinPrio1 = yes(MinPrio1Val),
            MinPrio2 = yes(MinPrio2Val),
            MinPrio = yes(min(MinPrio1Val,
                          min(MinPrio2Val, NewPrio)))
        )
    ).

:- pred all_nodes_loser_prio(ltree(K, P)::in) is semidet.

all_nodes_loser_prio(LTree) :-
    (
        LTree = start
    ;
        LTree = loser(_, K, Prio, TL, SplitKey, TR),
        ( K `leq` SplitKey ->
            min_prio_loser_tree(TL, Prio, MinPrio)
        ;
            min_prio_loser_tree(TR, Prio, MinPrio)
        ),
        ( MinPrio = no ->
            MinPrio0 = Prio
        ;
            MinPrio = yes(MinPrio0)
        ),
        compare(CMP, Prio, MinPrio0),
        CMP = (=),
        all_nodes_loser_prio(TL),
        all_nodes_loser_prio(TR)
    ).

%-----------------------------------------------------------------------%

is_search_tree(PSQ) :-
    (
        PSQ = void
    ;
        PSQ = winner(_, _, LTree, _),
        all_search_keys(LTree)
    ).

:- pred all_search_keys(ltree(K, P)::in) is semidet.

all_search_keys(LTree) :-
    (
        LTree = start
    ;
        LTree = loser(_, _, _, TL, SplitKey, TR),
        max_key_loser_tree(TL, MaxKeyL),
        min_key_loser_tree(TR, MinKeyR),
        (
            MaxKeyL = no
        ;
            MaxKeyL = yes(MaxKey),
            MaxKey `leq` SplitKey,
            all_search_keys(TL)
        ),
        (
            MinKeyR = no
        ;
            MinKeyR = yes(MinKey),
            compare(CMP, MinKey, SplitKey),
            CMP = (>),
            all_search_keys(TR)
        )
    ).

:- pred min_key_loser_tree(ltree(K, P)::in, maybe(K)::out) is det.

min_key_loser_tree(LTree, MinKey) :-
    (
        LTree = start,
        MinKey = no
    ;
        LTree = loser(_, Key, _, TL, _, TR),
        min_key_loser_tree(TL, Key, MinKey1),
        min_key_loser_tree(TR, Key, MinKey2),
        (
            MinKey1 = no,
            MinKey2 = no,
            MinKey = yes(Key)
        ;
            MinKey1 = yes(MinKey1Val),
            MinKey2 = no,
            MinKey = yes(min(MinKey1Val, Key))
        ;
            MinKey2 = yes(MinKey2Val),
            MinKey1 = no,
            MinKey = yes(min(MinKey2Val, Key))
        ;
            MinKey1 = yes(MinKey1Val),
            MinKey2 = yes(MinKey2Val),
            MinKey = yes(min(MinKey1Val,
                         min(Key, MinKey2Val)))
        )
    ).

:- pred min_key_loser_tree(ltree(K, P)::in, K::in, maybe(K)::out) is det.

min_key_loser_tree(LTree, CurrMin, MinKey) :-
    (
        LTree = start, MinKey = no
    ;
        LTree = loser(_, Key, _, TL, _, TR),
        ( CurrMin `leq` Key ->
            NewKey = CurrMin
        ;
            NewKey = Key
        ),
        min_key_loser_tree(TL, NewKey, MinKey1),
        min_key_loser_tree(TR, NewKey, MinKey2),
        (
            MinKey1 = no,
            MinKey2 = no,
            MinKey = yes(NewKey)
        ;
            MinKey1 = yes(MinKey1Val),
            MinKey2 = no,
            MinKey = yes(min(MinKey1Val, NewKey))
        ;
            MinKey2 = yes(MinKey2Val),
            MinKey1 = no,
            MinKey = yes(min(MinKey2Val, NewKey))
        ;
            MinKey1 = yes(MinKey1Val),
            MinKey2 = yes(MinKey2Val),
            MinKey = yes(min(MinKey1Val,
                         min(MinKey2Val, NewKey)))
        )
    ).

:- pred max_key_loser_tree(ltree(K, P)::in, maybe(K)::out) is det.

max_key_loser_tree(LTree, MaxKey) :-
    (
        LTree = start,
        MaxKey = no
    ;
        LTree = loser(_, Key, _, TL, _, TR),
        max_key_loser_tree(TL, Key, MaxKey1),
        max_key_loser_tree(TR, Key, MaxKey2),
        (
            MaxKey1 = no,
            MaxKey2 = no,
            MaxKey = yes(Key)
        ;
            MaxKey1 = yes(MaxKey1Val),
            MaxKey2 = no,
            MaxKey = yes(max(MaxKey1Val, Key))
        ;
            MaxKey2 = yes(MaxKey2Val),
            MaxKey1 = no,
            MaxKey = yes(max(MaxKey2Val, Key))
        ;
            MaxKey1 = yes(MaxKey1Val),
            MaxKey2 = yes(MaxKey2Val),
            MaxKey = yes(max(MaxKey1Val,
                         max(Key, MaxKey2Val)))
        )
    ).

:- pred max_key_loser_tree(ltree(K, P)::in, K::in, maybe(K)::out) is det.

max_key_loser_tree(LTree, CurrMax, MaxKey) :-
    (
        LTree = start, MaxKey = no
    ;
        LTree = loser(_, Key, _, TL, _, TR),
        compare(CMP, CurrMax, Key),
        (
            ( CMP = (=)
            ; CMP = (>)
            ),
            NewKey = CurrMax
        ;
            CMP = (<),
            NewKey = Key
        ),
        max_key_loser_tree(TL, NewKey, MaxKey1),
        max_key_loser_tree(TR, NewKey, MaxKey2),
        (
            MaxKey1 = no,
            MaxKey2 = no,
            MaxKey = yes(NewKey)
        ;
            MaxKey1 = yes(MaxKey1Val),
            MaxKey2 = no,
            MaxKey = yes(max(MaxKey1Val, NewKey))
        ;
            MaxKey2 = yes(MaxKey2Val),
            MaxKey1 = no,
            MaxKey = yes(max(MaxKey2Val, NewKey))
        ;
            MaxKey1 = yes(MaxKey1Val),
            MaxKey2 = yes(MaxKey2Val),
            MaxKey = yes(max(MaxKey1Val,
                         max(MaxKey2Val, NewKey)))
        )
    ).

%-----------------------------------------------------------------------%

key_condition(PSQ) :-
    (
        PSQ = void
    ;
        PSQ = winner(_, _, T, MaxKey),
        search(PSQ, MaxKey, _),
        key_condition(PSQ, T)
    ).

:- pred key_condition(psqueue(P, K)::in, ltree(K, P)::in) is semidet.

key_condition(PSQ, T) :-
    (
        T = start
    ;
        T = loser(_, _, _, TL, SplitKey, TR),
        search(PSQ, SplitKey, _),
        key_condition(PSQ, TL),
        key_condition(PSQ, TR)
    ).

%-----------------------------------------------------------------------%

is_finite_map(PSQ) :-
    (
        PSQ = void
    ;
        PSQ = winner(_, _, T, _),
        KeyList = get_keys(T),
        UniqList = list.sort_and_remove_dups(KeyList),
        length(KeyList, LK),
        length(UniqList, LUK),
        LK = LUK
    ).

:- func get_keys(ltree(K, P)) = list(K).

get_keys(T) = Res :-
    (
        T = start,
        Res = []
    ;
        T = loser(_, K, _, TL, _, TR),
        Res = [K | get_keys(TL) ++ get_keys(TR)]
    ).

%---------------------------------------------------------------------------%
:- end_module psqueue.
%---------------------------------------------------------------------------%
