%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2000, 2003-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2019, 2021, 2023-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rbtree.m.
% Main author: petdr.
% Stability: high.
%
% The file implements the 'map' abstract data type using red/black trees,
% a version of binary search trees that ensures that the tree always stays
% roughly in balance.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rbtree.
:- interface.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

:- type rbtree(K, V).

    % Initialise the data structure.
    %
:- func init = rbtree(K, V).
:- pred init(rbtree(K, V)::uo) is det.

    % Check whether a tree is empty.
    %
:- pred is_empty(rbtree(K, V)::in) is semidet.

    % Initialise an rbtree containing the given key-value pair.
    %
:- func singleton(K, V) = rbtree(K, V).

    % Inserts a new key-value pair into the tree.
    % Fails if the key is already in the tree.
    %
:- pred insert(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Updates the value associated with the given key.
    % Fails if the key is not already in the tree.
    %
:- pred update(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Update the value for the given key by applying the given transformation
    % to the old value. Fails if the key is not already in the tree.
    % This is faster than first searching for the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Sets a value regardless of whether key exists or not.
    %
:- func set(rbtree(K, V), K, V) = rbtree(K, V).
:- pred set(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

    % Insert a duplicate key into the tree.
    %
    % Note: search does not support looking for duplicate keys.
    %
:- func insert_duplicate(rbtree(K, V), K, V) = rbtree(K, V).
:- pred insert_duplicate(K::in, V::in,
    rbtree(K, V)::in, rbtree(K, V)::out) is det.

:- pred member(rbtree(K, V)::in, K::out, V::out) is nondet.

    % Search for the value stored with the given the key.
    % Fails if the key is not in the tree.
    %
:- pred search(rbtree(K, V)::in, K::in, V::out) is semidet.

    % Looks ip the value stored with the given the key.
    % Throws an exception if the key is not in the tree.
    %
:- func lookup(rbtree(K, V), K) = V.
:- pred lookup(rbtree(K, V)::in, K::in, V::out) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(rbtree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Throws an exception if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(rbtree(K, V)::in, K::in, K::out, V::out) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(rbtree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Throws an exception if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(rbtree(K, V)::in, K::in, K::out, V::out) is det.

    % Delete the given key and its associated value from the tree.
    % Do nothing if the key is not in the tree.
    %
:- func delete(rbtree(K, V), K) = rbtree(K, V).
:- pred delete(K::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

    % Delete the given key and its associated value from the tree.
    % Fail if the key is not in the tree.
    %
:- pred remove(K::in, V::out, rbtree(K, V)::in, rbtree(K, V)::out)
    is semidet.

    % Deletes the node with the minimum key from the tree,
    % and returns the key and value fields.
    %
:- pred remove_smallest(K::out, V::out,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Deletes the node with the maximum key from the tree,
    % and returns the key and value fields.
    %
:- pred remove_largest(K::out, V::out,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Returns an in-order list of all the keys in the rbtree.
    %
:- func keys(rbtree(K, V)) = list(K).
:- pred keys(rbtree(K, V)::in, list(K)::out) is det.

    % Returns a list of values such that the keys associated with the
    % values are in-order.
    %
:- func values(rbtree(K, V)) = list(V).
:- pred values(rbtree(K, V)::in, list(V)::out) is det.

    % Count the number of elements in the tree.
    %
:- func count(rbtree(K, V)) = int.
:- pred count(rbtree(K, V)::in, int::out) is det.
:- func ucount(rbtree(K, V)) = uint.
:- pred ucount(rbtree(K, V)::in, uint::out) is det.

:- func assoc_list_to_rbtree(assoc_list(K, V)) = rbtree(K, V).
:- pred assoc_list_to_rbtree(assoc_list(K, V)::in, rbtree(K, V)::out) is det.

:- func from_assoc_list(assoc_list(K, V)) = rbtree(K, V).

:- func rbtree_to_assoc_list(rbtree(K, V)) = assoc_list(K, V).
:- pred rbtree_to_assoc_list(rbtree(K, V)::in, assoc_list(K, V)::out)
    is det.

:- func to_assoc_list(rbtree(K, V)) = assoc_list(K, V).

:- func foldl(func(K, V, A) = A, rbtree(K, V), A) = A.
:- pred foldl(pred(K, V, A, A), rbtree(K, V), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out)
    is semidet.
:- mode foldl(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode foldl(in(pred(in, in, di, uo) is semidet), in, di, uo)
    is semidet.

:- pred foldl2(pred(K, V, A, A, B, B), rbtree(K, V), A, A, B, B).
:- mode foldl2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldl2(in(pred(in, in, di, uo, di, uo) is det),
    in, di, uo, di, uo) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

:- pred foldl3(pred(K, V, A, A, B, B, C, C), rbtree(K, V),
    A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, in, out, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo) is det.
:- mode foldl3(in(pred(in, in, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo) is det.

:- pred foldl_values(pred(V, A, A), rbtree(K, V), A, A).
:- mode foldl_values(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl_values(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl_values(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl_values(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl_values(in(pred(in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode foldl_values(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- pred foldl2_values(pred(V, A, A, B, B), rbtree(K, V), A, A, B, B).
:- mode foldl2_values(in(pred(in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldl2_values(in(pred(in, in, out, mdi, muo) is det), in, in, out,
    mdi, muo) is det.
:- mode foldl2_values(in(pred(in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldl2_values(in(pred(in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldl2_values(in(pred(in, in, out, mdi, muo) is semidet), in, in, out,
    mdi, muo) is semidet.
:- mode foldl2_values(in(pred(in, in, out, di, uo) is semidet), in, in, out,
    di, uo) is semidet.

:- func foldr(func(K, V, A) = A, rbtree(K, V), A) = A.
:- pred foldr(pred(K, V, A, A), rbtree(K, V), A, A).
:- mode foldr(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.

:- pred foldr2(pred(K, V, A, A, B, B), rbtree(K, V), A, A, B, B).
:- mode foldr2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldr2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldr2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldr2(in(pred(in, in, di, uo, di, uo) is det),
    in, di, uo, di, uo) is det.
:- mode foldr2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldr2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldr2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

:- pred foldr_values(pred(V, A, A), rbtree(K, V), A, A).
:- mode foldr_values(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr_values(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr_values(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr_values(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr_values(in(pred(in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode foldr_values(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- func map_values(func(K, V1) = V2, rbtree(K, V1)) = rbtree(K, V2).
:- pred map_values(pred(K, V1, V2), rbtree(K, V1), rbtree(K, V2)).
:- mode map_values(in(pred(in, in, out) is det), in, out) is det.
:- mode map_values(in(pred(in, in, out) is semidet), in, out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees:
%   * The root node cannot be red.
%   * There can never be 2 red nodes in a row.

:- type rbtree(K, V)
    --->    empty
    ;       red(K, V, rbtree(K, V), rbtree(K, V))
    ;       black(K, V, rbtree(K, V), rbtree(K, V)).

%---------------------------------------------------------------------------%

init = RBT :-
    rbtree.init(RBT).

init(empty).

is_empty(Tree) :-
    Tree = empty.

singleton(K, V) = black(K, V, empty, empty).

%---------------------------------------------------------------------------%

insert(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        Tree = black(K, V, empty, empty)
    ;
        Tree0 = red(_, _, _, _),
        error($pred, "root node should not be red!")
    ;
        Tree0 = black(_, _, _, _),
        insert_into_node(K, V, Tree0, Tree1),
        % Ensure that the root of the tree is black.
        (
            Tree1 = black(_, _, _, _),
            Tree = Tree1
        ;
            Tree1 = red(K1, V1, L1, R1),
            Tree = black(K1, V1, L1, R1)
        ;
            Tree1 = empty,
            error($pred, "new tree is empty")
        )
    ).

    % insert_into_node:
    %
    % We traverse down the tree until we find the correct spot to insert.
    % Then as we fall back out of the recursions we look for possible
    % rotation cases.
    %
:- pred insert_into_node(K::in, V::in,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

insert_into_node(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        % Red node always inserted at the bottom as it will be rotated into the
        % correct place as we move back up the tree.
        Tree = red(K, V, empty, empty)
    ;
        Tree0 = red(K0, V0, L0, R0),
        % Work out which side of the rbtree to insert.
        compare(Result, K, K0),
        (
            Result = (<),
            insert_into_node(K, V, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            insert_into_node(K, V, R0, R),
            Tree = red(K0, V0, L0, R)
        ;
            Result = (=),
            fail
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        % We only ever need to look for a possible rotation if we are
        % in a black node. The rotation criteria is when there are two
        % red nodes in a row.
        ( if
            L0 = red(LK, LV, LL, LR),
            R0 = red(RK, RV, RL, RR)
        then
            % On the way down the rbtree we split any 4-nodes we find.
            % This converts the current node to a red node, so we call
            % the red node version of insert_into_node/4.
            % XXX "red node version" does NOT help explain what is happening.
            L1 = black(LK, LV, LL, LR),
            R1 = black(RK, RV, RL, RR),
            Tree1 = red(K0, V0, L1, R1),
            insert_into_node(K, V, Tree1, Tree)
        else
            % Work out which side of the rbtree to insert into.
            compare(Result, K, K0),
            (
                Result = (<),
                insert_into_node(K, V, L0, L),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    L = red(LK, LV, LL, LR)
                then
                    % Check to see if a grandchild is red,
                    % and if it is, rotate.
                    ( if LL = red(_LLK, _LLV, _LLL, _LLR) then
                        TreeR = red(K0, V0, LR, R0),
                        Tree = black(LK, LV, LL, TreeR)
                    else if LR = red(LRK, LRV, LRL, LRR) then
                        TreeL = red(LK, LV, LL, LRL),
                        TreeR = red(K0, V0, LRR, R0),
                        Tree = black(LRK, LRV, TreeL, TreeR)
                    else
                        Tree = black(K0, V0, L, R0)
                    )
                else
                    Tree = black(K0, V0, L, R0)
                )
            ;
                Result = (>),
                insert_into_node(K, V, R0, R),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    R = red(RK, RV, RL, RR)
                then
                    % Check to see if a grandchild is red,
                    % and if it is, rotate.
                    ( if RL = red(RLK, RLV, RLL, RLR) then
                        TreeL = red(K0, V0, L0, RLL),
                        TreeR = red(RK, RV, RLR, RR),
                        Tree = black(RLK, RLV, TreeL, TreeR)
                    else if RR = red(_RRK, _RRV, _RRL, _RRR) then
                        TreeL = red(K0, V0, L0, RL),
                        Tree = black(RK, RV, TreeL, RR)
                    else
                        Tree = black(K0, V0, L0, R)
                    )
                else
                    Tree = black(K0, V0, L0, R)
                )
            ;
                Result = (=),
                fail
            )
        )
    ).

%---------------------------------------------------------------------------%

update(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        fail
    ;
        Tree0 = red(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            Tree = red(K, V, L0, R0)
        ;
            Result = (<),
            rbtree.update(K, V, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            rbtree.update(K, V, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            Tree = black(K, V, L0, R0)
        ;
            Result = (<),
            rbtree.update(K, V, L0, L),
            Tree = black(K0, V0, L, R0)
        ;
            Result = (>),
            rbtree.update(K, V, R0, R),
            Tree = black(K0, V0, L0, R)
        )
    ).

%---------------------------------------------------------------------------%

transform_value(P, K, Tree0, Tree) :-
    (
        Tree0 = empty,
        fail
    ;
        Tree0 = red(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            P(V0, V),
            Tree = red(K0, V, L0, R0)
        ;
            Result = (<),
            rbtree.transform_value(P, K, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            rbtree.transform_value(P, K, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            P(V0, V),
            Tree = black(K0, V, L0, R0)
        ;
            Result = (<),
            rbtree.transform_value(P, K, L0, L),
            Tree = black(K0, V0, L, R0)
        ;
            Result = (>),
            rbtree.transform_value(P, K, R0, R),
            Tree = black(K0, V0, L0, R)
        )
    ).

%---------------------------------------------------------------------------%

set(!.RBT, K, V) = !:RBT :-
    rbtree.set(K, V, !RBT).

set(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        Tree = black(K, V, empty, empty)
    ;
        Tree0 = red(_, _, _, _),
        error($pred, "root node should not be red!")
    ;
        Tree0 = black(_, _, _, _),
        set_in_node(K, V, Tree0, Tree1),
        % Ensure that the root of the tree is black.
        (
            Tree1 = black(_, _, _, _),
            Tree = Tree1
        ;
            Tree1 = red(K1, V1, L1, R1),
            Tree = black(K1, V1, L1, R1)
        ;
            Tree1 = empty,
            error($pred, "new tree is empty")
        )
    ).

    % set_in_node:
    %
    % We traverse down the tree until we find the correct spot to insert, or
    % update. Then as we fall back out of the recursions we look for possible
    % rotation cases.
    %
:- pred set_in_node(K, V, rbtree(K, V), rbtree(K, V)).
:- mode set_in_node(di, di, di, uo) is det.
:- mode set_in_node(in, in, in, out) is det.

set_in_node(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        % We always insert red nodes at the bottom. They will be rotated
        % into the correct place as we move back up the tree.
        Tree = red(K, V, empty, empty)
    ;
        Tree0 = red(K0, V0, L0, R0),
        % Work out which side of the rbtree to insert into.
        compare(Result, K, K0),
        (
            Result = (=),
            Tree = red(K, V, L0, R0)
        ;
            Result = (<),
            set_in_node(K, V, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            set_in_node(K, V, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        ( if
            L0 = red(LK, LV, LL, LR),
            R0 = red(RK, RV, RL, RR)
        then
            % On the way down the rbtree, split any 4-nodes we find.
            L1 = black(LK, LV, LL, LR),
            R1 = black(RK, RV, RL, RR),
            Tree1 = red(K0, V0, L1, R1),
            set_in_node(K, V, Tree1, Tree)
        else
            % Work out which side of the rbtree to insert into.
            compare(Result, K, K0),
            (
                Result = (=),
                Tree = black(K, V, L0, R0)
            ;
                Result = (<),
                set_in_node(K, V, L0, L),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    L = red(LK, LV, LL, LR)
                then
                    % Check to see if a grandchild is red, and if it is,
                    % rotate.
                    ( if LL = red(_, _, _, _) then
                        TreeR = red(K0, V0, LR, R0),
                        Tree = black(LK, LV, LL, TreeR)
                    else if LR = red(LRK, LRV, LRL, LRR) then
                        TreeL = red(LK, LV, LL, LRL),
                        TreeR = red(K0, V0, LRR, R0),
                        Tree = black(LRK, LRV, TreeL, TreeR)
                    else
                        % LU == L, but this hack is needed
                        % for unique modes to work.
                        LU = red(LK, LV, LL, LR),
                        Tree = black(K0, V0, LU, R0)
                    )
                else
                    Tree = black(K0, V0, L, R0)
                )
            ;
                Result = (>),
                set_in_node(K, V, R0, R),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    R = red(RK, RV, RL, RR)
                then
                    % Check to see if a grandchild is red, and if it is,
                    % rotate.
                    ( if RL = red(RLK, RLV, RLL, RLR) then
                        TreeL = red(K0, V0, L0, RLL),
                        TreeR = red(RK, RV, RLR, RR),
                        Tree = black(RLK, RLV, TreeL, TreeR)
                    else if RR = red(_, _, _, _) then
                        TreeL = red(K0, V0, L0, RL),
                        Tree = black(RK, RV, TreeL, RR)
                    else
                        % RU == R, but this hack is needed
                        % for unique modes to work.
                        RU = red(RK, RV, RL, RR),
                        Tree = black(K0, V0, L0, RU)
                    )
                else
                    Tree = black(K0, V0, L0, R)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

insert_duplicate(!.RBT, K, V) = !:RBT :-
    rbtree.insert_duplicate(K, V, !RBT).

insert_duplicate(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        Tree = black(K, V, empty, empty)
    ;
        Tree0 = red(_, _, _, _),
        error($pred, "root node should not be red!")
    ;
        Tree0 = black(_, _, _, _),
        insert_duplicate_into_node(K, V, Tree0, Tree1),
        % Ensure that the root of the tree is black.
        ( if Tree1 = red(K1, V1, L1, R1) then
            Tree = black(K1, V1, L1, R1)
        else
            Tree = Tree1
        )
    ).

    % insert_duplicate_into_node:
    %
    % We traverse down the tree until we find the correct spot to insert.
    % Then as we fall back out of the recursions we look for possible
    % rotation cases.
    %
:- pred insert_duplicate_into_node(K, V, rbtree(K, V), rbtree(K, V)).
:- mode insert_duplicate_into_node(in, in, in, out) is det.

insert_duplicate_into_node(K, V, Tree0, Tree) :-
    (
        Tree0 = empty,
        % We always insert red nodes at the bottom. They will be rotated
        % into the correct place as we move back up the tree.
        Tree = red(K, V, empty, empty)
    ;
        Tree0 = red(K0, V0, L0, R0),
        % Work out which side of the rbtree to insert.
        compare(Result, K, K0),
        (
            ( Result = (<)
            ; Result = (=)
            ),
            insert_duplicate_into_node(K, V, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            insert_duplicate_into_node(K, V, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        % We only ever need to look for a possible rotation
        % if we are in a black node. The rotation criteria is when
        % there are two red nodes in a row.
        ( if
            L0 = red(LK, LV, LL, LR),
            R0 = red(RK, RV, RL, RR)
        then
            % On the way down the rbtree, we split any 4-nodes we find.
            % This converts the current node to a red node, so we call
            % the red node version of insert_duplicate_into_node/4.
            % XXX "red node version" does NOT help explain what is happening.
            L1 = black(LK, LV, LL, LR),
            R1 = black(RK, RV, RL, RR),
            Tree1 = red(K0, V0, L1, R1),
            insert_duplicate_into_node(K, V, Tree1, Tree)
        else
            % Work out which side of the rbtree to insert.
            compare(Result, K, K0),
            (
                Result = (<),
                insert_duplicate_into_node(K, V, L0, L),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    L = red(LK, LV, LL, LR)
                then
                    % Check to see if a grandchild is red and if so rotate.
                    ( if LL = red(_, _, _, _) then
                        TreeR = red(K0, V0, LR, R0),
                        Tree = black(LK, LV, LL, TreeR)
                    else if LR = red(LRK, LRV, LRL, LRR) then
                        TreeL = red(LK, LV, LL, LRL),
                        TreeR = red(K0, V0, LRR, R0),
                        Tree = black(LRK, LRV, TreeL, TreeR)
                    else
                        Tree = black(K0, V0, L, R0)
                    )
                else
                    Tree = black(K0, V0, L, R0)
                )
            ;
                Result = (>),
                insert_duplicate_into_node(K, V, R0, R),
                ( if
                    % We only need to start looking for a rotation case
                    % if the current node is black(known), and its
                    % new child is red.
                    R = red(RK, RV, RL, RR)
                then
                    % Check to see if a grandchild is red and if so rotate.
                    ( if RL = red(RLK, RLV, RLL, RLR) then
                        TreeL = red(K0, V0, L0, RLL),
                        TreeR = red(RK, RV, RLR, RR),
                        Tree = black(RLK, RLV, TreeL, TreeR)
                    else if RR = red(_, _, _, _) then
                        TreeL = red(K0, V0, L0, RL),
                        Tree = black(RK, RV, TreeL, RR)
                    else
                        Tree = black(K0, V0, L0, R)
                    )
                else
                    Tree = black(K0, V0, L0, R)
                )
            ;
                Result = (=),
                insert_duplicate_into_node(K, V, L0, L),
                ( if
                    % We only need to start looking for a rotation case if the
                    % current node is black(known), and its new child is red.
                    L = red(LK, LV, LL, LR)
                then
                    % Check to see if a grandchild is red, and if it is,
                    % rotate.
                    ( if LL = red(_, _, _, _) then
                        TreeR = red(K0, V0, LR, R0),
                        Tree = black(LK, LV, LL, TreeR)
                    else if LR = red(LRK, LRV, LRL, LRR) then
                        TreeL = red(LK, LV, LL, LRL),
                        TreeR = red(K0, V0, LRR, R0),
                        Tree = black(LRK, LRV, TreeL, TreeR)
                    else
                        Tree = black(K0, V0, L, R0)
                    )
                else
                    Tree = black(K0, V0, L, R0)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

member(Tree0, K, V) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        fail
    ;
        ( Tree0 = red(K0, V0, L0, R0)
        ; Tree0 = black(K0, V0, L0, R0)
        ),
        (
            K = K0,
            V = V0
        ;
            rbtree.member(L0, K, V)
        ;
            rbtree.member(R0, K, V)
        )
    ).

%---------------------------------------------------------------------------%

search(Tree0, K, V) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        fail
    ;
        ( Tree0 = red(K0, V0, L, R)
        ; Tree0 = black(K0, V0, L, R)
        ),
        compare(Result, K, K0),
        (
            Result = (=),
            V = V0
        ;
            Result = (<),
            rbtree.search(L, K, V)
        ;
            Result = (>),
            rbtree.search(R, K, V)
        )
    ).

lookup(RBT, K) = V :-
    rbtree.lookup(RBT, K, V).

lookup(T, K, V) :-
    ( if rbtree.search(T, K, V0) then
        V = V0
    else
        report_lookup_error("rbtree.lookup: key not found", K, V)
    ).

%---------------------------------------------------------------------------%

lower_bound_search(Tree, SearchK, K, V) :-
    require_complete_switch [Tree]
    (
        Tree = empty,
        fail
    ;
        ( Tree = red(K0, V0, L0, R)
        ; Tree = black(K0, V0, L0, R)
        ),
        compare(Result, SearchK, K0),
        (
            Result = (=),
            K = K0,
            V = V0
        ;
            Result = (<),
            rbtree.lower_bound_search(L0, SearchK, K, V)
        ;
            Result = (>),
            ( if rbtree.lower_bound_search(R, SearchK, Kp, Vp) then
                K = Kp,
                V = Vp
            else
                K = K0,
                V = V0
            )
        )
    ).

lower_bound_lookup(T, SearchK, K, V) :-
    ( if rbtree.lower_bound_search(T, SearchK, K0, V0) then
        K = K0,
        V = V0
    else
        report_lookup_error("rbtree.lower_bound_lookup: key not found",
            SearchK, V)
    ).

%---------------------------------------------------------------------------%

upper_bound_search(Tree0, SearchK, K, V) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        fail
    ;
        ( Tree0 = red(K0, V0, L0, R0)
        ; Tree0 = black(K0, V0, L0, R0)
        ),
        compare(Result, SearchK, K0),
        (
            Result = (=),
            K = K0,
            V = V0
        ;
            Result = (<),
            ( if rbtree.upper_bound_search(L0, SearchK, Kp, Vp) then
                K = Kp,
                V = Vp
            else
                K = K0,
                V = V0
            )
        ;
            Result = (>),
            rbtree.upper_bound_search(R0, SearchK, K, V)
        )
    ).

upper_bound_lookup(T, SearchK, K, V) :-
    ( if rbtree.upper_bound_search(T, SearchK, K0, V0) then
        K = K0,
        V = V0
    else
        report_lookup_error("rbtree.upper_bound_lookup: key not found",
            SearchK, V)
    ).

%---------------------------------------------------------------------------%

delete(!.RBT, K) = !:RBT :-
    rbtree.delete(K, !RBT).

delete(K, !Tree) :-
    rbtree.delete_from_node(K, _MaybeValue, !Tree).

    % delete_from_node(Key, MaybeValue, Tree0, Tree):
    %
    % Search the tree Tree0, looking for a node with key Key to delete.
    %
    % If we find the key, return it in MaybeValue and delete the node,
    % returning the result as Tree.
    %
    % If we do not find the key, return 'no' as MaybeValue, and return Tree0
    % as Tree.
    %
    % Deletion algorithm:
    %
    % Search down the tree, looking for the node to delete. O(log N)
    % When we find it, there are 4 possible conditions ->
    %   * Leaf node
    %       Remove node  O(1)
    %   * Left subtree of node to be deleted exists
    %       Move maximum key of Left subtree to current node. O(log N)
    %   * Right subtree of node to be deleted exists
    %       Move minimum key of Right subtree to current node. O(log N)
    %   * Both left and right subtrees of node to be deleted exist
    %       Move maximum key of Left subtree to current node. O(log N)
    %
    % The algorithm complexity is O(log N).
    %
:- pred delete_from_node(K::in, maybe(V)::out,
    rbtree(K, V)::in, rbtree(K, V)::out) is det.

delete_from_node(K, MaybeV, Tree0, Tree) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        MaybeV = no,
        Tree = empty
    ;
        Tree0 = red(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            % If we cannot remove the largest or smallest from a tree,
            % that tree must be empty.
            ( if rbtree.remove_largest(NewK, NewV, L0, L) then
                Tree = red(NewK, NewV, L, R0)
            else if rbtree.remove_smallest(NewK, NewV, R0, R) then
                Tree = red(NewK, NewV, empty, R)
            else
                Tree = empty
            ),
            MaybeV = yes(V0)
        ;
            Result = (<),
            delete_from_node(K, MaybeV, L0, L),
            Tree = red(K0, V0, L, R0)
        ;
            Result = (>),
            delete_from_node(K, MaybeV, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        compare(Result, K, K0),
        (
            Result = (=),
            % If we cannot remove the largest or smallest from a tree,
            % that tree must be empty.
            ( if rbtree.remove_largest(NewK, NewV, L0, L) then
                Tree = black(NewK, NewV, L, R0)
            else if rbtree.remove_smallest(NewK, NewV, R0, R) then
                Tree = black(NewK, NewV, empty, R)
            else
                Tree = empty
            ),
            MaybeV = yes(V0)
        ;
            Result = (<),
            delete_from_node(K, MaybeV, L0, L),
            Tree = black(K0, V0, L, R0)
        ;
            Result = (>),
            delete_from_node(K, MaybeV, R0, R),
            Tree = black(K0, V0, L0, R)
        )
    ).

%---------------------------------------------------------------------------%

remove(K, V, !Tree) :-
    rbtree.delete_from_node(K, MaybeV, !Tree),
    MaybeV = yes(V).

remove_smallest(SmallestK, SmallestV, Tree0, Tree) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        fail
    ;
        Tree0 = red(K0, V0, L0, R0),
        (
            L0 = empty,
            SmallestK = K0,
            SmallestV = V0,
            Tree = R0
        ;
            ( L0 = red(_, _, _, _)
            ; L0 = black(_, _, _, _)
            ),
            rbtree.remove_smallest(SmallestK, SmallestV, L0, L),
            Tree = red(K0, V0, L, R0)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        (
            L0 = empty,
            SmallestK = K0,
            SmallestV = V0,
            Tree = R0
        ;
            ( L0 = red(_, _, _, _)
            ; L0 = black(_, _, _, _)
            ),
            rbtree.remove_smallest(SmallestK, SmallestV, L0, L),
            Tree = black(K0, V0, L, R0)
        )
    ).

remove_largest(LargestK, LargestV, Tree0, Tree) :-
    require_complete_switch [Tree0]
    (
        Tree0 = empty,
        fail
    ;
        Tree0 = red(K0, V0, L0, R0),
        (
            R0 = empty,
            LargestK = K0,
            LargestV = V0,
            Tree = L0
        ;
            ( R0 = red(_, _, _, _)
            ; R0 = black(_, _, _, _)
            ),
            rbtree.remove_largest(LargestK, LargestV, R0, R),
            Tree = red(K0, V0, L0, R)
        )
    ;
        Tree0 = black(K0, V0, L0, R0),
        (
            R0 = empty,
            LargestK = K0,
            LargestV = V0,
            Tree = L0
        ;
            ( R0 = red(_, _, _, _)
            ; R0 = black(_, _, _, _)
            ),
            rbtree.remove_largest(LargestK, LargestV, R0, R),
            Tree = black(K0, V0, L0, R)
        )
    ).

%---------------------------------------------------------------------------%

keys(RBT) = Ks :-
    rbtree.keys(RBT, Ks).

keys(Tree, Keys) :-
    (
        Tree = empty,
        Keys = []
    ;
        ( Tree = red(Key, _Value, L, R)
        ; Tree = black(Key, _Value, L, R)
        ),
        rbtree.keys(L, KeysL),
        rbtree.keys(R, KeysR),
        Keys = KeysL ++ [Key | KeysR]
    ).

%---------------------------------------------------------------------------%

values(RBT) = Vs :-
    rbtree.values(RBT, Vs).

values(Tree, Values) :-
    (
        Tree = empty,
        Values = []
    ;
        ( Tree = red(_Key, Value, L, R)
        ; Tree = black(_Key, Value, L, R)
        ),
        rbtree.values(L, ValuesL),
        rbtree.values(R, ValuesR),
        Values = ValuesL ++ [Value | ValuesR]
    ).

%---------------------------------------------------------------------------%

count(RBT) = IN :-
    rbtree.ucount(RBT, N),
    IN = uint.cast_to_int(N).

count(RBT, IN) :-
    rbtree.ucount(RBT, N),
    IN = uint.cast_to_int(N).

ucount(RBT) = N :-
    rbtree.ucount(RBT, N).

ucount(Tree, Count) :-
    (
        Tree = empty,
        Count = 0u
    ;
        ( Tree = red(_Key, _Value, L, R)
        ; Tree = black(_Key, _Value, L, R)
        ),
        rbtree.ucount(L, CountL),
        rbtree.ucount(R, CountR),
        Count = CountL + CountR
    ).

%---------------------------------------------------------------------------%

assoc_list_to_rbtree(AL) = RBT :-
    rbtree.assoc_list_to_rbtree(AL, RBT).

assoc_list_to_rbtree([], empty).
assoc_list_to_rbtree([K - V | T], Tree) :-
    rbtree.assoc_list_to_rbtree(T, Tree0),
    rbtree.set(K, V, Tree0, Tree).

from_assoc_list(AList) = rbtree.assoc_list_to_rbtree(AList).

%---------------------------------------------------------------------------%

rbtree_to_assoc_list(RBT) = AssocList :-
    rbtree.rbtree_to_assoc_list(RBT, AssocList).

rbtree_to_assoc_list(Tree, AssocList) :-
    (
        Tree = empty,
        AssocList = []
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.rbtree_to_assoc_list(L, AssocListL),
        rbtree.rbtree_to_assoc_list(R, AssocListR),
        AssocList = AssocListL ++ [K - V | AssocListR]
    ).

to_assoc_list(T) = rbtree.rbtree_to_assoc_list(T).

%---------------------------------------------------------------------------%

foldl(F, T, A) = B :-
    P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    rbtree.foldl(P, T, A, B).

foldl(Pred, Tree, !AccA) :-
    (
        Tree = empty
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.foldl(Pred, L, !AccA),
        Pred(K, V, !AccA),
        rbtree.foldl(Pred, R, !AccA)
    ).

foldl2(Pred, Tree, !AccA, !AccB) :-
    (
        Tree = empty
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.foldl2(Pred, L, !AccA, !AccB),
        Pred(K, V, !AccA, !AccB),
        rbtree.foldl2(Pred, R, !AccA, !AccB)
    ).

foldl3(Pred, Tree, !AccA, !AccB, !AccC) :-
    (
        Tree = empty
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.foldl3(Pred, L, !AccA, !AccB, !AccC),
        Pred(K, V, !AccA, !AccB, !AccC),
        rbtree.foldl3(Pred, R, !AccA, !AccB, !AccC)
    ).

%---------------------------------------------------------------------------%

foldl_values(Pred, Tree, !AccA) :-
    (
        Tree = empty
    ;
        ( Tree = red(_K, V, L, R)
        ; Tree = black(_K, V, L, R)
        ),
        rbtree.foldl_values(Pred, L, !AccA),
        Pred(V, !AccA),
        rbtree.foldl_values(Pred, R, !AccA)
    ).

foldl2_values(Pred, Tree, !AccA, !AccB) :-
    (
        Tree = empty
    ;
        ( Tree = red(_K, V, L, R)
        ; Tree = black(_K, V, L, R)
        ),
        rbtree.foldl2_values(Pred, L, !AccA, !AccB),
        Pred(V, !AccA, !AccB),
        rbtree.foldl2_values(Pred, R, !AccA, !AccB)
    ).

%---------------------------------------------------------------------------%

foldr(F, T, Acc0) = Acc :-
    P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    rbtree.foldr(P, T, Acc0, Acc).

foldr(Pred, Tree, !AccA) :-
    (
        Tree = empty
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.foldr(Pred, R, !AccA),
        Pred(K, V, !AccA),
        rbtree.foldr(Pred, L, !AccA)
    ).

foldr2(Pred, Tree, !AccA, !AccB) :-
    (
        Tree = empty
    ;
        ( Tree = red(K, V, L, R)
        ; Tree = black(K, V, L, R)
        ),
        rbtree.foldr2(Pred, R, !AccA, !AccB),
        Pred(K, V, !AccA, !AccB),
        rbtree.foldr2(Pred, L, !AccA, !AccB)
    ).

%---------------------------------------------------------------------------%

foldr_values(Pred, Tree, !AccA) :-
    (
        Tree = empty
    ;
        ( Tree = red(_K, V, L, R)
        ; Tree = black(_K, V, L, R)
        ),
        rbtree.foldr_values(Pred, R, !AccA),
        Pred(V, !AccA),
        rbtree.foldr_values(Pred, L, !AccA)
    ).

%---------------------------------------------------------------------------%

map_values(F, T1) = T2 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    rbtree.map_values(P, T1, T2).

map_values(Pred, Tree0, Tree) :-
    (
        Tree0 = empty,
        Tree = empty
    ;
        Tree0 = red(K0, V0, L0, R0),
        Pred(K0, V0, V),
        rbtree.map_values(Pred, L0, L),
        rbtree.map_values(Pred, R0, R),
        Tree = red(K0, V, L, R)
    ;
        Tree0 = black(K0, V0, L0, R0),
        Pred(K0, V0, V),
        rbtree.map_values(Pred, L0, L),
        rbtree.map_values(Pred, R0, R),
        Tree = black(K0, V, L, R)
    ).

%---------------------------------------------------------------------------%
:- end_module rbtree.
%---------------------------------------------------------------------------%
