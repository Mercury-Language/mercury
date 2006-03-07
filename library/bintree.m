%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997, 1999, 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: bintree.m.
% Main author: conway.
% Stability: medium (obsolete).
%
% This module exists primarily for historical reasons. It is unlikely
% to be useful, and may not be supported in future releases.
% You should use `map' instead.
%
% This file provides a straight-forward binary search tree implementation of
% a map (dictionary).
%
% bintree.insert, bintree.update, and bintree.set differ only in how they
% handle the case where the value being inserted already exists in the tree.
% `insert' will only insert new keys, and will fail if you attempt to insert
% an existing key into the tree. `update' will only allow you to modify the
% data for existing keys, and will fail if the key isn't already in the tree.
% `set' will always succeed; it will replace the old value for that key
% if the key was already in the tree, or insert a new node into the tree
% if the key wasn't already present.
%
%-----------------------------------------------------------------------------%

:- module bintree.
:- interface.

:- import_module assoc_list.
:- import_module list.

:- type bintree(K, V).

:- pred bintree.init(bintree(K, V)::uo) is det.

:- pred bintree.insert(bintree(K, V)::in, K::in, V::in, bintree(K, V)::out)
    is semidet.

:- pred bintree.update(bintree(K, V)::in, K::in, V::in, bintree(K, V)::out)
    is semidet.

:- pred bintree.set(bintree(K, V), K, V, bintree(K, V)).
:- mode bintree.set(di, di, di, uo) is det.
:- mode bintree.set(in, in, in, out) is det.

:- func bintree.set(bintree(K, V), K, V) = bintree(K, V).

:- pred bintree.search(bintree(K, V), K, V).
:- mode bintree.search(in, in, in) is semidet. % implied
:- mode bintree.search(in, in, out) is semidet.

:- pred bintree.lookup(bintree(K, V)::in, K::in, V::out) is det.
:- func bintree.lookup(bintree(K, V), K) = V.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred bintree.lower_bound_search(bintree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred bintree.lower_bound_lookup(bintree(K, V)::in, K::in, K::out, V::out)
    is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred bintree.upper_bound_search(bintree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred bintree.upper_bound_lookup(bintree(K, V)::in, K::in, K::out, V::out)
    is det.

:- pred bintree.delete(bintree(K, V)::in, K::in, bintree(K, V)::out) is det.
:- func bintree.delete(bintree(K, V), K) = bintree(K, V).

:- pred bintree.remove(bintree(K, V)::in, K::in, V::out, bintree(K, V)::out)
    is semidet.

:- pred bintree.keys(bintree(K, _V)::in, list(K)::out) is det.
:- func bintree.keys(bintree(K, _V)) = list(K).

:- pred bintree.values(bintree(_K, V)::in, list(V)::out) is det.
:- func bintree.values(bintree(_K, V)) = list(V).

:- pred bintree.from_list(assoc_list(K, V)::in, bintree(K, V)::out) is det.
:- func bintree.from_list(assoc_list(K, V)) = bintree(K, V).

:- pred bintree.from_sorted_list(assoc_list(K, V)::in, bintree(K, V)::out)
    is det.
:- func bintree.from_sorted_list(assoc_list(K, V)) = bintree(K, V).

:- pred bintree.from_corresponding_lists(list(K)::in, list(V)::in,
    bintree(K, V)::out) is det.
:- func bintree.from_corresponding_lists(list(K), list(V)) = bintree(K, V).

:- pred bintree.to_list(bintree(K, V)::in, assoc_list(K, V)::out) is det.
:- func bintree.to_list(bintree(K, V)) = assoc_list(K, V).

    % Count the number of elements in a tree.
    %
:- pred bintree.count(bintree(_K, _V)::in, int::out) is det.
:- func bintree.count(bintree(_K, _V)) = int.

    % Count the depth of a tree.
    %
:- pred bintree.depth(bintree(_K, _V)::in, int::out) is det.
:- func bintree.depth(bintree(_K, _V)) = int.

:- pred bintree.branching_factor(bintree(_K, _V)::in, int::out, int::out)
    is det.

:- pred bintree.balance(bintree(K, V)::in, bintree(K, V)::out) is det.
:- func bintree.balance(bintree(K, V)) = bintree(K, V).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.

:- type bintree(K, V)
    --->    empty
    ;       tree(K, V, bintree(K, V), bintree(K, V)).

%-----------------------------------------------------------------------------%

bintree.init(empty).

%-----------------------------------------------------------------------------%

bintree.insert(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree.insert(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
    compare(Result, Key0, Key),
    ( Result = (=) ->
        fail
    ; Result = (<) ->
        bintree.insert(Right, Key, Value, NewRight),
        Tree = tree(Key0, Value0, Left, NewRight)
    ;
        bintree.insert(Left, Key, Value, NewLeft),
        Tree = tree(Key0, Value0, NewLeft, Right)
    ).

%-----------------------------------------------------------------------------%

bintree.update(empty, _Key, _Value, _Tree) :-
    fail.
bintree.update(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
    compare(Result, Key0, Key),
    ( Result = (=) ->
        Tree = tree(Key0, Value, Left, Right)
    ; Result = (<) ->
        bintree.update(Right, Key, Value, NewRight),
        Tree = tree(Key0, Value0, Left, NewRight)
    ;
        bintree.update(Left, Key, Value, NewLeft),
        Tree = tree(Key0, Value0, NewLeft, Right)
    ).

%-----------------------------------------------------------------------------%

bintree.set(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree.set(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
    compare(Result, Key0, Key),
    ( Result = (=) ->
        Tree = tree(Key0, Value, Left, Right)
    ; Result = (<) ->
        bintree.set(Right, Key, Value, NewRight),
        Tree = tree(Key0, Value0, Left, NewRight)
    ;
        bintree.set(Left, Key, Value, NewLeft),
        Tree = tree(Key0, Value0, NewLeft, Right)
    ).

%-----------------------------------------------------------------------------%

bintree.search(tree(K0, V0, Left, Right), K, V) :-
    compare(Result, K0, K),
    ( Result = (=) ->
        V = V0
    ; Result = (<) ->
        bintree.search(Right, K, V)
    ;
        bintree.search(Left, K, V)
    ).

%-----------------------------------------------------------------------------%

bintree.lookup(Tree, K, V) :-
    ( bintree.search(Tree, K, V0) ->
        V = V0
    ;
        report_lookup_error("bintree.lookup: key not found", K, V)
    ).

%-----------------------------------------------------------------------------%

bintree.lower_bound_search(tree(K0, V0, Left, Right), SearchK, K, V) :-
    compare(Result, K0, SearchK),
    ( Result = (=) ->
        K = K0,
        V = V0
    ; Result = (<) ->
        ( bintree.lower_bound_search(Right, SearchK, Kp, Vp) ->
            K = Kp,
            V = Vp
        ;
            K = K0,
            V = V0
        )
    ;
        bintree.lower_bound_search(Left, SearchK, K, V)
    ).

bintree.lower_bound_lookup(Tree, SearchK, K, V) :-
    ( bintree.lower_bound_search(Tree, SearchK, K0, V0) ->
        K = K0,
        V = V0
    ;
        report_lookup_error("bintree.lower_bound_lookup: " ++
            "key not found", SearchK, V)
    ).

%-----------------------------------------------------------------------------%

bintree.upper_bound_search(tree(K0, V0, Left, Right), SearchK, K, V) :-
    compare(Result, K0, SearchK),
    ( Result = (=) ->
        K = K0,
        V = V0
    ; Result = (<) ->
        bintree.upper_bound_search(Right, SearchK, K, V)
    ;
        ( bintree.upper_bound_search(Left, SearchK, Kp, Vp) ->
            K = Kp,
            V = Vp
        ;
            K = K0,
            V = V0
        )
    ).

bintree.upper_bound_lookup(Tree, SearchK, K, V) :-
    ( bintree.upper_bound_search(Tree, SearchK, K0, V0) ->
        K = K0,
        V = V0
    ;
        report_lookup_error("bintree.lower_bound_lookup: key not found",
            SearchK, V)
    ).

%-----------------------------------------------------------------------------%

bintree.delete(empty, _K, empty).
bintree.delete(tree(K0, V0, Left, Right), K, Tree) :-
    compare(Result, K0, K),
    ( Result = (=) ->
        bintree.fixup(Left, Right, Tree)
    ; Result = (<) ->
        bintree.delete(Right, K, Tree1),
        Tree = tree(K0, V0, Left, Tree1)
    ;
        bintree.delete(Left, K, Tree1),
        Tree = tree(K0, V0, Tree1, Right)
    ).

%-----------------------------------------------------------------------------%

bintree.remove(tree(K0, V0, Left, Right), K, V, Tree) :-
    compare(Result, K0, K),
    ( Result = (=) ->
        V = V0,
        bintree.fixup(Left, Right, Tree)
    ; Result = (<) ->
        bintree.remove(Right, K, V, Tree1),
        Tree = tree(K0, V0, Left, Tree1)
    ;
        bintree.remove(Left, K, V, Tree1),
        Tree = tree(K0, V0, Tree1, Right)
    ).

%-----------------------------------------------------------------------------%

:- pred bintree.fixup(bintree(K, V)::in, bintree(K, V)::in,
    bintree(K, V)::out) is det.

bintree.fixup(Left, Right, Tree) :-
    ( Left = empty ->
        Tree = Right
    ; Right = empty ->
        Tree = Left
    ;
        bintree.right_depth(Left, LD),
        bintree.left_depth(Right, RD),
        ( LD > RD ->
            bintree.knock_left(Left, K, V, Left1),
            Right1 = Right
        ;
            bintree.knock_right(Right, K, V, Right1),
            Left1 = Left
        ),
        Tree = tree(K, V, Left1, Right1)
    ).

:- pred bintree.right_depth(bintree(_K, _V)::in, int::out) is det.

bintree.right_depth(empty, 0).
bintree.right_depth(tree(_K, _V, _Left, Right), N) :-
    bintree.right_depth(Right, M),
    N = M + 1.

:- pred bintree.left_depth(bintree(_K, _V)::in, int::out) is det.

bintree.left_depth(empty, 0).
bintree.left_depth(tree(_K, _V, Left, _Right), N) :-
    bintree.left_depth(Left, M),
    N = M + 1.

:- pred bintree.knock_left(bintree(K, V)::in, K::out, V::out,
    bintree(K, V)::out) is det.

bintree.knock_left(empty, _, _, _) :-
    error("bintree.knock_left: empty tree").
bintree.knock_left(tree(K0, V0, Left, Right), K, V, Tree) :-
    ( Right = empty ->
        K = K0,
        V = V0,
        Tree = Left
    ;
        bintree.knock_left(Right, K, V, Right1),
        Tree = tree(K0, V0, Left, Right1)
    ).

:- pred bintree.knock_right(bintree(K, V)::in, K::out, V::out,
    bintree(K, V)::out) is det.

bintree.knock_right(empty, _, _, _) :-
    error("bintree.knock_right: empty tree").
bintree.knock_right(tree(K0, V0, Left, Right), K, V, Tree) :-
    ( Left = empty ->
        K = K0,
        V = V0,
        Tree = Right
    ;
        bintree.knock_right(Left, K, V, Left1),
        Tree = tree(K0, V0, Left1, Right)
    ).

%-----------------------------------------------------------------------------%

bintree.from_list(List, Tree) :-
    bintree.from_list_2(List, empty, Tree).

:- pred bintree.from_list_2(assoc_list(K, V)::in, bintree(K, V)::in,
    bintree(K, V)::out) is det.

bintree.from_list_2([], Tree, Tree).
bintree.from_list_2([K - V | List], Tree0, Tree) :-
    ( bintree.insert(Tree0, K, V, Tree1) ->
        Tree2 = Tree1
    ;
        report_lookup_error("bintree.from_list: key already present", K, V)
    ),
    bintree.from_list_2(List, Tree2, Tree).

%-----------------------------------------------------------------------------%

bintree.from_sorted_list(List, Tree) :-
    list.length(List, Length),
    bintree.from_sorted_list_2(Length, List, Tree, _).

:- pred bintree.from_sorted_list_2(int::in, assoc_list(K, V)::in,
    bintree(K, V)::out, assoc_list(K, V)::out) is det.

bintree.from_sorted_list_2(Num, List0, Tree, List) :-
    ( Num = 0 ->
        List = List0,
        Tree = empty
    ;
        Num1 = Num - 1,
        SmallHalf = Num1 // 2,
        BigHalf = Num1 - SmallHalf,
        bintree.from_sorted_list_2(SmallHalf, List0, LeftSubTree, List1),
        (
            List1 = [HeadKey - HeadValue | List2],
            Tree = tree(HeadKey, HeadValue, LeftSubTree, RightSubTree),
            bintree.from_sorted_list_2(BigHalf, List2, RightSubTree, List)
        ;
            List1 = [],
            error("bintree.from_sorted_list_2")
        )
    ).

%-----------------------------------------------------------------------------%

bintree.balance(Tree0, Tree) :-
    bintree.to_list(Tree0, List),
    bintree.from_sorted_list(List, Tree).

%-----------------------------------------------------------------------------%

bintree.from_corresponding_lists(Keys, Values, Tree) :-
    ( bintree.from_corresponding_lists_2(Keys, Values, empty, Tree0) ->
        Tree = Tree0
    ;
        error("bintree.from_corresponding_lists: " ++
            "lists are of different lengths")
    ).

:- pred bintree.from_corresponding_lists_2(list(K)::in, list(V)::in,
    bintree(K, V)::in, bintree(K, V)::out) is semidet.

bintree.from_corresponding_lists_2([], [], Tree, Tree).
bintree.from_corresponding_lists_2([K | Ks], [V | Vs], Tree0, Tree) :-
    ( bintree.insert(Tree0, K, V, Tree1) ->
        Tree2 = Tree1
    ;
        report_lookup_error(
            "bintree.from_corresponding_lists: key already present", K, V)
    ),
    bintree.from_corresponding_lists_2(Ks, Vs, Tree2, Tree).

%-----------------------------------------------------------------------------%

bintree.to_list(Tree, List) :-
    bintree.to_list_2(Tree, [], List).

:- pred bintree.to_list_2(bintree(K, V)::in, assoc_list(K, V)::in,
    assoc_list(K, V)::out) is det.

bintree.to_list_2(empty, List, List).
bintree.to_list_2(tree(K, V, Left, Right), List0, List) :-
    bintree.to_list_2(Right, List0, List1),
    bintree.to_list_2(Left, [K - V | List1], List).

%-----------------------------------------------------------------------------%

bintree.keys(Tree, List) :-
    bintree.keys_2(Tree, [], List).

:- pred bintree.keys_2(bintree(K, _V)::in, list(K)::in, list(K)::out) is det.

bintree.keys_2(empty, List, List).
bintree.keys_2(tree(K, _V, Left, Right), List0, List) :-
    bintree.keys_2(Right, List0, List1),
    bintree.keys_2(Left, [K | List1], List).

%-----------------------------------------------------------------------------%

bintree.values(Tree, List) :-
    bintree.values_2(Tree, [], List).

:- pred bintree.values_2(bintree(_K, V)::in, list(V)::in, list(V)::out)
    is det.

bintree.values_2(empty, List, List).
bintree.values_2(tree(_K, V, Left, Right), List0, List) :-
    bintree.values_2(Right, List0, List1),
    bintree.values_2(Left, [V | List1], List).

%-----------------------------------------------------------------------------%

bintree.count(empty, 0).
bintree.count(tree(_K, _V, Left, Right), Count) :-
    bintree.count(Right, RightCount),
    bintree.count(Left, LeftCount),
    ChildCount = LeftCount + RightCount,
    Count = ChildCount + 1.

bintree.depth(empty, 0).
bintree.depth(tree(_K, _V, Left, Right), Depth) :-
    bintree.depth(Right, RightDepth),
    bintree.depth(Left, LeftDepth),
    int.max(LeftDepth, RightDepth, SubDepth),
    Depth = SubDepth + 1.

bintree.branching_factor(empty, 0, 0).
bintree.branching_factor(tree(_K, _V, L, R), Ones, Twos) :-
    ( L = empty ->
        ( R = empty ->
            Ones = 0,
            Twos = 0
        ;
            bintree.branching_factor(R, Ones0, Twos),
            Ones = Ones0 + 1
        )
    ;
        ( R = empty ->
            bintree.branching_factor(L, Ones0, Twos),
            Ones = Ones0 + 1
        ;
            bintree.branching_factor(L, Ones1, Twos1),
            bintree.branching_factor(R, Ones2, Twos2),
            Ones = Ones1 + Ones2,
            Twos0 = Twos1 + Twos2,
            Twos = Twos0 + 1
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

bintree.set(BT1, K, V) = BT2 :-
    bintree.set(BT1, K, V, BT2).

bintree.lookup(BT, K) = V :-
    bintree.lookup(BT, K, V).

bintree.delete(BT1, K) = BT2 :-
    bintree.delete(BT1, K, BT2).

bintree.keys(BT) = Ks :-
    bintree.keys(BT, Ks).

bintree.values(BT) = Vs :-
    bintree.values(BT, Vs).

bintree.from_list(AL) = BT :-
    bintree.from_list(AL, BT).

bintree.from_sorted_list(AL) = BT :-
    bintree.from_sorted_list(AL, BT).

bintree.from_corresponding_lists(Ks, Vs) = BT :-
    bintree.from_corresponding_lists(Ks, Vs, BT).

bintree.to_list(BT) = AL :-
    bintree.to_list(BT, AL).

bintree.count(BT) = N :-
    bintree.count(BT, N).

bintree.depth(BT) = N :-
    bintree.depth(BT, N).

bintree.balance(BT1) = BT2 :-
    bintree.balance(BT1, BT2).
