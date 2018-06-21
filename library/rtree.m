%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rtree.m.
% Main author: gjd.
% Stability: low.
%
% This module provides a region tree (R-tree) ADT. A region tree associates
% values with regions in some space, e.g. rectangles in the 2D plane, or
% bounding spheres in 3D space. Region trees accept spatial queries, e.g. a
% typical usage is "find all pubs within a 2km radius".
%
% This module also provides the typeclass region(K) which allows the user to
% define new regions and spaces. Three "builtin" instances for region(K)
% are provided: region(interval), region(box) and region(box3d)
% corresponding to "square" regions in one, two and three dimensional spaces
% respectively.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rtree.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type rtree(K, V).

:- typeclass region(K) where [

        % Succeeds iff two regions intersect.
        %
    pred intersects(K::in, K::in) is semidet,

        % Succeeds iff the first region is contained within the second.
        %
    pred contains(K::in, K::in) is semidet,

        % Returns the "size" of a region.
        % e.g. for a two dimensional box one possible measure of "size"
        %      would be the area.
        %
    func size(K) = float,

        % Return a region that contains both input regions.
        % The region returned should be minimal region that contains
        % both input regions.
        %
    func bounding_region(K, K) = K,

        % Computes the size of the bounding region returned by
        % bounding_region/2, i.e.
        %
        % bounding_region_size(K1, K2) = size(bounding_region(K1, K2)).
        %
        % While the above definition would suffice, a more efficient
        % implementation often exists, e.g. for intervals:
        %
        % bounding_region_size(interval(X0, X1), interval(Y0, Y1)) =
        %       max(X1, Y1) - min(X0, Y0).
        %
        % This version is more efficient since it does not create a
        % temporary interval.
        %
    func bounding_region_size(K, K) = float
].

%---------------------------------------------------------------------------%

    % Initialize an empty rtree.
    %
:- func init = (rtree(K, V)::uo) is det <= region(K).

    % Succeeds iff the given rtree is empty.
    %
:- pred is_empty(rtree(K, V)::in) is semidet.

    % Insert a new key and corresponding value into an rtree.
    %
:- func insert(K, V, rtree(K, V)) = rtree(K, V) <= region(K).
:- pred insert(K::in, V::in, rtree(K, V)::in, rtree(K, V)::out)
    is det <= region(K).

    % Delete a key-value pair from an rtree.
    % Assumes that K is either the key for V, or is contained in the key
    % for V.
    %
    % Fails if the key-value pair is not in the tree.
    %
:- pred delete(K::in, V::in, rtree(K, V)::in, rtree(K, V)::out)
    is semidet <= region(K).

    % Search for all values with keys that intersect the query key.
    %
:- func search_intersects(rtree(K, V), K) = list(V) <= region(K).

    % Search for all values with keys that contain the query key.
    %
:- func search_contains(rtree(K, V), K) = list(V) <= region(K).

    % search_general(KTest, VTest, T) = V.
    %
    % Search for all values V with associated keys K that satisfy
    % KTest(K) /\ VTest(V). The search assumes that for all K1, K2
    % such that K1 contains K2, then if KTest(K2) holds we have that
    % KTest(K1) also holds.
    %
    % We have that:
    %
    %   search_intersects(T, K, Vs)
    %       <=> search_general(intersects(K), true, T, Vs)
    %
    %   search_contains(T, K, Vs)
    %       <=> search_general(contains(K), true, T, Vs)
    %
:- func search_general(pred(K)::in(pred(in) is semidet),
    pred(V)::in(pred(in) is semidet), rtree(K, V)::in) = (list(V)::out)
    is det.

    % search_first(KTest, VTest, Max, T, V, L).
    %
    % Search for a value V with associated key K such that
    % KTest(K, _) /\ VTest(V, L) is satisfied and there does not exist a
    % V' with K' such that KTest(K', _) /\ VTest(V', L') /\ (L' < L) is
    % satisfied. Fail if no such key-value pair exists.
    %
    % The search assumes that for all K1, K2 such that
    % K1 contains K2, then if KTest(K2, L2) holds we have that
    % KTest(K1, L1) holds with L2 >= L1.
    %
    % If there exist multiple key-value pairs that satisfy the above
    % conditions, then one of the candidates is chosen arbitrarily.
    %
:- pred search_first(pred(K, L), pred(V, L), rtree(K, V), L, V, L).
:- mode search_first(pred(in, out) is semidet,
    pred(in, out) is semidet, in, in, out, out) is semidet.

    % search_general_fold(KTest, VPred, T, !A).
    %
    % Apply accumulator VPred to each key-value pair K-V that satisfies
    % KTest(K). The same assumptions for KTest from search_general apply
    % here.
    %
:- pred search_general_fold(pred(K), pred(K, V, A, A), rtree(K, V),
    A, A).
:- mode search_general_fold(pred(in) is semidet,
    pred(in, in, in, out) is det, in, in, out) is det.
:- mode search_general_fold(pred(in) is semidet,
    pred(in, in, di, uo) is det, in, di, uo) is det.

    % Perform a traversal of the rtree, applying an accumulator predicate
    % for each key-value pair.
    %
:- pred fold(pred(K, V, A, A), rtree(K, V), A, A).
:- mode fold(pred(in, in, in, out) is det, in, in, out) is det.
:- mode fold(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode fold(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.

    % Apply a transformation predicate to all the values in an rtree.
    %
:- pred map_values(pred(K, V, W), rtree(K, V), rtree(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.
:- mode map_values(pred(in, in, out) is semidet, in, out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Pre-defined regions.
%

    % An interval type represented as interval(Min, Max).
    %
:- type interval
    --->    interval(float, float).

    % A 2D axis aligned box represented as box(XMin, XMax, YMin, YMax).
    %
:- type box
    --->    box(float, float, float, float).

    % A 3D axis aligned box represented as
    % box(XMin, XMax, YMin, YMax, ZMin, ZMax).
    %
:- type box3d
    --->    box3d(float, float, float, float, float, float).

:- instance region(interval).
:- instance region(box).
:- instance region(box3d).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%
%
% The implementation here is based on:
%
% A. Guttman, "R-trees: a dynamic index structure for spatial searching,"
% in Proc. ACM-SIGMOD Int. Conf. Management of Data, Boston, MA, 1984,
% pp.47--54.
%
% NOTES
% -----
% * What the paper refers to as "rectangles", we refer to as "regions".
% * What the paper refers to as "area", we refer to as "size".
%
%---------------------------------------------------------------------------%

    % The empty rtree and singleton rtrees are treated as a special case.
    %
:- type rtree(K, V)
    --->    empty
    ;       one(K, V)
    ;       rtree(rtree_2(K, V)).

    % The "real" rtree structure is rtree_2, which consists of leaf nodes
    % (which contain the values) and 2/3/4 nodes which contain 2/3/4 keys
    % which are the bounding regions of the 2/3/4 subtrees.
    %
    % The key for the root node is not stored anywhere. This means queries
    % outside the bounds of the tree will be slower, since we must descend
    % to level 1 instead of level 0. However, we will win if the query is
    % within range. Also doing this simplifies the code slightly.
    %
:- type rtree_2(K, V)
    --->    leaf(V)
    ;       two(K, rtree_2(K, V), K, rtree_2(K, V))
    ;       three(K, rtree_2(K, V), K, rtree_2(K, V), K, rtree_2(K, V))
    ;       four(K, rtree_2(K, V), K, rtree_2(K, V), K, rtree_2(K, V), K,
                rtree_2(K, V)).

:- inst four for rtree_2/2 ==
    bound(four(ground, ground, ground, ground, ground, ground, ground,
        ground)).

:- type min_of_two_result
    --->    min2_first
    ;       min2_second.

:- type min_of_three_result
    --->    min3_first
    ;       min3_second
    ;       min3_third.

:- type min_of_four_result
    --->    min4_first
    ;       min4_second
    ;       min4_third
    ;       min4_fourth.

%---------------------------------------------------------------------------%
%
% Creation.
%

init = empty.

%---------------------------------------------------------------------------%
%
% Test for emptiness.
%

is_empty(empty).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Insertion.
%

insert(K, V, !.Tree) = !:Tree :-
    rtree.insert(K, V, !Tree).

insert(K, V, empty, one(K, V)).
insert(K, V, one(K0, V0), T) :-
    T = rtree(two(K0, leaf(V0), K, leaf(V))).
insert(K, V, rtree(!.T), rtree(!:T)) :-
    insert_2(!.T, K, V, !:T).

    % NOTE: the 4-node case means the input tree is the root node, otherwise
    % splitting ensures we only see 2 or 3 nodes for the rest of the descent.
    % Also, we should never see a leaf node.
    %
:- pred insert_2(rtree_2(K, V)::in, K::in, V::in, rtree_2(K, V)::out)
    is det <= region(K).

insert_2(leaf(_), _, _, _) :-
    error("insert: leaf unexpected").
insert_2(two(K0, T0, K1, T1), K, V, T) :-
    Result = choose_subtree(K, K0, K1),
    (
        Result = min2_first,
        insert_and_split_child2(K0, T0, K1, T1, K, V, T)
    ;
        Result = min2_second,
        insert_and_split_child2(K1, T1, K0, T0, K, V, T)
    ).
insert_2(three(K0, T0, K1, T1, K2, T2), K, V, T) :-
    Result = choose_subtree(K, K0, K1, K2),
    (
        Result = min3_first,
        insert_and_split_child3(K0, T0, K1, T1, K2, T2, K, V, T)
    ;
        Result = min3_second,
        insert_and_split_child3(K1, T1, K0, T0, K2, T2, K, V, T)
    ;
        Result = min3_third,
        insert_and_split_child3(K2, T2, K0, T0, K1, T1, K, V, T)
    ).
insert_2(Node, K, V, T) :-
    Node = four(_, _, _, _, _, _, _, _),
    split_4_node(Node, K0, T0, K1, T1),
    insert_2(two(K0, T0, K1, T1), K, V, T).

%---------------------------------------------------------------------------%
%
% Choosing what subtree to insert a new node into.
%

% The following functions choose a subtree into which to insert a new
% Key-Value pair. There are two versions, one for when we are inserting into
% two-nodes and another for when we are inserting into three-nodes.
% (Four-nodes should be split when they are encountered so we will never need
% to insert into them.)
%
% In either case the algorithm is the same (c.f. the ChooseLeaf algorithm in
% the paper by Guttman).
%
% We insert the new Value into the subtree whose associated region needs the
% least enlargement to include Key. Ties are resolved in favour of the
% subtree with the region of smallest size.

    % choose_subtree(Key, KA, KB).
    %
    % Returns min2_first if we should insert the value associated with Key
    % into the subtree associated with KA and min2_second if we should
    % insert it into the subtree associated with KB.
    %
:- func choose_subtree(K, K, K) = min_of_two_result <= region(K).

choose_subtree(Key, KA, KB) = Result :-
    SizeA = size(KA),
    SizeB = size(KB),
    %
    % Compute the size of each of KA and KB enlarged to include Key.
    %
    EnlargedSizeA = bounding_region_size(Key, KA),
    EnlargedSizeB = bounding_region_size(Key, KB),
    IncreaseForA  = EnlargedSizeA - SizeA,
    IncreaseForB  = EnlargedSizeB - SizeB,
    ( if IncreaseForA < IncreaseForB then
        Result = min2_first
    else if IncreaseForA > IncreaseForB then
        Result = min2_second
    else if SizeA =< SizeB then
        Result = min2_first
    else
        Result = min2_second
    ).

    % Decides which subtree to insert value with Key.
    %
:- func choose_subtree(K, K, K, K) = min_of_three_result <= region(K).

choose_subtree(Key, KA, KB, KC) = Result :-
    AreaA = size(KA),
    AreaB = size(KB),
    AreaC = size(KC),
    EnlargedAreaA = bounding_region_size(Key, KA),
    EnlargedAreaB = bounding_region_size(Key, KB),
    EnlargedAreaC = bounding_region_size(Key, KC),
    IncreaseForA = EnlargedAreaA - AreaA,
    IncreaseForB = EnlargedAreaB - AreaB,
    IncreaseForC = EnlargedAreaC - AreaC,
    include_min(IncreaseForA, IncreaseForB, AreaA, AreaB,
        min3_first, min3_second, Result0),
    ( if Result0 = min3_first then
        include_min(IncreaseForA, IncreaseForC, AreaA, AreaC,
            min3_first, min3_third, Result)
    else
        include_min(IncreaseForB, IncreaseForC, AreaA, AreaB,
            min3_second, min3_third, Result)
    ).

    % D1 and D2 are the enlargement to the two rectangles caused by adding
    % the new key. We choose the Key that causes the smallest enlargement.
    % In the event of a tie with choose the Key with the smallest area.
    %
:- pred include_min(float::in, float::in, float::in, float::in,
    min_of_three_result::in, min_of_three_result::in,
    min_of_three_result::out) is det.

include_min(D1, D2, A1, A2, R1, R2, R3) :-
    ( if D1 < D2 then
        R3 = R1
    else if D1 > D2 then
        R3 = R2
    else if A1 =< A2 then
        R3 = R1
    else
        R3 = R2
    ).

%---------------------------------------------------------------------------%

    % Split the child (if a 4 node) and insert into T0.
    %
:- pred insert_and_split_child2(K::in, rtree_2(K, V)::in, K::in,
    rtree_2(K, V)::in, K::in, V::in, rtree_2(K, V)::out) is det <= region(K).

insert_and_split_child2(K0, T0, K1, T1, K, V, T) :-
    (
        T0 = leaf(_),
        T = three(K0, T0, K1, T1, K, leaf(V))
    ;
        T0 = two(_, _, _, _),
        NK0 = bounding_region(K, K0),
        insert_2(T0, K, V, NT0),
        T = two(NK0, NT0, K1, T1)
    ;
        T0 = three(_, _, _, _, _, _),
        NK0 = bounding_region(K, K0),
        insert_2(T0, K, V, NT0),
        T = two(NK0, NT0, K1, T1)
    ;
        T0 = four(_, _, _, _, _, _, _, _),
        split_4_node(T0, K2, T2, K3, T3),
        Result = choose_subtree(K, K2, K3),
        (
            Result = min2_first,
            K4 = bounding_region(K, K2),
            insert_2(T2, K, V, T4),
            T = three(K1, T1, K3, T3, K4, T4)
        ;
            Result = min2_second,
            K4 = bounding_region(K, K3),
            insert_2(T3, K, V, T4),
            T = three(K1, T1, K2, T2, K4, T4)
        )
    ).

    % Split the child (if a 4 node) and insert into T0.
    %
:- pred insert_and_split_child3(K::in, rtree_2(K, V)::in, K::in,
    rtree_2(K, V)::in, K::in, rtree_2(K, V)::in, K::in, V::in,
    rtree_2(K, V)::out) is det <= region(K).

insert_and_split_child3(K0, T0, K1, T1, K2, T2, K, V, T) :-
    (
        T0 = leaf(_),
        T = four(K0, T0, K1, T1, K2, T2, K, leaf(V))
    ;
        T0 = two(_, _, _, _),
        NK0 = bounding_region(K, K0),
        insert_2(T0, K, V, NT0),
        T = three(NK0, NT0, K1, T1, K2, T2)
    ;
        T0 = three(_, _, _, _, _, _),
        NK0 = bounding_region(K, K0),
        insert_2(T0, K, V, NT0),
        T = three(NK0, NT0, K1, T1, K2, T2)
    ;
        T0 = four(_, _, _, _, _, _, _, _),
        split_4_node(T0, K3, T3, K4, T4),
        Result = choose_subtree(K, K2, K3),
        (
            Result = min2_first,
            K5 = bounding_region(K, K3),
            insert_2(T3, K, V, T5),
            T = four(K1, T1, K2, T2, K4, T4, K5, T5)
        ;
            Result = min2_second,
            K5 = bounding_region(K, K4),
            insert_2(T4, K, V, T5),
            T = four(K1, T1, K2, T2, K3, T3, K5, T5)
        )
    ).

%---------------------------------------------------------------------------%
%
% Node splitting.
%

    % Split a 4-node into two 2-nodes.
    % Attempts to minimise the size of the resulting keys.
    %
:- pred split_4_node(rtree_2(K, V)::in(four), K::out, rtree_2(K, V)::out,
    K::out, rtree_2(K, V)::out) is det <= region(K).

split_4_node(Four, K4, T4, K5, T5) :-
    Four = four(K0, T0, K1, T1, K2, T2, K3, T3),
    A01 = bounding_region_size(K0, K1),
    A23 = bounding_region_size(K2, K3),
    A0123 = A01 + A23,
    A02 = bounding_region_size(K0, K2),
    A13 = bounding_region_size(K1, K3),
    A0213 = A02 + A13,
    A03 = bounding_region_size(K0, K3),
    A12 = bounding_region_size(K1, K2),
    A0312 = A03 + A12,
    ( if A0123 =< A0213 then
        ( if A0123 =< A0312 then
            Min = min3_first
        else
            Min = min3_third
        )
    else
        ( if A0213 =< A0312 then
            Min = min3_second
        else
            Min = min3_third
        )
    ),
    (
        Min = min3_first,
        K4 = bounding_region(K0, K1),
        T4 = two(K0, T0, K1, T1),
        K5 = bounding_region(K2, K3),
        T5 = two(K2, T2, K3, T3)
    ;
        Min = min3_second,
        K4 = bounding_region(K0, K2),
        T4 = two(K0, T0, K2, T2),
        K5 = bounding_region(K1, K3),
        T5 = two(K1, T1, K3, T3)
    ;
        Min = min3_third,
        K4 = bounding_region(K0, K3),
        T4 = two(K0, T0, K3, T3),
        K5 = bounding_region(K1, K2),
        T5 = two(K1, T1, K2, T2)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Deletion.
%

    % When deleting from an rtree we may need to collect some subtrees that
    % need to be reinserted. These subtrees are called orphan entries.
    %
:- type orphan(K, V)
    --->    orphan(K, rtree_2(K, V)).

:- type orphans(K, V) == list(orphan(K, V)).

:- type delete_info(K, V)
    --->    deleting(orphans(K, V))
    ;       finished(int, orphans(K, V)).

delete(K, V, one(K0, V), empty) :-
    contains(K, K0).
delete(K, V, !Tree) :-
    some [!T] (
        !.Tree = rtree(!:T),
        delete_2(!.T, K, V, 1, _, !:T, Info),
        (
            Info = finished(Depth, Orphans),
            reinsert_deleted_subtrees(Orphans, Depth, !T),
            !:Tree = rtree(!.T)
        ;
            Info = deleting(Orphans),
            % We are still deleting and we have reached the root node. This
            % means the path to the deleted leaf contained all 2-nodes
            % (including the root-node).
            (
                Orphans = [ Orphan | Orphans0],
                Orphan  = orphan(OrphanKey, OrphanTree),
                % Here we detect the special case that the root was a 2-node
                % with two leaves (& one was deleted). Thus we need to drop
                % back to a 1-node.
                ( if OrphanTree = leaf(OrphanValue) then
                    ( if
                        Orphans0 = []
                    then
                        !:Tree = one(OrphanKey, OrphanValue)
                    else if
                        Orphans0 = [orphan(NextOrphanKey, NextOrphanTree)]
                    then
                        !:T = two(OrphanKey, OrphanTree, NextOrphanKey,
                            NextOrphanTree),
                        !:Tree = rtree(!.T)
                    else
                        error("delete: unbalanced rtree")
                    )
                else
                    reinsert_deleted_subtrees(Orphans0, 1, OrphanTree, !:T),
                    !:Tree = rtree(!.T)
                )
            ;
                Orphans = [],
                error("delete: expected delete info")
            )
        )
    ).

    % Algorithm: descend into subtrees with bounding regions that contain the
    % query key. Fail if key-value pair is not found in any subtree.
    %
:- pred delete_2(rtree_2(K, V)::in, K::in, V::in, int::in, K::out,
    rtree_2(K, V)::out, delete_info(K, V)::out) is semidet <= region(K).

delete_2(leaf(V), K, V, _, K, leaf(V), deleting([])).
delete_2(two(K0, T0, K1, T1), K, V, Depth, DK, DT, Info) :-
    ( if try_deletion2(K0, T0, K1, T1, K, V, Depth, DK0, DT0, Info0) then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else
        try_deletion2(K1, T1, K0, T0, K, V, Depth, DK, DT, Info)
    ).
delete_2(three(K0, T0, K1, T1, K2, T2), K, V, Depth, DK, DT, Info) :-
    ( if
        try_deletion3(K0, T0, K1, T1, K2, T2, K, V, Depth, DK0, DT0, Info0)
    then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else if
        try_deletion3(K1, T1, K0, T0, K2, T2, K, V, Depth, DK0, DT0, Info0)
    then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else
        try_deletion3(K2, T2, K0, T0, K1, T1, K, V, Depth, DK, DT, Info)
    ).
delete_2(four(K0, T0, K1, T1, K2, T2, K3, T3), K, V, Depth, DK, DT, Info) :-
    ( if
        try_deletion4(K0, T0, K1, T1, K2, T2, K3, T3, K, V, Depth, DK0, DT0,
            Info0)
    then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else if
        try_deletion4(K1, T1, K0, T0, K2, T2, K3, T3, K, V, Depth, DK0, DT0,
            Info0)
    then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else if
        try_deletion4(K2, T2, K0, T0, K1, T1, K3, T3, K, V, Depth, DK0, DT0,
            Info0)
    then
        DK = DK0,
        DT = DT0,
        Info = Info0
    else
        try_deletion4(K3, T3, K0, T0, K1, T1, K2, T2, K, V, Depth, DK, DT,
            Info)
    ).

%---------------------------------------------------------------------------%

:- pred try_deletion2(K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    K::in, V::in, int::in, K::out, rtree_2(K, V)::out, delete_info(K, V)::out)
    is semidet <= region(K).

try_deletion2(K0, T0, K1, T1, K, V, Depth, DK, DT, Info) :-
    contains(K, K0),
    delete_2(T0, K, V, Depth + 1, DK0, DT0, Info0),
    (
        Info0 = deleting(DLs),
        Del   = orphan(K1, T1),
        Info  = deleting([Del | DLs]),
        DT    = DT0,
        DK    = K
    ;
        Info0 = finished(_, _),
        DT    = two(DK0, DT0, K1, T1),
        DK    = bounding_region(K1, DK0),
        Info  = Info0
    ).

:- pred try_deletion3(K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    K::in, rtree_2(K, V)::in, K::in, V::in, int::in, K::out,
    rtree_2(K, V)::out, delete_info(K, V)::out) is semidet <= region(K).

try_deletion3(K0, T0, K1, T1, K2, T2, K, V, Depth, DK, DT, DI) :-
    contains(K, K0),
    delete_2(T0, K, V, Depth + 1, DK0, DT0, DI0),
    (
        DI0 = deleting(DLs),
        DI = finished(Depth + 1, DLs),
        DT = two(K1, T1, K2, T2),
        DK = bounding_region(K1, K2)
    ;
        DI0 = finished(_, _),
        DI = DI0,
        DT = three(DK0, DT0, K1, T1, K2, T2),
        TK = bounding_region(DK0, K1),
        DK = bounding_region(TK, K2)
    ).

:- pred try_deletion4(K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in, K::in, V::in, int::in,
    K::out, rtree_2(K, V)::out, delete_info(K, V)::out) is semidet
    <= region(K).

try_deletion4(K0, T0, K1, T1, K2, T2, K3, T3, K, V, D, DK, DT, DI) :-
    contains(K, K0),
    delete_2(T0, K, V, D + 1, DK0, DT0, DI0),
    (
        DI0 = deleting(DLs),
        DI  = finished(D + 1, DLs),
        DT  = three(K1, T1, K2, T2, K3, T3),
        K12 = bounding_region(K1, K2),
        DK  = bounding_region(K3, K12)
    ;
        DI0 = finished(_, _),
        DI  = DI0,
        DT  = four(DK0, DT0, K1, T1, K2, T2, K3, T3),
        TK  = bounding_region(DK0, K1),
        K23 = bounding_region(K2, K3),
        DK  = bounding_region(TK, K23)
    ).

%---------------------------------------------------------------------------%

    % Given a list of deleted trees (with their bounding regions),
    % (re)insert the trees back into the main tree at the specified depth.
    %
:- pred reinsert_deleted_subtrees(orphans(K, V)::in, int::in,
    rtree_2(K, V)::in, rtree_2(K, V)::out) is det <= region(K).

reinsert_deleted_subtrees([], _, !T).
reinsert_deleted_subtrees([orphan(K, T) | DLs], Depth, T0, T2) :-
    T1 = insert_tree(T0, K, T, 1, Depth),
    ( if T0 = four(_, _, _, _, _, _, _, _) then
        reinsert_deleted_subtrees(DLs, Depth + 2, T1, T2)
    else
        reinsert_deleted_subtrees(DLs, Depth + 1, T1, T2)
    ).

    % The code here is almost identical to 'insert', however we are
    % inserting a tree at depth D0 as opposed to data to a leaf.
    %
:- func insert_tree(rtree_2(K, V), K, rtree_2(K, V), int, int) =
    rtree_2(K, V) <= region(K).

insert_tree(leaf(_), _, _, _, _) =
    func_error("insert_tree: leaf unexpected").
insert_tree(two(K0, T0, K1, T1), K, S, D0, D) = T :-
    ( if D0 = D then
        T = three(K0, T0, K1, T1, K, S)
    else
        Result = choose_subtree(K, K0, K1),
        (
            Result = min2_first,
            insert_tree_and_split_child2(K0, T0, K1, T1, K, S, D0 + 1, D, T)
        ;
            Result = min2_second,
            insert_tree_and_split_child2(K1, T1, K0, T0, K, S, D0 + 1, D, T)
        )
    ).
insert_tree(three(K0, T0, K1, T1, K2, T2), K, S, D0, D) = T :-
    ( if D0 = D then
        T = four(K0, T0, K1, T1, K2, T2, K, S)
    else
        Result = choose_subtree(K, K0, K1, K2),
        (
            Result = min3_first,
            insert_tree_and_split_child3(K0, T0, K1, T1, K2, T2, K, S,
                D0 + 1, D, T)
        ;
            Result = min3_second,
            insert_tree_and_split_child3(K1, T1, K0, T0, K2, T2, K, S,
                D0 + 1, D, T)
        ;
            Result = min3_third,
            insert_tree_and_split_child3(K2, T2, K0, T0, K1, T1, K, S,
                D0 + 1, D, T)
        )
    ).
insert_tree(Node, K, S, _, D) = T :-
    Node = four(_, _, _, _, _, _, _, _),
    split_4_node(Node, K0, T0, K1, T1),
    NRT = two(K0, T0, K1, T1),
    T = insert_tree(NRT, K, S, 1, D + 1).

:- pred insert_tree_and_split_child2(K::in, rtree_2(K, V)::in, K::in,
    rtree_2(K, V)::in, K::in, rtree_2(K, V)::in, int::in, int::in,
    rtree_2(K, V)::out) is det <= region(K).

insert_tree_and_split_child2(K0, T0, K1, T1, K, S, D0, D, T) :-
    (
        T0 = leaf(_),
        T  = three(K0, T0, K1, T1, K, S)
    ;
        T0  = two(_, _, _, _),
        NK0 = bounding_region(K, K0),
        NT0 = insert_tree(T0, K, S, D0, D),
        T   = two(NK0, NT0, K1, T1)
    ;
        T0  = three(_, _, _, _, _, _),
        NK0 = bounding_region(K, K0),
        NT0 = insert_tree(T0, K, S, D0, D),
        T   = two(NK0, NT0, K1, T1)
    ;
        T0 = four(_, _, _, _, _, _, _, _),
        split_4_node(T0, K2, T2, K3, T3),
        Result = choose_subtree(K, K2, K3),
        (
            Result = min2_first,
            K4 = bounding_region(K, K2),
            T4 = insert_tree(T2, K, S, D0, D),
            T  = three(K1, T1, K3, T3, K4, T4)
        ;
            Result = min2_second,
            K4 = bounding_region(K, K3),
            T4 = insert_tree(T3, K, S, D0, D),
            T  = three(K1, T1, K2, T2, K4, T4)
        )
    ).

:- pred insert_tree_and_split_child3(K::in, rtree_2(K, V)::in, K::in,
    rtree_2(K, V)::in, K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    int::in, int::in, rtree_2(K, V)::out) is det <= region(K).

insert_tree_and_split_child3(K0, T0, K1, T1, K2, T2, K, S, D0, D, T) :-
    (
        T0 = leaf(_),
        T  = four(K0, T0, K1, T1, K2, T2, K, S)
    ;
        T0  = two(_, _, _, _),
        NK0 = bounding_region(K, K0),
        NT0 = insert_tree(T0, K, S, D0, D),
        T   = three(NK0, NT0, K1, T1, K2, T2)
    ;
        T0  = three(_, _, _, _, _, _),
        NK0 = bounding_region(K, K0),
        NT0 = insert_tree(T0, K, S, D0, D),
        T   = three(NK0, NT0, K1, T1, K2, T2)
    ;
        T0 = four(_, _, _, _, _, _, _, _),
        split_4_node(T0, K3, T3, K4, T4),
        Result = choose_subtree(K, K2, K3),
        (
            Result = min2_first,
            K5 = bounding_region(K, K3),
            T5 = insert_tree(T3, K, S, D0, D),
            T  = four(K1, T1, K2, T2, K4, T4, K5, T5)
        ;
            Result = min2_second,
            K5 = bounding_region(K, K4),
            T5 = insert_tree(T4, K, S, D0, D),
            T  = four(K1, T1, K2, T2, K3, T3, K5, T5)
        )
    ).

%---------------------------------------------------------------------------%
%
% Search_intersects.
%

search_intersects(empty, _) = [].
search_intersects(one(K, V), QueryKey) =
    (if intersects(QueryKey, K) then [V] else []).
search_intersects(rtree(RTree), QueryKey) = Values :-
    search_intersects_2(RTree, QueryKey, [], Values).

    % Algorithm: descend into subtrees with bounding regions that intersect
    % the query key and accumulate leaf values.
    %
:- pred search_intersects_2(rtree_2(K, V)::in, K::in, list(V)::in,
    list(V)::out) is det <= region(K).

search_intersects_2(leaf(Value), _QueryKey, Values, [Value | Values]).
search_intersects_2(two(K0, T0, K1, T1), QueryKey, !Values) :-
    search_intersects_subtree(K0, T0, QueryKey, !Values),
    search_intersects_subtree(K1, T1, QueryKey, !Values).
search_intersects_2(three(K0, T0, K1, T1, K2, T2), QueryKey, !Values) :-
    search_intersects_subtree(K0, T0, QueryKey, !Values),
    search_intersects_subtree(K1, T1, QueryKey, !Values),
    search_intersects_subtree(K2, T2, QueryKey, !Values).
search_intersects_2(four(K0, T0, K1, T1, K2, T2, K3, T3), QueryKey, !Values) :-
    search_intersects_subtree(K0, T0, QueryKey, !Values),
    search_intersects_subtree(K1, T1, QueryKey, !Values),
    search_intersects_subtree(K2, T2, QueryKey, !Values),
    search_intersects_subtree(K3, T3, QueryKey, !Values).

:- pred search_intersects_subtree(K::in, rtree_2(K, V)::in, K::in,
    list(V)::in, list(V)::out) is det <= region(K).

search_intersects_subtree(K, T, QueryKey, !Values) :-
    ( if intersects(QueryKey, K) then
        search_intersects_2(T, QueryKey, !Values)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Search_contains.
%

search_contains(empty, _) = [].
search_contains(one(K0, V0), K) =
    (if contains(K, K0) then [V0] else []).
search_contains(rtree(T), K) = Vs :-
    search_contains_2(T, K, [], Vs).

    % Algorithm: descend into subtrees with bounding regions that contain
    % the query key and accumulate leaf values.
    %
:- pred search_contains_2(rtree_2(K, V)::in, K::in, list(V)::in, list(V)::out)
    is det <= region(K).

search_contains_2(leaf(Value), _QueryKey, Values, [Value | Values]).
search_contains_2(two(K0, T0, K1, T1), QueryKey, !Values) :-
    search_contains_subtree(K0, T0, QueryKey, !Values),
    search_contains_subtree(K1, T1, QueryKey, !Values).
search_contains_2(three(K0, T0, K1, T1, K2, T2), QueryKey, !Values) :-
    search_contains_subtree(K0, T0, QueryKey, !Values),
    search_contains_subtree(K1, T1, QueryKey, !Values),
    search_contains_subtree(K2, T2, QueryKey, !Values).
search_contains_2(four(K0, T0, K1, T1, K2, T2, K3, T3), QueryKey, !Values) :-
    search_contains_subtree(K0, T0, QueryKey, !Values),
    search_contains_subtree(K1, T1, QueryKey, !Values),
    search_contains_subtree(K2, T2, QueryKey, !Values),
    search_contains_subtree(K3, T3, QueryKey, !Values).

:- pred search_contains_subtree(K::in, rtree_2(K, V)::in, K::in,
    list(V)::in, list(V)::out) is det <= region(K).

search_contains_subtree(K, T, QueryKey, !Values) :-
    ( if contains(QueryKey, K) then
        search_contains_2(T, QueryKey, !Values)
    else
        true
    ).

%---------------------------------------------------------------------------%

search_general(_KeyTest, _ValueTest, empty) = [].
search_general(KeyTest, ValueTest, one(K, V)) =
    (if KeyTest(K), ValueTest(V) then [V] else []).
search_general(KeyTest, ValueTest, rtree(T)) = Values :-
    search_general_2(T, KeyTest, ValueTest, [], Values).

    % Algorithm: descend into subtrees with bounding regions that satisfy
    % the key test and accumulate leaf values that satisfy the value test.
    %
:- pred search_general_2(rtree_2(K, V)::in,
    pred(K)::in(pred(in) is semidet), pred(V)::in(pred(in) is semidet),
    list(V)::in, list(V)::out) is det.

search_general_2(leaf(Value), _, ValueTest, !Values) :-
    ( if ValueTest(Value) then
        !:Values = [ Value | !.Values ]
    else
        true
    ).
search_general_2(Node, KeyTest, ValueTest, !Values) :-
    Node = two(K0, T0, K1, T1),
    search_general_subtree(K0, T0, KeyTest, ValueTest, !Values),
    search_general_subtree(K1, T1, KeyTest, ValueTest, !Values).
search_general_2(Node, KeyTest, ValueTest, !Values) :-
    Node = three(K0, T0, K1, T1, K2, T2),
    search_general_subtree(K0, T0, KeyTest, ValueTest, !Values),
    search_general_subtree(K1, T1, KeyTest, ValueTest, !Values),
    search_general_subtree(K2, T2, KeyTest, ValueTest, !Values).
search_general_2(Node, KeyTest, ValueTest, !Values) :-
    Node = four(K0, T0, K1, T1, K2, T2, K3, T3),
    search_general_subtree(K0, T0, KeyTest, ValueTest, !Values),
    search_general_subtree(K1, T1, KeyTest, ValueTest, !Values),
    search_general_subtree(K2, T2, KeyTest, ValueTest, !Values),
search_general_subtree(K3, T3, KeyTest, ValueTest, !Values).

:- pred search_general_subtree(K::in, rtree_2(K, V)::in,
    pred(K)::in(pred(in) is semidet),
    pred(V)::in(pred(in) is semidet),
    list(V)::in, list(V)::out) is det.

search_general_subtree(K, T, KeyTest, ValueTest, !Values) :-
    ( if KeyTest(K) then
        search_general_2(T, KeyTest, ValueTest, !Values)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Search_first.
%

search_first(P, C, one(K0, V0), L, V0, E0) :-
    maybe_limit(K0, P, L, _),
    maybe_limit(V0, C, L, E0).
search_first(P, C, rtree(T), L, V, E) :-
    search_first_2(T, P, C, L, V, E).

    % maybe_limit(K, P, L, E) holds if P(K, E) holds and E is less than the
    % limit L.
    %
:- pred maybe_limit(K::in, pred(K, E)::in(pred(in, out) is semidet),
    E::in, E::out) is semidet.

maybe_limit(K, P, L, E) :-
    P(K, E),
    compare((<), E, L).

    % Algorithm: searches for the first element by traversing the tree in
    % the order induced by KTest. If we find a solution, we try and find
    % a better solution by setting a tighter maximum.
    %
    % We avoid searching the entire tree by (1) not searching subtrees that
    % fail KTest, and (2) not searching trees with a value greater than the
    % maximum.
    %
:- pred search_first_2(rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2(in, pred(in, out) is semidet, pred(in, out) is semidet,
    in, out, out) is semidet.

search_first_2(leaf(V), _, C, L, V, E) :-
    maybe_limit(V, C, L, E).
search_first_2(two(K0, T0, K1, T1), P, C, L, V, E) :-
    ( if maybe_limit(K0, P, L, E0) then
        ( if maybe_limit(K1, P, L, E1) then
            search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
        else
            search_first_2(T0, P, C, L, V, E)
        )
    else
        maybe_limit(K1, P, L, _),
        search_first_2(T1, P, C, L, V, E)
    ).
search_first_2(three(K0, T0, K1, T1, K2, T2), P, C, L, V, E) :-
    ( if maybe_limit(K0, P, L, E0) then
        ( if maybe_limit(K1, P, L, E1) then
            ( if maybe_limit(K2, P, L, E2) then
                search_first_2_three_choices(E0, E1, E2, T0, T1, T2, P, C,
                    L, V, E)
            else
                search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
            )
        else if maybe_limit(K2, P, L, E2) then
            search_first_2_two_choices(E0, E2, T0, T2, P, C, L, V, E)
        else
            search_first_2(T0, P, C, L, V, E)
        )
    else if maybe_limit(K1, P, L, E1) then
        ( if maybe_limit(K2, P, L, E2) then
            search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
        else
            search_first_2(T1, P, C, L, V, E)
        )
    else
        maybe_limit(K2, P, L, _),
        search_first_2(T2, P, C, L, V, E)
    ).
search_first_2(four(K0, T0, K1, T1, K2, T2, K3, T3), P, C, L, V, E) :-
    ( if maybe_limit(K0, P, L, E0) then
        ( if maybe_limit(K1, P, L, E1) then
            ( if maybe_limit(K2, P, L, E2) then
                ( if maybe_limit(K3, P, L, E3) then
                    search_first_2_four_choices(E0, E1, E2, E3, T0, T1, T2,
                        T3, P, C, L, V, E)
                else
                    search_first_2_three_choices(E0, E1, E2, T0, T1, T2, P,
                        C, L, V, E)
                )
            else
                ( if maybe_limit(K3, P, L, E3) then
                    search_first_2_three_choices(E0, E1, E3, T0, T1, T3, P,
                        C, L, V, E)
                else
                    search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
                )
            )
        else
            ( if maybe_limit(K2, P, L, E2) then
                ( if maybe_limit(K3, P, L, E3) then
                    search_first_2_three_choices(E0, E2, E3, T0, T2, T3, P,
                        C, L, V, E)
                else
                    search_first_2_two_choices(E0, E2, T0, T2, P, C, L, V, E)
                )
            else
                ( if maybe_limit(K3, P, L, E3) then
                    search_first_2_two_choices(E0, E3, T0, T3, P, C, L, V, E)
                else
                    search_first_2(T0, P, C, L, V, E)
                )
            )
        )
    else if maybe_limit(K1, P, L, E1) then
        ( if maybe_limit(K2, P, L, E2) then
            ( if maybe_limit(K3, P, L, E3) then
                search_first_2_three_choices(E1, E2, E3, T1, T2, T3, P, C, L,
                    V, E)
            else
                search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
            )
        else
            ( if maybe_limit(K3, P, L, E3) then
                search_first_2_two_choices(E1, E3, T1, T3, P, C, L, V, E)
            else
                search_first_2(T1, P, C, L, V, E)
            )
        )
    else if maybe_limit(K2, P, L, E2) then
        ( if maybe_limit(K3, P, L, E3) then
            search_first_2_two_choices(E2, E3, T2, T3, P, C, L, V, E)
        else
            search_first_2(T2, P, C, L, V, E)
        )
    else
        maybe_limit(K3, P, L, _),
        search_first_2(T3, P, C, L, V, E)
    ).

    % Search the "closest" subtree first.
    %
:- pred search_first_2_two_choices(E, E, rtree_2(K, V), rtree_2(K, V),
    pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_two_choices(in, in, in, in, pred(in, out) is semidet,
    pred(in, out) is semidet, in, out, out) is semidet.

search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E) :-
    ( if compare((<), E0, E1) then
        search_first_2_try_first_from_two(E1, T0, T1, P, C, L, V, E)
    else
        search_first_2_try_first_from_two(E0, T1, T0, P, C, L, V, E)
    ).

:- pred search_first_2_three_choices(E::in, E::in, E::in,
    rtree_2(K, V)::in, rtree_2(K, V)::in, rtree_2(K, V)::in,
    pred(K, E)::in(pred(in, out) is semidet),
    pred(V, E)::in(pred(in, out) is semidet), E::in, V::out, E::out)
    is semidet.

search_first_2_three_choices(E0, E1, E2, T0, T1, T2, P, C, L, V, E) :-
    R = minimum_of_three(E0, E1, E2),
    (
        R = min3_first,
        search_first_2_try_first_from_three(E1, E2, T0, T1, T2, P, C, L, V, E)
    ;
        R = min3_second,
        search_first_2_try_first_from_three(E0, E2, T1, T0, T2, P, C, L, V, E)
    ;
        R = min3_third,
        search_first_2_try_first_from_three(E0, E1, T2, T0, T1, P, C, L, V, E)
    ).

:- pred search_first_2_four_choices(E, E, E, E, rtree_2(K, V), rtree_2(K, V),
    rtree_2(K, V), rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_four_choices(in, in, in, in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, in, out, out)
    is semidet.

search_first_2_four_choices(E0, E1, E2, E3, T0, T1, T2, T3, P, C, L, V, E) :-
    R = minimum_of_four(E0, E1, E2, E3),
    (
        R = min4_first,
        search_first_2_try_first_from_four(E1, E2, E3, T0, T1, T2, T3, P, C,
            L, V, E)
    ;
        R = min4_second,
        search_first_2_try_first_from_four(E0, E2, E3, T1, T0, T2, T3, P, C,
            L, V, E)
    ;
        R = min4_third,
        search_first_2_try_first_from_four(E0, E1, E3, T2, T0, T1, T3, P, C,
            L, V, E)
    ;
        R = min4_fourth,
        search_first_2_try_first_from_four(E0, E1, E2, T3, T0, T1, T2, P, C,
            L, V, E)
    ).

    % Search the first subtree, if we find a solution, we then try and find
    % a better solution. Otherwise we search the remaining 3 choices.
    % Arguments are ordered in terms of "goodness".
    %
:- pred search_first_2_try_first_from_four(E, E, E, rtree_2(K, V),
    rtree_2(K, V),
    rtree_2(K, V), rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_four(in, in, in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, in, out, out)
    is semidet.

search_first_2_try_first_from_four(E1, E2, E3, T0, T1, T2, T3,
        P, C, L, V, E) :-
    ( if search_first_2(T0, P, C, L, V0, E0) then
        search_first_2_find_better_solution_three(V0, E0, E1, E2, E3, T1, T2,
            T3, P, C, V, E)
    else
        search_first_2_three_choices(E1, E2, E3, T1, T2, T3, P, C, L, V, E)
    ).

:- pred search_first_2_try_first_from_three(E, E, rtree_2(K, V), rtree_2(K, V),
    rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_three(in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, in, out, out)
    is semidet.

search_first_2_try_first_from_three(E1, E2, T0, T1, T2, P, C, L, V, E) :-
    ( if search_first_2(T0, P, C, L, V0, E0) then
        search_first_2_find_better_solution_two(V0, E0, E1, E2, T1, T2, P,
            C, V, E)
    else
        search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
    ).

:- pred search_first_2_try_first_from_two(E, rtree_2(K, V), rtree_2(K, V),
    pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_two(in, in, in, pred(in, out) is semidet,
    pred(in, out) is semidet, in, out, out) is semidet.

search_first_2_try_first_from_two(E1, T0, T1, P, C, L, V, E) :-
    ( if search_first_2(T0, P, C, L, V0, E0) then
        search_first_2_find_better_solution_one(V0, E0, E1, T1, P, C, V, E)
    else
        search_first_2(T1, P, C, L, V, E)
    ).

    % We have found a solution, however it may not be the best solution,
    % so we search the other possibilities. The first solution becomes the
    % new maximum, so it is likely the new searches are cheaper.
    %
:- pred search_first_2_find_better_solution_one(V, E, E, rtree_2(K, V),
    pred(K, E), pred(V, E), V, E).
:- mode search_first_2_find_better_solution_one(in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_find_better_solution_one(VM, EM, E0, T0, P, C, V, E) :-
    ( if compare((<), EM, E0) then
        V = VM,
        E = EM
    else if search_first_2(T0, P, C, EM, V0, F0) then
        ( if compare((<), EM, F0) then
            V = VM,
            E = EM
        else
            V = V0,
            E = F0
        )
    else
        V = VM,
        E = EM
    ).

:- pred search_first_2_find_better_solution_two(V, E, E, E, rtree_2(K, V),
    rtree_2(K, V), pred(K, E), pred(V, E), V, E).
:- mode search_first_2_find_better_solution_two(in, in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_find_better_solution_two(VM, EM, E0, E1, T0, T1, P, C, V, E) :-
    R = minimum_of_three(EM, E0, E1),
    (
        R = min3_first,
        V = VM,
        E = EM
    ;
        R = min3_second,
        search_first_2_better_solution_two(VM, EM, E1, T0, T1, P, C, V, E)
    ;
        R = min3_third,
        search_first_2_better_solution_two(VM, EM, E0, T1, T0, P, C, V, E)
    ).

:- pred search_first_2_better_solution_two(V, E, E, rtree_2(K, V),
    rtree_2(K, V), pred(K, E), pred(V, E), V, E).
:- mode search_first_2_better_solution_two(in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_better_solution_two(VM, EM, E1, T0, T1, P, C, V, E) :-
    ( if search_first_2(T0, P, C, EM, V0, F0) then
        ( if compare((<), EM, F0) then
            search_first_2_find_better_solution_one(VM, EM, E1, T1, P, C, V, E)
        else
            search_first_2_find_better_solution_one(V0, F0, E1, T1, P, C, V, E)
        )
    else
        search_first_2_find_better_solution_one(VM, EM, E1, T1, P, C, V, E)
    ).

:- pred search_first_2_find_better_solution_three(V, E, E, E, E, rtree_2(K, V),
    rtree_2(K, V), rtree_2(K, V), pred(K, E), pred(V, E), V, E).
:- mode search_first_2_find_better_solution_three(in, in, in, in, in, in, in,
    in, pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_find_better_solution_three(VM, EM, E0, E1, E2, T0, T1, T2, P,
        C, V, E) :-
    R = minimum_of_four(EM, E0, E1, E2),
    (
        R = min4_first,
        V = VM,
        E = EM
    ;
        R = min4_second,
        search_first_2_better_solution_three(VM, EM, E1, E2, T0, T1, T2, P,
            C, V, E)
    ;
        R = min4_third,
        search_first_2_better_solution_three(VM, EM, E0, E2, T1, T0, T2, P,
            C, V, E)
    ;
        R = min4_fourth,
        search_first_2_better_solution_three(VM, EM, E0, E1, T2, T0, T1, P,
            C, V, E)
    ).

:- pred search_first_2_better_solution_three(V, E, E, E, rtree_2(K, V),
    rtree_2(K, V), rtree_2(K, V), pred(K, E), pred(V, E), V, E).
:- mode search_first_2_better_solution_three(in, in, in, in, in, in, in,
    pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_better_solution_three(VM, EM, E1, E2, T0, T1, T2, P, C, V, E) :-
    ( if search_first_2(T0, P, C, EM, V0, F0) then
        ( if compare((<), EM, F0) then
            search_first_2_find_better_solution_two(VM, EM, E1, E2, T1, T2, P,
                C, V, E)
        else
            search_first_2_find_better_solution_two(V0, F0, E1, E2, T1, T2, P,
                C, V, E)
        )
    else
        search_first_2_find_better_solution_two(VM, EM, E1, E2, T1, T2, P, C,
            V, E)
    ).

%---------------------------------------------------------------------------%
%
% Search_general_fold.
%

search_general_fold(_, _, empty, !Acc).
search_general_fold(KTest, VPred, one(K, V), !Acc) :-
    ( if KTest(K) then
        VPred(K, V, !Acc)
    else
        true
    ).
search_general_fold(KTest, VPred, rtree(T), !Acc) :-
    search_general_fold_2(T, KTest, VPred, !Acc).

    % Similar to search_general, except call accumulator over values.
    %
:- pred search_general_fold_2(rtree_2(K, V), pred(K),
    pred(K, V, A, A), A, A).
:- mode search_general_fold_2(in, pred(in) is semidet,
    pred(in, in, in, out) is det, in, out) is det.
:- mode search_general_fold_2(in, pred(in) is semidet,
    pred(in, in, di, uo) is det, di, uo) is det.

search_general_fold_2(leaf(_), _, _, _, _) :-
    error("search_general_fold_2: unexpected leaf node").
search_general_fold_2(Node, KTest, VPred, !Acc) :-
    Node = two(K0, T0, K1, T1),
    search_general_fold_subtree(K0, T0, KTest, VPred, !Acc),
    search_general_fold_subtree(K1, T1, KTest, VPred, !Acc).
search_general_fold_2(Node, KTest, VPred, !Acc) :-
    Node = three(K0, T0, K1, T1, K2, T2),
    search_general_fold_subtree(K0, T0, KTest, VPred, !Acc),
    search_general_fold_subtree(K1, T1, KTest, VPred, !Acc),
    search_general_fold_subtree(K2, T2, KTest, VPred, !Acc).
search_general_fold_2(Node, KTest, VPred, !Acc) :-
    Node = four(K0, T0, K1, T1, K2, T2, K3, T3),
    search_general_fold_subtree(K0, T0, KTest, VPred, !Acc),
    search_general_fold_subtree(K1, T1, KTest, VPred, !Acc),
    search_general_fold_subtree(K2, T2, KTest, VPred, !Acc),
    search_general_fold_subtree(K3, T3, KTest, VPred, !Acc).

:- pred search_general_fold_subtree(K, rtree_2(K, V),
    pred(K), pred(K, V, A, A), A, A).
:- mode search_general_fold_subtree(in, in, in(pred(in) is semidet),
    in(pred(in, in, in, out) is det), in, out) is det.
:- mode search_general_fold_subtree(in, in, in(pred(in) is semidet),
    in(pred(in, in, di, uo) is det), di, uo) is det.

search_general_fold_subtree(K, T, KTest, VPred, !Acc) :-
    ( if KTest(K) then
        ( if T = leaf(V) then
            VPred(K, V, !Acc)
        else
            search_general_fold_2(T, KTest, VPred, !Acc)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Fold.
%

fold(_P, empty, !Acc).
fold(P, one(K, V), !Acc) :-
    P(K, V, !Acc).
fold(P, rtree(T), !Acc) :-
    rtree.fold_2(P, T, !Acc).

:- pred fold_2(pred(K, V, A, A), rtree_2(K, V), A, A).
:- mode fold_2(pred(in, in, in, out) is det, in, in, out) is det.
:- mode fold_2(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode fold_2(pred(in, in, in, out) is semidet, in, in, out) is semidet.

fold_2(_, leaf(_), _, _) :-
    error("fold: leaf unexpected").
fold_2(P, two(K0, T0, K1, T1), !Acc) :-
    fold_subtree(P, K0, T0, !Acc),
    fold_subtree(P, K1, T1, !Acc).
fold_2(P, three(K0, T0, K1, T1, K2, T2), !Acc) :-
    fold_subtree(P, K0, T0, !Acc),
    fold_subtree(P, K1, T1, !Acc),
    fold_subtree(P, K2, T2, !Acc).
fold_2(P, four(K0, T0, K1, T1, K2, T2, K3, T3), !Acc) :-
    fold_subtree(P, K0, T0, !Acc),
    fold_subtree(P, K1, T1, !Acc),
    fold_subtree(P, K2, T2, !Acc),
    fold_subtree(P, K3, T3, !Acc).

:- pred fold_subtree(pred(K, V, A, A), K, rtree_2(K, V), A, A).
:- mode fold_subtree(pred(in, in, in, out) is det, in, in, in, out) is det.
:- mode fold_subtree(pred(in, in, di, uo) is det, in, in, di, uo) is det.
:- mode fold_subtree(pred(in, in, in, out) is semidet, in, in, in, out)
    is semidet.

fold_subtree(P, K, T, !Acc) :-
    (
        T = leaf(V),
        P(K, V, !Acc)
    ;
        ( T = two(_, _, _, _)
        ; T = three(_, _, _, _, _, _)
        ; T = four(_, _, _, _, _, _, _, _)
        ),
        fold_2(P, T, !Acc)
    ).

%---------------------------------------------------------------------------%
%
% Map_values.
%

map_values(_, empty, empty).
map_values(P, one(K, V), one(K, W)) :-
    P(K, V, W).
map_values(P, rtree(T), rtree(U)) :-
    map_values_2(P, T, U).

:- pred map_values_2(pred(K, V, W), rtree_2(K, V), rtree_2(K, W)).
:- mode map_values_2(pred(in, in, out) is det, in, out) is det.
:- mode map_values_2(pred(in, in, out) is semidet, in, out) is semidet.

map_values_2(_, leaf(_), _) :-
    error("map_values_2: unexpected leaf.").
map_values_2(P, two(K0, T0, K1, T1), two(K0, U0, K1, U1)) :-
    map_values_key_2(P, K0, T0, U0),
    map_values_key_2(P, K1, T1, U1).
map_values_2(P, three(K0, T0, K1, T1, K2, T2),
        three(K0, U0, K1, U1, K2, U2)) :-
    map_values_key_2(P, K0, T0, U0),
    map_values_key_2(P, K1, T1, U1),
    map_values_key_2(P, K2, T2, U2).
map_values_2(P, four(K0, T0, K1, T1, K2, T2, K3, T3),
        four(K0, U0, K1, U1, K2, U2, K3, U3)) :-
    map_values_key_2(P, K0, T0, U0),
    map_values_key_2(P, K1, T1, U1),
    map_values_key_2(P, K2, T2, U2),
    map_values_key_2(P, K3, T3, U3).

:- pred map_values_key_2(pred(K, V, W), K, rtree_2(K, V), rtree_2(K, W)).
:- mode map_values_key_2(pred(in, in, out) is det, in, in, out) is det.
:- mode map_values_key_2(pred(in, in, out) is semidet, in, in, out) is semidet.

map_values_key_2(P, K, T, U) :-
    (
        T = leaf(V),
        P(K, V, W),
        U = leaf(W)
    ;
        ( T = two(_, _, _, _)
        ; T = three(_, _, _, _, _, _)
        ; T = four(_, _, _, _, _, _, _, _)
        ),
        map_values_2(P, T, U)
    ).

%---------------------------------------------------------------------------%

    % Find the minimum of three values.
    %
:- func minimum_of_three(T, T, T) = min_of_three_result.

minimum_of_three(A, B, C) =
    ( if compare((<), A, B) then
        ( if compare((<), A, C) then
            min3_first
        else
            min3_third
        )
    else
        ( if compare((<), B, C) then
            min3_second
        else
            min3_third
        )
    ).

%---------------------------------------------------------------------------%

    % Find the minimum of four values.
    %
:- func minimum_of_four(T, T, T, T) = min_of_four_result.

minimum_of_four(A, B, C, D) = Min :-
    ( if compare((<), A, B) then
        Min0 = min4_first,
        MinItem0 = A
    else
        Min0 = min4_second,
        MinItem0 = B
    ),
    ( if compare((<), MinItem0, C) then
        Min1 = Min0,
        MinItem = MinItem0
    else
        Min1 = min4_third,
        MinItem = C
    ),
    ( if compare((<), MinItem, D) then
        Min = Min1
    else
        Min = min4_fourth
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pre-defined regions.
%

%---------------------------------------------------------------------------%

:- instance region(box3d) where [
    pred(intersects/2) is box3d_intersects,
    pred(contains/2) is box3d_contains,
    func(size/1) is box3d_volume,
    func(bounding_region/2) is box3d_bounding_region,
    func(bounding_region_size/2) is box3d_bounding_region_volume
].

%---------------------------------------------------------------------------%

:- pred box3d_intersects(box3d::in, box3d::in) is semidet.

box3d_intersects(A, B) :-
    A = box3d(AXMin, AXMax, AYMin, AYMax, AZMin, AZMax),
    B = box3d(BXMin, BXMax, BYMin, BYMax, BZMin, BZMax),
    ( if AXMin =< BXMin then
        AXMax >= BXMin
    else
        AXMin =< BXMax
    ),
    ( if AYMin =< BYMin then
        AYMax >= BYMin
    else
        AYMin =< BYMax
    ),
    ( if AZMin =< BZMin then
        AZMax >= BZMin
    else
        AZMin =< BZMax
    ).

%---------------------------------------------------------------------------%

:- pred box3d_contains(box3d::in, box3d::in) is semidet.

box3d_contains(A, B) :-
    A = box3d(AXMin, AXMax, AYMin, AYMax, AZMin, AZMax),
    B = box3d(BXMin, BXMax, BYMin, BYMax, BZMin, BZMax),
    AXMin >= BXMin,
    AXMax =< BXMax,
    AYMin >= BYMin,
    AYMax =< BYMax,
    AZMin >= BZMin,
    AZMax =< BZMax.

%---------------------------------------------------------------------------%

:- func box3d_volume(box3d) = float.

box3d_volume(Box) = (XMax - XMin) * (YMax - YMin) * (ZMax - ZMin) :-
    Box = box3d(XMin, XMax, YMin, YMax, ZMin, ZMax).

%---------------------------------------------------------------------------%

:- func box3d_bounding_region(box3d, box3d) = box3d.

box3d_bounding_region(A, B) = C :-
    A = box3d(AXMin, AXMax, AYMin, AYMax, AZMin, AZMax),
    B = box3d(BXMin, BXMax, BYMin, BYMax, BZMin, BZMax),
    CXMin = min(AXMin, BXMin),
    CXMax = max(AXMax, BXMax),
    CYMin = min(AYMin, BYMin),
    CYMax = max(AYMax, BYMax),
    CZMin = min(AZMin, BZMin),
    CZMax = max(AZMax, BZMax),
    C = box3d(CXMin, CXMax, CYMin, CYMax, CZMin, CZMax).

%---------------------------------------------------------------------------%

:- func box3d_bounding_region_volume(box3d, box3d) = float.

box3d_bounding_region_volume(A, B) = Volume :-
    A = box3d(AXMin, AXMax, AYMin, AYMax, AZMin, AZMax),
    B = box3d(BXMin, BXMax, BYMin, BYMax, BZMin, BZMax),
    XMin = min(AXMin, BXMin),
    XMax = max(AXMax, BXMax),
    YMin = min(AYMin, BYMin),
    YMax = max(AYMax, BYMax),
    ZMin = min(AZMin, BZMin),
    ZMax = max(AZMax, BZMax),
    Volume = (XMax - XMin) * (YMax - YMin) * (ZMax - ZMin).

%---------------------------------------------------------------------------%

:- instance region(box) where [
    pred(intersects/2) is box_intersects,
    pred(contains/2) is box_contains,
    func(size/1) is box_area,
    func(bounding_region/2) is box_bounding_region,
    func(bounding_region_size/2) is box_bounding_region_area
].

%---------------------------------------------------------------------------%

:- pred box_intersects(box::in, box::in) is semidet.

box_intersects(A, B) :-
    A = box(AXMin, AXMax, AYMin, AYMax),
    B = box(BXMin, BXMax, BYMin, BYMax),
    ( if AXMin =< BXMin then
        AXMax >= BXMin
    else
        AXMin =< BXMax
    ),
    ( if AYMin =< BYMin then
        AYMax >= BYMin
    else
        AYMin =< BYMax
    ).

%---------------------------------------------------------------------------%

:- pred box_contains(box::in, box::in) is semidet.

box_contains(A, B) :-
    A = box(AXMin, AXMax, AYMin, AYMax),
    B = box(BXMin, BXMax, BYMin, BYMax),
    AXMin >= BXMin,
    AXMax =< BXMax,
    AYMin >= BYMin,
    AYMax =< BYMax.

%---------------------------------------------------------------------------%

:- func box_area(box) = float.

box_area(box(XMin, XMax, YMin, YMax)) = (XMax - XMin) * (YMax - YMin).

%---------------------------------------------------------------------------%

:- func box_bounding_region(box, box) = box.

box_bounding_region(A, B) = C :-
    A = box(AXMin, AXMax, AYMin, AYMax),
    B = box(BXMin, BXMax, BYMin, BYMax),
    CXMin = min(AXMin, BXMin),
    CXMax = max(AXMax, BXMax),
    CYMin = min(AYMin, BYMin),
    CYMax = max(AYMax, BYMax),
    C = box(CXMin, CXMax, CYMin, CYMax).

%---------------------------------------------------------------------------%

:- func box_bounding_region_area(box, box) = float.

box_bounding_region_area(A, B) = (XMax - XMin) * (YMax - YMin) :-
    A = box(AXMin, AXMax, AYMin, AYMax),
    B = box(BXMin, BXMax, BYMin, BYMax),
    XMin = min(AXMin, BXMin),
    XMax = max(AXMax, BXMax),
    YMin = min(AYMin, BYMin),
    YMax = max(AYMax, BYMax).

%---------------------------------------------------------------------------%

:- instance region(interval) where [
    pred(intersects/2) is interval_intersects,
    pred(contains/2) is interval_contains,
    func(size/1) is interval_length,
    func(bounding_region/2) is interval_bounding_region,
    func(bounding_region_size/2) is interval_bounding_region_length
].

%---------------------------------------------------------------------------%

:- pred interval_intersects(interval::in, interval::in) is semidet.

interval_intersects(A, B) :-
    A = interval(AMin, AMax),
    B = interval(BMin, BMax),
    ( if AMin =< BMin then
        AMax >= BMin
    else
        AMin =< BMax
    ).

%---------------------------------------------------------------------------%

:- pred interval_contains(interval::in, interval::in) is semidet.

interval_contains(A, B) :-
    A = interval(AMin, AMax),
    B = interval(BMin, BMax),
    AMin >= BMin,
    AMax =< BMax.

%---------------------------------------------------------------------------%

:- func interval_length(interval) = float.

interval_length(interval(Max, Min)) = Max - Min.

%---------------------------------------------------------------------------%

:- func interval_bounding_region(interval, interval) = interval.

interval_bounding_region(A, B) = interval(min(AMin, BMin), max(AMax, BMax)) :-
    A = interval(AMin, AMax),
    B = interval(BMin, BMax).

%---------------------------------------------------------------------------%

:- func interval_bounding_region_length(interval, interval) = float.

interval_bounding_region_length(A, B) = max(AMax, BMax) - min(AMin, BMin) :-
    A = interval(AMin, AMax),
    B = interval(BMin, BMax).

%---------------------------------------------------------------------------%
:- end_module rtree.
%---------------------------------------------------------------------------%
