%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: rtree.m.
% Main author: gjd.
% Stability: low.
% 
% This file provides a region tree (R-tree) ADT.  An R-tree associates
% values with regions in some space, e.g. rectangles in the 2D plane, or
% bounding spheres in 3D space.  R-trees accept spatial queries, e.g. a
% typical usage is "find all pubs within a 2km radius".
% 
% This module provides a region(K) typeclass that allows the user to
% define new regions and spaces.  Three "builtin" instances for region(K)
% are provided: region(interval), region(box) and region(box3d) corresponding
% to "square" regions in 1, 2 and 3 dimensional space respectively.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module rtree.
:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type rtree(K, V).

:- typeclass region(K) where [

        % Tests whether two regions intersect.
        %
    pred intersects(K::in, K::in) is semidet,

        % Tests whether the first region is contained within the second.
        %
    pred contains(K::in, K::in) is semidet,

        % Computes the "size" of a region.
        % e.g. for a 2D box, the "size" is equivalent to area.
        %
    func size(K) = float,

        % Computes a bounding region that contains both input regions.
        % The resulting region should be as small as possible.
        % 
    func bounding_region(K, K) = K,

        % Computes the size of the bounding region returned by
        % bounding_region/2, i.e.
        %
        % bounding_region_size(K1, K2) = size(bounding_region(K1, K2)).
        %
        % Lazy programmers can use this definition, however usually a
        % better implementation can be found, e.g. for intervals:
        % 
        % bounding_region_size(interval(X0, X1), interval(Y0, Y1)) = 
        %       max(X1, Y1) - min(X0, Y0).
        % 
        % This version is more efficient since it does not create a 
        % temporary interval.
        %
    func bounding_region_size(K, K) = float
].

%-----------------------------------------------------------------------------%
    
    % Initialize an empty rtree.
    %
:- func rtree.init = (rtree(K, V)::uo) is det <= region(K).

    % Check whether an rtree is empty.
    % 
:- pred rtree.is_empty(rtree(K, V)::in) is semidet.

    % Insert a new key and corresponding value into an rtree.
    %
:- func rtree.insert(K, V, rtree(K, V)) = rtree(K, V) <= region(K).
:- pred rtree.insert(K::in, V::in, rtree(K, V)::in, rtree(K, V)::out) is det
    <= region(K).

    % Delete a key-value pair from an rtree.
    % Assumes that K is either the key for V, or is contained in the key for
    % V.
    %
    % Fails if the key-value pair is not in the tree.
    %
:- pred rtree.delete(K::in, V::in, rtree(K, V)::in, rtree(K, V)::out) 
    is semidet <= region(K).

    % Search for all values with keys that intersect the query key.
    % 
:- func rtree.search_intersects(rtree(K, V), K) = list(V) <= region(K).

    % Search for all values with keys that contain the query key.
    %
:- func rtree.search_contains(rtree(K, V), K) = list(V) <= region(K).

    % search_general(KTest, VTest, T) = V.
    %
    % Search for all values V with associated keys K that satisfy 
    % KTest(K) /\ VTest(V).  The search assumes that for all K1, K2 such
    % that K1 contains K2, then if KTest(K2) holds we have that KTest(K1) also
    % holds.
    %
    % We have that:
    %
    %   search_intersects(T, K, Vs)
    %       <=> search_general(intersects(K), true, T, Vs)
    %
    %   search_contains(T, K, Vs)
    %       <=> search_general(contains(K),true,T,Vs)
    %
:- func rtree.search_general(pred(K)::in(pred(in) is semidet),
    pred(V)::in(pred(in) is semidet), rtree(K, V)::in)
    = (list(V)::out) is det.

    % search_first(KTest, VTest, Max, T, V, L).
    %
    % Search for a value V with associated key K such that 
    % KTest(K, _) /\ VTest(V, L) is satisfied and there does not exist a
    % V' with K' such that KTest(K', _) /\ VTest(V', L') /\ (L' < L) is 
    % satisfied.  Fail if no such key-value pair exists.
    %
    % The search assumes that for all K1, K2 such that
    % K1 contains K2, then if KTest(K2, L2) holds we have that 
    % KTest(K1, L1) holds with L2 >= L1.
    %
    % If there exists multiple key-value pairs which satisfy the above 
    % conditions, then one of the candidates is chosen arbitrarily. 
    % 
:- pred rtree.search_first(pred(K, L), pred(V, L), rtree(K, V), L, V, L).
:- mode rtree.search_first(pred(in, out) is semidet, 
    pred(in, out) is semidet, in, in, out, out) is semidet.

    % search_general_fold(KTest, VPred, T, !A).
    %
    % Apply accumulator VPred to each key-value pair K-V that satisfies 
    % KTest(K).  The same assumptions for KTest from search_general apply
    % here.
    %
:- pred rtree.search_general_fold(pred(K), pred(K, V, A, A), rtree(K, V), 
    A, A).
:- mode rtree.search_general_fold(pred(in) is semidet, 
    pred(in, in, in, out) is det, in, in, out) is det.
:- mode rtree.search_general_fold(pred(in) is semidet,
    pred(in, in, di, uo) is det, in, di, uo) is det.

    % Perform a traversal of the rtree, applying an accumulator predicate
    % for each key-value pair.
    % 
:- pred rtree.fold(pred(K, V, A, A), rtree(K, V), A, A).
:- mode rtree.fold(pred(in, in, in, out) is det, in, in, out) is det.
:- mode rtree.fold(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode rtree.fold(pred(in, in, in, out) is semidet, in, in, out) is semidet.

    % Apply a transformation predicate to all the values in an rtree.
    %
:- pred rtree.map_values(pred(K, V, W), rtree(K, V), rtree(K, W)).
:- mode rtree.map_values(pred(in, in, out) is det, in, out) is det.
:- mode rtree.map_values(pred(in, in, out) is semidet, in, out) is semidet.

%---------------------------------------------------------------------------%
%
% Predefined regions
%
   
    % An interval type represented as interval(Min, Max).
    %
:- type interval ---> interval(float, float).
:- instance region(interval).

    % A 2D axis aligned box represented as box(XMin,XMax,YMin,YMax).
    % 
:- type box ---> box(float, float, float, float).
:- instance region(box).

    % A 3D axis aligned box represented as box(XMin,XMax,YMin,YMax,ZMin,ZMax).
    % 
:- type box3d ---> box3d(float, float, float, float, float, float).
:- instance region(box3d).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module require.

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
    % The key for the root node is not stored anywhere.  This means queries
    % outside the bounds of the tree will be slower, since we must descend
    % to level 1 instead of level 0.  However, we will win if the query is
    % within range.  Also doing this simplifies the code slightly.
    %
:- type rtree_2(K, V) 
    --->    leaf(V) 
    ;       two(K, rtree_2(K, V), K, rtree_2(K, V)) 
    ;       three(K, rtree_2(K, V), K, rtree_2(K, V), K, rtree_2(K, V)) 
    ;       four(K, rtree_2(K, V), K, rtree_2(K, V), K, rtree_2(K, V), K,
                rtree_2(K, V)).

:- inst four ==
    bound(four(ground, ground, ground, ground, ground, ground, ground, 
        ground)).

:- type min_of_three_result 
    --->    min3_first 
    ;       min3_second 
    ;       min3_third.

:- type min_of_four_result
    --->    min4_first
    ;       min4_second 
    ;       min4_third 
    ;       min4_fourth.

%-----------------------------------------------------------------------------%
%
% init
%

rtree.init = empty.

%-----------------------------------------------------------------------------%
%
% is_empty
%

rtree.is_empty(empty).

%-----------------------------------------------------------------------------%
%
% insert
%

rtree.insert(K, V, !.Tree) = !:Tree :-
    rtree.insert(K, V, !Tree).

rtree.insert(K, V, empty, one(K, V)).
rtree.insert(K, V, one(K0, V0), T) :-
    T = rtree(two(K0, leaf(V0), K, leaf(V))). 
rtree.insert(K, V, rtree(!.T), rtree(!:T)) :-
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
    Result = include2(K, K0, K1), 
    ( Result = min3_first ->
        insert_and_split_child2(K0, T0, K1, T1, K, V, T)
    ;
        insert_and_split_child2(K1, T1, K0, T0, K, V, T)
    ).
insert_2(three(K0, T0, K1, T1, K2, T2), K, V, T) :-
    Result = include3(K, K0, K1, K2), 
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
    NRT = two(K0, T0, K1, T1), 
    insert_2(NRT, K, V, T).

    % Decides which subtree to insert value with K0.
    %
:- func include2(K, K, K) = min_of_three_result <= region(K).

include2(K0, K1, K2) = Result :-
    A1 = size(K1), 
    A2 = size(K2), 
    A01 = bounding_region_size(K0, K1), 
    A02 = bounding_region_size(K0, K2), 
    D1 = A01 - A1, 
    D2 = A02 - A2, 
    include_min(D1, D2, A1, A2, min3_first, min3_second, Result).

    % Decides which subtree to insert value with K0.
    %
:- func include3(K, K, K, K) = min_of_three_result <= region(K).

include3(K0, K1, K2, K3) = Result :- 
    A1 = size(K1), 
    A2 = size(K2), 
    A3 = size(K3), 
    A01 = bounding_region_size(K0, K1), 
    A02 = bounding_region_size(K0, K2), 
    A03 = bounding_region_size(K0, K3), 
    D1 = A01 - A1, 
    D2 = A02 - A2, 
    D3 = A03 - A3, 
    include_min(D1, D2, A1, A2, min3_first, min3_second, Result0), 
    ( Result0 = min3_first ->
        include_min(D1, D3, A1, A3, min3_first, min3_third, Result)
    ;
        include_min(D2, D3, A2, A3, min3_second, min3_third, Result)
    ).

:- pred include_min(float::in, float::in, float::in, float::in,
    min_of_three_result::in, min_of_three_result::in,
    min_of_three_result::out) is det.

include_min(D1, D2, A1, A2, R1, R2, R3) :-
    ( D1 < D2 ->
        R3 = R1
    ; D1 > D2 ->
        R3 = R2
    ; A1 =< A2 ->
        R3 = R1
    ;
        R3 = R2
    ).
    
    % Split the child (if a 4 node) and insert into T0.
    %
:- pred insert_and_split_child2(K::in, rtree_2(K, V)::in, K::in,
    rtree_2(K, V)::in, K::in, V::in, rtree_2(K, V)::out) is det <= region(K).

insert_and_split_child2(K0, T0, K1, T1, K, V, T) :-
    ( T0 = leaf(_), 
        T = three(K0, T0, K1, T1, K, leaf(V))
    ; T0 = two(_, _, _, _), 
        NK0 = bounding_region(K, K0), 
        insert_2(T0, K, V, NT0), 
        T = two(NK0, NT0, K1, T1)
    ; T0 = three(_, _, _, _, _, _), 
        NK0 = bounding_region(K, K0), 
        insert_2(T0, K, V, NT0), 
        T = two(NK0, NT0, K1, T1)
    ; T0 = four(_, _, _, _, _, _, _, _), 
        split_4_node(T0, K2, T2, K3, T3), 
        Result = include2(K, K2, K3), 
        ( Result = min3_first ->
            K4 = bounding_region(K, K2), 
            insert_2(T2, K, V, T4), 
            T = three(K1, T1, K3, T3, K4, T4)
        ;
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
        Result = include2(K, K2, K3), 
        ( Result = min3_first ->
            K5 = bounding_region(K, K3), 
            insert_2(T3, K, V, T5), 
            T = four(K1, T1, K2, T2, K4, T4, K5, T5)
        ;
            K5 = bounding_region(K, K4), 
            insert_2(T4, K, V, T5), 
            T = four(K1, T1, K2, T2, K3, T3, K5, T5)
        )
    ).
    
    % Split a 4 node into two 2 nodes.
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
    ( A0123 =< A0213 ->
        ( A0123 =< A0312 ->
            Min = min3_first
        ;
            Min = min3_third
        )
    ;   ( A0213 =< A0312 ->
            Min = min3_second
        ;
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

%-----------------------------------------------------------------------------%
%
% delete
%    

    % When deleting from an rtree, we may need to collect some subtrees that
    % need to be reinserted.
    %
:- type deleted(K, V) 
    --->    deleted(K, rtree_2(K, V)).

:- type deleteds(K, V) == list(deleted(K, V)).

:- type delete_info(K, V) 
    --->    deleting(deleteds(K, V))
    ;       finished(int, deleteds(K, V)).

rtree.delete(K, V, one(K0, V), empty) :-
    contains(K, K0).
rtree.delete(K, V, rtree(T0), T) :-
    delete_2(T0, K, V, 1, _, DT, DI), 
    (
        DI = finished(D, DLs), 
        reinsert_deleted_subtrees(DLs, D, DT, T1), 
        T = rtree(T1)
    ;
        DI = deleting(DLs), 
        %
        % We are still deleting and we have reached the root node.  This 
        % means the path to the deleted leaf contained all 2-nodes 
        % (including the root-node).
        %
        (
            DLs = [deleted(NK, NT) | DLs0], 
            %
            % Here we detect the special case that the root was a 2-node
            % with two leaves (& one was deleted).  Thus we need to drop 
            % back to a 1-node.
            %
            ( NT = leaf(NV) ->
                ( DLs0 = [] ->
                    T = one(NK, NV)
                ; DLs0 = [deleted(NK1, NT1)] ->
                    T1 = two(NK, NT, NK1, NT1), 
                    T = rtree(T1)
                ;
                    error("delete: unbalanced rtree")
                )
            ;
                reinsert_deleted_subtrees(DLs0, 1, NT, T1), 
                T = rtree(T1)
            )
        ;
            DLs = [], 
            error("delete: expected delete info")
        )
    ).
    
    % Algorithm: descend into subtrees with bounding regions that contain the
    % query key.  Fail if key-value pair is not found in any subtree.
    %
:- pred delete_2(rtree_2(K, V)::in, K::in, V::in, int::in, K::out,
    rtree_2(K, V)::out, delete_info(K, V)::out) is semidet <= region(K).

delete_2(T, K, V, _, DK, DT, DeleteInfo) :-
    T = leaf(V), 
    DK = K, 
    DT = T, 
    DeleteInfo = deleting([]).
delete_2(two(K0, T0, K1, T1), K, V, D, DK, DT, DeleteInfo) :-
    ( try_deletion2(K0, T0, K1, T1, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DeleteInfo = DI0
    ;
        try_deletion2(K1, T1, K0, T0, K, V, D, DK, DT, DeleteInfo)
    ).
delete_2(three(K0, T0, K1, T1, K2, T2), K, V, D, DK, DT, DeleteInfo) :-
    ( try_deletion3(K0, T0, K1, T1, K2, T2, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DeleteInfo = DI0
    ; try_deletion3(K1, T1, K0, T0, K2, T2, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DeleteInfo = DI0
    ;
        try_deletion3(K2, T2, K0, T0, K1, T1, K, V, D, DK, DT, DeleteInfo)
    ).
delete_2(four(K0, T0, K1, T1, K2, T2, K3, T3), K, V, D, DK, DT, DI) :-
    ( try_deletion4(K0, T0, K1, T1, K2, T2, K3, T3, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DI = DI0
    ; try_deletion4(K1, T1, K0, T0, K2, T2, K3, T3, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DI = DI0
    ; try_deletion4(K2, T2, K0, T0, K1, T1, K3, T3, K, V, D, DK0, DT0, DI0) ->
        DK = DK0, 
        DT = DT0, 
        DI = DI0
    ;
        try_deletion4(K3, T3, K0, T0, K1, T1, K2, T2, K, V, D, DK, DT, DI)
    ).

:- pred try_deletion2(K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    K::in, V::in, int::in, K::out, rtree_2(K, V)::out, delete_info(K, V)::out)
    is semidet <= region(K).

try_deletion2(K0, T0, K1, T1, K, V, D, DK, DT, DI) :-
    contains(K, K0), 
    delete_2(T0, K, V, D + 1, DK0, DT0, DI0), 
    (
        DI0 = deleting(DLs), 
        Del = deleted(K1, T1), 
        DI = deleting([Del | DLs]), 
        DT = DT0, 
        DK = K
    ;
        DI0 = finished(_, _), 
        DT = two(DK0, DT0, K1, T1), 
        DK = bounding_region(K1, DK0), 
        DI = DI0
    ).

:- pred try_deletion3(K::in, rtree_2(K, V)::in, K::in, rtree_2(K, V)::in,
    K::in, rtree_2(K, V)::in, K::in, V::in, int::in, K::out,
    rtree_2(K, V)::out, delete_info(K, V)::out) is semidet <= region(K).

try_deletion3(K0, T0, K1, T1, K2, T2, K, V, D, DK, DT, DI) :-
    contains(K, K0), 
    delete_2(T0, K, V, D + 1, DK0, DT0, DI0), 
    (
        DI0 = deleting(DLs), 
        DI = finished(D + 1, DLs), 
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
    K::out, rtree_2(K, V)::out, delete_info(K, V)::out) is semidet <= region(K).

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

    % Given a list of deleted trees (with their bounding regions), 
    % (re)insert the trees back into the main tree at the specified depth.
    %
:- pred reinsert_deleted_subtrees(deleteds(K, V)::in, int::in,
    rtree_2(K, V)::in, rtree_2(K, V)::out) is det <= region(K).

reinsert_deleted_subtrees([], _, T, T).
reinsert_deleted_subtrees([deleted(K, T) | DLs], D, T0, T2) :-
    T1 = insert_tree(T0, K, T, 1, D), 
    ( T0 = four(_, _, _, _, _, _, _, _) ->
        reinsert_deleted_subtrees(DLs, D + 2, T1, T2)
    ;
        reinsert_deleted_subtrees(DLs, D + 1, T1, T2)
    ).
    
    % The code here is almost identical to 'insert', however we are 
    % inserting a tree at depth D0 as opposed to data to a leaf.
    %
:- func insert_tree(rtree_2(K, V), K, rtree_2(K, V), int, int) =
    rtree_2(K, V) <= region(K).

insert_tree(leaf(_), _, _, _, _) = 
    func_error("insert_tree: leaf unexpected").
insert_tree(two(K0, T0, K1, T1), K, S, D0, D) = T :-
    ( D0 = D ->
        T = three(K0, T0, K1, T1, K, S)
    ;
        Result = include2(K, K0, K1), 
        ( Result = min3_first ->
            insert_tree_and_split_child2(K0, T0, K1, T1, K, S, D0 + 1, D, T)
        ;
            insert_tree_and_split_child2(K1, T1, K0, T0, K, S, D0 + 1, D, T)
        )
    ).
insert_tree(three(K0, T0, K1, T1, K2, T2), K, S, D0, D) = T :-
    ( D0 = D ->
        T = four(K0, T0, K1, T1, K2, T2, K, S)
    ;
        Result = include3(K, K0, K1, K2), 
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
        Result = include2(K, K2, K3), 
        ( Result = min3_first ->
            K4 = bounding_region(K, K2), 
            T4 = insert_tree(T2, K, S, D0, D), 
            T  = three(K1, T1, K3, T3, K4, T4)
        ;
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
        Result = include2(K, K2, K3), 
        ( Result = min3_first ->
            K5 = bounding_region(K, K3), 
            T5 = insert_tree(T3, K, S, D0, D), 
            T  = four(K1, T1, K2, T2, K4, T4, K5, T5)
        ;
            K5 = bounding_region(K, K4), 
            T5 = insert_tree(T4, K, S, D0, D), 
            T  = four(K1, T1, K2, T2, K3, T3, K5, T5)
        )
    ).

%-----------------------------------------------------------------------------%
%
% search_intersects
%

rtree.search_intersects(empty, _) = [].
rtree.search_intersects(one(K, V), QueryKey) = 
    (if intersects(QueryKey, K) then [V] else []).
rtree.search_intersects(rtree(RTree), QueryKey) = Values :-
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
    ( intersects(QueryKey, K) ->
        search_intersects_2(T, QueryKey, !Values)
    ;
        true
    ).

%---------------------------------------------------------------------------%
%
% search_contains
%

rtree.search_contains(empty, _) = [].
rtree.search_contains(one(K0, V0), K) = 
    (if contains(K, K0) then [V0] else []).
rtree.search_contains(rtree(T), K) = Vs :-
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
    ( contains(QueryKey, K) ->
        search_contains_2(T, QueryKey, !Values)
    ;
        true
    ).

%---------------------------------------------------------------------------%

rtree.search_general(_KeyTest, _ValueTest, empty) = [].
rtree.search_general(KeyTest, ValueTest, one(K, V)) = 
    (if KeyTest(K), ValueTest(V) then [V] else []).
rtree.search_general(KeyTest, ValueTest, rtree(T)) = Values :-
    search_general_2(T, KeyTest, ValueTest, [], Values).
    
    % Algorithm: descend into subtrees with bounding regions that satisfy
    % the key test and accumulate leaf values that satisfy the value test.
    %
:- pred search_general_2(rtree_2(K, V)::in,
    pred(K)::in(pred(in) is semidet), pred(V)::in(pred(in) is semidet),
    list(V)::in, list(V)::out) is det.

search_general_2(leaf(Value), _, ValueTest, !Values) :-
    ( ValueTest(Value) ->
        !:Values = [ Value | !.Values ]
    ;
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
    ( KeyTest(K) ->
        search_general_2(T, KeyTest, ValueTest, !Values)
    ;
        true
    ).

%---------------------------------------------------------------------------%
%
% search_first
%

rtree.search_first(P, C, one(K0, V0), L, V0, E0) :-
    maybe_limit(K0, P, L, _), 
    maybe_limit(V0, C, L, E0).
rtree.search_first(P, C, rtree(T), L, V, E) :-
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
    % the order induced by KTest.  If we find a solution, we try and find
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
    ( maybe_limit(K0, P, L, E0) ->
        ( maybe_limit(K1, P, L, E1) ->
            search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
        ;
            search_first_2(T0, P, C, L, V, E)
        )
    ;
        maybe_limit(K1, P, L, _), 
        search_first_2(T1, P, C, L, V, E)
    ).
search_first_2(three(K0, T0, K1, T1, K2, T2), P, C, L, V, E) :-
    ( maybe_limit(K0, P, L, E0) ->
        ( maybe_limit(K1, P, L, E1) ->
            ( maybe_limit(K2, P, L, E2) ->
                search_first_2_three_choices(E0, E1, E2, T0, T1, T2, P, C, 
                    L, V, E)
            ;
                search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
            )
        ; maybe_limit(K2, P, L, E2) ->
            search_first_2_two_choices(E0, E2, T0, T2, P, C, L, V, E)
        ;
            search_first_2(T0, P, C, L, V, E)
        )
    ; maybe_limit(K1, P, L, E1) ->
        ( maybe_limit(K2, P, L, E2) ->
            search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
        ;
            search_first_2(T1, P, C, L, V, E)
        )
    ;
        maybe_limit(K2, P, L, _), 
        search_first_2(T2, P, C, L, V, E)
    ).
search_first_2(four(K0, T0, K1, T1, K2, T2, K3, T3), P, C, L, V, E) :-
    ( maybe_limit(K0, P, L, E0) ->
        ( maybe_limit(K1, P, L, E1) ->
            ( maybe_limit(K2, P, L, E2) ->
                ( maybe_limit(K3, P, L, E3) ->
                    search_first_2_four_choices(E0, E1, E2, E3, T0, T1, T2,
                        T3, P, C, L, V, E)
                ;
                    search_first_2_three_choices(E0, E1, E2, T0, T1, T2, P, 
                        C, L, V, E)
                )
            ;   ( maybe_limit(K3, P, L, E3) ->
                    search_first_2_three_choices(E0, E1, E3, T0, T1, T3, P, 
                        C, L, V, E)
                ;
                    search_first_2_two_choices(E0, E1, T0, T1, P, C, L, V, E)
                )
            )
        ;   ( maybe_limit(K2, P, L, E2) ->
                ( maybe_limit(K3, P, L, E3) ->
                    search_first_2_three_choices(E0, E2, E3, T0, T2, T3, P, 
                        C, L, V, E)
                ;
                    search_first_2_two_choices(E0, E2, T0, T2, P, C, L, V, E)
                )
            ;   ( maybe_limit(K3, P, L, E3) ->
                    search_first_2_two_choices(E0, E3, T0, T3, P, C, L, V, E)
                ;
                    search_first_2(T0, P, C, L, V, E)
                )
            )
        )
    ; maybe_limit(K1, P, L, E1) ->
        ( maybe_limit(K2, P, L, E2) ->
            ( maybe_limit(K3, P, L, E3) ->
                search_first_2_three_choices(E1, E2, E3, T1, T2, T3, P, C, L, 
                    V, E)
            ;
                search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
            )
        ;   ( maybe_limit(K3, P, L, E3) ->
                search_first_2_two_choices(E1, E3, T1, T3, P, C, L, V, E)
            ;
                search_first_2(T1, P, C, L, V, E)
            )
        )
    ; maybe_limit(K2, P, L, E2) ->
        ( maybe_limit(K3, P, L, E3) ->
            search_first_2_two_choices(E2, E3, T2, T3, P, C, L, V, E)
        ;
            search_first_2(T2, P, C, L, V, E)
        )
    ;
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
    ( compare((<), E0, E1) ->
        search_first_2_try_first_from_two(E1, T0, T1, P, C, L, V, E)
    ;
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
    % a better solution.  Otherwise we search the remaining 3 choices.
    % Arguments are ordered in terms of "goodness".
    %
:- pred search_first_2_try_first_from_four(E, E, E, rtree_2(K, V),
    rtree_2(K, V), 
    rtree_2(K, V), rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_four(in, in, in, in, in, in, in, 
    pred(in, out) is semidet, pred(in, out) is semidet, in, out, out) 
    is semidet.

search_first_2_try_first_from_four(E1, E2, E3, T0, T1, T2, T3, P, C, L, V, E) :-
    ( search_first_2(T0, P, C, L, V0, E0) ->
        search_first_2_find_better_solution_three(V0, E0, E1, E2, E3, T1, T2, 
            T3, P, C, V, E)
    ;
        search_first_2_three_choices(E1, E2, E3, T1, T2, T3, P, C, L, V, E)
    ).

:- pred search_first_2_try_first_from_three(E, E, rtree_2(K, V), rtree_2(K, V), 
    rtree_2(K, V), pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_three(in, in, in, in, in, 
    pred(in, out) is semidet, pred(in, out) is semidet, in, out, out) 
    is semidet.

search_first_2_try_first_from_three(E1, E2, T0, T1, T2, P, C, L, V, E) :-
    ( search_first_2(T0, P, C, L, V0, E0) ->
        search_first_2_find_better_solution_two(V0, E0, E1, E2, T1, T2, P, 
            C, V, E)
    ;
        search_first_2_two_choices(E1, E2, T1, T2, P, C, L, V, E)
    ).

:- pred search_first_2_try_first_from_two(E, rtree_2(K, V), rtree_2(K, V), 
    pred(K, E), pred(V, E), E, V, E).
:- mode search_first_2_try_first_from_two(in, in, in, pred(in, out) is semidet,
    pred(in, out) is semidet, in, out, out) is semidet.

search_first_2_try_first_from_two(E1, T0, T1, P, C, L, V, E) :-
    ( search_first_2(T0, P, C, L, V0, E0) ->
        search_first_2_find_better_solution_one(V0, E0, E1, T1, P, C, V, E)
    ;
        search_first_2(T1, P, C, L, V, E)
    ).

    % We have found a solution, however it may not be the best solution, 
    % so we search the other possibilities.  The first solution becomes the
    % new maximum, so it is likely the new searches are cheaper.
    %
:- pred search_first_2_find_better_solution_one(V, E, E, rtree_2(K, V), 
    pred(K, E), pred(V, E), V, E).
:- mode search_first_2_find_better_solution_one(in, in, in, in, 
    pred(in, out) is semidet, pred(in, out) is semidet, out, out) is det.

search_first_2_find_better_solution_one(VM, EM, E0, T0, P, C, V, E) :-
    ( compare((<), EM, E0) ->
        V = VM, 
        E = EM
    ; search_first_2(T0, P, C, EM, V0, F0) ->
        ( compare((<), EM, F0) ->
            V = VM, 
            E = EM
        ;
            V = V0, 
            E = F0
        )
    ;
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
    ( search_first_2(T0, P, C, EM, V0, F0) ->
        ( compare((<), EM, F0) ->
            search_first_2_find_better_solution_one(VM, EM, E1, T1, P, C, V, E)
        ;
            search_first_2_find_better_solution_one(V0, F0, E1, T1, P, C, V, E)
        )
    ;
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
    ( search_first_2(T0, P, C, EM, V0, F0) ->
        ( compare((<), EM, F0) ->
            search_first_2_find_better_solution_two(VM, EM, E1, E2, T1, T2, P, 
                C, V, E)
        ;
            search_first_2_find_better_solution_two(V0, F0, E1, E2, T1, T2, P, 
                C, V, E)
        )
    ;
        search_first_2_find_better_solution_two(VM, EM, E1, E2, T1, T2, P, C, 
            V, E)
    ).

%---------------------------------------------------------------------------%
%
% search_general_fold
%

rtree.search_general_fold(_, _, empty, !Acc).
rtree.search_general_fold(KTest, VPred, one(K, V), !Acc) :-
    ( KTest(K) ->
        VPred(K, V, !Acc)
    ;
        true
    ).
rtree.search_general_fold(KTest, VPred, rtree(T), !Acc) :-
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
    ( KTest(K) ->
        ( T = leaf(V) ->
            VPred(K, V, !Acc)
        ;
            search_general_fold_2(T, KTest, VPred, !Acc)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% fold
%

rtree.fold(_P, empty, !Acc).
rtree.fold(P, one(K, V), !Acc) :-
    P(K, V, !Acc).
rtree.fold(P, rtree(T), !Acc) :-
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
    ( T = leaf(V) ->
        P(K, V, !Acc)
    ;
        fold_2(P, T, !Acc)
    ).

%----------------------------------------------------------------------------%
%
% map_values
%

rtree.map_values(_, empty, empty).
rtree.map_values(P, one(K, V), one(K, W)) :-
    P(K, V, W).
rtree.map_values(P, rtree(T), rtree(U)) :-
    map_values_2(P, T, U).

:- pred map_values_2(pred(K, V, W), rtree_2(K, V), rtree_2(K, W)).
:- mode map_values_2(pred(in, in, out) is det, in, out) is det.
:- mode map_values_2(pred(in, in, out) is semidet, in, out) is semidet.

map_values_2(_, leaf(_), _) :-
    error("map_values_2: leaf unexpected").
map_values_2(P, two(K0, T0, K1, T1), two(K0, U0, K1, U1)) :-
    map_values_2(P, K0, T0, U0),
    map_values_2(P, K1, T1, U1).
map_values_2(P, three(K0, T0, K1, T1, K2, T2), three(K0, U0, K1, U1, K2, U2)) :-
    map_values_2(P, K0, T0, U0),
    map_values_2(P, K1, T1, U1),
    map_values_2(P, K2, T2, U2).
map_values_2(P, four(K0, T0, K1, T1, K2, T2, K3, T3), 
        four(K0, U0, K1, U1, K2, U2, K3, U3)) :-
    map_values_2(P, K0, T0, U0),
    map_values_2(P, K1, T1, U1),
    map_values_2(P, K2, T2, U2),
    map_values_2(P, K3, T3, U3).

:- pred map_values_2(pred(K, V, W), K, rtree_2(K, V), rtree_2(K, W)).
:- mode map_values_2(pred(in, in, out) is det, in, in, out) is det.
:- mode map_values_2(pred(in, in, out) is semidet, in, in, out) is semidet.

map_values_2(P, K, T, U) :-
    ( T = leaf(V) ->
        P(K, V, W),
        U = leaf(W)
    ;
        map_values_2(P, T, U)
    ).

%---------------------------------------------------------------------------%

    % Find the minimum of three values.
    %
:- func minimum_of_three(E, E, E) = min_of_three_result.

minimum_of_three(A, B, C) =
    ( compare((<), A, B) ->
        ( compare((<), A, C) ->
            min3_first
        ;
            min3_third
        )
    ;
        ( compare((<), B, C) ->
            min3_second
        ;
            min3_third
        )
    ).

%---------------------------------------------------------------------------%

    % Find the minimum of four values.
    %
:- func minimum_of_four(E, E, E, E) = min_of_four_result.

minimum_of_four(E0, E1, E2, E3) = M :-
    compare(R0, E0, E1), 
    ( R0 = (<) ->
        M0 = min4_first, 
        F0 = E0
    ;
        M0 = min4_second, 
        F0 = E1
    ), 
    compare(R1, F0, E2), 
    ( R1 = (<) ->
        M1 = M0, 
        F1 = F0
    ;
        M1 = min4_third, 
        F1 = E2
    ), 
    compare(R2, F1, E3), 
    ( R2 = (<) ->
        M = M1
    ;
        M = min4_fourth
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Predefined regions
%

%-----------------------------------------------------------------------------%

:- instance region(box3d) where [
    pred(intersects/2) is box3d_intersects, 
    pred(contains/2) is box3d_contains, 
    func(size/1) is box3d_volume, 
    func(bounding_region/2) is box3d_bounding_region, 
    func(bounding_region_size/2) is box3d_bounding_region_volume
].

%-----------------------------------------------------------------------------%

:- pred box3d_intersects(box3d::in, box3d::in) is semidet.

box3d_intersects(Bx0, Bx1) :-
    Bx0 = box3d(X0, X1, Y0, Y1, Z0, Z1), 
    Bx1 = box3d(A0, A1, B0, B1, C0, C1), 
    ( X0 =< A0 ->
        X1 >= A0
    ;
        X0 =< A1
    ), 
    ( Y0 =< B0 ->
        Y1 >= B0
    ;
        Y0 =< B1
    ), 
    ( Z0 =< C0 ->
        Z1 >= C0
    ;
        Z0 =< C1
    ).

%---------------------------------------------------------------------------%

:- pred box3d_contains(box3d::in, box3d::in) is semidet.

box3d_contains(Bx0, Bx1) :-
    Bx0 = box3d(X0, X1, Y0, Y1, Z0, Z1), 
    Bx1 = box3d(A0, A1, B0, B1, C0, C1), 
    X0 >= A0, 
    X1 =< A1, 
    Y0 >= B0, 
    Y1 =< B1, 
    Z0 >= C0, 
    Z1 =< C1.

%-----------------------------------------------------------------------------%

:- func box3d_volume(box3d) = float.

box3d_volume(Bx) = A :-
    Bx = box3d(X0, X1, Y0, Y1, Z0, Z1), 
    A = (X1 - X0) * (Y1 - Y0) * (Z1 - Z0).

%-----------------------------------------------------------------------------%

:- func box3d_bounding_region(box3d, box3d) = box3d.

box3d_bounding_region(Bx0, Bx1) = Bx2 :-
    Bx0 = box3d(X0, X1, Y0, Y1, Z0, Z1), 
    Bx1 = box3d(A0, A1, B0, B1, C0, C1), 
    M0 = min(X0, A0), 
    M1 = max(X1, A1), 
    N0 = min(Y0, B0), 
    N1 = max(Y1, B1), 
    O0 = min(Z0, C0), 
    O1 = max(Z1, C1), 
    Bx2 = box3d(M0, M1, N0, N1, O0, O1).

%-----------------------------------------------------------------------------%

:- func box3d_bounding_region_volume(box3d, box3d) = float.

box3d_bounding_region_volume(Bx0, Bx1) = CA :-
    Bx0 = box3d(X0, X1, Y0, Y1, Z0, Z1), 
    Bx1 = box3d(A0, A1, B0, B1, C0, C1), 
    M0 = min(X0, A0), 
    M1 = max(X1, A1), 
    N0 = min(Y0, B0), 
    N1 = max(Y1, B1), 
    O0 = min(Z0, C0), 
    O1 = max(Z1, C1), 
    CA = (M1 - M0) * (N1 - N0) * (O1 - O0).
    
%-----------------------------------------------------------------------------%

:- instance region(box) where [
    pred(intersects/2) is box_intersects, 
    pred(contains/2) is box_contains, 
    func(size/1) is box_area, 
    func(bounding_region/2) is box_bounding_region, 
    func(bounding_region_size/2) is box_bounding_region_area
].

%-----------------------------------------------------------------------------%

:- pred box_intersects(box::in, box::in) is semidet.

box_intersects(Bx0, Bx1) :-
    Bx0 = box(X0, X1, Y0, Y1), 
    Bx1 = box(A0, A1, B0, B1), 
    ( X0 =< A0 ->
        X1 >= A0
    ;   X0 =< A1
    ), 
    ( Y0 =< B0 ->
        Y1 >= B0
    ;   Y0 =< B1
    ).

%---------------------------------------------------------------------------%

:- pred box_contains(box::in, box::in) is semidet.

box_contains(Bx0, Bx1) :-
    Bx0 = box(X0, X1, Y0, Y1), 
    Bx1 = box(A0, A1, B0, B1), 
    X0 >= A0, 
    X1 =< A1, 
    Y0 >= B0, 
    Y1 =< B1.

%---------------------------------------------------------------------------%

:- func box_area(box) = float.

box_area(Bx) = A :-
    Bx = box(X0, X1, Y0, Y1), 
    A = (X1 - X0) * (Y1 - Y0).

%---------------------------------------------------------------------------%

:- func box_bounding_region(box, box) = box.

box_bounding_region(Bx0, Bx1) = Bx2 :-
    Bx0 = box(X0, X1, Y0, Y1), 
    Bx1 = box(A0, A1, B0, B1), 
    M0 = min(X0, A0), 
    M1 = max(X1, A1), 
    N0 = min(Y0, B0), 
    N1 = max(Y1, B1), 
    Bx2 = box(M0, M1, N0, N1).

%---------------------------------------------------------------------------%

:- func box_bounding_region_area(box, box) = float.

box_bounding_region_area(Bx0, Bx1) = CA :-
    Bx0 = box(X0, X1, Y0, Y1), 
    Bx1 = box(A0, A1, B0, B1), 
    M0 = min(X0, A0), 
    M1 = max(X1, A1), 
    N0 = min(Y0, B0), 
    N1 = max(Y1, B1), 
    CA = (M1 - M0) * (N1 - N0).
    
%-----------------------------------------------------------------------------%

:- instance region(interval) where [
    pred(intersects/2) is interval_intersects, 
    pred(contains/2) is interval_contains, 
    func(size/1) is interval_length, 
    func(bounding_region/2) is interval_bounding_region, 
    func(bounding_region_size/2) is interval_bounding_region_length
].

%-----------------------------------------------------------------------------%

:- pred interval_intersects(interval::in, interval::in) is semidet.

interval_intersects(Bx0, Bx1) :-
    Bx0 = interval(X0, X1), 
    Bx1 = interval(A0, A1), 
    ( X0 =< A0 ->
        X1 >= A0
    ;
        X0 =< A1
    ).

%---------------------------------------------------------------------------%

:- pred interval_contains(interval::in, interval::in) is semidet.

interval_contains(Bx0, Bx1) :-
    Bx0 = interval(X0, X1), 
    Bx1 = interval(A0, A1), 
    X0 >= A0, 
    X1 =< A1.

%-----------------------------------------------------------------------------%

:- func interval_length(interval) = float.

interval_length(Bx) = A :-
    Bx = interval(X0, X1), 
    A = X1-X0.

%-----------------------------------------------------------------------------%

:- func interval_bounding_region(interval, interval) = interval.

interval_bounding_region(Bx0, Bx1) = Bx2 :-
    Bx0 = interval(X0, X1), 
    Bx1 = interval(A0, A1), 
    Bx2 = interval(min(X0, A0), max(X1, A1)).

%-----------------------------------------------------------------------------%

:- func interval_bounding_region_length(interval, interval) = float.

interval_bounding_region_length(Bx0, Bx1) = CA :-
    Bx0 = interval(X0, X1), 
    Bx1 = interval(A0, A1), 
    CA = max(X1, A1) - min(X0, A0).

%-----------------------------------------------------------------------------%
:- end_module rtree.
%-----------------------------------------------------------------------------%
