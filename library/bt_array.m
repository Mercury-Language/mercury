%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997,1999-2000,2002-2003,2005-2006 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: bt_array.m
% Main author: bromage.
% Stability: medium-low
%
% This file contains a set of predicates for generating an manipulating a
% bt_array data structure. This implementation allows O(log n) access and
% update time, and does not require the bt_array to be unique. If you need
% O(1) access/update time, use the array datatype instead. (`bt_array' is
% supposed to stand for either "binary tree array" or "backtrackable array".)
%
% Implementation obscurity: This implementation is biased towards larger
% indices. The access/update time for a bt_array of size N with index I is
% actually O(log(N-I)). The reason for this is so that the resize operations
% can be optimised for a (possibly very) common case, and to exploit
% accumulator recursion in some operations. See the documentation of resize
% and shrink for more details.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bt_array.
:- interface.
:- import_module list.

:- type bt_array(T).

%---------------------------------------------------------------------------%
%
% Creating arrays.
%

    % init(Low, High, Init, Array) is true iff Array is a
    % bt_array with bounds from Low to High whose elements each equal Init.
    %
:- func init(int, int, T) = bt_array(T).
:- pred init(int::in, int::in, T::in, bt_array(T)::out) is det.

    % make_empty_array(Low, Array) is true iff Array is a
    % bt_array of size zero starting at index Low.
    %
:- func make_empty_array(int) = bt_array(T).
:- pred make_empty_array(int::in, bt_array(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Reading array elements.
%

    % lookup returns the Nth element of a bt_array.
    % It is an error if the index is out of bounds.
    %
:- func lookup(bt_array(T), int) = T.
:- pred lookup(bt_array(T)::in, int::in, T::out) is det.

    % semidet_lookup is like lookup except that it fails if the index is out of
    % bounds.
    %
:- pred semidet_lookup(bt_array(T)::in, int::in, T::out) is semidet.

    % Field selection for arrays.
    % Array ^ elem(Index) = lookup(Array, Index).
    %
:- func elem(int, bt_array(T)) = T.

%---------------------------------------------------------------------------%
%
% Writing array elements.
%

    % set sets the nth element of a bt_array, and returns the resulting
    % bt_array. It is an error if the index is out of bounds.
    %
:- func set(bt_array(T), int, T) = bt_array(T).
:- pred set(bt_array(T)::in, int::in, T::in, bt_array(T)::out) is det.

    % set sets the nth element of a bt_array, and returns the
    % resulting bt_array (good opportunity for destructive update ;-).
    % It fails if the index is out of bounds.
    %
:- pred semidet_set(bt_array(T)::in, int::in, T::in, bt_array(T)::out)
    is semidet.

    % Field update for arrays.
    % (Array ^ elem(Index) := Value) = set(Array, Index, Value).
    %
:- func 'elem :='(int, bt_array(T), T) = bt_array(T).

%---------------------------------------------------------------------------%

    % Returns the lower bound of the array.
    %
:- func min(bt_array(_T)) = int.
:- pred min(bt_array(_T)::in, int::out) is det.

    % Returns the upper bound of the array.
    % Returns lower bound - 1 for an empty array.
    %
:- func max(bt_array(_T)) = int.
:- pred max(bt_array(_T)::in, int::out) is det.

    % Returns the length of the array,
    % i.e. upper bound - lower bound + 1.
    %
:- func size(bt_array(_T)) = int.
:- pred size(bt_array(_T)::in, int::out) is det.

    % bounds(Array, Min, Max) returns the lower and upper bounds of a bt_array.
    % The upper bound will be the lower bound - 1 for an empty array.
    %
:- pred bounds(bt_array(_T)::in, int::out, int::out) is det.

    % in_bounds checks whether an index is in the bounds
    % of a bt_array.
    %
:- pred in_bounds(bt_array(_T)::in, int::in) is semidet.

%---------------------------------------------------------------------------%
%
% Resizing arrays.
%

    % `resize(BtArray0, Lo, Hi, Item, BtArray)' is true if BtArray
    % is a bt_array created by expanding or shrinking BtArray0 to fit the
    % bounds (Lo, Hi). If the new bounds are not wholly contained within
    % the bounds of BtArray0, Item is used to fill out the other places.
    %
    % Note: This operation is optimised for the case where the lower bound
    % of the new bt_array is the same as that of the old bt_array. In that
    % case, the operation takes time proportional to the absolute difference
    % in size between the two bt_arrays. If this is not the case, it may take
    % time proportional to the larger of the two bt_arrays.
    %
:- func resize(bt_array(T), int, int, T) = bt_array(T).
:- pred resize(bt_array(T)::in, int::in, int::in, T::in,
    bt_array(T)::out) is det.

    % shrink(BtArray0, Lo, Hi, Item, BtArray) is true if BtArray
    % is a bt_array created by shrinking BtArray0 to fit the bounds (Lo, Hi).
    % It is an error if the new bounds are not wholly within the bounds of
    % BtArray0.
    %
    % Note: This operation is optimised for the case where the lower bound
    % of the new bt_array is the same as that of the old bt_array. In that
    % case, the operation takes time proportional to the absolute difference
    % in size between the two bt_arrays. If this is not the case, it may take
    % time proportional to the larger of the two bt_arrays.
    %
:- func shrink(bt_array(T), int, int) = bt_array(T).
:- pred shrink(bt_array(T)::in, int::in, int::in, bt_array(T)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Conversions between bt_arrays and lists.
%

    % from_list(Low, List, BtArray) takes a list (of possibly zero
    % length), and returns a bt_array containing % those elements in the same
    % order that they occurred in the list. The lower bound of the new array
    % is Low.
    %
:- func from_list(int, list(T)) = bt_array(T).
:- pred from_list(int::in, list(T)::in, bt_array(T)::out) is det.

    % to_list takes a bt_array and returns a list containing
    % the elements of the bt_array in the same order that they occurred
    % in the bt_array.
    %
:- func to_list(bt_array(T)) = list(T).
:- pred to_list(bt_array(T)::in, list(T)::out) is det.

    % fetch_items takes a bt_array and a lower and upper index,
    % and places those items in the bt_array between these indices into a list.
    % It is an error if either index is out of bounds.
    %
:- func fetch_items(bt_array(T), int, int) = list(T).
:- pred fetch_items(bt_array(T)::in, int::in, int::in, list(T)::out)
    is det.

%---------------------------------------------------------------------------%

    % bsearch takes a bt_array, an element to be matched and a
    % comparison predicate and returns the position of the first occurrence
    % in the bt_array of an element which is equivalent to the given one
    % in the ordering provided. Assumes the bt_array is sorted according
    % to this ordering. Fails if the element is not present.
    %
:- pred bsearch(bt_array(T)::in, T::in,
    comparison_pred(T)::in(comparison_pred), int::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module ra_list.
:- import_module require.

:- type bt_array(T)
    --->    bt_array(int, int, ra_list(T)).
            % The lower bound, the upper bound, and the elements.
            %
            % The ra_list (short for random-access list) type is defined
            % at the bottom of this module.

:- pred actual_position(int::in, int::in, int::in, int::out) is det.
:- pragma inline(pred(actual_position/4)).

actual_position(Low, High, Index, Pos) :-
    Pos = High - Low - Index.

%---------------------------------------------------------------------------%

init(N1, N2, T) = BTA :-
    init(N1, N2, T, BTA).

init(Low, High, Item, bt_array(Low, High, List)) :-
    ra_list.init(List0),
    ElemsToAdd = High - Low + 1,
    add_elements(ElemsToAdd, Item, List0, List).

:- pred add_elements(int::in, T::in, ra_list(T)::in, ra_list(T)::out) is det.

add_elements(ElemsToAdd, Item, RaList0, RaList) :-
    ( if ElemsToAdd =< 0 then
        RaList0 = RaList
    else
        ra_list.cons(Item, RaList0, RaList1),
        ElemsToAdd1 = ElemsToAdd - 1,
        add_elements(ElemsToAdd1, Item, RaList1, RaList)
    ).

make_empty_array(N) = BTA :-
    make_empty_array(N, BTA).

make_empty_array(Low, bt_array(Low, High, List)) :-
    High = Low - 1,
    ra_list.init(List).

%---------------------------------------------------------------------------%

lookup(BTA, N) = T :-
    lookup(BTA, N, T).

lookup(bt_array(Low, High, RaList), Index, Item) :-
    actual_position(Low, High, Index, Pos),
    ( if ra_list.index0(RaList, Pos, Item0) then
        Item = Item0
    else
        unexpected($pred, "array subscript out of bounds")
    ).

semidet_lookup(bt_array(Low, High, RaList), Index, Item) :-
    actual_position(Low, High, Index, Pos),
    ra_list.index0(RaList, Pos, Item).

elem(Index, Array) = lookup(Array, Index).

%---------------------------------------------------------------------------%

set(BT1A, N, T) = BTA2 :-
    set(BT1A, N, T, BTA2).

set(BtArray0, Index, Item, BtArray) :-
    ( if semidet_set(BtArray0, Index, Item, BtArray1) then
        BtArray = BtArray1
    else
        unexpected($pred, "index out of bounds")
    ).

semidet_set(BtArray0, Index, Item, BtArray) :-
    BtArray0 = bt_array(Low, High, RaList0),
    actual_position(Low, High, Index, Pos),
    ra_list.update(Pos, Item, RaList0, RaList),
    BtArray = bt_array(Low, High, RaList).

'elem :='(Index, Array0, Value) = Array :-
    set(Array0, Index, Value, Array).

%---------------------------------------------------------------------------%

min(BTA) = N :-
    min(BTA, N).

min(bt_array(Low, _, _), Low).

max(BTA) = N :-
    max(BTA, N).

max(bt_array(_, High, _), High).

size(BTA) = N :-
    size(BTA, N).

size(bt_array(Low, High, _), Size) :-
    Size = High - Low + 1.

bounds(bt_array(Low, High, _), Low, High).

in_bounds(bt_array(Low, High, _), Index) :-
    Low =< Index, Index =< High.

%---------------------------------------------------------------------------%

resize(BT1A, N1, N2, T) = BTA2 :-
    resize(BT1A, N1, N2, T, BTA2).

resize(Array0, L, H, Item, Array) :-
    Array0 = bt_array(L0, H0, RaList0),
    ( if L = L0 then
        % Optimise the common case where the lower bounds are
        % the same.

        ( if H < H0 then
            SizeDiff = H0 - H,
            ( if ra_list.drop(SizeDiff, RaList0, RaList1) then
                RaList = RaList1
            else
                unexpected($pred, "can't resize to a less-than-empty array")
            ),
            Array = bt_array(L, H, RaList)
        else if H > H0 then
            SizeDiff = H - H0,
            add_elements(SizeDiff, Item, RaList0, RaList),
            Array = bt_array(L, H, RaList)
        else
            Array = Array0
        )
    else
        int.max(L, L0, L1),
        int.min(H, H0, H1),
        fetch_items(Array0, L1, H1, Items),
        init(L, H, Item, Array1),
        insert_items(Array1, L1, Items, Array)
    ).

shrink(BT1A, N1, N2) = BTA2 :-
    shrink(BT1A, N1, N2, BTA2).

shrink(Array0, L, H, Array) :-
    Array0 = bt_array(L0, H0, RaList0),
    ( if ( L < L0 ; H > H0 ) then
        unexpected($pred, "new bounds are larger than old ones")
    else if L = L0 then
        % Optimise the common case where the lower bounds are the same.

        SizeDiff = H0 - H,
        ( if ra_list.drop(SizeDiff, RaList0, RaList1) then
            RaList = RaList1
        else
            unexpected($pred, "can't resize to a less-than-empty array")
        ),
        Array = bt_array(L, H, RaList)
    else
        ( if ra_list.head(RaList0, Item0) then
            Item = Item0
        else
            unexpected($pred, "can't shrink an empty array")
        ),
        int.max(L, L0, L1),
        int.min(H, H0, H1),
        fetch_items(Array0, L1, H1, Items),
        init(L, H, Item, Array1),
        insert_items(Array1, L1, Items, Array)
    ).

%---------------------%

:- pred insert_items(bt_array(T)::in, int::in, list(T)::in, bt_array(T)::out)
    is det.

insert_items(Array, _N, [], Array).
insert_items(Array0, N, [Head|Tail], Array) :-
    set(Array0, N, Head, Array1),
    N1 = N + 1,
    insert_items(Array1, N1, Tail, Array).

%---------------------------------------------------------------------------%

from_list(N, Xs) = BTA :-
    from_list(N, Xs, BTA).

from_list(Low, List, bt_array(Low, High, RaList)) :-
    list.length(List, Len),
    High = Low + Len - 1,
    init(RaList0),
    reverse_into_ra_list(List, RaList0, RaList).

:- pred reverse_into_ra_list(list(T)::in,
    ra_list(T)::in, ra_list(T)::out) is det.

reverse_into_ra_list([], RaList, RaList).
reverse_into_ra_list([X | Xs], RaList0, RaList) :-
    cons(X, RaList0, RaList1),
    reverse_into_ra_list(Xs, RaList1, RaList).

%---------------------%

to_list(BTA) = Xs :-
    to_list(BTA, Xs).

to_list(bt_array(_, _, RaList), List) :-
    reverse_from_ra_list(RaList, [], List).

:- pred reverse_from_ra_list(ra_list(T)::in, list(T)::in, list(T)::out) is det.

reverse_from_ra_list(RaList0, Xs0, Xs) :-
    ( if head_tail(RaList0, X, RaList1) then
        reverse_from_ra_list(RaList1, [X | Xs0], Xs)
    else
        Xs0 = Xs
    ).

%---------------------%

fetch_items(BTA, N1, N2) = Xs :-
    fetch_items(BTA, N1, N2, Xs).

fetch_items(bt_array(ALow, AHigh, RaList0), Low, High, List) :-
    ( if
        Low > High
    then
        List = []
    else if
        actual_position(ALow, AHigh, High, Drop),
        ra_list.drop(Drop, RaList0, RaList),
        Take = High - Low + 1,
        reverse_from_ra_list_count(Take, RaList, [], List0)
    then
        List = List0
    else
        List = []
    ).

:- pred reverse_from_ra_list_count(int::in, ra_list(T)::in,
    list(T)::in, list(T)::out) is det.

reverse_from_ra_list_count(I, RaList0, Xs0, Xs) :-
    ( if
        ra_list.head_tail(RaList0, X, RaList1),
        I >= 0
    then
        I1 = I - 1,
        reverse_from_ra_list_count(I1, RaList1, [X | Xs0], Xs)
    else
        Xs0 = Xs
    ).

%---------------------------------------------------------------------------%

bsearch(A, SearchX, Compare, I) :-
    bounds(A, Lo, Hi),
    Lo =< Hi,
    bsearch_loop(A, Lo, Hi, SearchX, Compare, I).

    % XXX Would we gain anything by traversing the ra_list instead
    % of doing a vanilla binary chop?
    %
:- pred bsearch_loop(bt_array(T)::in, int::in, int::in, T::in,
    pred(T, T, comparison_result)::in(pred(in, in, out) is det), int::out)
    is semidet.

bsearch_loop(A, Lo, Hi, SearchX, Compare, I) :-
    Width = Hi - Lo,

    % If Width < 0, there is no range left.
    Width >= 0,

    % If Width == 0, we may just have found our element.
    % Do a Compare to check.
    ( if Width = 0 then
        lookup(A, Lo, LoX),
        Compare(SearchX, LoX, (=)),
        I = Lo
    else
        % We calculate Mid this way to avoid overflow, and because it works
        % even if Lo, and maybe Hi, is negative.
        Mid = Lo + ((Hi - Lo) `unchecked_right_shift` 1),
        % The right shift by one bit is a fast implementation
        % of division by 2.
        lookup(A, Mid, MidX),
        Compare(MidX, SearchX, Comp),
        (
            Comp = (<),
            bsearch_loop(A, Mid + 1, Hi, SearchX, Compare, I)
        ;
            Comp = (=),
            bsearch_loop(A, Lo, Mid, SearchX, Compare, I)
        ;
            Comp = (>),
            bsearch_loop(A, Lo, Mid - 1, SearchX, Compare, I)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module bt_array.
%---------------------------------------------------------------------------%
