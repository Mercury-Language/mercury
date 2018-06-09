%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997, 1999, 2002, 2005-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% 
% File: tr_array.m.
% Main authors: fjh.
% Stability: medium.
% 
% This module provides backtrackable destructive update operations on arrays.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module tr_array.
:- interface.

:- import_module array.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%
%
% Operations that perform backtrackable destructive update
%

    % tr_array.set sets the nth element of an array, and returns the
    % resulting array (good opportunity for destructive update ;-).  
    % It is an error if the index is out of bounds.
    %
:- pred tr_array.set(int::in, T::in,
    array(T)::array_mdi, array(T)::array_muo) is det.

    % tr_array.semidet_set sets the nth element of an array,
    % and returns the resulting array.
    % It fails if the index is out of bounds.
    %
:- pred tr_array.semidet_set(int::in, T::in,
    array(T)::array_mdi, array(T)::array_muo) is semidet.

%-----------------------------------------------------------------------------%

%
% "mui" (Mostly Unique Input) versions of standard array operations.
% It is necessary to use these ones rather than the standard array
% operations when dealing with mostly-unique values, to preserve
% "mostly_unique"-ness.
%

    % tr_array.min returns the lower bound of the array.
    % NOTE: in this implementation, the lower bound is always zero.
    %
:- pred tr_array.min(array(_T), int).
:- mode tr_array.min(array_mui, out) is det.
%:- mode tr_array.min(in, out) is det.

    % tr_array.max returns the upper bound of the array.
    %
:- pred tr_array.max(array(_T), int).
:- mode tr_array.max(array_mui, out) is det.
%:- mode tr_array.max(in, out) is det.

    % tr_array.size returns the length of the array,
    % i.e. upper bound - lower bound + 1.
    % 
:- pred tr_array.size(array(_T), int).
:- mode tr_array.size(array_mui, out) is det.
%:- mode tr_array.size(in, out) is det.

    % tr_array.bounds returns the upper and lower bounds of an array.
    % Note: in this implementation, the lower bound is always zero.
    % 
:- pred tr_array.bounds(array(_T), int, int).
:- mode tr_array.bounds(array_mui, out, out) is det.
%:- mode tr_array.bounds(in, out, out) is det.

    % tr_array.bounds checks whether an index is in the bounds
    % of an array.
    %
:- pred tr_array.in_bounds(array(_T), int).
:- mode tr_array.in_bounds(array_mui, in) is semidet.
%:- mode tr_array.in_bounds(in, in) is semidet.

%-----------------------------------------------------------------------------%

    % tr_array.lookup returns the Nth element of an array.
    % It is an error if the index is out of bounds.
    % 
:- pred tr_array.lookup(array(T), int, T).
:- mode tr_array.lookup(array_mui, in, out) is det.
%:- mode tr_array.lookup(in, in, out) is det.

    % tr_array.semidet_lookup returns the Nth element of an array.
    % It fails if the index is out of bounds.
    %
:- pred tr_array.semidet_lookup(array(T), int, T).
:- mode tr_array.semidet_lookup(array_mui, in, out) is semidet.
%:- mode tr_array.semidet_lookup(in, in, out) is semidet.

    % tr_array.slow_set sets the nth element of an array,
    % and returns the resulting array.  The initial array is not
    % required to be unique, so the implementation may not be able to use
    % destructive update.
    % It is an error if the index is out of bounds.
    % 
:- pred tr_array.slow_set(array(T), int, T, array(T)).
:- mode tr_array.slow_set(array_mui, in, in, array_uo) is det.
%:- mode tr_array.slow_set(in, in, in, array_uo) is det.

    % tr_array.semidet_slow_set sets the nth element of an array,
    % and returns the resulting array.  The initial array is not
    % required to be unique, so the implementation may not be able to use
    % destructive update.
    % It fails if the index is out of bounds.
    % 
:- pred tr_array.semidet_slow_set(array(T), int, T, array(T)).
:- mode tr_array.semidet_slow_set(array_mui, in, in, array_uo) is semidet.
%:- mode tr_array.semidet_slow_set(in, in, in, array_uo) is semidet.

    % tr_array.copy(Array0, Array):
    % Makes a new unique copy of an array.
    % 
:- pred tr_array.copy(array(T), array(T)).
:- mode tr_array.copy(array_mui, array_uo) is det.
%:- mode tr_array.copy(in, array_uo) is det.

    % tr_array.resize(Array0, Size, Init, Array):
    % The array is expanded or shrunk to make it fit
    % the new size `Size'.  Any new entries are filled
    % with `Init'.
    % 
:- pred tr_array.resize(array(T), int, T, array(T)).
:- mode tr_array.resize(array_mui, in, in, array_uo) is det.
%:- mode tr_array.resize(in, in, in, array_uo) is det.

    % tr_array.shrink(Array0, Size, Array):
    % The array is shrunk to make it fit the new size `Size'.
    % It is an error if `Size' is larger than the size of `Array0'.
    %
:- pred tr_array.shrink(array(T), int, array(T)).
:- mode tr_array.shrink(array_mui, in, array_uo) is det.
%:- mode tr_array.shrink(in, in, array_uo) is det.

    % tr_array.to_list takes an array and returns a list containing
    % the elements of the array in the same order that they
    % occurred in the array.
    %
:- pred tr_array.to_list(array(T), list(T)).
:- mode tr_array.to_list(array_mui, out) is det.
%:- mode tr_array.to_list(in, out) is det.

    % tr_array.fetch_items takes an array and a lower and upper
    % index, and places those items in the array between these
    % indices into a list.  It is an error if either index is
    % out of bounds.
    % 
:- pred tr_array.fetch_items(array(T), int, int, list(T)).
:- mode tr_array.fetch_items(array_mui, in, in, out) is det.
%:- mode tr_array.fetch_items(in, in, in, out) is det.

    % tr_array.bsearch takes an array, an element to be found
    % and a comparison predicate and returns the position of
    % the element in the array.  Assumes the array is in sorted
    % order.  Fails if the element is not present.  If the
    % element to be found appears multiple times, the index of
    % the first occurrence is returned.
    %
:- pred tr_array.bsearch(array(T), T, pred(T, T, comparison_result),
    maybe(int)).
:- mode tr_array.bsearch(array_mui, in, pred(in, in, out) is det, out)
    is det.
%:- mode tr_array.bsearch(in, in, pred(in, in, out) is det, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma require_feature_set([trailing]).

/****
lower bounds other than zero are not supported
    % tr_array.resize takes an array and new lower and upper bounds.
    % the array is expanded or shrunk at each end to make it fit
    % the new bounds.
:- pred tr_array.resize(array(T), int, int, array(T)).
:- mode tr_array.resize(in, in, in, out) is det.
****/

:- pragma foreign_import_module("C", array).

%-----------------------------------------------------------------------------%

% Arrays are implemented in array.m using the C interface.

% The C type which defines the representation of arrays is
% MR_ArrayType; it is defined in runtime/mercury_library_types.h.

:- pragma foreign_decl("C", "#include ""mercury_library_types.h""").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    tr_array.set(Index::in, Item::in, Array0::array_mdi, Array::array_muo),
    [promise_pure, will_not_call_mercury],
"
    MR_ArrayType *array = (MR_ArrayType *) Array0;
    if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
        MR_fatal_error(""tr_array.set: array index out of bounds"");
    }
    MR_trail_current_value(&array->elements[Index]);
    array->elements[Index] = Item;  /* destructive update! */
    Array = Array0;
").

tr_array.semidet_set(Index, Item, !Array) :-
    tr_array.in_bounds(!.Array, Index),
    tr_array.set(Index, Item, !Array).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    tr_array.min(Array::array_mui, Min::out),
    [promise_pure, will_not_call_mercury, will_not_modify_trail],
"
    /* Array not used */
    Min = 0;
").

% :- pragma foreign_proc("C",
%   tr_array.min(Array::in, Min::out),
%   [promise_pure, will_not_call_mercury],
% "
%   /* Array not used */
%   Min = 0;
% ").

:- pragma foreign_proc("C",
    tr_array.max(Array::array_mui, Max::out),
    [promise_pure, will_not_call_mercury, will_not_modify_trail],
"
    Max = ((MR_ArrayType *)Array)->size - 1;
").

% :- pragma foreign_proc("C",
%   tr_array.max(Array::in, Max::out),
%   [promise_pure, will_not_call_mercury],
% "
%   Max = ((MR_ArrayType *)Array)->size - 1;
% ").

tr_array.bounds(Array, Min, Max) :-
    tr_array.min(Array, Min),
    tr_array.max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    tr_array.size(Array::array_mui, Max::out),
    [promise_pure, will_not_call_mercury, will_not_modify_trail],
"
    Max = ((MR_ArrayType *)Array)->size;
").
% :- pragma foreign_proc("C",
%   tr_array.size(Array::in, Max::out),
%   [promise_pure, will_not_call_mercury],
% "
%   Max = ((MR_ArrayType *)Array)->size;
% ").

%-----------------------------------------------------------------------------%

tr_array.in_bounds(Array, Index) :-
    tr_array.bounds(Array, Min, Max),
    Min =< Index, Index =< Max.

tr_array.semidet_lookup(Array, Index, Item) :-
    tr_array.in_bounds(Array, Index),
    tr_array.lookup(Array, Index, Item).

tr_array.semidet_slow_set(Array0, Index, Item, Array) :-
    tr_array.in_bounds(Array0, Index),
    tr_array.slow_set(Array0, Index, Item, Array).

tr_array.slow_set(Array0, Index, Item, Array) :-
    tr_array.copy(Array0, Array1),
    array.set(Index, Item, Array1, Array).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    tr_array.lookup(Array::array_mui, Index::in, Item::out),
    [promise_pure, will_not_call_mercury, will_not_modify_trail],
"
    MR_ArrayType *array = (MR_ArrayType *) Array;
    if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
        MR_fatal_error(""tr_array.lookup: array index out of bounds"");
    }
    Item = array->elements[Index];
").

% :- pragma foreign_proc("C",
%   tr_array.lookup(Array::in, Index::in, Item::out),
%   [promise_pure, will_not_call_mercury],
% "
%   MR_ArrayType *array = (MR_ArrayType *) Array;
%   if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
%       MR_fatal_error(""tr_array.lookup: array index out of bounds"");
%   }
%   Item = array->elements[Index];
% ").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

extern void
ML_tr_resize_array(MR_ArrayType *array, const MR_ArrayType *old_array,
    MR_Integer array_size, MR_Word item);
").

:- pragma foreign_code("C", "
void
ML_tr_resize_array(MR_ArrayType *array, const MR_ArrayType *old_array,
    MR_Integer array_size, MR_Word item)
{
    MR_Integer i;
    MR_Integer elements_to_copy;

    elements_to_copy = old_array->size;
    if (elements_to_copy > array_size) {
        elements_to_copy = array_size;
    }

    array->size = array_size;
    for (i = 0; i < elements_to_copy; i++) {
        array->elements[i] = old_array->elements[i];
    }
    for (; i < array_size; i++) {
        array->elements[i] = item;
    }

    /*
    ** Since the mode on the old array is `array_mui', it is NOT safe to
    ** deallocate the storage for it.
    */
}
").

:- pragma foreign_proc("C",
    tr_array.resize(Array0::array_mui, Size::in, Item::in,
        Array::array_uo),
    [promise_pure, will_not_call_mercury],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
    ML_tr_resize_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0,
        Size, Item);
").

% :- pragma foreign_proc("C",
%   tr_array.resize(Array0::in, Size::in, Item::in, Array::array_uo),
%   [promise_pure, will_not_call_mercury],
% "
%   MR_incr_hp_msg(Array, Size + 1, MR_ALLOC_ID, ""array.array/1"");
%   ML_tr_resize_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0,
%       Size, Item);
% ").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_tr_shrink_array(MR_ArrayType *, const MR_ArrayType *old_array,
    MR_Integer array_size);
").

:- pragma foreign_code("C", "
void
ML_tr_shrink_array(MR_ArrayType *array, const MR_ArrayType *old_array,
    MR_Integer array_size)
{
    MR_Integer i;
    MR_Integer old_array_size;

    old_array_size = old_array->size;
    if (old_array_size < array_size) {
        MR_fatal_error(
            ""tr_array.shrink: can't shrink to a larger size"");
    }

    array->size = array_size;
    for (i = 0; i < array_size; i++) {
        array->elements[i] = old_array->elements[i];
    }

    /*
    ** Since the mode on the old array is `array_mui', it is NOT safe to
    ** deallocate the storage for it.
    */
}
").

:- pragma foreign_proc("C",
    tr_array.shrink(Array0::array_mui, Size::in, Array::array_uo),
    [promise_pure, will_not_call_mercury],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
    ML_tr_shrink_array((MR_ArrayType *) Array,
        (const MR_ArrayType *) Array0, Size);
").

% :- pragma foreign_proc("C",
%   tr_array.shrink(Array0::in, Size::in, Array::array_uo),
%   [promise_pure, will_not_call_mercury],
% "
%   MR_incr_hp_msg(Array, Size + 1, MR_ALLOC_ID, ""array.array/1"");
%   ML_tr_shrink_array((MR_ArrayType *) Array,
%       (const MR_ArrayType *) Array0, Size);
% ").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

extern void
ML_tr_copy_array(MR_ArrayType *array, const MR_ArrayType *old_array);

").

:- pragma foreign_code("C", "
void
ML_tr_copy_array(MR_ArrayType *array, const MR_ArrayType *old_array)
{
    /*
    ** Any changes to this function will probably also require
    ** changes to deepcopy() in runtime/deep_copy.c.
    */

    MR_Integer i;
    MR_Integer array_size;

    array_size = old_array->size;
    array->size = array_size;
    for (i = 0; i < array_size; i++) {
        array->elements[i] = old_array->elements[i];
    }
}
").

:- pragma foreign_proc("C",
    tr_array.copy(Array0::array_mui, Array::array_uo),
    [promise_pure, will_not_call_mercury],
"
    ML_alloc_array(Array, ((const MR_ArrayType *)Array0)->size + 1,
        MR_ALLOC_ID);
    ML_tr_copy_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0);
").

% :- pragma foreign_proc("C",
%   tr_array.copy(Array0::in, Array::array_uo),
%   [promise_pure, will_not_call_mercury],
% "
%   MR_incr_hp_msg(Array, ((const MR_ArrayType *)Array0)->size + 1,
%       MR_ALLOC_ID, ""array.array/1"");
%   ML_tr_copy_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0);
% ").

%-----------------------------------------------------------------------------%

tr_array.to_list(Array, List) :-
    tr_array.bounds(Array, Low, High),
    tr_array.fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

tr_array.fetch_items(Array, Low, High, List) :-
    (
            Low > High
    ->
            List = []
    ;
            Low1 = Low + 1,
            tr_array.fetch_items(Array, Low1, High, List0),
            tr_array.lookup(Array, Low, Item),
            List = [Item | List0]
    ).

%-----------------------------------------------------------------------------%

tr_array.bsearch(A, El, Compare, Result) :-
    tr_array.bounds(A, Lo, Hi),
    tr_array.bsearch_2(A, Lo, Hi, El, Compare, Result).

:- pred tr_array.bsearch_2(array(T), int, int, T,
    pred(T, T, comparison_result), maybe(int)).
:- mode tr_array.bsearch_2(in, in, in, in, pred(in, in, out) is det,
    out) is det.

tr_array.bsearch_2(Array, Lo, Hi, El, Compare, Result) :-
    Width = Hi - Lo,

    % If Width < 0, there is no range left.
    ( Width < 0 ->
        Result = no
    ;
        % If Width == 0, we may just have found our element.
        % Do a Compare to check.
        ( Width = 0 ->
            tr_array.lookup(Array, Lo, X),
            ( Compare(El, X, (=)) ->
                Result = yes(Lo)
            ;
                Result = no
            )
        ;
            % Otherwise find the middle element of the range
            % and check against that.
            Mid = (Lo + Hi) >> 1,   % `>> 1' is hand-optimized `div 2'.
            tr_array.lookup(Array, Mid, XMid),
            Compare(XMid, El, Comp),
            ( 
                Comp = (<),
                Mid1 = Mid + 1,
                tr_array.bsearch_2(Array, Mid1, Hi, El, Compare, Result)
            ; 
                Comp = (=),
                tr_array.bsearch_2(Array, Lo, Mid, El, Compare, Result)
            ;
                Comp = (>),
                Mid1 = Mid - 1,
                tr_array.bsearch_2(Array, Lo, Mid1, El, Compare, Result)
            )
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module tr_array.
%-----------------------------------------------------------------------------%
