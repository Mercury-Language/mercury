%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% any_array.m
% Ralph Becket <rafe@cs.mu.oz.au>
%
% A copy of array.m adapted for arrays of values with inst any.
%
%---------------------------------------------------------------------------%

:- module any_array.
:- interface.

:- import_module list.
:- import_module random.

:- type any_array(T).

:- inst any_array(I) == bound(any_array(I)).
:- inst any_array == any_array(any).
:- inst any_array_skel == any_array(free).

    % XXX the current Mercury compiler doesn't support `ui' modes,
    % so to work-around that problem, we currently don't use
    % unique modes in this module.

% :- inst uniq_any_array(I) == unique(any_array(I)).
% :- inst uniq_any_array == uniq_any_array(unique).
:- inst uniq_any_array(I) == bound(any_array(I)). % XXX work-around
:- inst uniq_any_array == uniq_any_array(ground). % XXX work-around
:- inst uniq_any_array_skel == uniq_any_array(free).

:- mode any_array_di == di(uniq_any_array).
:- mode any_array_uo == out(uniq_any_array).
:- mode any_array_ui == in(uniq_any_array).

% :- inst mostly_uniq_any_array(I) == mostly_unique(any_array(I)).
% :- inst mostly_uniq_any_array == mostly_uniq_any_array(mostly_unique).
:- inst mostly_uniq_any_array(I) == bound(any_array(I)).    % XXX work-around
:- inst mostly_uniq_any_array == mostly_uniq_any_array(ground).  % XXX ditto.
:- inst mostly_uniq_any_array_skel == mostly_uniq_any_array(free).

:- mode any_array_mdi == mdi(mostly_uniq_any_array).
:- mode any_array_muo == out(mostly_uniq_any_array).
:- mode any_array_mui == in(mostly_uniq_any_array).

    % An `index_out_of_bounds' is the exception thrown on out-of-bounds
    % any_array accesses. The string describes the predicate or function
    % reporting the error.
:- type index_out_of_bounds
    --->    index_out_of_bounds(string).

%---------------------------------------------------------------------------%

    % make_empty_array(Array) creates an any_array of size zero
    % starting at lower bound 0.
    %
:- func make_empty_array = (any_array(T)::any_array_uo) is det.
:- pred make_empty_array(any_array(T)::any_array_uo) is det.

    % init(Size, Init, Array) creates an any_array with bounds from
    % 0 to Size-1, with each element initialized to Init.
    %
:- func init(int::in, T::ia) = (any_array(T)::any_array_uo) is det.
:- pred init(int::in, T::ia, any_array(T)::any_array_uo) is det.

    % any_array/1 is a function that constructs an any_array from a list.
    % (It does the same thing as the predicate from_any_list/2.)
    % The syntax `any_array([...])' is used to represent any_arrays
    % for io.read, io.write, term_to_type, and type_to_term.
    %
:- func any_array(list(T)::ia) = (any_array(T)::any_array_uo) is det.

%---------------------------------------------------------------------------%

    % min returns the lower bound of the any_array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- func min(any_array(T)::any_array_ui) = (int::out) is det.
:- pred min(any_array(T)::any_array_ui, int::out) is det.
:- func least_index(any_array(T)::any_array_ui) = (int::out) is det.

    % max returns the upper bound of the any_array.
    %
:- func max(any_array(T)::any_array_ui) = (int::out) is det.
:- pred max(any_array(T)::any_array_ui, int::out) is det.
:- func greatest_index(any_array(T)::any_array_ui) = (int::out) is det.

    % size returns the length of the any_array,
    % i.e. upper bound - lower bound + 1.
    %
:- func size(any_array(T)::any_array_ui) = (int::out) is det.
:- pred size(any_array(T)::any_array_ui, int::out) is det.

    % bounds returns the upper and lower bounds of an any_array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred bounds(any_array(T)::any_array_ui, int::out, int::out) is det.

    % in_bounds checks whether an index is in the bounds of an any_array.
    %
:- pred in_bounds(any_array(T)::any_array_ui, int::in) is semidet.

%---------------------------------------------------------------------------%

    % lookup returns the Nth element of an any_array.
    % Throws an exception if the index is out of bounds.
    %
:- func lookup(any_array(T)::any_array_ui, int::in) = (T::oa) is det.
:- pred lookup(any_array(T)::any_array_ui, int::in, T::oa) is det.

    % semidet_lookup returns the Nth element of an any_array.
    % It fails if the index is out of bounds.
    %
:- pred semidet_lookup(any_array(T)::any_array_ui, int::in, T::oa) is semidet.

    % set sets the nth element of an any_array, and returns the resulting
    % any_array (good opportunity for destructive update ;-).
    % Throws an exception if the index is out of bounds.
    %
:- func set(any_array(T)::any_array_di, int::in, T::ia)
    = (any_array(T)::any_array_uo) is det.
:- pred set(int::in, T::ia,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

    % semidet_set sets the nth element of an any_array, and returns
    % the resulting any_array. It fails if the index is out of bounds.
    %
:- pred semidet_set(int::in, T::ia,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is semidet.

    % slow_set sets the nth element of an any_array, and returns the
    % resulting any_array. The initial any_array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It is an error if the index is out of bounds.
    %
:- func slow_set(any_array(T)::any_array_ui, int::in, T::ia)
    = (any_array(T)::any_array_uo) is det.
:- pred slow_set(int::in, T::ia,
    any_array(T)::any_array_ui, any_array(T)::any_array_uo) is det.

    % semidet_slow_set sets the nth element of an any_array, and
    % returns the resulting any_array. The initial any_array is not required to
    % be unique, so the implementation may not be able to use destructive
    % update.  It fails if the index is out of bounds.
    %
:- pred semidet_slow_set(int::in, T::ia,
    any_array(T)::any_array_ui, any_array(T)::any_array_uo) is semidet.

    % Field selection for any_arrays.
    % Array ^ elem(Index) = lookup(Array, Index).
    %
:- func any_array(T) ^ elem(int) = T.
:- mode any_array_ui ^ elem(in) = oa is det.

    % Field update for any_arrays.
    % (Array ^ elem(Index) := Value) = set(Array, Index, Value).
    %
:- func (any_array(T) ^ elem(int) := T) = any_array(T).
:- mode (any_array_di ^ elem(in) := ia) = any_array_uo is det.

%---------------------------------------------------------------------------%

    % copy(Array0, Array):
    % Makes a new unique copy of an any_array.
    %
:- pred copy(any_array(T)::any_array_ui, any_array(T)::any_array_uo) is det.

:- func copy(any_array(T)::any_array_ui) = (any_array(T)::any_array_uo) is det.

    % resize(Array0, Size, Init) = Array:
    % resize(Size, Init, Array0, Array):
    % The any_array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- func resize(any_array(T)::any_array_di, int::in, T::ia) =
    (any_array(T)::any_array_uo) is det.
:- pred resize(int::in, T::ia,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

    % shrink(Array0, Size) = Array:
    % shrink(Size, Array0, Array):
    % The any_array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- func shrink(any_array(T)::any_array_di, int::in) =
    (any_array(T)::any_array_uo) is det.
:- pred shrink(int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

    % from_any_list takes a list, and returns an any_array containing
    % those elements in the same order that they occurred in the list.
    %
:- func from_any_list(list(T)::ia) = (any_array(T)::any_array_uo) is det.
:- pred from_any_list(list(T)::ia, any_array(T)::any_array_uo) is det.

    % to_any_list takes an any_array and returns an any_list containing
    % the elements of the any_array in the same order that they occurred
    % in the any_array.
    %
:- func to_any_list(any_array(T)::any_array_ui) = (list(T)::oa) is det.
:- pred to_any_list(any_array(T)::any_array_ui, list(T)::oa) is det.

    % fetch_items takes an any_array and a lower and upper index,
    % and places those items in the any_array between these indices
    % into a list. It is an error if either index is out of bounds.
    %
:- func fetch_items(any_array(T)::any_array_ui, int::in, int::in) =
    (list(T)::oa) is det.
:- pred fetch_items(any_array(T)::any_array_ui, int::in, int::in,
    list(T)::oa) is det.

    % map(Pred, OldArray, NewArray) applies `Pred' to
    % each of the elements of `OldArray' to create `NewArray'.
    %
:- pred map(pred(T1, T2), any_array(T1), any_array(T2)).
:- mode map(pred(ia, oa) is det, any_array_di, any_array_uo) is det.

:- func map(func(T1) = T2, any_array(T1)) = any_array(T2).
:- mode map(func(ia) = oa is det, any_array_di) = any_array_uo is det.

    % foldl(Fn, Array, X) is equivalent to
    %   any_list.foldl(Fn, to_any_list(Array), X)
    % but more efficient.
    %
:- func foldl(func(T1, T2) = T2, any_array(T1), T2) = T2.
:- mode foldl(func(ia, in) = out is det, any_array_ui, in) = out is det.
:- mode foldl(func(ia, ia) = oa is det, any_array_ui, ia) = oa is det.

    % foldr(Fn, Array, X) is equivalent to
    %   list.foldr(Fn, to_any_list(Array), X)
    % but more efficient.
    %
:- func foldr(func(T1, T2) = T2, any_array(T1), T2) = T2.
:- mode foldr(func(ia, in) = out is det, any_array_ui, in) = out is det.
:- mode foldr(func(ia, ia) = oa is det, any_array_ui, ia) = oa is det.

    % random_permutation(A0, A, R0, R) permutes the elements in A0
    % given random number generator R0 and returns the permuted any_array in A
    % and the next random generator in R.
    %
:- pred random_permutation(
    any_array(T)::any_array_di, any_array(T)::any_array_uo,
    R::in, R::out) is det <= random(R).

    % random_permutation(RNG, A0, A, RS0, RS) permutes the elements
    % in A0 given unique random number generator RNG and initial random state
    % RS0. It returns the permuted any_array in A and the updated random
    % number generator state in RS.
    %
:- pred random_permutation(P::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo,
    S::di, S::uo) is det <= urandom(P, S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.

    % MR_ArrayPtr is defined in runtime/mercury_library_types.h.
:- pragma foreign_type("C", any_array(T), "MR_ArrayPtr").

%---------------------------------------------------------------------------%

:- pred bounds_checks is semidet.
:- pragma inline(bounds_checks/0).

:- pragma foreign_proc("C",
    bounds_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#ifdef ML_OMIT_ARRAY_BOUNDS_CHECKS
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("C#",
    bounds_checks,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if ML_OMIT_ARRAY_BOUNDS_CHECKS
    SUCCESS_INDICATOR = false;
#else
    SUCCESS_INDICATOR = true;
#endif
").

:- pragma foreign_proc("Java",
    bounds_checks,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Never do bounds checking for Java (throw exceptions instead).
    SUCCESS_INDICATOR = false;
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""             // for MR_maybe_record_allocation()
#include ""mercury_library_types.h""    // for MR_ArrayPtr

// We do not yet record term sizes for any_arrays in term size profiling
// grades. Doing so would require
//
// - modifying ML_alloc_any_array to allocate an extra word for the size;
// - modifying all the predicates that call ML_alloc_any_array to compute
//   the size of the any_array (the sum of the sizes of the elements and
//   the size of the any_array itself);
//
// - modifying all the predicates that update any_array elements to compute
//   the difference between the sizes of the terms being added to and
//   deleted from the any_array, and updating the any_array size accordingly.

#define ML_alloc_any_array(newarray, any_arraysize, alloc_id)               \
    do {                                                                    \
        MR_Word newarray_word;                                              \
        MR_offset_incr_hp_msg(newarray_word, 0, (any_arraysize),            \
            alloc_id, ""any_array.any_array/1"");                           \
        (newarray) = (MR_ArrayPtr) newarray_word;                           \
    } while (0)
").

:- pragma foreign_decl("C", "
void ML_init_any_array(MR_ArrayPtr, MR_Integer size, MR_Word item);
").

:- pragma foreign_code("C", "
// The caller is responsible for allocating the memory for the any_array.
// This routine does the job of initializing the already-allocated memory.

void
ML_init_any_array(MR_ArrayPtr any_array, MR_Integer size, MR_Word item)
{
    MR_Integer i;

    any_array->size = size;
    for (i = 0; i < size; i++) {
        any_array->elements[i] = item;
    }
}
").

init(Size, Item, Array) :-
    ( if Size < 0 then
        error("any_array.init: negative size")
    else
        init_2(Size, Item, Array)
    ).

:- pred init_2(int, T, any_array(T)).
:- mode init_2(in, ia, any_array_uo) is det.

:- pragma foreign_proc("C",
    init_2(Size::in, Item::ia, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
    ML_init_any_array(Array, Size, Item);
").

:- pragma foreign_proc("C",
    make_empty_array(Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, 1, MR_ALLOC_ID);
    ML_init_any_array(Array, 0, 0);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min(Array::any_array_ui, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Array not used
    Min = 0;
").

:- pragma foreign_proc("C",
    max(Array::any_array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = Array->size - 1;
").

bounds(Array, Min, Max) :-
    min(Array, Min),
    max(Array, Max).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    size(Array::any_array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = Array->size;
").

%---------------------------------------------------------------------------%

in_bounds(Array, Index) :-
    bounds(Array, Min, Max),
    Min =< Index,
    Index =< Max.

semidet_lookup(Array, Index, Item) :-
    ( if in_bounds(Array, Index) then
        unsafe_lookup(Array, Index, Item)
    else
        fail
    ).

semidet_set(Index, Item, Array0, Array) :-
    ( if in_bounds(Array0, Index) then
        unsafe_set(Index, Item, Array0, Array)
    else
        fail
    ).

semidet_slow_set(Index, Item, Array0, Array) :-
    ( if in_bounds(Array0, Index) then
        slow_set(Index, Item, Array0, Array)
    else
        fail
    ).

slow_set(Index, Item, Array0, Array) :-
    any_array.copy(Array0, Array1),
    any_array.set(Index, Item, Array1, Array).

%---------------------------------------------------------------------------%

lookup(Array, Index, Item) :-
    ( if
        bounds_checks,
        not in_bounds(Array, Index)
    then
        out_of_bounds_error(Array, Index, "any_array.lookup")
    else
        unsafe_lookup(Array, Index, Item)
    ).

:- pred unsafe_lookup(any_array(T), int, T).
:- mode unsafe_lookup(any_array_ui, in, oa) is det.

:- pragma foreign_proc("C",
    unsafe_lookup(Array::any_array_ui, Index::in, Item::oa),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = Array->elements[Index];
").

%---------------------------------------------------------------------------%

set(Index, Item, Array0, Array) :-
    ( if
        bounds_checks,
        not in_bounds(Array0, Index)
    then
        out_of_bounds_error(Array0, Index, "any_array.set")
    else
        unsafe_set(Index, Item, Array0, Array)
    ).

:- pred unsafe_set(int::in, T::ia,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

:- pragma foreign_proc("C",
    unsafe_set(Index::in, Item::ia, Array0::any_array_di, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array0->elements[Index] = Item; // destructive update!
    Array = Array0;
").

%---------------------------------------------------------------------------%

% lower bounds other than zero are not supported
%     % resize takes an any_array and new lower and upper bounds.
%     % the any_array is expanded or shrunk at each end to make it fit
%     % the new bounds.
% :- pred resize(any_array(T), int, int, any_array(T)).
% :- mode resize(in, in, in, out) is det.

:- pragma foreign_decl("C", "
void ML_resize_any_array(MR_ArrayPtr new_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size, MR_Word item);
").

:- pragma foreign_code("C", "
// The caller is responsible for allocating the storage for the new any_array.
// This routine does the job of copying the old any_array elements to the
// new any_array, initializing any additional elements in the new any_array,
// and deallocating the old any_array.

void
ML_resize_any_array(MR_ArrayPtr any_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size, MR_Word item)
{
    MR_Integer i;
    MR_Integer elements_to_copy;

    elements_to_copy = old_array->size;
    if (elements_to_copy > any_array_size) {
        elements_to_copy = any_array_size;
    }

    any_array->size = any_array_size;
    for (i = 0; i < elements_to_copy; i++) {
        any_array->elements[i] = old_array->elements[i];
    }
    for (; i < any_array_size; i++) {
        any_array->elements[i] = item;
    }

    // Since the mode on the old any_array is `any_array_di', it is safe to
    // deallocate the storage for it.
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

:- pragma foreign_proc("C",
    resize(Size::in, Item::ia, Array0::any_array_di, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if ((Array0)->size == Size) {
        Array = Array0;
    } else {
        ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
        ML_resize_any_array(Array, Array0, Size, Item);
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_shrink_any_array(MR_ArrayPtr any_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size);
").

:- pragma foreign_code("C", "
// The caller is responsible for allocating the storage for the new any_array.
// This routine does the job of copying the old any_array elements to the
// new any_array and deallocating the old any_array.

void
ML_shrink_any_array(MR_ArrayPtr any_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size)
{
    MR_Integer i;

    any_array->size = any_array_size;
    for (i = 0; i < any_array_size; i++) {
        any_array->elements[i] = old_array->elements[i];
    }

    // Since the mode on the old any_array is `any_array_di', it is safe to
    // deallocate the storage for it.
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

shrink(Size, Array0, Array) :-
    OldSize = size(Array0),
    ( if Size > OldSize then
        error("any_array.shrink: can't shrink to a larger size")
    else if Size = OldSize then
        Array = Array0
    else
        shrink_2(Size, Array0, Array)
    ).

:- pred shrink_2(int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

:- pragma foreign_proc("C",
    shrink_2(Size::in, Array0::any_array_di, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
    ML_shrink_any_array(Array, Array0, Size);
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_copy_any_array(MR_ArrayPtr any_array, MR_ConstArrayPtr old_array);
").

:- pragma foreign_code("C", "
// The caller is responsible for allocating the storage for the new any_array.
// This routine does the job of copying the any_array elements.

void
ML_copy_any_array(MR_ArrayPtr any_array, MR_ConstArrayPtr old_array)
{
    // Any changes to this function will probably also require
    // changes to deepcopy() in runtime/deep_copy.c.

    MR_Integer i;
    MR_Integer any_array_size;

    any_array_size = old_array->size;
    any_array->size = any_array_size;
    for (i = 0; i < any_array_size; i++) {
        any_array->elements[i] = old_array->elements[i];
    }
}
").

:- pragma foreign_proc("C",
    copy(Array0::any_array_ui, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Array0->size + 1, MR_ALLOC_ID);
    ML_copy_any_array(Array, (MR_ConstArrayPtr) Array0);
").

%---------------------------------------------------------------------------%

any_array(List) = Array :-
    from_any_list(List, Array).

from_any_list([], Array) :-
    make_empty_array(Array).
from_any_list(List, Array) :-
    List = [Head | Tail],
    Len = any_list.length(List),
    init(Len, Head, Array0),
    insert_items(Tail, 1, Array0, Array).

%---------------------------------------------------------------------------%

:- pred insert_items(list(T)::ia, int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

insert_items([], _N, !Array).
insert_items([Head | Tail], N, !Array) :-
    set(N, Head, !Array),
    insert_items(Tail, N + 1, !Array).

%---------------------------------------------------------------------------%

to_any_list(Array, List) :-
    bounds(Array, Low, High),
    fetch_items(Array, Low, High, List).

%---------------------------------------------------------------------------%

fetch_items(Array, Low, High, List) :-
    foldr_loop(func(X::ia, Xs::ia) = ([X | Xs]::oa) is det,
        Array, Low, High, [], List).

%---------------------------------------------------------------------------%

map(Pred, OldArray, NewArray) :-
    ( if semidet_lookup(OldArray, 0, Elem0) then
        size(OldArray, Size),
        call(Pred, Elem0, Elem),
        init(Size, Elem, NewArray0),
        map_loop(1, Size, Pred, OldArray, NewArray0, NewArray)
    else
        make_empty_array(NewArray)
    ).

:- pred map_loop(int::in, int::in,
    pred(T1, T2)::in(pred(ia, oa) is det), any_array(T1)::any_array_ui,
    any_array(T2)::any_array_di, any_array(T2)::any_array_uo) is det.

map_loop(N, Size, Pred, OldArray, !NewArray) :-
    ( if N >= Size then
        true
    else
        lookup(OldArray, N, OldElem),
        Pred(OldElem, NewElem),
        set(N, NewElem, !NewArray),
        map_loop(N + 1, Size, Pred, OldArray, !NewArray)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

make_empty_array = A :-
    any_array.make_empty_array(A).

init(N, X) = A :-
    any_array.init(N, X, A).

min(A) = N :-
    any_array.min(A, N).

max(A) = N :-
    any_array.max(A, N).

size(A) = N :-
    any_array.size(A, N).

lookup(A, N) = X :-
    any_array.lookup(A, N, X).

set(!.A, N, X) = !:A :-
    any_array.set(N, X, !A).

slow_set(!.A, N, X) = !:A :-
    any_array.slow_set(N, X, !A).

copy(A1) = A2 :-
    any_array.copy(A1, A2).

resize(A1, N, X) = A2 :-
    any_array.resize(N, X, A1, A2).

shrink(A1, N) = A2 :-
    any_array.shrink(N, A1, A2).

from_any_list(Xs) = A :-
    any_array.from_any_list(Xs, A).

to_any_list(A) = Xs :-
    any_array.to_any_list(A, Xs).

fetch_items(A, N1, N2) = Xs :-
    any_array.fetch_items(A, N1, N2, Xs).

map(F, A1) = A2 :-
    P = ( pred(X::ia, Y::oa) is det :- Y = F(X) ),
    any_array.map(P, A1, A2).

Array ^ elem(Index) =
    any_array.lookup(Array, Index).

(Array ^ elem(Index) := Value) =
    any_array.set(Array, Index, Value).

%---------------------------------------------------------------------------%

random_permutation(A0, A, !R) :-
    Lo = min(A0),
    Hi = max(A0),
    Sz = size(A0),
    permutation_loop(Lo, Lo, Hi, Sz, A0, A, !R).

:- pred permutation_loop(int::in, int::in, int::in, int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo,
    R::in, R::out) is det <= random(R).

permutation_loop(I, Lo, Hi, Sz, !A, !R) :-
    ( if I > Hi then
        true
    else
        uniform_int_in_range(Lo, Sz, J, !R),
        swap_elems(I, J, !A),
        permutation_loop(I + 1, Lo, Hi, Sz, !A, !R)
    ).

%---------------------------------------------------------------------------%

random_permutation(P, A0, A, !S) :-
    Lo = min(A0),
    Hi = max(A0),
    Sz = size(A0),
    permutation_loop(P, Lo, Lo, Hi, Sz, A0, A, !S).

:- pred permutation_loop(P::in, int::in, int::in, int::in, int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo, S::di, S::uo)
    is det <= urandom(P, S).

permutation_loop(P, I, Lo, Hi, Sz, !A, !S) :-
    ( if I > Hi then
        true
    else
        uniform_int_in_range(P, Lo, Sz, J, !S),
        swap_elems(I, J, !A),
        permutation_loop(P, I + 1, Lo, Hi, Sz, !A, !S)
    ).

%---------------------------------------------------------------------------%

:- pred swap_elems(int::in, int::in,
    any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

swap_elems(I, J, !A) :-
    any_array.lookup(!.A, I, XI),
    any_array.lookup(!.A, J, XJ),
    any_array.set(I, XJ, !A),
    any_array.set(J, XI, !A).

%---------------------------------------------------------------------------%

foldl(Func, Array, Acc0) = Acc :-
    foldl_loop(Func, Array, min(Array), max(Array), Acc0, Acc).

:- pred foldl_loop(func(T, Acc) = Acc, any_array(T), int, int, Acc, Acc).
:- mode foldl_loop(func(ia, in) = out is det, any_array_ui, in, in, in, out)
    is det.
:- mode foldl_loop(func(ia, ia) = oa is det, any_array_ui, in, in, ia, oa)
    is det.

foldl_loop(Func, Array, I, Max, !Acc) :-
    ( if Max < I then
        true
    else
        lookup(Array, I, XI),
        !:Acc = Func(XI, !.Acc),
        foldl_loop(Func, Array, I + 1, Max, !Acc)
    ).

%---------------------------------------------------------------------------%

foldr(Func, Array, Acc0) = Acc :-
    foldr_loop(Func, Array, min(Array), max(Array), Acc0, Acc).

:- pred foldr_loop(func(T, Acc) = Acc, any_array(T), int, int, Acc, Acc).
:- mode foldr_loop(func(ia, in) = out is det, any_array_ui, in, in, in, out)
    is det.
:- mode foldr_loop(func(ia, ia) = oa is det, any_array_ui, in, in, ia, oa)
    is det.
:- mode foldr_loop(func(ia, di) = uo is det, any_array_ui, in, in, di, uo)
    is det.

foldr_loop(Func, Array, Min, I, !Acc) :-
    ( if I < Min then
        true
    else
        lookup(Array, I, XI),
        !:Acc = Func(XI, !.Acc),
        foldr_loop(Func, Array, Min, I - 1, !Acc)
    ).

%---------------------------------------------------------------------------%

    % throw an exception indicating an any_array bounds error
:- pred out_of_bounds_error(any_array(T), int, string).
:- mode out_of_bounds_error(any_array_ui, in, in) is erroneous.

out_of_bounds_error(Array, Index, PredName) :-
    % Note: we deliberately do not include the any_array element type name in
    % the error message here, for performance reasons: using the type name
    % could prevent the compiler from optimizing away the construction of the
    % type_info in the caller, because it would prevent unused argument
    % elimination. Performance is important here, because set and lookup
    % are likely to be used in the inner loops of performance-critical
    % applications.
    bounds(Array, Min, Max),
    Msg = string.format("%s: index %d not in range [%d, %d]",
        [s(PredName), i(Index), i(Min), i(Max)]),
    throw(index_out_of_bounds(Msg)).

%---------------------------------------------------------------------------%

least_index(A) = min(A).

greatest_index(A) = max(A).

%---------------------------------------------------------------------------%
