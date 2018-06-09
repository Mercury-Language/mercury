%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% any_array.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Sep  8 15:18:38 EST 2005
%
% A copy of array.m adapted for arrays of values with inst any.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

    % An `any_array.index_out_of_bounds' is the exception thrown
    % on out-of-bounds any_array accesses. The string describes
    % the predicate or function reporting the error.
:- type any_array.index_out_of_bounds
    ---> any_array.index_out_of_bounds(string).

%-----------------------------------------------------------------------------%

    % any_array.make_empty_array(Array) creates an any_array of size zero
    % starting at lower bound 0.
    %
:- pred any_array.make_empty_array(any_array(T)::any_array_uo) is det.
:- func any_array.make_empty_array = (any_array(T)::any_array_uo) is det.

    % any_array.init(Size, Init, Array) creates an any_array with bounds from
    % 0 to Size-1, with each element initialized to Init.
    %
:- pred any_array.init(int, T, any_array(T)).
:- mode any_array.init(in, ia, any_array_uo) is det.

:- func any_array.init(int, T) = any_array(T).
:- mode any_array.init(in, ia) = any_array_uo is det.

    % any_array/1 is a function that constructs an any_array from a list.
    % (It does the same thing as the predicate any_array.from_any_list/2.)
    % The syntax `any_array([...])' is used to represent any_arrays
    % for io.read, io.write, term_to_type, and type_to_term.
    %
:- func any_array(list(T)) = any_array(T).
:- mode any_array(ia) = any_array_uo is det.

%-----------------------------------------------------------------------------%

    % any_array.min returns the lower bound of the any_array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred any_array.min(any_array(_T), int).
:- mode any_array.min(any_array_ui, out) is det.

:- func any_array.min(any_array(_T)) = int.
:- mode any_array.min(any_array_ui) = out is det.

:- func any_array.least_index(any_array(T)) = int.
:- mode any_array.least_index(any_array_ui) = out is det.

    % any_array.max returns the upper bound of the any_array.
    %
:- pred any_array.max(any_array(_T), int).
:- mode any_array.max(any_array_ui, out) is det.

:- func any_array.max(any_array(_T)) = int.
:- mode any_array.max(any_array_ui) = out is det.

:- func any_array.greatest_index(any_array(T)) = int.
:- mode any_array.greatest_index(any_array_ui) = out is det.

    % any_array.size returns the length of the any_array,
    % i.e. upper bound - lower bound + 1.
    %
:- pred any_array.size(any_array(_T), int).
:- mode any_array.size(any_array_ui, out) is det.

:- func any_array.size(any_array(_T)) = int.
:- mode any_array.size(any_array_ui) = out is det.

    % any_array.bounds returns the upper and lower bounds of an any_array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred any_array.bounds(any_array(_T), int, int).
:- mode any_array.bounds(any_array_ui, out, out) is det.

    % any_array.in_bounds checks whether an index is in the bounds of an
    % any_array.
    %
:- pred any_array.in_bounds(any_array(_T), int).
:- mode any_array.in_bounds(any_array_ui, in) is semidet.

%-----------------------------------------------------------------------------%

    % any_array.lookup returns the Nth element of an any_array.
    % Throws an exception if the index is out of bounds.
    %
:- pred any_array.lookup(any_array(T), int, T).
:- mode any_array.lookup(any_array_ui, in, oa) is det.

:- func any_array.lookup(any_array(T), int) = T.
:- mode any_array.lookup(any_array_ui, in) = oa is det.

    % any_array.semidet_lookup returns the Nth element of an any_array.
    % It fails if the index is out of bounds.
    %
:- pred any_array.semidet_lookup(any_array(T), int, T).
:- mode any_array.semidet_lookup(any_array_ui, in, oa) is semidet.

    % any_array.set sets the nth element of an any_array, and returns the
    % resulting any_array (good opportunity for destructive update ;-).
    % Throws an exception if the index is out of bounds.
    %
:- pred any_array.set(any_array(T), int, T, any_array(T)).
:- mode any_array.set(any_array_di, in, ia, any_array_uo) is det.

:- func any_array.set(any_array(T), int, T) = any_array(T).
:- mode any_array.set(any_array_di, in, ia) = any_array_uo is det.

    % any_array.semidet_set sets the nth element of an any_array, and returns
    % the resulting any_array. It fails if the index is out of bounds.
    %
:- pred any_array.semidet_set(any_array(T), int, T, any_array(T)).
:- mode any_array.semidet_set(any_array_di, in, ia, any_array_uo) is semidet.

    % any_array.slow_set sets the nth element of an any_array, and returns the
    % resulting any_array. The initial any_array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It is an error if the index is out of bounds.
    %
:- pred any_array.slow_set(any_array(T), int, T, any_array(T)).
:- mode any_array.slow_set(any_array_ui, in, ia, any_array_uo) is det.

:- func any_array.slow_set(any_array(T), int, T) = any_array(T).
:- mode any_array.slow_set(any_array_ui, in, ia) = any_array_uo is det.

    % any_array.semidet_slow_set sets the nth element of an any_array, and
    % returns the resulting any_array. The initial any_array is not required to
    % be unique, so the implementation may not be able to use destructive
    % update.  It fails if the index is out of bounds.
    %
:- pred any_array.semidet_slow_set(any_array(T), int, T, any_array(T)).
:- mode any_array.semidet_slow_set(any_array_ui, in, ia, any_array_uo)
        is semidet.

    % Field selection for any_arrays.
    % Array ^ elem(Index) = any_array.lookup(Array, Index).
    %
:- func any_array(T) ^ elem(int) = T.
:- mode any_array_ui ^ elem(in) = oa is det.

    % Field update for any_arrays.
    % (Array ^ elem(Index) := Value) = any_array.set(Array, Index, Value).
    %
:- func (any_array(T) ^ elem(int) := T) = any_array(T).
:- mode (any_array_di ^ elem(in) := ia) = any_array_uo is det.

%-----------------------------------------------------------------------------%

    % any_array.copy(Array0, Array):
    % Makes a new unique copy of an any_array.
    %
:- pred any_array.copy(any_array(T), any_array(T)).
:- mode any_array.copy(any_array_ui, any_array_uo) is det.

:- func any_array.copy(any_array(T)) = any_array(T).
:- mode any_array.copy(any_array_ui) = any_array_uo is det.

    % any_array.resize(Array0, Size, Init, Array):
    % The any_array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- pred any_array.resize(any_array(T), int, T, any_array(T)).
:- mode any_array.resize(any_array_di, in, ia, any_array_uo) is det.

:- func any_array.resize(any_array(T), int, T) = any_array(T).
:- mode any_array.resize(any_array_di, in, ia) = any_array_uo is det.

    % any_array.shrink(Array0, Size, Array):
    % The any_array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- pred any_array.shrink(any_array(T), int, any_array(T)).
:- mode any_array.shrink(any_array_di, in, any_array_uo) is det.

:- func any_array.shrink(any_array(T), int) = any_array(T).
:- mode any_array.shrink(any_array_di, in) = any_array_uo is det.

    % any_array.from_any_list takes a list, and returns an any_array
    % containing those elements in the same order that they occurred in the
    % list.
    %
:- pred any_array.from_any_list(list(T), any_array(T)).
:- mode any_array.from_any_list(ia, any_array_uo) is det.

:- func any_array.from_any_list(list(T)) = any_array(T).
:- mode any_array.from_any_list(ia) = any_array_uo is det.

    % any_array.to_any_list takes an any_array and returns an any_list
    % containing the elements of the any_array in the same order that they
    % occurred in the any_array.
    %
:- pred any_array.to_any_list(any_array(T), list(T)).
:- mode any_array.to_any_list(any_array_ui, oa) is det.

:- func any_array.to_any_list(any_array(T)) = list(T).
:- mode any_array.to_any_list(any_array_ui) = oa is det.

    % any_array.fetch_items takes an any_array and a lower and upper index,
    % and places those items in the any_array between these indices into a list.
    % It is an error if either index is out of bounds.
    %
:- pred any_array.fetch_items(any_array(T), int, int, list(T)).
:- mode any_array.fetch_items(any_array_ui, in, in, oa) is det.

:- func any_array.fetch_items(any_array(T), int, int) = list(T).
:- mode any_array.fetch_items(any_array_ui, in, in) = oa is det.

    % any_array.map(Closure, OldArray, NewArray) applies `Closure' to
    % each of the elements of `OldArray' to create `NewArray'.
    %
:- pred any_array.map(pred(T1, T2), any_array(T1), any_array(T2)).
:- mode any_array.map(pred(ia, oa) is det, any_array_di, any_array_uo) is det.

:- func any_array.map(func(T1) = T2, any_array(T1)) = any_array(T2).
:- mode any_array.map(func(ia) = oa is det, any_array_di) = any_array_uo
        is det.

    % any_array.foldl(Fn, Array, X) is equivalent to
    %   any_list.foldl(Fn, any_array.to_any_list(Array), X)
    % but more efficient.
    %
:- func any_array.foldl(func(T1, T2) = T2, any_array(T1), T2) = T2.
:- mode any_array.foldl(func(ia, in) = out is det, any_array_ui, in) = out
        is det.
:- mode any_array.foldl(func(ia, ia) = oa is det, any_array_ui, ia) = oa
        is det.

    % any_array.foldr(Fn, Array, X) is equivalent to
    %   list.foldr(Fn, any_array.to_any_list(Array), X)
    % but more efficient.
    %
:- func any_array.foldr(func(T1, T2) = T2, any_array(T1), T2) = T2.
:- mode any_array.foldr(func(ia, in) = out is det, any_array_ui, in) = out
        is det.
:- mode any_array.foldr(func(ia, ia) = oa is det, any_array_ui, ia) = oa
        is det.

    % any_array.random_permutation(A0, A, RS0, RS) permutes the elements in
    % A0 given random seed RS0 and returns the permuted any_array in A
    % and the next random seed in RS.
    %
:- pred any_array.random_permutation(any_array(T), any_array(T),
        random.supply, random.supply).
:- mode any_array.random_permutation(any_array_di, any_array_uo,
        mdi, muo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

    % MR_ArrayPtr is defined in runtime/mercury_library_types.h.
:- pragma foreign_type("C", any_array(T), "MR_ArrayPtr").

%-----------------------------------------------------------------------------%

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
    // never do bounds checking for Java (throw exceptions instead)
    SUCCESS_INDICATOR = false;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""     /* for MR_maybe_record_allocation() */
#include ""mercury_library_types.h""    /* for MR_ArrayPtr */

/*
** We do not yet record term sizes for any_arrays in term size profiling
** grades. Doing so would require
**
** - modifying ML_alloc_any_array to allocate an extra word for the size;
** - modifying all the predicates that call ML_alloc_any_array to compute the
**   size of the any_array (the sum of the sizes of the elements and the size of
**   the any_array itself);
** - modifying all the predicates that update any_array elements to compute the
**   difference between the sizes of the terms being added to and deleted from
**   the any_array, and updating the any_array size accordingly.
*/

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
/*
** The caller is responsible for allocating the memory for the any_array.
** This routine does the job of initializing the already-allocated memory.
*/
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

any_array.init(Size, Item, Array) :-
    ( Size < 0 ->
        error("any_array.init: negative size")
    ;
        any_array.init_2(Size, Item, Array)
    ).

:- pred any_array.init_2(int, T, any_array(T)).
:- mode any_array.init_2(in, ia, any_array_uo) is det.

:- pragma foreign_proc("C",
    any_array.init_2(Size::in, Item::ia, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
    ML_init_any_array(Array, Size, Item);
").

:- pragma foreign_proc("C",
    any_array.make_empty_array(Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, 1, MR_ALLOC_ID);
    ML_init_any_array(Array, 0, 0);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    any_array.min(Array::any_array_ui, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("C",
    any_array.max(Array::any_array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = Array->size - 1;
").

any_array.bounds(Array, Min, Max) :-
    any_array.min(Array, Min),
    any_array.max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    any_array.size(Array::any_array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = Array->size;
").

%-----------------------------------------------------------------------------%

any_array.in_bounds(Array, Index) :-
    any_array.bounds(Array, Min, Max),
    Min =< Index, Index =< Max.

any_array.semidet_lookup(Array, Index, Item) :-
    ( any_array.in_bounds(Array, Index) ->
        any_array.unsafe_lookup(Array, Index, Item)
    ;
        fail
    ).

any_array.semidet_set(Array0, Index, Item, Array) :-
    ( any_array.in_bounds(Array0, Index) ->
        any_array.unsafe_set(Array0, Index, Item, Array)
    ;
        fail
    ).

any_array.semidet_slow_set(Array0, Index, Item, Array) :-
    ( any_array.in_bounds(Array0, Index) ->
        any_array.slow_set(Array0, Index, Item, Array)
    ;
        fail
    ).

any_array.slow_set(Array0, Index, Item, Array) :-
    any_array.copy(Array0, Array1),
    any_array.set(Array1, Index, Item, Array).

%-----------------------------------------------------------------------------%

any_array.lookup(Array, Index, Item) :-
    ( bounds_checks, \+ any_array.in_bounds(Array, Index) ->
        out_of_bounds_error(Array, Index, "any_array.lookup")
    ;
        any_array.unsafe_lookup(Array, Index, Item)
    ).

:- pred any_array.unsafe_lookup(any_array(T), int, T).
:- mode any_array.unsafe_lookup(any_array_ui, in, oa) is det.

:- pragma foreign_proc("C",
    any_array.unsafe_lookup(Array::any_array_ui, Index::in, Item::oa),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = Array->elements[Index];
").

%-----------------------------------------------------------------------------%

any_array.set(Array0, Index, Item, Array) :-
    ( bounds_checks, \+ any_array.in_bounds(Array0, Index) ->
        out_of_bounds_error(Array0, Index, "any_array.set")
    ;
        any_array.unsafe_set(Array0, Index, Item, Array)
    ).

:- pred any_array.unsafe_set(any_array(T), int, T, any_array(T)).
:- mode any_array.unsafe_set(any_array_di, in, ia, any_array_uo) is det.

:- pragma foreign_proc("C",
    any_array.unsafe_set(Array0::any_array_di, Index::in, Item::ia,
    Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array0->elements[Index] = Item; /* destructive update! */
    Array = Array0;
").

%-----------------------------------------------------------------------------%

% lower bounds other than zero are not supported
%     % any_array.resize takes an any_array and new lower and upper bounds.
%     % the any_array is expanded or shrunk at each end to make it fit
%     % the new bounds.
% :- pred any_array.resize(any_array(T), int, int, any_array(T)).
% :- mode any_array.resize(in, in, in, out) is det.

:- pragma foreign_decl("C", "
void ML_resize_any_array(MR_ArrayPtr new_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size, MR_Word item);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new any_array.
** This routine does the job of copying the old any_array elements to the
** new any_array, initializing any additional elements in the new any_array,
** and deallocating the old any_array.
*/
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

    /*
    ** Since the mode on the old any_array is `any_array_di', it is safe to
    ** deallocate the storage for it.
    */
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

:- pragma foreign_proc("C",
    any_array.resize(Array0::any_array_di, Size::in, Item::ia,
    Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if ((Array0)->size == Size) {
        Array = Array0;
    } else {
        ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
        ML_resize_any_array(Array, Array0, Size, Item);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_shrink_any_array(MR_ArrayPtr any_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new any_array.
** This routine does the job of copying the old any_array elements to the
** new any_array and deallocating the old any_array.
*/
void
ML_shrink_any_array(MR_ArrayPtr any_array, MR_ArrayPtr old_array,
    MR_Integer any_array_size)
{
    MR_Integer i;

    any_array->size = any_array_size;
    for (i = 0; i < any_array_size; i++) {
        any_array->elements[i] = old_array->elements[i];
    }

    /*
    ** Since the mode on the old any_array is `any_array_di', it is safe to
    ** deallocate the storage for it.
    */
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

any_array.shrink(Array0, Size, Array) :-
    OldSize = any_array.size(Array0),
    ( Size > OldSize ->
        error("any_array.shrink: can't shrink to a larger size")
    ; Size = OldSize ->
        Array = Array0
    ;
        any_array.shrink_2(Array0, Size, Array)
    ).

:- pred any_array.shrink_2(any_array(T)::any_array_di, int::in,
        any_array(T)::any_array_uo) is det.

:- pragma foreign_proc("C",
    any_array.shrink_2(Array0::any_array_di, Size::in, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Size + 1, MR_ALLOC_ID);
    ML_shrink_any_array(Array, Array0, Size);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_copy_any_array(MR_ArrayPtr any_array, MR_ConstArrayPtr old_array);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new any_array.
** This routine does the job of copying the any_array elements.
*/
void
ML_copy_any_array(MR_ArrayPtr any_array, MR_ConstArrayPtr old_array)
{
    /*
    ** Any changes to this function will probably also require
    ** changes to deepcopy() in runtime/deep_copy.c.
    */

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
    any_array.copy(Array0::any_array_ui, Array::any_array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ML_alloc_any_array(Array, Array0->size + 1, MR_ALLOC_ID);
    ML_copy_any_array(Array, (MR_ConstArrayPtr) Array0);
").

%-----------------------------------------------------------------------------%

any_array(List) = Array :-
    any_array.from_any_list(List, Array).

any_array.from_any_list([], Array) :-
    any_array.make_empty_array(Array).
any_array.from_any_list(List, Array) :-
    List = [Head | Tail],
    Len = any_list.length(List),
    any_array.init(Len, Head, Array0),
    any_array.insert_items(Tail, 1, Array0, Array).

%-----------------------------------------------------------------------------%

:- pred any_array.insert_items(list(T)::ia, int::in,
        any_array(T)::any_array_di, any_array(T)::any_array_uo) is det.

any_array.insert_items([], _N, Array, Array).
any_array.insert_items([Head|Tail], N, Array0, Array) :-
    any_array.set(Array0, N, Head, Array1),
    N1 = N + 1,
    any_array.insert_items(Tail, N1, Array1, Array).

%-----------------------------------------------------------------------------%

any_array.to_any_list(Array, List) :-
    any_array.bounds(Array, Low, High),
    any_array.fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

any_array.fetch_items(Array, Low, High, List) :-
    List = foldr_0(func(X::ia, Xs::ia) = ([X | Xs]::oa) is det,
        Array, [], Low, High).

%-----------------------------------------------------------------------------%

any_array.map(Closure, OldArray, NewArray) :-
    ( any_array.semidet_lookup(OldArray, 0, Elem0) ->
        any_array.size(OldArray, Size),
        call(Closure, Elem0, Elem),
        any_array.init(Size, Elem, NewArray0),
        any_array.map_2(1, Size, Closure, OldArray,
        NewArray0, NewArray)
    ;
        any_array.make_empty_array(NewArray)
    ).

:- pred any_array.map_2(int::in, int::in,
        pred(T1, T2)::in(pred(ia, oa) is det), any_array(T1)::any_array_ui,
        any_array(T2)::any_array_di, any_array(T2)::any_array_uo) is det.

any_array.map_2(N, Size, Closure, OldArray, NewArray0, NewArray) :-
    ( N >= Size ->
        NewArray = NewArray0
    ;
        any_array.lookup(OldArray, N, OldElem),
        Closure(OldElem, NewElem),
        any_array.set(NewArray0, N, NewElem, NewArray1),
        any_array.map_2(N + 1, Size, Closure, OldArray,
        NewArray1, NewArray)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

any_array.make_empty_array = A :-
    any_array.make_empty_array(A).

any_array.init(N, X) = A :-
    any_array.init(N, X, A).

any_array.min(A) = N :-
    any_array.min(A, N).

any_array.max(A) = N :-
    any_array.max(A, N).

any_array.size(A) = N :-
    any_array.size(A, N).

any_array.lookup(A, N) = X :-
    any_array.lookup(A, N, X).

any_array.set(A1, N, X) = A2 :-
    any_array.set(A1, N, X, A2).

any_array.slow_set(A1, N, X) = A2 :-
    any_array.slow_set(A1, N, X, A2).

any_array.copy(A1) = A2 :-
    any_array.copy(A1, A2).

any_array.resize(A1, N, X) = A2 :-
    any_array.resize(A1, N, X, A2).

any_array.shrink(A1, N) = A2 :-
    any_array.shrink(A1, N, A2).

any_array.from_any_list(Xs) = A :-
    any_array.from_any_list(Xs, A).

any_array.to_any_list(A) = Xs :-
    any_array.to_any_list(A, Xs).

any_array.fetch_items(A, N1, N2) = Xs :-
    any_array.fetch_items(A, N1, N2, Xs).

any_array.map(F, A1) = A2 :-
    P = ( pred(X::ia, Y::oa) is det :- Y = F(X) ),
    any_array.map(P, A1, A2).

Array ^ elem(Index) =
    any_array.lookup(Array, Index).

(Array ^ elem(Index) := Value) =
    any_array.set(Array, Index, Value).

%------------------------------------------------------------------------------%

any_array.random_permutation(A0, A, RS0, RS) :-
    Lo = any_array.min(A0),
    Hi = any_array.max(A0),
    Sz = any_array.size(A0),
    permutation_2(Lo, Lo, Hi, Sz, A0, A, RS0, RS).

:- pred permutation_2(int, int, int, int, any_array(T), any_array(T),
        random.supply, random.supply).
:- mode permutation_2(in, in, in, in, any_array_di, any_array_uo, mdi, muo) is det.

permutation_2(I, Lo, Hi, Sz, A0, A, RS0, RS) :-
    ( I > Hi ->
        A  = A0,
        RS = RS0
    ;
        random.random(R, RS0, RS1),
        J  = Lo + (R `rem` Sz),
        A1 = swap_elems(A0, I, J),
        permutation_2(I + 1, Lo, Hi, Sz, A1, A, RS1, RS)
    ).

%------------------------------------------------------------------------------%

:- func swap_elems(any_array(T), int, int) = any_array(T).
:- mode swap_elems(any_array_di, in, in) = any_array_uo is det.

swap_elems(A0, I, J) = A :-
    XI = A0 ^ elem(I),
    XJ = A0 ^ elem(J),
    A  = ((A0 ^ elem(I) := XJ) ^ elem(J) := XI).

%------------------------------------------------------------------------------%

any_array.foldl(Fn, A, X) =
    foldl_0(Fn, A, X, any_array.min(A), any_array.max(A)).

:- func foldl_0(func(T1, T2) = T2, any_array(T1), T2, int, int) = T2.
:- mode foldl_0(func(ia, in) = out is det, any_array_ui, in, in, in) = out
        is det.
:- mode foldl_0(func(ia, ia) = oa is det, any_array_ui, ia, in, in) = oa
        is det.

foldl_0(Fn, A, X, I, Max) =
    ( Max < I ->
        X
    ;
        foldl_0(Fn, A, Fn(A ^ elem(I), X), I + 1, Max)
    ).

%-----------------------------------------------------------------------------%

any_array.foldr(Fn, A, X) =
    foldr_0(Fn, A, X, any_array.min(A), any_array.max(A)).

:- func foldr_0(func(T1, T2) = T2, any_array(T1), T2, int, int) = T2.
:- mode foldr_0(func(ia, in) = out is det, any_array_ui, in, in, in) = out
        is det.
:- mode foldr_0(func(ia, ia) = oa is det, any_array_ui, ia, in, in) = oa
        is det.
:- mode foldr_0(func(ia, di) = uo is det, any_array_ui, di, in, in) = uo
        is det.

foldr_0(Fn, A, X, Min, I) =
    ( I < Min ->
        X
    ;
        foldr_0(Fn, A, Fn(A ^ elem(I), X), Min, I - 1)
    ).

%------------------------------------------------------------------------------%

    % throw an exception indicating an any_array bounds error
:- pred out_of_bounds_error(any_array(T), int, string).
:- mode out_of_bounds_error(any_array_ui, in, in) is erroneous.

out_of_bounds_error(Array, Index, PredName) :-
    % Note: we deliberately do not include the any_array element type name in
    % the error message here, for performance reasons: using the type name
    % could prevent the compiler from optimizing away the construction of the
    % type_info in the caller, because it would prevent unused argument
    % elimination. Performance is important here, because any_array.set and
    % any_array.lookup are likely to be used in the inner loops of
    % performance-critical applications.
    any_array.bounds(Array, Min, Max),
    throw(any_array.index_out_of_bounds(
        string.format("%s: index %d not in range [%d, %d]",
            [s(PredName), i(Index), i(Min), i(Max)]))).

%-----------------------------------------------------------------------------%

any_array.least_index(A) = any_array.min(A).

any_array.greatest_index(A) = any_array.max(A).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
