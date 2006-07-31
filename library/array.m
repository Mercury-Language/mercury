%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: array.m.
% Main authors: fjh, bromage.
% Stability: medium-low.
%
% This module provides dynamically-sized one-dimensional arrays.
% Array indices start at zero.
%
% By default, the array.set and array.lookup procedures will check
% for bounds errors.  But for better performance, it is possible to
% disable some of the checking by compiling with `--intermodule-optimization'
% and with the C macro symbol `ML_OMIT_ARRAY_BOUNDS_CHECKS'
% defined, e.g. by using `MCFLAGS=--intermodule-optimization' and
% `CFLAGS=-DML_OMIT_ARRAY_BOUNDS_CHECKS' in your Mmakefile,
% or by compiling with the command
% `mmc --intermodule-optimization --cflags -DML_OMIT_ARRAY_BOUNDS_CHECKS'.
%
% For maximum performance, all bounds checking can be disabled by
% recompiling this module using `CFLAGS=-DML_OMIT_ARRAY_BOUNDS_CHECKS'
% or `mmc --cflags -DML_OMIT_ARRAY_BOUNDS_CHECKS' as above. You can
% either recompile the entire library, or just copy `array.m' to your
% application's source directory and link with it directly instead of as
% part of the library.
%
% WARNING!
%
% Arrays are currently not unique objects - until this situation is
% resolved it is up to the programmer to ensure that arrays are used
% in such a way as to preserve correctness.  In the absence of mode
% reordering, one should therefore assume that evaluation will take
% place in left-to-right order.  For example, the following code will
% probably not work as expected (f is a function, A an array, I an
% index, and X an appropriate value):
%
%       Y = f(A ^ elem(I) := X, A ^ elem(I))
%
% The compiler is likely to compile this as
%
%       V0 = A ^ elem(I) := X,
%       V1 = A ^ elem(I),
%       Y  = f(V0, V1)
%
% and will be unaware that the first line should be ordered
% *after* the second.  The safest thing to do is write things out
% by hand in the form
%
%       A0I = A0 ^ elem(I),
%       A1  = A0 ^ elem(I) := X,
%       Y   = f(A1, A0I)
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module array.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module random.

:- type array(T).

:- inst array(I) == bound(array(I)).
:- inst array == array(ground).
:- inst array_skel == array(free).

    % XXX the current Mercury compiler doesn't support `ui' modes,
    % so to work-around that problem, we currently don't use
    % unique modes in this module.

% :- inst uniq_array(I) == unique(array(I)).
% :- inst uniq_array == uniq_array(unique).
:- inst uniq_array(I) == bound(array(I)). % XXX work-around
:- inst uniq_array == uniq_array(ground). % XXX work-around
:- inst uniq_array_skel == uniq_array(free).

:- mode array_di == di(uniq_array).
:- mode array_uo == out(uniq_array).
:- mode array_ui == in(uniq_array).

% :- inst mostly_uniq_array(I) == mostly_unique(array(I)).
% :- inst mostly_uniq_array == mostly_uniq_array(mostly_unique).
:- inst mostly_uniq_array(I) == bound(array(I)).    % XXX work-around
:- inst mostly_uniq_array == mostly_uniq_array(ground). % XXX work-around
:- inst mostly_uniq_array_skel == mostly_uniq_array(free).

:- mode array_mdi == mdi(mostly_uniq_array).
:- mode array_muo == out(mostly_uniq_array).
:- mode array_mui == in(mostly_uniq_array).

    % An `array.index_out_of_bounds' is the exception thrown
    % on out-of-bounds array accesses. The string describes
    % the predicate or function reporting the error.
:- type array.index_out_of_bounds
    ---> array.index_out_of_bounds(string).

%-----------------------------------------------------------------------------%

    % array.make_empty_array(Array) creates an array of size zero
    % starting at lower bound 0.
    %
:- pred array.make_empty_array(array(T)::array_uo) is det.

:- func array.make_empty_array = (array(T)::array_uo) is det.

    % array.init(Size, Init, Array) creates an array with bounds from 0
    % to Size-1, with each element initialized to Init.
    %
:- pred array.init(int, T, array(T)).
:- mode array.init(in, in, array_uo) is det.

:- func array.init(int, T) = array(T).
:- mode array.init(in, in) = array_uo is det.

    % array/1 is a function that constructs an array from a list.
    % (It does the same thing as the predicate array.from_list/2.)
    % The syntax `array([...])' is used to represent arrays
    % for io.read, io.write, term_to_type, and type_to_term.
    %
:- func array(list(T)) = array(T).
:- mode array(in) = array_uo is det.

%-----------------------------------------------------------------------------%

    % array.min returns the lower bound of the array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred array.min(array(_T), int).
:- mode array.min(array_ui, out) is det.
:- mode array.min(in, out) is det.

:- func array.min(array(_T)) = int.
:- mode array.min(array_ui) = out is det.
:- mode array.min(in) = out is det.

:- func array.least_index(array(T)) = int.
:- mode array.least_index(array_ui) = out is det.
:- mode array.least_index(in) = out is det.

    % array.max returns the upper bound of the array.
    %
:- pred array.max(array(_T), int).
:- mode array.max(array_ui, out) is det.
:- mode array.max(in, out) is det.

:- func array.max(array(_T)) = int.
:- mode array.max(array_ui) = out is det.
:- mode array.max(in) = out is det.

:- func array.greatest_index(array(T)) = int.
:- mode array.greatest_index(array_ui) = out is det.
:- mode array.greatest_index(in) = out is det.

    % array.size returns the length of the array,
    % i.e. upper bound - lower bound + 1.
    %
:- pred array.size(array(_T), int).
:- mode array.size(array_ui, out) is det.
:- mode array.size(in, out) is det.

:- func array.size(array(_T)) = int.
:- mode array.size(array_ui) = out is det.
:- mode array.size(in) = out is det.

    % array.bounds returns the upper and lower bounds of an array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred array.bounds(array(_T), int, int).
:- mode array.bounds(array_ui, out, out) is det.
:- mode array.bounds(in, out, out) is det.

    % array.in_bounds checks whether an index is in the bounds of an array.
    %
:- pred array.in_bounds(array(_T), int).
:- mode array.in_bounds(array_ui, in) is semidet.
:- mode array.in_bounds(in, in) is semidet.

%-----------------------------------------------------------------------------%

    % array.lookup returns the Nth element of an array.
    % Throws an exception if the index is out of bounds.
    %
:- pred array.lookup(array(T), int, T).
:- mode array.lookup(array_ui, in, out) is det.
:- mode array.lookup(in, in, out) is det.

:- func array.lookup(array(T), int) = T.
:- mode array.lookup(array_ui, in) = out is det.
:- mode array.lookup(in, in) = out is det.

    % array.semidet_lookup returns the Nth element of an array.
    % It fails if the index is out of bounds.
    %
:- pred array.semidet_lookup(array(T), int, T).
:- mode array.semidet_lookup(array_ui, in, out) is semidet.
:- mode array.semidet_lookup(in, in, out) is semidet.

    % array.set sets the nth element of an array, and returns the
    % resulting array (good opportunity for destructive update ;-).
    % Throws an exception if the index is out of bounds.
    %
:- pred array.set(array(T), int, T, array(T)).
:- mode array.set(array_di, in, in, array_uo) is det.

:- func array.set(array(T), int, T) = array(T).
:- mode array.set(array_di, in, in) = array_uo is det.

    % array.semidet_set sets the nth element of an array, and returns
    % the resulting array. It fails if the index is out of bounds.
    %
:- pred array.semidet_set(array(T), int, T, array(T)).
:- mode array.semidet_set(array_di, in, in, array_uo) is semidet.

    % array.slow_set sets the nth element of an array, and returns the
    % resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It is an error if the index is out of bounds.
    %
:- pred array.slow_set(array(T), int, T, array(T)).
:- mode array.slow_set(array_ui, in, in, array_uo) is det.
:- mode array.slow_set(in, in, in, array_uo) is det.

:- func array.slow_set(array(T), int, T) = array(T).
:- mode array.slow_set(array_ui, in, in) = array_uo is det.
:- mode array.slow_set(in, in, in) = array_uo is det.

    % array.semidet_slow_set sets the nth element of an array, and returns
    % the resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It fails if the index is out of bounds.
    %
:- pred array.semidet_slow_set(array(T), int, T, array(T)).
:- mode array.semidet_slow_set(array_ui, in, in, array_uo) is semidet.
:- mode array.semidet_slow_set(in, in, in, array_uo) is semidet.

    % Field selection for arrays.
    % Array ^ elem(Index) = array.lookup(Array, Index).
    %
:- func array.elem(int, array(T)) = T.
:- mode array.elem(in, array_ui) = out is det.
:- mode array.elem(in, in) = out is det.

    % Field update for arrays.
    % (Array ^ elem(Index) := Value) = array.set(Array, Index, Value).
    %
:- func 'elem :='(int, array(T), T) = array(T).
:- mode 'elem :='(in, array_di, in) = array_uo is det.

%-----------------------------------------------------------------------------%

    % array.copy(Array0, Array):
    % Makes a new unique copy of an array.
    %
:- pred array.copy(array(T), array(T)).
:- mode array.copy(array_ui, array_uo) is det.
:- mode array.copy(in, array_uo) is det.

:- func array.copy(array(T)) = array(T).
:- mode array.copy(array_ui) = array_uo is det.
:- mode array.copy(in) = array_uo is det.

    % array.resize(Array0, Size, Init, Array):
    % The array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- pred array.resize(array(T), int, T, array(T)).
:- mode array.resize(array_di, in, in, array_uo) is det.

:- func array.resize(array(T), int, T) = array(T).
:- mode array.resize(array_di, in, in) = array_uo is det.

    % array.shrink(Array0, Size, Array):
    % The array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- pred array.shrink(array(T), int, array(T)).
:- mode array.shrink(array_di, in, array_uo) is det.

:- func array.shrink(array(T), int) = array(T).
:- mode array.shrink(array_di, in) = array_uo is det.

    % array.from_list takes a list, and returns an array containing those
    % elements in the same order that they occurred in the list.
    %
:- pred array.from_list(list(T), array(T)).
:- mode array.from_list(in, array_uo) is det.

:- func array.from_list(list(T)) = array(T).
:- mode array.from_list(in) = array_uo is det.

    % array.to_list takes an array and returns a list containing the elements
    % of the array in the same order that they occurred in the array.
    %
:- pred array.to_list(array(T), list(T)).
:- mode array.to_list(array_ui, out) is det.
:- mode array.to_list(in, out) is det.

:- func array.to_list(array(T)) = list(T).
:- mode array.to_list(array_ui) = out is det.
:- mode array.to_list(in) = out is det.

    % array.fetch_items takes an array and a lower and upper index,
    % and places those items in the array between these indices into a list.
    % It is an error if either index is out of bounds.
    %
:- pred array.fetch_items(array(T), int, int, list(T)).
:- mode array.fetch_items(in, in, in, out) is det.

:- func array.fetch_items(array(T), int, int) = list(T).
:- mode array.fetch_items(array_ui, in, in) = out is det.
:- mode array.fetch_items(in, in, in) = out is det.

    % array.bsearch takes an array, an element to be matched and a comparison
    % predicate and returns the position of the first occurrence in the array
    % of an element which is equivalent to the given one in the ordering
    % provided. Assumes the array is sorted according to this ordering.
    % Fails if the element is not present.
    %
:- pred array.bsearch(array(T), T, comparison_pred(T), maybe(int)).
:- mode array.bsearch(array_ui, in, in(comparison_pred), out) is det.
:- mode array.bsearch(in, in, in(comparison_pred), out) is det.

:- func array.bsearch(array(T), T, comparison_func(T)) = maybe(int).
:- mode array.bsearch(array_ui, in, in(comparison_func)) = out is det.
:- mode array.bsearch(in, in, in(comparison_func)) = out is det.

    % array.map(Closure, OldArray, NewArray) applies `Closure' to
    % each of the elements of `OldArray' to create `NewArray'.
    %
:- pred array.map(pred(T1, T2), array(T1), array(T2)).
:- mode array.map(pred(in, out) is det, array_di, array_uo) is det.

:- func array.map(func(T1) = T2, array(T1)) = array(T2).
:- mode array.map(func(in) = out is det, array_di) = array_uo is det.

:- func array_compare(array(T), array(T)) = comparison_result.
:- mode array_compare(in, in) = uo is det.

    % array.sort(Array) returns a version of Array sorted into ascending
    % order.
    %
    % This sort is not stable. That is, elements that compare/3 decides are
    % equal will appear together in the sorted array, but not necessarily
    % in the same order in which they occurred in the input array. This is
    % primarily only an issue with types with user-defined equivalence for
    % which `equivalent' objects are otherwise distinguishable.
    %
:- func array.sort(array(T)) = array(T).
:- mode array.sort(array_di) = array_uo is det.

    % array.foldl(Fn, Array, X) is equivalent to
    %   list.foldl(Fn, array.to_list(Array), X)
    % but more efficient.
    %
:- func array.foldl(func(T1, T2) = T2, array(T1), T2) = T2.
:- mode array.foldl(func(in, in) = out is det, array_ui, in) = out is det.
:- mode array.foldl(func(in, in) = out is det, in, in) = out is det.
:- mode array.foldl(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode array.foldl(func(in, di) = uo is det, in, di) = uo is det.

    % array.foldr(Fn, Array, X) is equivalent to
    %   list.foldr(Fn, array.to_list(Array), X)
    % but more efficient.
    %
:- func array.foldr(func(T1, T2) = T2, array(T1), T2) = T2.
:- mode array.foldr(func(in, in) = out is det, array_ui, in) = out is det.
:- mode array.foldr(func(in, in) = out is det, in, in) = out is det.
:- mode array.foldr(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode array.foldr(func(in, di) = uo is det, in, di) = uo is det.

    % array.random_permutation(A0, A, RS0, RS) permutes the elements in
    % A0 given random seed RS0 and returns the permuted array in A
    % and the next random seed in RS.
    %
:- pred array.random_permutation(array(T), array(T),
    random.supply, random.supply).
:- mode array.random_permutation(array_di, array_uo, mdi, muo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything beyond here is not intended as part of the public interface,
% and will not appear in the Mercury Library Reference Manual.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.

%
% Define the array type appropriately for the different targets.
% Note that the definitions here should match what is output by
% mlds_to_c.m, mlds_to_il.m, or mlds_to_java.m for mlds.mercury_array_type.
%

    % MR_ArrayPtr is defined in runtime/mercury_library_types.h.
:- pragma foreign_type("C", array(T), "MR_ArrayPtr")
    where equality is array.array_equal,
    comparison is array.array_compare.

:- pragma foreign_type(il,  array(T), "class [mscorlib]System.Array")
    where equality is array.array_equal,
    comparison is array.array_compare.

    % We can't use `java.lang.Object []', since we want a generic type
    % that is capable of holding any kind of array, including e.g. `int []'.
    % Java doesn't have any equivalent of .NET's System.Array class,
    % so we just use the universal base `java.lang.Object'.
:- pragma foreign_type(java,  array(T), "/* Array */ java.lang.Object")
    where equality is array.array_equal,
    comparison is array.array_compare.

    % unify/2 for arrays
    %
:- pred array_equal(array(T)::in, array(T)::in) is semidet.
:- pragma export(array_equal(in, in), "ML_array_equal").
:- pragma terminates(array_equal/2).

array_equal(Array1, Array2) :-
    (
        array.size(Array1, Size),
        array.size(Array2, Size)
    ->
        array.equal_elements(0, Size, Array1, Array2)
    ;
        fail
    ).

:- pred array.equal_elements(int, int, array(T), array(T)).
:- mode array.equal_elements(in, in, in, in) is semidet.

array.equal_elements(N, Size, Array1, Array2) :-
    ( N = Size ->
        true
    ;
        array.lookup(Array1, N, Elem),
        array.lookup(Array2, N, Elem),
        N1 = N + 1,
        array.equal_elements(N1, Size, Array1, Array2)
    ).

    % compare/3 for arrays
    %
:- pred array_compare(comparison_result::uo, array(T)::in, array(T)::in)
    is det.
:- pragma export(array_compare(uo, in, in), "ML_array_compare").
:- pragma terminates(array_compare/3).

array_compare(Result, Array1, Array2) :-
    array.size(Array1, Size1),
    array.size(Array2, Size2),
    compare(SizeResult, Size1, Size2),
    ( SizeResult = (=) ->
        array.compare_elements(0, Size1, Array1, Array2, Result)
    ;
        Result = SizeResult
    ).

:- pred array.compare_elements(int::in, int::in, array(T)::in, array(T)::in,
    comparison_result::uo) is det.

array.compare_elements(N, Size, Array1, Array2, Result) :-
    ( N = Size ->
        Result = (=)
    ;
        array.lookup(Array1, N, Elem1),
        array.lookup(Array2, N, Elem2),
        compare(ElemResult, Elem1, Elem2),
        ( ElemResult = (=) ->
            N1 = N + 1,
            array.compare_elements(N1, Size, Array1, Array2, Result)
        ;
            Result = ElemResult
        )
    ).

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
    succeeded = false;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""     /* for MR_maybe_record_allocation() */
#include ""mercury_library_types.h""    /* for MR_ArrayPtr */

/*
** We do not yet record term sizes for arrays in term size profiling
** grades. Doing so would require
**
** - modifying ML_alloc_array to allocate an extra word for the size;
** - modifying all the predicates that call ML_alloc_array to compute the
**   size of the array (the sum of the sizes of the elements and the size of
**   the array itself);
** - modifying all the predicates that update array elements to compute the
**   difference between the sizes of the terms being added to and deleted from
**   the array, and updating the array size accordingly.
*/

#define ML_alloc_array(newarray, arraysize, proclabel)                  \
    do {                                                                \
        MR_Word newarray_word;                                          \
        MR_offset_incr_hp_msg(newarray_word, 0, (arraysize),            \
            proclabel, ""array:array/1"");                              \
        (newarray) = (MR_ArrayPtr) newarray_word;                       \
    } while (0)
").

:- pragma foreign_decl("C", "
void ML_init_array(MR_ArrayPtr, MR_Integer size, MR_Word item);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the memory for the array.
** This routine does the job of initializing the already-allocated memory.
*/
void
ML_init_array(MR_ArrayPtr array, MR_Integer size, MR_Word item)
{
    MR_Integer i;

    array->size = size;
    for (i = 0; i < size; i++) {
        array->elements[i] = item;
    }
}
").

array.init(Size, Item, Array) :-
    ( Size < 0 ->
        error("array.init: negative size")
    ;
        array.init_2(Size, Item, Array)
    ).

:- pred array.init_2(int::in, T::in, array(T)::array_uo) is det.

:- pragma foreign_proc("C",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ML_alloc_array(Array, Size + 1, MR_PROC_LABEL);
    ML_init_array(Array, Size, Item);
").

:- pragma foreign_proc("C",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ML_alloc_array(Array, 1, MR_PROC_LABEL);
    ML_init_array(Array, 0, 0);
").

:- pragma foreign_proc("C#",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = System.Array.CreateInstance(Item.GetType(), Size);
    for (int i = 0; i < Size; i++) {
        Array.SetValue(Item, i);
    }
").
:- pragma foreign_proc("C#",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX A better solution then using the null pointer to represent
    // the empty array would be to create an array of size 0.  However
    // we need to determine the element type of the array before we can
    // do that.  This could be done by examing the RTTI of the array
    // type and then using System.Type.GetType(""<mercury type>"") to
    // determine it.  However constructing the <mercury type> string is
    // a non-trival amount of work.
    Array = null;
").

:- pragma foreign_proc("Java",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.lang.Class itemClass = Item.getClass();

    Array = java.lang.reflect.Array.newInstance(itemClass, Size);
    for (int i = 0; i < Size; i++) {
        java.lang.reflect.Array.set(Array, i, Item);
    }
").
:- pragma foreign_proc("Java",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX as per C#
    Array = null;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    array.min(Array::array_ui, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    /* Array not used */
    Min = 0;
").
:- pragma foreign_proc("C",
    array.min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("C#",
    array.min(Array::array_ui, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").
:- pragma foreign_proc("C#",
    array.min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("Java",
    array.min(_Array::array_ui, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").
:- pragma foreign_proc("Java",
    array.min(_Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("C",
    array.max(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Max = Array->size - 1;
").
:- pragma foreign_proc("C",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Max = Array->size - 1;
").
:- pragma foreign_proc("C#",
    array.max(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length - 1;
    } else {
        Max = -1;
    }
").
:- pragma foreign_proc("C#",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length - 1;
    } else {
        Max = -1;
    }
").

:- pragma foreign_proc("Java",
    array.max(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = java.lang.reflect.Array.getLength(Array) - 1;
    } else {
        Max = -1;
    }
").
:- pragma foreign_proc("Java",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = java.lang.reflect.Array.getLength(Array) - 1;
    } else {
        Max = -1;
    }
").

array.bounds(Array, Min, Max) :-
    array.min(Array, Min),
    array.max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    array.size(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Max = Array->size;
").
:- pragma foreign_proc("C",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Max = Array->size;
").

:- pragma foreign_proc("C#",
    array.size(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length;
    } else {
        Max = 0;
    }
").
:- pragma foreign_proc("C#",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length;
    } else {
        Max = 0;
    }
").

:- pragma foreign_proc("Java",
    array.size(Array::array_ui, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = java.lang.reflect.Array.getLength(Array);
    } else {
        Max = 0;
    }
").
:- pragma foreign_proc("Java",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = java.lang.reflect.Array.getLength(Array);
    } else {
        Max = 0;
    }
").

%-----------------------------------------------------------------------------%

array.in_bounds(Array, Index) :-
    array.bounds(Array, Min, Max),
    Min =< Index, Index =< Max.

array.semidet_lookup(Array, Index, Item) :-
    ( array.in_bounds(Array, Index) ->
        array.unsafe_lookup(Array, Index, Item)
    ;
        fail
    ).

array.semidet_set(Array0, Index, Item, Array) :-
    ( array.in_bounds(Array0, Index) ->
        array.unsafe_set(Array0, Index, Item, Array)
    ;
        fail
    ).

array.semidet_slow_set(Array0, Index, Item, Array) :-
    ( array.in_bounds(Array0, Index) ->
        array.slow_set(Array0, Index, Item, Array)
    ;
        fail
    ).

array.slow_set(Array0, Index, Item, Array) :-
    array.copy(Array0, Array1),
    array.set(Array1, Index, Item, Array).

%-----------------------------------------------------------------------------%

array.lookup(Array, Index, Item) :-
    ( bounds_checks, \+ array.in_bounds(Array, Index) ->
        out_of_bounds_error(Array, Index, "array.lookup")
    ;
        array.unsafe_lookup(Array, Index, Item)
    ).

:- pred array.unsafe_lookup(array(T), int, T).
:- mode array.unsafe_lookup(array_ui, in, out) is det.
:- mode array.unsafe_lookup(in, in, out) is det.

:- pragma foreign_proc("C",
    array.unsafe_lookup(Array::array_ui, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Item = Array->elements[Index];
").
:- pragma foreign_proc("C",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Item = Array->elements[Index];
").

:- pragma foreign_proc("C#",
    array.unsafe_lookup(Array::array_ui, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Item = Array.GetValue(Index);
}").
:- pragma foreign_proc("C#",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Item = Array.GetValue(Index);
}").

:- pragma foreign_proc("Java",
    array.unsafe_lookup(Array::array_ui, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = java.lang.reflect.Array.get(Array, Index);
").
:- pragma foreign_proc("Java",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = java.lang.reflect.Array.get(Array, Index);
").

%-----------------------------------------------------------------------------%

array.set(Array0, Index, Item, Array) :-
    ( bounds_checks, \+ array.in_bounds(Array0, Index) ->
        out_of_bounds_error(Array0, Index, "array.set")
    ;
        array.unsafe_set(Array0, Index, Item, Array)
    ).

:- pred array.unsafe_set(array(T), int, T, array(T)).
:- mode array.unsafe_set(array_di, in, in, array_uo) is det.

:- pragma foreign_proc("C",
    array.unsafe_set(Array0::array_di, Index::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Array0->elements[Index] = Item; /* destructive update! */
    Array = Array0;
").

:- pragma foreign_proc("C#",
    array.unsafe_set(Array0::array_di, Index::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Array0.SetValue(Item, Index);   /* destructive update! */
    Array = Array0;
}").

:- pragma foreign_proc("Java",
    array.unsafe_set(Array0::array_di, Index::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.lang.reflect.Array.set(Array0, Index, Item);
    Array = Array0;         /* destructive update! */
").

%-----------------------------------------------------------------------------%

% lower bounds other than zero are not supported
%     % array.resize takes an array and new lower and upper bounds.
%     % the array is expanded or shrunk at each end to make it fit
%     % the new bounds.
% :- pred array.resize(array(T), int, int, array(T)).
% :- mode array.resize(in, in, in, out) is det.

:- pragma foreign_decl("C", "
void ML_resize_array(MR_ArrayPtr new_array, MR_ArrayPtr old_array,
    MR_Integer array_size, MR_Word item);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new array.
** This routine does the job of copying the old array elements to the
** new array, initializing any additional elements in the new array,
** and deallocating the old array.
*/
void
ML_resize_array(MR_ArrayPtr array, MR_ArrayPtr old_array,
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
    ** Since the mode on the old array is `array_di', it is safe to
    ** deallocate the storage for it.
    */
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

:- pragma foreign_proc("C",
    array.resize(Array0::array_di, Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if ((Array0)->size == Size) {
        Array = Array0;
    } else {
        ML_alloc_array(Array, Size + 1, MR_PROC_LABEL);
        ML_resize_array(Array, Array0, Size, Item);
    }
").

:- pragma foreign_proc("C#",
    array.resize(Array0::array_di, Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 == null) {
        Array = System.Array.CreateInstance(Item.GetType(), Size);
        for (int i = 0; i < Size; i++) {
            Array.SetValue(Item, i);
        }
    }
    else if (Array0.Length == Size) {
        Array = Array0;
    } else if (Array0.Length > Size) {
        Array = System.Array.CreateInstance(Item.GetType(), Size);
        System.Array.Copy(Array0, Array, Size);
    } else {
        Array = System.Array.CreateInstance(Item.GetType(), Size);
        System.Array.Copy(Array0, Array, Array0.Length);
        for (int i = Array0.Length; i < Size; i++) {
            Array.SetValue(Item, i);
        }
    }
").

:- pragma foreign_proc("Java",
    array.resize(Array0::array_di, Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.lang.Class itemClass = Item.getClass();

    if (Size == 0) {
        Array = null;
    } else if (Array0 == null) {
        Array = java.lang.reflect.Array.newInstance(itemClass, Size);
        for (int i = 0; i < Size; i++) {
            java.lang.reflect.Array.set(Array, i, Item);
        }
    } else if (java.lang.reflect.Array.getLength(Array0) == Size) {
        Array = Array0;
    } else {
        Array = java.lang.reflect.Array.newInstance(itemClass, Size);

        int i;
        for (i = 0; i < java.lang.reflect.Array.getLength(Array0) &&
                i < Size; i++)
        {
            java.lang.reflect.Array.set(Array, i,
                    java.lang.reflect.Array.get(Array0, i)
                    );
        }
        for (/*i = Array0.length*/; i < Size; i++) {
            java.lang.reflect.Array.set(Array, i, Item);
        }
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_shrink_array(MR_ArrayPtr array, MR_ArrayPtr old_array,
    MR_Integer array_size);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new array.
** This routine does the job of copying the old array elements to the
** new array and deallocating the old array.
*/
void
ML_shrink_array(MR_ArrayPtr array, MR_ArrayPtr old_array,
    MR_Integer array_size)
{
    MR_Integer i;

    array->size = array_size;
    for (i = 0; i < array_size; i++) {
        array->elements[i] = old_array->elements[i];
    }

    /*
    ** Since the mode on the old array is `array_di', it is safe to
    ** deallocate the storage for it.
    */
#ifdef MR_CONSERVATIVE_GC
    GC_FREE(old_array);
#endif
}
").

array.shrink(Array0, Size, Array) :-
    OldSize = array.size(Array0),
    ( Size > OldSize ->
        error("array.shrink: can't shrink to a larger size")
    ; Size = OldSize ->
        Array = Array0
    ;
        array.shrink_2(Array0, Size, Array)
    ).

:- pred array.shrink_2(array(T)::array_di, int::in, array(T)::array_uo)
    is det.

:- pragma foreign_proc("C",
    array.shrink_2(Array0::array_di, Size::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ML_alloc_array(Array, Size + 1, MR_PROC_LABEL);
    ML_shrink_array(Array, Array0, Size);
").

:- pragma foreign_proc("C#",
    array.shrink_2(Array0::array_di, Size::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = System.Array.CreateInstance(Array0.GetType().GetElementType(),
        Size);
    System.Array.Copy(Array0, Array, Size);
").

:- pragma foreign_proc("Java",
    array.shrink_2(Array0::array_di, Size::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 == null) {
        Array = null;
    } else {
        java.lang.Class itemClass =
            java.lang.reflect.Array.get(Array0, 0).getClass();
        Array = java.lang.reflect.Array.newInstance(itemClass, Size);
        for (int i = 0; i < Size; i++) {
            java.lang.reflect.Array.set(Array, i,
                java.lang.reflect.Array.get(Array0, i));
        }
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
void ML_copy_array(MR_ArrayPtr array, MR_ConstArrayPtr old_array);
").

:- pragma foreign_code("C", "
/*
** The caller is responsible for allocating the storage for the new array.
** This routine does the job of copying the array elements.
*/
void
ML_copy_array(MR_ArrayPtr array, MR_ConstArrayPtr old_array)
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
    array.copy(Array0::array_ui, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ML_alloc_array(Array, Array0->size + 1, MR_PROC_LABEL);
    ML_copy_array(Array, (MR_ConstArrayPtr) Array0);
").

:- pragma foreign_proc("C",
    array.copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ML_alloc_array(Array, Array0->size + 1, MR_PROC_LABEL);
    ML_copy_array(Array, (MR_ConstArrayPtr) Array0);
").

:- pragma foreign_proc("C#",
    array.copy(Array0::array_ui, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX we implement the same as ML_copy_array, which doesn't appear
    // to deep copy the array elements
    Array = System.Array.CreateInstance(Array0.GetType().GetElementType(),
        Array0.Length);
    System.Array.Copy(Array0, Array, Array0.Length);
").

:- pragma foreign_proc("C#",
    array.copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX we implement the same as ML_copy_array, which doesn't appear
    // to deep copy the array elements
    Array = System.Array.CreateInstance(Array0.GetType().GetElementType(),
        Array0.Length);
    System.Array.Copy(Array0, Array, Array0.Length);
").

:- pragma foreign_proc("Java",
    array.copy(Array0::array_ui, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 == null) {
        Array = null;
    } else {
        java.lang.Class itemClass =
            java.lang.reflect.Array.get(Array0, 0).getClass();
        int length = java.lang.reflect.Array.getLength(Array0);
        Array = java.lang.reflect.Array.newInstance(itemClass, length);
        for (int i = 0; i < length; i++) {
            java.lang.reflect.Array.set(Array, i,
                java.lang.reflect.Array.get(Array0, i));
        }
    }
").
:- pragma foreign_proc("Java",
    array.copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 == null) {
        Array = null;
    } else {
        java.lang.Class itemClass =
            java.lang.reflect.Array.get(Array0, 0).getClass();
        int length = java.lang.reflect.Array.getLength(Array0);
        Array = java.lang.reflect.Array.newInstance(itemClass, length);
        for (int i = 0; i < length; i++) {
            java.lang.reflect.Array.set(Array, i,
                java.lang.reflect.Array.get(Array0, i));
        }
    }
").

%-----------------------------------------------------------------------------%

array(List) = Array :-
    array.from_list(List, Array).

array.from_list([], Array) :-
    array.make_empty_array(Array).
array.from_list(List, Array) :-
    List = [Head | Tail],
    list.length(List, Len),
    array.init(Len, Head, Array0),
    array.insert_items(Tail, 1, Array0, Array).

%-----------------------------------------------------------------------------%

:- pred array.insert_items(list(T)::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

array.insert_items([], _N, Array, Array).
array.insert_items([Head|Tail], N, Array0, Array) :-
    array.set(Array0, N, Head, Array1),
    N1 = N + 1,
    array.insert_items(Tail, N1, Array1, Array).

%-----------------------------------------------------------------------------%

array.to_list(Array, List) :-
    array.bounds(Array, Low, High),
    array.fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

array.fetch_items(Array, Low, High, List) :-
    List = foldr_0(func(X, Xs) = [X | Xs], Array, [], Low, High).

%-----------------------------------------------------------------------------%

array.bsearch(A, El, Compare, Result) :-
    array.bounds(A, Lo, Hi),
    array.bsearch_2(A, Lo, Hi, El, Compare, Result).

:- pred array.bsearch_2(array(T)::in, int::in, int::in, T::in,
    pred(T, T, comparison_result)::in(pred(in, in, out) is det),
    maybe(int)::out) is det.

array.bsearch_2(Array, Lo, Hi, El, Compare, Result) :-
    Width = Hi - Lo,

    % If Width < 0, there is no range left.
    ( Width < 0 ->
        Result = no
    ;
        % If Width == 0, we may just have found our element.
        % Do a Compare to check.
        ( Width = 0 ->
            array.lookup(Array, Lo, X),
            ( call(Compare, El, X, (=)) ->
                Result = yes(Lo)
            ;
                Result = no
            )
        ;
            % Otherwise find the middle element of the range
            % and check against that.
            Mid = (Lo + Hi) >> 1,   % `>> 1' is hand-optimized `div 2'.
            array.lookup(Array, Mid, XMid),
            call(Compare, XMid, El, Comp),
            (
                Comp = (<),
                Mid1 = Mid + 1,
                array.bsearch_2(Array, Mid1, Hi, El, Compare, Result)
            ;
                Comp = (=),
                array.bsearch_2(Array, Lo, Mid, El, Compare, Result)
            ;
                Comp = (>),
                Mid1 = Mid - 1,
                array.bsearch_2(Array, Lo, Mid1, El, Compare, Result)
            )
        )
    ).

%-----------------------------------------------------------------------------%

array.map(Closure, OldArray, NewArray) :-
    ( array.semidet_lookup(OldArray, 0, Elem0) ->
        array.size(OldArray, Size),
        call(Closure, Elem0, Elem),
        array.init(Size, Elem, NewArray0),
        array.map_2(1, Size, Closure, OldArray,
        NewArray0, NewArray)
    ;
        array.make_empty_array(NewArray)
    ).

:- pred array.map_2(int::in, int::in, pred(T1, T2)::in(pred(in, out) is det),
    array(T1)::in, array(T2)::array_di, array(T2)::array_uo) is det.

array.map_2(N, Size, Closure, OldArray, NewArray0, NewArray) :-
    ( N >= Size ->
        NewArray = NewArray0
    ;
        array.lookup(OldArray, N, OldElem),
        Closure(OldElem, NewElem),
        array.set(NewArray0, N, NewElem, NewArray1),
        array.map_2(N + 1, Size, Closure, OldArray,
        NewArray1, NewArray)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

array.make_empty_array = A :-
    array.make_empty_array(A).

array.init(N, X) = A :-
    array.init(N, X, A).

array.min(A) = N :-
    array.min(A, N).

array.max(A) = N :-
    array.max(A, N).

array.size(A) = N :-
    array.size(A, N).

array.lookup(A, N) = X :-
    array.lookup(A, N, X).

array.set(A1, N, X) = A2 :-
    array.set(A1, N, X, A2).

array.slow_set(A1, N, X) = A2 :-
    array.slow_set(A1, N, X, A2).

array.copy(A1) = A2 :-
    array.copy(A1, A2).

array.resize(A1, N, X) = A2 :-
    array.resize(A1, N, X, A2).

array.shrink(A1, N) = A2 :-
    array.shrink(A1, N, A2).

array.from_list(Xs) = A :-
    array.from_list(Xs, A).

array.to_list(A) = Xs :-
    array.to_list(A, Xs).

array.fetch_items(A, N1, N2) = Xs :-
    array.fetch_items(A, N1, N2, Xs).

array.bsearch(A, X, F) = MN :-
    P = ( pred(X1::in, X2::in, C::out) is det :- C = F(X1, X2) ),
    array.bsearch(A, X, P, MN).

array.map(F, A1) = A2 :-
    P = ( pred(X::in, Y::out) is det :- Y = F(X) ),
    array.map(P, A1, A2).

array_compare(A1, A2) = C :-
    array_compare(C, A1, A2).

array.elem(Index, Array) = array.lookup(Array, Index).

'elem :='(Index, Array, Value) = array.set(Array, Index, Value).

% ---------------------------------------------------------------------------- %

    % array.sort/1 has type specialised versions for arrays of
    % ints and strings on the expectation that these constitute
    % the common case and are hence worth providing a fast-path.
    %
    % Experiments indicate that type specialisation improves
    % array.sort/1 by a factor of 30-40%.
    %
:- pragma type_spec(array.sort/1, T = int).
:- pragma type_spec(array.sort/1, T = string).

array.sort(A) = samsort_subarray(A, array.min(A), array.max(A)).

%------------------------------------------------------------------------------%

array.random_permutation(A0, A, RS0, RS) :-
    Lo = array.min(A0),
    Hi = array.max(A0),
    Sz = array.size(A0),
    permutation_2(Lo, Lo, Hi, Sz, A0, A, RS0, RS).

:- pred permutation_2(int, int, int, int, array(T), array(T),
        random.supply, random.supply).
:- mode permutation_2(in, in, in, in, array_di, array_uo, mdi, muo) is det.

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

:- func swap_elems(array(T), int, int) = array(T).
:- mode swap_elems(array_di, in, in) = array_uo is det.

swap_elems(A0, I, J) = A :-
    XI = A0 ^ elem(I),
    XJ = A0 ^ elem(J),
    A  = ((A0 ^ elem(I) := XJ) ^ elem(J) := XI).

% ---------------------------------------------------------------------------- %

array.foldl(Fn, A, X) =
    foldl_0(Fn, A, X, array.min(A), array.max(A)).

:- func foldl_0(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
:- mode foldl_0(func(in, in) = out is det, array_ui, in, in, in) = out is det.
:- mode foldl_0(func(in, in) = out is det, in, in, in, in) = out is det.
:- mode foldl_0(func(in, di) = uo is det, array_ui, di, in, in) = uo is det.
:- mode foldl_0(func(in, di) = uo is det, in, di, in, in) = uo is det.

foldl_0(Fn, A, X, I, Max) =
    ( Max < I ->
        X
    ;
        foldl_0(Fn, A, Fn(A ^ elem(I), X), I + 1, Max)
    ).

% ---------------------------------------------------------------------------- %

array.foldr(Fn, A, X) =
    foldr_0(Fn, A, X, array.min(A), array.max(A)).

:- func foldr_0(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
:- mode foldr_0(func(in, in) = out is det, array_ui, in, in, in) = out is det.
:- mode foldr_0(func(in, in) = out is det, in, in, in, in) = out is det.
:- mode foldr_0(func(in, di) = uo is det, array_ui, di, in, in) = uo is det.
:- mode foldr_0(func(in, di) = uo is det, in, di, in, in) = uo is det.

foldr_0(Fn, A, X, Min, I) =
    ( I < Min ->
        X
    ;
        foldr_0(Fn, A, Fn(A ^ elem(I), X), Min, I - 1)
    ).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

    % SAMsort (smooth applicative merge) invented by R.A. O'Keefe.
    %
    % SAMsort is a mergesort variant that works by identifying contiguous
    % monotonic sequences and merging them, thereby taking advantage of
    % any existing order in the input sequence.
    %

:- func samsort_subarray(array(T)::array_di, int::in, int::in) =
    (array(T)::array_uo) is det.

:- pragma type_spec(samsort_subarray/3, T = int).
:- pragma type_spec(samsort_subarray/3, T = string).

samsort_subarray(A0, Lo, Hi) = A :-
    samsort_up(0, A0, _, array.copy(A0), A, Lo, Hi, Lo).

:- pred samsort_up(int::in, array(T)::array_di, array(T)::array_uo,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::in) is det.

:- pragma type_spec(samsort_up/8, T = int).
:- pragma type_spec(samsort_up/8, T = string).

    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 is sorted from Lo .. I - 1.
    %   A0 and B0 are identical from I .. Hi.
    % Postcondition:
    %   B is sorted from Lo .. Hi.
    %
samsort_up(N, A0, A, B0, B, Lo, Hi, I) :-
    ( I > Hi ->
        A = A0,
        B = B0
    ; N > 0 ->
        samsort_down(N - 1, B0, B1, A0, A1, I, Hi, J),
        % A1 is sorted from I .. J - 1.
        % A1 and B1 are identical from J .. Hi.
        B2 = merge_subarrays(A1, B1, Lo, I - 1, I, J - 1, Lo),
        A2 = A1,
        % B2 is sorted from Lo .. J - 1.
        samsort_up(N + 1, B2, B, A2, A, Lo, Hi, J)
    ;
        % N = 0, I = Lo
        copy_run_ascending(A0, B0, B1, Lo, Hi, J),
        % B1 is sorted from Lo .. J - 1.
        samsort_up(N + 1, B1, B, A0, A, Lo, Hi, J)
    ).

:- pred samsort_down(int::in, array(T)::array_di, array(T)::array_uo,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::out) is det.

:- pragma type_spec(samsort_down/8, T = int).
:- pragma type_spec(samsort_down/8, T = string).

    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 and B0 are identical from Lo .. Hi.
    % Postcondition:
    %   B is sorted from Lo .. I - 1.
    %   A and B are identical from I .. Hi.
    %
samsort_down(N, A0, A, B0, B, Lo, Hi, I) :-
    ( Lo > Hi ->
        A = A0,
        B = B0,
        I = Lo
    ; N > 0 ->
        samsort_down(N - 1, B0, B1, A0, A1, Lo, Hi, J),
        samsort_down(N - 1, B1, B2, A1, A2, J,  Hi, I),
        % A2 is sorted from Lo .. J - 1.
        % A2 is sorted from J  .. I - 1.
        A = A2,
        B = merge_subarrays(A2, B2, Lo, J - 1, J, I - 1, Lo)
        % B is sorted from Lo .. I - 1.
    ;
        A = A0,
        copy_run_ascending(A0, B0, B, Lo, Hi, I)
        % B is sorted from Lo .. I - 1.
    ).

%------------------------------------------------------------------------------%

:- pred copy_run_ascending(array(T)::array_ui,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::out) is det.

:- pragma type_spec(copy_run_ascending/6, T = int).
:- pragma type_spec(copy_run_ascending/6, T = string).

copy_run_ascending(A, B0, B, Lo, Hi, I) :-
    (
        Lo < Hi,
        compare((>), A ^ elem(Lo), A ^ elem(Lo + 1))
    ->
        I = search_until((<), A, Lo, Hi),
        B = copy_subarray_reverse(A, B0, Lo, I - 1, I - 1)
    ;
        I = search_until((>), A, Lo, Hi),
        B = copy_subarray(A, B0, Lo, I - 1, Lo)
    ).

%------------------------------------------------------------------------------%

:- func search_until(comparison_result::in, array(T)::array_ui,
    int::in, int::in) = (int::out) is det.

:- pragma type_spec(search_until/4, T = int).
:- pragma type_spec(search_until/4, T = string).

search_until(R, A, Lo, Hi) =
    (
        Lo < Hi,
        not compare(R, A ^ elem(Lo), A ^ elem(Lo + 1))
    ->
        search_until(R, A, Lo + 1, Hi)
    ;
        Lo + 1
    ).

%------------------------------------------------------------------------------%

:- func copy_subarray(array(T)::array_ui, array(T)::array_di, int::in, int::in,
    int::in) = (array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray/5, T = int).
:- pragma type_spec(copy_subarray/5, T = string).

copy_subarray(A, B, Lo, Hi, I) =
    ( Lo =< Hi ->
        copy_subarray(A, B ^ elem(I) := A ^ elem(Lo), Lo + 1, Hi, I + 1)
    ;
        B
    ).

%------------------------------------------------------------------------------%

:- func copy_subarray_reverse(array(T)::array_ui, array(T)::array_di,
    int::in, int::in, int::in) = (array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray_reverse/5, T = int).
:- pragma type_spec(copy_subarray_reverse/5, T = string).

copy_subarray_reverse(A, B, Lo, Hi, I) =
    ( Lo =< Hi ->
        copy_subarray_reverse(A, B ^ elem(I) := A ^ elem(Lo),
            Lo + 1, Hi, I - 1)
    ;
        B
    ).

%------------------------------------------------------------------------------%

    % merges the two sorted consecutive subarrays Lo1 .. Hi1 and
    % Lo2 .. Hi2 from A into the subarray starting at I in B.
    %
:- func merge_subarrays(array(T)::array_ui, array(T)::array_di,
    int::in, int::in, int::in, int::in, int::in) = (array(T)::array_uo) is det.

:- pragma type_spec(merge_subarrays/7, T = int).
:- pragma type_spec(merge_subarrays/7, T = string).

merge_subarrays(A, B0, Lo1, Hi1, Lo2, Hi2, I) = B :-
    ( Lo1 > Hi1 ->
        B = copy_subarray(A, B0, Lo2, Hi2, I)
    ; Lo2 > Hi2 ->
        B = copy_subarray(A, B0, Lo1, Hi1, I)
    ;
        X1 = A ^ elem(Lo1),
        X2 = A ^ elem(Lo2),
        compare(R, X1, X2),
        (
            R = (<),
            B = merge_subarrays(A, B0 ^ elem(I) := X1,
                Lo1 + 1, Hi1, Lo2, Hi2, I + 1)
        ;
            R = (=),
            B = merge_subarrays(A, B0 ^ elem(I) := X1,
                Lo1 + 1, Hi1, Lo2, Hi2, I + 1)
        ;
            R = (>),
            B = merge_subarrays(A, B0 ^ elem(I) := X2,
                Lo1, Hi1, Lo2 + 1, Hi2, I + 1)
        )
    ).

%------------------------------------------------------------------------------%

    % throw an exception indicating an array bounds error
:- pred out_of_bounds_error(array(T), int, string).
:- mode out_of_bounds_error(array_ui, in, in) is erroneous.
:- mode out_of_bounds_error(in, in, in) is erroneous.

out_of_bounds_error(Array, Index, PredName) :-
    % Note: we deliberately do not include the array element type name in the
    % error message here, for performance reasons: using the type name could
    % prevent the compiler from optimizing away the construction of the
    % type_info in the caller, because it would prevent unused argument
    % elimination. Performance is important here, because array.set and
    % array.lookup are likely to be used in the inner loops of
    % performance-critical applications.
    array.bounds(Array, Min, Max),
    throw(array.index_out_of_bounds(
        string.format("%s: index %d not in range [%d, %d]",
            [s(PredName), i(Index), i(Min), i(Max)]))).

%-----------------------------------------------------------------------------%

array.least_index(A) = array.min(A).

array.greatest_index(A) = array.max(A).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
