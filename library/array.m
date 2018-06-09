%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: array.m.
% Main authors: fjh, bromage.
% Stability: medium-low.
%
% This module provides dynamically-sized one-dimensional arrays.
% Array indices start at zero.
%
% WARNING!
%
% Arrays are currently not unique objects. until this situation is resolved,
% it is up to the programmer to ensure that arrays are used in ways that
% preserve correctness. In the absence of mode reordering, one should therefore
% assume that evaluation will take place in left-to-right order. For example,
% the following code will probably not work as expected (f is a function,
% A an array, I an index, and X an appropriate value):
%
%       Y = f(A ^ elem(I) := X, A ^ elem(I))
%
% The compiler is likely to compile this as
%
%       V0 = A ^ elem(I) := X,
%       V1 = A ^ elem(I),
%       Y  = f(V0, V1)
%
% and will be unaware that the first line should be ordered *after* the second.
% The safest thing to do is write things out by hand in the form
%
%       A0I = A0 ^ elem(I),
%       A1  = A0 ^ elem(I) := X,
%       Y   = f(A1, A0I)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module array.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module random.

:- type array(T).

:- inst array(I) == ground.
:- inst array == array(ground).

    % XXX the current Mercury compiler doesn't support `ui' modes,
    % so to work-around that problem, we currently don't use
    % unique modes in this module.

% :- inst uniq_array(I) == unique.
% :- inst uniq_array == uniq_array(unique).
:- inst uniq_array(I) == array(I).          % XXX work-around
:- inst uniq_array == uniq_array(ground).   % XXX work-around

:- mode array_di == di(uniq_array).
:- mode array_uo == out(uniq_array).
:- mode array_ui == in(uniq_array).

% :- inst mostly_uniq_array(I) == mostly_unique).
% :- inst mostly_uniq_array == mostly_uniq_array(mostly_unique).
:- inst mostly_uniq_array(I) == array(I).    % XXX work-around
:- inst mostly_uniq_array == mostly_uniq_array(ground). % XXX work-around

:- mode array_mdi == mdi(mostly_uniq_array).
:- mode array_muo == out(mostly_uniq_array).
:- mode array_mui == in(mostly_uniq_array).

    % An `index_out_of_bounds' is the exception thrown
    % on out-of-bounds array accesses. The string describes
    % the predicate or function reporting the error.
:- type index_out_of_bounds
    --->    index_out_of_bounds(string).

%---------------------------------------------------------------------------%

    % make_empty_array(Array) creates an array of size zero
    % starting at lower bound 0.
    %
:- pred make_empty_array(array(T)::array_uo) is det.

:- func make_empty_array = (array(T)::array_uo) is det.

    % init(Size, Init, Array) creates an array with bounds from 0
    % to Size-1, with each element initialized to Init.
    %
:- pred init(int, T, array(T)).
:- mode init(in, in, array_uo) is det.

:- func init(int, T) = array(T).
:- mode init(in, in) = array_uo is det.

    % array/1 is a function that constructs an array from a list.
    % (It does the same thing as the predicate from_list/2.)
    % The syntax `array([...])' is used to represent arrays
    % for io.read, io.write, term_to_type, and type_to_term.
    %
:- func array(list(T)) = array(T).
:- mode array(in) = array_uo is det.

    % generate(Size, Generate) = Array:
    % Create an array with bounds from 0 to Size - 1 using the function
    % Generate to set the initial value of each element of the array.
    % The initial value of the element at index K will be the result of
    % calling the function Generate(K).
    %
:- func generate(int::in, (func(int) = T)::in) = (array(T)::array_uo)
    is det.

    % generate_foldl(Size, Generate, Array, !Acc):
    % As above, but using a predicate with an accumulator threaded through it
    % to generate the initial value of each element.
    %
:- pred generate_foldl(int, pred(int, T, A, A), array(T), A, A).
:- mode generate_foldl(in, in(pred(in, out, in, out) is det),
    array_uo, in, out) is det.
:- mode generate_foldl(in, in(pred(in, out, mdi, muo) is det),
    array_uo, mdi, muo) is det.
:- mode generate_foldl(in, in(pred(in, out, di, uo) is det),
    array_uo, di, uo) is det.
:- mode generate_foldl(in, in(pred(in, out, in, out) is semidet),
    array_uo, in, out) is semidet.
:- mode generate_foldl(in, in(pred(in, out, mdi, muo) is semidet),
    array_uo, mdi, muo) is semidet.
:- mode generate_foldl(in, in(pred(in, out, di, uo) is semidet),
    array_uo, di, uo) is semidet.

%---------------------------------------------------------------------------%

    % min returns the lower bound of the array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred min(array(_T), int).
%:- mode min(array_ui, out) is det.
:- mode min(in, out) is det.

:- func min(array(_T)) = int.
%:- mode min(array_ui) = out is det.
:- mode min(in) = out is det.

    % least_index returns the lower bound of the array.
    % In future, it will be changed to behave like semidet_least_index,
    % failing on an empty array instead of returning an out-of-bounds index.
    %
:- pragma obsolete(least_index/1).
:- func least_index(array(T)) = int.
%:- mode least_index(array_ui) = out is det.
:- mode least_index(in) = out is det.

    % det_least_index returns the lower bound of the array.
    % Throws an exception if the array is empty.
    %
:- func det_least_index(array(T)) = int.
%:- mode det_least_index(array_ui) = out is det.
:- mode det_least_index(in) = out is det.

    % semidet_least_index returns the lower bound of the array,
    % or fails if the array is empty.
    %
:- func semidet_least_index(array(T)) = int.
%:- mode semidet_least_index(array_ui) = out is semidet.
:- mode semidet_least_index(in) = out is semidet.

    % max returns the upper bound of the array.
    % Returns lower bound - 1 for an empty array
    % (always -1 in this implementation).
    %
:- pred max(array(_T), int).
%:- mode max(array_ui, out) is det.
:- mode max(in, out) is det.

:- func max(array(_T)) = int.
%:- mode max(array_ui) = out is det.
:- mode max(in) = out is det.

    % greatest_index returns the upper bound of the array.
    % In future, it will be changed to behave like semidet_greatest_index,
    % failing on an empty array instead of returning an out-of-bounds index.
    %
:- pragma obsolete(greatest_index/1).
:- func greatest_index(array(T)) = int.
%:- mode greatest_index(array_ui) = out is det.
:- mode greatest_index(in) = out is det.

    % det_greatest_index returns the upper bound of the array.
    % Throws an exception if the array is empty.
    %
:- func det_greatest_index(array(T)) = int.
%:- mode det_greatest_index(array_ui) = out is det.
:- mode det_greatest_index(in) = out is det.

    % semidet_greatest_index returns the upper bound of the array,
    % or fails if the array is empty.
    %
:- func semidet_greatest_index(array(T)) = int.
%:- mode semidet_greatest_index(array_ui) = out is semidet.
:- mode semidet_greatest_index(in) = out is semidet.

    % size returns the length of the array,
    % i.e. upper bound - lower bound + 1.
    %
:- pred size(array(_T), int).
%:- mode size(array_ui, out) is det.
:- mode size(in, out) is det.

:- func size(array(_T)) = int.
%:- mode size(array_ui) = out is det.
:- mode size(in) = out is det.

    % bounds(Array, Min, Max) returns the lower and upper bounds of an array.
    % The upper bound will be lower bound - 1 for an empty array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred bounds(array(_T), int, int).
%:- mode bounds(array_ui, out, out) is det.
:- mode bounds(in, out, out) is det.

    % in_bounds checks whether an index is in the bounds of an array.
    %
:- pred in_bounds(array(_T), int).
%:- mode in_bounds(array_ui, in) is semidet.
:- mode in_bounds(in, in) is semidet.

    % is_empty(Array):
    % True iff Array is an array of size zero.
    %
:- pred is_empty(array(_T)).
%:- mode is_empty(array_ui) is semidet.
:- mode is_empty(in) is semidet.

%---------------------------------------------------------------------------%

    % lookup returns the N'th element of an array.
    % Throws an exception if the index is out of bounds.
    %
:- pred lookup(array(T), int, T).
%:- mode lookup(array_ui, in, out) is det.
:- mode lookup(in, in, out) is det.

:- func lookup(array(T), int) = T.
%:- mode lookup(array_ui, in) = out is det.
:- mode lookup(in, in) = out is det.

    % semidet_lookup returns the N'th element of an array.
    % It fails if the index is out of bounds.
    %
:- pred semidet_lookup(array(T), int, T).
%:- mode semidet_lookup(array_ui, in, out) is semidet.
:- mode semidet_lookup(in, in, out) is semidet.

    % unsafe_lookup returns the N'th element of an array.
    % It is an error if the index is out of bounds.
    %
:- pred unsafe_lookup(array(T), int, T).
%:- mode unsafe_lookup(array_ui, in, out) is det.
:- mode unsafe_lookup(in, in, out) is det.

    % set sets the nth element of an array, and returns the
    % resulting array (good opportunity for destructive update ;-).
    % Throws an exception if the index is out of bounds.
    %
:- pred set(int, T, array(T), array(T)).
:- mode set(in, in, array_di, array_uo) is det.

:- func set(array(T), int, T) = array(T).
:- mode set(array_di, in, in) = array_uo is det.

    % semidet_set sets the nth element of an array, and returns
    % the resulting array. It fails if the index is out of bounds.
    %
:- pred semidet_set(int, T, array(T), array(T)).
:- mode semidet_set(in, in, array_di, array_uo) is semidet.

    % unsafe_set sets the nth element of an array, and returns the
    % resulting array. It is an error if the index is out of bounds.
    %
:- pred unsafe_set(int, T, array(T), array(T)).
:- mode unsafe_set(in, in, array_di, array_uo) is det.

    % slow_set sets the nth element of an array, and returns the
    % resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It is an error if the index is out of bounds.
    %
:- pred slow_set(int, T, array(T), array(T)).
%:- mode slow_set(in, in, array_ui, array_uo) is det.
:- mode slow_set(in, in, in, array_uo) is det.

:- func slow_set(array(T), int, T) = array(T).
%:- mode slow_set(array_ui, in, in) = array_uo is det.
:- mode slow_set(in, in, in) = array_uo is det.

    % semidet_slow_set sets the nth element of an array, and returns
    % the resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It fails if the index is out of bounds.
    %
:- pred semidet_slow_set(int, T, array(T), array(T)).
%:- mode semidet_slow_set(in, in, array_ui, array_uo) is semidet.
:- mode semidet_slow_set(in, in, in, array_uo) is semidet.

    % Field selection for arrays.
    % Array ^ elem(Index) = lookup(Array, Index).
    %
:- func elem(int, array(T)) = T.
%:- mode elem(in, array_ui) = out is det.
:- mode elem(in, in) = out is det.

    % As above, but omit the bounds check.
    %
:- func unsafe_elem(int, array(T)) = T.
%:- mode unsafe_elem(in, array_ui) = out is det.
:- mode unsafe_elem(in, in) = out is det.

    % Field update for arrays.
    % (Array ^ elem(Index) := Value) = set(Array, Index, Value).
    %
:- func 'elem :='(int, array(T), T) = array(T).
:- mode 'elem :='(in, array_di, in) = array_uo is det.

    % As above, but omit the bounds check.
    %
:- func 'unsafe_elem :='(int, array(T), T) = array(T).
:- mode 'unsafe_elem :='(in, array_di, in) = array_uo is det.

    % Returns every element of the array, one by one.
    %
:- pred member(array(T)::in, T::out) is nondet.

%---------------------------------------------------------------------------%

    % copy(Array0, Array):
    % Makes a new unique copy of an array.
    %
:- pred copy(array(T), array(T)).
%:- mode copy(array_ui, array_uo) is det.
:- mode copy(in, array_uo) is det.

:- func copy(array(T)) = array(T).
%:- mode copy(array_ui) = array_uo is det.
:- mode copy(in) = array_uo is det.

    % resize(Size, Init, Array0, Array):
    % The array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- pred resize(int, T, array(T), array(T)).
:- mode resize(in, in, array_di, array_uo) is det.

    % resize(Array0, Size, Init) = Array:
    % The array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- func resize(array(T), int, T) = array(T).
:- mode resize(array_di, in, in) = array_uo is det.

    % shrink(Size, Array0, Array):
    % The array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- pred shrink(int, array(T), array(T)).
:- mode shrink(in, array_di, array_uo) is det.

    % shrink(Array0, Size) = Array:
    % The array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- func shrink(array(T), int) = array(T).
:- mode shrink(array_di, in) = array_uo is det.

    % from_list takes a list, and returns an array containing those
    % elements in the same order that they occurred in the list.
    %
:- func from_list(list(T)::in) = (array(T)::array_uo) is det.
:- pred from_list(list(T)::in, array(T)::array_uo) is det.

    % from_reverse_list takes a list, and returns an array containing
    % those elements in the reverse order that they occurred in the list.
    %
:- func from_reverse_list(list(T)::in) = (array(T)::array_uo) is det.

    % to_list takes an array and returns a list containing the elements
    % of the array in the same order that they occurred in the array.
    %
:- pred to_list(array(T), list(T)).
%:- mode to_list(array_ui, out) is det.
:- mode to_list(in, out) is det.

:- func to_list(array(T)) = list(T).
%:- mode to_list(array_ui) = out is det.
:- mode to_list(in) = out is det.

    % fetch_items takes an array and a lower and upper index,
    % and places those items in the array between these indices into a list.
    % It is an error if either index is out of bounds.
    %
:- pred fetch_items(array(T), int, int, list(T)).
:- mode fetch_items(in, in, in, out) is det.

:- func fetch_items(array(T), int, int) = list(T).
%:- mode fetch_items(array_ui, in, in) = out is det.
:- mode fetch_items(in, in, in) = out is det.

    % XXX We prefer users to call the new binary_search predicate
    % instead of bsearch, which is why bsearch is marked as obsolete.
    %
    % bsearch takes an array, an element to be matched and a comparison
    % predicate and returns the position of the first occurrence in the array
    % of an element which is equivalent to the given one in the ordering
    % provided. Assumes the array is sorted according to this ordering.
    %
:- pred bsearch(array(T), T, comparison_pred(T), maybe(int)).
%:- mode bsearch(array_ui, in, in(comparison_pred), out) is det.
:- mode bsearch(in, in, in(comparison_pred), out) is det.
:- pragma obsolete(bsearch/4).

:- func bsearch(array(T), T, comparison_func(T)) = maybe(int).
%:- mode bsearch(array_ui, in, in(comparison_func)) = out is det.
:- mode bsearch(in, in, in(comparison_func)) = out is det.
:- pragma obsolete(bsearch/3).

    % binary_search(A, X, I) does a binary search for the element X
    % in the array A. If there is an element with that value in the array,
    % it returns its index I; otherwise, it fails.
    %
    % The array A must be sorted into ascending order with respect to the
    % the builtin Mercury order on terms for binary_search/3, and with respect
    % to supplied comparison predicate for binary_search/4.
    %
    % The array may contain duplicates. If it does, and a search looks for
    % a duplicated value, the search will return the index of one of the
    % copies, but it is not specified *which* copy's index it will return.
    %
:- pred binary_search(array(T)::array_ui,
    T::in, int::out) is semidet.
:- pred binary_search(comparison_func(T)::in, array(T)::array_ui,
    T::in, int::out) is semidet.

    % approx_binary_search(A, X, I) does a binary search for the element X
    % in the array A. If there is an element with that value in the array,
    % it returns its index I. If there is no element with that value in the
    % array, it returns an index whose slot contains the highest value in the
    % array that is less than X, as measured by the builtin Mercury order
    % on terms for approx_binary_search/3, and as measured by the supplied
    % ordering for approx_binary_search/4. It will fail only if there is
    % no value smaller than X in the array.
    %
    % The array A must be sorted into ascending order with respect to the
    % the builtin Mercury order on terms for approx_binary_search/3, and
    % with respect to supplied comparison predicate for approx_binary_search/4.
    %
    % The array may contain duplicates. If it does, and if either the
    % searched-for value or (if that does not exist) the highest value
    % smaller than the searched-for value is duplicated, the search will return
    % the index of one of the copies, but it is not specified *which* copy's
    % index it will return.
    %
:- pred approx_binary_search(array(T)::array_ui,
    T::in, int::out) is semidet.
:- pred approx_binary_search(comparison_func(T)::in, array(T)::array_ui,
    T::in, int::out) is semidet.

    % map(Closure, OldArray, NewArray) applies `Closure' to
    % each of the elements of `OldArray' to create `NewArray'.
    %
:- pred map(pred(T1, T2), array(T1), array(T2)).
:- mode map(pred(in, out) is det, array_di, array_uo) is det.

:- func map(func(T1) = T2, array(T1)) = array(T2).
:- mode map(func(in) = out is det, array_di) = array_uo is det.

:- func array_compare(array(T), array(T)) = comparison_result.
:- mode array_compare(in, in) = uo is det.

    % sort(Array) returns a version of Array sorted into ascending
    % order.
    %
    % This sort is not stable. That is, elements that compare/3 decides are
    % equal will appear together in the sorted array, but not necessarily
    % in the same order in which they occurred in the input array. This is
    % primarily only an issue with types with user-defined equivalence for
    % which `equivalent' objects are otherwise distinguishable.
    %
:- func sort(array(T)) = array(T).
:- mode sort(array_di) = array_uo is det.

    % array.sort was previously buggy. This symbol provides a way to ensure
    % that you are using the fixed version.
    %
:- pred array.sort_fix_2014 is det.

    % foldl(Fn, Array, X) is equivalent to
    %   list.foldl(Fn, to_list(Array), X)
    % but more efficient.
    %
:- func foldl(func(T1, T2) = T2, array(T1), T2) = T2.
%:- mode foldl(func(in, in) = out is det, array_ui, in) = out is det.
:- mode foldl(func(in, in) = out is det, in, in) = out is det.
%:- mode foldl(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode foldl(func(in, di) = uo is det, in, di) = uo is det.

    % foldl(Pr, Array, !X) is equivalent to
    %   list.foldl(Pr, to_list(Array), !X)
    % but more efficient.
    %
:- pred foldl(pred(T1, T2, T2), array(T1), T2, T2).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % foldl2(Pr, Array, !X, !Y) is equivalent to
    %   list.foldl2(Pr, to_list(Array), !X, !Y)
    % but more efficient.
    %
:- pred foldl2(pred(T1, T2, T2, T3, T3), array(T1), T2, T2, T3, T3).
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % As above, but with three accumulators.
    %
:- pred foldl3(pred(T1, T2, T2, T3, T3, T4, T4), array(T1),
    T2, T2, T3, T3, T4, T4).
:- mode foldl3(pred(in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

    % As above, but with four accumulators.
    %
:- pred foldl4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), array(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred foldl5(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    array(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % foldr(Fn, Array, X) is equivalent to
    %   list.foldr(Fn, to_list(Array), X)
    % but more efficient.
    %
:- func foldr(func(T1, T2) = T2, array(T1), T2) = T2.
%:- mode foldr(func(in, in) = out is det, array_ui, in) = out is det.
:- mode foldr(func(in, in) = out is det, in, in) = out is det.
%:- mode foldr(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode foldr(func(in, di) = uo is det, in, di) = uo is det.

    % foldr(P, Array, !Acc) is equivalent to
    %   list.foldr(P, to_list(Array), !Acc)
    % but more efficient.
    %
:- pred foldr(pred(T1, T2, T2), array(T1), T2, T2).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred foldr2(pred(T1, T2, T2, T3, T3), array(T1), T2, T2, T3, T3).
:- mode foldr2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode foldr2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldr2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode foldr2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldr2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldr2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % As above, but with three accumulators.
    %
:- pred foldr3(pred(T1, T2, T2, T3, T3, T4, T4), array(T1),
    T2, T2, T3, T3, T4, T4).
:- mode foldr3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode foldr3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldr3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode foldr3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode foldr3(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

    % As above, but with four accumulators.
    %
:- pred foldr4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), array(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode foldr4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldr4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldr4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred foldr5(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    array(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % map_foldl(P, A, B, !Acc):
    % Invoke P(Aelt, Belt, !Acc) on each element of the A array,
    % and construct array B from the resulting values of Belt.
    %
:- pred map_foldl(pred(T1, T2, T3, T3), array(T1), array(T2), T3, T3).
:- mode map_foldl(in(pred(in, out, in, out) is det),
    in, array_uo, in, out) is det.
:- mode map_foldl(in(pred(in, out, mdi, muo) is det),
    in, array_uo, mdi, muo) is det.
:- mode map_foldl(in(pred(in, out, di, uo) is det),
    in, array_uo, di, uo) is det.
:- mode map_foldl(in(pred(in, out, in, out) is semidet),
    in, array_uo, in, out) is semidet.

    % map_corresponding_foldl(P, A, B, C, !Acc):
    %
    % Given two arrays A and B, invoke P(Aelt, Belt, Celt, !Acc) on
    % each corresponding pair of elements Aelt and Belt. Build up the array C
    % from the result Celt values. Return C and the final value of the
    % accumulator.
    %
    % C will have as many elements as A does. In most uses, B will also have
    % this many elements, but may have more; it may NOT have fewer.
    %
:- pred map_corresponding_foldl(pred(T1, T2, T3, T4, T4),
    array(T1), array(T2), array(T3), T4, T4).
:- mode map_corresponding_foldl(
    in(pred(in, in, out, in, out) is det),
    in, in, array_uo, in, out) is det.
:- mode map_corresponding_foldl(
    in(pred(in, in, out, mdi, muo) is det),
    in, in, array_uo, mdi, muo) is det.
:- mode map_corresponding_foldl(
    in(pred(in, in, out, di, uo) is det),
    in, in, array_uo, di, uo) is det.
:- mode map_corresponding_foldl(
    in(pred(in, in, out, in, out) is semidet),
    in, in, array_uo, in, out) is semidet.

    % all_true(Pred, Array):
    % True iff Pred is true for every element of Array.
    %
:- pred all_true(pred(T), array(T)).
%:- mode all_true(in(pred(in) is semidet), array_ui) is semidet.
:- mode all_true(in(pred(in) is semidet), in) is semidet.

    % all_false(Pred, Array):
    % True iff Pred is false for every element of Array.
    %
:- pred all_false(pred(T), array(T)).
%:- mode all_false(in(pred(in) is semidet), array_ui) is semidet.
:- mode all_false(in(pred(in) is semidet), in) is semidet.

    % append(A, B) = C:
    %
    % Make C a concatenation of the arrays A and B.
    %
:- func append(array(T)::in, array(T)::in) = (array(T)::array_uo) is det.

    % random_permutation(A0, A, RS0, RS) permutes the elements in
    % A0 given random seed RS0 and returns the permuted array in A
    % and the next random seed in RS.
    %
:- pred random_permutation(array(T)::array_di, array(T)::array_uo,
    random.supply::mdi, random.supply::muo) is det.

    % Convert an array to a pretty_printer.doc for formatting.
    %
:- func array_to_doc(array(T)) = pretty_printer.doc.
:- mode array_to_doc(array_ui) = out is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything beyond here is not intended as part of the public interface,
% and will not appear in the Mercury Library Reference Manual.

:- interface.

    % dynamic_cast/2 won't work for arbitrary arrays since array/1 is
    % not a ground type (that is, dynamic_cast/2 will work when the
    % target type is e.g. array(int), but not when it is array(T)).
    %
:- some [T2] pred dynamic_cast_to_array(T1::in, array(T2)::out) is semidet.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module type_desc.

%
% Define the array type appropriately for the different targets.
% Note that the definitions here should match what is output by
% mlds_to_c.m, mlds_to_il.m, or mlds_to_java.m for mlds.mercury_array_type.
%

    % MR_ArrayPtr is defined in runtime/mercury_types.h.
:- pragma foreign_type("C", array(T), "MR_ArrayPtr")
    where equality is array.array_equal,
    comparison is array.array_compare.

:- pragma foreign_type("C#",  array(T), "System.Array")
    where equality is array.array_equal,
    comparison is array.array_compare.

    % We can't use `java.lang.Object []', since we want a generic type
    % that is capable of holding any kind of array, including e.g. `int []'.
    % Java doesn't have any equivalent of .NET's System.Array class,
    % so we just use the universal base `java.lang.Object'.
:- pragma foreign_type("Java",  array(T), "/* Array */ java.lang.Object")
    where equality is array.array_equal,
    comparison is array.array_compare.

:- pragma foreign_type("Erlang", array(T), "")
    where equality is array.array_equal,
    comparison is array.array_compare.

    % unify/2 for arrays
    %
:- pred array_equal(array(T)::in, array(T)::in) is semidet.
:- pragma terminates(array_equal/2).

array_equal(Array1, Array2) :-
    ( if
        array.size(Array1, Size),
        array.size(Array2, Size)
    then
        array.equal_elements(0, Size, Array1, Array2)
    else
        fail
    ).

:- pred array.equal_elements(int, int, array(T), array(T)).
:- mode array.equal_elements(in, in, in, in) is semidet.

equal_elements(N, Size, Array1, Array2) :-
    ( if N = Size then
        true
    else
        array.lookup(Array1, N, Elem),
        array.lookup(Array2, N, Elem),
        N1 = N + 1,
        array.equal_elements(N1, Size, Array1, Array2)
    ).

array_compare(A1, A2) = C :-
    array_compare(C, A1, A2).

    % compare/3 for arrays
    %
:- pred array_compare(comparison_result::uo, array(T)::in, array(T)::in)
    is det.
:- pragma terminates(array_compare/3).

array_compare(Result, Array1, Array2) :-
    array.size(Array1, Size1),
    array.size(Array2, Size2),
    compare(SizeResult, Size1, Size2),
    (
        SizeResult = (=),
        array.compare_elements(0, Size1, Array1, Array2, Result)
    ;
        ( SizeResult = (<)
        ; SizeResult = (>)
        ),
        Result = SizeResult
    ).

:- pred compare_elements(int::in, int::in, array(T)::in, array(T)::in,
    comparison_result::uo) is det.

compare_elements(N, Size, Array1, Array2, Result) :-
    ( if N = Size then
        Result = (=)
    else
        array.lookup(Array1, N, Elem1),
        array.lookup(Array2, N, Elem2),
        compare(ElemResult, Elem1, Elem2),
        (
            ElemResult = (=),
            N1 = N + 1,
            array.compare_elements(N1, Size, Array1, Array2, Result)
        ;
            ( ElemResult = (<)
            ; ElemResult = (>)
            ),
            Result = ElemResult
        )
    ).

%---------------------------------------------------------------------------%

:- pred bounds_checks is semidet.
:- pragma inline(bounds_checks/0).

:- pragma foreign_proc("C",
    bounds_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
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

:- pragma foreign_proc("Erlang",
    bounds_checks,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""             /* for MR_maybe_record_allocation() */
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

#define ML_alloc_array(newarray, arraysize, alloc_id)                   \
    do {                                                                \
        MR_Word newarray_word;                                          \
        MR_offset_incr_hp_msg(newarray_word, 0, (arraysize),            \
            alloc_id, ""array.array/1"");                               \
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

:- pragma foreign_code("C#", "

public static System.Array
ML_new_array(int Size, object Item)
{
    System.Array arr;
    if (Size == 0) {
        return null;
    }
    if (Item is int || Item is double || Item is char || Item is bool) {
        arr = System.Array.CreateInstance(Item.GetType(), Size);
    } else {
        arr = new object[Size];
    }
    for (int i = 0; i < Size; i++) {
        arr.SetValue(Item, i);
    }
    return arr;
}

public static System.Array
ML_unsafe_new_array(int Size, object Item, int IndexToSet)
{
    System.Array arr;

    if (Item is int || Item is double || Item is char || Item is bool) {
        arr = System.Array.CreateInstance(Item.GetType(), Size);
    } else {
        arr = new object[Size];
    }
    arr.SetValue(Item, IndexToSet);
    return arr;
}

public static System.Array
ML_array_resize(System.Array arr0, int Size, object Item)
{
    if (Size == 0) {
        return null;
    }
    if (arr0 == null) {
        return ML_new_array(Size, Item);
    }
    if (arr0.Length == Size) {
        return arr0;
    }

    int OldSize = arr0.Length;
    System.Array arr;
    if (Item is int) {
        int[] tmp = (int[]) arr0;
        System.Array.Resize(ref tmp, Size);
        arr = tmp;
    } else if (Item is double) {
        double[] tmp = (double[]) arr0;
        System.Array.Resize(ref tmp, Size);
        arr = tmp;
    } else if (Item is char) {
        char[] tmp = (char[]) arr0;
        System.Array.Resize(ref tmp, Size);
        arr = tmp;
    } else if (Item is bool) {
        bool[] tmp = (bool[]) arr0;
        System.Array.Resize(ref tmp, Size);
        arr = tmp;
    } else {
        object[] tmp = (object[]) arr0;
        System.Array.Resize(ref tmp, Size);
        arr = tmp;
    }
    for (int i = OldSize; i < Size; i++) {
        arr.SetValue(Item, i);
    }
    return arr;
}

public static System.Array
ML_shrink_array(System.Array arr, int Size)
{
    if (arr == null) {
        return null;
    } else if (arr is int[]) {
        int[] tmp = (int[]) arr;
        System.Array.Resize(ref tmp, Size);
        return tmp;
    } else if (arr is double[]) {
        double[] tmp = (double[]) arr;
        System.Array.Resize(ref tmp, Size);
        return tmp;
    } else if (arr is char[]) {
        char[] tmp = (char[]) arr;
        System.Array.Resize(ref tmp, Size);
        return tmp;
    } else if (arr is bool[]) {
        bool[] tmp = (bool[]) arr;
        System.Array.Resize(ref tmp, Size);
        return tmp;
    } else {
        object[] tmp = (object[]) arr;
        System.Array.Resize(ref tmp, Size);
        return tmp;
    }
}

").

:- pragma foreign_code("Java", "
public static Object
ML_new_array(int Size, Object Item, boolean fill)
{
    if (Size == 0) {
        return null;
    }
    if (Item instanceof Integer) {
        int[] as = new int[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Integer) Item);
        }
        return as;
    }
    if (Item instanceof Double) {
        double[] as = new double[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Double) Item);
        }
        return as;
    }
    if (Item instanceof Character) {
        char[] as = new char[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Character) Item);
        }
        return as;
    }
    if (Item instanceof Boolean) {
        boolean[] as = new boolean[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Boolean) Item);
        }
        return as;
    }
    if (Item instanceof Byte) {
        byte[] as = new byte[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Byte) Item);
        }
        return as;
    }
    if (Item instanceof Short) {
        short[] as = new short[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Short) Item);
        }
        return as;
    }
    if (Item instanceof Long) {
        long[] as =  new long[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Long) Item);
        }
        return as;
    }
    if (Item instanceof Float) {
        float[] as = new float[Size];
        if (fill) {
            java.util.Arrays.fill(as, (Float) Item);
        }
        return as;
    }
    Object[] as = new Object[Size];
    if (fill) {
        java.util.Arrays.fill(as, Item);
    }
    return as;
}

public static Object
ML_unsafe_new_array(int Size, Object Item, int IndexToSet)
{
    if (Item instanceof Integer) {
        int[] as = new int[Size];
        as[IndexToSet] = (Integer) Item;
        return as;
    }
    if (Item instanceof Double) {
        double[] as = new double[Size];
        as[IndexToSet] = (Double) Item;
        return as;
    }
    if (Item instanceof Character) {
        char[] as = new char[Size];
        as[IndexToSet] = (Character) Item;
        return as;
    }
    if (Item instanceof Boolean) {
        boolean[] as = new boolean[Size];
        as[IndexToSet] = (Boolean) Item;
        return as;
    }
    if (Item instanceof Byte) {
        byte[] as = new byte[Size];
        as[IndexToSet] = (Byte) Item;
        return as;
    }
    if (Item instanceof Short) {
        short[] as = new short[Size];
        as[IndexToSet] = (Short) Item;
        return as;
    }
    if (Item instanceof Long) {
        long[] as = new long[Size];
        as[IndexToSet] = (Long) Item;
        return as;
    }
    if (Item instanceof Float) {
        float[] as = new float[Size];
        as[IndexToSet] = (Float) Item;
        return as;
    }
    Object[] as = new Object[Size];
    as[IndexToSet] = Item;
    return as;
}

public static int
ML_array_size(Object Array)
{
    if (Array == null) {
        return 0;
    } else if (Array instanceof int[]) {
        return ((int[]) Array).length;
    } else if (Array instanceof double[]) {
        return ((double[]) Array).length;
    } else if (Array instanceof char[]) {
        return ((char[]) Array).length;
    } else if (Array instanceof boolean[]) {
        return ((boolean[]) Array).length;
    } else if (Array instanceof byte[]) {
        return ((byte[]) Array).length;
    } else if (Array instanceof short[]) {
        return ((short[]) Array).length;
    } else if (Array instanceof long[]) {
        return ((long[]) Array).length;
    } else if (Array instanceof float[]) {
        return ((float[]) Array).length;
    } else {
        return ((Object[]) Array).length;
    }
}

public static Object
ML_array_resize(Object Array0, int Size, Object Item)
{
    if (Size == 0) {
        return null;
    }
    if (Array0 == null) {
        return ML_new_array(Size, Item, true);
    }
    if (ML_array_size(Array0) == Size) {
        return Array0;
    }
    if (Array0 instanceof int[]) {
        int[] arr0 = (int[]) Array0;
        int[] Array = new int[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Integer) Item;
        }
        return Array;
    }
    if (Array0 instanceof double[]) {
        double[] arr0 = (double[]) Array0;
        double[] Array = new double[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Double) Item;
        }
        return Array;
    }
    if (Array0 instanceof char[]) {
        char[] arr0 = (char[]) Array0;
        char[] Array = new char[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Character) Item;
        }
        return Array;
    }
    if (Array0 instanceof boolean[]) {
        boolean[] arr0 = (boolean[]) Array0;
        boolean[] Array = new boolean[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Boolean) Item;
        }
        return Array;
    }
    if (Array0 instanceof byte[]) {
        byte[] arr0 = (byte[]) Array0;
        byte[] Array = new byte[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Byte) Item;
        }
        return Array;
    }
    if (Array0 instanceof short[]) {
        short[] arr0 = (short[]) Array0;
        short[] Array = new short[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Short) Item;
        }
        return Array;
    }
    if (Array0 instanceof long[]) {
        long[] arr0 = (long[]) Array0;
        long[] Array = new long[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Long) Item;
        }
        return Array;
    }
    if (Array0 instanceof float[]) {
        float[] arr0 = (float[]) Array0;
        float[] Array = new float[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = (Float) Item;
        }
        return Array;
    } else {
        Object[] arr0 = (Object[]) Array0;
        Object[] Array = new Object[Size];

        System.arraycopy(arr0, 0, Array, 0, Math.min(arr0.length, Size));
        for (int i = arr0.length; i < Size; i++) {
            Array[i] = Item;
        }
        return Array;
    }
}
").

init(N, X) = A :-
    array.init(N, X, A).

init(Size, Item, Array) :-
    ( if Size < 0 then
        error("array.init: negative size")
    else
        array.init_2(Size, Item, Array)
    ).

:- pred init_2(int::in, T::in, array(T)::array_uo) is det.

:- pragma foreign_proc("C",
    init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, T, array(T)), [
            cel(Item, []) - cel(Array, [T])
        ])
    ],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
    ML_init_array(Array, Size, Item);
").

make_empty_array = A :-
    array.make_empty_array(A).

:- pragma foreign_proc("C",
    make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ML_alloc_array(Array, 1, MR_ALLOC_ID);
    ML_init_array(Array, 0, 0);
").

:- pragma foreign_proc("C#",
    init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_new_array(Size, Item);
").
:- pragma foreign_proc("C#",
    make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX A better solution than using the null pointer to represent
    // the empty array would be to create an array of size 0. However,
    // we need to determine the element type of the array before we can
    // do that. This could be done by examining the RTTI of the array
    // type and then using System.Type.GetType(""<mercury type>"") to
    // determine it. However constructing the <mercury type> string is
    // a non-trivial amount of work.
    Array = null;
").

:- pragma foreign_proc("Erlang",
    init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = erlang:make_tuple(Size, Item)
").
:- pragma foreign_proc("Erlang",
    make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = {}
").

:- pragma foreign_proc("Java",
    init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_new_array(Size, Item, true);
").
:- pragma foreign_proc("Java",
    make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX as per C#
    Array = null;
").

%---------------------------------------------------------------------------%

generate(Size, GenFunc) = Array :-
    compare(Result, Size, 0),
    (
        Result = (<),
        error("array.generate: negative size")
    ;
        Result = (=),
        make_empty_array(Array)
    ;
        Result = (>),
        FirstElem = GenFunc(0),
        Array0 = unsafe_init(Size, FirstElem, 0),
        Array = generate_2(1, Size, GenFunc, Array0)
    ).

:- func unsafe_init(int::in, T::in, int::in) = (array(T)::array_uo) is det.
:- pragma foreign_proc("C",
    unsafe_init(Size::in, FirstElem::in, IndexToSet::in) = (Array::array_uo),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);

    /*
    ** In debugging grades we fill the array with the first element
    ** in case the return value of a call to this predicate is examined
    ** in the debugger.
    */
    #if defined(MR_EXEC_TRACE)
        ML_init_array(Array, Size, FirstElem);
    #else
        Array->size = Size;
        Array->elements[IndexToSet] = FirstElem;
    #endif

").
:- pragma foreign_proc("C#",
    unsafe_init(Size::in, FirstElem::in, IndexToSet::in) = (Array::array_uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Array = array.ML_unsafe_new_array(Size, FirstElem, IndexToSet);
").
:- pragma foreign_proc("Java",
    unsafe_init(Size::in, FirstElem::in, IndexToSet::in) = (Array::array_uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Array = array.ML_unsafe_new_array(Size, FirstElem, IndexToSet);
").
:- pragma foreign_proc("Erlang",
    unsafe_init(Size::in, FirstElem::in, _IndexToSet::in) = (Array::array_uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Array = erlang:make_tuple(Size, FirstElem)
").

:- func generate_2(int::in, int::in, (func(int) = T)::in, array(T)::array_di)
    = (array(T)::array_uo) is det.

generate_2(Index, Size, GenFunc, !.Array) = !:Array :-
    ( if Index < Size then
        Elem = GenFunc(Index),
        array.unsafe_set(Index, Elem, !Array),
        !:Array = generate_2(Index + 1, Size, GenFunc, !.Array)
    else
        true
    ).

generate_foldl(Size, GenPred, Array, !Acc) :-
    compare(Result, Size, 0),
    (
        Result = (<),
        error("array.generate_foldl: negative size")
    ;
        Result = (=),
        make_empty_array(Array)
    ;
        Result = (>),
        GenPred(0, FirstElem, !Acc),
        Array0 = unsafe_init(Size, FirstElem, 0),
        generate_foldl_2(1, Size, GenPred, Array0, Array, !Acc)
    ).

:- pred generate_foldl_2(int, int, pred(int, T, A, A),
    array(T), array(T), A, A).
:- mode generate_foldl_2(in, in, in(pred(in, out, in, out) is det),
    array_di, array_uo, in, out) is det.
:- mode generate_foldl_2(in, in, in(pred(in, out, mdi, muo) is det),
    array_di, array_uo, mdi, muo) is det.
:- mode generate_foldl_2(in, in, in(pred(in, out, di, uo) is det),
    array_di, array_uo, di, uo) is det.
:- mode generate_foldl_2(in, in, in(pred(in, out, in, out) is semidet),
    array_di, array_uo, in, out) is semidet.
:- mode generate_foldl_2(in, in, in(pred(in, out, mdi, muo) is semidet),
    array_di, array_uo, mdi, muo) is semidet.
:- mode generate_foldl_2(in, in, in(pred(in, out, di, uo) is semidet),
    array_di, array_uo, di, uo) is semidet.

generate_foldl_2(Index, Size, GenPred, !Array, !Acc) :-
    ( if Index < Size then
        GenPred(Index, Elem, !Acc),
        array.unsafe_set(Index, Elem, !Array),
        generate_foldl_2(Index + 1, Size, GenPred, !Array, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

min(A) = N :-
    array.min(A, N).

:- pragma foreign_proc("C",
    min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("C#",
    min(_Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("Erlang",
    min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % Array not used
    Min = 0
").

:- pragma foreign_proc("Java",
    min(_Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

max(A) = N :-
    array.max(A, N).

:- pragma foreign_proc("C",
    max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Max = Array->size - 1;
").
:- pragma foreign_proc("C#",
    max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length - 1;
    } else {
        Max = -1;
    }
").
:- pragma foreign_proc("Erlang",
    max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = size(Array) - 1
").

:- pragma foreign_proc("Java",
    max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = array.ML_array_size(Array) - 1;
    } else {
        Max = -1;
    }
").

bounds(Array, Min, Max) :-
    array.min(Array, Min),
    array.max(Array, Max).

%---------------------------------------------------------------------------%

size(A) = N :-
    array.size(A, N).

:- pragma foreign_proc("C",
    size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Max = Array->size;
").

:- pragma foreign_proc("C#",
    size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = Array.Length;
    } else {
        Max = 0;
    }
").

:- pragma foreign_proc("Erlang",
    size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = size(Array)
").

:- pragma foreign_proc("Java",
    size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = jmercury.array.ML_array_size(Array);
").

%---------------------------------------------------------------------------%

in_bounds(Array, Index) :-
    array.bounds(Array, Min, Max),
    Min =< Index, Index =< Max.

is_empty(Array) :-
    array.size(Array, 0).

semidet_set(Index, Item, !Array) :-
    ( if array.in_bounds(!.Array, Index) then
        array.unsafe_set(Index, Item, !Array)
    else
        fail
    ).

semidet_slow_set(Index, Item, !Array) :-
    ( if array.in_bounds(!.Array, Index) then
        array.slow_set(Index, Item, !Array)
    else
        fail
    ).

slow_set(!.Array, N, X) = !:Array :-
    array.slow_set(N, X, !Array).

slow_set(Index, Item, !Array) :-
    array.copy(!Array),
    array.set(Index, Item, !Array).

%---------------------------------------------------------------------------%

elem(Index, Array) = array.lookup(Array, Index).

unsafe_elem(Index, Array) = Elem :-
    array.unsafe_lookup(Array, Index, Elem).

lookup(Array, N) = X :-
    array.lookup(Array, N, X).

lookup(Array, Index, Item) :-
    ( if
        bounds_checks,
        not array.in_bounds(Array, Index)
    then
        out_of_bounds_error(Array, Index, "array.lookup")
    else
        array.unsafe_lookup(Array, Index, Item)
    ).

semidet_lookup(Array, Index, Item) :-
    ( if array.in_bounds(Array, Index) then
        array.unsafe_lookup(Array, Index, Item)
    else
        fail
    ).

:- pragma foreign_proc("C",
    unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(array(T), int, T), [
            cel(Array, [T]) - cel(Item, [])
        ])
    ],
"
    Item = Array->elements[Index];
").

:- pragma foreign_proc("C#",
    unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Item = Array.GetValue(Index);
}").

:- pragma foreign_proc("Erlang",
    unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = element(Index + 1, Array)
").

:- pragma foreign_proc("Java",
    unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array instanceof int[]) {
        Item = ((int[]) Array)[Index];
    } else if (Array instanceof double[]) {
        Item = ((double[]) Array)[Index];
    } else if (Array instanceof char[]) {
        Item = ((char[]) Array)[Index];
    } else if (Array instanceof boolean[]) {
        Item = ((boolean[]) Array)[Index];
    } else if (Array instanceof byte[]) {
        Item = ((byte[]) Array)[Index];
    } else if (Array instanceof short[]) {
        Item = ((short[]) Array)[Index];
    } else if (Array instanceof long[]) {
        Item = ((long[]) Array)[Index];
    } else if (Array instanceof float[]) {
        Item = ((float[]) Array)[Index];
    } else {
        Item = ((Object[]) Array)[Index];
    }
").

%---------------------------------------------------------------------------%

'elem :='(Index, Array, Value) = array.set(Array, Index, Value).

set(A1, N, X) = A2 :-
    array.set(N, X, A1, A2).

set(Index, Item, !Array) :-
    ( if
        bounds_checks,
        not array.in_bounds(!.Array, Index)
    then
        out_of_bounds_error(!.Array, Index, "array.set")
    else
        array.unsafe_set(Index, Item, !Array)
    ).

'unsafe_elem :='(Index, !.Array, Value) = !:Array :-
    array.unsafe_set(Index, Value, !Array).

:- pragma foreign_proc("C",
    unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, T, array(T), array(T)), [
            cel(Array0, []) - cel(Array, []),
            cel(Item, [])   - cel(Array, [T])
        ])
    ],
"
    Array0->elements[Index] = Item; /* destructive update! */
    Array = Array0;
").

:- pragma foreign_proc("C#",
    unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Array0.SetValue(Item, Index);   /* destructive update! */
    Array = Array0;
}").

:- pragma foreign_proc("Erlang",
    unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = setelement(Index + 1, Array0, Item)
").

:- pragma foreign_proc("Java",
    unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 instanceof int[]) {
        ((int[]) Array0)[Index] = (Integer) Item;
    } else if (Array0 instanceof double[]) {
        ((double[]) Array0)[Index] = (Double) Item;
    } else if (Array0 instanceof char[]) {
        ((char[]) Array0)[Index] = (Character) Item;
    } else if (Array0 instanceof boolean[]) {
        ((boolean[]) Array0)[Index] = (Boolean) Item;
    } else if (Array0 instanceof byte[]) {
        ((byte[]) Array0)[Index] = (Byte) Item;
    } else if (Array0 instanceof short[]) {
        ((short[]) Array0)[Index] = (Short) Item;
    } else if (Array0 instanceof long[]) {
        ((long[]) Array0)[Index] = (Long) Item;
    } else if (Array0 instanceof float[]) {
        ((float[]) Array0)[Index] = (Float) Item;
    } else {
        ((Object[]) Array0)[Index] = Item;
    }
    Array = Array0;         /* destructive update! */
").

%---------------------------------------------------------------------------%

% lower bounds other than zero are not supported
%     % array.resize takes an array and new lower and upper bounds.
%     % the array is expanded or shrunk at each end to make it fit
%     % the new bounds.
% :- pred array.resize(array(T), int, int, array(T)).
% :- mode array.resize(in, in, in, out) is det.

:- pragma foreign_decl("C", "
extern void
ML_resize_array(MR_ArrayPtr new_array, MR_ArrayPtr old_array,
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
    MR_GC_free_attrib(old_array);
#endif
}
").

resize(!.Array, N, X) = !:Array :-
    array.resize(N, X, !Array).

:- pragma foreign_proc("C",
    resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, T, array(T), array(T)), [
            cel(Array0, []) - cel(Array, []),
            cel(Item, [])   - cel(Array, [T])
        ])
    ],
"
    if ((Array0)->size == Size) {
        Array = Array0;
    } else {
        ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
        ML_resize_array(Array, Array0, Size, Item);
    }
").

:- pragma foreign_proc("C#",
    resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_array_resize(Array0, Size, Item);
").

:- pragma foreign_proc("Erlang",
    resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    InitialSize = size(Array0),
    List = tuple_to_list(Array0),
    if
        Size < InitialSize ->
            Array = list_to_tuple(lists:sublist(List, Size));
        Size > InitialSize ->
            Array = list_to_tuple(lists:append(List,
                lists:duplicate(Size - InitialSize, Item)));
        true ->
            Array = Array0
    end
").

:- pragma foreign_proc("Java",
    resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = jmercury.array.ML_array_resize(Array0, Size, Item);
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
extern void
ML_shrink_array(MR_ArrayPtr array, MR_ArrayPtr old_array,
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
    MR_GC_free_attrib(old_array);
#endif
}
").

shrink(!.Array, N) = !:Array :-
    array.shrink(N, !Array).

shrink(Size, !Array) :-
    OldSize = array.size(!.Array),
    ( if Size > OldSize then
        error("array.shrink: can't shrink to a larger size")
    else if Size = OldSize then
        true
    else
        array.shrink_2(Size, !Array)
    ).

:- pred shrink_2(int::in, array(T)::array_di, array(T)::array_uo) is det.

:- pragma foreign_proc("C",
    shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(int, array(T), array(T)), [
            cel(Array0, []) - cel(Array, [])
        ])
    ],
"
    ML_alloc_array(Array, Size + 1, MR_ALLOC_ID);
    ML_shrink_array(Array, Array0, Size);
").

:- pragma foreign_proc("C#",
    shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_shrink_array(Array0, Size);
").

:- pragma foreign_proc("Erlang",
    shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = list_to_tuple(lists:sublist(tuple_to_list(Array0), Size))
").

:- pragma foreign_proc("Java",
    shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array0 == null) {
        Array = null;
    } else if (Array0 instanceof int[]) {
        Array = new int[Size];
    } else if (Array0 instanceof double[]) {
        Array = new double[Size];
    } else if (Array0 instanceof char[]) {
        Array = new char[Size];
    } else if (Array0 instanceof boolean[]) {
        Array = new boolean[Size];
    } else {
        Array = new Object[Size];
    }

    if (Array != null) {
        System.arraycopy(Array0, 0, Array, 0, Size);
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
extern void
ML_copy_array(MR_ArrayPtr array, MR_ConstArrayPtr old_array);
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
    ** Any changes to this function will probably also require changes to
    ** - array.append below, and
    ** - MR_deep_copy() in runtime/mercury_deep_copy.[ch].
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

copy(A1) = A2 :-
    array.copy(A1, A2).

:- pragma foreign_proc("C",
    copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(array(T), array(T)), [
            cel(Array0, [T]) - cel(Array, [T])
        ])
    ],
"
    ML_alloc_array(Array, Array0->size + 1, MR_ALLOC_ID);
    ML_copy_array(Array, (MR_ConstArrayPtr) Array0);
").

:- pragma foreign_proc("C#",
    copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = (System.Array) Array0.Clone();
").

:- pragma foreign_proc("Erlang",
    copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = Array0
").

:- pragma foreign_proc("Java",
    copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int Size;

    if (Array0 == null) {
        Array = null;
        Size = 0;
    } else if (Array0 instanceof int[]) {
        Size = ((int[]) Array0).length;
        Array = new int[Size];
    } else if (Array0 instanceof double[]) {
        Size = ((double[]) Array0).length;
        Array = new double[Size];
    } else if (Array0 instanceof char[]) {
        Size = ((char[]) Array0).length;
        Array = new char[Size];
    } else if (Array0 instanceof boolean[]) {
        Size = ((boolean[]) Array0).length;
        Array = new boolean[Size];
    } else {
        Size = ((Object[]) Array0).length;
        Array = new Object[Size];
    }

    if (Array != null) {
        System.arraycopy(Array0, 0, Array, 0, Size);
    }
").

%---------------------------------------------------------------------------%

array(List) = Array :-
    array.from_list(List, Array).

from_list(List) = Array :-
    array.from_list(List, Array).

from_list([], Array) :-
    array.make_empty_array(Array).
from_list(List, Array) :-
    List = [Head | Tail],
    list.length(List, Len),
    Array0 = array.unsafe_init(Len, Head, 0),
    array.unsafe_insert_items(Tail, 1, Array0, Array).

%---------------------------------------------------------------------------%

:- pred unsafe_insert_items(list(T)::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

unsafe_insert_items([], _N, !Array).
unsafe_insert_items([Head | Tail], N, !Array) :-
    unsafe_set(N, Head, !Array),
    unsafe_insert_items(Tail, N + 1, !Array).

%---------------------------------------------------------------------------%

from_reverse_list([]) = Array :-
    array.make_empty_array(Array).
from_reverse_list(RevList) = Array :-
    RevList = [Head | Tail],
    list.length(RevList, Len),
    Array0 = array.unsafe_init(Len, Head, Len - 1),
    unsafe_insert_items_reverse(Tail, Len - 2, Array0, Array).

:- pred unsafe_insert_items_reverse(list(T)::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

unsafe_insert_items_reverse([], _, !Array).
unsafe_insert_items_reverse([Head | Tail], N, !Array) :-
    unsafe_set(N, Head, !Array),
    unsafe_insert_items_reverse(Tail, N - 1, !Array).

%---------------------------------------------------------------------------%

to_list(Array) = List :-
    to_list(Array, List).

to_list(Array, List) :-
    bounds(Array, Low, High),
    fetch_items(Array, Low, High, List).

%---------------------------------------------------------------------------%

fetch_items(Array, Low, High) = List :-
    fetch_items(Array, Low, High, List).

fetch_items(Array, Low, High, List) :-
    ( if
        High < Low
    then
        % If High is less than Low, then there cannot be any array indexes
        % within the range Low -> High (inclusive). This can happen when
        % calling to_list/2 on the empty array. Testing for this condition
        % here rather than in to_list/2 is more general.
        List = []
    else if
        array.in_bounds(Array, Low),
        array.in_bounds(Array, High)
    then
        List = do_foldr_func(func(X, Xs) = [X | Xs], Array, [], Low, High)
    else
        error("array.fetch_items/4: One or more index is out of bounds")
    ).

%---------------------------------------------------------------------------%

bsearch(A, X, F) = MN :-
    P = (pred(X1::in, X2::in, C::out) is det :- C = F(X1, X2)),
    array.bsearch(A, X, P, MN).

bsearch(A, SearchX, Compare, Result) :-
    array.bounds(A, Lo, Hi),
    array.bsearch_2(A, Lo, Hi, SearchX, Compare, Result).

:- pred bsearch_2(array(T)::in, int::in, int::in, T::in,
    pred(T, T, comparison_result)::in(pred(in, in, out) is det),
    maybe(int)::out) is det.

bsearch_2(Array, Lo, Hi, SearchX, Compare, Result) :-
    Width = Hi - Lo,

    % If Width < 0, there is no range left.
    ( if Width < 0 then
        Result = no
    else
        % If Width == 0, we may just have found our element.
        % Do a Compare to check.
        ( if Width = 0 then
            array.lookup(Array, Lo, LoX),
            ( if Compare(SearchX, LoX, (=)) then
                Result = yes(Lo)
            else
                Result = no
            )
        else
            % Otherwise find the middle element of the range
            % and check against that.
            % We calculate Mid this way to avoid overflow.
            % The right shift by one bit is a fast implementation
            % of division by 2.
            Mid = Lo + ((Hi - Lo) `unchecked_right_shift` 1),
            array.lookup(Array, Mid, MidX),
            Compare(MidX, SearchX, Comp),
            (
                Comp = (<),
                array.bsearch_2(Array, Mid + 1, Hi, SearchX, Compare, Result)
            ;
                Comp = (=),
                array.bsearch_2(Array, Lo, Mid, SearchX, Compare, Result)
            ;
                Comp = (>),
                array.bsearch_2(Array, Lo, Mid - 1, SearchX, Compare, Result)
            )
        )
    ).

%---------------------------------------------------------------------------%

map(F, A1) = A2 :-
    P = (pred(X::in, Y::out) is det :- Y = F(X)),
    array.map(P, A1, A2).

map(Closure, OldArray, NewArray) :-
    ( if array.semidet_lookup(OldArray, 0, Elem0) then
        array.size(OldArray, Size),
        Closure(Elem0, Elem),
        NewArray0 = unsafe_init(Size, Elem, 0),
        array.map_2(1, Size, Closure, OldArray, NewArray0, NewArray)
    else
        array.make_empty_array(NewArray)
    ).

:- pred map_2(int::in, int::in, pred(T1, T2)::in(pred(in, out) is det),
    array(T1)::in, array(T2)::array_di, array(T2)::array_uo) is det.

map_2(N, Size, Closure, OldArray, !NewArray) :-
    ( if N >= Size then
        true
    else
        array.unsafe_lookup(OldArray, N, OldElem),
        Closure(OldElem, NewElem),
        array.unsafe_set(N, NewElem, !NewArray),
        map_2(N + 1, Size, Closure, OldArray, !NewArray)
    ).

%---------------------------------------------------------------------------%

member(A, X) :-
    nondet_int_in_range(array.min(A), array.max(A), N),
    array.unsafe_lookup(A, N, X).

%---------------------------------------------------------------------------%

    % array.sort/1 has type specialised versions for arrays of ints and strings
    % on the expectation that these constitute the common case and are hence
    % worth providing a fast-path.
    %
    % Experiments indicate that type specialisation improves the speed of
    % array.sort/1 by about 30-40%.
    %
:- pragma type_spec(array.sort/1, T = int).
:- pragma type_spec(array.sort/1, T = string).

sort(A) = samsort_subarray(A, array.min(A), array.max(A)).

:- pragma no_inline(array.sort_fix_2014/0).

sort_fix_2014.

%---------------------------------------------------------------------------%

binary_search(A, SearchX, I) :-
    array.binary_search(ordering, A, SearchX, I).

binary_search(Cmp, A, SearchX, I) :-
    Lo = 0,
    Hi = array.size(A) - 1,
    binary_search_loop(Cmp, A, SearchX, Lo, Hi, I).

:- pred binary_search_loop(comparison_func(T)::in, array(T)::array_ui,
    T::in, int::in, int::in, int::out) is semidet.

binary_search_loop(Cmp, A, SearchX, Lo, Hi, I) :-
    % loop invariant: if SearchX is anywhere in A[0] .. A[array.size(A)-1],
    % then it is in A[Lo] .. A[Hi].
    Lo =< Hi,
    % We calculate Mid this way to avoid overflow.
    % The right shift by one bit is a fast implementation of division by 2.
    Mid = Lo + ((Hi - Lo) `unchecked_right_shift` 1),
    array.unsafe_lookup(A, Mid, MidX),
    O = Cmp(MidX, SearchX),
    (
        O = (>),
        binary_search_loop(Cmp, A, SearchX, Lo, Mid - 1, I)
    ;
        O = (=),
        I = Mid
    ;
        O = (<),
        binary_search_loop(Cmp, A, SearchX, Mid + 1, Hi, I)
    ).

%---------------------------------------------------------------------------%

approx_binary_search(A, SearchX, I) :-
    approx_binary_search(ordering, A, SearchX, I).

approx_binary_search(Cmp, A, SearchX, I) :-
    Lo = 0,
    Hi = array.size(A) - 1,
    approx_binary_search_loop(Cmp, A, SearchX, Lo, Hi, I).

:- pred approx_binary_search_loop(comparison_func(T)::in, array(T)::array_ui,
    T::in, int::in, int::in, int::out) is semidet.

approx_binary_search_loop(Cmp, A, SearchX, Lo, Hi, I) :-
    % loop invariant: if SearchX is anywhere in A[0] .. A[array.size(A)-1],
    % then it is in A[Lo] .. A[Hi].
    Lo =< Hi,
    % We calculate Mid this way to avoid overflow.
    % The right shift by one bit is a fast implementation of division by 2.
    Mid = Lo + ((Hi - Lo) `unchecked_right_shift` 1),
    array.unsafe_lookup(A, Mid, MidX),
    O = Cmp(MidX, SearchX),
    (
        O = (>),
        approx_binary_search_loop(Cmp, A, SearchX, Lo, Mid - 1, I)
    ;
        O = (=),
        I = Mid
    ;
        O = (<),
        ( if
            ( if Mid < Hi then
                % We get here only if Mid + 1 cannot exceed Hi,
                % so the array access is safe.
                array.unsafe_lookup(A, Mid + 1, MidP1X),
                (<) = Cmp(SearchX, MidP1X)
            else
                Mid = Hi
            )
        then
            I = Mid
        else
            approx_binary_search_loop(Cmp, A, SearchX, Mid + 1, Hi, I)
        )
    ).

%---------------------------------------------------------------------------%

append(A, B) = C :-
    SizeA = array.size(A),
    SizeB = array.size(B),
    SizeC = SizeA + SizeB,
    ( if
        ( if SizeA > 0 then
            array.lookup(A, 0, InitElem)
        else if SizeB > 0 then
            array.lookup(B, 0, InitElem)
        else
            fail
        )
    then
        C0 = array.init(SizeC, InitElem),
        copy_subarray(A, 0, SizeA - 1, 0, C0, C1),
        copy_subarray(B, 0, SizeB - 1, SizeA, C1, C)
    else
        C = array.make_empty_array
    ).

:- pragma foreign_proc("C",
    append(ArrayA::in, ArrayB::in) = (ArrayC::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness,
        sharing(yes(array(T), array(T), array(T)), [
            cel(ArrayA, [T]) - cel(ArrayC, [T]),
            cel(ArrayB, [T]) - cel(ArrayC, [T])
        ])
    ],
"
    MR_Integer sizeC;
    MR_Integer i;
    MR_Integer offset;

    sizeC = ArrayA->size + ArrayB->size;
    ML_alloc_array(ArrayC, sizeC + 1, MR_ALLOC_ID);

    ArrayC->size = sizeC;
    for (i = 0; i < ArrayA->size; i++) {
        ArrayC->elements[i] = ArrayA->elements[i];
    }

    offset = ArrayA->size;
    for (i = 0; i < ArrayB->size; i++) {
        ArrayC->elements[offset + i] = ArrayB->elements[i];
    }
").

%---------------------------------------------------------------------------%

random_permutation(A0, A, RS0, RS) :-
    Lo = array.min(A0),
    Hi = array.max(A0),
    Sz = array.size(A0),
    permutation_2(Lo, Lo, Hi, Sz, A0, A, RS0, RS).

:- pred permutation_2(int::in, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo,
    random.supply::mdi, random.supply::muo) is det.

permutation_2(I, Lo, Hi, Sz, !A, !RS) :-
    ( if I > Hi then
        true
    else
        random.random(R, !RS),
        J  = Lo + (R `rem` Sz),
        swap_elems(I, J, !A),
        permutation_2(I + 1, Lo, Hi, Sz, !A, !RS)
    ).

:- pred swap_elems(int::in, int::in, array(T)::array_di, array(T)::array_uo)
    is det.

swap_elems(I, J, !A) :-
    array.lookup(!.A, I, XI),
    array.lookup(!.A, J, XJ),
    array.unsafe_set(I, XJ, !A),
    array.unsafe_set(J, XI, !A).

%---------------------------------------------------------------------------%

foldl(Fn, A, X) =
    do_foldl_func(Fn, A, X, array.min(A), array.max(A)).

:- func do_foldl_func(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
%:- mode do_foldl_func(func(in, in) = out is det, array_ui, in, in, in)
%   = out is det.
:- mode do_foldl_func(func(in, in) = out is det, in, in, in, in) = out is det.
%:- mode do_foldl_func(func(in, di) = uo is det, array_ui, di, in, in)
%   = uo is det.
:- mode do_foldl_func(func(in, di) = uo is det, in, di, in, in) = uo is det.

do_foldl_func(Fn, A, X, I, Max) =
    ( if Max < I then
        X
    else
        do_foldl_func(Fn, A, Fn(A ^ unsafe_elem(I), X), I + 1, Max)
    ).

%---------------------------------------------------------------------------%

foldl(P, A, !Acc) :-
    do_foldl_pred(P, A, array.min(A), array.max(A), !Acc).

:- pred do_foldl_pred(pred(T1, T2, T2), array(T1), int, int, T2, T2).
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, in, in, out) is det.
:- mode do_foldl_pred(pred(in, mdi, muo) is det, in, in, in, mdi, muo) is det.
:- mode do_foldl_pred(pred(in, di, uo) is det, in, in, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, in, in, out)
    is semidet.
:- mode do_foldl_pred(pred(in, mdi, muo) is semidet, in, in, in, mdi, muo)
    is semidet.
:- mode do_foldl_pred(pred(in, di, uo) is semidet, in, in, in, di, uo)
    is semidet.

do_foldl_pred(P, A, I, Max, !Acc) :-
    ( if Max < I then
        true
    else
        P(A ^ unsafe_elem(I), !Acc),
        do_foldl_pred(P, A, I + 1, Max, !Acc)
    ).

%---------------------------------------------------------------------------%

foldl2(P, A, !Acc1, !Acc2) :-
    do_foldl2(P, array.min(A), array.max(A), A, !Acc1, !Acc2).

:- pred do_foldl2(pred(T1, T2, T2, T3, T3), int, int, array(T1), T2, T2,
    T3, T3).
:- mode do_foldl2(pred(in, in, out, in, out) is det, in, in, in, in, out,
    in, out) is det.
:- mode do_foldl2(pred(in, in, out, mdi, muo) is det, in, in, in, in, out,
    mdi, muo) is det.
:- mode do_foldl2(pred(in, in, out, di, uo) is det, in, in, in, in, out,
    di, uo) is det.
:- mode do_foldl2(pred(in, in, out, in, out) is semidet, in, in, in, in, out,
    in, out) is semidet.
:- mode do_foldl2(pred(in, in, out, mdi, muo) is semidet, in, in, in, in, out,
    mdi, muo) is semidet.
:- mode do_foldl2(pred(in, in, out, di, uo) is semidet, in, in, in, in, out,
    di, uo) is semidet.

do_foldl2(P, I, Max, A, !Acc1, !Acc2) :-
    ( if Max < I then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2),
        do_foldl2(P, I + 1, Max, A, !Acc1, !Acc2)
    ).

%---------------------------------------------------------------------------%

foldl3(P, A, !Acc1, !Acc2, !Acc3) :-
    do_foldl3(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3).

:- pred do_foldl3(pred(T1, T2, T2, T3, T3, T4, T4), int, int, array(T1),
    T2, T2, T3, T3, T4, T4).
:- mode do_foldl3(pred(in, in, out, in, out, in, out) is det, in, in, in,
    in, out, in, out, in, out) is det.
:- mode do_foldl3(pred(in, in, out, in, out, mdi, muo) is det, in, in, in,
    in, out, in, out, mdi, muo) is det.
:- mode do_foldl3(pred(in, in, out, in, out, di, uo) is det, in, in, in,
    in, out, in, out, di, uo) is det.
:- mode do_foldl3(pred(in, in, out, in, out, in, out) is semidet, in, in, in,
    in, out, in, out, in, out) is semidet.
:- mode do_foldl3(pred(in, in, out, in, out, mdi, muo) is semidet, in, in, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode do_foldl3(pred(in, in, out, in, out, di, uo) is semidet, in, in, in,
    in, out, in, out, di, uo) is semidet.

do_foldl3(P, I, Max, A, !Acc1, !Acc2, !Acc3) :-
    ( if Max < I then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3),
        do_foldl3(P, I + 1, Max, A, !Acc1, !Acc2, !Acc3)
    ).

%---------------------------------------------------------------------------%

foldl4(P, A, !Acc1, !Acc2, !Acc3, !Acc4) :-
    do_foldl4(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3, !Acc4).

:- pred do_foldl4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), int, int,
    array(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode do_foldl4(pred(in, in, out, in, out, in, out, in, out) is det, in, in,
    in, in, out, in, out, in, out, in, out) is det.
:- mode do_foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det, in, in,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_foldl4(pred(in, in, out, in, out, in, out, di, uo) is det, in, in,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode do_foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode do_foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_foldl4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, di, uo) is semidet.

do_foldl4(P, I, Max, A, !Acc1, !Acc2, !Acc3, !Acc4) :-
    ( if Max < I then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3, !Acc4),
        do_foldl4(P, I + 1, Max, A, !Acc1, !Acc2, !Acc3, !Acc4)
    ).

%---------------------------------------------------------------------------%

foldl5(P, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5) :-
    do_foldl5(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3, !Acc4,
        !Acc5).

:- pred do_foldl5(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    int, int, array(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, di, uo) is semidet.

do_foldl5(P, I, Max, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5) :-
    ( if Max < I then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3, !Acc4, !Acc5),
        do_foldl5(P, I + 1, Max, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5)
    ).

%---------------------------------------------------------------------------%

foldr(Fn, A, X) =
    do_foldr_func(Fn, A, X, array.min(A), array.max(A)).

:- func do_foldr_func(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
%:- mode do_foldr_func(func(in, in) = out is det, array_ui, in, in, in)
%   = out is det.
:- mode do_foldr_func(func(in, in) = out is det, in, in, in, in) = out is det.
%:- mode do_foldr_func(func(in, di) = uo is det, array_ui, di, in, in)
%   = uo is det.
:- mode do_foldr_func(func(in, di) = uo is det, in, di, in, in) = uo is det.

do_foldr_func(Fn, A, X, Min, I) =
    ( if I < Min then
        X
    else
        do_foldr_func(Fn, A, Fn(A ^ unsafe_elem(I), X), Min, I - 1)
    ).

%---------------------------------------------------------------------------%

foldr(P, A, !Acc) :-
    do_foldr_pred(P, array.min(A), array.max(A), A, !Acc).

:- pred do_foldr_pred(pred(T1, T2, T2), int, int, array(T1), T2, T2).
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, in, in, out) is det.
:- mode do_foldr_pred(pred(in, mdi, muo) is det, in, in, in, mdi, muo) is det.
:- mode do_foldr_pred(pred(in, di, uo) is det, in, in, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, in, in, out)
    is semidet.
:- mode do_foldr_pred(pred(in, mdi, muo) is semidet, in, in, in, mdi, muo)
    is semidet.
:- mode do_foldr_pred(pred(in, di, uo) is semidet, in, in, in, di, uo)
    is semidet.

do_foldr_pred(P, Min, I, A, !Acc) :-
    ( if I < Min then
        true
    else
        P(A ^ unsafe_elem(I), !Acc),
        do_foldr_pred(P, Min, I - 1, A, !Acc)
    ).

%---------------------------------------------------------------------------%

foldr2(P, A, !Acc1, !Acc2) :-
    do_foldr2(P, array.min(A), array.max(A), A, !Acc1, !Acc2).

:- pred do_foldr2(pred(T1, T2, T2, T3, T3), int, int, array(T1), T2, T2,
    T3, T3).
:- mode do_foldr2(pred(in, in, out, in, out) is det, in, in, in, in, out,
    in, out) is det.
:- mode do_foldr2(pred(in, in, out, mdi, muo) is det, in, in, in, in, out,
    mdi, muo) is det.
:- mode do_foldr2(pred(in, in, out, di, uo) is det, in, in, in, in, out,
    di, uo) is det.
:- mode do_foldr2(pred(in, in, out, in, out) is semidet, in, in, in, in, out,
    in, out) is semidet.
:- mode do_foldr2(pred(in, in, out, mdi, muo) is semidet, in, in, in, in, out,
    mdi, muo) is semidet.
:- mode do_foldr2(pred(in, in, out, di, uo) is semidet, in, in, in, in, out,
    di, uo) is semidet.

do_foldr2(P, Min, I, A, !Acc1, !Acc2) :-
    ( if I < Min then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2),
        do_foldr2(P, Min, I - 1, A, !Acc1, !Acc2)
    ).

%---------------------------------------------------------------------------%

foldr3(P, A, !Acc1, !Acc2, !Acc3) :-
    do_foldr3(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3).

:- pred do_foldr3(pred(T1, T2, T2, T3, T3, T4, T4), int, int, array(T1),
    T2, T2, T3, T3, T4, T4).
:- mode do_foldr3(pred(in, in, out, in, out, in, out) is det, in, in, in,
    in, out, in, out, in, out) is det.
:- mode do_foldr3(pred(in, in, out, in, out, mdi, muo) is det, in, in, in,
    in, out, in, out, mdi, muo) is det.
:- mode do_foldr3(pred(in, in, out, in, out, di, uo) is det, in, in, in,
    in, out, in, out, di, uo) is det.
:- mode do_foldr3(pred(in, in, out, in, out, in, out) is semidet, in, in, in,
    in, out, in, out, in, out) is semidet.
:- mode do_foldr3(pred(in, in, out, in, out, mdi, muo) is semidet, in, in, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode do_foldr3(pred(in, in, out, in, out, di, uo) is semidet, in, in, in,
    in, out, in, out, di, uo) is semidet.

do_foldr3(P, Min, I, A, !Acc1, !Acc2, !Acc3) :-
    ( if I < Min then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3),
        do_foldr3(P, Min, I - 1, A, !Acc1, !Acc2, !Acc3)
    ).

%---------------------------------------------------------------------------%

foldr4(P, A, !Acc1, !Acc2, !Acc3, !Acc4) :-
    do_foldr4(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3, !Acc4).

:- pred do_foldr4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), int, int,
    array(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode do_foldr4(pred(in, in, out, in, out, in, out, in, out) is det, in, in,
    in, in, out, in, out, in, out, in, out) is det.
:- mode do_foldr4(pred(in, in, out, in, out, in, out, mdi, muo) is det, in, in,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_foldr4(pred(in, in, out, in, out, in, out, di, uo) is det, in, in,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode do_foldr4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode do_foldr4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_foldr4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, di, uo) is semidet.

do_foldr4(P, Min, I, A, !Acc1, !Acc2, !Acc3, !Acc4) :-
    ( if I < Min then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3, !Acc4),
        do_foldr4(P, Min, I - 1, A, !Acc1, !Acc2, !Acc3, !Acc4)
    ).

%---------------------------------------------------------------------------%

foldr5(P, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5) :-
    do_foldr5(P, array.min(A), array.max(A), A, !Acc1, !Acc2, !Acc3, !Acc4,
        !Acc5).

:- pred do_foldr5(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    int, int, array(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_foldr5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, in, out, di, uo) is semidet.

do_foldr5(P, Min, I, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5) :-
    ( if I < Min then
        true
    else
        P(A ^ unsafe_elem(I), !Acc1, !Acc2, !Acc3, !Acc4, !Acc5),
        do_foldr5(P, Min, I - 1, A, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5)
    ).

%---------------------------------------------------------------------------%

map_foldl(P, A, B, !Acc) :-
    N = array.size(A),
    ( if N =< 0 then
        B = array.make_empty_array
    else
        array.unsafe_lookup(A, 0, X),
        P(X, Y, !Acc),
        B1 = array.init(N, Y),
        map_foldl_2(P, 1, A, B1, B, !Acc)
    ).

:- pred map_foldl_2(pred(T1, T2, T3, T3),
    int, array(T1), array(T2), array(T2), T3, T3).
:- mode map_foldl_2(in(pred(in, out, in, out) is det),
    in, in, array_di, array_uo, in, out) is det.
:- mode map_foldl_2(in(pred(in, out, mdi, muo) is det),
    in, in, array_di, array_uo, mdi, muo) is det.
:- mode map_foldl_2(in(pred(in, out, di, uo) is det),
    in, in, array_di, array_uo, di, uo) is det.
:- mode map_foldl_2(in(pred(in, out, in, out) is semidet),
    in, in, array_di, array_uo, in, out) is semidet.

map_foldl_2(P, I, A, !B, !Acc) :-
    ( if I < array.size(A) then
        array.unsafe_lookup(A, I, X),
        P(X, Y, !Acc),
        array.unsafe_set(I, Y, !B),
        map_foldl_2(P, I + 1, A, !B, !Acc)
    else
        true
    ).

map_corresponding_foldl(P, A, B, C, !Acc) :-
    N = array.size(A),
    ( if N =< 0 then
        C = array.make_empty_array
    else
        array.unsafe_lookup(A, 0, X),
        array.unsafe_lookup(B, 0, Y),
        P(X, Y, Z, !Acc),
        C1 = array.init(N, Z),
        map_corresponding_foldl_2(P, 1, N, A, B, C1, C, !Acc)
    ).

:- pred map_corresponding_foldl_2(pred(T1, T2, T3, T4, T4),
    int, int, array(T1), array(T2), array(T3), array(T3), T4, T4).
:- mode map_corresponding_foldl_2(
    in(pred(in, in, out, in, out) is det),
    in, in, in, in, array_di, array_uo, in, out) is det.
:- mode map_corresponding_foldl_2(
    in(pred(in, in, out, mdi, muo) is det),
    in, in, in, in, array_di, array_uo, mdi, muo) is det.
:- mode map_corresponding_foldl_2(
    in(pred(in, in, out, di, uo) is det),
    in, in, in, in, array_di, array_uo, di, uo) is det.
:- mode map_corresponding_foldl_2(
    in(pred(in, in, out, in, out) is semidet),
    in, in, in, in, array_di, array_uo, in, out) is semidet.

map_corresponding_foldl_2(P, I, N, A, B, !C, !Acc) :-
    ( if I < N then
        array.unsafe_lookup(A, I, X),
        array.unsafe_lookup(B, I, Y),
        P(X, Y, Z, !Acc),
        array.unsafe_set(I, Z, !C),
        map_corresponding_foldl_2(P, I + 1, N, A, B, !C, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

all_true(Pred, Array) :-
    do_all_true(Pred, array.min(Array), array.max(Array), Array).

:- pred do_all_true(pred(T), int, int, array(T)).
%:- mode do_all_true(in(pred(in) is semidet), in, in, array_ui) is semidet.
:- mode do_all_true(in(pred(in) is semidet), in, in, in) is semidet.

do_all_true(Pred, I, UB, Array) :-
    ( if I =< UB then
        array.unsafe_lookup(Array, I, Elem),
        Pred(Elem),
        do_all_true(Pred, I + 1, UB, Array)
    else
        true
    ).

all_false(Pred, Array) :-
    do_all_false(Pred, array.min(Array), array.max(Array), Array).

:- pred do_all_false(pred(T), int, int, array(T)).
%:- mode do_all_false(in(pred(in) is semidet), in, in, array_ui) is semidet.
:- mode do_all_false(in(pred(in) is semidet), in, in, in) is semidet.

do_all_false(Pred, I, UB, Array) :-
    ( if I =< UB then
        array.unsafe_lookup(Array, I, Elem),
        not Pred(Elem),
        do_all_false(Pred, I + 1, UB, Array)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
    samsort_up(0, array.copy(A0), A, A0, _, Lo, Hi, Lo).

    % samsort_up(N, A0, A, B0, B, Lo, Hi, I):
    %
    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 is sorted from Lo .. I - 1.
    %   A0 and B0 are identical from I .. Hi.
    % Postcondition:
    %   A is sorted from Lo .. Hi.
    %
:- pred samsort_up(int::in, array(T)::array_di, array(T)::array_uo,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::in) is det.

:- pragma type_spec(samsort_up/8, T = int).
:- pragma type_spec(samsort_up/8, T = string).

samsort_up(N, A0, A, B0, B, Lo, Hi, I) :-
    trace [compile_time(flag("array_sort"))] (
        verify_sorted(A0, Lo, I - 1),
        verify_identical(A0, B0, I, Hi)
    ),
    ( if I > Hi then
        A = A0,
        B = B0
        % A is sorted from Lo .. Hi.
    else if N > 0 then
        % B0 and A0 are identical from I .. Hi.
        samsort_down(N - 1, B0, B1, A0, A1, I, Hi, J),
        % A1 is sorted from I .. J - 1.
        % B1 and A1 are identical from J .. Hi.

        merge_subarrays(A1, Lo, I - 1, I, J - 1, Lo, B1, B2),
        A2 = A1,

        % B2 is sorted from Lo .. J - 1.
        % B2 and A2 are identical from J .. Hi.
        samsort_up(N + 1, B2, B3, A2, A3, Lo, Hi, J),
        % B3 is sorted from Lo .. Hi.

        A = B3,
        B = A3
        % A is sorted from Lo .. Hi.
    else
        % N = 0, I = Lo
        copy_run_ascending(A0, B0, B1, Lo, Hi, J),

        % B1 is sorted from Lo .. J - 1.
        % B1 and A0 are identical from J .. Hi.
        samsort_up(N + 1, B1, B2, A0, A2, Lo, Hi, J),
        % B2 is sorted from Lo .. Hi.

        A = B2,
        B = A2
        % A is sorted from Lo .. Hi.
    ),
    trace [compile_time(flag("array_sort"))] (
        verify_sorted(A, Lo, Hi)
    ).

    % samsort_down(N, A0, A, B0, B, Lo, Hi, I):
    %
    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 and B0 are identical from Lo .. Hi.
    % Postcondition:
    %   B is sorted from Lo .. I - 1.
    %   A and B are identical from I .. Hi.
    %
:- pred samsort_down(int::in, array(T)::array_di, array(T)::array_uo,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::out) is det.

:- pragma type_spec(samsort_down/8, T = int).
:- pragma type_spec(samsort_down/8, T = string).

samsort_down(N, A0, A, B0, B, Lo, Hi, I) :-
    trace [compile_time(flag("array_sort"))] (
        verify_identical(A0, B0, Lo, Hi)
    ),
    ( if Lo > Hi then
        A = A0,
        B = B0,
        I = Lo
        % B is sorted from Lo .. I - 1.
    else if N > 0 then
        samsort_down(N - 1, B0, B1, A0, A1, Lo, Hi, J),
        samsort_down(N - 1, B1, B2, A1, A2, J,  Hi, I),
        % A2 is sorted from Lo .. J - 1.
        % A2 is sorted from J  .. I - 1.
        A = A2,
        merge_subarrays(A2, Lo, J - 1, J, I - 1, Lo, B2, B)
        % B is sorted from Lo .. I - 1.
    else
        A = A0,
        copy_run_ascending(A0, B0, B, Lo, Hi, I)
        % B is sorted from Lo .. I - 1.
    ),
    trace [compile_time(flag("array_sort"))] (
        verify_sorted(B, Lo, I - 1),
        verify_identical(A, B, I, Hi)
    ).

:- pred verify_sorted(array(T)::array_ui, int::in, int::in) is det.

verify_sorted(A, Lo, Hi) :-
    ( if Lo >= Hi then
        true
    else if compare((<), A ^ elem(Lo + 1), A ^ elem(Lo)) then
        unexpected($module, $pred, "array range not sorted")
    else
        verify_sorted(A, Lo + 1, Hi)
    ).

:- pred verify_identical(array(T)::array_ui, array(T)::array_ui,
    int::in, int::in) is det.

verify_identical(A, B, Lo, Hi) :-
    ( if Lo > Hi then
        true
    else if A ^ elem(Lo) = B ^ elem(Lo) then
        verify_identical(A, B, Lo + 1, Hi)
    else
        unexpected($module, $pred, "array ranges not identical")
    ).

%---------------------------------------------------------------------------%

:- pred copy_run_ascending(array(T)::array_ui,
    array(T)::array_di, array(T)::array_uo, int::in, int::in, int::out) is det.

:- pragma type_spec(copy_run_ascending/6, T = int).
:- pragma type_spec(copy_run_ascending/6, T = string).

copy_run_ascending(A, !B, Lo, Hi, I) :-
    ( if
        Lo < Hi,
        compare((>), A ^ elem(Lo), A ^ elem(Lo + 1))
    then
        I = search_until((<), A, Lo, Hi),
        copy_subarray_reverse(A, Lo, I - 1, I - 1, !B)
    else
        I = search_until((>), A, Lo, Hi),
        copy_subarray(A, Lo, I - 1, Lo, !B)
    ).

%---------------------------------------------------------------------------%

:- func search_until(comparison_result::in, array(T)::array_ui,
    int::in, int::in) = (int::out) is det.

:- pragma type_spec(search_until/4, T = int).
:- pragma type_spec(search_until/4, T = string).

search_until(R, A, Lo, Hi) =
    ( if
        Lo < Hi,
        not compare(R, A ^ elem(Lo), A ^ elem(Lo + 1))
    then
        search_until(R, A, Lo + 1, Hi)
    else
        Lo + 1
    ).

%---------------------------------------------------------------------------%

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI + (Ho - Lo + 1).
    % In this version, I is ascending, so B[InitI] gets A[Lo]
    %
:- pred copy_subarray(array(T)::array_ui, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray/6, T = int).
:- pragma type_spec(copy_subarray/6, T = string).

copy_subarray(A, Lo, Hi, I, !B) :-
    ( if Lo =< Hi then
        array.lookup(A, Lo, X),
        % XXX Would it be safe to replace this with array.unsafe_set?
        array.set(I, X, !B),
        copy_subarray(A, Lo + 1, Hi, I + 1, !B)
    else
        true
    ).

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI - (Ho - Lo + 1).
    % In this version, I is descending, so B[InitI] gets A[Hi].
    %
:- pred copy_subarray_reverse(array(T)::array_ui, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray_reverse/6, T = int).
:- pragma type_spec(copy_subarray_reverse/6, T = string).

copy_subarray_reverse(A, Lo, Hi, I, !B) :-
    ( if Lo =< Hi then
        array.lookup(A, Lo, X),
        % XXX Would it be safe to replace this with array.unsafe_set?
        array.set(I, X, !B),
        copy_subarray_reverse(A, Lo + 1, Hi, I - 1, !B)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % merges the two sorted consecutive subarrays Lo1 .. Hi1 and Lo2 .. Hi2
    % from A into the subarray starting at I in B.
    %
:- pred merge_subarrays(array(T)::array_ui,
    int::in, int::in, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(merge_subarrays/8, T = int).
:- pragma type_spec(merge_subarrays/8, T = string).

merge_subarrays(A, Lo1, Hi1, Lo2, Hi2, I, !B) :-
    ( if Lo1 > Hi1 then
        copy_subarray(A, Lo2, Hi2, I, !B)
    else if Lo2 > Hi2 then
        copy_subarray(A, Lo1, Hi1, I, !B)
    else
        array.lookup(A, Lo1, X1),
        array.lookup(A, Lo2, X2),
        compare(R, X1, X2),
        (
            R = (<),
            array.set(I, X1, !B),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, !B)
        ;
            R = (=),
            array.set(I, X1, !B),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, !B)
        ;
            R = (>),
            array.set(I, X2, !B),
            merge_subarrays(A, Lo1, Hi1, Lo2 + 1, Hi2, I + 1, !B)
        )
    ).

%---------------------------------------------------------------------------%

    % Throw an exception indicating an array bounds error.
    %
:- pred out_of_bounds_error(array(T), int, string).
%:- mode out_of_bounds_error(array_ui, in, in) is erroneous.
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
    string.format("%s: index %d not in range [%d, %d]",
        [s(PredName), i(Index), i(Min), i(Max)], Msg),
    throw(array.index_out_of_bounds(Msg)).

%---------------------------------------------------------------------------%

least_index(A) = array.min(A).

det_least_index(A) = Index :-
    ( if array.is_empty(A) then
        error("array.det_least_index: empty array")
    else
        Index = array.min(A)
    ).

semidet_least_index(A) = Index :-
    ( if array.is_empty(A) then
        fail
    else
        Index = array.min(A)
    ).

%---------------------------------------------------------------------------%

greatest_index(A) = array.max(A).

det_greatest_index(A) = Index :-
    ( if array.is_empty(A) then
        error("array.det_greatest_index: empty array")
    else
        Index = array.max(A)
    ).

semidet_greatest_index(A) = Index :-
    ( if array.is_empty(A) then
        fail
    else
        Index = array.max(A)
    ).

%---------------------------------------------------------------------------%

array_to_doc(A) =
    indent([str("array(["), array_to_doc_2(0, A), str("])")]).

:- func array_to_doc_2(int, array(T)) = doc.

array_to_doc_2(I, A) =
    ( if I > array.max(A) then
        str("")
    else
        docs([
            format_arg(format(A ^ elem(I))),
            ( if I = array.max(A) then str("") else group([str(", "), nl]) ),
            format_susp((func) = array_to_doc_2(I + 1, A))
        ])
    ).

%---------------------------------------------------------------------------%

dynamic_cast_to_array(X, A) :-
    % If X is an array then it has a type with one type argument.
    [ArgTypeDesc] = type_args(type_of(X)),

    % Convert ArgTypeDesc to a type variable ArgType.
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,

    % Constrain the type of A to be array(ArgType) and do the cast.
    dynamic_cast(X, A `with_type` array(ArgType)).

%---------------------------------------------------------------------------%
:- end_module array.
%---------------------------------------------------------------------------%
