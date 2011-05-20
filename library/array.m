%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997-2011 The University of Melbourne.
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
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

    % An `array.index_out_of_bounds' is the exception thrown
    % on out-of-bounds array accesses. The string describes
    % the predicate or function reporting the error.
:- type array.index_out_of_bounds
    --->    array.index_out_of_bounds(string).

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
%:- mode array.min(array_ui, out) is det.
:- mode array.min(in, out) is det.

:- func array.min(array(_T)) = int.
%:- mode array.min(array_ui) = out is det.
:- mode array.min(in) = out is det.

:- func array.least_index(array(T)) = int.
%:- mode array.least_index(array_ui) = out is det.
:- mode array.least_index(in) = out is det.

    % array.max returns the upper bound of the array.
    %
:- pred array.max(array(_T), int).
%:- mode array.max(array_ui, out) is det.
:- mode array.max(in, out) is det.

:- func array.max(array(_T)) = int.
%:- mode array.max(array_ui) = out is det.
:- mode array.max(in) = out is det.

:- func array.greatest_index(array(T)) = int.
%:- mode array.greatest_index(array_ui) = out is det.
:- mode array.greatest_index(in) = out is det.

    % array.size returns the length of the array,
    % i.e. upper bound - lower bound + 1.
    %
:- pred array.size(array(_T), int).
%:- mode array.size(array_ui, out) is det.
:- mode array.size(in, out) is det.

:- func array.size(array(_T)) = int.
%:- mode array.size(array_ui) = out is det.
:- mode array.size(in) = out is det.

    % array.bounds returns the upper and lower bounds of an array.
    % Note: in this implementation, the lower bound is always zero.
    %
:- pred array.bounds(array(_T), int, int).
%:- mode array.bounds(array_ui, out, out) is det.
:- mode array.bounds(in, out, out) is det.

    % array.in_bounds checks whether an index is in the bounds of an array.
    %
:- pred array.in_bounds(array(_T), int).
%:- mode array.in_bounds(array_ui, in) is semidet.
:- mode array.in_bounds(in, in) is semidet.

%-----------------------------------------------------------------------------%

    % array.lookup returns the Nth element of an array.
    % Throws an exception if the index is out of bounds.
    %
:- pred array.lookup(array(T), int, T).
%:- mode array.lookup(array_ui, in, out) is det.
:- mode array.lookup(in, in, out) is det.

:- func array.lookup(array(T), int) = T.
%:- mode array.lookup(array_ui, in) = out is det.
:- mode array.lookup(in, in) = out is det.

    % array.semidet_lookup returns the Nth element of an array.
    % It fails if the index is out of bounds.
    %
:- pred array.semidet_lookup(array(T), int, T).
%:- mode array.semidet_lookup(array_ui, in, out) is semidet.
:- mode array.semidet_lookup(in, in, out) is semidet.

    % array.unsafe_lookup returns the Nth element of an array.
    % It is an error if the index is out of bounds.
    %
:- pred array.unsafe_lookup(array(T), int, T).
%:- mode array.unsafe_lookup(array_ui, in, out) is det.
:- mode array.unsafe_lookup(in, in, out) is det.

    % array.set sets the nth element of an array, and returns the
    % resulting array (good opportunity for destructive update ;-).
    % Throws an exception if the index is out of bounds.
    %
:- pred array.set(int, T, array(T), array(T)).
:- mode array.set(in, in, array_di, array_uo) is det.

    % The same as array.set, except the arguments are in an order
    % that allows the use of state variables.
    %
:- pred array.svset(int, T, array(T), array(T)).
:- mode array.svset(in, in, array_di, array_uo) is det.

:- func array.set(array(T), int, T) = array(T).
:- mode array.set(array_di, in, in) = array_uo is det.

    % array.semidet_set sets the nth element of an array, and returns
    % the resulting array. It fails if the index is out of bounds.
    %
:- pred array.semidet_set(int, T, array(T), array(T)).
:- mode array.semidet_set(in, in, array_di, array_uo) is semidet.

    % array.unsafe_set sets the nth element of an array, and returns the
    % resulting array.  It is an error if the index is out of bounds.
    %
:- pred array.unsafe_set(int, T, array(T), array(T)).
:- mode array.unsafe_set(in, in, array_di, array_uo) is det.

    % The same as array.unsafe_set, except the arguments are in an order
    % that allows the use of state variables.
    %
:- pred array.unsafe_svset(int, T, array(T), array(T)).
:- mode array.unsafe_svset(in, in, array_di, array_uo) is det.

    % array.slow_set sets the nth element of an array, and returns the
    % resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It is an error if the index is out of bounds.
    %
:- pred array.slow_set(int, T, array(T), array(T)).
%:- mode array.slow_set(in, in, array_ui, array_uo) is det.
:- mode array.slow_set(in, in, in, array_uo) is det.

:- func array.slow_set(array(T), int, T) = array(T).
%:- mode array.slow_set(array_ui, in, in) = array_uo is det.
:- mode array.slow_set(in, in, in) = array_uo is det.

    % array.semidet_slow_set sets the nth element of an array, and returns
    % the resulting array. The initial array is not required to be unique,
    % so the implementation may not be able to use destructive update.
    % It fails if the index is out of bounds.
    %
:- pred array.semidet_slow_set(int, T, array(T), array(T)).
%:- mode array.semidet_slow_set(in, in, array_ui, array_uo) is semidet.
:- mode array.semidet_slow_set(in, in, in, array_uo) is semidet.

    % Field selection for arrays.
    % Array ^ elem(Index) = array.lookup(Array, Index).
    %
:- func array.elem(int, array(T)) = T.
%:- mode array.elem(in, array_ui) = out is det.
:- mode array.elem(in, in) = out is det.

    % As above, but omit the bounds check.
    %
:- func array.unsafe_elem(int, array(T)) = T.
%:- mode array.unsafe_elem(in, array_ui) = out is det.
:- mode array.unsafe_elem(in, in) = out is det.

    % Field update for arrays.
    % (Array ^ elem(Index) := Value) = array.set(Array, Index, Value).
    %
:- func 'elem :='(int, array(T), T) = array(T).
:- mode 'elem :='(in, array_di, in) = array_uo is det.

    % Returns every element of the array, one by one.
    %
:- pred array.member(array(T)::in, T::out) is nondet.

%-----------------------------------------------------------------------------%

    % array.copy(Array0, Array):
    % Makes a new unique copy of an array.
    %
:- pred array.copy(array(T), array(T)).
%:- mode array.copy(array_ui, array_uo) is det.
:- mode array.copy(in, array_uo) is det.

:- func array.copy(array(T)) = array(T).
%:- mode array.copy(array_ui) = array_uo is det.
:- mode array.copy(in) = array_uo is det.

    % array.resize(Array0, Size, Init, Array):
    % The array is expanded or shrunk to make it fit the new size `Size'.
    % Any new entries are filled with `Init'.
    %
:- pred array.resize(int, T, array(T), array(T)).
:- mode array.resize(in, in, array_di, array_uo) is det.

:- func array.resize(array(T), int, T) = array(T).
:- mode array.resize(array_di, in, in) = array_uo is det.

    % array.shrink(Array0, Size, Array):
    % The array is shrunk to make it fit the new size `Size'.
    % Throws an exception if `Size' is larger than the size of `Array0'.
    %
:- pred array.shrink(int, array(T), array(T)).
:- mode array.shrink(in, array_di, array_uo) is det.

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
%:- mode array.to_list(array_ui, out) is det.
:- mode array.to_list(in, out) is det.

:- func array.to_list(array(T)) = list(T).
%:- mode array.to_list(array_ui) = out is det.
:- mode array.to_list(in) = out is det.

    % array.fetch_items takes an array and a lower and upper index,
    % and places those items in the array between these indices into a list.
    % It is an error if either index is out of bounds.
    %
:- pred array.fetch_items(array(T), int, int, list(T)).
:- mode array.fetch_items(in, in, in, out) is det.

:- func array.fetch_items(array(T), int, int) = list(T).
%:- mode array.fetch_items(array_ui, in, in) = out is det.
:- mode array.fetch_items(in, in, in) = out is det.

    % XXX We prefer users to call the new array.binary_search predicate
    % instead of array.bsearch, which may be deprecated in later releases.
    %
    % array.bsearch takes an array, an element to be matched and a comparison
    % predicate and returns the position of the first occurrence in the array
    % of an element which is equivalent to the given one in the ordering
    % provided. Assumes the array is sorted according to this ordering.
    % Fails if the element is not present.
    %
:- pred array.bsearch(array(T), T, comparison_pred(T), maybe(int)).
%:- mode array.bsearch(array_ui, in, in(comparison_pred), out) is det.
:- mode array.bsearch(in, in, in(comparison_pred), out) is det.

:- func array.bsearch(array(T), T, comparison_func(T)) = maybe(int).
%:- mode array.bsearch(array_ui, in, in(comparison_func)) = out is det.
:- mode array.bsearch(in, in, in(comparison_func)) = out is det.

    % array.approx_binary_search(A, X, I) performs a binary search for an
    % approximate match for X in array A, computing I as the result.  More
    % specifically, if the call succeeds, then either A ^ elem(I) = X or
    % A ^ elem(I) @< X and either X @< A ^ elem(I + 1) or I is the last index
    % in A.
    %
    % array.binary_search(A, X, I) performs a binary search for an
    % exact match for X in array A (i.e., it succeeds iff X = A ^ elem(I)).
    %
    % A must be sorted into ascending order, but may contain duplicates
    % (the ordering must be with respect to the supplied comparison predicate
    % if one is supplied, otherwise with respect to the Mercury standard
    % ordering).
    %
:- pred array.approx_binary_search(array(T), T, int).
:- mode array.approx_binary_search(array_ui, in, out) is semidet.

:- pred array.approx_binary_search(comparison_func(T), array(T), T, int).
:- mode array.approx_binary_search(in, array_ui, in, out) is semidet.

:- pred array.binary_search(array(T), T, int).
:- mode array.binary_search(array_ui, in, out) is semidet.

:- pred array.binary_search(comparison_func(T), array(T), T, int).
:- mode array.binary_search(in, array_ui, in, out) is semidet.

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
%:- mode array.foldl(func(in, in) = out is det, array_ui, in) = out is det.
:- mode array.foldl(func(in, in) = out is det, in, in) = out is det.
%:- mode array.foldl(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode array.foldl(func(in, di) = uo is det, in, di) = uo is det.

    % array.foldl(Pr, Array, !X) is equivalent to
    %   list.foldl(Pr, array.to_list(Array), !X)
    % but more efficient.
    %
:- pred array.foldl(pred(T1, T2, T2), array(T1), T2, T2).
:- mode array.foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode array.foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode array.foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode array.foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode array.foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode array.foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % array.foldl2(Pr, Array, !X, !Y) is equivalent to
    %   list.foldl2(Pr, array.to_list(Array), !X, !Y)
    % but more efficient.
    %
:- pred array.foldl2(pred(T1, T2, T2, T3, T3), array(T1), T2, T2, T3, T3).
:- mode array.foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode array.foldl2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode array.foldl2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode array.foldl2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode array.foldl2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode array.foldl2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % array.foldr(Fn, Array, X) is equivalent to
    %   list.foldr(Fn, array.to_list(Array), X)
    % but more efficient.
    %
:- func array.foldr(func(T1, T2) = T2, array(T1), T2) = T2.
%:- mode array.foldr(func(in, in) = out is det, array_ui, in) = out is det.
:- mode array.foldr(func(in, in) = out is det, in, in) = out is det.
%:- mode array.foldr(func(in, di) = uo is det, array_ui, di) = uo is det.
:- mode array.foldr(func(in, di) = uo is det, in, di) = uo is det.

    % array.foldr(P, Array, !Acc) is equivalent to
    %   list.foldr(P, array.to_list(Array), !Acc)
    % but more efficient.
    %
:- pred array.foldr(pred(T1, T2, T2), array(T1), T2, T2).
:- mode array.foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode array.foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode array.foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode array.foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode array.foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode array.foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % array.foldr2(P, Array, !Acc1, !Acc2) is equivalent to
    %   list.foldr2(P, array.to_list(Array), !Acc1, !Acc2)
    % but more efficient.
    %
:- pred array.foldr2(pred(T1, T2, T2, T3, T3), array(T1), T2, T2, T3, T3).
:- mode array.foldr2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode array.foldr2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode array.foldr2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode array.foldr2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode array.foldr2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode array.foldr2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % array.map_foldl(P, A, B, !Acc):
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

    % array.map_corresponding_foldl(P, A, B, C, !Acc):
    %
    % Given two arrays A and B, invoke P(Aelt, Belt, Celt, !Acc) on
    % each corresponding pair of elements Aelt and Belt. Build up the array C
    % from the result Celt values. Return C and the final value of the
    % accumulator.
    %
    % C will have as many elements as A does. In most uses, B will also have
    % this many elements, but may have more; it may NOT have fewer.
    %
:- pred array.map_corresponding_foldl(pred(T1, T2, T3, T4, T4),
    array(T1), array(T2), array(T3), T4, T4).
:- mode array.map_corresponding_foldl(
    in(pred(in, in, out, in, out) is det),
    in, in, array_uo, in, out) is det.
:- mode array.map_corresponding_foldl(
    in(pred(in, in, out, mdi, muo) is det),
    in, in, array_uo, mdi, muo) is det.
:- mode array.map_corresponding_foldl(
    in(pred(in, in, out, di, uo) is det),
    in, in, array_uo, di, uo) is det.
:- mode array.map_corresponding_foldl(
    in(pred(in, in, out, in, out) is semidet),
    in, in, array_uo, in, out) is semidet.

    % array.append(A, B) = C:
    %
    % Make C a concatenation of the arrays A and B.
    %
:- func array.append(array(T)::in, array(T)::in) = (array(T)::array_uo) is det.

    % array.random_permutation(A0, A, RS0, RS) permutes the elements in
    % A0 given random seed RS0 and returns the permuted array in A
    % and the next random seed in RS.
    %
:- pred array.random_permutation(array(T)::array_di, array(T)::array_uo,
    random.supply::mdi, random.supply::muo) is det.

    % Convert an array to a pretty_printer.doc for formatting.
    %
:- func array.array_to_doc(array(T)) = pretty_printer.doc.
:- mode array.array_to_doc(array_ui) = out is det.

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

    % MR_ArrayPtr is defined in runtime/mercury_types.h.
:- pragma foreign_type("C", array(T), "MR_ArrayPtr")
    where equality is array.array_equal,
    comparison is array.array_compare.

:- pragma foreign_type("C#",  array(T), "System.Array")
    where equality is array.array_equal,
    comparison is array.array_compare.

:- pragma foreign_type("IL",  array(T), "class [mscorlib]System.Array")
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
:- pragma foreign_export("C", array_equal(in, in), "ML_array_equal").
:- pragma foreign_export("IL", array_equal(in, in), "ML_array_equal").
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

array_compare(A1, A2) = C :-
    array_compare(C, A1, A2).

    % compare/3 for arrays
    %
:- pred array_compare(comparison_result::uo, array(T)::in, array(T)::in)
    is det.
:- pragma foreign_export("C", array_compare(uo, in, in), "ML_array_compare").
:- pragma foreign_export("IL", array_compare(uo, in, in), "ML_array_compare").
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

:- pred array.compare_elements(int::in, int::in, array(T)::in, array(T)::in,
    comparison_result::uo) is det.

array.compare_elements(N, Size, Array1, Array2, Result) :-
    ( N = Size ->
        Result = (=)
    ;
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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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
ML_new_array(int Size, object Item, bool fill)
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
ML_array_resize(System.Array arr0, int Size, object Item)
{
    if (Size == 0) {
        return null;
    }
    if (arr0 == null) {
        return ML_new_array(Size, Item, true);
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
    Object[] as = new Object[Size];
    if (fill) {
        java.util.Arrays.fill(as, Item);
    }
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

array.init(N, X) = A :-
    array.init(N, X, A).

array.init(Size, Item, Array) :-
    ( Size < 0 ->
        error("array.init: negative size")
    ;
        array.init_2(Size, Item, Array)
    ).

:- pred array.init_2(int::in, T::in, array(T)::array_uo) is det.

:- pragma foreign_proc("C",
    array.init_2(Size::in, Item::in, Array::array_uo),
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

array.make_empty_array = A :-
    array.make_empty_array(A).

:- pragma foreign_proc("C",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ML_alloc_array(Array, 1, MR_ALLOC_ID);
    ML_init_array(Array, 0, 0);
").

:- pragma foreign_proc("C#",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_new_array(Size, Item, true);
").
:- pragma foreign_proc("C#",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX A better solution then using the null pointer to represent
    // the empty array would be to create an array of size 0.  However
    // we need to determine the element type of the array before we can
    // do that.  This could be done by examining the RTTI of the array
    // type and then using System.Type.GetType(""<mercury type>"") to
    // determine it.  However constructing the <mercury type> string is
    // a non-trival amount of work.
    Array = null;
").

:- pragma foreign_proc("Erlang",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = erlang:make_tuple(Size, Item)
").
:- pragma foreign_proc("Erlang",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = {}
").

:- pragma foreign_proc("Java",
    array.init_2(Size::in, Item::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_new_array(Size, Item, true);
").
:- pragma foreign_proc("Java",
    array.make_empty_array(Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX as per C#
    Array = null;
").

%-----------------------------------------------------------------------------%

array.min(A) = N :-
    array.min(A, N).

:- pragma foreign_proc("C",
    array.min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("C#",
    array.min(_Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

:- pragma foreign_proc("Erlang",
    array.min(Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % Array not used
    Min = 0
").

:- pragma foreign_proc("Java",
    array.min(_Array::in, Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Array not used */
    Min = 0;
").

array.max(A) = N :-
    array.max(A, N).

:- pragma foreign_proc("C",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Max = Array->size - 1;
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
:- pragma foreign_proc("Erlang",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = size(Array) - 1
").

:- pragma foreign_proc("Java",
    array.max(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Array != null) {
        Max = array.ML_array_size(Array) - 1;
    } else {
        Max = -1;
    }
").

array.bounds(Array, Min, Max) :-
    array.min(Array, Min),
    array.max(Array, Max).

%-----------------------------------------------------------------------------%

array.size(A) = N :-
    array.size(A, N).

:- pragma foreign_proc("C",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Max = Array->size;
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

:- pragma foreign_proc("Erlang",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = size(Array)
").

:- pragma foreign_proc("Java",
    array.size(Array::in, Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = jmercury.array.ML_array_size(Array);
").

%-----------------------------------------------------------------------------%

array.in_bounds(Array, Index) :-
    array.bounds(Array, Min, Max),
    Min =< Index, Index =< Max.

array.semidet_set(Index, Item, !Array) :-
    ( array.in_bounds(!.Array, Index) ->
        array.unsafe_set(Index, Item, !Array)
    ;
        fail
    ).

array.semidet_slow_set(Index, Item, !Array) :-
    ( array.in_bounds(!.Array, Index) ->
        array.slow_set(Index, Item, !Array)
    ;
        fail
    ).

array.slow_set(!.Array, N, X) = !:Array :-
    array.slow_set(N, X, !Array).

array.slow_set(Index, Item, !Array) :-
    array.copy(!Array),
    array.set(Index, Item, !Array).

%-----------------------------------------------------------------------------%

array.elem(Index, Array) = array.lookup(Array, Index).

array.unsafe_elem(Index, Array) = Elem :-
    array.unsafe_lookup(Array, Index, Elem).

array.lookup(Array, N) = X :-
    array.lookup(Array, N, X).

array.lookup(Array, Index, Item) :-
    ( bounds_checks, \+ array.in_bounds(Array, Index) ->
        out_of_bounds_error(Array, Index, "array.lookup")
    ;
        array.unsafe_lookup(Array, Index, Item)
    ).

array.semidet_lookup(Array, Index, Item) :-
    ( array.in_bounds(Array, Index) ->
        array.unsafe_lookup(Array, Index, Item)
    ;
        fail
    ).

:- pragma foreign_proc("C",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
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
    array.unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Item = Array.GetValue(Index);
}").

:- pragma foreign_proc("Erlang",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Item = element(Index + 1, Array)
").

:- pragma foreign_proc("Java",
    array.unsafe_lookup(Array::in, Index::in, Item::out),
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
    } else {
        Item = ((Object[]) Array)[Index];
    }
").

%-----------------------------------------------------------------------------%

'elem :='(Index, Array, Value) = array.set(Array, Index, Value).

array.set(A1, N, X) = A2 :-
    array.set(N, X, A1, A2).

array.set(Index, Item, !Array) :-
    ( bounds_checks, \+ array.in_bounds(!.Array, Index) ->
        out_of_bounds_error(!.Array, Index, "array.set")
    ;
        array.unsafe_set(Index, Item, !Array)
    ).

array.svset(Index, Item, !Array) :-
    ( bounds_checks, \+ array.in_bounds(!.Array, Index) ->
        out_of_bounds_error(!.Array, Index, "array.set")
    ;
        array.unsafe_svset(Index, Item, !Array)
    ).

:- pragma foreign_proc("C",
    array.unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
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

:- pragma foreign_proc("C",
    array.unsafe_svset(Index::in, Item::in, Array0::array_di, Array::array_uo),
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
    array.unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Array0.SetValue(Item, Index);   /* destructive update! */
    Array = Array0;
}").

:- pragma foreign_proc("C#",
    array.unsafe_svset(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Array0.SetValue(Item, Index);   /* destructive update! */
    Array = Array0;
}").

:- pragma foreign_proc("Erlang",
    array.unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = setelement(Index + 1, Array0, Item)
").

:- pragma foreign_proc("Erlang",
    array.unsafe_svset(Index::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = setelement(Index + 1, Array0, Item)
").

:- pragma foreign_proc("Java",
    array.unsafe_set(Index::in, Item::in, Array0::array_di, Array::array_uo),
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
    } else {
        ((Object[]) Array0)[Index] = Item;
    }
    Array = Array0;         /* destructive update! */
").

:- pragma foreign_proc("Java",
    array.unsafe_svset(Index::in, Item::in, Array0::array_di, Array::array_uo),
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
    } else {
        ((Object[]) Array0)[Index] = Item;
    }
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

array.resize(!.Array, N, X) = !:Array :-
    array.resize(N, X, !Array).

:- pragma foreign_proc("C",
    array.resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
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
    array.resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_array_resize(Array0, Size, Item);
").

:- pragma foreign_proc("Erlang",
    array.resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
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
    array.resize(Size::in, Item::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = jmercury.array.ML_array_resize(Array0, Size, Item);
").

%-----------------------------------------------------------------------------%

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

array.shrink(!.Array, N) = !:Array :-
    array.shrink(N, !Array).

array.shrink(Size, !Array) :-
    OldSize = array.size(!.Array),
    ( Size > OldSize ->
        error("array.shrink: can't shrink to a larger size")
    ; Size = OldSize ->
        true
    ;
        array.shrink_2(Size, !Array)
    ).

:- pred array.shrink_2(int::in, array(T)::array_di, array(T)::array_uo)
    is det.

:- pragma foreign_proc("C",
    array.shrink_2(Size::in, Array0::array_di, Array::array_uo),
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
    array.shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = array.ML_shrink_array(Array0, Size);
").

:- pragma foreign_proc("Erlang",
    array.shrink_2(Size::in, Array0::array_di, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = list_to_tuple(lists:sublist(tuple_to_list(Array0), Size))
").

:- pragma foreign_proc("Java",
    array.shrink_2(Size::in, Array0::array_di, Array::array_uo),
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

%-----------------------------------------------------------------------------%

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

array.copy(A1) = A2 :-
    array.copy(A1, A2).

:- pragma foreign_proc("C",
    array.copy(Array0::in, Array::array_uo),
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
    array.copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = (System.Array) Array0.Clone();
").

:- pragma foreign_proc("Erlang",
    array.copy(Array0::in, Array::array_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = Array0
").

:- pragma foreign_proc("Java",
    array.copy(Array0::in, Array::array_uo),
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

%-----------------------------------------------------------------------------%

array(List) = Array :-
    array.from_list(List, Array).

array.from_list(List) = Array :-
    array.from_list(List, Array).

array.from_list([], Array) :-
    array.make_empty_array(Array).
array.from_list(List, Array) :-
    List = [Head | Tail],
    list.length(List, Len),
    array.init(Len, Head, Array0),
    array.unsafe_insert_items(Tail, 1, Array0, Array).

%-----------------------------------------------------------------------------%

:- pred array.unsafe_insert_items(list(T)::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

array.unsafe_insert_items([], _N, !Array).
array.unsafe_insert_items([Head | Tail], N, !Array) :-
    array.unsafe_set(N, Head, !Array),
    array.unsafe_insert_items(Tail, N + 1, !Array).

%-----------------------------------------------------------------------------%

array.to_list(Array) = List :-
    array.to_list(Array, List).

array.to_list(Array, List) :-
    array.bounds(Array, Low, High),
    array.fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

array.fetch_items(Array, Low, High) = List :-
    array.fetch_items(Array, Low, High, List).

array.fetch_items(Array, Low, High, List) :-
    ( High < Low ->
        % If High is less than Low then there cannot be any array indexes
        % within the range Low -> High (inclusive).  This can happen when
        % calling to_list/2 on the empty array.  Testing for this condition
        % here rather than in to_list/2 is more general.
        List = []
    ;
        array.in_bounds(Array, Low),
        array.in_bounds(Array, High)
    ->
        List = do_foldr_func(func(X, Xs) = [X | Xs], Array, [], Low, High)
    ;
        error("array.fetch_items/4: One or more index is out of bounds")
    ).

%-----------------------------------------------------------------------------%

array.bsearch(A, X, F) = MN :-
    P = (pred(X1::in, X2::in, C::out) is det :- C = F(X1, X2)),
    array.bsearch(A, X, P, MN).

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
            ( Compare(El, X, (=)) ->
                Result = yes(Lo)
            ;
                Result = no
            )
        ;
            % Otherwise find the middle element of the range
            % and check against that.
            Mid = (Lo + Hi) >> 1,   % `>> 1' is hand-optimized `div 2'.
            array.lookup(Array, Mid, XMid),
            Compare(XMid, El, Comp),
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

array.map(F, A1) = A2 :-
    P = (pred(X::in, Y::out) is det :- Y = F(X)),
    array.map(P, A1, A2).

array.map(Closure, OldArray, NewArray) :-
    ( array.semidet_lookup(OldArray, 0, Elem0) ->
        array.size(OldArray, Size),
        Closure(Elem0, Elem),
        array.init(Size, Elem, NewArray0),
        array.map_2(1, Size, Closure, OldArray, NewArray0, NewArray)
    ;
        array.make_empty_array(NewArray)
    ).

:- pred array.map_2(int::in, int::in, pred(T1, T2)::in(pred(in, out) is det),
    array(T1)::in, array(T2)::array_di, array(T2)::array_uo) is det.

array.map_2(N, Size, Closure, OldArray, !NewArray) :-
    ( N >= Size ->
        true
    ;
        array.lookup(OldArray, N, OldElem),
        Closure(OldElem, NewElem),
        array.set(N, NewElem, !NewArray),
        array.map_2(N + 1, Size, Closure, OldArray, !NewArray)
    ).

%-----------------------------------------------------------------------------%

array.member(A, X) :-
    nondet_int_in_range(0, array.size(A) - 1, I0),
    X = A ^ elem(I0).

%-----------------------------------------------------------------------------%

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

array.binary_search(A, X, I) :-
    array.binary_search(ordering, A, X, I).

array.binary_search(Cmp, A, X, I) :-
    array.approx_binary_search(Cmp, A, X, I),
    A ^ elem(I) = X.

array.approx_binary_search(A, X, I) :-
    array.approx_binary_search(ordering, A, X, I).

array.approx_binary_search(Cmp, A, X, I) :-
    Lo = 0,
    Hi = array.size(A) - 1,
    approx_binary_search_2(Cmp, A, X, Lo, Hi, I).

:- pred approx_binary_search_2(comparison_func(T)::in, array(T)::array_ui,
    T::in, int::in, int::in, int::out) is semidet.

approx_binary_search_2(Cmp, A, X, Lo, Hi, I) :-
    Lo =< Hi,
    Mid = (Lo + Hi) / 2,
    O = Cmp(A ^ elem(Mid), X),
    (
        O = (>),
        approx_binary_search_2(Cmp, A, X, Lo, Mid - 1, I)
    ;
        O = (=),
        I = Mid
    ;
        O = (<),
        ( if ( Mid < Hi, X @< A ^ elem(Mid + 1) ; Mid = Hi ) then
            I = Mid
          else
            approx_binary_search_2(Cmp, A, X, Mid + 1, Hi, I)
        )
    ).

%-----------------------------------------------------------------------------%

array.append(A, B) = C :-
    SizeA = array.size(A),
    SizeB = array.size(B),
    SizeC = SizeA + SizeB,
    ( if
        ( if SizeA > 0 then
            InitElem = A ^ elem(0)
          else if SizeB > 0 then
            InitElem = B ^ elem(0)
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
    array.append(ArrayA::in, ArrayB::in) = (ArrayC::array_uo),
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

%-----------------------------------------------------------------------------%

array.random_permutation(A0, A, RS0, RS) :-
    Lo = array.min(A0),
    Hi = array.max(A0),
    Sz = array.size(A0),
    permutation_2(Lo, Lo, Hi, Sz, A0, A, RS0, RS).

:- pred permutation_2(int::in, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo,
    random.supply::mdi, random.supply::muo) is det.

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
    do_foldl_func(Fn, A, X, array.min(A), array.max(A)).

:- func do_foldl_func(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
%:- mode do_foldl_func(func(in, in) = out is det, array_ui, in, in, in)
%   = out is det.
:- mode do_foldl_func(func(in, in) = out is det, in, in, in, in) = out is det.
%:- mode do_foldl_func(func(in, di) = uo is det, array_ui, di, in, in)
%   = uo is det.
:- mode do_foldl_func(func(in, di) = uo is det, in, di, in, in) = uo is det.

do_foldl_func(Fn, A, X, I, Max) =
    ( Max < I ->
        X
    ;
        do_foldl_func(Fn, A, Fn(A ^ unsafe_elem(I), X), I + 1, Max)
    ).

% ---------------------------------------------------------------------------- %

array.foldl(P, A, !Acc) :-
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
    ( Max < I ->
        true
    ;
        P(A ^ unsafe_elem(I), !Acc),
        do_foldl_pred(P, A, I + 1, Max, !Acc)
    ).

%-----------------------------------------------------------------------------%

array.foldl2(P, A, !Acc1, !Acc2) :-
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
    ( Max < I ->
        true
    ;
        P(A ^ unsafe_elem(I), !Acc1, !Acc2),
        do_foldl2(P, I + 1, Max, A, !Acc1, !Acc2)
    ).

%-----------------------------------------------------------------------------%

array.foldr(Fn, A, X) =
    do_foldr_func(Fn, A, X, array.min(A), array.max(A)).

:- func do_foldr_func(func(T1, T2) = T2, array(T1), T2, int, int) = T2.
%:- mode do_foldr_func(func(in, in) = out is det, array_ui, in, in, in)
%   = out is det.
:- mode do_foldr_func(func(in, in) = out is det, in, in, in, in) = out is det.
%:- mode do_foldr_func(func(in, di) = uo is det, array_ui, di, in, in)
%   = uo is det.
:- mode do_foldr_func(func(in, di) = uo is det, in, di, in, in) = uo is det.

do_foldr_func(Fn, A, X, Min, I) =
    ( I < Min ->
        X
    ;
        do_foldr_func(Fn, A, Fn(A ^ unsafe_elem(I), X), Min, I - 1)
    ).

%-----------------------------------------------------------------------------%

array.foldr(P, A, !Acc) :-
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
    ( I < Min ->
        true
    ;
        P(A ^ unsafe_elem(I), !Acc),
        do_foldr_pred(P, Min, I - 1, A, !Acc)
    ).

%-----------------------------------------------------------------------------%

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
    ( I < Min ->
        true
    ;
        P(A ^ unsafe_elem(I), !Acc1, !Acc2),
        do_foldr2(P, Min, I - 1, A, !Acc1, !Acc2)
    ).

map_foldl(P, A, B, !Acc) :-
    N = array.size(A),
    ( if N =< 0 then
        B = array.make_empty_array
      else
        X = A ^ elem(0),
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

map_foldl_2(P, I, A, B0, B, !Acc) :-
    ( if I < array.size(A) then
        X = A ^ elem(I),
        P(X, Y, !Acc),
        B1 = B0 ^ elem(I) := Y,
        map_foldl_2(P, I + 1, A, B1, B, !Acc)
      else
        B = B0
    ).

array.map_corresponding_foldl(P, A, B, C, !Acc) :-
    N = array.size(A),
    ( if N =< 0 then
        C = array.make_empty_array
      else
        X = A ^ elem(0),
        Y = B ^ elem(0),
        P(X, Y, Z, !Acc),
        C1 = array.init(N, Z),
        array.map_corresponding_foldl_2(P, 1, N, A, B, C1, C, !Acc)
    ).

:- pred array.map_corresponding_foldl_2(pred(T1, T2, T3, T4, T4),
    int, int, array(T1), array(T2), array(T3), array(T3), T4, T4).
:- mode array.map_corresponding_foldl_2(
    in(pred(in, in, out, in, out) is det),
    in, in, in, in, array_di, array_uo, in, out) is det.
:- mode array.map_corresponding_foldl_2(
    in(pred(in, in, out, mdi, muo) is det),
    in, in, in, in, array_di, array_uo, mdi, muo) is det.
:- mode array.map_corresponding_foldl_2(
    in(pred(in, in, out, di, uo) is det),
    in, in, in, in, array_di, array_uo, di, uo) is det.
:- mode array.map_corresponding_foldl_2(
    in(pred(in, in, out, in, out) is semidet),
    in, in, in, in, array_di, array_uo, in, out) is semidet.

array.map_corresponding_foldl_2(P, I, N, A, B, C0, C, !D) :-
    ( if I < N then
        X = A ^ elem(I),
        Y = B ^ elem(I),
        P(X, Y, Z, !D),
        C1 = C0 ^ elem(I) := Z,
        array.map_corresponding_foldl_2(P, I + 1, N, A, B, C1, C, !D)
      else
        C = C0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
        merge_subarrays(A1, Lo, I - 1, I, J - 1, Lo, B1, B2),
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
        merge_subarrays(A2, Lo, J - 1, J, I - 1, Lo, B2, B)
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
        copy_subarray_reverse(A, Lo, I - 1, I - 1, B0, B)
    ;
        I = search_until((>), A, Lo, Hi),
        copy_subarray(A, Lo, I - 1, Lo, B0, B)
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

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI + (Ho - Lo + 1).
    % In this version, I is ascending, so B[InitI] gets A[Lo]
    %
:- pred copy_subarray(array(T)::array_ui, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray/6, T = int).
:- pragma type_spec(copy_subarray/6, T = string).

copy_subarray(A, Lo, Hi, I, B0, B) :-
    ( Lo =< Hi ->
        B1 = B0 ^ elem(I) := A ^ elem(Lo),
        copy_subarray(A, Lo + 1, Hi, I + 1, B1, B)
    ;
        B = B0
    ).

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI - (Ho - Lo + 1).
    % In this version, I is descending, so B[InitI] gets A[Hi].
    %
:- pred copy_subarray_reverse(array(T)::array_ui, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(copy_subarray_reverse/6, T = int).
:- pragma type_spec(copy_subarray_reverse/6, T = string).

copy_subarray_reverse(A, Lo, Hi, I, B0, B) :-
    ( Lo =< Hi ->
        B1 = B0 ^ elem(I) := A ^ elem(Lo),
        copy_subarray_reverse(A, Lo + 1, Hi, I - 1, B1, B)
    ;
        B = B0
    ).

%------------------------------------------------------------------------------%

    % merges the two sorted consecutive subarrays Lo1 .. Hi1 and
    % Lo2 .. Hi2 from A into the subarray starting at I in B.
    %
:- pred merge_subarrays(array(T)::array_ui,
    int::in, int::in, int::in, int::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

:- pragma type_spec(merge_subarrays/8, T = int).
:- pragma type_spec(merge_subarrays/8, T = string).

merge_subarrays(A, Lo1, Hi1, Lo2, Hi2, I, B0, B) :-
    ( Lo1 > Hi1 ->
        copy_subarray(A, Lo2, Hi2, I, B0, B)
    ; Lo2 > Hi2 ->
        copy_subarray(A, Lo1, Hi1, I, B0, B)
    ;
        X1 = A ^ elem(Lo1),
        X2 = A ^ elem(Lo2),
        compare(R, X1, X2),
        (
            R = (<),
            array.svset(I, X1, B0, B1),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, B1, B)
        ;
            R = (=),
            array.svset(I, X1, B0, B1),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, B1, B)
        ;
            R = (>),
            array.svset(I, X2, B0, B1),
            merge_subarrays(A, Lo1, Hi1, Lo2 + 1, Hi2, I + 1, B1, B)
        )
    ).

%------------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

array.least_index(A) = array.min(A).

array.greatest_index(A) = array.max(A).

%-----------------------------------------------------------------------------%

array.array_to_doc(A) =
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

%------------------------------------------------------------------------------%
:- end_module array.
%------------------------------------------------------------------------------%
