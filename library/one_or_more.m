%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: one_or_more.m.
% Stability: high.
%
% This module defines the one_or_more type, values of which represent
% nonempty lists, and various utility predicates that operate on them.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module one_or_more.
:- interface.

:- import_module list.
:- import_module pretty_printer.

%---------------------------------------------------------------------------%

:- type one_or_more(T)
    --->    one_or_more(T, list(T)).
            % The head, which is the one element that must be present,
            % and the tail of the list, which may be empty.

%---------------------------------------------------------------------------%

    % Convert from one_or_more to list.
    %
:- func one_or_more_to_list(one_or_more(T)) = list(T).
:- mode one_or_more_to_list(di) = uo is det.
:- mode one_or_more_to_list(in) = out is det.

    % Convert from list to one_or_more. The first version fails
    % if the list is empty, while the second throws an exception.
    %
:- pred list_to_one_or_more(list(T)::in, one_or_more(T)::out) is semidet.
:- pred det_list_to_one_or_more(list(T)::in, one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % Return the head and the tail respectively.
    %
:- func head(one_or_more(T)) = T.
:- func tail(one_or_more(T)) = list(T).

    % Add a new element to the front of a one_or_more.
    %
    % In list terms:
    %
    %   cons(X, Y) = Z <=> Z = [X | Y].
    %
:- func cons(T, one_or_more(T)) = one_or_more(T).
:- pred cons(T::in, one_or_more(T)::in, one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % Standard append predicate:
    % append(Start, End, List) is true iff
    % List is the result of concatenating Start and End.
    %
:- pred append(one_or_more(T), one_or_more(T), one_or_more(T)).
:- mode append(di, di, uo) is det.
:- mode append(in, in, out) is det.
:- mode append(in, in, in) is semidet.    % implied

    % L1 ++ L2 = L :- append(L1, L2, L).
    %
:- func one_or_more(T) ++ one_or_more(T) = one_or_more(T).

    % Append a list and a one_or_more, in either order.
    %
:- pred append_one_or_more_list(one_or_more(T), list(T), one_or_more(T)).
:- mode append_one_or_more_list(di, di, uo) is det.
:- mode append_one_or_more_list(in, in, out) is det.

:- pred append_list_one_or_more(list(T), one_or_more(T), one_or_more(T)).
:- mode append_list_one_or_more(di, di, uo) is det.
:- mode append_list_one_or_more(in, in, out) is det.

%---------------------%

    % Assert the associativity of append.
    %
:- promise all [A, B, C, ABC]
    (
        ( some [AB] (
            one_or_more.append(A, B, AB),
            one_or_more.append(AB, C, ABC))
        )
    <=>
        ( some [BC] (
            one_or_more.append(B, C, BC),
            one_or_more.append(A, BC, ABC))
        )
    ).

%---------------------------------------------------------------------------%

    % length(List, Length):
    %
    % True iff Length is the length of List, i.e. if List contains
    % Length elements.
    %
:- func length(one_or_more(T)) = int.
:- pred length(one_or_more(T)::in, int::out) is det.

    % same_length(ListA, ListB):
    %
    % True iff ListA and ListB have the same length,
    % i.e. iff they both contain the same number of elements.
    %
:- pred same_length(one_or_more(T1)::in, one_or_more(T2)::in) is semidet.

    % As above, but for three lists.
    %
:- pred same_length3(one_or_more(T1)::in, one_or_more(T2)::in,
    one_or_more(T3)::in) is semidet.

%---------------------------------------------------------------------------%

    % member(Elem, List):
    %
    % True iff List contains Elem.
    %
:- pred member(T, one_or_more(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is multi.

    % member_index0(Elem, List, Index).
    %
    % True iff List contains Elem at the zero-based index Index.
    %
:- pred member_index0(T, one_or_more(T), int).
:- mode member_index0(in, in, in) is semidet.
:- mode member_index0(in, in, out) is nondet.
:- mode member_index0(out, in, out) is multi.

    % member_indexes0(Elem, List, Indexes).
    %
    % True iff List contains Elem at the zero-based indexes Indexes.
    % Indexes will be sorted.
    %
:- pred member_indexes0(T::in, one_or_more(T)::in, list(int)::out) is det.

    % contains(List, Elem) iff member(Elem, List).
    % Sometimes you need the arguments in this order, because you want to
    % construct a closure with only the list.
    %
:- pred contains(one_or_more(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%

    % index*(List, Position, Elem):
    %
    % These predicates select an element in a list from it's position.
    % The `index0' preds consider the first element to be element
    % number zero, whereas the `index1' preds consider the first element
    % to be element number one. The `det_' preds call error/1 if the index
    % is out of range, whereas the semidet preds fail if the index is out of
    % range.
    %
:- pred index0(one_or_more(T)::in, int::in, T::out) is semidet.
:- pred index1(one_or_more(T)::in, int::in, T::out) is semidet.

:- func det_index0(one_or_more(T), int) = T.
:- pred det_index0(one_or_more(T)::in, int::in, T::out) is det.
:- func det_index1(one_or_more(T), int) = T.
:- pred det_index1(one_or_more(T)::in, int::in, T::out) is det.

    % nth_member_search(List, Elem, Position):
    %
    % Elem is the Position'th member of List.
    % (Position numbers start from 1.)
    % NOTE_TO_IMPLEMENTORS XXX This pred is identical
    % NOTE_TO_IMPLEMENTORS to index1_of_first_occurrence.
    %
:- pred nth_member_search(one_or_more(T)::in, T::in, int::out) is semidet.

    % A deterministic version of nth_member_search, which throws an exception
    % instead of failing if the element is not found in the list.
    % NOTE_TO_IMPLEMENTORS XXX This pred is identical
    % NOTE_TO_IMPLEMENTORS to det_index1_of_first_occurrence.
    %
:- pred nth_member_lookup(one_or_more(T)::in, T::in, int::out) is det.

    % index*_of_first_occurrence(List, Elem, Position):
    %
    % Computes the least value of Position such that
    % index*(List, Position, Elem). The `det_' funcs call error/1
    % if Elem is not a member of List.
    %
:- pred index0_of_first_occurrence(one_or_more(T)::in, T::in, int::out)
    is semidet.
:- pred index1_of_first_occurrence(one_or_more(T)::in, T::in, int::out)
    is semidet.
:- func det_index0_of_first_occurrence(one_or_more(T), T) = int.
:- func det_index1_of_first_occurrence(one_or_more(T), T) = int.

%---------------------------------------------------------------------------%

    % reverse(List, Reverse):
    %
    % Reverse is a list containing the same elements as List
    % but in reverse order.
    %
:- func reverse(one_or_more(T)) = one_or_more(T).
:- pred reverse(one_or_more(T), one_or_more(T)).
:- mode reverse(in, out) is det.
:- mode reverse(out, in) is det.

%---------------------------------------------------------------------------%

    % delete(List, Elem, Remainder):
    %
    % True iff Elem occurs in List, and Remainder is the result of
    % deleting one occurrence of Elem from List.
    %
:- pred delete(one_or_more(T)::in, T::in, list(T)::out) is nondet.

    % delete_first(List0, Elem, List) is true iff Elem occurs in List0
    % and List is List0 with the first occurrence of Elem removed.
    %
:- pred delete_first(one_or_more(T)::in, T::in, list(T)::out) is semidet.

    % delete_all(List0, Elem) = List is true iff List is List0 with
    % all occurrences of Elem removed.
    %
:- func delete_all(one_or_more(T), T) = list(T).
:- pred delete_all(one_or_more(T), T, list(T)).
:- mode delete_all(di, in, uo) is det.
:- mode delete_all(in, in, out) is det.

    % delete_elems(List0, Elems) = List is true iff List is List0 with
    % all occurrences of all elements of Elems removed.
    %
:- func delete_elems(one_or_more(T), list(T)) = list(T).
:- pred delete_elems(one_or_more(T)::in, list(T)::in, list(T)::out) is det.

    % sublist(SubList, FullList) is true if one can obtain SubList
    % by starting with FullList and deleting some of its elements.
    %
:- pred sublist(one_or_more(T)::in, one_or_more(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % replace(List0, D, R, List) is true iff List is List0
    % with an occurrence of D replaced with R.
    %
:- pred replace(one_or_more(T), T, T, one_or_more(T)).
:- mode replace(in, in, in, in) is semidet.
:- mode replace(in, in, in, out) is nondet.

    % replace_first(List0, D, R, List) is true iff List is List0
    % with the first occurrence of D replaced with R.
    %
:- pred replace_first(one_or_more(T)::in, T::in, T::in, one_or_more(T)::out)
    is semidet.

    % replace_all(List0, D, R) = List is true iff List is List0
    % with all occurrences of D replaced with R.
    %
:- func replace_all(one_or_more(T), T, T) = one_or_more(T).
:- pred replace_all(one_or_more(T)::in, T::in, T::in, one_or_more(T)::out)
    is det.

    % replace_nth(List0, N, R, List) is true iff List is List0
    % with N'th element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- pred replace_nth(one_or_more(T)::in, int::in, T::in, one_or_more(T)::out)
    is semidet.

    % det_replace_nth(List0, N, R) = List is true iff List is List0
    % with N'th element replaced with R.
    % Throws an exception if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func det_replace_nth(one_or_more(T), int, T) = one_or_more(T).
:- pred det_replace_nth(one_or_more(T)::in, int::in, T::in,
    one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % remove_dups(L0) = L:
    %
    % L is the result of deleting the second and subsequent occurrences
    % of every element that occurs twice in L0.
    %
:- func remove_dups(one_or_more(T)) = one_or_more(T).
:- pred remove_dups(one_or_more(T)::in, one_or_more(T)::out) is det.

    % remove_adjacent_dups(L0) = L:
    %
    % L is the result of replacing every sequence of duplicate elements in L0
    % with a single such element.
    %
:- func remove_adjacent_dups(one_or_more(T)) = one_or_more(T).
:- pred remove_adjacent_dups(one_or_more(T)::in, one_or_more(T)::out) is det.

    % remove_adjacent_dups(P, L0, L) is true iff L is the result
    % of replacing every sequence of elements in L0 which are equivalent
    % with respect to the ordering, with the first occurrence in L0 of
    % such an element.
    %
:- pred remove_adjacent_dups(comparison_pred(T)::in(comparison_pred),
    one_or_more(T)::in, one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % merge(L1, L2) = L:
    %
    % L is the result of merging the elements of L1 and L2, in ascending order.
    % L1 and L2 must be sorted.
    %
:- func merge(one_or_more(T), one_or_more(T)) = one_or_more(T).
:- pred merge(one_or_more(T)::in, one_or_more(T)::in, one_or_more(T)::out)
    is det.

    % merge(Compare, As, Bs) = Sorted is true iff, assuming As and
    % Bs are sorted with respect to the ordering defined by Compare,
    % Sorted is a list containing the elements of As and Bs which is
    % also sorted. For elements which are equivalent in the ordering,
    % if they come from the same list then they appear in the same
    % sequence in Sorted as they do in that list, otherwise the elements
    % from As appear before the elements from Bs.
    %
:- func merge(comparison_func(T), one_or_more(T), one_or_more(T))
    = one_or_more(T).
:- pred merge(comparison_pred(T)::in(comparison_pred),
    one_or_more(T)::in, one_or_more(T)::in, one_or_more(T)::out) is det.

    % merge_and_remove_dups(L1, L2) = L:
    %
    % L is the result of merging the elements of L1 and L2, in ascending order,
    % and eliminating any duplicates. L1 and L2 must be sorted and must each
    % not contain any duplicates.
    %
:- func merge_and_remove_dups(one_or_more(T), one_or_more(T)) = one_or_more(T).
:- pred merge_and_remove_dups(one_or_more(T)::in, one_or_more(T)::in,
    one_or_more(T)::out) is det.

    % merge_and_remove_dups(Compare, As, Bs) = Sorted is true iff,
    % assuming As and Bs are sorted with respect to the ordering defined
    % by Compare and neither contains any duplicates, Sorted is a list
    % containing the elements of As and Bs which is also sorted and
    % contains no duplicates. If an element from As is duplicated in
    % Bs (that is, they are equivalent in the ordering), then the element
    % from As is the one that appears in Sorted.
    %
:- func merge_and_remove_dups(comparison_func(T),
    one_or_more(T), one_or_more(T)) = one_or_more(T).
:- pred merge_and_remove_dups(comparison_pred(T)::in(comparison_pred),
    one_or_more(T)::in, one_or_more(T)::in, one_or_more(T)::out) is det.

%---------------------%

    % sort(List) = SortedList:
    %
    % SortedList is List sorted.
    %
:- func sort(one_or_more(T)) = one_or_more(T).
:- pred sort(one_or_more(T)::in, one_or_more(T)::out) is det.

    % sort_and_remove_dups(List) = SortedList:
    %
    % SortedList is List sorted with the second and subsequent occurrence of
    % any duplicates removed.
    %
:- func sort_and_remove_dups(one_or_more(T)) = one_or_more(T).
:- pred sort_and_remove_dups(one_or_more(T)::in, one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % sort(Compare, Unsorted) = Sorted is true iff Sorted is a
    % list containing the same elements as Unsorted, where Sorted is
    % sorted with respect to the ordering defined by Compare,
    % and the elements that are equivalent in this ordering appear
    % in the same sequence in Sorted as they do in Unsorted
    % (that is, the sort is stable).
    %
:- func sort(comparison_func(T), one_or_more(T)) = one_or_more(T).
:- pred sort(comparison_pred(T)::in(comparison_pred),
    one_or_more(T)::in, one_or_more(T)::out) is det.

    % sort_and_remove_dups(Compare, Unsorted, Sorted) is true iff
    % Sorted is a list containing the same elements as Unsorted, where
    % Sorted is sorted with respect to the ordering defined by the
    % predicate term Compare, except that if two elements in Unsorted
    % are equivalent with respect to this ordering only the one which
    % occurs first will be in Sorted.
    %
:- pred sort_and_remove_dups(comparison_pred(T)::in(comparison_pred),
    one_or_more(T)::in, one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % split_list(N, List, Start, End):
    %
    % splits List into a prefix Start of length N, and a remainder
    % End. Fails if N is not in `0 .. length(List)'.
    % See also: take, drop and split_upto.
    %
:- pred split_list(int::in, one_or_more(T)::in, list(T)::out, list(T)::out)
    is semidet.

    % det_split_list(N, List, Start, End):
    %
    % A deterministic version of split_list, which throws an exception
    % instead of failing if N is not in 0 .. length(List).
    %
:- pred det_split_list(int::in, one_or_more(T)::in, list(T)::out, list(T)::out)
    is det.

    % split_upto(N, List, Start, End):
    %
    % splits List into a prefix Start of length `min(N, length(List))',
    % and a remainder End. Throws an exception if N < 0.
    % See also: split_list, take, drop.
    %
:- pred split_upto(int::in, one_or_more(T)::in, list(T)::out, list(T)::out)
    is det.

%---------------------%

    % last(List, Last) is true if Last is the last element of List.
    %
:- func last(one_or_more(T)) = T.
:- pred last(one_or_more(T)::in, T::out) is det.

    % split_last(List, AllButLast, Last) is true if Last is the
    % last element of List and AllButLast is the list of elements before it.
    %
:- pred split_last(one_or_more(T)::in, list(T)::out, T::out) is det.

%---------------------------------------------------------------------------%

    % all_same(List) is true if all elements of the list are the same.
    %
:- pred all_same(one_or_more(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % condense(ListOfOoMs) = List:
    %
    % List is the result of concatenating all the elements of ListOfOoMs.
    %
:- func condense(list(one_or_more(T))) = list(T).
:- pred condense(list(one_or_more(T))::in, list(T)::out) is det.

    % chunk(List, ChunkSize) = Chunks:
    %
    % Takes a list List and breaks it into a list of lists Chunks,
    % such that the length of each list in Chunks is at most ChunkSize.
    % (More precisely, the length of each list in Chunks other than the
    % last one is exactly ChunkSize, and the length of the last list in
    % Chunks is between one and ChunkSize.)
    %
:- func chunk(one_or_more(T), int) = one_or_more(one_or_more(T)).
:- pred chunk(one_or_more(T)::in, int::in, one_or_more(one_or_more(T))::out)
    is det.

%---------------------------------------------------------------------------%

    % zip(ListA, ListB) = List:
    %
    % List is the result of alternating the elements of ListA and ListB,
    % starting with the first element of ListA (followed by the first element
    % of ListB, then the second element of listA, then the second element
    % of ListB, etc.). When there are no more elements remaining in one of
    % the lists, the remainder of the nonempty list is appended.
    %
:- func zip(one_or_more(T), one_or_more(T)) = one_or_more(T).
:- pred zip(one_or_more(T)::in, one_or_more(T)::in,
    one_or_more(T)::out) is det.

%---------------------------------------------------------------------------%

    % perm(List0, List):
    %
    % True iff List is a permutation of List0.
    %
:- pred perm(one_or_more(T)::in, one_or_more(T)::out) is multi.

%---------------------------------------------------------------------------%

    % Convert a list to a pretty_printer.doc for formatting.
    %
:- func one_or_more_to_doc(one_or_more(T)) = pretty_printer.doc.
:- pragma obsolete(func(one_or_more_to_doc/1),
    [pretty_printer.one_or_more_to_doc/1]).

%---------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%---------------------------------------------------------------------------%

    % find_first_match(Pred, List, FirstMatch) takes a closure with one
    % input argument. It returns the first element X of the list (if any)
    % for which Pred(X) is true.
    %
:- pred find_first_match(pred(T)::in(pred(in) is semidet), one_or_more(T)::in,
    T::out) is semidet.

    % any_true(Pred, List):
    % Succeeds iff Pred succeeds for at least one element of List.
    % Same as `not all_false(Pred, List)'.
    %
:- pred any_true(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    is semidet.

    % any_false(Pred, List):
    % Succeeds iff Pred fails for at least one element of List.
    % Same as `not all_true(Pred, List)'.
    %
:- pred any_false(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    is semidet.

    % all_true(Pred, List) takes a closure with one input argument.
    % If Pred succeeds for every member of List, all_true succeeds.
    % If Pred fails for any member of List, all_true fails.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    is semidet.

    % all_false(Pred, List) takes a closure with one input argument.
    % If Pred fails for every member of List, all_false succeeds.
    % If Pred succeeds for any member of List, all_false fails.
    %
:- pred all_false(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    is semidet.

    % all_true_corresponding(Pred, ListA, ListB):
    % Succeeds if Pred succeeds for every corresponding pair of elements from
    % ListA and ListB. Fails if Pred fails for any pair of corresponding
    % elements.
    %
    % An exception is raised if the list arguments differ in length.
    %
:- pred all_true_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    one_or_more(X)::in, one_or_more(Y)::in) is semidet.

    % all_false_corresponding(Pred, ListA, ListB):
    % Succeeds if Pred fails for every corresponding pair of elements from
    % ListA and ListB. Fails if Pred succeeds for any pair of corresponding
    % elements.
    %
    % An exception is raised if the list arguments differ in length.
    %
:- pred all_false_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    one_or_more(X)::in, one_or_more(Y)::in) is semidet.

%---------------------------------------------------------------------------%

    % filter(Pred, List) = TrueList takes a closure with one
    % input argument and for each member X of List, calls the closure.
    % X is included in TrueList iff Pred(X) is true.
    %
:- func filter(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    = (list(T)::out) is det.
:- pred filter(pred(T)::in(pred(in) is semidet), one_or_more(T)::in,
    list(T)::out) is det.

    % filter(Pred, List, TrueList, FalseList) takes a closure with one
    % input argument and for each member X of List, calls the closure.
    % X is included in TrueList iff Pred(T) is true.
    % X is included in FalseList iff Pred(T) is false.
    %
:- pred filter(pred(T)::in(pred(in) is semidet), one_or_more(T)::in,
    list(T)::out, list(T)::out) is det.

    % negated_filter(Pred, List) = FalseList takes a closure with one
    % input argument and for each member of List X, calls the closure.
    % X is included in FalseList iff Pred(X) is false.
    %
:- func negated_filter(pred(T)::in(pred(in) is semidet), one_or_more(T)::in)
    = (list(T)::out) is det.
:- pred negated_filter(pred(T)::in(pred(in) is semidet), one_or_more(T)::in,
    list(T)::out) is det.

    % filter_map(Transformer, List, TrueList) takes a semidet function
    % and calls it with each element of List. If a call succeeds, then
    % its return value is included in TrueList.
    %
:- func filter_map(func(X) = Y, one_or_more(X)) = list(Y).
:- mode filter_map(in(func(in) = out is semidet), in) = out is det.

    % filter_map(Transformer, List, TrueList) takes a predicate
    % with one input argument and one output argument, and calls it
    % with each element of List. If a call succeeds, then
    % its output is included in TrueList.
    %
:- pred filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    one_or_more(X)::in, list(Y)::out) is det.

    % filter_map(Transformer, List, TrueList, FalseList) takes
    % a predicate with one input argument and one output argument.
    % It is called with each element of List. If a call succeeds,
    % then the output is included in TrueList; otherwise, the failing
    % input is included in FalseList.
    %
:- pred filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    one_or_more(X)::in, list(Y)::out, list(X)::out) is det.

    % Same as filter_map/3 except that it only returns the first match:
    %
    %   find_first_map(X, Y, Z) <=> filter_map(X, Y, [Z | _])
    %
:- pred find_first_map(pred(X, Y)::in(pred(in, out) is semidet),
    one_or_more(X)::in, Y::out) is semidet.

    % Same as find_first_map, except with two outputs.
    %
:- pred find_first_map2(pred(X, A, B)::in(pred(in, out, out) is semidet),
    one_or_more(X)::in, A::out, B::out) is semidet.

    % Same as find_first_map, except with three outputs.
    %
:- pred find_first_map3(
    pred(X, A, B, C)::in(pred(in, out, out, out) is semidet),
    one_or_more(X)::in, A::out, B::out, C::out) is semidet.

    % find_index_of_match(Match, List, Index0, Index)
    %
    % Find the index of the first item in List for which Match is true,
    % where the first element in the list has the index Index0.
    % (Index0 is *not* the number of items to skip at the head of List.)
    %
:- pred find_index_of_match(pred(T), one_or_more(T), int, int).
:- mode find_index_of_match(in(pred(in) is semidet), in, in, out) is semidet.

%---------------------------------------------------------------------------%

    % map(T, L) = M:
    % map(T, L, M):
    %
    % Apply the closure T to transform the elements of L
    % into the elements of M.
    %
:- func map(func(X) = Y, one_or_more(X)) = one_or_more(Y).
:- pred map(pred(X, Y), one_or_more(X), one_or_more(Y)).
:- mode map(in(pred(in, out) is det), in, out) is det.
:- mode map(in(pred(in, out) is cc_multi), in, out) is cc_multi.
:- mode map(in(pred(in, out) is semidet), in, out) is semidet.
:- mode map(in(pred(in, out) is multi), in, out) is multi.
:- mode map(in(pred(in, out) is nondet), in, out) is nondet.
:- mode map(in(pred(in, in) is semidet), in, in) is semidet.

    % map2(T, L, M1, M2) uses the closure T
    % to transform the elements of L into the elements of M1 and M2.
    %
:- pred map2(pred(A, B, C), one_or_more(A), one_or_more(B), one_or_more(C)).
:- mode map2(in(pred(in, out, out) is det), in, out, out) is det.
:- mode map2(in(pred(in, out, out) is cc_multi), in, out, out) is cc_multi.
:- mode map2(in(pred(in, out, out) is semidet), in, out, out) is semidet.
:- mode map2(in(pred(in, out, out) is multi), in, out, out) is multi.
:- mode map2(in(pred(in, out, out) is nondet), in, out, out) is nondet.
:- mode map2(in(pred(in, in, in) is semidet), in, in, in) is semidet.

    % map3(T, L, M1, M2, M3) uses the closure T
    % to transform the elements of L into the elements of M1, M2 and M3.
    %
:- pred map3(pred(A, B, C, D),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D)).
:- mode map3(in(pred(in, out, out, out) is det), in, out, out, out) is det.
:- mode map3(in(pred(in, out, out, out) is cc_multi), in, out, out, out)
    is cc_multi.
:- mode map3(in(pred(in, out, out, out) is semidet), in, out, out, out)
    is semidet.
:- mode map3(in(pred(in, out, out, out) is multi), in, out, out, out)
    is multi.
:- mode map3(in(pred(in, out, out, out) is nondet), in, out, out, out)
    is nondet.
:- mode map3(in(pred(in, in, in, in) is semidet), in, in, in, in) is semidet.

    % map4(T, L, M1, M2, M3, M4) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3 and M4.
    %
:- pred map4(pred(A, B, C, D, E),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D),
    one_or_more(E)).
:- mode map4(in(pred(in, out, out, out, out) is det), in, out, out, out, out)
    is det.
:- mode map4(in(pred(in, out, out, out, out) is cc_multi), in, out, out, out,
    out) is cc_multi.
:- mode map4(in(pred(in, out, out, out, out) is semidet), in, out, out, out,
    out) is semidet.
:- mode map4(in(pred(in, out, out, out, out) is multi), in, out, out, out,
    out) is multi.
:- mode map4(in(pred(in, out, out, out, out) is nondet), in, out, out, out,
    out) is nondet.
:- mode map4(in(pred(in, in, in, in, in) is semidet), in, in, in, in, in)
    is semidet.

    % map5(T, L, M1, M2, M3, M4, M5) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4
    % and M5.
    %
:- pred map5(pred(A, B, C, D, E, F),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D),
    one_or_more(E), one_or_more(F)).
:- mode map5(in(pred(in, out, out, out, out, out) is det), in, out, out, out,
    out, out) is det.
:- mode map5(in(pred(in, out, out, out, out, out) is cc_multi), in, out, out,
    out, out, out) is cc_multi.
:- mode map5(in(pred(in, out, out, out, out, out) is semidet), in, out, out,
    out, out, out) is semidet.
:- mode map5(in(pred(in, out, out, out, out, out) is multi), in, out, out,
    out, out, out) is multi.
:- mode map5(in(pred(in, out, out, out, out, out) is nondet), in, out, out,
    out, out, out) is nondet.
:- mode map5(in(pred(in, in, in, in, in, in) is semidet), in, in, in, in, in,
    in) is semidet.

    % map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5 and M6.
    %
:- pred map6(pred(A, B, C, D, E, F, G),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D),
    one_or_more(E), one_or_more(F), one_or_more(G)).
:- mode map6(in(pred(in, out, out, out, out, out, out) is det), in, out, out,
    out, out, out, out) is det.
:- mode map6(in(pred(in, out, out, out, out, out, out) is cc_multi), in, out,
    out, out, out, out, out) is cc_multi.
:- mode map6(in(pred(in, out, out, out, out, out, out) is semidet), in, out,
    out, out, out, out, out) is semidet.
:- mode map6(in(pred(in, out, out, out, out, out, out) is multi), in, out,
    out, out, out, out, out) is multi.
:- mode map6(in(pred(in, out, out, out, out, out, out) is nondet), in, out,
    out, out, out, out, out) is nondet.
:- mode map6(in(pred(in, in, in, in, in, in, in) is semidet), in, in, in, in,
    in, in, in) is semidet.

    % map7(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6 and M7.
    %
:- pred map7(pred(A, B, C, D, E, F, G, H),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D),
    one_or_more(E), one_or_more(F), one_or_more(G), one_or_more(H)).
:- mode map7(in(pred(in, out, out, out, out, out, out, out) is det),
    in, out, out, out, out, out, out, out) is det.
:- mode map7(in(pred(in, out, out, out, out, out, out, out) is cc_multi),
    in, out, out, out, out, out, out, out) is cc_multi.
:- mode map7(in(pred(in, out, out, out, out, out, out, out) is semidet),
    in, out, out, out, out, out, out, out) is semidet.
:- mode map7(in(pred(in, out, out, out, out, out, out, out) is multi),
    in, out, out, out, out, out, out, out) is multi.
:- mode map7(in(pred(in, out, out, out, out, out, out, out) is nondet),
    in, out, out, out, out, out, out, out) is nondet.
:- mode map7(in(pred(in, in, in, in, in, in, in, in) is semidet),
    in, in, in, in, in, in, in, in) is semidet.

    % map8(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6, M7 and M8.
    %
:- pred map8(pred(A, B, C, D, E, F, G, H, I),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D),
    one_or_more(E), one_or_more(F), one_or_more(G), one_or_more(H),
    one_or_more(I)).
:- mode map8(in(pred(in, out, out, out, out, out, out, out, out) is det),
    in, out, out, out, out, out, out, out, out) is det.
:- mode map8(in(pred(in, out, out, out, out, out, out, out, out) is cc_multi),
    in, out, out, out, out, out, out, out, out) is cc_multi.
:- mode map8(in(pred(in, out, out, out, out, out, out, out, out) is semidet),
    in, out, out, out, out, out, out, out, out) is semidet.
:- mode map8(in(pred(in, out, out, out, out, out, out, out, out) is multi),
    in, out, out, out, out, out, out, out, out) is multi.
:- mode map8(in(pred(in, out, out, out, out, out, out, out, out) is nondet),
    in, out, out, out, out, out, out, out, out) is nondet.
:- mode map8(in(pred(in, in, in, in, in, in, in, in, in) is semidet),
    in, in, in, in, in, in, in, in, in) is semidet.

%---------------------%

    % map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
    %   [F(A1, B1), .., F(An, Bn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func map_corresponding(func(A, B) = R, one_or_more(A), one_or_more(B))
    = one_or_more(R).
:- pred map_corresponding(pred(A, B, R), one_or_more(A), one_or_more(B),
    one_or_more(R)).
:- mode map_corresponding(in(pred(in, in, out) is det), in, in, out)
    is det.
:- mode map_corresponding(in(pred(in, in, out) is semidet), in, in, out)
    is semidet.

    % map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func map_corresponding3(func(A, B, C) = R,
    one_or_more(A), one_or_more(B), one_or_more(C)) = one_or_more(R).
:- pred map_corresponding3(pred(A, B, C, R), one_or_more(A), one_or_more(B),
    one_or_more(C), one_or_more(R)).
:- mode map_corresponding3(in(pred(in, in, in, out) is det),
    in, in, in, out) is det.
:- mode map_corresponding3(in(pred(in, in, in, out) is semidet),
    in, in, in, out) is semidet.

%---------------------%

    % filter_map_corresponding/3 is like map_corresponding/3
    % except the function argument is semidet and the output list
    % consists of only those applications of the function argument that
    % succeeded.
    %
:- func filter_map_corresponding(
    (func(A, B) = R)::in(func(in, in) = out is semidet),
    one_or_more(A)::in, one_or_more(B)::in) = (list(R)::out) is det.
:- pred filter_map_corresponding(
    pred(A, B, R)::in(pred(in, in, out) is semidet),
    one_or_more(A)::in, one_or_more(B)::in, list(R)::out) is det.

    % filter_map_corresponding3/4 is like map_corresponding3/4
    % except the function argument is semidet and the output list
    % consists of only those applications of the function argument that
    % succeeded.
    %
:- func filter_map_corresponding3(
    (func(A, B, C) = R)::in(func(in, in, in) = out is semidet),
    one_or_more(A)::in, one_or_more(B)::in, one_or_more(C)::in)
    = (list(R)::out) is det.
:- pred filter_map_corresponding3(
    pred(A, B, C, R)::in(pred(in, in, in, out) is semidet),
    one_or_more(A)::in, one_or_more(B)::in, one_or_more(C)::in,
    list(R)::out) is det.

%---------------------------------------------------------------------------%

    % foldl(Func, List, Start) = End calls Func with each element of List
    % (working left-to-right) and an accumulator (with the initial value
    % of Start), and returns the final value in End.
    %
:- func foldl(func(L, A) = A, one_or_more(L), A) = A.

    % foldl(Pred, List, Start, End) calls Pred with each element of List
    % (working left-to-right) and an accumulator (with the initial value
    % of Start), and returns the final value in End.
    %
:- pred foldl(pred(L, A, A), one_or_more(L), A, A).
:- mode foldl(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldl(in(pred(in, in, out) is multi), in, in, out) is multi.
:- mode foldl(in(pred(in, in, out) is nondet), in, in, out) is nondet.
:- mode foldl(in(pred(in, mdi, muo) is nondet), in, mdi, muo) is nondet.
:- mode foldl(in(pred(in, in, out) is cc_multi), in, in, out) is cc_multi.
:- mode foldl(in(pred(in, di, uo) is cc_multi), in, di, uo) is cc_multi.

    % foldl2(Pred, List, !Acc1, !Acc2):
    % Does the same job as foldl, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl2(pred(L, A, A, Z, Z), one_or_more(L), A, A, Z, Z).
:- mode foldl2(in(pred(in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldl2(in(pred(in, di, uo, di, uo) is det),
    in, di, uo, di, uo) is det.
:- mode foldl2(in(pred(in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.
:- mode foldl2(in(pred(in, in, out, in, out) is nondet),
    in, in, out, in, out) is nondet.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is nondet),
    in, in, out, mdi, muo) is nondet.
:- mode foldl2(in(pred(in, in, out, in, out) is cc_multi),
    in, in, out, in, out) is cc_multi.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is cc_multi),
    in, in, out, mdi, muo) is cc_multi.
:- mode foldl2(in(pred(in, in, out, di, uo) is cc_multi),
    in, in, out, di, uo) is cc_multi.
:- mode foldl2(in(pred(in, di, uo, di, uo) is cc_multi),
    in, di, uo, di, uo) is cc_multi.

    % foldl3(Pred, List, !Acc1, !Acc2, !Acc3):
    % Does the same job as foldl, but with three accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl3(pred(L, A, A, B, B, C, C), one_or_more(L),
    A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is nondet),
    in, in, out, in, out, in, out) is nondet.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is nondet),
    in, in, out, in, out, mdi, muo) is nondet.
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is cc_multi),
    in, in, out, in, out, in, out) is cc_multi.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is cc_multi),
    in, in, out, in, out, di, uo) is cc_multi.

    % foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4):
    % Does the same job as foldl, but with four accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl4(pred(L, A, A, B, B, C, C, D, D), one_or_more(L),
    A, A, B, B, C, C, D, D).
:- mode foldl4(in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, in, out) is cc_multi),
    in, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, di, uo) is cc_multi),
    in, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, in, out) is nondet),
    in, in, out, in, out, in, out, in, out) is nondet.
:- mode foldl4(in(pred(in, in, out, in, out, in, out, mdi, muo) is nondet),
    in, in, out, in, out, in, out, mdi, muo) is nondet.

    % foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5):
    % Does the same job as foldl, but with five accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), one_or_more(L),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is nondet),
    in, in, out, in, out, in, out, in, out, in, out) is nondet.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is nondet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is nondet.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is cc_multi),
    in, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl5(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi),
    in, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

    % foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6):
    % Does the same job as foldl, but with six accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F), one_or_more(L),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl6(in(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

%---------------------%

    % foldr(Func, List, Start) = End calls Func with each element of List
    % (working right-to-left) and an accumulator (with the initial value
    % of Start), and returns the final value in End.
    %
:- func foldr(func(L, A) = A, one_or_more(L), A) = A.

    % foldr(Pred, List, Start, End) calls Pred with each element of List
    % (working right-to-left) and an accumulator (with the initial value
    % of Start), and returns the final value in End.
    %
:- pred foldr(pred(L, A, A), one_or_more(L), A, A).
:- mode foldr(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldr(in(pred(in, in, out) is multi), in, in, out) is multi.
:- mode foldr(in(pred(in, in, out) is nondet), in, in, out) is nondet.
:- mode foldr(in(pred(in, mdi, muo) is nondet), in, mdi, muo) is nondet.
:- mode foldr(in(pred(in, di, uo) is cc_multi), in, di, uo) is cc_multi.
:- mode foldr(in(pred(in, in, out) is cc_multi), in, in, out) is cc_multi.

    % foldr2(Pred, List, !Acc1, !Acc2):
    % Does the same job as foldr, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldr2(pred(L, A, A, B, B), one_or_more(L), A, A, B, B).
:- mode foldr2(in(pred(in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is det), in, in, out,
    mdi, muo) is det.
:- mode foldr2(in(pred(in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldr2(in(pred(in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is semidet), in, in, out,
    mdi, muo) is semidet.
:- mode foldr2(in(pred(in, in, out, di, uo) is semidet), in, in, out,
    di, uo) is semidet.
:- mode foldr2(in(pred(in, in, out, in, out) is nondet), in, in, out,
    in, out) is nondet.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is nondet), in, in, out,
    mdi, muo) is nondet.

    % foldr3(Pred, List, !Acc1, !Acc2, !Acc3):
    % Does the same job as foldr, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldr3(pred(L, A, A, B, B, C, C), one_or_more(L), A, A, B, B, C, C).
:- mode foldr3(in(pred(in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode foldr3(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, mdi, muo) is det.
:- mode foldr3(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode foldr3(in(pred(in, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out) is semidet.
:- mode foldr3(in(pred(in, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, di, uo) is semidet.
:- mode foldr3(in(pred(in, in, out, in, out, in, out) is nondet), in,
    in, out, in, out, in, out) is nondet.
:- mode foldr3(in(pred(in, in, out, in, out, mdi, muo) is nondet), in,
    in, out, in, out, mdi, muo) is nondet.

%---------------------%

    % foldl_corresponding(P, As, Bs, !Acc):
    %
    % Does the same job as foldl, but works on two lists in parallel.
    % An exception is raised if the list arguments differ in length.
    %
:- pred foldl_corresponding(pred(A, B, C, C),
    one_or_more(A), one_or_more(B), C, C).
:- mode foldl_corresponding(in(pred(in, in, in, out) is det),
    in, in, in, out) is det.
:- mode foldl_corresponding(in(pred(in, in, mdi, muo) is det),
    in, in, mdi, muo) is det.
:- mode foldl_corresponding(in(pred(in, in, di, uo) is det),
    in, in, di, uo) is det.
:- mode foldl_corresponding(in(pred(in, in, in, out) is semidet),
    in, in, in, out) is semidet.
:- mode foldl_corresponding(in(pred(in, in, mdi, muo) is semidet),
    in, in, mdi, muo) is semidet.
:- mode foldl_corresponding(in(pred(in, in, di, uo) is semidet),
    in, in, di, uo) is semidet.
:- mode foldl_corresponding(in(pred(in, in, in, out) is nondet),
    in, in, in, out) is nondet.
:- mode foldl_corresponding(in(pred(in, in, mdi, muo) is nondet),
    in, in, mdi, muo) is nondet.
:- mode foldl_corresponding(in(pred(in, in, in, out) is cc_multi),
    in, in, in, out) is cc_multi.
:- mode foldl_corresponding(in(pred(in, in, di, uo) is cc_multi),
    in, in, di, uo) is cc_multi.

:- func foldl_corresponding(func(A, B, C) = C,
    one_or_more(A), one_or_more(B), C) = C.

    % foldl2_corresponding(F, As, Bs, !Acc1, !Acc2):
    % Does the same job as foldl_corresponding, but has two
    % accumulators.
    %
:- pred foldl2_corresponding(pred(A, B, C, C, D, D),
    one_or_more(A), one_or_more(B), C, C, D, D).
:- mode foldl2_corresponding(in(pred(in, in, in, out, in, out) is det),
    in, in, in, out, in, out) is det.
:- mode foldl2_corresponding(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, in, out, mdi, muo) is det.
:- mode foldl2_corresponding(in(pred(in, in, in, out, di, uo) is det),
    in, in, in, out, di, uo) is det.
:- mode foldl2_corresponding(in(pred(in, in, in, out, in, out) is semidet),
    in, in, in, out, in, out) is semidet.
:- mode foldl2_corresponding(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, in, out, mdi, muo) is semidet.
:- mode foldl2_corresponding(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, in, out, di, uo) is semidet.
:- mode foldl2_corresponding(in(pred(in, in, in, out, in, out) is nondet),
    in, in, in, out, in, out) is nondet.
:- mode foldl2_corresponding(in(pred(in, in, in, out, mdi, muo) is nondet),
    in, in, in, out, mdi, muo) is nondet.
:- mode foldl2_corresponding(in(pred(in, in, in, out, in, out) is cc_multi),
    in, in, in, out, in, out) is cc_multi.
:- mode foldl2_corresponding(in(pred(in, in, in, out, di, uo) is cc_multi),
    in, in, in, out, di, uo) is cc_multi.

    % foldl3_corresponding(F, As, Bs, !Acc1, !Acc2, !Acc3):
    % Does the same job as foldl_corresponding, but has three
    % accumulators.
    %
:- pred foldl3_corresponding(pred(A, B, C, C, D, D, E, E),
    one_or_more(A), one_or_more(B), C, C, D, D, E, E).
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, in, out) is det), in, in, in, out,
    in, out, in, out) is det.
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, mdi, muo) is det), in, in, in, out,
    in, out, mdi, muo) is det.
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, di, uo) is det), in, in, in, out,
    in, out, di, uo) is det.
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, in, out) is semidet), in, in, in, out,
    in, out, in, out) is semidet.
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, mdi, muo) is semidet), in, in, in, out,
    in, out, mdi, muo) is semidet.
:- mode foldl3_corresponding(
    in(pred(in, in, in, out, in, out, di, uo) is semidet), in, in, in, out,
    in, out, di, uo) is semidet.

    % foldl_corresponding3(P, As, Bs, Cs, !Acc):
    % Like foldl_corresponding but folds over three corresponding
    % lists.
    %
:- pred foldl_corresponding3(pred(A, B, C, D, D),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D).
:- mode foldl_corresponding3(in(pred(in, in, in, in, out) is det),
    in, in, in, in, out) is det.
:- mode foldl_corresponding3(in(pred(in, in, in, mdi, muo) is det),
    in, in, in, mdi, muo) is det.
:- mode foldl_corresponding3(in(pred(in, in, in, di, uo) is det),
    in, in, in, di, uo) is det.
:- mode foldl_corresponding3(in(pred(in, in, in, in, out) is semidet),
    in, in, in, in, out) is semidet.
:- mode foldl_corresponding3(in(pred(in, in, in, mdi, muo) is semidet),
    in, in, in, mdi, muo) is semidet.
:- mode foldl_corresponding3(in(pred(in, in, in, di, uo) is semidet),
    in, in, in, di, uo) is semidet.

    % foldl2_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2):
    % like foldl_corresponding3 but with two accumulators.
    %
:- pred foldl2_corresponding3(pred(A, B, C, D, D, E, E),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D, E, E).
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, in, out) is det),
    in, in, in, in, out, in, out) is det.
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, mdi, muo) is det),
    in, in, in, in, out, mdi, muo) is det.
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, di, uo) is det),
    in, in, in, in, out, di, uo) is det.
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, in, out) is semidet),
    in, in, in, in, out, in, out) is semidet.
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, mdi, muo) is semidet),
    in, in, in, in, out, mdi, muo) is semidet.
:- mode foldl2_corresponding3(
    in(pred(in, in, in, in, out, di, uo) is semidet),
    in, in, in, in, out, di, uo) is semidet.

    % foldl3_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3):
    % like foldl_corresponding3 but with three accumulators.
    %
:- pred foldl3_corresponding3(pred(A, B, C, D, D, E, E, F, F),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D, E, E, F, F).
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out) is det),
    in, in, in, in, out, in, out, in, out) is det.
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, mdi, muo) is det),
    in, in, in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, di, uo) is det),
    in, in, in, in, out, in, out, di, uo) is det.
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out) is semidet),
    in, in, in, in, out, in, out, in, out) is semidet.
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, mdi, muo) is semidet),
    in, in, in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_corresponding3(
    in(pred(in, in, in, in, out, in, out, di, uo) is semidet),
    in, in, in, in, out, in, out, di, uo) is semidet.

    % foldl4_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3, !Acc4):
    % like foldl_corresponding3 but with four accumulators.
    %
:- pred foldl4_corresponding3(pred(A, B, C, D, D, E, E, F, F, G, G),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D, E, E, F, F, G, G).
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, in, out) is det),
    in, in, in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, di, uo) is det),
    in, in, in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, in, out) is semidet),
    in, in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4_corresponding3(
    in(pred(in, in, in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, in, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

    % map_foldl(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working left-to-right) to transform
    % InList into OutList. The final value of the accumulator is
    % returned in End.
    %
:- pred map_foldl(pred(L, M, A, A), one_or_more(L), one_or_more(M), A, A).
:- mode map_foldl(in(pred(in, out, in, out) is det), in, out, in, out)
    is det.
:- mode map_foldl(in(pred(in, out, mdi, muo) is det), in, out, mdi, muo)
    is det.
:- mode map_foldl(in(pred(in, out, di, uo) is det), in, out, di, uo)
    is det.
:- mode map_foldl(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is semidet.
:- mode map_foldl(in(pred(in, out, mdi, muo) is semidet), in, out, mdi, muo)
    is semidet.
:- mode map_foldl(in(pred(in, out, di, uo) is semidet), in, out, di, uo)
    is semidet.
:- mode map_foldl(in(pred(in, in, di, uo) is semidet), in, in, di, uo)
    is semidet.
:- mode map_foldl(in(pred(in, out, in, out) is nondet), in, out, in, out)
    is nondet.
:- mode map_foldl(in(pred(in, out, mdi, muo) is nondet), in, out, mdi, muo)
    is nondet.
:- mode map_foldl(in(pred(in, out, in, out) is cc_multi), in, out, in, out)
    is cc_multi.
:- mode map_foldl(in(pred(in, out, mdi, muo) is cc_multi), in, out, mdi, muo)
    is cc_multi.
:- mode map_foldl(in(pred(in, out, di, uo) is cc_multi), in, out, di, uo)
    is cc_multi.

    % Same as map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(L, M, A, A, B, B),
    one_or_more(L), one_or_more(M), A, A, B, B).
:- mode map_foldl2(in(pred(in, out, in, out, in, out) is det),
    in, out, in, out, in, out) is det.
:- mode map_foldl2(in(pred(in, out, in, out, mdi, muo) is det),
    in, out, in, out, mdi, muo) is det.
:- mode map_foldl2(in(pred(in, out, in, out, di, uo) is det),
    in, out, in, out, di, uo) is det.
:- mode map_foldl2(in(pred(in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out) is semidet.
:- mode map_foldl2(in(pred(in, out, in, out, mdi, muo) is semidet),
    in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl2(in(pred(in, out, in, out, di, uo) is semidet),
    in, out, in, out, di, uo) is semidet.
:- mode map_foldl2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, in, out, di, uo) is semidet.
:- mode map_foldl2(in(pred(in, out, in, out, in, out) is cc_multi),
    in, out, in, out, in, out) is cc_multi.
:- mode map_foldl2(in(pred(in, out, in, out, mdi, muo) is cc_multi),
    in, out, in, out, mdi, muo) is cc_multi.
:- mode map_foldl2(in(pred(in, out, in, out, di, uo) is cc_multi),
    in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl2(in(pred(in, out, in, out, in, out) is nondet),
    in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with three accumulators.
    %
:- pred map_foldl3(pred(L, M, A, A, B, B, C, C),
    one_or_more(L), one_or_more(M), A, A, B, B, C, C).
:- mode map_foldl3(in(pred(in, out, in, out, in, out, di, uo) is det),
    in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl3(in(pred(in, out, in, out, in, out, in, out) is det),
    in, out, in, out, in, out, in, out) is det.
:- mode map_foldl3(in(pred(in, out, in, out, in, out, di, uo) is cc_multi),
    in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl3(in(pred(in, out, in, out, in, out, in, out) is cc_multi),
    in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl3(in(pred(in, out, in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl3(in(pred(in, out, in, out, in, out, in, out) is nondet),
    in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with four accumulators.
    %
:- pred map_foldl4(pred(L, M, A, A, B, B, C, C, D, D),
    one_or_more(L), one_or_more(M), A, A, B, B, C, C, D, D).
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, di, uo)
    is det),
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, in, out)
    is det),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, di, uo)
    is cc_multi),
    in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, in, out)
    is cc_multi),
    in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl4(in(pred(in, out, in, out, in, out, in, out, in, out)
    is nondet),
    in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with five accumulators.
    %
:- pred map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
    one_or_more(L), one_or_more(M), A, A, B, B, C, C, D, D, E, E).
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is det),
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is det),
    in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi),
    in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi),
    in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet),
    in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl5(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet),
    in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with six accumulators.
    %
:- pred map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
    one_or_more(L), one_or_more(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det),
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is det),
    in, out, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is cc_multi),
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi.
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is cc_multi),
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is cc_multi.
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet),
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is semidet.
:- mode map_foldl6(in(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is nondet),
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is nondet.

    % Same as map_foldl, but with two mapped outputs.
    %
:- pred map2_foldl(pred(L, M, N, A, A),
    one_or_more(L), one_or_more(M), one_or_more(N), A, A).
:- mode map2_foldl(in(pred(in, out, out, in, out) is det), in, out, out,
    in, out) is det.
:- mode map2_foldl(in(pred(in, out, out, mdi, muo) is det), in, out, out,
    mdi, muo) is det.
:- mode map2_foldl(in(pred(in, out, out, di, uo) is det), in, out, out,
    di, uo) is det.
:- mode map2_foldl(in(pred(in, out, out, in, out) is semidet), in, out, out,
    in, out) is semidet.
:- mode map2_foldl(in(pred(in, out, out, mdi, muo) is semidet), in, out, out,
    mdi, muo) is semidet.
:- mode map2_foldl(in(pred(in, out, out, di, uo) is semidet), in, out, out,
    di, uo) is semidet.
:- mode map2_foldl(in(pred(in, out, out, in, out) is nondet), in, out, out,
    in, out) is nondet.
:- mode map2_foldl(in(pred(in, out, out, mdi, muo) is nondet), in, out, out,
    mdi, muo) is nondet.
:- mode map2_foldl(in(pred(in, out, out, in, out) is cc_multi), in, out, out,
    in, out) is cc_multi.
:- mode map2_foldl(in(pred(in, out, out, di, uo) is cc_multi), in, out, out,
    di, uo) is cc_multi.

    % Same as map_foldl, but with two mapped outputs and two
    % accumulators.
    %
:- pred map2_foldl2(pred(L, M, N, A, A, B, B),
    one_or_more(L), one_or_more(M), one_or_more(N), A, A, B, B).
:- mode map2_foldl2(in(pred(in, out, out, in, out, di, uo) is det),
    in, out, out, in, out, di, uo) is det.
:- mode map2_foldl2(in(pred(in, out, out, in, out, in, out) is det),
    in, out, out, in, out, in, out) is det.
:- mode map2_foldl2(in(pred(in, out, out, in, out, di, uo) is cc_multi),
    in, out, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl2(in(pred(in, out, out, in, out, in, out) is cc_multi),
    in, out, out, in, out, in, out) is cc_multi.
:- mode map2_foldl2(in(pred(in, out, out, in, out, in, out) is semidet),
    in, out, out, in, out, in, out) is semidet.
:- mode map2_foldl2(in(pred(in, out, out, in, out, in, out) is nondet),
    in, out, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with two mapped outputs and three
    % accumulators.
    %
:- pred map2_foldl3(pred(L, M, N, A, A, B, B, C, C),
    one_or_more(L), one_or_more(M), one_or_more(N), A, A, B, B, C, C).
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, in, out) is det),
    in, out, out, in, out, in, out, in, out) is det.
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, di, uo) is det),
    in, out, out, in, out, in, out, di, uo) is det.
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, in, out) is cc_multi),
    in, out, out, in, out, in, out, in, out) is cc_multi.
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, di, uo) is cc_multi),
    in, out, out, in, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, in, out) is semidet),
    in, out, out, in, out, in, out, in, out) is semidet.
:- mode map2_foldl3(
    in(pred(in, out, out, in, out, in, out, in, out) is nondet),
    in, out, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with two mapped outputs and four
    % accumulators.
    %
:- pred map2_foldl4(pred(L, M, N, A, A, B, B, C, C, D, D),
    one_or_more(L), one_or_more(M), one_or_more(N), A, A, B, B, C, C, D, D).
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, in, out) is det),
    in, out, out, in, out, in, out, in, out, in, out) is det.
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, di, uo) is det),
    in, out, out, in, out, in, out, in, out, di, uo) is det.
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, in, out) is cc_multi),
    in, out, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, di, uo) is cc_multi),
    in, out, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, in, out) is semidet),
    in, out, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map2_foldl4(
    in(pred(in, out, out, in, out, in, out, in, out, in, out) is nondet),
    in, out, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with three mapped outputs.
    %
:- pred map3_foldl(pred(L, M, N, O, A, A),
    one_or_more(L), one_or_more(M), one_or_more(N), one_or_more(O), A, A).
:- mode map3_foldl(in(pred(in, out, out, out, in, out) is det), in, out, out,
    out, in, out) is det.
:- mode map3_foldl(in(pred(in, out, out, out, mdi, muo) is det), in, out, out,
    out, mdi, muo) is det.
:- mode map3_foldl(in(pred(in, out, out, out, di, uo) is det), in, out, out,
    out, di, uo) is det.
:- mode map3_foldl(in(pred(in, out, out, out, in, out) is semidet), in, out,
    out, out, in, out) is semidet.
:- mode map3_foldl(in(pred(in, out, out, out, mdi, muo) is semidet), in, out,
    out, out, mdi, muo) is semidet.
:- mode map3_foldl(in(pred(in, out, out, out, di, uo) is semidet), in, out,
    out, out, di, uo) is semidet.
:- mode map3_foldl(in(pred(in, out, out, out, in, out) is nondet), in, out,
    out, out, in, out) is nondet.
:- mode map3_foldl(in(pred(in, out, out, out, mdi, muo) is nondet), in, out,
    out, out, mdi, muo) is nondet.
:- mode map3_foldl(in(pred(in, out, out, out, in, out) is cc_multi), in, out,
    out, out, in, out) is cc_multi.
:- mode map3_foldl(in(pred(in, out, out, out, di, uo) is cc_multi), in, out,
    out, out, di, uo) is cc_multi.

    % Same as map_foldl, but with three mapped outputs and two
    % accumulators.
    %
:- pred map3_foldl2(pred(L, M, N, O, A, A, B, B), one_or_more(L),
    one_or_more(M), one_or_more(N), one_or_more(O), A, A, B, B).
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, di, uo) is det),
    in, out, out, out, in, out, di, uo) is det.
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, in, out) is det),
    in, out, out, out, in, out, in, out) is det.
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, di, uo) is cc_multi),
    in, out, out, out, in, out, di, uo) is cc_multi.
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, in, out) is cc_multi),
    in, out, out, out, in, out, in, out) is cc_multi.
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, in, out) is semidet),
    in, out, out, out, in, out, in, out) is semidet.
:- mode map3_foldl2(in(pred(in, out, out, out, in, out, in, out) is nondet),
    in, out, out, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with four mapped outputs.
    %
:- pred map4_foldl(pred(L, M, N, O, P, A, A), one_or_more(L), one_or_more(M),
    one_or_more(N), one_or_more(O), one_or_more(P), A, A).
:- mode map4_foldl(in(pred(in, out, out, out, out, in, out) is det),
    in, out, out, out, out, in, out) is det.
:- mode map4_foldl(in(pred(in, out, out, out, out, mdi, muo) is det),
    in, out, out, out, out, mdi, muo) is det.
:- mode map4_foldl(in(pred(in, out, out, out, out, di, uo) is det),
    in, out, out, out, out, di, uo) is det.
:- mode map4_foldl(in(pred(in, out, out, out, out, in, out) is semidet),
    in, out, out, out, out, in, out) is semidet.
:- mode map4_foldl(in(pred(in, out, out, out, out, mdi, muo) is semidet),
    in, out, out, out, out, mdi, muo) is semidet.
:- mode map4_foldl(in(pred(in, out, out, out, out, di, uo) is semidet),
    in, out, out, out, out, di, uo) is semidet.
:- mode map4_foldl(in(pred(in, out, out, out, out, in, out) is nondet),
    in, out, out, out, out, in, out) is nondet.
:- mode map4_foldl(in(pred(in, out, out, out, out, mdi, muo) is nondet),
    in, out, out, out, out, mdi, muo) is nondet.
:- mode map4_foldl(in(pred(in, out, out, out, out, in, out) is cc_multi),
    in, out, out, out, out, in, out) is cc_multi.
:- mode map4_foldl(in(pred(in, out, out, out, out, di, uo) is cc_multi),
    in, out, out, out, out, di, uo) is cc_multi.

%---------------------%

    % map_foldr(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working right-to-left) to transform
    % InList into OutList. The final value of the accumulator is
    % returned in End.
    %
:- pred map_foldr(pred(L, M, A, A), one_or_more(L), one_or_more(M), A, A).
:- mode map_foldr(in(pred(in, out, in, out) is det), in, out, in, out)
    is det.
:- mode map_foldr(in(pred(in, out, mdi, muo) is det), in, out, mdi, muo)
    is det.
:- mode map_foldr(in(pred(in, out, di, uo) is det), in, out, di, uo)
    is det.
:- mode map_foldr(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is semidet.
:- mode map_foldr(in(pred(in, out, mdi, muo) is semidet), in, out, mdi, muo)
    is semidet.
:- mode map_foldr(in(pred(in, out, di, uo) is semidet), in, out, di, uo)
    is semidet.
:- mode map_foldr(in(pred(in, in, di, uo) is semidet), in, in, di, uo)
    is semidet.

%---------------------%

    % map_corresponding_foldl/6 is like map_corresponding except
    % that it has an accumulator threaded through it.
    %
:- pred map_corresponding_foldl(pred(A, B, C, D, D),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D).
:- mode map_corresponding_foldl(in(pred(in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode map_corresponding_foldl(in(pred(in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode map_corresponding_foldl(in(pred(in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode map_corresponding_foldl(in(pred(in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode map_corresponding_foldl(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode map_corresponding_foldl(in(pred(in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

    % Like map_corresponding_foldl/6 except that it has two
    % accumulators.
    %
:- pred map_corresponding_foldl2(pred(A, B, C, D, D, E, E),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D, E, E).
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, in, out) is det), in, in, out, in, out,
    in, out) is det.
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, mdi, muo) is det), in, in, out, in, out,
    mdi, muo) is det.
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, di, uo) is det), in, in, out, in, out,
    di, uo) is det.
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, in, out) is semidet), in, in, out, in, out,
    in, out) is semidet.
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, mdi, muo) is semidet), in, in, out, in, out,
    mdi, muo) is semidet.
:- mode map_corresponding_foldl2(
    in(pred(in, in, out, in, out, di, uo) is semidet), in, in, out, in, out,
    di, uo) is semidet.

    % Like map_corresponding_foldl/6 except that it has three
    % accumulators.
    %
:- pred map_corresponding_foldl3(pred(A, B, C, D, D, E, E, F, F),
    one_or_more(A), one_or_more(B), one_or_more(C), D, D, E, E, F, F).
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, in, out) is det), in, in, out, in, out,
    in, out, in, out) is det.
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is det), in, in, out, in, out,
    in, out, mdi, muo) is det.
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, di, uo) is det), in, in, out, in, out,
    in, out, di, uo) is det.
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, in, out) is semidet), in, in, out,
    in, out, in, out, in, out) is semidet.
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet), in, in, out,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_corresponding_foldl3(
    in(pred(in, in, out, in, out, in, out, di, uo) is semidet), in, in, out,
    in, out, in, out, di, uo) is semidet.

    % map_corresponding3_foldl/7 is like map_corresponding3 except
    % that it has an accumulator threaded through it.
    %
:- pred map_corresponding3_foldl(pred(A, B, C, D, E, E),
    one_or_more(A), one_or_more(B), one_or_more(C), one_or_more(D), E, E).
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, in, out) is det),
    in, in, in, out, in, out) is det.
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, mdi, muo) is det),
    in, in, in, out, mdi, muo) is det.
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, di, uo) is det),
    in, in, in, out, di, uo) is det.
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, in, out) is semidet),
    in, in, in, out, in, out) is semidet.
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, in, out, mdi, muo) is semidet.
:- mode map_corresponding3_foldl(
    in(pred(in, in, in, out, di, uo) is semidet),
    in, in, in, out, di, uo) is semidet.

%---------------------%

    % filter_map_foldl(Transformer, List, TrueList, Start, End):
    % Takes a predicate with one input argument, one output argument and an
    % accumulator. It is called with each element of List. If a call succeeds,
    % then the output is included in TrueList and the accumulator is updated.
    %
:- pred filter_map_foldl(
    pred(X, Y, A, A)::in(pred(in, out, in, out) is semidet),
    one_or_more(X)::in, list(Y)::out, A::in, A::out) is det.

%---------------------------------------------------------------------------%

% The following is a list of the functions and predicates that are
% present in list.m but not in one_or_more.m, together with the reasons
% for their absences.
%
% - pred is_empty/1
% - pred is_not_empty/1
%
%   When applied to known-to-be-nonempty lists, the outcomes of these tests
%   are known statically.
%
% - func det_head/1
% - func det_tail/1
%
%   For nonempty lists, the simple head and tail functions are already
%   deterministic.
%
% - func append/2
%
%   You can append two one_or_mores using the ++ function, and the compiler
%   already gets confused by whether references to append with an arity
%   less than three is a reference to the function or to the predicate version.
%
% - pred remove_suffix/3
%
%   When the suffix is removed, there may be no element left.
%
% - pred reverse_prepend/3
%
%   In list.m, it is part of the implementation of reverse, but one_or_more.m
%   implements reverse by calling list.reverse.
%   XXX We could implement reverse_prepend nevertheless.
%
% - pred insert/3
%
%   In the reverse modes, where the caller is deleting an element,
%   there may be no element left.
%
% - func ../2
%
%   Depending on the values of the lower and upper bounds, the resulting list
%   may be empty.
%
% - func det_last/1
% - pred det_last/2
% - pred det_split_last/3
%
%   For nonempty lists, the simple last function and predicate,
%   and the split_last predicate, are already deterministic.
%
% - pred take/3
% - pred det_take/3
% - func take_upto/2
% - pred take_upto/3
% - pred drop/3
% - pred det_drop/3
%
%   Depending on the value of the count parameter, the resulting list
%   may contain no elements.
%
% - pred take_while/4
% - func take_while/2
% - pred take_while/3
% - func drop_while/2
% - pred drop_while/3
%
%   Depending on the value of the count parameter, the resulting list
%   may contain no elements.
%
% - func duplicate/1
% - pred duplicate/2
%
%   Depending on the value of the count parameter, the resulting list
%   may contain no elements.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

one_or_more_to_list(one_or_more(Head, Tail)) = [Head | Tail].

list_to_one_or_more([Head | Tail], one_or_more(Head, Tail)).

det_list_to_one_or_more([], _) :-
    unexpected($pred, "empty list").
det_list_to_one_or_more([Head | Tail], one_or_more(Head, Tail)).

%---------------------------------------------------------------------------%

head(one_or_more(H, _T)) = H.

tail(one_or_more(_H, T)) = T.

cons(H, T) = L :-
    one_or_more.cons(H, T, L).

cons(Head, one_or_more(HeadTail, TailTail), R) :-
    R = one_or_more(Head, [HeadTail | TailTail]).

%---------------------------------------------------------------------------%

append(OoMA, OoMB, OoMC) :-
    OoMA = one_or_more(AH, AT),
    append_list_one_or_more(AT, OoMB, OoMATB),
    OoMC = one_or_more(AH, one_or_more_to_list(OoMATB)).

A ++ B = C :-
    one_or_more.append(A, B, C).

append_one_or_more_list(OoMA, ListB, OoMR) :-
    OoMA = one_or_more(AH, AT),
    list.append(AT, ListB, RT),
    OoMR = one_or_more(AH, RT).

append_list_one_or_more(ListA, OoMB, OoMR) :-
    (
        ListA = [AH | AT],
        append_list_one_or_more(AT, OoMB, ATOoMB),
        OoMR = one_or_more(AH, one_or_more_to_list(ATOoMB))
    ;
        ListA = [],
        OoMR = OoMB
    ).

%---------------------------------------------------------------------------%

length(OoM) = Length :-
    one_or_more.length(OoM, Length).

length(one_or_more(_H, T), Length) :-
    Length = 1 + list.length(T).

%---------------------------------------------------------------------------%

same_length(one_or_more(_AH, AT), one_or_more(_B, BT)) :-
    list.same_length(AT, BT).

same_length3(one_or_more(_, AT), one_or_more(_, BT), one_or_more(_, CT)) :-
    list.same_length3(AT, BT, CT).

%---------------------------------------------------------------------------%

member(X, one_or_more(X, _)).
member(X, one_or_more(_, T)) :-
    list.member(X, T).

member_index0(X, one_or_more(X, _), 0).
member_index0(X, one_or_more(_, T), Index + 1) :-
    list.member_index0(X, T, Index).

member_indexes0(X, one_or_more(H, T), Indexes) :-
    list.member_indexes0(X, [H | T], Indexes).

contains(one_or_more(H, T), Elem) :-
    list.member(Elem, [H | T]).

%---------------------------------------------------------------------------%

index0(one_or_more(H, T), N, Elem) :-
    ( if N = 0 then
        Elem = H
    else
        list.index0(T, N - 1, Elem)
    ).

index1(one_or_more(H, T), N, Elem) :-
    list.index0([H | T], N - 1, Elem).

det_index0(Xs, N) = A :-
    one_or_more.det_index0(Xs, N, A).

det_index0(OoM, N, Elem) :-
    ( if one_or_more.index0(OoM, N, ElemPrime) then
        Elem = ElemPrime
    else
        unexpected($pred, "index out of range")
    ).

det_index1(OoM, N) = A :-
    one_or_more.det_index1(OoM, N, A).

det_index1(OoM, N, Elem) :-
    one_or_more.det_index0(OoM, N - 1, Elem).

%---------------------------------------------------------------------------%

nth_member_search(one_or_more(H, T), SearchX, N) :-
    list.index1_of_first_occurrence([H | T], SearchX, N).

nth_member_lookup(one_or_more(H, T), SearchX, N) :-
    N = list.det_index1_of_first_occurrence([H | T], SearchX).

%---------------------------------------------------------------------------%

index0_of_first_occurrence(one_or_more(H, T), SearchX, N) :-
    list.index0_of_first_occurrence([H | T], SearchX, N).

index1_of_first_occurrence(OoM, SearchX, N + 1) :-
    one_or_more.index0_of_first_occurrence(OoM, SearchX, N).

det_index0_of_first_occurrence(OoM, SearchX) = N :-
    ( if one_or_more.index0_of_first_occurrence(OoM, SearchX, NPrime) then
        N = NPrime
    else
        unexpected($pred, "item not found")
    ).

det_index1_of_first_occurrence(OoM, SearchX) = N :-
    ( if one_or_more.index1_of_first_occurrence(OoM, SearchX, NPrime) then
        N = NPrime
    else
        unexpected($pred, "item not found")
    ).

%---------------------------------------------------------------------------%

reverse(OoM) = RevOoM :-
    List = one_or_more_to_list(OoM),
    list.reverse(List, RevList),
    det_list_to_one_or_more(RevList, RevOoM).

    % reverse(A, B) <=> reverse(B, A).
:- pragma promise_equivalent_clauses(pred(one_or_more.reverse/2)).

reverse(OoM::in, RevOoM::out) :-
    RevOoM = one_or_more.reverse(OoM).
reverse(RevOoM::out, OoM::in) :-
    RevOoM = one_or_more.reverse(OoM).

%---------------------------------------------------------------------------%

delete(one_or_more(H, T), ToDelete, R) :-
    (
        H = ToDelete,
        R = T
    ;
        list.delete(T, ToDelete, RT),
        R = [H | RT]
    ).

delete_first(one_or_more(H, T), ToDelete, R) :-
    ( if H = ToDelete then
        R = T
    else
        list.delete_first(T, ToDelete, RT),
        R = [H | RT]
    ).

delete_all(one_or_more(H, T), ToDelete) = R :-
    list.delete_all([H | T], ToDelete, R).

delete_all(one_or_more(H, T), ToDelete, R) :-
    list.delete_all([H | T], ToDelete, R).

delete_elems(one_or_more(H, T), ToDeletes) = R :-
    list.delete_elems([H | T], ToDeletes, R).

delete_elems(one_or_more(H, T), ToDeletes, R) :-
    list.delete_elems([H | T], ToDeletes, R).

sublist(SubOoM, FullOoM) :-
    SubList = one_or_more_to_list(SubOoM),
    FullList = one_or_more_to_list(FullOoM),
    list.sublist(SubList, FullList).

%---------------------------------------------------------------------------%

replace(one_or_more(H0, T0), From, To, one_or_more(H, T)) :-
    (
        H0 = From,
        H = To,
        T = T0
    ;
        list.replace(T0, From, To, T),
        H = H0
    ).

replace_first(one_or_more(H0, T0), From, To, one_or_more(H, T)) :-
    ( if H0 = From then
        H = To,
        T = T0
    else
        list.replace_first(T0, From, To, T),
        H = H0
    ).

replace_all(OoM, N, To) = R :-
    one_or_more.replace_all(OoM, N, To, R).

replace_all(one_or_more(H0, T0), From, To, one_or_more(H, T)) :-
    ( if H0 = From then
        H = To
    else
        H = H0
    ),
    list.replace_all(T0, From, To, T).

replace_nth(one_or_more(H0, T0), N, To, R) :-
    ( if N > 1 then
        list.replace_nth(T0, N - 1, To, T),
        R = one_or_more(H0, T)
    else if N > 0 then
        R = one_or_more(To, T0)
    else
        fail
    ).

det_replace_nth(OoM, N, To) = R :-
    one_or_more.det_replace_nth(OoM, N, To, R).

det_replace_nth(one_or_more(H0, T0), N, To, R) :-
    ( if N > 1 then
        list.det_replace_nth(T0, N - 1, To, T),
        R = one_or_more(H0, T)
    else if N > 0 then
        R = one_or_more(To, T0)
    else
        unexpected($pred,
            "Cannot replace element whose index position is less than 1.")
    ).

%---------------------------------------------------------------------------%

remove_dups(OoM) = FilteredOoM :-
    one_or_more.remove_dups(OoM, FilteredOoM).

remove_dups(one_or_more(H, T), FilteredOoM) :-
    list.remove_dups([H | T], FilteredList),
    det_list_to_one_or_more(FilteredList, FilteredOoM).

%---------------------------------------------------------------------------%

remove_adjacent_dups(OoM) = FilteredOoM :-
    one_or_more.remove_adjacent_dups(OoM, FilteredOoM).

remove_adjacent_dups(one_or_more(H, T), FilteredOoM) :-
    list.remove_adjacent_dups([H | T], FilteredList),
    det_list_to_one_or_more(FilteredList, FilteredOoM).

remove_adjacent_dups(ComparePred, one_or_more(H, T), FilteredOoM) :-
    list.remove_adjacent_dups(ComparePred, [H | T], FilteredList),
    det_list_to_one_or_more(FilteredList, FilteredOoM).

%---------------------------------------------------------------------------%

merge(As, Bs) = ResultOoM :-
    one_or_more.merge(As, Bs, ResultOoM).

merge(one_or_more(AH, AT), one_or_more(BH, BT), ResultOoM) :-
    list.merge([AH | AT], [BH | BT], ResultList),
    det_list_to_one_or_more(ResultList, ResultOoM).

merge(CompareFunc, one_or_more(AH, AT), one_or_more(BH, BT)) = ResultOoM :-
    list.merge(CompareFunc, [AH | AT], [BH | BT]) = ResultList,
    det_list_to_one_or_more(ResultList, ResultOoM).

merge(ComparePred, one_or_more(AH, AT), one_or_more(BH, BT), ResultOoM) :-
    list.merge(ComparePred, [AH | AT], [BH | BT], ResultList),
    det_list_to_one_or_more(ResultList, ResultOoM).

merge_and_remove_dups(As, Bs) = ResultOoM :-
    one_or_more.merge_and_remove_dups(As, Bs, ResultOoM).

merge_and_remove_dups(one_or_more(AH, AT), one_or_more(BH, BT), ResultOoM) :-
    list.merge_and_remove_dups([AH | AT], [BH | BT], ResultList),
    det_list_to_one_or_more(ResultList, ResultOoM).

merge_and_remove_dups(CompareFunc, one_or_more(AH, AT), one_or_more(BH, BT))
        = ResultOoM :-
    list.merge_and_remove_dups(CompareFunc, [AH | AT], [BH | BT]) = ResultList,
    det_list_to_one_or_more(ResultList, ResultOoM).

merge_and_remove_dups(ComparePred, one_or_more(AH, AT), one_or_more(BH, BT),
        ResultOoM) :-
    list.merge_and_remove_dups(ComparePred, [AH | AT], [BH | BT], ResultList),
    det_list_to_one_or_more(ResultList, ResultOoM).

%---------------------------------------------------------------------------%

sort(OoM) = SortedOoM :-
    one_or_more.sort(OoM, SortedOoM).

sort(one_or_more(H, T), SortedOoM) :-
    list.sort([H | T], SortedList),
    det_list_to_one_or_more(SortedList, SortedOoM).

sort_and_remove_dups(OoM) = SortedOoM :-
    one_or_more.sort_and_remove_dups(OoM, SortedOoM).

sort_and_remove_dups(one_or_more(H, T), SortedOoM) :-
    list.sort_and_remove_dups([H | T], SortedList),
    det_list_to_one_or_more(SortedList, SortedOoM).

%---------------------------------------------------------------------------%

sort(CompareFunc, OoM) = SortedOoM :-
    ComparePred =
        ( pred(X::in, Y::in, Res::out) is det :-
            Res = CompareFunc(X, Y)
        ),
    one_or_more.sort(ComparePred, OoM, SortedOoM).

sort(ComparePred, one_or_more(H, T), SortedOoM) :-
    list.sort(ComparePred, [H | T], SortedList),
    det_list_to_one_or_more(SortedList, SortedOoM).

sort_and_remove_dups(ComparePred, one_or_more(H, T), SortedOoM) :-
    list.sort_and_remove_dups(ComparePred, [H | T], SortedList),
    det_list_to_one_or_more(SortedList, SortedOoM).

%---------------------------------------------------------------------------%

split_list(N, one_or_more(H, T), Start, End) :-
    list.split_list(N, [H | T], Start, End).

det_split_list(N, one_or_more(H, T), Start, End) :-
    list.det_split_list(N, [H | T], Start, End).

split_upto(N, one_or_more(H, T), Start, End) :-
    list.split_upto(N, [H | T], Start, End).

last(OoM) = Last :-
    one_or_more.last(OoM, Last).

last(one_or_more(H, T), Last) :-
    ( if list.last(T, LastPrime) then
        Last = LastPrime
    else
        Last = H
    ).

split_last(one_or_more(H, T), AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        split_last_loop(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

:- pred split_last_loop(T::in, list(T)::in, list(T)::out, T::out) is det.

split_last_loop(H, T, AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        split_last_loop(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

%---------------------------------------------------------------------------%

all_same(one_or_more(H, T)) :-
    all_same_as(H, T).

:- pred all_same_as(T::in, list(T)::in) is semidet.

all_same_as(_, []).
all_same_as(SameAs, [H | T]) :-
    H = SameAs,
    all_same_as(SameAs, T).

%---------------------------------------------------------------------------%

condense(Xss) = Ys :-
    one_or_more.condense(Xss, Ys).

condense(Xss, Ys) :-
    list.reverse(Xss, RevXss),
    condense_acc(RevXss, [], Ys).

:- pred condense_acc(list(one_or_more(T))::in, list(T)::in, list(T)::out)
    is det.

condense_acc([], !Ys).
condense_acc([OoM | OoMs], !Ys) :-
    append(one_or_more_to_list(OoM), !Ys),
    condense_acc(OoMs, !Ys).

chunk(OoM, ChunkSize) = OoMChunks :-
    chunk(OoM, ChunkSize, OoMChunks).

chunk(one_or_more(H, T), ChunkSize, OoMChunks) :-
    compare(Cmp, ChunkSize, 0),
    expect(unify(Cmp, (>)), $pred, "chunk size must be at least one"),
    chunk_loop(H, T, ChunkSize, [], ChunkSize, OoMChunks).

:- pred chunk_loop(T::in, list(T)::in, int::in, list(T)::in, int::in,
    one_or_more(one_or_more(T))::out) is det.

chunk_loop(H, T, ChunkSize, RevChunkSoFar, RoomLeft, OoMChunks) :-
    ( if RoomLeft > 0 then
        (
            T = [],
            list.reverse([H | RevChunkSoFar], Chunk),
            det_list_to_one_or_more(Chunk, OoMChunk),
            OoMChunks = one_or_more(OoMChunk, [])
        ;
            T = [TH | TT],
            chunk_loop(TH, TT, ChunkSize, [H | RevChunkSoFar], RoomLeft - 1,
                OoMChunks)
        )
    else
        list.reverse(RevChunkSoFar, Chunk),
        det_list_to_one_or_more(Chunk, OoMChunk),
        chunk_loop(H, T, ChunkSize, [], ChunkSize, TailOoMChunks),
        OoMChunks = one_or_more.cons(OoMChunk, TailOoMChunks)
    ).

%---------------------------------------------------------------------------%

zip(A, B) = AB :-
    zip(A, B, AB).

zip(A, B, AB) :-
    ListA = one_or_more_to_list(A),
    ListB = one_or_more_to_list(B),
    list.zip(ListA, ListB, ListAB),
    det_list_to_one_or_more(ListAB, AB).

%---------------------------------------------------------------------------%

perm(OoM, PermutedOoM) :-
    OoM = one_or_more(H, T),
    list.perm([H | T], PermutedList),
    det_list_to_one_or_more(PermutedList, PermutedOoM).

%---------------------------------------------------------------------------%

one_or_more_to_doc(OoM) = pretty_printer.one_or_more_to_doc(OoM).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

find_first_match(P, one_or_more(H, T), FirstMatch) :-
    ( if P(H) then
        FirstMatch = H
    else
        list.find_first_match(P, T, FirstMatch)
    ).

any_true(P, OoM) :-
    not one_or_more.all_false(P, OoM).

any_false(P, L) :-
    not one_or_more.all_true(P, L).

all_true(P, one_or_more(H, T)) :-
    P(H),
    list.all_true(P, T).

all_false(P, one_or_more(H, T)) :-
    not P(H),
    list.all_false(P, T).

all_true_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT)) :-
    P(AH, BH),
    list.all_true_corresponding(P, AT, BT).

all_false_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT)) :-
    not P(AH, BH),
    list.all_false_corresponding(P, AT, BT).

%---------------------------------------------------------------------------%

filter(P, Xs) = Trues :-
    one_or_more.filter(P, Xs, Trues).

filter(P, one_or_more(H, T), Trues) :-
    ( if P(H) then
        list.filter(P, T, TruesTail),
        Trues = [H | TruesTail]
    else
        filter(P, T, Trues)
    ).

filter(P, one_or_more(H, T), Trues, Falses) :-
    ( if P(H) then
        filter(P, T, TruesTail, Falses),
        Trues = [H | TruesTail]
    else
        filter(P, T, Trues, FalsesTail),
        Falses = [H | FalsesTail]
    ).

negated_filter(P, Xs) = Falses :-
    one_or_more.negated_filter(P, Xs, Falses).

negated_filter(P, one_or_more(H, T), Falses) :-
    ( if P(H) then
        negated_filter(P, T, Falses)
    else
        negated_filter(P, T, FalsesTail),
        Falses = [H | FalsesTail]
    ).

filter_map(F, Xs) = Ys :-
    P = ( pred(X::in, Y::out) is semidet :- Y = F(X) ),
    one_or_more.filter_map(P, Xs, Ys).

filter_map(P, one_or_more(H0, T0), Trues) :-
    ( if P(H0, H) then
        filter_map(P, T0, TruesTail),
        Trues = [H | TruesTail]
    else
        filter_map(P, T0, Trues)
    ).

filter_map(P, one_or_more(H0, T0), Trues, Falses) :-
    ( if P(H0, H) then
        list.filter_map(P, T0, TruesTail, Falses),
        Trues = [H | TruesTail]
    else
        list.filter_map(P, T0, Trues, FalsesTail),
        Falses = [H0 | FalsesTail]
    ).

find_first_map(P, one_or_more(H, T), A) :-
    ( if P(H, A0) then
        A = A0
    else
        find_first_map(P, T, A)
    ).

find_first_map2(P, one_or_more(H, T), A, B) :-
    ( if P(H, A0, B0) then
        A = A0,
        B = B0
    else
        find_first_map2(P, T, A, B)
    ).

find_first_map3(P, one_or_more(H, T), A, B, C) :-
    ( if P(H, A0, B0, C0) then
        A = A0,
        B = B0,
        C = C0
    else
        list.find_first_map3(P, T, A, B, C)
    ).

find_index_of_match(Match, one_or_more(H, T), Index0, Index) :-
    ( if Match(H) then
        Index = Index0
    else
        list.find_index_of_match(Match, T, Index0 + 1, Index)
    ).

%---------------------------------------------------------------------------%

map(F, one_or_more(H0, T0)) = one_or_more(F(H0), list.map(F, T0)).

map(P, one_or_more(H0, T0), one_or_more(H, T)) :-
    P(H0, H),
    list.map(P, T0, T).

map2(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2)) :-
    P(H0, H1, H2),
    list.map2(P, T0, T1, T2).

map3(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3)) :-
    P(H0, H1, H2, H3),
    list.map3(P, T0, T1, T2, T3).

map4(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4)) :-
    P(H0, H1, H2, H3, H4),
    list.map4(P, T0, T1, T2, T3, T4).

map5(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4), one_or_more(H5, T5)) :-
    P(H0, H1, H2, H3, H4, H5),
    list.map5(P, T0, T1, T2, T3, T4, T5).

map6(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4), one_or_more(H5, T5),
        one_or_more(H6, T6)) :-
    P(H0, H1, H2, H3, H4, H5, H6),
    list.map6(P, T0, T1, T2, T3, T4, T5, T6).

map7(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4), one_or_more(H5, T5),
        one_or_more(H6, T6), one_or_more(H7, T7)) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7),
    list.map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

map8(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4), one_or_more(H5, T5),
        one_or_more(H6, T6), one_or_more(H7, T7), one_or_more(H8, T8)) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7, H8),
    list.map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

%---------------------------------------------------------------------------%

map_corresponding(F, one_or_more(AH, AT), one_or_more(BH, BT)) =
    one_or_more(F(AH, BH), list.map_corresponding(F, AT, BT)).

map_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(RH, RT)) :-
    P(AH, BH, RH),
    list.map_corresponding(P, AT, BT, RT).

map_corresponding3(F, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT)) =
    one_or_more(F(AH, BH, CH), list.map_corresponding3(F, AT, BT, CT)).

map_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), one_or_more(RH, RT)) :-
    P(AH, BH, CH, RH),
    list.map_corresponding3(P, AT, BT, CT, RT).

%---------------------------------------------------------------------------%

filter_map_corresponding(F, one_or_more(AH, AT), one_or_more(BH, BT)) =
    ( if F(AH, BH) = RH then
        [RH | list.filter_map_corresponding(F, AT, BT)]
    else
        list.filter_map_corresponding(F, AT, BT)
    ).

filter_map_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT), R) :-
    ( if P(AH, BH, RH) then
        list.filter_map_corresponding(P, AT, BT, RT),
        R = [RH | RT]
    else
        list.filter_map_corresponding(P, AT, BT, R)
    ).

filter_map_corresponding3(F, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT)) =
    ( if F(AH, BH, CH) = RH then
        [RH | list.filter_map_corresponding3(F, AT, BT, CT)]
    else
        list.filter_map_corresponding3(F, AT, BT, CT)
    ).

filter_map_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), R) :-
    ( if P(AH, BH, CH, RH) then
        list.filter_map_corresponding3(P, AT, BT, CT, RT),
        R = [RH | RT]
    else
        list.filter_map_corresponding3(P, AT, BT, CT, R)
    ).

%---------------------------------------------------------------------------%

foldl(F, OoM, !.A) = !:A :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    one_or_more.foldl(P, OoM, !A).

foldl(P, one_or_more(H, T), !A) :-
    P(H, !A),
    list.foldl(P, T, !A).

foldl2(P, one_or_more(H, T), !A, !B) :-
    P(H, !A, !B),
    list.foldl2(P, T, !A, !B).

foldl3(P, one_or_more(H, T), !A, !B, !C) :-
    P(H, !A, !B, !C),
    list.foldl3(P, T, !A, !B, !C).

foldl4(P, one_or_more(H, T), !A, !B, !C, !D) :-
    P(H, !A, !B, !C, !D),
    list.foldl4(P, T, !A, !B, !C, !D).

foldl5(P, one_or_more(H, T), !A, !B, !C, !D, !E) :-
    P(H, !A, !B, !C, !D, !E),
    list.foldl5(P, T, !A, !B, !C, !D, !E).

foldl6(P, one_or_more(H, T), !A, !B, !C, !D, !E, !F) :-
    P(H, !A, !B, !C, !D, !E, !F),
    list.foldl6(P, T, !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%

foldr(F, OoM, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    one_or_more.foldr(P, OoM, A, B).

foldr(P, one_or_more(H, T), !A) :-
    list.foldr(P, T, !A),
    P(H, !A).

foldr2(P, one_or_more(H, T), !A, !B) :-
    list.foldr2(P, T, !A, !B),
    P(H, !A, !B).

foldr3(P, one_or_more(H, T), !A, !B, !C) :-
    list.foldr3(P, T, !A, !B, !C),
    P(H, !A, !B, !C).

%---------------------------------------------------------------------------%

foldl_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT), !Acc) :-
    P(AH, BH, !Acc),
    list.foldl_corresponding(P, AT, BT, !Acc).

foldl_corresponding(F, one_or_more(AH, AT), one_or_more(BH, BT),
        !.Acc) = !:Acc :-
    !:Acc = F(AH, BH, !.Acc),
    !:Acc = list.foldl_corresponding(F, AT, BT, !.Acc).

foldl2_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT),
        !Acc1, !Acc2) :-
    P(AH, BH, !Acc1, !Acc2),
    list.foldl2_corresponding(P, AT, BT, !Acc1, !Acc2).

foldl3_corresponding(P, one_or_more(AH, AT), one_or_more(BH, BT),
        !Acc1, !Acc2, !Acc3) :-
    P(AH, BH, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, AT, BT, !Acc1, !Acc2, !Acc3).

foldl_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc) :-
    P(AH, BH, CH, !Acc),
    list.foldl_corresponding3(P, AT, BT, CT, !Acc).

foldl2_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc1, !Acc2) :-
    P(AH, BH, CH, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, AT, BT, CT, !Acc1, !Acc2).

foldl3_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc1, !Acc2, !Acc3) :-
    P(AH, BH, CH, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, AT, BT, CT, !Acc1, !Acc2, !Acc3).

foldl4_corresponding3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc1, !Acc2, !Acc3, !Acc4) :-
    P(AH, BH, CH, !Acc1, !Acc2, !Acc3, !Acc4),
    list.foldl4_corresponding3(P, AT, BT, CT, !Acc1, !Acc2, !Acc3, !Acc4).

%---------------------------------------------------------------------------%

map_foldl(P, one_or_more(H0, T0), one_or_more(H, T), !A) :-
    P(H0, H, !A),
    list.map_foldl(P, T0, T, !A).

map_foldl2(P, one_or_more(H0, T0), one_or_more(H, T), !A, !B) :-
    P(H0, H, !A, !B),
    list.map_foldl2(P, T0, T, !A, !B).

map_foldl3(P, one_or_more(H0, T0), one_or_more(H, T), !A, !B, !C) :-
    P(H0, H, !A, !B, !C),
    list.map_foldl3(P, T0, T, !A, !B, !C).

map_foldl4(P, one_or_more(H0, T0), one_or_more(H, T), !A, !B, !C, !D) :-
    P(H0, H, !A, !B, !C, !D),
    list.map_foldl4(P, T0, T, !A, !B, !C, !D).

map_foldl5(P, one_or_more(H0, T0), one_or_more(H, T), !A, !B, !C, !D, !E) :-
    P(H0, H, !A, !B, !C, !D, !E),
    list.map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

map_foldl6(P, one_or_more(H0, T0), one_or_more(H, T),
        !A, !B, !C, !D, !E, !F) :-
    P(H0, H, !A, !B, !C, !D, !E, !F),
    list.map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

map2_foldl(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        !A) :-
    P(H0, H1, H2, !A),
    list.map2_foldl(P, T0, T1, T2, !A).

map2_foldl2(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        !A, !B) :-
    P(H0, H1, H2, !A, !B),
    list.map2_foldl2(P, T0, T1, T2, !A, !B).

map2_foldl3(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        !A, !B, !C) :-
    P(H0, H1, H2, !A, !B, !C),
    list.map2_foldl3(P, T0, T1, T2, !A, !B, !C).

map2_foldl4(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        !A, !B, !C, !D) :-
    P(H0, H1, H2, !A, !B, !C, !D),
    list.map2_foldl4(P, T0, T1, T2, !A, !B, !C, !D).

map3_foldl(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), !A) :-
    P(H0, H1, H2, H3, !A),
    list.map3_foldl(P, T0, T1, T2, T3, !A).

map3_foldl2(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), !A, !B) :-
    P(H0, H1, H2, H3, !A, !B),
    list.map3_foldl2(P, T0, T1, T2, T3, !A, !B).

map4_foldl(P, one_or_more(H0, T0), one_or_more(H1, T1), one_or_more(H2, T2),
        one_or_more(H3, T3), one_or_more(H4, T4), !A) :-
    P(H0, H1, H2, H3, H4, !A),
    list.map4_foldl(P, T0, T1, T2, T3, T4, !A).

map_foldr(P, one_or_more(H0, T0), one_or_more(H, T), !A) :-
    list.map_foldr(P, T0, T, !A),
    P(H0, H, !A).

%---------------------------------------------------------------------------%

map_corresponding_foldl(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc) :-
    P(AH, BH, CH, !Acc),
    list.map_corresponding_foldl(P, AT, BT, CT, !Acc).

map_corresponding_foldl2(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc1, !Acc2) :-
    P(AH, BH, CH, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, AT, BT, CT, !Acc1, !Acc2).

map_corresponding_foldl3(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), !Acc1, !Acc2, !Acc3) :-
    P(AH, BH, CH, !Acc1, !Acc2, !Acc3),
    list.map_corresponding_foldl3(P, AT, BT, CT, !Acc1, !Acc2, !Acc3).

map_corresponding3_foldl(P, one_or_more(AH, AT), one_or_more(BH, BT),
        one_or_more(CH, CT), one_or_more(DH, DT), !Acc) :-
    P(AH, BH, CH, DH, !Acc),
    list.map_corresponding3_foldl(P, AT, BT, CT, DT, !Acc).

%---------------------------------------------------------------------------%

filter_map_foldl(P, one_or_more(H, T), PHs, !A) :-
    ( if P(H, PH, !A) then
        list.filter_map_foldl(P, T, PHsTail, !A),
        PHs = [PH | PHsTail]
    else
        list.filter_map_foldl(P, T, PHs, !A)
    ).

%---------------------------------------------------------------------------%
:- end_module one_or_more.
%---------------------------------------------------------------------------%
