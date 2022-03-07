%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: list.m.
% Authors: fjh, conway, trd, zs, philip, warwick, ...
% Stability: medium to high.
%
% This module defines the list type, and various utility predicates that
% operate on lists.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module list.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % The definition of the type list(T).
    % A list is either an empty list, denoted `[]',
    % or an element Head of type T followed by a tail Tail
    % of type list(T), denoted `[Head | Tail]'.
    %
:- type list(T)
    --->    []
    ;       [T | list(T)].

%---------------------------------------------------------------------------%

    % These instantiation states and modes can be used for instantiation
    % state subtyping.
    %
    % They could also be used for partial instantiation but partial
    % instantiation does not work completely, for information see the
    % LIMITATIONS.md file distributed with Mercury.
    %
:- inst list_skel(I) for list/1
    --->    []
    ;       [I | list_skel(I)].
:- inst list(I) == list_skel(I).

:- inst empty_list for list/1
    --->    [].
:- inst non_empty_list for list/1
    --->    [ground | ground].

%---------------------------------------------------------------------------%

:- pred is_empty(list(T)::in) is semidet.

:- pred is_not_empty(list(T)::in) is semidet.

%---------------------------------------------------------------------------%

:- func head(list(T)) = T is semidet.

    % det_head(List) returns the first element of List,
    % calling error/1 if List is empty.
    %
:- func det_head(list(T)) = T.

:- func tail(list(T)) = list(T) is semidet.

    % det_tail(List) returns the tail of List,
    % calling error/1 if List is empty.
    %
:- func det_tail(list(T)) = list(T).

    % det_head_tail(List, Head, Tail) returns the head and the tail of List,
    % calling error/1 if List is empty.
    %
:- pred det_head_tail(list(T)::in, T::out, list(T)::out) is det.

    % cons(X, Y) = Z <=> Z = [X | Y].
    %
:- func cons(T, list(T)) = list(T).
:- pred cons(T::in, list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%

    % Standard append predicate:
    % append(Start, End, List) is true iff
    % List is the result of concatenating Start and End.
    %
:- pred append(list(T), list(T), list(T)).
:- mode append(di, di, uo) is det.
:- mode append(in, in, out) is det.
:- mode append(in, in, in) is semidet.    % implied
:- mode append(in, out, in) is semidet.
:- mode append(out, out, in) is multi.
% The following mode is semidet in the sense that it does not
% succeed more than once - but it does create a choice-point, which means
% both that it is inefficient, and that the compiler can't deduce that
% it is semidet. Use remove_suffix instead.
% :- mode append(out, in, in) is semidet.

:- func append(list(T), list(T)) = list(T).

    % L1 ++ L2 = L :- append(L1, L2, L).
    %
:- func list(T) ++ list(T) = list(T).

    % remove_suffix(List, Suffix, Prefix):
    %
    % The same as append(Prefix, Suffix, List) except that
    % this is semidet, whereas append(out, in, in) is nondet.
    %
:- pred remove_suffix(list(T)::in, list(T)::in, list(T)::out) is semidet.

%---------------------%

    % Associativity of append.
:- promise all [A, B, C, ABC]
    (
        ( some [AB] (list.append(A, B, AB), list.append(AB, C, ABC)) )
    <=>
        ( some [BC] (list.append(B, C, BC), list.append(A, BC, ABC)) )
    ).
    % Construction equivalence law.
    % NOTE_TO_IMPLEMENTORS When we implement rewrite rules,
    % NOTE_TO_IMPLEMENTORS we should change this law to a rewrite rule.
:- promise all [L, H, T] ( append([H], T, L) <=> L = [H | T] ).

%---------------------------------------------------------------------------%

    % length(List) = Length:
    % length(List, Length):
    %
    % True iff Length is the length of List, i.e. if List contains
    % Length elements.
    %
:- func length(list(T)) = int.

:- pred length(list(_T), int).
:- mode length(in, out) is det.
% NOTE_TO_IMPLEMENTORS XXX The current mode checker can't handle this mode.
% NOTE_TO_IMPLEMENTORS :- mode length(input_list_skel, out) is det.

    % same_length(ListA, ListB):
    %
    % True iff ListA and ListB have the same length,
    % i.e. iff they both contain the same number of elements.
    %
    % Does not traverse *either* list further than the length
    % of the shorter list.
    %
:- pred same_length(list(T1), list(T2)).
% NOTE_TO_IMPLEMENTORS XXX The current mode checker can't handle these modes.
% NOTE_TO_IMPLEMENTORS :- mode same_length(in, output_list_skel) is det.
% NOTE_TO_IMPLEMENTORS :- mode same_length(output_list_skel, in) is det.
:- mode same_length(in, in) is semidet.
% NOTE_TO_IMPLEMENTORS XXX The current mode checker can't handle these modes.
% NOTE_TO_IMPLEMENTORS :- mode same_length(input_list_skel, output_list_skel)
% NOTE_TO_IMPLEMENTORS      is det.
% NOTE_TO_IMPLEMENTORS :- mode same_length(output_list_skel, input_list_skel)
% NOTE_TO_IMPLEMENTORS      is det.

    % As above, but for three lists.
    %
    % Does not traverse *any* of the three lists further than the length
    % of the shortest list.
    %
:- pred same_length3(list(T1)::in, list(T2)::in, list(T3)::in)
    is semidet.

%---------------------------------------------------------------------------%

    % member(Elem, List):
    %
    % True iff List contains Elem.
    %
:- pred member(T, list(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % member(Elem, List, SubList):
    %
    % True iff List contains Elem, and SubList is a suffix of List
    % beginning with Elem.
    % Same as `SubList = [Elem | _], append(_, SubList, List)'.
    %
:- pred member(T::out, list(T)::in, list(T)::out) is nondet.

    % member_index0(Elem, List, Index):
    %
    % True iff List contains Elem at the zero-based index Index.
    %
:- pred member_index0(T, list(T), int).
:- mode member_index0(in, in, in) is semidet.
:- mode member_index0(in, in, out) is nondet.
:- mode member_index0(out, in, out) is nondet.

    % member_indexes0(Elem, List, Indexes):
    %
    % True iff List contains Elem at the zero-based indexes Indexes.
    % Indexes will be sorted.
    %
:- pred member_indexes0(T::in, list(T)::in, list(int)::out) is det.

    % contains(List, Elem):
    %
    % Equivalent to member(Elem, List).
    %
    % Sometimes you need the arguments in this order, because you want to
    % construct a closure with only the list.
    %
:- pred contains(list(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%

    % index*(List, Position, Elem):
    %
    % These predicates select an element in a list from its position.
    % The `index0' preds consider the first element to be element
    % number zero, whereas the `index1' preds consider the first element
    % to be element number one. The `det_' preds call error/1 if the index
    % is out of range, whereas the semidet preds fail if the index is out of
    % range.
    %
:- pred index0(list(T)::in, int::in, T::out) is semidet.
:- pred index1(list(T)::in, int::in, T::out) is semidet.

:- func det_index0(list(T), int) = T.
:- pred det_index0(list(T)::in, int::in, T::out) is det.
:- func det_index1(list(T), int) = T.
:- pred det_index1(list(T)::in, int::in, T::out) is det.

    % nth_member_search(List, Elem, Position):
    %
    % Elem is the Position'th member of List.
    % (Position numbers start from 1.)
    % NOTE_TO_IMPLEMENTORS XXX This pred is identical
    % NOTE_TO_IMPLEMENTORS to index1_of_first_occurrence.
    %
:- pred nth_member_search(list(T)::in, T::in, int::out) is semidet.

    % nth_member_lookup(List, Elem, Position):
    %
    % A deterministic version of nth_member_search, which throws an exception
    % instead of failing if the element is not found in the list.
    % NOTE_TO_IMPLEMENTORS XXX This pred is identical
    % NOTE_TO_IMPLEMENTORS to det_index1_of_first_occurrence.
    %
:- pred nth_member_lookup(list(T)::in, T::in, int::out) is det.

    % index*_of_first_occurrence(List, Elem, Position):
    %
    % Computes the least value of Position such that
    % list_index*(List, Position, Elem).
    %
    % The `det_' funcs call error/1 if Elem is not a member of List.
    %
:- pred index0_of_first_occurrence(list(T)::in, T::in, int::out) is semidet.
:- pred index1_of_first_occurrence(list(T)::in, T::in, int::out) is semidet.
:- func det_index0_of_first_occurrence(list(T), T) = int.
:- func det_index1_of_first_occurrence(list(T), T) = int.

%---------------------------------------------------------------------------%

    % reverse(List, Reverse):
    %
    % Reverse is a list containing the same elements as List
    % but in reverse order.
    %
:- pred reverse(list(T), list(T)).
:- mode reverse(in, out) is det.
:- mode reverse(out, in) is det.

:- func reverse(list(T)) = list(T).

    % reverse_prepend(Xs, Ys, Zs):
    %
    % Same as `Zs = list.reverse(Xs) ++ Ys' but more efficient.
    %
:- pred reverse_prepend(list(T)::in, list(T)::in, list(T)::out) is det.
:- func reverse_prepend(list(T), list(T)) = list(T).

%---------------------------------------------------------------------------%

    % insert(Elem, List0, List):
    %
    % List is the result of inserting Elem somewhere in List0.
    % Same as `delete(List, Elem, List0)'.
    %
:- pred insert(T, list(T), list(T)).
:- mode insert(in, in, in) is semidet.
:- mode insert(in, out, in) is nondet.
:- mode insert(out, out, in) is nondet.
:- mode insert(in, in, out) is multi.

    % delete(List, Elem, Remainder):
    %
    % True iff Elem occurs in List, and Remainder is the result of
    % deleting one occurrence of Elem from List.
    %
:- pred delete(list(T), T, list(T)).
:- mode delete(in, in, in) is semidet.
:- mode delete(in, in, out) is nondet.
:- mode delete(in, out, out) is nondet.
:- mode delete(out, in, in) is multi.

    % delete_first(List0, Elem, List):
    %
    % True iff Elem occurs in List0
    % and List is List0 with the first occurrence of Elem removed.
    %
:- pred delete_first(list(T)::in, T::in, list(T)::out) is semidet.

    % delete_all(List0, Elem) = List:
    %
    % True iff List is List0 with all occurrences of Elem removed.
    %
:- func delete_all(list(T), T) = list(T).
:- pred delete_all(list(T), T, list(T)).
:- mode delete_all(di, in, uo) is det.
:- mode delete_all(in, in, out) is det.

    % delete_nth(List0, N, List):
    %
    % True iff List0 has an N'th element,
    % and List is List0 with this element deleted.
    %
:- pred delete_nth(list(T)::in, int::in, list(T)::out) is semidet.

    % delete_elems(List0, Elems) = List:
    %
    % True iff List is List0 with all occurrences of all elements of Elems
    % removed.
    %
:- func delete_elems(list(T), list(T)) = list(T).
:- pred delete_elems(list(T)::in, list(T)::in, list(T)::out) is det.

    % sublist(SubList, FullList):
    %
    % True if one can obtain SubList by starting with FullList
    % and deleting some of its elements.
    %
:- pred sublist(list(T)::in, list(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % replace(List0, D, R, List):
    %
    % True iff List is List0 with an occurrence of D replaced with R.
    %
:- pred replace(list(T), T, T, list(T)).
:- mode replace(in, in, in, in) is semidet.
:- mode replace(in, in, in, out) is nondet.

    % replace_first(List0, D, R, List):
    %
    % True iff List is List0 with the first occurrence of D replaced with R.
    %
:- pred replace_first(list(T)::in, T::in, T::in, list(T)::out) is semidet.

    % replace_all(List0, D, R) = List:
    %
    % True iff List is List0 with all occurrences of D replaced with R.
    %
:- func replace_all(list(T), T, T) = list(T).
:- pred replace_all(list(T)::in, T::in, T::in, list(T)::out) is det.

    % replace_nth(List0, N, R, List):
    %
    % True iff List is List0 with its N'th element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- pred replace_nth(list(T)::in, int::in, T::in, list(T)::out) is semidet.

    % det_replace_nth(List0, N, R) = List:
    %
    % True iff List is List0 with its N'th element replaced with R.
    % Throws an exception if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func det_replace_nth(list(T), int, T) = list(T).
:- pred det_replace_nth(list(T)::in, int::in, T::in, list(T)::out) is det.

%---------------------------------------------------------------------------%

    % Lo `..` Hi = [Lo, Lo + 1, ..., Hi] if Lo =< Hi, and [] otherwise.
    %
:- func int `..` int = list(int).

%---------------------------------------------------------------------------%

    % series(X, OK, Succ) = [X0, X1, ..., Xn]
    %
    % where X0 = X and successive elements Xj, Xk are computed as
    % Xk = Succ(Xj). The series terminates as soon as an element Xi is
    % generated such that OK(Xi) fails; Xi is not included in the output.
    %
:- func series(T, pred(T), func(T) = T) = list(T).
:- mode series(in, pred(in) is semidet, func(in) = out is det) = out is det.

%---------------------------------------------------------------------------%

    % remove_dups(L0) = L:
    %
    % L is the result of deleting the second and subsequent occurrences
    % of every element that occurs twice in L0.
    %
:- func remove_dups(list(T)) = list(T).
:- pred remove_dups(list(T)::in, list(T)::out) is det.

    % remove_adjacent_dups(L0) = L:
    %
    % L is the result of replacing every sequence of duplicate elements in L0
    % with a single such element.
    %
:- func remove_adjacent_dups(list(T)) = list(T).
:- pred remove_adjacent_dups(list(T)::in, list(T)::out) is det.

    % remove_adjacent_dups(P, L0, L):
    %
    % True iff L is the result of replacing every sequence of elements in L0
    % which are equivalent with respect to the ordering, with the first
    % occurrence in L0 of such an element.
    %
:- pred remove_adjacent_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::out) is det.

%---------------------------------------------------------------------------%

    % merge(L1, L2) = L:
    %
    % L is the result of merging the elements of L1 and L2, in ascending order.
    % L1 and L2 must be sorted.
    %
:- func merge(list(T), list(T)) = list(T).
:- pred merge(list(T)::in, list(T)::in, list(T)::out) is det.

    % merge(Compare, As, Bs) = Sorted:
    %
    % True iff, assuming As and Bs are sorted with respect to the ordering
    % defined by Compare, Sorted is a sorted list containing the elements
    % of As and Bs. For elements which are equivalent in the ordering,
    % if they come from the same list then they appear in the same sequence
    % in Sorted as they do in that list, otherwise the elements from As
    % appear before the elements from Bs.
    %
:- func merge(comparison_func(X), list(X), list(X)) = list(X).
:- pred merge(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::in, list(X)::out) is det.

    % merge_and_remove_dups(L1, L2) = L:
    %
    % L is the result of merging the elements of L1 and L2, in ascending order,
    % and eliminating any duplicates. L1 and L2 must be sorted and must each
    % not contain any duplicates.
    %
:- func merge_and_remove_dups(list(T), list(T)) = list(T).
:- pred merge_and_remove_dups(list(T)::in, list(T)::in, list(T)::out) is det.

    % merge_and_remove_dups(Compare, As, Bs) = Sorted:
    %
    % True iff, assuming As and Bs are sorted with respect to the ordering
    % defined by Compare and neither contains any duplicates, Sorted is a
    % sorted list containing the elements of As and Bs without any duplicates.
    % If an element from As is duplicated in Bs (that is, they are equivalent
    % in the ordering), then the element from As is the one that appears
    % in Sorted.
    %
:- func merge_and_remove_dups(comparison_func(X), list(X), list(X))
    = list(X).
:- pred merge_and_remove_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::in, list(X)::out) is det.

%---------------------%

    % sort(List) = SortedList:
    %
    % Sorts List and returns the result as SortedList.
    %
:- func sort(list(T)) = list(T).
:- pred sort(list(T)::in, list(T)::out) is det.

    % sort_and_remove_dups(List) = SortedList:
    %
    % Sorts List, removes the second and subsequent occurrences of
    % any duplicates, and returns the result as SortedList.
    %
:- func sort_and_remove_dups(list(T)) = list(T).
:- pred sort_and_remove_dups(list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%

    % sort(Compare, Unsorted) = Sorted:
    %
    % True iff Sorted is a list containing the same elements as Unsorted,
    % where Sorted is sorted with respect to the ordering defined by Compare,
    % and the elements that are equivalent in this ordering appear
    % in the same sequence in Sorted as they do in Unsorted
    % (that is, the sort is stable).
    %
:- func sort(comparison_func(X), list(X)) = list(X).
:- pred sort(comparison_pred(X)::in(comparison_pred), list(X)::in,
    list(X)::out) is det.

    % sort_and_remove_dups(Compare, Unsorted, Sorted):
    %
    % True iff Sorted is a list containing the same elements as Unsorted,
    % where Sorted is sorted with respect to the ordering defined by the
    % predicate term Compare, except that if two elements in Unsorted
    % are equivalent with respect to this ordering only the one which
    % occurs first will be in Sorted.
    %
:- pred sort_and_remove_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::out) is det.

%---------------------------------------------------------------------------%

    % split_list(N, List, Start, End):
    %
    % Splits List into a prefix Start of length N, and a remainder End.
    % Fails if N is not in `0 .. length(List)'.
    % See also: take, drop and split_upto.
    %
:- pred split_list(int::in, list(T)::in, list(T)::out, list(T)::out)
    is semidet.

    % det_split_list(N, List, Start, End):
    %
    % A deterministic version of split_list, which throws an exception
    % instead of failing if N is not in 0 .. length(List).
    %
:- pred det_split_list(int::in, list(T)::in, list(T)::out, list(T)::out)
    is det.

    % split_upto(N, List, Start, End):
    %
    % Splits List into a prefix Start of length `min(N, length(List))',
    % and a remainder End. Throws an exception if N < 0.
    % See also: split_list, take, drop.
    %
:- pred split_upto(int::in, list(T)::in, list(T)::out, list(T)::out) is det.

%---------------------%

    % last(List, Last):
    %
    % True if Last is the last element of List.
    %
:- pred last(list(T)::in, T::out) is semidet.

    % det_last(List, Last):
    %
    % A deterministic version of last, which throws an exception instead of
    % failing if the input list is empty.
    %
:- func det_last(list(T)) = T.
:- pred det_last(list(T)::in, T::out) is det.

    % split_last(List, AllButLast, Last):
    %
    % True if Last is the last element of List and AllButLast is the list
    % of elements before it.
    %
:- pred split_last(list(T)::in, list(T)::out, T::out) is semidet.

    % det_split_last(List, AllButLast, Last):
    %
    % A deterministic version of split_last, which throws an exception
    % instead of failing if the input list is empty.
    %
:- pred det_split_last(list(T)::in, list(T)::out, T::out) is det.

%---------------------%

    % take(N, List, Start):
    %
    % Start is the first Len elements of List.
    % Fails if N is not in `0 .. length(List)'.
    %
:- pred take(int::in, list(T)::in, list(T)::out) is semidet.

    % det_take(Len, List, Start):
    %
    % As above, but throw an exception instead of failing.
    %
:- pred det_take(int::in, list(T)::in, list(T)::out) is det.

    % take_upto(Len, List) = Start:
    %
    % Start is the first Len elements of List. If List has less than
    % Len elements, return the entire list. Throws an exception if N < 0.
    %
:- func take_upto(int, list(T)) = list(T).
:- pred take_upto(int::in, list(T)::in, list(T)::out) is det.

%---------------------%

    % drop(N, List, End):
    %
    % End is the remainder of List after removing the first N elements.
    % Fails if N is not in `0 .. length(List)'.
    % See also: split_list.
    %
:- pred drop(int::in, list(T)::in, list(T)::out) is semidet.

    % det_drop(N, List, End):
    %
    % End is the remainder of List after removing the first N elements.
    % Throws an exception if N is not in `0 .. length(List)'.
    % See also: split_list.
    %
:- pred det_drop(int::in, list(T)::in, list(T)::out) is det.

%---------------------%

    % take_while(Pred, List, Start, End)
    %
    % List = Start ++ End. Start is the longest prefix of List where Pred
    % succeeds for every element in Start. End is the remainder of the list.
    %
:- pred take_while(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out, list(T)::out) is det.

    % take_while(Pred, List) = Start :-
    %     take_while(Pred, List, Start, _End)
    %
    % Start is the longest prefix of List where Pred succeeds for every element
    % in Start.
    %
:- func take_while(pred(T), list(T)) = list(T).
:- mode take_while(pred(in) is semidet, in) = out is det.
:- pred take_while(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out) is det.

%---------------------%

    % drop_while(Pred, List) = End :-
    %     take_while(Pred, List, _Start, End).
    %
    % End is the remainder of List after removing all the consecutive
    % elements from the start of List for which Pred succeeds.
    %
:- func drop_while(pred(T), list(T)) = list(T).
:- mode drop_while(pred(in) is semidet, in) = out is det.
:- pred drop_while(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out) is det.

%---------------------------------------------------------------------------%

    % duplicate(Count, Elem) = List:
    %
    % True iff List is a list containing Count duplicate copies of Elem.
    %
:- func duplicate(int, T) = list(T).
:- pred duplicate(int::in, T::in, list(T)::out) is det.

    % all_same(List):
    %
    % True if all elements of the list are the same.
    %
:- pred all_same(list(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % condense(ListOfLists) = List:
    %
    % List is the result of concatenating all the elements of ListOfLists.
    %
:- func condense(list(list(T))) = list(T).
:- pred condense(list(list(T))::in, list(T)::out) is det.

    % chunk(List, ChunkSize) = Chunks:
    %
    % Takes a list List and breaks it into a list of lists Chunks,
    % such that the length of each list in Chunks is at most ChunkSize.
    % (More precisely, the length of each list in Chunks other than the
    % last one is exactly ChunkSize, while the length of the last list in
    % Chunks may vary between one and ChunkSize.)
    %
:- func chunk(list(T), int) = list(list(T)).
:- pred chunk(list(T)::in, int::in, list(list(T))::out) is det.

%---------------------------------------------------------------------------%

    % zip(ListA, ListB) = List:
    %
    % List is the result of alternating the elements of ListA and ListB,
    % starting with the first element of ListA (followed by the first element
    % of ListB, then the second element of listA, then the second element
    % of ListB, etc.). When there are no more elements remaining in one of
    % the lists, the remainder of the other list is appended.
    %
:- func zip(list(T), list(T)) = list(T).
:- pred zip(list(T)::in, list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%

    % perm(List0, List):
    %
    % True iff List is a permutation of List0.
    %
:- pred perm(list(T)::in, list(T)::out) is multi.

%---------------------------------------------------------------------------%

    % list_to_doc(List) = Doc:
    %
    % Convert a list to a pretty_printer.doc for formatting.
    %
:- func list_to_doc(list(T)) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%---------------------------------------------------------------------------%

    % find_first_match(Pred, List, FirstMatch):
    %
    % Takes a closure with one input argument. It returns the first element X
    % of the list (if any) for which Pred(X) is true.
    %
:- pred find_first_match(pred(X)::in(pred(in) is semidet), list(X)::in,
    X::out) is semidet.

    % any_true(Pred, List):
    %
    % Succeeds iff Pred succeeds for at least one element of List.
    % Same as `not all_false(Pred, List)'.
    %
:- pred any_true(pred(X)::in(pred(in) is semidet), list(X)::in) is semidet.

    % any_false(Pred, List):
    %
    % Succeeds iff Pred fails for at least one element of List.
    % Same as `not all_true(Pred, List)'.
    %
:- pred any_false(pred(X)::in(pred(in) is semidet), list(X)::in) is semidet.

    % all_true(Pred, List):
    %
    % Takes a closure with one input argument.
    % If Pred succeeds for every member of List, all_true succeeds.
    % If Pred fails for any member of List, all_true fails.
    %
:- pred all_true(pred(X)::in(pred(in) is semidet), list(X)::in) is semidet.

    % all_false(Pred, List):
    %
    % Takes a closure with one input argument.
    % If Pred fails for every member of List, all_false succeeds.
    % If Pred succeeds for any member of List, all_false fails.
    %
:- pred all_false(pred(X)::in(pred(in) is semidet), list(X)::in) is semidet.

    % all_true_corresponding(Pred, ListA, ListB):
    %
    % Succeeds if Pred succeeds for every corresponding pair of elements from
    % ListA and ListB. Fails if Pred fails for any pair of corresponding
    % elements.
    %
    % Raises an exception if the list arguments differ in length.
    %
:- pred all_true_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    list(X)::in, list(Y)::in) is semidet.

    % all_false_corresponding(Pred, ListA, ListB):
    %
    % Succeeds if Pred fails for every corresponding pair of elements from
    % ListA and ListB. Fails if Pred succeeds for any pair of corresponding
    % elements.
    %
    % Raises an exception if the list arguments differ in length.
    %
:- pred all_false_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    list(X)::in, list(Y)::in) is semidet.

%---------------------------------------------------------------------------%

    % filter(Pred, List) = TrueList:
    %
    % Takes a closure Pred with one input argument. It calls Pred(X)
    % on each member X of List, and includes X in TrueList iff Pred(X) is true.
    %
:- func filter(pred(X)::in(pred(in) is semidet), list(X)::in)
    = (list(X)::out) is det.
:- pred filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out) is det.

    % filter(Pred, List, TrueList, FalseList):
    %
    % Takes a closure Pred with one input argument. It calls Pred(X)
    % on each member X of List. Includes X in TrueList iff Pred(X) is true,
    % and includes X in FalseList iff Pred(X) is false.
    %
:- pred filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out, list(X)::out) is det.

    % negated_filter(Pred, List) = FalseList:
    %
    % Takes a closure Pred with one input argument. It calls Pred(X)
    % on each member X of List, and includes X in FalseList iff Pred(X)
    % is false.
    %
:- func negated_filter(pred(X)::in(pred(in) is semidet), list(X)::in)
    = (list(X)::out) is det.
:- pred negated_filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out) is det.

    % filter_map(Transformer, List, TrueList):
    %
    % Takes a semidet function Transformer and calls it on each element X
    % of List. If Transformer(X) succeeds, then it includes its return value
    % in TrueList.
    %
:- func filter_map(func(X) = Y, list(X)) = list(Y).
:- mode filter_map(func(in) = out is semidet, in) = out is det.

    % filter_map(Transformer, List, TrueList):
    %
    % Takes a predicate Transformer with one input and one output argument,
    % and calls it on each element of X of List. If Transformer(X, Y) succeeds,
    % then it includes Y in TrueList.
    %
:- pred filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, list(Y)::out) is det.

    % filter_map(Transformer, List, TrueList, FalseList):
    %
    % Takes a predicate Transformer with one input and one output argument,
    % and calls it on each element of X of List. If Transformer(X, Y) succeeds,
    % then it includes Y in TrueList; if it fails, then it includes X
    % in FalseList.
    %
:- pred filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, list(Y)::out, list(X)::out) is det.

    % find_first_map(Transformer, List, FirstTrue):
    %
    % Same as filter_map/3 except that it only returns the first match,
    % so that
    %
    %   find_first_map(Transformer, List, FirstTrue)
    %
    % is equivalent to
    %
    %   filter_map(Transformer, List, [FirstTrue | _])
    %
:- pred find_first_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, Y::out) is semidet.

    % find_first_map2(Transformer, List, FirstTrueA, FirstTrueB):
    %
    % Same as find_first_map, except with two outputs.
    %
:- pred find_first_map2(pred(X, A, B)::in(pred(in, out, out) is semidet),
    list(X)::in, A::out, B::out) is semidet.

    % find_first_map3(Transformer, List, FirstTrueA, FirstTrueB, FirstTrueB):
    %
    % Same as find_first_map, except with three outputs.
    %
:- pred find_first_map3(
    pred(X, A, B, C)::in(pred(in, out, out, out) is semidet),
    list(X)::in, A::out, B::out, C::out) is semidet.

    % find_index_of_match(Match, List, Index0, Index):
    %
    % Find the index of the first item in List for which Match is true,
    % where the first element in the list has the index Index0.
    % (Index0 is *not* the number of items to skip at the head of List.)
    %
:- pred find_index_of_match(pred(T), list(T), int, int).
:- mode find_index_of_match(pred(in) is semidet, in, in, out) is semidet.

%---------------------------------------------------------------------------%

    % map(T, L) = M:
    % map(T, L, M):
    %
    % Apply the closure T to transform the elements of L
    % into the elements of M.
    %
:- func map(func(X) = Y, list(X)) = list(Y).
:- pred map(pred(X, Y), list(X), list(Y)).
:- mode map(pred(in, out) is det, in, out) is det.
:- mode map(pred(in, out) is cc_multi, in, out) is cc_multi.
:- mode map(pred(in, out) is semidet, in, out) is semidet.
:- mode map(pred(in, out) is multi, in, out) is multi.
:- mode map(pred(in, out) is nondet, in, out) is nondet.
:- mode map(pred(in, in) is semidet, in, in) is semidet.

    % map2(T, L, M1, M2) uses the closure T
    % to transform the elements of L into the elements of M1 and M2.
    %
:- pred map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode map2(pred(in, out, out) is det, in, out, out) is det.
:- mode map2(pred(in, out, out) is cc_multi, in, out, out) is cc_multi.
:- mode map2(pred(in, out, out) is semidet, in, out, out) is semidet.
:- mode map2(pred(in, out, out) is multi, in, out, out) is multi.
:- mode map2(pred(in, out, out) is nondet, in, out, out) is nondet.
:- mode map2(pred(in, in, in) is semidet, in, in, in) is semidet.

    % map3(T, L, M1, M2, M3) uses the closure T
    % to transform the elements of L into the elements of M1, M2 and M3.
    %
:- pred map3(pred(A, B, C, D), list(A), list(B), list(C), list(D)).
:- mode map3(pred(in, out, out, out) is det, in, out, out, out) is det.
:- mode map3(pred(in, out, out, out) is cc_multi, in, out, out, out)
    is cc_multi.
:- mode map3(pred(in, out, out, out) is semidet, in, out, out, out)
    is semidet.
:- mode map3(pred(in, out, out, out) is multi, in, out, out, out)
    is multi.
:- mode map3(pred(in, out, out, out) is nondet, in, out, out, out)
    is nondet.
:- mode map3(pred(in, in, in, in) is semidet, in, in, in, in) is semidet.

    % map4(T, L, M1, M2, M3, M4) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3 and M4.
    %
:- pred map4(pred(A, B, C, D, E), list(A), list(B), list(C), list(D),
    list(E)).
:- mode map4(pred(in, out, out, out, out) is det, in, out, out, out, out)
    is det.
:- mode map4(pred(in, out, out, out, out) is cc_multi, in, out, out, out,
    out) is cc_multi.
:- mode map4(pred(in, out, out, out, out) is semidet, in, out, out, out,
    out) is semidet.
:- mode map4(pred(in, out, out, out, out) is multi, in, out, out, out,
    out) is multi.
:- mode map4(pred(in, out, out, out, out) is nondet, in, out, out, out,
    out) is nondet.
:- mode map4(pred(in, in, in, in, in) is semidet, in, in, in, in, in)
    is semidet.

    % map5(T, L, M1, M2, M3, M4, M5) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4
    % and M5.
    %
:- pred map5(pred(A, B, C, D, E, F), list(A), list(B), list(C), list(D),
    list(E), list(F)).
:- mode map5(pred(in, out, out, out, out, out) is det, in, out, out, out,
    out, out) is det.
:- mode map5(pred(in, out, out, out, out, out) is cc_multi, in, out, out,
    out, out, out) is cc_multi.
:- mode map5(pred(in, out, out, out, out, out) is semidet, in, out, out,
    out, out, out) is semidet.
:- mode map5(pred(in, out, out, out, out, out) is multi, in, out, out,
    out, out, out) is multi.
:- mode map5(pred(in, out, out, out, out, out) is nondet, in, out, out,
    out, out, out) is nondet.
:- mode map5(pred(in, in, in, in, in, in) is semidet, in, in, in, in, in,
    in) is semidet.

    % map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5 and M6.
    %
:- pred map6(pred(A, B, C, D, E, F, G), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G)).
:- mode map6(pred(in, out, out, out, out, out, out) is det, in, out, out,
    out, out, out, out) is det.
:- mode map6(pred(in, out, out, out, out, out, out) is cc_multi, in, out,
    out, out, out, out, out) is cc_multi.
:- mode map6(pred(in, out, out, out, out, out, out) is semidet, in, out,
    out, out, out, out, out) is semidet.
:- mode map6(pred(in, out, out, out, out, out, out) is multi, in, out,
    out, out, out, out, out) is multi.
:- mode map6(pred(in, out, out, out, out, out, out) is nondet, in, out,
    out, out, out, out, out) is nondet.
:- mode map6(pred(in, in, in, in, in, in, in) is semidet, in, in, in, in,
    in, in, in) is semidet.

    % map7(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6 and M7.
    %
:- pred map7(pred(A, B, C, D, E, F, G, H), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G), list(H)).
:- mode map7(pred(in, out, out, out, out, out, out, out) is det,
    in, out, out, out, out, out, out, out) is det.
:- mode map7(pred(in, out, out, out, out, out, out, out) is cc_multi,
    in, out, out, out, out, out, out, out) is cc_multi.
:- mode map7(pred(in, out, out, out, out, out, out, out) is semidet,
    in, out, out, out, out, out, out, out) is semidet.
:- mode map7(pred(in, out, out, out, out, out, out, out) is multi,
    in, out, out, out, out, out, out, out) is multi.
:- mode map7(pred(in, out, out, out, out, out, out, out) is nondet,
    in, out, out, out, out, out, out, out) is nondet.
:- mode map7(pred(in, in, in, in, in, in, in, in) is semidet,
    in, in, in, in, in, in, in, in) is semidet.

    % map8(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6, M7 and M8.
    %
:- pred map8(pred(A, B, C, D, E, F, G, H, I), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G), list(H), list(I)).
:- mode map8(pred(in, out, out, out, out, out, out, out, out) is det,
    in, out, out, out, out, out, out, out, out) is det.
:- mode map8(pred(in, out, out, out, out, out, out, out, out) is cc_multi,
    in, out, out, out, out, out, out, out, out) is cc_multi.
:- mode map8(pred(in, out, out, out, out, out, out, out, out) is semidet,
    in, out, out, out, out, out, out, out, out) is semidet.
:- mode map8(pred(in, out, out, out, out, out, out, out, out) is multi,
    in, out, out, out, out, out, out, out, out) is multi.
:- mode map8(pred(in, out, out, out, out, out, out, out, out) is nondet,
    in, out, out, out, out, out, out, out, out) is nondet.
:- mode map8(pred(in, in, in, in, in, in, in, in, in) is semidet,
    in, in, in, in, in, in, in, in, in) is semidet.

%---------------------%

    % map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
    %   [F(A1, B1), .., F(An, Bn)].
    %
    % Raises an exception if the list arguments differ in length.
    %
:- func map_corresponding(func(A, B) = R, list(A), list(B)) = list(R).
:- pred map_corresponding(pred(A, B, R), list(A), list(B), list(R)).
:- mode map_corresponding(in(pred(in, in, out) is det), in, in, out)
    is det.
:- mode map_corresponding(in(pred(in, in, out) is semidet), in, in, out)
    is semidet.

    % map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % Raises an exception if the list arguments differ in length.
    %
:- func map_corresponding3(func(A, B, C) = R, list(A), list(B), list(C))
    = list(R).
:- pred map_corresponding3(pred(A, B, C, R), list(A), list(B), list(C),
    list(R)).
:- mode map_corresponding3(in(pred(in, in, in, out) is det),
    in, in, in, out) is det.
:- mode map_corresponding3(in(pred(in, in, in, out) is semidet),
    in, in, in, out) is semidet.

%---------------------%

    % filter_map_corresponding/3 does the same job as map_corresponding/3,
    % except the function argument is semidet, and the output list consists of
    % only those applications of the function argument that succeeded.
    %
:- func filter_map_corresponding(func(A, B) = R, list(A), list(B))
    = list(R).
:- mode filter_map_corresponding(func(in, in) = out is semidet, in, in)
    = out is det.
:- pred filter_map_corresponding(
    pred(A, B, R)::in(pred(in, in, out) is semidet),
    list(A)::in, list(B)::in, list(R)::out) is det.

    % filter_map_corresponding3/4 does the same job as map_corresponding3/4,
    % except the function argument is semidet, and the output list consists of
    % only those applications of the function argument that succeeded.
    %
:- func filter_map_corresponding3(func(A, B, C) = R,
    list(A), list(B), list(C)) = list(R).
:- mode filter_map_corresponding3(func(in, in, in) = out is semidet,
    in, in, in) = out is det.
:- pred filter_map_corresponding3(
    pred(A, B, C, R)::in(pred(in, in, in, out) is semidet),
    list(A)::in, list(B)::in, list(C)::in, list(R)::out) is det.

%---------------------------------------------------------------------------%


    % foldl(Func, List, Start) = End:
    % foldl(Pred, List, Start, End):
    %
    % Calls Pred on each element of List, working left-to-right.
    % Each call to Pred will have a pair of arguments that represent
    % respectively the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- func foldl(func(L, A) = A, list(L), A) = A.
:- pred foldl(pred(L, A, A), list(L), A, A).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, out) is multi, in, in, out) is multi.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldl(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

    % foldl2(Pred, List, !Acc1, !Acc2):
    %
    % Does the same job as foldl, but with two accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl2(pred(L, A, A, B, B), list(L), A, A, B, B).
:- mode foldl2(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldl2(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldl2(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode foldl2(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode foldl2(pred(in, in, out, mdi, muo) is nondet,
    in, in, out, mdi, muo) is nondet.
:- mode foldl2(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- mode foldl2(pred(in, in, out, mdi, muo) is cc_multi,
    in, in, out, mdi, muo) is cc_multi.
:- mode foldl2(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode foldl2(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.

    % foldl3(Pred, List, !Acc1, !Acc2, !Acc3):
    %
    % Does the same job as foldl, but with three accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl3(pred(L, A, A, B, B, C, C), list(L),
    A, A, B, B, C, C).
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
:- mode foldl3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out) is nondet.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, out, in, out, mdi, muo) is nondet.
:- mode foldl3(pred(in, in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out) is cc_multi.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, di, uo) is cc_multi.

    % foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4):
    % Does the same job as foldl, but with four accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl4(pred(L, A, A, B, B, C, C, D, D), list(L),
    A, A, B, B, C, C, D, D).
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out, in, out) is nondet.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is nondet,
    in, in, out, in, out, in, out, mdi, muo) is nondet.

    % foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5):
    % Does the same job as foldl, but with five accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), list(L),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is nondet,
    in, in, out, in, out, in, out, in, out, in, out) is nondet.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is nondet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is nondet.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi,
    in, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

    % foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6):
    % Does the same job as foldl, but with six accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F), list(L),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

    % foldl7(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6, !Acc7):
    % Does the same job as foldl, but with seven accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl7(pred(L, A, A, B, B, C, C, D, D, E, E, F, F, G, G), list(L),
    A, A, B, B, C, C, D, D, E, E, F, F, G, G).
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is det.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is det.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is cc_multi.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is cc_multi.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is semidet.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is semidet.
:- mode foldl7(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is nondet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is nondet.

    % foldl8(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6, !Acc7,
    %   !Acc8):
    % Does the same job as foldl, but with seven accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldl8(pred(L, A, A, B, B, C, C, D, D, E, E, F, F, G, G, H, H),
    list(L),
    A, A, B, B, C, C, D, D, E, E, F, F, G, G, H , H).
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is det.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, mdi, muo) is det.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is det.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is cc_multi.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is cc_multi.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is semidet.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, di, uo) is semidet.
:- mode foldl8(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out, in, out, in, out,
    in, out, in, out, in, out) is nondet.

%---------------------%

    % foldr(Func, List, Start) = End:
    % foldr(Func, List, Start, End):
    %
    % Calls Pred on each element of List, working right-to-left.
    % Each call to Pred will have a pair of arguments that represent
    % respectively the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- func foldr(func(L, A) = A, list(L), A) = A.
:- pred foldr(pred(L, A, A), list(L), A, A).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldr(pred(in, in, out) is multi, in, in, out) is multi.
:- mode foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldr(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

    % foldr2(Pred, List, !Acc1, !Acc2):
    %
    % Does the same job as foldr, but with two accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldr2(pred(L, A, A, B, B), list(L), A, A, B, B).
:- mode foldr2(pred(in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode foldr2(pred(in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode foldr2(pred(in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode foldr2(pred(in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode foldr2(pred(in, in, out, mdi, muo) is semidet, in, in, out,
    mdi, muo) is semidet.
:- mode foldr2(pred(in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.
:- mode foldr2(pred(in, in, out, in, out) is nondet, in, in, out,
    in, out) is nondet.
:- mode foldr2(pred(in, in, out, mdi, muo) is nondet, in, in, out,
    mdi, muo) is nondet.

    % foldr3(Pred, List, !Acc1, !Acc2, !Acc3):
    %
    % Does the same job as foldr, but with two accumulators.
    % Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient.
    % The last accumulator may be an I/O state, or some other
    % destructively updated piece of state.
    %
:- pred foldr3(pred(L, A, A, B, B, C, C), list(L), A, A, B, B, C, C).
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
:- mode foldr3(pred(in, in, out, in, out, in, out) is nondet, in,
    in, out, in, out, in, out) is nondet.
:- mode foldr3(pred(in, in, out, in, out, mdi, muo) is nondet, in,
    in, out, in, out, mdi, muo) is nondet.

%---------------------%

    % foldl_corresponding(P, As, Bs, !Acc):
    %
    % Does the same job as foldl, but works on two lists in parallel.
    % Raises an exception if the list arguments differ in length.
    %
:- pred foldl_corresponding(pred(A, B, C, C), list(A), list(B), C, C).
:- mode foldl_corresponding(pred(in, in, in, out) is det,
    in, in, in, out) is det.
:- mode foldl_corresponding(pred(in, in, mdi, muo) is det,
    in, in, mdi, muo) is det.
:- mode foldl_corresponding(pred(in, in, di, uo) is det,
    in, in, di, uo) is det.
:- mode foldl_corresponding(pred(in, in, in, out) is semidet,
    in, in, in, out) is semidet.
:- mode foldl_corresponding(pred(in, in, mdi, muo) is semidet,
    in, in, mdi, muo) is semidet.
:- mode foldl_corresponding(pred(in, in, di, uo) is semidet,
    in, in, di, uo) is semidet.
:- mode foldl_corresponding(pred(in, in, in, out) is nondet,
    in, in, in, out) is nondet.
:- mode foldl_corresponding(pred(in, in, mdi, muo) is nondet,
    in, in, mdi, muo) is nondet.
:- mode foldl_corresponding(pred(in, in, in, out) is cc_multi,
    in, in, in, out) is cc_multi.
:- mode foldl_corresponding(pred(in, in, di, uo) is cc_multi,
    in, in, di, uo) is cc_multi.

:- func foldl_corresponding(func(A, B, C) = C, list(A), list(B), C) = C.

    % foldl2_corresponding(F, As, Bs, !Acc1, !Acc2):
    %
    % Does the same job as foldl_corresponding, but with two accumulators.
    %
:- pred foldl2_corresponding(pred(A, B, C, C, D, D), list(A), list(B),
    C, C, D, D).
:- mode foldl2_corresponding(pred(in, in, in, out, in, out) is det,
    in, in, in, out, in, out) is det.
:- mode foldl2_corresponding(pred(in, in, in, out, mdi, muo) is det,
    in, in, in, out, mdi, muo) is det.
:- mode foldl2_corresponding(pred(in, in, in, out, di, uo) is det,
    in, in, in, out, di, uo) is det.
:- mode foldl2_corresponding(pred(in, in, in, out, in, out) is semidet,
    in, in, in, out, in, out) is semidet.
:- mode foldl2_corresponding(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, in, out, mdi, muo) is semidet.
:- mode foldl2_corresponding(pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.
:- mode foldl2_corresponding(pred(in, in, in, out, in, out) is nondet,
    in, in, in, out, in, out) is nondet.
:- mode foldl2_corresponding(pred(in, in, in, out, mdi, muo) is nondet,
    in, in, in, out, mdi, muo) is nondet.
:- mode foldl2_corresponding(pred(in, in, in, out, in, out) is cc_multi,
    in, in, in, out, in, out) is cc_multi.
:- mode foldl2_corresponding(pred(in, in, in, out, di, uo) is cc_multi,
    in, in, in, out, di, uo) is cc_multi.

    % foldl3_corresponding(F, As, Bs, !Acc1, !Acc2, !Acc3):
    %
    % Does the same job as foldl_corresponding, but with three accumulators.
    %
:- pred foldl3_corresponding(pred(A, B, C, C, D, D, E, E),
    list(A), list(B), C, C, D, D, E, E).
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out) is det.
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, di, uo) is det.
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out) is semidet.
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_corresponding(
    pred(in, in, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, di, uo) is semidet.

    % foldl4_corresponding(F, As, Bs, !Acc1, !Acc2, !Acc3, !Acc4):
    %
    % Does the same job as foldl_corresponding, but with four accumulators.
    %
:- pred foldl4_corresponding(pred(A, B, C, C, D, D, E, E, F, F),
    list(A), list(B), C, C, D, D, E, E, F, F).
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4_corresponding(
    pred(in, in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, in, out, di, uo) is semidet.

    % foldl_corresponding3(P, As, Bs, Cs, !Acc):
    %
    % Does the same jobs as foldl_corresponding, but folds over
    % three corresponding lists.
    %
:- pred foldl_corresponding3(pred(A, B, C, D, D),
    list(A), list(B), list(C), D, D).
:- mode foldl_corresponding3(pred(in, in, in, in, out) is det,
    in, in, in, in, out) is det.
:- mode foldl_corresponding3(pred(in, in, in, mdi, muo) is det,
    in, in, in, mdi, muo) is det.
:- mode foldl_corresponding3(pred(in, in, in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode foldl_corresponding3(pred(in, in, in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode foldl_corresponding3(pred(in, in, in, mdi, muo) is semidet,
    in, in, in, mdi, muo) is semidet.
:- mode foldl_corresponding3(pred(in, in, in, di, uo) is semidet,
    in, in, in, di, uo) is semidet.

    % foldl2_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2):
    %
    % Does the same jobs as foldl_corresponding3, but with two accumulators.
    %
:- pred foldl2_corresponding3(pred(A, B, C, D, D, E, E),
    list(A), list(B), list(C), D, D, E, E).
:- mode foldl2_corresponding3(pred(in, in, in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode foldl2_corresponding3(pred(in, in, in, in, out, mdi, muo) is det,
    in, in, in, in, out, mdi, muo) is det.
:- mode foldl2_corresponding3(pred(in, in, in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode foldl2_corresponding3(
    pred(in, in, in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode foldl2_corresponding3(
    pred(in, in, in, in, out, mdi, muo) is semidet,
    in, in, in, in, out, mdi, muo) is semidet.
:- mode foldl2_corresponding3(
    pred(in, in, in, in, out, di, uo) is semidet,
    in, in, in, in, out, di, uo) is semidet.

    % foldl3_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3):
    %
    % like foldl_corresponding3 but with three accumulators.
    %
:- pred foldl3_corresponding3(pred(A, B, C, D, D, E, E, F, F),
    list(A), list(B), list(C), D, D, E, E, F, F).
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out) is det.
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, di, uo) is det.
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out) is semidet.
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, di, uo) is semidet.

    % foldl4_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3, !Acc4):
    %
    % like foldl_corresponding3 but with four accumulators.
    %
:- pred foldl4_corresponding3(pred(A, B, C, D, D, E, E, F, F, G, G),
    list(A), list(B), list(C), D, D, E, E, F, F, G, G).
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

    % map_foldl(Pred, InList, OutList, Start, End):
    %
    % Calls Pred on each element of InList, working left-to-right.
    % The second argument of that call will be included in OutList,
    % while the third and fourth will represent respectively
    % the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- pred map_foldl(pred(L, M, A, A), list(L), list(M), A, A).
:- mode map_foldl(pred(in, out, in, out) is det, in, out, in, out)
    is det.
:- mode map_foldl(pred(in, out, mdi, muo) is det, in, out, mdi, muo)
    is det.
:- mode map_foldl(pred(in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode map_foldl(pred(in, out, in, out) is semidet, in, out, in, out)
    is semidet.
:- mode map_foldl(pred(in, out, mdi, muo) is semidet, in, out, mdi, muo)
    is semidet.
:- mode map_foldl(pred(in, out, di, uo) is semidet, in, out, di, uo)
    is semidet.
:- mode map_foldl(pred(in, in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode map_foldl(pred(in, out, in, out) is nondet, in, out, in, out)
    is nondet.
:- mode map_foldl(pred(in, out, mdi, muo) is nondet, in, out, mdi, muo)
    is nondet.
:- mode map_foldl(pred(in, out, in, out) is cc_multi, in, out, in, out)
    is cc_multi.
:- mode map_foldl(pred(in, out, mdi, muo) is cc_multi, in, out, mdi, muo)
    is cc_multi.
:- mode map_foldl(pred(in, out, di, uo) is cc_multi, in, out, di, uo)
    is cc_multi.

    % Same as map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(L, M, A, A, B, B), list(L), list(M), A, A, B, B).
:- mode map_foldl2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_foldl2(pred(in, out, in, out, mdi, muo) is det,
    in, out, in, out, mdi, muo) is det.
:- mode map_foldl2(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_foldl2(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.
:- mode map_foldl2(pred(in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl2(pred(in, out, in, out, di, uo) is semidet,
    in, out, in, out, di, uo) is semidet.
:- mode map_foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.
:- mode map_foldl2(pred(in, out, in, out, in, out) is cc_multi,
    in, out, in, out, in, out) is cc_multi.
:- mode map_foldl2(pred(in, out, in, out, mdi, muo) is cc_multi,
    in, out, in, out, mdi, muo) is cc_multi.
:- mode map_foldl2(pred(in, out, in, out, di, uo) is cc_multi,
    in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl2(pred(in, out, in, out, in, out) is nondet,
    in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with three accumulators.
    %
:- pred map_foldl3(pred(L, M, A, A, B, B, C, C), list(L), list(M),
    A, A, B, B, C, C).
:- mode map_foldl3(pred(in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl3(pred(in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_foldl3(pred(in, out, in, out, in, out, di, uo) is cc_multi,
    in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl3(pred(in, out, in, out, in, out, in, out) is cc_multi,
    in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl3(pred(in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl3(pred(in, out, in, out, in, out, in, out) is nondet,
    in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with four accumulators.
    %
:- pred map_foldl4(pred(L, M, A, A, B, B, C, C, D, D), list(L), list(M),
    A, A, B, B, C, C, D, D).
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is det,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
    is cc_multi,
    in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is cc_multi,
    in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is nondet,
    in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with five accumulators.
    %
:- pred map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
    list(L), list(M), A, A, B, B, C, C, D, D, E, E).
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is det,
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is det,
    in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet,
    in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with six accumulators.
    %
:- pred map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
    list(L), list(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det,
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is det,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi.
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is cc_multi.
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is semidet.
:- mode map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is nondet,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is nondet.

    % Same as map_foldl, but with two mapped outputs.
    %
:- pred map2_foldl(pred(L, M, N, A, A), list(L), list(M), list(N),
    A, A).
:- mode map2_foldl(pred(in, out, out, in, out) is det, in, out, out,
    in, out) is det.
:- mode map2_foldl(pred(in, out, out, mdi, muo) is det, in, out, out,
    mdi, muo) is det.
:- mode map2_foldl(pred(in, out, out, di, uo) is det, in, out, out,
    di, uo) is det.
:- mode map2_foldl(pred(in, out, out, in, out) is semidet, in, out, out,
    in, out) is semidet.
:- mode map2_foldl(pred(in, out, out, mdi, muo) is semidet, in, out, out,
    mdi, muo) is semidet.
:- mode map2_foldl(pred(in, out, out, di, uo) is semidet, in, out, out,
    di, uo) is semidet.
:- mode map2_foldl(pred(in, out, out, in, out) is nondet, in, out, out,
    in, out) is nondet.
:- mode map2_foldl(pred(in, out, out, mdi, muo) is nondet, in, out, out,
    mdi, muo) is nondet.
:- mode map2_foldl(pred(in, out, out, in, out) is cc_multi, in, out, out,
    in, out) is cc_multi.
:- mode map2_foldl(pred(in, out, out, di, uo) is cc_multi, in, out, out,
    di, uo) is cc_multi.

    % Same as map_foldl, but with two mapped outputs and two accumulators.
    %
:- pred map2_foldl2(pred(L, M, N, A, A, B, B), list(L), list(M), list(N),
    A, A, B, B).
:- mode map2_foldl2(pred(in, out, out, in, out, di, uo) is det,
    in, out, out, in, out, di, uo) is det.
:- mode map2_foldl2(pred(in, out, out, in, out, in, out) is det,
    in, out, out, in, out, in, out) is det.
:- mode map2_foldl2(pred(in, out, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl2(pred(in, out, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out) is cc_multi.
:- mode map2_foldl2(pred(in, out, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out) is semidet.
:- mode map2_foldl2(pred(in, out, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with two mapped outputs and three accumulators.
    %
:- pred map2_foldl3(pred(L, M, N, A, A, B, B, C, C),
    list(L), list(M), list(N), A, A, B, B, C, C).
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is det,
    in, out, out, in, out, in, out, in, out) is det.
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, di, uo) is det,
    in, out, out, in, out, in, out, di, uo) is det.
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out, in, out) is cc_multi.
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out, in, out) is semidet.
:- mode map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with two mapped outputs and four accumulators.
    %
:- pred map2_foldl4(pred(L, M, N, A, A, B, B, C, C, D, D),
    list(L), list(M), list(N), A, A, B, B, C, C, D, D).
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is det,
    in, out, out, in, out, in, out, in, out, in, out) is det.
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, di, uo) is det,
    in, out, out, in, out, in, out, in, out, di, uo) is det.
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with three mapped outputs.
    %
:- pred map3_foldl(pred(L, M, N, O, A, A), list(L), list(M), list(N),
    list(O), A, A).
:- mode map3_foldl(pred(in, out, out, out, in, out) is det, in, out, out,
    out, in, out) is det.
:- mode map3_foldl(pred(in, out, out, out, mdi, muo) is det, in, out, out,
    out, mdi, muo) is det.
:- mode map3_foldl(pred(in, out, out, out, di, uo) is det, in, out, out,
    out, di, uo) is det.
:- mode map3_foldl(pred(in, out, out, out, in, out) is semidet, in, out,
    out, out, in, out) is semidet.
:- mode map3_foldl(pred(in, out, out, out, mdi, muo) is semidet, in, out,
    out, out, mdi, muo) is semidet.
:- mode map3_foldl(pred(in, out, out, out, di, uo) is semidet, in, out,
    out, out, di, uo) is semidet.
:- mode map3_foldl(pred(in, out, out, out, in, out) is nondet, in, out,
    out, out, in, out) is nondet.
:- mode map3_foldl(pred(in, out, out, out, mdi, muo) is nondet, in, out,
    out, out, mdi, muo) is nondet.
:- mode map3_foldl(pred(in, out, out, out, in, out) is cc_multi, in, out,
    out, out, in, out) is cc_multi.
:- mode map3_foldl(pred(in, out, out, out, di, uo) is cc_multi, in, out,
    out, out, di, uo) is cc_multi.

    % Same as map_foldl, but with three mapped outputs and two accumulators.
    %
:- pred map3_foldl2(pred(L, M, N, O, A, A, B, B), list(L),
    list(M), list(N), list(O), A, A, B, B).
:- mode map3_foldl2(pred(in, out, out, out, in, out, di, uo) is det,
    in, out, out, out, in, out, di, uo) is det.
:- mode map3_foldl2(pred(in, out, out, out, in, out, in, out) is det,
    in, out, out, out, in, out, in, out) is det.
:- mode map3_foldl2(pred(in, out, out, out, in, out, di, uo) is cc_multi,
    in, out, out, out, in, out, di, uo) is cc_multi.
:- mode map3_foldl2(pred(in, out, out, out, in, out, in, out) is cc_multi,
    in, out, out, out, in, out, in, out) is cc_multi.
:- mode map3_foldl2(pred(in, out, out, out, in, out, in, out) is semidet,
    in, out, out, out, in, out, in, out) is semidet.
:- mode map3_foldl2(pred(in, out, out, out, in, out, in, out) is nondet,
    in, out, out, out, in, out, in, out) is nondet.

    % Same as map_foldl, but with four mapped outputs.
    %
:- pred map4_foldl(pred(L, M, N, O, P, A, A), list(L), list(M), list(N),
    list(O), list(P), A, A).
:- mode map4_foldl(pred(in, out, out, out, out, in, out) is det,
    in, out, out, out, out, in, out) is det.
:- mode map4_foldl(pred(in, out, out, out, out, mdi, muo) is det,
    in, out, out, out, out, mdi, muo) is det.
:- mode map4_foldl(pred(in, out, out, out, out, di, uo) is det,
    in, out, out, out, out, di, uo) is det.
:- mode map4_foldl(pred(in, out, out, out, out, in, out) is semidet,
    in, out, out, out, out, in, out) is semidet.
:- mode map4_foldl(pred(in, out, out, out, out, mdi, muo) is semidet,
    in, out, out, out, out, mdi, muo) is semidet.
:- mode map4_foldl(pred(in, out, out, out, out, di, uo) is semidet,
    in, out, out, out, out, di, uo) is semidet.
:- mode map4_foldl(pred(in, out, out, out, out, in, out) is nondet,
    in, out, out, out, out, in, out) is nondet.
:- mode map4_foldl(pred(in, out, out, out, out, mdi, muo) is nondet,
    in, out, out, out, out, mdi, muo) is nondet.
:- mode map4_foldl(pred(in, out, out, out, out, in, out) is cc_multi,
    in, out, out, out, out, in, out) is cc_multi.
:- mode map4_foldl(pred(in, out, out, out, out, di, uo) is cc_multi,
    in, out, out, out, out, di, uo) is cc_multi.

%---------------------%

    % map_foldr(Pred, InList, OutList, Start, End):
    %
    % Calls Pred on each element of InList, working right-to-left.
    % The second argument of that call will be included in OutList,
    % while the third and fourth will represent respectively
    % the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- pred map_foldr(pred(L, M, A, A), list(L), list(M), A, A).
:- mode map_foldr(pred(in, out, in, out) is det, in, out, in, out)
    is det.
:- mode map_foldr(pred(in, out, mdi, muo) is det, in, out, mdi, muo)
    is det.
:- mode map_foldr(pred(in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode map_foldr(pred(in, out, in, out) is semidet, in, out, in, out)
    is semidet.
:- mode map_foldr(pred(in, out, mdi, muo) is semidet, in, out, mdi, muo)
    is semidet.
:- mode map_foldr(pred(in, out, di, uo) is semidet, in, out, di, uo)
    is semidet.
:- mode map_foldr(pred(in, in, di, uo) is semidet, in, in, di, uo)
    is semidet.

%---------------------%

    % map_corresponding_foldl/6:
    %
    % A version of map_corresponding that has an accumulator
    % threaded through it.
    %
:- pred map_corresponding_foldl(pred(A, B, C, D, D),
    list(A), list(B), list(C), D, D).
:- mode map_corresponding_foldl(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode map_corresponding_foldl(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode map_corresponding_foldl(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode map_corresponding_foldl(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode map_corresponding_foldl(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode map_corresponding_foldl(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

    % Same as map_corresponding_foldl/6 but with two accumulators.
    %
:- pred map_corresponding_foldl2(pred(A, B, C, D, D, E, E),
    list(A), list(B), list(C), D, D, E, E).
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, in, out) is det, in, in, out, in, out,
    in, out) is det.
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, mdi, muo) is det, in, in, out, in, out,
    mdi, muo) is det.
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, di, uo) is det, in, in, out, in, out,
    di, uo) is det.
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, in, out) is semidet, in, in, out, in, out,
    in, out) is semidet.
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, mdi, muo) is semidet, in, in, out, in, out,
    mdi, muo) is semidet.
:- mode map_corresponding_foldl2(
    pred(in, in, out, in, out, di, uo) is semidet, in, in, out, in, out,
    di, uo) is semidet.

    % Same as map_corresponding_foldl/6 but with three accumulators.
    %
:- pred map_corresponding_foldl3(pred(A, B, C, D, D, E, E, F, F),
    list(A), list(B), list(C), D, D, E, E, F, F).
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, in, out) is det, in, in, out, in, out,
    in, out, in, out) is det.
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, mdi, muo) is det, in, in, out, in, out,
    in, out, mdi, muo) is det.
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, di, uo) is det, in, in, out, in, out,
    in, out, di, uo) is det.
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, in, out) is semidet, in, in, out,
    in, out, in, out, in, out) is semidet.
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in, in, out,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, di, uo) is semidet, in, in, out,
    in, out, in, out, di, uo) is semidet.

    % map_corresponding3_foldl/6:
    %
    % A version of map_corresponding3 that has an accumulator
    % threaded through it.
    %
:- pred map_corresponding3_foldl(pred(A, B, C, D, E, E),
    list(A), list(B), list(C), list(D), E, E).
:- mode map_corresponding3_foldl(pred(in, in, in, out, in, out) is det,
    in, in, in, out, in, out) is det.
:- mode map_corresponding3_foldl(pred(in, in, in, out, mdi, muo) is det,
    in, in, in, out, mdi, muo) is det.
:- mode map_corresponding3_foldl(pred(in, in, in, out, di, uo) is det,
    in, in, in, out, di, uo) is det.
:- mode map_corresponding3_foldl(
    pred(in, in, in, out, in, out) is semidet,
    in, in, in, out, in, out) is semidet.
:- mode map_corresponding3_foldl(
    pred(in, in, in, out, mdi, muo) is semidet,
    in, in, in, out, mdi, muo) is semidet.
:- mode map_corresponding3_foldl(
    pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.

%---------------------%

    % filter_map_foldl(Transformer, List, TrueList, Start, End):
    %
    % Takes a predicate with one input argument, one output argument and an
    % accumulator. It is called on each element of List. If the call succeeds,
    % then the output is included in TrueList and the accumulator is updated.
    %
:- pred filter_map_foldl(
    pred(X, Y, A, A)::in(pred(in, out, in, out) is semidet),
    list(X)::in, list(Y)::out, A::in, A::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Some declarations for complicated modes using lists.
    %
    % See our comment above regarding incomplete support for partial
    % instantiation.

:- inst list_skel == list_skel(free).

:- mode in_list_skel  == list_skel >> list_skel.
:- mode out_list_skel == free >> list_skel.
:- mode list_skel_out == list_skel >> ground.

    % These modes are useful for passing around lists of higher order terms,
    % since they have complicated insts which are not correctly approximated
    % by "ground".
    %
    % These could be made public (not deprecated) however I prefer to
    % encourage the in(list_skel(I)) and out(list_skel(I)) style syntax.
    %
:- mode list_skel_in(I)  == list_skel(I) >> list_skel(I).
:- mode list_skel_out(I) == free >> list_skel(I).

    % This is the same as the usual forward mode of append, but preserves
    % any extra information available in the input arguments.
    % NOTE_TO_IMPLEMENTORS If Mercury recorded the mode and determinism
    % NOTE_TO_IMPLEMENTORS information of higher order types in the *types*
    % NOTE_TO_IMPLEMENTORS of higher order variables instead of in their
    % NOTE_TO_IMPLEMENTORS *insts*, this function would not be needed.
    %
:- func inst_preserving_append(list(T)::in(list_skel(I =< ground)),
    list(T)::in(list_skel(I =< ground))) =
    (list(T)::out(list_skel(I =< ground))) is det.

    % This is the same as the usual forward mode of reverse, but preserves
    % any extra information available in the input argument.
    %
:- func inst_preserving_reverse(list(T)::in(list_skel(I =< ground))) =
    (list(T)::out(list_skel(I =< ground))) is det.

:- import_module term.      % for var/1.

:- pragma type_spec(list.merge(in, in, out), T = var(_)).

:- pragma type_spec(list.merge_and_remove_dups(in, in, out), T = var(_)).
:- pragma type_spec(list.merge_and_remove_dups(in, in) = out, T = var(_)).

:- pragma type_spec(pred(list.remove_adjacent_dups/2), T = var(_)).
:- pragma type_spec(func(list.remove_adjacent_dups/1), T = var(_)).

:- pragma type_spec(list.member(in, in), T = var(_)).

:- pragma type_spec(pred(list.sort_and_remove_dups/2), T = var(_)).
:- pragma type_spec(func(list.sort_and_remove_dups/1), T = var(_)).

:- pragma type_spec(list.sort(in, out), T = var(_)).
:- pragma type_spec(list.sort(in) = out, T = var(_)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

is_empty([]).

is_not_empty([_ | _]).

%---------------------------------------------------------------------------%

head([H | _]) = H.

det_head([]) = _ :-
    unexpected($pred, "empty list").
det_head([H | _]) = H.

tail([_ | T]) = T.

det_tail([]) = _ :-
    unexpected($pred, "empty list").
det_tail([_ | T]) = T.

det_head_tail([], _, _) :-
    unexpected($pred, "empty list").
det_head_tail([H | T], H, T).

cons(H, T) = [H | T].

cons(H, T, [H | T]).

%---------------------------------------------------------------------------%

append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :-
    list.append(Xs, Ys, Zs).

append(Xs, Ys) = Zs :-
    list.append(Xs, Ys, Zs).

L1 ++ L2 = list.append(L1, L2).

remove_suffix(List, Suffix, Prefix) :-
    list.length(List, ListLength),
    list.length(Suffix, SuffixLength),
    PrefixLength = ListLength - SuffixLength,
    list.split_list(PrefixLength, List, Prefix, Suffix).

%---------------------------------------------------------------------------%

% Note - it is not possible to write a version of list.length/1
% in pure Mercury that works in both directions unless you make it semidet
% rather than det.

length(Xs) = N :-
    list.length(Xs, N).

length(L, N) :-
    list.length_acc(L, 0, N).

:- pred length_acc(list(T), int, int).
:- mode length_acc(in, in, out) is det.

length_acc([], N, N).
length_acc([_ | L1], N0, N) :-
    N1 = N0 + 1,
    list.length_acc(L1, N1, N).

%---------------------------------------------------------------------------%

same_length([], []).
same_length([_ | L1], [_ | L2]) :-
    list.same_length(L1, L2).

same_length3([], [], []).
same_length3([_ | L1], [_ | L2], [_ | L3]) :-
    list.same_length3(L1, L2, L3).

%---------------------------------------------------------------------------%

member(X, [X | _]).
member(X, [_ | Xs]) :-
    list.member(X, Xs).

member(Element, List, SubList) :-
    SubList = [Element | _],
    list.append(_, SubList, List).

member_index0(X, [X | _], 0).
member_index0(X, [_ | Xs], Index + 1) :-
    list.member_index0(X, Xs, Index).

member_indexes0(X, List, Indexes) :-
    list.member_indexes0_loop(X, 0, List, Indexes).

:- pred member_indexes0_loop(T::in, int::in, list(T)::in,
    list(int)::out) is det.

member_indexes0_loop(_X, _I, [], []).
member_indexes0_loop(X, I, [H | T], Indexes) :-
    list.member_indexes0_loop(X, I + 1, T, IndexesTail),
    ( if X = H then
        Indexes = [I | IndexesTail]
    else
        Indexes = IndexesTail
    ).

contains(List, Elem) :-
    list.member(Elem, List).

%---------------------------------------------------------------------------%

index0([X | Xs], N, Elem) :-
    ( if N = 0 then
        Elem = X
    else
        list.index0(Xs, N - 1, Elem)
    ).

index1(List, N, Elem) :-
    list.index0(List, N - 1, Elem).

det_index0(Xs, N) = A :-
    list.det_index0(Xs, N, A).

det_index0(List, N, Elem) :-
    ( if list.index0(List, N, Elem0) then
        Elem = Elem0
    else
        unexpected($pred, "index out of range")
    ).

det_index1(Xs, N) = A :-
    list.det_index1(Xs, N, A).

det_index1(Xs, N, Elem) :-
    list.det_index0(Xs, N - 1, Elem).

%---------------------------------------------------------------------------%

nth_member_search(Xs, SearchX, N) :-
    list.index1_of_first_occurrence(Xs, SearchX, N).

nth_member_lookup(Xs, SearchX, N) :-
    N = list.det_index1_of_first_occurrence(Xs, SearchX).

%---------------------------------------------------------------------------%

index0_of_first_occurrence(Xs, SearchX, N) :-
    list.index0_of_first_occurrence_2(Xs, SearchX, 0, N).

:- pred index0_of_first_occurrence_2(list(T)::in, T::in,
    int::in, int::out) is semidet.

index0_of_first_occurrence_2([X | Xs], SearchX, Cur, N) :-
    ( if X = SearchX then
        N = Cur
    else
        list.index0_of_first_occurrence_2(Xs, SearchX, Cur + 1, N)
    ).

index1_of_first_occurrence(Xs, SearchX, N + 1) :-
    list.index0_of_first_occurrence(Xs, SearchX, N).

det_index0_of_first_occurrence(Xs, SearchX) = N :-
    ( if list.index0_of_first_occurrence(Xs, SearchX, N0) then
        N = N0
    else
        unexpected($pred, "item not found")
    ).

det_index1_of_first_occurrence(Xs, SearchX) = N :-
    ( if list.index1_of_first_occurrence(Xs, SearchX, N0) then
        N = N0
    else
        unexpected($pred, "item not found")
    ).

%---------------------------------------------------------------------------%

    % reverse(A, B) <=> reverse(B, A).
:- pragma promise_equivalent_clauses(pred(list.reverse/2)).

reverse(L0::in, L::out) :-
    list.reverse_prepend(L0, [], L).
reverse(L::out, L0::in) :-
    list.reverse_prepend(L0, [], L).

reverse(Xs) = Ys :-
    list.reverse(Xs, Ys).

reverse_prepend([], L, L).
reverse_prepend([X | Xs], L0, L) :-
    list.reverse_prepend(Xs, [X | L0], L).

reverse_prepend(Xs, Ys) = Zs :-
    list.reverse_prepend(Xs, Ys, Zs).

%---------------------------------------------------------------------------%

insert(Elem, List0, List) :-
    list.delete(List, Elem, List0).

delete([X | Xs], ToDelete, Xs) :-
    X = ToDelete.
delete([X | Xs], ToDelete, [X | DXs]) :-
    list.delete(Xs, ToDelete, DXs).

delete_first([X | Xs], ToDelete, DXs) :-
    ( if X = ToDelete then
        DXs = Xs
    else
        list.delete_first(Xs, ToDelete, DXs0),
        DXs = [X | DXs0]
    ).

delete_all(Xs, A) = DXs :-
    list.delete_all(Xs, A, DXs).

delete_all([], _, []).
delete_all([X | Xs], ToDelete, DXs) :-
    ( if X = ToDelete then
        list.delete_all(Xs, ToDelete, DXs)
    else
        list.delete_all(Xs, ToDelete, DXs0),
        DXs = [X | DXs0]
    ).

delete_nth([X | Xs], N, Result) :-
    ( if N > 1 then
        delete_nth(Xs, N - 1, ResultTail),
        Result = [X | ResultTail]
    else
        Result = Xs
    ).

delete_elems(Xs, ToDeletes) = DXs :-
    list.delete_elems(Xs, ToDeletes, DXs).

delete_elems(Xs, [], Xs).
delete_elems(Xs, [ToDelete | ToDeletes], DDXs) :-
    list.delete_all(Xs, ToDelete, DXs),
    list.delete_elems(DXs, ToDeletes, DDXs).

sublist([], _).
sublist([SH | ST], [FH | FT]) :-
    ( if SH = FH then
        list.sublist(ST, FT)
    else
        list.sublist([SH | ST], FT)
    ).

%---------------------------------------------------------------------------%

replace([X | Xs0], From, To, [To | Xs0]) :-
    X = From.
replace([X | Xs0], From, To, [X | Xs]) :-
    list.replace(Xs0, From, To, Xs).

replace_first([X | Xs], From, To, RXs) :-
    ( if X = From then
        RXs = [To | Xs]
    else
        list.replace_first(Xs, From, To, RXs0),
        RXs = [X | RXs0]
    ).

replace_all(Xs, A, B) = RXs :-
    list.replace_all(Xs, A, B, RXs).

replace_all([], _, _, []).
replace_all([X | Xs], From, To, RXs) :-
    ( if X = From then
        list.replace_all(Xs, From, To, RXs0),
        RXs = [To | RXs0]
    else
        list.replace_all(Xs, From, To, RXs0),
        RXs = [X | RXs0]
    ).

replace_nth(Xs, N, To, RXs) :-
    N > 0,
    list.replace_nth_loop(Xs, N, To, RXs).

:- pred replace_nth_loop(list(T)::in, int::in, T::in, list(T)::out)
    is semidet.

replace_nth_loop([X | Xs], N, To, RXs) :-
    ( if N > 1 then
        list.replace_nth_loop(Xs, N - 1, To, RXs0),
        RXs = [X | RXs0]
    else if N = 1 then
        RXs = [To | Xs]
    else
        fail
    ).

det_replace_nth(Xs, N, To) = RXs :-
    list.det_replace_nth(Xs, N, To, RXs).

det_replace_nth(Xs, N, To, RXs) :-
    ( if N > 0 then
        ( if list.replace_nth_loop(Xs, N, To, RXsPrime) then
            RXs = RXsPrime
        else
            unexpected($pred,
                "Cannot replace element whose index position " ++
                "is past the end of the list")
        )
    else
        unexpected($pred,
            "Cannot replace element whose index position is less than 1.")
    ).

%---------------------------------------------------------------------------%

Lo `..` Hi = List :-
    successive_integers(Lo, Hi, [], List).

:- pred successive_integers(int::in, int::in, list(int)::in, list(int)::out)
    is det.

successive_integers(Lo, Hi, !Ints) :-
    ( if Lo =< Hi then
        !:Ints = [Hi | !.Ints],
        successive_integers(Lo, Hi - 1, !Ints)
    else
        true
    ).

%---------------------------------------------------------------------------%

series(I, OK, Succ) = Series :-
    % In order to ensure that our stack consumption is constant,
    % not linear, we build the series "backwards" and then reverse it.
    series_acc(I, OK, Succ, [], RevSeries),
    reverse(RevSeries, Series).

:- pred series_acc(T, pred(T), func(T) = T, list(T), list(T)).
:- mode series_acc(in, pred(in) is semidet, func(in) = out is det, in, out)
    is det.

series_acc(I, OK, Succ, !RevSeries) :-
    ( if OK(I) then
        !:RevSeries = [I | !.RevSeries],
        series_acc(Succ(I), OK, Succ, !RevSeries)
    else
        true
    ).

%---------------------------------------------------------------------------%

remove_dups(Xs) = FilteredXs :-
    remove_dups(Xs, FilteredXs).

remove_dups(Xs, FilteredXs) :-
    remove_dups_loop(Xs, set_tree234.init, FilteredXs).

:- pred remove_dups_loop(list(T)::in, set_tree234(T)::in, list(T)::out)
    is det.

remove_dups_loop([], _SoFar, []).
remove_dups_loop([X | Xs], SoFar0, FilteredXs) :-
    ( if set_tree234.contains(SoFar0, X) then
        remove_dups_loop(Xs, SoFar0, FilteredXs)
    else
        set_tree234.insert(X, SoFar0, SoFar),
        remove_dups_loop(Xs, SoFar, FilteredXsTail),
        FilteredXs = [X | FilteredXsTail]
    ).

%---------------------------------------------------------------------------%

remove_adjacent_dups(Xs) = FilteredXs :-
    list.remove_adjacent_dups(Xs, FilteredXs).

remove_adjacent_dups([], []).
remove_adjacent_dups([X | Xs], FilteredXs) :-
    remove_adjacent_dups_loop(X, Xs, FilteredXs).

:- pred remove_adjacent_dups_loop(T::in, list(T)::in, list(T)::out) is det.
:- pragma type_spec(pred(list.remove_adjacent_dups_loop/3), T = var(_)).

remove_adjacent_dups_loop(X, [], [X]).
remove_adjacent_dups_loop(X0, [X1 | Xs], FilteredXs) :-
    ( if X0 = X1 then
        remove_adjacent_dups_loop(X0, Xs, FilteredXs)
    else
        remove_adjacent_dups_loop(X1, Xs, FilteredXsTail),
        FilteredXs = [X0 | FilteredXsTail]
    ).

remove_adjacent_dups(_, [], []).
remove_adjacent_dups(ComparePred, [X | Xs], FilteredXs) :-
    remove_adjacent_dups_loop(ComparePred, X, Xs, FilteredXs).

:- pred remove_adjacent_dups_loop(comparison_pred(T)::in(comparison_pred),
    T::in, list(T)::in, list(T)::out) is det.

remove_adjacent_dups_loop(_, X, [], [X]).
remove_adjacent_dups_loop(ComparePred, X0, [X1 | Xs], FilteredXs) :-
    ( if ComparePred(X0, X1, (=)) then
        remove_adjacent_dups_loop(ComparePred, X0, Xs, FilteredXs)
    else
        remove_adjacent_dups_loop(ComparePred, X1, Xs, FilteredXsTail),
        FilteredXs = [X0 | FilteredXsTail]
    ).

%---------------------------------------------------------------------------%

merge(As, Bs) = Cs :-
    list.merge(As, Bs, Cs).

merge([], [], []).
merge([], [B | Bs], [B | Bs]).
merge([A | As], [], [A | As]).
merge([A | As], [B | Bs], Cs) :-
    ( if compare(>, A, B) then
        list.merge([A | As], Bs, Cs0),
        Cs = [B | Cs0]
    else
        % If compare((=), A, B), take A first.
        list.merge(As, [B | Bs], Cs0),
        Cs = [A | Cs0]
    ).

merge(CompareFunc, As, Bs) = Cs :-
    ComparePred =
        ( pred(A::in, B::in, Res::out) is det :- Res = CompareFunc(A, B) ),
    list.merge(ComparePred, As, Bs, Cs).

merge(_ComparePred, [], [], []).
merge(_ComparePred, [], [Y | Ys], [Y | Ys]).
merge(_ComparePred, [A | As], [], [A | As]).
merge(ComparePred, [A | As], [Y | Ys], Cs) :-
    ( if ComparePred(A, Y, (>)) then
        list.merge(ComparePred, [A | As], Ys, CsTail),
        Cs = [Y | CsTail]
    else
        list.merge(ComparePred, As, [Y | Ys], CsTail),
        Cs = [A | CsTail]
    ).

merge_and_remove_dups(As, Bs) = Zs :-
    list.merge_and_remove_dups(As, Bs, Zs).

merge_and_remove_dups([], [], []).
merge_and_remove_dups([], [B | Bs], [B | Bs]).
merge_and_remove_dups([A | As], [], [A | As]).
merge_and_remove_dups([A | As], [B | Bs], Cs) :-
    compare(Res, A, B),
    (
        Res = (<),
        merge_and_remove_dups(As, [B | Bs], CsTail),
        Cs = [A | CsTail]
    ;
        Res = (=),
        merge_and_remove_dups(As, Bs, CsTail),
        Cs = [A | CsTail]
    ;
        Res = (>),
        merge_and_remove_dups([A | As], Bs, CsTail),
        Cs = [B | CsTail]
    ).

merge_and_remove_dups(CompareFunc, As, Bs) = Cs :-
    ComparePred =
        ( pred(A::in, B::in, Res::out) is det :- Res = CompareFunc(A, B) ),
    merge_and_remove_dups(ComparePred, As, Bs, Cs).

merge_and_remove_dups(_ComparePred, [], [], []).
merge_and_remove_dups(_ComparePred, [], [B | Bs], [B | Bs]).
merge_and_remove_dups(_ComparePred, [A | As], [], [A | As]).
merge_and_remove_dups(ComparePred, [A | As], [B | Bs], Cs) :-
    ComparePred(A, B, Res),
    (
        Res = (<),
        merge_and_remove_dups(ComparePred, As, [B | Bs], CsTail),
        Cs = [A | CsTail]
    ;
        Res = (=),
        merge_and_remove_dups(ComparePred, As, Bs, CsTail),
        Cs = [A | CsTail]
    ;
        Res = (>),
        merge_and_remove_dups(ComparePred, [A | As], Bs, CsTail),
        Cs = [B | CsTail]
    ).

%---------------------------------------------------------------------------%

sort(List) = SortedList :-
    sort(List, SortedList).

sort(List, SortedList) :-
    merge_sort(list.length(List), List, SortedList).

sort_and_remove_dups(List) = SortedList :-
    sort_and_remove_dups(List, SortedList).

sort_and_remove_dups(List, SortedList) :-
    merge_sort_and_remove_dups(list.length(List), List, SortedList).

%---------------------------------------------------------------------------%

:- pred merge_sort(int::in, list(T)::in, list(T)::out) is det.
:- pragma type_spec(list.merge_sort(in, in, out), T = var(_)).

merge_sort(Length, List, SortedList) :-
    ( if Length > 1 then
        HalfLength = Length // 2,
        det_split_list(HalfLength, List, Front, Back),
        merge_sort(HalfLength, Front, SortedFront),
        merge_sort(Length - HalfLength, Back, SortedBack),
        merge(SortedFront, SortedBack, SortedList)
    else
        SortedList = List
    ).

:- pred merge_sort_and_remove_dups(int::in, list(T)::in, list(T)::out)
    is det.
:- pragma type_spec(merge_sort_and_remove_dups(in, in, out), T = var(_)).

merge_sort_and_remove_dups(Length, List, SortedList) :-
    ( if Length > 1 then
        HalfLength = Length // 2,
        det_split_list(HalfLength, List, Front, Back),
        merge_sort_and_remove_dups(HalfLength, Front, SortedFront),
        merge_sort_and_remove_dups(Length - HalfLength, Back, SortedBack),
        merge_and_remove_dups(SortedFront, SortedBack, SortedList)
    else
        SortedList = List
    ).

%---------------------------------------------------------------------------%

sort(CompareFunc, Xs) = Ys :-
    ComparePred =
        ( pred(X::in, Y::in, Res::out) is det :-
            Res = CompareFunc(X, Y)
        ),
    sort(ComparePred, Xs, Ys).

sort(ComparePred, List, SortedList) :-
    list.length(List, N),
    ( if N = 0 then
        SortedList = []
    else
        ( if
            hosort(ComparePred, N, List, SortedListPrime, LeftOver),
            LeftOver = []
        then
            SortedList = SortedListPrime
        else
            unexpected($pred, "hosort failed")
        )
    ).

sort_and_remove_dups(ComparePred, L0, L) :-
    sort(ComparePred, L0, L1),
    remove_adjacent_dups(ComparePred, L1, L).

    % list.hosort is a Mercury implementation of the mergesort described
    % in The Craft of Prolog.
    %
    % N denotes the length of the part of L0 that this call is sorting.
    % (require((length(L0, M), M >= N)))
    % Since we have redundant information about the list (N, and the length
    % implicit in the list itself), we get a semidet unification when we
    % deconstruct the list. list.hosort is therefore actually det but the
    % compiler can't confirm it.
    %
:- pred hosort(comparison_pred(X)::in(comparison_pred), int::in,
    list(X)::in, list(X)::out, list(X)::out) is semidet.

hosort(ComparePred, N, List, SortedInitialN, LeftOver) :-
    ( if N = 1 then
        List = [X | LeftOver],
        SortedInitialN = [X]
    else if N = 2 then
        List = [X, Y | LeftOver],
        ComparePred(X, Y, Res),
        (
            Res = (<),
            SortedInitialN = [X, Y]
        ;
            Res = (=),
            SortedInitialN = [X, Y]
        ;
            Res = (>),
            SortedInitialN = [Y, X]
        )
    else
        N1 = N // 2,
        N2 = N - N1,
        hosort(ComparePred, N1, List, SortedInitialN1, Middle),
        hosort(ComparePred, N2, Middle, SortedNextN2, LeftOver),
        merge(ComparePred, SortedInitialN1, SortedNextN2, SortedInitialN)
    ).

%---------------------------------------------------------------------------%

split_list(N, List, Start, End) :-
    ( if N > 0 then
        List = [Head | Tail],
        list.split_list(N - 1, Tail, StartTail, End),
        Start = [Head | StartTail]
    else
        N = 0,
        Start = [],
        End = List
    ).

det_split_list(N, List, Start, End) :-
    ( if N > 0 then
        (
            List = [Head | Tail],
            list.det_split_list(N - 1, Tail, StartTail, End),
            Start = [Head | StartTail]
        ;
            List = [],
            unexpected($pred, "index out of range")
        )
    else if N = 0 then
        Start = [],
        End = List
    else
        unexpected($pred, "index out of range")
    ).

split_upto(N, List, Start, End) :-
    ( if N < 0 then
        unexpected($file, $pred, "index is negative")
    else
        do_split_upto(N, List, Start, End)
    ).

:- pred do_split_upto(int::in, list(T)::in, list(T)::out, list(T)::out)
    is det.

do_split_upto(N, List, Start, End) :-
    ( if
        N > 0,
        List = [Head | Tail]
    then
        do_split_upto(N - 1, Tail, StartTail, End),
        Start = [Head | StartTail]
    else
        Start = [],
        End = List
    ).

%---------------------------------------------------------------------------%

last([H | T], Last) :-
    (
        T = [],
        Last = H
    ;
        T = [_ | _],
        list.last(T, Last)
    ).

det_last(List) = Last :-
    list.det_last(List, Last).

det_last([], _) :-
    unexpected($pred, "empty list").
det_last([H | T], Last) :-
    list.det_last_loop(H, T, Last).

:- pred det_last_loop(T::in, list(T)::in, T::out) is det.

det_last_loop(H, T, Last) :-
    (
        T = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_last_loop(TH, TT, Last)
    ).

split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_loop(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

det_split_last([], _, _) :-
    unexpected($pred, "empty list").
det_split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_loop(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

:- pred det_split_last_loop(T::in, list(T)::in, list(T)::out, T::out) is det.

det_split_last_loop(H, T, AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_loop(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

%---------------------------------------------------------------------------%

take(N, Xs, InitialXs) :-
    ( if N > 0 then
        Xs = [HeadX | TailXs],
        list.take(N - 1, TailXs, InitialXsTail),
        InitialXs = [HeadX | InitialXsTail]
    else
        N = 0,
        InitialXs = []
    ).

det_take(N, Xs, InitialXs) :-
    ( if list.take(N, Xs, InitialXsPrime) then
        InitialXs = InitialXsPrime
    else
        unexpected($file, $pred, "index out of range")
    ).

take_upto(N, Xs) = InitialXs :-
    list.take_upto(N, Xs, InitialXs).

take_upto(N, Xs, InitialXs) :-
    ( if N < 0 then
        unexpected($file, $pred, "index is negative")
    else
        do_take_upto(N, Xs, InitialXs)
    ).

:- pred do_take_upto(int::in, list(T)::in, list(T)::out) is det.

do_take_upto(N, Xs, InitialXs) :-
    ( if list.take(N, Xs, InitialXsPrime) then
        InitialXs = InitialXsPrime
    else
        InitialXs = Xs
    ).

%---------------------------------------------------------------------------%

drop(N, Xs, FinalXs) :-
    ( if N > 0 then
        Xs = [_ | Tail],
        list.drop(N - 1, Tail, FinalXs)
    else
        N = 0,
        FinalXs = Xs
    ).

det_drop(N, Xs, FinalXs) :-
    ( if list.drop(N, Xs, FinalXsPrime) then
        FinalXs = FinalXsPrime
    else
        unexpected($file, $pred, "index out of range")
    ).

%---------------------------------------------------------------------------%

take_while(_, [], [], []).
take_while(P, [X | Xs], Ins, Outs) :-
    ( if P(X) then
        Ins = [X | Ins0],
        take_while(P, Xs, Ins0, Outs)
    else
        Ins = [],
        Outs = [X | Xs]
    ).

take_while(P, Xs) = Start :-
    take_while(P, Xs, Start).

take_while(_, [], []).
take_while(P, [X | Xs], Start) :-
    ( if P(X) then
        take_while(P, Xs, Start0),
        Start = [X | Start0]
    else
        Start = []
    ).

%---------------------------------------------------------------------------%

drop_while(P, Xs) = End :-
    drop_while(P, Xs, End).

drop_while(_, [], []).
drop_while(P, [X | Xs], End) :-
    ( if P(X) then
        drop_while(P, Xs, End)
    else
        End = [X | Xs]
    ).

%---------------------------------------------------------------------------%

duplicate(N, X) = Xs :-
    accumulate_n_copies(N, X, [], Xs).

duplicate(N, X, Xs) :-
    accumulate_n_copies(N, X, [], Xs).

:- pred accumulate_n_copies(int::in, T::in, list(T)::in, list(T)::out) is det.

accumulate_n_copies(N, X, !Xs) :-
    ( if N > 0 then
        !:Xs = [X | !.Xs],
        accumulate_n_copies(N - 1, X, !Xs)
    else
        true
    ).

all_same([]).
all_same([H | T]) :-
    all_same_as(H, T).

:- pred all_same_as(T::in, list(T)::in) is semidet.

all_same_as(_, []).
all_same_as(SameAs, [H | T]) :-
    H = SameAs,
    all_same_as(SameAs, T).

%---------------------------------------------------------------------------%

condense(Xss) = Ys :-
    list.condense(Xss, Ys).

condense(Xss, Ys) :-
    reverse(Xss, RevXss),
    condense_acc(RevXss, [], Ys).

:- pred condense_acc(list(list(T))::in, list(T)::in, list(T)::out) is det.

condense_acc([], !Ys).
condense_acc([L | Ls], !Ys) :-
    append(L, !Ys),
    condense_acc(Ls, !Ys).

chunk(Xs, N) = Ys :-
    chunk(Xs, N, Ys).

chunk(List, ChunkSize, ListOfSmallLists) :-
    chunk_loop(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred chunk_loop(list(T)::in, int::in, list(T)::in, int::in,
    list(list(T))::out) is det.

chunk_loop([], _ChunkSize, List0, _N, Lists) :-
    (
        List0 = [],
        Lists = []
    ;
        List0 = [_ | _],
        reverse(List0, List),
        Lists = [List]
    ).
chunk_loop([X | Xs], ChunkSize, List0, N, Lists) :-
    ( if N > 1 then
        chunk_loop(Xs, ChunkSize, [X | List0], N - 1, Lists)
    else
        reverse([X | List0], List),
        chunk_loop(Xs, ChunkSize, [], ChunkSize, ListsTail),
        Lists = [List | ListsTail]
    ).

%---------------------------------------------------------------------------%

zip(Xs, Ys) = Zs :-
    zip(Xs, Ys, Zs).

zip([], Bs, Bs).
zip([A | As], Bs, [A | Cs]) :-
    zip_2(As, Bs, Cs).

:- pred zip_2(list(T)::in, list(T)::in, list(T)::out) is det.

zip_2(As, [], As).
zip_2(As, [B | Bs], [B | Cs]) :-
    zip(As, Bs, Cs).

%---------------------------------------------------------------------------%

perm([], []).
perm([X | Xs], Ys) :-
    perm(Xs, Ys0),
    insert(X, Ys0, Ys).

%---------------------------------------------------------------------------%

list_to_doc(Xs) = indent(" ", [str("["), list_to_doc_2(Xs), str("]")]).

:- func list_to_doc_2(list(T)) = doc.

list_to_doc_2([]) = str("").
list_to_doc_2([X | Xs]) = Doc :-
    (
        Xs = [],
        Doc = format_arg(format(X))
    ;
        Xs = [_ | _],
        Doc = docs([
            format_arg(format(X)),
            group([str(", "), nl]),
            format_susp((func) = list_to_doc_2(Xs))
        ])
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

find_first_match(P, [H | T], FirstMatch) :-
    ( if P(H) then
        FirstMatch = H
    else
        find_first_match(P, T, FirstMatch)
    ).

any_true(P, L) :-
    not all_false(P, L).

any_false(P, L) :-
    not all_true(P, L).

all_true(_P, []).
all_true(P, [X | Xs]) :-
    P(X),
    all_true(P, Xs).

all_false(_P, []).
all_false(P, [X | Xs]) :-
    not P(X),
    all_false(P, Xs).

all_true_corresponding(_P, [], []).
all_true_corresponding(_P, [], [_ | _]) :-
    unexpected($pred, "mismatched list lengths").
all_true_corresponding(_P, [_ | _], []) :-
    unexpected($pred, "mismatched list lengths").
all_true_corresponding(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    all_true_corresponding(P, Xs, Ys).

all_false_corresponding(_P, [], []).
all_false_corresponding(_P, [], [_ | _]) :-
    unexpected($pred, "mismatched list lengths").
all_false_corresponding(_P, [_ | _], []) :-
    unexpected($pred, "mismatched list lengths").
all_false_corresponding(P, [X | Xs], [Y | Ys]) :-
    not P(X, Y),
    all_false_corresponding(P, Xs, Ys).

%---------------------------------------------------------------------------%

filter(P, Xs) = Trues :-
    filter(P, Xs, Trues).

filter(_, [],  []).
filter(P, [H | T], True) :-
    ( if P(H) then
        filter(P, T, TrueTail),
        True = [H | TrueTail]
    else
        filter(P, T, True)
    ).

filter(_, [],  [], []).
filter(P, [H | T], True, False) :-
    ( if P(H) then
        filter(P, T, TrueTail, False),
        True = [H | TrueTail]
    else
        filter(P, T, True, FalseTail),
        False = [H | FalseTail]
    ).

negated_filter(P, Xs) = Falses :-
    negated_filter(P, Xs, Falses).

negated_filter(_, [],  []).
negated_filter(P, [H | T], False) :-
    ( if P(H) then
        negated_filter(P, T, False)
    else
        negated_filter(P, T, FalseTail),
        False = [H | FalseTail]
    ).

filter_map(F, Xs) = Ys :-
    P = ( pred(X::in, Y::out) is semidet :- Y = F(X) ),
    filter_map(P, Xs, Ys).

filter_map(_, [],  []).
filter_map(P, [H0 | T0], True) :-
    ( if P(H0, H) then
        filter_map(P, T0, TrueTail),
        True = [H | TrueTail]
    else
        filter_map(P, T0, True)
    ).

filter_map(_, [], [], []).
filter_map(P, [H0 | T0], True, False) :-
    ( if P(H0, H) then
        filter_map(P, T0, TrueTail, False),
        True = [H | TrueTail]
    else
        filter_map(P, T0, True, FalseTail),
        False = [H0 | FalseTail]
    ).

find_first_map(P, [X | Xs], A) :-
    ( if P(X, A0) then
        A = A0
    else
        find_first_map(P, Xs, A)
    ).

find_first_map2(P, [X | Xs], A, B) :-
    ( if P(X, A0, B0) then
        A = A0,
        B = B0
    else
        find_first_map2(P, Xs, A, B)
    ).

find_first_map3(P, [X | Xs], A, B, C) :-
    ( if P(X, A0, B0, C0) then
        A = A0,
        B = B0,
        C = C0
    else
        find_first_map3(P, Xs, A, B, C)
    ).

find_index_of_match(Match, [X | Xs], Index0, Index) :-
    ( if Match(X) then
        Index = Index0
    else
        find_index_of_match(Match, Xs, Index0 + 1, Index)
    ).

%---------------------------------------------------------------------------%

map(_F, []) = [].
map(F, [H | T]) = [F(H) | list.map(F, T)].

map(_, [],  []).
map(P, [H0 | T0], [H | T]) :-
    P(H0, H),
    list.map(P, T0, T).

map2(_, [],  [],  []).
map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
    P(H0, H1, H2),
    list.map2(P, T0, T1, T2).

map3(_, [],  [],  [],  []).
map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
    P(H0, H1, H2, H3),
    list.map3(P, T0, T1, T2, T3).

map4(_, [], [], [], [], []).
map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
    P(H0, H1, H2, H3, H4),
    list.map4(P, T0, T1, T2, T3, T4).

map5(_, [], [], [], [], [], []).
map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5])
        :-
    P(H0, H1, H2, H3, H4, H5),
    list.map5(P, T0, T1, T2, T3, T4, T5).

map6(_, [], [], [], [], [], [], []).
map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6]) :-
    P(H0, H1, H2, H3, H4, H5, H6),
    list.map6(P, T0, T1, T2, T3, T4, T5, T6).

map7(_, [], [], [], [], [], [], [], []).
map7(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7),
    list.map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

map8(_, [], [], [], [], [], [], [], [], []).
map8(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7], [H8 | T8]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7, H8),
    list.map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

%---------------------------------------------------------------------------%

map_corresponding(_, [], []) = [].
map_corresponding(_, [], [_ | _]) =
    unexpected($pred, "mismatched list lengths").
map_corresponding(_, [_ | _], []) =
    unexpected($pred, "mismatched list lengths").
map_corresponding(F, [HA | TAs], [HB | TBs]) =
    [F(HA, HB) | list.map_corresponding(F, TAs, TBs)].

map_corresponding(_, [], [], []).
map_corresponding(_, [], [_ | _], _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding(_, [_ | _], [], _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding(P, [HA | TAs], [HB | TBs], [HR | TRs]) :-
    P(HA, HB, HR),
    list.map_corresponding(P, TAs, TBs, TRs).

map_corresponding3(F, A, B, C) =
    ( if
        A = [AH | AT],
        B = [BH | BT],
        C = [CH | CT]
    then
        [F(AH, BH, CH) | list.map_corresponding3(F, AT, BT, CT)]
    else if
        A = [],
        B = [],
        C = []
    then
        []
    else
        unexpected($pred, "mismatched list lengths")
    ).

map_corresponding3(P, A, B, C, R) :-
    ( if
        A = [AH | AT],
        B = [BH | BT],
        C = [CH | CT]
    then
        P(AH, BH, CH, RH),
        list.map_corresponding3(P, AT, BT, CT, RT),
        R = [RH | RT]
    else if
        A = [],
        B = [],
        C = []
    then
        R = []
    else
        unexpected($pred, "mismatched list lengths")
    ).

%---------------------------------------------------------------------------%

filter_map_corresponding(_, [], []) = [].
filter_map_corresponding(_, [], [_ | _]) =
    unexpected($pred, "mismatched list lengths").
filter_map_corresponding(_, [_ | _], []) =
    unexpected($pred, "mismatched list lengths").
filter_map_corresponding(F, [HA | TAs], [HB | TBs]) =
    ( if F(HA, HB) = HR then
        [HR | list.filter_map_corresponding(F, TAs, TBs)]
    else
        list.filter_map_corresponding(F, TAs, TBs)
    ).

filter_map_corresponding(_, [], [], []).
filter_map_corresponding(_, [], [_ | _], _) :-
    unexpected($pred, "mismatched list lengths").
filter_map_corresponding(_, [_ | _], [], _) :-
    unexpected($pred, "mismatched list lengths").
filter_map_corresponding(P, [HA | TAs], [HB | TBs], Rs) :-
    ( if P(HA, HB, HR) then
        list.filter_map_corresponding(P, TAs, TBs, TRs),
        Rs = [HR | TRs]
    else
        list.filter_map_corresponding(P, TAs, TBs, Rs)
    ).

filter_map_corresponding3(F, As, Bs, Cs) =
    ( if
        As = [HA | TAs],
        Bs = [HB | TBs],
        Cs = [HC | TCs]
    then
        ( if F(HA, HB, HC) = HR then
            [HR | list.filter_map_corresponding3(F, TAs, TBs, TCs)]
        else
            list.filter_map_corresponding3(F, TAs, TBs, TCs)
        )
    else if
        As = [],
        Bs = [],
        Cs = []
    then
        []
    else
        unexpected($pred, "mismatched list lengths")
    ).

filter_map_corresponding3(P, As, Bs, Cs, Rs) :-
    ( if
        As = [HA | TAs],
        Bs = [HB | TBs],
        Cs = [HC | TCs]
    then
        ( if P(HA, HB, HC, HR) then
            list.filter_map_corresponding3(P, TAs, TBs, TCs, TRs),
            Rs = [HR | TRs]
        else
            list.filter_map_corresponding3(P, TAs, TBs, TCs, Rs)
        )
    else if
        As = [],
        Bs = [],
        Cs = []
    then
        Rs = []
    else
        unexpected($pred, "mismatched list lengths")
    ).

%---------------------------------------------------------------------------%

foldl(F, Xs, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.foldl(P, Xs, A, B).

foldl(_, [], !A).
foldl(P, [H | T], !A) :-
    P(H, !A),
    list.foldl(P, T, !A).

foldl2(_, [], !A, !B).
foldl2(P, [H | T], !A, !B) :-
    P(H, !A, !B),
    list.foldl2(P, T, !A, !B).

foldl3(_, [], !A, !B, !C).
foldl3(P, [H | T], !A, !B, !C) :-
    P(H, !A, !B, !C),
    list.foldl3(P, T, !A, !B, !C).

foldl4(_, [], !A, !B, !C, !D).
foldl4(P, [H | T], !A, !B, !C, !D) :-
    P(H, !A, !B, !C, !D),
    list.foldl4(P, T, !A, !B, !C, !D).

foldl5(_, [], !A, !B, !C, !D, !E).
foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
    P(H, !A, !B, !C, !D, !E),
    list.foldl5(P, T, !A, !B, !C, !D, !E).

foldl6(_, [], !A, !B, !C, !D, !E, !F).
foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H, !A, !B, !C, !D, !E, !F),
    list.foldl6(P, T, !A, !B, !C, !D, !E, !F).

foldl7(_, [], !A, !B, !C, !D, !E, !F, !G).
foldl7(P, [H | T], !A, !B, !C, !D, !E, !F, !G) :-
    P(H, !A, !B, !C, !D, !E, !F, !G),
    list.foldl7(P, T, !A, !B, !C, !D, !E, !F, !G).

foldl8(_, [], !A, !B, !C, !D, !E, !F, !G, !H).
foldl8(P, [H | T], !A, !B, !C, !D, !E, !F, !G, !H) :-
    P(H, !A, !B, !C, !D, !E, !F, !G, !H),
    list.foldl8(P, T, !A, !B, !C, !D, !E, !F, !G, !H).

%---------------------------------------------------------------------------%

foldr(F, Xs, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.foldr(P, Xs, A, B).

foldr(_, [], !A).
foldr(P, [H | T], !A) :-
    list.foldr(P, T, !A),
    P(H, !A).

foldr2(_, [], !A, !B).
foldr2(P, [H | T], !A, !B) :-
    list.foldr2(P, T, !A, !B),
    P(H, !A, !B).

foldr3(_, [], !A, !B, !C).
foldr3(P, [H | T], !A, !B, !C) :-
    list.foldr3(P, T, !A, !B, !C),
    P(H, !A, !B, !C).

%---------------------------------------------------------------------------%

foldl_corresponding(_, [], [], !Acc).
foldl_corresponding(_, [], [_ | _], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding(_, [_ | _], [], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    P(A, B, !Acc),
    list.foldl_corresponding(P, As, Bs, !Acc).

foldl_corresponding(_, [], [], Acc) = Acc.
foldl_corresponding(_, [], [_ | _], _) = _ :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding(_, [_ | _], [], _) = _ :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding(F, [A | As], [B | Bs], !.Acc) = !:Acc :-
    !:Acc = F(A, B, !.Acc),
    !:Acc = list.foldl_corresponding(F, As, Bs, !.Acc).

foldl2_corresponding(_, [], [], !Acc1, !Acc2).
foldl2_corresponding(_, [], [_ | _], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding(_, [_ | _], [], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2) :-
    P(A, B, !Acc1, !Acc2),
    list.foldl2_corresponding(P, As, Bs, !Acc1, !Acc2).

foldl3_corresponding(_, [], [], !Acc1, !Acc2, !Acc3).
foldl3_corresponding(_, [], [_ | _], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding(_, [_ | _], [], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2, !Acc3) :-
    P(A, B, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, As, Bs, !Acc1, !Acc2, !Acc3).

foldl4_corresponding(_, [], [], !Acc1, !Acc2, !Acc3, !Acc4).
foldl4_corresponding(_, [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding(_, [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2, !Acc3, !Acc4) :-
    P(A, B, !Acc1, !Acc2, !Acc3, !Acc4),
    list.foldl4_corresponding(P, As, Bs, !Acc1, !Acc2, !Acc3, !Acc4).

foldl_corresponding3(_, [], [], [], !Acc).
foldl_corresponding3(_, [_ | _], [], [], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(_, [], [_ | _], [], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(_, [], [], [_ | _], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(_, [], [_ | _], [_ | _], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(_, [_ | _], [], [_ | _], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(_, [_ | _], [_ | _], [], _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs], !Acc) :-
    P(A, B, C, !Acc),
    list.foldl_corresponding3(P, As, Bs, Cs, !Acc).

foldl2_corresponding3(_, [], [], [], !Acc1, !Acc2).
foldl2_corresponding3(_, [_ | _], [], [], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(_, [], [_ | _], [], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(_, [], [], [_ | _], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl2_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2) :-
    P(A, B, C, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2).

foldl3_corresponding3(_, [], [], [], !Acc1, !Acc2, !Acc3).
foldl3_corresponding3(_, [_ | _], [], [], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(_, [], [_ | _], [], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(_, [], [], [_ | _], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl3_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2, !Acc3) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3).

foldl4_corresponding3(_, [], [], [], !Acc1, !Acc2, !Acc3, !Acc4).
foldl4_corresponding3(_, [_ | _], [], [], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(_, [], [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(_, [], [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
foldl4_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2, !Acc3, !Acc4) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3, !Acc4),
    list.foldl4_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3, !Acc4).

%---------------------------------------------------------------------------%

map_foldl(_, [], [], !A).
map_foldl(P, [H0 | T0], [H | T], !A) :-
    P(H0, H, !A),
    list.map_foldl(P, T0, T, !A).

map_foldl2(_, [], [], !A, !B).
map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
    P(H0, H, !A, !B),
    list.map_foldl2(P, T0, T, !A, !B).

map_foldl3(_, [], [], !A, !B, !C).
map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
    P(H0, H, !A, !B, !C),
    list.map_foldl3(P, T0, T, !A, !B, !C).

map_foldl4(_, [], [], !A, !B, !C, !D).
map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
    P(H0, H, !A, !B, !C, !D),
    list.map_foldl4(P, T0, T, !A, !B, !C, !D).

map_foldl5(_, [], [], !A, !B, !C, !D, !E).
map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
    P(H0, H, !A, !B, !C, !D, !E),
    list.map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H0, H, !A, !B, !C, !D, !E, !F),
    list.map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

map2_foldl(_, [], [], [], !A).
map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
    P(H0, H1, H2, !A),
    list.map2_foldl(P, T0, T1, T2, !A).

map2_foldl2(_, [], [], [], !A, !B).
map2_foldl2(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B) :-
    P(H0, H1, H2, !A, !B),
    list.map2_foldl2(P, T0, T1, T2, !A, !B).

map2_foldl3(_, [], [], [], !A, !B, !C).
map2_foldl3(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B, !C) :-
    P(H0, H1, H2, !A, !B, !C),
    list.map2_foldl3(P, T0, T1, T2, !A, !B, !C).

map2_foldl4(_, [], [], [], !A, !B, !C, !D).
map2_foldl4(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B, !C, !D) :-
    P(H0, H1, H2, !A, !B, !C, !D),
    list.map2_foldl4(P, T0, T1, T2, !A, !B, !C, !D).

map3_foldl(_, [], [], [], [], !A).
map3_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], !A) :-
    P(H0, H1, H2, H3, !A),
    list.map3_foldl(P, T0, T1, T2, T3, !A).

map3_foldl2(_, [], [], [], [], !A, !B).
map3_foldl2(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], !A, !B) :-
    P(H0, H1, H2, H3, !A, !B),
    list.map3_foldl2(P, T0, T1, T2, T3, !A, !B).

map4_foldl(_, [], [], [], [], [], !A).
map4_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], !A) :-
    P(H0, H1, H2, H3, H4, !A),
    list.map4_foldl(P, T0, T1, T2, T3, T4, !A).

map_foldr(_, [], [], !A).
map_foldr(P, [H0 | T0], [H | T], !A) :-
    list.map_foldr(P, T0, T, !A),
    P(H0, H, !A).

%---------------------------------------------------------------------------%

map_corresponding_foldl(_, [], [], [], !Acc).
map_corresponding_foldl(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl(_, [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl(P, [A | As], [B | Bs], [C | Cs], !Acc) :-
    P(A, B, C, !Acc),
    list.map_corresponding_foldl(P, As, Bs, Cs, !Acc).

map_corresponding_foldl2(_, [], [], [], !Acc1, !Acc2).
map_corresponding_foldl2(_, [], [_ | _], _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl2(_, [_ | _], [], _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl2(P, [A | As], [B | Bs], [C | Cs], !Acc1, !Acc2) :-
    P(A, B, C, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, As, Bs, Cs, !Acc1, !Acc2).

map_corresponding_foldl3(_, [], [], [], !Acc1, !Acc2, !Acc3).
map_corresponding_foldl3(_, [], [_ | _], _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl3(_, [_ | _], [], _, _, _, _, _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding_foldl3(P, [A | As], [B | Bs], [C | Cs], !Acc1,
        !Acc2, !Acc3) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3),
    list.map_corresponding_foldl3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3).

map_corresponding3_foldl(_, [], [], [], [], !Acc).
map_corresponding3_foldl(_, [], [_ | _], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(_, [_ | _], [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(_, [_ | _], [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(_, [], [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(_, [], [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(_, [_ | _], [], [], _, _, _) :-
    unexpected($pred, "mismatched list lengths").
map_corresponding3_foldl(P, [A | As], [B | Bs], [C | Cs], [D | Ds],
        !Acc) :-
    P(A, B, C, D, !Acc),
    list.map_corresponding3_foldl(P, As, Bs, Cs, Ds, !Acc).

%---------------------------------------------------------------------------%

filter_map_foldl(_, [], [], !A).
filter_map_foldl(P, [X | Xs], True, !A) :-
    ( if P(X, Y, !A) then
        list.filter_map_foldl(P, Xs, TrueTail, !A),
        True = [Y | TrueTail]
    else
        list.filter_map_foldl(P, Xs, True, !A)
    ).

%---------------------------------------------------------------------------%

inst_preserving_append([], L) = L.
inst_preserving_append([H | T], L) = [H | NT] :-
    inst_preserving_append(T, L) = NT.

inst_preserving_reverse(Xs) = Ys :-
    inst_preserving_reverse_prepend(Xs, [], Ys).

:- pred inst_preserving_reverse_prepend(list(T)::in(list_skel(I =< ground)),
    list(T)::in(list_skel(I =< ground)), list(T)::out(list_skel(I =< ground)))
    is det.

inst_preserving_reverse_prepend([], L, L).
inst_preserving_reverse_prepend([X | Xs], L0, L) :-
    inst_preserving_reverse_prepend(Xs, [X | L0], L).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "

// We don't use `:- pragma foreign_export' to generate these methods,
// because the interfaces would expect type_info arguments.

// If you need to specify the type parameter, you must use the qualified
// method name, e.g. list.<Integer>empty_list()

public static <E>
List_1<E> empty_list()
{
    return new List_1.F_nil_0<E>();
}

public static <E, F extends E>
List_1<E> cons(F head, List_1<E> tail)
{
    return new List_1.F_cons_2<E>(head, tail);
}

public static <E>
boolean is_empty(List_1<E> lst)
{
    return (lst instanceof List_1.F_nil_0);
}

public static <E>
E det_head(List_1<E> lst)
{
    return ((List_1.F_cons_2<E>) lst).F1;
}

public static <E>
List_1<E> det_tail(List_1<E> lst)
{
    return ((List_1.F_cons_2<E>) lst).F2;
}

// A wrapper class to allow for-each syntax.
// You must use a new instance of this class for each loop!

public static class ListIterator<E>
    implements java.lang.Iterable<E>, java.util.Iterator<E>
{
    private List_1<E> lst;

    public ListIterator(List_1<E> lst)
    {
        this.lst = lst;
    }

    public java.util.Iterator<E> iterator()
    {
        return this;
    }

    public boolean hasNext()
    {
        return !is_empty(lst);
    }

    public E next()
    {
        if (!is_empty(lst)) {
            E head = det_head(lst);
            lst = det_tail(lst);
            return head;
        } else {
            throw new java.util.NoSuchElementException();
        }
    }

    public void remove()
    {
        throw new java.lang.UnsupportedOperationException();
    }
}
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
public static List_1 empty_list()
{
    return new List_1.F_nil_0();
}

public static List_1 cons(object head, List_1 tail)
{
    return new List_1.F_cons_2(head, tail);
}

public static bool is_empty(List_1 lst)
{
    return (lst is List_1.F_nil_0);
}

public static object det_head(List_1 lst)
{
    return ((List_1.F_cons_2) lst).F1;
}

public static List_1 det_tail(List_1 lst)
{
    return ((List_1.F_cons_2) lst).F2;
}
").

%---------------------------------------------------------------------------%
:- end_module list.
%---------------------------------------------------------------------------%
