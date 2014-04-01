%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

%-----------------------------------------------------------------------------%

    % The definition of the type `list(T)':
    % A list is either an empty list, denoted `[]',
    % or an element `Head' of type `T' followed by a tail `Tail'
    % of type `list(T)', denoted `[Head | Tail]'.
    %
:- type list(T)
    --->    []
    ;       [T | list(T)].

%-----------------------------------------------------------------------------%

    % These instantiation states and modes can be used for instantiation
    % state subtyping.
    %
    % They could also be used for partial instantiation but partial
    % instantiation does not work completely, for information see the
    % LIMITATIONS file distributed with Mercury.
    %
:- inst list_skel(I) ---> [] ; [I | list_skel(I)].
:- inst list(I) == list_skel(I).

:- inst non_empty_list ---> [ground | ground].

%-----------------------------------------------------------------------------%

:- pred list.is_empty(list(T)::in) is semidet.

:- pred list.is_not_empty(list(T)::in) is semidet.

    % list.cons(X, Y, Z) <=> Z = [X | Y].
    %
:- pred list.cons(T::in, list(T)::in, list(T)::out) is det.
:- func list.cons(T, list(T)) = list(T).

    % Standard append predicate:
    % list.append(Start, End, List) is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- pred list.append(list(T), list(T), list(T)).
:- mode list.append(di, di, uo) is det.
:- mode list.append(in, in, out) is det.
:- mode list.append(in, in, in) is semidet.    % implied
:- mode list.append(in, out, in) is semidet.
:- mode list.append(out, out, in) is multi.
%   The following mode is semidet in the sense that it doesn't
%   succeed more than once - but it does create a choice-point,
%   which means it's inefficient and that the compiler can't deduce
%   that it is semidet.  Use list.remove_suffix instead.
% :- mode list.append(out, in, in) is semidet.

:- func list.append(list(T), list(T)) = list(T).

    % associativity of append
:- promise all [A, B, C, ABC]
    (
        ( some [AB] (list.append(A, B, AB), list.append(AB, C, ABC)) )
    <=>
        ( some [BC] (list.append(B, C, BC), list.append(A, BC, ABC)) )
    ).
    % construction equivalence law.
    % XXX when we implement rewrite rules, we should change this law
    % to a rewrite rule.
:- promise all [L, H, T] ( append([H], T, L) <=> L = [H | T] ).

    % L1 ++ L2 = L :- list.append(L1, L2, L).
    %
:- func list(T) ++ list(T) = list(T).

    % list.remove_suffix(List, Suffix, Prefix):
    %
    % The same as list.append(Prefix, Suffix, List) except that
    % this is semidet whereas list.append(out, in, in) is nondet.
    %
:- pred list.remove_suffix(list(T)::in, list(T)::in, list(T)::out) is semidet.

    % list.merge(L1, L2, L):
    %
    % L is the result of merging the elements of L1 and L2, in ascending order.
    % L1 and L2 must be sorted.
    %
:- pred list.merge(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list.merge(list(T), list(T)) = list(T).

    % list.merge_and_remove_dups(L1, L2, L):
    %
    % L is the result of merging the elements of L1 and L2, in ascending order,
    % and eliminating any duplicates. L1 and L2 must be sorted and must each
    % not contain any duplicates.
    %
:- pred list.merge_and_remove_dups(list(T)::in, list(T)::in, list(T)::out)
    is det.
:- func list.merge_and_remove_dups(list(T), list(T)) = list(T).

    % list.remove_adjacent_dups(L0, L):
    %
    % L is the result of replacing every sequence of duplicate elements in L0
    % with a single such element.
    %
:- pred list.remove_adjacent_dups(list(T)::in, list(T)::out) is det.
:- func list.remove_adjacent_dups(list(T)) = list(T).

    % list.remove_dups(L0, L):
    %
    % L is the result of deleting the second and subsequent occurrences
    % of every element that occurs twice in L0.
    %
:- pred list.remove_dups(list(T)::in, list(T)::out) is det.
:- func list.remove_dups(list(T)) = list(T).

    % list.member(Elem, List):
    %
    % True iff `List' contains `Elem'.
    %
:- pred list.member(T, list(T)).
:- mode list.member(in, in) is semidet.
:- mode list.member(out, in) is nondet.

    % list.member(Elem, List, SubList):
    %
    % True iff `List' contains `Elem', and `SubList' is a suffix of `List'
    % beginning with `Elem'.
    % Same as `SubList = [Elem | _], list.append(_, SubList, List)'.
    %
:- pred list.member(T::out, list(T)::in, list(T)::out) is nondet.

    % list.member_index0(Elem, List, Index).
    %
    % True iff `List' contains `Elem' at the zero-based index `Index'.
    %
:- pred list.member_index0(T, list(T), int).
:- mode list.member_index0(in, in, in) is semidet.
:- mode list.member_index0(in, in, out) is nondet.
:- mode list.member_index0(out, in, out) is nondet.

    % list.contains(List, Elem) iff list.member(Elem, List).
    % Sometimes you need the arguments in this order, because you want to
    % construct a closure with only the list.
    %
:- pred list.contains(list(T)::in, T::in) is semidet.

    % list.length(List, Length):
    %
    % True iff `Length' is the length of `List', i.e. if `List' contains
    % `Length' elements.
    %
:- pred list.length(list(_T), int).
:- mode list.length(in, out) is det.
    % XXX The current mode checker can't handle this mode
% :- mode list.length(input_list_skel, out) is det.

:- func list.length(list(T)) = int.

    % list.same_length(ListA, ListB):
    %
    % True iff `ListA' and `ListB' have the same length,
    % i.e. iff they both contain the same number of elements.
    %
:- pred list.same_length(list(T1), list(T2)).
    % XXX The current mode checker can't handle these modes.
% :- mode list.same_length(in, output_list_skel) is det.
% :- mode list.same_length(output_list_skel, in) is det.
:- mode list.same_length(in, in) is semidet.
% XXX The current mode checker can't handle these modes
% :- mode list.same_length(input_list_skel, output_list_skel) is det.
% :- mode list.same_length(output_list_skel, input_list_skel) is det.

    % As above, but for three lists.
    %
:- pred list.same_length3(list(T1)::in, list(T2)::in, list(T3)::in)
    is semidet.

    % list.split_list(Len, List, Start, End):
    %
    % splits `List' into a prefix `Start' of length `Len', and a remainder
    % `End'. See also: list.take, list.drop and list.split_upto.
    %
:- pred list.split_list(int::in, list(T)::in, list(T)::out, list(T)::out)
    is semidet.

    % list.det_split_list(Len, List, Start, End):
    %
    % A deterministic version of list.split_list, which aborts instead
    % of failing if Len > list.length(List).
    %
:- pred list.det_split_list(int::in, list(T)::in, list(T)::out, list(T)::out)
    is det.

    % list.split_upto(Len, List, Start, End):
    %
    % splits `List' into a prefix `Start' of length `min(Len, length(List))',
    % and a remainder `End'. See also: list.split_list, list.take, list.drop.
    %
:- pred list.split_upto(int::in, list(T)::in, list(T)::out, list(T)::out)
    is det.

    % list.take(Len, List, Start):
    %
    % `Start' is the first `Len' elements of `List'. Fails if `List' has
    % less than `Len' elements. See also: list.split_list.
    %
:- pred list.take(int::in, list(T)::in, list(T)::out) is semidet.

    % list.det_take(Len, List, Start):
    %
    % As above, but throw an exception instead of failing.
    %
:- pred list.det_take(int::in, list(T)::in, list(T)::out) is det.

    % list.take_upto(Len, List, Start):
    %
    % `Start' is the first `Len' elements of `List'. If `List' has less than
    % `Len' elements, return the entire list.
    %
:- pred list.take_upto(int::in, list(T)::in, list(T)::out) is det.
:- func list.take_upto(int, list(T)) = list(T).

    % list.drop(Len, List, End):
    %
    % `End' is the remainder of `List' after removing the first `Len' elements.
    % Fails if `List' does not have at least `Len' elements.
    % See also: list.split_list.
    %
:- pred list.drop(int::in, list(T)::in, list(T)::out) is semidet.

    % list.det_drop(Len, List, End):
    %
    % `End' is the remainder of `List' after removing the first `Len' elements.
    % Aborts if `List' does not have at least `Len' elements.
    % See also: list.split_list.
    %
:- pred list.det_drop(int::in, list(T)::in, list(T)::out) is det.

    % list.insert(Elem, List0, List):
    %
    % `List' is the result of inserting `Elem' somewhere in `List0'.
    % Same as `list.delete(List, Elem, List0)'.
    %
:- pred list.insert(T, list(T), list(T)).
:- mode list.insert(in, in, in) is semidet.
:- mode list.insert(in, out, in) is nondet.
:- mode list.insert(out, out, in) is nondet.
:- mode list.insert(in, in, out) is multi.

    % list.delete(List, Elem, Remainder):
    %
    % True iff `Elem' occurs in `List', and `Remainder' is the result of
    % deleting one occurrence of `Elem' from `List'.
    %
:- pred list.delete(list(T), T, list(T)).
:- mode list.delete(in, in, in) is semidet.
:- mode list.delete(in, in, out) is nondet.
:- mode list.delete(in, out, out) is nondet.
:- mode list.delete(out, in, in) is multi.

:- func list.delete_all(list(T), T) = list(T).

    % list.delete_first(List0, Elem, List) is true iff Elem occurs in List0
    % and List is List0 with the first occurrence of Elem removed.
    %
:- pred list.delete_first(list(T)::in, T::in, list(T)::out) is semidet.

    % list.delete_all(List0, Elem, List) is true iff List is List0 with
    % all occurrences of Elem removed.
    %
:- pred list.delete_all(list(T), T, list(T)).
:- mode list.delete_all(di, in, uo) is det.
:- mode list.delete_all(in, in, out) is det.

    % list.delete_elems(List0, Elems, List) is true iff List is List0 with
    % all occurrences of all elements of Elems removed.
    %
:- pred list.delete_elems(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list.delete_elems(list(T), list(T)) = list(T).

    % list.replace(List0, D, R, List) is true iff List is List0
    % with an occurrence of D replaced with R.
    %
:- pred list.replace(list(T), T, T, list(T)).
:- mode list.replace(in, in, in, in) is semidet.
:- mode list.replace(in, in, in, out) is nondet.

    % list.replace_first(List0, D, R, List) is true iff List is List0
    % with the first occurrence of D replaced with R.
    %
:- pred list.replace_first(list(T)::in, T::in, T::in, list(T)::out)
    is semidet.

    % list.replace_all(List0, D, R, List) is true iff List is List0
    % with all occurrences of D replaced with R.
    %
:- pred list.replace_all(list(T)::in, T::in, T::in, list(T)::out) is det.
:- func list.replace_all(list(T), T, T) = list(T).

    % list.replace_nth(List0, N, R, List) is true iff List is List0
    % with Nth element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- pred list.replace_nth(list(T)::in, int::in, T::in, list(T)::out)
    is semidet.

    % list.det_replace_nth(List0, N, R, List) is true iff List is List0
    % with Nth element replaced with R.
    % Aborts if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- pred list.det_replace_nth(list(T)::in, int::in, T::in, list(T)::out) is det.
:- func list.det_replace_nth(list(T), int, T) = list(T).

    % list.sort_and_remove_dups(List0, List):
    %
    % List is List0 sorted with the second and subsequent occurrence of
    % any duplicates removed.
    %
:- pred list.sort_and_remove_dups(list(T)::in, list(T)::out) is det.
:- func list.sort_and_remove_dups(list(T)) = list(T).

    % list.sort(List0, List):
    %
    % List is List0 sorted.
    %
:- pred list.sort(list(T)::in, list(T)::out) is det.
:- func list.sort(list(T)) = list(T).

    % list.reverse(List, Reverse):
    %
    % `Reverse' is a list containing the same elements as `List'
    % but in reverse order.
    %
:- pred list.reverse(list(T), list(T)).
:- mode list.reverse(in, out) is det.
:- mode list.reverse(out, in) is det.

:- func list.reverse(list(T)) = list(T).

    % list.perm(List0, List):
    %
    % True iff `List' is a permutation of `List0'.
    %
:- pred list.perm(list(T)::in, list(T)::out) is multi.

    % list.nth_member_search(List, Elem, Position):
    %
    % Elem is the Position'th member of List.
    % (Position numbers start from 1.)
    %
:- pred list.nth_member_search(list(T)::in, T::in, int::out) is semidet.

    % A deterministic version of list.nth_member_search, which aborts
    % instead of failing if the element is not found in the list.
    %
:- pred list.nth_member_lookup(list(T)::in, T::in, int::out) is det.

    % list.index*(List, Position, Elem):
    %
    % These predicates select an element in a list from it's position.
    % The `index0' preds consider the first element to be element
    % number zero, whereas the `index1' preds consider the first element
    % to be element number one. The `det_' preds call error/1 if the index
    % is out of range, whereas the semidet preds fail if the index is out of
    % range.
    %
:- pred list.index0(list(T)::in, int::in, T::out) is semidet.
:- pred list.index1(list(T)::in, int::in, T::out) is semidet.
:- pred list.det_index0(list(T)::in, int::in, T::out) is det.
:- pred list.det_index1(list(T)::in, int::in, T::out) is det.

:- func list.det_index0(list(T), int) = T.
:- func list.det_index1(list(T), int) = T.

    % list.index*_of_first_occurrence(List, Elem, Position):
    %
    % Computes the least value of Position such that
    % list_index*(List, Position, Elem). The `det_' funcs call error/1
    % if Elem is not a member of List.
    %
:- pred list.index0_of_first_occurrence(list(T)::in, T::in, int::out)
    is semidet.
:- pred list.index1_of_first_occurrence(list(T)::in, T::in, int::out)
    is semidet.
:- func list.det_index0_of_first_occurrence(list(T), T) = int.
:- func list.det_index1_of_first_occurrence(list(T), T) = int.

    % list.zip(ListA, ListB, List):
    %
    % List is the result of alternating the elements of ListA and ListB,
    % starting with the first element of ListA (followed by the first element
    % of ListB, then the second element of listA, then the second element
    % of ListB, etc.). When there are no more elements remaining in one of
    % the lists, the remainder of the nonempty list is appended.
    %
:- pred list.zip(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list.zip(list(T), list(T)) = list(T).

    % list.duplicate(Count, Elem, List) is true iff List is a list
    % containing Count duplicate copies of Elem.
    %
:- pred list.duplicate(int::in, T::in, list(T)::out) is det.
:- func list.duplicate(int, T) = list(T).

    % list.condense(ListOfLists, List):
    %
    % `List' is the result of concatenating all the elements of `ListOfLists'.
    %
:- pred list.condense(list(list(T))::in, list(T)::out) is det.
:- func list.condense(list(list(T))) = list(T).

    % list.chunk(List, ChunkSize, Chunks):
    %
    % Takes a list `List' and breaks it into a list of lists `Chunks',
    % such that the length of each list in `Chunks' is at most `ChunkSize.
    % (More precisely, the length of each list in `Chunks' other than the
    % last one is exactly `ChunkSize', and the length of the last list in
    % `Chunks' is between one and `ChunkSize'.)
    %
:- pred list.chunk(list(T)::in, int::in, list(list(T))::out) is det.
:- func list.chunk(list(T), int) = list(list(T)).

    % list.sublist(SubList, FullList) is true if one can obtain SubList
    % by starting with FullList and deleting some of its elements.
    %
:- pred list.sublist(list(T)::in, list(T)::in) is semidet.

    % list.all_same(List) is true if all elements of the list are the same.
    %
:- pred list.all_same(list(T)::in) is semidet.

    % list.last(List, Last) is true if Last is the last element of List.
    %
:- pred list.last(list(T)::in, T::out) is semidet.

    % A deterministic version of list.last, which aborts instead of
    % failing if the input list is empty.
    %
:- pred list.det_last(list(T)::in, T::out) is det.
:- func list.det_last(list(T)) = T.

    % list.split_last(List, AllButLast, Last) is true if Last is the
    % last element of List and AllButLast is the list of elements before it.
    %
:- pred list.split_last(list(T)::in, list(T)::out, T::out) is semidet.

    % A deterministic version of list.split_last, which aborts instead of
    % failing if the input list is empty.
    %
:- pred list.det_split_last(list(T)::in, list(T)::out, T::out) is det.

%-----------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%-----------------------------------------------------------------------------%

    % list.map(T, L, M) uses the closure T
    % to transform the elements of L into the elements of M.
    %
:- pred list.map(pred(X, Y), list(X), list(Y)).
:- mode list.map(pred(in, out) is det, in, out) is det.
:- mode list.map(pred(in, out) is cc_multi, in, out) is cc_multi.
:- mode list.map(pred(in, out) is semidet, in, out) is semidet.
:- mode list.map(pred(in, out) is multi, in, out) is multi.
:- mode list.map(pred(in, out) is nondet, in, out) is nondet.
:- mode list.map(pred(in, in) is semidet, in, in) is semidet.

:- func list.map(func(X) = Y, list(X)) = list(Y).

    % list.map2(T, L, M1, M2) uses the closure T
    % to transform the elements of L into the elements of M1 and M2.
    %
:- pred list.map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode list.map2(pred(in, out, out) is det, in, out, out) is det.
:- mode list.map2(pred(in, out, out) is cc_multi, in, out, out) is cc_multi.
:- mode list.map2(pred(in, out, out) is semidet, in, out, out) is semidet.
:- mode list.map2(pred(in, out, out) is multi, in, out, out) is multi.
:- mode list.map2(pred(in, out, out) is nondet, in, out, out) is nondet.
:- mode list.map2(pred(in, in, in) is semidet, in, in, in) is semidet.

    % list.map3(T, L, M1, M2, M3) uses the closure T
    % to transform the elements of L into the elements of M1, M2 and M3.
    %
:- pred list.map3(pred(A, B, C, D), list(A), list(B), list(C), list(D)).
:- mode list.map3(pred(in, out, out, out) is det, in, out, out, out) is det.
:- mode list.map3(pred(in, out, out, out) is cc_multi, in, out, out, out)
    is cc_multi.
:- mode list.map3(pred(in, out, out, out) is semidet, in, out, out, out)
    is semidet.
:- mode list.map3(pred(in, out, out, out) is multi, in, out, out, out)
    is multi.
:- mode list.map3(pred(in, out, out, out) is nondet, in, out, out, out)
    is nondet.
:- mode list.map3(pred(in, in, in, in) is semidet, in, in, in, in) is semidet.

    % list.map4(T, L, M1, M2, M3, M4) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3 and M4.
    %
:- pred list.map4(pred(A, B, C, D, E), list(A), list(B), list(C), list(D),
    list(E)).
:- mode list.map4(pred(in, out, out, out, out) is det, in, out, out, out, out)
    is det.
:- mode list.map4(pred(in, out, out, out, out) is cc_multi, in, out, out, out,
    out) is cc_multi.
:- mode list.map4(pred(in, out, out, out, out) is semidet, in, out, out, out,
    out) is semidet.
:- mode list.map4(pred(in, out, out, out, out) is multi, in, out, out, out,
    out) is multi.
:- mode list.map4(pred(in, out, out, out, out) is nondet, in, out, out, out,
    out) is nondet.
:- mode list.map4(pred(in, in, in, in, in) is semidet, in, in, in, in, in)
    is semidet.

    % list.map5(T, L, M1, M2, M3, M4, M5) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4
    % and M5.
    %
:- pred list.map5(pred(A, B, C, D, E, F), list(A), list(B), list(C), list(D),
    list(E), list(F)).
:- mode list.map5(pred(in, out, out, out, out, out) is det, in, out, out, out,
    out, out) is det.
:- mode list.map5(pred(in, out, out, out, out, out) is cc_multi, in, out, out,
    out, out, out) is cc_multi.
:- mode list.map5(pred(in, out, out, out, out, out) is semidet, in, out, out,
    out, out, out) is semidet.
:- mode list.map5(pred(in, out, out, out, out, out) is multi, in, out, out,
    out, out, out) is multi.
:- mode list.map5(pred(in, out, out, out, out, out) is nondet, in, out, out,
    out, out, out) is nondet.
:- mode list.map5(pred(in, in, in, in, in, in) is semidet, in, in, in, in, in,
    in) is semidet.

    % list.map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5 and M6.
    %
:- pred list.map6(pred(A, B, C, D, E, F, G), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G)).
:- mode list.map6(pred(in, out, out, out, out, out, out) is det, in, out, out,
    out, out, out, out) is det.
:- mode list.map6(pred(in, out, out, out, out, out, out) is cc_multi, in, out,
    out, out, out, out, out) is cc_multi.
:- mode list.map6(pred(in, out, out, out, out, out, out) is semidet, in, out,
    out, out, out, out, out) is semidet.
:- mode list.map6(pred(in, out, out, out, out, out, out) is multi, in, out,
    out, out, out, out, out) is multi.
:- mode list.map6(pred(in, out, out, out, out, out, out) is nondet, in, out,
    out, out, out, out, out) is nondet.
:- mode list.map6(pred(in, in, in, in, in, in, in) is semidet, in, in, in, in,
    in, in, in) is semidet.

    % list.map7(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6 and M7.
    %
:- pred list.map7(pred(A, B, C, D, E, F, G, H), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G), list(H)).
:- mode list.map7(pred(in, out, out, out, out, out, out, out) is det,
    in, out, out, out, out, out, out, out) is det.
:- mode list.map7(pred(in, out, out, out, out, out, out, out) is cc_multi,
    in, out, out, out, out, out, out, out) is cc_multi.
:- mode list.map7(pred(in, out, out, out, out, out, out, out) is semidet,
    in, out, out, out, out, out, out, out) is semidet.
:- mode list.map7(pred(in, out, out, out, out, out, out, out) is multi,
    in, out, out, out, out, out, out, out) is multi.
:- mode list.map7(pred(in, out, out, out, out, out, out, out) is nondet,
    in, out, out, out, out, out, out, out) is nondet.
:- mode list.map7(pred(in, in, in, in, in, in, in, in) is semidet,
    in, in, in, in, in, in, in, in) is semidet.

    % list.map8(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4,
    % M5, M6, M7 and M8.
    %
:- pred list.map8(pred(A, B, C, D, E, F, G, H, I), list(A), list(B), list(C),
    list(D), list(E), list(F), list(G), list(H), list(I)).
:- mode list.map8(pred(in, out, out, out, out, out, out, out, out) is det,
    in, out, out, out, out, out, out, out, out) is det.
:- mode list.map8(pred(in, out, out, out, out, out, out, out, out) is cc_multi,
    in, out, out, out, out, out, out, out, out) is cc_multi.
:- mode list.map8(pred(in, out, out, out, out, out, out, out, out) is semidet,
    in, out, out, out, out, out, out, out, out) is semidet.
:- mode list.map8(pred(in, out, out, out, out, out, out, out, out) is multi,
    in, out, out, out, out, out, out, out, out) is multi.
:- mode list.map8(pred(in, out, out, out, out, out, out, out, out) is nondet,
    in, out, out, out, out, out, out, out, out) is nondet.
:- mode list.map8(pred(in, in, in, in, in, in, in, in, in) is semidet,
    in, in, in, in, in, in, in, in, in) is semidet.

    % list.map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
    %   [F(A1, B1), .., F(An, Bn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func list.map_corresponding(func(A, B) = C, list(A), list(B)) = list(C).
:- pred list.map_corresponding(pred(A, B, C), list(A), list(B), list(C)).
:- mode list.map_corresponding(in(pred(in, in, out) is det), in, in, out)
    is det.
:- mode list.map_corresponding(in(pred(in, in, out) is semidet), in, in, out)
    is semidet.

    % list.map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func list.map_corresponding3(func(A, B, C) = D, list(A), list(B), list(C))
    = list(D).

    % list.filter_map_corresponding/3 is like list.map_corresponding/3
    % except the function argument is semidet and the output list
    % consists of only those applications of the function argument that
    % succeeded.
    %
:- func list.filter_map_corresponding(func(A, B) = C, list(A), list(B))
    = list(C).
:- mode list.filter_map_corresponding(func(in, in) = out is semidet, in, in)
    = out is det.

    % list.filter_map_corresponding3/4 is like list.map_corresponding3/4
    % except the function argument is semidet and the output list
    % consists of only those applications of the function argument that
    % succeeded.
    %
:- func list.filter_map_corresponding3(func(A, B, C) = D,
    list(A), list(B), list(C)) = list(D).
:- mode list.filter_map_corresponding3(func(in, in, in) = out is semidet,
    in, in, in) = out is det.

    % list.map_corresponding_foldl/6 is like list.map_corresponding except
    % that it has an accumulator threaded through it.
    %
:- pred list.map_corresponding_foldl(pred(A, B, C, D, D),
    list(A), list(B), list(C), D, D).
:- mode list.map_corresponding_foldl(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode list.map_corresponding_foldl(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode list.map_corresponding_foldl(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode list.map_corresponding_foldl(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode list.map_corresponding_foldl(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode list.map_corresponding_foldl(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

    % Like list.map_corresponding_foldl/6 except that it has two
    % accumulators.
    %
:- pred list.map_corresponding_foldl2(pred(A, B, C, D, D, E, E),
    list(A), list(B), list(C), D, D, E, E).
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, in, out) is det, in, in, out, in, out,
    in, out) is det.
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, mdi, muo) is det, in, in, out, in, out,
    mdi, muo) is det.
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, di, uo) is det, in, in, out, in, out,
    di, uo) is det.
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, in, out) is semidet, in, in, out, in, out,
    in, out) is semidet.
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, mdi, muo) is semidet, in, in, out, in, out,
    mdi, muo) is semidet.
:- mode list.map_corresponding_foldl2(
    pred(in, in, out, in, out, di, uo) is semidet, in, in, out, in, out,
    di, uo) is semidet.

    % Like list.map_corresponding_foldl/6 except that it has three
    % accumulators.
    %
:- pred list.map_corresponding_foldl3(pred(A, B, C, D, D, E, E, F, F),
    list(A), list(B), list(C), D, D, E, E, F, F).
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, in, out) is det, in, in, out, in, out,
    in, out, in, out) is det.
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, mdi, muo) is det, in, in, out, in, out,
    in, out, mdi, muo) is det.
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, di, uo) is det, in, in, out, in, out,
    in, out, di, uo) is det.
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, in, out) is semidet, in, in, out,
    in, out, in, out, in, out) is semidet.
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in, in, out,
    in, out, in, out, mdi, muo) is semidet.
:- mode list.map_corresponding_foldl3(
    pred(in, in, out, in, out, in, out, di, uo) is semidet, in, in, out,
    in, out, in, out, di, uo) is semidet.

    % list.map_corresponding3_foldl/7 is like list.map_corresponding3 except
    % that it has an accumulator threaded through it.
    %
:- pred list.map_corresponding3_foldl(pred(A, B, C, D, E, E),
    list(A), list(B), list(C), list(D), E, E).
:- mode list.map_corresponding3_foldl(pred(in, in, in, out, in, out) is det,
    in, in, in, out, in, out) is det.
:- mode list.map_corresponding3_foldl(pred(in, in, in, out, mdi, muo) is det,
    in, in, in, out, mdi, muo) is det.
:- mode list.map_corresponding3_foldl(pred(in, in, in, out, di, uo) is det,
    in, in, in, out, di, uo) is det.
:- mode list.map_corresponding3_foldl(
    pred(in, in, in, out, in, out) is semidet,
    in, in, in, out, in, out) is semidet.
:- mode list.map_corresponding3_foldl(
    pred(in, in, in, out, mdi, muo) is semidet,
    in, in, in, out, mdi, muo) is semidet.
:- mode list.map_corresponding3_foldl(
    pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.

    % list.foldl(Pred, List, Start, End) calls Pred with each
    % element of List (working left-to-right) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    %
:- pred list.foldl(pred(L, A, A), list(L), A, A).
:- mode list.foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode list.foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode list.foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list.foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list.foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode list.foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode list.foldl(pred(in, in, out) is multi, in, in, out) is multi.
:- mode list.foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list.foldl(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode list.foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode list.foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- func list.foldl(func(L, A) = A, list(L), A) = A.

    % list.foldl2(Pred, List, !Acc1, !Acc2):
    % Does the same job as list.foldl, but with two accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldl2(pred(L, A, A, Z, Z), list(L), A, A, Z, Z).
:- mode list.foldl2(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode list.foldl2(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode list.foldl2(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode list.foldl2(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode list.foldl2(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode list.foldl2(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode list.foldl2(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode list.foldl2(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode list.foldl2(pred(in, in, out, mdi, muo) is nondet,
    in, in, out, mdi, muo) is nondet.
:- mode list.foldl2(pred(in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- mode list.foldl2(pred(in, in, out, mdi, muo) is cc_multi,
    in, in, out, mdi, muo) is cc_multi.
:- mode list.foldl2(pred(in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode list.foldl2(pred(in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.

    % list.foldl3(Pred, List, !Acc1, !Acc2, !Acc3):
    % Does the same job as list.foldl, but with three accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldl3(pred(L, A, A, B, B, C, C), list(L),
    A, A, B, B, C, C).
:- mode list.foldl3(pred(in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode list.foldl3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode list.foldl3(pred(in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode list.foldl3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode list.foldl3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.
:- mode list.foldl3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out) is nondet.
:- mode list.foldl3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, out, in, out, mdi, muo) is nondet.
:- mode list.foldl3(pred(in, in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out) is cc_multi.
:- mode list.foldl3(pred(in, in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, di, uo) is cc_multi.

    % list.foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4):
    % Does the same job as list.foldl, but with four accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldl4(pred(L, A, A, B, B, C, C, D, D), list(L),
    A, A, B, B, C, C, D, D).
:- mode list.foldl4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out, in, out) is nondet.
:- mode list.foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is nondet,
    in, in, out, in, out, in, out, mdi, muo) is nondet.

    % list.foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5):
    % Does the same job as list.foldl, but with five accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), list(L),
    A, A, B, B, C, C, D, D, E, E).
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is nondet,
    in, in, out, in, out, in, out, in, out, in, out) is nondet.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is nondet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is nondet.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
    is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi,
    in, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

    % list.foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6):
    % Does the same job as list.foldl, but with six accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F), list(L),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.
:- mode list.foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

    % list.foldr(Pred, List, Start, End) calls Pred with each
    % element of List (working right-to-left) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    %
:- pred list.foldr(pred(L, A, A), list(L), A, A).
:- mode list.foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode list.foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode list.foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list.foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list.foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode list.foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode list.foldr(pred(in, in, out) is multi, in, in, out) is multi.
:- mode list.foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list.foldr(pred(in, mdi, muo) is nondet, in, mdi, muo) is nondet.
:- mode list.foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode list.foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- func list.foldr(func(L, A) = A, list(L), A) = A.

    % list.foldr2(Pred, List, !Acc1, !Acc2):
    % Does the same job as list.foldr, but with two accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldr2(pred(L, A, A, B, B), list(L), A, A, B, B).
:- mode list.foldr2(pred(in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode list.foldr2(pred(in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode list.foldr2(pred(in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode list.foldr2(pred(in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode list.foldr2(pred(in, in, out, mdi, muo) is semidet, in, in, out,
    mdi, muo) is semidet.
:- mode list.foldr2(pred(in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.
:- mode list.foldr2(pred(in, in, out, in, out) is nondet, in, in, out,
    in, out) is nondet.
:- mode list.foldr2(pred(in, in, out, mdi, muo) is nondet, in, in, out,
    mdi, muo) is nondet.

    % list.foldr3(Pred, List, !Acc1, !Acc2, !Acc3):
    % Does the same job as list.foldr, but with two accumulators.
    % (Although no more expressive than list.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred list.foldr3(pred(L, A, A, B, B, C, C), list(L), A, A, B, B, C, C).
:- mode list.foldr3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode list.foldr3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode list.foldr3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode list.foldr3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode list.foldr3(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode list.foldr3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.
:- mode list.foldr3(pred(in, in, out, in, out, in, out) is nondet, in,
    in, out, in, out, in, out) is nondet.
:- mode list.foldr3(pred(in, in, out, in, out, mdi, muo) is nondet, in,
    in, out, in, out, mdi, muo) is nondet.

    % list.foldl_corresponding(P, As, Bs, !Acc):
    % Does the same job as list.foldl, but works on two lists in
    % parallel.  An exception is raised if the list arguments differ
    % in length.
    %
:- pred list.foldl_corresponding(pred(A, B, C, C), list(A), list(B), C, C).
:- mode list.foldl_corresponding(pred(in, in, in, out) is det,
    in, in, in, out) is det.
:- mode list.foldl_corresponding(pred(in, in, mdi, muo) is det,
    in, in, mdi, muo) is det.
:- mode list.foldl_corresponding(pred(in, in, di, uo) is det,
    in, in, di, uo) is det.
:- mode list.foldl_corresponding(pred(in, in, in, out) is semidet,
    in, in, in, out) is semidet.
:- mode list.foldl_corresponding(pred(in, in, mdi, muo) is semidet,
    in, in, mdi, muo) is semidet.
:- mode list.foldl_corresponding(pred(in, in, di, uo) is semidet,
    in, in, di, uo) is semidet.
:- mode list.foldl_corresponding(pred(in, in, in, out) is nondet,
    in, in, in, out) is nondet.
:- mode list.foldl_corresponding(pred(in, in, mdi, muo) is nondet,
    in, in, mdi, muo) is nondet.
:- mode list.foldl_corresponding(pred(in, in, in, out) is cc_multi,
    in, in, in, out) is cc_multi.
:- mode list.foldl_corresponding(pred(in, in, di, uo) is cc_multi,
    in, in, di, uo) is cc_multi.

:- func list.foldl_corresponding(func(A, B, C) = C, list(A), list(B), C) = C.

    % list.foldl2_corresponding(F, As, Bs, !Acc1, !Acc2):
    % Does the same job as list.foldl_corresponding, but has two
    % accumulators.
    %
:- pred list.foldl2_corresponding(pred(A, B, C, C, D, D), list(A), list(B),
    C, C, D, D).
:- mode list.foldl2_corresponding(pred(in, in, in, out, in, out) is det,
    in, in, in, out, in, out) is det.
:- mode list.foldl2_corresponding(pred(in, in, in, out, mdi, muo) is det,
    in, in, in, out, mdi, muo) is det.
:- mode list.foldl2_corresponding(pred(in, in, in, out, di, uo) is det,
    in, in, in, out, di, uo) is det.
:- mode list.foldl2_corresponding(pred(in, in, in, out, in, out) is semidet,
    in, in, in, out, in, out) is semidet.
:- mode list.foldl2_corresponding(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, in, out, mdi, muo) is semidet.
:- mode list.foldl2_corresponding(pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.
:- mode list.foldl2_corresponding(pred(in, in, in, out, in, out) is nondet,
    in, in, in, out, in, out) is nondet.
:- mode list.foldl2_corresponding(pred(in, in, in, out, mdi, muo) is nondet,
    in, in, in, out, mdi, muo) is nondet.
:- mode list.foldl2_corresponding(pred(in, in, in, out, in, out) is cc_multi,
    in, in, in, out, in, out) is cc_multi.
:- mode list.foldl2_corresponding(pred(in, in, in, out, di, uo) is cc_multi,
    in, in, in, out, di, uo) is cc_multi.

    % list.foldl3_corresponding(F, As, Bs, !Acc1, !Acc2, !Acc3):
    % Does the same job as list.foldl_corresponding, but has three
    % accumulators.
    %
:- pred list.foldl3_corresponding(pred(A, B, C, C, D, D, E, E),
    list(A), list(B), C, C, D, D, E, E).
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, in, out) is det, in, in, in, out,
    in, out, in, out) is det.
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, mdi, muo) is det, in, in, in, out,
    in, out, mdi, muo) is det.
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, di, uo) is det, in, in, in, out,
    in, out, di, uo) is det.
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, in, out) is semidet, in, in, in, out,
    in, out, in, out) is semidet.
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, mdi, muo) is semidet, in, in, in, out,
    in, out, mdi, muo) is semidet.
:- mode list.foldl3_corresponding(
    pred(in, in, in, out, in, out, di, uo) is semidet, in, in, in, out,
    in, out, di, uo) is semidet.

    % list.foldl_corresponding3(P, As, Bs, Cs, !Acc):
    % Like list.foldl_corresponding but folds over three corresponding
    % lists.
    %
:- pred list.foldl_corresponding3(pred(A, B, C, D, D),
    list(A), list(B), list(C), D, D).
:- mode list.foldl_corresponding3(pred(in, in, in, in, out) is det,
    in, in, in, in, out) is det.
:- mode list.foldl_corresponding3(pred(in, in, in, mdi, muo) is det,
    in, in, in, mdi, muo) is det.
:- mode list.foldl_corresponding3(pred(in, in, in, di, uo) is det,
    in, in, in, di, uo) is det.
:- mode list.foldl_corresponding3(pred(in, in, in, in, out) is semidet,
    in, in, in, in, out) is semidet.
:- mode list.foldl_corresponding3(pred(in, in, in, mdi, muo) is semidet,
    in, in, in, mdi, muo) is semidet.
:- mode list.foldl_corresponding3(pred(in, in, in, di, uo) is semidet,
    in, in, in, di, uo) is semidet.

    % list.foldl2_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2):
    % like list.foldl_corresponding3 but with two accumulators.
    %
:- pred list.foldl2_corresponding3(pred(A, B, C, D, D, E, E),
    list(A), list(B), list(C), D, D, E, E).
:- mode list.foldl2_corresponding3(pred(in, in, in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode list.foldl2_corresponding3(pred(in, in, in, in, out, mdi, muo) is det,
    in, in, in, in, out, mdi, muo) is det.
:- mode list.foldl2_corresponding3(pred(in, in, in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode list.foldl2_corresponding3(
    pred(in, in, in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode list.foldl2_corresponding3(
    pred(in, in, in, in, out, mdi, muo) is semidet,
    in, in, in, in, out, mdi, muo) is semidet.
:- mode list.foldl2_corresponding3(
    pred(in, in, in, in, out, di, uo) is semidet,
    in, in, in, in, out, di, uo) is semidet.

    % list.foldl3_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3):
    % like list.foldl_corresponding3 but with three accumulators.
    %
:- pred list.foldl3_corresponding3(pred(A, B, C, D, D, E, E, F, F),
    list(A), list(B), list(C), D, D, E, E, F, F).
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out) is det.
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, mdi, muo) is det.
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, di, uo) is det.
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out) is semidet.
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl3_corresponding3(
    pred(in, in, in, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, di, uo) is semidet.

    % list.foldl4_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3, !Acc4):
    % like list.foldl_corresponding3 but with four accumulators.
    %
:- pred list.foldl4_corresponding3(pred(A, B, C, D, D, E, E, F, F, G, G),
    list(A), list(B), list(C), D, D, E, E, F, F, G, G).
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, in, out) is det,
    in, in, in, in, out, in, out, in, out, in, out) is det.
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, in, out, in, out, in, out, mdi, muo) is det.
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, in, in, out, in, out, in, out, di, uo) is det.
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode list.foldl4_corresponding3(
    pred(in, in, in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, in, out, in, out, in, out, di, uo) is semidet.

    % list.map_foldl(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working left-to-right) to transform
    % InList into OutList.  The final value of the accumulator is
    % returned in End.
    %
:- pred list.map_foldl(pred(L, M, A, A), list(L), list(M), A, A).
:- mode list.map_foldl(pred(in, out, in, out) is det, in, out, in, out)
    is det.
:- mode list.map_foldl(pred(in, out, mdi, muo) is det, in, out, mdi, muo)
    is det.
:- mode list.map_foldl(pred(in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode list.map_foldl(pred(in, out, in, out) is semidet, in, out, in, out)
    is semidet.
:- mode list.map_foldl(pred(in, out, mdi, muo) is semidet, in, out, mdi, muo)
    is semidet.
:- mode list.map_foldl(pred(in, out, di, uo) is semidet, in, out, di, uo)
    is semidet.
:- mode list.map_foldl(pred(in, in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode list.map_foldl(pred(in, out, in, out) is nondet, in, out, in, out)
    is nondet.
:- mode list.map_foldl(pred(in, out, mdi, muo) is nondet, in, out, mdi, muo)
    is nondet.
:- mode list.map_foldl(pred(in, out, in, out) is cc_multi, in, out, in, out)
    is cc_multi.
:- mode list.map_foldl(pred(in, out, mdi, muo) is cc_multi, in, out, mdi, muo)
    is cc_multi.
:- mode list.map_foldl(pred(in, out, di, uo) is cc_multi, in, out, di, uo)
    is cc_multi.

    % Same as list.map_foldl, but with two accumulators.
    %
:- pred list.map_foldl2(pred(L, M, A, A, B, B), list(L), list(M), A, A, B, B).
:- mode list.map_foldl2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode list.map_foldl2(pred(in, out, in, out, mdi, muo) is det,
    in, out, in, out, mdi, muo) is det.
:- mode list.map_foldl2(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode list.map_foldl2(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.
:- mode list.map_foldl2(pred(in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, mdi, muo) is semidet.
:- mode list.map_foldl2(pred(in, out, in, out, di, uo) is semidet,
    in, out, in, out, di, uo) is semidet.
:- mode list.map_foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, in, out, di, uo) is semidet.
:- mode list.map_foldl2(pred(in, out, in, out, in, out) is cc_multi,
    in, out, in, out, in, out) is cc_multi.
:- mode list.map_foldl2(pred(in, out, in, out, mdi, muo) is cc_multi,
    in, out, in, out, mdi, muo) is cc_multi.
:- mode list.map_foldl2(pred(in, out, in, out, di, uo) is cc_multi,
    in, out, in, out, di, uo) is cc_multi.
:- mode list.map_foldl2(pred(in, out, in, out, in, out) is nondet,
    in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with three accumulators.
    %
:- pred list.map_foldl3(pred(L, M, A, A, B, B, C, C), list(L), list(M),
    A, A, B, B, C, C).
:- mode list.map_foldl3(pred(in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode list.map_foldl3(pred(in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode list.map_foldl3(pred(in, out, in, out, in, out, di, uo) is cc_multi,
    in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.map_foldl3(pred(in, out, in, out, in, out, in, out) is cc_multi,
    in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.map_foldl3(pred(in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.
:- mode list.map_foldl3(pred(in, out, in, out, in, out, in, out) is nondet,
    in, out, in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with four accumulators.
    %
:- pred list.map_foldl4(pred(L, M, A, A, B, B, C, C, D, D), list(L), list(M),
    A, A, B, B, C, C, D, D).
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is det,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
    is cc_multi,
    in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is cc_multi,
    in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list.map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
    is nondet,
    in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with five accumulators.
    %
:- pred list.map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
    list(L), list(M), A, A, B, B, C, C, D, D, E, E).
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is det,
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is det,
    in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    di, uo) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list.map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
    in, out) is nondet,
    in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with six accumulators.
    %
:- pred list.map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
    list(L), list(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det,
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is det,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, di, uo) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out, di, uo)
    is cc_multi.
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is cc_multi,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is cc_multi.
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is semidet.
:- mode list.map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
    in, out, in, out) is nondet,
    in, out, in, out, in, out, in, out, in, out, in, out, in, out)
    is nondet.

    % Same as list.map_foldl, but with two mapped outputs.
    %
:- pred list.map2_foldl(pred(L, M, N, A, A), list(L), list(M), list(N),
    A, A).
:- mode list.map2_foldl(pred(in, out, out, in, out) is det, in, out, out,
    in, out) is det.
:- mode list.map2_foldl(pred(in, out, out, mdi, muo) is det, in, out, out,
    mdi, muo) is det.
:- mode list.map2_foldl(pred(in, out, out, di, uo) is det, in, out, out,
    di, uo) is det.
:- mode list.map2_foldl(pred(in, out, out, in, out) is semidet, in, out, out,
    in, out) is semidet.
:- mode list.map2_foldl(pred(in, out, out, mdi, muo) is semidet, in, out, out,
    mdi, muo) is semidet.
:- mode list.map2_foldl(pred(in, out, out, di, uo) is semidet, in, out, out,
    di, uo) is semidet.
:- mode list.map2_foldl(pred(in, out, out, in, out) is nondet, in, out, out,
    in, out) is nondet.
:- mode list.map2_foldl(pred(in, out, out, mdi, muo) is nondet, in, out, out,
    mdi, muo) is nondet.
:- mode list.map2_foldl(pred(in, out, out, in, out) is cc_multi, in, out, out,
    in, out) is cc_multi.
:- mode list.map2_foldl(pred(in, out, out, di, uo) is cc_multi, in, out, out,
    di, uo) is cc_multi.

    % Same as list.map_foldl, but with two mapped outputs and two
    % accumulators.
    %
:- pred list.map2_foldl2(pred(L, M, N, A, A, B, B), list(L), list(M), list(N),
    A, A, B, B).
:- mode list.map2_foldl2(pred(in, out, out, in, out, di, uo) is det,
    in, out, out, in, out, di, uo) is det.
:- mode list.map2_foldl2(pred(in, out, out, in, out, in, out) is det,
    in, out, out, in, out, in, out) is det.
:- mode list.map2_foldl2(pred(in, out, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, di, uo) is cc_multi.
:- mode list.map2_foldl2(pred(in, out, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out) is cc_multi.
:- mode list.map2_foldl2(pred(in, out, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out) is semidet.
:- mode list.map2_foldl2(pred(in, out, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with two mapped outputs and three
    % accumulators.
    %
:- pred list.map2_foldl3(pred(L, M, N, A, A, B, B, C, C),
    list(L), list(M), list(N), A, A, B, B, C, C).
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is det,
    in, out, out, in, out, in, out, in, out) is det.
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, di, uo) is det,
    in, out, out, in, out, in, out, di, uo) is det.
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out, in, out) is cc_multi.
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out, in, out) is semidet.
:- mode list.map2_foldl3(
    pred(in, out, out, in, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with two mapped outputs and four
    % accumulators.
    %
:- pred list.map2_foldl4(pred(L, M, N, A, A, B, B, C, C, D, D),
    list(L), list(M), list(N), A, A, B, B, C, C, D, D).
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is det,
    in, out, out, in, out, in, out, in, out, in, out) is det.
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, di, uo) is det,
    in, out, out, in, out, in, out, in, out, di, uo) is det.
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is cc_multi,
    in, out, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, di, uo) is cc_multi,
    in, out, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is semidet,
    in, out, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list.map2_foldl4(
    pred(in, out, out, in, out, in, out, in, out, in, out) is nondet,
    in, out, out, in, out, in, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with three mapped outputs.
    %
:- pred list.map3_foldl(pred(L, M, N, O, A, A), list(L), list(M), list(N),
    list(O), A, A).
:- mode list.map3_foldl(pred(in, out, out, out, in, out) is det, in, out, out,
    out, in, out) is det.
:- mode list.map3_foldl(pred(in, out, out, out, mdi, muo) is det, in, out, out,
    out, mdi, muo) is det.
:- mode list.map3_foldl(pred(in, out, out, out, di, uo) is det, in, out, out,
    out, di, uo) is det.
:- mode list.map3_foldl(pred(in, out, out, out, in, out) is semidet, in, out, 
    out, out, in, out) is semidet.
:- mode list.map3_foldl(pred(in, out, out, out, mdi, muo) is semidet, in, out,
    out, out, mdi, muo) is semidet.
:- mode list.map3_foldl(pred(in, out, out, out, di, uo) is semidet, in, out, 
    out, out, di, uo) is semidet.
:- mode list.map3_foldl(pred(in, out, out, out, in, out) is nondet, in, out, 
    out, out, in, out) is nondet.
:- mode list.map3_foldl(pred(in, out, out, out, mdi, muo) is nondet, in, out,
    out, out, mdi, muo) is nondet.
:- mode list.map3_foldl(pred(in, out, out, out, in, out) is cc_multi, in, out, 
    out, out, in, out) is cc_multi.
:- mode list.map3_foldl(pred(in, out, out, out, di, uo) is cc_multi, in, out, 
    out, out, di, uo) is cc_multi.

    % Same as list.map_foldl, but with three mapped outputs and two
    % accumulators.
    %
:- pred list.map3_foldl2(pred(L, M, N, O, A, A, B, B), list(L),
    list(M), list(N), list(O), A, A, B, B).
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, di, uo) is det,
    in, out, out, out, in, out, di, uo) is det.
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, in, out) is det,
    in, out, out, out, in, out, in, out) is det.
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, di, uo) is cc_multi,
    in, out, out, out, in, out, di, uo) is cc_multi.
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, in, out) is cc_multi,
    in, out, out, out, in, out, in, out) is cc_multi.
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, in, out) is semidet,
    in, out, out, out, in, out, in, out) is semidet.
:- mode list.map3_foldl2(pred(in, out, out, out, in, out, in, out) is nondet,
    in, out, out, out, in, out, in, out) is nondet.

    % Same as list.map_foldl, but with four mapped outputs.
    %
:- pred list.map4_foldl(pred(L, M, N, O, P, A, A), list(L), list(M), list(N),
    list(O), list(P), A, A).
:- mode list.map4_foldl(pred(in, out, out, out, out, in, out) is det,
    in, out, out, out, out, in, out) is det.
:- mode list.map4_foldl(pred(in, out, out, out, out, mdi, muo) is det,
    in, out, out, out, out, mdi, muo) is det.
:- mode list.map4_foldl(pred(in, out, out, out, out, di, uo) is det,
    in, out, out, out, out, di, uo) is det.
:- mode list.map4_foldl(pred(in, out, out, out, out, in, out) is semidet,
    in, out, out, out, out, in, out) is semidet.
:- mode list.map4_foldl(pred(in, out, out, out, out, mdi, muo) is semidet,
    in, out, out, out, out, mdi, muo) is semidet.
:- mode list.map4_foldl(pred(in, out, out, out, out, di, uo) is semidet,
    in, out, out, out, out, di, uo) is semidet.
:- mode list.map4_foldl(pred(in, out, out, out, out, in, out) is nondet,
    in, out, out, out, out, in, out) is nondet.
:- mode list.map4_foldl(pred(in, out, out, out, out, mdi, muo) is nondet,
    in, out, out, out, out, mdi, muo) is nondet.
:- mode list.map4_foldl(pred(in, out, out, out, out, in, out) is cc_multi,
    in, out, out, out, out, in, out) is cc_multi.
:- mode list.map4_foldl(pred(in, out, out, out, out, di, uo) is cc_multi,
    in, out, out, out, out, di, uo) is cc_multi.

    % list.map_foldr(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working right-to-left) to transform
    % InList into OutList.  The final value of the accumulator is
    % returned in End.
    %
:- pred list.map_foldr(pred(L, M, A, A), list(L), list(M), A, A).
:- mode list.map_foldr(pred(in, out, in, out) is det, in, out, in, out)
    is det.
:- mode list.map_foldr(pred(in, out, mdi, muo) is det, in, out, mdi, muo)
    is det.
:- mode list.map_foldr(pred(in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode list.map_foldr(pred(in, out, in, out) is semidet, in, out, in, out)
    is semidet.
:- mode list.map_foldr(pred(in, out, mdi, muo) is semidet, in, out, mdi, muo)
    is semidet.
:- mode list.map_foldr(pred(in, out, di, uo) is semidet, in, out, di, uo)
    is semidet.
:- mode list.map_foldr(pred(in, in, di, uo) is semidet, in, in, di, uo)
    is semidet.

    % list.filter_map_foldl(Transformer, List, TrueList, Start, End):
    % Takes a predicate with one input argument, one output argument and an
    % accumulator. It is called with each element of List. If a call succeeds,
    % then the output is included in TrueList and the accumulator is updated.
    %
:- pred list.filter_map_foldl(pred(X, Y, A, A)::in(pred(in, out, in, out)
    is semidet), list(X)::in, list(Y)::out, A::in, A::out) is det.

    % list.all_true(Pred, List) takes a closure with one input argument.
    % If Pred succeeds for every member of List, all_true succeeds.
    % If Pred fails for any member of List, all_true fails.
    %
:- pred list.all_true(pred(X)::in(pred(in) is semidet), list(X)::in)
    is semidet.

    % list.all_true_corresponding(Pred, ListA, ListB):
    % Succeeds if Pred succeeds for every corresponding pair of elements from
    % ListA and ListB.  Fails if Pred fails for any pair of corresponding
    % elements.
    %
    % An exception is raised if the list arguments differ in length.
    %
:- pred list.all_true_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    list(X)::in, list(Y)::in) is semidet.

    % list.all_false(Pred, List) takes a closure with one input argument.
    % If Pred fails for every member of List, all_false succeeds.
    % If Pred succeeds for any member of List, all_false fails.
    %
:- pred list.all_false(pred(X)::in(pred(in) is semidet), list(X)::in)
    is semidet.

    % list.all_false_corresponding(Pred, ListA, ListB):
    % Succeeds if Pred fails for every corresponding pair of elements from
    % ListA and ListB.  Fails if Pred succeeds for any pair of corresponding
    % elements.
    %
    % An exception is raised if the list arguments differ in length.
    %
:- pred list.all_false_corresponding(pred(X, Y)::in(pred(in, in) is semidet),
    list(X)::in, list(Y)::in) is semidet.

    % list.find_first_match(Pred, List, FirstMatch) takes a closure with one
    % input argument. It returns the element X of the list (if any) for which
    % Pred(X) is true.
    %
:- pred list.find_first_match(pred(X)::in(pred(in) is semidet), list(X)::in,
    X::out) is semidet.

    % list.filter(Pred, List, TrueList) takes a closure with one
    % input argument and for each member X of List, calls the closure.
    % X is included in TrueList iff Pred(X) is true.
    %
:- pred list.filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out) is det.
:- func list.filter(pred(X)::in(pred(in) is semidet), list(X)::in)
    = (list(X)::out) is det.

    % list.negated_filter(Pred, List, FalseList) takes a closure with one
    % input argument and for each member of List `X', calls the closure.
    % X is included in FalseList iff Pred(X) is true.
    %
:- pred list.negated_filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out) is det.
:- func list.negated_filter(pred(X)::in(pred(in) is semidet), list(X)::in)
    = (list(X)::out) is det.

    % list.filter(Pred, List, TrueList, FalseList) takes a closure with one
    % input argument and for each member X of List, calls the closure.
    % X is included in TrueList iff Pred(X) is true.
    % X is included in FalseList iff Pred(X) is true.
    %
:- pred list.filter(pred(X)::in(pred(in) is semidet), list(X)::in,
    list(X)::out, list(X)::out) is det.

    % list.filter_map(Transformer, List, TrueList) takes a predicate
    % with one input argument and one output argument. It is called
    % with each element of List. If a call succeeds, then the output is
    % included in TrueList.
    %
:- pred list.filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, list(Y)::out) is det.

:- func list.filter_map(func(X) = Y, list(X)) = list(Y).
:- mode list.filter_map(func(in) = out is semidet, in) = out is det.

    % list.filter_map(Transformer, List, TrueList, FalseList) takes
    % a predicate with one input argument and one output argument.
    % It is called with each element of List. If a call succeeds,
    % then the output is included in TrueList; otherwise, the failing
    % input is included in FalseList.
    %
:- pred list.filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, list(Y)::out, list(X)::out) is det.

    % Same as list.filter_map/3 except that it only returns the first
    % match:
    %   find_first_map(X, Y, Z) <=> list.filter_map(X, Y, [Z | _])
    %
:- pred list.find_first_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, Y::out) is semidet.

    % Same as list.find_first_map, except with two outputs.
    %
:- pred list.find_first_map2(pred(X, A, B)::in(pred(in, out, out) is semidet),
    list(X)::in, A::out, B::out) is semidet.

    % Same as list.find_first_map, except with three outputs.
    %
:- pred list.find_first_map3(
    pred(X, A, B, C)::in(pred(in, out, out, out) is semidet),
    list(X)::in, A::out, B::out, C::out) is semidet.

    % find_index_of_match(Match, List, Index0, Index)
    %
    % Find the index of an item in the list for which Match is true where the
    % first element in the list has the index Index0.
    %
:- pred list.find_index_of_match(pred(T), list(T), int, int).
:- mode list.find_index_of_match(pred(in) is semidet, in, in, out) is semidet.

    % list.takewhile(Predicate, List, UptoList, AfterList) takes a
    % closure with one input argument, and calls it on successive members
    % of List as long as the calls succeed. The elements for which
    % the call succeeds are placed in UptoList and the first element for
    % which the call fails, and all the remaining elements of List are
    % placed in AfterList.
    %
:- pred list.takewhile(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::out, list(T)::out) is det.

%-----------------------------------------------------------------------------%

    % list.sort(Compare, Unsorted, Sorted) is true iff Sorted is a
    % list containing the same elements as Unsorted, where Sorted is
    % sorted with respect to the ordering defined by the predicate
    % term Compare, and the elements that are equivalent in this ordering
    % appear in the same sequence in Sorted as they do in Unsorted
    % (that is, the sort is stable).
    %
:- pred list.sort(comparison_pred(X)::in(comparison_pred), list(X)::in,
    list(X)::out) is det.
:- func list.sort(comparison_func(X), list(X)) = list(X).

    % list.sort_and_remove_dups(Compare, Unsorted, Sorted) is true iff
    % Sorted is a list containing the same elements as Unsorted, where
    % Sorted is sorted with respect to the ordering defined by the
    % predicate term Compare, except that if two elements in Unsorted
    % are equivalent with respect to this ordering only the one which
    % occurs first will be in Sorted.
    %
:- pred list.sort_and_remove_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::out) is det.

    % list.remove_adjacent_dups(P, L0, L) is true iff L is the result
    % of replacing every sequence of elements in L0 which are equivalent
    % with respect to the ordering, with the first occurrence in L0 of
    % such an element.
    %
:- pred list.remove_adjacent_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::out) is det.

    % list.merge(Compare, As, Bs, Sorted) is true iff, assuming As and
    % Bs are sorted with respect to the ordering defined by Compare,
    % Sorted is a list containing the elements of As and Bs which is
    % also sorted.  For elements which are equivalent in the ordering,
    % if they come from the same list then they appear in the same
    % sequence in Sorted as they do in that list, otherwise the elements
    % from As appear before the elements from Bs.
    %
:- pred list.merge(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::in, list(X)::out) is det.

:- func list.merge(comparison_func(X), list(X), list(X)) = list(X).

    % list.merge_and_remove_dups(P, As, Bs, Sorted) is true iff, assuming
    % As and Bs are sorted with respect to the ordering defined by
    % Compare and neither contains any duplicates, Sorted is a list
    % containing the elements of As and Bs which is also sorted and
    % contains no duplicates.  If an element from As is duplicated in
    % Bs (that is, they are equivalent in the ordering), then the element
    % from As is the one that appears in Sorted.
    %
:- pred list.merge_and_remove_dups(comparison_pred(X)::in(comparison_pred),
    list(X)::in, list(X)::in, list(X)::out) is det.

:- func list.merge_and_remove_dups(comparison_func(X), list(X), list(X))
    = list(X).

%-----------------------------------------------------------------------------%

    % list.series(X, OK, Succ) = [X0, X1, ..., Xn]
    %
    % where X0 = X and successive elements Xj, Xk are computed as
    % Xk = Succ(Xj). The series terminates as soon as an element Xi is
    % generated such that OK(Xi) fails; Xi is not included in the output.
    %
:- func list.series(T, pred(T), func(T) = T) = list(T).
:- mode list.series(in, pred(in) is semidet, func(in) = out is det) = out
    is det.

%-----------------------------------------------------------------------------%

    % Lo `..` Hi = [Lo, Lo + 1, ..., Hi] if Lo =< Hi
    %            =                    [] otherwise
    %
:- func int `..` int = list(int).

%-----------------------------------------------------------------------------%

:- func list.head(list(T)) = T is semidet.

:- func list.tail(list(T)) = list(T) is semidet.

    % list.det_head(List) returns the first element of List,
    % calling error/1 if List is empty.
    %
:- func list.det_head(list(T)) = T.

    % list.det_tail(List) returns the tail of List,
    % calling error/1 if List is empty.
    %
:- func list.det_tail(list(T)) = list(T).

%-----------------------------------------------------------------------------%

    % Convert a list to a pretty_printer.doc for formatting.
    %
:- func list_to_doc(list(T)) = pretty_printer.doc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Some declarations for complicated modes using lists.
    %
    % See our comment above regarding incomplete  support for partial
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
    % NOTE: If Mercury recorded the mode and determinism information
    % of higher order types in the *types* of higher order variables
    % instead of in their *insts*, this function would not be needed.
    %
:- func inst_preserving_append(list(T)::in(list_skel(I =< ground)),
    list(T)::in(list_skel(I =< ground))) =
    (list(T)::out(list_skel(I =< ground))) is det.

:- import_module term.      % for var/1.

:- pragma type_spec(list.merge(in, in, out), T = var(_)).

:- pragma type_spec(list.merge_and_remove_dups(in, in, out), T = var(_)).
:- pragma type_spec(list.merge_and_remove_dups/2, T = var(_)).

:- pragma type_spec(list.remove_adjacent_dups/2, T = var(_)).
:- pragma type_spec(list.remove_adjacent_dups/1, T = var(_)).

:- pragma type_spec(list.member(in, in), T = var(_)).

:- pragma type_spec(list.sort_and_remove_dups/2, T = var(_)).
:- pragma type_spec(list.sort_and_remove_dups/1, T = var(_)).

:- pragma type_spec(list.sort(in, out), T = var(_)).
:- pragma type_spec(list.sort/1, T = var(_)).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%-----------------------------------------------------------------------------%

list.is_empty([]).

list.is_not_empty([_ | _]).

list.cons(H, T, [H | T]).

list.cons(H, T) = [H | T].

list.append(Xs, Ys) = Zs :-
    list.append(Xs, Ys, Zs).

list.append([], Ys, Ys).
list.append([X | Xs], Ys, [X | Zs]) :-
    list.append(Xs, Ys, Zs).

list.remove_suffix(List, Suffix, Prefix) :-
    list.length(List, ListLength),
    list.length(Suffix, SuffixLength),
    PrefixLength = ListLength - SuffixLength,
    list.split_list(PrefixLength, List, Prefix, Suffix).

%-----------------------------------------------------------------------------%

list.nth_member_search([X | Xs], Y, N) :-
    list.nth_member_search_2([X | Xs], Y, 1, N).

:- pred list.nth_member_search_2(list(T)::in, T::in, int::in, int::out)
    is semidet.

list.nth_member_search_2([X | Xs], Y, P, N) :-
    ( X = Y ->
        N = P
    ;
        list.nth_member_search_2(Xs, Y, P + 1, N)
    ).

nth_member_lookup(List, Elem, Position) :-
    ( list.nth_member_search(List, Elem, PositionPrime) ->
        Position = PositionPrime
    ;
        unexpected($module, $pred, "element not in list")
    ).

%-----------------------------------------------------------------------------%

list.index0([X | Xs], N, Elem) :-
    ( N = 0 ->
        Elem = X
    ;
        list.index0(Xs, N - 1, Elem)
    ).

list.det_index0(Xs, N) = A :-
    list.det_index0(Xs, N, A).

list.det_index0(List, N, Elem) :-
    ( list.index0(List, N, Elem0) ->
        Elem = Elem0
    ;
        unexpected($module, $pred, "index out of range")
    ).

list.index1(List, N, Elem) :-
    list.index0(List, N - 1, Elem).

list.det_index1(Xs, N) = A :-
    list.det_index1(Xs, N, A).

list.det_index1(List, N, Elem) :-
    list.det_index0(List, N - 1, Elem).

%-----------------------------------------------------------------------------%

list.index0_of_first_occurrence(List, Elem, N) :-
    list.index0_of_first_occurrence_2(List, Elem, 0, N).

:- pred list.index0_of_first_occurrence_2(list(T)::in, T::in,
    int::in, int::out) is semidet.

list.index0_of_first_occurrence_2([X | Xs], Y, N0, N) :-
    ( Y = X -> N = N0
            ;  list.index0_of_first_occurrence_2(Xs, Y, N0 + 1, N)
    ).

list.index1_of_first_occurrence(List, Elem, N + 1) :-
    list.index0_of_first_occurrence(List, Elem, N).

list.det_index0_of_first_occurrence(List, Elem) = N :-
    ( list.index0_of_first_occurrence(List, Elem, N0) ->
        N = N0
    ;
        unexpected($module, $pred, "item not found")
    ).

list.det_index1_of_first_occurrence(List, Elem) = N :-
    ( list.index1_of_first_occurrence(List, Elem, N0) ->
        N = N0
    ;
        unexpected($module, $pred, "item not found")
    ).

%-----------------------------------------------------------------------------%

list.condense(Xss) = Ys :-
    list.condense(Xss, Ys).

list.condense(Xss, Ys) :-
    list.reverse(Xss, RevXss),
    list.condense_2(RevXss, [], Ys).

:- pred list.condense_2(list(list(T))::in, list(T)::in, list(T)::out) is det.

list.condense_2([], !Ys).
list.condense_2([L | Ls], !Ys) :-
    list.append(L, !Ys),
    list.condense_2(Ls, !Ys).
    
%-----------------------------------------------------------------------------%

list.same_length([], []).
list.same_length([_ | L1], [_ | L2]) :-
    list.same_length(L1, L2).

list.same_length3([], [], []).
list.same_length3([_ | L1], [_ | L2], [_ | L3]) :-
    list.same_length3(L1, L2, L3).

%-----------------------------------------------------------------------------%

list.insert(Elem, List0, List) :-
    list.delete(List, Elem, List0).

%-----------------------------------------------------------------------------%

list.delete([X | L], X, L).
list.delete([X | Xs], Y, [X | L]) :-
    list.delete(Xs, Y, L).

list.delete_first([X | Xs], Y, Zs) :-
    ( X = Y ->
        Zs = Xs
    ;
        list.delete_first(Xs, Y, Zs1),
        Zs = [X | Zs1]
    ).

list.delete_all(Xs, A) = Ys :-
    list.delete_all(Xs, A, Ys).

list.delete_all([], _, []).
list.delete_all([X | Xs], Y, Zs) :-
    ( X = Y ->
        list.delete_all(Xs, Y, Zs)
    ;
        list.delete_all(Xs, Y, Zs1),
        Zs = [X | Zs1]
    ).

list.delete_elems(Xs, Ys) = Zs :-
    list.delete_elems(Xs, Ys, Zs).

list.delete_elems(Xs, [], Xs).
list.delete_elems(Xs, [E | Es], Zs) :-
    list.delete_all(Xs, E, Ys),
    list.delete_elems(Ys, Es, Zs).

%-----------------------------------------------------------------------------%

list.replace([X | L], X, Z, [Z | L]).
list.replace([X | Xs], Y, Z, [X | L]) :-
    list.replace(Xs, Y, Z, L).

list.replace_first([X | Xs], Y, Z, List) :-
    ( X = Y ->
        List = [Z | Xs]
    ;
        list.replace_first(Xs, Y, Z, L1),
        List = [X | L1]
    ).

list.replace_all(Xs, A, B) = Ys :-
    list.replace_all(Xs, A, B, Ys).

list.replace_all([], _, _, []).
list.replace_all([X | Xs], Y, Z, L) :-
    ( X = Y ->
        list.replace_all(Xs, Y, Z, L0),
        L = [Z | L0]
    ;
        list.replace_all(Xs, Y, Z, L0),
        L = [X | L0]
    ).

list.replace_nth(Xs, P, R, L) :-
    P > 0,
    list.replace_nth_2(Xs, P, R, L).

list.det_replace_nth(Xs, N, A) = Ys :-
    list.det_replace_nth(Xs, N, A, Ys).

list.det_replace_nth(Xs, P, R, L) :-
    ( P > 0 ->
        ( list.replace_nth_2(Xs, P, R, L0) ->
            L = L0
        ;
            unexpected($module, $pred,
                "Cannot replace element whose index position " ++
                "is past the end of the list")
        )
    ;
        unexpected($module, $pred,
            "Cannot replace element whose index position " ++
            "is less than 1.")
    ).

:- pred list.replace_nth_2(list(T)::in, int::in, T::in, list(T)::out)
    is semidet.

list.replace_nth_2([X | Xs], P, R, L) :-
    ( P = 1 ->
        L = [R | Xs]
    ;
        list.replace_nth(Xs, P - 1, R, L0),
        L = [X | L0]
    ).

%-----------------------------------------------------------------------------%

list.member(X, [X | _]).
list.member(X, [_ | Xs]) :-
    list.member(X, Xs).

list.member(Element, List, SubList) :-
    SubList = [Element | _],
    list.append(_, SubList, List).

list.member_index0(X, [X | _], 0).
list.member_index0(X, [_ | Xs], Index + 1) :-
    list.member_index0(X, Xs, Index).

list.contains(List, Elem) :-
    list.member(Elem, List).

%-----------------------------------------------------------------------------%

list.merge(Xs, Ys) = Zs :-
    list.merge(Xs, Ys, Zs).

list.merge([], [], []).
list.merge([A | As], [], [A | As]).
list.merge([], [B | Bs], [B | Bs]).
list.merge([A | As], [B | Bs], Cs) :-
    ( compare(>, A, B) ->
        list.merge([A | As], Bs, Cs0),
        Cs = [B | Cs0]
    ;
        % If compare((=), A, B), take A first.
        list.merge(As, [B | Bs], Cs0),
        Cs = [A | Cs0]
    ).

list.merge_and_remove_dups(Xs, Ys) = Zs :-
    list.merge_and_remove_dups(Xs, Ys, Zs).

list.merge_and_remove_dups([], [], []).
list.merge_and_remove_dups([A | As], [], [A | As]).
list.merge_and_remove_dups([], [B | Bs], [B | Bs]).
list.merge_and_remove_dups([A | As], [B | Bs], Cs) :-
    compare(Res, A, B),
    (
        Res = (<),
        list.merge_and_remove_dups(As, [B | Bs], Cs0),
        Cs = [A | Cs0]
    ;
        Res = (=),
        list.merge_and_remove_dups(As, Bs, Cs0),
        Cs = [A | Cs0]
    ;
        Res = (>),
        list.merge_and_remove_dups([A | As], Bs, Cs0),
        Cs = [B | Cs0]
    ).

%-----------------------------------------------------------------------------%

% Note - it is not possible to write a version of
% list.length/1 in pure Mercury that works in both directions
% unless you make it semidet rather than det.

list.length(Xs) = N :-
    list.length(Xs, N).

list.length(L, N) :-
    list.length_2(L, 0, N).

:- pred list.length_2(list(T), int, int).
:- mode list.length_2(in, in, out) is det.

list.length_2([], N, N).
list.length_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    list.length_2(L1, N1, N).

%-----------------------------------------------------------------------------%

list.reverse(Xs) = Ys :-
    list.reverse(Xs, Ys).

    % reverse(A, B) <=> reverse(B, A).
:- pragma promise_equivalent_clauses(list.reverse/2).

:- pragma foreign_proc("Erlang",
    list.reverse(L0::in, L::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    L = lists:reverse(L0)
").

list.reverse(L0::in, L::out) :-
    list.reverse_2(L0, [], L).

list.reverse(L::out, L0::in) :-
    list.reverse_2(L0, [], L).

:- pred list.reverse_2(list(T)::in, list(T)::in, list(T)::out) is det.

list.reverse_2([], L, L).
list.reverse_2([X | Xs], L0, L) :-
    list.reverse_2(Xs, [X | L0], L).

%-----------------------------------------------------------------------------%

list.sort(Xs) = Ys :-
    list.sort(Xs, Ys).

list.sort(L0, L) :-
    list.merge_sort(L0, L).

list.sort_and_remove_dups(Xs) = Ys :-
    list.sort_and_remove_dups(Xs, Ys).
list.sort_and_remove_dups(L0, L) :-
    list.merge_sort_and_remove_dups_2(list.length(L0), L0, L).

:- pred list.merge_sort(list(T)::in, list(T)::out) is det.
:- pragma type_spec(list.merge_sort(in, out), T = var(_)).

list.merge_sort(List, SortedList) :-
    list.merge_sort_2(list.length(List), List, SortedList).

%-----------------------------------------------------------------------------%

:- pred list.merge_sort_and_remove_dups_2(int::in, list(T)::in, list(T)::out)
    is det.
:- pragma type_spec(list.merge_sort_and_remove_dups_2(in, in, out),
    T = var(_)).

list.merge_sort_and_remove_dups_2(Length, List, SortedList) :-
    ( Length > 1 ->
        HalfLength = Length // 2,
        ( list.split_list(HalfLength, List, Front, Back) ->
            list.merge_sort_and_remove_dups_2(HalfLength,
                Front, SortedFront),
            list.merge_sort_and_remove_dups_2(Length - HalfLength,
                Back, SortedBack),
            list.merge_and_remove_dups(SortedFront, SortedBack, SortedList)
        ;
            unexpected($module, $pred, "split failed")
        )
    ;
        SortedList = List
    ).

%-----------------------------------------------------------------------------%

:- pred list.merge_sort_2(int::in, list(T)::in, list(T)::out) is det.
:- pragma type_spec(list.merge_sort_2(in, in, out), T = var(_)).

list.merge_sort_2(Length, List, SortedList) :-
    ( Length > 1 ->
        HalfLength = Length // 2,
        ( list.split_list(HalfLength, List, Front, Back) ->
            list.merge_sort_2(HalfLength, Front, SortedFront),
            list.merge_sort_2(Length - HalfLength, Back, SortedBack),
            list.merge(SortedFront, SortedBack, SortedList)
        ;
            unexpected($module, $pred, "split failed")
        )
    ;
        SortedList = List
    ).

%-----------------------------------------------------------------------------%

list.remove_dups(Xs) = Ys :-
    list.remove_dups(Xs, Ys).

list.remove_dups(Xs, Ys) :-
    list.remove_dups_2(Xs, set_tree234.init, Ys).

:- pred list.remove_dups_2(list(T)::in, set_tree234(T)::in, list(T)::out)
    is det.

list.remove_dups_2([], _SoFar, []).
list.remove_dups_2([X | Xs], SoFar0, Zs) :-
    ( set_tree234.contains(SoFar0, X) ->
        list.remove_dups_2(Xs, SoFar0, Zs)
    ;
        set_tree234.insert(X, SoFar0, SoFar),
        list.remove_dups_2(Xs, SoFar, Ys),
        Zs = [X | Ys]
    ).

%-----------------------------------------------------------------------------%

list.remove_adjacent_dups(Xs) = Ys :-
    list.remove_adjacent_dups(Xs, Ys).

list.remove_adjacent_dups([], []).
list.remove_adjacent_dups([X | Xs], L) :-
    list.remove_adjacent_dups_2(Xs, X, L).

:- pred list.remove_adjacent_dups_2(list(T)::in, T::in, list(T)::out) is det.
:- pragma type_spec(list.remove_adjacent_dups_2/3, T = var(_)).

list.remove_adjacent_dups_2([], X, [X]).
list.remove_adjacent_dups_2([X1 | Xs], X0, L) :-
    ( X0 = X1 ->
        list.remove_adjacent_dups_2(Xs, X0, L)
    ;
        list.remove_adjacent_dups_2(Xs, X1, L0),
        L = [X0 | L0]
    ).

%-----------------------------------------------------------------------------%

list.zip(Xs, Ys) = Zs :-
    list.zip(Xs, Ys, Zs).

list.zip([], Bs, Bs).
list.zip([A | As], Bs, [A | Cs]) :-
    list.zip2(As, Bs, Cs).

:- pred list.zip2(list(T), list(T), list(T)).
:- mode list.zip2(in, in, out) is det.

list.zip2(As, [], As).
list.zip2(As, [B | Bs], [B | Cs]) :-
    list.zip(As, Bs, Cs).

%-----------------------------------------------------------------------------%

list.split_list(N, List, Start, End) :-
    ( N = 0 ->
        Start = [],
        End = List
    ;
        N > 0,
        List = [Head | List1],
        Start = [Head | Start1],
        list.split_list(N - 1, List1, Start1, End)
    ).

list.det_split_list(N, List, Start, End) :-
    ( list.split_list(N, List, Start0, End0) ->
        Start = Start0,
        End = End0
    ;
        unexpected($module, $pred, "index out of range")
    ).

list.split_upto(N, List, Start, End) :-
    (
        N > 0,
        List = [Head | List1]
    ->
        split_upto(N - 1, List1, Start1, End),
        Start = [Head | Start1]
    ;
        Start = [],
        End = List
    ).

list.take(N, Xs0, Xs) :-
    ( N > 0 ->
        Xs0 = [X | Xs1],
        list.take(N - 1, Xs1, Xs2),
        Xs = [X | Xs2]
    ;
        Xs = []
    ).

list.det_take(N, As, Bs) :-
    ( list.take(N, As, BsPrime) ->
        Bs = BsPrime
    ;
        unexpected($file, $pred, "not enough elements")
    ).

list.take_upto(N, Xs) = Ys :-
    list.take_upto(N, Xs, Ys).

list.take_upto(N, As, Bs) :-
    ( list.take(N, As, Bs0) ->
        Bs = Bs0
    ;
        Bs = As
    ).

list.drop(N, As, Bs) :-
    ( N > 0 ->
        As = [_ | TailAs],
        list.drop(N - 1, TailAs, Bs)
    ;
        As = Bs
    ).

list.det_drop(N, As, Bs) :-
    ( N > 0 ->
        (
            As = [_ | TailAs],
            list.det_drop(N - 1, TailAs, Bs)
        ;
            As = [],
            unexpected($module, $pred, "not enough elements")
        )
    ;
        As = Bs
    ).

%-----------------------------------------------------------------------------%

list.duplicate(N, A) = Xs :-
    list.duplicate(N, A, Xs).

list.duplicate(N, X, list.duplicate_2(N, X, [])).

:- func list.duplicate_2(int, T, list(T)) = list(T).

list.duplicate_2(N, X, Xs) =
    ( N > 0 ->
        list.duplicate_2(N - 1, X, [X | Xs])
    ;
        Xs
    ).

%-----------------------------------------------------------------------------%

list.chunk(Xs, N) = Ys :-
    list.chunk(Xs, N, Ys).

list.chunk(List, ChunkSize, ListOfSmallLists) :-
    list.chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred list.chunk_2(list(T)::in, int::in, list(T)::in, int::in,
    list(list(T))::out) is det.

list.chunk_2([], _ChunkSize, List0, _N, Lists) :-
    (
        List0 = [],
        Lists = []
    ;
        List0 = [_ | _],
        list.reverse(List0, List),
        Lists = [List]
    ).
list.chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( N > 1 ->
        list.chunk_2(Xs, ChunkSize, [X | List0], N - 1, Lists)
    ;
        list.reverse([X | List0], List),
        list.chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1),
        Lists = [List | Lists1]
    ).

%-----------------------------------------------------------------------------%

list.perm([], []).
list.perm([X | Xs], Ys) :-
    list.perm(Xs, Ys0),
    list.insert(X, Ys0, Ys).

%-----------------------------------------------------------------------------%

list.sublist([], _).
list.sublist([SH | ST], [FH | FT]) :-
    ( SH = FH ->
        list.sublist(ST, FT)
    ;
        list.sublist([SH | ST], FT)
    ).

%-----------------------------------------------------------------------------%

list.all_same([]).
list.all_same([H | T]) :-
    list.all_same_2(H, T).

:- pred list.all_same_2(T::in, list(T)::in) is semidet.

list.all_same_2(_, []).
list.all_same_2(H, [H | T]) :-
    list.all_same_2(H, T).

%-----------------------------------------------------------------------------%

list.last([H | T], Last) :-
    (
        T = [],
        Last = H
    ;
        T = [_ | _],
        list.last(T, Last)
    ).

list.det_last(List) = Last :-
    list.det_last(List, Last).

list.det_last([], _) :-
    unexpected($module, $pred, "empty list").
list.det_last([H | T], Last) :-
    list.det_last_2(H, T, Last).

:- pred list.det_last_2(T::in, list(T)::in, T::out) is det.

list.det_last_2(H, T, Last) :-
    (
        T = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_last_2(TH, TT, Last)
    ).

list.split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_2(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

list.det_split_last([], _, _) :-
    unexpected($module, $pred, "empty list").
list.det_split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_2(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

:- pred list.det_split_last_2(T::in, list(T)::in, list(T)::out, T::out) is det.

list.det_split_last_2(H, T, AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [TH | TT],
        list.det_split_last_2(TH, TT, AllButLastTail, Last),
        AllButLast = [H | AllButLastTail]
    ).

%-----------------------------------------------------------------------------%

list.map(_F, []) = [].
list.map(F, [H | T]) = [F(H) | list.map(F, T)].

list.map(_, [],  []).
list.map(P, [H0 | T0], [H | T]) :-
    P(H0, H),
    list.map(P, T0, T).

list.map2(_, [],  [],  []).
list.map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
    P(H0, H1, H2),
    list.map2(P, T0, T1, T2).

list.map3(_, [],  [],  [],  []).
list.map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
    P(H0, H1, H2, H3),
    list.map3(P, T0, T1, T2, T3).

list.map4(_, [], [], [], [], []).
list.map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
    P(H0, H1, H2, H3, H4),
    list.map4(P, T0, T1, T2, T3, T4).

list.map5(_, [], [], [], [], [], []).
list.map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5])
        :-
    P(H0, H1, H2, H3, H4, H5),
    list.map5(P, T0, T1, T2, T3, T4, T5).

list.map6(_, [], [], [], [], [], [], []).
list.map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6]) :-
    P(H0, H1, H2, H3, H4, H5, H6),
    list.map6(P, T0, T1, T2, T3, T4, T5, T6).

list.map7(_, [], [], [], [], [], [], [], []).
list.map7(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7),
    list.map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

list.map8(_, [], [], [], [], [], [], [], [], []).
list.map8(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7], [H8 | T8]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7, H8),
    list.map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

list.map_corresponding(_, [], []) = [].
list.map_corresponding(_, [], [_ | _]) =
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding(_, [_ | _], []) =
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding(F, [A | As], [B | Bs]) =
    [F(A, B) | list.map_corresponding(F, As, Bs)].

list.map_corresponding(_, [], [], []).
list.map_corresponding(_, [], [_ | _], _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding(_, [_ | _], [], _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding(P, [A | As], [B | Bs], [C | Cs]) :-
    P(A, B, C),
    list.map_corresponding(P, As, Bs, Cs).

list.map_corresponding3(F, As, Bs, Cs) =
    (
        As = [A | As0],
        Bs = [B | Bs0],
        Cs = [C | Cs0]
    ->
        [F(A, B, C) | list.map_corresponding3(F, As0, Bs0, Cs0)]
    ;
        As = [],
        Bs = [],
        Cs = []
    ->
        []
    ;
        unexpected($module, $pred, "mismatched list lengths")
    ).

list.filter_map_corresponding(_, [], []) = [].
list.filter_map_corresponding(_, [], [_ | _]) =
    unexpected($module, $pred, "mismatched list lengths").
list.filter_map_corresponding(_, [_ | _], []) =
    unexpected($module, $pred, "mismatched list lengths").
list.filter_map_corresponding(F, [A | As], [B | Bs]) =
    ( F(A, B) = C ->
        [C | list.filter_map_corresponding(F, As, Bs)]
    ;
        list.filter_map_corresponding(F, As, Bs)
    ).

list.filter_map_corresponding3(F, As, Bs, Cs) =
    (
        As = [A | As0],
        Bs = [B | Bs0],
        Cs = [C | Cs0]
    ->
        ( F(A, B, C) = D ->
            [D | list.filter_map_corresponding3(F, As0, Bs0, Cs0)]
        ;
            list.filter_map_corresponding3(F, As0, Bs0, Cs0)
        )
    ;
        As = [],
        Bs = [],
        Cs = []
    ->
        []
    ;
        unexpected($module, $pred, "mismatched list lengths")
    ).

list.map_corresponding_foldl(_, [], [], [], !Acc).
list.map_corresponding_foldl(_, [], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl(_, [_ | _], [], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl(P, [A | As], [B | Bs], [C | Cs], !Acc) :-
    P(A, B, C, !Acc),
    list.map_corresponding_foldl(P, As, Bs, Cs, !Acc).

list.map_corresponding_foldl2(_, [], [], [], !Acc1, !Acc2).
list.map_corresponding_foldl2(_, [], [_ | _], _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl2(_, [_ | _], [], _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl2(P, [A | As], [B | Bs], [C | Cs], !Acc1, !Acc2) :-
    P(A, B, C, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, As, Bs, Cs, !Acc1, !Acc2).

list.map_corresponding_foldl3(_, [], [], [], !Acc1, !Acc2, !Acc3).
list.map_corresponding_foldl3(_, [], [_ | _], _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl3(_, [_ | _], [], _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding_foldl3(P, [A | As], [B | Bs], [C | Cs], !Acc1,
        !Acc2, !Acc3) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3),
    list.map_corresponding_foldl3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3).

list.map_corresponding3_foldl(_, [], [], [], [], !Acc).
list.map_corresponding3_foldl(_, [], [_ | _], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(_, [_ | _], [], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(_, [_ | _], [_ | _], [], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(_, [], [], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(_, [], [_ | _], [], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(_, [_ | _], [], [], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.map_corresponding3_foldl(P, [A | As], [B | Bs], [C | Cs], [D | Ds],
        !Acc) :-
    P(A, B, C, D, !Acc),
    list.map_corresponding3_foldl(P, As, Bs, Cs, Ds, !Acc).

list.foldl(F, Xs, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.foldl(P, Xs, A, B).

list.foldl(_, [], !A).
list.foldl(P, [H | T], !A) :-
    P(H, !A),
    list.foldl(P, T, !A).

list.foldl2(_, [], !A, !B).
list.foldl2(P, [H | T], !A, !B) :-
    P(H, !A, !B),
    list.foldl2(P, T, !A, !B).

list.foldl3(_, [], !A, !B, !C).
list.foldl3(P, [H | T], !A, !B, !C) :-
    P(H, !A, !B, !C),
    list.foldl3(P, T, !A, !B, !C).

list.foldl4(_, [], !A, !B, !C, !D).
list.foldl4(P, [H | T], !A, !B, !C, !D) :-
    P(H, !A, !B, !C, !D),
    list.foldl4(P, T, !A, !B, !C, !D).

list.foldl5(_, [], !A, !B, !C, !D, !E).
list.foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
    P(H, !A, !B, !C, !D, !E),
    list.foldl5(P, T, !A, !B, !C, !D, !E).

list.foldl6(_, [], !A, !B, !C, !D, !E, !F).
list.foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H, !A, !B, !C, !D, !E, !F),
    list.foldl6(P, T, !A, !B, !C, !D, !E, !F).

list.foldl_corresponding(_, [], [], !Acc).
list.foldl_corresponding(_, [], [_ | _], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding(_, [_ | _], [], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    P(A, B, !Acc),
    list.foldl_corresponding(P, As, Bs, !Acc).

list.foldl_corresponding(_, [], [], Acc) = Acc.
list.foldl_corresponding(_, [], [_ | _], _) = _ :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding(_, [_ | _], [], _) = _ :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding(F, [A | As], [B | Bs], !.Acc) = !:Acc :-
    !:Acc = F(A, B, !.Acc),
    !:Acc = list.foldl_corresponding(F, As, Bs, !.Acc).

list.foldl2_corresponding(_, [], [], !Acc1, !Acc2).
list.foldl2_corresponding(_, [], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding(_, [_ | _], [], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2) :-
    P(A, B, !Acc1, !Acc2),
    list.foldl2_corresponding(P, As, Bs, !Acc1, !Acc2).

list.foldl3_corresponding(_, [], [], !Acc1, !Acc2, !Acc3).
list.foldl3_corresponding(_, [], [_ | _], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding(_, [_ | _], [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2, !Acc3) :-
    P(A, B, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, As, Bs, !Acc1, !Acc2, !Acc3).

list.foldl_corresponding3(_, [], [], [], !Acc).
list.foldl_corresponding3(_, [_ | _], [], [], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(_, [], [_ | _], [], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(_, [], [], [_ | _], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(_, [], [_ | _], [_ | _], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(_, [_ | _], [], [_ | _], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(_, [_ | _], [_ | _], [], _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs], !Acc) :-
    P(A, B, C, !Acc),
    list.foldl_corresponding3(P, As, Bs, Cs, !Acc).

list.foldl2_corresponding3(_, [], [], [], !Acc1, !Acc2).
list.foldl2_corresponding3(_, [_ | _], [], [], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(_, [], [_ | _], [], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(_, [], [], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl2_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2) :-
    P(A, B, C, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2).

list.foldl3_corresponding3(_, [], [], [], !Acc1, !Acc2, !Acc3).
list.foldl3_corresponding3(_, [_ | _], [], [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(_, [], [_ | _], [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(_, [], [], [_ | _], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl3_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2, !Acc3) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3).

list.foldl4_corresponding3(_, [], [], [], !Acc1, !Acc2, !Acc3, !Acc4).
list.foldl4_corresponding3(_, [_ | _], [], [], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(_, [], [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(_, [], [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(_, [], [_ | _], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(_, [_ | _], [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(_, [_ | _], [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
list.foldl4_corresponding3(P, [ A | As ], [ B | Bs ], [ C | Cs],
        !Acc1, !Acc2, !Acc3, !Acc4) :-
    P(A, B, C, !Acc1, !Acc2, !Acc3, !Acc4),
    list.foldl4_corresponding3(P, As, Bs, Cs, !Acc1, !Acc2, !Acc3, !Acc4).

list.map_foldl(_, [], [], !A).
list.map_foldl(P, [H0 | T0], [H | T], !A) :-
    P(H0, H, !A),
    list.map_foldl(P, T0, T, !A).

list.map_foldl2(_, [], [], !A, !B).
list.map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
    P(H0, H, !A, !B),
    list.map_foldl2(P, T0, T, !A, !B).

list.map_foldl3(_, [], [], !A, !B, !C).
list.map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
    P(H0, H, !A, !B, !C),
    list.map_foldl3(P, T0, T, !A, !B, !C).

list.map_foldl4(_, [], [], !A, !B, !C, !D).
list.map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
    P(H0, H, !A, !B, !C, !D),
    list.map_foldl4(P, T0, T, !A, !B, !C, !D).

list.map_foldl5(_, [], [], !A, !B, !C, !D, !E).
list.map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
    P(H0, H, !A, !B, !C, !D, !E),
    list.map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

list.map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
list.map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H0, H, !A, !B, !C, !D, !E, !F),
    list.map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

list.map2_foldl(_, [], [], [], !A).
list.map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
    P(H0, H1, H2, !A),
    list.map2_foldl(P, T0, T1, T2, !A).

list.map2_foldl2(_, [], [], [], !A, !B).
list.map2_foldl2(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B) :-
    P(H0, H1, H2, !A, !B),
    list.map2_foldl2(P, T0, T1, T2, !A, !B).

list.map2_foldl3(_, [], [], [], !A, !B, !C).
list.map2_foldl3(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B, !C) :-
    P(H0, H1, H2, !A, !B, !C),
    list.map2_foldl3(P, T0, T1, T2, !A, !B, !C).

list.map2_foldl4(_, [], [], [], !A, !B, !C, !D).
list.map2_foldl4(P, [H0 | T0], [H1 | T1], [H2 | T2], !A, !B, !C, !D) :-
    P(H0, H1, H2, !A, !B, !C, !D),
    list.map2_foldl4(P, T0, T1, T2, !A, !B, !C, !D).

list.map3_foldl(_, [], [], [], [], !A).
list.map3_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], !A) :-
    P(H0, H1, H2, H3, !A),
    list.map3_foldl(P, T0, T1, T2, T3, !A).

list.map3_foldl2(_, [], [], [], [], !A, !B).
list.map3_foldl2(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], !A, !B) :-
    P(H0, H1, H2, H3, !A, !B),
    list.map3_foldl2(P, T0, T1, T2, T3, !A, !B).

list.map4_foldl(_, [], [], [], [], [], !A).
list.map4_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], !A) :-
    P(H0, H1, H2, H3, H4, !A),
    list.map4_foldl(P, T0, T1, T2, T3, T4, !A).

list.map_foldr(_, [], [], !A).
list.map_foldr(P, [H0 | T0], [H | T], !A) :-
    list.map_foldr(P, T0, T, !A),
    P(H0, H, !A).

list.filter_map_foldl(_, [], [], !A).
list.filter_map_foldl(P, [X | Xs], True, !A) :-
    ( P(X, Y, !A) ->
        list.filter_map_foldl(P, Xs, TrueTail, !A),
        True = [Y | TrueTail]
    ;
        list.filter_map_foldl(P, Xs, True, !A)
    ).

list.foldr(F, Xs, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.foldr(P, Xs, A, B).

list.foldr(_, [], !A).
list.foldr(P, [H | T], !A) :-
    list.foldr(P, T, !A),
    P(H, !A).

list.foldr2(_, [], !A, !B).
list.foldr2(P, [H | T], !A, !B) :-
    list.foldr2(P, T, !A, !B),
    P(H, !A, !B).

list.foldr3(_, [], !A, !B, !C).
list.foldr3(P, [H | T], !A, !B, !C) :-
    list.foldr3(P, T, !A, !B, !C),
    P(H, !A, !B, !C).

list.all_true(_P, []).
list.all_true(P, [X | Xs]) :-
    P(X),
    list.all_true(P, Xs).

list.all_true_corresponding(_P, [], []).
list.all_true_corresponding(_P, [], [_ | _]) :-
    unexpected($module, $pred, "mismatched list lengths").
list.all_true_corresponding(_P, [_ | _], []) :-
    unexpected($module, $pred, "mismatched list lengths").
list.all_true_corresponding(P, [X | Xs], [Y | Ys]) :-
    P(X, Y),
    list.all_true_corresponding(P, Xs, Ys).

list.all_false(_P, []).
list.all_false(P, [X | Xs]) :-
    not P(X),
    list.all_false(P, Xs).

list.all_false_corresponding(_P, [], []).
list.all_false_corresponding(_P, [], [_ | _]) :-
    unexpected($module, $pred, "mismatched list lengths").
list.all_false_corresponding(_P, [_ | _], []) :-
    unexpected($module, $pred, "mismatched list lengths").
list.all_false_corresponding(P, [X | Xs], [Y | Ys]) :-
    not P(X, Y),
    list.all_false_corresponding(P, Xs, Ys).

list.find_first_match(P, [H | T], FirstMatch) :-
    ( P(H) ->
        FirstMatch = H
    ;
        list.find_first_match(P, T, FirstMatch)
    ).

list.filter(P, Xs) = Trues :-
    list.filter(P, Xs, Trues).

list.filter(_, [],  []).
list.filter(P, [H | T], True) :-
    ( P(H) ->
        list.filter(P, T, TrueTail),
        True = [H | TrueTail]
    ;
        list.filter(P, T, True)
    ).

list.negated_filter(P, Xs) = Falses :-
    list.negated_filter(P, Xs, Falses).

list.negated_filter(_, [],  []).
list.negated_filter(P, [H | T], False) :-
    ( P(H) ->
        list.negated_filter(P, T, False)
    ;
        list.negated_filter(P, T, FalseTail),
        False = [H | FalseTail]
    ).

list.filter(_, [],  [], []).
list.filter(P, [H | T], True, False) :-
    ( P(H) ->
        list.filter(P, T, TrueTail, False),
        True = [H | TrueTail]
    ;
        list.filter(P, T, True, FalseTail),
        False = [H | FalseTail]
    ).

list.filter_map(F, Xs) = Ys :-
    P = ( pred(X::in, Y::out) is semidet :- Y = F(X) ),
    list.filter_map(P, Xs, Ys).

list.filter_map(_, [],  []).
list.filter_map(P, [H0 | T0], True) :-
    ( P(H0, H) ->
        list.filter_map(P, T0, TrueTail),
        True = [H | TrueTail]
    ;
        list.filter_map(P, T0, True)
    ).

list.filter_map(_, [], [], []).
list.filter_map(P, [H0 | T0], True, False) :-
    ( P(H0, H) ->
        list.filter_map(P, T0, TrueTail, False),
        True = [H | TrueTail]
    ;
        list.filter_map(P, T0, True, FalseTail),
        False = [H0 | FalseTail]
    ).

list.find_first_map(P, [X | Xs], A) :-
    ( P(X, A0) ->
        A = A0
    ;
        list.find_first_map(P, Xs, A)
    ).

list.find_first_map2(P, [X | Xs], A, B) :-
    ( P(X, A0, B0) ->
        A = A0,
        B = B0
    ;
        list.find_first_map2(P, Xs, A, B)
    ).

list.find_first_map3(P, [X | Xs], A, B, C) :-
    ( P(X, A0, B0, C0) ->
        A = A0,
        B = B0,
        C = C0
    ;
        list.find_first_map3(P, Xs, A, B, C)
    ).

list.find_index_of_match(Match, [X | Xs], Index0, Index) :-
    ( Match(X) ->
        Index = Index0
    ;
        find_index_of_match(Match, Xs, Index0 + 1, Index)
    ).

%----------------------------------------------------------------------------%

list.takewhile(_, [], [], []).
list.takewhile(P, [X | Xs], Ins, Outs) :-
    ( P(X) ->
        Ins = [X | Ins0],
        list.takewhile(P, Xs, Ins0, Outs)
    ;
        Ins = [],
        Outs = [X | Xs]
    ).

list.sort_and_remove_dups(P, L0, L) :-
    list.sort(P, L0, L1),
    list.remove_adjacent_dups(P, L1, L).

list.remove_adjacent_dups(_, [], []).
list.remove_adjacent_dups(P, [X | Xs], L) :-
    list.remove_adjacent_dups_2(P, Xs, X, L).

:- pred list.remove_adjacent_dups_2(comparison_pred(T)::in(comparison_pred),
    list(T)::in, T::in, list(T)::out) is det.

list.remove_adjacent_dups_2(_, [], X, [X]).
list.remove_adjacent_dups_2(P, [X1 | Xs], X0, L) :-
    ( P(X0, X1, (=)) ->
        list.remove_adjacent_dups_2(P, Xs, X0, L)
    ;
        list.remove_adjacent_dups_2(P, Xs, X1, L0),
        L = [X0 | L0]
    ).

list.sort(F, Xs) = Ys :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.sort(P, Xs, Ys).

list.sort(P, L0, L) :-
    list.length(L0, N),
    ( N = 0 ->
        L = []
    ; list.hosort(P, N, L0, L1, []) ->
        L = L1
    ;
        unexpected($module, $pred, "hosort failed")
    ).

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
:- pred list.hosort(comparison_pred(X)::in(comparison_pred), int::in,
    list(X)::in, list(X)::out, list(X)::out) is semidet.

list.hosort(P, N, L0, L, Rest) :-
    ( N = 1 ->
        L0 = [X | Rest],
        L = [X]
    ; N = 2 ->
        L0 = [X, Y | Rest],
        P(X, Y, C),
        (
            C = (<),
            L = [X, Y]
        ;
            C = (=),
            L = [X, Y]
        ;
            C = (>),
            L = [Y, X]
        )
    ;
        N1 = N // 2,
        list.hosort(P, N1, L0, L1, Middle),
        N2 = N - N1,
        list.hosort(P, N2, Middle, L2, Rest),
        list.merge(P, L1, L2, L)
    ).

list.merge(F, Xs, Ys) = Zs :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.merge(P, Xs, Ys, Zs).

list.merge(_P, [], [], []).
list.merge(_P, [], [Y | Ys], [Y | Ys]).
list.merge(_P, [X | Xs], [], [X | Xs]).
list.merge(P, [H1 | T1], [H2 | T2], L) :-
    ( P(H1, H2, (>)) ->
        list.merge(P, [H1 | T1], T2, T),
        L = [H2 | T]
    ;
        list.merge(P, T1, [H2 | T2], T),
        L = [H1 | T]
    ).

list.merge_and_remove_dups(F, Xs, Ys) = Zs :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    list.merge_and_remove_dups(P, Xs, Ys, Zs).

list.merge_and_remove_dups(_P, [], [], []).
list.merge_and_remove_dups(_P, [], [Y | Ys], [Y | Ys]).
list.merge_and_remove_dups(_P, [X | Xs], [], [X | Xs]).
list.merge_and_remove_dups(P, [H1 | T1], [H2 | T2], L) :-
    P(H1, H2, C),
    (
        C = (<),
        list.merge_and_remove_dups(P, T1, [H2 | T2], T),
        L = [H1 | T]
    ;
        C = (=),
        list.merge_and_remove_dups(P, T1, T2, T),
        L = [H1 | T]
    ;
        C = (>),
        list.merge_and_remove_dups(P, [H1 | T1], T2, T),
        L = [H2 | T]
    ).

%-----------------------------------------------------------------------------%

% These functions are exported so that they can be used instead of the
% names [|]_2 and []_0. These two names can be difficult to use from
% C# in the il backend.

:- func empty_list = list(T).
:- pragma foreign_export("C", empty_list = out, "ML_empty_list").
:- pragma foreign_export("IL", empty_list = out, "ML_empty_list").

empty_list = [].

:- pragma foreign_export("C", (list.cons(in, in) = (out)), "ML_cons").
:- pragma foreign_export("IL", (list.cons(in, in) = (out)), "ML_cons").

%-----------------------------------------------------------------------------%

L1 ++ L2 = list.append(L1, L2).

%-----------------------------------------------------------------------------%

list.series(I, OK, Succ) = Series :-
    % In order to ensure that our stack consumption is constant,
    % not linear, we build the series "backwards" and then reverse it.
    list.series_2(I, OK, Succ, [], Series0),
    list.reverse(Series0, Series).

:- pred list.series_2(T, pred(T), func(T) = T, list(T), list(T)).
:- mode list.series_2(in, pred(in) is semidet, func(in) = out is det,
    in, out) is det.

list.series_2(I, OK, Succ, !Series) :-
    ( OK(I) ->
        !:Series = [I | !.Series],
        list.series_2(Succ(I), OK, Succ, !Series)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

Lo `..` Hi = List :- successive_integers(Lo, Hi, [], List).

:- pred successive_integers(int::in, int::in, list(int)::in, list(int)::out)
    is det.

successive_integers(Lo, Hi, !Ints) :-
    ( Lo =< Hi ->
        !:Ints = [ Hi | !.Ints ],
        successive_integers(Lo, Hi - 1, !Ints)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

list.head([X | _]) = X.

list.det_head([]) = _ :-
    unexpected($module, $pred, "empty list").
list.det_head([X | _]) = X.

list.tail([_ | Xs]) = Xs.

list.det_tail([]) = _ :-
    unexpected($module, $pred, "empty list").
list.det_tail([_ | Xs]) = Xs.

%-----------------------------------------------------------------------------%

inst_preserving_append([], L) = L.
inst_preserving_append([H | T], L) = [H | NT] :-
    inst_preserving_append(T, L) = NT.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_code("Java", "

/*
** We don't use `:- pragma foreign_export' to generate these methods because
** the interfaces would expect type_info arguments.
*/

/*
** If you need to specify the type parameter, you must use the qualified
** method name, e.g. list.<Integer>empty_list()
*/
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

/*
** A wrapper class to allow for-each syntax.
** You must use a new instance of this class for each loop!
*/

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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
:- end_module list.
%-----------------------------------------------------------------------------%
