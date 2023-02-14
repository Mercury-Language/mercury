%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
% any_list.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Started Wed Aug 10 18:17:22 EST 2005
%
% A version of the list module adapted for lists whose members have
% inst `any' (typically [types containing] solver types).
%
% To avoid ambiguity problems, users should `import_module list' and
% `use_module any_list' and explicitly module qualify calls to any of the
% predicates and functions in this module.
%
%---------------------------------------------------------------------------%

:- module any_list.

:- interface.

:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred any_list.is_empty(list(T)::ia) is semidet.

:- pred any_list.is_not_empty(list(T)::ia) is semidet.

:- func any_list.cons(T, list(T)) = list(T).

    % Standard append predicate:
    % append(Start, End, List) is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- pred any_list.append(list(T), list(T), list(T)).
:- mode any_list.append(ia, ia, oa) is det.
:- mode any_list.append(oa, oa, ia) is multi.

:- func any_list.append(list(T), list(T)) = list(T).
:- mode any_list.append(ia, ia) = oa is det.

    % L1 ++ L2 = L :- append(L1, L2, L).
    %
:- func any_list.(list(T) ++ list(T)) = list(T).
:- mode any_list.(ia ++ ia) = oa is det.

    % member(Elem, List) :
    %   True iff `List' contains `Elem'.
    %
:- pred any_list.member(T::oa, list(T)::ia) is nondet.

    % member(Elem, List, SubList) :
    %   True iff `List' contains `Elem', and `SubList' is
    %   a suffix of `List' beginning with `Elem'.
    %   Same as `SubList = [Elem | _], append(_, SubList, List)'.
    %
:- pred any_list.member(T::oa, list(T)::ia, list(T)::oa) is nondet.

    % length(List, Length) :
    %   True iff `Length' is the length of `List', i.e. if
    %   `List' contains `Length' elements.
    %
:- pred any_list.length(list(T)::ia, int::out) is det.
:- func any_list.length(list(T)::ia) = (int::out) is det.

    % same_length(ListA, ListB) :
    %   True iff `ListA' and `ListB' have the same length,
    %   i.e. iff they both contain the same number of elements.
    %
:- pred any_list.same_length(list(T1)::ia, list(T2)::ia) is semidet.

    % split_list(Len, List, Start, End):
    %   splits `List' into a prefix `Start' of length `Len',
    %   and a remainder `End'.
    %   See also: take, drop.
    %
:- pred any_list.split_list(int::in, list(T)::ia, list(T)::oa,
    list(T)::oa) is semidet.

    % take(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   Fails if `List' has less than `Len' elements.
    %   See also: split_list.
    %
:- pred any_list.take(int::in, list(T)::ia, list(T)::oa) is semidet.

    % take_upto(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   If `List' has less than `Len' elements, return the entire list.
    %
:- func any_list.take_upto(int::in, list(T)::ia) = (list(T)::oa)
    is det.

    % drop(Len, List) = End:
    %   `End' is the remainder of `List' after removing the
    %   first `Len' elements.
    %   See also: split_list.
    %
:- func any_list.drop(int::in, list(T)::ia) = (list(T)::oa) is semidet.

    % insert(Elem, List0, List):
    %   `List' is the result of inserting `Elem' somewhere in `List0'.
    %   Same as `delete(List, Elem, List0)'.
    %
:- pred any_list.insert(T, list(T), list(T)).
:- mode any_list.insert(oa, oa, ia) is nondet.
:- mode any_list.insert(ia, ia, oa) is multi.

    % any_list.delete(List, Elem, Remainder):
    %   True iff `Elem' occurs in `List', and
    %   `Remainder' is the result of deleting one occurrence of
    %   `Elem' from `List'.
    %
:- pred any_list.delete(list(T)::ia, T::oa, list(T)::oa) is nondet.

    % replace_nth(List0, N, R) = List is true iff List is List0
    % with Nth element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func any_list.replace_nth(list(T)::ia, int::in, T::ia) =
    (list(T)::oa) is semidet.

    % replace_nth_det(List0, N, R, List) is true iff List is List0
    % with Nth element replaced with R.
    % Aborts if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func any_list.replace_nth_det(list(T)::ia, int::in, T::ia) =
    (list(T)::oa) is det.

    % reverse(List) = Reverse:
    %   `Reverse' is a list containing the same elements as `List'
    %   but in reverse order.
    %
:- func any_list.reverse(list(T)::ia) = (list(T)::oa) is det.

:- pred any_list.reverse(list(T)::ia, list(T)::oa) is det.

    % perm(List0, List):
    %   True iff `List' is a permutation of `List0'.
    %
:- pred any_list.perm(list(T)::ia, list(T)::oa) is multi.

    % index*(List, Position, Elem):
    %   These predicates select an element in a list from it's
    %   position.  The `index0' preds consider the first element
    %   element to be element number zero, whereas the `index1' preds
    %   consider the first element to be element number one.
    %   The `_det' preds call error/1 if the index is out of
    %   range, whereas the semidet preds fail if the index is out of
    %   range.
    %
:- pred any_list.index0(list(T)::ia, int::in, T::oa) is semidet.
:- pred any_list.index1(list(T)::ia, int::in, T::oa) is semidet.

:- pred any_list.index0_det(list(T)::ia, int::in, T::oa) is det.
:- pred any_list.index1_det(list(T)::ia, int::in, T::oa) is det.

:- func any_list.index0_det(list(T)::ia, int::in) = (T::oa) is det.
:- func any_list.index1_det(list(T)::ia, int::in) = (T::oa) is det.

    % zip(ListA, ListB) = List:
    %   List is the result of alternating the elements
    %   of ListA and ListB, starting with the first element
    %   of ListA (followed by the first element of ListB,
    %   then the second element of listA, then the second
    %   element of ListB, etc.).  When there are no more
    %   elements remaining in one of the lists,
    %   the remainder of the nonempty list is appended.
    %
:- func any_list.zip(list(T)::ia, list(T)::ia) = (list(T)::oa)
    is det.

    % duplicate(Count, Elem) = List is true iff List is a list
    % containing Count duplicate copies of Elem.
    %
:- func any_list.duplicate(int::in, T::ia) = (list(T)::oa) is det.

    % condense(ListOfLists) = List:
    %   `List' is the result of concatenating all the
    %   elements of `ListOfLists'.
    %
:- func any_list.condense(list(list(T))::ia) = (list(T)::oa)
        is det.

    % chunk(List, ChunkSize, Chunks):
    %   Takes a list `List' and breaks it into a list of lists
    %   `Chunks', such that the length of each list in `Chunks'
    %   is at most `ChunkSize.  (More precisely, the length of
    %   each list in `Chunks' other than the last one is exactly
    %   `ChunkSize', and the length of the last list in `Chunks'
    %   is between one and `ChunkSize'.)
    %
:- func any_list.chunk(list(T)::ia, int::in) = (list(list(T))::oa) is det.

    % all_same(List) is true
    %   if all elements of the list are the same
    %
:- pred any_list.all_same(list(T)::ia) is semidet.

    % last(List, Last) is true
    %   if Last is the last element of List.
    %
:- pred any_list.last(list(T)::ia, T::oa) is semidet.

    % A deterministic version of last, which aborts instead of
    % failing if the input list is empty.
    %
:- func any_list.det_last(list(T)::ia) = (T::oa) is det.

    % split_last(List, AllButLast, Last) is true
    %   if Last is the last element of List and AllButLast is the list
    %   of elements before it.
    %
:- pred any_list.split_last(list(T)::ia, list(T)::oa, T::oa)
        is semidet.

    % A deterministic version of split_last, which aborts instead of
    % failing if the input list is empty.
    %
:- pred any_list.split_last_det(list(T)::ia, list(T)::oa, T::oa)
        is det.

    % Transpose a matrix represented as a list of lists.  All elements of the
    % input list must have the same length.  The input list must be non-empty,
    % otherwise an exception is thrown.
    %
:- func transpose(list(list(T))::ia) = (list(list(T))::oa) is det.

    % As above, except that we specify the length of the resulting list.
    % The input list may be empty in which case the result is a list of
    % the required length, each of whose elements is the empty list.
    %
:- func transpose0(int::in, list(list(T))::ia) = (list(list(T))::oa) is det.

%-----------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%-----------------------------------------------------------------------------%

    % map(T, L, M) uses the closure T
    % to transform the elements of L into the elements of M.
:- pred any_list.map(pred(X, Y), list(X), list(Y)).
:- mode any_list.map(pred(ia, oa) is det, ia, oa) is det.
:- mode any_list.map(pred(ia, out) is det, ia, out) is det.
:- mode any_list.map(pred(ia, oa) is cc_multi, ia, oa) is cc_multi.
:- mode any_list.map(pred(ia, oa) is semidet, ia, oa) is semidet.
:- mode any_list.map(pred(ia, oa) is multi, ia, oa) is multi.
:- mode any_list.map(pred(ia, oa) is nondet, ia, oa) is nondet.
:- mode any_list.map(pred(ia, ia) is semidet, ia, ia) is semidet.

:- func any_list.map(func(X) = Y, list(X)) = list(Y).
:- mode any_list.map(func(ia) = oa is det, ia) = oa is det.

    % Same as map/2 but takes an impure function.
    %
:- impure func any_list.impure_map(impure func(X) = Y, list(X)) = list(Y).
:- mode any_list.impure_map(func(in) = (out) is det, in) = out is det.
:- mode any_list.impure_map(func(ia) = (out) is det, ia) = out is det.
:- mode any_list.impure_map(func(ia) = (oa) is det, ia) = oa is det.
:- mode any_list.impure_map(func(in) = (oa) is det, in) = oa is det.

    % Same as map/3, but takes an impure pred.
    %
:- impure pred any_list.impure_map(impure pred(X, Y), list(X), list(Y)).
:- mode any_list.impure_map(pred(ia, oa) is det, ia, oa) is det.
:- mode any_list.impure_map(pred(ia, out) is det, ia, out) is det.
:- mode any_list.impure_map(pred(in, oa) is det, in, oa) is det.
:- mode any_list.impure_map(pred(ia, oa) is semidet, ia, oa) is semidet.
:- mode any_list.impure_map(pred(ia, out) is semidet, ia, out) is semidet.
:- mode any_list.impure_map(pred(in, oa) is semidet, in, oa) is semidet.

    % map2(T, L, M1, M2) uses the closure T
    % to transform the elements of L into the elements of M1 and M2.
:- pred any_list.map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode any_list.map2(pred(ia, oa, oa) is det, ia, oa, oa) is det.
:- mode any_list.map2(pred(ia, oa, oa) is cc_multi, ia, oa, oa) is cc_multi.
:- mode any_list.map2(pred(ia, oa, oa) is semidet, ia, oa, oa) is semidet.
:- mode any_list.map2(pred(ia, oa, oa) is multi, ia, oa, oa) is multi.
:- mode any_list.map2(pred(ia, oa, oa) is nondet, ia, oa, oa) is nondet.
:- mode any_list.map2(pred(ia, ia, ia) is semidet, ia, ia, ia) is semidet.

    % Same as map2/4, but takes an impure pred.
    %
:- impure pred impure_map2(impure pred(A, B, C), list(A), list(B), list(C)).
:- mode impure_map2(pred(ia, oa, oa) is det, ia, oa, oa) is det.
:- mode impure_map2(pred(ia, oa, oa) is semidet, ia, oa, oa) is semidet.

    % map3(T, L, M1, M2, M3) uses the closure T
    % to transform the elements of L into the elements of M1, M2 and M3.
:- pred any_list.map3(pred(A, B, C, D), list(A), list(B), list(C),
        list(D)).
:- mode any_list.map3(pred(ia, oa, oa, oa) is det, ia, oa, oa, oa) 
        is det.
:- mode any_list.map3(pred(ia, oa, oa, oa) is cc_multi, ia, oa, oa, oa) 
        is cc_multi.
:- mode any_list.map3(pred(ia, oa, oa, oa) is semidet, ia, oa, oa, oa) 
        is semidet.
:- mode any_list.map3(pred(ia, oa, oa, oa) is multi, ia, oa, oa, oa) 
        is multi.
:- mode any_list.map3(pred(ia, oa, oa, oa) is nondet, ia, oa, oa, oa) 
        is nondet.
:- mode any_list.map3(pred(ia, ia, ia, ia) is semidet, ia, ia, ia, ia) 
        is semidet.

    % map4(T, L, M1, M2, M3, M4) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3 and 
    % M4.
:- pred any_list.map4(pred(A, B, C, D, E), list(A), list(B),
        list(C), list(D), list(E)).
:- mode any_list.map4(pred(ia, oa, oa, oa, oa) is det, ia, oa, oa, oa, oa)
        is det.
:- mode any_list.map4(pred(ia, oa, oa, oa, oa) is cc_multi, ia, oa, oa, oa, oa)
        is cc_multi.
:- mode any_list.map4(pred(ia, oa, oa, oa, oa) is semidet, ia, oa, oa, oa, oa)
        is semidet.
:- mode any_list.map4(pred(ia, oa, oa, oa, oa) is multi, ia, oa, oa, oa, oa)
        is multi.
:- mode any_list.map4(pred(ia, oa, oa, oa, oa) is nondet, ia, oa, oa, oa, oa)
        is nondet.
:- mode any_list.map4(pred(ia, ia, ia, ia, ia) is semidet, ia, ia, ia, ia, ia)
        is semidet.

    % map5(T, L, M1, M2, M3, M4, M5) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4 
    % and M5.
:- pred any_list.map5(pred(A, B, C, D, E, F), list(A), list(B),
        list(C), list(D), list(E), list(F)).
:- mode any_list.map5(pred(ia, oa, oa, oa, oa, oa) is det, ia, oa, oa, oa,
        oa, oa) is det.
:- mode any_list.map5(pred(ia, oa, oa, oa, oa, oa) is cc_multi, ia, oa, oa,
        oa, oa, oa) is cc_multi.
:- mode any_list.map5(pred(ia, oa, oa, oa, oa, oa) is semidet, ia, oa, oa, 
        oa, oa, oa) is semidet.
:- mode any_list.map5(pred(ia, oa, oa, oa, oa, oa) is multi, ia, oa, oa, 
        oa, oa, oa) is multi.
:- mode any_list.map5(pred(ia, oa, oa, oa, oa, oa) is nondet, ia, oa, oa, 
        oa, oa, oa) is nondet.
:- mode any_list.map5(pred(ia, ia, ia, ia, ia, ia) is semidet, ia, ia, ia,
        ia, ia, ia) is semidet.

    % map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5 and M6.
:- pred any_list.map6(pred(A, B, C, D, E, F, G), list(A), list(B),
        list(C), list(D), list(E), list(F), list(G)).
:- mode any_list.map6(pred(ia, oa, oa, oa, oa, oa, oa) is det, ia, oa, oa, 
        oa, oa, oa, oa) is det.
:- mode any_list.map6(pred(ia, oa, oa, oa, oa, oa, oa) is cc_multi, ia, oa,
        oa, oa, oa, oa, oa) is cc_multi.
:- mode any_list.map6(pred(ia, oa, oa, oa, oa, oa, oa) is semidet, ia, oa, 
        oa, oa, oa, oa, oa) is semidet.
:- mode any_list.map6(pred(ia, oa, oa, oa, oa, oa, oa) is multi, ia, oa, 
        oa, oa, oa, oa, oa) is multi.
:- mode any_list.map6(pred(ia, oa, oa, oa, oa, oa, oa) is nondet, ia, oa, 
        oa, oa, oa, oa, oa) is nondet.
:- mode any_list.map6(pred(ia, ia, ia, ia, ia, ia, ia) is semidet, ia, ia,
        ia, ia, ia, ia, ia) is semidet.

    % map7(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5, M6 and M7.
:- pred any_list.map7(pred(A, B, C, D, E, F, G, H), list(A), list(B),
        list(C), list(D), list(E), list(F), list(G),
        list(H)).
:- mode any_list.map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is det,
        ia, oa, oa, oa, oa, oa, oa, oa) is det.
:- mode any_list.map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is cc_multi,
        ia, oa, oa, oa, oa, oa, oa, oa) is cc_multi.
:- mode any_list.map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is semidet,
        ia, oa, oa, oa, oa, oa, oa, oa) is semidet.
:- mode any_list.map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is multi,
        ia, oa, oa, oa, oa, oa, oa, oa) is multi.
:- mode any_list.map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is nondet,
        ia, oa, oa, oa, oa, oa, oa, oa) is nondet.
:- mode any_list.map7(pred(ia, ia, ia, ia, ia, ia, ia, ia) is semidet,
        ia, ia, ia, ia, ia, ia, ia, ia) is semidet.

    % map8(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5, M6, M7 and M8.
:- pred any_list.map8(pred(A, B, C, D, E, F, G, H, I), list(A),
        list(B), list(C), list(D), list(E), list(F),
        list(G), list(H), list(I)).
:- mode any_list.map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is det,
        ia, oa, oa, oa, oa, oa, oa, oa, oa) is det.
:- mode any_list.map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is cc_multi,
        ia, oa, oa, oa, oa, oa, oa, oa, oa) is cc_multi.
:- mode any_list.map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is semidet,
        ia, oa, oa, oa, oa, oa, oa, oa, oa) is semidet.
:- mode any_list.map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is multi,
        ia, oa, oa, oa, oa, oa, oa, oa, oa) is multi.
:- mode any_list.map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is nondet,
        ia, oa, oa, oa, oa, oa, oa, oa, oa) is nondet.
:- mode any_list.map8(pred(ia, ia, ia, ia, ia, ia, ia, ia, ia) is semidet,
        ia, ia, ia, ia, ia, ia, ia, ia, ia) is semidet.

    % map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
    %   [F(A1, B1), .., F(An, Bn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func any_list.map_corresponding(func(A, B) = C, list(A), list(B)) =
        list(C).
:- mode any_list.map_corresponding(func(ia, ia) = oa is det, ia, ia) = oa 
        is det.
:- mode any_list.map_corresponding(func(ia, in) = oa is det, ia, in) = oa 
        is det.
:- mode any_list.map_corresponding(func(in, ia) = oa is det, in, ia) = oa 
        is det.

:- pred any_list.map_corresponding(pred(A, B, C), list(A), list(B), list(C)).
:- mode any_list.map_corresponding(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode any_list.map_corresponding(pred(ia, in, oa) is det, ia, in, oa) is det.
:- mode any_list.map_corresponding(pred(in, ia, oa) is det, in, ia, oa) is det.

    % map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func any_list.map_corresponding3(func(A, B, C) = D, list(A),
        list(B), list(C)) = list(D).
:- mode any_list.map_corresponding3(func(ia, ia, ia) = oa is det, ia, ia, ia) =
        oa is det.

    % foldl(Pred, List, Start, End) calls Pred with each
    % element of List (working left-to-right) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    %
:- pred any_list.foldl(pred(L, A, A), list(L), A, A).
:- mode any_list.foldl(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode any_list.foldl(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode any_list.foldl(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode any_list.foldl(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode any_list.foldl(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode any_list.foldl(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func any_list.foldl(func(L, A) = A, list(L), A) = A.
:- mode any_list.foldl(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % Same as foldl/4, but takes an impure pred.
    %
:- impure pred impure_foldl(impure pred(L, A, A), list(L), A, A).
:- mode impure_foldl(pred(ia, in, out) is det, ia, in, out) is det.
:- mode impure_foldl(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode impure_foldl(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode impure_foldl(pred(ia, di, uo) is semidet, ia, di, uo) is semidet.

    % foldr(Pred, List, Start, End) calls Pred with each
    % element of List (working right-to-left) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    % 
:- pred any_list.foldr(pred(L, A, A), list(L), A, A).
:- mode any_list.foldr(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode any_list.foldr(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode any_list.foldr(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode any_list.foldr(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode any_list.foldr(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode any_list.foldr(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func any_list.foldr(func(L, A) = A, list(L), A) = A.
:- mode any_list.foldr(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % foldl2(Pred, List, !Acc1, !Acc2)
    % Does the same job as foldl, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred any_list.foldl2(pred(L, A, A, Z, Z), list(L), A, A, Z, Z).
:- mode any_list.foldl2(pred(ia, ia, oa, ia, oa) is det,
        ia, ia, oa, ia, oa) is det.
:- mode any_list.foldl2(pred(ia, ia, oa, ia, oa) is cc_multi,
        ia, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl2(pred(ia, ia, oa, ia, oa) is semidet,
        ia, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl2(pred(ia, ia, oa, ia, oa) is nondet,
        ia, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl2(pred(ia, ia, oa, mdi, muo) is det,
        ia, ia, oa, mdi, muo) is det.
:- mode any_list.foldl2(pred(ia, ia, oa, di, uo) is det,
        ia, ia, oa, di, uo) is det.
:- mode any_list.foldl2(pred(ia, di, uo, di, uo) is det,
        ia, di, uo, di, uo) is det.
:- mode any_list.foldl2(pred(ia, ia, oa, mdi, muo) is cc_multi,
        ia, ia, oa, mdi, muo) is cc_multi.
:- mode any_list.foldl2(pred(ia, ia, oa, di, uo) is cc_multi,
        ia, ia, oa, di, uo) is cc_multi.
:- mode any_list.foldl2(pred(ia, di, uo, di, uo) is cc_multi,
        ia, di, uo, di, uo) is cc_multi.

    % Same as foldl2/6, but takes an impure pred.
    %
:- impure pred impure_foldl2(impure pred(L, A, A, B, B), list(L), A, A, B, B).
:- mode impure_foldl2(pred(ia, ia, oa, ia, oa) is semidet, ia, ia, oa, ia, oa)
	is semidet.

    % foldl3(Pred, List, !Acc1, !Acc2, !Acc3)
    % Does the same job as foldl, but with three accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred any_list.foldl3(pred(L, A, A, B, B, C, C), list(L),
        A, A, B, B, C, C).
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is det,
        ia, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is cc_multi,
        ia, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is semidet,
        ia, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is nondet,
        ia, ia, oa, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, di, uo) is det,
        ia, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.foldl3(pred(ia, ia, oa, ia, oa, di, uo) is cc_multi,
        ia, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4)
    % Does the same job as foldl, but with four accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred any_list.foldl4(pred(L, A, A, B, B, C, C, D, D), list(L),
        A, A, B, B, C, C, D, D).
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is det,
        ia, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is semidet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is nondet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, di, uo) is det,
        ia, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.foldl4(pred(ia, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5)
    % Does the same job as foldl, but with five accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred any_list.foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), list(L),
        A, A, B, B, C, C, D, D, E, E).
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is det,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is semidet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is nondet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
        is det,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
        is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6)
    % Does the same job as foldl, but with six accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred any_list.foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F),
        list(L), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is det,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is semidet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is nondet,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        di, uo) is det,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        di, uo) is cc_multi,
        ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl_corresponding(F, As, Bs, !Acc)
    % Does the same job as foldl, but works on two lists in
    % parallel.  An exception is raised if the list arguments differ
    % in length.
    %
:- pred any_list.foldl_corresponding(pred(A, B, C, C),
        list(A), list(B), C, C).
:- mode any_list.foldl_corresponding(pred(ia, ia, ia, oa) is det,
        ia, ia, ia, oa) is det.
:- mode any_list.foldl_corresponding(pred(ia, ia, ia, oa) is cc_multi,
        ia, ia, ia, oa) is cc_multi.
:- mode any_list.foldl_corresponding(pred(ia, ia, ia, oa) is semidet,
        ia, ia, ia, oa) is semidet.
:- mode any_list.foldl_corresponding(pred(ia, ia, ia, oa) is nondet,
        ia, ia, ia, oa) is nondet.
:- mode any_list.foldl_corresponding(pred(ia, ia, di, uo) is det,
        ia, ia, di, uo) is det.
:- mode any_list.foldl_corresponding(pred(ia, ia, di, uo) is cc_multi,
        ia, ia, di, uo) is cc_multi.

    % Same as foldl_corresponding, but takes an impure predicate.
    % 
:- impure pred any_list.impure_foldl_corresponding(impure pred(A, B, C, C),
        list(A), list(B), C, C).
:- mode any_list.impure_foldl_corresponding(pred(ia, ia, ia, oa) is det,
        ia, ia, ia, oa) is det.
:- mode any_list.impure_foldl_corresponding(pred(ia, ia, di, uo) is semidet,
        ia, ia, di, uo) is semidet.
:- mode any_list.impure_foldl_corresponding(pred(ia, ia, ia, oa) is semidet,
        ia, ia, ia, oa) is semidet.

    % foldl2_corresponding(F, As, Bs, !Acc1, !Acc2)
    % Does the same job as foldl_corresponding, but has two
    % accumulators.
    %
:- pred any_list.foldl2_corresponding(pred(A, B, C, C, D, D),
        list(A), list(B), C, C, D, D).
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is det,
        ia, ia, ia, oa, ia, oa) is det.
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is cc_multi,
        ia, ia, ia, oa, ia, oa) is cc_multi.
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is semidet,
        ia, ia, ia, oa, ia, oa) is semidet.
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is nondet,
        ia, ia, ia, oa, ia, oa) is nondet.
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, di, uo) is det,
        ia, ia, ia, oa, di, uo) is det.
:- mode any_list.foldl2_corresponding(pred(ia, ia, ia, oa, di, uo) is cc_multi,
        ia, ia, ia, oa, di, uo) is cc_multi.

    % map_foldl(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working left-to-right) to transform
    % InList into OutList.  The final value of the accumulator is
    % returned in End.
    % 
:- pred any_list.map_foldl(pred(L, M, A, A), list(L), list(M), A, A).
:- mode any_list.map_foldl(pred(ia, oa, di, uo) is det, ia, oa, di, uo)
        is det.
:- mode any_list.map_foldl(pred(ia, oa, ia, oa) is det, ia, oa, ia, oa)
        is det.
:- mode any_list.map_foldl(pred(ia, oa, di, uo) is cc_multi, ia, oa, di, uo)
        is cc_multi.
:- mode any_list.map_foldl(pred(ia, oa, ia, oa) is cc_multi, ia, oa, ia, oa)
        is cc_multi.
:- mode any_list.map_foldl(pred(ia, oa, ia, oa) is semidet, ia, oa, ia, oa)
        is semidet.
:- mode any_list.map_foldl(pred(ia, oa, ia, oa) is nondet, ia, oa, ia, oa)
        is nondet.

    % Same as map_foldl/5, but takes an impure pred.
    %
:- impure pred impure_map_foldl(impure pred(L, M, A, A), list(L), list(M),
	A, A).
:- mode impure_map_foldl(pred(ia, oa, ia, oa) is semidet, ia, oa, ia, oa)
	is semidet.
:- mode impure_map_foldl(pred(ia, oa, di, uo) is semidet, ia, oa, di, uo)
	is semidet.

    % Same as map_foldl, but with two mapped outputs.
    % 
:- pred any_list.map2_foldl(pred(L, M, N, A, A),
        list(L), list(M), list(N), A, A).
:- mode any_list.map2_foldl(pred(ia, oa, oa, di, uo) is det, ia, oa, oa,
        di, uo) is det.
:- mode any_list.map2_foldl(pred(ia, oa, oa, ia, oa) is det, ia, oa, oa,
        ia, oa) is det.
:- mode any_list.map2_foldl(pred(ia, oa, oa, di, uo) is cc_multi, ia, oa, oa,
        di, uo) is cc_multi.
:- mode any_list.map2_foldl(pred(ia, oa, oa, ia, oa) is cc_multi, ia, oa, oa,
        ia, oa) is cc_multi.
:- mode any_list.map2_foldl(pred(ia, oa, oa, ia, oa) is semidet, ia, oa, oa,
        ia, oa) is semidet.
:- mode any_list.map2_foldl(pred(ia, oa, oa, ia, oa) is nondet, ia, oa, oa,
        ia, oa) is nondet.

    % Same as map2_foldl/6, but takes an impure pred.
    %
:- impure pred impure_map2_foldl(impure pred(L, M, N, A, A),
	list(L), list(M), list(N), A, A).
:- mode impure_map2_foldl(pred(ia, oa, oa, ia, oa) is semidet,
	ia, oa, oa, ia, oa) is semidet.

    % Same as map_foldl, but with two accumulators.
    %
:- pred any_list.map_foldl2(pred(L, M, A, A, B, B),
        list(L), list(M), A, A, B, B).
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, di, uo) is det,
        ia, oa, ia, oa, di, uo) is det.
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, ia, oa) is det,
        ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, di, uo) is cc_multi,
        ia, oa, ia, oa, di, uo) is cc_multi.
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, ia, oa) is cc_multi,
        ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, ia, oa) is semidet,
        ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.map_foldl2(pred(ia, oa, ia, oa, ia, oa) is nondet,
        ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl2/6, but takes an impure pred.
    %
:- impure pred impure_map_foldl2(impure pred(L, M, A, A, B, B),
	list(L), list(M), A, A, B, B).
:- mode impure_map_foldl2(pred(ia, oa, ia, oa, ia, oa) is semidet,
	ia, oa, ia, oa, ia, oa) is semidet.
:- mode impure_map_foldl2(pred(ia, oa, ia, oa, di, uo) is semidet,
    ia, oa, ia, oa, di, uo) is semidet.

    % Same as map_foldl, but with three accumulators.
    %
:- pred any_list.map_foldl3(pred(L, M, A, A, B, B, C, C),
        list(L), list(M), A, A, B, B, C, C).
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, di, uo) is det,
        ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is det,
        ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, di, uo) is cc_multi,
        ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is semidet,
        ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is nondet,
        ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with four accumulators.
    %
:- pred any_list.map_foldl4(pred(L, M, A, A, B, B, C, C, D, D),
        list(L), list(M), A, A, B, B, C, C, D, D).
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
        is det,
        ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is det,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
        is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is semidet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is nondet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with five accumulators.
    %
:- pred any_list.map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
        list(L), list(M), A, A, B, B, C, C, D, D, E, E).
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        di, uo) is det,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is det,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        di, uo) is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is semidet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode any_list.map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa) is nondet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with six accumulators.
    %
:- pred any_list.map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
        list(L), list(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, di, uo) is det,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, ia, oa) is det,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, di, uo) is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
        is cc_multi.
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, ia, oa) is cc_multi,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is cc_multi.
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, ia, oa) is semidet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is semidet.
:- mode any_list.map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
        ia, oa, ia, oa) is nondet,
        ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
        is nondet.

    % all_true(Pred, List) takes a closure with one input argument.
    % If Pred succeeds for every member of List, all_true succeeds.
    % If Pred fails for any member of List, all_true fails.
    %
:- pred any_list.all_true(pred(T)::in(pred(ia) is semidet), list(T)::ia) 
        is semidet.

    % Same as all_true, but traverses two lists in parallel.
    % An exception is raised if the list arguments differ in length.
    %
:- pred any_list.all_true_corresponding(pred(A, B), list(A), list(B)).
:- mode any_list.all_true_corresponding(pred(ia, ia) is semidet, ia, ia)
	is semidet.

    % Same as all_true_corresponding, but takes an impure pred.
    %
:- impure pred any_list.impure_all_true_corresponding(impure pred(A, B),
	list(A), list(B)).
:- mode any_list.impure_all_true_corresponding(pred(ia, ia) is det, ia, ia)
	is det.
:- mode any_list.impure_all_true_corresponding(pred(ia, ia) is semidet, ia, ia)
	is semidet.

%-----------------------------------------------------------------------------%

:- func any_list.head(list(T)::ia) = (T::oa) is semidet.

:- func any_list.tail(list(T)::ia) = (list(T)::oa) is semidet.

    % det_head(List) returns the first element of List,
    % calling error/1 if List is empty.
    %
:- func any_list.det_head(list(T)::ia) = (T::oa) is det.

    % det_tail(List) returns the tail of List,
    % calling error/1 if List is empty.
    %
:- func any_list.det_tail(list(T)::ia) = (list(T)::oa) is det.

%-----------------------------------------------------------------------------%

    % any_list.sort(Compare, Unsorted, Sorted) is true iff Sorted is a
    % list containing the same elements as Unsorted, where Sorted is
    % sorted with respect to the ordering defined by the predicate
    % Compare, and the elements that are equivalent in this ordering
    % appear in the same sequence in Sorted as they do in Unsorted
    % (that is, the sort is stable).
    %
:- pred any_list.sort(comparison_pred(T)::in(comparison_pred(any)),
    list(T)::ia, list(T)::oa) is det.

    % any_list.merge(Compare, As, Bs, Sorted) is true iff, assuming As and
    % Bs are sorted with respect to the ordering defined by Compare,
    % Sorted is a list containing the elements of As and Bs which is
    % also sorted.  For elements which are equivalent in the ordering,
    % if they come from the same list then they appear in the same
    % sequence in Sorted as they do in that list, otherwise the elements
    % from As appear before the elements from Bs.
    %
:- pred any_list.merge(comparison_pred(T)::in(comparison_pred(any)),
    list(T)::ia, list(T)::ia, list(T)::oa) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

any_list.is_empty([]).

any_list.is_not_empty([_ | _]).

any_list.cons(H, T) = [H | T].

any_list.append([], Ys, Ys).
any_list.append([X | Xs], Ys, [X | Zs]) :-
    any_list.append(Xs, Ys, Zs).

any_list.append(Xs, Ys) = Zs :-
    any_list.append(Xs, Ys, Zs).

%-----------------------------------------------------------------------------%

any_list.index0([X | Xs], N, Y) :-
    ( if N = 0 then Y = X else any_list.index0(Xs, N - 1, Y) ).

any_list.index1([X | Xs], N, Y) :-
    ( if N = 1 then Y = X else any_list.index1(Xs, N - 1, Y) ).

any_list.index0_det(List, N, Elem) :-
    promise_pure (
        any_list.index0(List, N, Elem0)
    ->
        Elem = Elem0
    ;
        error("any_list.index0_det: index out of range")
    ).

any_list.index1_det(List, N, Elem) :-
    promise_pure (
        any_list.index1(List, N, Elem0)
    ->
        Elem = Elem0
    ;
        error("any_list.index1_det: index out of range")
    ).

any_list.index0_det(List, N) = Elem :-
    any_list.index0_det(List, N, Elem).

any_list.index1_det(List, N) = Elem :-
    any_list.index1_det(List, N, Elem).

%-----------------------------------------------------------------------------%

any_list.condense([])       = [].
any_list.condense([L | Ls]) = any_list.(L ++ any_list.condense(Ls)).

%-----------------------------------------------------------------------------%

any_list.same_length([], []).
any_list.same_length([_ | L1], [_ | L2]) :-
    any_list.same_length(L1, L2).

%-----------------------------------------------------------------------------%

any_list.insert(X, Ys,       [X | Ys]).
any_list.insert(X, [Y | Ys], [Y | Zs]) :-
    any_list.insert(X, Ys, Zs).

%-----------------------------------------------------------------------------%

any_list.delete(List, Elem, Remainder) :-
    any_list.insert(Elem, Remainder, List).

%-----------------------------------------------------------------------------%

any_list.replace_nth(Xs, P, R) = L :-
    P > 0,
    any_list.replace_nth_2(Xs, P, R, L).

any_list.replace_nth_det(Xs, P, R) = L :-
    promise_pure (
        P > 0
    ->
        ( any_list.replace_nth_2(Xs, P, R, L0) ->
            L = L0
        ;
            error("any_list.replace_nth_det: " ++
                "Can't replace element whose index " ++
                "position is past the end of the list")
        )
    ;
        error("any_list.replace_nth_det: " ++
            "Can't replace element whose index " ++
            "position is less than 1.")
    ).

:- pred any_list.replace_nth_2(list(T)::ia, int::in, T::ia,
        list(T)::oa) is semidet.

any_list.replace_nth_2([X | Xs], P, R, L) :-
    ( P = 1 ->
        L = [R | Xs]
    ;
        any_list.replace_nth_2(Xs, P - 1, R, L0),
        L = [X | L0]
    ).

%-----------------------------------------------------------------------------%

any_list.member(X, [X | _]).
any_list.member(X, [_ | Xs]) :-
    any_list.member(X, Xs).

any_list.member(Element, List, SubList) :-
    SubList = [Element | _],
    any_list.append(_, SubList, List).

%-----------------------------------------------------------------------------%

% Note - it is not possible to write a version of
% length/1 in pure Mercury that works in both directions
% unless you make it semidet rather than det.

any_list.length(L) = N :-
    any_list.length_2(L, 0, N).

any_list.length(L, N) :-
    any_list.length_2(L, 0, N).

:- pred any_list.length_2(list(T)::ia, int::in, int::out) is det.

any_list.length_2([], N, N).
any_list.length_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    any_list.length_2(L1, N1, N).

%-----------------------------------------------------------------------------%

any_list.reverse(L0) = L :-
    any_list.reverse_2(L0, [], L).

any_list.reverse(L0, L) :-
    any_list.reverse_2(L0, [], L).

:- pred any_list.reverse_2(list(T)::ia, list(T)::ia, list(T)::oa)
        is det.

any_list.reverse_2([], L, L).
any_list.reverse_2([X | Xs], L0, L) :-
    any_list.reverse_2(Xs, [X | L0], L).

%-----------------------------------------------------------------------------%

any_list.zip([], Bs) = Bs.
any_list.zip([A | As], Bs) = [A | Cs] :-
    any_list.zip2(As, Bs, Cs).

:- pred any_list.zip2(list(T)::ia, list(T)::ia, list(T)::oa) is det.

any_list.zip2(As, [], As).
any_list.zip2(As, [B | Bs], [B | Cs]) :-
    any_list.zip2(As, Bs, Cs).

%-----------------------------------------------------------------------------%

any_list.split_list(N, List, Start, End) :-
    ( N = 0 ->
        Start = [],
        End = List
    ;
        N > 0,
        List = [Head | List1],
        Start = [Head | Start1],
        any_list.split_list(N - 1, List1, Start1, End)
    ).

any_list.take(N, As, Bs) :-
    ( N > 0 ->
        As = [A | As1],
        any_list.take(N - 1, As1, Bs1),
        Bs = [A | Bs1]
    ;
        Bs = []
    ).

any_list.take_upto(N, As) = Bs :-
    promise_pure (
        any_list.take(N, As, Bs0)
    ->
        Bs = Bs0
    ;
        Bs = As
    ).

any_list.drop(N, As) = Bs :-
    ( N > 0 ->
        As = [_ | Cs],
        Bs = any_list.drop(N - 1, Cs)
    ;
        As = Bs
    ).

%-----------------------------------------------------------------------------%

any_list.duplicate(N, X) = any_list.duplicate(N, X, []).

:- func any_list.duplicate(int::in, T::ia, list(T)::ia) = (list(T)::oa)
        is det.

any_list.duplicate(N, X, Xs) =
    ( N > 0 ->
        any_list.duplicate(N-1, X, [X|Xs])
    ;
        Xs
    ).

%-----------------------------------------------------------------------------%

any_list.chunk(List, ChunkSize) = ListOfSmallLists :-
    any_list.chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred any_list.chunk_2(list(T)::ia, int::in, list(T)::ia, int::in,
        list(list(T))::oa) is det.

any_list.chunk_2([], _ChunkSize, List0, _N, Lists) :-
    promise_pure
    ( List0 = [] ->
        Lists = []
    ;
        List  = any_list.reverse(List0),
        Lists = [List]
    ).
any_list.chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( N > 1 ->
        any_list.chunk_2(Xs, ChunkSize, [X | List0], N - 1, Lists)
    ;
        List = any_list.reverse([X | List0]),
        any_list.chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1),
        Lists = [List | Lists1]
    ).

%-----------------------------------------------------------------------------%

any_list.perm([], []).
any_list.perm([X | Xs], Ys) :-
    any_list.perm(Xs, Ys0),
    any_list.insert(X, Ys0, Ys).

%-----------------------------------------------------------------------------%

any_list.all_same([]).
any_list.all_same([H | T]) :-
    any_list.all_same_2(H, T).

:- pred any_list.all_same_2(T::ia, list(T)::ia) is semidet.

any_list.all_same_2(_, []).
any_list.all_same_2(H, [H | T]) :-
    any_list.all_same_2(H, T).

%-----------------------------------------------------------------------------%

any_list.last([H | T], Last) :-
    (
        T = [],
        Last = H
    ;
        T = [_ | _],
        any_list.last(T, Last)
    ).

any_list.det_last(List) = Last :-
    promise_pure (
        any_list.last(List, LastPrime)
    ->
        Last = LastPrime
    ;
        error("any_list.last_det: empty list")
    ).

any_list.split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [_ | _],
        any_list.split_last(T, AllButLast1, Last),
        AllButLast = [H | AllButLast1]
    ).

any_list.split_last_det(List, AllButLast, Last) :-
    promise_pure (
        any_list.split_last(List, AllButLastPrime, LastPrime)
    ->
        AllButLast = AllButLastPrime,
        Last = LastPrime
    ;
        error("any_list.split_last_det: empty list")
    ).

%-----------------------------------------------------------------------------%

transpose([]) = _ :-
    throw("any_list.transpose: list is empty").
transpose([A | As]) = transpose0(any_list.length(A), [A | As]).

transpose0(N, As) = Bs :-
	Bs0 = any_list.duplicate(N, []),
	transpose_2(As, Bs0, Bs).

:- pred transpose_2(list(list(T))::ia, list(list(T))::ia, list(list(T))::oa)
	is det.

transpose_2([], Bs, Bs).
transpose_2([A | As], Bs0, Bs) :-
	transpose_2(As, Bs0, Bs1),
	Bs = any_list.map_corresponding(
		(func(X::ia, Xs::ia) = ([X | Xs]::oa) is det),
		A, Bs1).

%-----------------------------------------------------------------------------%

any_list.map(_, [],  []).
any_list.map(P, [H0 | T0], [H | T]) :-
    call(P, H0, H),
    any_list.map(P, T0, T).

any_list.impure_map(_, []) = [].
any_list.impure_map(F, [H0 | T0]) = [H | T] :-
    impure H = impure_apply(F, H0),
    impure T = any_list.impure_map(F, T0).

any_list.impure_map(_, [],  []).
any_list.impure_map(P, [H0 | T0], [H | T]) :-
    impure call(P, H0, H),
    impure any_list.impure_map(P, T0, T).

any_list.map2(_, [],  [],  []).
any_list.map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
    call(P, H0, H1, H2),
    any_list.map2(P, T0, T1, T2).

any_list.impure_map2(_, [],  [],  []).
any_list.impure_map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
	impure P(H0, H1, H2),
	impure any_list.impure_map2(P, T0, T1, T2).

any_list.map3(_, [],  [],  [],  []).
any_list.map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
    call(P, H0, H1, H2, H3),
    any_list.map3(P, T0, T1, T2, T3).

any_list.map4(_, [], [], [], [], []).
any_list.map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
    call(P, H0, H1, H2, H3, H4),
    any_list.map4(P, T0, T1, T2, T3, T4).

any_list.map5(_, [], [], [], [], [], []).
any_list.map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5]) :-
    call(P, H0, H1, H2, H3, H4, H5),
    any_list.map5(P, T0, T1, T2, T3, T4, T5).

any_list.map6(_, [], [], [], [], [], [], []).
any_list.map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6),
    any_list.map6(P, T0, T1, T2, T3, T4, T5, T6).

any_list.map7(_, [], [], [], [], [], [], [], []).
any_list.map7(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6], [H7 | T7]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6, H7),
    any_list.map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

any_list.map8(_, [], [], [], [], [], [], [], [], []).
any_list.map8(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6], [H7 | T7], [H8 | T8]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6, H7, H8),
    any_list.map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

any_list.map_corresponding(_, [], []) = [].
any_list.map_corresponding(_, [], [_ | _]) =
    func_error("any_list.map_corresponding/3: mismatched list arguments").
any_list.map_corresponding(_, [_ | _], []) =
    func_error("any_list.map_corresponding/3: mismatched list arguments").
any_list.map_corresponding(F, [A | As], [B | Bs]) =
    [F(A, B) | any_list.map_corresponding(F, As, Bs)].

any_list.map_corresponding(_, [], [], []).
any_list.map_corresponding(_, [], [_ | _], _) :-
    error("any_list.map_corresponding/4: mismatched list arguments").
any_list.map_corresponding(_, [_ | _], [], _) :-
    error("any_list.map_corresponding/4: mismatched list arguments").
any_list.map_corresponding(P, [A | As], [B | Bs], [C | Cs]) :-
    P(A, B, C),
    any_list.map_corresponding(P, As, Bs, Cs).

any_list.map_corresponding3(F, As, Bs, Cs) = Ds :-
    promise_pure
    (
        As = [A | As0],
        Bs = [B | Bs0],
        Cs = [C | Cs0]
    ->
        Ds = [F(A, B, C) | any_list.map_corresponding3(F, As0, Bs0, Cs0)]
    ;
        As = [],
        Bs = [],
        Cs = []
    ->
        Ds = []
    ;
        error("any_list.map_corresponding3: mismatched list arguments")
    ).

any_list.foldl(_, [], !A).
any_list.foldl(P, [H | T], !A) :-
    call(P, H, !A),
    any_list.foldl(P, T, !A).

any_list.impure_foldl(_, [], !A).
any_list.impure_foldl(P, [H | T], !A) :-
	impure P(H, !A),
	impure any_list.impure_foldl(P, T, !A).

any_list.foldl2(_, [], !A, !B).
any_list.foldl2(P, [H | T], !A, !B) :-
    call(P, H, !A, !B),
    any_list.foldl2(P, T, !A, !B).

any_list.impure_foldl2(_, [], !A, !B).
any_list.impure_foldl2(P, [H | T], !A, !B) :-
	impure P(H, !A, !B),
	impure any_list.impure_foldl2(P, T, !A, !B).

any_list.foldl3(_, [], !A, !B, !C).
any_list.foldl3(P, [H | T], !A, !B, !C) :-
    call(P, H, !A, !B, !C),
    any_list.foldl3(P, T, !A, !B, !C).

any_list.foldl4(_, [], !A, !B, !C, !D).
any_list.foldl4(P, [H | T], !A, !B, !C, !D) :-
    call(P, H, !A, !B, !C, !D),
    any_list.foldl4(P, T, !A, !B, !C, !D).

any_list.foldl5(_, [], !A, !B, !C, !D, !E).
any_list.foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
    call(P, H, !A, !B, !C, !D, !E),
    any_list.foldl5(P, T, !A, !B, !C, !D, !E).

any_list.foldl6(_, [], !A, !B, !C, !D, !E, !F).
any_list.foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
    call(P, H, !A, !B, !C, !D, !E, !F),
    any_list.foldl6(P, T, !A, !B, !C, !D, !E, !F).

any_list.foldl_corresponding(_, [], [], !Acc).
any_list.foldl_corresponding(_, [], [_ | _], _, _) :-
    error("any_list.foldl_corresponding/5: mismatched list arguments").
any_list.foldl_corresponding(_, [_ | _], [], _, _) :-
    error("any_list.foldl_corresponding/5: mismatched list arguments").
any_list.foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    P(A, B, !Acc),
    any_list.foldl_corresponding(P, As, Bs, !Acc).

any_list.impure_foldl_corresponding(_, [], [], !Acc).
any_list.impure_foldl_corresponding(_, [], [_ | _], _, _) :-
    error("any_list.impure_foldl_corresponding/5: mismatched list arguments").
any_list.impure_foldl_corresponding(_, [_ | _], [], _, _) :-
    error("any_list.impure_foldl_corresponding/5: mismatched list arguments").
any_list.impure_foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    impure P(A, B, !Acc),
    impure any_list.impure_foldl_corresponding(P, As, Bs, !Acc).

any_list.foldl2_corresponding(_, [], [], !Acc1, !Acc2).
any_list.foldl2_corresponding(_, [], [_ | _], _, _, _, _) :-
    error("any_list.foldl2_corresponding/7: mismatched list arguments").
any_list.foldl2_corresponding(_, [_ | _], [], _, _, _, _) :-
    error("any_list.foldl2_corresponding/7: mismatched list arguments").
any_list.foldl2_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2) :-
    P(A, B, !Acc1, !Acc2),
    any_list.foldl2_corresponding(P, As, Bs, !Acc1, !Acc2).

any_list.map_foldl(_, [], [], !A).
any_list.map_foldl(P, [H0 | T0], [H | T], !A) :-
    call(P, H0, H, !A),
    any_list.map_foldl(P, T0, T, !A).

impure_map_foldl(_, [], [], !A).
impure_map_foldl(P, [H0 | T0], [H | T], !A) :-
	impure P(H0, H, !A),
	impure impure_map_foldl(P, T0, T, !A).

any_list.map2_foldl(_, [], [], [], !A).
any_list.map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
    call(P, H0, H1, H2, !A),
    any_list.map2_foldl(P, T0, T1, T2, !A).

any_list.impure_map2_foldl(_, [], [], [], !A).
any_list.impure_map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
	impure P(H0, H1, H2, !A),
	impure any_list.impure_map2_foldl(P, T0, T1, T2, !A).

any_list.map_foldl2(_, [], [], !A, !B).
any_list.map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
    call(P, H0, H, !A, !B),
    any_list.map_foldl2(P, T0, T, !A, !B).

any_list.impure_map_foldl2(_, [], [], !A, !B).
any_list.impure_map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
	impure P(H0, H, !A, !B),
	impure any_list.impure_map_foldl2(P, T0, T, !A, !B).

any_list.map_foldl3(_, [], [], !A, !B, !C).
any_list.map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
    call(P, H0, H, !A, !B, !C),
    any_list.map_foldl3(P, T0, T, !A, !B, !C).

any_list.map_foldl4(_, [], [], !A, !B, !C, !D).
any_list.map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
    call(P, H0, H, !A, !B, !C, !D),
    any_list.map_foldl4(P, T0, T, !A, !B, !C, !D).

any_list.map_foldl5(_, [], [], !A, !B, !C, !D, !E).
any_list.map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
    call(P, H0, H, !A, !B, !C, !D, !E),
    any_list.map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

any_list.map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
any_list.map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
    call(P, H0, H, !A, !B, !C, !D, !E, !F),
    any_list.map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

any_list.foldr(_, [], !A).
any_list.foldr(P, [H | T], !A) :-
    any_list.foldr(P, T, !A),
    call(P, H, !A).

any_list.all_true(_P, []).
any_list.all_true(P, [X | Xs]) :-
    P(X),
    any_list.all_true(P, Xs).

any_list.all_true_corresponding(_, [], []).
any_list.all_true_corresponding(P, [A | As], [B | Bs]) :-
	P(A, B),
	any_list.all_true_corresponding(P, As, Bs).
any_list.all_true_corresponding(_, [], [_ | _]) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").
any_list.all_true_corresponding(_, [_ | _], []) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").


any_list.impure_all_true_corresponding(_, [], []).
any_list.impure_all_true_corresponding(P, [A | As], [B | Bs]) :-
	impure P(A, B),
	impure any_list.impure_all_true_corresponding(P, As, Bs).
any_list.impure_all_true_corresponding(_, [], [_ | _]) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").
any_list.impure_all_true_corresponding(_, [_ | _], []) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 27/04/99
%       Function forms added.

any_list.det_head([]) = _ :-
    error("any_list.det_head/1: empty list as argument").
any_list.det_head([X | _]) = X.

any_list.det_tail([]) = _ :-
    error("any_list.det_tail/1: empty list as argument").
any_list.det_tail([_ | Xs]) = Xs.

any_list.head([X | _]) = X.

any_list.tail([_ | Xs]) = Xs.

any_list.map(F, Xs) = Ys :-
    P = ( pred(X::ia, Y::oa) is det :- Y = F(X) ),
    any_list.map(P, Xs, Ys).

any_list.foldl(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_list.foldl(P, Xs, A, B).

any_list.foldr(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_list.foldr(P, Xs, A, B).

any_list.(L1 ++ L2) = L3 :-
    any_list.append(L1, L2, L3).

%-----------------------------------------------------------------------------%

any_list.sort(P, L0, L) :-
    any_list.length(L0, N),
    ( N = 0 ->
        L = []
    ; promise_pure
      ( any_list.hosort(P, N, L0, L1, []) ->
        L = L1
    ;
        error("hosort failed")
      )
    ).


    % any_list.hosort is actually det but the compiler can't confirm it.
    %
:- pred any_list.hosort(pred(T, T, comparison_result)::
    (pred(ia, ia, out) is det), int::in, list(T)::ia, list(T)::oa, list(T)::oa)
    is semidet.

    % any_list.hosort is a Mercury implementation of the mergesort
    % described in The Craft of Prolog.
    % N denotes the length of the part of L0 that this call is sorting.
    % (require((length(L0, M), M >= N)))
    % Since we have redundant information about the list (N, and the
    % length implicit in the list itself), we get a semidet unification
    % when we deconstruct the any_list.
any_list.hosort(P, N, L0, L, Rest) :-
    ( N = 1 ->
        L0 = [X | Rest],
        L = [X]
    ; N = 2 ->
        L0 = [X, Y | Rest],
        P(X, Y, C),
        (
            (C = (<) ; C = (=)),
            L = [X, Y]
        ;
            C = (>),
            L = [Y, X]
        )
    ;
        N1 = N // 2,
        any_list.hosort(P, N1, L0, L1, Middle),
        N2 = N - N1,
        any_list.hosort(P, N2, Middle, L2, Rest),
        any_list.merge(P, L1, L2, L)
    ).


any_list.merge(_P, [], [], []).
any_list.merge(_P, [], [Y | Ys], [Y | Ys]).
any_list.merge(_P, [X | Xs], [], [X | Xs]).
any_list.merge(P, [H1 | T1], [H2 | T2], L) :-
    P(H1, H2, R),
    ( R = (>) ->
        L = [H2 | T],
        any_list.merge(P, [H1 | T1], T2, T)
    ;
        L = [H1 | T],
        any_list.merge(P, T1, [H2 | T2], T)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
