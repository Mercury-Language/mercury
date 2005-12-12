%---------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
% any_list.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Started Wed Aug 10 18:17:22 EST 2005
%
% A version of the list module adapted for lists whose members have
% inst `any' (typically [types containing] solver types).
%
%---------------------------------------------------------------------------%

:- module any_list.
:- interface.
:- import_module int.



:- type any_list(T) ---> [] ; [T | any_list(T)].

%-----------------------------------------------------------------------------%

:- pred is_empty(any_list(T)::ia) is semidet.

:- pred is_not_empty(any_list(T)::ia) is semidet.

:- func cons(T, any_list(T)) = any_list(T).

    % Standard append predicate:
    % append(Start, End, List) is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- pred append(any_list(T), any_list(T), any_list(T)).
:- mode append(ia, ia, oa) is det.
:- mode append(oa, oa, ia) is multi.

    % L1 ++ L2 = L :- append(L1, L2, L).
    %
:- func any_list(T) ++ any_list(T) = any_list(T).
:- mode ia ++ ia = oa is det.

    % member(Elem, List) :
    %   True iff `List' contains `Elem'.
    %
:- pred member(T::oa, any_list(T)::ia) is nondet.

    % member(Elem, List, SubList) :
    %   True iff `List' contains `Elem', and `SubList' is
    %   a suffix of `List' beginning with `Elem'.
    %   Same as `SubList = [Elem | _], append(_, SubList, List)'.
    %
:- pred member(T::oa, any_list(T)::ia, any_list(T)::oa) is nondet.

    % length(List, Length) :
    %   True iff `Length' is the length of `List', i.e. if
    %   `List' contains `Length' elements.
    %
:- func length(any_list(T)::ia) = (int::out) is det.

    % same_length(ListA, ListB) :
    %   True iff `ListA' and `ListB' have the same length,
    %   i.e. iff they both contain the same number of elements.
    %
:- pred same_length(any_list(T1)::ia, any_list(T2)::ia) is semidet.

    % split_list(Len, List, Start, End):
    %   splits `List' into a prefix `Start' of length `Len',
    %   and a remainder `End'.
    %   See also: take, drop.
    %
:- pred split_list(int::in, any_list(T)::ia, any_list(T)::oa, any_list(T)::oa)
        is semidet.

    % take(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   Fails if `List' has less than `Len' elements.
    %   See also: split_list.
    %
:- pred take(int::in, any_list(T)::ia, any_list(T)::oa) is semidet.

    % take_upto(Len, List, Start):
    %   `Start' is the first `Len' elements of `List'.
    %   If `List' has less than `Len' elements, return the entire list.
    %
:- func take_upto(int::in, any_list(T)::ia) = (any_list(T)::oa) is det.

    % drop(Len, List) = End:
    %   `End' is the remainder of `List' after removing the
    %   first `Len' elements.
    %   See also: split_list.
    %
:- func drop(int::in, any_list(T)::ia) = (any_list(T)::oa) is semidet.

    % insert(Elem, List0, List):
    %   `List' is the result of inserting `Elem' somewhere in `List0'.
    %   Same as `delete(List, Elem, List0)'.
    %
:- pred insert(T, any_list(T), any_list(T)).
:- mode insert(oa, oa, ia) is nondet.
:- mode insert(ia, ia, oa) is multi.

    % list__delete(List, Elem, Remainder):
    %   True iff `Elem' occurs in `List', and
    %   `Remainder' is the result of deleting one occurrence of
    %   `Elem' from `List'.
    %
:- pred delete(any_list(T)::ia, T::oa, any_list(T)::oa) is nondet.

    % replace_nth(List0, N, R) = List is true iff List is List0
    % with Nth element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func replace_nth(any_list(T)::ia, int::in, T::ia) = (any_list(T)::oa)
    is semidet.

    % replace_nth_det(List0, N, R, List) is true iff List is List0
    % with Nth element replaced with R.
    % Aborts if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func replace_nth_det(any_list(T)::ia, int::in, T::ia) = (any_list(T)::oa)
    is det.

    % reverse(List) = Reverse:
    %   `Reverse' is a list containing the same elements as `List'
    %   but in reverse order.
    %
:- func reverse(any_list(T)::ia) = (any_list(T)::oa) is det.

    % perm(List0, List):
    %   True iff `List' is a permutation of `List0'.
    %
:- pred perm(any_list(T)::ia, any_list(T)::oa) is multi.

    % index*(List, Position, Elem):
    %   These predicates select an element in a list from it's
    %   position.  The `index0' preds consider the first element
    %   element to be element number zero, whereas the `index1' preds
    %   consider the first element to be element number one.
    %   The `_det' preds call error/1 if the index is out of
    %   range, whereas the semidet preds fail if the index is out of
    %   range.
    %
:- func index0_det(any_list(T)::ia, int::in) = (T::oa) is det.
:- func index1_det(any_list(T)::ia, int::in) = (T::oa) is det.

    % zip(ListA, ListB) = List:
    %   List is the result of alternating the elements
    %   of ListA and ListB, starting with the first element
    %   of ListA (followed by the first element of ListB,
    %   then the second element of listA, then the second
    %   element of ListB, etc.).  When there are no more
    %   elements remaining in one of the lists,
    %   the remainder of the nonempty list is appended.
    %
:- func zip(any_list(T)::ia, any_list(T)::ia) = (any_list(T)::oa) is det.

    % duplicate(Count, Elem) = List is true iff List is a list
    % containing Count duplicate copies of Elem.
    %
:- func duplicate(int::in, T::ia) = (any_list(T)::oa) is det.

    % condense(ListOfLists) = List:
    %   `List' is the result of concatenating all the
    %   elements of `ListOfLists'.
    %
:- func condense(any_list(any_list(T))::ia) = (any_list(T)::oa) is det.

    % chunk(List, ChunkSize, Chunks):
    %   Takes a list `List' and breaks it into a list of lists
    %   `Chunks', such that the length of each list in `Chunks'
    %   is at most `ChunkSize.  (More precisely, the length of
    %   each list in `Chunks' other than the last one is exactly
    %   `ChunkSize', and the length of the last list in `Chunks'
    %   is between one and `ChunkSize'.)
    %
:- func chunk(any_list(T)::ia, int::in) = (any_list(any_list(T))::oa) is det.

    % all_same(List) is true
    %   if all elements of the list are the same
    %
:- pred all_same(any_list(T)::ia) is semidet.

    % last(List, Last) is true
    %   if Last is the last element of List.
    %
:- pred last(any_list(T)::ia, T::oa) is semidet.

    % A deterministic version of last, which aborts instead of
    % failing if the input list is empty.
    %
:- func det_last(any_list(T)::ia) = (T::oa) is det.

    % split_last(List, AllButLast, Last) is true
    %   if Last is the last element of List and AllButLast is the list
    %   of elements before it.
    %
:- pred split_last(any_list(T)::ia, any_list(T)::oa, T::oa) is semidet.

    % A deterministic version of split_last, which aborts instead of
    % failing if the input list is empty.
    %
:- pred split_last_det(any_list(T)::ia, any_list(T)::oa, T::oa) is det.

%-----------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%-----------------------------------------------------------------------------%

    % map(T, L, M) uses the closure T
    % to transform the elements of L into the elements of M.
:- pred map(pred(X, Y), any_list(X), any_list(Y)).
:- mode map(pred(ia, oa) is det, ia, oa) is det.
:- mode map(pred(ia, oa) is cc_multi, ia, oa) is cc_multi.
:- mode map(pred(ia, oa) is semidet, ia, oa) is semidet.
:- mode map(pred(ia, oa) is multi, ia, oa) is multi.
:- mode map(pred(ia, oa) is nondet, ia, oa) is nondet.
:- mode map(pred(ia, ia) is semidet, ia, ia) is semidet.

:- func map(func(X) = Y, any_list(X)) = any_list(Y).
:- mode map(func(ia) = oa is det, ia) = oa is det.

    % map2(T, L, M1, M2) uses the closure T
    % to transform the elements of L into the elements of M1 and M2.
:- pred map2(pred(A, B, C), any_list(A), any_list(B), any_list(C)).
:- mode map2(pred(ia, oa, oa) is det, ia, oa, oa) is det.
:- mode map2(pred(ia, oa, oa) is cc_multi, ia, oa, oa) is cc_multi.
:- mode map2(pred(ia, oa, oa) is semidet, ia, oa, oa) is semidet.
:- mode map2(pred(ia, oa, oa) is multi, ia, oa, oa) is multi.
:- mode map2(pred(ia, oa, oa) is nondet, ia, oa, oa) is nondet.
:- mode map2(pred(ia, ia, ia) is semidet, ia, ia, ia) is semidet.

    % map3(T, L, M1, M2, M3) uses the closure T
    % to transform the elements of L into the elements of M1, M2 and M3.
:- pred map3(pred(A, B, C, D), any_list(A), any_list(B), any_list(C),
    any_list(D)).
:- mode map3(pred(ia, oa, oa, oa) is det, ia, oa, oa, oa) is det.
:- mode map3(pred(ia, oa, oa, oa) is cc_multi, ia, oa, oa, oa) is cc_multi.
:- mode map3(pred(ia, oa, oa, oa) is semidet, ia, oa, oa, oa) is semidet.
:- mode map3(pred(ia, oa, oa, oa) is multi, ia, oa, oa, oa) is multi.
:- mode map3(pred(ia, oa, oa, oa) is nondet, ia, oa, oa, oa) is nondet.
:- mode map3(pred(ia, ia, ia, ia) is semidet, ia, ia, ia, ia) is semidet.

    % map4(T, L, M1, M2, M3, M4) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3 and 
    % M4.
:- pred map4(pred(A, B, C, D, E), any_list(A), any_list(B), any_list(C),
    any_list(D), any_list(E)).
:- mode map4(pred(ia, oa, oa, oa, oa) is det, ia, oa, oa, oa, oa)
        is det.
:- mode map4(pred(ia, oa, oa, oa, oa) is cc_multi, ia, oa, oa, oa, oa)
        is cc_multi.
:- mode map4(pred(ia, oa, oa, oa, oa) is semidet, ia, oa, oa, oa, oa)
        is semidet.
:- mode map4(pred(ia, oa, oa, oa, oa) is multi, ia, oa, oa, oa, oa)
        is multi.
:- mode map4(pred(ia, oa, oa, oa, oa) is nondet, ia, oa, oa, oa, oa)
        is nondet.
:- mode map4(pred(ia, ia, ia, ia, ia) is semidet, ia, ia, ia, ia, ia)
        is semidet.

    % map5(T, L, M1, M2, M3, M4, M5) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4 
    % and M5.
:- pred map5(pred(A, B, C, D, E, F), any_list(A), any_list(B), any_list(C),
    any_list(D), any_list(E), any_list(F)).
:- mode map5(pred(ia, oa, oa, oa, oa, oa) is det, ia, oa, oa, oa,
    oa, oa) is det.
:- mode map5(pred(ia, oa, oa, oa, oa, oa) is cc_multi, ia, oa, oa,
    oa, oa, oa) is cc_multi.
:- mode map5(pred(ia, oa, oa, oa, oa, oa) is semidet, ia, oa, oa, 
    oa, oa, oa) is semidet.
:- mode map5(pred(ia, oa, oa, oa, oa, oa) is multi, ia, oa, oa, 
    oa, oa, oa) is multi.
:- mode map5(pred(ia, oa, oa, oa, oa, oa) is nondet, ia, oa, oa, 
    oa, oa, oa) is nondet.
:- mode map5(pred(ia, ia, ia, ia, ia, ia) is semidet, ia, ia, ia,
    ia, ia, ia) is semidet.

    % map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5 and M6.
:- pred map6(pred(A, B, C, D, E, F, G), any_list(A), any_list(B), any_list(C), 
    any_list(D), any_list(E), any_list(F), any_list(G)).
:- mode map6(pred(ia, oa, oa, oa, oa, oa, oa) is det, ia, oa, oa, 
    oa, oa, oa, oa) is det.
:- mode map6(pred(ia, oa, oa, oa, oa, oa, oa) is cc_multi, ia, oa,
    oa, oa, oa, oa, oa) is cc_multi.
:- mode map6(pred(ia, oa, oa, oa, oa, oa, oa) is semidet, ia, oa, 
    oa, oa, oa, oa, oa) is semidet.
:- mode map6(pred(ia, oa, oa, oa, oa, oa, oa) is multi, ia, oa, 
    oa, oa, oa, oa, oa) is multi.
:- mode map6(pred(ia, oa, oa, oa, oa, oa, oa) is nondet, ia, oa, 
    oa, oa, oa, oa, oa) is nondet.
:- mode map6(pred(ia, ia, ia, ia, ia, ia, ia) is semidet, ia, ia,
    ia, ia, ia, ia, ia) is semidet.

    % map7(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5, M6 and M7.
:- pred map7(pred(A, B, C, D, E, F, G, H), any_list(A), any_list(B),
    any_list(C), any_list(D), any_list(E), any_list(F), any_list(G),
    any_list(H)).
:- mode map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is det,
    ia, oa, oa, oa, oa, oa, oa, oa) is det.
:- mode map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is cc_multi,
    ia, oa, oa, oa, oa, oa, oa, oa) is cc_multi.
:- mode map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is semidet,
    ia, oa, oa, oa, oa, oa, oa, oa) is semidet.
:- mode map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is multi,
    ia, oa, oa, oa, oa, oa, oa, oa) is multi.
:- mode map7(pred(ia, oa, oa, oa, oa, oa, oa, oa) is nondet,
    ia, oa, oa, oa, oa, oa, oa, oa) is nondet.
:- mode map7(pred(ia, ia, ia, ia, ia, ia, ia, ia) is semidet,
    ia, ia, ia, ia, ia, ia, ia, ia) is semidet.

    % map8(T, L, M1, M2, M3, M4, M5, M6, M7) uses the closure T
    % to transform the elements of L into the elements of M1, M2, M3, M4, 
    % M5, M6, M7 and M8.
:- pred map8(pred(A, B, C, D, E, F, G, H, I), any_list(A), any_list(B),
    any_list(C), any_list(D), any_list(E), any_list(F), any_list(G),
    any_list(H), any_list(I)).
:- mode map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is det,
    ia, oa, oa, oa, oa, oa, oa, oa, oa) is det.
:- mode map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is cc_multi,
    ia, oa, oa, oa, oa, oa, oa, oa, oa) is cc_multi.
:- mode map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is semidet,
    ia, oa, oa, oa, oa, oa, oa, oa, oa) is semidet.
:- mode map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is multi,
    ia, oa, oa, oa, oa, oa, oa, oa, oa) is multi.
:- mode map8(pred(ia, oa, oa, oa, oa, oa, oa, oa, oa) is nondet,
    ia, oa, oa, oa, oa, oa, oa, oa, oa) is nondet.
:- mode map8(pred(ia, ia, ia, ia, ia, ia, ia, ia, ia) is semidet,
    ia, ia, ia, ia, ia, ia, ia, ia, ia) is semidet.

    % map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
    %   [F(A1, B1), .., F(An, Bn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func map_corresponding(func(A, B) = C, any_list(A), any_list(B)) =
    any_list(C).
:- mode map_corresponding(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func map_corresponding3(func(A, B, C) = D, any_list(A), any_list(B),
    any_list(C)) = any_list(D).
:- mode map_corresponding3(func(ia, ia, ia) = oa is det, ia, ia, ia) = oa
    is det.

    % foldl(Pred, List, Start, End) calls Pred with each
    % element of List (working left-to-right) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    %
:- pred foldl(pred(L, A, A), any_list(L), A, A).
:- mode foldl(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode foldl(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldl(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode foldl(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode foldl(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode foldl(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func foldl(func(L, A) = A, any_list(L), A) = A.
:- mode foldl(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % foldr(Pred, List, Start, End) calls Pred with each
    % element of List (working right-to-left) and an accumulator
    % (with the initial value of Start), and returns the final
    % value in End.
    % 
:- pred foldr(pred(L, A, A), any_list(L), A, A).
:- mode foldr(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode foldr(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldr(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode foldr(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode foldr(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode foldr(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func foldr(func(L, A) = A, any_list(L), A) = A.
:- mode foldr(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % foldl2(Pred, List, !Acc1, !Acc2)
    % Does the same job as foldl, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred foldl2(pred(L, A, A, Z, Z), any_list(L), A, A, Z, Z).
:- mode foldl2(pred(ia, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa) is det.
:- mode foldl2(pred(ia, ia, oa, ia, oa) is cc_multi,
    ia, ia, oa, ia, oa) is cc_multi.
:- mode foldl2(pred(ia, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa) is semidet.
:- mode foldl2(pred(ia, ia, oa, ia, oa) is nondet,
    ia, ia, oa, ia, oa) is nondet.
:- mode foldl2(pred(ia, ia, oa, mdi, muo) is det,
    ia, ia, oa, mdi, muo) is det.
:- mode foldl2(pred(ia, ia, oa, di, uo) is det,
    ia, ia, oa, di, uo) is det.
:- mode foldl2(pred(ia, di, uo, di, uo) is det,
    ia, di, uo, di, uo) is det.
:- mode foldl2(pred(ia, ia, oa, mdi, muo) is cc_multi,
    ia, ia, oa, mdi, muo) is cc_multi.
:- mode foldl2(pred(ia, ia, oa, di, uo) is cc_multi,
    ia, ia, oa, di, uo) is cc_multi.
:- mode foldl2(pred(ia, di, uo, di, uo) is cc_multi,
    ia, di, uo, di, uo) is cc_multi.

    % foldl3(Pred, List, !Acc1, !Acc2, !Acc3)
    % Does the same job as foldl, but with three accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred foldl3(pred(L, A, A, B, B, C, C), any_list(L),
    A, A, B, B, C, C).
:- mode foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is cc_multi,
    ia, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa, ia, oa) is semidet.
:- mode foldl3(pred(ia, ia, oa, ia, oa, ia, oa) is nondet,
    ia, ia, oa, ia, oa, ia, oa) is nondet.
:- mode foldl3(pred(ia, ia, oa, ia, oa, di, uo) is det,
    ia, ia, oa, ia, oa, di, uo) is det.
:- mode foldl3(pred(ia, ia, oa, ia, oa, di, uo) is cc_multi,
    ia, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4)
    % Does the same job as foldl, but with four accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred foldl4(pred(L, A, A, B, B, C, C, D, D), any_list(L),
    A, A, B, B, C, C, D, D).
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa) is nondet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, di, uo) is det,
    ia, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode foldl4(pred(ia, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5)
    % Does the same job as foldl, but with five accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    % 
:- pred foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), any_list(L),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is det,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is semidet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is nondet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
    is det,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode foldl5(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
    is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6)
    % Does the same job as foldl, but with six accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F), any_list(L),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is det,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is semidet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is nondet,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    di, uo) is det,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode foldl6(pred(ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    di, uo) is cc_multi,
    ia, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.

    % foldl_corresponding(F, As, Bs, !Acc)
    % Does the same job as foldl, but works on two lists ia
    % parallel.  An exception is raised if the list arguments differ
    % in length.
    %
:- pred foldl_corresponding(pred(A, B, C, C), any_list(A), any_list(B), C, C).
:- mode foldl_corresponding(pred(ia, ia, ia, oa) is det,
    ia, ia, ia, oa) is det.
:- mode foldl_corresponding(pred(ia, ia, ia, oa) is cc_multi,
    ia, ia, ia, oa) is cc_multi.
:- mode foldl_corresponding(pred(ia, ia, ia, oa) is semidet,
    ia, ia, ia, oa) is semidet.
:- mode foldl_corresponding(pred(ia, ia, ia, oa) is nondet,
    ia, ia, ia, oa) is nondet.
:- mode foldl_corresponding(pred(ia, ia, di, uo) is det,
    ia, ia, di, uo) is det.
:- mode foldl_corresponding(pred(ia, ia, di, uo) is cc_multi,
    ia, ia, di, uo) is cc_multi.

    % foldl2_corresponding(F, As, Bs, !Acc1, !Acc2)
    % Does the same job as foldl_corresponding, but has two
    % accumulators.
    %
:- pred foldl2_corresponding(pred(A, B, C, C, D, D), any_list(A), any_list(B),
    C, C, D, D).
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is det,
    ia, ia, ia, oa, ia, oa) is det.
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is cc_multi,
    ia, ia, ia, oa, ia, oa) is cc_multi.
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is semidet,
    ia, ia, ia, oa, ia, oa) is semidet.
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, ia, oa) is nondet,
    ia, ia, ia, oa, ia, oa) is nondet.
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, di, uo) is det,
    ia, ia, ia, oa, di, uo) is det.
:- mode foldl2_corresponding(pred(ia, ia, ia, oa, di, uo) is cc_multi,
    ia, ia, ia, oa, di, uo) is cc_multi.

    % map_foldl(Pred, InList, OutList, Start, End) calls Pred
    % with an accumulator (with the initial value of Start) on
    % each element of InList (working left-to-right) to transform
    % InList into OutList.  The final value of the accumulator is
    % returned in End.
    % 
:- pred map_foldl(pred(L, M, A, A), any_list(L), any_list(M), A, A).
:- mode map_foldl(pred(ia, oa, di, uo) is det, ia, oa, di, uo)
    is det.
:- mode map_foldl(pred(ia, oa, ia, oa) is det, ia, oa, ia, oa)
    is det.
:- mode map_foldl(pred(ia, oa, di, uo) is cc_multi, ia, oa, di, uo)
    is cc_multi.
:- mode map_foldl(pred(ia, oa, ia, oa) is cc_multi, ia, oa, ia, oa)
    is cc_multi.
:- mode map_foldl(pred(ia, oa, ia, oa) is semidet, ia, oa, ia, oa)
    is semidet.
:- mode map_foldl(pred(ia, oa, ia, oa) is nondet, ia, oa, ia, oa)
    is nondet.

    % Same as map_foldl, but with two mapped outputs.
    % 
:- pred map2_foldl(pred(L, M, N, A, A), any_list(L), any_list(M), any_list(N),
    A, A).
:- mode map2_foldl(pred(ia, oa, oa, di, uo) is det, ia, oa, oa,
    di, uo) is det.
:- mode map2_foldl(pred(ia, oa, oa, ia, oa) is det, ia, oa, oa,
    ia, oa) is det.
:- mode map2_foldl(pred(ia, oa, oa, di, uo) is cc_multi, ia, oa, oa,
    di, uo) is cc_multi.
:- mode map2_foldl(pred(ia, oa, oa, ia, oa) is cc_multi, ia, oa, oa,
    ia, oa) is cc_multi.
:- mode map2_foldl(pred(ia, oa, oa, ia, oa) is semidet, ia, oa, oa,
    ia, oa) is semidet.
:- mode map2_foldl(pred(ia, oa, oa, ia, oa) is nondet, ia, oa, oa,
    ia, oa) is nondet.

    % Same as map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(L, M, A, A, B, B), any_list(L), any_list(M),
    A, A, B, B).
:- mode map_foldl2(pred(ia, oa, ia, oa, di, uo) is det,
    ia, oa, ia, oa, di, uo) is det.
:- mode map_foldl2(pred(ia, oa, ia, oa, ia, oa) is det,
    ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl2(pred(ia, oa, ia, oa, di, uo) is cc_multi,
    ia, oa, ia, oa, di, uo) is cc_multi.
:- mode map_foldl2(pred(ia, oa, ia, oa, ia, oa) is cc_multi,
    ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode map_foldl2(pred(ia, oa, ia, oa, ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa) is semidet.
:- mode map_foldl2(pred(ia, oa, ia, oa, ia, oa) is nondet,
    ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with three accumulators.
    %
:- pred map_foldl3(pred(L, M, A, A, B, B, C, C), any_list(L), any_list(M),
    A, A, B, B, C, C).
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, di, uo) is det,
    ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is det,
    ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, di, uo) is cc_multi,
    ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode map_foldl3(pred(ia, oa, ia, oa, ia, oa, ia, oa) is nondet,
    ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with four accumulators.
    %
:- pred map_foldl4(pred(L, M, A, A, B, B, C, C, D, D),
    any_list(L), any_list(M), A, A, B, B, C, C, D, D).
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
    is det,
    ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is det,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
    is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is semidet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode map_foldl4(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is nondet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with five accumulators.
    %
:- pred map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
    any_list(L), any_list(M), A, A, B, B, C, C, D, D, E, E).
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    di, uo) is det,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is det,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    di, uo) is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is cc_multi.
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is cc_multi.
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is semidet.
:- mode map_foldl5(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa) is nondet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is nondet.

    % Same as map_foldl, but with six accumulators.
    %
:- pred map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
    any_list(L), any_list(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, di, uo) is det,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo) is det.
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, ia, oa) is det,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, di, uo) is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, di, uo)
    is cc_multi.
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, ia, oa) is cc_multi,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is cc_multi.
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is semidet.
:- mode map_foldl6(pred(ia, oa, ia, oa, ia, oa, ia, oa, ia, oa,
    ia, oa, ia, oa) is nondet,
    ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa, ia, oa)
    is nondet.

    % all_true(Pred, List) takes a closure with one input argument.
    % If Pred succeeds for every member of List, all_true succeeds.
    % If Pred fails for any member of List, all_true fails.
    %
:- pred all_true(pred(X)::(pred(ia) is semidet), any_list(X)::ia) is semidet.

%-----------------------------------------------------------------------------%

:- func head(any_list(T)::ia) = (T::oa) is semidet.

:- func tail(any_list(T)::ia) = (any_list(T)::oa) is semidet.

    % det_head(List) returns the first element of List,
    % calling error/1 if List is empty.
    %
:- func det_head(any_list(T)::ia) = (T::oa) is det.

    % det_tail(List) returns the tail of List,
    % calling error/1 if List is empty.
    %
:- func det_tail(any_list(T)::ia) = (any_list(T)::oa) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module set_tree234.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

is_empty([]).

is_not_empty([_ | _]).

cons(H, T) = [H | T].

append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :-
    append(Xs, Ys, Zs).

%-----------------------------------------------------------------------------%

index0_det(List, N) = Elem :-
    promise_pure (
        impure index0(List, N, Elem0)
    ->
        Elem = Elem0
    ;
        error("index: index out of range")
    ).

:- pred index0(any_list(T)::ia, int::in, T::oa) is semidet.

index0([X | Xs], N, Y) :-
    ( if N = 0 then Y = X else index0(Xs, N - 1, Y) ).

index1_det(List, N) = index0_det(List, N - 1).

%-----------------------------------------------------------------------------%

condense([])       = [].
condense([L | Ls]) = L ++ condense(Ls).

%-----------------------------------------------------------------------------%

same_length([], []).
same_length([_ | L1], [_ | L2]) :-
    same_length(L1, L2).

%-----------------------------------------------------------------------------%

insert(X, Ys,       [X | Ys]).
insert(X, [Y | Ys], [Y | Zs]) :-
    insert(X, Ys, Zs).

%-----------------------------------------------------------------------------%

delete(List, Elem, Remainder) :-
    insert(Elem, Remainder, List).

%-----------------------------------------------------------------------------%

replace_nth(Xs, P, R) = L :-
    P > 0,
    replace_nth_2(Xs, P, R, L).

replace_nth_det(Xs, P, R) = L :-
    promise_pure (
        P > 0
    ->
        ( impure replace_nth_2(Xs, P, R, L0) ->
            L = L0
        ;
            error("replace_nth_det: " ++
                "Can't replace element whose index " ++
                "position is past the end of the list")
        )
    ;
        error("replace_nth_det: " ++
            "Can't replace element whose index " ++
            "position is less than 1.")
    ).

:- pred replace_nth_2(any_list(T)::ia, int::in, T::ia, any_list(T)::oa)
    is semidet.

replace_nth_2([X | Xs], P, R, L) :-
    ( P = 1 ->
        L = [R | Xs]
    ;
        replace_nth_2(Xs, P - 1, R, L0),
        L = [X | L0]
    ).

%-----------------------------------------------------------------------------%

member(X, [X | _]).
member(X, [_ | Xs]) :-
    member(X, Xs).

member(Element, List, SubList) :-
    SubList = [Element | _],
    append(_, SubList, List).

%-----------------------------------------------------------------------------%

% Note - it is not possible to write a version of
% length/1 in pure Mercury that works in both directions
% unless you make it semidet rather than det.

length(L) = N :-
    length_2(L, 0, N).

:- pred length_2(any_list(T)::ia, int::in, int::out) is det.

length_2([], N, N).
length_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    length_2(L1, N1, N).

%-----------------------------------------------------------------------------%

reverse(L0) = L :-
    reverse_2(L0, [], L).

:- pred reverse_2(any_list(T)::ia, any_list(T)::ia, any_list(T)::oa) is det.

reverse_2([], L, L).
reverse_2([X | Xs], L0, L) :-
    reverse_2(Xs, [X | L0], L).

%-----------------------------------------------------------------------------%

zip([], Bs) = Bs.
zip([A | As], Bs) = [A | Cs] :-
    zip2(As, Bs, Cs).

:- pred zip2(any_list(T)::ia, any_list(T)::ia, any_list(T)::oa) is det.

zip2(As, [], As).
zip2(As, [B | Bs], [B | Cs]) :-
    zip2(As, Bs, Cs).

%-----------------------------------------------------------------------------%

split_list(N, List, Start, End) :-
    ( N = 0 ->
        Start = [],
        End = List
    ;
        N > 0,
        List = [Head | List1],
        Start = [Head | Start1],
        split_list(N - 1, List1, Start1, End)
    ).

take(N, As, Bs) :-
    ( N > 0 ->
        As = [A | As1],
        take(N - 1, As1, Bs1),
        Bs = [A | Bs1]
    ;
        Bs = []
    ).

take_upto(N, As) = Bs :-
    promise_pure (
        impure take(N, As, Bs0)
    ->
        Bs = Bs0
    ;
        Bs = As
    ).

drop(N, As) = Bs :-
    ( N > 0 ->
        As = [_ | Cs],
        Bs = drop(N - 1, Cs)
    ;
        As = Bs
    ).

%-----------------------------------------------------------------------------%

duplicate(N, X) = duplicate(N, X, []).

:- func duplicate(int::in, T::ia, any_list(T)::ia) = (any_list(T)::oa) is det.

duplicate(N, X, Xs) =
    ( N > 0 ->
        duplicate(N-1, X, [X|Xs])
    ;
        Xs
    ).

%-----------------------------------------------------------------------------%

chunk(List, ChunkSize) = ListOfSmallLists :-
    chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred chunk_2(any_list(T)::ia, int::in, any_list(T)::ia, int::in,
    any_list(any_list(T))::oa) is det.

chunk_2([], _ChunkSize, List0, _N, Lists) :-
    ( List0 = [] ->
        Lists = []
    ;
        List  = reverse(List0),
        Lists = [List]
    ).
chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
    ( N > 1 ->
        chunk_2(Xs, ChunkSize, [X | List0], N - 1, Lists)
    ;
        List = reverse([X | List0]),
        chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1),
        Lists = [List | Lists1]
    ).

%-----------------------------------------------------------------------------%

perm([], []).
perm([X | Xs], Ys) :-
    perm(Xs, Ys0),
    insert(X, Ys0, Ys).

%-----------------------------------------------------------------------------%

all_same([]).
all_same([H | T]) :-
    all_same_2(H, T).

:- pred all_same_2(T::ia, any_list(T)::ia) is semidet.

all_same_2(_, []).
all_same_2(H, [H | T]) :-
    all_same_2(H, T).

%-----------------------------------------------------------------------------%

last([H | T], Last) :-
    (
        T = [],
        Last = H
    ;
        T = [_ | _],
        last(T, Last)
    ).

det_last(List) = Last :-
    promise_pure (
        impure last(List, LastPrime)
    ->
        Last = LastPrime
    ;
        error("last_det: empty list")
    ).

split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [_ | _],
        split_last(T, AllButLast1, Last),
        AllButLast = [H | AllButLast1]
    ).

split_last_det(List, AllButLast, Last) :-
    promise_pure (
        impure split_last(List, AllButLastPrime, LastPrime)
    ->
        AllButLast = AllButLastPrime,
        Last = LastPrime
    ;
        error("split_last_det: empty list")
    ).

%-----------------------------------------------------------------------------%

map(_, [],  []).
map(P, [H0 | T0], [H | T]) :-
    call(P, H0, H),
    map(P, T0, T).

map2(_, [],  [],  []).
map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
    call(P, H0, H1, H2),
    map2(P, T0, T1, T2).

map3(_, [],  [],  [],  []).
map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
    call(P, H0, H1, H2, H3),
    map3(P, T0, T1, T2, T3).

map4(_, [], [], [], [], []).
map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
    call(P, H0, H1, H2, H3, H4),
    map4(P, T0, T1, T2, T3, T4).

map5(_, [], [], [], [], [], []).
map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5])
        :-
    call(P, H0, H1, H2, H3, H4, H5),
    map5(P, T0, T1, T2, T3, T4, T5).

map6(_, [], [], [], [], [], [], []).
map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6),
    map6(P, T0, T1, T2, T3, T4, T5, T6).

map7(_, [], [], [], [], [], [], [], []).
map7(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6, H7),
    map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

map8(_, [], [], [], [], [], [], [], [], []).
map8(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
        [H6 | T6], [H7 | T7], [H8 | T8]) :-
    call(P, H0, H1, H2, H3, H4, H5, H6, H7, H8),
    map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

map_corresponding(_, [], []) = [].
map_corresponding(_, [], [_ | _]) =
    func_error("map_corresponding/3: mismatched list arguments").
map_corresponding(_, [_ | _], []) =
    func_error("map_corresponding/3: mismatched list arguments").
map_corresponding(F, [A | As], [B | Bs]) =
    [F(A, B) | map_corresponding(F, As, Bs)].

map_corresponding3(F, As, Bs, Cs) =
    (
        As = [A | As0],
        Bs = [B | Bs0],
        Cs = [C | Cs0]
    ->
        [F(A, B, C) | map_corresponding3(F, As0, Bs0, Cs0)]
    ;
        As = [],
        Bs = [],
        Cs = []
    ->
        []
    ;
        func_error("map_corresponding3: " ++
            "mismatched list arguments")
    ).

foldl(_, [], !A).
foldl(P, [H | T], !A) :-
    call(P, H, !A),
    foldl(P, T, !A).

foldl2(_, [], !A, !B).
foldl2(P, [H | T], !A, !B) :-
    call(P, H, !A, !B),
    foldl2(P, T, !A, !B).

foldl3(_, [], !A, !B, !C).
foldl3(P, [H | T], !A, !B, !C) :-
    call(P, H, !A, !B, !C),
    foldl3(P, T, !A, !B, !C).

foldl4(_, [], !A, !B, !C, !D).
foldl4(P, [H | T], !A, !B, !C, !D) :-
    call(P, H, !A, !B, !C, !D),
    foldl4(P, T, !A, !B, !C, !D).

foldl5(_, [], !A, !B, !C, !D, !E).
foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
    call(P, H, !A, !B, !C, !D, !E),
    foldl5(P, T, !A, !B, !C, !D, !E).

foldl6(_, [], !A, !B, !C, !D, !E, !F).
foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
    call(P, H, !A, !B, !C, !D, !E, !F),
    foldl6(P, T, !A, !B, !C, !D, !E, !F).

foldl_corresponding(_, [], [], !Acc).
foldl_corresponding(_, [], [_ | _], _, _) :-
    error("foldl_corresponding/5: mismatched list arguments").
foldl_corresponding(_, [_ | _], [], _, _) :-
    error("foldl_corresponding/5: mismatched list arguments").
foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    P(A, B, !Acc),
    foldl_corresponding(P, As, Bs, !Acc).

foldl2_corresponding(_, [], [], !Acc1, !Acc2).
foldl2_corresponding(_, [], [_ | _], _, _, _, _) :-
    error("foldl2_corresponding/7: mismatched list arguments").
foldl2_corresponding(_, [_ | _], [], _, _, _, _) :-
    error("foldl2_corresponding/7: mismatched list arguments").
foldl2_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2) :-
    P(A, B, !Acc1, !Acc2),
    foldl2_corresponding(P, As, Bs, !Acc1, !Acc2).

map_foldl(_, [], [], !A).
map_foldl(P, [H0 | T0], [H | T], !A) :-
    call(P, H0, H, !A),
    map_foldl(P, T0, T, !A).

map2_foldl(_, [], [], [], !A).
map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
    call(P, H0, H1, H2, !A),
    map2_foldl(P, T0, T1, T2, !A).

map_foldl2(_, [], [], !A, !B).
map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
    call(P, H0, H, !A, !B),
    map_foldl2(P, T0, T, !A, !B).

map_foldl3(_, [], [], !A, !B, !C).
map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
    call(P, H0, H, !A, !B, !C),
    map_foldl3(P, T0, T, !A, !B, !C).

map_foldl4(_, [], [], !A, !B, !C, !D).
map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
    call(P, H0, H, !A, !B, !C, !D),
    map_foldl4(P, T0, T, !A, !B, !C, !D).

map_foldl5(_, [], [], !A, !B, !C, !D, !E).
map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
    call(P, H0, H, !A, !B, !C, !D, !E),
    map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
    call(P, H0, H, !A, !B, !C, !D, !E, !F),
    map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

foldr(_, [], !A).
foldr(P, [H | T], !A) :-
    foldr(P, T, !A),
    call(P, H, !A).

all_true(_P, []).
all_true(P, [X | Xs]) :-
    P(X),
    all_true(P, Xs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 27/04/99
%       Function forms added.

det_head([]) = _ :-
    error("det_head/1: empty list as argument").
det_head([X | _]) = X.

det_tail([]) = _ :-
    error("det_tail/1: empty list as argument").
det_tail([_ | Xs]) = Xs.

head([X | _]) = X.

tail([_ | Xs]) = Xs.

map(F, Xs) = Ys :-
    P = ( pred(X::ia, Y::oa) is det :- Y = F(X) ),
    map(P, Xs, Ys).

foldl(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    foldl(P, Xs, A, B).

foldr(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    foldr(P, Xs, A, B).

L1 ++ L2 = L3 :-
    append(L1, L2, L3).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
