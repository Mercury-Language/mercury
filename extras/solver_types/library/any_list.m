%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% any_list.m
% Ralph Becket <rafe@cs.mu.oz.au>
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

%---------------------------------------------------------------------------%

:- pred is_empty(list(T)::ia) is semidet.

:- pred is_not_empty(list(T)::ia) is semidet.

:- func cons(T, list(T)) = list(T).

    % append(L1, L2, L):
    %
    % True iff L is the result of concatenating L1 and L2.
    %
:- pred append(list(T), list(T), list(T)).
:- mode append(ia, ia, oa) is det.
:- mode append(oa, oa, ia) is multi.

:- func append(list(T), list(T)) = list(T).
:- mode append(ia, ia) = oa is det.

    % L1 ++ L2 = L iff append(L1, L2, L).
    %
:- func (list(T) ++ list(T)) = list(T).
:- mode (ia ++ ia) = oa is det.

    % member(Elem, List):
    %
    % True iff List contains Elem.
    %
:- pred member(T::oa, list(T)::ia) is nondet.

    % member(Elem, List, SubList):
    %
    % True iff List contains Elem, and SubList is a suffix of List
    % beginning with Elem. Same as
    %
    %   `SubList = [Elem | _], append(_, SubList, List)'.
    %
:- pred member(T::oa, list(T)::ia, list(T)::oa) is nondet.

    % length(List, Length):
    %
    % True iff Length is the length of List, i.e. if List contains
    % Length elements.
    %
:- pred length(list(T)::ia, int::out) is det.
:- func length(list(T)::ia) = (int::out) is det.

    % same_length(ListA, ListB):
    %
    % True iff ListA and ListB have the same length,
    % i.e. iff they both contain the same number of elements.
    %
:- pred same_length(list(T1)::ia, list(T2)::ia) is semidet.

    % split_list(Len, List, Start, End):
    %
    % Splits List into a prefix Start of length Len, and a remainder End.
    % See also: take, drop.
    %
:- pred split_list(int::in, list(T)::ia, list(T)::oa, list(T)::oa) is semidet.

    % take(Len, List, Start):
    %
    % Start is the first Len elements of List.
    % Fails if List has less than Len elements.
    % See also: split_list.
    %
:- pred take(int::in, list(T)::ia, list(T)::oa) is semidet.

    % take_upto(Len, List, Start):
    %
    % Start is the first Len elements of List.
    % If List has less than Len elements, return the entire list.
    %
:- func take_upto(int::in, list(T)::ia) = (list(T)::oa) is det.

    % drop(Len, List) = End:
    %
    % End is the remainder of List after removing the first Len elements.
    % See also: split_list.
    %
:- func drop(int::in, list(T)::ia) = (list(T)::oa) is semidet.

    % insert(Elem, List0, List):
    %
    % List is the result of inserting Elem somewhere in List0.
    % Same as `delete(List, Elem, List0)'.
    %
:- pred insert(T, list(T), list(T)).
:- mode insert(oa, oa, ia) is nondet.
:- mode insert(ia, ia, oa) is multi.

    % delete(List, Elem, Remainder):
    %
    % True iff Elem occurs in List, and Remainder is the result of
    % deleting one occurrence of Elem from List.
    %
:- pred delete(list(T)::ia, T::oa, list(T)::oa) is nondet.

    % replace_nth(List0, N, R) = List is true
    % iff List is List0 with Nth element replaced with R.
    % Fails if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func replace_nth(list(T)::ia, int::in, T::ia) = (list(T)::oa) is semidet.

    % replace_nth_det(List0, N, R, List) is true iff List is List0
    % with Nth element replaced with R.
    % Aborts if N < 1 or if length of List0 < N.
    % (Position numbers start from 1.)
    %
:- func replace_nth_det(list(T)::ia, int::in, T::ia) = (list(T)::oa) is det.

    % reverse(List) = Reverse:
    %
    % Reverse is a list containing the same elements as List
    % but in reverse order.
    %
:- func reverse(list(T)::ia) = (list(T)::oa) is det.
:- pred reverse(list(T)::ia, list(T)::oa) is det.

    % perm(List0, List):
    %
    % True iff List is a permutation of List0.
    %
:- pred perm(list(T)::ia, list(T)::oa) is multi.

    % index*(List, Position, Elem):
    %
    % These predicates select an element in a list by specifying its position.
    % The `index0' preds consider the first element element to be
    % element number zero, whereas the `index1' preds consider
    % the first element to be element number one. The `_det' preds
    % call error/1 if the index is out of range, whereas the semidet preds
    % fail if the index is out of range.
    %
:- pred index0(list(T)::ia, int::in, T::oa) is semidet.
:- pred index1(list(T)::ia, int::in, T::oa) is semidet.

:- pred index0_det(list(T)::ia, int::in, T::oa) is det.
:- pred index1_det(list(T)::ia, int::in, T::oa) is det.

:- func index0_det(list(T)::ia, int::in) = (T::oa) is det.
:- func index1_det(list(T)::ia, int::in) = (T::oa) is det.

    % zip(ListA, ListB) = List:
    %
    % List is the result of alternating the elements of ListA and ListB,
    % starting with the first element of ListA (followed by the first element
    % of ListB, then the second element of listA, then the second element
    % of ListB, etc.). When there are no more elements remaining in one
    % of the lists, the remainder of the nonempty list is appended.
    %
:- func zip(list(T)::ia, list(T)::ia) = (list(T)::oa) is det.

    % duplicate(Count, Elem) = List:
    %
    % True iff List is a list containing Count duplicate copies of Elem.
    %
:- func duplicate(int::in, T::ia) = (list(T)::oa) is det.

    % condense(ListOfLists) = List:
    %
    % List is the result of concatenating all the elements of ListOfLists.
    %
:- func condense(list(list(T))::ia) = (list(T)::oa) is det.

    % chunk(List, ChunkSize, Chunks):
    %
    % Takes a list List and breaks it into a list of lists Chunks,
    % such that the length of each list in Chunks is at most ChunkSize.
    % (More precisely, the length of each list in Chunks other than
    % the last one is exactly ChunkSize, and the length of the last list
    % in Chunks is between one and ChunkSize.)
    %
:- func chunk(list(T)::ia, int::in) = (list(list(T))::oa) is det.

    % all_same(List):
    %
    % True iff all elements of the list are the same.
    %
:- pred all_same(list(T)::ia) is semidet.

    % last(List, Last):
    %
    % True iff Last is the last element of List.
    %
:- pred last(list(T)::ia, T::oa) is semidet.

    % A deterministic version of last, which aborts instead of failing
    % if the input list is empty.
    %
:- func det_last(list(T)::ia) = (T::oa) is det.

    % split_last(List, AllButLast, Last):
    %
    % True iff Last is the last element of List and AllButLast is the list
    % of elements before it.
    %
:- pred split_last(list(T)::ia, list(T)::oa, T::oa) is semidet.

    % A deterministic version of split_last, which aborts instead of
    % failing if the input list is empty.
    %
:- pred split_last_det(list(T)::ia, list(T)::oa, T::oa) is det.

    % Transpose a matrix represented as a list of lists. All elements of the
    % input list must have the same length. The input list must be non-empty,
    % otherwise an exception is thrown.
    %
:- func transpose(list(list(T))::ia) = (list(list(T))::oa) is det.

    % As above, except that we specify the length of the resulting list.
    % The input list may be empty in which case the result is a list of
    % the required length, each of whose elements is the empty list.
    %
:- func transpose0(int::in, list(list(T))::ia) = (list(list(T))::oa) is det.

%---------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%---------------------------------------------------------------------------%

    % map(Pred, L, M):
    %
    % Applies Pred to transform the elements of L into the elements of M.
    %
:- pred map(pred(X, Y), list(X), list(Y)).
:- mode map(pred(ia, oa) is det, ia, oa) is det.
:- mode map(pred(ia, out) is det, ia, out) is det.
:- mode map(pred(ia, oa) is cc_multi, ia, oa) is cc_multi.
:- mode map(pred(ia, oa) is semidet, ia, oa) is semidet.
:- mode map(pred(ia, oa) is multi, ia, oa) is multi.
:- mode map(pred(ia, oa) is nondet, ia, oa) is nondet.
:- mode map(pred(ia, ia) is semidet, ia, ia) is semidet.

:- func map(func(X) = Y, list(X)) = list(Y).
:- mode map(func(ia) = oa is det, ia) = oa is det.

    % Same as map/2, but takes an impure function.
    %
:- impure func impure_map(impure func(X) = Y, list(X)) = list(Y).
:- mode impure_map(func(in) = (out) is det, in) = out is det.
:- mode impure_map(func(ia) = (out) is det, ia) = out is det.
:- mode impure_map(func(ia) = (oa) is det, ia) = oa is det.
:- mode impure_map(func(in) = (oa) is det, in) = oa is det.

    % Same as map/3, but takes an impure pred.
    %
:- impure pred impure_map(impure pred(X, Y), list(X), list(Y)).
:- mode impure_map(pred(ia, oa) is det, ia, oa) is det.
:- mode impure_map(pred(ia, out) is det, ia, out) is det.
:- mode impure_map(pred(in, oa) is det, in, oa) is det.
:- mode impure_map(pred(ia, oa) is semidet, ia, oa) is semidet.
:- mode impure_map(pred(ia, out) is semidet, ia, out) is semidet.
:- mode impure_map(pred(in, oa) is semidet, in, oa) is semidet.

    % map2(Pred, L, M1, M2):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2 and M3.
    %
:- pred map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode map2(pred(ia, oa, oa) is det, ia, oa, oa) is det.
:- mode map2(pred(ia, oa, oa) is cc_multi, ia, oa, oa) is cc_multi.
:- mode map2(pred(ia, oa, oa) is semidet, ia, oa, oa) is semidet.
:- mode map2(pred(ia, oa, oa) is multi, ia, oa, oa) is multi.
:- mode map2(pred(ia, oa, oa) is nondet, ia, oa, oa) is nondet.
:- mode map2(pred(ia, ia, ia) is semidet, ia, ia, ia) is semidet.

    % Same as map2/4, but takes an impure pred.
    %
:- impure pred impure_map2(impure pred(A, B, C), list(A), list(B), list(C)).
:- mode impure_map2(pred(ia, oa, oa) is det, ia, oa, oa) is det.
:- mode impure_map2(pred(ia, oa, oa) is semidet, ia, oa, oa) is semidet.

    % map3(Pred, L, M1, M2, M3):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2 and M3.
    %
:- pred map3(pred(A, B, C, D), list(A), list(B), list(C), list(D)).
:- mode map3(pred(ia, oa, oa, oa) is det, ia, oa, oa, oa) is det.
:- mode map3(pred(ia, oa, oa, oa) is cc_multi, ia, oa, oa, oa) is cc_multi.
:- mode map3(pred(ia, oa, oa, oa) is semidet, ia, oa, oa, oa) is semidet.
:- mode map3(pred(ia, oa, oa, oa) is multi, ia, oa, oa, oa) is multi.
:- mode map3(pred(ia, oa, oa, oa) is nondet, ia, oa, oa, oa) is nondet.
:- mode map3(pred(ia, ia, ia, ia) is semidet, ia, ia, ia, ia) is semidet.

    % map4(Pred, L, M1, M2, M3, M4):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2, M3 and M4.
    %
:- pred map4(pred(A, B, C, D, E), list(A), list(B), list(C), list(D), list(E)).
:- mode map4(pred(ia, oa, oa, oa, oa) is det, ia, oa, oa, oa, oa) is det.
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

    % map5(Pred, L, M1, M2, M3, M4, M5):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2, M3, M4 and M5.
    %
:- pred map5(pred(A, B, C, D, E, F), list(A), list(B),
    list(C), list(D), list(E), list(F)).
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

    % map6(Pred, L, M1, M2, M3, M4, M5, M6):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2, M3, M4, M5 and M6.
    %
:- pred map6(pred(A, B, C, D, E, F, G), list(A), list(B),
    list(C), list(D), list(E), list(F), list(G)).
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

    % map7(Pred, L, M1, M2, M3, M4, M5, M6, M7):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2, M3, M4, M5, M6 and M7.
    %
:- pred map7(pred(A, B, C, D, E, F, G, H), list(A), list(B),
    list(C), list(D), list(E), list(F), list(G), list(H)).
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

    % map8(Pred, L, M1, M2, M3, M4, M5, M6, M7, M8):
    %
    % Applies Pred to transform the elements of L into the elements of
    % M1, M2, M3, M4, M5, M6, M7 and M8.
    %
:- pred map8(pred(A, B, C, D, E, F, G, H, I), list(A),
    list(B), list(C), list(D), list(E), list(F), list(G), list(H), list(I)).
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
:- func map_corresponding(func(A, B) = C, list(A), list(B)) = list(C).
:- mode map_corresponding(func(ia, ia) = oa is det, ia, ia) = oa is det.
:- mode map_corresponding(func(ia, in) = oa is det, ia, in) = oa is det.
:- mode map_corresponding(func(in, ia) = oa is det, in, ia) = oa is det.

:- pred map_corresponding(pred(A, B, C), list(A), list(B), list(C)).
:- mode map_corresponding(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode map_corresponding(pred(ia, in, oa) is det, ia, in, oa) is det.
:- mode map_corresponding(pred(in, ia, oa) is det, in, ia, oa) is det.

    % map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
    %   [F(A1, B1, C1), .., F(An, Bn, Cn)].
    %
    % An exception is raised if the list arguments differ in length.
    %
:- func map_corresponding3(func(A, B, C) = D, list(A), list(B), list(C))
    = list(D).
:- mode map_corresponding3(func(ia, ia, ia) = oa is det, ia, ia, ia)
    = oa is det.

    % foldl(Pred, List, Start, End) calls Pred with each
    % element of List (working left-to-right) and an accumulator
    % (with the initial value of Start), and returns the final
    % accumulator value in End.
    %
:- pred foldl(pred(L, A, A), list(L), A, A).
:- mode foldl(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode foldl(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldl(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode foldl(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode foldl(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode foldl(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func foldl(func(L, A) = A, list(L), A) = A.
:- mode foldl(func(ia, ia) = oa is det, ia, ia) = oa is det.

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
    % accumulator value in End.
    %
:- pred foldr(pred(L, A, A), list(L), A, A).
:- mode foldr(pred(ia, di, uo) is det, ia, di, uo) is det.
:- mode foldr(pred(ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldr(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode foldr(pred(ia, ia, oa) is nondet, ia, ia, oa) is nondet.
:- mode foldr(pred(ia, di, uo) is cc_multi, ia, di, uo) is cc_multi.
:- mode foldr(pred(ia, ia, oa) is cc_multi, ia, ia, oa) is cc_multi.

:- func foldr(func(L, A) = A, list(L), A) = A.
:- mode foldr(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % foldl2(Pred, List, !Acc1, !Acc2)
    % Does the same job as foldl, but with two accumulators.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl2(pred(L, A, A, Z, Z), list(L), A, A, Z, Z).
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
:- pred foldl3(pred(L, A, A, B, B, C, C), list(L),
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
:- pred foldl4(pred(L, A, A, B, B, C, C, D, D), list(L),
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
:- pred foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), list(L),
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
:- pred foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F),
    list(L), A, A, B, B, C, C, D, D, E, E, F, F).
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
    %
    % Does the same job as foldl, but works on two lists in parallel.
    % Raises an exception if the list arguments have different lengths.
    %
:- pred foldl_corresponding(pred(A, B, C, C),
    list(A), list(B), C, C).
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

    % Same as foldl_corresponding, but takes an impure predicate.
    %
:- impure pred impure_foldl_corresponding(impure pred(A, B, C, C),
    list(A), list(B), C, C).
:- mode impure_foldl_corresponding(pred(ia, ia, ia, oa) is det,
    ia, ia, ia, oa) is det.
:- mode impure_foldl_corresponding(pred(ia, ia, di, uo) is semidet,
    ia, ia, di, uo) is semidet.
:- mode impure_foldl_corresponding(pred(ia, ia, ia, oa) is semidet,
    ia, ia, ia, oa) is semidet.

    % foldl2_corresponding(F, As, Bs, !Acc1, !Acc2)
    %
    % Does the same job as foldl_corresponding, but has two accumulators.
    %
:- pred foldl2_corresponding(pred(A, B, C, C, D, D),
    list(A), list(B), C, C, D, D).
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
:- pred map_foldl(pred(L, M, A, A), list(L), list(M), A, A).
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
:- pred map2_foldl(pred(L, M, N, A, A),
    list(L), list(M), list(N), A, A).
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

    % Same as map2_foldl/6, but takes an impure pred.
    %
:- impure pred impure_map2_foldl(impure pred(L, M, N, A, A),
	list(L), list(M), list(N), A, A).
:- mode impure_map2_foldl(pred(ia, oa, oa, ia, oa) is semidet,
	ia, oa, oa, ia, oa) is semidet.

    % Same as map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(L, M, A, A, B, B),
    list(L), list(M), A, A, B, B).
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
:- pred map_foldl3(pred(L, M, A, A, B, B, C, C),
    list(L), list(M), A, A, B, B, C, C).
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
    list(L), list(M), A, A, B, B, C, C, D, D).
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
    list(L), list(M), A, A, B, B, C, C, D, D, E, E).
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
    list(L), list(M), A, A, B, B, C, C, D, D, E, E, F, F).
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
:- pred all_true(pred(T)::in(pred(ia) is semidet), list(T)::ia) is semidet.

    % Same as all_true, but traverses two lists in parallel.
    % An exception is raised if the list arguments differ in length.
    %
:- pred all_true_corresponding(pred(A, B), list(A), list(B)).
:- mode all_true_corresponding(pred(ia, ia) is semidet, ia, ia) is semidet.

    % Same as all_true_corresponding, but takes an impure pred.
    %
:- impure pred impure_all_true_corresponding(impure pred(A, B),
	list(A), list(B)).
:- mode impure_all_true_corresponding(pred(ia, ia) is det, ia, ia)
	is det.
:- mode impure_all_true_corresponding(pred(ia, ia) is semidet, ia, ia)
	is semidet.

%---------------------------------------------------------------------------%

:- func head(list(T)::ia) = (T::oa) is semidet.

:- func tail(list(T)::ia) = (list(T)::oa) is semidet.

    % det_head(List) returns the first element of List,
    % calling error/1 if List is empty.
    %
:- func det_head(list(T)::ia) = (T::oa) is det.

    % det_tail(List) returns the tail of List,
    % calling error/1 if List is empty.
    %
:- func det_tail(list(T)::ia) = (list(T)::oa) is det.

%---------------------------------------------------------------------------%

    % sort(Compare, Unsorted, Sorted) is true iff Sorted is a
    % list containing the same elements as Unsorted, where Sorted is
    % sorted with respect to the ordering defined by the predicate
    % Compare, and the elements that are equivalent in this ordering
    % appear in the same sequence in Sorted as they do in Unsorted
    % (that is, the sort is stable).
    %
:- pred sort(comparison_pred(T)::in(comparison_pred(any)),
    list(T)::ia, list(T)::oa) is det.

    % merge(Compare, As, Bs, Sorted) is true iff, assuming As and
    % Bs are sorted with respect to the ordering defined by Compare,
    % Sorted is a list containing the elements of As and Bs which is
    % also sorted.  For elements which are equivalent in the ordering,
    % if they come from the same list then they appear in the same
    % sequence in Sorted as they do in that list, otherwise the elements
    % from As appear before the elements from Bs.
    %
:- pred merge(comparison_pred(T)::in(comparison_pred(any)),
    list(T)::ia, list(T)::ia, list(T)::oa) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

is_empty([]).

is_not_empty([_ | _]).

cons(H, T) = [H | T].

append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :-
    any_list.append(Xs, Ys, Zs).

append(Xs, Ys) = Zs :-
    any_list.append(Xs, Ys, Zs).

%---------------------------------------------------------------------------%

index0([X | Xs], N, Y) :-
    ( if N = 0 then Y = X else any_list.index0(Xs, N - 1, Y) ).

index1([X | Xs], N, Y) :-
    ( if N = 1 then Y = X else any_list.index1(Xs, N - 1, Y) ).

index0_det(List, N, Elem) :-
    promise_pure (
        ( if any_list.index0(List, N, Elem0) then
            Elem = Elem0
        else
            error("any_list.index0_det: index out of range")
        )
    ).

index1_det(List, N, Elem) :-
    promise_pure (
        ( if any_list.index1(List, N, Elem0) then
            Elem = Elem0
        else
            error("any_list.index1_det: index out of range")
        )
    ).

index0_det(List, N) = Elem :-
    any_list.index0_det(List, N, Elem).

index1_det(List, N) = Elem :-
    any_list.index1_det(List, N, Elem).

%---------------------------------------------------------------------------%

condense([]) = [].
condense([L | Ls]) = any_list.(L ++ any_list.condense(Ls)).

%---------------------------------------------------------------------------%

same_length([], []).
same_length([_ | L1], [_ | L2]) :-
    any_list.same_length(L1, L2).

%---------------------------------------------------------------------------%

insert(X, Ys, [X | Ys]).
insert(X, [Y | Ys], [Y | Zs]) :-
    any_list.insert(X, Ys, Zs).

%---------------------------------------------------------------------------%

delete(List, Elem, Remainder) :-
    any_list.insert(Elem, Remainder, List).

%---------------------------------------------------------------------------%

replace_nth(List0, P, R) = List :-
    P > 0,
    replace_nth_loop(P, R, List0, List).

replace_nth_det(List0, P, R) = List :-
    promise_pure (
        ( if P > 0 then
            ( if replace_nth_loop(P, R, List0, List1) then
                List = List1
            else
                error("any_list.replace_nth_det: " ++
                    "Can't replace element whose index " ++
                    "position is past the end of the list")
            )
        else
            error("any_list.replace_nth_det: " ++
                "Can't replace element whose index " ++
                "position is less than 1.")
        )
    ).

:- pred replace_nth_loop(int::in, T::ia, list(T)::ia, list(T)::oa) is semidet.

replace_nth_loop(P, R, [X | Xs], List) :-
    ( if P = 1 then
        List = [R | Xs]
    else
        replace_nth_loop(P - 1, R, Xs, Tail),
        List = [X | Tail]
    ).

%---------------------------------------------------------------------------%

member(X, [X | _]).
member(X, [_ | Xs]) :-
    any_list.member(X, Xs).

member(Element, List, SubList) :-
    SubList = [Element | _],
    any_list.append(_, SubList, List).

%---------------------------------------------------------------------------%

% Note - it is not possible to write a version of
% length/1 in pure Mercury that works in both directions
% unless you make it semidet rather than det.

length(L) = N :-
    length_2(L, 0, N).

length(L, N) :-
    length_2(L, 0, N).

:- pred length_2(list(T)::ia, int::in, int::out) is det.

length_2([], N, N).
length_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    length_2(L1, N1, N).

%---------------------------------------------------------------------------%

reverse(L0) = L :-
    reverse_loop(L0, [], L).

reverse(L0, L) :-
    reverse_loop(L0, [], L).

:- pred reverse_loop(list(T)::ia, list(T)::ia, list(T)::oa) is det.

reverse_loop([], L, L).
reverse_loop([X | Xs], L0, L) :-
    reverse_loop(Xs, [X | L0], L).

%---------------------------------------------------------------------------%

zip([], Bs) = Bs.
zip([A | As], Bs) = [A | Cs] :-
    zip2(As, Bs, Cs).

:- pred zip2(list(T)::ia, list(T)::ia, list(T)::oa) is det.

zip2(As, [], As).
zip2(As, [B | Bs], [B | Cs]) :-
    zip2(As, Bs, Cs).

%---------------------------------------------------------------------------%

split_list(N, List, Start, End) :-
    ( if N = 0 then
        Start = [],
        End = List
    else
        N > 0,
        List = [Head | List1],
        Start = [Head | Start1],
        any_list.split_list(N - 1, List1, Start1, End)
    ).

take(N, As, Bs) :-
    ( if N > 0 then
        As = [A | As1],
        any_list.take(N - 1, As1, Bs1),
        Bs = [A | Bs1]
    else
        Bs = []
    ).

take_upto(N, As) = Bs :-
    promise_pure (
        ( if any_list.take(N, As, Bs0) then
            Bs = Bs0
        else
            Bs = As
        )
    ).

drop(N, As) = Bs :-
    ( if N > 0 then
        As = [_ | Cs],
        Bs = any_list.drop(N - 1, Cs)
    else
        As = Bs
    ).

%---------------------------------------------------------------------------%

duplicate(N, X) = duplicate(N, X, []).

:- func duplicate(int::in, T::ia, list(T)::ia) = (list(T)::oa) is det.

duplicate(N, X, Xs) =
    ( if N > 0 then
        duplicate(N - 1, X, [X | Xs])
    else
        Xs
    ).

%---------------------------------------------------------------------------%

chunk(List, ChunkSize) = ListOfSmallLists :-
    chunk_loop(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred chunk_loop(list(T)::ia, int::in, list(T)::ia, int::in,
    list(list(T))::oa) is det.

chunk_loop([], _ChunkSize, List0, _N, Lists) :-
    promise_pure
    (
        List0 = [],
        Lists = []
    ;
        List0 = [_ | _],
        List  = any_list.reverse(List0),
        Lists = [List]
    ).
chunk_loop([X | Xs], ChunkSize, List0, N, Lists) :-
    ( if N > 1 then
        chunk_loop(Xs, ChunkSize, [X | List0], N - 1, Lists)
    else
        List = any_list.reverse([X | List0]),
        chunk_loop(Xs, ChunkSize, [], ChunkSize, Lists1),
        Lists = [List | Lists1]
    ).

%---------------------------------------------------------------------------%

perm([], []).
perm([X | Xs], Ys) :-
    any_list.perm(Xs, Ys0),
    any_list.insert(X, Ys0, Ys).

%---------------------------------------------------------------------------%

all_same([]).
all_same([H | T]) :-
    all_same_loop(H, T).

:- pred all_same_loop(T::ia, list(T)::ia) is semidet.

all_same_loop(_, []).
all_same_loop(H, [H | T]) :-
    all_same_loop(H, T).

%---------------------------------------------------------------------------%

last([H | T], Last) :-
    (
        T = [],
        Last = H
    ;
        T = [_ | _],
        any_list.last(T, Last)
    ).

det_last(List) = Last :-
    promise_pure (
        ( if any_list.last(List, LastPrime) then
            Last = LastPrime
        else
            error("any_list.last_det: empty list")
        )
    ).

split_last([H | T], AllButLast, Last) :-
    (
        T = [],
        AllButLast = [],
        Last = H
    ;
        T = [_ | _],
        any_list.split_last(T, AllButLast1, Last),
        AllButLast = [H | AllButLast1]
    ).

split_last_det(List, AllButLast, Last) :-
    promise_pure (
        ( if any_list.split_last(List, AllButLastPrime, LastPrime) then
            AllButLast = AllButLastPrime,
            Last = LastPrime
        else
            error("any_list.split_last_det: empty list")
        )
    ).

%---------------------------------------------------------------------------%

transpose([]) = _ :-
    throw("any_list.transpose: list is empty").
transpose([A | As]) = transpose0(any_list.length(A), [A | As]).

transpose0(N, As) = Bs :-
	Bs0 = any_list.duplicate(N, []),
	transpose_loop(As, Bs0, Bs).

:- pred transpose_loop(list(list(T))::ia, list(list(T))::ia, list(list(T))::oa)
	is det.

transpose_loop([], Bs, Bs).
transpose_loop([A | As], Bs0, Bs) :-
	transpose_loop(As, Bs0, Bs1),
	Bs = any_list.map_corresponding(
		(func(X::ia, Xs::ia) = ([X | Xs]::oa) is det),
		A, Bs1).

%---------------------------------------------------------------------------%

map(_, [],  []).
map(P, [H0 | T0], [H | T]) :-
    P(H0, H),
    any_list.map(P, T0, T).

impure_map(_, []) = [].
impure_map(F, [H0 | T0]) = [H | T] :-
    impure H = impure_apply(F, H0),
    impure T = any_list.impure_map(F, T0).

impure_map(_, [],  []).
impure_map(P, [H0 | T0], [H | T]) :-
    impure P(H0, H),
    impure any_list.impure_map(P, T0, T).

map2(_, [],  [],  []).
map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
    P(H0, H1, H2),
    any_list.map2(P, T0, T1, T2).

impure_map2(_, [],  [],  []).
impure_map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
	impure P(H0, H1, H2),
	impure any_list.impure_map2(P, T0, T1, T2).

map3(_, [],  [],  [],  []).
map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
    P(H0, H1, H2, H3),
    any_list.map3(P, T0, T1, T2, T3).

map4(_, [], [], [], [], []).
map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
    P(H0, H1, H2, H3, H4),
    any_list.map4(P, T0, T1, T2, T3, T4).

map5(_, [], [], [], [], [], []).
map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5]) :-
    P(H0, H1, H2, H3, H4, H5),
    any_list.map5(P, T0, T1, T2, T3, T4, T5).

map6(_, [], [], [], [], [], [], []).
map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6]) :-
    P(H0, H1, H2, H3, H4, H5, H6),
    any_list.map6(P, T0, T1, T2, T3, T4, T5, T6).

map7(_, [], [], [], [], [], [], [], []).
map7(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6], [H7 | T7]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7),
    any_list.map7(P, T0, T1, T2, T3, T4, T5, T6, T7).

map8(_, [], [], [], [], [], [], [], [], []).
map8(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4],
        [H5 | T5], [H6 | T6], [H7 | T7], [H8 | T8]) :-
    P(H0, H1, H2, H3, H4, H5, H6, H7, H8),
    any_list.map8(P, T0, T1, T2, T3, T4, T5, T6, T7, T8).

map_corresponding(_, [], []) = [].
map_corresponding(_, [], [_ | _]) =
    func_error("any_list.map_corresponding/3: mismatched list arguments").
map_corresponding(_, [_ | _], []) =
    func_error("any_list.map_corresponding/3: mismatched list arguments").
map_corresponding(F, [A | As], [B | Bs]) =
    [F(A, B) | any_list.map_corresponding(F, As, Bs)].

map_corresponding(_, [], [], []).
map_corresponding(_, [], [_ | _], _) :-
    error("any_list.map_corresponding/4: mismatched list arguments").
map_corresponding(_, [_ | _], [], _) :-
    error("any_list.map_corresponding/4: mismatched list arguments").
map_corresponding(P, [A | As], [B | Bs], [C | Cs]) :-
    P(A, B, C),
    any_list.map_corresponding(P, As, Bs, Cs).

map_corresponding3(F, As, Bs, Cs) = Ds :-
    promise_pure
    ( if
        As = [A | As0],
        Bs = [B | Bs0],
        Cs = [C | Cs0]
    then
        Ds = [F(A, B, C) | any_list.map_corresponding3(F, As0, Bs0, Cs0)]
    else if
        As = [],
        Bs = [],
        Cs = []
    then
        Ds = []
    else
        error("any_list.map_corresponding3: mismatched list arguments")
    ).

foldl(_, [], !A).
foldl(P, [H | T], !A) :-
    P(H, !A),
    any_list.foldl(P, T, !A).

impure_foldl(_, [], !A).
impure_foldl(P, [H | T], !A) :-
	impure P(H, !A),
	impure any_list.impure_foldl(P, T, !A).

foldl2(_, [], !A, !B).
foldl2(P, [H | T], !A, !B) :-
    P(H, !A, !B),
    any_list.foldl2(P, T, !A, !B).

impure_foldl2(_, [], !A, !B).
impure_foldl2(P, [H | T], !A, !B) :-
	impure P(H, !A, !B),
	impure any_list.impure_foldl2(P, T, !A, !B).

foldl3(_, [], !A, !B, !C).
foldl3(P, [H | T], !A, !B, !C) :-
    P(H, !A, !B, !C),
    any_list.foldl3(P, T, !A, !B, !C).

foldl4(_, [], !A, !B, !C, !D).
foldl4(P, [H | T], !A, !B, !C, !D) :-
    P(H, !A, !B, !C, !D),
    any_list.foldl4(P, T, !A, !B, !C, !D).

foldl5(_, [], !A, !B, !C, !D, !E).
foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
    P(H, !A, !B, !C, !D, !E),
    any_list.foldl5(P, T, !A, !B, !C, !D, !E).

foldl6(_, [], !A, !B, !C, !D, !E, !F).
foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H, !A, !B, !C, !D, !E, !F),
    any_list.foldl6(P, T, !A, !B, !C, !D, !E, !F).

foldl_corresponding(_, [], [], !Acc).
foldl_corresponding(_, [], [_ | _], _, _) :-
    error("any_list.foldl_corresponding/5: mismatched list arguments").
foldl_corresponding(_, [_ | _], [], _, _) :-
    error("any_list.foldl_corresponding/5: mismatched list arguments").
foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    P(A, B, !Acc),
    any_list.foldl_corresponding(P, As, Bs, !Acc).

impure_foldl_corresponding(_, [], [], !Acc).
impure_foldl_corresponding(_, [], [_ | _], _, _) :-
    error("any_list.impure_foldl_corresponding/5: mismatched list arguments").
impure_foldl_corresponding(_, [_ | _], [], _, _) :-
    error("any_list.impure_foldl_corresponding/5: mismatched list arguments").
impure_foldl_corresponding(P, [A | As], [B | Bs], !Acc) :-
    impure P(A, B, !Acc),
    impure any_list.impure_foldl_corresponding(P, As, Bs, !Acc).

foldl2_corresponding(_, [], [], !Acc1, !Acc2).
foldl2_corresponding(_, [], [_ | _], _, _, _, _) :-
    error("any_list.foldl2_corresponding/7: mismatched list arguments").
foldl2_corresponding(_, [_ | _], [], _, _, _, _) :-
    error("any_list.foldl2_corresponding/7: mismatched list arguments").
foldl2_corresponding(P, [A | As], [B | Bs], !Acc1, !Acc2) :-
    P(A, B, !Acc1, !Acc2),
    any_list.foldl2_corresponding(P, As, Bs, !Acc1, !Acc2).

map_foldl(_, [], [], !A).
map_foldl(P, [H0 | T0], [H | T], !A) :-
    P(H0, H, !A),
    any_list.map_foldl(P, T0, T, !A).

impure_map_foldl(_, [], [], !A).
impure_map_foldl(P, [H0 | T0], [H | T], !A) :-
	impure P(H0, H, !A),
	impure any_list.impure_map_foldl(P, T0, T, !A).

map2_foldl(_, [], [], [], !A).
map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
    P(H0, H1, H2, !A),
    any_list.map2_foldl(P, T0, T1, T2, !A).

impure_map2_foldl(_, [], [], [], !A).
impure_map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
	impure P(H0, H1, H2, !A),
	impure any_list.impure_map2_foldl(P, T0, T1, T2, !A).

map_foldl2(_, [], [], !A, !B).
map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
    P(H0, H, !A, !B),
    any_list.map_foldl2(P, T0, T, !A, !B).

impure_map_foldl2(_, [], [], !A, !B).
impure_map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
	impure P(H0, H, !A, !B),
	impure any_list.impure_map_foldl2(P, T0, T, !A, !B).

map_foldl3(_, [], [], !A, !B, !C).
map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
    P(H0, H, !A, !B, !C),
    any_list.map_foldl3(P, T0, T, !A, !B, !C).

map_foldl4(_, [], [], !A, !B, !C, !D).
map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
    P(H0, H, !A, !B, !C, !D),
    any_list.map_foldl4(P, T0, T, !A, !B, !C, !D).

map_foldl5(_, [], [], !A, !B, !C, !D, !E).
map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
    P(H0, H, !A, !B, !C, !D, !E),
    any_list.map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
    P(H0, H, !A, !B, !C, !D, !E, !F),
    any_list.map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

foldr(_, [], !A).
foldr(P, [H | T], !A) :-
    any_list.foldr(P, T, !A),
    P(H, !A).

all_true(_P, []).
all_true(P, [X | Xs]) :-
    P(X),
    any_list.all_true(P, Xs).

all_true_corresponding(_, [], []).
all_true_corresponding(P, [A | As], [B | Bs]) :-
	P(A, B),
	any_list.all_true_corresponding(P, As, Bs).
all_true_corresponding(_, [], [_ | _]) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").
all_true_corresponding(_, [_ | _], []) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").

impure_all_true_corresponding(_, [], []).
impure_all_true_corresponding(P, [A | As], [B | Bs]) :-
	impure P(A, B),
	impure any_list.impure_all_true_corresponding(P, As, Bs).
impure_all_true_corresponding(_, [], [_ | _]) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").
impure_all_true_corresponding(_, [_ | _], []) :-
	error("any_list.all_true_corresponding/3: mismatched list arguments").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 27/04/99
%       Function forms added.

det_head([]) = _ :-
    error("any_list.det_head/1: empty list as argument").
det_head([X | _]) = X.

det_tail([]) = _ :-
    error("any_list.det_tail/1: empty list as argument").
det_tail([_ | Xs]) = Xs.

head([X | _]) = X.

tail([_ | Xs]) = Xs.

map(F, Xs) = Ys :-
    P = ( pred(X::ia, Y::oa) is det :- Y = F(X) ),
    any_list.map(P, Xs, Ys).

foldl(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_list.foldl(P, Xs, A, B).

foldr(F, Xs, A) = B :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_list.foldr(P, Xs, A, B).

(L1 ++ L2) = L3 :-
    any_list.append(L1, L2, L3).

%---------------------------------------------------------------------------%

sort(P, L0, L) :-
    any_list.length(L0, N),
    ( if N = 0 then
        L = []
    else
        promise_pure (
            ( if hosort(P, N, L0, L1, []) then
                L = L1
            else
                error("hosort failed")
            )
        )
    ).

    % hosort is actually det but the compiler can't confirm it.
    %
:- pred hosort(pred(T, T, comparison_result)::
    (pred(ia, ia, out) is det), int::in, list(T)::ia, list(T)::oa, list(T)::oa)
    is semidet.

    % hosort is a Mercury implementation of the mergesort
    % described in The Craft of Prolog.
    % N denotes the length of the part of L0 that this call is sorting.
    % (require((length(L0, M), M >= N)))
    % Since we have redundant information about the list (N, and the
    % length implicit in the list itself), we get a semidet unification
    % when we deconstruct the any_list.
hosort(P, N, L0, L, Rest) :-
    ( if N = 1 then
        L0 = [X | Rest],
        L = [X]
    else if N = 2 then
        L0 = [X, Y | Rest],
        P(X, Y, C),
        (
            ( C = (<)
            ; C = (=)
            ),
            L = [X, Y]
        ;
            C = (>),
            L = [Y, X]
        )
    else
        N1 = N // 2,
        hosort(P, N1, L0, L1, Middle),
        N2 = N - N1,
        hosort(P, N2, Middle, L2, Rest),
        any_list.merge(P, L1, L2, L)
    ).

merge(_P, [], [], []).
merge(_P, [], [Y | Ys], [Y | Ys]).
merge(_P, [X | Xs], [], [X | Xs]).
merge(P, [H1 | T1], [H2 | T2], L) :-
    P(H1, H2, R),
    ( if R = (>) then
        L = [H2 | T],
        any_list.merge(P, [H1 | T1], T2, T)
    else
        L = [H1 | T],
        any_list.merge(P, T1, [H2 | T2], T)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
