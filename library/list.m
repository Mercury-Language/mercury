%---------------------------------------------------------------------------%
% Copyright (C) 1993-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Module `list' - defines the list type, and various utility predicates
% that operate on lists.
%
% Authors: fjh, conway, trd, zs, philip, warwick, ...
% Stability: medium to high.
%
%---------------------------------------------------------------------------%

:- module list.
:- interface.
:- import_module int.

%-----------------------------------------------------------------------------%

	% The definition of the type `list(T)':
	%	A list is either an empty list, denoted `[]',
	%	or an element `Head' of type `T' followed by a tail `Tail'
	%	of type `list(T)', denoted `[Head | Tail]'.
	%

:- type list(T) ---> [] ; [T | list(T)].

%-----------------------------------------------------------------------------%

	% Some declarations for complicated modes using lists.
	% Partially instantiated mode aren't fully implemented yet,
	% so don't try to use these.

:- inst list_skel(I) = bound(([] ; [I | list_skel(I)])).
:- inst list_skel = list_skel(free).

:- inst non_empty_list = bound([ground | ground]).

:- mode in_list_skel :: list_skel -> list_skel.
:- mode out_list_skel :: free -> list_skel.
:- mode list_skel_out :: list_skel -> ground.

	% These more verbose versions are deprecated.
	% They exist only for backwards compatibility,
	% and will be removed in a future release.
:- mode input_list_skel :: in_list_skel.
:- mode output_list_skel :: out_list_skel.
:- mode list_skel_output :: list_skel_out.

	% These modes are particularly useful for passing around lists
	% of higher order terms, since they have complicated insts
	% which are not correctly approximated by "ground".
:- mode list_skel_in(I) :: list_skel(I) -> list_skel(I).
:- mode list_skel_out(I) :: free -> list_skel(I).

%-----------------------------------------------------------------------------%

:- pred list__is_empty(list(T)::in) is semidet.

:- pred list__is_not_empty(list(T)::in) is semidet.

	% Standard append predicate:
	% list__append(Start, End, List) is true iff
	% `List' is the result of concatenating `Start' and `End'.
	%
:- pred list__append(list(T), list(T), list(T)).
:- mode list__append(di, di, uo) is det.
:- mode list__append(in, in, out) is det.
:- mode list__append(in, in, in) is semidet.	% implied
:- mode list__append(in, out, in) is semidet.
:- mode list__append(out, out, in) is multi.
%	The following mode is semidet in the sense that it doesn't
%	succeed more than once - but it does create a choice-point,
%	which means it's inefficient and that the compiler can't deduce
%	that it is semidet.  Use list__remove_suffix instead.
% :- mode list__append(out, in, in) is semidet.

:- func list__append(list(T), list(T)) = list(T).

	% associativity of append
:- promise all [A, B, C, ABC]
	(
		( some [AB]
			(list__append(A, B, AB), list__append(AB, C, ABC)) )
	<=>
		( some [BC]
			(list__append(B, C, BC), list__append(A, BC, ABC)) )
	).
	% construction equivalence law.
	% XXX when we implement rewrite rules, we should change this law
	% to a rewrite rule.
:- promise all [L, H, T] ( append([H], T, L) <=> L = [H | T] ).

	% L1 ++ L2 = L :- list__append(L1, L2, L).
	%
:- func list(T) ++ list(T) = list(T).

	% list__remove_suffix(List, Suffix, Prefix):
	%	The same as list__append(Prefix, Suffix, List) except that
	%	this is semidet whereas list__append(out, in, in) is nondet.
:- pred list__remove_suffix(list(T), list(T), list(T)).
:- mode list__remove_suffix(in, in, out) is semidet.

	% list__merge(L1, L2, L):
	%	L is the result of merging the elements of L1 and L2,
	%	in ascending order.  L1 and L2 must be sorted.
:- pred list__merge(list(T), list(T), list(T)).
:- mode list__merge(in, in, out) is det.

:- func list__merge(list(T), list(T)) = list(T).

	% list__merge_and_remove_dups(L1, L2, L):
	%	L is the result of merging the elements of L1 and L2,
	%	in ascending order, and eliminating any duplicates.
	%	L1 and L2 must be sorted and must each not contain any
	%	duplicates.
:- pred list__merge_and_remove_dups(list(T), list(T), list(T)).
:- mode list__merge_and_remove_dups(in, in, out) is det.

:- func list__merge_and_remove_dups(list(T), list(T)) = list(T).

	% list__remove_adjacent_dups(L0, L) :
	%	L is the result of replacing every sequence of duplicate
	%	elements in L0 with a single such element.
:- pred list__remove_adjacent_dups(list(T), list(T)).
:- mode list__remove_adjacent_dups(in, out) is det.

:- func list__remove_adjacent_dups(list(T)) = list(T).

	% list__remove_dups(L0, L) :
	%	L is the result of deleting the second and subsequent
	%	occurrences of every element that occurs twice in L.
:- pred list__remove_dups(list(T), list(T)).
:- mode list__remove_dups(in, out) is det.

:- func list__remove_dups(list(T)) = list(T).

	% list__member(Elem, List) :
	%	True iff `List' contains `Elem'.
:- pred list__member(T, list(T)).
:- mode list__member(in, in) is semidet.
:- mode list__member(out, in) is nondet.

	% list__member(Elem, List, SubList) :
	%	True iff `List' contains `Elem', and `SubList' is
	%	a suffix of `List' beginning with `Elem'.
	%	Same as `SubList = [Elem | _], list__append(_, SubList, List)'.
	%
:- pred list__member(T, list(T), list(T)).
:- mode list__member(out, in, out) is nondet.

	% list__length(List, Length) :
	%	True iff `Length' is the length of `List', i.e. if
	%	`List' contains `Length' elements.
	%
:- pred list__length(list(_T), int).
:- mode list__length(in, out) is det.
	% XXX The current mode checker can't handle this mode
% :- mode list__length(input_list_skel, out) is det.

:- func list__length(list(T)) = int.

	% list__same_length(ListA, ListB) :
	%	True iff `ListA' and `ListB' have the same length,
	%	i.e. iff they both contain the same number of elements.
	%
:- pred list__same_length(list(T1), list(T2)).
:- mode list__same_length(in, output_list_skel) is det.
:- mode list__same_length(output_list_skel, in) is det.
:- mode list__same_length(in, in) is semidet.
	% XXX The current mode checker can't handle these modes
% :- mode list__same_length(input_list_skel, output_list_skel) is det.
% :- mode list__same_length(output_list_skel, input_list_skel) is det.

	% list__split_list(Len, List, Start, End):
	%	splits `List' into a prefix `Start' of length `Len',
	%	and a remainder `End'.
	%	See also: list__take, list__drop.
	%
:- pred list__split_list(int, list(T), list(T), list(T)).
:- mode list__split_list(in, in, out, out) is semidet.

	% list__take(Len, List, Start):
	%	`Start' is the first `Len' elements of `List'.
	%	Fails if `List' has less than `Len' elements.
	%	See also: list__split_list.
	%
:- pred list__take(int, list(T), list(T)).
:- mode list__take(in, in, out) is semidet.

	% list__take_upto(Len, List, Start):
	%	`Start' is the first `Len' elements of `List'.
	%	If `List' has less than `Len' elements, return the entire list.
	%
:- pred list__take_upto(int, list(T), list(T)).
:- mode list__take_upto(in, in, out) is det.

:- func list__take_upto(int, list(T)) = list(T).

	% list__drop(Len, List, End):
	%	`End' is the remainder of `List' after removing the
	%	first `Len' elements.
	%	See also: list__split_list.
	%
:- pred list__drop(int, list(T), list(T)).
:- mode list__drop(in, in, out) is semidet.

	% list__insert(Elem, List0, List):
	%	`List' is the result of inserting `Elem' somewhere in `List0'.
	%	Same as `list__delete(List, Elem, List0)'.
	%
:- pred list__insert(T, list(T), list(T)).
:- mode list__insert(in, in, in) is semidet.
:- mode list__insert(in, out, in) is nondet.
:- mode list__insert(out, out, in) is nondet.
:- mode list__insert(in, in, out) is multi.

	% list__delete(List, Elem, Remainder):
	%	True iff `Elem' occurs in `List', and
	%	`Remainder' is the result of deleting one occurrence of
	%	`Elem' from `List'.
	%
:- pred list__delete(list(T), T, list(T)).
:- mode list__delete(in, in, in) is semidet.
:- mode list__delete(in, in, out) is nondet.
:- mode list__delete(in, out, out) is nondet.
:- mode list__delete(out, in, in) is multi.

:- func list__delete_all(list(T), T) = list(T).

	% list__delete_first(List0, Elem, List) is true iff Elem occurs in List0
	% and List is List0 with the first occurence of Elem removed
	%
:- pred list__delete_first(list(T), T, list(T)).
:- mode list__delete_first(in, in, out) is semidet.

	% list__delete_all(List0, Elem, List) is true iff List is List0 with
	% all occurences of Elem removed
	%
:- pred list__delete_all(list(T), T, list(T)).
:- mode list__delete_all(di, in, uo) is det.
:- mode list__delete_all(in, in, out) is det.

	% list__delete_elems(List0, Elems, List) is true iff List is List0 with
	% all occurences of all elements of Elems removed
	%
:- pred list__delete_elems(list(T), list(T), list(T)).
:- mode list__delete_elems(in, in, out) is det.

:- func list__delete_elems(list(T), list(T)) = list(T).

	% list__replace(List0, D, R, List) is true iff List is List0
	% with an occurence of D replaced with R.
	%
:- pred list__replace(list(T), T, T, list(T)).
:- mode list__replace(in, in, in, in) is semidet.
:- mode list__replace(in, in, in, out) is nondet.

	% list__replace_first(List0, D, R, List) is true iff List is List0
	% with the first occurence of D replaced with R.
	%
:- pred list__replace_first(list(T), T, T, list(T)).
:- mode list__replace_first(in, in, in, out) is semidet.

	% list__replace_all(List0, D, R, List) is true iff List is List0
	% with all occurences of D replaced with R.
	%
:- pred list__replace_all(list(T), T, T, list(T)).
:- mode list__replace_all(in, in, in, out) is det.

:- func list__replace_all(list(T), T, T) = list(T).

	% list__replace_nth(List0, N, R, List) is true iff List is List0
	% with Nth element replaced with R.
	% Fails if N < 1 or if length of List0 < N.
	% (Position numbers start from 1.)
	%
:- pred list__replace_nth(list(T), int, T, list(T)).
:- mode list__replace_nth(in, in, in, out) is semidet.

	% list__replace_nth_det(List0, N, R, List) is true iff List is List0
	% with Nth element replaced with R.
	% Aborts if N < 1 or if length of List0 < N.
	% (Position numbers start from 1.)
	%
:- pred list__replace_nth_det(list(T), int, T, list(T)).
:- mode list__replace_nth_det(in, in, in, out) is det.

:- func list__replace_nth_det(list(T), int, T) = list(T).

	% list__sort_and_remove_dups(List0, List):
	%	List is List0 sorted with duplicates removed.
	%
:- pred list__sort_and_remove_dups(list(T), list(T)).
:- mode list__sort_and_remove_dups(in, out) is det.

:- func list__sort_and_remove_dups(list(T)) = list(T).

	% list__sort(List0, List):
	%	List is List0 sorted.
	%
:- pred list__sort(list(T), list(T)).
:- mode list__sort(in, out) is det.

:- func list__sort(list(T)) = list(T).

	% list__reverse(List, Reverse):
	%	`Reverse' is a list containing the same elements as `List'
	%	but in reverse order.
	%
:- pred list__reverse(list(T), list(T)).
:- mode list__reverse(in, out) is det.

:- func list__reverse(list(T)) = list(T).

	% list__perm(List0, List):
	%	True iff `List' is a permutation of `List0'.
	%
:- pred	list__perm(list(T), list(T)).
:- mode list__perm(in, out) is nondet.

	% list__nth_member_search(List, Elem, Position):
	%	Elem is the Position'th member of List.
	% 	(Position numbers start from 1.)
	%
:- pred list__nth_member_search(list(T), T, int).
:- mode list__nth_member_search(in, in, out) is semidet.

	% list__index*(List, Position, Elem):
	%	These predicates select an element in a list from it's
	%	position.  The `index0' preds consider the first element
	%	element to be element number zero, whereas the `index1' preds
	%	consider the first element to be element number one.
	%	The `_det' preds call error/1 if the index is out of
	%	range, whereas the semidet preds fail if the index is out of
	%	range.
	%
:- pred list__index0(list(T)::in, int::in, T::out) is semidet.
:- pred list__index1(list(T)::in, int::in, T::out) is semidet.
:- pred list__index0_det(list(T)::in, int::in, T::out) is det.
:- pred list__index1_det(list(T)::in, int::in, T::out) is det.

:- func list__index0_det(list(T), int) = T.
:- func list__index1_det(list(T), int) = T.

	% list__zip(ListA, ListB, List):
	%	List is the result of alternating the elements
	%	of ListA and ListB, starting with the first element
	%	of ListA (followed by the first element of ListB,
	%	then the second element of listA, then the second
	%	element of ListB, etc.).  When there are no more
	%	elements remaining in one of the lists,
	% 	the remainder of the nonempty list is appended.
	%
:- pred list__zip(list(T), list(T), list(T)).
:- mode list__zip(in, in, out) is det.

:- func list__zip(list(T), list(T)) = list(T).

	% list__duplicate(Count, Elem, List) is true iff List is a list
	% containing Count duplicate copies of Elem.
	%
:- pred list__duplicate(int, T, list(T)).
:- mode list__duplicate(in, in, out) is det.

:- func list__duplicate(int, T) = list(T).

	% list__condense(ListOfLists, List):
	%	`List' is the result of concatenating all the
	%	elements of `ListOfLists'.
	%
:- pred list__condense(list(list(T)), list(T)).
:- mode list__condense(in, out) is det.

:- func list__condense(list(list(T))) = list(T).

	% list__chunk(List, ChunkSize, Chunks):
	%	Takes a list `List' and breaks it into a list of lists `Chunks',
	%	such that the length of each list in `Chunks' is at most
	%	`ChunkSize.  (More precisely, the length of each list in
	%	`Chunks' other than the last one is exactly `ChunkSize',
	%	and the length of the last list in `Chunks' is between one
	%	and `ChunkSize'.)
	%
:- pred list__chunk(list(T), int, list(list(T))).
:- mode list__chunk(in, in, out) is det.

:- func list__chunk(list(T), int) = list(list(T)).

	% list__sublist(SubList, FullList) is true
	%	if one can obtain SubList by starting with FullList
	%	and deleting some of its elements.
:- pred list__sublist(list(T), list(T)).
:- mode list__sublist(in, in) is semidet.

	% list__all_same(List) is true
	% 	if all elements of the list are the same
:- pred list__all_same(list(T)).
:- mode list__all_same(in) is semidet.

	% list__last(List, Last) is true
	%	if Last is the last element of List.
:- pred list__last(list(T), T).
:- mode list__last(in, out) is semidet.

	% A deterministic version of list__last, which aborts instead of
	% failing if the input list is empty.
:- pred list__last_det(list(T), T).
:- mode list__last_det(in, out) is det.

	% list__split_last(List, AllButLast, Last) is true
	%	if Last is the last element of List and AllButLast is the list
	%	of elements before it.
:- pred list__split_last(list(T), list(T), T).
:- mode list__split_last(in, out, out) is semidet.

	% A deterministic version of list__split_last, which aborts instead of
	% failing if the input list is empty.
:- pred list__split_last_det(list(T), list(T), T).
:- mode list__split_last_det(in, out, out) is det.

%-----------------------------------------------------------------------------%
%
% The following group of predicates use higher-order terms to simplify
% various list processing tasks. They implement pretty much standard
% sorts of operations provided by standard libraries for functional languages.
%
%-----------------------------------------------------------------------------%

	% list__map(T, L, M) uses the closure T
	% to transform the elements of L into the elements of M.
:- pred list__map(pred(X, Y), list(X), list(Y)).
:- mode list__map(pred(in, out) is det, in, out) is det.
:- mode list__map(pred(in, out) is semidet, in, out) is semidet.
:- mode list__map(pred(in, out) is multi, in, out) is multi.
:- mode list__map(pred(in, out) is nondet, in, out) is nondet.
:- mode list__map(pred(in, in) is semidet, in, in) is semidet.

:- func list__map(func(X) = Y, list(X)) = list(Y).

	% list__map2(T, L, M1, M2) uses the closure T
	% to transform the elements of L into the elements of M1 and M2.
:- pred list__map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode list__map2(pred(in, out, out) is det, in, out, out) is det.
:- mode list__map2(pred(in, out, out) is semidet, in, out, out) is semidet.
:- mode list__map2(pred(in, out, out) is multi, in, out, out) is multi.
:- mode list__map2(pred(in, out, out) is nondet, in, out, out) is nondet.
:- mode list__map2(pred(in, in, in) is semidet, in, in, in) is semidet.

	% list__map3(T, L, M1, M2, M3) uses the closure T
	% to transform the elements of L into the elements of M1, M2 and M3.
:- pred list__map3(pred(A, B, C, D), list(A), list(B), list(C), list(D)).
:- mode list__map3(pred(in, out, out, out) is det, in, out, out, out) is det.
:- mode list__map3(pred(in, out, out, out) is semidet, in, out, out, out)
	is semidet.
:- mode list__map3(pred(in, out, out, out) is multi, in, out, out, out)
	is multi.
:- mode list__map3(pred(in, out, out, out) is nondet, in, out, out, out)
	is nondet.
:- mode list__map3(pred(in, in, in, in) is semidet, in, in, in, in) is semidet.

	% list__map_corresponding(F, [A1, .. An], [B1, .. Bn]) =
	% 	[F(A1, B1), .., F(An, Bn)].
	%
	% An exception is raised if the list arguments differ in length.
	%
:- func list__map_corresponding(func(A, B) = C, list(A), list(B)) = list(C).

	% list__map_corresponding3(F, [A1, .. An], [B1, .. Bn], [C1, .. Cn]) =
	% 	[F(A1, B1, C1), .., F(An, Bn, Cn)].
	%
	% An exception is raised if the list arguments differ in length.
	%
:- func list__map_corresponding3(func(A, B, C) = D, list(A), list(B), list(C)) =
		list(D).

	% list__filter_map_corresponding/3 is like list__map_corresponding/3
	% except the function argument is semidet and the output list
	% consists of only those applications of the function argument that
	% succeeded.
	%
:- func list__filter_map_corresponding(func(A, B) = C,
		list(A), list(B)
	) = list(C).
:- mode list__filter_map_corresponding(func(in, in) = out is semidet,
		in, in
	) = out is det.

	% list__filter_map_corresponding3/4 is like list__map_corresponding3/4
	% except the function argument is semidet and the output list
	% consists of only those applications of the function argument that
	% succeeded.
	%
:- func list__filter_map_corresponding3(func(A, B, C) = D,
		list(A), list(B), list(C)
	) = list(D).
:- mode list__filter_map_corresponding3(func(in, in, in) = out is semidet,
		in, in, in
	) = out is det.

	% list__foldl(Pred, List, Start, End) calls Pred with each
	% element of List (working left-to-right) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
:- pred list__foldl(pred(X, Y, Y), list(X), Y, Y).
:- mode list__foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list__foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list__foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list__foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode list__foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- func list__foldl(func(X, Y) = Y, list(X), Y) = Y.

	% list__foldr(Pred, List, Start, End) calls Pred with each
	% element of List (working right-to-left) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
:- pred list__foldr(pred(X, Y, Y), list(X), Y, Y).
:- mode list__foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list__foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list__foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode list__foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- func list__foldr(func(X, Y) = Y, list(X), Y) = Y.

	% list__foldl2(Pred, List, Start, End, Start2, End2)
	% calls Pred with each element of List (working left-to-right),
	% 2 accumulators (with the initial values of Start and Start2),
	% and returns the final values in End and End2.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
:- pred list__foldl2(pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode list__foldl2(pred(in, in, out, in, out) is det,
		in, in, out, in, out) is det.
:- mode list__foldl2(pred(in, in, out, in, out) is semidet,
		in, in, out, in, out) is semidet.
:- mode list__foldl2(pred(in, in, out, in, out) is nondet,
		in, in, out, in, out) is nondet.
:- mode list__foldl2(pred(in, in, out, mdi, muo) is det,
		in, in, out, mdi, muo) is det.
:- mode list__foldl2(pred(in, in, out, di, uo) is det,
		in, in, out, di, uo) is det.
:- mode list__foldl2(pred(in, di, uo, di, uo) is det,
		in, di, uo, di, uo) is det.

	% list__foldl3(Pred, List, Start1, End1, Start2, End2, Start3, End3)
	% calls Pred with each element of List (working left-to-right),
	% 3 accumulators (with the initial values of Start1, Start2 and Start3),
	% and returns the final values in End1, End2 and End3.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
:- pred list__foldl3(pred(L, A1, A1, A2, A2, A3, A3), list(L),
		A1, A1, A2, A2, A3, A3).
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is det,
		in, in, out, in, out, in, out) is det.
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is semidet,
		in, in, out, in, out, in, out) is semidet.
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is nondet,
		in, in, out, in, out, in, out) is nondet.
:- mode list__foldl3(pred(in, in, out, in, out, di, uo) is det,
		in, in, out, in, out, di, uo) is det.

	% list__map_foldl(Pred, InList, OutList, Start, End) calls Pred
	% with an accumulator (with the initial value of Start) on
	% each element of InList (working left-to-right) to transform
	% InList into OutList.  The final value of the accumulator is
	% returned in End.
:- pred list__map_foldl(pred(X, Y, Z, Z), list(X), list(Y), Z, Z).
:- mode list__map_foldl(pred(in, out, di, uo) is det, in, out, di, uo)
		is det.
:- mode list__map_foldl(pred(in, out, in, out) is det, in, out, in, out)
		is det.
:- mode list__map_foldl(pred(in, out, in, out) is semidet, in, out, in, out)
		is semidet.
:- mode list__map_foldl(pred(in, out, in, out) is nondet, in, out, in, out)
		is nondet.

	% Same as list__map_foldl, but with two accumulators.
:- pred list__map_foldl2(pred(X, Y, A, A, B, B), list(X), list(Y), A, A, B, B).
:- mode list__map_foldl2(pred(in, out, in, out, di, uo) is det,
		in, out, in, out, di, uo) is det.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is det,
		in, out, in, out, in, out) is det.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is semidet,
		in, out, in, out, in, out) is semidet.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is nondet,
		in, out, in, out, in, out) is nondet.

	% list__filter(Pred, List, TrueList) takes a closure with one
	% input argument and for each member of List `X', calls the closure.
	% Iff call(Pred, X) is true, then X is included in TrueList.
:- pred list__filter(pred(X), list(X), list(X)).
:- mode list__filter(pred(in) is semidet, in, out) is det.

:- func list__filter(pred(X), list(X)) = list(X).
:- mode list__filter(pred(in) is semidet, in) = out is det.

	% list__filter(Pred, List, TrueList, FalseList) takes a closure with one
	% input argument and for each member of List `X', calls the closure.
	% Iff call(Pred, X) is true, then X is included in TrueList.
	% Iff call(Pred, X) is false, then X is included in FalseList.
:- pred list__filter(pred(X), list(X), list(X), list(X)).
:- mode list__filter(pred(in) is semidet, in, out, out) is det.

	% list__filter_map(Transformer, List, TrueList) takes a predicate
	% with one input argument and one output argument. It is called
	% with each element of List. If a call succeeds, then the output is
	% included in TrueList.
:- pred list__filter_map(pred(X, Y), list(X), list(Y)).
:- mode list__filter_map(pred(in, out) is semidet, in, out) is det.

:- func list__filter_map(func(X) = Y, list(X)) = list(Y).
:- mode list__filter_map(func(in) = out is semidet, in) = out is det.

	% list__filter_map(Transformer, List, TrueList, FalseList) takes
	% a predicate with one input argument and one output argument.
	% It is called with each element of List. If a call succeeds,
	% then the output is included in TrueList; otherwise, the failing
	% input is included in FalseList.
:- pred list__filter_map(pred(X, Y), list(X), list(Y), list(X)).
:- mode list__filter_map(pred(in, out) is semidet, in, out, out) is det.

	% list__takewhile(Predicate, List, UptoList, AfterList) takes a
	% closure with one input argument, and calls it on successive members
	% of List as long as the calls succeed. The elements for which
	% the call succeeds are placed in UptoList and the first element for
	% which the call fails, and all the remaining elements of List are
	% placed in AfterList.
:- pred list__takewhile(pred(T), list(T), list(T), list(T)).
:- mode list__takewhile(pred(in) is semidet, in, out, out) is det.

%-----------------------------------------------------------------------------%

	% list__sort(Compare, Unsorted, Sorted) is true iff Sorted is a
	% list containing the same elements as Unsorted, where Sorted is
	% a sorted list, with respect to the ordering defined by the predicate
	% term Compare.
:- pred list__sort(pred(X, X, comparison_result), list(X), list(X)).
:- mode list__sort(pred(in, in, out) is det, in, out) is det.

:- func list__sort(func(X, X) = comparison_result, list(X)) = list(X).

	% list__sort_and_remove_dups(Compare, Unsorted, Sorted) is true iff
	% Sorted is a list containing the same elements as Unsorted, but with
	% any duplicates removed. Where Sorted is a sorted list, with respect
	% to the ordering defined by the predicate term Compare.
:- pred list__sort_and_remove_dups(pred(X, X, comparison_result), list(X),
	list(X)).
:- mode list__sort_and_remove_dups(pred(in, in, out) is det, in, out) is det.

	% list__merge(Compare, As, Bs, Sorted) is true iff Sorted is a
	% list containing the elements of As and Bs in the order implied
	% by their sorted merge. The ordering of elements is defined by
	% the higher order comparison predicate Compare.
:- pred list__merge(pred(X, X, comparison_result), list(X), list(X), list(X)).
:- mode list__merge(pred(in, in, out) is det, in, in, out) is det.

:- func list__merge(func(X, X) = comparison_result, list(X), list(X)) = list(X).

	% list__merge_and_remove_dups(P, As, Bs, Sorted) is true if and only if
	% Sorted is a list containing the elements of As and Bs in the order
	% implied by their sorted merge. The ordering of elements is defined by
	% the higher order comparison predicate P.
	% As and Bs must be sorted.
:- pred list__merge_and_remove_dups(pred(X, X, comparison_result),
	list(X), list(X), list(X)).
:- mode list__merge_and_remove_dups(pred(in, in, out) is det,
	in, in, out) is det.

:- func list__merge_and_remove_dups(func(X, X) = comparison_result, list(X), list(X)) = list(X).

%-----------------------------------------------------------------------------%

	% list__series(X, OK, Succ) = [X0, X1, ..., Xn]
	% 	where X0 = X and successive elements Xj, Xk
	% 	are computed as Xk = Succ(Xj).  The series
	% 	terminates as soon as an element Xi is
	% 	generated such that OK(Xi) fails; Xi is not
	% 	included in the output.
	%
:- func list__series(T, pred(T), func(T) = T) = list(T).
:- mode list__series(in, pred(in) is semidet, func(in) = out is det) = out
		is det.

% ---------------------------------------------------------------------------- %

	% Lo `..` Hi = [Lo, Lo + 1, ..., Hi] if Lo =< Hi
	%            =                    [] otherwise
	%
:- func int `..` int = list(int).

%-----------------------------------------------------------------------------%

	% list__det_head(List) returns the first element of List,
	% calling error/1 if List is empty.
:- func list__det_head(list(T)) = T.

	% list__det_tail(List) returns the tail of List,
	% calling error/1 if List is empty.
:- func list__det_tail(list(T)) = list(T).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.	% for var/1.

:- pragma type_spec(list__merge(in, in, out), T = var(_)).

:- pragma type_spec(list__merge_and_remove_dups(in, in, out), T = var(_)).
:- pragma type_spec(list__merge_and_remove_dups/2, T = var(_)).

:- pragma type_spec(list__remove_adjacent_dups/2, T = var(_)).
:- pragma type_spec(list__remove_adjacent_dups/1, T = var(_)).

:- pragma type_spec(list__member(in, in), T = var(_)).

:- pragma type_spec(list__sort_and_remove_dups/2, T = var(_)).
:- pragma type_spec(list__sort_and_remove_dups/1, T = var(_)).

:- pragma type_spec(list__sort(in, out), T = var(_)).
:- pragma type_spec(list__sort/1, T = var(_)).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bintree_set, require, std_util.

%-----------------------------------------------------------------------------%

list__is_empty([]).

list__is_not_empty([_ | _]).

list__append([], Ys, Ys).
list__append([X | Xs], Ys, [X | Zs]) :-
	list__append(Xs, Ys, Zs).

list__remove_suffix(List, Suffix, Prefix) :-
	list__length(List, ListLength),
	list__length(Suffix, SuffixLength),
	PrefixLength = ListLength - SuffixLength,
	list__split_list(PrefixLength, List, Prefix, Suffix).

%-----------------------------------------------------------------------------%

list__nth_member_search([X | Xs], Y, N) :-
	( X = Y ->
		N = 1
	;
		list__nth_member_search(Xs, Y, N0),
		N = N0 + 1
	).

%-----------------------------------------------------------------------------%

list__index0([X | Xs], N, Elem) :-
	( N = 0 ->
		Elem = X
	;
		N1 = N - 1,
		list__index0(Xs, N1, Elem)
	).

list__index0_det(List, N, Elem) :-
	( list__index0(List, N, Elem0) ->
		Elem = Elem0
	;
		error("list__index: index out of range")
	).

list__index1(List, N, Elem) :-
	N1 = N - 1,
	list__index0(List, N1, Elem).

list__index1_det(List, N, Elem) :-
	N1 = N - 1,
	list__index0_det(List, N1, Elem).

%-----------------------------------------------------------------------------%

list__condense([], []).
list__condense([L | Ls], R) :-
	list__condense(Ls, R1),
	list__append(L, R1, R).

%-----------------------------------------------------------------------------%

list__same_length([], []).
list__same_length([_ | L1], [_ | L2]) :-
	list__same_length(L1, L2).

%-----------------------------------------------------------------------------%

list__insert(Elem, List0, List) :-
	list__delete(List, Elem, List0).

%-----------------------------------------------------------------------------%

list__delete([X | L], X, L).
list__delete([X | Xs], Y, [X | L]) :-
	list__delete(Xs, Y, L).

list__delete_first([X | Xs], Y, Zs) :-
	( X = Y ->
		Zs = Xs
	;
		Zs = [X | Zs1],
		list__delete_first(Xs, Y, Zs1)
	).

list__delete_all([], _, []).
list__delete_all([X | Xs], Y, Zs) :-
	( X = Y ->
		list__delete_all(Xs, Y, Zs)
	;
		Zs = [X | Zs1],
		list__delete_all(Xs, Y, Zs1)
	).

list__delete_elems(Xs, [], Xs).
list__delete_elems(Xs, [E | Es], Zs) :-
	list__delete_all(Xs, E, Ys),
	list__delete_elems(Ys, Es, Zs).

%-----------------------------------------------------------------------------%

list__replace([X | L], X, Z, [Z | L]).
list__replace([X | Xs], Y, Z, [X | L]) :-
	list__replace(Xs, Y, Z, L).

list__replace_first([X | Xs], Y, Z, List) :-
	( X = Y ->
		List = [Z | Xs]
	;
		List = [X | L1],
		list__replace_first(Xs, Y, Z, L1)
	).

list__replace_all([], _, _, []).
list__replace_all([X | Xs], Y, Z, L) :-
	( X = Y ->
		L = [Z | L0],
		list__replace_all(Xs, Y, Z, L0)
	;
		L = [X | L0],
		list__replace_all(Xs, Y, Z, L0)
	).

list__replace_nth(Xs, P, R, L) :-
	P > 0,
	list__replace_nth_2(Xs, P, R, L).

list__replace_nth_det(Xs, P, R, L) :-
	( P > 0 ->
		(
			list__replace_nth_2(Xs, P, R, L0)
		->
			L = L0
		;
			error("list__replace_nth_det: Can't replace element whose index position is past the end of the list")
		)
	;
		error("list__replace_nth_det: Can't replace element whose index position is less than 1.")
	).

:- pred list__replace_nth_2(list(T), int, T, list(T)).
:- mode list__replace_nth_2(in, in, in, out) is semidet.

list__replace_nth_2([X | Xs], P, R, L) :-
	( P = 1 ->
		L = [R | Xs]
	;
		P1 = P - 1,
		list__replace_nth(Xs, P1, R, L0),
		L = [X | L0]
	).

%-----------------------------------------------------------------------------%

list__member(X, [X | _]).
list__member(X, [_ | Xs]) :-
	list__member(X, Xs).

list__member(Element, List, SubList) :-
	SubList = [Element | _],
	list__append(_, SubList, List).

%-----------------------------------------------------------------------------%

list__merge(A, B, C) :-
	( A = [X | Xs] ->
		( B = [Y | Ys] ->
			C = [Z | Zs],
			( compare(<, X, Y) ->
				Z = X,
				list__merge(Xs, B, Zs)
			;
				Z = Y,
				list__merge(A, Ys, Zs)
			)
		;
			C = A
		)
	;
		C = B
	).

list__merge_and_remove_dups(A, B, C) :-
	( A = [X | Xs] ->
		( B = [Y | Ys] ->
			compare(Res, X, Y),
			( Res = (<) ->
				C = [X | Zs],
				list__merge_and_remove_dups(Xs, B, Zs)
			; Res = (>) ->
				C = [Y | Zs],
				list__merge_and_remove_dups(A, Ys, Zs)
			;
				list__merge_and_remove_dups(Xs, B, C)
			)
		;
			C = A
		)
	;
		C = B
	).

%-----------------------------------------------------------------------------%

% Note - it is not possible to write a version of
% list__length/1 in pure Mercury that works in both directions
% unless you make it semidet rather than det.

list__length(L, N) :-
	list__length_2(L, 0, N).

:- pred list__length_2(list(T), int, int).
:- mode list__length_2(in, in, out) is det.

list__length_2([], N, N).
list__length_2([_ | L1], N0, N) :-
	N1 = N0 + 1,
	list__length_2(L1, N1, N).

%-----------------------------------------------------------------------------%

list__reverse(L0, L) :-
	list__reverse_2(L0, [], L).

:- pred list__reverse_2(list(T), list(T), list(T)).
:- mode list__reverse_2(in, in, out) is det.

list__reverse_2([], L, L).
list__reverse_2([X | Xs], L0, L) :-
	list__reverse_2(Xs, [X | L0], L).

%-----------------------------------------------------------------------------%

list__sort(L0, L) :-
	list__merge_sort(L0, L).

list__sort_and_remove_dups(L0, L) :-
	list__merge_sort(L0, L1),
	list__remove_adjacent_dups(L1, L).

:- pred list__qsort(list(T), list(T), list(T)).
:- mode list__qsort(in, in, out) is det.

list__qsort([], R, R).
list__qsort([X | L], R0, R) :-
	list__partition(L, X, L1, L2),
	list__qsort(L2, R0, R1),
	list__qsort(L1, [X | R1], R).

:- pred list__partition(list(T), T, list(T), list(T)).
:- mode list__partition(in, in, out, out) is det.

list__partition([], _, [], []).
list__partition([Head | Tail], Partition, Low, High) :-
	( compare(<, Head, Partition) ->
		list__partition(Tail, Partition, Low1, High),
		Low = [Head | Low1]
	;
		list__partition(Tail, Partition, Low, High1),
		High = [Head | High1]
	).

%-----------------------------------------------------------------------------%

:- pred list__merge_sort(list(T), list(T)).
:- mode list__merge_sort(in, out) is det.
:- pragma type_spec(list__merge_sort(in, out), T = var(_)).

list__merge_sort(List, SortedList) :-
	list__merge_sort_2(list__length(List), List, SortedList).

:- pred list__merge_sort_2(int, list(T), list(T)).
:- mode list__merge_sort_2(in, in, out) is det.
:- pragma type_spec(list__merge_sort_2(in, in, out), T = var(_)).

list__merge_sort_2(Length, List, SortedList) :-
	( Length > 1 ->
		HalfLength = Length // 2,
		( list__split_list(HalfLength, List, Front, Back) ->
			list__merge_sort_2(HalfLength, Front, SortedFront),
			list__merge_sort_2(Length - HalfLength,
				Back, SortedBack),
			list__merge(SortedFront, SortedBack, SortedList)
		;
			error("list__merge_sort_2")
		)
	;
		SortedList = List
	).

%-----------------------------------------------------------------------------%

list__remove_dups(Xs, Ys) :-
	bintree_set__init(Zs0),
	list__remove_dups_2(Xs, Zs0, Ys).

:- pred list__remove_dups_2(list(T), bintree_set(T), list(T)).
:- mode list__remove_dups_2(in, in, out) is det.

list__remove_dups_2([], _SoFar, []).
list__remove_dups_2([X | Xs], SoFar0, Zs) :-
	(
		bintree_set__member(X, SoFar0)
	->
		list__remove_dups_2(Xs, SoFar0, Zs)
	;
		bintree_set__insert(SoFar0, X, SoFar),
		list__remove_dups_2(Xs, SoFar, Ys),
		Zs = [X | Ys]
	).

%-----------------------------------------------------------------------------%

list__remove_adjacent_dups([], []).
list__remove_adjacent_dups([X | Xs], L) :-
	list__remove_adjacent_dups_2(Xs, X, L).

:- pred list__remove_adjacent_dups_2(list(T), T, list(T)).
:- mode list__remove_adjacent_dups_2(in, in, out) is det.
:- pragma type_spec(list__remove_adjacent_dups_2/3, T = var(_)).

list__remove_adjacent_dups_2([], X, [X]).
list__remove_adjacent_dups_2([X1 | Xs], X0, L) :-
	(X0 = X1 ->
		list__remove_adjacent_dups_2(Xs, X1, L)
	;
		L = [X0 | L0],
		list__remove_adjacent_dups_2(Xs, X1, L0)
	).

%-----------------------------------------------------------------------------%

list__zip([], Bs, Bs).
list__zip([A | As], Bs, [A | Cs]) :-
	list__zip2(As, Bs, Cs).

:- pred list__zip2(list(T), list(T), list(T)).
:- mode list__zip2(in, in, out) is det.

list__zip2(As, [], As).
list__zip2(As, [B | Bs], [B | Cs]) :-
	list__zip(As, Bs, Cs).

%-----------------------------------------------------------------------------%

/**** unused
:- pred list__split3(list(T), list(T), list(T), list(T)).
:- mode list__split3(in, out, in, in) is semidet.

list__split3(As, Bs, Cs, Ds) :-
	list__length(As, AL),
	list__length(Cs, CL),
	list__length(Ds, DL),
	N1 = AL + CL,
	BL = DL - N1,
	N2 = AL + BL,
	list__take(AL, Ds, As),
	list__drop(N2, Ds, Cs),
	list__drop(AL, Ds, Ts),
	list__take(BL, Ts, Bs).
*****/

%-----------------------------------------------------------------------------%

list__split_list(N, List, Start, End) :-
	( N = 0 ->
		Start = [],
		End = List
	;
		N > 0,
		N1 = N - 1,
		List = [Head | List1],
		Start = [Head | Start1],
		list__split_list(N1, List1, Start1, End)
	).

list__take(N, As, Bs) :-
	( N > 0 ->
		N1 = N - 1,
		As = [A | As1],
		Bs = [A | Bs1],
		list__take(N1, As1, Bs1)
	;
		Bs = []
	).

list__take_upto(N, As, Bs) :-
	( list__take(N, As, Bs0) ->
		Bs = Bs0
	;
		Bs = As
	).

list__drop(N, As, Bs) :-
	( N > 0 ->
		N1 = N - 1,
		As = [_ | Cs],
		list__drop(N1, Cs, Bs)
	;
		As = Bs
	).

%-----------------------------------------------------------------------------%

list__duplicate(N, X, L) :-
	( N > 0 ->
		N1 = N - 1,
		L = [X | L1],
		list__duplicate(N1, X, L1)
	;
		L = []
	).

%-----------------------------------------------------------------------------%

list__chunk(List, ChunkSize, ListOfSmallLists) :-
	list__chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred list__chunk_2(list(T), int, list(T), int, list(list(T))).
:- mode list__chunk_2(in, in, in, in, out) is det.

list__chunk_2([], _ChunkSize, List0, _N, Lists) :-
	( List0 = [] ->
		Lists = []
	;
		list__reverse(List0, List),
		Lists = [List]
	).
list__chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
	( N > 1 ->
		N1 = N - 1,
		list__chunk_2(Xs, ChunkSize, [X | List0], N1, Lists)
	;
		list__reverse([X | List0], List),
		Lists = [List | Lists1],
		list__chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1)
	).

%-----------------------------------------------------------------------------%

list__perm([], []).
list__perm([X | Xs], Ys) :-
	list__perm(Xs, Ys0),
	list__insert(X, Ys0, Ys).

%-----------------------------------------------------------------------------%

list__sublist([], _).
list__sublist([SH | ST], [FH | FT]) :-
	( SH = FH ->
		list__sublist(ST, FT)
	;
		list__sublist([SH | ST], FT)
	).

%-----------------------------------------------------------------------------%

list__all_same([]).
list__all_same([H | T]) :-
	list__all_same_2(H, T).

:- pred list__all_same_2(T, list(T)).
:- mode list__all_same_2(in, in) is semidet.

list__all_same_2(_, []).
list__all_same_2(H, [H | T]) :-
	list__all_same_2(H, T).

%-----------------------------------------------------------------------------%

list__last([H | T], Last) :-
	(
		T = [],
		Last = H
	;
		T = [_ | _],
		list__last(T, Last)
	).

list__last_det(List, Last) :-
	( list__last(List, LastPrime) ->
		Last = LastPrime
	;
		error("list__last_det: empty list")
	).

list__split_last([H | T], AllButLast, Last) :-
	(
		T = [],
		AllButLast = [],
		Last = H
	;
		T = [_ | _],
		list__split_last(T, AllButLast1, Last),
		AllButLast = [H | AllButLast1]
	).

list__split_last_det(List, AllButLast, Last) :-
	( list__split_last(List, AllButLastPrime, LastPrime) ->
		AllButLast = AllButLastPrime,
		Last = LastPrime
	;
		error("list__split_last_det: empty list")
	).

%-----------------------------------------------------------------------------%

list__map(_, [],  []).
list__map(P, [H0 | T0], [H | T]) :-
	call(P, H0, H),
	list__map(P, T0, T).

list__map2(_, [],  [],  []).
list__map2(P, [H0 | T0], [H1 | T1], [H2 | T2]) :-
	call(P, H0, H1, H2),
	list__map2(P, T0, T1, T2).

list__map3(_, [],  [],  [],  []).
list__map3(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3]) :-
	call(P, H0, H1, H2, H3),
	list__map3(P, T0, T1, T2, T3).

list__map_corresponding(_, [],       []      ) = [].

list__map_corresponding(_, [],       [_ | _] ) =
	func_error("list__map_corresponding/3: mismatched list arguments").

list__map_corresponding(_, [_ | _],  []      ) =
	func_error("list__map_corresponding/3: mismatched list arguments").

list__map_corresponding(F, [A | As], [B | Bs]) =
	[F(A, B) | list__map_corresponding(F, As, Bs)].

list__map_corresponding3(F, As, Bs, Cs) =

	( if      As = [A | As0], Bs = [B | Bs0], Cs = [C | Cs0]
	  then    [F(A, B, C) | list__map_corresponding3(F, As0, Bs0, Cs0)]

	  else if As = [],        Bs = [],        Cs = []
	  then    []

	  else    func_error("list__map_corresponding3: \
mismatched list arguments")
	).

list__filter_map_corresponding(_, [],       []      ) = [].

list__filter_map_corresponding(_, [],       [_ | _] ) =
	func_error("list__filter_map_corresponding/3: \
mismatched list arguments").

list__filter_map_corresponding(_, [_ | _],  []      ) =
	func_error("list__filter_map_corresponding/3: \
mismatched list arguments").

list__filter_map_corresponding(F, [A | As], [B | Bs]) =
	( if   F(A, B) = C
	  then [C | list__filter_map_corresponding(F, As, Bs)]
	  else list__filter_map_corresponding(F, As, Bs)
	).

list__filter_map_corresponding3(F, As, Bs, Cs) =

	( if      As = [A | As0], Bs = [B | Bs0], Cs = [C | Cs0]
	  then
	  	  ( if   F(A, B, C) = D
		    then [D | list__filter_map_corresponding3(F, As0, Bs0, Cs0)]
		    else list__filter_map_corresponding3(F, As0, Bs0, Cs0)
		  )

	  else if As = [],        Bs = [],        Cs = []
	  then    []

	  else    func_error("list__filter_map_corresponding3: \
mismatched list arguments")
	).

list__foldl(_, [], Acc, Acc).
list__foldl(P, [H | T], Acc0, Acc) :-
	call(P, H, Acc0, Acc1),
	list__foldl(P, T, Acc1, Acc).

list__foldl2(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
list__foldl2(P, [H | T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
	call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
	list__foldl2(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc).

list__foldl3(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc, ThirdAcc, ThirdAcc).
list__foldl3(P, [H | T], FirstAcc0, FirstAcc, SecAcc0, SecAcc,
		ThirdAcc0, ThirdAcc) :-
	call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1,
		ThirdAcc0, ThirdAcc1),
	list__foldl3(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc,
		ThirdAcc1, ThirdAcc).

list__map_foldl(_, [],  []) -->
	[].
list__map_foldl(P, [H0 | T0], [H | T]) -->
	call(P, H0, H),
	list__map_foldl(P, T0, T).

list__map_foldl2(_, [], [], A, A) --> [].
list__map_foldl2(P, [H0 | T0], [H | T], A0, A) -->
	call(P, H0, H, A0, A1),
	list__map_foldl2(P, T0, T, A1, A).

list__foldr(_, [], Acc, Acc).
list__foldr(P, [H | T], Acc0, Acc) :-
	list__foldr(P, T, Acc0, Acc1),
	call(P, H, Acc1, Acc).

list__filter(P, Xs, Ys) :-
	list__filter(P, Xs, Ys, _).

list__filter(_, [],  [], []).
list__filter(P, [H | T], L, M) :-
	( call(P, H) ->
		L = [H | L1],
		M = M1
	;
		L = L1,
		M = [H | M1]
	),
	list__filter(P, T, L1, M1).

list__filter_map(_, [],  []).
list__filter_map(P, [H0 | T0], L) :-
	( call(P, H0, H) ->
		L = [H | L1]
	;
		L = L1
	),
	list__filter_map(P, T0, L1).

list__filter_map(_, [], [], []).
list__filter_map(P, [H0 | T0], L, M) :-
	( call(P, H0, H) ->
		L = [H | L1],
		M = M1
	;
		L = L1,
		M = [H0 | M1]
	),
	list__filter_map(P, T0, L1, M1).

list__takewhile(_, [], [], []).
list__takewhile(P, [X | Xs], Ins, Outs) :-
	( call(P, X) ->
		Ins = [X | Ins0],
		list__takewhile(P, Xs, Ins0, Outs)
	;
		Ins = [],
		Outs = [X | Xs]
	).

list__sort_and_remove_dups(P, L0, L) :-
	list__sort(P, L0, L1),
	list__remove_adjacent_dups(L1, L).

list__sort(P, L0, L) :-
	list__length(L0, N),
	(
		N = 0
	->
		L = []
	;
		list__hosort(P, N, L0, L1, [])
	->
		L = L1
	;
		error("hosort failed")
	).

% list__hosort is actually det but the compiler can't confirm it
:- pred list__hosort(pred(X, X, comparison_result), int, list(X),
	list(X), list(X)).
:- mode list__hosort(pred(in, in, out) is det, in, in, out, out) is semidet.

	% list__hosort is a Mercury implementation of the mergesort
	% described in The Craft of Prolog.
	% N denotes the length of the part of L0 that this call is sorting.
	% 		(require((length(L0, M), M >= N)))
	% Since we have redundant information about the list (N, and the
	% length implicit in the list itself), we get a semidet unification
	% when we deconstruct the list.
list__hosort(P, N, L0, L, Rest) :-
	(
		N = 1
	->
		L0 = [X | Rest],
		L = [X]
	;
		N = 2
	->
		L0 = [X, Y | Rest],
		call(P, X, Y, C),
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
		list__hosort(P, N1, L0, L1, Middle),
		N2 = N - N1,
		list__hosort(P, N2, Middle, L2, Rest),
		list__merge(P, L1, L2, L)
	).

list__merge(_P, [], [], []).
list__merge(_P, [], [Y | Ys], [Y | Ys]).
list__merge(_P, [X | Xs], [], [X | Xs]).
list__merge(P, [H1 | T1], [H2 | T2], L) :-
	call(P, H1, H2, C),
	(
		C = (<),
		L = [H1 | T],
		list__merge(P, T1, [H2 | T2], T)
	;
		C = (=),
		L = [H1, H2 | T],
		list__merge(P, T1, T2, T)
	;
		C = (>),
		L = [H2 | T],
		list__merge(P, [H1 | T1], T2, T)
	).

list__merge_and_remove_dups(_P, [], [], []).
list__merge_and_remove_dups(_P, [], [Y | Ys], [Y | Ys]).
list__merge_and_remove_dups(_P, [X | Xs], [], [X | Xs]).
list__merge_and_remove_dups(P, [H1 | T1], [H2 | T2], L) :-
	call(P, H1, H2, C),
	(
		C = (<),
		L = [H1 | T],
		list__merge_and_remove_dups(P, T1, [H2 | T2], T)
	;
		C = (=),
		L = [H1  |  T],
		list__merge_and_remove_dups(P, T1, T2, T)
	;
		C = (>),
		L = [H2 | T],
		list__merge_and_remove_dups(P, [H1 | T1], T2, T)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 27/04/99
%       Function forms added.

list__det_head([]) = _ :- error("list__det_head/1: empty list as argument").
list__det_head([X | _]) = X.

list__det_tail([]) = _ :- error("list__det_tail/1: empty list as argument").
list__det_tail([_ | Xs]) = Xs.

list__append(Xs, Ys) = Zs :-
	list__append(Xs, Ys, Zs).

list__merge(Xs, Ys) = Zs :-
	list__merge(Xs, Ys, Zs).

list__merge_and_remove_dups(Xs, Ys) = Zs :-
	list__merge_and_remove_dups(Xs, Ys, Zs).

list__remove_adjacent_dups(Xs) = Ys :-
	list__remove_adjacent_dups(Xs, Ys).

list__remove_dups(Xs) = Ys :-
	list__remove_dups(Xs, Ys).

list__length(Xs) = N :-
	list__length(Xs, N).

list__take_upto(N, Xs) = Ys :-
	list__take_upto(N, Xs, Ys).

list__delete_all(Xs, A) = Ys :-
	list__delete_all(Xs, A, Ys).

list__delete_elems(Xs, Ys) = Zs :-
	list__delete_elems(Xs, Ys, Zs).

list__replace_all(Xs, A, B) = Ys :-
	list__replace_all(Xs, A, B, Ys).

list__replace_nth_det(Xs, N, A) = Ys :-
	list__replace_nth_det(Xs, N, A, Ys).

list__sort_and_remove_dups(Xs) = Ys :-
	list__sort_and_remove_dups(Xs, Ys).

list__sort(Xs) = Ys :-
	list__sort(Xs, Ys).

list__reverse(Xs) = Ys :-
	list__reverse(Xs, Ys).

list__index0_det(Xs, N) = A :-
	list__index0_det(Xs, N, A).

list__index1_det(Xs, N) = A :-
	list__index1_det(Xs, N, A).

list__zip(Xs, Ys) = Zs :-
	list__zip(Xs, Ys, Zs).

list__duplicate(N, A) = Xs :-
	list__duplicate(N, A, Xs).

list__condense(Xss) = Ys :-
	list__condense(Xss, Ys).

list__chunk(Xs, N) = Ys :-
	list__chunk(Xs, N, Ys).

list__map(F, Xs) = Ys :-
	P = ( pred(X::in, Y::out) is det :- Y = F(X) ),
	list__map(P, Xs, Ys).

list__foldl(F, Xs, A) = B :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	list__foldl(P, Xs, A, B).

list__foldr(F, Xs, A) = B :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	list__foldr(P, Xs, A, B).

list__filter(P, Xs) = Ys :-
	list__filter(P, Xs, Ys).

list__filter_map(F, Xs) = Ys :-
	P = ( pred(X::in, Y::out) is semidet :- Y = F(X) ),
	list__filter_map(P, Xs, Ys).

list__sort(F, Xs) = Ys :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	list__sort(P, Xs, Ys).

list__merge(F, Xs, Ys) = Zs :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	list__merge(P, Xs, Ys, Zs).

list__merge_and_remove_dups(F, Xs, Ys) = Zs :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	list__merge_and_remove_dups(P, Xs, Ys, Zs).

% ---------------------------------------------------------------------------- %

L1 ++ L2 = list__append(L1, L2).

% ---------------------------------------------------------------------------- %

list__series(I, OK, Succ) =
	( if OK(I) then
		[I | list__series(Succ(I), OK, Succ)]
	  else
	  	[]
	).

% ---------------------------------------------------------------------------- %

Lo `..` Hi =
	list__series(Lo, ( pred(I::in) is semidet :- I =< Hi ), plus(1)).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
