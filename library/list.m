%---------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
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
	% (Note that partial instantiation is not currently supported.)

:- inst list_skel(I) ---> [] ; [I | list_skel(I)].
:- inst list_skel == list_skel(free).
:- inst list(I) == list_skel(I).

:- inst non_empty_list ---> [ground | ground].

:- mode in_list_skel  == list_skel >> list_skel.
:- mode out_list_skel == free >> list_skel.
:- mode list_skel_out == list_skel >> ground.

	% These more verbose versions are deprecated.
	% They exist only for backwards compatibility,
	% and will be removed in a future release.
:- mode input_list_skel  == in_list_skel.
:- mode output_list_skel == out_list_skel.
:- mode list_skel_output == list_skel_out.

	% These modes are particularly useful for passing around lists
	% of higher order terms, since they have complicated insts
	% which are not correctly approximated by "ground".
:- mode list_skel_in(I)  == list_skel(I) >> list_skel(I).
:- mode list_skel_out(I) == free >> list_skel(I).

%-----------------------------------------------------------------------------%

:- pred list__is_empty(list(T)::in) is semidet.

:- pred list__is_not_empty(list(T)::in) is semidet.

	% list__cons(X, Y, Z) <=> Z = [X | Y].
	%
:- pred list__cons(T, list(T), list(T)).
:- mode list__cons(in, in, out) is det.
:- func list__cons(T, list(T)) = list(T).

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
	%
:- pred list__remove_suffix(list(T)::in, list(T)::in, list(T)::out) is semidet.

	% list__merge(L1, L2, L):
	%	L is the result of merging the elements of L1 and L2,
	%	in ascending order.  L1 and L2 must be sorted.
	%
:- pred list__merge(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list__merge(list(T), list(T)) = list(T).

	% list__merge_and_remove_dups(L1, L2, L):
	%	L is the result of merging the elements of L1 and L2,
	%	in ascending order, and eliminating any duplicates.
	%	L1 and L2 must be sorted and must each not contain any
	%	duplicates.
	%
:- pred list__merge_and_remove_dups(list(T)::in, list(T)::in, list(T)::out)
	is det.
:- func list__merge_and_remove_dups(list(T), list(T)) = list(T).

	% list__remove_adjacent_dups(L0, L) :
	%	L is the result of replacing every sequence of duplicate
	%	elements in L0 with a single such element.
	%
:- pred list__remove_adjacent_dups(list(T)::in, list(T)::out) is det.
:- func list__remove_adjacent_dups(list(T)) = list(T).

	% list__remove_dups(L0, L) :
	%	L is the result of deleting the second and subsequent
	%	occurrences of every element that occurs twice in L0.
	%
:- pred list__remove_dups(list(T)::in, list(T)::out) is det.
:- func list__remove_dups(list(T)) = list(T).

	% list__member(Elem, List) :
	%	True iff `List' contains `Elem'.
	%
:- pred list__member(T, list(T)).
:- mode list__member(in, in) is semidet.
:- mode list__member(out, in) is nondet.

	% list__member(Elem, List, SubList) :
	%	True iff `List' contains `Elem', and `SubList' is
	%	a suffix of `List' beginning with `Elem'.
	%	Same as `SubList = [Elem | _], list__append(_, SubList, List)'.
	%
:- pred list__member(T::out, list(T)::in, list(T)::out) is nondet.

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
	% XXX The current mode checker can't handle these modes.
% :- mode list__same_length(in, output_list_skel) is det.
% :- mode list__same_length(output_list_skel, in) is det.
:- mode list__same_length(in, in) is semidet.
% XXX The current mode checker can't handle these modes
% :- mode list__same_length(input_list_skel, output_list_skel) is det.
% :- mode list__same_length(output_list_skel, input_list_skel) is det.

	% list__split_list(Len, List, Start, End):
	%	splits `List' into a prefix `Start' of length `Len',
	%	and a remainder `End'.
	%	See also: list__take, list__drop.
	%
:- pred list__split_list(int::in, list(T)::in, list(T)::out, list(T)::out)
	is semidet.

	% list__take(Len, List, Start):
	%	`Start' is the first `Len' elements of `List'.
	%	Fails if `List' has less than `Len' elements.
	%	See also: list__split_list.
	%
:- pred list__take(int::in, list(T)::in, list(T)::out) is semidet.

	% list__take_upto(Len, List, Start):
	%	`Start' is the first `Len' elements of `List'.
	%	If `List' has less than `Len' elements, return the entire list.
	%
:- pred list__take_upto(int::in, list(T)::in, list(T)::out) is det.
:- func list__take_upto(int, list(T)) = list(T).

	% list__drop(Len, List, End):
	%	`End' is the remainder of `List' after removing the
	%	first `Len' elements.
	%	See also: list__split_list.
	%
:- pred list__drop(int::in, list(T)::in, list(T)::out) is semidet.

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
	% and List is List0 with the first occurrence of Elem removed.
	%
:- pred list__delete_first(list(T)::in, T::in, list(T)::out) is semidet.

	% list__delete_all(List0, Elem, List) is true iff List is List0 with
	% all occurrences of Elem removed.
	%
:- pred list__delete_all(list(T), T, list(T)).
:- mode list__delete_all(di, in, uo) is det.
:- mode list__delete_all(in, in, out) is det.

	% list__delete_elems(List0, Elems, List) is true iff List is List0 with
	% all occurrences of all elements of Elems removed.
	%
:- pred list__delete_elems(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list__delete_elems(list(T), list(T)) = list(T).

	% list__replace(List0, D, R, List) is true iff List is List0
	% with an occurrence of D replaced with R.
	%
:- pred list__replace(list(T), T, T, list(T)).
:- mode list__replace(in, in, in, in) is semidet.
:- mode list__replace(in, in, in, out) is nondet.

	% list__replace_first(List0, D, R, List) is true iff List is List0
	% with the first occurrence of D replaced with R.
	%
:- pred list__replace_first(list(T)::in, T::in, T::in, list(T)::out)
	is semidet.

	% list__replace_all(List0, D, R, List) is true iff List is List0
	% with all occurrences of D replaced with R.
	%
:- pred list__replace_all(list(T)::in, T::in, T::in, list(T)::out) is det.
:- func list__replace_all(list(T), T, T) = list(T).

	% list__replace_nth(List0, N, R, List) is true iff List is List0
	% with Nth element replaced with R.
	% Fails if N < 1 or if length of List0 < N.
	% (Position numbers start from 1.)
	%
:- pred list__replace_nth(list(T)::in, int::in, T::in, list(T)::out)
	is semidet.

	% list__replace_nth_det(List0, N, R, List) is true iff List is List0
	% with Nth element replaced with R.
	% Aborts if N < 1 or if length of List0 < N.
	% (Position numbers start from 1.)
	%
:- pred list__replace_nth_det(list(T)::in, int::in, T::in, list(T)::out)
	is det.
:- func list__replace_nth_det(list(T), int, T) = list(T).

:- func list__det_replace_nth(list(T), int, T) = list(T).

	% list__sort_and_remove_dups(List0, List):
	%	List is List0 sorted with the second and subsequent
	%	occurrence of any duplicates removed.
	%
:- pred list__sort_and_remove_dups(list(T)::in, list(T)::out) is det.
:- func list__sort_and_remove_dups(list(T)) = list(T).

	% list__sort(List0, List):
	%	List is List0 sorted.
	%
:- pred list__sort(list(T)::in, list(T)::out) is det.
:- func list__sort(list(T)) = list(T).

	% list__reverse(List, Reverse):
	%	`Reverse' is a list containing the same elements as `List'
	%	but in reverse order.
	%
:- pred list__reverse(list(T)::in, list(T)::out) is det.
:- func list__reverse(list(T)) = list(T).

	% list__perm(List0, List):
	%	True iff `List' is a permutation of `List0'.
	%
:- pred	list__perm(list(T)::in, list(T)::out) is multi.

	% list__nth_member_search(List, Elem, Position):
	%	Elem is the Position'th member of List.
	% 	(Position numbers start from 1.)
	%
:- pred list__nth_member_search(list(T)::in, T::in, int::out) is semidet.

	% A deterministic version of list__nth_member_search, which aborts
	% instead of failing if the element is not found in the list.
	%
:- pred list__nth_member_lookup(list(T)::in, T::in, int::out) is det.

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
:- func list__det_index0(list(T), int) = T.
:- func list__det_index1(list(T), int) = T.

	% list__zip(ListA, ListB, List):
	%	List is the result of alternating the elements
	%	of ListA and ListB, starting with the first element
	%	of ListA (followed by the first element of ListB,
	%	then the second element of listA, then the second
	%	element of ListB, etc.).  When there are no more
	%	elements remaining in one of the lists,
	% 	the remainder of the nonempty list is appended.
	%
:- pred list__zip(list(T)::in, list(T)::in, list(T)::out) is det.
:- func list__zip(list(T), list(T)) = list(T).

	% list__duplicate(Count, Elem, List) is true iff List is a list
	% containing Count duplicate copies of Elem.
	%
:- pred list__duplicate(int::in, T::in, list(T)::out) is det.
:- func list__duplicate(int, T) = list(T).

	% list__condense(ListOfLists, List):
	%	`List' is the result of concatenating all the
	%	elements of `ListOfLists'.
	%
:- pred list__condense(list(list(T))::in, list(T)::out) is det.
:- func list__condense(list(list(T))) = list(T).

	% list__chunk(List, ChunkSize, Chunks):
	%	Takes a list `List' and breaks it into a list of lists `Chunks',
	%	such that the length of each list in `Chunks' is at most
	%	`ChunkSize.  (More precisely, the length of each list in
	%	`Chunks' other than the last one is exactly `ChunkSize',
	%	and the length of the last list in `Chunks' is between one
	%	and `ChunkSize'.)
	%
:- pred list__chunk(list(T)::in, int::in, list(list(T))::out) is det.
:- func list__chunk(list(T), int) = list(list(T)).

	% list__sublist(SubList, FullList) is true
	%	if one can obtain SubList by starting with FullList
	%	and deleting some of its elements.
	%
:- pred list__sublist(list(T)::in, list(T)::in) is semidet.

	% list__all_same(List) is true
	% 	if all elements of the list are the same
	%
:- pred list__all_same(list(T)::in) is semidet.

	% list__last(List, Last) is true
	%	if Last is the last element of List.
	%
:- pred list__last(list(T)::in, T::out) is semidet.

	% A deterministic version of list__last, which aborts instead of
	% failing if the input list is empty.
	%
:- pred list__last_det(list(T)::in, T::out) is det.
:- pred list__det_last(list(T)::in, T::out) is det.
:- func list__det_last(list(T)) = T.

	% list__split_last(List, AllButLast, Last) is true
	%	if Last is the last element of List and AllButLast is the list
	%	of elements before it.
	%
:- pred list__split_last(list(T)::in, list(T)::out, T::out) is semidet.

	% A deterministic version of list__split_last, which aborts instead of
	% failing if the input list is empty.
	%
:- pred list__split_last_det(list(T)::in, list(T)::out, T::out) is det.
:- pred list__det_split_last(list(T)::in, list(T)::out, T::out) is det.

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
:- mode list__map(pred(in, out) is cc_multi, in, out) is cc_multi.
:- mode list__map(pred(in, out) is semidet, in, out) is semidet.
:- mode list__map(pred(in, out) is multi, in, out) is multi.
:- mode list__map(pred(in, out) is nondet, in, out) is nondet.
:- mode list__map(pred(in, in) is semidet, in, in) is semidet.

:- func list__map(func(X) = Y, list(X)) = list(Y).

	% list__map2(T, L, M1, M2) uses the closure T
	% to transform the elements of L into the elements of M1 and M2.
:- pred list__map2(pred(A, B, C), list(A), list(B), list(C)).
:- mode list__map2(pred(in, out, out) is det, in, out, out) is det.
:- mode list__map2(pred(in, out, out) is cc_multi, in, out, out) is cc_multi.
:- mode list__map2(pred(in, out, out) is semidet, in, out, out) is semidet.
:- mode list__map2(pred(in, out, out) is multi, in, out, out) is multi.
:- mode list__map2(pred(in, out, out) is nondet, in, out, out) is nondet.
:- mode list__map2(pred(in, in, in) is semidet, in, in, in) is semidet.

	% list__map3(T, L, M1, M2, M3) uses the closure T
	% to transform the elements of L into the elements of M1, M2 and M3.
:- pred list__map3(pred(A, B, C, D), list(A), list(B), list(C), list(D)).
:- mode list__map3(pred(in, out, out, out) is det, in, out, out, out) is det.
:- mode list__map3(pred(in, out, out, out) is cc_multi, in, out, out, out)
	is cc_multi.
:- mode list__map3(pred(in, out, out, out) is semidet, in, out, out, out)
	is semidet.
:- mode list__map3(pred(in, out, out, out) is multi, in, out, out, out)
	is multi.
:- mode list__map3(pred(in, out, out, out) is nondet, in, out, out, out)
	is nondet.
:- mode list__map3(pred(in, in, in, in) is semidet, in, in, in, in) is semidet.

	% list__map4(T, L, M1, M2, M3, M4) uses the closure T
	% to transform the elements of L into the elements of M1, M2, M3 and 
	% M4.
:- pred list__map4(pred(A, B, C, D, E), list(A), list(B), list(C), list(D),
	list(E)).
:- mode list__map4(pred(in, out, out, out, out) is det, in, out, out, out, out) 
	is det.
:- mode list__map4(pred(in, out, out, out, out) is cc_multi, in, out, out, out,
	out) is cc_multi.
:- mode list__map4(pred(in, out, out, out, out) is semidet, in, out, out, out,
	out) is semidet.
:- mode list__map4(pred(in, out, out, out, out) is multi, in, out, out, out,
	out) is multi.
:- mode list__map4(pred(in, out, out, out, out) is nondet, in, out, out, out,
	out) is nondet.
:- mode list__map4(pred(in, in, in, in, in) is semidet, in, in, in, in, in) 
	is semidet.

	% list__map5(T, L, M1, M2, M3, M4, M5) uses the closure T
	% to transform the elements of L into the elements of M1, M2, M3, M4 
	% and M5.
:- pred list__map5(pred(A, B, C, D, E, F), list(A), list(B), list(C), list(D),
	list(E), list(F)).
:- mode list__map5(pred(in, out, out, out, out, out) is det, in, out, out, out,
	out, out) is det.
:- mode list__map5(pred(in, out, out, out, out, out) is cc_multi, in, out, out,
	out, out, out) is cc_multi.
:- mode list__map5(pred(in, out, out, out, out, out) is semidet, in, out, out, 
	out, out, out) is semidet.
:- mode list__map5(pred(in, out, out, out, out, out) is multi, in, out, out, 
	out, out, out) is multi.
:- mode list__map5(pred(in, out, out, out, out, out) is nondet, in, out, out, 
	out, out, out) is nondet.
:- mode list__map5(pred(in, in, in, in, in, in) is semidet, in, in, in, in, in,
	in) is semidet.

	% list__map6(T, L, M1, M2, M3, M4, M5, M6) uses the closure T
	% to transform the elements of L into the elements of M1, M2, M3, M4, 
	% M5 and M6.
:- pred list__map6(pred(A, B, C, D, E, F, G), list(A), list(B), list(C), 
	list(D), list(E), list(F), list(G)).
:- mode list__map6(pred(in, out, out, out, out, out, out) is det, in, out, out, 
	out, out, out, out) is det.
:- mode list__map6(pred(in, out, out, out, out, out, out) is cc_multi, in, out,
	out, out, out, out, out) is cc_multi.
:- mode list__map6(pred(in, out, out, out, out, out, out) is semidet, in, out, 
	out, out, out, out, out) is semidet.
:- mode list__map6(pred(in, out, out, out, out, out, out) is multi, in, out, 
	out, out, out, out, out) is multi.
:- mode list__map6(pred(in, out, out, out, out, out, out) is nondet, in, out, 
	out, out, out, out, out) is nondet.
:- mode list__map6(pred(in, in, in, in, in, in, in) is semidet, in, in, in, in,
	in, in, in) is semidet.

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
:- func list__map_corresponding3(func(A, B, C) = D, list(A), list(B), list(C))
	= list(D).

	% list__filter_map_corresponding/3 is like list__map_corresponding/3
	% except the function argument is semidet and the output list
	% consists of only those applications of the function argument that
	% succeeded.
	%
:- func list__filter_map_corresponding(func(A, B) = C, list(A), list(B))
	= list(C).
:- mode list__filter_map_corresponding(func(in, in) = out is semidet, in, in)
	= out is det.

	% list__filter_map_corresponding3/4 is like list__map_corresponding3/4
	% except the function argument is semidet and the output list
	% consists of only those applications of the function argument that
	% succeeded.
	%
:- func list__filter_map_corresponding3(func(A, B, C) = D,
	list(A), list(B), list(C)) = list(D).
:- mode list__filter_map_corresponding3(func(in, in, in) = out is semidet,
	in, in, in) = out is det.

	% list__foldl(Pred, List, Start, End) calls Pred with each
	% element of List (working left-to-right) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
	%
:- pred list__foldl(pred(L, A, A), list(L), A, A).
:- mode list__foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list__foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list__foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list__foldl(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode list__foldl(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- func list__foldl(func(L, A) = A, list(L), A) = A.

	% list__foldr(Pred, List, Start, End) calls Pred with each
	% element of List (working right-to-left) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
	% 
:- pred list__foldr(pred(L, A, A), list(L), A, A).
:- mode list__foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list__foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode list__foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode list__foldr(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode list__foldr(pred(in, in, out) is cc_multi, in, in, out) is cc_multi.

:- func list__foldr(func(L, A) = A, list(L), A) = A.

	% list__foldl2(Pred, List, !Acc1, !Acc2)
	% Does the same job as list__foldl, but with two accumulators.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
	% 
:- pred list__foldl2(pred(L, A, A, Z, Z), list(L), A, A, Z, Z).
:- mode list__foldl2(pred(in, in, out, in, out) is det,
	in, in, out, in, out) is det.
:- mode list__foldl2(pred(in, in, out, in, out) is cc_multi,
	in, in, out, in, out) is cc_multi.
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
:- mode list__foldl2(pred(in, in, out, mdi, muo) is cc_multi,
	in, in, out, mdi, muo) is cc_multi.
:- mode list__foldl2(pred(in, in, out, di, uo) is cc_multi,
	in, in, out, di, uo) is cc_multi.
:- mode list__foldl2(pred(in, di, uo, di, uo) is cc_multi,
	in, di, uo, di, uo) is cc_multi.

	% list__foldl3(Pred, List, !Acc1, !Acc2, !Acc3)
	% Does the same job as list__foldl, but with three accumulators.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
	% 
:- pred list__foldl3(pred(L, A, A, B, B, C, C), list(L),
	A, A, B, B, C, C).
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is det,
	in, in, out, in, out, in, out) is det.
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is cc_multi,
	in, in, out, in, out, in, out) is cc_multi.
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is semidet,
	in, in, out, in, out, in, out) is semidet.
:- mode list__foldl3(pred(in, in, out, in, out, in, out) is nondet,
	in, in, out, in, out, in, out) is nondet.
:- mode list__foldl3(pred(in, in, out, in, out, di, uo) is det,
	in, in, out, in, out, di, uo) is det.
:- mode list__foldl3(pred(in, in, out, in, out, di, uo) is cc_multi,
	in, in, out, in, out, di, uo) is cc_multi.

	% list__foldl4(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4)
	% Does the same job as list__foldl, but with four accumulators.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
	% 
:- pred list__foldl4(pred(L, A, A, B, B, C, C, D, D), list(L),
	A, A, B, B, C, C, D, D).
:- mode list__foldl4(pred(in, in, out, in, out, in, out, in, out) is det,
	in, in, out, in, out, in, out, in, out) is det.
:- mode list__foldl4(pred(in, in, out, in, out, in, out, in, out) is cc_multi,
	in, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet,
	in, in, out, in, out, in, out, in, out) is semidet.
:- mode list__foldl4(pred(in, in, out, in, out, in, out, in, out) is nondet,
	in, in, out, in, out, in, out, in, out) is nondet.
:- mode list__foldl4(pred(in, in, out, in, out, in, out, di, uo) is det,
	in, in, out, in, out, in, out, di, uo) is det.
:- mode list__foldl4(pred(in, in, out, in, out, in, out, di, uo) is cc_multi,
	in, in, out, in, out, in, out, di, uo) is cc_multi.

	% list__foldl5(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5)
	% Does the same job as list__foldl, but with five accumulators.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
	% 
:- pred list__foldl5(pred(L, A, A, B, B, C, C, D, D, E, E), list(L),
	A, A, B, B, C, C, D, D, E, E).
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
	is det,
	in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
	is cc_multi,
	in, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
	is semidet,
	in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, in, out)
	is nondet,
	in, in, out, in, out, in, out, in, out, in, out) is nondet.
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
	is det,
	in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list__foldl5(pred(in, in, out, in, out, in, out, in, out, di, uo)
	is cc_multi,
	in, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

	% list__foldl6(Pred, List, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5, !Acc6)
	% Does the same job as list__foldl, but with six accumulators.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
	%
:- pred list__foldl6(pred(L, A, A, B, B, C, C, D, D, E, E, F, F), list(L),
	A, A, B, B, C, C, D, D, E, E, F, F).
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	in, out) is det,
	in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	in, out) is cc_multi,
	in, in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	in, out) is semidet,
	in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	in, out) is nondet,
	in, in, out, in, out, in, out, in, out, in, out, in, out) is nondet.
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	di, uo) is det,
	in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list__foldl6(pred(in, in, out, in, out, in, out, in, out, in, out,
	di, uo) is cc_multi,
	in, in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.

	% list__map_foldl(Pred, InList, OutList, Start, End) calls Pred
	% with an accumulator (with the initial value of Start) on
	% each element of InList (working left-to-right) to transform
	% InList into OutList.  The final value of the accumulator is
	% returned in End.
	% 
:- pred list__map_foldl(pred(L, M, A, A), list(L), list(M), A, A).
:- mode list__map_foldl(pred(in, out, di, uo) is det, in, out, di, uo)
	is det.
:- mode list__map_foldl(pred(in, out, in, out) is det, in, out, in, out)
	is det.
:- mode list__map_foldl(pred(in, out, di, uo) is cc_multi, in, out, di, uo)
	is cc_multi.
:- mode list__map_foldl(pred(in, out, in, out) is cc_multi, in, out, in, out)
	is cc_multi.
:- mode list__map_foldl(pred(in, out, in, out) is semidet, in, out, in, out)
	is semidet.
:- mode list__map_foldl(pred(in, out, in, out) is nondet, in, out, in, out)
	is nondet.

	% Same as list__map_foldl, but with two mapped outputs.
	% 
:- pred list__map2_foldl(pred(L, M, N, A, A), list(L), list(M), list(N),
	A, A).
:- mode list__map2_foldl(pred(in, out, out, di, uo) is det, in, out, out,
	di, uo) is det.
:- mode list__map2_foldl(pred(in, out, out, in, out) is det, in, out, out,
	in, out) is det.
:- mode list__map2_foldl(pred(in, out, out, di, uo) is cc_multi, in, out, out,
	di, uo) is cc_multi.
:- mode list__map2_foldl(pred(in, out, out, in, out) is cc_multi, in, out, out,
	in, out) is cc_multi.
:- mode list__map2_foldl(pred(in, out, out, in, out) is semidet, in, out, out,
	in, out) is semidet.
:- mode list__map2_foldl(pred(in, out, out, in, out) is nondet, in, out, out,
	in, out) is nondet.

	% Same as list__map_foldl, but with two accumulators.
	%
:- pred list__map_foldl2(pred(L, M, A, A, B, B), list(L), list(M), A, A, B, B).
:- mode list__map_foldl2(pred(in, out, in, out, di, uo) is det,
	in, out, in, out, di, uo) is det.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is det,
	in, out, in, out, in, out) is det.
:- mode list__map_foldl2(pred(in, out, in, out, di, uo) is cc_multi,
	in, out, in, out, di, uo) is cc_multi.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is cc_multi,
	in, out, in, out, in, out) is cc_multi.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is semidet,
	in, out, in, out, in, out) is semidet.
:- mode list__map_foldl2(pred(in, out, in, out, in, out) is nondet,
	in, out, in, out, in, out) is nondet.

	% Same as list__map_foldl, but with three accumulators.
	%
:- pred list__map_foldl3(pred(L, M, A, A, B, B, C, C), list(L), list(M),
	A, A, B, B, C, C).
:- mode list__map_foldl3(pred(in, out, in, out, in, out, di, uo) is det,
	in, out, in, out, in, out, di, uo) is det.
:- mode list__map_foldl3(pred(in, out, in, out, in, out, in, out) is det,
	in, out, in, out, in, out, in, out) is det.
:- mode list__map_foldl3(pred(in, out, in, out, in, out, di, uo) is cc_multi,
	in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list__map_foldl3(pred(in, out, in, out, in, out, in, out) is cc_multi,
	in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__map_foldl3(pred(in, out, in, out, in, out, in, out) is semidet,
	in, out, in, out, in, out, in, out) is semidet.
:- mode list__map_foldl3(pred(in, out, in, out, in, out, in, out) is nondet,
	in, out, in, out, in, out, in, out) is nondet.

	% Same as list__map_foldl, but with four accumulators.
	%
:- pred list__map_foldl4(pred(L, M, A, A, B, B, C, C, D, D), list(L), list(M),
	A, A, B, B, C, C, D, D).
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
	is det,
	in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
	is det,
	in, out, in, out, in, out, in, out, in, out) is det.
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, di, uo)
	is cc_multi,
	in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
	is cc_multi,
	in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
	is semidet,
	in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list__map_foldl4(pred(in, out, in, out, in, out, in, out, in, out)
	is nondet,
	in, out, in, out, in, out, in, out, in, out) is nondet.

	% Same as list__map_foldl, but with five accumulators.
	%
:- pred list__map_foldl5(pred(L, M, A, A, B, B, C, C, D, D, E, E),
	list(L), list(M), A, A, B, B, C, C, D, D, E, E).
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	di, uo) is det,
	in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	in, out) is det,
	in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	di, uo) is cc_multi,
	in, out, in, out, in, out, in, out, in, out, di, uo) is cc_multi.
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	in, out) is cc_multi,
	in, out, in, out, in, out, in, out, in, out, in, out) is cc_multi.
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	in, out) is semidet,
	in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode list__map_foldl5(pred(in, out, in, out, in, out, in, out, in, out,
	in, out) is nondet,
	in, out, in, out, in, out, in, out, in, out, in, out) is nondet.

	% Same as list__map_foldl, but with six accumulators.
	%
:- pred list__map_foldl6(pred(L, M, A, A, B, B, C, C, D, D, E, E, F, F),
	list(L), list(M), A, A, B, B, C, C, D, D, E, E, F, F).
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, di, uo) is det,
	in, out, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, in, out) is det,
	in, out, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, di, uo) is cc_multi,
	in, out, in, out, in, out, in, out, in, out, in, out, di, uo)
	is cc_multi.
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, in, out) is cc_multi,
	in, out, in, out, in, out, in, out, in, out, in, out, in, out)
	is cc_multi.
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, in, out) is semidet,
	in, out, in, out, in, out, in, out, in, out, in, out, in, out)
	is semidet.
:- mode list__map_foldl6(pred(in, out, in, out, in, out, in, out, in, out,
	in, out, in, out) is nondet,
	in, out, in, out, in, out, in, out, in, out, in, out, in, out)
	is nondet.

	% list__all_true(Pred, List) takes a closure with one input argument.
	% If Pred succeeds for every member of List, all_true succeeds.
	% If Pred fails for any member of List, all_true fails.
	%
:- pred list__all_true(pred(X)::in(pred(in) is semidet), list(X)::in)
	is semidet.

	% list__all_false(Pred, List) takes a closure with one input argument.
	% If Pred fails for every member of List, all_false succeeds.
	% If Pred succeeds for any member of List, all_false fails.
	%
:- pred list__all_false(pred(X)::in(pred(in) is semidet), list(X)::in)
	is semidet.

	% list__filter(Pred, List, TrueList) takes a closure with one
	% input argument and for each member of List `X', calls the closure.
	% Iff call(Pred, X) is true, then X is included in TrueList.
	%
:- pred list__filter(pred(X)::in(pred(in) is semidet), list(X)::in,
	list(X)::out) is det.
:- func list__filter(pred(X)::in(pred(in) is semidet), list(X)::in)
	= (list(X)::out) is det.

	% list__filter(Pred, List, TrueList, FalseList) takes a closure with one
	% input argument and for each member of List `X', calls the closure.
	% Iff call(Pred, X) is true, then X is included in TrueList.
	% Iff call(Pred, X) is false, then X is included in FalseList.
	%
:- pred list__filter(pred(X)::in(pred(in) is semidet), list(X)::in,
	list(X)::out, list(X)::out) is det.

	% list__filter_map(Transformer, List, TrueList) takes a predicate
	% with one input argument and one output argument. It is called
	% with each element of List. If a call succeeds, then the output is
	% included in TrueList.
	%
:- pred list__filter_map(pred(X, Y)::in(pred(in, out) is semidet),
	list(X)::in, list(Y)::out) is det.

:- func list__filter_map(func(X) = Y, list(X)) = list(Y).
:- mode list__filter_map(func(in) = out is semidet, in) = out is det.

	% list__filter_map(Transformer, List, TrueList, FalseList) takes
	% a predicate with one input argument and one output argument.
	% It is called with each element of List. If a call succeeds,
	% then the output is included in TrueList; otherwise, the failing
	% input is included in FalseList.
	%
:- pred list__filter_map(pred(X, Y)::in(pred(in, out) is semidet),
	list(X)::in, list(Y)::out, list(X)::out) is det.

	% Same as list__filter_map/3 except that it only returns the first
	% match:
	% 	find_first_map(X, Y, Z) <=> list__filter_map(X, Y, [Z | _])
:- pred list__find_first_map(pred(X, Y)::in(pred(in, out) is semidet),
        list(X)::in, Y::out) is semidet.

	% Same as list__find_first_map, except with two outputs.
:- pred list__find_first_map2(pred(X, A, B)::in(pred(in, out, out) is semidet),
	list(X)::in, A::out, B::out) is semidet.

	% Same as list__find_first_map, except with three outputs.
:- pred list__find_first_map3(
	pred(X, A, B, C)::in(pred(in, out, out, out) is semidet),
	list(X)::in, A::out, B::out, C::out) is semidet.

	% list__takewhile(Predicate, List, UptoList, AfterList) takes a
	% closure with one input argument, and calls it on successive members
	% of List as long as the calls succeed. The elements for which
	% the call succeeds are placed in UptoList and the first element for
	% which the call fails, and all the remaining elements of List are
	% placed in AfterList.
	%
:- pred list__takewhile(pred(T)::in(pred(in) is semidet), list(T)::in,
	list(T)::out, list(T)::out) is det.

%-----------------------------------------------------------------------------%

	% list__sort(Compare, Unsorted, Sorted) is true iff Sorted is a
	% list containing the same elements as Unsorted, where Sorted is
	% sorted with respect to the ordering defined by the predicate
	% term Compare, and the elements that are equivalent in this ordering
	% appear in the same sequence in Sorted as they do in Unsorted
	% (that is, the sort is stable).
	%
:- pred list__sort(comparison_pred(X)::in(comparison_pred), list(X)::in,
	list(X)::out) is det.
:- func list__sort(comparison_func(X), list(X)) = list(X).

	% list__sort_and_remove_dups(Compare, Unsorted, Sorted) is true iff
	% Sorted is a list containing the same elements as Unsorted, where
	% Sorted is sorted with respect to the ordering defined by the
	% predicate term Compare, except that if two elements in Unsorted
	% are equivalent with respect to this ordering only the one which
	% occurs first will be in Sorted.
	%
:- pred list__sort_and_remove_dups(comparison_pred(X)::in(comparison_pred),
	list(X)::in, list(X)::out) is det.

	% list__remove_adjacent_dups(P, L0, L) is true iff L is the result
	% of replacing every sequence of elements in L0 which are equivalent
	% with respect to the ordering, with the first occurrence in L0 of
	% such an element.
	%
:- pred list__remove_adjacent_dups(comparison_pred(X)::in(comparison_pred),
	list(X)::in, list(X)::out) is det.

	% list__merge(Compare, As, Bs, Sorted) is true iff, assuming As and
	% Bs are sorted with respect to the ordering defined by Compare,
	% Sorted is a list containing the elements of As and Bs which is
	% also sorted.  For elements which are equivalent in the ordering,
	% if they come from the same list then they appear in the same
	% sequence in Sorted as they do in that list, otherwise the elements
	% from As appear before the elements from Bs.
	%
:- pred list__merge(comparison_pred(X)::in(comparison_pred),
	list(X)::in, list(X)::in, list(X)::out) is det.

:- func list__merge(comparison_func(X), list(X), list(X)) = list(X).

	% list__merge_and_remove_dups(P, As, Bs, Sorted) is true iff, assuming
	% As and Bs are sorted with respect to the ordering defined by
	% Compare and neither contains any duplicates, Sorted is a list
	% containing the elements of As and Bs which is also sorted and
	% contains no duplicates.  If an element from As is duplicated in
	% Bs (that is, they are equivalent in the ordering), then the element
	% from As is the one that appears in Sorted.
	%
:- pred list__merge_and_remove_dups(comparison_pred(X)::in(comparison_pred),
	list(X)::in, list(X)::in, list(X)::out) is det.

:- func list__merge_and_remove_dups(comparison_func(X), list(X), list(X))
	= list(X).

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

%-----------------------------------------------------------------------------%

	% Lo `..` Hi = [Lo, Lo + 1, ..., Hi] if Lo =< Hi
	%            =                    [] otherwise
	%
:- func int `..` int = list(int).

%-----------------------------------------------------------------------------%

:- func list__head(list(T)) = T is semidet.

:- func list__tail(list(T)) = list(T) is semidet.

	% list__det_head(List) returns the first element of List,
	% calling error/1 if List is empty.
	%
:- func list__det_head(list(T)) = T.

	% list__det_tail(List) returns the tail of List,
	% calling error/1 if List is empty.
	%
:- func list__det_tail(list(T)) = list(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.		% for var/1.

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

:- import_module string, set_tree234, require, std_util.

%-----------------------------------------------------------------------------%

list__is_empty([]).

list__is_not_empty([_ | _]).

list__cons(H, T, [H | T]).

list__cons(H, T) = [H | T].

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
	list__nth_member_search_2([X | Xs], Y, 1, N).

:- pred list__nth_member_search_2(list(T)::in, T::in, int::in, int::out)
	is semidet.

list__nth_member_search_2([X | Xs], Y, P, N) :-
	( X = Y ->
		N = P
	;
		list__nth_member_search_2(Xs, Y, P + 1, N)
	).

nth_member_lookup(List, Elem, Position) :-
	( list__nth_member_search(List, Elem, PositionPrime) ->
		Position = PositionPrime
	;
		error("list__nth_member_lookup/3: element not found in list")
	).

%-----------------------------------------------------------------------------%

list__index0([X | Xs], N, Elem) :-
	( N = 0 ->
		Elem = X
	;
		list__index0(Xs, N - 1, Elem)
	).

list__index0_det(List, N, Elem) :-
	( list__index0(List, N, Elem0) ->
		Elem = Elem0
	;
		error("list__index: index out of range")
	).

list__index1(List, N, Elem) :-
	list__index0(List, N - 1, Elem).

list__index1_det(List, N, Elem) :-
	list__index0_det(List, N - 1, Elem).

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
		list__delete_first(Xs, Y, Zs1),
		Zs = [X | Zs1]
	).

list__delete_all([], _, []).
list__delete_all([X | Xs], Y, Zs) :-
	( X = Y ->
		list__delete_all(Xs, Y, Zs)
	;
		list__delete_all(Xs, Y, Zs1),
		Zs = [X | Zs1]
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
		list__replace_first(Xs, Y, Z, L1),
		List = [X | L1]
	).

list__replace_all([], _, _, []).
list__replace_all([X | Xs], Y, Z, L) :-
	( X = Y ->
		list__replace_all(Xs, Y, Z, L0),
		L = [Z | L0]
	;
		list__replace_all(Xs, Y, Z, L0),
		L = [X | L0]
	).

list__replace_nth(Xs, P, R, L) :-
	P > 0,
	list__replace_nth_2(Xs, P, R, L).

list__replace_nth_det(Xs, P, R, L) :-
	( P > 0 ->
		( list__replace_nth_2(Xs, P, R, L0) ->
			L = L0
		;
			error("list__replace_nth_det: " ++
				"Can't replace element whose index " ++
				"position is past the end of the list")
		)
	;
		error("list__replace_nth_det: " ++
			"Can't replace element whose index " ++
			"position is less than 1.")
	).

:- pred list__replace_nth_2(list(T)::in, int::in, T::in, list(T)::out)
	is semidet.

list__replace_nth_2([X | Xs], P, R, L) :-
	( P = 1 ->
		L = [R | Xs]
	;
		list__replace_nth(Xs, P - 1, R, L0),
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

list__merge([], [], []).
list__merge([A | As], [], [A | As]).
list__merge([], [B | Bs], [B | Bs]).
list__merge([A | As], [B | Bs], [C | Cs]) :-
	( compare(>, A, B) ->
		C = B,
		list__merge([A | As], Bs, Cs)
	;
		% If compare((=), A, B), take A first.
		C = A,
		list__merge(As, [B | Bs], Cs)
	).

list__merge_and_remove_dups([], [], []).
list__merge_and_remove_dups([A | As], [], [A | As]).
list__merge_and_remove_dups([], [B | Bs], [B | Bs]).
list__merge_and_remove_dups([A | As], [B | Bs], [C | Cs]) :-
	compare(Res, A, B),
	(
		Res = (<),
		C = A,
		list__merge_and_remove_dups(As, [B | Bs], Cs)
	;
		Res = (=),
		C = A,
		list__merge_and_remove_dups(As, Bs, Cs)
	;
		Res = (>),
		C = B,
		list__merge_and_remove_dups([A | As], Bs, Cs)
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

:- pred list__reverse_2(list(T)::in, list(T)::in, list(T)::out) is det.

list__reverse_2([], L, L).
list__reverse_2([X | Xs], L0, L) :-
	list__reverse_2(Xs, [X | L0], L).

%-----------------------------------------------------------------------------%

list__sort(L0, L) :-
	list__merge_sort(L0, L).

list__sort_and_remove_dups(L0, L) :-
	list__merge_sort(L0, L1),
	list__remove_adjacent_dups(L1, L).

%-----------------------------------------------------------------------------%

:- pred list__merge_sort(list(T)::in, list(T)::out) is det.

:- pragma type_spec(list__merge_sort(in, out), T = var(_)).

list__merge_sort(List, SortedList) :-
	list__merge_sort_2(list__length(List), List, SortedList).

:- pred list__merge_sort_2(int::in, list(T)::in, list(T)::out) is det.

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
	list__remove_dups_2(Xs, set_tree234__init, Ys).

:- pred list__remove_dups_2(list(T)::in, set_tree234(T)::in, list(T)::out)
	is det.

list__remove_dups_2([], _SoFar, []).
list__remove_dups_2([X | Xs], SoFar0, Zs) :-
	( set_tree234__member(SoFar0, X) ->
		list__remove_dups_2(Xs, SoFar0, Zs)
	;
		set_tree234__insert(X, SoFar0, SoFar),
		list__remove_dups_2(Xs, SoFar, Ys),
		Zs = [X | Ys]
	).

%-----------------------------------------------------------------------------%

list__remove_adjacent_dups([], []).
list__remove_adjacent_dups([X | Xs], L) :-
	list__remove_adjacent_dups_2(Xs, X, L).

:- pred list__remove_adjacent_dups_2(list(T)::in, T::in, list(T)::out) is det.
:- pragma type_spec(list__remove_adjacent_dups_2/3, T = var(_)).

list__remove_adjacent_dups_2([], X, [X]).
list__remove_adjacent_dups_2([X1 | Xs], X0, L) :-
	( X0 = X1 ->
		list__remove_adjacent_dups_2(Xs, X0, L)
	;
		list__remove_adjacent_dups_2(Xs, X1, L0),
		L = [X0 | L0]
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

list__split_list(N, List, Start, End) :-
	( N = 0 ->
		Start = [],
		End = List
	;
		N > 0,
		List = [Head | List1],
		Start = [Head | Start1],
		list__split_list(N - 1, List1, Start1, End)
	).

list__take(N, As, Bs) :-
	( N > 0 ->
		As = [A | As1],
		list__take(N - 1, As1, Bs1),
		Bs = [A | Bs1]
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
		As = [_ | Cs],
		list__drop(N - 1, Cs, Bs)
	;
		As = Bs
	).

%-----------------------------------------------------------------------------%

list__duplicate(N, X, list__duplicate(N, X, [])).

:- func list__duplicate(int, T, list(T)) = list(T).

list__duplicate(N, X, Xs) =
	( N > 0 ->
		list__duplicate(N-1, X, [X|Xs])
	;
		Xs
	).

%-----------------------------------------------------------------------------%

list__chunk(List, ChunkSize, ListOfSmallLists) :-
	list__chunk_2(List, ChunkSize, [], ChunkSize, ListOfSmallLists).

:- pred list__chunk_2(list(T)::in, int::in, list(T)::in, int::in,
	list(list(T))::out) is det.

list__chunk_2([], _ChunkSize, List0, _N, Lists) :-
	( List0 = [] ->
		Lists = []
	;
		list__reverse(List0, List),
		Lists = [List]
	).
list__chunk_2([X | Xs], ChunkSize, List0, N, Lists) :-
	( N > 1 ->
		list__chunk_2(Xs, ChunkSize, [X | List0], N - 1, Lists)
	;
		list__reverse([X | List0], List),
		list__chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1),
		Lists = [List | Lists1]
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

:- pred list__all_same_2(T::in, list(T)::in) is semidet.

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

list__det_last(List, Last) :-
	list__last_det(List, Last).

list__det_last(List) = Last :-
	list__last_det(List, Last).

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

list__det_split_last(List, AllButLast, Last) :-
	list__split_last_det(List, AllButLast, Last).

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

list__map4(_, [], [], [], [], []).
list__map4(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4]) :-
	call(P, H0, H1, H2, H3, H4),
	list__map4(P, T0, T1, T2, T3, T4).

list__map5(_, [], [], [], [], [], []).
list__map5(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5])
		:-
	call(P, H0, H1, H2, H3, H4, H5),
	list__map5(P, T0, T1, T2, T3, T4, T5).

list__map6(_, [], [], [], [], [], [], []).
list__map6(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], [H5 | T5],
		[H6 | T6]) :-
	call(P, H0, H1, H2, H3, H4, H5, H6),
	list__map6(P, T0, T1, T2, T3, T4, T5, T6).

list__map_corresponding(_, [], []) = [].
list__map_corresponding(_, [], [_ | _]) =
	func_error("list__map_corresponding/3: mismatched list arguments").
list__map_corresponding(_, [_ | _], []) =
	func_error("list__map_corresponding/3: mismatched list arguments").
list__map_corresponding(F, [A | As], [B | Bs]) =
	[F(A, B) | list__map_corresponding(F, As, Bs)].

list__map_corresponding3(F, As, Bs, Cs) =
	(
		As = [A | As0],
		Bs = [B | Bs0],
		Cs = [C | Cs0]
	->
		[F(A, B, C) | list__map_corresponding3(F, As0, Bs0, Cs0)]
	;
		As = [],
		Bs = [],
		Cs = []
	->
		[]
	;
		func_error("list__map_corresponding3: " ++
			"mismatched list arguments")
	).

list__filter_map_corresponding(_, [], []) = [].
list__filter_map_corresponding(_, [], [_ | _]) =
	func_error("list__filter_map_corresponding/3: " ++
		"mismatched list arguments").
list__filter_map_corresponding(_, [_ | _], []) =
	func_error("list__filter_map_corresponding/3: " ++
		"mismatched list arguments").
list__filter_map_corresponding(F, [A | As], [B | Bs]) =
	( F(A, B) = C ->
		[C | list__filter_map_corresponding(F, As, Bs)]
	;
		list__filter_map_corresponding(F, As, Bs)
	).

list__filter_map_corresponding3(F, As, Bs, Cs) =
	(
		As = [A | As0],
		Bs = [B | Bs0],
		Cs = [C | Cs0]
	->
	  	( F(A, B, C) = D ->
			[D | list__filter_map_corresponding3(F, As0, Bs0, Cs0)]
		;
			list__filter_map_corresponding3(F, As0, Bs0, Cs0)
		)
	;
		As = [],
		Bs = [],
		Cs = []
	->
		[]
	;
		func_error("list__filter_map_corresponding3: " ++
			"mismatched list arguments")
	).

list__foldl(_, [], !A).
list__foldl(P, [H | T], !A) :-
	call(P, H, !A),
	list__foldl(P, T, !A).

list__foldl2(_, [], !A, !B).
list__foldl2(P, [H | T], !A, !B) :-
	call(P, H, !A, !B),
	list__foldl2(P, T, !A, !B).

list__foldl3(_, [], !A, !B, !C).
list__foldl3(P, [H | T], !A, !B, !C) :-
	call(P, H, !A, !B, !C),
	list__foldl3(P, T, !A, !B, !C).

list__foldl4(_, [], !A, !B, !C, !D).
list__foldl4(P, [H | T], !A, !B, !C, !D) :-
	call(P, H, !A, !B, !C, !D),
	list__foldl4(P, T, !A, !B, !C, !D).

list__foldl5(_, [], !A, !B, !C, !D, !E).
list__foldl5(P, [H | T], !A, !B, !C, !D, !E) :-
	call(P, H, !A, !B, !C, !D, !E),
	list__foldl5(P, T, !A, !B, !C, !D, !E).

list__foldl6(_, [], !A, !B, !C, !D, !E, !F).
list__foldl6(P, [H | T], !A, !B, !C, !D, !E, !F) :-
	call(P, H, !A, !B, !C, !D, !E, !F),
	list__foldl6(P, T, !A, !B, !C, !D, !E, !F).

list__map_foldl(_, [], [], !A).
list__map_foldl(P, [H0 | T0], [H | T], !A) :-
	call(P, H0, H, !A),
	list__map_foldl(P, T0, T, !A).

list__map2_foldl(_, [], [], [], !A).
list__map2_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], !A) :-
	call(P, H0, H1, H2, !A),
	list__map2_foldl(P, T0, T1, T2, !A).

list__map_foldl2(_, [], [], !A, !B).
list__map_foldl2(P, [H0 | T0], [H | T], !A, !B) :-
	call(P, H0, H, !A, !B),
	list__map_foldl2(P, T0, T, !A, !B).

list__map_foldl3(_, [], [], !A, !B, !C).
list__map_foldl3(P, [H0 | T0], [H | T], !A, !B, !C) :-
	call(P, H0, H, !A, !B, !C),
	list__map_foldl3(P, T0, T, !A, !B, !C).

list__map_foldl4(_, [], [], !A, !B, !C, !D).
list__map_foldl4(P, [H0 | T0], [H | T], !A, !B, !C, !D) :-
	call(P, H0, H, !A, !B, !C, !D),
	list__map_foldl4(P, T0, T, !A, !B, !C, !D).

list__map_foldl5(_, [], [], !A, !B, !C, !D, !E).
list__map_foldl5(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E) :-
	call(P, H0, H, !A, !B, !C, !D, !E),
	list__map_foldl5(P, T0, T, !A, !B, !C, !D, !E).

list__map_foldl6(_, [], [], !A, !B, !C, !D, !E, !F).
list__map_foldl6(P, [H0 | T0], [H | T], !A, !B, !C, !D, !E, !F) :-
	call(P, H0, H, !A, !B, !C, !D, !E, !F),
	list__map_foldl6(P, T0, T, !A, !B, !C, !D, !E, !F).

list__foldr(_, [], !A).
list__foldr(P, [H | T], !A) :-
	list__foldr(P, T, !A),
	call(P, H, !A).

list__all_true(_P, []).
list__all_true(P, [X | Xs]) :-
	P(X),
	list__all_true(P, Xs).

list__all_false(_P, []).
list__all_false(P, [X | Xs]) :-
	not P(X),
	list__all_false(P, Xs).

list__filter(P, Xs, Ys) :-
	list__filter(P, Xs, Ys, _).

list__filter(_, [],  [], []).
list__filter(P, [H | T], True, False) :-
	list__filter(P, T, TrueTail, FalseTail),
	( call(P, H) ->
		True = [H | TrueTail],
		False = FalseTail
	;
		True = TrueTail,
		False = [H | FalseTail]
	).

list__filter_map(_, [],  []).
list__filter_map(P, [H0 | T0], True) :-
	list__filter_map(P, T0, TrueTail),
	( call(P, H0, H) ->
		True = [H | TrueTail]
	;
		True = TrueTail
	).

list__filter_map(_, [], [], []).
list__filter_map(P, [H0 | T0], True, False) :-
	list__filter_map(P, T0, TrueTail, FalseTail),
	( call(P, H0, H) ->
		True = [H | TrueTail],
		False = FalseTail
	;
		True = TrueTail,
		False = [H0 | FalseTail]
	).

list__find_first_map(P, [X | Xs], A) :-
        ( call(P, X, A0) ->
		A = A0
	;
		list__find_first_map(P, Xs, A)
	).

list__find_first_map2(P, [X | Xs], A, B) :-
        ( call(P, X, A0, B0) ->
		A = A0,
		B = B0
	;
		list__find_first_map2(P, Xs, A, B)
	).

list__find_first_map3(P, [X | Xs], A, B, C) :-
        ( call(P, X, A0, B0, C0) ->
		A = A0,
		B = B0,
		C = C0
	;
		list__find_first_map3(P, Xs, A, B, C)
	).

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
	list__remove_adjacent_dups(P, L1, L).

list__remove_adjacent_dups(_, [], []).
list__remove_adjacent_dups(P, [X | Xs], L) :-
	list__remove_adjacent_dups_2(P, Xs, X, L).

:- pred list__remove_adjacent_dups_2(comparison_pred(T)::in(comparison_pred),
	list(T)::in, T::in, list(T)::out) is det.

list__remove_adjacent_dups_2(_, [], X, [X]).
list__remove_adjacent_dups_2(P, [X1 | Xs], X0, L) :-
	( P(X0, X1, (=)) ->
		list__remove_adjacent_dups_2(P, Xs, X0, L)
	;
		list__remove_adjacent_dups_2(P, Xs, X1, L0),
		L = [X0 | L0]
	).

list__sort(P, L0, L) :-
	list__length(L0, N),
	( N = 0 ->
		L = []
	; list__hosort(P, N, L0, L1, []) ->
		L = L1
	;
		error("hosort failed")
	).

% list__hosort is actually det but the compiler can't confirm it
:- pred list__hosort(comparison_pred(X)::in(comparison_pred), int::in,
	list(X)::in, list(X)::out, list(X)::out) is semidet.

	% list__hosort is a Mercury implementation of the mergesort
	% described in The Craft of Prolog.
	% N denotes the length of the part of L0 that this call is sorting.
	% (require((length(L0, M), M >= N)))
	% Since we have redundant information about the list (N, and the
	% length implicit in the list itself), we get a semidet unification
	% when we deconstruct the list.
list__hosort(P, N, L0, L, Rest) :-
	( N = 1 ->
		L0 = [X | Rest],
		L = [X]
	; N = 2 ->
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
	(
		P(H1, H2, (>))
	->
		L = [H2 | T],
		list__merge(P, [H1 | T1], T2, T)
	;
		L = [H1 | T],
		list__merge(P, T1, [H2 | T2], T)
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

%
% These functions are exported so that they can be used instead of the
% names [|]_2 and []_0. These two names can be difficult to use from other
% managed languages on the il backend.
%
:- func empty_list = list(T).
:- pragma export(empty_list = out, "ML_empty_list").

empty_list = [].

:- pragma export((list__cons(in, in) = (out)), "ML_cons").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 27/04/99
%       Function forms added.

list__det_head([]) = _ :-
	error("list__det_head/1: empty list as argument").
list__det_head([X | _]) = X.

list__det_tail([]) = _ :-
	error("list__det_tail/1: empty list as argument").
list__det_tail([_ | Xs]) = Xs.

list__head([X | _]) = X.

list__tail([_ | Xs]) = Xs.

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

list__det_replace_nth(Xs, N, A) = Ys :-
	list__replace_nth_det(Xs, N, A, Ys).

list__sort_and_remove_dups(Xs) = Ys :-
	list__sort_and_remove_dups(Xs, Ys).

list__sort(Xs) = Ys :-
	list__sort(Xs, Ys).

list__reverse(Xs) = Ys :-
	list__reverse(Xs, Ys).

list__index0_det(Xs, N) = A :-
	list__index0_det(Xs, N, A).

list__det_index0(Xs, N) = A :-
	list__index0_det(Xs, N, A).

list__index1_det(Xs, N) = A :-
	list__index1_det(Xs, N, A).

list__det_index1(Xs, N) = A :-
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

%-----------------------------------------------------------------------------%

L1 ++ L2 = list__append(L1, L2).

%-----------------------------------------------------------------------------%

list__series(I, OK, Succ) =
	( OK(I) ->
		[I | list__series(Succ(I), OK, Succ)]
	;
	  	[]
	).

%-----------------------------------------------------------------------------%

Lo `..` Hi =
	list__series(Lo, ( pred(I::in) is semidet :- I =< Hi ), plus(1)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
