%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

	% list__merge_and_remove_dups(L1, L2, L):
	%	L is the result of merging the elements of L1 and L2,
	%	in ascending order, and eliminating any duplicates.
	%	L1 and L2 must be sorted and must each not contain any
	%	duplicates.
:- pred list__merge_and_remove_dups(list(T), list(T), list(T)).
:- mode list__merge_and_remove_dups(in, in, out) is det.

	% list__remove_adjacent_dups(L0, L) :
	%	L is the result of replacing every sequence of duplicate
	%	elements in L0 with a single such element.
:- pred list__remove_adjacent_dups(list(T), list(T)).
:- mode list__remove_adjacent_dups(in, out) is det.

	% list__remove_dups(L0, L) :
	%	L is the result of deleting the second and subsequent
	%	occurrences of every element that occurs twice in L.
:- pred list__remove_dups(list(T), list(T)).
:- mode list__remove_dups(in, out) is det.

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
	%	See also: list__split_list.
	%
:- pred list__take(int, list(T), list(T)).
:- mode list__take(in, in, out) is semidet.

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

	% list__sort_and_remove_dups(List0, List):
	%	List is List0 sorted with duplicates removed.
	%
:- pred list__sort_and_remove_dups(list(T), list(T)).
:- mode list__sort_and_remove_dups(in, out) is det.

	% list__sort(List0, List):
	%	List is List0 sorted.
	%
:- pred list__sort(list(T), list(T)).
:- mode list__sort(in, out) is det.

	% list__reverse(List, Reverse):
	%	`Reverse' is a list containing the same elements as `List'
	%	but in reverse order.
	%
:- pred list__reverse(list(T), list(T)).
:- mode list__reverse(in, out) is det.

	% list__perm(List0, List):
	%	True iff `List' is a permutation of `List0'.
	%
:- pred	list__perm(list(T), list(T)).
:- mode list__perm(in, out) is nondet.

	% list__nth_member_search(List, Elem, Position):
	%	Elem is the Position'th member of List.
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

	% list__zip(ListA, ListB, List):
	%	List is the result of alternating the elements
	%	of ListA and ListB.  When one of the lists goes to empty,
	% 	the remainder of the nonempty list is appended.
	%
:- pred list__zip(list(T), list(T), list(T)).
:- mode list__zip(in, in, out) is det.

	% list__duplicate(Count, Elem, List) is true iff List is a list
	% containing Count duplicate copies of Elem.
	%
:- pred list__duplicate(int, T, list(T)).
:- mode list__duplicate(in, in, out) is det.

	% list__condense(ListOfLists, List):
	%	`List' is the result of concatenating all the
	%	elements of `ListOfLists'.
	%
:- pred list__condense(list(list(T)), list(T)).
:- mode list__condense(in, out) is det.

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

	% list__foldl(Pred, List, Start, End) calls Pred with each
	% element of List (working left-to-right) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
:- pred list__foldl(pred(X, Y, Y), list(X), Y, Y).
:- mode list__foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode list__foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldl(pred(in, in, out) is semidet, in, in, out) is semidet.

	% list__foldr(Pred, List, Start, End) calls Pred with each
	% element of List (working right-to-left) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
:- pred list__foldr(pred(X, Y, Y), list(X), Y, Y).
:- mode list__foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode list__foldr(pred(in, in, out) is semidet, in, in, out) is semidet.

	% list__foldl2(Pred, List, Start, End, Start2, End2) 
	% calls Pred with each element of List (working left-to-right),
	% 2 accumulators (with the initial values of Start and Start2),
	% and returns the final values in End and End2.
	% (Although no more expressive than list__foldl, this is often
	% a more convenient format, and a little more efficient).
:- pred list__foldl2(pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode list__foldl2(pred(in, in, out, in, out) is det,
		in, in, out, in, out) is det.
:- mode list__foldl2(pred(in, in, out, di, uo) is det,
		in, in, out, di, uo) is det.
:- mode list__foldl2(pred(in, di, uo, di, uo) is det,
		in, di, uo, di, uo) is det.

	% list__map_foldl(Pred, InList, OutList, Start, End) calls Pred
	% with an accumulator (with the initial value of Start) on
	% each element of InList (working left-to-right) to transform
	% InList into OutList.  The final value of the acumulator is
	% returned in End.
:- pred list__map_foldl(pred(X, Y, Z, Z), list(X), list(Y), Z, Z).
:- mode list__map_foldl(pred(in, out, di, uo) is det, in, out, di, uo) is det.
:- mode list__map_foldl(pred(in, out, in, out) is det, in, out, in, out) is det.
:- mode list__map_foldl(pred(in, out, in, out) is semidet, in, out, in, out)
                                                                is semidet.

	% list__filter(Pred, List, TrueList) takes a closure with one
	% input argument and for each member of List `X', calls the closure.
	% Iff call(Pred, X) is true, then X is included in TrueList.
:- pred list__filter(pred(X), list(X), list(X)).
:- mode list__filter(pred(in) is semidet, in, out) is det.

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

	% list__filter_map(Transformer, List, TrueList, FalseList) takes
	% a predicate with one input argument and one output argument.
	% It is called with each element of List. If a call succeeds,
	% then the output is included in TrueList; otherwise, the failing
	% input is included in FalseList.
:- pred list__filter_map(pred(X, Y), list(X), list(Y), list(X)).
:- mode list__filter_map(pred(in, out) is semidet, in, out, out) is det.

%-----------------------------------------------------------------------------%

	% list__sort(Compare, Unsorted, Sorted) is true iff Sorted is a
	% list containing the same elements as Unsorted, where Sorted is
	% a sorted list, with respect to the ordering defined by the predicate
	% term Compare.
:- pred list__sort(pred(X, X, comparison_result), list(X), list(X)).
:- mode list__sort(pred(in, in, out) is det, in, out) is det.

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

	% list__merge_and_remove_dups(P, As, Bs, Sorted) is true if and only if
	% Sorted is a list containing the elements of As and Bs in the order 
	% implied by their sorted merge. The ordering of elements is defined by
	% the higher order comparison predicate P.
	% As and Bs must be sorted.
:- pred list__merge_and_remove_dups(pred(X, X, comparison_result),
	list(X), list(X), list(X)).
:- mode list__merge_and_remove_dups(pred(in, in, out) is det,
	in, in, out) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bintree_set, require.

%-----------------------------------------------------------------------------%

list__append([], Ys, Ys).
list__append([X | Xs], Ys, [X | Zs]) :-
	list__append(Xs, Ys, Zs).

:- list__remove_suffix(_List, Suffix, _Prefix) when Suffix.

list__remove_suffix(List, Suffix, Prefix) :-
	list__length(List, ListLength),
	list__length(Suffix, SuffixLength),
	PrefixLength is ListLength - SuffixLength,
	list__split_list(PrefixLength, List, Prefix, Suffix).

%-----------------------------------------------------------------------------%

list__nth_member_search([X | Xs], Y, N) :-
	( X = Y ->
		N = 1
	;
		list__nth_member_search(Xs, Y, N0),
		N is N0 + 1
	).

%-----------------------------------------------------------------------------%

list__index0([X | Xs], N, Elem) :-
	( N = 0 ->
		Elem = X
	; 
		N1 is N - 1,
		list__index0(Xs, N1, Elem)
	).

list__index0_det(List, N, Elem) :-
	( list__index0(List, N, Elem0) ->
		Elem = Elem0
	;
		error("list__index: index out of range")
	).

list__index1(List, N, Elem) :-
	N1 is N - 1,
	list__index0(List, N1, Elem).

list__index1_det(List, N, Elem) :-
	N1 is N - 1,
	list__index0_det(List, N1, Elem).

%-----------------------------------------------------------------------------%

list__condense([], []).
list__condense([L|Ls], R) :-
	list__condense(Ls, R1),
	list__append(L, R1, R).

%-----------------------------------------------------------------------------%

list__same_length([], []).
list__same_length([_|L1], [_|L2]) :-
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
	(
		X = Y
	->
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

%-----------------------------------------------------------------------------%

list__member(X, [X | _]).
list__member(X, [_ | Xs]) :-
	list__member(X, Xs).

list__member(Element, List, SubList) :-
	SubList = [Element | _],
	list__append(_, SubList, List).

%-----------------------------------------------------------------------------%

list__merge(A, B, C) :-
	( A = [X|Xs] ->
		( B = [Y|Ys] ->
			C = [Z|Zs],
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
	( A = [X|Xs] ->
		( B = [Y|Ys] ->
			compare(Res, X, Y),
			( Res = (<) ->
				C = [X|Zs],
				list__merge_and_remove_dups(Xs, B, Zs)
			; Res = (>) ->
				C = [Y|Zs],
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
	N1 is N0 + 1,
	list__length_2(L1, N1, N).

%-----------------------------------------------------------------------------%

list__reverse(L0, L) :-
	list__reverse_2(L0, [], L).

:- pred list__reverse_2(list(T), list(T), list(T)).
:- mode list__reverse_2(in, in, out) is det.

list__reverse_2([], L, L).
list__reverse_2([X|Xs], L0, L) :-
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
list__qsort([X|L], R0, R) :-
        list__partition(L, X, L1, L2),
        list__qsort(L2, R0, R1),
        list__qsort(L1, [X|R1], R).

:- pred list__partition(list(T), T, list(T), list(T)).
:- mode list__partition(in, in, out, out) is det.

list__partition([], _, [], []).
list__partition([Head|Tail], Partition, Low, High) :-
        ( compare(<, Head, Partition) ->
                list__partition(Tail, Partition, Low1, High),
                Low = [Head|Low1]
        ;
                list__partition(Tail, Partition, Low, High1),
		High = [Head|High1]
	).

%-----------------------------------------------------------------------------%

:- pred list__merge_sort(list(T), list(T)).
:- mode list__merge_sort(in, out) is det.

list__merge_sort([], []).
list__merge_sort([X], [X]).
list__merge_sort(List, SortedList) :-
	List = [_,_|_],
	list__length(List, Length),
	HalfLength is Length // 2,
	( list__split_list(HalfLength, List, Front, Back) ->
		list__merge_sort(Front, SortedFront),
		list__merge_sort(Back, SortedBack),
		list__merge(SortedFront, SortedBack, SortedList)
	;
		error("list__merge_sort")
	).

%-----------------------------------------------------------------------------%

list__remove_dups(Xs, Ys) :-
	bintree_set__init(Zs0),
	list__remove_dups_2(Xs, Zs0, Ys).

:- pred list__remove_dups_2(list(T), bintree_set(T), list(T)).
:- mode list__remove_dups_2(in, in, out) is det.

list__remove_dups_2([], _SoFar, []).
list__remove_dups_2([X|Xs], SoFar0, Zs) :-
	(
		bintree_set__member(X, SoFar0)
	->
		list__remove_dups_2(Xs, SoFar0, Zs)
	;
		bintree_set__insert(SoFar0, X, SoFar),
		list__remove_dups_2(Xs, SoFar, Ys),
		Zs = [X|Ys]
	).

%-----------------------------------------------------------------------------%

list__remove_adjacent_dups([], []).
list__remove_adjacent_dups([X|Xs], L) :-
	list__remove_adjacent_dups_2(Xs, X, L).

:- pred list__remove_adjacent_dups_2(list(T), T, list(T)).
:- mode list__remove_adjacent_dups_2(in, in, out) is det.

list__remove_adjacent_dups_2([], X, [X]).
list__remove_adjacent_dups_2([X1|Xs], X0, L) :-
	(X0 = X1 ->
		list__remove_adjacent_dups_2(Xs, X1, L)
	;
		L = [X0 | L0],
		list__remove_adjacent_dups_2(Xs, X1, L0)
	).

%-----------------------------------------------------------------------------%

list__zip([], Bs, Bs).
list__zip([A|As], Bs, [A|Cs]) :-
	list__zip2(As, Bs, Cs).

:- pred list__zip2(list(T), list(T), list(T)).
:- mode list__zip2(in, in, out) is det.

:- list__zip2(_, Bs, _) when Bs. % NU-Prolog indexing

list__zip2(As, [], As).
list__zip2(As, [B|Bs], [B|Cs]) :-
	list__zip(As, Bs, Cs).

%-----------------------------------------------------------------------------%

/**** unused
:- pred list__split3(list(T), list(T), list(T), list(T)).
:- mode list__split3(in, out, in, in) is semidet.

list__split3(As, Bs, Cs, Ds) :-
	list__length(As, AL),
	list__length(Cs, CL),
	list__length(Ds, DL),
	N1 is AL + CL,
	BL is DL - N1,
	N2 is AL + BL,
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
		N1 is N - 1,
		List = [Head | List1],
		Start = [Head | Start1],
		list__split_list(N1, List1, Start1, End)
	).

list__take(N, As, Bs) :-
	(
		N > 0
	->
		N1 is N - 1,
		As = [A|As1],
		Bs = [A|Bs1],
		list__take(N1, As1, Bs1)
	;
		Bs = []
	).

list__drop(N, As, Bs) :-
	(
		N > 0
	->
		N1 is N - 1,
		As = [_|Cs],
		list__drop(N1, Cs, Bs)
	;
		As = Bs
	).

%-----------------------------------------------------------------------------%

list__duplicate(N, X, L) :-
	( N > 0 ->
		N1 is N - 1,
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
list__chunk_2([X|Xs], ChunkSize, List0, N, Lists) :-
	( N > 1 ->
		N1 is N - 1,
		list__chunk_2(Xs, ChunkSize, [X | List0], N1, Lists)
	;
		list__reverse([X | List0], List),
		Lists = [List | Lists1],
		list__chunk_2(Xs, ChunkSize, [], ChunkSize, Lists1)
	).

%-----------------------------------------------------------------------------%

list__perm([], []).
list__perm([X|Xs], Ys) :-
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
list__all_same([H|T]) :-
	list__all_same_2(H, T).

:- pred list__all_same_2(T, list(T)).
:- mode list__all_same_2(in, in) is semidet.

list__all_same_2(_, []).
list__all_same_2(H, [H|T]) :-
	list__all_same_2(H, T).

%-----------------------------------------------------------------------------%

list__last([H|T], Last) :-
	(
		T = [],
		Last = H
	;
		T = [_|_],
		list__last(T, Last)
	).

%-----------------------------------------------------------------------------%

list__map(_, [],  []).
list__map(P, [H0|T0], [H|T]) :-
	call(P, H0, H),
	list__map(P, T0, T).

list__foldl(_, [], Acc, Acc).
list__foldl(P, [H|T], Acc0, Acc) :-
	call(P, H, Acc0, Acc1),
	list__foldl(P, T, Acc1, Acc).

list__foldl2(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
list__foldl2(P, [H|T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
	call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
	list__foldl2(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc).

list__map_foldl(_, [],  []) -->
        [].
list__map_foldl(P, [H0|T0], [H|T]) -->
        call(P, H0, H),
        list__map_foldl(P, T0, T).

list__foldr(_, [], Acc, Acc).
list__foldr(P, [H|T], Acc0, Acc) :-
	list__foldr(P, T, Acc0, Acc1),
	call(P, H, Acc1, Acc).

list__filter(P, Xs, Ys) :-
	list__filter(P, Xs, Ys, _).

list__filter(_, [],  [], []).
list__filter(P, [H|T], L, M) :-
	( call(P, H) ->
		L = [H|L1],
		M = M1
	;
		L = L1,
		M = [H|M1]
	),
	list__filter(P, T, L1, M1).

list__filter_map(_, [],  []).
list__filter_map(P, [H0|T0], L) :-
	( call(P, H0, H) ->
		L = [H|L1]
	;
		L = L1
	),
	list__filter_map(P, T0, L1).

list__filter_map(_, [], [], []).
list__filter_map(P, [H0|T0], L, M) :-
        ( call(P, H0, H) ->
                L = [H|L1],
		M = M1
        ;
                L = L1,
		M = [H0|M1]
        ),
        list__filter_map(P, T0, L1, M1).

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
:- pred list__hosort(pred(X, X, comparison_result), int, list(X), list(X), list(X)).
:- mode list__hosort(pred(in, in, out) is det, in, in, out, out) is semidet.

	% list__hosort is a Mercury implementation of the mergesort described in
	% The Craft of Prolog.
	% N denotes the length of the part of L0 that this call is sorting.
	% 		(require((length(L0, M), M >= N)))
	% Since we have redundant information about the list (N, and the
	% length implicit in the list itself), we get a semidet unification
	% when we deconstruct the list.
list__hosort(P, N, L0, L, Rest) :-
        (
		N = 1
	->
                L0 = [X|Rest],
		L = [X]
        ;
		N = 2
	->
		L0 = [X,Y|Rest],
		call(P, X, Y, C),
		(
			C = (<),
			L = [X,Y]
		; 
			C = (=),
			L = [X,Y]
		;
			C = (>),
			L = [Y,X]
		)
        ;      
		N1 is N//2,
		list__hosort(P, N1, L0, L1, Middle),
                N2 is N-N1,
		list__hosort(P, N2, Middle, L2, Rest),
		list__merge(P, L1, L2, L)
        ).

list__merge(_P, [], [], []).
list__merge(_P, [], [Y|Ys], [Y|Ys]).
list__merge(_P, [X|Xs], [], [X|Xs]).
list__merge(P, [H1|T1], [H2|T2], L) :-
	call(P, H1, H2, C),
	(
		C = (<),
		L = [H1|T],   
		list__merge(P, T1, [H2|T2], T)
	;
		C = (=),
		L = [H1,H2|T],
		list__merge(P, T1, T2, T)
	;
		C = (>),
		L = [H2|T],   
		list__merge(P, [H1|T1], T2, T)
	).

list__merge_and_remove_dups(_P, [], [], []).
list__merge_and_remove_dups(_P, [], [Y|Ys], [Y|Ys]).
list__merge_and_remove_dups(_P, [X|Xs], [], [X|Xs]).
list__merge_and_remove_dups(P, [H1|T1], [H2|T2], L) :-
	call(P, H1, H2, C),
	(
		C = (<),
		L = [H1|T],   
		list__merge(P, T1, [H2|T2], T)
	;
		C = (=),
		L = [H1 | T],
		list__merge(P, T1, T2, T)
	;
		C = (>),
		L = [H2|T],   
		list__merge(P, [H1|T1], T2, T)
	).


%-----------------------------------------------------------------------------%

:- interface.

	% list__apply(Cs, Bs) takes a list of closures with one
	% output argument Cs, and calls the closures, returning
	% the resulting bindings in Bs.
:- pred list__apply(list(pred(T)), list(T)).
:- mode list__apply(list_skel_in(pred(out) is det), out) is det.
:- mode list__apply(list_skel_in(pred(out) is semidet), out) is semidet.
:- mode list__apply(list_skel_in(pred(out) is multi), out) is multi.
:- mode list__apply(list_skel_in(pred(out) is nondet), out) is nondet.

:- implementation.

list__apply([], []).
list__apply([C|Cs], [B|Bs]) :-
	call(C, B),
	list__apply(Cs, Bs).

%-----------------------------------------------------------------------------%
