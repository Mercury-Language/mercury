%-----------------------------------------------------------------------------%

:- module list.
:- import_module int.

:- interface.

:- type list(T) ---> [] ; [T | list(T)].

:- inst list_skel(I) = bound([] ; [I | list_skel(I)]).
:- inst list_skel = list_skel(free).

:- inst non_empty_list = bound([ground | ground]).

:- mode input_list_skel :: list_skel -> list_skel.
:- mode output_list_skel :: free -> list_skel.
:- mode list_skel_output :: list_skel -> ground.

%-----------------------------------------------------------------------------%

	% standard append predicate

:- pred list__append(list(T), list(T), list(T)).
:- mode list__append(in, in, out) is det.
:- mode list__append(in, out, in) is semidet.
:- mode list__append(out, out, in) is nondet.
%	The following mode is semidet in the sense that it doesn't
%	succeed more than once - but it does create a choice-point,
%	which means it's inefficient and that the compiler can't deduce
%	that it is semidet.  Use list__remove_prefix instead.
% :- mode list__append(out, in, in) is semidet.

:- pred list__remove_suffix(list(T), list(T), list(T)).
:- mode list__remove_suffix(in, in, out) is semidet.
%	list__remove_suffix(List, Suffix, Prefix):
%	The same as list__append(Prefix, Suffix, List) except that
%	this is semidet whereas list__append(out, in, in) is nondet.

	% merge - see NU-Prolog documentation

:- pred list__merge(list(T), list(T), list(T)).
:- mode list__merge(in, in, out) is det.

:- pred list__member(T, list(T)).
:- mode list__member(in, in) is semidet.
:- mode list__member(out, in) is nondet.

:- pred list__member(T, list(T), list(T)).
:- mode list__member(out, in, out) is nondet.

:- pred list__length(list(_T), int).
/****
	% XXX The current mode checker can't handle this
:- mode list__length(input_list_skel, out) is det.
*****/
:- mode list__length(in, out) is det.
:- mode list__length(output_list_skel, in) is det.

:- pred list__condense(list(list(T)), list(T)).
:- mode list__condense(in, out) is det.

:- pred list__same_length(list(T), list(T)).
/**** 
	% XXX The current mode checker can't handle this
:- mode list__same_length(input_list_skel, output_list_skel) is det.
:- mode list__same_length(output_list_skel, input_list_skel) is det.
****/
:- mode list__same_length(in, output_list_skel) is det.
:- mode list__same_length(output_list_skel, in) is det.
:- mode list__same_length(in, in) is semidet.

	% list__split_list(Len, List, Start, End):
	%	splits `List' into a prefix `Start' of length `Len',
	%	and a remainder `End'.

:- pred list__split_list(int, list(T), list(T), list(T)).
:- mode list__split_list(in, in, out, out) is semidet.

:- pred list__reverse(list(T), list(T)).
:- mode list__reverse(in, out) is det.

:- pred list__delete(list(T), T, list(T)).
:- mode list__delete(in, in, out) is semidet.
:- mode list__delete(in, out, out) is nondet.

	% list__delete_first(List0, Elem, List) is true iff Elem occurs in List0
	% and List is List0 with the first occurence of Elem removed

:- pred list__delete_first(list(T), T, list(T)).
:- mode list__delete_first(in, in, out) is semidet.

	% list__delete_all(List0, Elem, List) is true iff List is List0 with
	% all occurences of Elem removed

:- pred list__delete_all(list(T), T, list(T)).
:- mode list__delete_all(in, in, out) is det.

	% list__sort(List0, List):
	%	List is List0 sorted with duplicates removed.

:- pred list__sort(list(T), list(T)).
:- mode list__sort(in, out) is det.

:- pred list__nth_member_search(list(T), T, int).
:- mode list__nth_member_search(in, in, out) is semidet.

:- pred list__nth_member_lookup(list(T), int, T).
:- mode list__nth_member_lookup(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

list__nth_member_search([X | Xs], Y, N) :-
	( X = Y ->
		N = 1
	;
		list__nth_member_search(Xs, Y, N0),
		N is N0 + 1
	).

list__nth_member_lookup([], _, _) :-
	error("invalid index in nth_member_lookup").
list__nth_member_lookup([X | Xs], N, Y) :-
	( N = 1 ->
		Y = X
	; 
		N1 is N - 1,
		list__nth_member_lookup(Xs, N1, Y)
	).

list__condense([], []).
list__condense([L|Ls], R) :-
	list__condense(Ls, R1),
	list__append(L, R1, R).

list__same_length([], []).
list__same_length([_|L1], [_|L2]) :-
	list__same_length(L1, L2).

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

list__append([], Ys, Ys).
list__append([X | Xs], Ys, [X | Zs]) :-
	list__append(Xs, Ys, Zs).

:- list__remove_suffix(_List, Suffix, _Prefix) when Suffix.

list__remove_suffix(List, Suffix, Prefix) :-
	length(List, ListLength),
	length(Suffix, SuffixLength),
	PrefixLength is ListLength - SuffixLength,
	list__split_list(PrefixLength, List, Prefix, Suffix0),
	Suffix = Suffix0.	% work around bug in determinism analysis

list__member(X, [X | _]).
list__member(X, [_ | Xs]) :-
	list__member(X, Xs).

%-----------------------------------------------------------------------------%

	% Declarations for NU-Prolog builtins.

:- pred merge(list(T), list(T), list(T)).
:- mode merge(in, in, out) is det.

:- pred member(T, list(T), list(T)).
:- mode member(out, in, out) is nondet.

:- pred length(list(_T), int).
:- mode length(input_list_skel, out) is det.
:- mode length(output_list_skel, in) is det.

:- pred reverse(list(T), list(T)).
:- mode reverse(in, out) is det.

:- pred delete(list(T), T, list(T)).
:- mode delete(in, in, out) is semidet.
:- mode delete(in, out, out) is nondet.

:- pred sort(list(T), list(T)).
:- mode sort(in, out) is det.

/*
:- external("NU-Prolog", merge/3).
:- external("NU-Prolog", member/3).
:- external("NU-Prolog", length/2).
:- external("NU-Prolog", reverse/2).
:- external("NU-Prolog", delete/3).
:- external("NU-Prolog", sort/2).
*/

	% Just forward to the NU-Prolog builtins.

list__merge(L0, L1, L) :- merge(L0, L1, L).
list__member(Elem, List, SubList) :- member(Elem, List, SubList).
list__length(L, N) :- length(L, N).
list__reverse(L0, L) :- reverse(L0, L).
list__delete(X, L0, L) :- delete(X, L0, L).
list__sort(L0, L) :- sort(L0, L).

%-----------------------------------------------------------------------------%
