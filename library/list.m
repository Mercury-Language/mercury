%-----------------------------------------------------------------------------%

:- module list.
:- import_module int.

:- interface.

:- type list(T) ---> [] ; [T | list(T)].

:- inst list_skel(I) = bound([] ; [I | list_skel(I)]).
:- inst list_skel = list_skel(free).

:- mode input_list_skel :: list_skel -> list_skel.
:- mode output_list_skel :: free -> list_skel.
:- mode list_skel_output :: list_skel -> ground.

%-----------------------------------------------------------------------------%

	% standard append predicate

:- pred append(list(T), list(T), list(T)).
:- mode append(in, in, out) is det.
:- mode append(out, out, in) is nondet.
:- mode append(in, out, in) is semidet.
:- mode append(out, in, in) is semidet.

	% merge - see NU-Prolog documentation

:- pred merge(list(T), list(T), list(T)).
:- mode merge(in, in, out) is det.

:- pred member(T, list(T)).
:- mode member(out, in) is nondet.
:- mode member(in, in) is semidet.

	% member_chk/2 is just a particular mode of member/2.
	% The reason that it is a separate predicate is because
	% the usual implementation of member/2 leaves choice points
	% around if you call it with both args input.

:- pred member_chk(T, list(T)).
:- mode member_chk(in, in) is semidet.

:- pred member(T, list(T), list(T)).
:- mode member(out, in, out) is nondet.

:- pred length(list(_T), int).
:- mode length(input_list_skel, out) is det.
:- mode length(output_list_skel, in) is det.

:- pred condense(list(list(T)), list(T)).
:- mode condense(in, out) is det.

:- pred same_length(list(T), list(T)).
:- mode same_length(input_list_skel, output_list_skel) is det.
:- mode same_length(output_list_skel, input_list_skel) is det.

	% split_list(Len, List, Start, End):
	%	splits `List' into a prefix `Start' of length `Len',
	%	and a remainder `End'.

:- pred split_list(int, list(T), list(T), list(T)).
:- mode split_list(in, in, out, out) is det.

:- pred reverse(list(T), list(T)).
:- mode reverse(in, out) is det.

:- pred delete(list(T), T, list(T)).
:- mode delete(in, in, out) is semidet.
:- mode delete(in, out, out) is nondet.

	% delete_first(List0, Elem, List) is true iff Elem occurs in List0
	% and List is List0 with the first occurence of Elem removed

:- pred delete_first(list(T), T, list(T)).
:- mode delete_first(in, in, out) is semidet.

	% delete_all(List0, Elem, List) is true iff List is List0 with
	% all occurences of Elem removed

:- pred delete_all(list(T), T, list(T)).
:- mode delete_all(in, in, out) is det.

	% sort(List0, List):
	%	List is List0 sorted with duplicates removed.

:- pred sort(list(T), list(T)).
:- mode sort(in, out) is det.

:- pred nth_member_search(list(T), T, int).
:- mode nth_member_search(in, in, out) is semidet.

:- pred nth_member_lookup(list(T), int, T).
:- mode nth_member_lookup(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

nth_member_search([X | Xs], Y, N) :-
	( X = Y ->
		N = 1
	;
		nth_member_search(Xs, Y, N0),
		N is N0 + 1
	).

nth_member_lookup([], _, _) :-
	error("invalid index in nth_member_lookup").
nth_member_lookup([X | Xs], N, Y) :-
	( N = 1 ->
		Y = X
	; 
		N1 is N - 1,
		nth_member_lookup(Xs, N1, Y)
	).

/*
:- external("NU-Prolog", append/3).
:- external("NU-Prolog", merge/3).
:- external("NU-Prolog", member/2).
:- external("NU-Prolog", member/3).
:- external("NU-Prolog", delete/3).
:- external("NU-Prolog", length/2).
:- external("NU-Prolog", reverse/2).
:- external("NU-Prolog", sort/2).
*/

condense([], []).
condense([L|Ls], R) :-
	condense(Ls, R1),
	append(L, R1, R).

same_length([], []).
same_length([_|L1], [_|L2]) :-
	same_length(L1, L2).

member_chk(X, [Y | Ys]) :-
	( X = Y ->
		true
	;
		member_chk(X, Ys)
	).

split_list(N, List, Start, End) :-
	( N = 0 ->
		Start = [],
		End = List
	;
		N > 0,
		N1 is N - 1,
		List = [Head | List1],
		Start = [Head | Start1],
		split_list(N1, List1, Start1, End)
	).

delete_first([X | Xs], Y, Zs) :-
	( X = Y ->
		Zs = Xs
	;
		Zs = [X | Zs1],
		delete_first(Xs, Y, Zs1)
	).

delete_all([], _, []).
delete_all([X | Xs], Y, Zs) :-
	( X = Y ->
		delete_all(Xs, Y, Zs)
	;
		Zs = [X | Zs1],
		delete_all(Xs, Y, Zs1)
	).

%-----------------------------------------------------------------------------%
