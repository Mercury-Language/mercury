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

:- pred append(list(T), list(T), list(T)).
:- mode append(input, input, output).
:- mode append(output, output, input).

:- pred member(T, list(T)).
:- mode member(output, input).
:- mode member(input, input).

	% member_chk/2 is just a particular mode of member/2.
	% The reason that it is a separate predicate is because
	% the usual implementation of member/2 leaves choice points
	% around if you call it with both args input.

:- pred member_chk(T, list(T)).
:- mode member_chk(input, input).

:- pred member(T, list(T), list(T)).
:- mode member(output, input, output).

:- pred length(list(_T), int).
:- mode length(input_list_skel, output).
:- mode length(output_list_skel, input).

:- pred condense(list(list(T)), list(T)).
:- mode condense(input, output).

:- pred same_length(list(T), list(T)).
:- mode same_length(input_list_skel, output_list_skel).
:- mode same_length(output_list_skel, input_list_skel).

	% split_list(Len, List, Start, End):
	%	splits `List' into a prefix `Start' of length `Len',
	%	and a remainder `End'.

:- pred split_list(int, list(T), list(T), list(T)).
:- mode split_list(input, input, output, output).

:- pred reverse(list(T), list(T)).
:- mode reverse(input, output).

%-----------------------------------------------------------------------------%

:- implementation.

/*
:- external("NU-Prolog", append/3).
:- external("NU-Prolog", member/2).
:- external("NU-Prolog", member/3).
:- external("NU-Prolog", length/2).
:- external("NU-Prolog", reverse/2).
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

%-----------------------------------------------------------------------------%
