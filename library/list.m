%-----------------------------------------------------------------------------%

:- module list.
:- import_module int.

:- interface.

:- type list(T) ---> [] ; [T | list(T)].

:- inst list_skel = bound([] ; [free | list_skel]).

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

:- pred member(T, list(T), list(T)).
:- mode member(output, input, output).

:- pred length(list(T), integer).
:- mode length(input_list_skel, output).
:- mode length(ouput_list_skel, input).

:- pred condense(list(list(T)), list(T)).
:- mode condense(input, output).

:- pred same_length(list(T), list(T)).
:- mode same_length(input_list_skel, output_list_skel).
:- mode same_length(output_list_skel, input_list_skel).

%-----------------------------------------------------------------------------%

:- implementation.

/*
:- external("NU-Prolog", append/3).
:- external("NU-Prolog", member/2).
:- external("NU-Prolog", member/3).
:- external("NU-Prolog", length/2).
*/

condense([], []).
condense([L|Ls], R) :-
	condense(Ls, R1),
	append(L, R1, R).

same_length([], []).
same_length([_|L1], [_|L2]) :-
	same_length(L1, L2).

%-----------------------------------------------------------------------------%
