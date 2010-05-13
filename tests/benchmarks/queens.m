% This program solves the N-queens problem. Given an N-by-N chessboard,
% this problem asks us to find positions for N queens on the board such
% that they do not attack each other. This means that no two queens can be
%
% - in the same row
% - in the same column
% - on the same diagnonal.
%
% We print the result as a list of integers. Each integer corresponds to one
% of the board's N columns, and it gives the number of the row occupied by
% the queen in that column. (Given the constraints of the problem, every column
% must be occupied by exactly one queen.)

:- module queens.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
	data(Data),
	( queen(Data, Out) ->
		print_list(Out, !IO)
	;
		io.write_string("No solution\n", !IO)
	).

% With this data, this program solves the 8-queens problem. To solve the
% N-queens problem for some other N, make this predicate return the first
% N integers.
:- pred data(list(int)::out) is det.

data([1,2,3,4,5,6,7,8]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Perm) :-
	qperm(Data, Perm),
	safe(Perm).

:- pred qperm(list(int)::in, list(int)::out) is nondet.

qperm([], []).
qperm([H | T], Perm) :-
	qdelete([H | T], Element, Rest),
	qperm(Rest, RestPerm),
	Perm = [Element | RestPerm].

:- pred qdelete(list(int)::in, int::out, list(int)::out) is nondet.

qdelete([H | T], H, T).
qdelete([H | T], E, [H | NT]) :-
	qdelete(T, E, NT).

:- pred safe(list(int)::in) is semidet.

safe([]).
safe([H | T]) :-
	nodiag(H, 1, T),
	safe(T).

:- pred nodiag(int::in, int::in, list(int)::in) is semidet.

nodiag(_, _, []).
nodiag(TestRow, !.Diff, [Row | Rows]) :-
	( !.Diff = Row - TestRow ->
		fail
	; !.Diff = TestRow - Row ->
		fail
	;
		true
	),
	!:Diff = !.Diff + 1,
	nodiag(TestRow, !.Diff, Rows).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

print_list(Xs, !IO) :-
	(
		Xs = [],
		io.write_string("[]\n", !IO)
	;
		Xs = [H | T],
		io.write_string("[", !IO),
		print_list_elements(H, T, !IO),
		io.write_string("]\n", !IO)
	).

:- pred print_list_elements(int::in, list(int)::in, io::di, io::uo) is det.

print_list_elements(X, Xs, !IO) :-
	io.write_int(X, !IO),
	(
		Xs = []
	;
		Xs = [H | T],
		io.write_string(", ", !IO),
		print_list_elements(H, T, !IO)
	).
