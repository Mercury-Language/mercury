%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% 9-queens program.
%

:- module cqueens.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module prolog.

main(!IO) :-
    data(Data),
    ( if queen(Data, Out) then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5, 6, 7, 8, 9]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    queen_2(Data, [], Out).

:- pred queen_2(list(int)::in, list(int)::in, list(int)::out) is nondet.

queen_2([], _, []).
queen_2(L, History, [Q | M]) :-
    L = [_ | _],
    qdelete(L, Q, L1),
    nodiag(Q, 1, History),
    queen_2(L1, [Q | History], M).

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
    ( if !.Diff = Row - TestRow then
        fail
    else if !.Diff = TestRow - Row then
        fail
    else
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
