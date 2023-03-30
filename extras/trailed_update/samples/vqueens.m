%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% N-queens program -- using the `var' module and freeze/2.
%
%---------------------------------------------------------------------------%

:- module vqueens.

:- interface.

:- import_module list, int, io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.
:- import_module unsafe.
:- import_module var.

main(!IO) :-
    ( if data(Data), queen(Data, Out) then
        io.print_line(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    same_len(Data, Posn),
    safe(Posn),
    qperm(Data, Posn, Posn2),
    conv_posn(Posn2, Out).

:- pred same_len(list(int)::in, list(var(int))::out(list(any))) is det.

same_len([], []).
same_len([_ | Xs], [N | Ys]) :-
    init(N),
    same_len(Xs, Ys).

:- pred conv_posn(list(var(int))::in, list(int)::out) is det.

conv_posn([], []).
conv_posn([var(N) | Xs], [N | Ys]) :-
    conv_posn(Xs, Ys).

:- pred qperm(list(int)::in, list(var(int))::in(list(any)),
    list(var(int))::out) is nondet.

qperm([], [], []).
qperm([X | Y], [var(U) | V], [var(U) | V2]) :-
    qdelete(U, [X|Y], Z),
    qperm(Z, V, V2).

:- pred qdelete(int::out, list(int)::in, list(int)::out) is nondet.

qdelete(A, [A | L], L).
qdelete(X, [A | Z], [A | R]) :-
    qdelete(X, Z, R).

:- pred safe(list(var(int))::in(list(any))) is semidet.

safe([]).
safe([NVar | L]) :-
    freeze(NVar, (any_pred(N::in) is semidet :- nodiag(N, 1, L))),
    safe(L).

:- pred nodiag(int::in, int::in, list(var(int))::in(list(any))) is semidet.

nodiag(_, _, []).
nodiag(B, D, [NVar | L]) :-
    freeze(NVar, (any_pred(N::in) is semidet :- D \= N - B, D \= B - N)),
    nodiag(B, D + 1, L).

%---------------------------------------------------------------------------%
:- end_module vqueens.
%---------------------------------------------------------------------------%
