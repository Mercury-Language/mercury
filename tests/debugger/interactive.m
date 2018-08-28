%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module interactive.

:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is cc_multi.

    % exported for use with interactive queries in the debugger
:- pred queen(list(int)::in, list(int)::out) is nondet.

    % exported for use with interactive queries in the debugger
:- pred qperm(list(T)::in, list(T)::out) is nondet.

:- implementation.

:- import_module int.

main(!IO) :-
    ( if data(Data), queen(Data, Out) then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5]).

queen(Data, Out) :-
    qperm(Data, Out),
    safe(Out).

qperm([], []).
qperm([X | Y], K) :-
    qdelete(U, [X | Y], Z),
    K = [U | V],
    qperm(Z, V).

:- pred qdelete(T::out, list(T)::in, list(T)::out) is nondet.

qdelete(A, [A | L], L).
qdelete(X, [A | Z], [A | R]) :-
    qdelete(X, Z, R).

:- pred nodiag(int::in, int::in, list(int)::in) is semidet.

nodiag(_, _, []).
nodiag(B, D, [N | L]) :-
    NmB is N - B,
    BmN is B - N,
    ( if D = NmB then
        fail
    else if D = BmN then
        fail
    else
        true
    ),
    D1 is D + 1,
    nodiag(B, D1, L).

:- pred safe(list(int)::in) is semidet.

safe([]).
safe([N | L]) :-
    nodiag(N, 1, L),
    safe(L).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

print_list(Xs, !IO) :-
    (
        Xs = [],
        io.write_string("[]\n", !IO)
    ;
        Xs = [_ | _],
        io.write_string("[", !IO),
        print_list_2(Xs, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred print_list_2(list(int)::in, io::di, io::uo) is det.

print_list_2([], !IO).
print_list_2([X | Xs], !IO) :-
    io.write_int(X, !IO),
    (
        Xs = []
    ;
        Xs = [_ | _],
        io.write_string(", ", !IO),
        print_list_2(Xs, !IO)
    ).
