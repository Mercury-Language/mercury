%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module breakpoints.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is cc_multi.
:- func string / string = string.

:- implementation.

:- include_module breakpoints.print_list.
:- include_module breakpoints.a.
:- include_module breakpoints.b.
:- import_module breakpoints.print_list.
:- import_module breakpoints.a.
:- import_module breakpoints.b.
:- import_module breakpoints.a.testmod.
:- import_module breakpoints.b.testmod.

:- import_module list.
:- import_module int.
:- import_module string.

main(!IO) :-
    ( if queen(data, Out) then
        print_list(Out, !IO),
        io.write(test_in_a, !IO),
        io.nl(!IO),
        io.write(test_in_b, !IO),
        io.nl(!IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- func data = list(int).

data = D :-
    data(D).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    qperm(Data, Out),
    safe(Out).

:- pred qperm(list(T)::in, list(T)::out) is nondet.

qperm([], []).
qperm([X | Y], K) :-
    qdelete(U, [X | Y], Z),
    K = [U | V],
    qperm(Z, V).

:- pred qdelete(T::out, list(T)::in, list(T)::out) is nondet.

qdelete(A, [A | L], L).
qdelete(X, [A | Z], [A | R]) :-
    qdelete(X, Z, R).

:- pred safe(list(int)::in) is semidet.

safe([]).
safe([N | L]) :-
    nodiag(N, 1, L),
    safe(L).

:- pred nodiag(int::in, int::in, list(int)::in) is semidet.

nodiag(_, _, []).
nodiag(B, D, [N | L]) :-
    NmB = N - B,
    BmN = B - N,
    ( if D = NmB then
        fail
    else if D = BmN then
        fail
    else
        true
    ),
    D1 = D + 1,
    nodiag(B, D1, L).

X / _ = X.

:- pred test_in_both(io::di, io::uo) is det.

test_in_both(!IO) :-
    io.write_string("test_in_both in breakpoints\n", !IO).
