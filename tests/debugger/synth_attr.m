%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module synth_attr.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

:- type listint == list(int).

main(!IO) :-
    data(Data),
    ( if queen(Data, Out) then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    qperm(Data, Out),
    event safe_test(Out, testlen(10)),
    safe(Out).

:- func testlen(int, list(int)) = int.

testlen(Min, L) = N :-
    list.length(L, N0),
    ( if N0 >= Min then
        N = N0
    else
        error("testlen: N < Min")
    ).

:- pred qperm(list(T)::in, list(T)::out) is nondet.

qperm([], []).
qperm(L, K) :-
    L = [_ | _],
    qdelete(U, L, Z),
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
        event nodiag_fail("N - B", B, N, list.length, list.sort, [N | L]),
        fail
    else if D = BmN then
        event nodiag_fail("B - N", B, N, list.length, list.sort, [N | L]),
        fail
    else
        true
    ),
    D1 = D + 1,
    nodiag(B, D1, L).

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
