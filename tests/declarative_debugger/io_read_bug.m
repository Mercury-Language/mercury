% vim: ts=4 sw=4 et ft=mercury
%
% This program is a test case for a bug that causes the declarative debugger
% to ask questions about a node outside the subtree it was asked to debug.
% In this case, the .inp file asks it to debug the subtree of the call to
% io.read, and yet it asks about main.

:- module io_read_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module prolog.

main(!IO) :-
    io.write_string("Please input the number of queens and a period:\n", !IO),
    io.read(NRes, !IO),
    (
        NRes = ok(N),
        iota(N, RevData),
        list.reverse(RevData, Data),
        ( if queen(Data, Out) then
            print_list(Out, !IO)
        else
            io.write_string("No solution\n", !IO)
        )
    ;
        NRes = error(Msg, _LineNumber),
        io.write_string(Msg, !IO)
    ;
        NRes = eof,
        io.write_string("eof", !IO)
    ).

:- pred iota(int::in, list(int)::out) is det.

iota(N, List) :-
    ( if N =< 0 then
        List = []
    else
        iota(N - 1, Tail),
        List = [N | Tail]
    ).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    qperm(Data, Out),
    safe(Out).

:- pred qperm(list(int)::in, list(int)::out) is nondet.

qperm([], []).
qperm([X | Y], K) :-
    qdelete(U, [X | Y], Z),
    K = [U | V],
    qperm(Z, V).

:- pred qdelete(int::out, list(int)::in, list(int)::out) is nondet.

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
