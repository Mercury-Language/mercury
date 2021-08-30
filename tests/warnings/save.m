%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module save.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module list.
:- import_module int.
:- import_module prolog.
:- import_module string.

main(!IO) :-
    ( if
        data(Data),
        queen(Data, Out)
    then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ),
    prolog.see("save_file", SeeRes, !IO),
    (
        SeeRes = ok,
        io.read_file_as_string(ReadRes, !IO),
        (
            ReadRes = ok(FileContents),
            io.write_string(FileContents, !IO)
        ;
            ReadRes = error(_PartialFileContents, ReadError),
            io.error_message(ReadError, ReadMsg),
            io.format("Read error: %s\n", [s(ReadMsg)], !IO)
        )
    ;
        SeeRes = error(SeeError),
        io.error_message(SeeError, SeeMsg),
        io.write_string("See error: " ++ SeeMsg ++ "\n", !IO)
    ).

:- pred data(list(int)::out) is det.

data([1, 2, 3, 4, 5]).

:- pred queen(list(int)::in, list(int)::out) is nondet.

queen(Data, Out) :-
    qperm(Data, Out),
    safe(Out).

:- pred qperm(list(T)::in, list(T)::out) is nondet.

qperm(L, K) :-
    (
        L = [],
        K = []
    ;
        L = [_ | _], qdelete(U, L, Z),
        K = [U | V],
        qperm(Z, V)
    ).

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

nodiag(B, D, L) :-
    (
        L = []
    ;
        L = [N | T],
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
        nodiag(B, D1, T)
    ).

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
