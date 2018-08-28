%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for XXX.
% The .exp2 file is for asm_fast.gc bootchecks.
%
%---------------------------------------------------------------------------%

:- module shallow.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module list.
:- import_module int.
:- import_module shallow2.

main(!IO) :-
    ( if data(Data), queen(Data, Out) then
        print_list(Out, !IO)
    else
        io.write_string("No solution\n", !IO)
    ).

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
