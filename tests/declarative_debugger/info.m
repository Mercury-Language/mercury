%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module info.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    ( if
        info.last([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], A),
        info.last([101, 112, 103, 104, 105, 106, 107, 108], B)
    then
        q(0, "lala", C, D),
        q(1, "lala", E, F),
        io.write(A, !IO),
        io.write(B, !IO),
        io.write(C, !IO),
        io.write(D, !IO),
        io.write(E, !IO),
        io.write(F, !IO)
    else
        true
    ).

:- pred q(int::in, T::in, t(t(t(T)))::out, int::out) is det.

q(N, X, Y, M) :-
    ( if N = 0 then
        Y = f(N, X),
        M = 2
    else
        M = fproc(N),
        Y = t(t(t(X)))
    ).

:- pred last(list(T)::in, t(T)::out) is semidet.

info.last([H], t(H)).
info.last([_ , H2 | T], L) :-
    info.last([H2 | T], L).

:- type t(T)
    --->    t(T).

:- func f(int, T) = t(t(t(T))).

f(_, X) = Y :-
    Z = t(t(t(X))),
    Y = Z.

:- func fproc(int) = int.

:- pragma foreign_proc("C", fproc(X::in) = (Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X + 1;
").
