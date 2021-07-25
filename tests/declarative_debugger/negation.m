%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module negation.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module library_forwarding.

main(S0, S) :-
    p(1, X),
    io.write_int(X, S0, S1),
    io.nl(S1, S).

:- pred p(int, int).
:- mode p(in, out) is det.

p(A, B) :-
    ( if
        not (
            r(A, C),
            not q(C)
        )
    then
        B = 42
    else
        B = A
    ).

:- pred q(int::in) is semidet.

q(N) :-
    N > 10.

:- pred r(int::in, int::out) is multi.

r(K, K + 10).
r(K, K + 20).
