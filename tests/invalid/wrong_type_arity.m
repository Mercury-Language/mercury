%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module wrong_type_arity.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(42, X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred p(int::in, int::out) is det.

p(A, C) :-
    q(A, B),
    r(B, C).

:- type t1(T) == int.
:- type t2(T) == int.
:- type t2(T, U) == int.
:- type t2(T, U, V) == int.

% This would work:
% :- pred q(t1(int)::in, int::out) is det.
:- pred q(t1::in, int::out) is det.

q(X, X).

% This would work:
% :- pred r(t2(int)::in, t2(int, int)::out) is det.
:- pred r(t2::in, t2::out) is det.

r(Y, Y).
