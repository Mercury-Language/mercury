%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the case of calling a semidet pred in
% a nondet lambda expression; Mercury-0.4 got this case wrong.

:- module semidet_lambda.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

main(!IO) :-
    q(Y),
    ( if Y = [Z] then
        io.write_int(Z, !IO),
        io.nl(!IO)
    else
        io.write_string("Hello, world\n", !IO)
    ).

:- pred p(int::out) is semidet.

p(42).

:- pred q(list(int)::out) is det.

q(Y) :-
    solutions((pred(X::out) is nondet :- p(X)), Y).
