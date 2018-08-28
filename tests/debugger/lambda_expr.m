%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The version of the system on 16 July 2004
% generated bad RTTI for the lambda expression, which caused the debugger
% to believe that neither argument of the lambda predicate was instantiated.

:- module lambda_expr.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    P = (pred(X::in, Y::out) is det :- Y = X + 1),
    P(1, Z),
    io.write_int(Z, !IO),
    io.nl(!IO).
