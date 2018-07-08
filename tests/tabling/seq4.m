%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is supposed to be the same as "coup2', albeit without a coup.
%
% Currently (12 Mar 2004), it does not work correctly for q/1 (some answers
% are missing perhaps due to a bug in the completion algorithm.

:- module seq4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(p, SolnsP),
    io.write_string("P = ", !IO),
    io.write(SolnsP, !IO),
    io.write_string("\n", !IO),
    solutions(q, SolnsQ),
    io.write_string("Q = ", !IO),
    io.write(SolnsQ, !IO),
    io.write_string("\n", !IO).

:- pred p(int::out) is nondet.
:- pragma minimal_model(p/1).

p(X) :-
    (
        p(Y),
        X = 3 * Y,
        X < 20
    ;
        q(X)
    ).

:- pred q(int::out) is nondet.
:- pragma minimal_model(q/1).

q(X) :-
    (
        X = 1
    ;
        q(Y),
        X = 2 * Y,
        X < 20
    ).
