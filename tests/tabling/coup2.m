%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test for whether a change of leaders is correctly handled.
%
% On 12 Marth 2004, this program did not work correctly (some of p's
% answers were missing) due to a bug in the completion implementation.

:- module coup2.

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
        p(Y), % here a coup takes place
        X = 2 * Y,
        X < 20
    ).
