%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks whether we get incorrect answers
% when a consumer gets suspended inside a commit.

:- module consumer_in_commit.

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
    io.write(SolnsP, !IO),
    io.write_string("\n", !IO),
    solutions(q, SolnsQ),
    io.write(SolnsQ, !IO),
    io.write_string("\n", !IO).

:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
    q(X),
    r(X).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(X) :-
    (
        X = 1
    ;
        q(Y),
        X = Y + 1,
        X < 10
    ).

:- pred r(int).
:- mode r(in) is semidet.

r(X) :-
    X < 5,
    q(_).
