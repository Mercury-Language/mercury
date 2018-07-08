%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is a variant of coup. It does not use commits,
% but does use the output value of every tabled subgoal.

:- module coup_no_commit.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(p, Solns),
    io.write(Solns, !IO),
    io.write_string("\n", !IO).

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
    (
        q(X)
    ;
        X = 1
    ).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(Y) :-
    p(Z),
    Y = Z + 3,
    Y < 20.
