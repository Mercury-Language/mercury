%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks whether we get incorrect answers
% when a consumer gets suspended inside solutions.

:- module consumer_in_solutions.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(q, Solns),
    io.write(Solns, !IO),
    io.write_string("\n", !IO).

:- pragma minimal_model(q/1).
:- pred q(pair(int, list(int))).
:- mode q(out) is nondet.

q(X - L) :-
    p(X),
    solutions(p, L).

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(1).
p(2).
p(3).
