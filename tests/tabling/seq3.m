%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks the correctness of the code that performs
% the fixpoint loop returning answers to consumers. The fixpoint
% computation has to repeatedly switch from one consumer to the
% other to obtain all answers for p/1.

:- module seq3.

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

:- pred p(int).
:- mode p(out) is nondet.

:- pragma minimal_model(p/1).

p(X) :-
    (
        X = 1
    ;
        p(Y),
        X = 2 * Y,
        X < 20
    ;
        p(Y),
        X = 3 * Y,
        X < 20
    ).
