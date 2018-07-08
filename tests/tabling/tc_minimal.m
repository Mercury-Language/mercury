%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tc_minimal.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(tc(1), Solns),
    io.write(Solns, !IO),
    io.write_string("\n", !IO).

:- pred tc(int::in, int::out) is nondet.
:- pragma minimal_model(tc/2).

tc(A, B) :-
    edge(A, C),
    (
        B = C
    ;
        tc(C, B)
    ).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(2, 1).
edge(3, 4).
