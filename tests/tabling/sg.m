%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is the same generation program on a 24-24-2 cylinder.

:- module sg.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(sg1, Solns1),
    io.write(Solns1, !IO),
    io.write_string("\n", !IO),
    solutions(sg, Solns),
    io.write_int(list.length(Solns), !IO),
    io.write_string("\n", !IO).

:- pred sg(pair(int, int)::out) is nondet.

sg(X - Y) :-
    tsg(X, Y).

% just to test a non-open call.
:- pred sg1(int::out) is nondet.

sg1(X) :-
    tsg(1, X).

:- pred tsg(int, int).
:- mode tsg(in, out) is nondet.
:- mode tsg(out, out) is nondet.
:- pragma minimal_model(tsg/2).
:- pragma promise_pure(tsg/2).

tsg(X, Y) :-
    cyl(X, X1),
    tsg(X1, Y1),
    acyl(Y1, Y).
tsg(X::in, X::out).
tsg(X::out, X::out) :-
    (
        X = 1
    ;
        cyl(_, X)
    ).

:- pred cyl(int, int).
:- mode cyl(in, out) is nondet.
:- mode cyl(out, out) is multi.

:- pred acyl(int, int).
:- mode acyl(in, out) is nondet.

:- pragma fact_table(cyl/2, "sg_cyl").
:- pragma fact_table(acyl/2, "sg_acyl").
