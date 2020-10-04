%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test --
% a previous version of Mercury generated code for this test
% which ran out of memory, if the test was compiled with `-O3'.

:- module bigtest.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module float.
:- import_module list.
:- import_module lp.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

main(!IO) :-
    data(Eqns, Dir, Obj, Varset),
    lp_solve(Eqns, Dir, Obj, Varset, Result, !IO),
    (
        Result = satisfiable(_, _),
        io.write_string("satisfiable.\n", !IO)
    ;
        Result = unsatisfiable,
        io.write_string("unsatisfiable.\n", !IO)
    ).

:- pred data(equations::out, direction::out, objective::out, varset::out)
    is det.

data(Eqns, max, Obj, Varset) :-
    varset.init(Varset0 ),
    varset.new_vars(80, Vars0, Varset0, Varset),
    list.sort(Vars0, Vars),
    list.map(mkeqn, Vars, Eqns),
    list.map(mkobj, Vars, Obj).

:- pred mkeqn(var::in, equation::out) is det.

mkeqn(Var, eqn([Var - 1.0], (=<), 42.0)).

:- pred mkobj(var::in, coeff::out) is det.

mkobj(Var, Var - 1.0).
