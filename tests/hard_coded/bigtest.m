%
% This is a regression test --
% a previous version of Mercury generated code for this test
% which ran out of memory, if the test was compiled with `-O3'.
%

:- module bigtest.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module lp.
:- import_module float, list, require, term, varset, pair.

main -->
	{ data(Eqns, Dir, Obj, Varset) },
	lp_solve(Eqns, Dir, Obj, Varset, Result),
	(
		{ Result = satisfiable(_, _) },
		io__write_string("satisfiable.\n")
	;
		{ Result = unsatisfiable },
		io__write_string("unsatisfiable.\n")
	).

:- pred data(equations::out, direction::out, objective::out,
		varset::out) is det.

data(Eqns, max, Obj, Varset) :-
	varset__init(Varset0 ),
	varset__new_vars(Varset0, 80, Vars0, Varset),
	list__sort(Vars0, Vars),
	list__map(mkeqn, Vars, Eqns),
	list__map(mkobj, Vars, Obj).

:- pred mkeqn(var::in, equation::out) is det.

mkeqn(Var, eqn([Var - 1.0], (=<), 42.0)).

:- pred mkobj(var::in, coeff::out) is det.

mkobj(Var, Var - 1.0).

