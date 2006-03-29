:- module term_to_univ_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module term, term_io, varset, univ.

main -->
	{ X = 4 },
	{ type_to_univ(X, Univ0) },
	{ type_to_univ(Univ0, Univ) },
	{ term__univ_to_term(Univ, Term) },
	write(Term), nl,
	{ varset__init(VarSet) },
	write_term(VarSet, Term), nl.
