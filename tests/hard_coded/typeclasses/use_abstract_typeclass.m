:- module use_abstract_typeclass.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module abstract_typeclass.
:- import_module list.

main -->
	p(43),
	p("Forty-three"),
	p([43]),
	p([[[], [43]]]),
	{ q(X) },
	p(X).

