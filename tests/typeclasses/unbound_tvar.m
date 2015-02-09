:- module unbound_tvar.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ p(a(1)) }.

:- typeclass c(T) where [ 
	pred p(T), 
	mode p(in) is det 
].

:- type t(A, B) ---> a(A) ; b(B).

:- instance c(t(A,B)) where [ pred(p/1) is x ].

:- pred x(t(A, B)::in) is det.
x(_).

