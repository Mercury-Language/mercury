:- module exist_disjunction.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type t
	--->	a(int)
	;	b(int)
	.

:- type u
	--->	some [T] (u(T) => v(T))
	%--->	some [T] u(T)
	;	f.

:- typeclass v(T) where [].

:- type w
	--->	f(int).

:- instance v(w) where [].

:- pred p(t, u).
:- mode p(in, out) is det.

p(T, V) :-
    (
    	T = a(C),
	V = 'new u'(f(C))
    ;
    	T = b(C),
	V = 'new u'(f(C))
    ).

main -->
	{ p(a(42), X) },
	write(X), nl.

