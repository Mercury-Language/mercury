:- module field_detism.

:- interface.

:- type t
	--->	a
	;	b(x :: int).

:- pred p(t::in, t::out) is det.

:- implementation.

:- import_module int.

p(a, a).
p(T0 @ b(N), T) :-
	T = T0 ^ x := N + 1.
