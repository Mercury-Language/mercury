% Test the warning for ambiguous overloading.
:- module ambiguous_overloading.

:- interface.
:- import_module list.

:- type foo ---> f ; g.
:- type bar ---> f ; h.

:- pred ambig_overload(list(foo)::out) is det.

:- implementation.

ambig_overload(L) :-
	A = f, B = f, C = f, D = f, E = f, F = f, G = f,
	L = [A, B, C, D, E, F, G].

