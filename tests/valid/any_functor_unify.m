:- module any_functor_unify.
:- interface.

:- type wrap ---> wrap(int).
:- inst wrap(I) ---> wrap(I).

:- pred p(wrap).
:- mode p(in(any)) is det.

:- implementation.

p(X) :-
	X = wrap(_).
