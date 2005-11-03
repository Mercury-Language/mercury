:- module intermod_poly_mode_2.

:- interface.

:- func new(T::in(I)) = (T::out(I)) is det.

:- implementation.

:- pragma foreign_proc("C",
	new(X::in(I)) = (R::out(I)),
	[promise_pure, will_not_call_mercury],
"
	R = X;
").
