% The ROTD of 25/10/1999 gave a spurious mode error for this
% test case because the type-info argument of the cons_id was not being
% put in the non-locals of the construction.
:- module existential_cons.

:- interface.

:- type exist_cons
	---> some[T] exist_cons(a::int, b::T, c::int).

:- pred construct_exist_cons(exist_cons::out) is det.

:- implementation.

construct_exist_cons(ExistCons) :-
	A = 1,
	X = 'b',
	C = 2,
	ExistCons = 'new exist_cons'(A, X, C).

