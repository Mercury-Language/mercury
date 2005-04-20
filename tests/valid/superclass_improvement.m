:- module superclass_improvement.
:- interface.

:- typeclass z(A, B) <= (A -> B) where [].

:- typeclass x(A, B) <= z(A, B) where [ func f(A) = B ].
:- typeclass y(A, B) <= z(A, B) where [ func g(A) = B ].

:- pred p(int::in) is semidet.

:- implementation.

:- instance z(int, int) where [].

:- instance x(int, int) where [ (f(N) = N) ].
:- instance y(int, int) where [ (g(_) = 1) ].

p(N) :-
	% The FD on z/2 is required in order to prove that these two
	% function calls have the same return type.
	f(N) = g(N).

