:- module overloading.
:- interface.

:- type foo(T)
	--->	f1(T, T)
	;	f2.

:- pred bar(foo(T)::in, T::in) is semidet.

:- implementation.

% The symbol f1/2 is overloaded here, but the overloading is resolved
% because the baz/1 constraint cannot be reduced.
bar(f1(X, _), X).

:- typeclass baz(T) where [].
:- func f1(T, T) = foo(T) <= baz(T).
f1(_, _) = f2.

