:- module typeclass_test_7.
:- interface.
:- import_module int.
:- pred p(T::in, int::out) is det.

:- implementation.

p(X, Y) :-
	F = 42,
	N = type_num(X),	% error should be reported on *this* line
	N2 = N * 2,
	Y = N2 + F.

:- typeclass numbered_type(T) where [
	func type_num(T) = int
].

:- instance numbered_type(int) where [
	func(type_num/1) is foo_type_num
].

:- func foo_type_num(T) = int.
foo_type_num(_) = 42.

