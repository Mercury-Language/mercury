:- module transitive_import_class3.

:- interface.

	% For all instances the following must hold:
	%	all [X, Int] (X = from_int(to_int(X)))
:- typeclass my_enum(T) where [
	func to_int(T) = int,
	func from_int(int) = T is semidet
].

:- implementation.

