%-----------------------------------------------------------------------------%
:- module func_method.
:- interface.

:- type pair(A, B) ---> pair(A, B).

:- typeclass c(A) where [
	func op(pair(A, B)) = pair(A, B),
	mode op(in) = out is det
].

:- instance c(int).

:- implementation.

:- instance c(int) where [
	func(op/1) is op_int
].

:- func op_int(pair(int, T)) = pair(int, T).
:- mode op_int(in) = out is det.
op_int(X) = X.

%-----------------------------------------------------------------------------%
