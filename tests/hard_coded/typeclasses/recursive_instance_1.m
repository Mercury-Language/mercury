:- module recursive_instance_1.
:- interface.

:- import_module int, io.
:- import_module list.

:- typeclass foo(T, U) where [
	func bar(T) = U
].

:- instance foo(int, list(T)) <= foo(list(int), T) where [
	bar(N) = ( N < 0 -> [bar([N+1])] ; [] )
].

:- instance foo(list(T), int) <= foo(T, list(int)) where [
	( bar([X | Xs]) = N + bar(Xs) :-
		bar(X) = B,
		( B = [N | _]
		; B = [],
			N = 1
		)
	),
	( bar([]) = 0 )
].

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util, char.

main -->
	{ X = bar([0,1,2]) },
	io__write_int(X),
	io__nl.
