:- module inference_test.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int.

:- typeclass foo(T) where [
	pred p(T::in, int::out) is det
].

:- instance foo(int) where [
	pred(p/2) is forty_two
].

% :- pred forty_two(int, int) is det.
:- mode forty_two(in, out) is det.
forty_two(X, Y) :- Y is X + 42.

main -->
	( { q(0) } ->
		print("yes\n")
	;
		print("no\n")
	).

% :- pred q(T) <= foo(T).
q(X) :- p(X, 42).

