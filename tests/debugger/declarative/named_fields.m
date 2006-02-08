:- module named_fields.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	gen_t(T),
	_ = swap(T).

:- type t --->
	t(
		field1 :: int,
		field2 :: int
	).

:- pred gen_t(t :: out) is det.

gen_t(t(1, 2)).

:- func swap(t) = t.

swap(t(X, Y)) = t(Y, X).
