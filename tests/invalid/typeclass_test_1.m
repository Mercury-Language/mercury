:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> io__write_int(type_num(42)).

:- typeclass numbered_type(T) where [
	func type_num(T) = int
].

:- instance foo(int) where [
	type_num is foo_type_num
].

foo_type_num _ = 42.

