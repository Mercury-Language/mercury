:- module uniq_duplicate_call.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module array.

main -->
	{ dup_call(1, 2, Array1, Array2) },
	io__write_int(array__lookup(Array1, 0)),
	io__nl,
	io__write_int(array__lookup(Array2, 0)),
	io__nl.

:- pred dup_call(T::in, T::in, array(T)::out, array(T)::out) is det.

dup_call(T, T2,
	array__set(array__init(1, T), 0, T2),
	array__init(1, T)).

