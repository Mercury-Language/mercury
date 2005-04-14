
:- module typeclass_test_5.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass c1(T) where [].
:- typeclass c2(T) <= c1(T) where [ 
	pred p(T), mode p(out) is det
].

:- pred get_int(int::out) is det.
:- pred get_int2(int::out) is det.

:- implementation.

:- instance c2(int) where [
	pred(p/1) is get_int,
	pred(p/1) is get_int2
].

main --> [].

get_int(42).
get_int2(43).
