:- module extract_typeinfo.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	p(1),
	io__nl.

:- import_module list.

:- typeclass foo(T) where [
	pred printit(T::in, io__state::di, io__state::uo) is det
].

:- instance foo(int) where [
	pred(printit/3) is io__write_int
].

:- pred p(T, io__state, io__state) <= foo(T).
:- mode p(in, di, uo) is det.

p(X) -->
	(
			% At this call, the type-info gets extracted from the
			% typeclass-info.
		{ list__append([X], [X], [X,X]) }
	->
		printit(X)
	;
		[]
	).
