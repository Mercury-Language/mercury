:- module ambiguous_method.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list.

main(!S) :-
	(
		test([0], 1)
	->
		write_string("yes\n", !S)
	;
		write_string("no\n", !S)
	).

:- typeclass coll(C, E) where [
	func e = C,
	func i(C, E) = C,
	pred m(E::in, C::in) is semidet
].

:- type intcoll == list(int).

:- instance coll(intcoll, int) where [
	(e = []),
	(i(Ns, N) = [N | Ns]),
	m(N, [N | _]),
	m(N, [_ | Ns]) :- m(N, Ns)
].

:- pred test(C, E) <= coll(C, E).
:- mode test(in, in) is semidet.

test(C, E) :-
	m(E, i(C, E)).

