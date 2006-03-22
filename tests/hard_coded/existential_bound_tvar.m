:- module existential_bound_tvar.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- import_module list.
:- import_module type_desc.

main -->
	{ blah(101, X) },
	print("X: value = "), print(X), nl,
	print("X: type = "), print(type_of(X)), nl,

	{ blah2(101, Y) },
	print("Y: value = "), print(Y), nl,
	print("Y: type = "), print(type_of(Y)), nl,

	(
		{ blah3([101], Z) }
	->
		print("Z: value = "), print(Z), nl,
		print("Z: type = "), print(type_of(Z)), nl
	;
		write("ERROR\n")
	).

:- some [T1] pred blah(T, T1).
:- mode blah(in, out) is det.

blah(X, X).

:- some [T1] pred blah2(T, T1).
:- mode blah2(in, out) is det.

blah2(X, [X]).

:- some [T1] pred blah3(list(T), T1).
:- mode blah3(in, out) is semidet.

blah3([X], X).
