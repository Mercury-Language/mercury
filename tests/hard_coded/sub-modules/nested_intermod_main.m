:- module nested_intermod_main.
:- interface.
:- import_module io.

:- pred xyzzy(int).
:- mode xyzzy(in) is semidet.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module nested_intermod.

main -->
	test(1),
	test(2),
	test(3),
	test(4),
	test(5).

:- pred test(int::in, io__state::di, io__state::uo) is det.

test(X) -->
	print("X = "), print(X), print(": "),
	( { xyzzy(X) } ->
		print("yes")
	;
		print("no")
	),
	nl.

xyzzy(X) :-
	foo(X).

:- end_module nested_intermod_main.
