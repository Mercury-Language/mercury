% This is a regression test;
% the Mercury compiler of 26/10/1999 failed this test.

:- module user_defined_equality.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, std_util.

:- type foo ---> bar ; baz
	where equality is foo_equal.

foo_equal(_, _) :-
	semidet_succeed.

main -->
	( { append([bar], [baz], [baz, bar]) } ->
		print("yes"), nl
	;
		print("no"), nl
	).
