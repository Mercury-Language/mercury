% "Hello World" in Mercury.

:- module myset_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module myset, list.

main -->
	print_myset_rep({1}), nl,
	print_myset_rep({1} + {2}), nl,
	print_myset_rep({2} + {1}), nl,
	( { {1} + {2} = [First | Rest] } ->
		print(First), print("+"), print_myset_rep(Rest), nl
	;
		print("failed\n")
	),
	( { {2} + {1} = [First2 | Rest2] } ->
		print(First2), print("+"), print_myset_rep(Rest2), nl
	;
		print("failed\n")
	),
	{ S1 = {3} + {4} },
	{ S2 = {4} + {3} },
	( { append([S1], [S2], [S2, S1]) } ->
		print("yes"), nl
	;
		print("no"), nl
	).
