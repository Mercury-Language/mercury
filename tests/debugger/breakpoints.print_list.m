:- module breakpoints__print_list.

:- interface.

:- import_module list, io.

:- pred print_list(list(int), io__state, io__state).
:- mode print_list(in, di, uo) is det.

:- func string / string = string.
:- func string - string = string.

:- pred test_only_in_printlist(io__state::di, io__state::uo) is det.

:- pred test_in_both(io__state::di, io__state::uo) is det.

:- implementation.

print_list(Xs) -->
	(
		{ Xs = [] }
	->
		io__write_string("[]\n")
	;
		io__write_string("["),
		print_list_2(Xs),
		io__write_string("]\n")
	).

:- pred print_list_2(list(int), io__state, io__state).
:- mode print_list_2(in, di, uo) is det.

print_list_2([]) --> [].
print_list_2([X|Xs]) --> 
	io__write_int(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		print_list_2(Xs)
	).

Str1 / Str2 = Str1 ++ "/" ++ Str2.
Str1 - Str2 = Str1 ++ "-" ++ Str2.

test_only_in_printlist(!IO) :-
	io__write_string("test_only_in_printlist\n", !IO).

test_in_both(!IO) :-
	io__write_string("test_in_both\n", !IO).
