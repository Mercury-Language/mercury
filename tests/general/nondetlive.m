:- module nondetlive.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, list, std_util.

main -->
	{ solutions(p, List) },
	write_list(List),
	io__write_string("\n").

:- pred write_list(list(int), io__state, io__state).
:- mode write_list(in, di, uo) is det.

write_list([]) --> [].
write_list([I|Is]) -->
	io__write_int(I),
	io__write_string(" "),
	write_list(Is).

:- pred p(int::out) is multi.

p(X) :-
	q(W),
	some [Y] (
		(
			Y = 1
		;
			Y = 2
		),
		Z is Y + W
	),
	q(V),
	X is Z * V.

:- pred q(int::out) is multi.

q(1).
q(2).
