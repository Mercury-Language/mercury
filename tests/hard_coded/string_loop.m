% Tom says "The following module loops forever on mundook".

:- module string_loop.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, float, string.

main(IO0, IO) :-
	string__format("%ei\n", [f(0.0)], Str),
	io__write_string(Str, IO0, IO).
