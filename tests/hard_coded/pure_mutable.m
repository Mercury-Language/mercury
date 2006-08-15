:- module pure_mutable.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- mutable(global, int, 561, ground,
	[untrailed, attach_to_io_state]).

main(!IO) :-
	get_global(X0, !IO),
	io.format("Initial value of global = %d\n", [i(X0)], !IO),
	set_global(X0 + 1, !IO),
	get_global(X, !IO),
	io.format("Final value of global = %d\n", [i(X)], !IO).
