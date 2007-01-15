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

:- mutable(const, int, 562, ground, [constant]).

:- mutable(thrlocal, int, 563, ground,
	[untrailed, attach_to_io_state, thread_local]).

main(!IO) :-
	get_global(X0, !IO),
	io.format("Initial value of global = %d\n", [i(X0)], !IO),
	set_global(X0 + 1, !IO),
	get_global(X, !IO),
	io.format("Final value of global = %d\n", [i(X)], !IO),

	get_const(C),
	io.format("Value of const = %d\n", [i(C)], !IO),

	get_thrlocal(Y0, !IO),
	io.format("Initial value of thrlocal = %d\n", [i(Y0)], !IO),
	set_thrlocal(Y0 + 1, !IO),
	get_thrlocal(Y, !IO),
	io.format("Final value of thrlocal = %d\n", [i(Y)], !IO).

