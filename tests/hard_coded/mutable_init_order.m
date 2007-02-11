% Test that mutable initialisation occurs in the correct order
% relative to initialise declarations.
%
:- module mutable_init_order.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- mutable(global, int, 561, ground, [untrailed, attach_to_io_state]).
:- initialise foo/2.

:- pred foo(io::di, io::uo) is det.

foo(!IO) :-
	set_global(908, !IO).

main(!IO) :-
	get_global(V, !IO),
	io.write_string("V = ", !IO),
	io.write_int(V, !IO),
	io.nl(!IO).
