% This is a regression test; with `--no-inlining --nondet-copy-out',
% Mercury 0.10.1 generated code for this which went into an infinite
% loop.

:- module nondet_copy_out.

:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	( { p(2) } ->
		io__write_string("success.\n")
	;
		io__write_string("failure.\n")
	).

:- pred p(int::out) is multi.

p(1).
p(2).
