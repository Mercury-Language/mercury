:- module cc_nondet_disj.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module list.

main --> io__read_line(Res),
	( { Res = ok(['y'|_]) }, io__write_string("Yes\n")
	; { Res = ok(['n'|_]) }, io__write_string("No\n")
	; io__write_string("Huh?\n")
	).
