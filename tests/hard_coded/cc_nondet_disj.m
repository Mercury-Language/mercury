:- module cc_nondet_disj.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module list.

main --> io__read_line(Res),
	( { Res = ok(['y'|_]), Message = "Yes\n" }
	; { Res = ok(['n'|_]), Message = "No\n" }
	; { Message = "Huh?\n" }
	),
	io__write_string(Message).

/***
% This test used to be written as follows, but currently
% the unique mode analysis is not smart enough to realize
% that the disjuncts which update the I/O state won't
% backtrack over I/O if the code is written like that.
main --> io__read_line(Res),
	( { Res = ok(['y'|_]) }, io__write_string("Yes\n")
	; { Res = ok(['n'|_]) }, io__write_string("No\n")
	; io__write_string("Huh?\n")
	).
***/
