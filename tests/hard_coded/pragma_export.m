% This test case tests using `pragma export' on a procedure defined
% in a different module (in this case, one from the standard library).
% Previously the MLDS back-end was failing this test case.

:- module pragma_export.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	my_write_string("pragma_export test succeeded\n").

:- pred my_write_string(string::in, io__state::di, io__state::uo) is det.

:- pragma import(my_write_string(in, di, uo),
	[may_call_mercury, thread_safe], "write_str").

:- pragma export(io__write_string(in, di, uo), "write_str").
