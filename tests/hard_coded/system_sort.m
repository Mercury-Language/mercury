% This test case is to ensure that stdin/stdout are redirected correctly for
% system commands.

:- module system_sort.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__call_system("sort", _).

