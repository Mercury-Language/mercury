% Mercury 0.9 had a bug where a program compiled with tracing
% could not have a module with the same name as one of the
% browser library modules, which are not user-visible.
:- module parse.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_string("ok\n").
