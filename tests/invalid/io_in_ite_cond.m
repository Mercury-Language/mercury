:- module io_in_ite_cond.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- pred foo(io__state::di, io__state::uo) is semidet.
:- external(foo/2).

% This should be a unique mode error, since if foo does I/O
% before failing, we won't be able to undo it before printing "No".

main -->
	( foo ->
		io__write_string("Yes")
	;
		io__write_string("No")
	).
