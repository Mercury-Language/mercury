:- module ho_unique_error.

:- interface.

:- import_module io.

:- pred call_ho(io__state::di, io__state::uo) is multi.

:- implementation.

call_ho -->
	( call(io__write_string, "First\n")
	; call(io__write_string, "Second\n")
	).
