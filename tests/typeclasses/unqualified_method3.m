:- module unqualified_method3.

:- interface.

:- import_module io.

:- pred print_modified_int(int::in, io__state::di, io__state::uo) is det.

:- implementation.

print_modified_int(_) -->
	io__write_string("This is the right method.\n").
