:- module unqualified_method.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module unqualified_method2.

main -->
	print_modified(1).


:- pred print_modified_int(int::in, io__state::di, io__state::uo) is det.

print_modified_int(_) -->
	io__write_string("This is the wrong method.\n").

