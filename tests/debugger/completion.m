:- module completion.

:- interface.

:- import_module io.

:- include_module completion__sub1, completion__sub2.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_string("ok\n").

:- func z = int.
z = 0.

:- func zz = int.
zz = 0.
