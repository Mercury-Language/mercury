
:- module accessibility2.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module sub_a:sub1.
:- import_module sub_a.

main -->
        io__write_string("Hello.\n").



