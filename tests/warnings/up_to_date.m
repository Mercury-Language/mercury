:- module up_to_date.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> io__write_string("Hello, world\n").
