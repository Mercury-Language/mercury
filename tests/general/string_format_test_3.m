:- module string_format_test_3.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, string.

main --> io__format("%s\n", []).
