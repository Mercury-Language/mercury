:- module imported_mode.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module exported_mode.

main --> ( { p(41, 42) } -> print("yes"), nl ; print("no"), nl ).

