% vim: ft=mercury ts=4 sts=4 sw=4 et

:- module no_warn_format_imports.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    S = string.format("%s", [s("foo")]),
    io.write_string(S, !IO).
