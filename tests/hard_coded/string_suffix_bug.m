%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_suffix_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO) :-
    ( if string.suffix("testing string__suffix", "suffix") then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).
