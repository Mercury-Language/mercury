%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module imported_mode.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module exported_mode.

main(!IO) :-
    ( if p(41, 42) then
        io.print_line("yes", !IO)
    else
        io.print_line("no", !IO)
    ).
