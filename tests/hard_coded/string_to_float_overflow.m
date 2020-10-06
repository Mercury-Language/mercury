%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the overflow behaviour of string.to_float/2.
%

:- module string_to_float_overflow.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO) :-
    ( if string.to_float("4123e358", Float1) then
        io.print_line(Float1, !IO)
    else
        io.print_line("conversion (+) failed", !IO)
    ),
    ( if string.to_float("-4123e358", Float2) then
        io.print_line(Float2, !IO)
    else
        io.print_line("conversion (-) failed", !IO)
    ).
