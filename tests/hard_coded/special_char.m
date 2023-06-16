%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test output of special characters such as \r
%

:- module special_char.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.print("Hello world\r\n", !IO),
    io.print("\r\n", !IO),
    io.print("\a\b\v\f\t\n", !IO),
    io.print("\077\", !IO),
    io.print("\0123\", !IO),
    io.print("\0321\", !IO),
    io.print("\n", !IO).
