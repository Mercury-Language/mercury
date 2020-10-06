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
    print("Hello world\r\n", !IO),
    print("\r\n", !IO),
    print("\a\b\v\f\t\n", !IO),
    print("\077\", !IO),
    print("\0123\", !IO),
    print("\0321\", !IO),
    print("\n", !IO).
