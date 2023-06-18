%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test output of special characters such as \r
%
% The .exp file is for systems that use "\n" as the end of line.
% The .exp2 file is for system that use "\r\n" as the end of line.
% (While we we do compare the test outputs using diff --strip-trailing-cr,
% diff gets confused in this case because we output additional carriage
% returns.)
%
%---------------------------------------------------------------------------%

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
