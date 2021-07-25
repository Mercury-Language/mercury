%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_test_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    string.append("abc", "def", S1),
    io.write_string(S1, !IO),
    io.write_string("\n", !IO),
    string.append("", "abcdef", S2),
    io.write_string(S2, !IO),
    io.write_string("\n", !IO),
    string.append("abcdef", "", S3),
    io.write_string(S3, !IO),
    io.write_string("\n", !IO),
    ( if string.append("", S4, "abcdef") then
        io.write_string(S4, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    ( if string.append("abc", S5, "abcdef") then
        io.write_string(S5, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    ( if string.remove_prefix("abc", "abcdef", S6) then
        io.write_string(S6, !IO),
        io.nl(!IO)
    else
        io.write_string("failed\n", !IO)
    ).

%   ( if string.append(S6, "", "abcdef") then
%       io.write_string(S6, !IO),
%       io.write_string("\n", !IO)
%   else
%       io.write_string("failed\n", !IO)
%   ),
%   ( if string.append(S7, "def", "abcdef") then
%       io.write_string(S7, !IO),
%       io.write_string("\n", !IO)
%   else
%       io.write_string("failed\n", !IO)
%   ).
