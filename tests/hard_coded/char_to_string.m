%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module char_to_string.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % null character
    test_char_to_string_fwd(char.det_from_int(0), !IO),
    % surrogate code points
    test_char_to_string_fwd(char.det_from_int(0xd83d), !IO),
    test_char_to_string_fwd(char.det_from_int(0xde00), !IO),
    % non-BMP code point
    test_char_to_string_fwd('😀', !IO),
    io.nl(!IO),

    S = "😀",
    S0 = between(S, 0, 1),
    S1 = between(S, 1, length(S)),

    % empty string
    test_char_to_string_rev("", !IO),
    % string too long
    test_char_to_string_rev(S0 ++ S, !IO),
    test_char_to_string_rev(S ++ S0, !IO),
    % ill-formed (unpaired surrogate in UTF-16 backends)
    test_char_to_string_rev(S0, !IO),
    test_char_to_string_rev(S1, !IO),
    % non-BMP code point
    test_char_to_string_rev(S, !IO).

:- pred test_char_to_string_fwd(char::in, io::di, io::uo) is cc_multi.

test_char_to_string_fwd(Char, !IO) :-
    ( try []
        char_to_string(Char, Str)
    then
        io.write_string("succeeded: ", !IO),
        write_string_debug(Str, !IO),
        io.nl(!IO)
    catch_any Excp ->
        io.write_string("exception: ", !IO),
        io.write(Excp, !IO),
        io.nl(!IO)
    ).

:- pred test_char_to_string_rev(string::in, io::di, io::uo) is cc_multi.

test_char_to_string_rev(Str, !IO) :-
    ( try []
        char_to_string(Char, Str)
    then
        io.write_string("succeeded: ", !IO),
        write_char_or_hex(Char, !IO),
        io.nl(!IO)
    else
        io.write_string("failed\n", !IO)
    catch_any Excp ->
        io.write_string("exception: ", !IO),
        io.write(Excp, !IO),
        io.nl(!IO)
    ).

:- pred write_string_debug(string::in, io::di, io::uo) is det.

write_string_debug(S, !IO) :-
    write_string_debug_loop(S, 0, !IO).

:- pred write_string_debug_loop(string::in, int::in, io::di, io::uo) is det.

write_string_debug_loop(S, Index, !IO) :-
    ( if string.index_next(S, Index, NextIndex, Char) then
        write_char_or_hex(Char, !IO),
        write_string_debug_loop(S, NextIndex, !IO)
    else
        true
    ).

:- pred write_char_or_hex(char::in, io::di, io::uo) is det.

write_char_or_hex(Char, !IO) :-
    ( if char.is_surrogate(Char) then
        io.format("%#x", [i(char.to_int(Char))], !IO)
    else
        io.write_char(Char, !IO)
    ).
