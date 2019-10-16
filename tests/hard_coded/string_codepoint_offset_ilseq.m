%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_codepoint_offset_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S1 = string.between(S0, 0, count_code_units(S0) - 1),
    S = "a" ++ S0 ++ "b" ++ S1 ++ "z",

    io.write_string("start counting from offset 0\n", !IO),
    test_codepoint_offset(S, 0, 0, !IO),
    io.nl(!IO),

    io.write_string("start counting from offset 1\n", !IO),
    test_codepoint_offset(S, 1, 0, !IO),
    io.nl(!IO),

    io.write_string("start counting from offset 2\n", !IO),
    test_codepoint_offset(S, 2, 0, !IO),
    io.nl(!IO).

:- pred test_codepoint_offset(string::in, int::in, int::in, io::di, io::uo)
    is det.

test_codepoint_offset(S, StartOffset, Count, !IO) :-
    ( if string.codepoint_offset(S, StartOffset, Count, Offset) then
        io.format("string.codepoint_offset(S, %d, %d, %d); ",
            [i(StartOffset), i(Count), i(Offset)], !IO),
        ( if string.index(S, Offset, Char) then
            io.write_string("Char = ", !IO),
            write_char_or_hex(Char, !IO),
            io.nl(!IO)
        else
            io.write_string("string.index/3 failed\n", !IO)
        ),
        test_codepoint_offset(S, StartOffset, Count + 1, !IO)
    else
        io.format("string.codepoint_offset(S, %d, %d, _) failed\n",
            [i(StartOffset), i(Count)], !IO)
    ).

:- pred write_char_or_hex(char::in, io::di, io::uo) is det.

write_char_or_hex(Char, !IO) :-
    ( if Char = '\ufffd' ; char.is_surrogate(Char) then
        io.format("%#x", [i(char.to_int(Char))], !IO)
    else
        io.write_char(Char, !IO)
    ).
