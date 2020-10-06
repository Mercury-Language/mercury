%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_codepoint.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Str = "aßξ啕𐀀.",

    io.write_string("count_code_units:\n", !IO),
    count_code_units(Str, NumCodeUnits),
    io.write_int(NumCodeUnits, !IO),
    io.nl(!IO),

    io.write_string("\ncount_codepoints:\n", !IO),
    count_codepoints(Str, NumCodePoints),
    io.write_int(NumCodePoints, !IO),
    io.nl(!IO),

    io.write_string("\ncodepoint_offset:\n", !IO),
    test_codepoint_offset(Str, 0, !IO),
    test_codepoint_offset(Str, 1, !IO),
    test_codepoint_offset(Str, 2, !IO),
    test_codepoint_offset(Str, 3, !IO),
    test_codepoint_offset(Str, 4, !IO),
    test_codepoint_offset(Str, 5, !IO),
    test_codepoint_offset(Str, 6, !IO),

    io.write_string("\nto_char_list:\n", !IO),
    string.to_char_list(Str, CharList),
    io.write(CharList, !IO),
    io.nl(!IO),

    io.write_string("\nfrom_char_list:\n", !IO),
    string.from_char_list(CharList, Str1),
    io.write_string(Str1, !IO),
    io.nl(!IO),

    io.write_string("\nfrom_rev_char_list:\n", !IO),
    string.from_rev_char_list(CharList, RevStr),
    io.write_string(RevStr, !IO),
    io.nl(!IO),

    io.write_string("\nto_code_unit_list:\n", !IO),
    string.to_code_unit_list(Str, CodeUnitList),
    io.write(CodeUnitList, !IO),
    io.nl(!IO),

    ( if string.from_code_unit_list(CodeUnitList, Str2) then
        io.write_string("\nfrom_code_unit_list:\n", !IO),
        io.write_string(Str2, !IO),
        io.nl(!IO)
    else
        true
    ),

    io.write_string("\nindex_next:\n", !IO),
    test_index_next(Str, 0, !IO),

    io.write_string("\nindex_next(-1):\n", !IO),
    test_index_next(Str, -1, !IO),

    io.write_string("\nunsafe_index_next:\n", !IO),
    test_unsafe_index_next(Str, 0, !IO),

    io.write_string("\nunsafe_prev_index:\n", !IO),
    test_unsafe_prev_index(Str, length(Str), !IO),

    io.write_string("\nsplit_by_codepoint:\n", !IO),
    test_split_by_codepoint(Str, -1, !IO),
    test_split_by_codepoint(Str,  0, !IO),
    test_split_by_codepoint(Str,  1, !IO),
    test_split_by_codepoint(Str,  2, !IO),
    test_split_by_codepoint(Str,  3, !IO),
    test_split_by_codepoint(Str,  4, !IO),
    test_split_by_codepoint(Str,  5, !IO),
    test_split_by_codepoint(Str,  6, !IO),

    io.write_string("\nleft_by_codepoint:\n", !IO),
    string.left_by_codepoint(Str, 3, L3),
    io.write_string(L3, !IO),
    io.nl(!IO),

    io.write_string("\nright_by_codepoint:\n", !IO),
    string.right_by_codepoint(Str, 3, R3),
    io.write_string(R3, !IO),
    io.nl(!IO),

    io.write_string("\nbetween_codepoints:\n", !IO),
    Range = -2 .. (NumCodePoints + 1),
    foldl(test_between_codepoints(Str, Range), Range, !IO),
    io.nl(!IO).

:- pred test_codepoint_offset(string::in, int::in, io::di, io::uo) is det.

test_codepoint_offset(Str, Pos, !IO) :-
    ( if string.codepoint_offset(Str, Pos, Offset) then
        io.format("string.codepoint_offset(Str, %d, %d)\n",
            [i(Pos), i(Offset)], !IO),
        ( if string.codepoint_offset(Str, Offset, 1, Offset2) then
            io.format("string.codepoint_offset(Str, %d, 1, %d)\n",
                [i(Offset), i(Offset2)], !IO)
        else
            io.format("string.codepoint_offset(Str, %d, 1, _) failed\n",
                [i(Offset)], !IO)
        ),
        ( if string.index(Str, Offset, Char) then
            io.format("string.index(Str, %d, '%c')\n",
                [i(Offset), c(Char)], !IO)
        else
            io.format("string.index(Str, %d, _) failed\n",
                [i(Offset)], !IO)
        )
    else
        io.format("string.codepoint_offset(Str, %d, _) failed\n",
            [i(Pos)], !IO)
    ).

:- pred test_index_next(string::in, int::in, io::di, io::uo) is det.

test_index_next(Str, Index, !IO) :-
    ( if string.index_next(Str, Index, NextIndex, C) then
        io.format("index_next(Str, %d, %d, '%c')\n",
            [i(Index), i(NextIndex), c(C)], !IO),
        test_index_next(Str, NextIndex, !IO)
    else
        io.write_string("end\n", !IO)
    ).

:- pred test_unsafe_index_next(string::in, int::in, io::di, io::uo) is det.

test_unsafe_index_next(Str, Index, !IO) :-
    ( if string.unsafe_index_next(Str, Index, NextIndex, C) then
        io.format("unsafe_index_next(Str, %d, %d, '%c')\n",
            [i(Index), i(NextIndex), c(C)], !IO),
        test_unsafe_index_next(Str, NextIndex, !IO)
    else
        io.write_string("end\n", !IO)
    ).

:- pred test_unsafe_prev_index(string::in, int::in, io::di, io::uo) is det.

test_unsafe_prev_index(Str, Index, !IO) :-
    ( if string.unsafe_prev_index(Str, Index, PrevIndex, C) then
        io.format("unsafe_prev_index(Str, %d, %d, '%c')\n",
            [i(Index), i(PrevIndex), c(C)], !IO),
        test_unsafe_prev_index(Str, PrevIndex, !IO)
    else
        io.write_string("end\n", !IO)
    ).

:- pred test_split_by_codepoint(string::in, int::in, io::di, io::uo) is det.

test_split_by_codepoint(Str, Pos, !IO) :-
    string.split_by_codepoint(Str, Pos, L, R),
    io.format("split_by_codepoint(Str, %d, ""%s"", ""%s"")\n",
        [i(Pos), s(L), s(R)], !IO).

:- pred test_between_codepoints(string::in, list(int)::in, int::in,
    io::di, io::uo) is det.

test_between_codepoints(Str, EndRange, Start, !IO) :-
    foldl(test_between_codepoints_2(Str, Start), EndRange, !IO).

:- pred test_between_codepoints_2(string::in, int::in, int::in,
    io::di, io::uo) is det.

test_between_codepoints_2(Str, Start, End, !IO) :-
    string.between_codepoints(Str, Start, End, SubString),
    io.format("between_codepoints(Str, %d, %d, ""%s"")\n",
        [i(Start), i(End), s(SubString)], !IO),

    slow_between_codepoints(Str, Start, End, SlowSubString),
    ( if SubString = SlowSubString then
        true
    else
        io.write_string("but slow_between_codepoints returned: \"", !IO),
        io.write_string(SlowSubString, !IO),
        io.write_string("\"\n", !IO)
    ).

:- pred slow_between_codepoints(string::in, int::in, int::in, string::out)
    is det.

slow_between_codepoints(Str, Start, End, SubString) :-
    Chars = to_char_list(Str),
    NumCodePoints = length(Chars),
    ClampStart = clamp(0, Start, NumCodePoints),
    ClampEnd = clamp(ClampStart, End, NumCodePoints),
    ClampLen = ClampEnd - ClampStart,
    det_split_list(ClampStart, Chars, _, CharsRight),
    det_split_list(ClampLen, CharsRight, CharsMid, _),
    SubString = from_char_list(CharsMid).

:- func clamp(int, int, int) = int.

clamp(Min, X, Max) = max(Min, min(X, Max)).
