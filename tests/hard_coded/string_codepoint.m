%-----------------------------------------------------------------------------%

:- module string_codepoint.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

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

    ( string.from_code_unit_list(CodeUnitList, Str2) ->
        io.write_string("\nfrom_code_unit_list:\n", !IO),
        io.write_string(Str2, !IO),
        io.nl(!IO)
    ;
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
    string.between_codepoints(Str, 2, 4, Sub),
    io.write_string(Sub, !IO),
    io.nl(!IO).

:- pred test_codepoint_offset(string::in, int::in, io::di, io::uo) is det.

test_codepoint_offset(Str, Pos, !IO) :-
    ( string.codepoint_offset(Str, Pos, Offset) ->
        io.format("string.codepoint_offset(Str, %d, %d)\n",
            [i(Pos), i(Offset)], !IO),
        ( string.codepoint_offset(Str, Offset, 1, Offset2) ->
            io.format("string.codepoint_offset(Str, %d, 1, %d)\n",
                [i(Offset), i(Offset2)], !IO)
        ;
            io.format("string.codepoint_offset(Str, %d, 1, _) failed\n",
                [i(Offset)], !IO)
        ),
        ( string.index(Str, Offset, Char) ->
            io.format("string.index(Str, %d, '%c')\n",
                [i(Offset), c(Char)], !IO)
        ;
            io.format("string.index(Str, %d, _) failed\n",
                [i(Offset)], !IO)
        )
    ;
        io.format("string.codepoint_offset(Str, %d, _) failed\n",
            [i(Pos)], !IO)
    ).

:- pred test_index_next(string::in, int::in, io::di, io::uo) is det.

test_index_next(Str, Index, !IO) :-
    ( string.index_next(Str, Index, NextIndex, C) ->
        io.format("index_next(Str, %d, %d, '%c')\n",
            [i(Index), i(NextIndex), c(C)], !IO),
        test_index_next(Str, NextIndex, !IO)
    ;
        io.write_string("end\n", !IO)
    ).

:- pred test_unsafe_index_next(string::in, int::in, io::di, io::uo) is det.

test_unsafe_index_next(Str, Index, !IO) :-
    ( string.unsafe_index_next(Str, Index, NextIndex, C) ->
        io.format("unsafe_index_next(Str, %d, %d, '%c')\n",
            [i(Index), i(NextIndex), c(C)], !IO),
        test_unsafe_index_next(Str, NextIndex, !IO)
    ;
        io.write_string("end\n", !IO)
    ).

:- pred test_unsafe_prev_index(string::in, int::in, io::di, io::uo) is det.

test_unsafe_prev_index(Str, Index, !IO) :-
    ( string.unsafe_prev_index(Str, Index, PrevIndex, C) ->
        io.format("unsafe_prev_index(Str, %d, %d, '%c')\n",
            [i(Index), i(PrevIndex), c(C)], !IO),
        test_unsafe_prev_index(Str, PrevIndex, !IO)
    ;
        io.write_string("end\n", !IO)
    ).

:- pred test_split_by_codepoint(string::in, int::in, io::di, io::uo) is det.

test_split_by_codepoint(Str, Pos, !IO) :-
    string.split_by_codepoint(Str, Pos, L, R),
    io.format("split_by_codepoint(Str, %d, ""%s"", ""%s"")\n",
        [i(Pos), s(L), s(R)], !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
