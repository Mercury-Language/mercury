%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_replace.

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
    Str = "aaa bbbb ccccc aaa",
    Str2 = "aßξ啕ßξ啕𐀀.",
    Smiley = "😀",
    Str3 = between(Smiley, 1, length(Smiley)) ++ Smiley,
    Tests = [
        % pattern not in string
        {"", "a", "bc"},
        {Str, "aab", "**"},
        {Str, "aaaa", "**"},

        % pattern is empty string
        {Str, "", "**"},
        {Str2, "", "**"},
        {Str3, "", "**"},

        % pattern in string
        {Str, "aaa", ""},
        {Str, "cc", "**"},
        {Str2, "ßξ", "**"},
        {Str2, "ßξ", "★★"},
        {Str2, "啕ßξ", "***"}
    ],
    list.foldl(test_replace, Tests, !IO),
    list.foldl(test_replace_all, Tests, !IO).

:- pred test_replace({string, string, string}::in, io::di, io::uo) is det.

test_replace({Str, Pat, Subst}, !IO) :-
    io.write_string("string__replace(\"", !IO),
    write_string_debug(Str, !IO),
    io.write_string("\", \"", !IO),
    io.write_string(Pat, !IO),
    io.write_string("\", \"", !IO),
    io.write_string(Subst, !IO),
    io.write_string("\", Result) \n\t", !IO),
    ( if string.replace(Str, Pat, Subst, Result) then
        io.write_string("\"", !IO),
        write_string_debug(Result, !IO),
        io.write_string("\"\n", !IO)
    else
        io.write_string("FAIL!\n", !IO)
    ).

:- pred test_replace_all({string, string, string}::in, io::di, io::uo) is det.

test_replace_all({Str, Pat, Subst}, !IO) :-
    io.write_string("string__replace_all(\"", !IO),
    write_string_debug(Str, !IO),
    io.write_string("\", \"", !IO),
    io.write_string(Pat, !IO),
    io.write_string("\", \"", !IO),
    io.write_string(Subst, !IO),
    io.write_string("\", Result) \n\t", !IO),
    io.write_string("\"", !IO),
    string.replace_all(Str, Pat, Subst, Result),
    write_string_debug(Result, !IO),
    io.write_string("\"\n", !IO).

:- pred write_string_debug(string::in, io::di, io::uo) is det.

write_string_debug(S, !IO) :-
    write_string_debug_loop(S, 0, !IO).

:- pred write_string_debug_loop(string::in, int::in, io::di, io::uo) is det.

write_string_debug_loop(S, Index, !IO) :-
    ( if string.index_next(S, Index, NextIndex, Char) then
        ( if char.is_surrogate(Char) ; Char = '\ufffd' then
            unsafe_index_code_unit(S, Index, Code),
            io.format("[%#x]", [i(Code)], !IO)
        else
            io.write_char(Char, !IO)
        ),
        write_string_debug_loop(S, NextIndex, !IO)
    else
        true
    ).
