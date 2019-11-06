%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_replace.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Str = "aaa bbbb ccccc aaa",
    Str2 = "aßξ啕ßξ啕𐀀.",
    Tests = [
        % pattern not in string
        {"", "a", "bc"},
        {Str, "aab", "**"},
        {Str, "aaaa", "**"},

        % pattern is empty string
        {Str, "", "**"},
        {Str2, "", "**"},

        % pattern in string
        {Str, "aaa", ""},
        {Str, "cc", "**"},
        {Str2, "ßξ", "**"},
        {Str2, "ßξ", "★★"},
        {Str2, "啕ßξ", "***"}
    ],
    list__foldl(test_replace, Tests, !IO),
    list__foldl(test_replace_all, Tests, !IO).

:- pred test_replace({string, string, string}::in, io::di, io::uo) is det.

test_replace({Str, Pat, Subst}, !IO) :-
    io__write_string("string__replace(\"" ++ Str ++
            "\", \"" ++ Pat ++
            "\", \"" ++ Subst ++ "\", Result) \n\t", !IO),
    ( string__replace(Str, Pat, Subst, Result) ->
        io__write(Result, !IO),
        io__nl(!IO)
    ;
        io__write_string("FAIL!\n", !IO)
    ).

:- pred test_replace_all({string, string, string}::in, io::di, io::uo) is det.

test_replace_all({Str, Pat, Subst}, !IO) :-
    io__write_string("string__replace_all(\"" ++ Str ++
            "\", \"" ++ Pat ++
            "\", \"" ++ Subst ++ "\", Result) \n\t", !IO),
    string__replace_all(Str, Pat, Subst, Result),
    io__write(Result, !IO),
    io__nl(!IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
