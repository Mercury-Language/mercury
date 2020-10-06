%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test string prefix- and suffix-related predicates.
%

:- module string_presuffix.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Str = "aßξ啕𐀀.",
    io.write_string("prefix(in, in):\n", !IO),
    ( if
        string.prefix(Str, ""),
        string.prefix(Str, "a"),
        string.prefix(Str, "aß"),
        string.prefix(Str, "aßξ"),
        string.prefix(Str, "aßξ啕"),
        string.prefix(Str, "aßξ啕𐀀"),
        string.prefix(Str, "aßξ啕𐀀."),
        not string.prefix(Str, "aßξ啕𐀀.z")
    then
        io.write_string("pass\n", !IO)
    else
        io.write_string("fail\n", !IO)
    ),

    io.write_string("\nprefix(in, out):\n", !IO),
    solutions(pred(Pre::out) is multi :- string.prefix(Str, Pre), Prefixes),
    io.write(Prefixes, !IO),
    io.nl(!IO),

    io.write_string("\nsuffix(in, in):\n", !IO),
    ( if
        not string.suffix(Str, "aßξ啕𐀀.z"),
        string.suffix(Str, "aßξ啕𐀀."),
        string.suffix(Str, "ßξ啕𐀀."),
        string.suffix(Str, "ξ啕𐀀."),
        string.suffix(Str, "啕𐀀."),
        string.suffix(Str, "𐀀."),
        string.suffix(Str, "."),
        string.suffix(Str, "")
    then
        io.write_string("pass\n", !IO)
    else
        io.write_string("fail\n", !IO)
    ),

    io.write_string("\nsuffix(in, out):\n", !IO),
    solutions(pred(Suf::out) is multi :- string.suffix(Str, Suf), Suffixes),
    io.write(Suffixes, !IO),
    io.nl(!IO),

    io.write_string("\nremove_prefix:\n", !IO),
    ( if
        string.remove_prefix(Str, Str, ""),
        string.remove_prefix("aßξ", Str, "啕𐀀."),
        not string.remove_prefix("☿", Str, Str)
    then
        io.write_string("pass\n", !IO)
    else
        io.write_string("fail\n", !IO)
    ),

    io.write_string("\nremove_suffix:\n", !IO),
    ( if
        string.remove_suffix(Str, Str, ""),
        string.remove_suffix(Str, "啕𐀀.", "aßξ"),
        not string.remove_suffix(Str, "☿", Str)
    then
        io.write_string("pass\n", !IO)
    else
        io.write_string("fail\n", !IO)
    ).
