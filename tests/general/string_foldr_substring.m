%-----------------------------------------------------------------------------%
% string_foldr_substring.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Oct 28 16:32:19 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module string_foldr_substring.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string, char, list, int.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io__write_strings([
            "sub(\"Hello, World!\",  0,  5) = \"",
               sub("Hello, World!",  0,  5),
        "\"\nsub(\"Hello, World!\",  0, 50) = \"",
               sub("Hello, World!",  0, 50),
        "\"\nsub(\"Hello, World!\",  0, -5) = \"",
               sub("Hello, World!",  0, -5),
        "\"\nsub(\"Hello, World!\", -5, 12) = \"",
               sub("Hello, World!", -5, 12),
        "\"\nsub(\"Hello, World!\", -5, 50) = \"",
               sub("Hello, World!", -5, 50),
        "\"\nsub(\"Hello, World!\",  7,  0) = \"",
               sub("Hello, World!",  7,  0),
        "\"\nsub(\"Hello, World!\", 50, 10) = \"",
               sub("Hello, World!", 50, 10),
        "\"\n"
    ], !IO).

:- func sub(string, int, int) = string.

sub(S, I, N) =
    from_char_list(foldr_substring(func(X, Xs) = [X | Xs], S, I, N, [])).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
