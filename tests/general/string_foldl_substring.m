%-----------------------------------------------------------------------------%
% string_foldl_substring.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Oct 28 16:32:19 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module string_foldl_substring.

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
            "rev(\"Hello, World!\",  0,  5) = \"",
               rev("Hello, World!",  0,  5),
        "\"\nrev(\"Hello, World!\",  0, 50) = \"",
               rev("Hello, World!",  0, 50),
        "\"\nrev(\"Hello, World!\",  0, -5) = \"",
               rev("Hello, World!",  0, -5),
        "\"\nrev(\"Hello, World!\", -5, 12) = \"",
               rev("Hello, World!", -5, 12),
        "\"\nrev(\"Hello, World!\", -5, 50) = \"",
               rev("Hello, World!", -5, 50),
        "\"\nrev(\"Hello, World!\",  7,  0) = \"",
               rev("Hello, World!",  7,  0),
        "\"\nrev(\"Hello, World!\", 50, 10) = \"",
               rev("Hello, World!", 50, 10),
        "\"\n"
    ], !IO).

:- func rev(string, int, int) = string.

rev(S, I, N) =
    from_char_list(foldl_substring(func(X, Xs) = [X | Xs], S, I, N, [])).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
