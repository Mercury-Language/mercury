%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module words_separator.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test_words_separator, [
        "",
        "|",
        "||",
        "x|",
        "|x",
        "x|y",
        "x||y",
        "|x||y|",
        "‖",
        "‖‖",
        "ẋ‖",
        "‖ẋ",
        "ẋ‖ẏ",
        "ẋ‖‖ẏ",
        "‖ẋ‖‖ẏ‖"
    ], !IO).

:- pred test_words_separator(string::in, io::di, io::uo) is det.

test_words_separator(Str, !IO) :-
    L = words_separator(is_sep, Str),
    io.write(L, !IO),
    io.nl(!IO).

:- pred is_sep(char::in) is semidet.

is_sep('|').
is_sep('‖').  % U+2016 DOUBLE VERTICAL LINE
