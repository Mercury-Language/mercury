%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% A test of string.contains_match/2.
%---------------------------------------------------------------------------%

:- module string_contains_match.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_contains_match(is_whitespace, "is_whitespace", "", !IO),
    test_contains_match(is_whitespace, "is_whitespace", "abc", !IO),
    test_contains_match(is_whitespace, "is_whitespace", " abc", !IO),
    test_contains_match(is_whitespace, "is_whitespace", "a bc", !IO),
    test_contains_match(is_whitespace, "is_whitespace", "abc ", !IO),
    test_contains_match(is_whitespace, "is_whitespace",
        "μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος", !IO),
    test_contains_match(is_chess_piece, "is_chess_piece",
        "♜    ♞   ♝   ♛   ♚   ♝   ♞   ♜", !IO),
    test_contains_match(is_chess_piece, "is_chess_piece",
        "abc♜ def", !IO),
    test_contains_match(is_chess_piece, "is_chess_piece",
        "no chess pieces here!", !IO).

:- pred test_contains_match(pred(char)::in(pred(in) is semidet),
    string::in, string::in, io::di, io::uo) is det.

test_contains_match(Pred, PredName, String, !IO) :-
    ( if string.contains_match(Pred, String) then
        Result = "true"
    else
        Result = "false"
    ),
    io.format("contains_match(%s, %s) ==> %s.\n",
        [s(PredName), s(string(String)), s(Result)], !IO).

:- pred is_chess_piece(char::in) is semidet.

is_chess_piece(Char) :-
    char.to_int(Char, CodePoint),
    CodePoint >= 0x2654, CodePoint =< 0x265F.

%---------------------------------------------------------------------------%
:- end_module string_contains_match.
%---------------------------------------------------------------------------%
