%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% A test of string.all_match/2.
%
% The .exp file is for C and C# grades.
% The .exp2 file is for Java grades.
%
%---------------------------------------------------------------------------%

:- module string_all_match.
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
    test_all_match(is_alpha, "is_alpha", "", !IO),
    test_all_match(is_alpha, "is_alpha", "abc", !IO),
    test_all_match(is_alpha, "is_alpha", "abc123", !IO),
    test_all_match(is_chess_piece, "is_chess_piece", "♜♞♝♛♚♝♞♜", !IO),
    test_all_match(is_chess_piece, "is_chess_piece", "♜♞♝♛K♝♞♜", !IO),

    Ilseq = string.between("😀", 0, 1),
    S = "abc" ++ Ilseq ++ "def",
    test_all_match(is_alpha, "is_alpha", S, !IO).

:- pred test_all_match(pred(char)::in(pred(in) is semidet),
    string::in, string::in, io::di, io::uo) is det.

test_all_match(Pred, PredName, String, !IO) :-
    ( if string.all_match(Pred, String) then
        Result = "true"
    else
        Result = "false"
    ),
    io.format("all_match(%s, %s) ==> %s.\n",
        [s(PredName), s(string(String)), s(Result)], !IO).

:- pred is_chess_piece(char::in) is semidet.

is_chess_piece(Char) :-
    char.to_int(Char, CodePoint),
    CodePoint >= 0x2654, CodePoint =< 0x265F.

%---------------------------------------------------------------------------%
:- end_module string_all_match.
%---------------------------------------------------------------------------%
