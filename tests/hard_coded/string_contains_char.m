%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% A test of string.contains_char/2.
%
% The .exp file is for C and C# grades.
% The .exp2 file is for Java grades.
%
%---------------------------------------------------------------------------%

:- module string_contains_char.
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
    test_contains_char("", 'X', !IO),
    test_contains_char("abc", 'X', !IO),
    test_contains_char("abc", 'a', !IO),
    test_contains_char("abc", 'b', !IO),
    test_contains_char("abc", 'c', !IO),

    test_contains_char("aßξ啕𐀀.", 'ß', !IO),
    test_contains_char("aßξ啕𐀀.", 'ξ', !IO),
    test_contains_char("aßξ啕𐀀.", '啕', !IO),
    test_contains_char("aßξ啕𐀀.", '.', !IO),
    test_contains_char("aßξ啕𐀀.", '☿', !IO),

    Ilseq = string.between("😀", 0, 1),
    S = "abc" ++ Ilseq ++ "123",
    test_contains_char(S, 'c', !IO),
    test_contains_char(S, '1', !IO),
    test_contains_char(S, '2', !IO),
    test_contains_char(S, '3', !IO),
    test_contains_char(S, '\uFFFD', !IO).

:- pred test_contains_char(string::in, char::in, io::di, io::uo) is det.

test_contains_char(String, Char, !IO) :-
    ( if string.contains_char(String, Char) then
        Result = "true"
    else
        Result = "false"
    ),
    io.format("contains_char(%s, %s) ==> %s.\n",
        [s(string(String)), s(string(Char)), s(Result)], !IO).

%---------------------------------------------------------------------------%
:- end_module string_contains_char.
%---------------------------------------------------------------------------%
