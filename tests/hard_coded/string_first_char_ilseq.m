%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_first_char_ilseq.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",                              % UTF-16: 0xD83D 0xDE00
    S1 = string.between(S0, 1, length(S0)), % UTF-16: 0xDE00
    Rest = "rest",
    S = S1 ++ Rest,

    test_first_char_ioi(S, Rest, !IO),
    test_first_char_ioo(S, !IO),

    % An implementation might return U+FFFD as the first char of a string
    % beginning with an ill-formed code unit sequence, which is wrong by our
    % definition.
    Replacement = char.det_from_int(0xFFFD),
    test_first_char_iii(S, Replacement, Rest, !IO),
    test_first_char_iio(S, Replacement, !IO),

    % An implementation might separate out a surrogate code point as the
    % first code point, which is wrong by our definition.
    HiSurr = char.det_from_int(0xD83D),
    LoSurr = char.det_from_int(0xDE00),
    test_first_char_iii(S, HiSurr, Rest, !IO),
    test_first_char_iio(S, HiSurr, !IO),
    test_first_char_iii(S, LoSurr, Rest, !IO),
    test_first_char_iio(S, LoSurr, !IO),

    % Prepending a surrogate code point is disallowed.
    test_first_char_oii(HiSurr, S, !IO),
    test_first_char_oii(LoSurr, S, !IO).

:- pred test_first_char_iii(string::in, char::in, string::in, io::di, io::uo)
    is det.

test_first_char_iii(Str, FirstChar, Rest, !IO) :-
    ( if string.first_char(Str, FirstChar, Rest) then
        io.write_string("first_char(in, in, in) succeeded\n", !IO)
    else
        io.write_string("first_char(in, in, in) failed\n", !IO)
    ).

:- pred test_first_char_ioi(string::in, string::in, io::di, io::uo) is det.

test_first_char_ioi(Str, Rest, !IO) :-
    ( if string.first_char(Str, _FirstChar, Rest) then
        io.write_string("first_char(in, out, in) succeeded\n", !IO)
    else
        io.write_string("first_char(in, out, in) failed\n", !IO)
    ).

:- pred test_first_char_iio(string::in, char::in, io::di, io::uo) is det.

test_first_char_iio(Str, FirstChar, !IO) :-
    ( if string.first_char(Str, FirstChar, _Rest) then
        io.write_string("first_char(in, in, out) succeeded\n", !IO)
    else
        io.write_string("first_char(in, in, out) failed\n", !IO)
    ).

:- pred test_first_char_ioo(string::in, io::di, io::uo) is det.

test_first_char_ioo(Str, !IO) :-
    ( if string.first_char(Str, _FirstChar, _Rest) then
        io.write_string("first_char(in, out, out) succeeded\n", !IO)
    else
        io.write_string("first_char(in, out, out) failed\n", !IO)
    ).

:- pred test_first_char_oii(char::in, string::in, io::di, io::uo) is cc_multi.

test_first_char_oii(FirstChar, Rest, !IO) :-
    ( try []
        string.first_char(_Str, FirstChar, Rest)
    then
        io.write_string("first_char(out, in, in) succeeded\n", !IO)
    catch_any Excp ->
        io.write_string("first_char(out, in, in) threw exception: ", !IO),
        io.print_line(Excp, !IO)
    ).
