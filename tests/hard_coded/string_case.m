%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_case.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module require.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    write_string("testing string.to_lower(in, out)\n", !IO),
    foldl(test_to_lower_in_out, strings, !IO),

    write_string("testing string.to_upper(in, out)\n", !IO),
    foldl(test_to_upper_in_out, strings, !IO),

    write_string("testing string.to_lower(in, in)\n", !IO),
    unsorted_aggregate(string_pairs, test_to_lower_in_in, !IO),

    write_string("testing string.to_upper(in, in)\n", !IO),
    unsorted_aggregate(string_pairs, test_to_upper_in_in, !IO),

    write_string("testing string.compare_ignore_case_ascii\n", !IO),
    aggregate(string_pairs, test_compare_ignore_case_ascii, !IO),

    write_string("done.\n", !IO).

:- func strings = list(string).

strings = [
    "",
    "0123456789:;<=?@",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~",
    "abcdefghijklmnopqrstuvwxyz{|}~",
    "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß",
    "àáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ",
    "Dog",
    "DOGgie",
    "狗仔\U0001F415dog",
    "狗仔\U0001F415DOG"
].

:- pred string_pairs({string, string}::out) is nondet.

string_pairs({X, Y}) :-
    member(X, strings),
    member(Y, strings).

:- pred test_to_lower_in_out(string::in, io::di, io::uo) is det.

test_to_lower_in_out(S, !IO) :-
    string.to_lower(S, LowerS),

    string.to_char_list(S, Chars),
    list.map(char.to_lower, Chars, LowerChars),
    string.from_char_list(LowerChars, Expected),

    expect(unify(LowerS, Expected), $pred, "LowerS != Expected").

:- pred test_to_upper_in_out(string::in, io::di, io::uo) is det.

test_to_upper_in_out(S, !IO) :-
    string.to_upper(S, UpperS),

    string.to_char_list(S, Chars),
    list.map(char.to_upper, Chars, UpperChars),
    string.from_char_list(UpperChars, Expected),

    expect(unify(UpperS, Expected), $pred, "UpperS != Expected").

:- pred test_to_lower_in_in({string, string}::in, io::di, io::uo) is det.

test_to_lower_in_in({X, Y}, !IO) :-
    compare(Expected, to_lower(X), Y),
    ( if string.to_lower(X, Y) then
        expect(unify(Expected, (=)), $pred, "to_lower wrong")
    else
        expect_not(unify(Expected, (=)), $pred, "to_lower wrong")
    ).

:- pred test_to_upper_in_in({string, string}::in, io::di, io::uo) is det.

test_to_upper_in_in({X, Y}, !IO) :-
    compare(Expected, to_upper(X), Y),
    ( if string.to_upper(X, Y) then
        expect(unify(Expected, (=)), $pred, "to_upper wrong")
    else
        expect_not(unify(Expected, (=)), $pred, "to_upper wrong")
    ).

:- pred test_compare_ignore_case_ascii({string, string}::in,
    io::di, io::uo) is det.

test_compare_ignore_case_ascii({X, Y}, !IO) :-
    compare_ignore_case_ascii(ResIgnCase, X, Y),
    compare(ResLower, to_lower(X), to_lower(Y) : string),
    compare(ResUpper, to_upper(X), to_upper(Y) : string),
    expect(unify(ResIgnCase, ResLower), $pred, "ResIgnCase != ResLower"),
    expect(unify(ResIgnCase, ResUpper), $pred, "ResIgnCase != ResUpper").
