%-----------------------------------------------------------------------------%
% test_parsing_utils.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Jan 27 13:44:59 EST 2009
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module test_parsing_utils.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    unsorted_aggregate(run_test, io.write_string, !IO),
    test_pos("123456789\n123456789\n", 14, !IO),
    test_pos("\n123456789\n123456789\n\n\n\n\n\n", 3, !IO),
    test_pos("\n1234\n12\n\n\nfewefwef\nwwfwe\n\n", 20, !IO),
    test_pos("123456789\n123456789\n\n1234567890", 22, !IO),
    test_pos("123456789\n123456789\n\n1234567890", 20, !IO),
    test_pos("123456789", 2, !IO),
    test_pos("123456789", 0, !IO),
    test_pos("123456789\n123456789\n\n", 19, !IO),
    test_pos("123456789\n123456789\n\n", 20, !IO),
    test_pos("", 0, !IO).

%-----------------------------------------------------------------------------%

:- pred run_test(string::out) is multi.

run_test(Result) :-
    test_case(ParserName, Parser, TestString, ExpectedOutput),
    new_src_and_ps(TestString, Src, PS0),
    ( if Parser(Src, ActualOutput0, PS0, PS) then
        ActualOutput = yes(ActualOutput0),
        current_offset(Src, CurrentOffset0, PS, _),
        CurrentOffset = yes(CurrentOffset0)
      else
        ActualOutput = no,
        CurrentOffset = no
    ),
    (
        ExpectedOutput = no,
        ActualOutput = no,
        Outcome = "failed as expected",
        PassFail = "pass"
    ;
        ExpectedOutput = no,
        ActualOutput = yes(ActualOutputString),
        Outcome = "should have failed, but returned \"" ++
            ActualOutputString ++ "\"",
        PassFail = "fail"
    ;
        ExpectedOutput = yes(ExpectedOutputString),
        ActualOutput = no,
        Outcome = "failed, but should have returned \"" ++
            ExpectedOutputString ++ "\"",
        PassFail = "fail"
    ;
        ExpectedOutput = yes(ExpectedOutputString),
        ActualOutput = yes(ActualOutputString),
        Outcome0 = "returned " ++ ActualOutputString,
        ( if ActualOutputString = ExpectedOutputString then
            Outcome = Outcome0 ++ " as expected",
            PassFail = "pass"
          else
            Outcome = Outcome0 ++ ", but should have returned " ++
                ExpectedOutputString,
            PassFail = "fail"
        )
    ),
    Result = PassFail ++ ": " ++
        ParserName ++ " on \"" ++ TestString ++ "\"\n\t" ++
        Outcome ++
        ( if CurrentOffset = yes(CO) then
            string.format("\n\t[%d chars consumed]", [i(CO)])
          else
            ""
        ) ++
        "\n".

%-----------------------------------------------------------------------------%

:- pred test_case(
        string::out,
        pred(src, string, ps, ps)::out(pred(in, out, in, out) is semidet),
        string::out,
        maybe(string)::out)
        is multi.

test_case("next_char", stringify(next_char),
    "", no).
test_case("next_char", stringify(next_char),
    "123", yes("'1'")).

test_case("char_in_class(\"123\")", stringify(char_in_class("123")),
    "", no).
test_case("char_in_class(\"123\")", stringify(char_in_class("123")),
    "abc", no).
test_case("char_in_class(\"123\")", stringify(char_in_class("123")),
    "123", yes("'1'")).

test_case("punct(\"!\")", stringify(punct("!")),
    "", no).
test_case("punct(\"!\")", stringify(punct("!")),
    "abc", no).
test_case("punct(\"!\")", stringify(punct("!")),
    "*", no).
test_case("punct(\"!\")", stringify(punct("!")),
    "!", yes("unit")).

test_case("keyword(\"ABC\", \"ABC\")", stringify(keyword("ABC", "ABC")),
    "", no).
test_case("keyword(\"ABC\", \"ABC\")", stringify(keyword("ABC", "ABC")),
    "123", no).
test_case("keyword(\"ABC\", \"ABC\")", stringify(keyword("ABC", "ABC")),
    "ABCA", no).
test_case("keyword(\"ABC\", \"ABC\")", stringify(keyword("ABC", "ABC")),
    "ABC 123", yes("unit")).

test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "", no).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "abc", no).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "_", no).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "A", yes("\"A\"")).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "Ab_c", yes("\"Ab_c\"")).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "*", no).
test_case("identifier(\"ABC\", \"ABCabc_\")", stringify(identifier("ABC", "ABCabc_")),
    "Abc !", yes("\"Abc\"")).

test_case("whitespace", stringify(whitespace),
    "", yes("unit")).
test_case("whitespace", stringify(whitespace),
    "123", yes("unit")).
test_case("whitespace", stringify(whitespace),
    "   ", yes("unit")).
test_case("whitespace", stringify(whitespace),
    "   123", yes("unit")).

test_case("skip_to_eol", stringify(skip_to_eol),
    "", no).
test_case("skip_to_eol", stringify(skip_to_eol),
    "blah blah\n", yes("unit")).
test_case("skip_to_eol", stringify(skip_to_eol),
    "blah blah\n123", yes("unit")).

test_case("eof", stringify(eof),
    "123", no).
test_case("eof", stringify(eof),
    "", yes("unit")).

test_case("float_literal_as_string", stringify(float_literal_as_string),
    "", no).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "abc", no).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "123", no).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "123.0   abc", yes("\"123.0\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "123.0e1   abc", yes("\"123.0e1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0   abc", yes("\"-123.0\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0e1   abc", yes("\"-123.0e1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0E-1   abc", yes("\"-123.0E-1\"")).

test_case("float_literal", stringify(float_literal),
    "", no).
test_case("float_literal", stringify(float_literal),
    "abc", no).
test_case("float_literal", stringify(float_literal),
    "123", no).
test_case("float_literal", stringify(float_literal),
    "123.0   abc", yes("123.0")).
test_case("float_literal", stringify(float_literal),
    "123.0e1   abc", yes("1230.0")).
test_case("float_literal", stringify(float_literal),
    "-123.0   abc", yes("-123.0")).
test_case("float_literal", stringify(float_literal),
    "-123.0e1   abc", yes("-1230.0")).
test_case("float_literal", stringify(float_literal),
    "-123.0E-1   abc", yes("-12.3")).

test_case("int_literal_as_string", stringify(int_literal_as_string),
    "", no).
test_case("int_literal_as_string", stringify(int_literal_as_string),
    "abc", no).
test_case("int_literal_as_string", stringify(int_literal_as_string),
    "123.0", no).
test_case("int_literal_as_string", stringify(int_literal_as_string),
    "123   abc", yes("\"123\"")).
test_case("int_literal_as_string", stringify(int_literal_as_string),
    "-123   abc", yes("\"-123\"")).
test_case("int_literal_as_string", stringify(int_literal_as_string),
    "999999999999999999999   abc", yes("\"999999999999999999999\"")).

test_case("int_literal", stringify(int_literal),
    "", no).
test_case("int_literal", stringify(int_literal),
    "abc", no).
test_case("int_literal", stringify(int_literal),
    "123.0", no).
test_case("int_literal", stringify(int_literal),
    "123   abc", yes("123")).
test_case("int_literal", stringify(int_literal),
    "-123   abc", yes("-123")).
test_case("int_literal", stringify(int_literal),
    "999999999999999999999   abc", no).

test_case("string_literal('\\\"')", stringify(string_literal('\"')),
    "", no).
test_case("string_literal('\\\"')", stringify(string_literal('\"')),
    "\"123\"   abc", yes("\"123\"")).
test_case("string_literal('\\\"')", stringify(string_literal('\"')),
    "\"1\\\"2\\\"3\"   abc", yes("\"1\\\\\\\"2\\\\\\\"3\"")).
test_case("string_literal('\\\'')", stringify(string_literal('\'')),
    "", no).
test_case("string_literal('\\\'')", stringify(string_literal('\'')),
    "\'123\'   abc", yes("\"123\"")).
test_case("string_literal('\\\'')", stringify(string_literal('\'')),
    "\'1\\\'2\\\'3\'   abc", yes("\"1\\\\\\\'2\\\\\\\'3\"")).

test_case("optional(punct(\"!\"))", stringify(optional(punct("!"))),
    "", yes("no")).
test_case("optional(punct(\"!\"))", stringify(optional(punct("!"))),
    "abc", yes("no")).
test_case("optional(punct(\"!\"))", stringify(optional(punct("!"))),
    "!   ", yes("yes(unit)")).

test_case("zero_or_more(punct(\"!\"))", stringify(zero_or_more(punct("!"))),
    "", yes("[]")).
test_case("zero_or_more(punct(\"!\"))", stringify(zero_or_more(punct("!"))),
    "abc", yes("[]")).
test_case("zero_or_more(punct(\"!\"))", stringify(zero_or_more(punct("!"))),
    "!!!   abc", yes("[unit, unit, unit]")).

test_case("one_or_more(punct(\"!\"))", stringify(one_or_more(punct("!"))),
    "", no).
test_case("one_or_more(punct(\"!\"))", stringify(one_or_more(punct("!"))),
    "abc", no).
test_case("one_or_more(punct(\"!\"))", stringify(one_or_more(punct("!"))),
    "!!!   abc", yes("[unit, unit, unit]")).

test_case("brackets(\"(\", \")\", punct(\"!\"))",
    stringify(brackets("(", ")", punct("!"))),
    "", no).
test_case("brackets(\"(\", \")\", punct(\"!\"))",
    stringify(brackets("(", ")", punct("!"))),
    "abc", no).
test_case("brackets(\"(\", \")\", punct(\"!\"))",
    stringify(brackets("(", ")", punct("!"))),
    "(abc)", no).
test_case("brackets(\"(\", \")\", punct(\"!\"))",
    stringify(brackets("(", ")", punct("!"))),
    "(!)   abc", yes("unit")).

test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "", no).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "abc", no).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "!   abc", yes("[unit]")).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "!+ ! + !   abc", yes("[unit, unit, unit]")).

test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "", no).
test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "abc", no).
test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "!   abc", yes("[unit]")).
test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "!, ! , !   abc", yes("[unit, unit, unit]")).

test_case("optional(int_with_state)",
    stringify_state(optional(int_with_state)),
    "abc", yes("[]")).

test_case("optional(int_with_state)",
    stringify_state(optional(int_with_state)),
    "1", yes("[1]")).

test_case("zero_or_more(int_with_state)",
    stringify_state(zero_or_more(int_with_state)),
    "abc", yes("[]")).

test_case("zero_or_more(int_with_state)",
    stringify_state(zero_or_more(int_with_state)),
    "1 2 3", yes("[3, 2, 1]")).

test_case("one_or_more(int_with_state)",
    stringify_state(one_or_more(int_with_state)),
    "abc", no).

test_case("one_or_more(int_with_state)",
    stringify_state(one_or_more(int_with_state)),
    "1 2 3", yes("[3, 2, 1]")).

%-----------------------------------------------------------------------------%

:- pred int_with_state(src::in, int::out, list(int)::in, list(int)::out,
        ps::in, ps::out) is semidet.

int_with_state(Src, X, Xs, [X | Xs], !PS) :-
    int_literal(Src, X, !PS).

%-----------------------------------------------------------------------------%

:- pred stringify(
        pred(src, T, ps, ps)::in(pred(in, out, in, out) is semidet),
        src::in,
        string::out,
        ps::in,
        ps::out)
        is semidet.

stringify(P, Src, String, !PS) :-
    P(Src, X, !PS),
    String = string.string(X).

%-----------------------------------------------------------------------------%

:- pred test_pos(string::in, int::in, io::di, io::uo) is det.

test_pos(Str, OS, !IO) :-
    new_src_and_ps(Str, Src, _),
    offset_to_line_number_and_position(src_to_line_numbers(Src), OS, Line,
        Pos),
    io.format("Line = %d, Pos = %d\n", [i(Line), i(Pos)], !IO).

%-----------------------------------------------------------------------------%

:- pred stringify_state(
        pred(src, T, list(S), list(S), ps, ps)::
            in(pred(in, out, in, out, in, out) is semidet),
        src::in,
        string::out,
        ps::in,
        ps::out)
        is semidet.

stringify_state(P, Src, String, !PS) :-
    P(Src, _, [], State, !PS),
    String = string.string(State).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
