%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% test_parsing_utils.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Jan 27 13:44:59 EST 2009
%
% The .exp file is for backends that use UTF-8 encoded strings.
% The .exp2 file is for backends that use UTF-16 encoded strings.
%
%---------------------------------------------------------------------------%

:- module test_parsing_utils.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module solutions.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

main(!IO) :-
    unsorted_aggregate(run_test, io.write_string, !IO),
    io.write_string("--\n", !IO),
    test_pos("123456789\n123456789\n", 14, !IO),
    test_pos("\n123456789\n123456789\n\n\n\n\n\n", 3, !IO),
    test_pos("\n1234\n12\n\n\nfewefwef\nwwfwe\n\n", 20, !IO),
    test_pos("123456789\n123456789\n\n1234567890", 22, !IO),
    test_pos("123456789\n123456789\n\n1234567890", 20, !IO),
    test_pos("123456789", 2, !IO),
    test_pos("123456789", 0, !IO),
    test_pos("123456789\n123456789\n\n", 19, !IO),
    test_pos("123456789\n123456789\n\n", 20, !IO),
    test_pos("", 0, !IO),
    io.write_string("--\n", !IO),
    test_pos("ábc\n☿\n\n", 0, !IO),
    test_pos("ábc\n☿\n\n", 1, !IO),
    test_pos("ábc\n☿\n\n", 2, !IO),
    test_pos("ábc\n☿\n\n", 3, !IO),
    test_pos("ábc\n☿\n\n", 4, !IO),
    test_pos("ábc\n☿\n\n", 5, !IO),
    test_pos("ábc\n☿\n\n", 6, !IO),
    test_pos("ábc\n☿\n\n", 7, !IO),
    test_pos("ábc\n☿\n\n", 8, !IO),
    test_pos("ábc\n☿\n\n", 9, !IO),
    io.write_string("--\n", !IO),
    test_err("12 + x-pow(x + 3; y)", expr_top, !IO),
    test_err("abs(x ++ 3)", expr_top, !IO),
    test_err("abs (x))", expr_top, !IO),
    test_err("1 + 3 MoD 2 + f(3 + x)", expr_top, !IO),
    test_err("1 + /* comment */ 3 mody 2 + f(3 + x)", expr_top, !IO),
    test_err("1 + 1x", expr_top, !IO),
    test_err("1 + 2 /* blah blah ...", expr_top, !IO),
    true.

%---------------------------------------------------------------------------%

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
    (
        CurrentOffset = yes(CO),
        input_substring(Src, 0, CO, Substring)
    ->
        NumCodePoints = string.count_codepoints(Substring),
        NumCodeUnits = string.count_code_units(Substring),
        ( NumCodeUnits = NumCodePoints ->
            What = "chars"
        ;
            What = "code points"
        ),
        Consumed = string.format("\n\t[%d %s consumed]",
            [i(NumCodePoints), s(What)])
    ;
        Consumed = ""
    ),
    Result = PassFail ++ ": " ++
        ParserName ++ " on \"" ++ TestString ++ "\"\n\t" ++
        Outcome ++ Consumed ++ "\n".

%---------------------------------------------------------------------------%

:- pred test_case(string::out,
    pred(src, string, ps, ps)::out(pred(in, out, in, out) is semidet),
    string::out, maybe(string)::out) is multi.

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

test_case("keyword(\"αβγ\", \"αβγ\")", stringify(keyword("αβγ", "αβγ")),
    "", no).
test_case("keyword(\"αβγ\", \"αβγ\")", stringify(keyword("αβγ", "αβγ")),
    "123", no).
test_case("keyword(\"αβγ\", \"αβγ\")", stringify(keyword("αβγ", "αβγ")),
    "αβγα", no).
test_case("keyword(\"αβγ\", \"αβγ\")", stringify(keyword("αβγ", "αβγ")),
    "αβγ 123", yes("unit")).

test_case("keyword(\"ABC\", \"ABC\")", stringify(ikeyword("ABC", "ABC")),
    "abc 123", yes("unit")).
test_case("ikeyword(\"αβγ\", \"αβγ\")", stringify(ikeyword("αβγ", "αβγ")),
    "αβγ 123", yes("unit")).

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

test_case("identifier(\"αβγ\", \"αβγ_\")", stringify(identifier("αβγ", "αβγ_")),
    "", no).
test_case("identifier(\"αβγ\", \"αβγ_\")", stringify(identifier("αβγ", "αβγ_")),
    "abc", no).
test_case("identifier(\"αβγ\", \"αβγ_\")", stringify(identifier("αβγ", "αβγ_")),
    "_", no).
test_case("identifier(\"αβγ\", \"αβγ_\")", stringify(identifier("αβγ", "αβγ_")),
    "α", yes("\"α\"")).
test_case("identifier(\"αβγ\", \"αβγ_\")", stringify(identifier("αβγ", "αβγ_")),
    "αβ_γ", yes("\"αβ_γ\"")).

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
    "-123.0e-1   abc", yes("\"-123.0e-1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0e+1   abc", yes("\"-123.0e+1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0E1   abc", yes("\"-123.0E1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0E-1   abc", yes("\"-123.0E-1\"")).
test_case("float_literal_as_string", stringify(float_literal_as_string),
    "-123.0E+1   abc", yes("\"-123.0E+1\"")).

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
    "-123.0e+1   abc", yes("-1230.0")).
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

test_case("string_literal('‖')", stringify(string_literal('‖')),
    "", no).
test_case("string_literal('‖')", stringify(string_literal('‖')),
    "‖123‖   abc", yes("\"123\"")).
test_case("string_literal('‖')", stringify(string_literal('‖')),
    "‖αβγ‖   abc", yes("\"αβγ\"")).

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
    "", yes("[]")).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "abc", yes("[]")).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "!   abc", yes("[unit]")).
test_case("separated_list(\"+\", punct(\"!\"))",
    stringify(separated_list("+", punct("!"))),
    "!+ ! + !   abc", yes("[unit, unit, unit]")).

test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "", yes("[]")).
test_case("comma_separated_list(punct(\"!\"))",
    stringify(comma_separated_list(punct("!"))),
    "abc", yes("[]")).
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

%---------------------------------------------------------------------------%

:- pred int_with_state(src::in, int::out, list(int)::in, list(int)::out,
    ps::in, ps::out) is semidet.

int_with_state(Src, X, Xs, [X | Xs], !PS) :-
    int_literal(Src, X, !PS).

%---------------------------------------------------------------------------%

:- pred stringify(pred(src, T, ps, ps)::in(pred(in, out, in, out) is semidet),
    src::in, string::out, ps::in, ps::out) is semidet.

stringify(P, Src, String, !PS) :-
    P(Src, X, !PS),
    String = string.string(X).

%---------------------------------------------------------------------------%

:- pred test_pos(string::in, int::in, io::di, io::uo) is det.

test_pos(Str, OS, !IO) :-
    new_src_and_ps(Str, Src, _),
    offset_to_line_number_and_position(src_to_line_numbers(Src), OS, Line,
        Pos),
    io.format("Line = %d, Pos = %d\n", [i(Line), i(Pos)], !IO).

%---------------------------------------------------------------------------%

:- pred stringify_state(
    pred(src, T, list(S), list(S), ps, ps)::
        in(pred(in, out, in, out, in, out) is semidet),
    src::in, string::out, ps::in, ps::out) is semidet.

stringify_state(P, Src, String, !PS) :-
    P(Src, _, [], State, !PS),
    String = string.string(State).

%---------------------------------------------------------------------------%

:- pred test_err(string::in, parser(expr)::in(parser), io::di, io::uo)
    is cc_multi.

test_err(Input, Parser, !IO) :-
    parse(Input, skip_ws, Parser, Result),
    (
        Result = ok(Expr),
        io.write(Expr, !IO),
        io.nl(!IO)
    ;
        Result = error(MaybeMsg, LineNo, Col),
        Lines = string.words_separator(unify('\n'), Input),
        Line= list.det_index1(Lines, LineNo),
        Spaces = string.from_char_list(list.duplicate(Col - 1, ' ')),
        (
            MaybeMsg = yes(Msg),
            io.write_string(Msg ++ "\n", !IO)
        ;
            MaybeMsg = no,
            io.write_string("syntax error\n", !IO)
        ),
        io.write_string(Line ++ "\n", !IO),
        io.write_string(Spaces ++ "^\n", !IO)
    ).

:- pred skip_ws(src::in, unit::out, ps::in, ps::out) is semidet.

skip_ws(Src, unit) -->
    whitespace(Src, _),
    ( next_char(Src, ('/')), next_char(Src, ('*')) ->
        find_close_comment(Src),
        skip_ws(Src, _)
    ;
        { true }
    ).

:- pred find_close_comment(src::in, ps::in, ps::out) is semidet.

find_close_comment(Src) -->
    ( next_char(Src, C) ->
        ( { C = ('*') } ->
            ( next_char(Src, ('/')) ->
                { true }
            ;
                find_close_comment(Src)
            )
        ;
            find_close_comment(Src)
        )
    ;
        fail_with_message("unterminated comment", Src, _:unit)
    ).

:- type expr
    --->    op(op, expr, expr)
    ;       function_application(string, list(expr))
    ;       integer(int)
    ;       variable(string).

:- type op
    --->    plus
    ;       minus
    ;       modulo.

:- pred expr_top(src::in, expr::out, ps::in, ps::out) is semidet.

expr_top(Src, Expr) -->
    expr(Src, Expr),
    eof(Src, _).

:- pred expr(src::in, expr::out, ps::in, ps::out) is semidet.

expr(Src, Expr) -->
    term(Src, Term1),
    ( op(Src, Op) ->
        expr(Src, Expr2),
        { Expr = op(Op, Term1, Expr2) }
    ;
        { Expr = Term1 }
    ).

:- pred term(src::in, expr::out, ps::in, ps::out) is semidet.

term(Src, Term) -->
    current_offset(Src, Start),
    ( int_literal(Src, Int) ->
        { Term = integer(Int) }
    ;
        id(Src, Id)
    ->
        ( punct("(", Src, _) ->
            ( { known_function(Id) } ->
                comma_separated_list(expr, Src, Args),
                punct(")", Src, _),
                { Term = function_application(Id, Args) }
            ;
                fail_with_message("unknown function: " ++ Id, Start, Src, Term)
            )
        ;
            { Term = variable(Id) }
        )
    ;
        { fail }
    ).

:- pred known_function(string::in) is semidet.

known_function("abs").
known_function("pow").

:- pred op(src::in, op::out, ps::in, ps::out) is semidet.

op(Src, Op) -->
    ( punct("+", Src, _) ->
        { Op = plus }
    ; punct("-", Src, _) ->
        { Op = minus }
    ; ikeyword(id_chars, "mod", Src, _) ->
        { Op = modulo }
    ;
        fail_with_message("expecting an operator", Src, Op)
    ).

:- pred id(src::in, string::out, ps::in, ps::out) is semidet.

id(Src, Id) -->
    identifier(id_chars, id_chars ++ "0123456789", Src, Id).

:- func id_chars = string.

id_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".

%---------------------------------------------------------------------------%
