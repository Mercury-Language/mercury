%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for io.write_binary/io.read_binary.
% (adapted from tests/hard_coded/write.m, which tests io.write).

% XXX currently we do not pass the test of "univ"!

:- module write_binary.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module io.file.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module univ.

%---------------------------------------------------------------------------%

:- type enum
    --->    one
    ;       two
    ;       three.

:- type fruit
    --->    apple(list(int))
    ;       banana(list(enum)).

:- type thingie
    --->    foo
    ;       bar(int)
    ;       bar(int, int)
    ;       qux(int)
    ;       quux(int)
    ;       quuux(int, int)
    ;       wombat
    ;       zoom(int)
    ;       zap(int, float)
    ;       zip(int, int)
    ;       zop(float, float).

:- type poly(A, B)
    --->    poly_one(A)
    ;       poly_two(B)
    ;       poly_three(B, A, poly(B, A)).

:- type no_tag
    --->    qwerty(int).

:- type expr
    --->    var(string)
    ;       int(int)
    ;       expr + expr
    ;       expr - expr
    ;       expr * expr
    ;       (expr, expr)
    ;       {expr; expr}
    ;       {{expr}}
    ;       (type)
    ;       blah
    ;       (?-).

main(!IO) :-
    test_ops(!IO),
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(!IO),
    test_other(!IO).

:- pred test_ops(io::di, io::uo) is cc_multi.

test_ops(!IO) :-
    io.write_string("TESTING TERMS WITH OPERATORS\n", !IO),
    do_test(var("X") + int(3) * var("X^2") ; (type), !IO),
    do_test(write_binary.{type}, !IO),
    do_test(write_binary.{?-}, !IO),
    do_test((?-), !IO),
    do_test(write_binary.{blah}, !IO),
    do_test((blah ; (type), (type) * blah ; (type)), !IO),
    do_test(((blah ; blah), blah) * blah ; blah, !IO),
    do_test((type) * blah ; (type), !IO).

:- pred test_discriminated(io::di, io::uo) is cc_multi.

test_discriminated(!IO) :-
    io.write_string("\nTESTING DISCRIMINATED UNIONS\n", !IO),

    % test enumerations
    do_test(one, !IO),
    do_test(two, !IO),
    do_test(three, !IO),

    % test simple tags
    do_test(apple([9, 5, 1]), !IO),
    do_test(banana([three, one, two]), !IO),

    % test complicated tags
    do_test(zop(3.3, 2.03), !IO),
    do_test(zip(3, 2), !IO),
    do_test(zap(3, -2.111), !IO),

    % test complicated constant
    do_test(wombat, !IO),
    do_test(foo, !IO).

:- pred test_polymorphism(io::di, io::uo) is cc_multi.

test_polymorphism(!IO) :-
    io.write_string("\nTESTING POLYMORPHISM\n", !IO),
    do_test(poly_one([2399.3]) `with_type` poly(list(float), int), !IO),
    do_test(poly_two(3) `with_type` poly(list(float), int), !IO),
    do_test(poly_three(3.33, 4, poly_one(9.11)), !IO).

:- pred test_builtins(io::di, io::uo) is cc_multi.

test_builtins(!IO) :-
    io.write_string("\nTESTING BUILTINS\n", !IO),

    % test strings
    do_test("", !IO),
    do_test("Hello, world\n", !IO),
    do_test("Foo%sFoo", !IO),
    do_test("""", !IO),

    % test characters
    do_test('a' `with_type` char, !IO),
    do_test('&' `with_type` char, !IO),
    do_test('.' `with_type` char, !IO),
    do_test('%' `with_type` char, !IO),
    do_test(' ' `with_type` char, !IO),
    do_test('\t' `with_type` char, !IO),
    do_test('\n' `with_type` char, !IO),
    do_test(('\\') `with_type` char, !IO),
    do_test('*' `with_type` char, !IO),
    do_test('/' `with_type` char, !IO),

    % test floats
    do_test(3.14159, !IO),
    do_test(11.28324983E-22, !IO),
    do_test(22.3954899E22, !IO),

    % test ints
    do_test(-65, !IO),
    do_test(4, !IO),

    % Test uints.
    do_test(651u, !IO),

    % Test fixed-size ints.
    do_test(-128i8, !IO),
    do_test(127i8, !IO),
    do_test(255u8, !IO),
    do_test(-32768i16, !IO),
    do_test(32767i16, !IO),
    do_test(65535u16, !IO),
    do_test(-2147483648i32, !IO),
    do_test(2147483647i32, !IO),
    do_test(4294967295u32, !IO),

%%% XXX currently we do not pass this test!
%%%     % test univ.
%%% type_to_univ(["hi! I'm a univ!"], Univ),
%%% do_test(Univ, !IO),

    % test predicates
    % io.read_binary doesn't work for higher-order terms,
    % so this test is expected to fail.
    io.write_string("\nthe next test is expected to fail:\n", !IO),
    do_test(do_test `with_type` pred(int, io, io), !IO).

:- pred test_other(io::di, io::uo) is cc_multi.

test_other(!IO) :-
    io.write_string("\nTESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply `with_type` var_supply(generic)),
    term.create_var(Var, VarSupply, NewVarSupply),
    do_test(Var, !IO),
    do_test(VarSupply, !IO),
    do_test(NewVarSupply, !IO),

    % presently, at least, map is an equivalence and
    % an abstract type.
    map.init(Map `with_type` map(int, string)),
    do_test(Map, !IO),

    % a no tag type
    do_test(qwerty(4), !IO),

    array.from_list([1, 2, 3, 4], Array),
    do_test(Array, !IO).

%---------------------------------------------------------------------------%

:- pred do_test(T::in, io::di, io::uo) is cc_multi.

do_test(Term, !IO) :-
    io.write_string("\ntest input <", !IO),
    io.print(Term, !IO),
    io.write_string(">\n", !IO),
    % The code of do_test_2 itself does not throw any exceptions (anymore),
    % but the implementations of io.write_binary and io.read_binary in the
    % Mercury standard library *may* throw exceptions. We catch them here.
    try_io(do_test_2(Term), TryResult, !IO),
    (
        TryResult = succeeded(MaybeTestErrorMsg),
        (
            MaybeTestErrorMsg = no,
            io.print("test passed\n", !IO)
        ;
            MaybeTestErrorMsg = yes(ErrorMsg),
            io.format("test failed with this error:\n%s\n", [s(ErrorMsg)], !IO)
        )
    ;
        TryResult = exception(Univ),
        io.write_string("test threw this exception:\n", !IO),
        io.print_line(Univ, !IO)
    ).

:- pred do_test_2(T::in, maybe(string)::out, io::di, io::uo) is det.

do_test_2(Term, MaybeTestErrorMsg, !IO) :-
    io.file.make_temp_file(FileNameRes, !IO),
    ( if FileNameRes = ok(FileName) then
        io.open_binary_output(FileName, OutputResult, !IO),
        (
            OutputResult = ok(OutputStream),
            io.write_byte(OutputStream, 42, !IO),
            io.write_binary(OutputStream, Term, !IO),
            io.write_byte(OutputStream, 43, !IO),
            io.close_binary_output(OutputStream, !IO),
            io.open_binary_input(FileName, InputResult, !IO),
            (
                InputResult = ok(InputStream),
                io.read_byte(InputStream, Result42, !IO),
                io.read_binary(InputStream, ResultTerm, !IO),
                io.read_byte(InputStream, Result43, !IO),
                io.close_binary_input(InputStream, !IO),
                ( if
                    Result42 = ok(42),
                    ResultTerm = ok(Term),
                    Result43 = ok(43)
                then
                    MaybeTestErrorMsg = no
                else
                    ( if
                        Result42 = ok(42),
                        ResultTerm = ok(Term)
                    then
                        (
                            Result43 = ok(ReadBack43),
                            BackStr = string.string(ReadBack43),
                            string.format("orig 43, readback <%s>",
                                [s(BackStr)], Msg)
                        ;
                            Result43 = error(IOError),
                            io.error_message(IOError, IOErrorMsg),
                            string.format("orig 43, readback error\n<%s>",
                                [s(IOErrorMsg)], Msg)
                        ;
                            Result43 = eof,
                            Msg = "orig 43, readback eof"
                        )
                    else if
                        Result42 = ok(42)
                    then
                        OrigStr = string.string(Term),
                        (
                            ResultTerm = ok(ReadBackTerm),
                            BackStr = string.string(ReadBackTerm),
                            string.format("orig %s, readback <%s>",
                                [s(OrigStr), s(BackStr)], Msg)
                        ;
                            ResultTerm = error(IOError),
                            io.error_message(IOError, IOErrorMsg),
                            string.format("orig %s, readback error\n<%s>",
                                [s(OrigStr), s(IOErrorMsg)], Msg)
                        ;
                            ResultTerm = eof,
                            string.format("orig %s, readback eof",
                                [s(OrigStr)], Msg)
                        )
                    else
                        (
                            Result42 = ok(ReadBack42),
                            BackStr = string.string(ReadBack42),
                            string.format("orig 42, readback <%s>",
                                [s(BackStr)], Msg)
                        ;
                            Result42 = error(IOError),
                            io.error_message(IOError, IOErrorMsg),
                            string.format("orig 42, readback error\n<%s>",
                                [s(IOErrorMsg)], Msg)
                        ;
                            Result42 = eof,
                            Msg = "orig 42, readback eof"
                        )
                    ),
                    MaybeTestErrorMsg = yes(Msg)
                )
            ;
                InputResult = error(IOError),
                io.error_message(IOError, IOErrorMsg),
                MaybeTestErrorMsg = yes(IOErrorMsg)
            )
        ;
            OutputResult = error(IOError),
            io.error_message(IOError, IOErrorMsg),
            MaybeTestErrorMsg = yes(IOErrorMsg)
        ),
        io.file.remove_file(FileName, _, !IO)
    else
        MaybeTestErrorMsg = yes("cannot create a temp file")
    ).

%---------------------------------------------------------------------------%
