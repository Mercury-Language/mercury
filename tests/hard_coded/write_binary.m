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

:- implementation.

:- import_module array.
:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module io.file.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module term.

:- pred test_ops(io::di, io::uo) is cc_multi.
:- pred test_builtins(io::di, io::uo) is cc_multi.
:- pred test_discriminated(io::di, io::uo) is cc_multi.
:- pred test_polymorphism(io::di, io::uo) is cc_multi.
:- pred test_other(io::di, io::uo) is cc_multi.
:- pred do_test(T::in, io::di, io::uo) is cc_multi.
:- pred do_test_2(T::in, T::out, io::di, io::uo) is det.

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

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

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

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    do_test(poly_one([2399.3]) `with_type` poly(list(float), int), !IO),
    do_test(poly_two(3) `with_type` poly(list(float), int), !IO),
    do_test(poly_three(3.33, 4, poly_one(9.11)), !IO).

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

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
    io.write_string("next text is expected to fail:\n", !IO),
    do_test(do_test `with_type` pred(int, io, io), !IO).

test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),
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

do_test(Term, !IO) :-
    try_io(do_test_2(Term), Result, !IO),
    ( if Result = succeeded(TermRead) then
        io.print("test passed:\n", !IO),
        io.print(Term, !IO), io.nl(!IO),
        io.print(TermRead, !IO), io.nl(!IO)
    else
        io.print("test failed:\n", !IO),
        io.print(Result, !IO), io.nl(!IO),
        io.print(Term, !IO), io.nl(!IO)
    ).

do_test_2(Term, TermRead, !IO) :-
    io.file.make_temp_file(FileNameRes, !IO),
    ( if FileNameRes = ok(FileName) then
        io.open_binary_output(FileName, OutputRes, !IO),
        ( if OutputRes = ok(OutputStream) then
            io.write_byte(OutputStream, 42, !IO),
            io.write_binary(OutputStream, Term, !IO),
            io.write_byte(OutputStream, 43, !IO),
            io.close_binary_output(OutputStream, !IO),
            io.open_binary_input(FileName, InputRes, !IO),
            ( if InputRes = ok(InputStream) then
                io.read_byte(InputStream, B42, !IO),
                io.read_binary(InputStream, Result, !IO),
                io.read_byte(InputStream, B43, !IO),
                io.close_binary_input(InputStream, !IO),
                ( if
                    B42 = ok(42),
                    B43 = ok(43),
                    Result = ok(TermRead0),
                    TermRead0 = Term
                then
                    io.file.remove_file(FileName, _, !IO),
                    io.print("ok... ", !IO),
                    TermRead = TermRead0
                else
                    io.file.remove_file(FileName, _, !IO),
                    throw("error reading term back in again")
                )
            else
                io.file.remove_file(FileName, _, !IO),
                throw(InputRes)
            )
        else
            io.file.remove_file(FileName, _, !IO),
            throw(OutputRes)
        )
    else
        unexpected($pred, "cannot open temp file")
    ).
