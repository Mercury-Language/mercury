% vim: ft=mercury ts=4 sw=4 et
% Test case for io.write (and io.write_line).
%
% Author: trd

:- module write.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module term.
:- import_module univ.
:- import_module version_array.

:- pred test_ops(io::di, io::uo) is det.
:- pred test_builtins(io::di, io::uo) is det.
:- pred test_discriminated(io::di, io::uo) is det.
:- pred test_polymorphism(io::di, io::uo) is det.
:- pred test_other(io::di, io::uo) is det.
:- pred newline(io::di, io::uo) is det.

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
    io.write(var("X") + int(3) * var("X^2") ; (type), !IO), newline(!IO),
    io.write(write.{type}, !IO), newline(!IO),
    io.write(write.{?-}, !IO), newline(!IO),
    io.write((?-), !IO), newline(!IO),
    io.write(write.{blah}, !IO), newline(!IO),
    io.write((blah ; (type), (type) * blah ; (type)), !IO), newline(!IO),
    io.write(((blah ; blah), blah) * blah ; blah, !IO), newline(!IO),
    io.write((type) * blah ; (type), !IO), newline(!IO).

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % Test enumerations.
    io.write_line(one, !IO),
    io.write_line(two, !IO),
    io.write_line(three, !IO),

    % Test simple tags.
    io.write(apple([9, 5, 1]), !IO), newline(!IO),
    io.write(banana([three, one, two]), !IO), newline(!IO),

    % Test complicated tags.
    io.write(zop(3.3, 2.03), !IO), newline(!IO),
    io.write(zip(3, 2), !IO), newline(!IO),
    io.write(zap(3, -2.111), !IO), newline(!IO),

    % Test complicated constant.
    io.write(wombat, !IO), newline(!IO),
    io.write(foo, !IO), newline(!IO),

    newline(!IO).

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    io.write(poly_one([2399.3]), !IO), newline(!IO),
    io.write(poly_two(3), !IO), newline(!IO),
    io.write(poly_three(3.33, 4, poly_one(9.11)), !IO), newline(!IO),

    newline(!IO).

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % Test strings.
    io.write("", !IO), newline(!IO),
    io.write("Hello, world\n", !IO), newline(!IO),
    io.write("Foo%sFoo", !IO), newline(!IO),
    io.write("""", !IO), newline(!IO),    % interesting - prints """ of course

    % Test characters.
    io.write('a', !IO), newline(!IO),
    io.write('&', !IO), newline(!IO),

    % Test floats.
    io.write(3.14159, !IO), newline(!IO),
    io.write(11.28324983E-22, !IO), newline(!IO),
    io.write(22.3954899E22, !IO), newline(!IO),

    % Test ints.
    io.write(-65, !IO), newline(!IO),
    io.write(4, !IO), newline(!IO),

    % Test uints.
    io.write(651u, !IO), newline(!IO),

    % Test fixed-size ints.
    io.write(-128i8, !IO), newline(!IO),
    io.write(127i8, !IO), newline(!IO),
    io.write(255u8, !IO), newline(!IO),
    io.write(-32768i16, !IO), newline(!IO),
    io.write(32767i16, !IO), newline(!IO),
    io.write(65535u16, !IO), newline(!IO),
    io.write(-2147483648i32, !IO), newline(!IO),
    io.write(2147483647i32, !IO), newline(!IO),
    io.write(4294967295u32, !IO), newline(!IO),

    % Test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    io.write(Univ, !IO), newline(!IO),

    % Test predicates.
    io.write(newline, !IO), newline(!IO),

    newline(!IO).

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    %
test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply),
    term.create_var(Var, VarSupply, NewVarSupply),
    io.write(Var, !IO), newline(!IO),
    io.write(VarSupply, !IO), newline(!IO),
    io.write(NewVarSupply, !IO), newline(!IO),

    % Presently, at least, map is an equivalence and
    % an abstract type.
    map.init(Map),
    io.write(Map, !IO), newline(!IO),

    % A no tag type.
    io.write(qwerty(4), !IO), newline(!IO),

    array.from_list([1, 2, 3, 4], Array),
    io.write(Array, !IO), newline(!IO),

    VersionArray = version_array.from_list([1, 2, 3, 4]),
    io.write(VersionArray, !IO), newline(!IO),

    newline(!IO).

newline(!IO) :-
    io.write_char('\n', !IO).
