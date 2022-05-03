%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for io.write (and io.write_line).
%
% Author: trd
%
% The .exp2 file is for MSVC.
% The .exp3 file is for the C# grade.
% The .exp4 file is for the Java grade.
% The .exp file is for other systems.

:- module write.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module char.
:- import_module float.
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
    io.write_line(var("X") + int(3) * var("X^2") ; (type), !IO),
    io.write_line(write.{type}, !IO),
    io.write_line(write.{?-}, !IO),
    io.write_line((?-), !IO),
    io.write_line(write.{blah}, !IO),
    io.write_line((blah ; (type), (type) * blah ; (type)), !IO),
    io.write_line(((blah ; blah), blah) * blah ; blah, !IO),
    io.write_line((type) * blah ; (type), !IO).

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % Test enumerations.
    io.write_line(one, !IO),
    io.write_line(two, !IO),
    io.write_line(three, !IO),

    % Test simple tags.
    io.write_line(apple([9, 5, 1]), !IO),
    io.write_line(banana([three, one, two]), !IO),

    % Test complicated tags.
    io.write_line(zop(3.3, 2.03), !IO),
    io.write_line(zip(3, 2), !IO),
    io.write_line(zap(3, -2.111), !IO),

    % Test complicated constant.
    io.write_line(wombat, !IO),
    io.write_line(foo, !IO),

    io.nl(!IO).

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    io.write_line(poly_one([2399.3]), !IO),
    io.write_line(poly_two(3), !IO),
    io.write_line(poly_three(3.33, 4, poly_one(9.11)), !IO),

    io.nl(!IO).

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % Test strings.
    io.write_line("", !IO),
    io.write_line("Hello, world\n", !IO),
    io.write_line("Foo%sFoo", !IO),
    io.write_line("""", !IO),    % interesting - prints """ of course
    io.write_line("\a\b\f\t\n\r\v\"\\", !IO),
    io.write_line("\001\\037\\177\\200\\237\\240\", !IO),

    % Test characters.
    io.write_line('a', !IO),
    io.write_line('A', !IO),
    io.write_line('&', !IO),

    io.write_line('\001\', !IO), % Second C0 control.
    io.write_line('\a', !IO),
    io.write_line('\b', !IO),
    io.write_line('\f', !IO),
    io.write_line('\t', !IO),
    io.write_line('\n', !IO),
    io.write_line('\r', !IO),
    io.write_line('\v', !IO),
    io.write_line('\037\', !IO), % Last C0 control.
    io.write_line(' ', !IO),

    io.write_line('\'', !IO),
    io.write_line(('\\') : character, !IO),
    io.write_line('\"', !IO),

    io.write_line('~', !IO),
    io.write_line('\177\', !IO), % Delete.
    io.write_line('\200\', !IO), % First C1 control.
    io.write_line('\237\', !IO), % Last C1 control.
    io.write_line('\240\', !IO), % No-break space.

    % Test floats.
    io.write_line(0.0, !IO),
    io.write_line(3.14159, !IO),
    io.write_line(11.28324983E-22, !IO),
    io.write_line(22.3954899E22, !IO),
    NegInf : float = -float.infinity,
    io.write_line(NegInf, !IO),
    io.write_line(float.infinity, !IO),

    % Test ints.
    io.write_line(-65, !IO),
    io.write_line(4, !IO),

    % Test uints.
    io.write_line(651u, !IO),

    % Test fixed-size ints.
    io.write_line(-128i8, !IO),
    io.write_line(127i8, !IO),
    io.write_line(255u8, !IO),
    io.write_line(-32768i16, !IO),
    io.write_line(32767i16, !IO),
    io.write_line(65535u16, !IO),
    io.write_line(-2147483648i32, !IO),
    io.write_line(2147483647i32, !IO),
    io.write_line(4294967295u32, !IO),
    io.write_line(-9223372036854775808i64, !IO),
    io.write_line(9223372036854775807i64, !IO),
    io.write_line(18446744073709551615u64, !IO),

    % Test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    io.write_line(Univ, !IO),

    % Test predicates.
    io.write_line(newline, !IO),

    io.nl(!IO).

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    %
test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply),
    term.create_var(Var, VarSupply, NewVarSupply),
    io.write_line(Var, !IO),
    io.write_line(VarSupply, !IO),
    io.write_line(NewVarSupply, !IO),

    % Presently, at least, map is an equivalence and
    % an abstract type.
    map.init(Map),
    io.write_line(Map, !IO),

    % A no tag type.
    io.write_line(qwerty(4), !IO),

    array.from_list([1, 2, 3, 4], Array),
    io.write_line(Array, !IO),

    VersionArray = version_array.from_list([1, 2, 3, 4]),
    io.write_line(VersionArray, !IO),

    io.nl(!IO).

newline(!IO) :-
    io.write_char('\n', !IO).
