%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for deep_copy
%
% Authors: trd, zs

:- module deep_copy.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module univ.

:- pred test_builtins(io__state::di, io__state::uo) is det.
:- pred test_discriminated(io__state::di, io__state::uo) is det.
:- pred test_polymorphism(io__state::di, io__state::uo) is det.
:- pred test_other(io__state::di, io__state::uo) is det.

:- pred newline(io__state::di, io__state::uo) is det.

:- pred test_all(T::in, io__state::di, io__state::uo) is det.

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
    ;       poly_three(B, A, poly(B, A))
    ;       poly_four(A, B).

:- type no_tag
    --->    qwerty(int).

:- type bit_vector_test(T)
    --->    tuple_a(
                int,    % 0
                int,    % 1
                int,    % 2
                int,    % 3
                int,    % 4
                int,    % 5
                int,    % 6
                int,    % 7
                int,    % 8
                int,    % 9
                int,    % 10
                int,    % 11
                int,    % 12
                int,    % 13
                int,    % 14
                T,      % 15
                int,    % 16
                int     % 17
            )
    ;       tuple_b(
                int,    % 0
                int,    % 1
                int,    % 2
                int,    % 3
                int,    % 4
                int,    % 5
                int,    % 6
                int,    % 7
                int,    % 8
                int,    % 9
                int,    % 10
                int,    % 11
                int,    % 12
                int,    % 13
                int,    % 14
                T,  % 15
                int,    % 16
                T   % 17
            )
    ;       tuple_c(
                int,    % 0
                int,    % 1
                int,    % 2
                int,    % 3
                int,    % 4
                int,    % 5
                int,    % 6
                int,    % 7
                int,    % 8
                int,    % 9
                int,    % 10
                int,    % 11
                int,    % 12
                int,    % 13
                int,    % 14
                int,    % 15
                T,      % 16
                int     % 17
            )
    ;       tuple_d(
                int,    % 0
                int,    % 1
                int,    % 2
                int,    % 3
                int,    % 4
                int,    % 5
                int,    % 6
                int,    % 7
                int,    % 8
                int,    % 9
                int,    % 10
                int,    % 11
                int,    % 12
                int,    % 13
                T,      % 14
                int,    % 15
                T,      % 16
                int     % 17
            ).

%---------------------------------------------------------------------------%

main -->
    test_discriminated,
    test_polymorphism,
    test_builtins,
    test_other.

%---------------------------------------------------------------------------%

test_all(T) -->
    io__write(T),
    io__write_string("\n"),
    { copy(T, T1) },
    io__write(T),
    io__write_string("\n"),
    io__write(T1),
    newline.

%---------------------------------------------------------------------------%

test_discriminated -->
    io__write_string("TESTING DISCRIMINATED UNIONS\n"),

        % test enumerations
    test_all(two),
    test_all(one),
    test_all(three),

        % test no secondary tags
    test_all(apple([9, 5, 1])),
    test_all(banana([three, one, two])),

        % test remote secondary tags
    test_all(zop(3.3, 2.03)),
    test_all(zip(3, 2)),
    test_all(zap(3, -2.111)),

        % test local secondary tags
    test_all(wombat),
    test_all(foo),

        % test the contains_var bit vector
    test_all(tuple_a(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, 17)),
    test_all(tuple_b(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, ["x", "y", "z"])),
    test_all(tuple_c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, 15, ["p", "q"], 17)),
    test_all(tuple_d(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        ["d", "e", "f"], 15, ["u", "v", "w"], 17)),

    newline.

test_polymorphism -->
    io__write_string("TESTING POLYMORPHISM\n"),
    test_all(poly_three(3.33, 4, poly_one(9.11))),
    test_all(poly_two(3)),
    test_all(poly_one([2399.3])),

    newline.

test_builtins -->
    io__write_string("TESTING BUILTINS\n"),

        % test strings
    test_all(""),
    test_all("Hello, world\n"),
    test_all("Foo%sFoo"),
    test_all(""""),

        % test characters
    test_all('a'),
    test_all('&'),

        % test floats
    test_all(3.14159),
    test_all(11.28324983E-22),
    test_all(22.3954899E22),

        % test integers
    test_all(-65),
    test_all(4),

    test_all(561u),

    test_all(-10i8),
    test_all(11i8),
    test_all(12u8),

    test_all(-13i16),
    test_all(14i16),
    test_all(15i16),

    test_all(-16i32),
    test_all(17i32),
    test_all(18u32),

    test_all(-19i64),
    test_all(20i64),
    test_all(21u64),

        % test univ.
    { type_to_univ(["hi! I'm a univ!"], Univ) },
    test_all(Univ),

        % test predicates
        % XXX we don't deep copy predicates correctly yet
    %test_all(newline),

        % test tuples
    test_all({1, "two", '3', {4, '5', "6"}}),

    newline.

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.

test_other -->
    io__write_string("TESTING OTHER TYPES\n"),
    { term__init_var_supply(VarSupply) },
    { term__create_var(Var, VarSupply, NewVarSupply) },
    test_all(Var),
    test_all(VarSupply),
    test_all(NewVarSupply),

        % presently, at least, map is an equivalence and
        % an abstract type.
    { map__init(Map) },
    test_all(Map),

        % a no tag type
    test_all(qwerty(4)),

    newline.

newline -->
    io__write_char('\n').
