%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for deep_copy
%

:- module deep_copy.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
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
                T,      % 15
                int,    % 16
                T       % 17
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

main(!IO) :-
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(!IO),
    test_other(!IO).

%---------------------------------------------------------------------------%

:- pred test_all(T::in, io::di, io::uo) is det.

test_all(T, !IO) :-
    io.write(T, !IO),
    io.write_string("\n", !IO),
    copy(T, TCopy),
    io.write(T, !IO),
    io.write_string("\n", !IO),
    io.write(TCopy, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_discriminated(io::di, io::uo) is det.

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % Test enumerations.
    test_all(two, !IO),
    test_all(one, !IO),
    test_all(three, !IO),

    % Test no secondary tags.
    test_all(apple([9, 5, 1]), !IO),
    test_all(banana([three, one, two]), !IO),

    % Test remote secondary tags.
    test_all(zop(3.3, 2.03), !IO),
    test_all(zip(3, 2), !IO),
    test_all(zap(3, -2.111), !IO),

    % Test local secondary tags.
    test_all(wombat, !IO),
    test_all(foo, !IO),

    % Test the contains_var bit vector.
    test_all(tuple_a(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, 17), !IO),
    test_all(tuple_b(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, ["x", "y", "z"]), !IO),
    test_all(tuple_c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, 15, ["p", "q"], 17), !IO),
    test_all(tuple_d(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        ["d", "e", "f"], 15, ["u", "v", "w"], 17), !IO),

    io.nl(!IO).

:- pred test_polymorphism(io::di, io::uo) is det.

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    test_all(poly_three(3.33, 4, poly_one(9.11)), !IO),
    test_all(poly_two(3) : poly(int, int), !IO),
    test_all(poly_one([2399.3]) : poly(list(float), int), !IO),

    io.nl(!IO).

:- pred test_builtins(io::di, io::uo) is det.

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % Test strings.
    test_all("", !IO),
    test_all("Hello, world\n", !IO),
    test_all("Foo%sFoo", !IO),
    test_all("""", !IO),

    % Test characters.
    test_all('a', !IO),
    test_all('&', !IO),

    % Test floats.
    test_all(3.14159, !IO),
    test_all(11.28324983E-22, !IO),
    test_all(22.3954899E22, !IO),

    % Test integers.
    test_all(-65, !IO),
    test_all(4, !IO),

    test_all(561u, !IO),

    test_all(-10i8, !IO),
    test_all(11i8, !IO),
    test_all(12u8, !IO),

    test_all(-13i16, !IO),
    test_all(14i16, !IO),
    test_all(15i16, !IO),

    test_all(-16i32, !IO),
    test_all(17i32, !IO),
    test_all(18u32, !IO),

    test_all(-19i64, !IO),
    test_all(20i64, !IO),
    test_all(21u64, !IO),

    % Test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    test_all(Univ, !IO),

    % Test predicates.
    % XXX We don't deep copy predicates correctly yet.
    % test_all(test_other),

    % Test tuples.
    test_all({1, "two", '3', {4, '5', "6"}}, !IO),

    io.nl(!IO).

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    %
:- pred test_other(io::di, io::uo) is det.

test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply : var_supply(int)),
    term.create_var(Var, VarSupply, NewVarSupply),
    test_all(Var, !IO),
    test_all(VarSupply, !IO),
    test_all(NewVarSupply, !IO),

    % Presently, at least, map is an equivalence and an abstract type.
    map.init(Map : map(int, int)),
    test_all(Map, !IO),

    % Test a notag type.
    test_all(qwerty(4), !IO),

    io.nl(!IO).
