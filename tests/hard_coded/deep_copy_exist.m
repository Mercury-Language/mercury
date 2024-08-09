%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for deep_copy of existentially quantified
% data types.

:- module deep_copy_exist.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module enum.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module univ.

:- type my_enum
    --->    one
    ;       two
    ;       three.

:- instance enum(my_enum) where [
    to_int(one) = 1,
    to_int(two) = 2,
    to_int(three) = 3,
    from_int(1, one),
    from_int(2, two),
    from_int(3, three)
].

:- type fruit
    --->    some [T] apple(list(T))
    ;       some [T] banana(list(T)) => enum(T).

:- type thingie
    --->    foo
    ;       some [T] bar(T) ; some [T1, T2] bar(T1, T2)
    ;       some [T] (qux(T) => enum(T))
    ;       some [T] (quux(list(T)) => enum(T))
    ;       some [T3] quuux(int, T3) ; wombat
    ;       zoom(int) ; some [T] zap(int, T)
    ;       some [T1, T2] zip(T1, T2) => enum(T2)
    ;       some [T] zop(float, T).

:- type poly(A)
    --->    poly_one(A)
    ;       some [B] poly_two(B)
    ;       some [B] poly_three(B, A, poly(B))
    ;       some [B] poly_four(A, B) => enum(B).

:- type bit_vector_test
    --->    some [T] tuple_a(
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
    ;       some [T] tuple_b(
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
    ;       some [T1] tuple_c(
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
                T1,     % 16
                int     % 17
            )
    ;       some [T1, T2] tuple_d(
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
                T1,     % 14
                int,    % 15
                T2,     % 16
                int     % 17
            ) => enum(T2).

%---------------------------------------------------------------------------%

main(!IO) :-
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(!IO).

%---------------------------------------------------------------------------%

:- pred test_all(T::in, io::di, io::uo) is det.

test_all(T, !IO) :-
    io.write(T, !IO),
    io.write_string("\n", !IO),
    copy(T, T1),
    io.write(T, !IO),
    io.write_string("\n", !IO),
    io.write_line(T1, !IO).

%---------------------------------------------------------------------------%

:- pred test_discriminated(io::di, io::uo) is det.

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % test no secondary tags
    test_all('new apple'([9, 5, 1]), !IO),
    test_all('new banana'([three, one, two]), !IO),

    % test remote secondary tags
    test_all('new zop'(3.3, 2.03), !IO),
    test_all('new zip'(3, 2), !IO),
    test_all('new zap'(3, -2.111), !IO),

    % test local secondary tags
    test_all(wombat, !IO),
    test_all(foo, !IO),

    % test the contains_var bit vector
    test_all('new tuple_a'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, 17), !IO),
    test_all('new tuple_b'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, ["a", "b", "c"], 16, ["x", "y", "z"]), !IO),
    test_all('new tuple_c'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        14, 15, ["p", "q"], 17), !IO),
    test_all('new tuple_d'(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
        'a', 15, 'z', 17), !IO),

    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_polymorphism(io::di, io::uo) is det.

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    test_all('new poly_three'(3.33, 4, poly_one(9.11)), !IO),
    test_all('new poly_two'(3), !IO),
    test_all(poly_one([2399.3]), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_builtins(io::di, io::uo) is det.

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    test_all(Univ, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
