%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test:
%
% This test ensures that output from io.write is put on the correct
% output stream. All the functionality of io.write is tested so that
% an additional changes (such as adding a printf instead of a
% fprintf(current_stream, ...)) should be caught.
%
% The Mercury compiler of 20 Dec 1996 failed to correctly run this test.
%
% Author: trd
%

:- module write_reg1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module term.
:- import_module univ.

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

main(!IO) :-
    io.stderr_stream(StdErr, !IO),
    io.set_output_stream(StdErr, _StdOut, !IO),
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(StdErr, !IO),
    test_other(!IO).

:- pred test_discriminated(io::di, io::uo) is det.

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % test enumerations
    io.write(one, !IO), newline(!IO),
    io.write(two, !IO), newline(!IO),
    io.write(three, !IO), newline(!IO),

    % test simple tags
    io.write(apple([9, 5, 1]), !IO), newline(!IO),
    io.write(banana([three, one, two]), !IO), newline(!IO),

    % test complicated tags
    io.write(zop(3.3, 2.03), !IO), newline(!IO),
    io.write(zip(3, 2), !IO), newline(!IO),
    io.write(zap(3, -2.111), !IO), newline(!IO),

    % test complicated constant
    io.write(wombat, !IO), newline(!IO),
    io.write(foo, !IO), newline(!IO),

    newline(!IO).

:- pred test_polymorphism(io::di, io::uo) is det.

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    io.write(poly_one([2399.3]) : poly(list(float), float), !IO), newline(!IO),
    io.write(poly_two(3) : poly(int, int), !IO), newline(!IO),
    io.write(poly_three(3.33, 4, poly_one(9.11)), !IO), newline(!IO),

    newline(!IO).

:- pred test_builtins(io.output_stream::in, io::di, io::uo) is cc_multi.

test_builtins(StdErr, !IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % test strings
    io.write("", !IO), newline(!IO),
    io.write("Hello, world\n", !IO), newline(!IO),
    io.write("Foo%sFoo", !IO), newline(!IO),
    io.write("""", !IO), newline(!IO),   % interesting - prints """ of course

    % test characters
    io.write('a', !IO), newline(!IO),
    io.write('&', !IO), newline(!IO),

    % test floats
    io.write(3.14159, !IO), newline(!IO),
    io.write(11.28324983E-22, !IO), newline(!IO),
    io.write(22.3954899E22, !IO), newline(!IO),

    % test integers
    io.write(-65, !IO), newline(!IO),
    io.write(4, !IO), newline(!IO),

    % test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    io.write(Univ, !IO), newline(!IO),

    % test predicates
    io.write(newline, !IO), newline(!IO),
    io.write(StdErr, include_details_cc, newline, !IO), newline(!IO),

    newline(!IO).

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    %
:- pred test_other(io::di, io::uo) is det.

test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply),
    term.create_var(Var : var, VarSupply, NewVarSupply),
    io.write(Var, !IO), newline(!IO),
    io.write(VarSupply, !IO), newline(!IO),
    io.write(NewVarSupply, !IO), newline(!IO),

    % presently, at least, map is an equivalence and an abstract type.
    map.init(Map : map(int, int)),
    io.write(Map, !IO), newline(!IO),

    % a no tag type
    io.write(qwerty(4), !IO), newline(!IO),

    newline(!IO).

:- pred newline(io::di, io::uo) is det.

newline(!IO) :-
    io.write_char('\n', !IO).
