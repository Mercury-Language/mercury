%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for io.write
%
% Author: trd
%
% The .exp file is for the C backends.
% The .exp2 file is for the C backends with older versions of MSVC.
% The .exp3 file is for the C# backend.
% The .exp4 file is for the Java backend.
%
%---------------------------------------------------------------------------%

:- module expand.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module prolog.
:- import_module string.
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
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(!IO),
    test_other(!IO).

:- pred test_discriminated(io::di, io::uo) is cc_multi.

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % test enumerations
    test_all(one, !IO),
    test_all(two, !IO),
    test_all(three, !IO),

    % test simple tags
    test_all(apple([9, 5, 1]), !IO),
    test_all(banana([three, one, two]), !IO),

    % test complicated tags
    test_all(zop(3.3, 2.03), !IO),
    test_all(zip(3, 2), !IO),
    test_all(zap(3, -2.111), !IO),

    % test complicated constant

    test_all(wombat, !IO),
    test_all(foo, !IO),

    io.nl(!IO).

:- pred test_all(T::in, io::di, io::uo) is cc_multi.

test_all(T, !IO) :-
    test_functor(T, !IO),
    test_functor_number(T, !IO),
    test_arg(T, !IO),
    test_expand(T, !IO),
    test_expand_du(T, !IO),
    io.nl(!IO).

:- pred test_functor(T::in, io::di, io::uo) is det.

test_functor(T, !IO) :-
    functor(T, canonicalize, Functor, Arity),
    io.format("%s/%d\n", [s(Functor), i(Arity)], !IO).

:- pred test_functor_number(T::in, io::di, io::uo) is cc_multi.

test_functor_number(T, !IO) :-
    ( if functor_number_cc(T, FunctorNumber, Arity) then
        io.format("%d/%d\n", [i(FunctorNumber), i(Arity)], !IO)
    else
        io.write_string("functor_number_cc failed\n", !IO)
    ).

:- pred test_arg(T::in, io::di, io::uo) is det.

test_arg(T, !IO) :-
    functor(T, canonicalize, Functor, Arity),
    ( if arg(Arity, T, Argument) then
        string.format("argument %d of functor %s was:",
            [i(Arity), s(Functor)], Str),
        io.write_string(Str, !IO),
        io.print_line(Argument, !IO)
    else
        io.write_string("no arguments\n", !IO)
    ).

:- pred test_expand(T::in, io::di, io::uo) is det.

test_expand(T, !IO) :-
    deconstruct(T, canonicalize, Functor, Arity, Arguments),
    string.format("expand: functor %s arity %d arguments ",
        [s(Functor), i(Arity)], Str),
    io.write_string(Str, !IO),
    io.write_string("[", !IO),
    io.write_list(Arguments, ", ", io.print, !IO),
    io.write_string("]\n", !IO).

:- pred test_expand_du(T::in, io::di, io::uo) is cc_multi.

test_expand_du(T, !IO) :-
    ( if
        deconstruct_du(T, include_details_cc, FunctorNumber,
            Arity, Arguments)
    then
        string.format("expand: functor %d arity %d arguments ",
            [i(FunctorNumber), i(Arity)], Str),
        io.write_string(Str, !IO),
        io.write_string("[", !IO),
        io.write_list(Arguments, ", ", io.print, !IO),
        io.write_string("]\n", !IO)
    else
        io.write_string("deconstruct_du failed\n", !IO)
    ).

:- pred test_polymorphism(io::di, io::uo) is cc_multi.

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    test_all(poly_two(3), !IO),
    test_all(poly_three(3.33, 4, poly_one(9.11)), !IO),
    test_all(poly_one([2399.3]), !IO),

    io.nl(!IO).

:- pred test_builtins(io::di, io::uo) is cc_multi.

test_builtins(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

    % test strings
    test_all("", !IO),
    test_all("Hello, world\n", !IO),
    test_all("Foo%sFoo", !IO),
    test_all("""", !IO),

    % test characters
    test_all('a', !IO),
    test_all('&', !IO),

    % test floats
    test_all(3.14159, !IO),
    test_all(11.28324983E-22, !IO),
    test_all(22.3954899E22, !IO),

    % test integers
    test_all(-65, !IO),
    test_all(4, !IO),

    % test univ.
    type_to_univ(["hi! I'm a univ!"], Univ),
    test_all(Univ, !IO),

    % test predicates
    test_all(newline, !IO),

    % test tuples
    test_all({1, 'b', "third", {1, 2, 3, 4}}, !IO),

    io.nl(!IO).

:- pred test_other(io::di, io::uo) is cc_multi.

test_other(!IO) :-
    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.

    io.write_string("TESTING OTHER TYPES\n", !IO),
    term.init_var_supply(VarSupply),
    term.create_var(Var, VarSupply, NewVarSupply),
    test_all(Var : var(generic), !IO),
    test_all(VarSupply, !IO),
    test_all(NewVarSupply, !IO),

    % presently, at least, map is an equivalence and an abstract type.
    map.init(Map : map(int, int)),
    test_all(Map, !IO),

    % a no tag type
    test_all(qwerty(4), !IO),
    io.nl(!IO).

:- pred newline(io::di, io::uo) is det.

newline(!IO) :-
    io.nl(!IO).
