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

:- pred test_builtins(io::di, io::uo) is cc_multi.
:- pred test_discriminated(io::di, io::uo) is cc_multi.
:- pred test_polymorphism(io::di, io::uo) is cc_multi.
:- pred test_other(io::di, io::uo) is cc_multi.
:- pred newline(io::di, io::uo) is det.
:- pred test_functor(T::in, io::di, io::uo) is det.
:- pred test_functor_number(T::in, io::di, io::uo) is cc_multi.
:- pred test_arg(T::in, io::di, io::uo) is det.
:- pred test_expand(T::in, io::di, io::uo) is det.
:- pred test_expand_du(T::in, io::di, io::uo) is cc_multi.
:- pred test_all(T::in, io::di, io::uo) is cc_multi.

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

main -->
    test_discriminated,
    test_polymorphism,
    test_builtins,
    test_other.

test_discriminated -->
    io.write_string("TESTING DISCRIMINATED UNIONS\n"),

        % test enumerations
    test_all(one), newline,
    test_all(two), newline,
    test_all(three), newline,

        % test simple tags
    test_all(apple([9, 5, 1])), newline,
    test_all(banana([three, one, two])), newline,

        % test complicated tags
    test_all(zop(3.3, 2.03)), newline,
    test_all(zip(3, 2)), newline,
    test_all(zap(3, -2.111)), newline,

        % test complicated constant

    test_all(wombat), newline,
    test_all(foo), newline,

    newline.

test_all(T) -->
    test_functor(T), newline,
    test_functor_number(T), newline,
    test_arg(T), newline,
    test_expand(T), newline,
    test_expand_du(T), newline.

test_functor(T) -->
    { functor(T, canonicalize, Functor, Arity) },
    io.write_string(Functor),
    io.write_string("/"),
    io.write_int(Arity).

test_functor_number(T) -->
    ( { functor_number_cc(T, FunctorNumber, Arity) } ->
        io.write_int(FunctorNumber),
        io.write_string("/"),
        io.write_int(Arity)
    ;
        io.write_string("functor_number_cc failed")
    ).

test_arg(T) -->
    { functor(T, canonicalize, Functor, Arity) },
    (
        { arg(Arity, T, Argument) }
    ->
        { string.format("argument %d of functor %s was:",
            [i(Arity), s(Functor)], Str) },
        io.write_string(Str),
        io.print(Argument)
    ;
        io.write_string("no arguments")
    ).

test_expand(T) -->
    { deconstruct(T, canonicalize, Functor, Arity, Arguments) },
    { string.format("expand: functor %s arity %d arguments ",
        [s(Functor), i(Arity)], Str) },
    io.write_string(Str),
    io.write_string("["),
    io.write_list(Arguments, ", ", io.print),
    io.write_string("]").

test_expand_du(T) -->
    (
        { deconstruct_du(T, include_details_cc, FunctorNumber,
            Arity, Arguments) }
    ->
        { string.format("expand: functor %d arity %d arguments ",
            [i(FunctorNumber), i(Arity)], Str) },
        io.write_string(Str),
        io.write_string("["),
        io.write_list(Arguments, ", ", io.print),
        io.write_string("]")
    ;
        io.write_string("deconstruct_du failed")
    ).

test_polymorphism -->
    io.write_string("TESTING POLYMORPHISM\n"),
    test_all(poly_two(3)), newline,
    test_all(poly_three(3.33, 4, poly_one(9.11))), newline,
    test_all(poly_one([2399.3])), newline,

    newline.

test_builtins -->
    io.write_string("TESTING BUILTINS\n"),

        % test strings
    test_all(""), newline,
    test_all("Hello, world\n"), newline,
    test_all("Foo%sFoo"), newline,
    test_all(""""), newline,    % interesting - prints """ of course

        % test characters
    test_all('a'), newline,
    test_all('&'), newline,

        % test floats
    test_all(3.14159), newline,
    test_all(11.28324983E-22), newline,
    test_all(22.3954899E22), newline,

        % test integers
    test_all(-65), newline,
    test_all(4), newline,

        % test univ.
    { type_to_univ(["hi! I'm a univ!"], Univ) },
    test_all(Univ), newline,

        % test predicates
    test_all(newline), newline,

        % test tuples
    test_all({1, 'b', "third", {1, 2, 3, 4}}), newline,

    newline.

    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.

test_other -->
    io.write_string("TESTING OTHER TYPES\n"),
    { term.init_var_supply(VarSupply) },
    { term.create_var(Var, VarSupply, NewVarSupply) },
    test_all(Var), newline,
    test_all(VarSupply), newline,
    test_all(NewVarSupply), newline,

        % presently, at least, map is an equivalence and
        % an abstract type.
    { map.init(Map) },
    test_all(Map), newline,

        % a no tag type
    test_all(qwerty(4)), newline,

    newline.

newline -->
    io.write_char('\n').
