%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for construct, num_functors, type_of and get_functor.
%
% Original author: trd
%

:- module construct_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.
:- import_module univ.
:- import_module maybe.
:- import_module term.
:- import_module map.
:- import_module string.
:- import_module require.
:- import_module construct.
:- import_module type_desc.

:- type enum
    --->    one
    ;       two
    ;       three.

:- type fruit
    --->    apple(apple_list :: list(int))
    ;       banana(banana_list :: list(enum)).

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
    ;       poly_three(B, poly3_field2 :: A, poly(B, A))
    ;       poly_four(A, B).

:- type no_tag
    --->    qwerty(qwerty_field :: int).

:- type dummy
    --->    dummy.

:- type unboxed_arg
    --->    no
    ;       unboxed_arg(unboxed_struct).

:- type unboxed_struct
    --->    unboxed_struct(int, int).

:- type exist_type
    --->    some [T] xyzzy(f21name :: T).

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("------- TESTING TYPE DESCRIPTIONS -------\n", !IO),

    describe_functors_in_du_types(!IO),
    describe_functors_in_polymorphic_types(!IO),
    describe_functors_in_builtin_types(!IO),
    describe_functors_in_other_types(!IO),

    io.write_string("\n------- TESTING CONSTRUCTION OF TERMS -------\n", !IO),
    test_construction(!IO).

%---------------------------------------------------------------------------%

:- pred describe_functors_in_du_types(io::di, io::uo) is det.

describe_functors_in_du_types(!IO) :-
    io.write_string("\nTESTING DISCRIMINATED UNIONS\n", !IO),

    % Test enumerations.
    describe_all_functors_of_type(two, !IO),
    describe_all_functors_of_type(one, !IO),
    describe_all_functors_of_type(three, !IO),

    % Test simple tags.
    describe_all_functors_of_type(apple([9, 5, 1]), !IO),
    describe_all_functors_of_type(banana([three, one, two]), !IO),

    % Test complicated tags.
    describe_all_functors_of_type(zop(3.3, 2.03), !IO),
    describe_all_functors_of_type(zip(3, 2), !IO),
    describe_all_functors_of_type(zap(3, -2.111), !IO),

    % Test complicated constants.
    describe_all_functors_of_type(wombat, !IO),
    describe_all_functors_of_type(foo, !IO).

:- pred describe_functors_in_polymorphic_types(io::di, io::uo) is det.

describe_functors_in_polymorphic_types(!IO) :-
    io.write_string("\nTESTING POLYMORPHISM\n", !IO),
    describe_all_functors_of_type(poly_three(3.33, 4, poly_one(9.11)), !IO),
    describe_all_functors_of_type(poly_two(3) : poly(dummy, int), !IO),
    describe_all_functors_of_type(poly_one([2399.3]) :
        poly(list(float), dummy), !IO).

:- pred describe_functors_in_builtin_types(io::di, io::uo) is det.

describe_functors_in_builtin_types(!IO) :-
    io.write_string("\nTESTING BUILTINS\n", !IO),

    % Test strings.
    describe_all_functors_of_type("", !IO),
    describe_all_functors_of_type("Hello, world\n", !IO),
    describe_all_functors_of_type("Foo%sFoo", !IO),
    describe_all_functors_of_type("""", !IO),

    % Test characters.
    describe_all_functors_of_type('a', !IO),
    describe_all_functors_of_type('&', !IO),

    % Test floats.
    describe_all_functors_of_type(3.14159, !IO),
    describe_all_functors_of_type(11.28324983E-22, !IO),
    describe_all_functors_of_type(22.3954899E22, !IO),

    % Test integers.
    describe_all_functors_of_type(-65, !IO),
    describe_all_functors_of_type(4, !IO),

    % Test unsigned integers.
    describe_all_functors_of_type(42u, !IO),

    % Test fixed size integers.
    describe_all_functors_of_type(42i8, !IO),
    describe_all_functors_of_type(42u8, !IO),
    describe_all_functors_of_type(42i16, !IO),
    describe_all_functors_of_type(42u16, !IO),
    describe_all_functors_of_type(42i32, !IO),
    describe_all_functors_of_type(42u32, !IO),
    describe_all_functors_of_type(42i64, !IO),
    describe_all_functors_of_type(42u64, !IO),

    % Test univ.
    % type_to_univ(["hi! I'm a univ!"], Univ),
    % describe_all_functors_of_type(Univ, !IO),

    % Test predicates.
    describe_all_functors_of_type(newline, !IO),

    % Test tuples.
    describe_all_functors_of_type({1, "a", 'a', {4, 'd'}}, !IO),

    % Test lists.
    describe_all_functors_of_type([1, 2, 3, 4], !IO).

:- pred describe_functors_in_other_types(io::di, io::uo) is det.

describe_functors_in_other_types(!IO) :-
    io.write_string("\nTESTING OTHER TYPES\n", !IO),

    term.init_var_supply(VarSupply : var_supply(int)),
    term.create_var(Var, VarSupply, NewVarSupply),
    describe_all_functors_of_type(Var, !IO),
    describe_all_functors_of_type(VarSupply, !IO),
    describe_all_functors_of_type(NewVarSupply, !IO),

    % Presently, at least, map is an equivalence and an abstract type.
    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    map.init(Map : map(int, int)),
    describe_all_functors_of_type(Map, !IO),

    % A no tag type.
    describe_all_functors_of_type(qwerty(4), !IO),

    % A dummy type.
    describe_all_functors_of_type(dummy, !IO),

    % A functor with a single unboxed argument.
    describe_all_functors_of_type(unboxed_arg(unboxed_struct(12, 34)), !IO),

    % An existential type.
    ExistVal = 'new xyzzy'(8),
    describe_all_functors_of_type(ExistVal, !IO).

%---------------------------------------------------------------------------%

:- pred describe_all_functors_of_type(T::in, io::di, io::uo) is det.

describe_all_functors_of_type(T, !IO) :-
    io.nl(!IO),
    TypeInfo = type_desc.type_of(T),
    ( if NumFunctors = construct.num_functors(TypeInfo) then
        io.format("#functors in this type = %d\n", [i(NumFunctors)], !IO),
        describe_functors_of_type_loop(TypeInfo, 0, NumFunctors, !IO)
    else
        io.format("#functors in this type = %d\n", [i(0)], !IO)
    ).

:- pred describe_functors_of_type_loop(type_desc.type_desc::in,
    int::in, int::in, io::di, io::uo) is det.

describe_functors_of_type_loop(TypeInfo, Cur, NumFunctors, !IO) :-
    ( if Cur >= NumFunctors then
        true
    else
        describe_nth_functor_of_type(TypeInfo, Cur, !IO),
        describe_functors_of_type_loop(TypeInfo, Cur + 1, NumFunctors, !IO)
    ).

:- pred describe_nth_functor_of_type(type_desc.type_desc::in, int::in,
    io::di, io::uo) is det.

describe_nth_functor_of_type(TypeInfo, N, !IO) :-
    ( if
        % Ordinal = construct.get_functor_ordinal(TypeInfo, N),
        % Lex = construct.get_functor_lex(TypeInfo, Ordinal),
        Lex = construct.get_functor_lex(TypeInfo, N),
        Ordinal = construct.get_functor_ordinal(TypeInfo, Lex),
        construct.get_functor_with_names(TypeInfo, Lex,
            FunctorName, FunctorArity, _ArgTypesList, ArgMaybeNames)
    then
        ( if N = Ordinal then
            ArgNames = list.map(name_or_underscore, ArgMaybeNames),
            ArgNamesDesc = join_list(", ", ArgNames),
            FunctorNA = string.format("%s/%d",
                [s(FunctorName), i(FunctorArity)]),
            io.format("%2d - %-14s lex: %2d [%s]\n",
                [i(N), s(FunctorNA), i(Lex), s(ArgNamesDesc)], !IO)
        else
            io.format("N (%d) != Ordinal (%d)\n", [i(N), i(Ordinal)], !IO)
        )
    else
        io.format("%d failed\n", [i(N)], !IO)
    ).

:- func name_or_underscore(maybe(string)) = string.

name_or_underscore(MaybeName) = Str :-
    (
        MaybeName = yes(Name),
        Str = Name
    ;
        MaybeName = no,
        Str = "_"
    ).

%---------------------------------------------------------------------------%

:- pred newline(io::di, io::uo) is det.

newline(!IO) :-
    io.nl( !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred test_construction(io::di, io::uo) is det.

test_construction(!IO) :-
    % Valid tests.

    % Enumerations:

    test_construct(type_desc.type_of(one),
        "three", 0, [], !IO),

    type_to_univ([1, 2, 3], NumList),
    test_construct(type_desc.type_of(apple([])),
        "apple", 1, [NumList], !IO),

    type_to_univ([one, two, three], EnumList),
    test_construct(type_desc.type_of(apple([])),
        "banana", 1, [EnumList], !IO),

    % Discriminated union:
    % (Simple, complicated and complicated constant tags).

    type_to_univ(1, One),
    type_to_univ(2.1, TwoPointOne),

    test_construct(type_desc.type_of(wombat),
        "foo", 0, [], !IO),
    test_construct(type_desc.type_of(wombat),
        "bar", 1, [One], !IO),
    test_construct(type_desc.type_of(wombat),
        "bar", 2, [One, One], !IO),
    test_construct(type_desc.type_of(wombat),
        "qux", 1, [One], !IO),
    test_construct(type_desc.type_of(wombat),
        "quux", 1, [One], !IO),
    test_construct(type_desc.type_of(wombat),
        "quuux", 2, [One, One], !IO),
    test_construct(type_desc.type_of(wombat),
        "wombat", 0, [], !IO),
    test_construct(type_desc.type_of(wombat),
        "zoom", 1, [One], !IO),
    test_construct(type_desc.type_of(wombat),
        "zap", 2, [One, TwoPointOne], !IO),
    test_construct(type_desc.type_of(wombat),
        "zip", 2, [One, One], !IO),
    test_construct(type_desc.type_of(wombat),
        "zop", 2, [TwoPointOne, TwoPointOne], !IO),

    % No-tag type.
    test_construct(type_desc.type_of(qwerty(7)),
        "qwerty", 1, [One], !IO),

    % Functor with single unboxed argument.
    type_to_univ(unboxed_struct(12, 34), UnboxedStruct),
    test_construct(type_desc.type_of(_ : unboxed_arg),
        "unboxed_arg", 1, [UnboxedStruct], !IO),

    type_to_univ("goodbye", Bye),

    test_construct(type_desc.type_of(poly_four(3, "hello")),
        "poly_one", 1, [One], !IO),
    test_construct(type_desc.type_of(poly_four(3, "hello")),
        "poly_two", 1, [Bye], !IO),
    test_construct(type_desc.type_of(poly_four(3, "hello")),
        "poly_four", 2, [One, Bye], !IO),
    test_construct(type_desc.type_of({1, "two", '3'}),
        "{}", 3, [univ(4), univ("five"), univ('6')], !IO),

    io.nl(!IO),
    io.write_string("About to construct a tuple\n", !IO),
    Tuple = construct.construct_tuple([NumList, EnumList, One, TwoPointOne]),
    io.write(Tuple, !IO),
    io.nl(!IO).

:- pred test_construct(type_desc.type_desc::in, string::in, int::in,
    list(univ)::in, io::di, io::uo) is det.

test_construct(TypeInfo, FunctorName, Arity, Args, !IO) :-
    io.nl(!IO),
    find_functor(TypeInfo, FunctorName, Arity, FunctorNumber),
    io.format("About to construct %s/%d\n", [s(FunctorName), i(Arity)], !IO),
    ( if Constructed = construct.construct(TypeInfo, FunctorNumber, Args) then
        io.print(Constructed, !IO),
        io.nl(!IO)
    else
        io.write_string("Construction failed.\n", !IO)
    ).

:- pred find_functor(type_desc.type_desc::in, string::in, int::in, int::out)
    is det.

find_functor(TypeInfo, Functor, Arity, FunctorNumber) :-
    ( if N = construct.num_functors(TypeInfo) then
        find_functor_loop(TypeInfo, Functor, Arity, N, FunctorNumber)
    else
        error("unable to find number of functors")
    ).

:- pred find_functor_loop(type_desc.type_desc::in, string::in, int::in,
    int::in, int::out) is det.

find_functor_loop(TypeInfo, Functor, Arity, Num, FunctorNumber) :-
    ( if Num < 0 then
        error("unable to find functor")
    else
        ( if construct.get_functor(TypeInfo, Num, Functor, Arity, _) then
            FunctorNumber = Num
        else
            find_functor_loop(TypeInfo, Functor, Arity, Num - 1,
                FunctorNumber)
        )
    ).

%---------------------------------------------------------------------------%
