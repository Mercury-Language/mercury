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
    describe_functors_in_du_types(!IO),
    describe_functors_in_polymorphic_types(!IO),
    describe_functors_in_builtin_types(!IO),
    describe_functors_in_other_types(!IO),

    test_construction(!IO).

%---------------------------------------------------------------------------%

:- pred describe_functors_in_du_types(io::di, io::uo) is det.

describe_functors_in_du_types(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

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
    describe_all_functors_of_type(foo, !IO),

    io.nl(!IO).

:- pred describe_functors_in_polymorphic_types(io::di, io::uo) is det.

describe_functors_in_polymorphic_types(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    describe_all_functors_of_type(poly_three(3.33, 4, poly_one(9.11)), !IO),
    describe_all_functors_of_type(poly_two(3) : poly(dummy, int), !IO),
    describe_all_functors_of_type(poly_one([2399.3]) :
        poly(list(float), dummy), !IO),

    io.nl(!IO).

:- pred describe_functors_in_builtin_types(io::di, io::uo) is det.

describe_functors_in_builtin_types(!IO) :-
    io.write_string("TESTING BUILTINS\n", !IO),

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
    describe_all_functors_of_type([1, 2, 3, 4], !IO),

    io.nl(!IO).

:- pred describe_functors_in_other_types(io::di, io::uo) is det.

describe_functors_in_other_types(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),

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
    TypeInfo = type_desc.type_of(T),
    ( if N = construct.num_functors(TypeInfo) then
        io.write_int(N, !IO),
        io.write_string(" functors in this type", !IO),
        io.nl(!IO),
        describe_functors_of_type_loop(TypeInfo, N, !IO),
        io.nl(!IO)
    else
        io.write_string("no functors in this type\n", !IO)
    ),
    io.nl(!IO).

:- pred describe_functors_of_type_loop(type_desc.type_desc::in, int::in,
    io::di, io::uo) is det.

describe_functors_of_type_loop(TypeInfo, N, !IO) :-
    ( if N =< 0 then
        true
    else
        describe_nth_functor_of_type(TypeInfo, N - 1, !IO),
        describe_functors_of_type_loop(TypeInfo, N - 1, !IO)
    ).

:- pred describe_nth_functor_of_type(type_desc.type_desc::in, int::in,
    io::di, io::uo) is det.

describe_nth_functor_of_type(TypeInfo, N, !IO) :-
    io.write_int(N, !IO),
    ( if
        Ordinal = construct.get_functor_ordinal(TypeInfo, N),
        Lex = construct.get_functor_lex(TypeInfo, Ordinal),
        construct.get_functor_with_names(TypeInfo, N, Name, Arity,
            _List, Names)
    then
        io.write_string(" - ", !IO),
        io.write_string(Name, !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO),
        io.write_string(" [", !IO),
        io.write_list(Names, ", ", print_maybe_name, !IO),
        io.write_string("] ", !IO),
        io.write_string("ordinal: ", !IO),
        io.write_int(Ordinal, !IO),
        io.write_string(" lex: ", !IO),
        io.write_int(Lex, !IO),
        io.nl(!IO)
    else
        io.write_string(" failed ", !IO),
        io.nl(!IO)
    ).

:- pred print_maybe_name(maybe(string)::in, io::di, io::uo) is det.

print_maybe_name(MaybeName, !IO) :-
    (
        MaybeName = yes(FieldName),
        io.write_string(FieldName, !IO)
    ;
        MaybeName = no,
        io.write_string("_", !IO)
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

    io.write_string("About to call construct_tuple\n", !IO),
    Tuple = construct.construct_tuple([NumList, EnumList, One, TwoPointOne]),
    io.write_string("Constructed tuple: ", !IO),
    io.write(Tuple, !IO),
    io.nl(!IO).

:- pred test_construct(type_desc.type_desc::in, string::in, int::in,
    list(univ)::in, io::di, io::uo) is det.

test_construct(TypeInfo, FunctorName, Arity, Args, !IO) :-
    find_functor(TypeInfo, FunctorName, Arity, FunctorNumber),
    io.write_string("About to construct ", !IO),
    io.write_string(FunctorName, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.nl(!IO),
    ( if Constructed = construct.construct(TypeInfo, FunctorNumber, Args) then
        io.write_string("Constructed: ", !IO),
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
