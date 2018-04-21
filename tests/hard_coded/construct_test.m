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
    test_discriminated(!IO),
    test_polymorphism(!IO),
    test_builtins(!IO),
    test_other(!IO),
    test_construct(!IO).

%---------------------------------------------------------------------------%

:- pred test_construct(io::di, io::uo) is det.

test_construct(!IO) :-
    % Valid tests.

    % Enumerations:

    test_construct_2(type_desc.type_of(one), "three", 0, [], !IO),

    type_to_univ([1, 2, 3], NumList),
    test_construct_2(type_desc.type_of(apple([])), "apple", 1, [NumList], !IO),

    type_to_univ([one, two, three], EnumList),
    test_construct_2(type_desc.type_of(apple([])), "banana", 1,
        [EnumList], !IO),

    % Discriminated union:
    % (Simple, complicated and complicated constant tags).

    type_to_univ(1, One),
    type_to_univ(2.1, TwoPointOne),

    test_construct_2(type_desc.type_of(wombat),
        "foo", 0, [], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "bar", 1, [One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "bar", 2, [One, One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "qux", 1, [One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "quux", 1, [One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "quuux", 2, [One, One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "wombat", 0, [], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "zoom", 1, [One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "zap", 2, [One, TwoPointOne], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "zip", 2, [One, One], !IO),
    test_construct_2(type_desc.type_of(wombat),
        "zop", 2, [TwoPointOne, TwoPointOne], !IO),

    % No-tag type.
    test_construct_2(type_desc.type_of(qwerty(7)), "qwerty", 1, [One], !IO),

    % Functor with single unboxed argument.
    type_to_univ(unboxed_struct(12, 34), UnboxedStruct),
    test_construct_2(type_desc.type_of(_ : unboxed_arg), "unboxed_arg",
        1, [UnboxedStruct], !IO),

    type_to_univ("goodbye", Bye),

    test_construct_2(type_desc.type_of(poly_four(3, "hello")),
        "poly_one", 1, [One], !IO),
    test_construct_2(type_desc.type_of(poly_four(3, "hello")),
        "poly_two", 1, [Bye], !IO),
    test_construct_2(type_desc.type_of(poly_four(3, "hello")),
        "poly_four", 2, [One, Bye], !IO),
    test_construct_2(type_desc.type_of({1, "two", '3'}), "{}", 3,
        [univ(4), univ("five"), univ('6')], !IO),

    io.write_string("About to call construct_tuple\n", !IO),
    Tuple = construct.construct_tuple([NumList, EnumList, One, TwoPointOne]),
    io.write_string("Constructed tuple: ", !IO),
    io.write(Tuple, !IO),
    io.nl(!IO).

:- pred test_construct_2(type_desc.type_desc::in, string::in, int::in,
    list(univ)::in, io::di, io::uo) is det.

test_construct_2(TypeInfo, FunctorName, Arity, Args, !IO) :-
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
        error("unable to find functor")
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

:- pred test_all(T::in, io::di, io::uo) is det.

test_all(T, !IO) :-
    TypeInfo = type_desc.type_of(T),
    ( if N = construct.num_functors(TypeInfo) then
        io.write_int(N, !IO),
        io.write_string(" functors in this type", !IO),
        io.nl(!IO),
        test_all_functors(TypeInfo, N, !IO),
        io.nl(!IO)
    else
        io.write_string("no functors in this type\n", !IO)
    ),
    io.nl(!IO).

:- pred test_all_functors(type_desc.type_desc::in, int::in, io::di, io::uo)
    is det.

test_all_functors(TypeInfo, N, !IO) :-
    ( if N =< 0 then
        true
    else
        test_nth_functor(TypeInfo, N - 1, !IO),
        test_all_functors(TypeInfo, N - 1, !IO)
    ).

:- pred test_nth_functor(type_desc.type_desc::in, int::in, io::di, io::uo)
    is det.

test_nth_functor(TypeInfo, N, !IO) :-
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

:- pred test_discriminated(io::di, io::uo) is det.

test_discriminated(!IO) :-
    io.write_string("TESTING DISCRIMINATED UNIONS\n", !IO),

    % Test enumerations.
    test_all(two, !IO),
    test_all(one, !IO),
    test_all(three, !IO),

    % Test simple tags.
    test_all(apple([9, 5, 1]), !IO),
    test_all(banana([three, one, two]), !IO),

    % Test complicated tags.
    test_all(zop(3.3, 2.03), !IO),
    test_all(zip(3, 2), !IO),
    test_all(zap(3, -2.111), !IO),

    % Test complicated constants.
    test_all(wombat, !IO),
    test_all(foo, !IO),

    io.nl(!IO).

:- pred test_polymorphism(io::di, io::uo) is det.

test_polymorphism(!IO) :-
    io.write_string("TESTING POLYMORPHISM\n", !IO),
    test_all(poly_three(3.33, 4, poly_one(9.11)), !IO),
    test_all(poly_two(3) : poly(dummy, int), !IO),
    test_all(poly_one([2399.3]) : poly(list(float), dummy), !IO),

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

    % Test unsigned integers.
    test_all(42u, !IO),

    % Test fixed size integers.
    test_all(42i8, !IO),
    test_all(42u8, !IO),
    test_all(42i16, !IO),
    test_all(42u16, !IO),
    test_all(42i32, !IO),
    test_all(42u32, !IO),
    test_all(42i64, !IO),
    test_all(42u64, !IO),

    % Test univ.
    % type_to_univ(["hi! I'm a univ!"], Univ),
    % test_all(Univ, !IO),

    % Test predicates.
    test_all(newline, !IO),

    % Test tuples.
    test_all({1, "a", 'a', {4, 'd'}}, !IO),

    % Test lists.
    test_all([1, 2, 3, 4], !IO),

    io.nl(!IO).

:- pred test_other(io::di, io::uo) is det.

test_other(!IO) :-
    io.write_string("TESTING OTHER TYPES\n", !IO),

    term.init_var_supply(VarSupply : var_supply(int)),
    term.create_var(Var, VarSupply, NewVarSupply),
    test_all(Var, !IO),
    test_all(VarSupply, !IO),
    test_all(NewVarSupply, !IO),

    % Presently, at least, map is an equivalence and an abstract type.
    % Note: testing abstract types is always going to have results
    % that are dependent on the implementation. If someone changes
    % the implementation, the results of this test can change.
    map.init(Map : map(int, int)),
    test_all(Map, !IO),

    % A no tag type.
    test_all(qwerty(4), !IO),

    % A dummy type.
    test_all(dummy, !IO),

    % A functor with a single unboxed argument.
    test_all(unboxed_arg(unboxed_struct(12, 34)), !IO),

    % An existential type.
    ExistVal = 'new xyzzy'(8),
    test_all(ExistVal, !IO).

:- pred newline(io::di, io::uo) is det.

newline(!IO) :-
    io.nl( !IO).
