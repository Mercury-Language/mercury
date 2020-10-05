%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test RTTI foreign enum types.
%
%---------------------------------------------------------------------------%

:- module foreign_enum_rtti.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

main(!IO) :-
    io.write_string("Checking io.write for foreign enum ...\n", !IO),
    io.write(foo, !IO),
    io.nl(!IO),
    io.write(bar, !IO),
    io.nl(!IO),
    io.write(baz, !IO),
    io.nl(!IO),
    io.write_string("Checking deep copy for foreign enum ...\n", !IO),
    X = [foo, bar, baz],
    copy(X, Y),
    io.write(Y, !IO),
    io.nl(!IO),
    TypeDescForFoo = type_of(foo),
    io.write_string("Number of functors of foo/0: ", !IO),
    NumFunctors = det_num_functors(TypeDescForFoo),
    io.write_int(NumFunctors, !IO),
    io.nl(!IO),
    io.write_string("Checking construct.get_functor for foreign_enum ...\n",
        !IO),
    int.fold_up(test_get_functor(TypeDescForFoo), 0, NumFunctors - 1, !IO),
    io.write_string(
        "Checking construct.get_functor_ordinal for foreign_enum ...\n", !IO),
    list.foldl(check_get_functor_ordinal(TypeDescForFoo), [0, 1, 2], !IO),
    io.write_string(
        "Checking construct.construct for foreign_enum ...\n", !IO),
    list.foldl(check_construct(TypeDescForFoo), [0, 1, 2], !IO),
    io.write_string(
        "Checking deconstruct.deconstruct for foreign_enum ...\n", !IO),
    list.foldl(check_deconstruct, [foo, bar, baz], !IO).

:- pred check_deconstruct(foo::in, io::di, io::uo) is det.

check_deconstruct(Data, !IO) :-
    deconstruct(Data, do_not_allow, Name, Arity, Args),
    io.format("    name = %s\n",  [s(Name)],  !IO),
    io.format("    arity = %d\n", [i(Arity)], !IO),
    (
        Args = [],
        io.write_string("    no args\n", !IO)
    ;
        Args = [_ | _],
        io.write_string("FAILED: unexpected args for foreign enum constant.\n",
            !IO)
    ).

:- pred check_construct(type_desc::in, functor_number_lex::in,
    io::di, io::uo) is det.

check_construct(TypeDesc, LexFunctorNum, !IO) :-
    ( if Univ = construct(TypeDesc, LexFunctorNum, []) then
        io.write_string("    ", !IO),
        io.write(Univ, !IO),
        io.nl(!IO)
    else
        io.write_string("FAILED: construct.construct\n", !IO)
    ).

:- pred check_get_functor_ordinal(type_desc::in, functor_number_lex::in,
        io::di, io::uo) is det.

check_get_functor_ordinal(TypeDesc, Lex, !IO) :-
    ( if Ordinal = get_functor_ordinal(TypeDesc, Lex) then
        io.format("    lex = %d, ordinal = %d\n", [i(Lex), i(Ordinal)], !IO)
    else
        io.write_string("FAILED: get_functor_ordinal\n", !IO)
    ).

:- pred test_get_functor(type_desc::in, functor_number_lex::in,
    io::di, io::uo) is det.

test_get_functor(TypeDesc, LexFunctorNum, !IO) :-
    io.format("functor_number_lex = %d\n", [i(LexFunctorNum)], !IO),
    ( if get_functor(TypeDesc, LexFunctorNum, Name, Arity, ArgTypes) then
        io.format("    name  = %s\n", [s(Name)],  !IO),
        io.format("    arity = %d\n", [i(Arity)], !IO),
        (
            ArgTypes = [],
            io.write_string("    no arguments\n", !IO)
        ;
            ArgTypes = [_ | _],
            io.write_string("FAILED: args for foreign enum functor.\n", !IO)
        )
    else
        io.write_string("FAILED: not a d.u type.\n", !IO)
    ).

:- type foo
    --->    foo
    ;       bar
    ;       baz.

:- pragma foreign_decl("C", "

    #define CONSTANT1 300
    #define CONSTANT2 400
    #define CONSTANT3 500
").

:- pragma foreign_enum("C", foo/0,
    [
        foo - "CONSTANT1",
        bar - "CONSTANT2",
        baz - "CONSTANT3"
    ]).

:- pragma foreign_enum("C#", foo/0,
    [
        foo - "2",
        bar - "4",
        baz - "6"
    ]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
