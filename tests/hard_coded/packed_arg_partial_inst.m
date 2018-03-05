%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the construction of a cell in which a free argument is packed
% together into the same word with other arguments.

:- module packed_arg_partial_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

:- type struct
    --->    struct(int, fruit, bool, fruit, bool, fruit, string).

:- type fruit
    --->    apple
    ;       pear
    ;       orange.

:- pred foo(list(struct)::in, string::out) is det.

foo(Xs, R) :-
    ( if
        X = struct(42, _, yes, orange, _, _, _),
        member(X, Xs)
    then
        R = "found struct(42, _, yes, orange, _, _, _)\n"
    else
        R = "not found struct(42, _, yes, orange, _, _, _)\n"
    ).

:- pred bar(list(struct)::in, string::out) is det.

bar(Xs, R) :-
    ( if
        X = struct(_, _, _, orange, _, pear, _),
        member(X, Xs)
    then
        R = "found struct(_, _, _, orange, _, pear, _)\n"
    else
        R = "not found struct(_, _, _, orange, _, pear, _)\n"
    ).

main(!IO) :-
    StructA = struct(11, apple, yes, orange, no, pear, "abc"),
    StructB = struct(22, apple, yes, orange, no, pear, "abc"),
    StructC = struct(22, apple, no, orange, no, pear, "abc"),
    StructD = struct(42, apple, yes, orange, no, pear, "abc"),
    StructE = struct(42, apple, yes, apple, no, pear, "abc"),
    StructF = struct(42, apple, no, orange, no, pear, "abc"),

    Tests = [
        [StructA],
        [StructB],
        [StructC],
        [StructD],
        [StructE],
        [StructF],
        [StructA, StructB],
        [StructA, StructB, StructC],
        [StructA, StructB, StructC, StructD],
        [StructA, StructB, StructC, StructE],
        [StructA, StructB, StructC, StructF],
        [StructA, StructD, StructE, StructF]
    ],
    list.foldl(test, Tests, !IO).

:- pred test(list(struct)::in, io::di, io::uo) is det.

test(Struct, !IO) :-
    io.write_line(Struct, !IO),
    foo(Struct, FooResult),
    io.write_string(FooResult, !IO),
    bar(Struct, BarResult),
    io.write_string(BarResult, !IO),
    io.nl(!IO).
