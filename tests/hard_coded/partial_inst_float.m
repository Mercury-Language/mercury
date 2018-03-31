%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the construction of a cell in which a free argument
% is a double word float. Old compilers fail at this, because
% they implicitly treat the free argument as taking one word.
% This leads them to believe that the address of every argument
% that is preceded by N free double-word floats is N words lower
% than reality.
%

:- module partial_inst_float.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type struct
    --->    struct(int, int, float, int, float, int, int, int).

:- pred foo(list(struct)::in, string::out) is det.

foo(Xs, R) :-
    ( if
        X = struct(42, 43, _, _, _, _, _, _),
        list.member(X, Xs)
    then
        R = "found struct(42, 43, _, _, _, _, _, _)"
    else
        R = "not found struct(42, 43, _, _, _, _, _, _)"
    ).

:- pred bar(list(struct)::in, string::out) is det.

bar(Xs, R) :-
    ( if
        X = struct(42, 43, _, 44, _, 45, 46, _),
        list.member(X, Xs)
    then
        R = "found struct(42, 43, _, 44, _, 45, 46, _)"
    else
        R = "not found struct(42, 43, _, 44, _, 45, 46, _)"
    ).

main(!IO) :-
    StructA = struct(11, 43, 0.0, 44, 9.1, 45, 46, 47),
    StructB = struct(22, 43, 1.1, 44, 9.2, 45, 46, 57),
    StructC = struct(22, 43, 2.2, 44, 9.3, 45, 46, 67),
    StructD = struct(42, 43, 3.3, 44, 9.4, 45, 46, 77),
    StructE = struct(42, 43, 4.4, 44, 9.5, 45, 46, 87),
    StructF = struct(42, 43, 5.5, 44, 9.6, 45, 46, 97),

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
    io.nl(!IO),
    io.write_line(Struct, !IO),
    foo(Struct, FooResult),
    io.write_string(FooResult, !IO),
    io.nl(!IO),
    bar(Struct, BarResult),
    io.write_string(BarResult, !IO),
    io.nl(!IO).
