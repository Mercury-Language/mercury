%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Before 15/3/2002, we got incorrect answers
% due to simplistic treatment of type_ctor_descs.

:- module type_ctor_desc_manip.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module type_desc.

main(!IO) :-
    map.init(MapII0),
    map.det_insert(1, 1, MapII0, MapII),
    map.init(MapIF0),
    map.det_insert(1, 1.0, MapIF0, MapIF),
    map.init(MapFI0),
    map.det_insert(1.0, 1, MapFI0, MapFI),

    TypeF1a = type_of(f1a),
    TypeF1b = type_of(f1b),
    TypeF2  = type_of(f2),
    TypeP1  = type_of(p1),
    TypeP2  = type_of(p2),
    TypeT1  = type_of({"one"}),
    TypeT3  = type_of({"one", 2, 3.0}),
    TypeLI  = type_of([1]),
    TypeLF  = type_of([1.0]),
    TypeMFI = type_of(MapFI),
    TypeMIF = type_of(MapIF),
    TypeMII = type_of(MapII),

    TypeDescs = [TypeF1a, TypeF1b, TypeF2, TypeP1, TypeP2,
        TypeT1, TypeT3, TypeLF, TypeLI, TypeMFI, TypeMIF, TypeMII],
    TypeCtorDescs0 = list.map(type_ctor, TypeDescs),
    TypeCtorDescs = list.remove_adjacent_dups(TypeCtorDescs0),

    list.foldl(test_deconstruct, TypeDescs, !IO),
    test_comparisons_among(TypeDescs, TypeDescs, !IO),
    test_comparisons_among(TypeCtorDescs, TypeCtorDescs, !IO).

:- pred test_deconstruct(type_desc::in, io::di, io::uo) is det.

test_deconstruct(Type, !IO) :-
    type_ctor_and_args(Type, TypeCtor, TypeArgs),
    io.write(TypeCtor, !IO),
    io.write_string(" ", !IO),
    io.write(TypeArgs, !IO),
    io.nl(!IO).

:- pred test_comparisons_among(list(T)::in, list(T)::in,
    io::di, io::uo) is det.

test_comparisons_among(L, All, !IO) :-
    (
        L = []
    ;
        L = [H | T],
        test_comparisons_with(H, All, !IO),
        test_comparisons_among(T, All, !IO)
    ).

:- pred test_comparisons_with(T::in, list(T)::in,
    io::di, io::uo) is det.

test_comparisons_with(X, L, !IO) :-
    (
        L = []
    ;
        L = [H | T],
        test_comparison(X, H, !IO),
        test_comparisons_with(X, T, !IO)
    ).

:- pred test_comparison(T::in, T::in, io::di, io::uo) is det.

test_comparison(X, Y, !IO) :-
    compare(R, X, Y),
    (
        R = (<),
        io.write(X, !IO),
        io.write_string(" < ", !IO),
        io.write_line(Y, !IO)
    ;
        R = (=),
        io.write(X, !IO),
        io.write_string(" = ", !IO),
        io.write_line(Y, !IO)
    ;
        R = (>),
        io.write(X, !IO),
        io.write_string(" > ", !IO),
        io.write_line(Y, !IO)
    ).

:- func f1a(float) = float.

f1a(X) = Y :-
    Y = X + 1.0.

:- func f1b(int) = int.

f1b(X) = Y :-
    Y = X + 1.

:- func f2(int, int) = int.

f2(X, Y) = Z :-
    Z = X + Y.

:- pred p1(int::in) is semidet.

p1(X) :-
    X < 42.

:- pred p2(int::in, int::in) is semidet.

p2(X, Y) :-
    X < Y.
