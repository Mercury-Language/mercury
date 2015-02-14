% vim: ts=4 sw=4 et ft=mercury
%
% This test case tests the compiler's treatment of static ground terms of
% polymorphic type. It is in this directory because the main reason for
% turning on typeinfo liveness is execution tracing, and because printing
% values of polymorphic types is the easiest way to check whether the typeinfos
% associated with values have been handled correctly.

:- module polymorphic_ground_term.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

:- type poly(T1, T2)
    --->    zero(list(int))
    ;       one(T1)
    ;       two(T2).

main(!IO) :-
    I0 = zero([0]) : poly(int, string),
    I1 = one(42) : poly(int, string),
    I2 = two("fortytwo") : poly(int, string),
    make_maybe_ground_term(I0, L0),
    io.write(L0, !IO),
    io.nl(!IO),
    make_maybe_ground_term(I1, L1),
    io.write(L1, !IO),
    io.nl(!IO),
    make_maybe_ground_term(I2, L2),
    io.write(L2, !IO),
    io.nl(!IO),
    make_exist_ground_term(L),
    io.write(L, !IO),
    io.nl(!IO).

:- pred make_maybe_ground_term(poly(T1, T2)::in, list(poly(T1, T2))::out)
    is det.
:- pragma no_inline(make_maybe_ground_term/2).

make_maybe_ground_term(Item, List) :-
    (
        Item = zero(_),
        List = [zero([1, 2]), zero([3, 4, 5]), zero([6, 7, 8, 9])]
    ;
        Item = one(_),
        List = [Item]
    ;
        Item = two(_),
        List = [Item, Item]
    ).

:- some [T1, T2] pred make_exist_ground_term(list(poly(T1, T2))::out) is det.
:- pragma no_inline(make_exist_ground_term/1).

make_exist_ground_term(List) :-
    % The polymorphism pass of the compiler introduces a cast between List
    % and a new variable ExistQList, and then substitutes ExistQList rather
    % than List for HeadVar__1. This is a bug, but it is not the bug that this
    % test case was created to test for.

    ListA = [zero([1, 2]), zero([3, 4, 5]), zero([6, 7, 8, 9])],
    ListB = [one(1)],
    ListC = [two("two")],
    List = ListA ++ ListB ++ ListC.
