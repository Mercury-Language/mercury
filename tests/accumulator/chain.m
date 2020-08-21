%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests chained calls to a predicate that requires rearrangement.
%

:- module chain.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    io.write_string("pa: ", !IO),
    pa([5, 6, 7], ListA),
    io.write_line(ListA, !IO),
    io.write_string("pb: ", !IO),
    pb([5, 6, 7], ListB),
    io.write_line(ListB, !IO),
    io.write_string("pc: ", !IO),
    pc([1, 3, 5], ValC),
    io.write_line(ValC, !IO),
    io.write_string("pd: ", !IO),
    pd([2, 4, 5], ValD),
    io.write_line(ValD, !IO).

    % append([H], [1], NewH) is static so we can introduce
    % accumulator recursion.
    %
:- pred pa(list(int)::in, list(int)::out) is det.

pa([], []).
pa(X, Y) :-
    X = [H | T],
    pa(T, T0),
    append([H], [1], NewH),
    append(T0, NewH, Y).

    % We have two calls to append with dynamic variables in them
    % that require rearrangement. Hence we can't introduce
    % accumulator recursion.
    %
:- pred pb(list(int)::in, list(int)::out) is det.

pb([], []).
pb(X, Y) :-
    X = [H | T],
    pb(T, T0),
    append([1], T0, NewT),
    append([H], NewT, Y).

    % We have two calls to append with dynamic variables in them
    % that don't require rearrangement. Hence we CAN introduce
    % accumulator recursion.
    %
:- pred pc(list(int)::in, int::out) is det.

pc([], 0).
pc(X, Y) :-
    X = [H | T],
    pc(T, Y0),
    Tmp = Y0 + (2 * H),
    Y = Tmp + H.

    % We CANNOT introduce accumulators because the chain of calls
    % are to different predicates.
    %
:- pred pd(list(int)::in, int::out) is det.

pd([], 0).
pd(X, Y) :-
    X = [H | T],
    pd(T, Y0),
    Tmp = 2 * Y0,
    Y = Tmp + H.
