%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that any construction unifications get handled properly.
%

:- module construct_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    io.write_string("p1: ", !IO),
    p([1, 10, 100], ListA),
    io.write_line(ListA, !IO),
    io.write_string("pb: ", !IO),
    p2([5, 6, 7], ListB),
    io.write_line(ListB, !IO).

:- pred p(list(T)::in, list(T)::out) is det.

    % Direct construction unification.
    %
p([], []).
p(X, Y) :-
    X = [H | T],
    p(T, T0),
    Y = [H | T0].

    % Hide the construction by introducing some intermediate variables.
    %
    % This will introduce accumulators provided
    % --optimize-constructor-last-call is turned on.
    %
:- pred p2(list(int)::in, list(int)::out) is det.

p2([], []).
p2(X, Y) :-
    X = [H | T],
    p2(T, T0),
    append(T0, [1], T1),
    Y = [H | T1].
