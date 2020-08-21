%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that any deconstruction unifications get handled properly.
%

:- module deconstruct_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

:- type wrapper
    --->    wrapper(int, list(int)).

main(!IO) :-
    io.write_string("p1: ", !IO),
    ( if p([1, 10, 100], ListA) then
        io.write_line(ListA, !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    io.write_string("pb: ", !IO),
    ( if p2([5, 6, 7], ListB) then
        io.write_line(ListB, !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred p(list(int)::in, list(int)::out) is semidet.

    % Direct deconstruction unification.
    %
p([], [1000]).
p(X, Y) :-
    X = [H | T],
    p(T, T0),
    T0 = [Ht | Tt],
    append([Ht], [H], NewH),
    append(NewH, Tt, Y).

    % Using a deconstruction as a wrapper.
    % Should introduce accumlator recursion, doesn't.
    %
:- pred p2(list(int)::in, wrapper::out) is semidet.

p2([], wrapper(0, [])).
p2(X, W) :-
    X = [H | T],
    p2(T, W0),
    W0 = wrapper(L0, R0),
    L = L0 + 1,
    append(R0, [H], R),
    W = wrapper(L, R).
