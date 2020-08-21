%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests the case where the base case contains some goals which
% must be left in the base case of the introduced predicate.
%

:- module dcg.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    io.write_string("p A: ", !IO),
    p([1, 10, 100, 9, 0], ListA, [], ListB),
    io.write_line(ListA, !IO),
    io.write_string("p B: ", !IO),
    io.write_line(ListB, !IO),
    io.write_string("p2 A2: ", !IO),
    p2([1, 10, 100, 9, 0], ListA2, [], ListB2),
    io.write_line(ListA2, !IO),
    io.write_string("p2 B2: ", !IO),
    io.write_line(ListB2, !IO).

    % We can introduce accumulators, but the DCG goals must be left
    % in the base case of the accumulator version of the predicate.
    %
:- pred p(list(T)::in, list(T)::out, list(T)::in, list(T)::out) is det.

p([], []) --> [].
p(X, Y) -->
    { X = [H | T] },
    q(H),
    p(T, T0),
    { list.append(T0, [H], Y) }.

    % We cannot introduce accumulators because the second call to q
    % can't be moved before p2.
    %
:- pred p2(list(T)::in, list(T)::out, list(T)::in, list(T)::out) is det.

p2([], []) --> [].
p2(X, Y) -->
    { X = [H | T] },
    q(H),
    p2(T, T0),
    q(H),
    { list.append(T0, [H], Y) }.

:- pred q(T::in, list(T)::in, list(T)::out) is det.
:- pragma no_inline(q/3).

q(H, DCG0, DCG) :-
    DCG = [H | DCG0].
