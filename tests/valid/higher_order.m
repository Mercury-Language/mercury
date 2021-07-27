%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order.

:- interface.

:- import_module list.

:- pred map_list(pred(T1, T2)::in(pred(in, out) is det),
    list(T1)::in, list(T2)::out) is det.

:- implementation.
:- import_module int.

map_list(_P, [], []).
map_list(P, [X | Xs], [Y | Ys]) :-
    call(P, X, Y),
    map_list(P, Xs, Ys).

:- pred plus2(int::in, int::out) is det.

plus2(X, Y) :- Y = X + 2.

:- pred test(list(int)::in, list(int)::out) is det.

test(L1, L2) :-
    map_list(plus2, L1, L2).

:- pred t is semidet.
t.

:- pred f is semidet.
f :- fail.

:- pred test((pred)::out((pred) is semidet)) is nondet.

test(t).
test(f).
