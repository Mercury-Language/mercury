%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module map_impl.

:- interface.

:- import_module list.

:- pred map_rev(list(list(T))::in, list(list(T))::out) is det.

:- pred map_reduce_add(list(list(int))::in, list(int)::out) is det.

:- implementation.

:- import_module int.

map_reduce_add(L, R) :-
    mymap(reduce_add, L, R).

map_rev(L, R) :-
    mymap(rev, L, R).

:- pred mymap(pred(T, U), list(T), list(U)).
:- mode mymap(pred(in, out) is det, in, out) is det.

mymap(_P, [], []).
mymap(P, [H | T], [PH | PT]) :-
    call(P, H, PH),
    mymap(P, T, PT).

:- pred reduce_add(list(int)::in, int::out) is det.

reduce_add(List, Res) :-
    reduce(add, 0, List, Res).

:- pred add(int::in, int::in, int::out) is det.

add(X, Y, Z) :-
    Z = X + Y.

:- pred reduce(pred(U, T, T), T, list(U), T).
:- mode reduce(pred(in, in, out) is det, in, in, out) is det.

reduce(_Func, Base, [], Base).
reduce(Func, Base, [H | T], Res) :-
    reduce(Func, Base, T, TRes),
    call(Func, H, TRes, Res).

:- pred rev(list(T)::in, list(T)::out) is det.

rev(L, R) :-
    rev(L, [], R).

:- pred rev(list(T)::in, list(T)::in, list(T)::out) is det.

rev([], L, L).
rev([H | T], A, R) :-
    rev(T, [H | A], R).
