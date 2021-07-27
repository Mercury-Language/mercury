%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests currying of arguments in specialized versions.

:- module higher_order2.

:- interface.

:- import_module list.

:- pred ppp(pred(int, int)::in(pred(in, out) is det), list(int)::in) is det.

:- implementation.

ppp(_, []).
ppp(P, [H0 | T0]) :-
    call(P, H0, _H),
    ppp(P, T0).

:- pred qqq(list(int)::in, pred(int, int)::pred(in, out) is det) is det.

qqq(L, F) :-
    ppp((pred(I::in, O::out) is det :- call(F, I, O)), L).
