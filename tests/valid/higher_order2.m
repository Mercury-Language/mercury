% Tests currying of arguments in specialized versions.

:- module higher_order2. 

:- import_module list.

:- pred ppp(pred(int, int), list(int)).
:- mode ppp(pred(in, out) is det, in) is det.
ppp(_, []).
ppp(P, [H0|T0]) :-
        call(P, H0, H),
        ppp(P, T0).

:- pred qqq(list(int), pred(int, int)).
:- mode qqq(in, pred(in, out) is det) is det.
qqq(L, F) :-
        ppp((pred(I::in, O::out) is det :- call(F, I, O)), L).

