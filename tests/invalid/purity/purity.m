:- module purity.
:- interface.
%----------------------------------------------------------------
%  Needed for later tests.
:- type foo ---> a ; b.
:- implementation.

:- impure pred imp is det.
:- pragma c_code(imp, will_not_call_mercury, ";").

:- semipure pred semi is semidet.
:- pragma c_code(semi, will_not_call_mercury, "SUCCESS_INDICATOR=0;").

:- pred in(foo).
:- mode in(in) is semidet.
in(a).

:- impure pred imp1(foo).
:- mode imp1(in) is semidet.
:- pragma c_code(imp1(_X::in), will_not_call_mercury, "SUCCESS_INDICATOR=0;").



%----------------------------------------------------------------
%  Warnings


:- impure pred w1 is det.

w1.

:- semipure pred w2 is det.

w2.

:- impure pred w3 is semidet.

w3 :- semipure semi.

:- pred w4 is det.
:- pragma promise_pure(w4/0).

w4.

:- impure pred w5 is det.
:- pragma promise_pure(w5/0).

w5 :- impure imp.

:- semipure pred w6 is semidet.
:- pragma promise_pure(w6/0).

w6 :- semipure semi.


%----------------------------------------------------------------
%  Errors

:- pred e1 is det.

e1 :- impure imp.


:- pred e2 is semidet.

e2 :- semipure semi.

:- semipure pred e3 is det.

e3 :- impure imp.

:- impure pred e4 is det.

e4 :- imp.

:- semipure pred e5 is semidet.

e5 :- semi.

:- impure pred e6 is semidet.

e6 :-
	in(X),
	impure imp,
	X = a.

:- impure pred e7 is semidet.

e7 :-
	impure imp1(X),
	X = a.

:- type e8 ---> e8(foo) where equality is imp2.

:- impure pred imp2(e8, e8).
:- mode imp2(in, in) is semidet.

:- pragma c_code(imp2(_X::in, _Y::in), will_not_call_mercury,
	"SUCCESS_INDICATOR=0;").

:- type e9 ---> e9(foo) where equality is semi2.

:- semipure pred semi2(e9, e9).
:- mode semi2(in, in) is semidet.

:- pragma c_code(semi2(_X::in, _Y::in), will_not_call_mercury,
	"SUCCESS_INDICATOR=0;").

:- pred e10 is semidet.

e10 :-
	Goal1 = lambda([] is semidet, imp1(b)),
	call(Goal1).

:- pred e11 is semidet.

e11 :-
	Goal2 = lambda([] is semidet, semi),
	call(Goal2).

:- import_module std_util.
imp.
semi :- semidet_fail.
imp1(_) :- semidet_fail.
imp2(_, _) :- semidet_fail.
semi2(_, _) :- semidet_fail.
