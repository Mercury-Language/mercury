%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module purity.
:- interface.

% This type is used by the tests below.
:- type foo
    --->    a
    ;       b.

:- implementation.

:- impure pred imp is det.

:- pragma foreign_proc("C",
    imp,
    [will_not_call_mercury],
"
    ;
").
:- pragma foreign_proc("Java",
    imp,
    [will_not_call_mercury],
"
    ;
").
:- pragma foreign_proc("C#",
    imp,
    [will_not_call_mercury],
"
    ;
").
imp.

:- semipure pred semi is semidet.
:- pragma foreign_proc("C",
    semi,
    [promise_semipure, will_not_call_mercury],
"
    SUCCESS_INDICATOR = 0;
").
:- pragma foreign_proc("Java",
    semi,
    [promise_semipure, will_not_call_mercury],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("C#",
    semi,
    [promise_semipure, will_not_call_mercury],
"
    SUCCESS_INDICATOR = false;
").
semi :-
    semidet_fail.

:- pred in(foo::in) is semidet.

in(a).

:- semipure pred semi(foo::in) is semidet.

:- pragma foreign_proc("C",
    semi(X::in),
    [will_not_call_mercury, promise_semipure],
"
    /* X */
    SUCCESS_INDICATOR = 0;
").
:- pragma foreign_proc("Java",
    semi(X::in),
    [will_not_call_mercury, promise_semipure],
"
    /* X */
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("C#",
    semi(X::in),
    [will_not_call_mercury, promise_semipure],
"
    /* X */
    SUCCESS_INDICATOR = false;
").

:- impure pred imp1(foo).
:- mode imp1(in) is semidet.
:- pragma foreign_proc("C",
    imp1(_X::in),
    [will_not_call_mercury],
"
    SUCCESS_INDICATOR = 0;
").

:- pragma foreign_proc("Java",
    imp1(_X::in),
    [will_not_call_mercury],
"
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    imp1(_X::in),
    [will_not_call_mercury],
"
    SUCCESS_INDICATOR = false;
").
imp1(_) :-
    semidet_fail.

%----------------------------------------------------------------
%  Warnings.

:- impure pred w1 is det.

w1.

:- semipure pred w2 is det.

w2.

:- impure pred w3 is semidet.

w3 :-
    semipure semi.

:- pred w4 is det.
:- pragma promise_pure(w4/0).

w4.

:- impure pred w5 is det.
:- pragma promise_pure(w5/0).

w5 :-
    impure imp.

:- semipure pred w6 is semidet.
:- pragma promise_pure(w6/0).

w6 :-
    semipure semi.

%----------------------------------------------------------------
%  Errors.

:- pred e1 is det.

e1 :-
    impure imp.

:- pred e2 is semidet.

e2 :-
    semipure semi.

:- semipure pred e3 is det.

e3 :-
    impure imp.

:- impure pred e4 is det.

e4 :-
    imp.

:- semipure pred e5 is semidet.

e5 :-
    semi.

:- impure pred e6 is semidet.

e6 :-
    in(X),
    impure imp,
    X = a.

:- impure pred e7 is semidet.

e7 :-
    impure imp1(X),
    X = a.

:- type e8
    --->    e8(foo)
    where equality is imp2.

:- impure pred imp2(e8::in, e8::in) is semidet.

:- pragma foreign_proc("C",
    imp2(_X::in, _Y::in),
    [will_not_call_mercury],
"
    SUCCESS_INDICATOR = 0;
").
imp2(_, _) :-
    semidet_fail.

:- type e9
    --->    e9(foo)
    where equality is semi2.

:- semipure pred semi2(e9::in, e9::in) is semidet.

:- pragma foreign_proc("C",
    semi2(_X::in, _Y::in),
    [promise_semipure, will_not_call_mercury],
"
    SUCCESS_INDICATOR = 0;
").
semi2(_, _) :-
    semidet_fail.

:- pred e10 is semidet.

e10 :-
    Goal1 = (pred(X::in) is semidet :- imp1(X)),
    call(Goal1, b).

:- pred e11 is semidet.

e11 :-
    Goal2 = (pred(X::in) is semidet :- semi(X)),
    call(Goal2, b).
