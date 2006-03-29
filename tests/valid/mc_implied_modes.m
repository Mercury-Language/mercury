
% This is a regression test. The propagation solver approach to constraints
% based mode analysis was failing to consider implied modes and therefore
% failing, because the `in' mode of foo implied X wouldn't be produced,
% but the `out' mode of bar implied the call should produce it.

:- module mc_implied_modes.

:- interface.

:- import_module int.

:- pred foo(int::in, int::out) is nondet.

:- implementation.

foo(X, Y) :-
    bar(X, Y).

:- pred bar(int::out, int::out) is multi.

bar(1, 1).
bar(2, 2).
bar(3, 3).
bar(4, 4).

