%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test; version 0.6 or thereabouts got
% an internal error in arg_modes.m, due to a bug in polymorphism.m
% for predicates with no modes.

:- module pred_with_no_modes.

:- implementation.

:- pred foo(((func int) = T2), T2).

foo(X, Y) :-
    Y = apply(X, 1).
