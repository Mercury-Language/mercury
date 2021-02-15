%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_abstract.
:- interface.

:- type fruit.

:- type citrus.

:- type lemon =< citrus         % abstract type as supertype
    --->    lemon.

:- type basket
    --->    basket(fruit).

:- type lemons_only =< basket
    --->    basket(lemon).      % cannot determine lemon =< fruit

:- implementation.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.
