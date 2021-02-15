%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_foreign_supertype.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- pragma foreign_type("C", fruit, "int").
:- pragma foreign_type("Java", fruit, "int").
:- pragma foreign_type("C#", fruit, "int").
