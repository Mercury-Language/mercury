%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_foreign.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- pragma foreign_type("C", citrus, "int").
:- pragma foreign_type("Java", citrus, "int").
:- pragma foreign_type("C#", citrus, "int").
