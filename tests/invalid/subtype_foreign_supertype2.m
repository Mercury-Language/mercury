%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for non-Java grades.
% The .err_exp2 file is for Java grades.

:- module subtype_foreign_supertype2.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- pragma foreign_type("Java", fruit, "int").
