%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Versions of the compiler before 2015 oct 29 couldn't handle having
% the foreign_type definition of mytype occurring *before* mytype's
% abstract type definition.

:- module bug318.

% :- interface.
% :- type mytype. % used to be ok

:- implementation.
:- pragma foreign_type("C", mytype, "void *").
:- pragma foreign_type("Java", mytype, "java.lang.Object").

:- interface.
:- type mytype. % used to lead to an error
