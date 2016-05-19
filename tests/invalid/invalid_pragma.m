% Test error message for misformed pragam declaration.

:- module invalid_pragma.
:- interface.
:- type foo == int.
:- pragma 1235.
