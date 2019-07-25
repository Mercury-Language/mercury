%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the error message we generate for unrecognized pragmas.
%
%---------------------------------------------------------------------------%

:- module unrecognized_pragma.
:- interface.

:- type foo == int.

:- pragma not_a_pragma([]).
