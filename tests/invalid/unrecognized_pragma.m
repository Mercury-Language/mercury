% Test the error message we generate for unrecognized pragmas.

:- module unrecognized_pragma.
:- interface.

:- type foo == int.

:- pragma not_a_pragma([]).
