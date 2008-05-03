% Test the --no-warn-unresolved-polymorphism option.
%
:- module no_warn_unresolved_poly.
:- interface.

:- func foo = int.

:- implementation.

:- import_module list.

foo = list.length([]).
