%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the --no-warn-unresolved-polymorphism option.

:- module no_warn_unresolved_poly.
:- interface.

:- func foo = int.

:- implementation.

:- import_module list.

foo = list.length([]).
