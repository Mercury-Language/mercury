%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Assertion in the interface refers to a predicate which is imported
% in the implementation section of the module.
%

:- module assert_in_interface.
:- interface.

:- promise all [X, Y] list.last(X, Y).

:- implementation.
:- import_module list.
