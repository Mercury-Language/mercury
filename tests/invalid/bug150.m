% vim: ts=4 sw=4 et ft=mercury
%
% This is a regression test for Mantis bug #150. The problem was that the
% message about the determinism error for car referred to car's determinism
% declaration, even though the function doesn't have one.

:- module bug150.

:- interface.
:- import_module list.

:- func car(list(T)) = T.

:- implementation.

car([H | _T]) = H.
