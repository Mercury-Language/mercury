%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a copy of bug150.m, which is compiled with a --color-scheme option
% whose argument does not specify a color for a role.
%

:- module bug150_partial_color.

:- interface.
:- import_module list.

:- func car(list(T)) = T.

:- implementation.

car([H | _T]) = H.
