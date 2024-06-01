%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp file is for non-trailing grades.
% The .err_exp2 file is for trailing grades.
%---------------------------------------------------------------------------%

:- module trailed_mutable.

:- interface.

:- type foo.

:- implementation.

:- type foo == int.

:- mutable(global, int, 42, ground, [trailed]).
:- mutable(global2, int, 42.3, ground, [untrailed]).
