%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module breakpoints.a.

:- interface.

:- include_module breakpoints.a.testmod.
:- import_module breakpoints.a.testmod.

:- func afunc = int.

:- implementation.

afunc = 42.
