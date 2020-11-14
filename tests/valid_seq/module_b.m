%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module module_b.
:- interface.

:- use_module module_c.
:- type module_b.foo == module_c.foo.
