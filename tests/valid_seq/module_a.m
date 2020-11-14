%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module module_a.
:- interface.

:- use_module io.
:- import_module module_e.
:- use_module module_b.

:- type module_a.foo == module_b.foo.
