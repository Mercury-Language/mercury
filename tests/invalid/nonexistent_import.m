%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nonexistent_import.

:- interface.

:- type foo == int.

:- import_module nonexistent.
