%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This should warn about the imports of list and float being
% unnecessary in the interface.
%
%---------------------------------------------------------------------------%

:- module unused_import.

:- interface.

:- import_module float.
:- import_module list.

:- type junk == int.
