% This should warn about the imports of list and float being
% unnecessary in the interface.
:- module unused_import.

:- interface.

:- import_module list, float.

:- type junk == int.

