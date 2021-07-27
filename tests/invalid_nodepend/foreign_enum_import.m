%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_enum_import.
:- interface.

:- import_module bool.

:- func this = bool.

:- implementation.

:- pragma foreign_enum("C", bool.bool/0, [
    yes - "561",
    no  - "75"
]).

this = yes.
