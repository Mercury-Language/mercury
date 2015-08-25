%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_du_field_2.
:- interface.

:- type abstype.

:- implementation.

:- type abstype
    --->    abstype(hiddentype).

:- type hiddentype
    --->    hiddentype(int, int).

:- pragma foreign_type("Java", abstype, "Object").
