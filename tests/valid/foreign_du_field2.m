:- module foreign_du_field2.
:- interface.

:- type abstype.

:- implementation.

:- type abstype
    --->    abstype(hiddentype).

:- type hiddentype
    --->    hiddentype(int, int).

:- pragma foreign_type("Java", abstype, "Object").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
