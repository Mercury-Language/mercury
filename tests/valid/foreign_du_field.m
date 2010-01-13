% Regression test.  When a type is written out to the implementation section of
% an interface file (because it has an alternative foreign type definition),
% the types of its fields must not be undefined.

:- module foreign_du_field.
:- interface.

:- import_module foreign_du_field2.

:- type wrap
    --->    wrap(abstype).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
