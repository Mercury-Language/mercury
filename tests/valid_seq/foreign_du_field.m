%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. When a type is written out to the implementation section
% of an interface file (because it has an alternative foreign type definition),
% the types of its fields must not be undefined.
%

:- module foreign_du_field.
:- interface.

:- import_module foreign_du_field_2.

:- type wrap
    --->    wrap(abstype).
