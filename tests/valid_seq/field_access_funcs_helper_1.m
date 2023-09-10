%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module field_access_funcs_helper_1.

:- interface.

:- type record.

:- func field(record) = int.

:- implementation.

:- type record
    --->    record(
                field :: int
            ).
