%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test --warn-suspicious-foreign-code
%---------------------------------------------------------------------------%

:- module suspicious_foreign_code.
:- interface.

:- type foo
    --->    foo.

:- implementation.

:- pragma foreign_code("C", "
static int X = MR_ALLOC_ID;
").
