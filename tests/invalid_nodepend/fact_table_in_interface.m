%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the effect of the presence of misplaced fact_table
% pragmas in the interface on error messages about missing clauses
% for the named predicates and/or functions.
%

:- module fact_table_in_interface.

:- interface.

:- pred foo(int::in, int::out) is det.
:- func foo(int) = int.
:- pred bar(int::in, int::out) is det.
:- func bar(int) = int.
:- pred baz(int::in, int::out) is det.
:- func baz(int, int) = int.

:- pragma fact_table(pred(foo/2), "nonexistent_foo").
:- pragma fact_table(func(bar/1), "nonexistent_bar").
:- pragma fact_table(baz/2,       "nonexistent_baz").   % Ambiguity error.

:- implementation.

% No definition of pred foo/2, but there is a misplaced fact_table above.
% No definition of func foo/1. Should get an error message.
% No definition of pred bar/2. Should get an error message.
% No definition of func bar/1, but there is a misplaced fact_table above.
% No definition of pred baz/2. Should get an error message.
% No definition of func baz/2. Should get an error message.
