%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the effect of the presence of misplaced external
% declarations in the interface on error messages about missing clauses
% for the named predicates and/or functions.
%

:- module external_in_interface.

:- interface.

:- pred foo(int::in, int::out) is det.
:- func foo(int) = int.
:- pred bar(int::in, int::out) is det.
:- func bar(int) = int.

:- pragma external_pred(foo/2).
:- pragma external_func(bar/1).

:- implementation.

% No definition of pred foo/2, but there is a misplaced external decl above.
% No definition of func foo/1. Should get an error message.
% No definition of pred bar/2. Should get an error message.
% No definition of func bar/1, but there is a misplaced external decl above.
