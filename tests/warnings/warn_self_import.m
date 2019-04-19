%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that the warning about a module importing itself prints the context
% of the import.
%
%---------------------------------------------------------------------------%

:- module warn_self_import.
:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

:- import_module warn_self_import.

foo(X, X).
