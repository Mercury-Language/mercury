%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_vars.
:- interface.

:- typeclass c(T) where [].

:- type t1 --->     some [T1, T2] foo(T1) => c(T2).
                                                % T2 doesn't occur in ctor args

% More tests in invalid_make_int/type_vars_int.m.
