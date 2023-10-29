%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The test cases tests/invalid_make_int/unbound_type_vars_int.m and
% tests/invalid_nodepend/unbound_type_vars.m together test that the compiler
% reports errors for type variables which occur only in type class constraints.
% The former tests errors that the compiler discovers when making .int files,
% while the latter tests all other errors.
%
% The tests cover this issue for typeclass declarations, instance declarations,
% method declarations, and predicate declarations.
%
% Tests of this issue for `:- type' declarations are covered in the
% tests/invalid_nodepend/type_vars.m test case.
%

:- module unbound_type_vars_int.
:- interface.

:- typeclass c1(T1, T2) where [].
:- typeclass c2(T1) <= c1(T1, T2) where [].     % T2 unbound (typeclass decl)
:- typeclass c3(T1) where [].

:- instance c1(int, float) <= c3(T1) where [].  % T1 unbound (instance decl)
