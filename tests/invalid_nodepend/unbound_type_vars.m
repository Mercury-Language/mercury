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

:- module unbound_type_vars.
:- interface.

:- typeclass c1(T1, T2) where [].
:- typeclass c3(T1) where [].

:- typeclass c3(T1, T2) where [
    pred p1(T1),                            % T2 unbound (method decl,
    mode p1(in) is det,                     % type var from typeclass decl)

    pred p2(T1, T2) <= c1(T1, T3),          % T3 unbound (method decl,
    mode p2(in, in) is det                  % type var from method decl)
].

:- pred p3(T1) <= c1(T1, T2).               % T2 unbound (pred decl)
:- mode p3(in) is det.

:- all [T1, T2] pred p4(T1) <= c1(T1, T2).  % T2 still unbound (pred decl
:- mode p4(in) is det.                      % with universal quantifier)

:- some [T2] pred p5(T1) <= c1(T1, T2).     % T2 still unbound (pred decl
:- mode p5(in) is det.                      % with existential quantifier)

:- implementation.

p3(_).
p4(_).
p5(_).
