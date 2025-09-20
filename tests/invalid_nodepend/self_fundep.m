%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test that obviously redundant functional dependencies are reported as an
% error.
%---------------------------------------------------------------------------%

:- module self_fundep.
:- interface.

:- type suppress_warnings ---> suppress_warnings.

:- typeclass foo(A) <= (A -> A) where [].

:- typeclass bar(A, B) <= (A -> A, B) where [].

:- typeclass baaz(A, B) <= (A, B -> B) where [].

:- typeclass quux(A, B) <= (A, B -> B, A) where [].

:- typeclass thud(A, B, C) <= (A, B, C -> A, B, C) where [].
