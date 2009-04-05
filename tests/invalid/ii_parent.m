% rotd-2009-04-03 and before failed to detect the overlapping
% instance in the child module unless --intermodule-optimization
% was enabled, despite the fact that the necessary information
% for doing is contained in the parent module's private
% interface.
%
:- module ii_parent.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- instance foo(int, float).

:- include_module ii_child.

:- implementation.

:- instance foo(int, float) where [].
