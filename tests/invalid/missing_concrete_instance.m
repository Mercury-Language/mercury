%
% rotd-2005-05-12 and before did not emit
% an error about the missing concrete instance.
%
:- module missing_concrete_instance.

:- interface.

:- typeclass foo(T) where [].

:- instance foo(int).

:- implementation.

:- instance foo(float).
