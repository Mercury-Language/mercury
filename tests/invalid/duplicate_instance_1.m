%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module duplicate_instance_1.
:- interface.

:- typeclass foo(T) where [].
:- instance foo(int).

:- implementation.

:- instance foo(int) where [].
