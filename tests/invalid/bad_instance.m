%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_instance2.
:- interface.

:- type bar(T)
    --->    bar(T).

:- typeclass foo(A, B) where [].
:- instance foo(bar(T), U).

:- implementation.

:- instance foo(bar(T), T) where [].
