%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_instance.
:- interface.

:- typeclass foo(A, B) where [].
:- instance foo(bad_instance.bar(T), U).

:- implementation.

:- type bar(T)
    --->    bar(T).

:- instance foo(bar(T), T) where [].
