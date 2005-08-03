:- module bad_instance.
:- interface.

:- typeclass foo(A, B) where [].
:- instance foo(bar(T), U).

:- implementation.

:- type bar(T) ---> bar(T).

:- instance foo(bar(T), T) where [].

