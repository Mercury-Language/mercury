:- module abstract_typeclass.

:- interface.

:- typeclass group(T) where [].
:- typeclass scalable(T) where [].

:- typeclass vector(T) <= (group(T), scalable(T)).
:- typeclass vector(T) <= (group(T), scalable(T)) where [
        func dot(T, T) = float
].

