:- module duplicate_instance_1.
:- interface.
:- typeclass foo(T) where [].
:- instance foo(int).
:- implementation.
:- instance foo(int) where [].
