:- module typeclass_test_12.
:- interface.
:- typeclass foo(T) where [pred p is semidet].
:- typeclass bar(T) where [].
:- typeclass baz(T) where [func q = int].
