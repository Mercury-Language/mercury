:- module type_vars.
:- interface.

:- typeclass c(T) where [].

:- type t1 ---> some [T1, T2] foo(T1) => c(T2).
:- type t2 ---> some [T1, T2] bar(T1).
:- type t3(T1) ---> some [T1] bar(T1).
