:- module invalid_new.
:- interface.

:- typeclass foo(T) where [].

:- type a ---> some [T] (a(T) => foo(T)).
:- type b ---> some [T] b(T).
:- type c(T) ---> c(T).

:- func na(T) = a <= foo(T).
:- func nb(T) = b.
:- func nc(T) = c(T).

:- implementation.

na(T) = 'new a'(T).
nb(T) = 'new b'(T).

	% Error: using 'new' where not required.
nc(T) = 'new c'(T).

