:- module exists_fundeps_2.
:- interface.

:- typeclass foo1(S, T) <= (S -> T) where [].
:- typeclass foo2(T) where [].
:- typeclass foo3(T) where [].

:- some [S, T] (pred p(R, S) => (foo1(S, T), foo2(S))) <= foo3(R).
:- mode p(in, out) is det.

:- implementation.

:- type bar ---> bar.
:- instance foo1(bar, bar) where [].
:- instance foo2(bar) where [].

p(_, bar).

