:- module record_syntax_bug_5.
:- interface.

:- type t ---> some[T] functor(a :: T, b :: T).

:- some [T] func foo(t) = T.

:- implementation.

foo(Z) = Z^b.

