:- module typeclass_test_9.
:- interface.
:- typeclass foo(T) where [pred p is semidet].
:- typeclass bar(T) where [].
:- typeclass baz(T) where [pred q is semidet].

:- instance foo(int).
:- instance foo(int).
:- instance bar(int).
:- instance baz(int).

:- implementation.
:- import_module std_util.

:- instance foo(int) where [
	pred(p/0) is semidet_succeed
].
:- instance foo(int) where [
	pred(p/0) is semidet_fail
].
:- instance bar(int) where [
	pred(p/0) is semidet_fail
].
:- instance baz(int) where [
	pred(r/0) is semidet_fail,
	pred(q/0) is semidet_fail
].
