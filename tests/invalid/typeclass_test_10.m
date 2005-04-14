:- module typeclass_test_10.
:- interface.
:- typeclass bar(T) where [].
:- typeclass baz(T) where [pred q(T::in) is semidet].

:- instance bar(int).
:- instance baz(int).

:- implementation.
:- import_module std_util.

:- instance bar(int) where [
	pred(p/0) is semidet_fail
].
:- instance baz(int) where [
	pred(r/0) is semidet_fail,
	q(_) :- semidet_fail
].
