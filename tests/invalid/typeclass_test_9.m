:- module typeclass_test_9.
:- interface.
:- import_module std_util.
:- typeclass foo(T) where [pred p is semidet].
:- typeclass bar(T) where [].
:- typeclass baz(T) where [pred q is semidet].
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
