:- module typeclass_bogus_method.
:- interface.

:- typeclass tc1(T) where [ func foo1(T) = int ].
:- typeclass tc2(T) where [ func foo2(T) = int ].
:- typeclass tc3(T) where [ func foo3(T) = int ].
:- typeclass tc4(T) where [ func foo4(T) = int ].
:- typeclass tc5(T) where [ func foo5(T) = int ].

:- instance tc1(int).
:- instance tc2(int).
:- instance tc3(int).
:- instance tc4(int).

:- implementation.
:- import_module int.

:- func incr(int) = int.
incr(X) = X + 1.

:- instance tc1(int) where [
	func(foo1/1) is incr,
	func(bar/2) is incr
].
:- instance tc2(int) where [
	func(foo2/1) is incr,
	baz(X) = X + 1
].
:- instance tc3(int) where [
	func(foo5/1) is incr
].
:- instance tc4(int) where [
	func(foo4/1) is incr,
	func(foo5/1) is incr
].
