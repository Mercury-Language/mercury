:- module cyclic_typeclass_2.

% This test is a cut down version of cyclic_typeclass.  This one doesn't
% cause the compiler to go into an infinite loop, but it still contains
% an error that goes unreported, and may cause an infinite loop when
% compiling other modules that import it.

:- interface.

:- typeclass foo(A) <= bar(A) where [
	func foo(A) = int
].

:- typeclass bar(A) <= foo(A) where [
	func bar(A) = int
].

