% Check that we handle repeated type class constraints on pred declarations

% We check this twice in this module: on the predicate 'p/1', and on the
% class method 'foo/2' which repeats the 'fooable/1' class constraint that 
% gets introduced by the compiler because it is a class method.

:- module repeated_class_constraint.

:- interface.

:- typeclass fooable(T) where
[
    pred foo(T,T) <= fooable(T),
    mode foo(in,out) is det
].

:- pred p(T) <= (fooable(T), fooable(T)).
:- mode p(in) is det.

:- implementation.

p(_).

