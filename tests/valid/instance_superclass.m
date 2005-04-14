:- module instance_superclass.

% This is a regression test for the case where there is a superclass
% relationship, and the instance declaration has a type variable in the
% instance type.

:- interface.

:- import_module list.

:- typeclass c1(T) where [ ].
:- typeclass c2(T) <= c1(T) where [ ].

:- instance c1(list(T)).
:- instance c2(list(T)).
:- pred p(T::in) is det.

:- implementation.

:- instance c1(list(T)) where [].
:- instance c2(list(T)) where [].

	% The bug that this test case is checking for is at the creation of
	% the typeclass info for this call: if the substitution from "T" in
	% the typeclass decl to "list(T)" in the instance is applied
	% recursively, an infinite loop results.
p(X) :- q([X]).

:- pred q(T) <= c2(T).
:- mode q(in) is det.

q(_).
