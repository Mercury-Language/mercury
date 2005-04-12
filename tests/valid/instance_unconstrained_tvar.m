:- module instance_unconstrained_tvar.

:- interface.

:- import_module list.

:- typeclass c1(T) where [ ].
:- typeclass c2(T) <= c1(T) where [ ].

:- instance c1(list(T)).
:- instance c2(list(T)).

:- pred p is det.

:- implementation.

:- instance c1(list(T)) where [].
:- instance c2(list(T)) where [].

	% The bug that this test case is checking for is at the creation of
	% the typeclass info for this call, when creating the type-info the
	% the int (1), the type bindings were not being applied properly.
p :- q([1]).

:- pred q(T) <= c2(T).
:- mode q(in) is det.

q(_).
