	% assertion in the interface refers to a predicate which is
	% imported in the implementation of the module.
:- module assert_in_interface.
:- interface.
:- promise all [X, Y] list__last(X, Y).

:- implementation.
:- import_module list.
