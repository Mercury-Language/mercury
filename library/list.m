:- module list.
:- import_module integer.
:- interface.

:- type list(T) ---> [] ; T.list(T).

:- pred append(list(T), list(T), list(T)).
:- pred member(T, list(T), list(T)).
:- pred member(T, list(T)).
:- pred length(list(T), integer).

