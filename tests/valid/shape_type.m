:- module shape_type.

:- interface.
:- import_module list.

:- type bar ---> bar(int).

:- type foo_info ---> foo_info(list(bar)).

:- pred foo_info_init(foo_info).
:- mode foo_info_init(out) is det.

:- implementation.

foo_info_init(PredInfo) :-
	list__append([], [], EmptyList),
	PredInfo = foo_info(EmptyList).
