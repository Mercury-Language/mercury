:- module bug.

:- interface.

:- pred map_reduce is semidet.

:- implementation.

:- import_module list.

map_reduce :-
	map_reduce_add([[1,2],[1,2,3]],[_L1,_L2]).

:- pred map_reduce_add(list(list(int))::in, list(int)::out) is det.
:- external(map_reduce_add/2).
