% XXX The Mercury compiler reports a spurious mode error for this code,
%     due to the lack of support for partially instantiated data
%     structures.
%
% XXX This test, which is extracted from tests/dppd/map_reduce.m,
%     should go in tests/valid.

:- module bug.

:- interface.

:- pred map_reduce is semidet.

:- implementation.

:- import_module list.

map_reduce :-
	map_reduce_add([[1,2],[1,2,3]],[_L1,_L2]).

:- pred map_reduce_add(list(list(int))::in, list(int)::out) is det.
:- external(map_reduce_add/2).
