%
% This is a regression test -- previous versions of the Mercury compiler
% (Oct 98) got a `var not found' error for this test case, due to
% mode analysis producing incorrect instmap delta annotations
% for the complicated unification in the implied mode in the
% first call to map__from_assoc_list in next/3.
%

:- module var_not_found.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, map, std_util.
:- import_module array, bool, float, int, map, require, std_util, string.

main -->
	{ map__from_assoc_list([int(1) - int(2)], Map) },
	( { next(Map, Map, Next) } ->
		write(Next), nl
	;
		write_string("failed (as we should)\n")
	).

:- type data
	--->	int(int)
	;	flt(float)
	;	str(string)
	;	array(map(data, data))
	;	void
	.

:- pred next(map(data, data), map(data, data), map(data, data)).
:- mode next(in, in, out) is semidet.

next(Thing, Array, Next) :-
	map__to_assoc_list(Thing, [int(0) - Key, int(1) - _]),
	map__to_assoc_list(Array, List),
	next_pair(List, Key, NewKey - NewValue),
	map__from_assoc_list([int(0) - NewKey, int(1) - NewValue], Next).

:- pred next_pair(list(pair(data)), data, pair(data)).
:- mode next_pair(in, in, out) is semidet.

next_pair([Pair0|Pairs], Key, Pair) :-
	( Pair0 = Key - _ ->
		Pairs = [Pair|_]
	;
		next_pair(Pairs, Key, Pair)
	).

