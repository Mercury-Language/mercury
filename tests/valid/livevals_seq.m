% This is a regression test. The compiler used to abort in this code
% when both frame optimization and value numbering were turned on but
% full jump optimization wasn't.
%
% The reason was that frameopt, in keeping the stack frame of det_insert_fcl,
% would generate the instruction sequence "livevals, assign to succip, goto",
% which violated an invariant expected by value numbering, namely that
% the livevals giving info about the variables live at a goto must
% immediately precede the goto. (Fulljump fixes the problem by eliminating
% the goto.)

:- module livevals_seq.
:- interface.
:- import_module list, tree234.

:- type map(K,V)	==	tree234(K,V).

:- pred det_insert_fcl(map(K,V), list(K),
						list(V), map(K,V)).
:- mode det_insert_fcl(in, in, in, out) is det.

:- implementation.
:- import_module require.

det_insert_fcl(Map0, Ks, Vs, Map) :-
	(
		Ks = [Key | Keys], Vs = [Value | Values]
	->
		det_insert(Map0, Key, Value, Map1),
		det_insert_fcl(Map1, Keys, Values, Map)
	;
		Ks = [], Vs = []
	->
		Map = Map0
	;
		error("lists do not correspond")
	).

:- pred det_insert(map(K,V), K, V, map(K,V)).
:- mode det_insert(in, in, in, out) is det.

:- external(det_insert/4).
