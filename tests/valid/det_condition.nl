:- module det_condition.

	% Check that we get determinism analysis and code generation
	% right for code which has an if-then-else with a
	% deterministic condition.

:- pred p is det.

p :-
	if true then true else not true.

