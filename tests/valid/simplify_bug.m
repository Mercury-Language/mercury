% Regression test:
% When compiling this file with `mc -O-1', Mercury 0.6
% got an internal compiler error ("nondet code in det/semidet context").

:- module simplify_bug.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module require, list, std_util.

main -->
	( { nasty([]) } ->
		main, main
	;
		[]
	).

:- pred nasty(list(T)::in) is nondet.

nasty(_) :-
	( semidet_succeed ->
		list__append(X, _, []),
		X \= [],
		e(X)
	;
		semidet_succeed
	).

:- pred e(list(T)::in) is erroneous.
:- external(e/1).
