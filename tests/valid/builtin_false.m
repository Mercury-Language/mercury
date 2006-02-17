% rotd-02-17 emitted a warning about the call to false never succeeding if
% compiling with --intermodule-optimization and --no-fully-strict.
%
:- module builtin_false.

:- interface.

:- pred foo(int::in, int::out) is semidet.

:- implementation.

foo(X, Y) :-
	( X = 3 ->
		Y = 4
	;
		false
	).
