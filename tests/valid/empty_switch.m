% This is a regression test. The compiler should eliminate both arms
% of the second switch, since they can't succeed, which means that
% X (or HeadVar__1) is not used in the second switch, which leads
% liveness to say that X dies at the end of the first switch.
% However, some versions of the compiler didn't turn the switch
% with no arms into a fail, which caused the compiler to abort
% with an internal error when trying to flush the switch variable.

:- module empty_switch.

:- interface.

:- pred s(int).
:- mode s(in) is failure.

:- implementation.

s(X) :-
	(
		X = 1
	;
		X = 2
	),
	(
		X = 3
	;
		X = 4
	).
