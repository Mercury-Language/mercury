%
% The Mercury compiler of Monday November 25, 1996 (13:00) produced
% incorrect code for this. 
%
% Suspected liveness bug, basically the compiler picks the value of 
% the result variable (Num) up from the wrong register - containing
% Test not NumLast as it should.
%
% The switch must be in the condition of the if-then-else, and the 
% test unification must follow it.
%

:- module liveness2.

:- interface.

:- import_module int, bool, io, std_util.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred gather(bool, int, int, int).
:- mode gather(in, in, in, out) is det.

:- implementation.

main -->
	{ gather(yes, 0, 7, Num) },
	(
		{ Num = 0 }
	->
		io__write_string("Num is correct: ")	
	;
		io__write_string("Num is incorrect: ")	
	),
	io__write_int(Num),
	io__write_string("\n").

gather(P, NumLast, Test, Num) :-
	(
		( P = yes, Test1 = 1, NumThis = 10 ;
		P = no, Test1 = 2, NumThis = 20),
		Test = Test1
	->
		Num = NumThis
	;
		Num = NumLast
	).

