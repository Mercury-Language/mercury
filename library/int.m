%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% int - some predicates for dealing with machine-size integer numbers.
%
% Main author: conway.
%
%-----------------------------------------------------------------------------%

:- module int.

:- interface.

:- pred (int < int).
:- mode (input < input).

:- pred (int =< int).
:- mode (input =< input).

:- pred (int >= int).
:- mode (input >= input).

:- pred (int > int).
:- mode (input > input).

:- pred int__abs(int, int).
:- mode int__abs(input, output).

:- pred int__max(int, int, int).
:- mode int__max(input, input, output).

:- pred int__min(int, int, int).
:- mode int__min(intput, input, output).

/*

% Undiscriminated unions aren't implemented yet,
% so this won't work.

:- type int__expr 	= 	int__expr_2 + int.
:- type int__expr_2	--->	(int__expr + int__expr)
			;	(int__expr * int__expr)
			;	(int__expr - int__expr)
			;	(int__expr mod int__expr)
			;	(int__expr // int__expr).

:- pred is(int :: output, int__expr :: input) is det.

*/

:- type int__simple_expr --->	(int + int)
			;	(int * int)
			;	(int - int)
			;	(int mod int)
			;	(int // int).

:- pred is(int :: output, int__simple_expr :: input) is det.

:- implementation.

/*
:- external("NU-Prolog", (is)/2).
:- external("NU-Prolog", (<)/2).
:- external("NU-Prolog", (=<)/2).
:- external("NU-Prolog", (>)/2).
:- external("NU-Prolog", (>=)/2).
*/

int__abs(I0, I) :-
	(if
		I0 < 0
	then
		I is 0 - I0
	else
		I = I0
	).

int__max(I0, I1, I) :-
	(if
		I0 > I1
	then
		I = I0
	else
		I = I1
	).

int__min(I0, I1, I) :-
	(if
		I0 < I1
	then
		I = I0
	else
		I = I1
	).

%-----------------------------------------------------------------------------%
