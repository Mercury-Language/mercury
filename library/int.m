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
:- mode ((in) < (in)).

:- pred (int =< int).
:- mode ((in) =< (in)).

:- pred (int >= int).
:- mode ((in) >= (in)).

:- pred (int > int).
:- mode ((in) > (in)).

:- pred int__abs(int, int).
:- mode int__abs((in), out).

:- pred int__max(int, int, int).
:- mode int__max((in), (in), out).

:- pred int__min(int, int, int).
:- mode int__min((in), (in), out).

/*

% Undiscriminated unions aren't implemented yet,
% so this won't work.

:- type int__expr 	= 	int__expr_2 + int.
:- type int__expr_2	--->	(int__expr + int__expr)
			;	(int__expr * int__expr)
			;	(int__expr - int__expr)
			;	(int__expr mod int__expr)
			;	(int__expr // int__expr).

:- pred is(int :: out, int__expr :: (in)) is det.

*/

:- type int__simple_expr --->	(int + int)
			;	(int * int)
			;	(int - int)
			;	(int mod int)
			;	(int // int).

:- pred is(int :: out, int__simple_expr :: (in)) is det.

:- implementation.

/*
:- external("NU-Prolog", (is)/2).
:- external("NU-Prolog", (<)/2).
:- external("NU-Prolog", (=<)/2).
:- external("NU-Prolog", (>)/2).
:- external("NU-Prolog", (>=)/2).
*/

int__abs(I0, I) :-
	(
		I0 < 0
	->
		I is 0 - I0
	;
		I = I0
	).

int__max(I0, I1, I) :-
	(
		I0 > I1
	->
		I = I0
	;
		I = I1
	).

int__min(I0, I1, I) :-
	(
		I0 < I1
	->
		I = I0
	;
		I = I1
	).

%-----------------------------------------------------------------------------%
