%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% int - some predicates for dealing with machine-size integer numbers.
%
% Main author: conway.
%
%-----------------------------------------------------------------------------%

:- module int.

:- interface.

:- pred <(int, int).
:- mode <(in, in) is semidet.

:- pred =<(int, int).
:- mode =<(in, in) is semidet.

:- pred >(int, int).
:- mode >(in, in) is semidet.

:- pred >=(int, int).
:- mode >=(in, in) is semidet.

:- pred int__abs(int, int).
:- mode int__abs(in, out) is det.

:- pred int__max(int, int, int).
:- mode int__max(in, in, out) is det.

:- pred int__min(int, int, int).
:- mode int__min(in, in, out) is det.

:- pred int__pow(int, int, int).
:- mode int__pow(in, in, out) is det.

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

:- pred is(int :: out, int__simple_expr :: in) is det.

/* NB: calls to `is' get automagically converted into
   calls to builtin_whatever by the (wait for it...) *parser*.
   That is a quick hack to allow us to generate efficient code for it.
*/

:- pred builtin_plus(int, int, int).
:- mode builtin_plus(in, in, out) is det.

:- pred builtin_minus(int, int, int).
:- mode builtin_minus(in, in, out) is det.

:- pred builtin_times(int, int, int).
:- mode builtin_times(in, in, out) is det.

:- pred builtin_div(int, int, int).
:- mode builtin_div(in, in, out) is det.

:- pred builtin_mod(int, int, int).
:- mode builtin_mod(in, in, out) is det.

:- implementation.

:- import_module require.

/*
:- external("NU-Prolog", (is)/2).
:- external("NU-Prolog", (<)/2).
:- external("NU-Prolog", (=<)/2).
:- external("NU-Prolog", (>)/2).
:- external("NU-Prolog", (>=)/2).
*/

int__abs(Num, Abs) :-
	(
		Num < 0
	->
		Abs is 0 - Num
	;
		Abs = Num
	).

int__max(X, Y, Max) :-
	(
		X > Y
	->
		Max = X
	;
		Max = Y
	).

int__min(X, Y, Min) :-
	(
		X < Y
	->
		Min = X
	;
		Min = Y
	).

int__pow(Val, Exp, Result) :-
	( Exp < 0 ->
		error("int__pow: negative exponent")
	;
		int__pow_2(Val, Exp, 1, Result)
	).

:- pred int__pow_2(int, int, int, int).
:- mode int__pow_2(in, in, in, out) is det.

int__pow_2(Val, Exp, Result0, Result) :-
	( Exp = 0 ->
		Result = Result0
	;
		Exp1 is Exp - 1,
		Result1 is Result0 * Val,
		int__pow_2(Val, Exp1, Result1, Result)
	).

%-----------------------------------------------------------------------------%
