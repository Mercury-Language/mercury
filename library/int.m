%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% int - some predicates for dealing with machine-size integer numbers.
%
% Main authors: conway, fjh.
%
%-----------------------------------------------------------------------------%

:- module int.

:- interface.

% '<' and '>' are currently defined in mercury_builtin.nl, since they need to
% be because they are used in the implementation of compare/3.
% 
% :- pred <(int, int).
% :- mode <(in, in) is semidet.
%
% :- pred >(int, int).
% :- mode >(in, in) is semidet.

:- pred =<(int, int).
:- mode =<(in, in) is semidet.

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
	% int__pow(X, Y, Z): Z is X raised to the Yth power
	% Y must not be negative.

:- pred int__log2(int, int).
:- mode int__log2(in, out) is det.
	% int__log2(X, N): N is the least integer such that 2 to the power N
	% is greater than or equal to X.  X must be positive.

/*

% If Mercury had undiscriminated unions (like the NU-Prolog type checker)
% we could handle is/2 better:

:- type int__expr 	= 	int__expr_2 + int.
:- type int__expr_2	--->	(int__expr + int__expr)
			;	(int__expr * int__expr)
			;	(int__expr - int__expr)
			;	(int__expr mod int__expr)
			;	(int__expr // int__expr).

:- pred is(int :: out, int__expr :: (in)) is det.

*/

% Instead, we use some hacks in the parser.

:- type int__simple_expr --->	(int + int)
			;	(int * int)
			;	(int - int)
			;	(int mod int)	% modulus
			;	(int // int)	% integer division
			;	(int << int)	% left shift
			;	(int >> int)	% right shift
			;	(int /\ int)	% bitwise and
			;	(int \/ int)	% bitwise or
			;	(int ^ int)	% bitwise exclusive or
			;	(\ int).	% bitwise complement

:- pred is(int :: out, int__simple_expr :: in) is det.

/* NB: calls to `is' get automagically converted into
   calls to builtin_whatever by the parser.
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

:- pred builtin_left_shift(int, int, int).
:- mode builtin_left_shift(in, in, out) is det.

:- pred builtin_right_shift(int, int, int).
:- mode builtin_right_shift(in, in, out) is det.

:- pred builtin_bit_or(int, int, int).
:- mode builtin_bit_or(in, in, out) is det.

:- pred builtin_bit_and(int, int, int).
:- mode builtin_bit_and(in, in, out) is det.

:- pred builtin_bit_xor(int, int, int).
:- mode builtin_bit_xor(in, in, out) is det.

:- pred builtin_bit_neg(int, int).
:- mode builtin_bit_neg(in, out) is det.

:- implementation.

:- import_module require.

:- external((is)/2).
/*
Z is (X + Y)	:-	builtin_plus(X, Y, Z).
Z is (X * Y)	:-	builtin_times(X, Y, Z).
Z is (X - Y)	:-	builtin_minus(X, Y, Z).
Z is (X mod Y)	:-	builtin_mod(X, Y, Z).
Z is (X // Y)	:-	builtin_div(X, Y, Z).
Z is (X << Y)	:-	builtin_left_shift(X, Y, Z).
Z is (X >> Y)	:-	builtin_right_shift(X, Y, Z).
Z is (X /\ Y)	:-	builtin_bit_and(X, Y, Z).
Z is (X \/ Y)	:-	builtin_bit_or(X, Y, Z).
Z is (X ^ Y)	:-	builtin_bit_xor(X, Y, Z).
Z is (\ X)	:-	builtin_bit_neg(X, Z).
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

int__log2(X, N) :-
	( X =< 0 ->
		error("int__log2: cannot take log of a non-positive number")
	;
		int__log2_2(X, 0, N)
	).

:- pred int__log2_2(int, int, int).
:- mode int__log2_2(in, in, out) is det.

int__log2_2(X, N0, N) :-
	( X = 1 ->
		N = N0
	;
		X1 is X + 1,
		X2 is X1 // 2,
		N1 is N0 + 1,
		int__log2_2(X2, N1, N)
	).

%-----------------------------------------------------------------------------%
