%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% int - some predicates for dealing with machine-size integer numbers.
%
% Main authors: conway, fjh.
% Stability: medium.
%
%-----------------------------------------------------------------------------%

:- module int.

:- interface.

:- import_module float.

% '<' and '>' are currently defined in mercury_builtin.m, since they need to
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

:- pred int__to_float(int, float) is det.
:- mode int__to_float(in, out) is det.

:- pred int__pow(int, int, int).
:- mode int__pow(in, in, out) is det.
	% int__pow(X, Y, Z): Z is X raised to the Yth power
	% Y must not be negative.

:- pred int__log2(int, int).
:- mode int__log2(in, out) is det.
	% int__log2(X, N): N is the least integer such that 2 to the power N
	% is greater than or equal to X.  X must be positive.

	% addition
:- func int + int = int.
:- mode in  + in  = uo  is det.

	% multiplication
:- func int * int = int.
:- mode in  * in  = uo  is det.

	% subtraction
:- func int - int = int.
:- mode in  - in  = uo  is det.

	% modulus (or is it remainder?)
:- func int mod int = int.
:- mode in  mod in  = uo  is det.

	% truncating integer division
	% should round toward zero
	% (if it doesn't, file a bug report)
:- func int // int = int.
:- mode in  // in  = uo  is det.

	% left shift
:- func int << int = int.
:- mode in  << in  = uo  is det.

	% (arithmetic) right shift
:- func int >> int = int.
:- mode in  >> in  = uo  is det.

	% bitwise and
:- func int /\ int = int.
:- mode in  /\ in  = uo  is det.

	% bitwise or
:- func int \/ int = int.
:- mode in  \/ in  = uo  is det.

	% bitwise exclusive or (xor)
:- func int ^ int = int.
:- mode in  ^ in  = uo  is det.

	% bitwise complement
:- func \ int = int.
:- mode \ in  = uo  is det.

	% is/2, for backwards compatiblity with Prolog (and with
	% early implementations of Mercury)
:- pred is(T, T) is det.
:- mode is(uo, di) is det.
:- mode is(out, in) is det.

/* The following routines are builtins that the compiler knows about. */

:- pred builtin_plus(int, int, int).
:- mode builtin_plus(in, in, uo) is det.
:- mode builtin_plus(in, in, uo) is det.

:- pred builtin_unary_plus(int, int).
:- mode builtin_unary_plus(in, uo) is det.

:- pred builtin_minus(int, int, int).
:- mode builtin_minus(in, in, uo) is det.

:- pred builtin_unary_minus(int, int).
:- mode builtin_unary_minus(in, uo) is det.

:- pred builtin_times(int, int, int).
:- mode builtin_times(in, in, uo) is det.

:- pred builtin_div(int, int, int).
:- mode builtin_div(in, in, uo) is det.

:- pred builtin_mod(int, int, int).
:- mode builtin_mod(in, in, uo) is det.

:- pred builtin_left_shift(int, int, int).
:- mode builtin_left_shift(in, in, uo) is det.

:- pred builtin_right_shift(int, int, int).
:- mode builtin_right_shift(in, in, uo) is det.

:- pred builtin_bit_or(int, int, int).
:- mode builtin_bit_or(in, in, uo) is det.

:- pred builtin_bit_and(int, int, int).
:- mode builtin_bit_and(in, in, uo) is det.

:- pred builtin_bit_xor(int, int, int).
:- mode builtin_bit_xor(in, in, uo) is det.

:- pred builtin_bit_neg(int, int).
:- mode builtin_bit_neg(in, uo) is det.

:- implementation.

:- import_module require.

:- pragma(c_code, is(X::uo, Y::di),  "X = Y;").
:- pragma(c_code, is(X::out, Y::in), "X = Y;").

/*
(X + Y) = Z	:-	builtin_plus(X, Y, Z).
(X * Y) = Z	:-	builtin_times(X, Y, Z).
(X - Y) = Z	:-	builtin_minus(X, Y, Z).
(X mod Y) = Z	:-	builtin_mod(X, Y, Z).
(X // Y) = Z	:-	builtin_div(X, Y, Z).
(X << Y) = Z	:-	builtin_left_shift(X, Y, Z).
(X >> Y) = Z	:-	builtin_right_shift(X, Y, Z).
(X /\ Y) = Z	:-	builtin_bit_and(X, Y, Z).
(X \/ Y) = Z	:-	builtin_bit_or(X, Y, Z).
(X ^ Y) = Z	:-	builtin_bit_xor(X, Y, Z).
(\ X) = Z	:-	builtin_bit_neg(X, Z).
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
	( X > 0 ->
		int__log2_2(X, 0, N)
	;
		error("int__log2: cannot take log of a non-positive number")
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

% builtin_unary_minus and builtin_unary_plus aren't actually builtin yet,
% although they should be... still, cross-module inlining would do the trick

builtin_unary_minus(X, Y) :-
	Y is 0 - X.

builtin_unary_plus(X, Y) :-
	Y is 0 + X.

%-----------------------------------------------------------------------------%

/*
:- pred int__to_float(int, float) is det.
:- mode int__to_float(in, out) is det.
*/
:- pragma(c_code, int__to_float(IntVal::in, FloatVal::out), "
	FloatVal = IntVal;
").

%-----------------------------------------------------------------------------%
