%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: rational.m
% authors: aet Apr 1998. (with plagiarism from rat.m)
% 
% Implements a rational number type and a set of basic operations on 
% rational numbers.

:- module rational.

:- interface.

:- import_module integer.

:- type rational.

:- pred rational:'<'(rational, rational).
:- mode rational:'<'(in, in) is semidet.

:- pred rational:'>'(rational, rational).
:- mode rational:'>'(in, in) is semidet.

:- pred rational:'=<'(rational, rational).
:- mode rational:'=<'(in, in) is semidet.

:- pred rational:'>='(rational, rational).
:- mode rational:'>='(in, in) is semidet.


:- func rational(int, int) = rational.

:- func rational_from_integers(integer, integer) = rational.

% :- func float(rational) = float.

:- func rational:'+'(rational) = rational.

:- func rational:'-'(rational) = rational.

:- func rational:'+'(rational, rational) = rational.

:- func rational:'-'(rational, rational) = rational.

:- func rational:'*'(rational, rational) = rational.

:- func rational:'/'(rational, rational) = rational.

:- func rational__numer(rational) = integer.

:- func rational__denom(rational) = integer.

:- func rational__abs(rational) = rational.

:- func one = rational.

:- func zero = rational.


:- implementation.

:- import_module require.

	% The normal form of a rational number has the following
	% properties:
	%	- numerator and denominator have no common factors.
	%	- denominator is positive.
	%	- denominator is not zero.
	%	- if numerator is zero, then denominator is one.
	%
	% These invariants must be preserved by any rational number
	% constructed using this module since the equality predicate
	% on rationals is simply Mercury's default unification
	% predicate =/2. If the invariants were not maintained,
	% we would have pathologies like r(-1,2) \= r(1,-2).
	%
	% The rational_norm/2 function generates rationals in this
	% normal form.
	%
:- type rational
	--->	r(integer, integer).

rational:'<'(R1, R2) :-
	Cmp = cmp(R1, R2),
	Cmp = lessthan.

rational:'>'(R1, R2) :-
	Cmp = cmp(R1, R2),
	Cmp = greaterthan.

rational:'=<'(R1, R2) :-
	Cmp = cmp(R1, R2),
	(Cmp = lessthan ; Cmp = equal).

rational:'>='(R1, R2) :-
	Cmp = cmp(R1, R2),
	(Cmp = greaterthan ; Cmp = equal).

rational(Num, Den) = rational_norm(integer(Num), integer(Den)).

rational_from_integers(Num, Den) = rational_norm(Num, Den).

%% XXX: There are ways to do this in some cases even if the
%% float conversions would overflow.
% rational__float(r(Num, Den)) =
%	float:'/'(integer__float(Num), integer__float(Den)).

one = r(integer(1), integer(1)).

zero = r(integer(0), integer(1)).

rational:'+'(Rat) = Rat.

rational:'-'(r(Num, Den)) = r(-Num, Den).

rational:'+'(r(An, Ad), r(Bn, Bd)) = rational_norm(Numer, M) :-
	Numer = An * CA + Bn * CB,
	M = lcm(Ad, Bd),
	CA = M // Ad,
	CB = M // Bd.

rational:'-'(R1, R2) =
	R1 + (-R2).

	% XXX: need we call rational_norm here?
rational:'*'(r(An, Ad), r(Bn, Bd)) = rational_norm(Numer, Denom) :-
	Numer = (An//G1) * (Bn//G2),
	Denom = (Ad//G2) * (Bd//G1),
	G1 = gcd(An, Bd),
	G2 = gcd(Ad, Bn).

rational:'/'(R1, R2) =
	R1 * inverse(R2).

:- func inverse(rational) = rational.
inverse(r(Num, Den)) = Rat :-
	( Num = izero ->
		error("rational__inverse: division by zero")
	;
		Rat = r(signum(Num)*Den, abs(Num))
	).

rational__numer(r(Num, _)) = Num.

rational__denom(r(_, Den)) = Den.

rational__abs(r(Num, Den)) = r(abs(Num), Den).

:- func rational_norm(integer, integer) = rational.
rational_norm(Num, Den) = Rat :-
	( Den = izero ->
		error("rational__rational_norm: division by zero")
	; Num = izero ->
		Rat = r(izero, ione)
	;
		Rat = r(Num2//G, Den2//G),
		Num2 = Num * signum(Den),
		Den2 = abs(Den),
		G = gcd(Num, Den)
	).

:- func gcd(integer, integer) = integer.
gcd(A, B) =
	gcd_2(abs(A), abs(B)).

:- func gcd_2(integer, integer) = integer.
gcd_2(A, B) =
	( B = izero -> A
	; gcd_2(B, A rem B)
	).

:- func lcm(integer, integer) = integer.
lcm(A, B) =
	( A = izero -> izero
	; B = izero -> izero
	; abs((A // gcd(A, B)) * B)
	).

:- func izero = integer.
izero = integer(0).

:- func ione = integer.
ione = integer(1).

:- func signum(integer) = integer.
signum(N) =
	( N = izero -> izero
	; N < izero -> -ione
	; ione
	).

:- type comparison
	--->	equal
	;	lessthan
	;	greaterthan.

:- func cmp(rational, rational) = comparison.
cmp(R1, R2) = Cmp :-
	Diff = R1 - R2,
	( is_zero(Diff) ->
		Cmp = equal
	; is_negative(Diff) ->
		Cmp = lessthan
	;
		Cmp = greaterthan
	).

:- pred is_zero(rational).
:- mode is_zero(in) is semidet.
is_zero(r(Num, _)) :-
	Num = izero.

:- pred is_negative(rational).
:- mode is_negative(in) is semidet.
is_negative(r(Num, _)) :-
	Zero = izero,
	Num < Zero.

