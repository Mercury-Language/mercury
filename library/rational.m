%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: rational.m
% authors: aet Apr 1998. (with plagiarism from rat.m)
% 
% Implements a rational number type and a set of basic operations on 
% rational numbers.
%
%-----------------------------------------------------------------------------%

:- module rational.

:- interface.

:- import_module integer.

:- type rational.

:- pred '<'(rational, rational).
:- mode '<'(in, in) is semidet.

:- pred '>'(rational, rational).
:- mode '>'(in, in) is semidet.

:- pred '=<'(rational, rational).
:- mode '=<'(in, in) is semidet.

:- pred '>='(rational, rational).
:- mode '>='(in, in) is semidet.

:- func rational__rational(int) = rational.

:- func rational__rational(int, int) = rational.

:- func rational__from_integer(integer) = rational.

:- func rational__from_integers(integer, integer) = rational.

	% New programs should use rational.from_integers/2. 
:- pragma obsolete(rational_from_integers/2).
:- func rational__rational_from_integers(integer, integer) = rational.

% :- func float(rational) = float.

:- func '+'(rational) = rational.

:- func '-'(rational) = rational.

:- func rational + rational = rational.

:- func rational - rational = rational.

:- func rational * rational = rational.

:- func rational / rational = rational.

:- func rational__numer(rational) = integer.

:- func rational__denom(rational) = integer.

:- func rational__abs(rational) = rational.

:- func rational__reciprocal(rational) = rational.

:- func rational__one = rational.

:- func rational__zero = rational.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

'<'(R1, R2) :-
	Cmp = cmp(R1, R2),
	Cmp = lessthan.

'>'(R1, R2) :-
	Cmp = cmp(R1, R2),
	Cmp = greaterthan.

'=<'(R1, R2) :-
	Cmp = cmp(R1, R2),
	(Cmp = lessthan ; Cmp = equal).

'>='(R1, R2) :-
	Cmp = cmp(R1, R2),
	(Cmp = greaterthan ; Cmp = equal).

rational__rational(Int) = rational_norm(integer(Int), integer__one).

rational__rational(Num, Den) = rational_norm(integer(Num), integer(Den)).

rational__from_integer(Integer) = rational_norm(Integer, integer__one).

rational__from_integers(Num, Den) = rational_norm(Num, Den).

rational_from_integers(Num, Den) = rational_norm(Num, Den).

%% XXX: There are ways to do this in some cases even if the
%% float conversions would overflow.
% rational__float(r(Num, Den)) =
%	float:'/'(integer__float(Num), integer__float(Den)).

rational__one = r(integer__one, integer__one).

rational__zero = r(integer__zero, integer__one).

'+'(Rat) = Rat.

'-'(r(Num, Den)) = r(-Num, Den).

r(An, Ad) + r(Bn, Bd) = rational_norm(Numer, M) :-
	M = lcm(Ad, Bd),
	CA = M // Ad,
	CB = M // Bd,
	Numer = An * CA + Bn * CB.

R1 - R2 = R1 + (-R2).

	% XXX: need we call rational_norm here?
r(An, Ad) * r(Bn, Bd) = rational_norm(Numer, Denom) :-
	G1 = gcd(An, Bd),
	G2 = gcd(Ad, Bn),
	Numer = (An // G1) * (Bn // G2),
	Denom = (Ad // G2) * (Bd // G1).

R1 / R2 = R1 * reciprocal(R2).

rational__reciprocal(r(Num, Den)) = 
	( Num = integer__zero ->
		func_error("rational.reciprocal: division by zero")
	;
		r(signum(Num) * Den, integer__abs(Num))
	).

rational__numer(r(Num, _)) = Num.

rational__denom(r(_, Den)) = Den.

rational__abs(r(Num, Den)) = r(integer__abs(Num), Den).

:- func rational_norm(integer, integer) = rational.

rational_norm(Num, Den) = Rat :-
	( Den = integer__zero ->
		error("rational__rational_norm: division by zero")
	; Num = integer__zero ->
		Rat = r(integer__zero, integer__one)
	;
		G    = gcd(Num, Den),
		Num2 = Num * signum(Den),
		Den2 = integer__abs(Den),
		Rat  = r(Num2 // G, Den2 // G)
	).

:- func gcd(integer, integer) = integer.

gcd(A, B) = gcd_2(integer__abs(A), integer__abs(B)).

:- func gcd_2(integer, integer) = integer.

gcd_2(A, B) = ( B = integer__zero -> A ; gcd_2(B, A rem B) ).

:- func lcm(integer, integer) = integer.

lcm(A, B) =
	( A = integer__zero -> integer__zero
	; B = integer__zero -> integer__zero
	; integer__abs((A // gcd(A, B)) * B)
	).

:- func signum(integer) = integer.

signum(N) =
	( N = integer__zero -> integer__zero
	; N < integer__zero -> -integer__one
	; integer__one
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

:- pred is_zero(rational::in) is semidet.

is_zero(r(integer__zero, _)).

:- pred is_negative(rational::in) is semidet.

is_negative(r(Num, _)) :-
	Num < integer__zero.

%------------------------------------------------------------------------------%
:- end_module rational.
%------------------------------------------------------------------------------%
