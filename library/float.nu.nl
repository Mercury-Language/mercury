%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: float.nu.nl.
% Main author: fjh.
%
% This file provides a Prolog implementation of `float__min', `float__max',
% and `float__epsilon'.
%
%-----------------------------------------------------------------------------%

% XXX These definitions are highly platform-dependent.
%     Currently we just hard-wire the definitions for IEEE double precision.

% This code is written very carefully to avoid bugs and incompatibilities
% in NU-Prolog and SICStus Prolog:
%
% 	We mustn't write `X = 1.7976931348623157E308', because NU-Prolog
% 	has a bug which means that it fails to compile that.
%
% 	We mustn't write `X is 1.7976931348623157 * 10 ** 308', as NU-Prolog
%	wants, because SICStus reports a syntax error.
%
% 	We mustn't write `X is 1.7976931348623157 * exp(10, 308)', as SICStus
%	Prolog wants, because NU-Prolog has a different bug which means that
%	it fails to compile that too.
% 
% Writing this code was like walking a mine-field... if you change it,
% be sure to test it on both NU-Prolog and SICStus.

float__max(X) :-
	% 1.7976931348623157E+308
	( nuprolog ->
		X is 1.79769313486231 * '**'(10, 308)
	;
		Exp = exp(10, 308),
		X is 1.79769313486231 * Exp
	).

	% really float__min(2.2250738585072014e-308).
	% but if we put in the last two digits, then
	% NU-Prolog (1.6.12) will underflow to zero.
	% (This happens with compiled code (nc), but not
	% with consulted code (np).)
float__min(2.22507385850721e-308).

float__epsilon(2.2204460492503131e-16).

	%  Note:  This hash function is different to the one defined in
	%  float.m.
	%  How it works:  first ensure Float is positive by taking the
	%  absolute value (Abs) and then adding epsilon if Abs == 0.0
	%  (Abs2).  Next calculate the lowest power of e that is >=
	%  Abs2 (i.e.  exp(ceil(log(Abs2)))).  Divide Abs2 by this
	%  number to get a number in the range exp(-1) to 1.  Multiply
	%  this by 2^31-1 to get a number, Float2,  in the range
	%  790015083 to 2147483647.  To include the magnitude of Float
	%  in the computation, add ceil(log(Abs2)) to Float2 and then
	%  truncate the result to an integer (N2).  Truncate Float2 to
	%  integer (N1), XOR with N2 and take the absolute value to
	%  ensure the result in non-negative.

float__hash(Float, Hash) :-
	( Float >= 0 -> Abs = Float ; Abs is -Float),
	( Abs = 0.0 -> float__epsilon(Abs2) ; Abs2 = Abs ),
	Log is log(Abs2),
	R0 is round(Log),
	( R0 < Log -> R is R + 1 ; R is R0),
	Exp is exp(R),
	Float2 is Abs2 / Exp * 2147483647.0,   % 2147483647 = (2^31)-1
	N1 is integer(Float2),
	( R >= 0 -> R1 = R ; R1 is -R),
	N2 is integer(R1),
	Hash0 is N1 ^ N2,
	int__abs(Hash0, Hash).

%-----------------------------------------------------------------------------%
