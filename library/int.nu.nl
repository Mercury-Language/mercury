%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: int.nu.nl.
% Main author: fjh.
%
% This file provides a Prolog implementation of `int__to_float'.
%
%-----------------------------------------------------------------------------%

int__to_float(Int, Float) :-
	% we could use `Float is float(Int)',
	% but the following is more portable (e.g. works on SWI-Prolog)
	Float is Int + 0.0.

int__bits_per_int(Bits) :-
	int__bits_per_int_2(1, 64, Bits).

int__bits_per_int_2(N, Max, Bits) :-
	( N >= Max ->
		% Some Prologs (e.g. SICStus) have arbitrary precision
		% integers, so we have to stop somewhere
		Bits = Max
	; (1 << N) < 0 ->
		Bits is N + 1
	; 
		N1 is N + 1,
		int__bits_per_int_2(N1, Max, Bits)
	).

%-----------------------------------------------------------------------------%
