%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

builtin_strcmp(Res, S1, S2) :-
	compare(R, S1, S2),
	builtin_strcmp_2(R, Res).

builtin_strcmp_2(<, -1).
builtin_strcmp_2(=, 0).
builtin_strcmp_2(>, 1).

unify(X, X).

index(_F, _I) :-
	error("mercury_builtin.nu.nl: index/2 called").

builtin_float_plus(X, Y, Z) :- Z is X + Y.
builtin_float_minus(X, Y, Z) :- Z is X - Y.
builtin_float_times(X, Y, Z) :- Z is X * Y.
builtin_float_divide(X, Y, Z) :- Z is X / Y.
builtin_float_le(X, Y, Z) :- X =< Y.
builtin_float_lt(X, Y, Z) :- X < Y.
builtin_float_ge(X, Y, Z) :- X >= Y.
builtin_float_gt(X, Y, Z) :- X > Y.

