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

copy(X, X).

unify(X, X).

index(_F, _I) :-
	error("mercury_builtin.nu.nl: index/2 called").

builtin_float_plus(X, Y, Z) :- Z is X + Y.
builtin_float_minus(X, Y, Z) :- Z is X - Y.
builtin_float_times(X, Y, Z) :- Z is X * Y.
builtin_float_divide(X, Y, Z) :- Z is X / Y.
builtin_float_le(X, Y) :- X =< Y.
builtin_float_lt(X, Y) :- X < Y.
builtin_float_ge(X, Y) :- X >= Y.
builtin_float_gt(X, Y) :- X > Y.

	% dynamic_environment_hack/2 is a mechanism for
	% storing any environment variables which are
	% set during the course of the program.  This is
	% because NU-Prolog does not provide a putenv-
	% like operation for modifying the environment.
	%
	% Entries are stored as
	%       dynamic_environment_hack(Var, Value)
	% where Var (an atom) is the variable and Value
	% (a string) is the associated value.
:- dynamic(dynamic_environment_hack/2).

io__getenv(Name, MaybeValue) :-
	name(NameA, Name),
	( dynamic_environment_hack(NameA, Value) ->
	    MaybeValue = Value
	;
	    ( getenv(NameA, Value) ->
		MaybeValue = Value
	    ;
		fail
	    )
	).

io__putenv(Line) :-
	np_builtin__split_env_string(Line, VarS, Value),
	name(Var, VarS),
	retractall(dynamic_environment_hack(Var, _)),
	assert(dynamic_environment_hack(Var, Value)).

np_builtin__split_env_string([], [], []).
np_builtin__split_env_string([C | Cs], Var, Rest) :-
	( C = (0'=) ->
	    Var = [], Rest = Cs
	;
	    Var = [C | Var0],
	    np_builtin__split_env_string(Cs, Var0, Rest)
	).

