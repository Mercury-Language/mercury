%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

builtin_strcmp(Res, S1, S2) :-
	compare(R, S1, S2),
	builtin_strcmp_2(R, Res).

builtin_strcmp_2(<, -1).
builtin_strcmp_2(=, 0).
builtin_strcmp_2(>, 1).

copy(X, Y) :-
	duplicate(X, Y).

unsafe_promise_unique(X, X).

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

term_to_type(term__functor(Const, Args, _Context), Type) :-
	term_to_type_2(Const, Args, Type).

term_to_type_2(term__integer(Int), [], Int).

term_to_type_2(term__string(String), [], String).

term_to_type_2(term__float(Float), [], Float).

term_to_type_2(term__atom(NameString), Args, Type) :-
	length(Args, NumArgs),
	name(NameAtom, NameString),
	functor(Type1, NameAtom, NumArgs),
	term_to_type_args(Args, Type1, 1, Type).


term_to_type_args([], Type, _ArgNum, Type).

term_to_type_args([Arg | Args], Type1, ArgNum, Type) :-
	term_to_type(Arg, ArgType),
	arg(ArgNum, Type1, ArgType),
	NextArgNum is ArgNum + 1,
	term_to_type_args(Args, Type1, NextArgNum, Type).


type_to_term(Type, Term) :-
	Context = term__context("", 0),
	(
		integer(Type)
	->
		Term = term__functor(term__integer(Type), [], Context)
	;
		% Because string a treated as a list of ints all list of ints
		% are converted back to strings.
		Type = [Int | _Ints],
		integer(Int)
	->
		Term = term__functor(term__string(Type), [], Context)
	;
		float(Type)
	->
		Term = term__functor(term__float(Type), [], Context)
	;
		functor(Type, FunctorAtom, Arity),
		name(FunctorAtom, FunctorString),
		type_to_term_args(Type, Arity, 1, Args),
		Term = term__functor(term__atom(FunctorString), Args, Context)
	).


type_to_term_args(Type, Arity, ArgNum, Terms) :-
	 ( ArgNum > Arity ->
	 	Terms = []
	;
		arg(ArgNum, Type, ArgType),
		type_to_term(ArgType, Term),
		NextArgNum is ArgNum + 1,
		Terms = [Term | Terms1],
		type_to_term_args(Type, Arity, NextArgNum, Terms1)
	).


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

lambda(LambdaExpression, Goal0) :-
	( LambdaExpression = ([] is _Det) ->
		duplicate(Goal0, Goal)
	;
		error("error in use of `lambda'")
	),
	call(Goal).

lambda(LambdaExpression, Goal0, Arg1) :-
	( LambdaExpression = ([Var1::_Mode1] is _Det), var(Var1) ->
		duplicate([Var1|Goal0], [Arg1|Goal])
	;
		error("error in use of `lambda'")
	),
	call(Goal).

lambda(LambdaExpression, Goal0, Arg1, Arg2) :-
	(
		LambdaExpression = ([Var1::_Mode1, Var2::_Mode2] is _Det),
		var(Var1), var(Var2)
	->
		duplicate([Var1, Var2 | Goal0], [Arg1, Arg2 | Goal])
	;
		error("error in use of `lambda'")
	),
	call(Goal).

lambda(LambdaExpression, Goal0, Arg1, Arg2, Arg3) :-
	(
		LambdaExpression = ([Var1::_Mode1, Var2::_Mode2, Var3::_Mode3]
			is _Det),
		var(Var1), var(Var2), var(Var3)
	->
		duplicate([Var1, Var2, Var3 | Goal0], [Arg1, Arg2, Arg3 | Goal])
	;
		error("error in use of `lambda'")
	),
	call(Goal).

lambda(LambdaExpression, Goal0, Arg1, Arg2, Arg3, Arg4) :-
	(
		LambdaExpression = ([Var1::_Mode1, Var2::_Mode2, Var3::_Mode3,
			Var4::_Mode4] is _Det),
		var(Var1), var(Var2), var(Var3), var(Var4)
	->
		duplicate([Var1, Var2, Var3, Var4 | Goal0],
			[Arg1, Arg2, Arg3, Arg4 | Goal])
	;
		error("error in use of `lambda'")
	),
	call(Goal).

lambda(LambdaExpression, Goal0, Arg1, Arg2, Arg3, Arg4, Arg5) :-
	(
		LambdaExpression = ([Var1::_Mode1, Var2::_Mode2, Var3::_Mode3,
			Var4::_Mode4, Var5::_Mode5] is _Det),
		var(Var1), var(Var2), var(Var3), var(Var4), var(Var5)
	->
		duplicate([Var1, Var2, Var3, Var4, Var5 | Goal0],
			[Arg1, Arg2, Arg3, Arg4, Arg5 | Goal])
	;
		error("error in use of `lambda'")
	),
	call(Goal).

% Call/N are built into NU-Prolog for N < 8, but the Mercury compiler
% has some places where it needs N to be higher than that.

call(Goal0, A, B, C, D, E, F, G) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H, I) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H, I], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H, I, J) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H, I, J], L),
	Goal =.. L,
	call(Goal).
