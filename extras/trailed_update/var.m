%-----------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: var.m
% Main author: fjh
%
% This module provides constraint solving for unification constraints;
% in other words, it provides Prolog-style variables.
% It also provides some features for delaying (a.k.a dynamic scheduling,
% or corouting), specifically freeze/2 and freeze/3.
%
% There is no occurs check -- this module does not provide Herbrand terms.
% Values of type var/1 may be cyclic.  However, the solver not complete
% for cyclic terms; if you attempt to do anything much with cyclic terms,
% your program will probably not terminate.
%
% XXX TODO: add code to check for floundering.
%
%-----------------------------------------------------------------------------%
:- module var.
:- interface.

	% A `var(T)' is a Prolog-style variable that holds a value of type T.
:- type var(T).

	% `init(Var)' can be used to initialize
	% the inst of a variable to `any'.
:- pred init(var(T)::out(any)) is det.

	% `Var = var(Value)' unifies a variable with its value.
	% This can be used in several ways:
	% to bind a variable to a particular value, `X = var(42)';
	% to extract the value of that variable, `X = var(Y)';
	% or (NYI) to initialize the inst of a variable to any, `X = var(_)'.
:- func var(T) = var(T).
:- mode var(in) = out is det.
:- mode var(in) = in(any) is semidet.
:- mode var(in) = in is semidet.
:- mode var(out) = in is det.

	% `Var1 == Var2' can be used to unify two variables.
	% Alternatively, you can just use `=' rather than `==', 
	% but `=' doesn't support the `out(any) = out(any)' mode yet.
:- pred var(T) == var(T).
:- mode in == in is semidet.
:- mode in == out is det.
:- mode out == in is det.
:- mode in(any) == in(any) is semidet.
:- mode in(any) == out(any) is det.
:- mode out(any) == in(any) is det.
:- mode out(any) == out(any) is det.

	% `freeze(Var, Pred)' can be used to delay execution of a goal
	% until a variable is ground.
	% Often the freeze/3 version is more useful, though,
	% since this version doesn't allow `Pred' to have any outputs.
	% (XXX the compiler doesn't check that yet - this is a bug!)
	% Declaratively, freeze(Var, Pred) is true iff Pred(Var) is true.
	% Operationally, freeze(Var, Pred) delays until Var becomes ground
	% and then calls Pred(Var).
:- pred freeze(var(T), pred(T)).
:- mode freeze(in(any), pred(in) is semidet) is semidet.
:- mode freeze(out(any), pred(in) is semidet) is semidet.

	% `debug_freeze(Message, Var, Pred)'
	% is the same as `freeze(Var, Pred)' except
	% that it also prints out some debugging information.
	% WARNING: this is a non-logical hack, use only for debugging!
:- pred debug_freeze(string, var(T), pred(T)).
:- mode debug_freeze(in, in(any), pred(in) is semidet) is semidet.

	% `freeze(Var1, Pred, Var2)' can be used to delay
	% execution of a goal until a variable is ground.
	% This version is more flexible than freeze/2, since
	% it allows the delayed goal to have an output.
	% Declaratively, freeze(X, Pred, Y) is true iff Pred(X, Y) is true.
	% Operationally, freeze(X, Pred, Y) delays until X becomes ground
	% and then calls Pred(X, Y).
:- pred freeze(var(T1),  pred(T1, T2), var(T2)).
:- mode freeze(in,	 pred(in, out) is det, out) is semidet. % really det
:- mode freeze(in,	 pred(in, out) is semidet, out) is semidet.
:- mode freeze(out(any), pred(in, out) is det, out(any)) is semidet.
:- mode freeze(out(any), pred(in, out) is semidet, out(any)) is semidet.

:- pred freeze_var(var(T1),  pred(T1, var(T2)), var(T2)).
:- mode freeze_var(out(any), pred(in, in(any)) is semidet, out(any)) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%
:- import_module std_util.
:- import_module unsafe, io.
:- import_module require.
%-----------------------------------------------------------------------------%
%
% The implementation is mostly written in impure unsafe Mercury,
% using non-logical destructive update.  The C interface is used
% as a means of providing different implementations for different
% modes of a predicate and for doing unchecked type/inst casts to
% cast values of type and inst `var(T)::any' to `var_rep(T)::ground'
% and back again -- that is, to convert from variables to their concrete
% representation and back.
%
% PLEASE DO NOT IMITATE THIS CODING STYLE!
%
%---------------------------------------------------------------------------%

:- type var(T) ---> var_rep(var_rep(T))
	where equality is (==).

:- type var_rep(T)
	--->	free
	;	free(conj(delayed_goal(T)))
	;	alias(var_rep(T))
	;	ground(T).

:- inst var_rep_any =
	bound(	free
	;	free(conj(delayed_goal))
	;	alias(var_rep_any)
	;	ground(ground)
	).
:- inst var_rep_ground =
	bound(	alias(var_rep_ground)
	;	ground(ground)
	).
:- inst var_rep_deref_ground =
	bound(	ground(ground)
	).
:- inst var_rep_deref_delayed =
	bound(	free(conj(delayed_goal))
	).

% We use an extra level of indirection so that we can do
% (backtrackable) destructive update on variable representations
% using setarg/3.
%
% Note: I didn't use `uniq_ptr' in all the places where it
% ought to be unique, because the lack of support for aliasing
% makes `unique-input' modes impossible.

:- inst ptr(I) = bound(alias(I)).
:- inst uniq_ptr(I) = unique(alias(I)).

% The type `conj(T)' represents a conjunction of goals of type T.

:- type conj(T)
	--->	goal(T)
	;	(conj(T), conj(T)).
:- inst conj(I) =
	bound(	goal(I)
	;	(conj(I), conj(I))
	).

% The type `delayed_goal(T)' represents a goal delayed on a variable
% of type T.

% Handling delayed goals with outputs properly would require existential
% types; instead we just hack it by munging the type_infos manually
% using some unsafe casts

:- type type_info_for_t2 == type_info.
:- type t2 == c_pointer.

:- type delayed_goal(T)
	--->	unary_pred(pred(T))

	;	/* some [T2] binary_pred(pred(T, T2), var(T2)) */
		binary_det_pred(
			pred(type_info_for_t2, T, t2),
			type_info_for_t2, var(t2))

	;	/* some [T2] binary_pred(pred(T, T2), var(T2)) */
		binary_semidet_pred(
			pred(type_info_for_t2, T, t2),
			type_info_for_t2, var(t2))

	;	/* some [T2]
		   binary_semidet_pred_any(pred(T, var(T2)), var(T2)) */
		binary_semidet_pred_any(
			pred(type_info_for_t2, T, var(t2)),
			type_info_for_t2, var(t2))
	.

:- inst delayed_goal
	--->	unary_pred(pred(in) is semidet)
	;	binary_det_pred(
			pred(in, in, out) is det,
			ground, any)
	;	binary_semidet_pred(
			pred(in, in, out) is semidet,
			ground, any)
	;	binary_semidet_pred_any(
			pred(in, in, in(any)) is semidet,
			ground, any).

%-----------------------------------------------------------------------------%

:- pragma c_code(init(Var::out(any)), may_call_mercury, "
	Var = ML_var_alias(TypeInfo_for_T, ML_var_free(TypeInfo_for_T));
").

/*
% The compiler generates wrong code for this --
% the output is not unique, even thought we declared it to be unique.
% It puts the `alias(free)' term in read-only memory.  Hence, to avoid this,
% we use separate calls to functions for alias/1 and free/0.
:- pred var__rep_init(var_rep(T)::out(uniq_ptr(var_rep_any))) is det.
:- pragma export(var__rep_init(out(uniq_ptr(var_rep_any))), "ML_var_init").
var__rep_init(alias(free)).
*/

:- func var__rep_free = (var_rep(T)::out(var_rep_any)) is det.
:- pragma export(var__rep_free = out(var_rep_any), "ML_var_free").
var__rep_free = free.

:- func var__rep_alias(var_rep(T)::in(var_rep_any)) =
		(var_rep(T)::out(var_rep_any)) is det.
:- pragma export(var__rep_alias(in(var_rep_any)) = out(var_rep_any),
		"ML_var_alias").
var__rep_alias(T) = alias(T).

%-----------------------------------------------------------------------------%

/****
:- pragma c_code( var(Value::(free -> clobbered_any)) = (Var::out(any)), % det
	may_call_mercury,
"
	* Value unused *
	ML_var_init(&Var);
").
****/

:- pragma c_code( var(Value::in) = (Var::out) /* det */,
	may_call_mercury,
"
	ML_var_init_with_value(TypeInfo_for_T, Value, &Var);
").

:- pred var__rep_init_with_value(T::in, var_rep(T)::out(var_rep_ground))
	is det.
:- pragma export(var__rep_init_with_value(in, out(var_rep_ground)),
	"ML_var_init_with_value").
var__rep_init_with_value(Value, ground(Value)).

:- pragma c_code( var(Value::out) = (Var::in) /* det */, may_call_mercury,
"
	ML_var_get_value(TypeInfo_for_T, Var, &Value);
").

:- pred var__rep_get_value(var_rep(T)::in(var_rep_ground), T::out) is det.
:- pragma export(var__rep_get_value(in(var_rep_ground), out),
	"ML_var_get_value").
var__rep_get_value(ground(Value), Value).
var__rep_get_value(alias(Var), Value) :-
	var__rep_get_value(Var, Value).

:- pragma c_code( var(Value::in) = (Var::in) /* semidet */,
	may_call_mercury,
"
	SUCCESS_INDICATOR = ML_var_test_value(TypeInfo_for_T, Var, Value);
").

:- pred var__rep_test_value(var_rep(T)::in(var_rep_ground), T::in) is semidet.
:- pragma export(var__rep_test_value(in(var_rep_ground), in),
	"ML_var_test_value").
var__rep_test_value(Var, Value) :-
	var__rep_get_value(Var, VarValue),
	Value = VarValue.

:- pragma c_code( var(Value::in) = (Var::in(any)) /* semidet */,
	may_call_mercury,
"
	SUCCESS_INDICATOR = ML_var_unify_with_val(TypeInfo_for_T, Value, Var);
").

:- pred var__rep_unify_with_val(T, var_rep(T)).
:- mode var__rep_unify_with_val(in, in(ptr(var_rep_any))) is semidet.
:- pragma export(var__rep_unify_with_val(in, in(ptr(var_rep_any))),
	"ML_var_unify_with_val").
var__rep_unify_with_val(Value, VarPtr) :-
	VarPtr = alias(Var),
	( 
		Var = alias(_),
		var__rep_unify_with_val(Value, Var)
	;
		Var = ground(OldValue),
		Value = OldValue
	;
		Var = free,
		destructively_update_binding(VarPtr, ground(Value))
	;
		Var = free(DelayedGoals),
		call_delayed_goals(DelayedGoals, Value),
		destructively_update_binding(VarPtr, ground(Value))
	).

%-----------------------------------------------------------------------------%

:- pred call_delayed_goals(conj(delayed_goal(T)), T).
:- mode call_delayed_goals(in(conj(delayed_goal)), in) is semidet.

call_delayed_goals(goal(Goal), Value) :-
	call_delayed_goal(Goal, Value).
call_delayed_goals((GoalsX, GoalsY), Value) :-
	call_delayed_goals(GoalsX, Value),
	call_delayed_goals(GoalsY, Value).

:- pred call_delayed_goal(delayed_goal(T), T).
:- mode call_delayed_goal(in(delayed_goal), in) is semidet.

call_delayed_goal(unary_pred(Pred), Value) :-
	call(Pred, Value).
call_delayed_goal(binary_det_pred(Pred, TypeInfo2, var(Arg2)), Value) :-
	call(Pred, TypeInfo2, Value, Arg2).
call_delayed_goal(binary_semidet_pred(Pred, TypeInfo2, var(Arg2)), Value) :-
	call(Pred, TypeInfo2, Value, Arg2).
call_delayed_goal(binary_semidet_pred_any(Pred, TypeInfo2, Arg2), Value) :-
	call(Pred, TypeInfo2, Value, Arg2).

%-----------------------------------------------------------------------------%

freeze(Var, Pred) :-
	do_freeze(Var, unary_pred(Pred)).

:- pred do_freeze(var(T), delayed_goal(T)).
:- mode do_freeze(in(any), in(delayed_goal)) is semidet.
:- mode do_freeze(out(any), in(delayed_goal)) is semidet.

:- pragma c_code(
	do_freeze(Var::in(any), Pred::in(delayed_goal)) /* semidet */,
	may_call_mercury,
"
	ML_var_freeze_in(TypeInfo_for_T, Var, Pred);
	SUCCESS_INDICATOR = TRUE;
").
:- pragma c_code(
	do_freeze(Var::out(any), Pred::in(delayed_goal)) /* semidet */,
	may_call_mercury,
"
	ML_var_freeze_out(TypeInfo_for_T, &Var, Pred);
	SUCCESS_INDICATOR = TRUE;
").


:- pred var__rep_freeze_out(var_rep(T), delayed_goal(T)).
:- mode var__rep_freeze_out(out(ptr(var_rep_any)), in(delayed_goal))
	is det.
:- pragma export(
	var__rep_freeze_out(out(ptr(var_rep_any)), in(delayed_goal)),
	"ML_var_freeze_out").

var__rep_freeze_out(Var, Pred) :-
	Var = alias(free(goal(Pred))).

:- pred var__rep_freeze_in(var_rep(T), delayed_goal(T)).
:- mode var__rep_freeze_in(in(ptr(var_rep_any)), in(delayed_goal)) is semidet.
:- pragma export(
	var__rep_freeze_in(in(ptr(var_rep_any)), in(delayed_goal)),
	"ML_var_freeze_in").

var__rep_freeze_in(VarPtr, Pred) :-
	VarPtr = alias(Var),
	(
		Var = alias(_),
		var__rep_freeze_in(Var, Pred)
	;
		Var = ground(Value),
		call_delayed_goal(Pred, Value)
	;
		Var = free,
		NewVar = free(goal(Pred)),
		destructively_update_binding(VarPtr, NewVar)
	;
		Var = free(OldGoals),
		NewVar = free((OldGoals, goal(Pred))),
		destructively_update_binding(VarPtr, NewVar)
	).

/*
:- pred freeze(var(T1),  pred(T1, T2), var(T2)).
:- mode freeze(in,	 pred(in, out) is det, out) is semidet. % no delay
:- mode freeze(in,	 pred(in, out) is semidet, out) is semidet. % no delay
:- mode freeze(out(any), pred(in, out) is det, out(any)) is semidet.
:- mode freeze(out(any), pred(in, out) is semidet, out(any)) is semidet.
:- mode freeze_var(out(any), pred(in, in(any)) is semidet, out(any)) is semidet.
*/
:- pragma c_code(
	freeze(X::in, Pred::(pred(in, out) is det), Y::out), % det
	may_call_mercury,
"{
	Word XVal, YVal;

	/* don't delay, just call the pred */
	ML_var_get_value(TypeInfo_for_T1, X, &XVal);
	ML_var_call_det_pred(TypeInfo_for_T1, TypeInfo_for_T2,
		Pred, XVal, &YVal);
	ML_var_init_with_value(TypeInfo_for_T2, YVal, &Y);
}").
:- pragma c_code(
	freeze(X::in, Pred::(pred(in, out) is semidet), Y::out), % semidet
	may_call_mercury,
"{
	Word XVal, YVal;

	/* don't delay, just call the pred */
	ML_var_get_value(TypeInfo_for_T1, X, &XVal);
	if (ML_var_call_semidet_pred(TypeInfo_for_T1, TypeInfo_for_T2,
			Pred, XVal, &YVal))
	{
		ML_var_init_with_value(TypeInfo_for_T2, YVal, &Y);
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").
:- pragma c_code(
	freeze(X::out(any), Pred::(pred(in, out) is det), Y::out(any)),
		% semidet
	may_call_mercury,
"{
	Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_det_pred(TypeInfo_for_T1, Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = TRUE;
}").
:- pragma c_code(
	freeze(X::out(any), Pred::(pred(in, out) is semidet), Y::out(any)),
		% semidet
	may_call_mercury,
"{
	Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_semidet_pred(TypeInfo_for_T1,
			Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = TRUE;
}").

:- pragma c_code(
	freeze_var(X::out(any), Pred::(pred(in, in(any)) is semidet),
		Y::out(any)), % semidet
	may_call_mercury,
"{
	Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_semidet_pred_any(TypeInfo_for_T1,
			Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = TRUE;
}").

%-----------------------------------------------------------------------------%

% The following code just exports the constructors for the type
% delayed_goal/1 to C.

:- func var_binary_det_pred(pred(type_info_for_t2, T, t2),
		type_info_for_t2, var(t2)) = delayed_goal(T).
:- mode var_binary_det_pred(pred(in, in, out) is det, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_det_pred(pred(in, in, out) is det, in, in(any)) =
		out(delayed_goal), "ML_var_binary_det_pred").
var_binary_det_pred(Pred, TypeInfo, SecondArg) =
	binary_det_pred(Pred, TypeInfo, SecondArg).

:- func var_binary_semidet_pred(pred(type_info_for_t2, T, t2),
		type_info_for_t2, var(t2)) = delayed_goal(T).
:- mode var_binary_semidet_pred(pred(in, in, out) is semidet, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_semidet_pred(pred(in, in, out) is semidet, in, in(any)) =
		out(delayed_goal), "ML_var_binary_semidet_pred").
var_binary_semidet_pred(Pred, TypeInfo, SecondArg) =
	binary_semidet_pred(Pred, TypeInfo, SecondArg).

:- func var_binary_semidet_pred_any(
		pred(type_info_for_t2, T, var(t2)),
		type_info_for_t2, var(t2)) = delayed_goal(T).
:- mode var_binary_semidet_pred_any(
		pred(in, in, in(any)) is semidet, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_semidet_pred_any(
		pred(in, in, in(any)) is semidet, in, in(any)) =
		out(delayed_goal), "ML_var_binary_semidet_pred_any").
var_binary_semidet_pred_any(Pred, TypeInfo, SecondArg) =
	binary_semidet_pred_any(Pred, TypeInfo, SecondArg).

%-----------------------------------------------------------------------------%

:- pred call_det_pred(pred(T1, T2), T1, T2).
:- mode call_det_pred(pred(in, out) is det, in, out) is det.
:- pragma export(
	call_det_pred(pred(in, out) is det, in, out),
	"ML_var_call_det_pred").
call_det_pred(Pred, X, Y) :-
	call(Pred, X, Y).

:- pred call_semidet_pred(pred(T1, T2), T1, T2).
:- mode call_semidet_pred(pred(in, out) is semidet, in, out) is semidet.
:- pragma export(
	call_semidet_pred(pred(in, out) is semidet, in, out),
	"ML_var_call_semidet_pred").
call_semidet_pred(Pred, X, Y) :-
	call(Pred, X, Y).

%-----------------------------------------------------------------------------%

/*
	% `Var1 == Var2' can be used to unify two variables.
:- pred var(T) == var(T).
:- mode in == in is semidet.
:- mode in == out is det.
:- mode out == in is det.
:- mode in(any) == in(any) is semidet.
:- mode in(any) == out(any) is det.
:- mode out(any) == out(any) is det.
:- mode out(any) == in(any) is det.
*/

:- pragma c_code((X::in) == (Y::out) /* det */, may_call_mercury,
	"Y = X;").
:- pragma c_code((X::out) == (Y::in) /* det */, may_call_mercury,
	"X = Y;").
:- pragma c_code((X::in(any)) == (Y::out(any)) /* det */, may_call_mercury,
	"Y = X;").
:- pragma c_code((X::out(any)) == (Y::in(any)) /* det */, may_call_mercury,
	"X = Y;").
:- pragma c_code((X::in) == (Y::in) /* semidet */, may_call_mercury,
	"SUCCESS_INDICATOR = ML_var_unify(TypeInfo_for_T, X, Y);").
:- pragma c_code((X::in(any)) == (Y::in(any)) /* semidet */, may_call_mercury,
	"SUCCESS_INDICATOR = ML_var_unify(TypeInfo_for_T, X, Y);").
:- pragma c_code((X::out(any)) == (Y::out(any)) /* semidet */, may_call_mercury,
	"X = Y = ML_var_alias(TypeInfo_for_T, ML_var_free(TypeInfo_for_T));").

:- pred var__rep_unify(var_rep(T), var_rep(T)).
:- mode var__rep_unify(in(ptr(var_rep_any)), in(ptr(var_rep_any))) is semidet.
:- pragma export(var__rep_unify(in(ptr(var_rep_any)), in(ptr(var_rep_any))),
	"ML_var_unify").
var__rep_unify(XPtr, YPtr) :-
	XPtr = alias(X),
	(
		X = alias(_),
		var__rep_unify(X, YPtr)
	;
		X = free,
		% would it be better to deref YPtr here?
		destructively_update_binding(XPtr, YPtr)
	;
		X = ground(_),
		var__rep_unify_gr(X, YPtr)
	;
		X = free(_),
		var__rep_unify_fr(XPtr, YPtr, X)
	).

	% This is the case when the first var is ground
:- pred var__rep_unify_gr(var_rep(T), var_rep(T)).
:- mode var__rep_unify_gr(in(var_rep_deref_ground), in(ptr(var_rep_any)))
	is semidet.
var__rep_unify_gr(X, YPtr) :-
	YPtr = alias(Y),
	(
		Y = alias(_),
		var__rep_unify_gr(X, Y)
	;
		Y = ground(Value),
		X = ground(Value)
	;
		Y = free,
		destructively_update_binding(YPtr, X)
	;
		Y = free(DelayedGoals),
		X = ground(Value),
		call_delayed_goals(DelayedGoals, Value),
		destructively_update_binding(YPtr, X)
	).

	% This is the case when the first var is free(DelayedGoals).
:- pred var__rep_unify_fr(var_rep(T), var_rep(T), var_rep(T)).
:- mode var__rep_unify_fr(in(ptr(var_rep_any)), % really deref_delayed
			in(ptr(var_rep_any)),
			in(var_rep_deref_delayed)) is semidet.
var__rep_unify_fr(XPtr, YPtr, X) :-
	YPtr = alias(Y),
	(
		Y = alias(_),
		var__rep_unify_fr(XPtr, Y, X)
	;
		Y = free,
		destructively_update_binding(YPtr, X)
	;
		Y = ground(Value),
		X = free(XGoals),
		call_delayed_goals(XGoals, Value),
		destructively_update_binding(XPtr, Y)
	;
		Y = free(YGoals),
		X = free(XGoals),
		XY = free((XGoals, YGoals)),
		destructively_update_binding(XPtr, XY),
		destructively_update_binding(YPtr, XY)
	).

%-----------------------------------------------------------------------------%

/* impure */
:- pred destructively_update_binding(var_rep(T), var_rep(T)).
:- mode destructively_update_binding(in(ptr(var_rep_any)), in(var_rep_any))
	is det.

destructively_update_binding(VarPtr, NewBinding) :-
	setarg(VarPtr, 1, NewBinding).

%-----------------------------------------------------------------------------%
/*
** setarg/3 provides non-logical backtrackable destructive update.
** `setarg(Term, N, Value)' destructively modifies the Nth
** argument of `Term' to be `Value'.  The modification will be undone
** on backtracking.
**
** WARNING: setarg/3 uses side-effects and is not type-safe!
**          Also it does not work for types with exactly one
**	    functor that has exactly one arg.
**	    It may not work with future release of the Mercury compiler,
**	    or with other Mercury implementations.
**          Use only with great care!
*/
:- pred setarg(T1, int, T2).
:- mode setarg(in(any), in, in(any)) is det.

%-----------------------------------------------------------------------------%

:- pragma c_code(
	setarg(MercuryTerm::in(any), ArgNum::in, NewValue::in(any)),
	will_not_call_mercury,
"{
	Word *ptr = (Word *) strip_tag(MercuryTerm); /* strip off tag bits */
	MR_trail_current_value(&ptr[ArgNum - 1]);
	ptr[ArgNum - 1] = NewValue;
}").

%-----------------------------------------------------------------------------%

:- pragma no_inline(debug_freeze/3).

debug_freeze(Msg, Var, Pred) :-
	unsafe_perform_io(print("freezing: ")),
	unsafe_perform_io(print(Msg)),
	unsafe_perform_io(print(": ")),
	unsafe_perform_io(print_any(Var)),
	unsafe_perform_io(nl),
	( freeze(Var, debug_pred(Msg, Pred)) ->
		unsafe_perform_io(print("frozen: ")),
		unsafe_perform_io(print(Msg)),
		unsafe_perform_io(print(": ")),
		unsafe_perform_io(print_any(Var)),
		unsafe_perform_io(nl)
	;
		unsafe_perform_io(print("freeze failed: ")),
		unsafe_perform_io(print(Msg)),
		unsafe_perform_io(print(": ")),
		unsafe_perform_io(print_any(Var)),
		unsafe_perform_io(nl),
		semidet_fail
	).


:- pred debug_pred(string::in, pred(T)::(pred(in) is semidet), T::in)
	is semidet.

debug_pred(Msg, Pred, Var) :-
	unsafe_perform_io(print("woke: ")),
	unsafe_perform_io(print(Msg)),
	unsafe_perform_io(print(": ")),
	unsafe_perform_io(print(Var)),
	unsafe_perform_io(nl),
	( call(Pred, Var) ->
		unsafe_perform_io(print("succeeded: ")),
		unsafe_perform_io(print(Msg)),
		unsafe_perform_io(print(": ")),
		unsafe_perform_io(print(Var)),
		unsafe_perform_io(nl)
	;
		unsafe_perform_io(print("failed: ")),
		unsafe_perform_io(print(Msg)),
		unsafe_perform_io(print(": ")),
		unsafe_perform_io(print(Var)),
		unsafe_perform_io(nl),
		semidet_fail
	).

:- pred print_any(T::in(any), io__state::di, io__state::uo) is det.
print_any(X) -->
	print(unsafe_promise_ground(X)).

%-----------------------------------------------------------------------------%
