%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: var.m
% Main author: fjh
% Stability: medium-low
%
% This module provides constraint solving for unification constraints;
% in other words, it provides Prolog-style variables.
%
% It also provides some features for delaying (a.k.a dynamic scheduling,
% or corouting), specifically freeze/2 and freeze/3.  However, this
% interface is not yet stable; it may undergo significant changes,
% or even be removed, in future releases.  (The reason for this is
% that there are some problems with mode checking higher-order terms
% containing non-local vars whose inst is `any', and we have not yet
% solved those problems.)
%
% There is no occurs check -- this module does not provide Herbrand terms.
% Values of type var/1 may be cyclic.  However, the solver not complete
% for cyclic terms; if you attempt to do anything much with cyclic terms,
% your program will probably not terminate.
%
%-----------------------------------------------------------------------------%
:- module var.
:- interface.
:- import_module io, std_util.

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
	% (XXX the compiler doesn't always check that yet - this is a bug!)
	% Declaratively, freeze(Var, Pred) is true iff Pred(Var) is true.
	% Operationally, freeze(Var, Pred) delays until Var becomes ground
	% and then calls Pred(Var).
	% Warning: the interface to this predicate may be modified in
	% future releases.
:- pred freeze(var(T), pred(T)).
:- mode freeze(in(any), pred(in) is semidet) is semidet.
:- mode freeze(out(any), pred(in) is semidet) is semidet.

	% `freeze(Var1, Pred, Var2)' can be used to delay
	% execution of a goal until a variable is ground.
	% This version is more flexible than freeze/2, since
	% it allows the delayed goal to have an output.
	% Declaratively, freeze(X, Pred, Y) is true iff Pred(X, Y) is true.
	% Operationally, freeze(X, Pred, Y) delays until X becomes ground
	% and then calls Pred(X, Y).
	% Warning: the interface to this predicate may be modified in
	% future releases.
:- pred freeze(var(T1),  pred(T1, T2), var(T2)).
:- mode freeze(in,	 pred(in, out) is det, out) is semidet. % really det
:- mode freeze(in,	 pred(in, out) is semidet, out) is semidet.
:- mode freeze(out(any), pred(in, out) is det, out(any)) is semidet.
:- mode freeze(out(any), pred(in, out) is semidet, out(any)) is semidet.

:- pred freeze_var(var(T1),  pred(T1, var(T2)), var(T2)).
:- mode freeze_var(out(any), pred(in, in(any)) is semidet, out(any)) is semidet.

	% `debug_freeze(Message, Var, Pred)'
	% is the same as `freeze(Var, Pred)' except
	% that it also prints out some debugging information.
	% WARNING: this is a non-logical hack, use only for debugging!
:- impure pred debug_freeze(string, var(T), pred(T)).
:- mode debug_freeze(in, in(any), pred(in) is semidet) is semidet.
:- mode debug_freeze(in, out(any), pred(in) is semidet) is semidet.

:- impure pred debug_freeze(string, var(T1), pred(T1, T2), var(T2)).
:- mode debug_freeze(in, in,	   pred(in, out) is semidet, out) is semidet.
:- mode debug_freeze(in, out(any), pred(in, out) is semidet, out(any))
	is semidet.

	% dump_var prints out a representation of a variable.
:- pred dump_var(var(T)::in(any), io__state::di, io__state::uo) is cc_multi.

	% unsafe_dump_var/1: an impure version of dump_var/3.
:- impure pred unsafe_dump_var(var(T)::in(any)) is det.

	% var__is_ground/2 can be used to test if a variable is ground.
	%
	% Declaratively, is_ground(Var, Result) is true iff
	% either Result = no or Var = var(Value) and Result = yes(Value);
	% that is, it is equivalent to the following clauses:
	%
	%	is_ground(var(Value), yes(Value)).
	%	is_ground(_, no).
	%
	% Operationally, is_ground(Var, Result) returns Result = no
	% if Var is non-ground, and Result = yes(Value) if Var is ground;
	% that is, execution will select the first clause if the variable
	% is ground, and the second clause if the variable is non-ground.
	%
	% Beware that is_ground is, and must be, `cc_multi';
	% making it `det' would not be safe.

:- pred is_ground(var(T)::in(any), maybe(T)::out) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%
:- import_module bool.
:- import_module unsafe, io.
:- import_module require.

:- pragma c_header_code("#include <stdio.h>").

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

	% We use '$VAR'/1 as the name for the functor for variable
	% representations simply because that name is "traditional".
	% (Note that the representation can be printed out, if you call
	% io__write(Var), so this is not entirely hidden from the user.)
:- type var(T) ---> '$VAR'(var_rep(T))
	where equality is (==).

:- type var_rep(T)
	--->	free
	;	free(delayed_conj(T))
	;	alias(var_rep(T))
	;	ground(T).

:- inst var_rep_any =
	bound(	free
	;	free(delayed_conj)
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
	bound(	free(delayed_conj)
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

% The type `delayed_conj(T)' represents a conjunction of delayed goals
% that are delayed on a variable of type T.

:- type delayed_conj(T)
	/* Warning: the layout of this type must match its layout in C */
	--->	goal(delayed_goal(T), bool, delayed_conj(T), delayed_conj(T))
			% the goal, `yes' if the goal has been woken,
			% and a pointer to the previous and next delayed goals
	;	(delayed_conj(T), delayed_conj(T)).
:- inst delayed_conj =
	bound(	goal(delayed_goal, ground, delayed_goal_list, delayed_goal_list)
	;	(delayed_conj, delayed_conj)
	).
:- inst delayed_goal_list =
	bound(	goal(delayed_goal, ground, delayed_goal_list, delayed_goal_list)
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
			pred(T, t2),
			type_info_for_t2, var(t2))

	;	/* some [T2] binary_pred(pred(T, T2), var(T2)) */
		binary_semidet_pred(
			pred(T, t2),
			type_info_for_t2, var(t2))

	;	/* some [T2]
		   binary_semidet_pred_any(pred(T, var(T2)), var(T2)) */
		binary_semidet_pred_any(
			pred(T, var(t2)),
			type_info_for_t2, var(t2))
	.

:- inst delayed_goal
	--->	unary_pred(pred(in) is semidet)
	;	binary_det_pred(
			pred(in, out) is det,
			ground, any)
	;	binary_semidet_pred(
			pred(in, out) is semidet,
			ground, any)
	;	binary_semidet_pred_any(
			pred(in, in(any)) is semidet,
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

:- pred var__rep_init_with_value(T::in, var_rep(T)::out(ptr(var_rep_ground)))
	is det.
:- pragma export(var__rep_init_with_value(in, out(ptr(var_rep_ground))),
	"ML_var_init_with_value").
var__rep_init_with_value(Value, alias(ground(Value))).

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

:- impure pred var__rep_unify_with_val(T, var_rep(T)).
:-        mode var__rep_unify_with_val(in, in(ptr(var_rep_any))) is semidet.
:- pragma export(var__rep_unify_with_val(in, in(ptr(var_rep_any))),
	"ML_var_unify_with_val").
var__rep_unify_with_val(Value, VarPtr) :-
	VarPtr = alias(Var),
	( 
		Var = alias(_),
		impure var__rep_unify_with_val(Value, Var)
	;
		Var = ground(OldValue),
		Value = OldValue
	;
		Var = free,
		impure destructively_update_binding(VarPtr, ground(Value))
	;
		Var = free(DelayedGoals),
		impure destructively_update_binding(VarPtr, ground(Value)),
		impure wakeup_delayed_goals(DelayedGoals, Value)
	).

:- pragma c_code( is_ground(Var::in(any), Result::out) /* cc_multi */,
	may_call_mercury,
"
	ML_var_is_ground(TypeInfo_for_T, Var, &Result);
").

:- pred var__rep_is_ground(var_rep(T), maybe(T)).
:- mode var__rep_is_ground(in(ptr(var_rep_any)), out) is det.
:- pragma export(var__rep_is_ground(in(ptr(var_rep_any)), out),
	"ML_var_is_ground").
var__rep_is_ground(VarPtr, Result) :-
	VarPtr = alias(Var),
	( 
		Var = alias(_),
		var__rep_is_ground(Var, Result)
	;
		Var = ground(Value),
		Result = yes(Value)
	;
		Var = free,
		Result = no
	;
		Var = free(_DelayedGoals),
		Result = no
	).

%-----------------------------------------------------------------------------%

%
% To allow detection of floundering,
% we keep a global doubly-linked list of all delayed goals,
% ordered from oldest to newest.
% To simplify the code to insert/delete from this list,
% we keep two dummy nodes, one for the start and one for the end of the list.
%

:- pragma c_header_code("
	/* Warning: the layout of this type must match its layout in Mercury */
	typedef struct ML_var_delayed_conj_struct {
		MR_Word goal;
		MR_Word woken;
		struct ML_var_delayed_conj_struct *prev;
		struct ML_var_delayed_conj_struct *next;
	} ML_var_delayed_conj;
	extern ML_var_delayed_conj ML_var_first_goal, ML_var_last_goal;
").

:- pragma c_code("
	ML_var_delayed_conj ML_var_first_goal = {
		0,
		MR_FALSE,
		NULL,
		&ML_var_last_goal
	};
	ML_var_delayed_conj ML_var_last_goal = {
		0,
		MR_FALSE,
		&ML_var_first_goal,
		NULL
	};
").

:- semipure
   pred get_last_delayed_goal(delayed_conj(_)::out(delayed_goal_list))
	is det.
:- pragma c_code(get_last_delayed_goal(Ptr::out(delayed_goal_list)),
	will_not_call_mercury,
"
	Ptr = (MR_Word) &ML_var_last_goal;
").

:- impure
   pred set_last_delayed_goal_prev(delayed_conj(_)::in(delayed_goal_list))
	is det.
:- pragma c_code(set_last_delayed_goal_prev(Ptr::in(delayed_goal_list)),
	will_not_call_mercury,
"
	MR_trail_function(ML_var_untrail_func, ML_var_last_goal.prev);
	ML_var_last_goal.prev = (void *) Ptr;
").

%-----------------------------------------------------------------------------%

:- impure pred new_delayed_goal(delayed_goal(T), delayed_conj(T)).
:-        mode new_delayed_goal(in(delayed_goal), out(delayed_conj)) is det.

new_delayed_goal(Pred, Goal) :-
	%
	% insert Pred at the end of the global list of delayed goals
	%
	semipure get_last_delayed_goal(LastGoal),
	LastGoal = goal(_, _LastWoken, LastPrev, _LastNext),
	Goal = goal(Pred, no, LastPrev, LastGoal),
	impure setarg(LastPrev, 4, Goal),	 % LastPrev->next := Goal
	impure set_last_delayed_goal_prev(Goal). % LastGoal->prev := Goal

:- impure pred wakeup_delayed_goals(delayed_conj(T), T).
:-        mode wakeup_delayed_goals(in(delayed_conj), in) is semidet.

wakeup_delayed_goals(Goal, Value) :-
	Goal = goal(DelayedGoal, _Woken, Prev, Next),
	%
	% Delete the goal from the global list of delayed goals,
	% and mark it as woken.
	%
	impure setarg(Goal, 2, yes),	% Goal->woken := yes
	impure setarg(Next, 3, Prev),	% Next->prev := Prev
	impure setarg(Prev, 4, Next),	% Prev->next := Next
	%
	% Call it.
	%
	call_delayed_goal(DelayedGoal, Value).

wakeup_delayed_goals((GoalsX, GoalsY), Value) :-
	impure wakeup_delayed_goals(GoalsX, Value),
	impure wakeup_delayed_goals(GoalsY, Value).

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
	#include ""mercury_trail.h""

	void ML_var_untrail_func(
		ML_var_delayed_conj *old_delayed_goals,
		MR_untrail_reason reason);
").

:- pragma c_code("

static void
ML_var_report_goal_floundered(ML_var_delayed_conj *old_goal);

void
ML_var_untrail_func(ML_var_delayed_conj *old_goal, MR_untrail_reason reason)
{
	switch (reason) {
		case MR_exception:
		case MR_undo:
		case MR_retry:
			/* just undo the update */
			ML_var_last_goal.prev = old_goal;
			break;

		case MR_commit:
		case MR_solve:
			/*
			** Skip past any goals that were created before
			** the choice point which we're committing over,
			** but which we have since woken up.
			*/
			while (old_goal->woken) {
				old_goal = old_goal->prev;
			}
			/*
			** `old_goal' now points to the delayed goal that
			** was most recent at the time we created the choice
			** point.  If that is not the same as what is currently
			** at the end of the list of outstanding (unwoken)
			** delayed goals, then we must have created some
			** new delayed goals since the choice point started.
			** Since there are outstanding delayed goals, we
			** can't commit, so the goal flounders.
			*/
			if (old_goal != ML_var_last_goal.prev) {
				ML_var_report_goal_floundered(old_goal);
			}
			break;

		default:
			MR_fatal_error(""ML_var_untrail_func: ""
				""unknown untrail reason"");
	}
}

static void
ML_var_report_goal_floundered(ML_var_delayed_conj *old_goal)
{
	ML_var_delayed_conj *last = ML_var_last_goal.prev;
	int num_delayed_goals;

	/* XXX should improve error message */
	fflush(stdout);
	fprintf(stderr, ""var.m: warning: goal floundered.\n"");

	num_delayed_goals = 0; 
	while (last && last != old_goal) {
		if (!last->woken) {
			num_delayed_goals++;
		}
		last = last->prev;
	}
	fprintf(stderr, ""       %d outstanding delayed goal%s.\n"",
		num_delayed_goals, (num_delayed_goals == 1 ? """" : ""s""));
}
").

%-----------------------------------------------------------------------------%

:- pred call_delayed_goal(delayed_goal(T), T).
:- mode call_delayed_goal(in(delayed_goal), in) is semidet.

call_delayed_goal(unary_pred(Pred), Value) :-
	call(Pred, Value).
call_delayed_goal(binary_det_pred(Pred, _TypeInfo2, var(Arg2)), Value) :-
	call(Pred, Value, Arg2).
call_delayed_goal(binary_semidet_pred(Pred, _TypeInfo2, var(Arg2)), Value) :-
	call(Pred, Value, Arg2).
call_delayed_goal(binary_semidet_pred_any(Pred, _TypeInfo2, Arg2), Value) :-
	call(Pred, Value, Arg2).

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
	SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma c_code(
	do_freeze(Var::out(any), Pred::in(delayed_goal)) /* semidet */,
	may_call_mercury,
"
	ML_var_freeze_out(TypeInfo_for_T, &Var, Pred);
	SUCCESS_INDICATOR = MR_TRUE;
").


:- impure pred var__rep_freeze_out(var_rep(T), delayed_goal(T)).
:-        mode var__rep_freeze_out(out(ptr(var_rep_any)), in(delayed_goal))
	       is det.
:- pragma export(
	var__rep_freeze_out(out(ptr(var_rep_any)), in(delayed_goal)),
	"ML_var_freeze_out").

var__rep_freeze_out(Var, Pred) :-
	impure new_delayed_goal(Pred, Goal),
	Var = alias(free(Goal)).

:- impure
   pred var__rep_freeze_in(var_rep(T), delayed_goal(T)).
:- mode var__rep_freeze_in(in(ptr(var_rep_any)), in(delayed_goal)) is semidet.
:- pragma export(
	var__rep_freeze_in(in(ptr(var_rep_any)), in(delayed_goal)),
	"ML_var_freeze_in").

var__rep_freeze_in(VarPtr, Pred) :-
	VarPtr = alias(Var),
	(
		Var = alias(_),
		impure var__rep_freeze_in(Var, Pred)
	;
		Var = ground(Value),
		call_delayed_goal(Pred, Value)
	;
		Var = free,
		impure new_delayed_goal(Pred, Goal),
		NewVar = free(Goal),
		impure destructively_update_binding(VarPtr, NewVar)
	;
		Var = free(OldGoals),
		impure new_delayed_goal(Pred, Goal),
		NewVar = free((OldGoals, Goal)),
		impure destructively_update_binding(VarPtr, NewVar)
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
	MR_Word XVal, YVal;

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
	MR_Word XVal, YVal;

	/* don't delay, just call the pred */
	ML_var_get_value(TypeInfo_for_T1, X, &XVal);
	if (ML_var_call_semidet_pred(TypeInfo_for_T1, TypeInfo_for_T2,
			Pred, XVal, &YVal))
	{
		ML_var_init_with_value(TypeInfo_for_T2, YVal, &Y);
		SUCCESS_INDICATOR = MR_TRUE;
	} else {
		SUCCESS_INDICATOR = MR_FALSE;
	}
}").
:- pragma c_code(
	freeze(X::out(any), Pred::(pred(in, out) is det), Y::out(any)),
		% semidet
	may_call_mercury,
"{
	MR_Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_det_pred(TypeInfo_for_T1, Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = MR_TRUE;
}").
:- pragma c_code(
	freeze(X::out(any), Pred::(pred(in, out) is semidet), Y::out(any)),
		% semidet
	may_call_mercury,
"{
	MR_Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_semidet_pred(TypeInfo_for_T1,
			Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = MR_TRUE;
}").

:- pragma c_code(
	freeze_var(X::out(any), Pred::(pred(in, in(any)) is semidet),
		Y::out(any)), % semidet
	may_call_mercury,
"{
	MR_Word p;

	Y = ML_var_alias(TypeInfo_for_T2, ML_var_free(TypeInfo_for_T2));
	p = ML_var_binary_semidet_pred_any(TypeInfo_for_T1,
			Pred, TypeInfo_for_T2, Y);
	ML_var_freeze_out(TypeInfo_for_T1, &X, p);
	SUCCESS_INDICATOR = MR_TRUE;
}").

%-----------------------------------------------------------------------------%

% The following code just exports the constructors for the type
% delayed_goal/1 to C.

:- func var_binary_det_pred(pred(T, t2), type_info_for_t2, var(t2))
	= delayed_goal(T).
:- mode var_binary_det_pred(pred(in, out) is det, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_det_pred(pred(in, out) is det, in, in(any)) =
		out(delayed_goal), "ML_var_binary_det_pred").
var_binary_det_pred(Pred, TypeInfo, SecondArg) =
	binary_det_pred(Pred, TypeInfo, SecondArg).

:- func var_binary_semidet_pred(pred(T, t2), type_info_for_t2, var(t2))
	= delayed_goal(T).
:- mode var_binary_semidet_pred(pred(in, out) is semidet, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_semidet_pred(pred(in, out) is semidet, in, in(any)) =
		out(delayed_goal), "ML_var_binary_semidet_pred").
var_binary_semidet_pred(Pred, TypeInfo, SecondArg) =
	binary_semidet_pred(Pred, TypeInfo, SecondArg).

:- func var_binary_semidet_pred_any(
		pred(T, var(t2)),
		type_info_for_t2, var(t2)) = delayed_goal(T).
:- mode var_binary_semidet_pred_any(
		pred(in, in(any)) is semidet, in, in(any)) =
		out(delayed_goal) is det.
:- pragma export(
	var_binary_semidet_pred_any(
		pred(in, in(any)) is semidet, in, in(any)) =
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

:- impure
   pred var__rep_unify(var_rep(T), var_rep(T)).
:- mode var__rep_unify(in(ptr(var_rep_any)), in(ptr(var_rep_any))) is semidet.
:- pragma export(var__rep_unify(in(ptr(var_rep_any)), in(ptr(var_rep_any))),
	"ML_var_unify").
var__rep_unify(XPtr, YPtr) :-
	XPtr = alias(X),
	(
		X = alias(_),
		impure var__rep_unify(X, YPtr)
	;
		X = free,
		( impure identical(XPtr, YPtr) ->
			true
		;
			impure destructively_update_binding(XPtr, YPtr)
		)
	;
		X = ground(_),
		impure var__rep_unify_gr(X, YPtr)
	;
		X = free(_),
		impure var__rep_unify_fr(XPtr, YPtr, X)
	).

	% This is the case when the first var is ground
:- impure
   pred var__rep_unify_gr(var_rep(T), var_rep(T)).
:- mode var__rep_unify_gr(in(var_rep_deref_ground), in(ptr(var_rep_any)))
	is semidet.
var__rep_unify_gr(X, YPtr) :-
	YPtr = alias(Y),
	(
		Y = alias(_),
		impure var__rep_unify_gr(X, Y)
	;
		Y = ground(Value),
		X = ground(Value)
	;
		Y = free,
		impure destructively_update_binding(YPtr, X)
	;
		Y = free(DelayedGoals),
		X = ground(Value),
		impure wakeup_delayed_goals(DelayedGoals, Value),
		impure destructively_update_binding(YPtr, X)
	).

	% This is the case when the first var is free(DelayedGoals).
:- impure
   pred var__rep_unify_fr(var_rep(T), var_rep(T), var_rep(T)).
:- mode var__rep_unify_fr(in(ptr(var_rep_any)), % really deref_delayed
			in(ptr(var_rep_any)),
			in(var_rep_deref_delayed)) is semidet.
var__rep_unify_fr(XPtr, YPtr, X) :-
	YPtr = alias(Y),
	(
		Y = alias(_),
		impure var__rep_unify_fr(XPtr, Y, X)
	;
		Y = free,
		impure destructively_update_binding(YPtr, X)
	;
		Y = ground(Value),
		X = free(XGoals),
		impure wakeup_delayed_goals(XGoals, Value),
		impure destructively_update_binding(XPtr, Y)
	;
		Y = free(YGoals),
		X = free(XGoals),
		( impure identical(XPtr, YPtr) ->
			true
		;
			XY = free((XGoals, YGoals)),
			impure destructively_update_binding(XPtr, XY),
			impure destructively_update_binding(YPtr, XY)
		)
	).

%-----------------------------------------------------------------------------%

:- impure pred identical(var_rep(T), var_rep(T)).
:- mode identical(in(ptr(var_rep_any)), in(ptr(var_rep_any))) is semidet.

:- pragma c_code(identical(X::in(ptr(var_rep_any)), Y::in(ptr(var_rep_any))),
		will_not_call_mercury,
"{
	SUCCESS_INDICATOR = (X == Y);
}").

%-----------------------------------------------------------------------------%

:- impure pred destructively_update_binding(var_rep(T), var_rep(T)).
:- mode destructively_update_binding(in(ptr(var_rep_any)), in(var_rep_any))
	is det.

destructively_update_binding(VarPtr, NewBinding) :-
	impure setarg(VarPtr, 1, NewBinding).

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
:- impure pred setarg(T1, int, T2).
:-        mode setarg(in(any), in, in(any)) is det.

:- pragma c_code(
	setarg(MercuryTerm::in(any), ArgNum::in, NewValue::in(any)),
	will_not_call_mercury,
"{
	/* strip off tag bits */
	MR_Word *ptr = (MR_Word *) MR_strip_tag(MercuryTerm);
	MR_trail_current_value(&ptr[ArgNum - 1]);
	ptr[ArgNum - 1] = NewValue;
}").

% untrailed_setarg/3 is similar to setarg/3 except the update is not
% trailed, so it will not be undone on backtracking.
:- pred untrailed_setarg(T1, int, T2).
:- mode untrailed_setarg(in(any), in, in(any)) is det.

:- pragma c_code(
	untrailed_setarg(MercuryTerm::in(any), ArgNum::in, NewValue::in(any)),
	will_not_call_mercury,
"{
	/* strip off tag bits */
	MR_Word *ptr = (MR_Word *) MR_strip_tag(MercuryTerm);
	ptr[ArgNum - 1] = NewValue;
}").

%-----------------------------------------------------------------------------%

:- pragma no_inline(debug_freeze/3).

debug_freeze(Msg, Var, Pred) :-
	init(Var),
	(
		impure unsafe_perform_io(print("freezing: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(Var),
		impure unsafe_perform_io(nl),

		freeze(Var, debug_pred(Msg, Pred)),

		impure unsafe_perform_io(print("frozen: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(Var),
		impure unsafe_perform_io(nl)
	;
		impure unsafe_perform_io(print("freeze failed: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(Var),
		impure unsafe_perform_io(nl),
		fail
	).

:- pragma no_inline(debug_freeze/4).

debug_freeze(Msg, X, Pred, Y) :-
	init(X),
	(
		impure unsafe_perform_io(print("freezing: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(X),
		impure unsafe_perform_io(nl),

		freeze(X, debug_pred2(Msg, Pred), Y),

		impure unsafe_perform_io(print("frozen: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(X),
		impure unsafe_perform_io(nl)
	;
		impure unsafe_perform_io(print("freeze failed: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_dump_var(X),
		impure unsafe_perform_io(nl),
		fail
	).

:- /* impure */
   pred debug_pred(string, pred(T), T) is semidet.
:- mode debug_pred(in, pred(in) is semidet, in) is semidet.

	% XXX the `pragma promise_pure' here is a lie,
	% but it's needed, because currently Mercury doesn't
	% support taking the address of an impure procedure.
	% The `pragma no_inline' is intended to reduce the
	% likelihood of the false `pragma promise_pure' causing
	% trouble.
:- pragma promise_pure(debug_pred/3).
:- pragma no_inline(debug_pred/3).

debug_pred(Msg, Pred, Var) :-
	impure unsafe_perform_io(print("woke: ")),
	impure unsafe_perform_io(print(Msg)),
	impure unsafe_perform_io(print(": ")),
	impure unsafe_perform_io(print(Var)),
	impure unsafe_perform_io(nl),
	( call(Pred, Var) ->
		impure unsafe_perform_io(print("succeeded: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_perform_io(print(Var)),
		impure unsafe_perform_io(nl)
	;
		impure unsafe_perform_io(print("failed: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_perform_io(print(Var)),
		impure unsafe_perform_io(nl),
		semidet_fail
	).

:- /* impure */
   pred debug_pred2(string, pred(T1, T2), T1, T2) is semidet.
:- mode debug_pred2(in, pred(in, out) is semidet, in, out) is semidet.

	% XXX the `pragma promise_pure' here is a lie,
	% but it's needed, because currently Mercury doesn't
	% support taking the address of an impure procedure.
	% The `pragma no_inline' is intended to reduce the
	% likelihood of the false `pragma promise_pure' causing
	% trouble.
:- pragma promise_pure(debug_pred2/4).
:- pragma no_inline(debug_pred2/4).

debug_pred2(Msg, Pred, X, Y) :-
	impure unsafe_perform_io(print("woke: ")),
	impure unsafe_perform_io(print(Msg)),
	impure unsafe_perform_io(print(": ")),
	impure unsafe_perform_io(print(X)),
	impure unsafe_perform_io(nl),
	(	call(Pred, X, Y),
		impure unsafe_perform_io(print("succeeded: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_perform_io(print(X)),
		impure unsafe_perform_io(print(", ")),
		impure unsafe_perform_io(print(Y)),
		impure unsafe_perform_io(nl)
	;
		impure unsafe_perform_io(print("failed: ")),
		impure unsafe_perform_io(print(Msg)),
		impure unsafe_perform_io(print(": ")),
		impure unsafe_perform_io(print(X)),
		impure unsafe_perform_io(nl),
		fail
	).

:- pragma c_code(dump_var(Var::in(any), IO0::di, IO::uo), may_call_mercury, "
	ML_var_print(TypeInfo_for_T, Var);
	IO = IO0;
").

:- pragma c_code(unsafe_dump_var(Var::in(any)), may_call_mercury, "
	ML_var_print(TypeInfo_for_T, Var);
").

:- pred dump_var_rep(var_rep(T)::in(var_rep_any),
		io__state::di, io__state::uo) is det.
:- pragma export(dump_var_rep(in(var_rep_any), di, uo), "ML_var_print").

dump_var_rep(alias(Var)) -->
	print("alias("), dump_var_rep(Var), print(")").
dump_var_rep(ground(Val)) -->
	print("ground("), print(Val), print(")").
dump_var_rep(free) -->
	print("free").
dump_var_rep(free(Goals)) -->
	print("free("), dump_goals(Goals), print(")").

:- pred dump_goals(delayed_conj(T)::in(delayed_conj),
		io__state::di, io__state::uo) is det.
dump_goals((A,B)) -->
	print("("), dump_goals(A), print(", "), dump_goals(B), print(")").
dump_goals(goal(_, Woken, _, _)) -->
	( { Woken = yes } ->
		print("<woken goal>")
	;
		print("<delayed goal>")
	).

%-----------------------------------------------------------------------------%
