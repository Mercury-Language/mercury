%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% main author: fjh

% various utility predicates acting on the parse tree data
% structure defined in prog_data.m.

:- module prog_util.

:- interface.

:- import_module list, term, std_util.
:- import_module prog_data.

%-----------------------------------------------------------------------------%

	% Convert a sym_name into a string.

:- pred unqualify_name(sym_name, string).
:- mode unqualify_name(in, out) is det.

:- pred sym_name_get_module_name(sym_name, module_name, module_name).
:- mode sym_name_get_module_name(in, in, out) is det.

        % Given a possible module qualified sym_name and a list of
	% argument types and a context, construct a term. This is
	% used to construct types. 

:- pred construct_qualified_term(sym_name, list(term), term).
:- mode construct_qualified_term(in, in, out) is det.

:- pred construct_qualified_term(sym_name, list(term), term__context, term).
:- mode construct_qualified_term(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% A pred declaration may contains just types, as in
	%	:- pred list__append(list(T), list(T), list(T)).
	% or it may contain both types and modes, as in
	%	:- pred list__append(list(T)::in, list(T)::in,
	%			list(T)::output).
	%
	% This predicate takes the argument list of a pred declaration,
	% splits it into two separate lists for the types and (if present)
	% the modes.

:- type maybe_modes == maybe(list(mode)).

:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(in, out, out) is det.

:- pred split_type_and_mode(type_and_mode, type, maybe(mode)).
:- mode split_type_and_mode(in, out, out) is det.

%-----------------------------------------------------------------------------%

	% Perform a substitution on a goal.

:- pred prog_util__rename_in_goal(goal, var, var, goal).
:- mode prog_util__rename_in_goal(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module (inst).
:- import_module bool, std_util, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

construct_qualified_term(qualified(Module, Name), Args, Context, Term) :-
	ModuleTerm = term__functor(term__atom(Module), [], Context),
	UnqualifiedTerm = term__functor(term__atom(Name), Args, Context),
	Term = term__functor(term__atom(":"), [ModuleTerm, UnqualifiedTerm],
							Context).
construct_qualified_term(unqualified(Name), Args, Context, Term) :-
	Term = term__functor(term__atom(Name), Args, Context).


construct_qualified_term(SymName, Args, Term) :-
	term__context_init(Context),
	construct_qualified_term(SymName, Args, Context, Term).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
	(
		Result = yes
	->
		MaybeModes = yes(Modes)
	;
		MaybeModes = no
	).

:- pred split_types_and_modes_2(list(type_and_mode), bool,
				list(type), list(mode), bool).
:- mode split_types_and_modes_2(in, in, out, out, out) is det.

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the modes are ignored
	% - should this be an error instead?

:- pred split_type_and_mode(type_and_mode, bool, type, mode, bool).
:- mode split_type_and_mode(in, in, out, out, out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T,M), T, yes(M)).

%-----------------------------------------------------------------------------%

prog_util__rename_in_goal(Goal0 - Context, OldVar, NewVar, Goal - Context) :-
	prog_util__rename_in_goal_expr(Goal0, OldVar, NewVar, Goal).

:- pred prog_util__rename_in_goal_expr(goal_expr, var, var, goal_expr).
:- mode prog_util__rename_in_goal_expr(in, in, in, out) is det.

prog_util__rename_in_goal_expr((GoalA0, GoalB0), OldVar, NewVar,
		(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(true, _Var, _NewVar, true).
prog_util__rename_in_goal_expr((GoalA0; GoalB0), OldVar, NewVar,
		(GoalA; GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(fail, _Var, _NewVar, fail).
prog_util__rename_in_goal_expr(not(Goal0), OldVar, NewVar, not(Goal)) :-
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(some(Vars0, Goal0), OldVar, NewVar,
		some(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(all(Vars0, Goal0), OldVar, NewVar,
		all(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(implies(GoalA0, GoalB0), OldVar, NewVar,
		implies(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(equivalent(GoalA0, GoalB0), OldVar, NewVar,
		equivalent(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(if_then(Vars0, Cond0, Then0), OldVar, NewVar,
		if_then(Vars, Cond, Then)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then).
prog_util__rename_in_goal_expr(if_then_else(Vars0, Cond0, Then0, Else0),
		OldVar, NewVar, if_then_else(Vars, Cond, Then, Else)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then),
	prog_util__rename_in_goal(Else0, OldVar, NewVar, Else).
prog_util__rename_in_goal_expr(call(SymName, Terms0, Purity), OldVar, NewVar,
		call(SymName, Terms, Purity)) :-
	term__substitute_list(Terms0, OldVar, term__variable(NewVar), Terms).
prog_util__rename_in_goal_expr(unify(TermA0, TermB0), OldVar, NewVar,
		unify(TermA, TermB)) :-
	term__substitute(TermA0, OldVar, term__variable(NewVar), TermA),
	term__substitute(TermB0, OldVar, term__variable(NewVar), TermB).

:- pred prog_util__rename_in_vars(list(var), var, var, list(var)).
:- mode prog_util__rename_in_vars(in, in, in, out) is det.

prog_util__rename_in_vars([], _, _, []).
prog_util__rename_in_vars([Var0 | Vars0], OldVar, NewVar, [Var | Vars]) :-
	( Var0 = OldVar ->
		Var = NewVar
	;
		Var = Var0
	),
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars).

%-----------------------------------------------------------------------------%
