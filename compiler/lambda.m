%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: lambda.m
% main author: fjh

% This module is a pass over the HLDS to deal with lambda expressions.
%
% Lambda expressions are converted into separate predicates, so for
% example we translate
%
%	:- pred p(int::in) is det.
%	p(X) :-
%		V__1 = lambda([Y::out] is nondet, q(Y, X))),
%		solutions(V__1, List),
%		...
%	:- pred q(int::out, int::in) is nondet.
%
% into
%
%	p(X) :-
%		V__1 = '__LambdaGoal__1'(X)
%		solutions(V__1, List),
%		...
%
%	:- pred '__LambdaGoal__1'(int::in, int::out) is nondet.
%	'__LambdaGoal__1'(X, Y) :- q(Y, X).

%-----------------------------------------------------------------------------%

:- module (lambda).
:- interface. 
:- import_module hlds.
:- import_module list, set, map.
:- import_module varset, term.
:- import_module prog_io.

:- pred lambda__process_pred(pred_id, module_info, module_info).
:- mode lambda__process_pred(in, in, out) is det.

:- pred lambda__transform_lambda(list(var), list(mode), determinism,
		set(var), hlds__goal, unification,
		varset, map(var, type), tvarset, module_info,
		unify_rhs, unification, module_info).
:- mode lambda__transform_lambda(in, in, in, in, in, in, in, in, in, in,
		out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, string, std_util, require.
:- import_module make_hlds.

:- type lambda_info --->
		lambda_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			tvarset,		% from the proc_info
			module_info
		).

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

lambda__process_pred(PredId, ModuleInfo0, ModuleInfo) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	lambda__process_procs(PredId, ProcIds, ModuleInfo0, ModuleInfo).

:- pred lambda__process_procs(pred_id, list(proc_id), module_info, module_info).
:- mode lambda__process_procs(in, in, in, out) is det.

lambda__process_procs(_PredId, [], ModuleInfo, ModuleInfo).
lambda__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0, ModuleInfo) :-
	lambda__process_proc(PredId, ProcId, ModuleInfo0, ModuleInfo1),
	lambda__process_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

:- pred lambda__process_proc(pred_id, proc_id, module_info, module_info).
:- mode lambda__process_proc(in, in, in, out) is det.

lambda__process_proc(PredId, ProcId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	lambda__process_proc_2(ProcInfo0, PredInfo0, ModuleInfo0,
					ProcInfo, PredInfo1, ModuleInfo1),

	pred_info_procedures(PredInfo1, ProcTable1),
	map__set(ProcTable1, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	module_info_preds(ModuleInfo1, PredTable1),
	map__set(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo).

:- pred lambda__process_proc_2(proc_info, pred_info, module_info,
				proc_info, pred_info, module_info).
:- mode lambda__process_proc_2(in, in, in, out, out, out) is det.

lambda__process_proc_2(ProcInfo0, PredInfo0, ModuleInfo0,
				ProcInfo, PredInfo, ModuleInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_typevarset(PredInfo0, TypeVarSet0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),

	% process the goal
	Info0 = lambda_info(VarSet0, VarTypes0, TypeVarSet0, ModuleInfo0),
	lambda__process_goal(Goal0, Goal, Info0, Info),
	Info = lambda_info(VarSet, VarTypes, TypeVarSet, ModuleInfo),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_variables(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).

:- pred lambda__process_goal(hlds__goal, hlds__goal,
					lambda_info, lambda_info).
:- mode lambda__process_goal(in, out, in, out) is det.

lambda__process_goal(Goal0 - GoalInfo0, Goal) -->
	lambda__process_goal_2(Goal0, GoalInfo0, Goal).

:- pred lambda__process_goal_2(hlds__goal_expr, hlds__goal_info,
					hlds__goal, lambda_info, lambda_info).
:- mode lambda__process_goal_2(in, in, out, in, out) is det.

lambda__process_goal_2(unify(XVar, Y, Mode, Unification, Context), GoalInfo,
			Unify - GoalInfo) -->
	( { Y = lambda_goal(Vars, Modes, Det, LambdaGoal0) } ->
		% for lambda expressions, we must convert the lambda expression
		% into a new predicate
		{ LambdaGoal0 = _ - GoalInfo0 },
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals0) },
		lambda__process_lambda(Vars, Modes, Det, NonLocals0,
				LambdaGoal0, Unification, Y1, Unification1),
		{ Unify = unify(XVar, Y1, Mode, Unification1, Context) }
	;
		% ordinary unifications are left unchanged
		{ Unify = unify(XVar, Y, Mode, Unification, Context) }
	).

	% the rest of the clauses just process goals recursively

lambda__process_goal_2(conj(Goals0), GoalInfo, conj(Goals) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(disj(Goals0), GoalInfo, disj(Goals) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(switch(Var, CanFail, Cases0), GoalInfo, 
			switch(Var, CanFail, Cases) - GoalInfo) -->
	lambda__process_cases(Cases0, Cases).
lambda__process_goal_2(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(if_then_else(Vars, A0, B0, C0), GoalInfo,
			if_then_else(Vars, A, B, C) - GoalInfo) -->
	lambda__process_goal(A0, A),
	lambda__process_goal(B0, B),
	lambda__process_goal(C0, C).
lambda__process_goal_2(call(A,B,C,D,E,F,G), GoalInfo,
			call(A,B,C,D,E,F,G) - GoalInfo) -->
	[].
lambda__process_goal_2(pragma_c_code(A,B,C,D,E), GoalInfo,
			pragma_c_code(A,B,C,D,E) - GoalInfo) -->
	[].

:- pred lambda__process_goal_list(list(hlds__goal), list(hlds__goal),
					lambda_info, lambda_info).
:- mode lambda__process_goal_list(in, out, in, out) is det.

lambda__process_goal_list([], []) --> [].
lambda__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	lambda__process_goal(Goal0, Goal),
	lambda__process_goal_list(Goals0, Goals).

:- pred lambda__process_cases(list(case), list(case),
					lambda_info, lambda_info).
:- mode lambda__process_cases(in, out, in, out) is det.

lambda__process_cases([], []) --> [].
lambda__process_cases([case(ConsId, Goal0) | Cases0],
		[case(ConsId, Goal) | Cases]) -->
	lambda__process_goal(Goal0, Goal),
	lambda__process_cases(Cases0, Cases).

:- pred lambda__process_lambda(list(var), list(mode), determinism,
		set(var), hlds__goal, unification, unify_rhs, unification,
		lambda_info, lambda_info).
:- mode lambda__process_lambda(in, in, in, in, in, in, out, out,
		in, out) is det.

lambda__process_lambda(Vars, Modes, Det, OrigNonLocals0, LambdaGoal,
		Unification0, Functor, Unification, LambdaInfo0, LambdaInfo) :-
	LambdaInfo0 = lambda_info(VarSet, VarTypes, TVarSet, ModuleInfo0),
	lambda__transform_lambda(Vars, Modes, Det, OrigNonLocals0, LambdaGoal,
		Unification0, VarSet, VarTypes, TVarSet, ModuleInfo0,
		Functor, Unification, ModuleInfo),
	LambdaInfo = lambda_info(VarSet, VarTypes, TVarSet, ModuleInfo).

lambda__transform_lambda(Vars, Modes, Det, OrigNonLocals0, LambdaGoal,
		Unification0, VarSet, VarTypes, TVarSet, ModuleInfo0,
		Functor, Unification, ModuleInfo) :-
	(
		Unification0 = construct(Var0, _, _, UniModes0)
	->
		Var = Var0,
		UniModes = UniModes0
	;
		error("polymorphism__transform_lambda: wierd unification")
	),

	%
	% Optimize a special case: replace
	%	`lambda([Y1, Y2, ...] is Det, p(X1, X2, ..., Y1, Y2, ...))'
	% where `p' has determinism `Det' with
	%	`p(X1, X2, ...)'
	%
	% XXX This optimization is only valid if the modes of the Xi are
	% input, since only input arguments can be curried.
	% Until this check is added, this optimization is incorrect,
	% so I have disabled it - fjh.

	LambdaGoal = _ - LambdaGoalInfo,
	goal_info_get_nonlocals(LambdaGoalInfo, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals),
	set__to_sorted_list(NonLocals, ArgVars1),
	( 
/****************
XXX this optimization temporarily disabled, see comment above

		LambdaGoal = call(PredId0, ModeId0, CallVars, _, _, PredName, _)
					- _,
		list__remove_suffix(CallVars, Vars, InitialVars),

		module_info_pred_proc_info(ModuleInfo0, PredId0, ModeId0, _,
			Call_ProcInfo),
		proc_info_interface_code_model(Call_ProcInfo, Call_CodeModel),
		determinism_to_code_model(Det, CodeModel),
			% Check that the code models are compatible.
			% Note that det is not compatible with semidet,
			% and semidet is not compatible with nondet,
			% since the arguments go in different registers.
			% But det is compatible with nondet.
		( CodeModel = Call_CodeModel
		; CodeModel = model_non, Call_CodeModel = model_det
		)
	->
		ArgVars = InitialVars,
		PredId = PredId0,
		ModeId = ModeId0,
		unqualify_name(PredName, PName),
		ModuleInfo = ModuleInfo0
	;
****************/
		% Prepare to create a new predicate for the lambda
		% expression: work out the arguments, module name, predicate
		% name, arity, arg types, determinism,
		% context, status, etc. for the new predicate

		ArgVars = ArgVars1,
		list__append(ArgVars, Vars, AllArgVars),

		module_info_name(ModuleInfo0, ModuleName),
		module_info_next_lambda_count(ModuleInfo0, LambdaCount,
					ModuleInfo1),
		string__int_to_string(LambdaCount, LambdaCountStr),
		string__append("__LambdaGoal__", LambdaCountStr, PName0),
		string__append(ModuleName, PName0, PName),
		PredName = unqualified(PName),
		list__length(AllArgVars, Arity),
		map__apply_to_list(AllArgVars, VarTypes, ArgTypes),
		Cond = true,
		goal_info_context(LambdaGoalInfo, LambdaContext),
		Status = local,
		MaybeDet = yes(Det),
		% the TVarSet is a superset of what it really ought be,
		% but that shouldn't matter
		lambda__uni_modes_to_modes(UniModes, OrigArgModes),

		% We have to jump through hoops to work out the mode
		% of the lambda predicate. For introduced
		% type_info arguments, we use the mode "in".  For the original
		% non-local vars, we use the modes from `UniModes'.
		% For the lambda var arguments at the end,
		% we use the mode in the lambda expression. 

		list__length(ArgVars, NumArgVars),
		In = user_defined_mode(unqualified("in"), []),
		list__duplicate(NumArgVars, In, InModes),
		map__from_corresponding_lists(ArgVars, InModes,
			ArgModesMap),

		set__delete_list(OrigNonLocals0, Vars, OrigNonLocals),
		set__to_sorted_list(OrigNonLocals, OrigArgVars),
		map__from_corresponding_lists(OrigArgVars, OrigArgModes,
			OrigArgModesMap),
		map__overlay(ArgModesMap, OrigArgModesMap, ArgModesMap1),
		map__values(ArgModesMap1, ArgModes1),

		list__append(ArgModes1, Modes, AllArgModes),

		% 
		% Now construct the pred_info for the new predicate, using
		% the information computed above
		%
		clauses_info_init(Arity, ClausesInfo),
		pred_info_init(ModuleName, PredName, Arity, TVarSet,
			ArgTypes, Cond, LambdaContext, ClausesInfo, Status,
			no, none, predicate, PredInfo0),

		%	
		% Create a single mode for the new predicate, and insert
		% the lambda goal as the body of that procedure.
		%
		pred_info_procedures(PredInfo0, Procs0),
		next_mode_id(Procs0, MaybeDet, ModeId),
		proc_info_init(Arity, AllArgModes, MaybeDet, LambdaContext,
				ProcInfo0),
		proc_info_set_body(ProcInfo0, VarSet, VarTypes, AllArgVars,
				LambdaGoal, ProcInfo),
		map__set(Procs0, ModeId, ProcInfo, Procs),
		pred_info_set_procedures(PredInfo0, Procs, PredInfo),

		%
		% save the new predicate in the predicate table
		%
		module_info_get_predicate_table(ModuleInfo1, PredicateTable0),
		predicate_table_insert(PredicateTable0, PredInfo,
			PredId, PredicateTable),
		module_info_set_predicate_table(ModuleInfo1, PredicateTable,
			ModuleInfo)
	),
	Functor = functor(term__atom(PName), ArgVars),
	ConsId = pred_const(PredId, ModeId),
	Unification = construct(Var, ConsId, ArgVars, UniModes).

:- pred lambda__uni_modes_to_modes(list(uni_mode), list(mode)).
:- mode lambda__uni_modes_to_modes(in, out) is det.

	% This predicate works out the modes of the original non-local
	% variables of a lambda expression based on the list of uni_mode
	% in the unify_info for the lambda unification.

lambda__uni_modes_to_modes([], []).
lambda__uni_modes_to_modes([UniMode | UniModes], [Mode | Modes]) :-
	UniMode = ((_Initial0 - Initial1) -> (_Final0 - _Final1)),
	Mode = (Initial1 -> Initial1),
	lambda__uni_modes_to_modes(UniModes, Modes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
