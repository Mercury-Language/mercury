%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module inlining.
% Main author: conway.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds_module, llds.

:- pred inlining(module_info, module_info).
:- mode inlining(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_pred, hlds_goal, mode_util, goal_util, code_aux, prog_io.
:- import_module list, map, set, std_util, assoc_list.
:- import_module term, varset, require.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `inlining__do_inlining'
	% for each procedure body.

inlining(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	inlining_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred inlining_in_preds(list(pred_id), module_info, module_info).
:- mode inlining_in_preds(in, in, out) is det.

inlining_in_preds([], ModuleInfo, ModuleInfo).
inlining_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	inlining_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	inlining_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred inlining_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode inlining_in_procs(in, in, in, out) is det.

inlining_in_procs([], _PredId, ModuleInfo, ModuleInfo).
inlining_in_procs([ProcId | ProcIds], PredId, ModuleInfo0,
					ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),

	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo0,
		Goal, Varset, VarTypes),

	proc_info_set_variables(ProcInfo0, Varset, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	inlining_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal(hlds__goal, varset, map(var, type),
			module_info, hlds__goal, varset, map(var, type)).
:- mode inlining__inlining_in_goal(in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal(Goal0 - GoalInfo, Varset0, VarTypes0, ModuleInfo,
		Goal - GoalInfo, Varset, VarTypes) :-
	inlining__inlining_in_goal_2(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal_2(hlds__goal_expr, varset, map(var, type),
		module_info, hlds__goal_expr, varset, map(var, type)).
:- mode inlining__inlining_in_goal_2(in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal_2(conj(Goals0), Varset0, VarTypes0, ModuleInfo,
		conj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_conj(Goals0, Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(disj(Goals0), Varset0, VarTypes0, ModuleInfo,
		disj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_disj(Goals0, Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(switch(Var, Det, Cases0), Varset0, VarTypes0,
		ModuleInfo, switch(Var, Det, Cases), Varset, VarTypes) :-
	inlining__inlining_in_cases(Cases0, Varset0, VarTypes0, ModuleInfo,
		Cases, Varset, VarTypes).

inlining__inlining_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Varset0,
		VarTypes0, ModuleInfo, if_then_else(Vars, Cond, Then, Else),
		Varset, VarTypes) :-
	inlining__inlining_in_goal(Cond0, Varset0, VarTypes0, ModuleInfo,
		Cond, Varset1, VarTypes1),
	inlining__inlining_in_goal(Then0, Varset1, VarTypes1, ModuleInfo,
		Then, Varset2, VarTypes2),
	inlining__inlining_in_goal(Else0, Varset2, VarTypes2, ModuleInfo,
		Else, Varset, VarTypes).

inlining__inlining_in_goal_2(not(Goal0), Varset0, VarTypes0, ModuleInfo,
		not(Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(some(Vars, Goal0), Varset0, VarTypes0, ModuleInfo,
		some(Vars, Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Context, Sym, Follow),
		Varset0, VarTypes0, ModuleInfo, Goal, Varset, VarTypes) :-
	(
		% check to see if we are going to inline the call.
		% We do this if the called predicate has an annotation
		% indicating that it should be inlined, or if the goal
		% is a conjunction of builtins.
		\+ hlds__is_builtin_is_internal(Builtin),
        	module_info_preds(ModuleInfo, Preds),
        	map__lookup(Preds, PredId, PredInfo),
		\+ pred_info_is_imported(PredInfo),
			% this catches the case of locally defined
			% unification predicates for imported types.
		\+ (pred_info_is_pseudo_imported(PredInfo), ProcId = 0),
        	pred_info_procedures(PredInfo, Procs),
        	map__lookup(Procs, ProcId, ProcInfo),
        	proc_info_goal(ProcInfo, CalledGoal),
		(
			pred_info_is_inlined(PredInfo)
		;

				% this heuristic could be improved
			code_aux__contains_only_builtins(CalledGoal),
			code_aux__goal_is_flat(CalledGoal)
		)
	->
		proc_info_headvars(ProcInfo, HeadVars),
        	proc_info_variables(ProcInfo, PVarset),
		proc_info_vartypes(ProcInfo, CVarTypes),
		varset__vars(PVarset, Vars0),
		map__from_corresponding_lists(HeadVars, ArgVars, Subn0),
		goal_util__create_variables(Vars0, Varset0,
			VarTypes0, Subn0, CVarTypes, Varset, VarTypes, Subn),
		goal_util__rename_vars_in_goal(CalledGoal, Subn, Goal - _GInfo)
	;
		Goal = call(PredId, ProcId, ArgVars, Builtin, Context, Sym,
			Follow),
		Varset = Varset0,
		VarTypes = VarTypes0
	).

inlining__inlining_in_goal_2(unify(A,B,C,D,E), Varset, VarTypes, _,
					unify(A,B,C,D,E), Varset, VarTypes).

inlining__inlining_in_goal_2(
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		Varset, VarTypes, _, 
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_disj(list(hlds__goal), varset, map(var, type),
			module_info, list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_disj(in, in, in, in, out, out, out) is det.

inlining__inlining_in_disj([], Varset, VarTypes, _ModuleInfo,
				[], Varset, VarTypes).
inlining__inlining_in_disj([Goal0|Goals0], Varset0, VarTypes0, ModuleInfo,
		[Goal|Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset1, VarTypes1),
	inlining__inlining_in_disj(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_cases(list(case), varset, map(var, type), module_info,
		list(case), varset, map(var, type)).
:- mode inlining__inlining_in_cases(in, in, in, in, out, out, out) is det.

inlining__inlining_in_cases([], Varset, VarTypes, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_cases([case(Cons, Goal0)|Goals0], Varset0, VarTypes0,
		ModuleInfo, [case(Cons, Goal)|Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset1, VarTypes1),
	inlining__inlining_in_cases(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_conj(list(hlds__goal), varset, map(var, type),
		module_info, list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_conj(in, in, in, in, out, out, out) is det.

	% Since a single goal may become a conjunction,
	% we flatten the conjunction as we go.

inlining__inlining_in_conj([], Varset, VarTypes, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_conj([Goal0 | Goals0], Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal1, Varset1, VarTypes1),
	goal_to_conj_list(Goal1, Goal1List),
	inlining__inlining_in_conj(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals1, Varset, VarTypes),
	list__append(Goal1List, Goals1, Goals).

%-----------------------------------------------------------------------------%
