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

:- import_module prog_io, hlds_pred, hlds_goal.
:- import_module type_util, mode_util, goal_util, code_aux.

:- import_module list, assoc_list, map, set, std_util.
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

	pred_info_typevarset(PredInfo0, TypeVarSet),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),

	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TypeVarSet,
		ModuleInfo0, Goal, Varset, VarTypes),

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
				tvarset, module_info,
				hlds__goal, varset, map(var, type)).
:- mode inlining__inlining_in_goal(in, in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal(Goal0 - GoalInfo, Varset0, VarTypes0,
				TypeVarSet, ModuleInfo,
				Goal - GoalInfo, Varset, VarTypes) :-
	inlining__inlining_in_goal_2(Goal0, Varset0, VarTypes0,
				TypeVarSet, ModuleInfo,
				Goal, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal_2(hlds__goal_expr, varset, map(var, type),
				tvarset, module_info,
				hlds__goal_expr, varset, map(var, type)).
:- mode inlining__inlining_in_goal_2(in, in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal_2(conj(Goals0), Varset0, VarTypes0,
			TVarSet, ModuleInfo,
			conj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_conj(Goals0, Varset0, VarTypes0,
			TVarSet, ModuleInfo,
			Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(disj(Goals0), Varset0, VarTypes0,
			TVarset, ModuleInfo,
			disj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_disj(Goals0, Varset0, VarTypes0,
			TVarset, ModuleInfo,
			Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(switch(Var, Det, Cases0), Varset0, VarTypes0,
			TVarset, ModuleInfo,
			switch(Var, Det, Cases), Varset, VarTypes) :-
	inlining__inlining_in_cases(Cases0, Varset0, VarTypes0,
			TVarset, ModuleInfo,
			Cases, Varset, VarTypes).

inlining__inlining_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Varset0,
		VarTypes0, TVarSet, ModuleInfo,
		if_then_else(Vars, Cond, Then, Else), Varset, VarTypes) :-
	inlining__inlining_in_goal(Cond0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Cond, Varset1, VarTypes1),
	inlining__inlining_in_goal(Then0, Varset1, VarTypes1,
		TVarSet, ModuleInfo, Then, Varset2, VarTypes2),
	inlining__inlining_in_goal(Else0, Varset2, VarTypes2,
		TVarSet, ModuleInfo, Else, Varset, VarTypes).

inlining__inlining_in_goal_2(not(Goal0), Varset0, VarTypes0,
		TVarSet, ModuleInfo, not(Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(some(Vars, Goal0), Varset0, VarTypes0,
		TVarSet, ModuleInfo, some(Vars, Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Context, Sym, Follow),
		Varset0, VarTypes0,
		TypeVarSet, ModuleInfo, Goal, Varset, VarTypes) :-
	%
	% should we inline this call?
	%
	( inlining__should_inline_proc(PredId, ProcId, Builtin, ModuleInfo) ->
		%
		% Yes.  So look up the info for the called procedure.
		%
		module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			PredInfo, ProcInfo),
		pred_info_typevarset(PredInfo, CalleeTypeVarSet),
		proc_info_headvars(ProcInfo, HeadVars),
		proc_info_goal(ProcInfo, CalledGoal),
        	proc_info_variables(ProcInfo, CalleeVarset),
		proc_info_vartypes(ProcInfo, CalleeVarTypes0),

		%
		% Substitute the appropriate types into the type
		% mapping of the called procedure.  For example, if we
		% call `:- pred foo(T)' with an argument of type
		% `int', then we need to replace all occurrences of
		% type `T' with type `int' when we inline it.
		%

		% first, rename apart the type variables in the callee.
		% (we can throw away the new typevarset, since
		% we are about to substitute away any new type variables)
		varset__merge_subst(TypeVarSet, CalleeTypeVarSet,
			_NewTypeVarSet, TypeRenaming),
		apply_substitution_to_type_map(CalleeVarTypes0, TypeRenaming,
			CalleeVarTypes1),
		% next, compute the type substitution and then apply it
		map__apply_to_list(HeadVars, CalleeVarTypes1, HeadTypes),
		map__apply_to_list(ArgVars, VarTypes0, ArgTypes),
		(
			type_list_subsumes(HeadTypes, ArgTypes, TypeSubn)
		->
			apply_rec_substitution_to_type_map(CalleeVarTypes1,
				TypeSubn, CalleeVarTypes)
		;
			% the head types should always subsume the
			% actual argument types, otherwise it is a type error
			% that should have been detected by typechecking
			error("inlining.m: subsumption check failed")
		),

		%
		% Now rename apart the variables in the called goal.
		%
		varset__vars(CalleeVarset, CalleeVars0),
		map__from_corresponding_lists(HeadVars, ArgVars, Subn0),
		goal_util__create_variables(CalleeVars0, Varset0,
			VarTypes0, Subn0, CalleeVarTypes, CalleeVarset,
				Varset, VarTypes, Subn),
		goal_util__must_rename_vars_in_goal(CalledGoal, Subn,
			Goal - _GInfo)
	;
		Goal = call(PredId, ProcId, ArgVars, Builtin, Context, Sym,
			Follow),
		Varset = Varset0,
		VarTypes = VarTypes0
	).

inlining__inlining_in_goal_2(unify(A,B,C,D,E), Varset, VarTypes, _, _,
					unify(A,B,C,D,E), Varset, VarTypes).

inlining__inlining_in_goal_2(
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		Varset, VarTypes, _, _,
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_disj(list(hlds__goal), varset, map(var, type),
			tvarset, module_info,
			list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_disj(in, in, in, in, in, out, out, out) is det.

inlining__inlining_in_disj([], Varset, VarTypes, _TVarSet, _ModuleInfo,
				[], Varset, VarTypes).
inlining__inlining_in_disj([Goal0|Goals0], Varset0, VarTypes0,
		TVarSet, ModuleInfo, [Goal|Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goal, Varset1, VarTypes1),
	inlining__inlining_in_disj(Goals0, Varset1, VarTypes1,
		TVarSet, ModuleInfo, Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_cases(list(case), varset, map(var, type),
		tvarset, module_info, list(case), varset, map(var, type)).
:- mode inlining__inlining_in_cases(in, in, in, in, in, out, out, out) is det.

inlining__inlining_in_cases([], Varset, VarTypes, _TVarSet, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_cases([case(Cons, Goal0)|Goals0], Varset0, VarTypes0,
		TVarSet, ModuleInfo, [case(Cons, Goal)|Goals], Varset,
		VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goal, Varset1, VarTypes1),
	inlining__inlining_in_cases(Goals0, Varset1, VarTypes1,
		TVarSet, ModuleInfo, Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_conj(list(hlds__goal), varset, map(var, type),
		tvarset, module_info, list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_conj(in, in, in, in, in, out, out, out) is det.

	% Since a single goal may become a conjunction,
	% we flatten the conjunction as we go.

inlining__inlining_in_conj([], Varset, VarTypes, _TVarSet, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_conj([Goal0 | Goals0], Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goals, Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0,
		TVarSet, ModuleInfo, Goal1, Varset1, VarTypes1),
	goal_to_conj_list(Goal1, Goal1List),
	inlining__inlining_in_conj(Goals0, Varset1, VarTypes1,
		TVarSet, ModuleInfo, Goals1, Varset, VarTypes),
	list__append(Goal1List, Goals1, Goals).

%-----------------------------------------------------------------------------%

	%
	% Check to see if we should inline a call.
	%
	% Fails if the called predicate is a builtin or is imported.
	%
	% Succeeds if the called predicate has an annotation
	% indicating that it should be inlined, or if the goal
	% is a conjunction of builtins.
	%
:- pred inlining__should_inline_proc(pred_id, proc_id, is_builtin,
					module_info).
:- mode inlining__should_inline_proc(in, in, in, in) is semidet.

inlining__should_inline_proc(PredId, ProcId, Builtin, ModuleInfo) :-
	%
	% don't inline builtins, the code generator will handle them
	%
	\+ hlds__is_builtin_is_internal(Builtin),

	%
	% don't try to inline imported predicates, since we don't
	% have the code for them. 
	% (We don't yet support cross-module inlining.)
	%
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	\+ pred_info_is_imported(PredInfo),
		% this next line catches the case of locally defined
		% unification predicates for imported types.
	\+ (pred_info_is_pseudo_imported(PredInfo), ProcId = 0),

	%
	% OK, we could inline it - but should we?  Apply our heuristic.
	%
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_goal(ProcInfo, CalledGoal),
	(
		pred_info_is_inlined(PredInfo)
	;
			% this heuristic could be improved
		code_aux__contains_only_builtins(CalledGoal),
		code_aux__goal_is_flat(CalledGoal)
	).

%-----------------------------------------------------------------------------%

	% apply a type substitution (i.e. map from tvar -> type)
	% to all the types in a variable typing (i.e. map from var -> type).

:- pred apply_substitution_to_type_map(map(var, type)::in, tsubst::in,
					map(var, type)::out) is det.

apply_substitution_to_type_map(VarTypes0, Subst, VarTypes) :-
	% optimize the common case of an empty type substitution
	( map__is_empty(Subst) ->
		VarTypes = VarTypes0
	;
		map__keys(VarTypes0, Vars),
		apply_substitution_to_type_map_2(Vars, VarTypes0, Subst,
			VarTypes)
	).

:- pred apply_substitution_to_type_map_2(list(var)::in, map(var, type)::in,
				tsubst::in, map(var, type)::out) is det.

apply_substitution_to_type_map_2([], VarTypes, _Subst, VarTypes).
apply_substitution_to_type_map_2([Var | Vars], VarTypes0, Subst,
		VarTypes) :-
	map__lookup(VarTypes0, Var, VarType0),
	term__apply_substitution(VarType0, Subst, VarType),
	map__det_update(VarTypes0, Var, VarType, VarTypes1),
	apply_substitution_to_type_map_2(Vars, VarTypes1, Subst, VarTypes).

%-----------------------------------------------------------------------------%

	% same thing as above, except for a recursive substitution
	% (i.e. we keep applying the substitution recursively until
	% there are no more changes).

:- pred apply_rec_substitution_to_type_map(map(var, type)::in, tsubst::in,
					map(var, type)::out) is det.

apply_rec_substitution_to_type_map(VarTypes0, Subst, VarTypes) :-
	% optimize the common case of an empty type substitution
	( map__is_empty(Subst) ->
		VarTypes = VarTypes0
	;
		map__keys(VarTypes0, Vars),
		apply_rec_substitution_to_type_map_2(Vars, VarTypes0, Subst,
			VarTypes)
	).

:- pred apply_rec_substitution_to_type_map_2(list(var)::in, map(var, type)::in,
				tsubst::in, map(var, type)::out) is det.

apply_rec_substitution_to_type_map_2([], VarTypes, _Subst, VarTypes).
apply_rec_substitution_to_type_map_2([Var | Vars], VarTypes0, Subst,
		VarTypes) :-
	map__lookup(VarTypes0, Var, VarType0),
	term__apply_rec_substitution(VarType0, Subst, VarType),
	map__det_update(VarTypes0, Var, VarType, VarTypes1),
	apply_rec_substitution_to_type_map_2(Vars, VarTypes1, Subst, VarTypes).

%-----------------------------------------------------------------------------%
