%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.

:- module inlining.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds_module, llds.
:- import_module io.

:- pred inlining(module_info, module_info, io__state, io__state).
:- mode inlining(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, globals, options.
:- import_module dead_proc_elim, type_util, mode_util, goal_util.
:- import_module passes_aux, code_aux.

:- import_module bool, int, list, assoc_list, map, set, std_util.
:- import_module term, varset, require.

%-----------------------------------------------------------------------------%

:- type inline_params	--->	params(bool, bool, int).
				% simple, single_use, threshold

	% Traverse the module structure, calling `inlining__do_inlining'
	% for each procedure body.

inlining(ModuleInfo0, ModuleInfo) -->
	{ dead_proc_elim__analyze(ModuleInfo0, NeededMap) },
	{ module_info_predids(ModuleInfo0, PredIds) },
	{ set__init(InlinedProcs0) },
	globals__io_lookup_bool_option(inline_simple, Simple),
	globals__io_lookup_bool_option(inline_single_use, SingleUse),
	globals__io_lookup_int_option(inline_threshold, Threshold),
	{ Params = params(Simple, SingleUse, Threshold) },
	inlining__mark_in_preds(PredIds, ModuleInfo0, NeededMap, Params,
		InlinedProcs0, InlinedProcs),
	{ inlining__in_preds(PredIds, InlinedProcs, ModuleInfo0, ModuleInfo) }.

:- pred inlining__mark_in_preds(list(pred_id), module_info, needed_map,
	inline_params, set(pred_proc_id), set(pred_proc_id),
	io__state, io__state).
:- mode inlining__mark_in_preds(in, in, in, in, in, out, di, uo) is det.

inlining__mark_in_preds([], _, _, _, InlinedProcs, InlinedProcs) --> [].
inlining__mark_in_preds([PredId | PredIds], ModuleInfo, NeededMap, Params,
		InlinedProcs0, InlinedProcs) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	inlining__mark_in_procs(ProcIds, PredId, ModuleInfo, NeededMap, Params,
		InlinedProcs0, InlinedProcs1),
	inlining__mark_in_preds(PredIds, ModuleInfo, NeededMap, Params,
		InlinedProcs1, InlinedProcs).

:- pred inlining__mark_in_procs(list(proc_id), pred_id, module_info,
	needed_map, inline_params, set(pred_proc_id), set(pred_proc_id),
	io__state, io__state).
:- mode inlining__mark_in_procs(in, in, in, in, in, in, out, di, uo) is det.

inlining__mark_in_procs([], _, _, _, _, InlinedProcs, InlinedProcs) --> [].
inlining__mark_in_procs([ProcId | ProcIds], PredId, ModuleInfo,
		NeededMap, Params, InlinedProcs0, InlinedProcs) -->
	(
		{ Params = params(Simple, SingleUse, Threshold) },
		{ PredProcId = proc(PredId, ProcId) },
		(
				% this heuristic could be improved
			{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
			{ pred_info_procedures(PredInfo, Procs) },
			{ map__lookup(Procs, ProcId, ProcInfo) },
			{ proc_info_goal(ProcInfo, CalledGoal) },
			(
				{ Simple = yes },
				{ inlining__simple_goal(CalledGoal) }
			;
				{ map__search(NeededMap, PredProcId, Needed) },
				{ Needed = yes(NumUses) },
				{ goal_size(CalledGoal, Size) },
				{ Size * NumUses =< Threshold }
			)
		;
			{ SingleUse = yes },
			{ map__search(NeededMap, PredProcId, Needed) },
			{ Needed = yes(NumUses) },
			{ NumUses = 1 }
		)
	->
		inlining__mark_proc_as_inlined(PredProcId, ModuleInfo,
			InlinedProcs0, InlinedProcs1)
	;
		{ InlinedProcs1 = InlinedProcs0 }
	),
	inlining__mark_in_procs(ProcIds, PredId, ModuleInfo, NeededMap, Params,
		InlinedProcs1, InlinedProcs).

:- pred inlining__simple_goal(hlds__goal).
:- mode inlining__simple_goal(in) is semidet.

inlining__simple_goal(Goal) :-
	(
		code_aux__contains_only_builtins(Goal),
		code_aux__goal_is_flat(Goal)
	;
		goal_size(Goal, Size),
		Size < 5
	).

:- pred inlining__mark_proc_as_inlined(pred_proc_id, module_info,
	set(pred_proc_id), set(pred_proc_id), io__state, io__state).
:- mode inlining__mark_proc_as_inlined(in, in, in, out, di, uo) is det.

inlining__mark_proc_as_inlined(proc(PredId, ProcId), ModuleInfo,
		InlinedProcs0, InlinedProcs) -->
	{ set__insert(InlinedProcs0, proc(PredId, ProcId), InlinedProcs) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	( { pred_info_is_inlined(PredInfo) } ->
		[]
	;
		write_progress_message("% Inlining a procedure of ",
			PredId, ModuleInfo)
	).

%-----------------------------------------------------------------------------%

:- pred inlining__in_preds(list(pred_id), set(pred_proc_id),
	module_info, module_info).
:- mode inlining__in_preds(in, in, in, out) is det.

inlining__in_preds([], _, ModuleInfo, ModuleInfo).
inlining__in_preds([PredId | PredIds], InlinedProcs, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	inlining__in_procs(ProcIds, PredId, InlinedProcs,
		ModuleInfo0, ModuleInfo1),
	inlining__in_preds(PredIds, InlinedProcs, ModuleInfo1, ModuleInfo).

:- pred inlining__in_procs(list(proc_id), pred_id, set(pred_proc_id),
	module_info, module_info).
:- mode inlining__in_procs(in, in, in, in, out) is det.

inlining__in_procs([], _PredId, _, ModuleInfo, ModuleInfo).
inlining__in_procs([ProcId | ProcIds], PredId, InlinedProcs,
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	pred_info_typevarset(PredInfo0, TypeVarSet),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),

	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TypeVarSet,
		ModuleInfo0, InlinedProcs, Goal, Varset, VarTypes),

	proc_info_set_variables(ProcInfo0, Varset, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	inlining__in_procs(ProcIds, PredId, InlinedProcs,
		ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal(hlds__goal, varset, map(var, type),
	tvarset, module_info, set(pred_proc_id), hlds__goal,
	varset, map(var, type)).
:- mode inlining__inlining_in_goal(in, in, in, in, in, in, out, out, out)
	is det.

inlining__inlining_in_goal(Goal0 - GoalInfo, Varset0, VarTypes0, TypeVarSet,
		ModuleInfo, InlinedProcs, Goal - GoalInfo, Varset, VarTypes) :-
	inlining__inlining_in_goal_2(Goal0, Varset0, VarTypes0, TypeVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal_2(hlds__goal_expr, varset, map(var, type),
	tvarset, module_info, set(pred_proc_id), hlds__goal_expr,
	varset, map(var, type)).
:- mode inlining__inlining_in_goal_2(in, in, in, in, in, in, out, out, out)
	is det.

inlining__inlining_in_goal_2(conj(Goals0), Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, conj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_conj(Goals0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(disj(Goals0, FV), Varset0, VarTypes0, TVarset,
		ModuleInfo, InlinedProcs, disj(Goals, FV), Varset, VarTypes) :-
	inlining__inlining_in_disj(Goals0, Varset0, VarTypes0, TVarset,
		ModuleInfo, InlinedProcs, Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(switch(Var, Det, Cases0, FV),
		Varset0, VarTypes0, TVarset, ModuleInfo, InlinedProcs,
		switch(Var, Det, Cases, FV), Varset, VarTypes) :-
	inlining__inlining_in_cases(Cases0, Varset0, VarTypes0, TVarset,
		ModuleInfo, InlinedProcs, Cases, Varset, VarTypes).

inlining__inlining_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV),
		Varset0, VarTypes0, TVarSet, ModuleInfo, InlinedProcs,
		if_then_else(Vars, Cond, Then, Else, FV), Varset, VarTypes) :-
	inlining__inlining_in_goal(Cond0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Cond, Varset1, VarTypes1),
	inlining__inlining_in_goal(Then0, Varset1, VarTypes1, TVarSet,
		ModuleInfo, InlinedProcs, Then, Varset2, VarTypes2),
	inlining__inlining_in_goal(Else0, Varset2, VarTypes2, TVarSet,
		ModuleInfo, InlinedProcs, Else, Varset, VarTypes).

inlining__inlining_in_goal_2(not(Goal0), Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, not(Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(some(Vars, Goal0), Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, some(Vars, Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Context, Sym, Follow),
		Varset0, VarTypes0, TypeVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset, VarTypes) :-

	% should we inline this call?
	(
		inlining__should_inline_proc(PredId, ProcId, Builtin,
			InlinedProcs, ModuleInfo)
	->
		% Yes.  So look up the info for the called procedure.

		module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			PredInfo, ProcInfo),
		pred_info_typevarset(PredInfo, CalleeTypeVarSet),
		proc_info_headvars(ProcInfo, HeadVars),
		proc_info_goal(ProcInfo, CalledGoal),
        	proc_info_variables(ProcInfo, CalleeVarset),
		proc_info_vartypes(ProcInfo, CalleeVarTypes0),

		% Substitute the appropriate types into the type
		% mapping of the called procedure.  For example, if we
		% call `:- pred foo(T)' with an argument of type
		% `int', then we need to replace all occurrences of
		% type `T' with type `int' when we inline it.

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
			% The head types should always subsume the
			% actual argument types, otherwise it is a type error
			% that should have been detected by typechecking
			% But polymorphism.m introduces type-incorrect code --
			% e.g. compare(Res, EnumA, EnumB) gets converted
			% into builtin_compare_int(Res, EnumA, EnumB), which
			% is a type error since it assumes that an enumeration
			% is an int.  In those cases, we don't need to
			% worry about the type substitution.
			CalleeVarTypes = CalleeVarTypes1
		),

		% Now rename apart the variables in the called goal.

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

inlining__inlining_in_goal_2(unify(A, B, C, D, E), Varset, VarTypes,
		_, _, _, unify(A, B, C, D, E), Varset, VarTypes).

inlining__inlining_in_goal_2(pragma_c_code(A, B, C, D, E), Varset, VarTypes,
		_, _, _, pragma_c_code(A, B, C, D, E), Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_disj(list(hlds__goal), varset, map(var, type),
	tvarset, module_info, set(pred_proc_id), list(hlds__goal),
	varset, map(var, type)).
:- mode inlining__inlining_in_disj(in, in, in, in, in, in, out, out, out)
	is det.

inlining__inlining_in_disj([], Varset, VarTypes, _, _, _, [], Varset, VarTypes).
inlining__inlining_in_disj([Goal0 | Goals0], Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, [Goal | Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset1, VarTypes1),
	inlining__inlining_in_disj(Goals0, Varset1, VarTypes1, TVarSet,
		ModuleInfo, InlinedProcs, Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_cases(list(case), varset, map(var, type),
	tvarset, module_info, set(pred_proc_id), list(case),
	varset, map(var, type)).
:- mode inlining__inlining_in_cases(in, in, in, in, in, in, out, out, out)
	is det.

inlining__inlining_in_cases([], Varset, VarTypes, _, _, _,
				[], Varset, VarTypes).
inlining__inlining_in_cases([case(Cons, Goal0) | Goals0],
		Varset0, VarTypes0, TVarSet, ModuleInfo, InlinedProcs,
		[case(Cons, Goal) | Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goal, Varset1, VarTypes1),
	inlining__inlining_in_cases(Goals0, Varset1, VarTypes1, TVarSet,
		ModuleInfo, InlinedProcs, Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_conj(list(hlds__goal), varset, map(var, type),
	tvarset, module_info, set(pred_proc_id), list(hlds__goal),
	varset, map(var, type)).
:- mode inlining__inlining_in_conj(in, in, in, in, in, in, out, out, out)
	is det.

	% Since a single goal may become a conjunction,
	% we flatten the conjunction as we go.

inlining__inlining_in_conj([], Varset, VarTypes, _, _, _, [], Varset, VarTypes).
inlining__inlining_in_conj([Goal0 | Goals0], Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goals, Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, TVarSet,
		ModuleInfo, InlinedProcs, Goal1, Varset1, VarTypes1),
	goal_to_conj_list(Goal1, Goal1List),
	inlining__inlining_in_conj(Goals0, Varset1, VarTypes1, TVarSet,
		ModuleInfo, InlinedProcs, Goals1, Varset, VarTypes),
	list__append(Goal1List, Goals1, Goals).

%-----------------------------------------------------------------------------%

	% Check to see if we should inline a call.
	%
	% Fails if the called predicate is a builtin or is imported.
	%
	% Succeeds if the called predicate has an annotation
	% indicating that it should be inlined, or if the goal
	% is a conjunction of builtins.

:- pred inlining__should_inline_proc(pred_id, proc_id, is_builtin,
	set(pred_proc_id), module_info).
:- mode inlining__should_inline_proc(in, in, in, in, in) is semidet.

inlining__should_inline_proc(PredId, ProcId, Builtin, InlinedProcs,
		ModuleInfo) :-

	% don't inline builtins, the code generator will handle them

	\+ hlds__is_builtin_is_internal(Builtin),

	% don't try to inline imported predicates, since we don't
	% have the code for them.
	% (We don't yet support cross-module inlining.)

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	\+ pred_info_is_imported(PredInfo),
		% this next line catches the case of locally defined
		% unification predicates for imported types.
	\+ (pred_info_is_pseudo_imported(PredInfo), ProcId = 0),

	% OK, we could inline it - but should we?  Apply our heuristic.
	(
		pred_info_is_inlined(PredInfo)
	;
		set__member(proc(PredId, ProcId), InlinedProcs)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
