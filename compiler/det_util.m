%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Utility predicates used in two or more of the modules concerned with
% determinism: switch_detection, cse_detection, det_analysis, det_report
% and simplify.
%
% Main authors: fjh and zs.
%
%-----------------------------------------------------------------------------%

:- module det_util.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, globals.
:- import_module set.

:- type maybe_changed	--->	changed ; unchanged.

:- type det_info.

	% Given a goal and an initial instmap, compute the final instmap that
	% results from the initial instmap after execution of the goal.

:- pred update_instmap(hlds__goal, instmap, instmap).
:- mode update_instmap(in, in, out) is det.

	% Update the current substitution to account for the effects
	% of the given unification.

:- pred interpret_unify(var, unify_rhs, substitution, substitution).
:- mode interpret_unify(in, in, in, out) is semidet.

	% Look up the determinism of a procedure.

:- pred det_lookup_detism(det_info, pred_id, proc_id, determinism).
:- mode det_lookup_detism(in, in, in, out) is det.

:- pred det_get_proc_info(det_info, proc_info).
:- mode det_get_proc_info(in, out) is det.

:- pred det_lookup_var_type(module_info, proc_info, var, hlds__type_defn).
:- mode det_lookup_var_type(in, in, in, out) is semidet.

:- pred no_output_vars(set(var), instmap, instmap_delta, det_info).
:- mode no_output_vars(in, in, in, in) is semidet.

:- pred det_info_init(module_info, pred_id, proc_id, globals, det_info).
:- mode det_info_init(in, in, in, in, out) is det.

:- pred det_info_get_module_info(det_info, module_info).
:- mode det_info_get_module_info(in, out) is det.

:- pred det_info_get_pred_id(det_info, pred_id).
:- mode det_info_get_pred_id(in, out) is det.

:- pred det_info_get_proc_id(det_info, proc_id).
:- mode det_info_get_proc_id(in, out) is det.

:- pred det_info_get_reorder_conj(det_info, bool).
:- mode det_info_get_reorder_conj(in, out) is det.

:- pred det_info_get_reorder_disj(det_info, bool).
:- mode det_info_get_reorder_disj(in, out) is det.

:- pred det_info_get_fully_strict(det_info, bool).
:- mode det_info_get_fully_strict(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module inst_match, mode_util, type_util, options.
:- import_module map, term, require, std_util.

update_instmap(_Goal0 - GoalInfo0, InstMap0, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	apply_instmap_delta(InstMap0, DeltaInstMap, InstMap).

interpret_unify(X, var(Y), Subst0, Subst) :-
	term__unify(term__variable(X), term__variable(Y), Subst0, Subst).
interpret_unify(X, functor(Functor, ArgVars), Subst0, Subst) :-
	term__context_init(Context),
	term__var_list_to_term_list(ArgVars, ArgTerms),
	term__unify(term__variable(X),
		term__functor(Functor, ArgTerms, Context),
		Subst0, Subst).
interpret_unify(_X, lambda_goal(_LambdaVars, _Modes, _Det, _Goal),
		Subst0, Subst) :-
		% For ease of implementation we just ignore unifications with
		% lambda terms.  This is a safe approximation, it just
		% prevents us from optimizing them as well as we would like.
	Subst = Subst0.

det_lookup_detism(DetInfo, PredId, ModeId, Detism) :-
	det_info_get_module_info(DetInfo, ModuleInfo),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism).

det_get_proc_info(DetInfo, ProcInfo) :-
	det_info_get_module_info(DetInfo, ModuleInfo),
	det_info_get_pred_id(DetInfo, PredId),
	det_info_get_proc_id(DetInfo, ProcId),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo).

det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn) :-
	proc_info_vartypes(ProcInfo, VarTypes),
	map__lookup(VarTypes, Var, Type),
	( type_to_type_id(Type, TypeId, _) ->
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeId, TypeDefn)
	;
		error("cannot lookup the type of a variable")
	).

no_output_vars(_, _, unreachable, _).
no_output_vars(Vars, InstMap0, reachable(InstMapDelta), DetInfo) :-
	set__to_sorted_list(Vars, VarList),
	det_info_get_module_info(DetInfo, ModuleInfo),
	no_output_vars_2(VarList, InstMap0, InstMapDelta, ModuleInfo).

:- pred no_output_vars_2(list(var), instmap, instmapping, module_info).
:- mode no_output_vars_2(in, in, in, in) is semidet.

no_output_vars_2([], _, _, _).
no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, ModuleInfo) :-
	( map__search(InstMapDelta, Var, Inst) ->
		% The instmap delta contains the variable, but the variable may
		% still not be output, if the change is just an increase in
		% information rather than an increase in instantiatedness.
		% We use `inst_matches_binding' to check that the new inst
		% has only added information or lost uniqueness,
		% not bound anything.
		instmap_lookup_var(InstMap0, Var, Inst0),
		inst_matches_binding(Inst, Inst0, ModuleInfo)
	;
		true
	),
	no_output_vars_2(Vars, InstMap0, InstMapDelta, ModuleInfo).

%-----------------------------------------------------------------------------%

:- type det_info	--->	det_info(
					module_info,
					pred_id,	% the id of the proc
					proc_id, 	% currently processed
					bool,		% --reorder-conj
					bool,		% --reorder-disj
					bool		% --fully-strict
				).

det_info_init(ModuleInfo, PredId, ProcId, Globals, DetInfo) :-
	globals__lookup_bool_option(Globals, reorder_conj, ReorderConj),
	globals__lookup_bool_option(Globals, reorder_disj, ReorderDisj),
	globals__lookup_bool_option(Globals, fully_strict, FullyStrict),
	DetInfo = det_info(ModuleInfo, PredId, ProcId,
		ReorderConj, ReorderDisj, FullyStrict).

det_info_get_module_info(det_info(ModuleInfo, _, _, _, _, _), ModuleInfo).
det_info_get_pred_id(det_info(_, PredId, _, _, _, _), PredId).
det_info_get_proc_id(det_info(_, _, ProcId, _, _, _), ProcId).
det_info_get_reorder_conj(det_info(_, _, _, ReorderConj, _, _), ReorderConj).
det_info_get_reorder_disj(det_info(_, _, _, _, ReorderDisj, _), ReorderDisj).
det_info_get_fully_strict(det_info(_, _, _, _, _, FullyStrict), FullyStrict).
