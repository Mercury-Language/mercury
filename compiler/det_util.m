%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2000,2002 The University of Melbourne.
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

:- module check_hlds__det_util.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module hlds__hlds_data, libs__globals.
:- import_module hlds__instmap, parse_tree__prog_data.
:- import_module bool, set, list.

:- type maybe_changed	--->	changed ; unchanged.

:- type det_info.

	% Given a goal and an initial instmap, compute the final instmap that
	% results from the initial instmap after execution of the goal.

:- pred update_instmap(hlds_goal, instmap, instmap).
:- mode update_instmap(in, in, out) is det.

	% Given a list of cases, and a list of the possible cons_ids
	% that the switch variable could be bound to, select out only
	% those cases whose cons_id occurs in the list of cases
	% We assume that the list of cases and the list of cons_ids
	% are sorted, so that we can do this using a simple sorted merge.

:- pred delete_unreachable_cases(list(case), list(cons_id),
	list(case)).
:- mode delete_unreachable_cases(in, in, out) is det.

	% Update the current substitution to account for the effects
	% of the given unification.

:- pred interpret_unify(prog_var, unify_rhs, prog_substitution,
		prog_substitution).
:- mode interpret_unify(in, in, in, out) is semidet.

	% Look up the determinism of a procedure.

:- pred det_lookup_detism(det_info, pred_id, proc_id, determinism).
:- mode det_lookup_detism(in, in, in, out) is det.

:- pred det_get_proc_info(det_info, proc_info).
:- mode det_get_proc_info(in, out) is det.

:- pred det_lookup_var_type(module_info, proc_info, prog_var, hlds_type_defn).
:- mode det_lookup_var_type(in, in, in, out) is semidet.

:- pred det_no_output_vars(set(prog_var), instmap, instmap_delta, det_info).
:- mode det_no_output_vars(in, in, in, in) is semidet.

:- pred det_info_init(module_info, vartypes, pred_id, proc_id, globals,
		det_info).
:- mode det_info_init(in, in, in, in, in, out) is det.

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

:- pred det_info_set_module_info(det_info, module_info, det_info).
:- mode det_info_set_module_info(in, in, out) is det.

:- pred det_info_get_vartypes(det_info, vartypes).
:- mode det_info_get_vartypes(in, out) is det.

:- pred det_info_set_vartypes(det_info, vartypes, det_info).
:- mode det_info_set_vartypes(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match, check_hlds__mode_util.
:- import_module check_hlds__type_util, libs__options, term.
:- import_module map, require, std_util.

update_instmap(_Goal0 - GoalInfo0, InstMap0, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	instmap__apply_instmap_delta(InstMap0, DeltaInstMap, InstMap).

delete_unreachable_cases([], _, []).
delete_unreachable_cases([_ | _], [], []).
delete_unreachable_cases([Case | Cases0], [ConsId | ConsIds], Cases) :-
	Case = case(CaseConsId, _DisjList),
	( CaseConsId = ConsId ->
		Cases = [Case | Cases1],
		delete_unreachable_cases(Cases0, ConsIds, Cases1)
	; compare(<, CaseConsId, ConsId) ->
		delete_unreachable_cases(Cases0, [ConsId | ConsIds], Cases)
	;
		delete_unreachable_cases([Case | Cases0], ConsIds, Cases)
	).

interpret_unify(X, var(Y), Subst0, Subst) :-
	term__unify(term__variable(X), term__variable(Y),
		Subst0, Subst).
interpret_unify(X, functor(ConsId, _, ArgVars), Subst0, Subst) :-
	term__var_list_to_term_list(ArgVars, ArgTerms),
	cons_id_and_args_to_term(ConsId, ArgTerms, RhsTerm),
	term__unify(term__variable(X), RhsTerm, Subst0, Subst).
interpret_unify(_X, lambda_goal(_POrF, _Method, _Fix, _NonLocals,
			_Vars, _Modes, _Det, _Goal), Subst0, Subst) :-
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
	( type_to_ctor_and_args(Type, TypeCtor, _) ->
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeCtor, TypeDefn)
	;
		error("cannot lookup the type of a variable")
	).

det_no_output_vars(Vars, InstMap, InstMapDelta, DetInfo) :-
	det_info_get_module_info(DetInfo, ModuleInfo),
	instmap__no_output_vars(InstMap, InstMapDelta, Vars, DetInfo^vartypes,
		ModuleInfo).

%-----------------------------------------------------------------------------%

:- type det_info	
	--->	det_info(
		module_info :: module_info,
		vartypes :: vartypes,
		pred_id :: pred_id,	% the id of the proc
		proc_id :: proc_id, 	% currently processed
		reorder_conj :: bool,	% --reorder-conj
		reorder_disj :: bool,	% --reorder-disj
		fully_strict :: bool	% --fully-strict
	).

det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo) :-
	globals__lookup_bool_option(Globals, reorder_conj, ReorderConj),
	globals__lookup_bool_option(Globals, reorder_disj, ReorderDisj),
	globals__lookup_bool_option(Globals, fully_strict, FullyStrict),
	DetInfo = det_info(ModuleInfo, VarTypes, PredId, ProcId,
		ReorderConj, ReorderDisj, FullyStrict).

det_info_get_module_info(DetInfo, DetInfo^module_info).
det_info_get_pred_id(DetInfo, DetInfo^pred_id).
det_info_get_proc_id(DetInfo, DetInfo^proc_id).
det_info_get_reorder_conj(DetInfo, DetInfo^reorder_conj).
det_info_get_reorder_disj(DetInfo, DetInfo^reorder_disj).
det_info_get_fully_strict(DetInfo, DetInfo^fully_strict).
det_info_get_vartypes(DetInfo, DetInfo^vartypes).

det_info_set_module_info(DetInfo, ModuleInfo,
		DetInfo^module_info := ModuleInfo).
det_info_set_vartypes(DetInfo, VarTypes,
		DetInfo^vartypes := VarTypes).
