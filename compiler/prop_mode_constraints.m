%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prop_mode_constraints.m
% Main author: richardf
%
% This module provides an alternate process_scc predicate for the
% mode_constraints module that builds the new abstract constraint data
% structures intended for a propagation solver. It deals only with the
% simple constraint system, described in the paper "Constraint-based
% mode analysis of Mercury" by David Overton, Zoltan Somogyi and Peter
% Stuckey.
%
% XXX That paper is the main documentation of the concepts
% behind the algorithm as well as the algorithm itself.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.prop_mode_constraints.
:- interface.

:- import_module check_hlds.abstract_mode_constraints.
:- import_module check_hlds.build_mode_constraints.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

	% This predicate adds to the pred_constraints_map the mode
	% declaration and goal constraints for each of the predicates in
	% the provided SCC.  Any required constraint variables are added
	% to the mc_varset and mc_var_map. Calls to predicates with no
	% mode declaration require head variable constraint variables,
	% so these are produced first for all preds in the SCC before
	% goal constraints.
	%
:- pred process_scc(module_info::in, list(pred_id)::in,
	mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out,
	pred_constraints_map::in, pred_constraints_map::out) is det.

	% Storing constraints by predicate.
	%
:- type pred_constraints_map == map(pred_id, mode_constraints).

	% Writes in human readable form to the current output stream the
	% information in the pred_constraints_map, indicating which
	% predicate each set of constraints applies to.
	%
:- pred pretty_print_pred_constraints_map(module_info::in, mc_varset::in,
	pred_constraints_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_ordering.
:- import_module check_hlds.mode_util.

:- import_module hlds.hhf.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.inst_graph.
:- import_module hlds.passes_aux.

:- import_module libs.globals.
:- import_module libs.options.

:- import_module mode_robdd.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.modules.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module multi_map.
:- import_module require.
:- import_module robdd.
:- import_module set.
:- import_module sparse_bitset.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

process_scc(ModuleInfo, SCC0, !Varset, !VarMap, !Constraints) :-
		% Process only predicates from this module
	list.filter(
		(pred(PredId::in) is semidet :-
			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			(	pred_info_is_imported(PredInfo)
			;	pred_info_is_pseudo_imported(PredInfo)
			)
		),
		SCC0,
		_,
		SCC
	),

		% Prepares the solver variables for the home path of
		% each predicate of the SCC - needed for calls to
		% predicates in the same SCC that do not have mode
		% declarations.
	add_mc_vars_for_scc_heads(ModuleInfo, SCC, !Varset, !VarMap),

		% Now go through the SCC and add the constraint
		% variables and then constraints predicate by predicate
	list.foldl3(process_pred(ModuleInfo), SCC, !Varset, !VarMap, !Constraints).

	% Performs a number of tasks for one predicate:
	% 	1) Fills out the goal_path information in the
	% 	   module_info structure
	% 	2) Adds constraint variables for program variables
	% 	   corresponding to any location at which they are
	% 	   nonlocal
	% 	3) Adds mode declaration constraints
	% 	4) Adds goal constraints
	% 
	% NOTE: it relies on the head variables for any predicate
	% without mode declarations that is called to have the
	% constraint variables corresponding to [] to already be in the
	% mc_var_map
	%
:- pred process_pred(module_info::in, pred_id::in,
	mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out,
	pred_constraints_map::in, pred_constraints_map::out) is det.

process_pred(ModuleInfo, PredId, !Varset, !VarMap, !Constraints) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	process_pred(ModuleInfo, PredId, PredInfo, !Varset, !VarMap,
		!Constraints).

	% The working part of process_pred/8 - just with the pred_info
	% unpacked from the module_info
	%
:- pred process_pred(module_info::in, pred_id::in, pred_info::in,
	mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out,
	pred_constraints_map::in, pred_constraints_map::out) is det.

process_pred(ModuleInfo, PredId, PredInfo0, !Varset, !VarMap, !Constraints) :-

		% XXX Currently the constraints simply say that if a
		% variable is bound at a disjunct it is bound at the
		% disjunction by making the relevant variables
		% equivalent. Setting GoalPathOptimisation to yes will
		% cause the disjucts to be given the same path as the
		% disjunction, so that the relevant constraint variables
		% will not need to be constrained equivalent - they will
		% be the same variable. It will do the same for other
		% path types with similar equivalence constraints -
		% refer to the goal_path module for a more detailed
		% description.
	GoalPathOptimisation = no,

		% XXX If we want the goal path info to be retained, then
		% ModuleInfo needs to be a state variable and to be
		% updated with the new PredInfo, but for now, this will
		% do.
	goal_path.fill_slots_in_clauses(ModuleInfo, GoalPathOptimisation,
		PredInfo0, PredInfo),

	pred_info_procedures(PredInfo, ProcTable),
		% Needed for defined modes.
	pred_info_clauses_info(PredInfo, ClausesInfo),
	clauses_info_headvars(ClausesInfo, HeadVars),
	clauses_info_clauses(ClausesInfo, Clauses),
	clauses_info_varset(ClausesInfo, ProgVarset),
	Goals = list.map((func(clause(_, ClauseBody, _, _)) = ClauseBody),
		Clauses),

		% Here build goal constraint vars.
	list.foldl2(add_mc_vars_for_goal(PredId, ProgVarset),
		Goals, !Varset, !VarMap),

		% Here check for mode declarations and add apppropriate
		% constraints.
	map.values(ProcTable, ProcInfos),
	
	list.filter_map(
		(pred(ProcInfo::in, (ProcHVars - ArgModes)::out) is semidet :-
			proc_info_maybe_declared_argmodes(
				ProcInfo,
				yes(_OriginalArgModes)
			),
			proc_info_argmodes(ProcInfo, ArgModes),
			proc_info_headvars(ProcInfo, ProcHVars)
%			add_sufficient_in_modes_for_type_info_args(
%				ProcHVars, ArgModes0, ArgModes
%			)	% XXX type_info args should be at the
				% start and should be 'in' so that is
				% what this predicate adds however, I am
				% not happy with it.
		),
		ProcInfos,
		HeadVarArgModesPairs
	),
	%
	% Pair up the any existing arg mode declarations with
	% their corresponding head variables from the proc_infos.
	%
	(
		HeadVarArgModesPairs = [], % No declared modes, no constraints
		ModeDeclConstraints = []
	;	
		HeadVarArgModesPairs = [_|_],	% Some declared modes
		mode_decls_constraints(ModuleInfo, !.VarMap, PredId,
			list.map(snd, HeadVarArgModesPairs),
			list.map(fst, HeadVarArgModesPairs),
			ModeDeclConstraints)
	),
	%
	% This builds the constraints for this predicate. Note
	% that the main goal may need to be temporarily formed
	% by putting clauses into a disjunction. The goal paths
	% added by goal_path.fill_slots_in_clauses reflect this
	% disjunction.
	%
	(
		Goals = [],
		GoalConstraints = []
			% If the clause list is empty, then there are no
			% goals to produce constraints for.
	;
		Goals = [_| _],
		MainGoal = disj(Goals),
		MainGoalPath = [],
		Nonlocals = set.list_to_set(HeadVars),
		goal_expr_constraints(ModuleInfo, !.VarMap, PredId, MainGoal,
			MainGoalPath, Nonlocals, GoalConstraints)
	),
	PredConstraints = list.append(ModeDeclConstraints, GoalConstraints),
	svmap.det_insert(PredId, PredConstraints, !Constraints).

	% Put the constraints to the current output stream in human
	% readable format. It titles each pred's constraints with a
	% module qualification based on the default filename for the
	% module followed by the predicate's name.
	%
pretty_print_pred_constraints_map(
	ModuleInfo, ConstraintVarset, PredConstraintsMap, !IO) :-
	ConstrainedPreds = map.keys(PredConstraintsMap),
	list.foldl(
		pretty_print_pred_constraints(
			ModuleInfo,
			ConstraintVarset,
			PredConstraintsMap
		),
		ConstrainedPreds,
		!IO
	).

	% Puts the constraints for the specified predicate from the
	% pred_constraints_map to the current output stream in human
	% readable format.
	%
:- pred pretty_print_pred_constraints(module_info::in, mc_varset::in,
	pred_constraints_map::in, pred_id::in, io::di, io::uo) is det.

pretty_print_pred_constraints(ModuleInfo, ConstraintVarset,
		PredConstraintsMap, PredId, !IO) :-
	io.write_string("\nConstraints for pred ", !IO),
	hlds_module.module_info_pred_info(ModuleInfo, PredId, PredInfo),
	ModuleName = hlds_pred.pred_info_module(PredInfo),
	PredName = hlds_pred.pred_info_name(PredInfo),
	CreateDirectories = no,
	parse_tree.modules.module_name_to_file_name(
		ModuleName, "." ++ PredName,
		CreateDirectories,
		FullPredNameString, !IO
	),
	io.write_string(FullPredNameString ++ ":\n", !IO),

	map.lookup(PredConstraintsMap, PredId, PredConstraints),
	abstract_mode_constraints.pretty_print_constraints(ConstraintVarset,
		PredConstraints, !IO).

%----------------------------------------------------------------------------%
:- end_module prop_mode_constraints.
%----------------------------------------------------------------------------%
