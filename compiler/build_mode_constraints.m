%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: build_mode_constraints.m
% Main author: richardf
%
% This module contains predicates and data structures needed for
% traversing the HLDS and building a list of abstract constraint formulae to
% describe variable producers in a Mercury program.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.build_mode_constraints.

:- interface.

:- import_module check_hlds.abstract_mode_constraints.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
% :- import_module hlds.inst_graph.
% 	% Needed if converting for partial instantiation.
:- import_module parse_tree__prog_data.

:- import_module bimap.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

	% XXX Change to include more information?  This will just be a
	% list of constraints representing the conjunction of them. When
	% a solver is written, a more elaborate data structure with
	% update functions will no doubt be needed - at that time
	% constraints should be passed as state variables.
	%
	% This represents the conjunction of the constraints it
	% contains.
	% 
:- type mode_constraints == constraint_formulae.

	% A map between the constraint variables (mc_var) and what they
	% represent ie that some program variable is produced at some
	% goal path (for a particular predicate).
	%
	% This provides a quick way of looking up the constraint
	% variable we need when we want to constrain the position in
	% which a program variable can be produced.
	%
:- type mc_var_map == bimap(mc_rep_var, mc_var).

	% Just a conveniently descriptive name.
	%
:- type args == list(prog_var).

	% Just a conveniently descriptive name.
	%
:- type nonlocals == set(prog_var).

	% In order to uniquely distinguish prog_var that may not be
	% unique amongst predicates, this data structure is used to
	% specify the predicate to which this prog_var is intended to
	% apply.
	%
:- type mc_prog_var ---> prog_var `in` pred_id.

	% An abstract representation of a mode constraint variable.
	%
	% It represents the constraint variable that determines whether
	% the program variable specified is produced at the goal path
	% specified.
	%
:- type mc_rep_var ---> mc_prog_var `at` goal_path.

	% Provides a nice way to display constraint variables - just
	% convert to the relevant mc_rep_var using the mc_var_map and
	% then convert to a string using this. It comes out in format
	% "ProgVarName.GoalPath"
	%
:- func rep_var_to_string(prog_varset, mc_rep_var) = (string).

	% For each head variable of each predicate in the supplied SCC,
	% this predicate adds to the varset the constraint variable for
	% HeadVariable `in` PredId `at` []. In other words, it creates
	% the constraint variables that describe whether or not a head
	% variable is produced by a call to the predicate. At the same
	% time it records in the mc_var_map the position and program
	% variable to which the new constraint variable corresponds.
	%
:- pred add_mc_vars_for_scc_heads(module_info::in, list(pred_id)::in,
	mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out) is det.

	% Makes sure that the necessary constraint variables exist to
	% create goal constraints for all goal types except predicate
	% calls.
	%
	% At this stage, constraint variables are created at a goal for
	% each variable in the nonlocal set of that goal - other
	% variables are either local and must be produced at the current
	% path or do not occur in the goal and so are not produced at
	% the current path.
	%
	% Note that in order to produce constraints for a goal,
	% constraint variables for the head variables of called
	% predicates with no declared modes are needed as well as these
	% goal constraint variables, so before constraints are built for
	% any predicate in an SCC add_mc_vars_for_scc_heads should be
	% called for that whole SCC.
	%
:- pred add_mc_vars_for_goal(pred_id::in, prog_varset::in, hlds_goal::in,
    mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out) is det.

	% mode_decls_constraints(ModuleInfo, VarMap, PredId, Decls,
	% HeadVarsList, Constraints)
	%
	% Constraints is the disjunction of the constraints for
	% individual declared modes being satisfied.  ie
	% disj([ConstraintsForMode1, ..., ConstraintsForModen])
	%
	% Note that if Decls is an empty list, this is interpreted as
	% there being no modes for which the predicate can be executed
	% and the returned constraints are [disj([])] which cannot be
	% satisfied. Mode declarations for predicates with no declared
	% modes should not be handled by this - they must be handled
	% elsewhere.
	%
	% The constraints for a predicate with declared modes is the
	% disjunction of the constraints for each declared mode. If,
	% according to the mode declaration, a variable is not initially
	% free then it cannot be produced by a call to this predicate;
	% if a variable is initially free and becomes not free then it
	% is produced by the predicate.  Otherwise it is free -> free
	% and is not produced.
	%
:- pred mode_decls_constraints(module_info::in, mc_var_map::in, 
	pred_id::in, list(list(mode))::in, list(args)::in, mode_constraints::out)
    is det.

	% In the event that type_info arguments have been added to a
	% predicate call's arguments/headvars, but the modes have not
	% been edited to reflect this, extra in modes need to be added
	% to the front of the mode declaration to account for this.
	%
	% XXX This predicate shouldn't be needed, but if you're getting
	% map_corresponding errors for the use of lists of different
	% lengths it would be worthwhile modifying any calls to
	% mode_decl predicates with this so that the headvars list and
	% mode declarations are the same length.
	% Unfortunately this predicate relies the type_info arguments
	% being added to the front of the arg list, and assumes that
	% they are all specifically of mode in as given by function
	% prog_tree.prog_mode.in_mode
:- pred add_sufficient_in_modes_for_type_info_args(args::in,
    list(mode)::in, list(mode)::out) is det.

	% goal_expr_constraints generates the constraints that apply to
	% a given goal_expr at a given goal path.
	%
:- pred goal_expr_constraints( module_info::in, mc_var_map::in,
	pred_id::in, hlds_goal_expr::in, goal_path::in, nonlocals::in,
    mode_constraints::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.inst_match.

:- import_module hlds.hlds_data.
:- import_module hlds.passes_aux.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_mode.

:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module sparse_bitset.
:- import_module std_util.
:- import_module string.
:- import_module svbimap.
:- import_module svmulti_map.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_mc_vars_for_scc_heads(_ModuleInfo, [], !Varset, !VarMap).
add_mc_vars_for_scc_heads(ModuleInfo, [PredId | PredIds], !Varset, !VarMap) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	clauses_info_headvars(ClausesInfo, Headvars),
	clauses_info_varset(ClausesInfo, ProgVarset),
	list.foldl2(add_mc_var_for_pred_head(PredId, ProgVarset), Headvars,
		!Varset, !VarMap),
	add_mc_vars_for_scc_heads(ModuleInfo, PredIds, !Varset, !VarMap).
		% XXX This potentially needs to be performed for the
		% headvars in the proc_infos associated with a predicate
		% as well, since the mode declaration constraints refer
		% to them, however at this point in time they should be
		% the same as the headvars in the clauses_info

	% Makes sure a constraint variable exists to say that the
	% supplied program variable is produced by the predicate in
	% which it exists.
	%
	% Just checks that the constraint variable in question doesn't
	% already exist, and then gets a new one from the varset and
	% adds it to the varmap if it doesn't.
	%
:- pred add_mc_var_for_pred_head(pred_id::in, prog_varset::in, prog_var::in,
	mc_varset::in, mc_varset::out, mc_var_map::in, mc_var_map::out) is det.

add_mc_var_for_pred_head(PredId, ProgVarset, HeadVar, !Varset, !VarMap) :-
	RepVar = (HeadVar `in` PredId) `at` [],
	( bimap.search(!.VarMap, RepVar, _MCVar) ->
    	true
	;	
        svvarset.new_named_var(rep_var_to_string(ProgVarset, RepVar),
			NewMCvar, !Varset),
		svbimap.det_insert(RepVar, NewMCvar, !VarMap)
	).

add_mc_vars_for_goal(PredId, ProgVarset, GoalExpr - GoalInfo,
        !Varset, !VarMap) :-
	goal_info_get_nonlocals(GoalInfo, Nonlocals),
	goal_info_get_goal_path(GoalInfo, GoalPath),

	{!:Varset, !:VarMap} = set.fold(
		(func(Nonlocal, {Vset0, Vmap0}) = {Vset, Vmap} :-
			RepVar = (Nonlocal `in` PredId) `at` GoalPath,
			(	bimap.search(Vmap0, RepVar, _)
			->	Vset = Vset0,
				Vmap = Vmap0
			;	varset.new_named_var(
					Vset0,
					rep_var_to_string(ProgVarset, RepVar),
					NewMCvar,
					Vset
				),
				bimap.det_insert(Vmap0, RepVar, NewMCvar, Vmap)
			)
		),
		Nonlocals,
		{!.Varset, !.VarMap}
	),
		% Switch on GoalExpr for recursion
	(
        GoalExpr = conj(Goals),
		list.foldl2(
			add_mc_vars_for_goal(PredId, ProgVarset),
			Goals,
			!Varset,
			!VarMap
		)
	;
        GoalExpr = call(_, _, _, _, _, _)
	;
        GoalExpr = generic_call(_, _, _, _)
	;
        GoalExpr = switch(_, _, Cases),
		Goals = list.map(func(case(_, Goal)) = Goal, Cases),
		list.foldl2(add_mc_vars_for_goal(PredId, ProgVarset), Goals,
			!Varset, !VarMap)
	;
        GoalExpr = unify(_, _, _, _, _)
	;
        GoalExpr = disj(Goals),
		list.foldl2(
			add_mc_vars_for_goal(PredId, ProgVarset),
			Goals,
			!Varset,
			!VarMap
		)
	;
        GoalExpr = not(Goal),
		add_mc_vars_for_goal(
			PredId, ProgVarset, Goal, !Varset, !VarMap
		)
	;
        GoalExpr = some(_, _, Goal),
		add_mc_vars_for_goal(
			PredId, ProgVarset, Goal, !Varset, !VarMap
		)
	;
        GoalExpr = if_then_else(_, Cond, Then, Else),
		Goals = [Cond, Then, Else],
		list.foldl2(
			add_mc_vars_for_goal(PredId, ProgVarset),
			Goals,
			!Varset,
			!VarMap
		)
	;
        GoalExpr = foreign_proc(_, _, _, _, _, _)
	;
        GoalExpr = par_conj(_Goals)
	;
        GoalExpr = shorthand(_ShorthandGoalExpr)
	).

rep_var_to_string(ProgVarset, (ProgVar `in` _) `at` GoalPath) = RepString :-
	goal_path_to_string(GoalPath, GoalPathString),
	varset.lookup_name(ProgVarset, ProgVar, ProgVarString),
	( GoalPathString = "" ->
        RepString = ProgVarString
	;	
        RepString = ProgVarString ++ "." ++ GoalPathString
	).

%-----------------------------------------------------------------------------%

	% goal_constraints gives the mode constraints for the supplied
	% hlds_goal
	%
:- pred goal_constraints(module_info::in, mc_var_map::in, pred_id::in,
	hlds_goal::in, mode_constraints::out) is det.

goal_constraints(ModuleInfo, VarMap, PredId, GoalExpr - GoalInfo,
        Constraints) :-
	goal_info_get_nonlocals(GoalInfo, Nonlocals),
	goal_info_get_goal_path(GoalInfo, GoalPath),
	goal_expr_constraints(ModuleInfo, VarMap, PredId, GoalExpr, GoalPath,
        Nonlocals, Constraints).

	% Goal:
	% G1, ..., Gn where Goals = [G1, ..., Gn]
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	conj(Goals), GoalPath, Nonlocals, Constraints) :-

	list.map(
		goal_constraints(ModuleInfo, VarMap, PredId),
		Goals, ConjunctConstraints
	),
	Constraints0 = list.condense(ConjunctConstraints),
	list.foldl(
		fold_local_var_into_conj_constraints(VarMap, LocalsPositions),
		multi_map.keys(LocalsPositions),
		Constraints0, Constraints1
	),
	list.foldl(
		fold_nonlocal_var_into_conj_constraints(
			VarMap,
			PredId,
			NonlocalsPositions,
			GoalPath
		),
		multi_map.keys(NonlocalsPositions),
			% Nonlocal variables that are nonlocals to subgoals.
		Constraints1, Constraints
	),

	EmptyMultiMap = multi_map.init,
	list.foldl2(
		fold_goal_into_var_position_maps(VarMap, PredId, Nonlocals),
		Goals,
		EmptyMultiMap,
		LocalsPositions,
                    % A map from each local variable to
					% its corresponding constraint
					% variables for the paths at each of
					% the conjuncts it is non-local to,
					% if such conjuncts exist. (If a
					% variable is local to one conjunct we
					% need not deal with it here).
		EmptyMultiMap,
		NonlocalsPositions
                    % A map from each non-local variable
					% to its corresponding constraint
					% variables at the paths of each of the
					% conjuncts it is non-local to. Note
					% that all non-local variables should
					% appear in this map, since if a
					% variable is in the non-local set of a
					% conjunction it ought to be in the
					% non-local set of at least one of the
					% conjuncts.
	).

	% Pred Call
	% 
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	call(CalledPred, _ProcID, Args, _Builtin, _UnifyContext, _Name),
	GoalPath, _Nonlocals, Constraints) :-

		% Get the declared modes (if any exist)
	module_info_pred_info(ModuleInfo, CalledPred, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map.values(ProcTable, ProcInfos),

	list.filter_map(
		(pred(PInfo::in, MDecl::out) is semidet :-
			proc_info_maybe_declared_argmodes(PInfo, yes(_)),
			proc_info_argmodes(PInfo, MDecl)
		),
		ProcInfos,
		ArgModeDecls
	),

	(
			% No modes declared, must be in the same SCC as
			% the calling predicate.
        ArgModeDecls = [],

			% Get the head variables of the called pred.
		pred_info_clauses_info(PredInfo, ClausesInfo),
		clauses_info_headvars(ClausesInfo, HeadVars),

		call_headvar_constraints( VarMap, GoalPath, PredId, Args, CalledPred,
            HeadVars, Constraints)
	;
			% At least one declared mode
        ArgModeDecls = [_| _],

%		list.map(
%			add_sufficient_in_modes_for_type_info_args(Args),
%			ArgModeDecls,
%			FullArgModeDecls
%		),	% XXX type_info args should be at the start and
%			% should be 'in' so that is what this predicate
%			% adds however, I am not happy with it.

		call_mode_decls_constraints(ModuleInfo, VarMap, PredId, ArgModeDecls,
			GoalPath, Args, Constraints)
	).

	% XXX Need to do something here.
	%
goal_expr_constraints(_ModuleInfo, _VarMap, _PredId,
        generic_call(_, _, _, _), _GoalPath, _Nonlocals, _Constraints) :-
	sorry(this_file, "generic_call NYI.").

	% XXX Need to do something here.
	%
goal_expr_constraints(_ModuleInfo, _VarMap, _PredId,
        switch(_, _, _), _GoalPath, _Nonlocals, _Constraints) :-
	sorry(this_file, "switch NYI.").

	% Unification Goals
	%
goal_expr_constraints(_ModuleInfo, VarMap, PredId,
        unify(LHSvar, RHS, _Mode, _Kind, _Context),
	GoalPath, _Nonlocals, Constraints) :-
	(
        RHS = var(RHSvar),
			% Goal: LHSvar = RHSvar
		Constraints = [
			atomic_constraint(at_most_one([
				prog_var_at_path(VarMap, PredId, GoalPath, LHSvar),
				prog_var_at_path(VarMap, PredId, GoalPath, RHSvar)
			]))	% At most one of the left and right hand
				% sides of a unification is produced
				% at the unification.
		]
	;	
        RHS = functor(_Functor, _IsExistConstr, Args),
		LHSproducedHere = prog_var_at_path(
			VarMap, PredId, GoalPath, LHSvar
		),
		ArgsProducedHere =
			list.map(prog_var_at_path(VarMap, PredId, GoalPath), Args),
		(	
            ArgsProducedHere = [OneArgProducedHere, _Two| _],
				% Goal: LHSvar = functor(Args)
			Constraints = [
                    % If one arg is produced here, then they all are.
				atomic_constraint(equivalent(ArgsProducedHere)),
                    % At most one side of the unification is produced.
				atomic_constraint(
                    at_most_one([LHSproducedHere, OneArgProducedHere]))
			]
		;	
            ArgsProducedHere = [OneArgProducedHere],
				% Goal: LHSvar = functor(Arg)
			Constraints = [
                    % At most one side of the unification is produced.
				atomic_constraint(
                    at_most_one([LHSproducedHere, OneArgProducedHere]))
			]
		;	
            ArgsProducedHere = [],
				% Goal: LHSvar = functor
				% In this case, LHSvar need not be produced
				% - it could be a test, so no constraints.
			Constraints = []
		)

	;	
        RHS = lambda_goal(_, _, _, _, _, _, _, _, _),
		sorry(this_file, "unify with lambda goal NYI")
	).

	% Goal:
	% G1; ...; Gn where Goals = [G1, ..., Gn]
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	disj(Goals), GoalPath, Nonlocals, Constraints) :-

	nonlocals_at_path_and_subpaths(
		VarMap, PredId, GoalPath, DisjunctGoalPaths,
		Nonlocals, NonlocalsHere, NonlocalsAtDisjuncts
	),
	list.map(
		pred(_GExpr - GInfo::in, GPath::out) is det :-
			goal_info_get_goal_path(GInfo, GPath),
		Goals,
		DisjunctGoalPaths
	),

	list.map(goal_constraints(ModuleInfo, VarMap, PredId),
		Goals, DisjunctConstraints),

	Constraints = list.condense([
		list.map_corresponding(
			func(X, Ys) = atomic_constraint(equivalent([X | Ys])),
				% A variable bound at any disjunct is
				% bound for the disjunct as a whole. If
				% a variable can be bound at one
				% conjunct it must be able to be bound
				% at any.
			NonlocalsHere, NonlocalsAtDisjuncts
		) |
		DisjunctConstraints
	]).

	% Goal:
	% not (Goal)
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	not(Goal), GoalPath, Nonlocals, Constraints) :-
	Goal = _ - NegatedGoalInfo,
	goal_info_get_goal_path(NegatedGoalInfo, NegatedGoalPath),
	NonlocalsConstraintVars = set.fold(
		func(Nonlocal, MCVars) = [
			prog_var_at_path(VarMap, PredId, GoalPath, Nonlocal),
			prog_var_at_path(VarMap, PredId, NegatedGoalPath, Nonlocal)
            |
			MCVars
		],
		Nonlocals,
		[]
	),
	goal_constraints(
		ModuleInfo, VarMap, PredId, Goal, NegatedGoalConstraints
	),
	Constraints = list.foldl(
		func(MCVar, Cnstrnts) = [
			atomic_constraint(equiv_bool(MCVar, no))|
				% The variables non-local to the
				% negation are not to be produced at the
				% negation or any deeper.
			Cnstrnts
		],
		NonlocalsConstraintVars,
		NegatedGoalConstraints
	).

	% Goal:
	% some Xs Goal
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	some(_ExistVars, _CanRemove, Goal),
	GoalPath, Nonlocals, Constraints) :-
	Goal = _ - SomeGoalInfo,
	goal_info_get_goal_path(SomeGoalInfo, SomeGoalPath),
	Constraints = set.fold(
		func(NL, NLConstraints) = [
			atomic_constraint(equivalent([
				prog_var_at_path(VarMap, PredId, GoalPath, NL),
				prog_var_at_path(
					VarMap, PredId, SomeGoalPath, NL
				)
			]))|		% If a program variable is produced
					% by the sub-goal of the some
					% statement, it is produced at the
					% main goal as well.
			NLConstraints
		],
		Nonlocals,
		SomeGoalConstraints	% Include the constraints from the
					% recursive call on the sub-goal.
	),
	goal_constraints(
		ModuleInfo, VarMap, PredId, Goal, SomeGoalConstraints
	).

	% Goal:
	% If -> Then; Else
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	if_then_else(ExistVars, If, Then, Else),
	GoalPath, Nonlocals, Constraints) :-
	If = _ - IfInfo, Then = _ - ThenInfo, Else = _ - ElseInfo,
	goal_info_get_goal_path(IfInfo, CondPath),
	goal_info_get_goal_path(ThenInfo, ThenPath),
	goal_info_get_goal_path(ElseInfo, ElsePath),

	NonlocalsHere = list.map(
		prog_var_at_path(VarMap, PredId, GoalPath),
		NonlocalsList
	),
	NonlocalsAtCond = list.map(
		prog_var_at_path(VarMap, PredId, CondPath),
		NonlocalsList
	),
	NonlocalsAtThen = list.map(
		prog_var_at_path(VarMap, PredId, ThenPath),
		NonlocalsList
	),
	NonlocalsAtElse = list.map(
		prog_var_at_path(VarMap, PredId, ElsePath),
		NonlocalsList
	),
	NonlocalsList = set.to_sorted_list(Nonlocals),

		% The existentially quantified variables shared between
		% the condition and the then-part have special
		% constraints
		%
	LocalAndSharedAtCond = list.map(
		prog_var_at_path(VarMap, PredId, CondPath),
		ExistVars
	),
	LocalAndSharedAtThen = list.map(
		prog_var_at_path(VarMap, PredId, ThenPath),
		ExistVars
	),

	goal_constraints(ModuleInfo, VarMap, PredId, If, IfConstraints),
	goal_constraints(ModuleInfo, VarMap, PredId, Then, ThenConstraints),
	goal_constraints(ModuleInfo, VarMap, PredId, Else, ElseConstraints),

	Constraints = list.condense([
		list.map_corresponding3(
			func(NLHere, NLAtThen, NLAtElse) =
				atomic_constraint(equivalent(
					[NLHere, NLAtThen, NLAtElse]
				)),	% If a variable is to be produced
					% at this path the then and else
					% parts must be able to produce it.
			NonlocalsHere,
			NonlocalsAtThen,
			NonlocalsAtElse
		),
		list.map(
			func(Cond) = atomic_constraint(equiv_bool(Cond, no)),
			NonlocalsAtCond
		),	% No nonlocal is produced in the condition.
		list.map_corresponding(
			(func(LocalAtCond, LocalAtThen) =
				atomic_constraint(exactly_one(
					[LocalAtCond, LocalAtThen]
				))
			),
			LocalAndSharedAtCond,
			LocalAndSharedAtThen
		),	% XXX Do we want this, or do we just constrain
			% 'Cond true and 'Then false? Maybe a local
			% variable shared between the condition and
			% then-part should always be bound in the
			% condition, but I'm not sure about the
			% possibility of checking the variable's type in
			% the Cond and then binding it in the Then...
		IfConstraints,
		ThenConstraints,
		ElseConstraints

	]).

	% Foreign procedure
	%
goal_expr_constraints(ModuleInfo, VarMap, PredId,
	foreign_proc(_, CalledPred, ProcID, ForeignArgs, _, _),
	GoalPath, _Nonlocals, Constraints) :-
	CallArgs = list.map(foreign_arg_var, ForeignArgs),
	module_info_pred_proc_info(ModuleInfo, CalledPred, ProcID, _, ProcInfo),
	( proc_info_maybe_declared_argmodes(ProcInfo, yes(_OrigDecl)) ->
        proc_info_argmodes(ProcInfo, Decl),

%		add_sufficient_in_modes_for_type_info_args(
%			CallArgs,
%			Decl,
%			FullDecl
%		),	% XXX type_info args should be at the start and
%			% should be 'in' so that is what this predicate
%			% adds however, I am not happy with it.

            % This pred should strip the disj(conj()) for the
			% single declaration.
		call_mode_decls_constraints(ModuleInfo, VarMap, PredId, [Decl],
			GoalPath, CallArgs, Constraints)
	;	
		unexpected(this_file, "no mode declaration for foreign proc")
	).

	% Parallel conjunction
	%
	% XXX What to do here?
	%
goal_expr_constraints(_ModuleInfo, _VarMap, _PredId,
	    par_conj(_Goals), _GoalPath, _Nonlocals, _Constraints) :-
    sorry(this_file, "NYI par_conj").    	

	% Shorthand goals. Should not exist at this point in compilation.
	%
goal_expr_constraints(_ModuleInfo, _VarMap, _PredId,
	    shorthand(_ShorthandGoalExpr), _GoalPath, _Nonlocals, _Constraints) :-
	unexpected(this_file, "shorthand goal").

%-----------------------------------------------------------------------------%

	% prog_var_at_path(VarMap, PredId, GoalPath, ProgVar) =
	% ConstraintVar consults the map to get the constraint variable
	% ConstraintVar that says that ProgVar is produced at GoalPath.
	% The lookup function will report an error if the key (ProgVar
	% `in` PredId) `at` GoalPath does not exist in the map.
	% 
:- func prog_var_at_path(mc_var_map, pred_id, goal_path, prog_var) = (mc_var).

prog_var_at_path(VarMap, PredId, GoalPath, ProgVar) =
	bimap.lookup(VarMap, ((ProgVar `in` PredId) `at` GoalPath)).

	% prog_var_at_paths(VarMap, GoalPaths, ProgVar) = ConstraintVars
	% consults the map to form a list of the constraint variables
	% that say that ProgVar is produced at each of the paths in
	% GoalPaths respectively.  The lookup function will report an
	% error if the key (ProgVar `in` PredId) `at` GoalPath does not
	% exist in the map for any of the 'GoalPath's in GoalPaths.
	%
:- func prog_var_at_paths(mc_var_map, pred_id, list(goal_path), prog_var) =
	list(mc_var).

prog_var_at_paths(VarMap, PredId, GoalPaths, ProgVar) =
	list.map(
		func(GoalPath) = bimap.lookup(
			VarMap,
			(ProgVar `in` PredId) `at` GoalPath
		),
		GoalPaths
	).

	% nonlocals_at_path_and_subpaths(VarMap, GoalPath,
	% 	SubPaths, Nonlocals, NonlocalsAtPath,
	% 	NonlocalsAtSubPaths)
	% consults the VarMap to find constraint variables associated
	% with each of the program variables in the Nonlocals set for a
	% GoalPath eg a conjunction and its SubPaths (ie the individual
	% conjuncts), although it doesn't check that the SubPaths are
	% indeed subpaths of GoalPath.  Nonlocals are converted to a
	% sorted set, so the Nth entry of NonlocalsAtPath and the Nth
	% entry of NonlocalsAtSubPaths are respectively the constraint
	% variable at the goal and a list of the constraint variables
	% for the subgoals, for the same program variable.
	%
:- pred nonlocals_at_path_and_subpaths(
	mc_var_map::in, pred_id::in, goal_path::in, list(goal_path)::in,
	nonlocals::in, list(mc_var)::out, list(list(mc_var))::out) is det.

nonlocals_at_path_and_subpaths(VarMap, PredId, GoalPath, SubPaths, Nonlocals,
	NonlocalsAtPath, NonlocalsAtSubPaths) :-
	NonlocalsAtPath = list.map(
		prog_var_at_path(VarMap, PredId, GoalPath),
		NonlocalsList
	),
	NonlocalsAtSubPaths = list.map(
		prog_var_at_paths(VarMap, PredId, SubPaths),
		NonlocalsList
	),
	NonlocalsList = set.to_sorted_list(Nonlocals).

%----------------------------------------------------------------------------%

mode_decls_constraints(
	ModuleInfo, VarMap, PredId, Decls, HeadVarsList, Constraints) :-
	ConstraintsList = list.map_corresponding(
		mode_decl_constraints(ModuleInfo),
		list.map(
			list.map(prog_var_at_path(VarMap, PredId, [])),
			HeadVarsList
		),
		Decls
	),
	Constraints0 = list.condense(ConstraintsList),
	( Constraints0 = [conj(OneModeOnlyConstraints)] ->
        Constraints = OneModeOnlyConstraints
	;	
        Constraints = [disj(Constraints0)]
	).

	% call_mode_decls_constraints(ModuleInfo, VarMap, CallingPred,
	% 	Decls, GoalPath, CallArgs, Constraints)
	% 
	% Returns
	% disj([ConstraintsForMode1, ..., ConstraintsForModen])
	%
	% Note that if Decls is an empty list, this is interpreted as
	% there being no modes for which the predicate can be executed
	% and the returned constraints are [disj([])] which cannot be
	% satisfied. Predicate calls for predicates with no declared
	% modes should not be handled by this - they must be handled
	% elsewhere.
	%
	% The constraints for a (call to a) predicate with declared
	% modes is the disjunction of the constraints for each declared
	% mode. If, according to the mode declaration, a variable is not
	% initially free then it cannot be produced by a call to this
	% goal; If a variable is initially free and becomes not free
	% then it is produced by the predicate. Otherwise it is free ->
	% free and is not produced.
	%
:- pred call_mode_decls_constraints(module_info::in, 
	mc_var_map::in, pred_id::in, list(list(mode))::in,
	goal_path::in, args::in, mode_constraints::out) is det.

call_mode_decls_constraints(ModuleInfo, VarMap, CallingPred, Decls, GoalPath,
        CallArgs, Constraints) :-
	CallArgsHere = list.map(prog_var_at_path(VarMap, CallingPred, GoalPath),
		CallArgs),
	Constraints0 = 
        list.condense(list.map(mode_decl_constraints(ModuleInfo, CallArgsHere),
		    Decls)),
	( Constraints0 = [conj(OneModeOnlyConstraints)] ->
        Constraints = OneModeOnlyConstraints
	;	
        Constraints = [disj(Constraints0)]
	).

	% mode_decl_constraints(ModuleInfo, ConstraintVars, ArgModes)
	% looks at each mode to see if its variable is produced, and
	% creates constraints for the corresponding constraint variables
	% accordingly.
	%
:- func mode_decl_constraints(module_info, list(mc_var), list(mode)) = 
    mode_constraints.

mode_decl_constraints(ModuleInfo, ConstraintVars, ArgModes) =
	[conj(list.map_corresponding(
		(func(CVar, Mode) =
			atomic_constraint(equiv_bool(CVar, IsProduced)) :-
			mode_util.mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
			(	
			    % Already produced.
                not inst_match.inst_is_free(ModuleInfo, InitialInst)
			->	
                IsProduced = no		% Not produced here.
			;	
                % free -> non-free
                not inst_match.inst_is_free( ModuleInfo, FinalInst)
			->	
                IsProduced = yes	% Produced here.
			;	
                IsProduced = no		% free -> free
				% Not produced here.
			)
		),
		ConstraintVars,
		ArgModes
	))].

add_sufficient_in_modes_for_type_info_args(Args, Decl, FullDecl) :-
	NumArgs = list.length(Args),
	NumArgModes = list.length(Decl),
	Diff = NumArgs - NumArgModes,
	( Diff = 0 ->
        FullDecl = Decl
	; Diff > 0 ->
        FullDecl = list.append(list.duplicate(Diff, prog_mode.in_mode), Decl)
	;	
        unexpected(this_file, "Too many mode declared args.m")
	).

	% call_headvar_constraints(HeadVars, VarMap, GoalPath, CallArgs, 
	% 	Constraints)
	% Forms constraints that mean any call arg will be produced if
	% the corresponding headvar is produced by the main goal of the
	% called predicate.
	%
	% This should not be used if a mode declaration is supplied, as
	% it means a predicate can only be used in a single mode
	% throughout a whole SCC.
	% 	
:- pred call_headvar_constraints(mc_var_map::in, goal_path::in,
	    pred_id::in, args::in, pred_id::in, args::in,
	    mode_constraints::out) is det.

call_headvar_constraints(VarMap, GoalPath,
	CallingPred, CallArgs, CalledPred, HeadVars, Constraints) :-
	HeadVarsAtHome = list.map(prog_var_at_path(VarMap, CalledPred, []),
	    HeadVars),
	CallArgsHere = list.map( prog_var_at_path(VarMap, CallingPred, GoalPath),
		CallArgs),
	Constraints = list.map_corresponding(
		(func(HeadVarThere, CallArgHere) =
			atomic_constraint(equivalent(
				[HeadVarThere, CallArgHere]
			))
		),
		HeadVarsAtHome,
		CallArgsHere
	).

%-----------------------------------------------------------------------------%

	% At times we want constraint variables at the sub-paths of a
	% goal (eg conjunction/disjunction) but only at the paths in
	% which the corresponding program variable occurs and is
	% non-local.  This predicate builds two maps - one for variables
	% non-local to the parent goal and one for those local to it.
	% They map program variables that are non-local to the sub-goals
	% to the constraint variables associated with them being
	% produced at each of the goal paths in which they appear.
	%
:- pred fold_goal_into_var_position_maps(
    mc_var_map::in, pred_id::in, nonlocals::in, hlds_goal::in,
   	multi_map(prog_var, mc_var)::in, multi_map(prog_var, mc_var)::out,
   	multi_map(prog_var, mc_var)::in, multi_map(prog_var, mc_var)::out) is det.

fold_goal_into_var_position_maps(VarMap, PredId, Nonlocals,
	    _SubGoalExpr - SubGoalInfo, !LocalsMap, !NonlocalsMap) :-
	goal_info_get_nonlocals(SubGoalInfo, SubGoalNonlocals),
	goal_info_get_goal_path(SubGoalInfo, SubGoalPath),
	Nonlocal = set.intersect(SubGoalNonlocals, Nonlocals),
		% Note that if any variable in Nonlocals does not
		% appear in the nonlocal set of any conjunct then
		% this is simply due a conservative estimate on the
		% non-locals of the conjunction and we can't form
		% reliable constraints for it anyway (XXX although we
		% might want to consider constraining it to false
		% considering as it can't be produced at this goal path)
	Local = set.difference(SubGoalNonlocals, Nonlocals),
		% Note this is the local variables that
		% are non-local to this particular sub-goal
	set.fold(fold_variable_into_var_position_map(VarMap, PredId, SubGoalPath),
		Local, !LocalsMap),
	set.fold(fold_variable_into_var_position_map(VarMap, PredId, SubGoalPath),
		Nonlocal, !NonlocalsMap).

	% A subsection of fold_goal_into_var_position_maps, puts into
	% the map the key ProgVar with the constraint variable that says
	% ProgVar is bound at GoalPath.
	%
:- pred fold_variable_into_var_position_map(
	mc_var_map::in, pred_id::in, goal_path::in, prog_var::in,
	multi_map(prog_var, mc_var)::in, multi_map(prog_var, mc_var)::out) is det.

fold_variable_into_var_position_map(VarMap, PredId, GoalPath, ProgVar, !Map) :-
	MCVar = prog_var_at_path(VarMap, PredId, GoalPath, ProgVar),
	svmulti_map.set(ProgVar, MCVar, !Map).

	% This predicate adds the constraints for a variable in the
	% non-local set of a conjunction (to other previously
	% constructed constraints).  The NonLocalsMap should provide the
	% list of constraint variables representing the program variable
	% being produced at each conjunct it appears in respectively.
	%
:- pred fold_nonlocal_var_into_conj_constraints(mc_var_map::in,
	pred_id::in, multi_map(prog_var, mc_var)::in, goal_path::in,
	prog_var::in, mode_constraints::in, mode_constraints::out) is det.

fold_nonlocal_var_into_conj_constraints(VarMap, PredId, NonlocalsMap,
        GoalPath, ProgVar, !Constraints) :-
    NewConstraints = [atomic_constraint(equiv_disj(ProgVarAtGoalPath, Xs)),
		atomic_constraint(at_most_one(Xs))],
	list.append(NewConstraints, !Constraints),
	ProgVarAtGoalPath = prog_var_at_path(VarMap, PredId, GoalPath, ProgVar),
	Xs = multi_map.lookup(NonlocalsMap, ProgVar).

	% This predicate adds the constraints for a variable in the
	% non-local set of a conjunct but not nonlocal to the
	% conjunction as a whole, to the constraints supplied. The
	% NonLocalsMap should provide the list of constraint variables
	% representing the program variable being produced at each
	% conjunct it appears in respectively.
	%
:- pred fold_local_var_into_conj_constraints(
	mc_var_map::in, multi_map(prog_var, mc_var)::in,
	prog_var::in, mode_constraints::in, mode_constraints::out) is det.

fold_local_var_into_conj_constraints(_VarMap, LocalsMap, ProgVar,
        !Constraints) :-
	list.cons(atomic_constraint(exactly_one(Xs)), !Constraints),
	Xs = multi_map.lookup(LocalsMap, ProgVar).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "build_mode_constraints.m".

%-----------------------------------------------------------------------------%
:- end_module build_mode_constraints.
%-----------------------------------------------------------------------------%
