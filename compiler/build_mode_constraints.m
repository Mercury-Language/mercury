%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: build_mode_constraints.m
% Main author: richardf

% This module contains predicates and data structures needed for
% traversing the HLDS and building a list of abstract constraint formulae to
% describe variable producers in a Mercury program.

%-----------------------------------------------------------------------------%

:- module check_hlds.build_mode_constraints.
:- interface.

:- import_module check_hlds.abstract_mode_constraints.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module bimap.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % This represents the conjunction of the constraints it
    % contains (which typically will refer to only one predicate).
    %
:- type mode_constraints == pred_p_c_constraints.

    % A store of information about mode constraint variables
    % associated with constraints based mode analysis of a module
    % (not just of a single predicate).
    %
:- type mc_var_info
    --->    mc_var_info(
                mc_varset   ::  mc_varset,
                            % Produces constraint variables
                            % for producer/consumer constraints.

                rep_var_map ::  mc_var_map
                            % Bimap between constraint variables
                            % and the abstract representation
                            % of the proposition they represent.
            ).

    % A map between the constraint variables (mc_var) and what they
    % represent ie the proposition that some program variable is
    % produced at some goal path (for a particular predicate).
    %
    % This provides a quick way of looking up the constraint variable we
    % need when we want to constrain the position in which a program
    % variable can be produced.
    %
:- type mc_var_map == bimap(mc_rep_var, mc_var).

    % Just a conveniently descriptive name.
    %
:- type args == list(prog_var).

    % Just a conveniently descriptive name.
    %
:- type nonlocals == set(prog_var).

    % In order to uniquely distinguish a prog_var that may not be
    % unique amongst predicates, this data structure is used to
    % specify the predicate to which this prog_var is intended to
    % apply.
    %
:- type mc_prog_var ---> prog_var `in` pred_id.

    % An abstract representation of a mode constraint variable.
    %
    % ProgVar `in` PredId, `at` GoalPath represents the constraint
    % variable of the proposition that ProgVar is produced at
    % goal GoalPath in predicate PredId.
    %
:- type mc_rep_var ---> mc_prog_var `at` goal_path.

%-----------------------------------------------------------------------------%

    % add_mc_vars_for_scc_heads(ModuleInfo, SCC, !VarInfo)
    %
    % For each HeadVariable of each predicate PredId, in the supplied
    % SCC, this predicate adds to VarInfo the constraint variable for
    % HeadVariable `in` PredId `at` []. In other words, it creates the
    % constraint variables that describe whether or not a head variable
    % is produced by a call to the predicate.
    %
    % (We do this all at once because intra-SCC calls refer to the
    % production of program variables at the head of the callee,
    % and the caller doesn't neccessarily have access to the
    % relevant prog_varset to create the necessary constraint
    % variables.)
    %
:- pred add_mc_vars_for_scc_heads(module_info::in, list(pred_id)::in,
    mc_var_info::in, mc_var_info::out) is det.

    % add_mc_vars_for_goal(PredId, ProgVarset, Goal, !VarInfo)
    %
    % For each nonlocal of Goal, makes sure a constraint variable exists
    % in VarInfo representing the proposition that the nonlocal is
    % produced at that Goal. Other variables are either local and must
    % be produced at the current path or do not occur in the goal and so
    % are not produced at the current path (although some such
    % constraint variables are created later so as to simplify
    % constraint generation).
    %
    % NOTE: this predicate *does* recurse on subgoals.
    %
:- pred add_mc_vars_for_goal(pred_id::in, prog_varset::in, hlds_goal::in,
    mc_var_info::in, mc_var_info::out) is det.

    % add_clauses_constraints(ModuleInfo, PredId, PredInfo, !VarInfo,
    %   !Constraints)
    %
    % Adds to Constraints the constraints for the body of predicate
    % PredId, from the module described in ModuleInfo. (PredInfo should
    % be the unpacked info for PredId from ModuleInfo, and VarInfo is
    % included to keep track of mode constraint variables for that
    % module.)
    %
:- pred add_clauses_constraints(module_info::in, pred_id::in, pred_info::in,
    mc_var_info::in, mc_var_info::out,
    mode_constraints::in, mode_constraints::out) is det.

    % mode_decls_constraints(ModuleInfo, VarMap, PredId, Decls,
    %   HeadVarsList, Constraints)
    %
    % Constraints is the disjunction of the constraints for
    % individual declared modes being satisfied.  ie
    % disj([ConstraintsForMode1, ..., ConstraintsForModen])
    %
    % The constraints for a predicate with declared modes is the
    % disjunction of the constraints for each declared mode. If,
    % according to the mode declaration, a variable is not initially
    % free then it is constrained so that it cannot be produced by a
    % call to this predicate; if a variable is initially free and
    % becomes not free then it is constrained so as to be produced by
    % the predicate. Otherwise it is free -> free and is constrained
    % so as to not be produced.
    %
    % NOTE: if Decls is an empty list, this is interpreted as
    % there being no modes for which the predicate can be executed
    % and the returned constraints are [disj([])] which cannot be
    % satisfied. Predicates with mode inference requested should
    % not be handled by this.
    %
:- pred mode_decls_constraints(module_info::in, mc_var_map::in,
    pred_id::in, list(list(mer_mode))::in, list(args)::in,
    constraint_formulae::out) is det.

    % add_mode_decl_constraints(ModuleInfo, PredId, ProcId,
    %   Decl, Args, !VarInfo, !Constraints)
    %
    % Constrains (in the Constraints) the production of Args at the head
    % of predicate PredId when called in mode ProcId with declaration
    % Decl.  If, according to the mode declaration, a variable is not
    % initially free then it is constrained so that it cannot be
    % produced by a call to this predicate; if a variable is initially
    % free and becomes not free then it is constrained so as to be
    % produced by the predicate.  Otherwise it is free -> free and is
    % constrained so as to not be produced.
    %
:- pred add_mode_decl_constraints(module_info::in, pred_id::in, proc_id::in,
    list(mer_mode)::in, args::in, mc_var_info::in, mc_var_info::out,
    mode_constraints::in, mode_constraints::out) is det.

    % Creates a new mc_var_info structure (with no mode constraint
    % variables stored in it).
    %
:- func var_info_init = mc_var_info.

    % rep_var_to_string(ProgVarset, ProgVar `in` PredId `at` GoalPath)
    %
    % returns a string representation of the constraint variable with
    % the abstract representation given, in format "ProgVarName.GoalPath" -
    % the ProgVarset should have the ProgVarName in it for ProgVar,
    % and should therefore be taken from predicate PredId.
    %
:- func rep_var_to_string(prog_varset, mc_rep_var) = (string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
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

:- type conj_constraints_info
    --->    conj_constraints_info(
                locals_positions    ::  conjunct_production_map,
                    % Keys are program variables local to the
                    % conjunction. They are mapped to constraint
                    % variables representing the proposition that they
                    % are produced at each conjunct, but only for
                    % conjuncts they appear in/are nonlocal to.

                nonlocals_positions ::  conjunct_production_map
                    % Keys are program variables nonlocal to the
                    % conjunction. They are mapped to constraint
                    % variables representing the proposition that they
                    % are produced at each conjunct, but only for
                    % conjuncts they appear in/are nonlocal to.
            ).

    % Map from program variables to corresponding constraint variables.
    % The constraint variables should represent a set of propositions
    % that the program variable they are mapped from is produced at
    % some conjunct in a single conjunction.
    %
    % ie if multi_map.member(ConjunctProductionMap, ProgVar, MCVar)
    % then MCVar should represent the proposition that ProgVar is
    % produced at some specific conjunct, and all such `MCVar's should
    % refer to the same conjuction.
    %
:- type conjunct_production_map == multi_map(prog_var, mc_var).

%-----------------------------------------------------------------------------%

add_mc_vars_for_scc_heads(ModuleInfo, PredIds, !VarInfo) :-
    list.foldl(add_mc_vars_for_pred_head(ModuleInfo), PredIds, !VarInfo).

    % add_mc_vars_for_pred_head(ModuleInfo, PredId, !VarInfo)
    %
    % For each HeadVariable of predicate PredId this predicate adds to
    % VarInfo the constraint variable for HeadVariable `in` PredId `at`
    % []. In other words, it creates the constraint variables that
    % describe whether or not a head variable is produced by a call to
    % the predicate.
    %
:- pred add_mc_vars_for_pred_head(module_info::in, pred_id::in,
    mc_var_info::in, mc_var_info::out) is det.

add_mc_vars_for_pred_head(ModuleInfo, PredId, !VarInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    clauses_info_headvars(ClausesInfo, Headvars),
    clauses_info_varset(ClausesInfo, ProgVarset),
    list.foldl(add_mc_var_for_pred_head(ProgVarset, PredId), Headvars,
        !VarInfo).

    % add_mc_var_for_pred_head(ProgVarset, PredId, ProgVar, !VarInfo)
    %
    % For ProgVar, a head variable of predicate PredId, this predicate
    % adds to VarInfo the constraint variable for ProgVar `in` PredId
    % `at` []. In other words, it creates the constraint variable that
    % describes whether or not ProgVar is produced by a call to the
    % predicate. ProgVarset should contain ProgVar, and a string name
    % for it.
    %
:- pred add_mc_var_for_pred_head(prog_varset::in, pred_id::in, prog_var::in,
    mc_var_info::in, mc_var_info::out) is det.

add_mc_var_for_pred_head(ProgVarset, PredId, HeadVar, !VarInfo) :-
    prog_var_at_path(ProgVarset, PredId, HeadVar, [], _, !VarInfo).

%-----------------------------------------------------------------------------%

add_mc_vars_for_goal(PredId, ProgVarset, GoalExpr - GoalInfo, !VarInfo) :-
    goal_info_get_nonlocals(GoalInfo, Nonlocals),
    goal_info_get_goal_path(GoalInfo, GoalPath),

    set.to_sorted_list(Nonlocals, NlsList),
    prog_vars_at_path(ProgVarset, PredId, NlsList, GoalPath, _, !VarInfo),

        % Switch on GoalExpr for recursion
    (
        GoalExpr = conj(Goals),
        list.foldl(add_mc_vars_for_goal(PredId, ProgVarset), Goals, !VarInfo)
    ;
        GoalExpr = call(_, _, _, _, _, _)
    ;
        GoalExpr = generic_call(_, _, _, _)
    ;
        GoalExpr = switch(_, _, Cases),
        Goals = list.map(func(case(_, Goal)) = Goal, Cases),
        list.foldl(add_mc_vars_for_goal(PredId, ProgVarset), Goals, !VarInfo)
    ;
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = disj(Goals),
        list.foldl(add_mc_vars_for_goal(PredId, ProgVarset), Goals, !VarInfo)
    ;
        GoalExpr = not(Goal),
        add_mc_vars_for_goal(PredId, ProgVarset, Goal, !VarInfo)
    ;   GoalExpr = scope(_, Goal),
        add_mc_vars_for_goal(PredId, ProgVarset, Goal, !VarInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Goals = [Cond, Then, Else],
        list.foldl(add_mc_vars_for_goal(PredId, ProgVarset), Goals, !VarInfo)
    ;
        GoalExpr = foreign_proc(_, _, _, _, _, _)
    ;
        GoalExpr = par_conj(_Goals)
    ;
        GoalExpr = shorthand(_ShorthandGoalExpr)
    ).

%-----------------------------------------------------------------------------%

add_clauses_constraints(ModuleInfo, PredId, PredInfo, !VarInfo,
        !Constraints) :-
    pred_info_clauses_info(PredInfo, ClausesInfo),
    clauses_info_headvars(ClausesInfo, HeadVars),
    clauses_info_clauses_only(ClausesInfo, Clauses),
    clauses_info_varset(ClausesInfo, ProgVarset),

    (
        % If the clause list is empty, then there are no goals
        % to produce constraints for.
        Clauses = []
    ;
        Clauses = [FirstClause|_],

        % Use the first clause for the context of the top level
        % goal constraints.
        Context = clause_context(FirstClause),

        % All clauses are considered for all procedures.
        % Though some may not be applicable, overall the
        % waste should not be large.
        Goals = list.map((func(clause(_, Body, _, _)) = Body), Clauses),

        list.foldl(add_mc_vars_for_goal(PredId, ProgVarset),
            Goals, !VarInfo),

        % Temporarily form the disjunction implied by the goal path
        % annotations.
        MainGoal = disj(Goals),
        HeadGoalPath = [],
        Nonlocals = set.list_to_set(HeadVars),
        add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId, MainGoal,
            Context, HeadGoalPath, Nonlocals, !VarInfo, !Constraints)
    ).

    % add_goal_constraints(ModuleInfo, ProgVarset, PredId, Goal,
    %   !VarInfo, !Constraints)
    %
    % Adds goal constraints to Constraints to describe the producing and
    % consuming goals for program variables nonlocal to Goal in
    % predicate PredId of the module described in ModuleInfo.
    % VarInfo is used to keep track of the meaning of any constraint
    % variables used.
    %
:- pred add_goal_constraints(module_info::in, prog_varset::in, pred_id::in,
    hlds_goal::in, mc_var_info::in, mc_var_info::out, mode_constraints::in,
    mode_constraints::out) is det.

add_goal_constraints(ModuleInfo, ProgVarset, PredId, GoalExpr - GoalInfo,
        !VarInfo, !Constraints) :-
    goal_info_get_nonlocals(GoalInfo, Nonlocals),
    goal_info_get_goal_path(GoalInfo, GoalPath),
    goal_info_get_context(GoalInfo, Context),
    add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId, GoalExpr, Context,
        GoalPath, Nonlocals, !VarInfo, !Constraints).

    % add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId, GoalExpr,
    %   Context, GoalPath, Nonlocals, !VarInfo, !Constraints)
    %
    % Adds goal constraints to Constraints to describe the producing and
    % consuming goals for Nonlocals, with respect to GoalExpr and its
    % subgoals.
    % Context and GoalPath refer to the position of GoalExpr, in the
    % source file for the module described in ModuleInfo, and in the
    % body of predicate PredId respectively.
    % VarInfo is used to keep track of the meaning of any constraint
    % variables used. ProgVarset should contain string names for
    % all of the variables in Nonlocals.
    %
:- pred add_goal_expr_constraints(module_info::in, prog_varset::in,
    pred_id::in, hlds_goal_expr::in, prog_context::in, goal_path::in,
    nonlocals::in, mc_var_info::in, mc_var_info::out, mode_constraints::in,
    mode_constraints::out) is det.

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        conj(Goals), Context, GoalPath, Nonlocals, !VarInfo, !Constraints) :-
    list.foldl(add_goal_nonlocals_to_conjunct_production_maps(VarMap, PredId,
        Nonlocals), Goals, conj_constraints_info_init, ConjConstraintsInfo),
    VarMap = rep_var_map(!.VarInfo),

    list.foldl2(add_goal_constraints(ModuleInfo, ProgVarset, PredId),
        Goals, !VarInfo, !Constraints),
    map.foldl(add_local_var_conj_constraints(Context),
        locals_positions(ConjConstraintsInfo), !Constraints),
    map.foldl2(add_nonlocal_var_conj_constraints(ProgVarset, PredId,
        Context, GoalPath), nonlocals_positions(ConjConstraintsInfo),
        !VarInfo, !Constraints).

add_goal_expr_constraints(ModuleInfo, ProgVarset, CallerPredId, GoalExpr,
        Context, GoalPath, _Nonlocals, !VarInfo, !Constraints) :-
    GoalExpr = call(CalleePredId, _, Args, _, _, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),

    ( pred_info_infer_modes(CalleePredInfo) ->
        % No modes declared so just constrain the hearvars
        pred_info_clauses_info(CalleePredInfo, CalleeClausesInfo),
        clauses_info_headvars(CalleeClausesInfo, CalleeHeadVars),
        add_mode_infer_callee(CalleePredId, !Constraints),
        add_call_headvar_constraints(ProgVarset, Context, GoalPath,
            CallerPredId, Args, CalleePredId, CalleeHeadVars,
            !VarInfo, !Constraints)
    ;
        % At least one declared mode
        pred_info_procedures(CalleePredInfo, CalleeProcTable),
        map.values(CalleeProcTable, CalleeProcInfos),
        list.map(proc_info_argmodes, CalleeProcInfos, CalleeArgModeDecls),
        add_call_mode_decls_constraints(ModuleInfo, ProgVarset, Context,
            CallerPredId, CalleeArgModeDecls, GoalPath, Args, !VarInfo,
            !Constraints)
    ).

add_goal_expr_constraints(_ModuleInfo, _ProgVarset, _PredId,
        generic_call(Details, _, _, _), _Context,  _GoalPath,
        _Nonlocals, !VarInfo, !Constraints) :-
    % XXX Need to do something here.
    (
        % XXX Need to do something here.
        Details = higher_order(_, _, _, _),
        sorry(this_file, "higher_order generic_call")
    ;
        % XXX Need to do something here.
        Details = class_method(_, _, _, _),
        sorry(this_file, "class_method generic_call")
    ;
        % No mode constraints
        Details = cast(_)
    ;
        % XXX Need to do something here.
        Details = aditi_builtin(_, _),
        sorry(this_file, "class_method generic_call")
    ).

add_goal_expr_constraints(_ModuleInfo, _ProgVarset, _PredId,
        switch(_, _, _), _Context,  _GoalPath, _Nonlocals, _, _, _, _) :-
    unexpected(this_file, "switch").

add_goal_expr_constraints(_ModuleInfo, ProgVarset, PredId,
        unify(LHSvar, RHS, _Mode, _Kind, _UnifyContext),
        GoalContext, GoalPath, _Nonlocals, !VarInfo, !Constraints) :-
    prog_var_at_path(ProgVarset, PredId, LHSvar, GoalPath,
        LHSvarProducedHere, !VarInfo),
    (
        RHS = var(RHSvar),
        % Goal: LHSvar = RHSvar
        % At most one of the left and right hand sides of a unification
        % is produced at the unification.
        prog_var_at_path(ProgVarset, PredId, RHSvar, GoalPath,
            RHSvarProducedHere, !VarInfo),
        not_both(GoalContext, LHSvarProducedHere, RHSvarProducedHere,
            !Constraints)
    ;
        RHS = functor(_Functor, _IsExistConstr, Args),
        prog_vars_at_path(ProgVarset, PredId, Args, GoalPath,
            ArgsProducedHere, !VarInfo),
        (
            ArgsProducedHere = [OneArgProducedHere, _Two| _],
            % Goal: LHSvar = functor(Args)
            % (a): If one arg is produced here, then they all are.
            % (b): At most one side of the unification is produced.
            equivalent(GoalContext, ArgsProducedHere, !Constraints),
            not_both(GoalContext, LHSvarProducedHere, OneArgProducedHere,
                !Constraints)
        ;
            ArgsProducedHere = [OneArgProducedHere],
            % Goal: LHSvar = functor(Arg)
            % At most one side of the unification is produced.
            not_both(GoalContext, LHSvarProducedHere, OneArgProducedHere,
                !Constraints)
        ;
            ArgsProducedHere = []
            % Goal: LHSvar = functor
            % In this case, LHSvar need not be produced
            % - it could be a test, so no constraints.
        )
    ;
        RHS = lambda_goal(_, _, _, _, _, _, _, _, _),
        sorry(this_file, "unify with lambda goal")
    ).

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        disj(Goals), Context, GoalPath, Nonlocals, !VarInfo, !Constraints) :-
    nonlocals_at_path_and_subpaths(ProgVarset, PredId, GoalPath,
        DisjunctGoalPaths, Nonlocals, NonlocalsHere, NonlocalsAtDisjuncts,
        !VarInfo),

    list.map(snd, Goals, GoalInfos),
    list.map(goal_info_get_goal_path, GoalInfos, DisjunctGoalPaths),

    list.foldl2(add_goal_constraints(ModuleInfo, ProgVarset, PredId),
        Goals, !VarInfo, !Constraints),

    % A variable bound at any disjunct is bound for the disjunction
    % as a whole. If a variable can be bound at one disjunct
    % it must be able to be bound at any.
    EquivVarss = list.map_corresponding(list.cons, NonlocalsHere,
        NonlocalsAtDisjuncts),
    list.foldl(equivalent(Context), EquivVarss, !Constraints).

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        not(Goal), Context, GoalPath, Nonlocals, !VarInfo, !Constraints) :-
    Goal = _ - NegatedGoalInfo,
    goal_info_get_goal_path(NegatedGoalInfo, NegatedGoalPath),
    VarMap = rep_var_map(!.VarInfo),
    NonlocalsAtPath = set.fold(cons_prog_var_at_path(VarMap, PredId, GoalPath),
        Nonlocals, []),
    NonlocalsConstraintVars = set.fold(cons_prog_var_at_path(VarMap, PredId,
        NegatedGoalPath), Nonlocals, NonlocalsAtPath),

    add_goal_constraints(ModuleInfo, ProgVarset, PredId, Goal, !VarInfo,
        !Constraints),

    % The variables non-local to the negation are not to be produced
    % at the negation or any deeper, so we constrain their mode constraint
    % variables for these positions to `no'.
    list.foldl(equiv_no(Context), NonlocalsConstraintVars, !Constraints).

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        scope(_Reason, Goal), Context, GoalPath, Nonlocals, !VarInfo,
        !Constraints) :-
    Goal = _ - SomeGoalInfo,
    goal_info_get_goal_path(SomeGoalInfo, SomeGoalPath),

    % If a program variable is produced by the sub-goal of the some
    % statement, it is produced at the main goal as well
    % - here we pair up equivalent mode constraint vars and
    % then constrain them to reflect this.
    NonlocalsList = set.to_sorted_list(Nonlocals),
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, GoalPath,
        NonlocalsHere, !VarInfo),
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, SomeGoalPath,
        NonlocalsAtSubGoal, !VarInfo),
    EquivVarss = list.map_corresponding(
        func(NlAtPath, NlAtSubPath) = [NlAtPath, NlAtSubPath],
        NonlocalsHere, NonlocalsAtSubGoal),
    list.foldl(equivalent(Context), EquivVarss, !Constraints),

    add_goal_constraints(ModuleInfo, ProgVarset, PredId, Goal, !VarInfo,
        !Constraints).

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        if_then_else(ExistVars, Cond, Then, Else),
        Context, GoalPath, Nonlocals, !VarInfo, !Constraints) :-
    Cond = _ - CondInfo, Then = _ - ThenInfo, Else = _ - ElseInfo,
    goal_info_get_goal_path(CondInfo, CondPath),
    goal_info_get_goal_path(ThenInfo, ThenPath),
    goal_info_get_goal_path(ElseInfo, ElsePath),

    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, GoalPath,
        NonlocalsHere, !VarInfo),
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, CondPath,
        NonlocalsAtCond, !VarInfo),
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, ThenPath,
        NonlocalsAtThen, !VarInfo),
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, ElsePath,
        NonlocalsAtElse, !VarInfo),
    NonlocalsList = set.to_sorted_list(Nonlocals),

    %
    % The existentially quantified variables shared between the condition
    % and the then-part have special constraints.
    %
    goal_info_get_nonlocals(CondInfo, CondNonlocals),
    goal_info_get_nonlocals(ThenInfo, ThenNonlocals),
    list.filter(set.contains(CondNonlocals), ExistVars, NonlocalToCond),
    list.filter(set.contains(ThenNonlocals), NonlocalToCond, LocalAndShared),
    prog_vars_at_path(ProgVarset, PredId, LocalAndShared, CondPath,
        LocalAndSharedAtCond, !VarInfo),
    prog_vars_at_path(ProgVarset, PredId, LocalAndShared, ThenPath,
        LocalAndSharedAtThen, !VarInfo),

    add_goal_constraints(ModuleInfo, ProgVarset, PredId, Cond, !VarInfo,
        !Constraints),
    add_goal_constraints(ModuleInfo, ProgVarset, PredId, Then, !VarInfo,
        !Constraints),
    add_goal_constraints(ModuleInfo, ProgVarset, PredId, Else, !VarInfo,
        !Constraints),

    % If a variable is to be produced at this path,
    % the then and else parts must be able to produce it.
    % Here we group constraint variables into lists to be
    % constrained equivalent to reflect this.
    EquivVarss = list.map_corresponding3(func(A, B, C) = [A, B, C],
        NonlocalsHere, NonlocalsAtThen, NonlocalsAtElse),
    list.foldl(equivalent(Context), EquivVarss, !Constraints),

    % No nonlocal is produced in the condition.
    list.foldl(equiv_no(Context), NonlocalsAtCond, !Constraints),

    % In case the program variable appears in the nonlocal set
    % of the condition, but its value is not used, we do not
    % simply constrain LocalAtCond = yes and LocalAtThen = no.
    % Instead we constrain exactly one of them to be yes.
    list.foldl_corresponding(xor(Context), LocalAndSharedAtCond,
        LocalAndSharedAtThen, !Constraints).

add_goal_expr_constraints(ModuleInfo, ProgVarset, PredId,
        foreign_proc(_, CalledPred, ProcId, ForeignArgs, _, _),
        Context, GoalPath, _Nonlocals, !VarInfo, !Constraints) :-
    CallArgs = list.map(foreign_arg_var, ForeignArgs),
    module_info_pred_proc_info(ModuleInfo, CalledPred, ProcId, _, ProcInfo),
    ( proc_info_maybe_declared_argmodes(ProcInfo, yes(_OrigDecl)) ->
        proc_info_argmodes(ProcInfo, Decl),

        % This pred should strip the disj(conj()) for the single declaration.
        add_call_mode_decls_constraints(ModuleInfo, ProgVarset, Context,
            PredId, [Decl], GoalPath, CallArgs, !VarInfo, !Constraints)
    ;
        unexpected(this_file, "no mode declaration for foreign proc")
    ).

add_goal_expr_constraints(_ModuleInfo, _ProgVarset, _PredId,
        par_conj(_Goals), _Context, _GoalPath, _Nonlocals, _, _, _, _) :-
    % XXX Need to do something here.
    sorry(this_file, "par_conj").

add_goal_expr_constraints(_ModuleInfo, _ProgVarset, _PredId,
        shorthand(_ShorthandGoalExpr), _Context, _GoalPath, _Nonlocals,
        _, _, _, _) :-
    % Shorthand goals should not exist at this point in compilation.
    unexpected(this_file, "shorthand goal").

%-----------------------------------------------------------------------------%

mode_decls_constraints(ModuleInfo, VarMap, PredId, Decls, HeadVarsList,
        Constraints) :-

    % Transform each headvar into the constraint variable representing
    % the proposition that it is produced at the empty goal path (ie
    % that it is produced by a call to the predicate).
    HeadVarsMCVars =
        list.map(list.map(prog_var_at_path(VarMap, PredId, [])), HeadVarsList),
    % Make the constraints for each declaration.
    ConstraintsList = list.map_corresponding(
        mode_decl_constraints(ModuleInfo), HeadVarsMCVars, Decls),

    Constraints0 = list.condense(ConstraintsList),
    ( Constraints0 = [conj(OneModeOnlyConstraints)] ->
        Constraints = OneModeOnlyConstraints
    ;
        Constraints = [disj(Constraints0)]
    ).

add_mode_decl_constraints(ModuleInfo, PredId, ProcId, Decl, Args,
        !VarInfo, !Constraints) :-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_varset(ProcInfo, ProgVarset),
    proc_info_context(ProcInfo, Context),

    prog_vars_at_path(ProgVarset, PredId, Args, [], ArgsAtHead, !VarInfo),

    DeclConstraints = mode_decl_constraints(ModuleInfo, ArgsAtHead, Decl),

    list.foldl(abstract_mode_constraints.add_constraint(Context, ProcId),
        DeclConstraints, !Constraints).

    % add_call_mode_decls_constraints(ModuleInfo, ProgVarset, Context,
    %   CallingPred, Decls, GoalPath, CallArgs, !VarInfo, !Constraints)
    %
    % Adds the following to the mode_constraints:
    % disj([ConstraintsForMode1, ..., ConstraintsForModeN])
    % with context Context, and goal path GoalPath (of the position of
    % the call). ConstraintsForModei are the constraints for when the
    % callee is called in the ith mode of Decls.
    % Assumes CallArgs to be the program variables from CallingPred
    % that are to be used as arguments for the called predicate.
    % Uses and stores constraint variables in VarInfo, finding
    % the string names for program variables in ProgVarset.
    %
    % Note that if Decls is an empty list, this is interpreted as
    % there being no modes for which the predicate can be executed
    % and the returned constraints are [disj([])] which cannot be
    % satisfied. Predicate calls for callee predicates with no declared
    % modes should not be handled by this.
    %
    % The constraints for a (call to a) predicate with declared
    % modes is the disjunction of the constraints for each declared
    % mode. If, according to the mode declaration, a variable is not
    % initially free then it cannot be produced by a call to this
    % goal; If a variable is initially free and becomes not free
    % then it is produced by the predicate. Otherwise it is free ->
    % free and is not produced.
    %
:- pred add_call_mode_decls_constraints(module_info::in,
    prog_varset::in, prog_context::in, pred_id::in, list(list(mer_mode))::in,
    goal_path::in, args::in, mc_var_info::in, mc_var_info::out,
    mode_constraints::in, mode_constraints::out) is det.

add_call_mode_decls_constraints(ModuleInfo, ProgVarset, CallContext,
        CallingPred, Decls, GoalPath, CallArgs, !VarInfo, !Constraints) :-
    prog_vars_at_path(ProgVarset, CallingPred, CallArgs, GoalPath,
        CallArgsHere, !VarInfo),
    ConstraintFormulae = list.condense(list.map(
        mode_decl_constraints(ModuleInfo, CallArgsHere),
        Decls)),
    ( ConstraintFormulae = [conj(OneModeOnlyConstraints)] ->
        list.foldl(add_constraint(CallContext), OneModeOnlyConstraints,
            !Constraints)
    ;
        add_constraint(CallContext, disj(ConstraintFormulae), !Constraints)
    ).

    % add_call_headvar_constraints(ProgVarset, Context, GoalPath, Caller,
    %   CallArgs, Callee, HeadVars, CallArgs, !VarInfo, !Constraints)
    %
    % Forms constraints that, when satisfied, mean any call arg will be
    % produced if the corresponding headvar is produced by the main goal
    % of the called predicate.
    % The constraints are annotated with Context, which should be the
    % context of the call goal in the original program.
    %
    % This should not be used if a mode declaration is supplied, as
    % it means a predicate can only be used in a single mode
    % throughout the whole body of the calling predicate.
    %
:- pred add_call_headvar_constraints(prog_varset::in, prog_context::in,
    goal_path::in, pred_id::in, args::in, pred_id::in, args::in,
    mc_var_info::in, mc_var_info::out, mode_constraints::in,
    mode_constraints::out) is det.

add_call_headvar_constraints(ProgVarset, Context, GoalPath, CallerPredId,
        CallArgs, CalleePredId, CalleeHeadVars, !VarInfo, !Constraints) :-
    prog_vars_at_path(ProgVarset, CalleePredId, CalleeHeadVars, [],
        HeadVarsAtHead, !VarInfo),
    prog_vars_at_path(ProgVarset, CallerPredId, CallArgs, GoalPath,
        CallArgsHere, !VarInfo),

    % If the head var is produced by the body of the callee, then the
    % corresponding argument variable is produced at the goal path
    % of the call.
    EquivVarss = list.map_corresponding(func(A, B) = [A, B],
        HeadVarsAtHead, CallArgsHere),
    list.foldl(equivalent(Context), EquivVarss, !Constraints).

    % mode_decl_constraints(ModuleInfo, ConstraintVars, ArgModes)
    % looks at each mode in ArgModes to see if its variable is produced,
    % and creates constraints for the corresponding constraint variable
    % in ConstraintVars accordingly.
    %
:- func mode_decl_constraints(module_info, list(mc_var), list(mer_mode)) =
    constraint_formulae.

mode_decl_constraints(ModuleInfo, ConstraintVars, ArgModes) =
    [conj(list.map_corresponding(
        single_mode_constraints(ModuleInfo), ConstraintVars, ArgModes))].

    % single_mode_constraints(ModuleInfo, MCVar, Mode) returns a
    % constraint on MCVar such that the program variable it
    % refers to should change instantiation according to Mode
    % at the goal path it refers to.
    %
:- func single_mode_constraints(module_info, mc_var, mer_mode) =
    constraint_formula.

single_mode_constraints(ModuleInfo, MCVar, Mode) =
        atomic_constraint(equiv_bool(MCVar, IsProduced)) :-
    mode_util.mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    (
        % Already produced?
        not inst_match.inst_is_free(ModuleInfo, InitialInst)
    ->
        IsProduced = no     % Not produced here.
    ;
        % free -> non-free
        not inst_match.inst_is_free(ModuleInfo, FinalInst)
    ->
        IsProduced = yes    % Produced here.
    ;
        % free -> free
        IsProduced = no     % Not produced here.
    ).

%-----------------------------------------------------------------------------%

    % add_goal_nonlocals_to_conjunct_production_maps(VarMap, PredId,
    %   Nonlocals, Goal, !ConjConstraintsInfo)
    %
    % Assumes Goal is a conjunct of a conjunction with nonlocal
    % variables Nonlocals, in predicate PredId. Adds the nonlocal
    % variables from Goal to ConjConstraintsInfo according to whether
    % they are local or nonlocal to the conjunction as a whole. The
    % constraint variable propositions that the nonlocals of Goal are
    % produced at Goal are associated with their corresponding program
    % variable in ConjConstraintsInfo. These constraint variables are
    % found in VarMap.
    %
:- pred add_goal_nonlocals_to_conjunct_production_maps(
    mc_var_map::in, pred_id::in, nonlocals::in, hlds_goal::in,
    conj_constraints_info::in, conj_constraints_info::out) is det.

add_goal_nonlocals_to_conjunct_production_maps(VarMap, PredId, Nonlocals,
        _SubGoalExpr - SubGoalInfo, !ConjConstraintsInfo) :-
    goal_info_get_nonlocals(SubGoalInfo, SubGoalNonlocals),
    goal_info_get_goal_path(SubGoalInfo, SubGoalPath),

    % These are variables nonlocal to the conjunction that
    % appear in this particular conjunct.
    Nonlocal = set.intersect(SubGoalNonlocals, Nonlocals),

    % These are variables local to the conjunction that
    % are non-local to this particular conjunct.
    Local = set.difference(SubGoalNonlocals, Nonlocals),

    some [!LocalsMap, !NonlocalsMap] (
        !:LocalsMap = !.ConjConstraintsInfo ^ locals_positions,
        !:NonlocalsMap = !.ConjConstraintsInfo ^ nonlocals_positions,

        set.fold(add_variable_to_conjunct_production_map(VarMap, PredId,
            SubGoalPath), Local, !LocalsMap),
        set.fold(add_variable_to_conjunct_production_map(VarMap, PredId,
            SubGoalPath), Nonlocal, !NonlocalsMap),

        !:ConjConstraintsInfo = !.ConjConstraintsInfo ^ locals_positions :=
            !.LocalsMap,
        !:ConjConstraintsInfo = !.ConjConstraintsInfo ^ nonlocals_positions :=
            !.NonlocalsMap
    ).

    % add_variable_to_conjunct_production_map(VarMap, PredId, GoalPath
    %   ProgVar, !ConjunctProductionMap)
    %
    % A subsection of add_goal_nonlocals_to_conjunct_production_maps,
    % puts into the ConjunctProductionMap the key ProgVar with the
    % constraint variable found in VarMap, that represents the
    % proposition that ProgVar is bound at GoalPath in predicate PredId.
    %
:- pred add_variable_to_conjunct_production_map(
    mc_var_map::in, pred_id::in, goal_path::in, prog_var::in,
    conjunct_production_map::in, conjunct_production_map::out) is det.

add_variable_to_conjunct_production_map(VarMap, PredId, GoalPath, ProgVar,
        !ConjunctProductionMap) :-
    MCVar = prog_var_at_path(VarMap, PredId, GoalPath, ProgVar),
    svmulti_map.set(ProgVar, MCVar, !ConjunctProductionMap).

    % add_nonlocal_var_conj_constraints(ProgVarset, PredId, Context, GoalPath,
    %     ProgVar, ProgVarAtConjuncts, !VarInfo, !Constraints)
    %
    % Adds conjunction constraints concerning ProgVar, a variable
    % in predicate PredID, to Constraints. The conjunction should
    % exist at the position indicated by GoalPath in the predicate,
    % and have context Context in the source file. ProgVarset should
    % contain a string name for ProgVar.
    % ProgVar should be a variable in the nonlocal set of the conjunction,
    % and as such may be produced outside it. Therefore the constraints
    % will be that ProgVar is produced at "at most one" of the conjuncts.
    % ProgVarAtConjuncts should contain the constraint variable
    % propositions that ProgVar is produced at a conjunct for each
    % conjunct in which it appears.
    %
:- pred add_nonlocal_var_conj_constraints(prog_varset::in, pred_id::in,
    prog_context::in, goal_path::in, prog_var::in, list(mc_var)::in,
    mc_var_info::in, mc_var_info::out, mode_constraints::in,
    mode_constraints::out) is det.

add_nonlocal_var_conj_constraints(ProgVarset, PredId, Context, GoalPath,
    ProgVar, ProgVarAtConjuncts, !VarInfo, !Constraints) :-

    equiv_disj(Context, ProgVarAtGoalPath, ProgVarAtConjuncts, !Constraints),
    at_most_one(Context, ProgVarAtConjuncts, !Constraints),

    prog_var_at_path(ProgVarset, PredId, ProgVar, GoalPath, ProgVarAtGoalPath,
        !VarInfo).

    % add_local_var_conj_constraints(Context, ProgVar, MCVars, !Constraints)
    %
    % Adds conjunct constraints relating to ProgVar, to Constraints.
    % ProgVar should be local to the conjunction, and MCVars should
    % represent propositions that ProgVar is produced at conjuncts, for
    % each conjunct that ProgVar is nonlocal to. Context should be
    % the location in the source file that the conjunction in question
    % came from.
    %
    % The constraints are: the program variable must be produced at
    % exactly one conjunct.
    %
:- pred add_local_var_conj_constraints(prog_context::in, prog_var::in,
    list(mc_var)::in, mode_constraints::in, mode_constraints::out) is det.

add_local_var_conj_constraints(Context, _ProgVar, ProgVarAtConjuncts,
        !Constraints) :-
    exactly_one(Context, ProgVarAtConjuncts, !Constraints).

    % Initialises the conj_constraints_info_structure.
    %
:- func conj_constraints_info_init = conj_constraints_info.

conj_constraints_info_init =
    conj_constraints_info(multi_map.init, multi_map.init).

%-----------------------------------------------------------------------------%

    % prog_var_at_path(ProgVarset, PredId, ProgVar, GoalPath, MCVar, !VarInfo)
    %
    % MCVar is the constraint variable corresponding to the proposition
    % that ProgVar is created at GoalPath in predicate PredId,
    % it is created if necessary and stored in VarInfo (in which
    % case we need a string name of ProgVar in ProgVarset).
    %
:- pred prog_var_at_path(prog_varset::in, pred_id::in, prog_var::in,
    goal_path::in, mc_var::out, mc_var_info::in, mc_var_info::out) is det.

prog_var_at_path(ProgVarset, PredId, ProgVar, GoalPath, MCVar, !VarInfo) :-
    !.VarInfo = mc_var_info(MCVarset0, VarMap0),
    ensure_prog_var_at_path(ProgVarset, PredId, GoalPath, ProgVar,
        MCVarset0, MCVarset, VarMap0, VarMap),
    MCVar = prog_var_at_path(VarMap, PredId, GoalPath, ProgVar),
    !:VarInfo = mc_var_info(MCVarset, VarMap).

    % prog_var_at_paths(ProgVarset, PredId, ProgVar, GoalPaths, MCVars,
    %   !VarInfo)
    %
    % MCVars are the constraint variables corresponding to propositions
    % that ProgVar is created at GoalPath in predicate PredId, for each
    % GoalPath in GoalPaths. The constraint variables are created if
    % necessary and stored in VarInfo (in which case we need a string
    % name of ProgVar in ProgVarset).
    %
:- pred prog_var_at_paths(prog_varset::in, pred_id::in, prog_var::in,
    list(goal_path)::in, list(mc_var)::out, mc_var_info::in,
    mc_var_info::out) is det.

prog_var_at_paths(ProgVarset, PredID, ProgVar, GoalPaths, MCVars, !VarInfo) :-
    list.map_foldl(prog_var_at_path(ProgVarset, PredID, ProgVar),
        GoalPaths, MCVars, !VarInfo).

    % prog_vars_at_path(ProgVarset, PredId, ProgVars, GoalPath, MCVars,
    %   !VarInfo)
    %
    % MCVars are the constraint variables corresponding to propositions
    % that ProgVar is created at GoalPath in predicate PredId, for each
    % ProgVar in ProgVars. They are created if necessary and stored in
    % VarInfo (in which case we will need a string name for ProgVar from
    % ProgVarset).
    %
:- pred prog_vars_at_path(prog_varset::in, pred_id::in, list(prog_var)::in,
    goal_path::in, list(mc_var)::out, mc_var_info::in, mc_var_info::out)
    is det.

prog_vars_at_path(ProgVarset, PredId, ProgVars, GoalPath, MCVars, !VarInfo) :-
    list.map_foldl(
        (pred(ProgVar::in, MCVar::out, !.VarInfo::in, !:VarInfo::out) is det :-
            prog_var_at_path(ProgVarset, PredId, ProgVar, GoalPath, MCVar,
                !VarInfo)
        ), ProgVars, MCVars, !VarInfo).

    % ensure_prog_var_at_path(ProgVarset, PredId, GoalPath, ProgVar,
    %   !Varset, !VarMap)
    %
    % Adds, if necessary, to Varset and VarMap the constraint variable
    % and its abstract representation that represents the proposition
    % that ProgVar is produced at GoalPath in PredId (in some mode
    % of execution). ProgVarset should contain a string name for ProgVar.
    %
:- pred ensure_prog_var_at_path(prog_varset::in, pred_id::in, goal_path::in,
    prog_var::in, mc_varset::in, mc_varset::out,
    mc_var_map::in, mc_var_map::out) is det.

ensure_prog_var_at_path(ProgVarset, PredId, GoalPath, ProgVar,
        !Varset, !VarMap) :-
    RepVar = (ProgVar `in` PredId) `at` GoalPath,
    ( bimap.search(!.VarMap, RepVar, _) ->
        true
    ;
        MCVarName = rep_var_to_string(ProgVarset, RepVar),
        svvarset.new_named_var(MCVarName, NewMCVar, !Varset),
        svbimap.det_insert(RepVar, NewMCVar, !VarMap)
    ).

    % prog_var_at_path(VarMap, PredId, GoalPath, ProgVar) = ConstraintVar:
    %
    % Consults the VarMap to get the constraint variable ConstraintVar
    % that represents the proposition that ProgVar is produced at
    % GoalPath in predicate PredId. The lookup function will report an
    % error if the key (ProgVar `in` PredId) `at` GoalPath does not
    % exist in the map.
    %
:- func prog_var_at_path(mc_var_map, pred_id, goal_path, prog_var) = mc_var.

prog_var_at_path(VarMap, PredId, GoalPath, ProgVar) =
    bimap.lookup(VarMap, ((ProgVar `in` PredId) `at` GoalPath)).

    % Retrieves the mode constraint var as per prog_var_at_path, but
    % attaches it to the list supplied rather than return it directly.
    %
:- func cons_prog_var_at_path(mc_var_map, pred_id, goal_path, prog_var,
    list(mc_var)) = list(mc_var).

cons_prog_var_at_path(VarMap, PredId, GoalPath, ProgVar, MCVars) =
    [prog_var_at_path(VarMap, PredId, GoalPath, ProgVar) | MCVars].

    % prog_var_at_paths(VarMap, GoalPaths, ProgVar) = ConstraintVars
    % consults the map to form a list of the constraint variable
    % propositions that ProgVar is produced at each of the paths in
    % GoalPaths respectively.  The lookup function will report an
    % error if the key (ProgVar `in` PredId) `at` GoalPath does not
    % exist in the map for any of the 'GoalPath's in GoalPaths.
    %
:- func prog_var_at_paths(mc_var_map, pred_id, list(goal_path), prog_var) =
    list(mc_var).

prog_var_at_paths(VarMap, PredId, GoalPaths, ProgVar) =
    list.map(
        func(GoalPath) = prog_var_at_path(VarMap, PredId, GoalPath, ProgVar),
        GoalPaths).

    % nonlocals_at_path_and_subpaths(ProgVarset, GoalPath, SubPaths,
    %   Nonlocals, NonlocalsAtPath, NonlocalsAtSubPaths, !VarInfo)
    %
    % consults the VarInfo to find constraint variables associated
    % with each of the program variables in the Nonlocals set for a
    % GoalPath eg a conjunction and its SubPaths (ie the individual
    % conjuncts), although it doesn't check that the SubPaths are
    % indeed subpaths of GoalPath.  Nonlocals are converted to a
    % sorted list so the Nth entry of NonlocalsAtPath and the Nth
    % entry of NonlocalsAtSubPaths are respectively the constraint
    % variable at the goal and a list of the constraint variables
    % for the subgoals, for the same program variable.
    %
:- pred nonlocals_at_path_and_subpaths(prog_varset::in, pred_id::in,
    goal_path::in, list(goal_path)::in, nonlocals::in, list(mc_var)::out,
    list(list(mc_var))::out, mc_var_info::in, mc_var_info::out) is det.

nonlocals_at_path_and_subpaths(ProgVarset, PredId, GoalPath, SubPaths,
        Nonlocals, NonlocalsAtPath, NonlocalsAtSubPaths, !VarInfo) :-
    prog_vars_at_path(ProgVarset, PredId, NonlocalsList, GoalPath,
        NonlocalsAtPath, !VarInfo),
    list.map_foldl(
        (pred(Nl::in, NlAtSubPaths::out, !.VInfo::in, !:VInfo::out) is det :-
            prog_var_at_paths(ProgVarset, PredId, Nl, SubPaths,
                NlAtSubPaths, !VInfo)
        ), NonlocalsList, NonlocalsAtSubPaths, !VarInfo),
    NonlocalsList = set.to_sorted_list(Nonlocals).

%----------------------------------------------------------------------------%

var_info_init = mc_var_info(varset.init, bimap.init).

rep_var_to_string(ProgVarset, (ProgVar `in` _) `at` GoalPath) = RepString :-
    goal_path_to_string(GoalPath, GoalPathString),
    varset.lookup_name(ProgVarset, ProgVar, ProgVarString),
    ( GoalPathString = "" ->
        RepString = ProgVarString
    ;
        RepString = ProgVarString ++ "." ++ GoalPathString
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "build_mode_constraints.m".

%-----------------------------------------------------------------------------%
:- end_module build_mode_constraints.
%-----------------------------------------------------------------------------%
