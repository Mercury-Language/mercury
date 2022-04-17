%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_constraints.m.
% Main author: dmo.
%
% This module implements the top level of the algorithm described in the
% paper "Constraint-based mode analysis of Mercury" by David Overton,
% Zoltan Somogyi and Peter Stuckey. That paper is the main documentation
% of the concepts behind the algorithm as well as the algorithm itself.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_constraints.
:- interface.

:- import_module check_hlds.abstract_mode_constraints.
:- import_module check_hlds.prop_mode_constraints.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred mc_process_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % dump_abstract_constraints(ModuleInfo, Varset, PredConstraintsMap, !IO)
    %
    % Dumps the constraints in the PredConstraintsMap to file
    % modulename.mode_constraints
    %
:- pred dump_abstract_constraints(module_info::in, mc_varset::in,
    pred_constraints_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.build_mode_constraints.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_ordering.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.ordering_mode_constraints.
:- import_module hlds.goal_form.
:- import_module hlds.goal_path.
:- import_module hlds.hhf.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_graph.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mode_robdd.
:- import_module mode_robdd.tfeirn.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module robdd.
:- import_module set.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module string.
:- import_module term.
:- import_module varset.

% :- import_module unsafe.

%-----------------------------------------------------------------------------%

:- typeclass has_mc_info(T) where [
    func mc_info(T) = mode_constraint_info,
    func 'mc_info :='(T, mode_constraint_info) = T
].

:- typeclass has_module_info(T) where [
    func module_info(T) = module_info,
    func 'module_info :='(T, module_info) = T
].

:- typeclass has_ho_modes(T) where [
    func ho_modes(T) = ho_modes,
    func 'ho_modes :='(T, ho_modes) = T
].

mc_process_module(!ModuleInfo, !IO) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, simple_mode_constraints, Simple),
    globals.lookup_bool_option(Globals, prop_mode_constraints, New),

    (
        New = no,
        list.foldl2(convert_pred_to_hhf(Simple), PredIds, !ModuleInfo, !IO),
        get_predicate_sccs(!.ModuleInfo, SCCs),

        % Stage 1: Process SCCs bottom-up to determine variable producers.
        list.foldl2(mc_process_scc(Simple), SCCs,
            map.init, PredConstraintMap, !ModuleInfo),

        % Stage 2: Process SCCs top-down to determine execution order of
        % conjuctions and which modes are needed for each predicate.
        mode_ordering(PredConstraintMap, list.reverse(SCCs), !ModuleInfo, !IO),

        % Stage 3, which would turn the results of the mode analysis
        % into goal annotations that the rest of the compiler can
        % understand, doesn't exist yet.The whole point of this way of
        % doing mode analysis is to gain extra expressive power (e.g.
        % partially instantiated data structures), and the rest of the
        % compiler doesn't handle the extra expressive power yet.

        clear_caches(!IO)
    ;
        New = yes,
        get_predicate_sccs(!.ModuleInfo, SCCs),

        % Preprocess to accommodate implied modes.
        % XXX The following transformation adds more unifications than is
        % necessary; for example, for arguments that will eventually have `in'
        % modes anyway. The resulting loosening of constraints makes analysis
        % take up to twice as long. Therefore, a more subtle approach would
        % likely be a significant optimization.
        list.foldl(ensure_unique_arguments, PredIds, !ModuleInfo),

        % Requantify to avoid the appearance of variables in nonlocal sets
        % that don't appear in the goal. (This makes it appear that the goal
        % consumes the variable.)
        list.foldl(correct_nonlocals_in_pred, PredIds, !ModuleInfo),

        % Stage 1: Process SCCs bottom-up to determine constraints on
        % variable producers and consumers.
        list.foldl3(prop_mode_constraints_in_scc,
            SCCs, !ModuleInfo, var_info_init, VarInfo,
            map.init, AbstractModeConstraints),

        globals.lookup_bool_option(Globals, debug_mode_constraints, Debug),
        (
            Debug = yes,
            ConstraintVarset = mc_varset(VarInfo),
            trace [io(!TIO)] (
                pretty_print_pred_constraints_map(!.ModuleInfo,
                    ConstraintVarset, AbstractModeConstraints, !TIO)
            )
        ;
            Debug = no
        ),

        % Stage 2: Order conjunctions based on solutions to
        % the producer-consumer constraints.
        ConstraintVarMap = rep_var_map(VarInfo),
        mode_reordering(AbstractModeConstraints, ConstraintVarMap, SCCs,
            !ModuleInfo),

        (
            Debug = yes,
            trace [io(!TIO)] (
                list.foldl(
                    ordering_mode_constraints.dump_goal_paths(!.ModuleInfo),
                    SCCs, !TIO)
            )
        ;
            Debug = no
        )
    ).

dump_abstract_constraints(ModuleInfo, ConstraintVarset, ModeConstraints,
        !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".mode_constraints")), ModuleName, FileName, !IO),
    OutputFile = FileName,

    io.open_output(OutputFile, IOResult, !IO),
    (
        IOResult = ok(OutputStream),
        io.set_output_stream(OutputStream, OldOutStream, !IO),
        pretty_print_pred_constraints_map(ModuleInfo, ConstraintVarset,
            ModeConstraints, !IO),
        io.set_output_stream(OldOutStream, _, !IO),
        io.close_output(OutputStream, !IO)
    ;
        IOResult = error(_),
        unexpected($pred, "failed to open " ++ FileName ++ " for output.")
    ).

    % correct_nonlocals_in_pred(PredId, !ModuleInfo) requantifies
    % the clause_body of PredId. This is to ensure that no variable
    % appears in the nonlocal set of a goal that doesn't also appear
    % in that goal.
    %
:- pred correct_nonlocals_in_pred(pred_id::in, module_info::in,
    module_info::out) is det.

correct_nonlocals_in_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    some [!ClausesInfo, !Varset, !Vartypes, !Clauses, !Goals, !RttiVarMaps] (
        pred_info_get_clauses_info(PredInfo0, !:ClausesInfo),
        clauses_info_clauses(!:Clauses, ItemNumbers, !ClausesInfo),
        clauses_info_get_headvar_list(!.ClausesInfo, HeadVars),
        clauses_info_get_varset(!.ClausesInfo, !:Varset),
        clauses_info_get_vartypes(!.ClausesInfo, !:Vartypes),
        clauses_info_get_rtti_varmaps(!.ClausesInfo, !:RttiVarMaps),
        !:Goals = list.map(func(X) = clause_body(X), !.Clauses),
        list.map_foldl3(correct_nonlocals_in_clause_body(HeadVars), !Goals,
            !Varset, !Vartypes, !RttiVarMaps),
        !:Clauses = list.map_corresponding(
            func(Clause, Goal) = 'clause_body :='(Clause, Goal),
            !.Clauses, !.Goals),
        set_clause_list(!.Clauses, ClausesRep),
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers, !ClausesInfo),
        clauses_info_set_varset(!.Varset, !ClausesInfo),
        clauses_info_set_vartypes(!.Vartypes, !ClausesInfo),
        clauses_info_set_rtti_varmaps(!.RttiVarMaps, !ClausesInfo),
        pred_info_set_clauses_info(!.ClausesInfo, PredInfo0, PredInfo)
    ),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

    % correct_nonlocals_in_clause_body(Headvars, !Goals, !Varset, !Vartypes,
    %   RttiVarMaps)
    % requantifies the clause body Goal. This is to ensure that no variable
    % appears in the nonlocal set of a goal that doesn't also appear
    % in that goal.
    %
:- pred correct_nonlocals_in_clause_body(list(prog_var)::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

correct_nonlocals_in_clause_body(Headvars, !Goals, !Varset, !Vartypes,
        !RttiVarMaps) :-
    implicitly_quantify_clause_body_general(ordinary_nonlocals_maybe_lambda,
        Headvars, Warnings, !Goals, !Varset,
        !Vartypes, !RttiVarMaps),
    (
        Warnings = []
    ;
        Warnings = [_ | _],
        unexpected($pred,
            "Quantification error during constraints based mode analysis")
    ).

:- pred mc_process_scc(bool::in, list(pred_id)::in,
    pred_constraint_map::in, pred_constraint_map::out,
    module_info::in, module_info::out) is det.

mc_process_scc(Simple, SCC, !PredConstraintMap, !ModuleInfo) :-
    some [!ModeConstraint, !MCI] (
        !:ModeConstraint = one,
        !:MCI = init_mode_constraint_info(Simple),
        list.foldl2(number_robdd_variables_in_pred, SCC, !ModuleInfo, !MCI),

        save_threshold(!.MCI, Threshold),
        mc_process_scc_pass_1(SCC, SCC, !ModeConstraint, !MCI, !ModuleInfo),

        !:ModeConstraint = restrict_threshold(Threshold, !.ModeConstraint),
        !:ModeConstraint = ensure_normalised(!.ModeConstraint),
        mc_process_scc_pass_2(SCC, !.ModeConstraint, !.MCI, !ModuleInfo),

        Insert =
            ( pred(PredId::in, PCM0::in, PCM::out) is det :-
                NewPCI = pci(!.ModeConstraint,
                    mci_set_pred_id(!.MCI, PredId)),
                map.det_insert(PredId, NewPCI, PCM0, PCM)
            ),
        list.foldl(Insert, SCC, !PredConstraintMap)
        % clear_caches(!IO).
    ).

:- type number_robdd_info
    --->    number_robdd_info(
                n_mc_info       :: mode_constraint_info,
                n_module_info   :: module_info,
                n_vartypes      :: vartypes
            ).

:- instance has_mc_info(number_robdd_info) where [
    func(mc_info/1) is n_mc_info,
    func('mc_info :='/2) is 'n_mc_info :='
].

:- instance has_module_info(number_robdd_info) where [
    func(module_info/1) is n_module_info,
    func('module_info :='/2) is 'n_module_info :='
].

:- pred update_mc_info_t(pred(T, mode_constraint_info, mode_constraint_info),
    T, C, C) <= has_mc_info(C).
:- mode update_mc_info_t(pred(out, in, out) is det, out, in, out) is det.

update_mc_info_t(P, R, !C) :-
    MCInfo0 = !.C ^ mc_info,
    P(R, MCInfo0, MCInfo),
    !C ^ mc_info := MCInfo.

:- pred update_mc_info(pred(mode_constraint_info, mode_constraint_info),
    C, C) <= has_mc_info(C).
:- mode update_mc_info(pred(in, out) is det, in, out) is det.
:- mode update_mc_info(pred(in, out) is semidet, in, out) is semidet.

update_mc_info(P, !C) :-
    MCInfo0 = !.C ^ mc_info,
    P(MCInfo0, MCInfo),
    !C ^ mc_info := MCInfo.

:- pred update_md_info(pred(T, mode_decl_info, mode_decl_info), T, C, C)
    <= (has_mc_info(C), has_ho_modes(C)).
:- mode update_md_info(pred(out, in, out) is det, out, in, out) is det.

update_md_info(P, R, !C) :-
    MCInfo0 = !.C ^ mc_info,
    HOModes0 = !.C ^ ho_modes,
    MDInfo0 = mode_decl_info(MCInfo0, HOModes0),
    P(R, MDInfo0, MDInfo),
    !C ^ mc_info := MDInfo ^ mc_info,
    !C ^ ho_modes := MDInfo ^ ho_modes.

    % Assign a number to all the ROBDD variables that we want to keep at
    % the end of the analysis.
    % This allows us to use `restrict_threshold' during the analysis
    % to remove all unwanted variables.
    % `Restrict_threshold' is much faster than using `robdd.filter'
    % or `robdd.restrict'.
    %
:- pred number_robdd_variables_in_pred(pred_id::in,
    module_info::in, module_info::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

number_robdd_variables_in_pred(PredId, !ModuleInfo, !MCI) :-
    !:MCI = mci_set_pred_id(!.MCI, PredId),
    save_min_var_for_pred(PredId, !MCI),

    % Variables in each branch of a branched goal are always equivalent.
    % Likewise, a variable in a negated or existentially quantified goal
    % will always be equivalent to the variable in the parent goal. This
    % means we can use the same mode_constraint_var for each of these
    % equivalent variables, avoiding adding lots of equivalence constraints
    % to the ROBDD. This is a good thing since equivalence constraints tend
    % to cause exponential explosions in ROBDDs. We achieve this by passing
    % `OmitModeEquivPrefix = yes' to `goal_path.fill_slots_in_clauses'.
    % XXX We do not actually do that anymore, since this (a) that capability
    % has not yet been implemented for the new goal_id system, and (b)
    % this mode analysis system is obsolete.

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    clauses_info_get_headvar_list(ClausesInfo0, HeadVars),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes),

    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    fill_goal_id_slots_in_clauses(!.ModuleInfo, ContainingGoalMap,
        ClausesInfo0, ClausesInfo1),
    pred_info_set_clauses_info(ClausesInfo1, PredInfo0, PredInfo1),

    ForwardGoalPathMap = create_forward_goal_path_map(ContainingGoalMap),
    add_forward_goal_path_map(PredId, ForwardGoalPathMap, !MCI),

    pred_info_get_inst_graph_info(PredInfo1, InstGraphInfo),
    InstGraph = InstGraphInfo ^ implementation_inst_graph,
    inst_graph.foldl_reachable_from_list(
        ( pred(V::in, S0::in, S::out) is det :-
            mode_constraint_var(in(V), _, S0, S1),
            mode_constraint_var(out(V), _, S1, S2),
            mode_constraint_var(V `at` goal_id(0), _, S2, S)
        ), InstGraph, HeadVars, !MCI),

    ( if pred_info_is_imported(PredInfo1) then
        true
    else
        clauses_info_clauses(Clauses2, ItemNumbers,
            ClausesInfo1, ClausesInfo2),
        NRInfo0 = number_robdd_info(!.MCI, !.ModuleInfo, VarTypes),

        list.map_foldl(
            ( pred(Clause0::in, Clause::out, S0::in, S::out) is det :-
                Goal0 = Clause0 ^ clause_body,
                number_robdd_variables_in_goal(InstGraph,
                    set_of_var.init, _, Goal0, Goal, S0, S),
                Clause = Clause0 ^ clause_body := Goal
            ), Clauses2, Clauses, NRInfo0, NRInfo),

        !:MCI = NRInfo ^ mc_info,
        set_clause_list(Clauses, ClausesRep),
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
            ClausesInfo2, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ),
    save_max_var_for_pred(PredId, !MCI).

:- pred number_robdd_variables_in_goal(inst_graph::in,
    set_of_progvar::in, set_of_progvar::out, hlds_goal::in, hlds_goal::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_in_goal(InstGraph, ParentNonLocals, Occurring,
        Goal0, Goal, !RInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    GoalId = goal_info_get_goal_id(GoalInfo0),
    number_robdd_variables_in_goal_2(InstGraph, GoalId, ParentNonLocals,
        NonLocals, Occurring, GoalExpr0, GoalExpr, !RInfo),
    goal_info_set_occurring_vars(Occurring, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred number_robdd_variables_in_goal_2(inst_graph::in, goal_id::in,
    set_of_progvar::in, set_of_progvar::in, set_of_progvar::out,
    hlds_goal_expr::in, hlds_goal_expr::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        conj(ConjType, Goals0), conj(ConjType, Goals), !RInfo) :-
    number_robdd_variables_in_goals(InstGraph, NonLocals, Occurring,
        Goals0, Goals, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        disj(Goals0), disj(Goals), !RInfo) :-
    number_robdd_variables_in_goals(InstGraph, NonLocals, Occurring,
        Goals0, Goals, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        switch(V, CF, Cases0), switch(V, CF, Cases), !RInfo) :-
    number_robdd_variables_in_cases(InstGraph, NonLocals, Occurring,
        Cases0, Cases, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        negation(Goal0), negation(Goal), !RInfo) :-
    number_robdd_variables_in_goal(InstGraph, NonLocals, Occurring,
        Goal0, Goal, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        scope(Reason, Goal0), scope(Reason, Goal), !RInfo) :-
    number_robdd_variables_in_goal(InstGraph, NonLocals, Occurring,
        Goal0, Goal, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
        if_then_else(Vs, Cond0, Then0, Else0),
        if_then_else(Vs, Cond, Then, Else), !RInfo) :-
    number_robdd_variables_in_goal(InstGraph, NonLocals, OccCond,
        Cond0, Cond, !RInfo),
    number_robdd_variables_in_goal(InstGraph, NonLocals, OccThen,
        Then0, Then, !RInfo),
    number_robdd_variables_in_goal(InstGraph, NonLocals, OccElse,
        Else0, Else, !RInfo),
    Occurring = OccCond `set_of_var.union` OccThen `set_of_var.union` OccElse.
number_robdd_variables_in_goal_2(_, _, _, _, _, shorthand(_), _, !RInfo) :-
    unexpected($pred, "shorthand").
% number_robdd_variables_in_goal_2(InstGraph, _, _, NonLocals, Occurring,
%         atomic_goal(GoalType, Inner, Outer, Vars, MainGoal0, OrElseGoals0),
%         atomic_goal(GoalType, Inner, Outer, Vars, MainGoal, OrElseGoals),
%         !RInfo) :-
%     number_robdd_variables_in_goal(InstGraph, NonLocals, OccMain,
%         MainGoal0, MainGoal, !RInfo),
%     number_robdd_variables_in_goals(InstGraph, NonLocals, OccOrElse,
%         OrElseGoals0, OrElseGoals, !RInfo),
%     Occurring = OccMain `set.union` OccOrElse.
number_robdd_variables_in_goal_2(InstGraph, GoalId, ParentNonLocals, _,
        Occurring, GoalExpr, GoalExpr, !RInfo) :-
    GoalExpr = plain_call(_, _, Args, _, _, _),
    number_robdd_variables_at_goal_path(InstGraph, GoalId,
        ParentNonLocals, Args, Occurring, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, GoalId, ParentNonLocals, _,
        Occurring, GoalExpr, GoalExpr, !RInfo) :-
    GoalExpr = generic_call(_, Args, _, _, _),
    number_robdd_variables_at_goal_path(InstGraph, GoalId,
        ParentNonLocals, Args, Occurring, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, GoalId, ParentNonLocals, _,
        Occurring, GoalExpr0, GoalExpr, !RInfo) :-
    GoalExpr0 = unify(VarL, RHS0, _, _, _),
    number_robdd_variables_in_rhs(InstGraph, GoalId, Vars, RHS0, RHS,
        !RInfo),
    GoalExpr = GoalExpr0 ^ unify_rhs := RHS,
    number_robdd_variables_at_goal_path(InstGraph, GoalId,
        ParentNonLocals, [VarL | Vars], Occurring, !RInfo).
number_robdd_variables_in_goal_2(InstGraph, GoalId, ParentNonLocals, _,
        Occurring, GoalExpr, GoalExpr, !RInfo) :-
    GoalExpr = call_foreign_proc(_, _, _, Args, _, _, _),
    ArgVars = list.map(foreign_arg_var, Args),
    number_robdd_variables_at_goal_path(InstGraph, GoalId,
        ParentNonLocals, ArgVars, Occurring, !RInfo).

:- pred number_robdd_variables_in_rhs(inst_graph::in, goal_id::in,
    list(prog_var)::out, unify_rhs::in, unify_rhs::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_in_rhs(_, _, Vars, !RHS, !NRInfo) :-
    !.RHS = rhs_var(VarR),
    Vars = [VarR].
number_robdd_variables_in_rhs(_, _, Vars, !RHS, !NRInfo) :-
    !.RHS = rhs_functor(_, _, Args),
    Vars = Args.
number_robdd_variables_in_rhs(InstGraph, GoalId, Vars, !RHS, !NRInfo) :-
    !.RHS = rhs_lambda_goal(_, _, _, _, LambdaNonLocals, ArgVarsModes,
        _, LambdaGoal0),
    Vars = LambdaNonLocals,
    assoc_list.keys(ArgVarsModes, ArgVars),
    update_mc_info(enter_lambda_goal(GoalId), !NRInfo),

    % Number arguments to the lambda goal, i.e. the nonlocals and the
    % lambda-quantified variables.
    LambdaHeadVars = LambdaNonLocals ++ ArgVars,
    update_mc_info(pred(in, out) is det -->
        inst_graph.foldl_reachable_from_list(
            ( pred(V::in, in, out) is det -->
                mode_constraint_var(in(V), _),
                mode_constraint_var(out(V), _),
                mode_constraint_var(V `at` whole_body_goal_id, _)
            ), InstGraph, LambdaHeadVars), !NRInfo),

    % Number variables within the lambda goal.
    number_robdd_variables_in_goal(InstGraph, set_of_var.init, _Occurring,
        LambdaGoal0, LambdaGoal, !NRInfo),

    update_mc_info(leave_lambda_goal, !NRInfo),
    !RHS ^ rhs_lambda_goal := LambdaGoal.

:- pred number_robdd_variables_at_goal_path(inst_graph::in, goal_id::in,
    set_of_progvar::in, list(prog_var)::in, set_of_progvar::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_at_goal_path(InstGraph, GoalId, ParentNonLocals,
        Vars0, Occurring, !NRInfo) :-
    solutions.solutions(inst_graph.reachable_from_list(InstGraph, Vars0),
        OccurringList),
    set_of_var.list_to_set(OccurringList, Occurring),
    Vars = set_of_var.to_sorted_list(ParentNonLocals `set_of_var.union`
        set_of_var.list_to_set(Vars0)),
    % XXX We may be able to make this more efficient.
    inst_graph.foldl_reachable_from_list(
        (pred(V::in, S0::in, S::out) is det :-
            update_mc_info_t(mode_constraint_var(V `at` GoalId), _, S0, S)
        ), InstGraph, Vars, !NRInfo).

:- pred number_robdd_variables_in_goals(inst_graph::in, set_of_progvar::in,
    set_of_progvar::out, hlds_goals::in, hlds_goals::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_in_goals(_, _, Occurring, [], [], !RInfo) :-
    set_of_var.init(Occurring).
number_robdd_variables_in_goals(InstGraph, NonLocals, Occurring,
        [Goal0 | Goals0], [Goal | Goals], !RInfo) :-
    number_robdd_variables_in_goal(InstGraph, NonLocals, Occurring0,
        Goal0, Goal, !RInfo),
    number_robdd_variables_in_goals(InstGraph, NonLocals, Occurring1,
        Goals0, Goals, !RInfo),
    Occurring = Occurring0 `set_of_var.union` Occurring1.

:- pred number_robdd_variables_in_cases(inst_graph::in, set_of_progvar::in,
    set_of_progvar::out, list(case)::in, list(case)::out,
    number_robdd_info::in, number_robdd_info::out) is det.

number_robdd_variables_in_cases(_, _, Occurring, [], [], !RInfo) :-
    set_of_var.init(Occurring).
number_robdd_variables_in_cases(InstGraph, NonLocals, Occurring,
        [Case0 | Cases0], [Case | Cases], !RInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    number_robdd_variables_in_goal(InstGraph, NonLocals, Occurring0,
        Goal0, Goal, !RInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    number_robdd_variables_in_cases(InstGraph, NonLocals, Occurring1,
        Cases0, Cases, !RInfo),
    Occurring = Occurring0 `set_of_var.union` Occurring1.

%-----------------------------------------------------------------------------%

:- pred mc_process_scc_pass_1(list(pred_id)::in,
    list(pred_id)::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out,
    module_info::in, module_info::out) is det.

mc_process_scc_pass_1([], _, !ModeConstraint, !MCI, !ModuleInfo).
mc_process_scc_pass_1([PredId | PredIds], SCC,
        !ModeConstraint, !MCI, !ModuleInfo) :-
    !:MCI = mci_set_pred_id(!.MCI, PredId),
    mc_process_pred(PredId, SCC, !ModeConstraint, !MCI, !ModuleInfo),
    mc_process_scc_pass_1(PredIds, SCC, !ModeConstraint, !MCI, !ModuleInfo).

:- pred mc_process_scc_pass_2(list(pred_id)::in,
    mode_constraint::in, mode_constraint_info::in,
    module_info::in, module_info::out) is det.

mc_process_scc_pass_2([], _, _, !ModuleInfo).
mc_process_scc_pass_2([PredId | PredIds], ModeConstraint, MCI, !ModuleInfo) :-
    mc_process_pred_2(PredId, ModeConstraint,
        mci_set_pred_id(MCI, PredId), !ModuleInfo),
    mc_process_scc_pass_2(PredIds, ModeConstraint, MCI, !ModuleInfo).

:- pred mc_process_pred(pred_id::in, list(pred_id)::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out,
    module_info::in, module_info::out) is det.

mc_process_pred(PredId, SCC, !ModeConstraint, !MCI,
        !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    trace [io(!IO)] (
        write_pred_progress_message(!.ModuleInfo,
            "Calculating mode constraints for ", PredId, !IO),
        io.flush_output(!IO)
    ),

    pred_info_get_inst_graph_info(PredInfo0, InstGraphInfo),
    InstGraph = InstGraphInfo ^ implementation_inst_graph,
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_headvar_list(ClausesInfo0, HeadVars),

    HOModes0 = map.init,
    ( if
        ( map.is_empty(ProcTable0)
        ; pred_info_infer_modes(PredInfo0)
        )
    then
        DeclConstraint = one,
        HOModes = HOModes0,
        PredInfo1 = PredInfo0
    else
        ModeDeclInfo0 = mode_decl_info(!.MCI, HOModes0),
        map.map_foldl2(
            mode_decl_to_constraint(!.ModuleInfo, InstGraph, HeadVars,
                PredInfo0),
            ProcTable0, ProcTable,
            zero, DeclConstraint, ModeDeclInfo0, ModeDeclInfo),
        !:MCI = ModeDeclInfo ^ mc_info,
        HOModes = ModeDeclInfo ^ ho_modes,
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo1)
    ),
    !:ModeConstraint = !.ModeConstraint * DeclConstraint,
    set_input_nodes(!ModeConstraint, !MCI),

    % clauses_info_get_varset(ClausesInfo0, ProgVarSet),
    % pred_id_to_int(PredId, PredIdInt),
    % robdd_to_dot(DeclConstraint, ProgVarSet, MCI,
    %   format("mode_decl_%d.dot", [i(PredIdInt)]), !IO),
    % robdd_to_dot(ModeConstraint1, ProgVarSet, MCI,
    %   format("mode_constraint1_%d.dot", [i(PredIdInt)]), !IO),
    % io.flush_output(!IO),

    ( if pred_info_is_imported(PredInfo1) then
        PredInfo = PredInfo1
    else
        process_clauses_info(!.ModuleInfo, SCC, ClausesInfo0, ClausesInfo,
            InstGraph, HOModes, !ModeConstraint, !MCI),
        pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo)
    ),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred mc_process_pred_2(pred_id::in, mode_constraint::in,
    mode_constraint_info::in, module_info::in, module_info::out) is det.

mc_process_pred_2(PredId, ModeConstraint, MCI0, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_inst_graph_info(PredInfo0, InstGraphInfo),
    InstGraph = InstGraphInfo ^ implementation_inst_graph,
    pred_info_get_clauses_info(PredInfo0, ClausesInfo),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),

    % DMO document this better
    % XXX Needed for analysing calls. May want to store the constraint
    % as an ROBDD instead.
    solutions(arg_modes_map(HeadVars, InstGraph, ModeConstraint, MCI0), Modes),
    pred_info_set_arg_modes_maps(Modes, PredInfo0, PredInfo),
    % PredInfo = PredInfo0,

    % DEBUGGING CODE
    % dump_mode_constraints(!.ModuleInfo, PredInfo0, InstGraph,
    %   ModeConstraint, MCI0),
    % io.flush_output(!IO),
    %
    % list.foldl((pred(M - _::in, di, uo) is det -->
    %   map.foldl((pred(_MV::in, Val::in, di, uo) is det -->
    %       io.write_string(Val = yes -> "1 " ; "0 ")
    %   ), M),
    %   io.nl
    % ), Modes),
    %
    % io.nl(!IO),
    %
    % solutions(inst_graph.reachable_from_list(InstGraph, HeadVars),
    %   ReachVars),
    % list.map_foldl((pred(PV::in, MV::out, in, out) is det -->
    %   mode_constraint_var(in(PV), MV)
    % ), ReachVars, InVars, MCI0, MCI),
    %
    % InVarConstraint = restrict_filter((pred(in(V)::in) is semidet :-
    %       list.member(V, ReachVars)),
    %   MCI, ModeConstraint),
    % aggregate(fundamental_mode(set.list_to_set(InVars), InVarConstraint),
    %   (pred(M::in, di, uo) is det -->
    %       map.foldl((pred(_MV::in, Val::in, di, uo) is det -->
    %           io.write_string(Val = yes -> "1 " ; "0 ")
    %       ), M),
    %   io.nl
    % ), !IO),

    % DMO justify or delete
    % split_constraint_into_modes(PredId, HeadVars, InstGraph,
    %   ModeConstraint, _ProcConstraints, MCI0, MCI),

    % DEBUGGING CODE
    % clauses_info_get_varset(ClausesInfo, ProgVarSet),
    % pred_info_name(PredInfo, Name),
    % robdd_to_dot(ModeConstraint, ProgVarSet, MCI, Name ++ ".dot", !IO),
    % io.flush_output(!IO),

    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- type goal_constraints_info
    --->    goal_constraints_info(
                g_module_info   :: module_info,
                scc             :: list(pred_id),
                inst_graph      :: inst_graph,
                headvars        :: list(prog_var),
                prog_varset     :: prog_varset,
                atomic_goals    :: set(goal_id),
                g_mc_info       :: mode_constraint_info,
                g_ho_modes      :: ho_modes,
                ho_calls        :: ho_calls
            ).

:- instance has_mc_info(goal_constraints_info) where [
    func(mc_info/1) is g_mc_info,
    func('mc_info :='/2) is 'g_mc_info :='
].

:- instance has_module_info(goal_constraints_info) where [
    func(module_info/1) is g_module_info,
    func('module_info :='/2) is 'g_module_info :='
].

:- instance has_ho_modes(goal_constraints_info) where [
    func(ho_modes/1) is g_ho_modes,
    func('ho_modes :='/2) is 'g_ho_modes :='
].

:- type ho_modes ==
    multi_map(prog_var_and_level, list(mer_mode)).

:- type ho_calls ==
    multi_map(prog_var_and_level, pair(goal_id, list(prog_var))).

:- pred get_var(rep_var::in, mode_constraint_var::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

get_var(RepVar, MCVar, !GCInfo) :-
    update_mc_info_t(mode_constraint_var(RepVar), MCVar, !GCInfo).

:- pred get_var_in_pred(pred_id::in, rep_var::in, mode_constraint_var::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

get_var_in_pred(PredId, RepVar, MCVar, !GCInfo) :-
    update_mc_info_t(mode_constraint_var(PredId, RepVar), MCVar, !GCInfo).

:- pred add_atomic_goal(goal_id::in,
    goal_constraints_info::in, goal_constraints_info::out) is det.

add_atomic_goal(GoalId, !GCInfo) :-
    AtomicGoals = !.GCInfo ^ atomic_goals,
    !GCInfo ^ atomic_goals := AtomicGoals `set.insert` GoalId.

:- type mode_decl_info
    --->    mode_decl_info(
                d_mc_info   :: mode_constraint_info,
                d_ho_modes  :: ho_modes
            ).

:- instance has_mc_info(mode_decl_info) where [
    func(mc_info/1) is d_mc_info,
    func('mc_info :='/2) is 'd_mc_info :='
].

:- instance has_ho_modes(mode_decl_info) where [
    func(ho_modes/1) is d_ho_modes,
    func('ho_modes :='/2) is 'd_ho_modes :='
].

    % Convert a procedure's arg_modes to a constraint.
    %
:- pred mode_decl_to_constraint(module_info::in,
    inst_graph::in, list(prog_var)::in, pred_info::in, proc_id::in,
    proc_info::in, proc_info::out,
    mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

mode_decl_to_constraint(ModuleInfo, InstGraph, HeadVars,
        _PredInfo, _ProcId, !ProcInfo, !Constraint, !MDI) :-
    process_mode_decl_for_proc(ModuleInfo,
        InstGraph, HeadVars,
        false_var(initial), true_var(initial), yes,
        false_var(final), true_var(final), no,
        !.ProcInfo, zero, DeclConstraint, !MDI),

    % proc_id_to_int(ProcId, ProcIdInt),
    % pred_info_name(PredInfo, Name),
    % pred_info_clauses_info(PredInfo, ClausesInfo),
    % clauses_info_get_varset(ClausesInfo, ProgVarSet),
    % unsafe_perform_io(robdd_to_dot(DeclConstraint, ProgVarSet,
    %   !.MDI ^ mc_info, Name ++ int_to_string(ProcIdInt) ++ ".dot")),

    !:Constraint = !.Constraint + DeclConstraint,
    proc_info_set_head_modes_constraint(DeclConstraint, !ProcInfo).

:- pred process_mode_decl_for_proc(module_info::in,
    inst_graph::in, list(prog_var)::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in,
    proc_info::in, mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

process_mode_decl_for_proc(ModuleInfo, InstGraph, HeadVars,
        InitialFree, InitialBound, InitialHO, FinalFree, FinalBound, FinalHO,
        ProcInfo, !Constraint, !MDI) :-
    % proc_info_declared_argmodes(ProcInfo, ArgModes),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    process_mode_decl(ModuleInfo, InstGraph, HeadVars,
        InitialFree, InitialBound, InitialHO, FinalFree, FinalBound, FinalHO,
        ArgModes, !Constraint, !MDI).

:- pred process_mode_decl(module_info::in,
    inst_graph::in, list(prog_var)::in, constrain_var::in(constrain_var),
    constrain_var::in(constrain_var), bool::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in, list(mer_mode)::in, mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

process_mode_decl(ModuleInfo, InstGraph, HeadVars,
        InitialFree, InitialBound, InitialHO,
        FinalFree, FinalBound, FinalHO, ArgModes, !Constraint, !MDI) :-
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, VarModes),
    list.foldl2(process_arg_modes(ModuleInfo, InstGraph,
        InitialFree, InitialBound, InitialHO, FinalFree, FinalBound, FinalHO),
        VarModes, one, NewConstraint, !MDI),
    !:Constraint = !.Constraint + NewConstraint.

:- pred process_arg_modes(module_info::in, inst_graph::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in,
    pair(prog_var, mer_mode)::in,
    mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

process_arg_modes(ModuleInfo, InstGraph,
        InitialFree, InitialBound, InitialHO,
        FinalFree, FinalBound, FinalHO,
        Var - Mode, !Constraint, !MDI) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    process_inst(ModuleInfo, InstGraph,
        InitialFree, InitialBound, InitialHO, InitialInst,
        set_of_var.init, Var, !Constraint, !MDI),
    process_inst(ModuleInfo, InstGraph,
        FinalFree, FinalBound, FinalHO, FinalInst,
        set_of_var.init, Var, !Constraint, !MDI).

:- func initial(prog_var) = rep_var.

initial(Var) = in(Var).

:- func final(prog_var) = rep_var.

final(Var) = out(Var).

:- func var_at_goal_id(goal_id, prog_var) = rep_var.

var_at_goal_id(GoalId, Var) = Var `at` GoalId.

:- pred true_var((func(prog_var) = rep_var)::in(func(in) = out is det),
    prog_var::in, mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

true_var(F, V, !C, !MCI) :-
    mode_constraint_var(F(V), CV, !MCI),
    !:C = !.C ^ var(CV).

:- pred false_var((func(prog_var) = rep_var)::in(func(in) = out is det),
    prog_var::in, mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

false_var(F, V, !C, !MCI) :-
    mode_constraint_var(F(V), CV, !MCI),
    !:C = !.C ^ not_var(CV).

:- pred ignore(prog_var::in, mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

ignore(_, !C, !MCI).

:- pred call_in(goal_id::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

call_in(Path, Var, !C, !MCI) :-
    mode_constraint_var(Var `at` Path, VarGP, !MCI),
    mode_constraint_var(out(Var), VarOut, !MCI),
    !:C = !.C ^ not_var(VarGP) ^ var(VarOut).

:- pred call_out(goal_id::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

call_out(Path, Var, C0, C, !MCI) :-
    mode_constraint_var(Var `at` Path, VarGP, !MCI),
    C1 = C0 ^ var(VarGP),
    ( if C1 = zero then
        C = C0
    else
        C = C1
    ).

:- type constrain_var == pred(prog_var, mode_constraint, mode_constraint,
    mode_constraint_info, mode_constraint_info).
:- inst constrain_var == (pred(in, in, out, in, out) is det).

:- pred process_inst(module_info::in, inst_graph::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in, mer_inst::in, set_of_progvar::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

process_inst(ModuleInfo, InstGraph, Free, Bound, DoHO, Inst,
        Seen, Var, !Constraint, !MDI) :-
    ( if set_of_var.member(Seen, Var) then
        true
    else
        ( if Inst = defined_inst(InstName) then
            inst_lookup(ModuleInfo, InstName, Inst1),
            process_inst(ModuleInfo, InstGraph,
                Free, Bound, DoHO, Inst1, Seen, Var, !Constraint, !MDI)
        else
            do_process_inst(ModuleInfo, InstGraph,
                Free, Bound, DoHO, Inst, Seen, Var, !Constraint, !MDI)
        )
    ).

:- pred do_process_inst(module_info::in, inst_graph::in,
    constrain_var::in(constrain_var), constrain_var::in(constrain_var),
    bool::in, mer_inst::in, set_of_progvar::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    mode_decl_info::in, mode_decl_info::out) is det.

do_process_inst(ModuleInfo, InstGraph, Free, Bound, DoHO,
        Inst, Seen, Var, !Constraint, !MDI) :-
    update_mc_info_t(
        ( pred(C::out, S0::in, S::out) is det :-
            ( if
                ( Inst = any(_, _)
                ; Inst = bound(_, _, _)
                ; Inst = ground(_, _)
                )
            then
                Bound(Var, !.Constraint, C, S0, S)
            else if
                ( Inst = free
                ; Inst = free(_)
                )
            then
                Free(Var, !.Constraint, C, S0, S)
            else
                C = !.Constraint,
                S = S0
            )
        ), !:Constraint, !MDI),

    map.lookup(InstGraph, Var, node(Functors, _)),
    map.foldl2(
        ( pred(ConsId::in, Vs::in, C0::in, C::out, S0::in, S::out) is det :-
            ( if Inst = bound(_, _, BIs) then
                ( if cons_id_in_bound_insts(ConsId, BIs, Insts) then
                    assoc_list.from_corresponding_lists(Vs, Insts, VarInsts),
                    list.foldl2(
                        ( pred((V - I)::in, C1::in, C2::out,
                                T0::in, T::out) is det :-
                            set_of_var.insert(Var, Seen, SeenVar),
                            process_inst(ModuleInfo, InstGraph,
                                Free, Bound, DoHO, I, SeenVar,
                                V, C1, C2, T0, T)
                        ), VarInsts, C0, C, S0, S)
                else
                    C = C0,
                    S = S0
                )
            else
                set_of_var.insert(Var, Seen, SeenVar),
                list.foldl2(
                    process_inst(ModuleInfo, InstGraph,
                        Free, Bound, DoHO, Inst, SeenVar),
                    Vs, C0, C, S0, S)
            )
        ), Functors, !Constraint, !MDI),
    ( if
        DoHO = yes,
        Inst = ground(_, higher_order(pred_inst_info(_, ArgModes, _, _)))
    then
        HoModes0 = !.MDI ^ ho_modes,
        MCI = !.MDI ^ mc_info,
        get_prog_var_level(MCI, Var, VarLevel),
        multi_map.set(VarLevel, ArgModes, HoModes0, HoModes),
        !MDI ^ ho_modes := HoModes
    else
        true
    ).

:- pred process_clauses_info(module_info::in,
    list(pred_id)::in, clauses_info::in, clauses_info::out, inst_graph::in,
    ho_modes::in, mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

process_clauses_info(ModuleInfo, SCC, !ClausesInfo,
        InstGraph, HOModes0, !Constraint, !MCI) :-
    clauses_info_get_varset(!.ClausesInfo, VarSet0),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            inst_graph.dump(InstGraph, VarSet0, !IO)
        )
    ;
        VeryVerbose = no
    ),

    clauses_info_get_headvar_list(!.ClausesInfo, HeadVars),
    map.foldl2(input_output_constraints(HeadVars, InstGraph),
        InstGraph, !Constraint, !MCI),

    clauses_info_clauses(Clauses, _ItemNumbers, !ClausesInfo),
    Goals = list.map(clause_body, Clauses),
    DisjGoal = disj(Goals),
    AtomicGoals0 = set.init,
    GCInfo0 = goal_constraints_info(ModuleInfo, SCC, InstGraph, HeadVars,
        VarSet0, AtomicGoals0, !.MCI, HOModes0, map.init),
    NonLocals = set_of_var.list_to_set(HeadVars),
    GoalVars = set_of_var.sorted_list_to_set(map.sorted_keys(InstGraph)),

    goal_constraints_2(whole_body_goal_id, NonLocals, GoalVars, _CanSucceed,
        DisjGoal, _, !Constraint, GCInfo0, GCInfo1),

    % DMO justify this or eliminate it
    % constrict_to_vars(HeadVars, GoalVars, [], !Constraint,
    %   Info1, Info2),
    GCInfo2 = GCInfo1,

    % robdd_to_dot(!.Constraint, Info2 ^ prog_varset,
    %   Info2 ^ mc_info, "before_higher_order.dot, !IO"),
    % io.flush_output(!IO),

    higher_order_call_constraints(!Constraint, GCInfo2, GCInfo),

    % robdd_to_dot(!.Constraint, GCInfo ^ prog_varset,
    %   GCInfo ^ mc_info, "after_higher_order.dot", !IO),
    % io.flush_output(!IO),

    clauses_info_set_varset(GCInfo ^ prog_varset, !ClausesInfo),
    !:MCI = GCInfo ^ mc_info.

    % 1.2.1 Input output constraints.
    % These constraints relate the relationships between the above
    % variables and relationships of boundedness on input and output.
    %
:- pred input_output_constraints(list(prog_var)::in, inst_graph::in,
    prog_var::in, inst_graph.node::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

input_output_constraints(HeadVars, InstGraph, V, Node, !Constraint, !MCI) :-
    % For each node V not reachable from an argument node, add Vin = 0.
    inst_graph.top_level_node(InstGraph, V, TopLevel),
    mode_constraint_var(in(V), V_in, !MCI),
    mode_constraint_var(out(V), V_out, !MCI),
    mode_constraint_var(V `at` whole_body_goal_id, V_, !MCI),
    ( if TopLevel `list.member` HeadVars then
        % For each variable V in the instantiation graph, add
        %   (Vout = Vin + V), ~(Vin * V).
        !:Constraint = !.Constraint ^ io_constraint(V_in, V_out, V_)
    else
        !:Constraint = !.Constraint ^ not_var(V_in) ^ eq_vars(V_out, V_)
    ),

    % For each node V in the graph with child f with child W, add
    %   Wout -> Vout, Win -> Vin.
    Node = node(Functors, _),
    map.values(Functors, Children0),
    list.condense(Children0, Children),
    list.foldl2(add_in_and_out_implications(V, V_in, V_out), Children,
        !Constraint, !MCI).

:- pred add_in_and_out_implications(prog_var::in,
    mode_constraint_var::in, mode_constraint_var::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

add_in_and_out_implications(V, V_in, V_out, W, !Cs, !MCI) :-
    ( if W = V then
        true
    else
        mode_constraint_var(in(W), W_in, !MCI),
        mode_constraint_var(out(W), W_out, !MCI),
        !:Cs = !.Cs ^ imp_vars(W_out, V_out) ^ imp_vars(W_in, V_in)
    ).

:- type can_succeed == bool.

:- pred goal_constraints(set_of_progvar::in, can_succeed::out, hlds_goal::in,
    hlds_goal::out, mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

goal_constraints(ParentNonLocals, CanSucceed, Goal0, Goal,
        !Constraint, !GCInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr0),
    (
        HasSubGoals = has_subgoals
    ;
        HasSubGoals = does_not_have_subgoals,
        add_atomic_goal(GoalId, !GCInfo)
    ),

    GoalId = goal_info_get_goal_id(GoalInfo0),
    goal_info_get_occurring_vars(GoalInfo0, Vars),

    % Number the vars we want to keep for this goal.
    % XXX
    list.foldl(
        ( pred(V::in, S0::in, S::out) is det :-
            get_var(V `at` GoalId, _, S0, S)
        ), set_of_var.to_sorted_list(Vars), !GCInfo),
    save_threshold(!.GCInfo ^ mc_info, Threshold),

    NonLocals = goal_info_get_nonlocals(GoalInfo0),

    InstGraph = !.GCInfo ^ inst_graph,
    NonLocalReachableList = solutions.solutions(inst_graph.reachable_from_list(
        InstGraph, set_of_var.to_sorted_list(NonLocals))),
    set_of_var.sorted_list_to_set(NonLocalReachableList, NonLocalReachable),
    LocalVars = Vars `set_of_var.difference` NonLocalReachable,

    ( if using_simple_mode_constraints(!.GCInfo ^ g_mc_info) then
        % With simple mode constraints, it is more efficient to do this
        % constraint before doing the goal constraints.
        constrain_local_vars(LocalVars, GoalId, !Constraint, !GCInfo),
        goal_constraints_2(GoalId, NonLocals, Vars, CanSucceed, GoalExpr0,
            GoalExpr, !Constraint, !GCInfo)
    else
        goal_constraints_2(GoalId, NonLocals, Vars, CanSucceed, GoalExpr0,
            GoalExpr, !Constraint, !GCInfo),
        % Without simple mode constraints, it is more efficient to do this
        % constraint after doing the goal constraints.
        constrain_local_vars(LocalVars, GoalId, !Constraint, !GCInfo)
    ),

    % DEBUGGING CODE
    % ModuleInfo = !GCInfo ^ module_info,
    % ProgVarset = !GCInfo ^ prog_varset,
    % functor(GoalExpr, Functor, _),
    % unsafe_perform_io(io.format("\nFunctor: %s\n", [s(Functor)])),
    % unsafe_perform_io(dump_constraints(ModuleInfo, ProgVarset,
    %   !.Constraint)),

    % DMO document
    % constrict_to_vars(set.to_sorted_list(NonLocals), Vars,
    %   GoalId, !Constraint, !GCInfo)

    % DEBUGGING CODE
    % size(Constraint1, NumNodes1, Depth1),
    % unsafe_perform_io(io.format(
    %   "Pre restrict Size: %d, Depth: %d\n",
    %   [i(NumNodes1), i(Depth1)])),
    % unsafe_perform_io(io.flush_output),

    !:Constraint = restrict_threshold(Threshold, !.Constraint),

    % DEBUGGING CODE
    % size(Constraint2, NumNodes2, Depth2),
    % unsafe_perform_io(io.format(
    %   "Post restrict Size: %d, Depth: %d\n",
    %   [i(NumNodes2), i(Depth2)])),
    % unsafe_perform_io(io.flush_output),

    constrain_non_occurring_vars(CanSucceed, ParentNonLocals, Vars,
        GoalId, !Constraint, !GCInfo),

    % DEBUGGING CODE
    % unsafe_perform_io(dump_constraints(ModuleInfo, ProgVarset,
    %   !.Constraint)),
    % goal_info_set_mode_constraint(GoalInfo0, !.Constraint, GoalInfo).

    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred goal_constraints_2(goal_id::in, set_of_progvar::in,
    set_of_progvar::in, can_succeed::out, hlds_goal_expr::in,
    hlds_goal_expr::out, mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

goal_constraints_2(GoalId, NonLocals, Vars, CanSucceed, GoalExpr0, GoalExpr,
        !Constraint, !GCInfo) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            multi_map.init(Usage0),

            Usage = list.foldl(func(G, U0) =
                list.foldl((func(V, U1) = U :-
                    multi_map.set(V, get_goal_id(G), U1, U)),
                    set_of_var.to_sorted_list(vars(G)), U0),
                Goals0, Usage0),

            known_vars(ensure_normalised(!.Constraint), KnownTrue, KnownFalse),

            % Generate conj constraints for known vars first since these
            % should be more efficient and provide lots of useful information
            % for the subgoal constraints.
            conj_constraints(bool.yes, KnownTrue, KnownFalse, GoalId, Usage,
                !Constraint, !GCInfo),

            conj_subgoal_constraints(NonLocals, CanSucceed, !Constraint,
                Goals0, Goals, !GCInfo),

            % Generate the rest of the constraints.
            conj_constraints(bool.no, KnownTrue, KnownFalse, GoalId, Usage,
                !Constraint, !GCInfo)
        ;
            ConjType = parallel_conj,
            sorry($pred, "par_conj NYI")
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        disj_constraints(NonLocals, CanSucceed, !Constraint, Goals0, Goals,
            [], DisjunctPaths, !GCInfo),
        list.foldl2(
            ( pred(V::in, Cons0::in, Cons::out, in, out) is det -->
                get_var(V `at` GoalId, Vgp),
                list.foldl2(
                    ( pred(I::in, C0::in, C::out, in, out) is det -->
                        get_var(V `at` I, VI),
                        { C = C0 ^ eq_vars(Vgp, VI) }
                    ), DisjunctPaths, Cons0, Cons)
            ), set_of_var.to_sorted_list(Vars), !Constraint, !GCInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = unify(Var, RHS0, _, _, _),
        unify_constraints(Var, GoalId, RHS0, RHS, !Constraint, !GCInfo),
        GoalExpr = GoalExpr0 ^ unify_rhs := RHS,
        CanSucceed = yes    % XXX Can we be more precise here?
    ;
        GoalExpr0 = plain_call(PredId, _, Args, _, _, _),
        SCC = !.GCInfo ^ scc,
        InstGraph = !.GCInfo ^ inst_graph,
        ModuleInfo = !.GCInfo ^ module_info,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),

        CanSucceed = ( pred_can_succeed(PredInfo) -> bool.yes ; bool.no ),

        ( if list.member(PredId, SCC) then
            % This is a recursive call.
            % XXX we currently assume that all recursive calls are to the
            % same mode of the predicate.
            pred_info_get_clauses_info(PredInfo, ClausesInfo),
            clauses_info_get_headvar_list(ClausesInfo, HeadVars),
            call_constraints(GoalId, PredId, HeadVars, Args,
                !Constraint, !GCInfo)
        else
            % This is a non-recursive call.
            ( if pred_has_mode_decl(ModuleInfo, PredId) then
                % The predicate has mode declarations so use them
                % to obtain the constraints for the call.

                pred_info_get_proc_table(PredInfo, ProcTable),
                map.values(ProcTable, ProcInfos),
                update_md_info(
                    ( pred(C::out, S0::in, S::out) is det :-
                        list.foldl2(
                            process_mode_decl_for_proc(ModuleInfo,
                                InstGraph, Args, ignore, call_in(GoalId),
                                bool.no, false_var(var_at_goal_id(GoalId)),
                                call_out(GoalId), yes),
                            ProcInfos, zero, C, S0, S)
                    ), CallConstraint, !GCInfo)
            else
                % The called predicate is from a lower (i.e. already
                % mode-analysed) SCC, but does not have any mode declarations.
                pred_info_get_arg_modes_maps(PredInfo, ArgModes),
                pred_info_get_inst_graph_info(PredInfo, InstGraphInfo),
                PredInstGraph = InstGraphInfo ^ interface_inst_graph,
                pred_info_get_clauses_info(PredInfo, PredClausesInfo),
                clauses_info_get_headvar_list(PredClausesInfo, PredHeadVars),
                solutions(
                    ( pred((V - W)::out) is nondet :-
                        inst_graph.corresponding_nodes_from_lists(
                            PredInstGraph, InstGraph, PredHeadVars, Args, V, W)
                    ), CorrespondingNodes),
                list.foldl2(
                    ( pred(ArgMap::in, Cn0::in, Cn::out,
                            S0::in, S::out) is det :-
                        ArgMap = InArgs - OutArgs,
                        list.foldl2(
                            ( pred((V - W)::in, C0::in, C::out,
                                    T0::in, T::out) is det :-
                                get_var(W `at` GoalId, Wgp, T0, T1),
                                get_var(out(W), Wout, T1, T),
                                ( if map.lookup(InArgs, V, yes) then
                                    C = C0 ^ var(Wout) ^ not_var(Wgp)
                                else if map.lookup(OutArgs, V, yes) then
                                    C = C0 ^ var(Wgp)
                                else
                                    C = C0 ^ not_var(Wgp)
                                )
                            ), CorrespondingNodes, one, Cn1, S0, S),
                        Cn = Cn0 + Cn1
                    ), ArgModes, zero, CallConstraint, !GCInfo)
                % XXX ArgModes is [] for `odd' - why?
            ),
            !:Constraint = !.Constraint * CallConstraint
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(GenericCall, Args, _Modes, _MaybeArgRegs,
            _Det),
        % Note: `_Modes' is invalid for higher-order calls at this point.
        (
            GenericCall = higher_order(Var, _, _, _),
            generic_call_constrain_var(Var, GoalId, !Constraint, !GCInfo),

            % Record that the argument vars need to be constrained
            % once we know the higher order mode of the Var we are calling.
            HoCalls0 = !.GCInfo ^ ho_calls,
            get_prog_var_level(!.GCInfo ^ mc_info, Var, VarLevel),
            multi_map.set(VarLevel, GoalId - Args, HoCalls0, HoCalls),
            !GCInfo ^ ho_calls := HoCalls,

            CanSucceed = yes % XXX should check this
        ;
            GenericCall = class_method(Var, _, _, _),
            generic_call_constrain_var(Var, GoalId, !Constraint, !GCInfo),
            unexpected($pred, "class_method call in clause")
        ;
            GenericCall = event_call(_),
            sorry($pred, "event_call NYI")
        ;
            GenericCall = cast(_),
            sorry($pred, "type/inst cast or coerce NYI")
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = switch(_, _, _),
        unexpected($pred, "switch (should be disj)")
    ;
        GoalExpr0 = negation(SubGoal0),
        goal_constraints(NonLocals, _, SubGoal0, SubGoal,
            !Constraint, !GCInfo),

        CanSucceed = yes,

        list.foldl2(
            ( pred(V::in, C0::in, C::out, in, out) is det -->
                get_var(V `at` GoalId, Vgp),
                get_var(V `at` get_goal_id(SubGoal), Vneg),
                { C = C0 ^ eq_vars(Vgp, Vneg) }
            ), set_of_var.to_sorted_list(Vars), !Constraint, !GCInfo),

        % Make sure the negation doesn't bind any nonlocal variables.
        negation_constraints(GoalId, NonLocals, !Constraint, !GCInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        goal_constraints(NonLocals, CanSucceed, SubGoal0, SubGoal, !Constraint,
            !GCInfo),

        list.foldl2(
            ( pred(V::in, C0::in, C::out, in, out) is det -->
                get_var(V `at` GoalId, Vgp),
                get_var(V `at` get_goal_id(SubGoal), Vexist),
                { C = C0 ^ eq_vars(Vgp, Vexist) }
            ), set_of_var.to_sorted_list(Vars), !Constraint, !GCInfo),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(IteNonLocals, Cond0, Then0, Else0),

        % Make sure that the condition doesn't bind any variables that are
        % non-local to the if-then-else.
        negation_constraints(get_goal_id(Cond0), NonLocals, !Constraint,
            !GCInfo),

        goal_constraints(NonLocals, CanSucceedC, Cond0, Cond, !Constraint,
            !GCInfo),
        goal_constraints(NonLocals, CanSucceedT, Then0, Then, !Constraint,
            !GCInfo),
        goal_constraints(NonLocals, CanSucceedE, Else0, Else, !Constraint,
            !GCInfo),

        bool.and(CanSucceedC, CanSucceedT, CanSucceedCT),
        bool.or(CanSucceedCT, CanSucceedE, CanSucceed),

        InstGraph = !.GCInfo ^ inst_graph,
        NonLocalReachable = solutions.solutions(inst_graph.reachable_from_list(
            InstGraph, set_of_var.to_sorted_list(NonLocals))),

        % Make sure variables have the same bindings in both the then and else
        % branches.
        list.foldl2(
            ( pred(V::in, C0::in, C::out, S0::in, S::out) is det :-
                get_var(V `at` GoalId, Vgp, S0, S1),
                get_var(V `at` get_goal_id(Then0), Vthen, S1, S2),
                get_var(V `at` get_goal_id(Else0), Velse, S2, S),
                C = C0 ^ eq_vars(Vgp, Vthen) ^ eq_vars(Vgp, Velse)
            ), NonLocalReachable, !Constraint, !GCInfo),

        % Make sure variables are bound in at most one of the cond and then
        % goals.
        list.foldl2(
            ( pred(V::in, C0::in, C::out, S0::in, S::out) is det :-
                get_var(V `at` get_goal_id(Cond0), Vcond, S0, S1),
                get_var(V `at` get_goal_id(Then0), Vthen, S1, S),
                C = C0 ^ not_both(Vcond, Vthen)
            ), set_of_var.to_sorted_list(
                vars(Cond0) `set_of_var.union` vars(Then0)),
            !Constraint, !GCInfo),

        % Local variables bound in cond, then or else should be treated as
        % though they are bound in the ite as well. (Although all such
        % variables will be local to the ite, the _out constraints still
        % need to be satisfied.)
        set_of_var.sorted_list_to_set(NonLocalReachable, NonLocalReachableSet),
        set_of_var.difference(Vars, NonLocalReachableSet,
            NotNonLocalReachableVars),
        set_of_var.to_sorted_list(NotNonLocalReachableVars, Locals),
        list.foldl2(
            ( pred(V::in, C0::in, C::out, S0::in, S::out) is det :-
                get_var(V `at` get_goal_id(Cond), Vcond, S0, S1),
                get_var(V `at` get_goal_id(Then), Vthen, S1, S2),
                get_var(V `at` get_goal_id(Else), Velse, S2, S3),
                get_var(V `at` GoalId, Vgp, S3, S),
                sparse_bitset.list_to_set([Vcond, Vthen, Velse], Vs),
                C = C0 ^ disj_vars_eq(Vs, Vgp)
            ), Locals, !Constraint, !GCInfo),

        GoalExpr = if_then_else(IteNonLocals, Cond, Then, Else)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        sorry($pred, "foreign_proc NYI")
    ;
        GoalExpr0 = shorthand(_),
        sorry($pred, "shorthand")
    ).

% goal_constraints_2(GoalId, NonLocals, Vars, CanSucceed,
%         AtomicGoal0, AtomicGoal, !Constraint, !GCInfo) :-
%     AtomicGoal0 = atomic_goal(GoalType, Outer, Inner, OutVars,
%         MainGoal0, OrElseGoals0),
%     Goals0 = [MainGoal0 | OrElseGoals0],
%     disj_constraints(NonLocals, CanSucceed, !Constraint, Goals0, Goals,
%         [], DisjunctPaths, !GCInfo),
%     list.foldl2((pred(V::in, Cons0::in, Cons::out, in, out) is det -->
%         get_var(V `at` GoalId, Vgp),
%         list.foldl2((pred(Path::in, C0::in, C::out, in, out) is det -->
%             get_var(V `at` Path, VPath),
%             { C = C0 ^ eq_vars(Vgp, VPath) }
%         ), DisjunctPaths, Cons0, Cons)
%     ), set.to_sorted_list(Vars), !Constraint, !GCInfo),
%     MainGoal = list.det_head(Goals),
%     OrElseGoals = list.det_tail(Goals),
%     AtomicGoal = atomic_goal(GoalType, Outer, Inner, OutVars,
%         MainGoal, OrElseGoals).

    % Constraints for the conjunction. If UseKnownVars = yes, generate
    % constraints only for the vars in KnownVars, otherwise generate
    % constraints only for the vars _not_ is KnownVars.
    %
:- pred conj_constraints(bool::in, mode_constraint_vars::in,
    mode_constraint_vars::in, goal_id::in, multi_map(prog_var, goal_id)::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

conj_constraints(UseKnownVars, KnownTrue, KnownFalse, GoalId, UsageMap,
        !Constraint, !GCInfo) :-
    UsageList = map.to_assoc_list(UsageMap), % XXX needed for deep profiler
    list.foldl2(
        conj_constraints_process_var(UseKnownVars, KnownTrue, KnownFalse,
            GoalId),
        UsageList, !Constraint, !GCInfo).

:- pred conj_constraints_process_var(bool::in, mode_constraint_vars::in,
    mode_constraint_vars::in, goal_id::in, pair(prog_var, list(goal_id))::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

conj_constraints_process_var(UseKnownVars, KnownTrue, KnownFalse, GoalId,
        Var - Ids, !Constraint, !GCInfo) :-
    list.map_foldl(
        ( pred(I::in, CV::out, in, out) is det -->
            mode_constraints.get_var(Var `at` I, CV)
        ), Ids, ConstraintVars, !GCInfo),
    mode_constraints.get_var(Var `at` GoalId, VConj, !GCInfo),
    ConstraintVarSet = sparse_bitset.list_to_set(ConstraintVars),

    % If UseKnownVars = yes we want to only generate the constraints
    % which are 2-sat. If UseKnownVars = no, we generate the other
    % constraints.
    ( if sparse_bitset.contains(KnownFalse, VConj) then
        (
            UseKnownVars = bool.yes,
            !:Constraint = !.Constraint ^ conj_not_vars(ConstraintVarSet)
        ;
            UseKnownVars = bool.no
        )
    else if sparse_bitset.contains(KnownTrue, VConj) then
        (
            ConstraintVars = [],
            !:Constraint = zero
        ;
            ConstraintVars = [ConstraintVar],
            (
                UseKnownVars = bool.yes,
                !:Constraint = !.Constraint ^ var(ConstraintVar)
            ;
                UseKnownVars = bool.no
            )
        ;
            ConstraintVars = [ConstraintVar1, ConstraintVar2],
            (
                UseKnownVars = bool.yes,
                !:Constraint = !.Constraint
                    ^ neq_vars(ConstraintVar1, ConstraintVar2)
            ;
                UseKnownVars = bool.no
            )
        ;
            ConstraintVars = [_, _, _ | _],
            (
                UseKnownVars = bool.yes
            ;
                UseKnownVars = bool.no,
                !:Constraint = !.Constraint
                    ^ at_most_one_of(ConstraintVarSet)
                    ^ disj_vars_eq(ConstraintVarSet, VConj)
            )
        )
    else
        (
            UseKnownVars = bool.yes
        ;
            UseKnownVars = bool.no,
            !:Constraint = !.Constraint
                ^ at_most_one_of(ConstraintVarSet)
                ^ disj_vars_eq(ConstraintVarSet, VConj)
        )
    ).

:- pred conj_subgoal_constraints(set_of_progvar::in, can_succeed::out,
    mode_constraint::in, mode_constraint::out,
    hlds_goals::in, hlds_goals::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

conj_subgoal_constraints(_, yes, !Constraint, [], [], !GCInfo).
conj_subgoal_constraints(NonLocals, CanSucceed, !Constraint,
        [Goal0 | Goals0], [Goal | Goals], !GCInfo) :-
    goal_constraints(NonLocals, CanSucceed0, Goal0, Goal, !Constraint,
        !GCInfo),
    conj_subgoal_constraints(NonLocals, CanSucceed1, !Constraint,
        Goals0, Goals, !GCInfo),
    bool.and(CanSucceed0, CanSucceed1, CanSucceed).

:- pred disj_constraints(set_of_progvar::in, can_succeed::out,
    mode_constraint::in, mode_constraint::out,
    hlds_goals::in, hlds_goals::out,
    list(goal_id)::in, list(goal_id)::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

disj_constraints(_, no, !Constraint, [], [], Ids, Ids, !GCInfo).
disj_constraints(NonLocals, CanSucceed, !Constraint,
        [Goal0 | Goals0], [Goal | Goals], Ids0, Ids, !GCInfo) :-
    goal_constraints(NonLocals, CanSucceed0, Goal0, Goal,
        !Constraint, !GCInfo),
    disj_constraints(NonLocals, CanSucceed1, !Constraint, Goals0, Goals,
        [get_goal_id(Goal) | Ids0], Ids, !GCInfo),
    bool.or(CanSucceed0, CanSucceed1, CanSucceed).

    % See 1.2.3 The literals themselves
    %
:- pred unify_constraints(prog_var::in, goal_id::in, unify_rhs::in,
    unify_rhs::out, mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

unify_constraints(LHSVar, GoalId, RHS0, RHS, !Constraint, !GCInfo) :-
    (
        RHS0 = rhs_var(RHSVar),
        InstGraph = !.GCInfo ^ inst_graph,
        Generator =
            ( pred((V - W)::out) is multi :-
                inst_graph.same_graph_corresponding_nodes(InstGraph,
                    LHSVar, RHSVar, V, W)
            ),
        Accumulator =
            ( pred((V - W)::in, C0::in, C::out, S0::in, S::out) is det :-
                mode_constraints.get_var(out(V), Vout, S0, S1),
                mode_constraints.get_var(out(W), Wout, S1, S2),
                mode_constraints.get_var(V `at` GoalId, Vgi, S2, S3),
                mode_constraints.get_var(W `at` GoalId, Wgi, S3, S),
                C = C0 ^ eq_vars(Vout, Wout) ^ not_both(Vgi, Wgi)
            ),
        solutions.aggregate2(Generator, Accumulator, !Constraint, !GCInfo),
        get_var(out(LHSVar), LHSVarOut, !GCInfo),
        !:Constraint = !.Constraint ^ var(LHSVarOut),

        HoModes0 = !.GCInfo ^ ho_modes,
        update_mc_info_t(share_ho_modes(LHSVar, RHSVar, HoModes0), HoModes,
            !GCInfo),
        !GCInfo ^ ho_modes := HoModes,
        RHS = RHS0
    ;
        RHS0 = rhs_functor(_ConsId, _IsExistConstruct, Args),
        get_var(out(LHSVar), LHSVarOut, !GCInfo),
        !:Constraint = !.Constraint ^ var(LHSVarOut),
        ( if using_simple_mode_constraints(!.GCInfo ^ g_mc_info) then
            % In the simple system a var-functor unification must be either
            % a construction or a deconstruction.
            list.map_foldl(
                ( pred(ProgVar::in, RepVar::out, S0::in, S::out) is det :-
                    mode_constraints.get_var(ProgVar `at` GoalId, RepVar,
                        S0, S)
                ), Args, ArgsGi0, !GCInfo),
            set_of_var.list_to_set(ArgsGi0, ArgsGi),
            get_var(LHSVar `at` GoalId, LHSVargi, !GCInfo),
            ( if set_of_var.remove_least(Arg1gi, ArgsGi, ArgsGi1) then
                !:Constraint = neq_vars(Arg1gi, LHSVargi, !.Constraint),
                set_of_var.fold_func(eq_vars(Arg1gi), ArgsGi1, !Constraint)
            else
                !:Constraint = !.Constraint
            )
            % Constraint = Constraint1 *
            %   ( one ^ var(Agp) ^ conj_not_vars(ArgsGp)
            %   + one ^ not_var(Agp) ^ conj_vars(ArgsGp)
            %   )
        else
            InstGraph = !.GCInfo ^ inst_graph,
            inst_graph.foldl_reachable_from_list2(
                ( pred(V::in, C0::in, C::out, S0::in, S::out) is det :-
                    ( if V = LHSVar then
                        C = C0,
                        S = S0
                    else
                        mode_constraints.get_var(V `at` GoalId, Vgp, S0, S),
                        C = C0 ^ not_var(Vgp)
                    )
                ), InstGraph, Args, !Constraint, !GCInfo)
        ),
        RHS = RHS0
    ;
        RHS0 = rhs_lambda_goal(_, _, _, _, NonLocals, ArgVarsModes, _, Goal0),
        assoc_list.keys_and_values(ArgVarsModes, ArgVars, Modes),
        InstGraph = !.GCInfo ^ inst_graph,

        % Variable Var is made ground by this goal.
        inst_graph.foldl_reachable2(
            ( pred(V::in, Cn0::in, Cn::out, in, out) is det -->
                mode_constraints.get_var(V `at` GoalId, Vgp),
                { Cn = Cn0 ^ var(Vgp) }
            ), InstGraph, LHSVar, !Constraint, !GCInfo),

        % The lambda NonLocals are not bound by this goal.
        inst_graph.foldl_reachable_from_list2(
            ( pred(V::in, Cn0::in, Cn::out, in, out) is det -->
                mode_constraints.get_var(V `at` GoalId, Vgp),
                { Cn = Cn0 ^ not_var(Vgp) }
            ), InstGraph, NonLocals, !Constraint, !GCInfo),

        % Record the higher-order mode of this lambda goal.
        HoModes0 = !.GCInfo ^ ho_modes,
        get_prog_var_level(!.GCInfo ^ mc_info, LHSVar, LHSVarLevel),
        multi_map.set(LHSVarLevel, Modes, HoModes0, HoModes),
        !GCInfo ^ ho_modes := HoModes,

        % Analyse the lambda goal.
        update_mc_info(enter_lambda_goal(GoalId), !GCInfo),

        % XXX Rather than adding `in' modes for lambda nonlocals we should just
        % place a constraint `V_prod = 0' for all nodes reachable from these
        % variables in the lambda goal.
        ArgModes = list.duplicate(length(NonLocals), in_mode) ++ Modes,
        LambdaHeadVars = NonLocals ++ ArgVars,
        ModuleInfo = !.GCInfo ^ module_info,
        update_md_info(process_mode_decl(ModuleInfo,
            InstGraph, LambdaHeadVars, false_var(initial),
            true_var(initial), bool.yes, false_var(final), true_var(final),
            bool.no, ArgModes, zero), DeclConstraint, !GCInfo),
        !:Constraint = !.Constraint * DeclConstraint,

        % XXX This will put constraints on variables that do not occur in
        % the lambda goal. These constraints will be removed at the next
        % restrict, but it would be more efficient not to put them in the
        % first place.

        % DEBUGGING CODE
        % size(!.Constraint, NumNodes3, Depth3, _),
        % unsafe_perform_io(io.format(
        %   "Pre lambda Size: %d, Depth: %d\n",
        %   [i(NumNodes3), i(Depth3)])),

        update_mc_info_t(
            ( pred(C::out, S0::in, S::out) is det :-
                map.foldl2(input_output_constraints(LambdaHeadVars, InstGraph),
                    InstGraph, !.Constraint, C, S0, S)
            ), !:Constraint, !GCInfo),

        % DEBUGGING CODE
        % size(!.Constraint, NumNodes5, Depth5, _),
        % unsafe_perform_io(io.format(
        %   "lambda io_constraints Size: %d, Depth: %d\n",
        %   [i(NumNodes5), i(Depth5)])),

        goal_constraints(set_of_var.init, _CanSucceed, Goal0, Goal,
            !Constraint, !GCInfo),

        % DEBUGGING CODE
        % size(Constraint, NumNodes, Depth),
        % unsafe_perform_io(io.format(
        %   "post lambda Size: %d, Depth: %d\n",
        %   [i(NumNodes), i(Depth)])),

        update_mc_info(leave_lambda_goal, !GCInfo),
        RHS = RHS0 ^ rhs_lambda_goal := Goal
    ).

:- pred call_constraints(goal_id::in, pred_id::in,
    list(prog_var)::in, list(prog_var)::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

call_constraints(GoalId, PredId, HeadVars, Args, !Constraint, !GCInfo) :-
    InstGraph = !.GCInfo ^ inst_graph,
    Generator =
        ( pred((V - W)::out) is nondet :-
            corresponding_members(HeadVars, Args, X, Y),
            inst_graph.same_graph_corresponding_nodes(InstGraph, X, Y, V, W)
        ),
    Accumulator =
        ( pred((V - W)::in, C0::in, C::out, S0::in, S::out) is det :-
            get_var_in_pred(PredId, V `at` whole_body_goal_id, V_, S0, S1),
            get_var(W `at` GoalId, Wgi, S1, S2),
            get_var_in_pred(PredId, in(V), Vin, S2, S3),
            get_var(out(W), Wout, S3, S),
            C = C0 ^ eq_vars(V_, Wgi) ^ imp_vars(Vin, Wout)
        ),
    solutions.aggregate2(Generator, Accumulator, !Constraint, !GCInfo).

:- pred higher_order_call_constraints(
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

higher_order_call_constraints(Constraint0, Constraint, !GCInfo) :-
    HoModes = !.GCInfo ^ ho_modes,
    HoCalls = !.GCInfo ^ ho_calls,
    ModuleInfo = !.GCInfo ^ module_info,
    InstGraph = !.GCInfo ^ inst_graph,
    update_md_info(
        ( pred(Constraint1::out, in, out) is det -->
            map.foldl2(
                ( pred(HoVarLevel::in, Calls::in, Cons0::in, Cons::out,
                        in, out) is det -->
                    update_mc_info(set_level_from_var(HoVarLevel)),
                    ( if { map.search(HoModes, HoVarLevel, ArgModesList) } then
                        list.foldl2(
                            ( pred((GoalId - Args)::in, C0::in, C::out,
                                    in, out) is det -->
                                list.foldl2(
                                    process_mode_decl(ModuleInfo, InstGraph,
                                        Args, ignore, call_in(GoalId), no,
                                        false_var(var_at_goal_id(GoalId)),
                                        call_out(GoalId), no),
                                    ArgModesList, zero, C1),
                                { C = C0 * C1 }
                            ), Calls, Cons0, Cons)
                    else
                        { Cons = Cons0 }
                    )
                ), HoCalls, Constraint0, Constraint1)
        ), Constraint, !GCInfo).

:- pred negation_constraints(goal_id::in, set_of_progvar::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

negation_constraints(GoalId, NonLocals, !Constraint, !GCInfo) :-
    InstGraph = !.GCInfo ^ inst_graph,
    inst_graph.foldl_reachable_from_list2(
        ( pred(V::in, C0::in, C::out, in, out) is det -->
            get_var(V `at` GoalId, Vgp),
            { C = C0 ^ not_var(Vgp) }
        ), InstGraph, set_of_var.to_sorted_list(NonLocals),
        !Constraint, !GCInfo).

:- pred generic_call_constrain_var(prog_var::in, goal_id::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

generic_call_constrain_var(Var, GoalId, !Constraint, !GCInfo) :-
    InstGraph = !.GCInfo ^ inst_graph,
    inst_graph.foldl_reachable2(
        ( pred(V::in, C0::in, C::out, in, out) is det -->
            get_var(out(V), Vout),
            get_var(V `at` GoalId, Vgp),
            { C = C0 ^ var(Vout) ^ not_var(Vgp) }
        ), InstGraph, Var, !Constraint, !GCInfo).

:- pred constrict_to_vars(list(prog_var)::in, set_of_progvar::in,
    goal_id::in, mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.
:- pragma consider_used(pred(constrict_to_vars/7)).

constrict_to_vars(NonLocals, GoalVars, GoalId, !Constraint, !GCInfo) :-
    get_forward_goal_path_map(!.GCInfo ^ mc_info, ForwardGoalPathMap),
    !:Constraint = restrict_filter(
        keep_var(ForwardGoalPathMap, NonLocals, GoalVars, GoalId,
            !.GCInfo ^ atomic_goals, !.GCInfo ^ inst_graph),
        !.GCInfo ^ mc_info, !.Constraint).

:- pred keep_var(goal_forward_path_map::in, list(prog_var)::in,
    set_of_progvar::in, goal_id::in, set(goal_id)::in, inst_graph::in,
    rep_var::in) is semidet.

keep_var(_ForwardGoalPathMap, NonLocals, GoalVars, _GoalId, AtomicGoals,
        InstGraph, RepVar) :-
    (
        RepVar = _V `at` RepGoalId,
        set.member(RepGoalId, AtomicGoals)
    ;
        (
            ( RepVar = in(V)
            ; RepVar = out(V)
            ; RepVar = V `at` _
            ),
            set_of_var.member(GoalVars, V)
        )
        =>
        (
            list.member(NonLocal, NonLocals),
            inst_graph.reachable(InstGraph, NonLocal, V)
            % The call to list.remove_suffix is equivalent to:
            % list.append([_ | _], GoalPathSteps, RepGoalPathSteps)
            % I (zs) do not see how that can possibly make sense,
            % which is why I have disabled this test.
%           not (
%               RepVar = _ `at` RepGoalId,
%               % XXX What higher level operation is being implemented here?
%               map.lookup(ForwardGoalPathMap, GoalId, GoalPath),
%               map.lookup(ForwardGoalPathMap, RepGoalId, RepGoalPath),
%               GoalPath = fgp(GoalPathSteps),
%               RepGoalPath = fgp(RepGoalPathSteps),
%               list.remove_suffix(RepGoalPathSteps, GoalPathSteps, [_ | _])
%           )
        )
    ).

:- type sccs == list(list(pred_id)).

    % Obtain the SCCs for the module.
    %
:- pred get_predicate_sccs(module_info::in, sccs::out) is det.

get_predicate_sccs(ModuleInfo, SCCs) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    DepInfo = build_pred_dependency_graph(ModuleInfo, PredIds,
        do_not_include_imported),
    SCCs0 = dependency_info_get_bottom_up_sccs(DepInfo),

    % Remove predicates that have mode declarations and place them in
    % their own ``SCC'' at the end of the list.
    % Predicates with mode declarations do not need to be processed with
    % the rest of their SCC since the mode declaration can be used in any
    % calls to them.  Such predicates should be processed last to take
    % advantage of mode info inferred from other predicates.
    extract_mode_decl_preds(ModuleInfo, SCCs0, [], SCCs1),

    % We add imported preds to the end of the SCC list, one SCC per pred.
    % This allows a constraint to be created for each imported pred
    % based on its mode declarations.
    add_imported_preds(ModuleInfo, SCCs1, SCCs).

:- pred extract_mode_decl_preds(module_info::in, list(set(pred_id))::in,
    sccs::in, sccs::out) is det.

extract_mode_decl_preds(_ModuleInfo, [], !DeclaredPreds).
extract_mode_decl_preds(ModuleInfo, [SCC0 | SCCs0], !DeclaredPreds) :-
    list.filter(pred_has_mode_decl(ModuleInfo), set.to_sorted_list(SCC0),
        Declared, SCC),
    (
        Declared = []
    ;
        Declared = [_ | _],
        list.foldl(
            ( pred(Pred::in, Preds0::in, Preds::out) is det :-
                Preds = [[Pred] | Preds0]
            ), Declared, !DeclaredPreds)
    ),
    extract_mode_decl_preds(ModuleInfo, SCCs0, !DeclaredPreds),
    (
        SCC = []
    ;
        SCC = [_ | _],
        !:DeclaredPreds = [SCC | !.DeclaredPreds]
    ).

:- pred pred_has_mode_decl(module_info::in, pred_id::in) is semidet.

pred_has_mode_decl(ModuleInfo, PredId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    not pred_info_infer_modes(PredInfo).

:- pred add_imported_preds(module_info::in, sccs::in, sccs::out) is det.

add_imported_preds(ModuleInfo, SCCs0, SCCs) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.filter_map(
        ( pred(PredId::in, [PredId]::out) is semidet :-
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_is_imported(PredInfo)
        ), PredIds, ImportedPredIds),
    SCCs = SCCs0 ++ ImportedPredIds.

:- pred cons_id_in_bound_insts(cons_id::in, list(bound_inst)::in,
        list(mer_inst)::out) is semidet.

cons_id_in_bound_insts(ConsId, [bound_functor(ConsId0, Insts0) | BIs],
        Insts) :-
    ( if equivalent_cons_ids(ConsId0, ConsId) then
        Insts = Insts0
    else
        cons_id_in_bound_insts(ConsId, BIs, Insts)
    ).

%------------------------------------------------------------------------%

% For local variables, V_ must be equivalent to Vgp.

:- pred constrain_local_vars(set_of_progvar::in, goal_id::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

constrain_local_vars(Locals, GoalId, !Constraint, !GCInfo) :-
    list.foldl2(do_constrain_local_vars(GoalId),
        set_of_var.to_sorted_list(Locals), !Constraint, !GCInfo).

:- pred do_constrain_local_vars(goal_id::in, prog_var::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

do_constrain_local_vars(GoalId, Var, !Constraint, !GCInfo) :-
    get_var(Var `at` GoalId, Vgp, !GCInfo),
    get_var(out(Var), Vout, !GCInfo),
    ( if using_simple_mode_constraints(!.GCInfo ^ g_mc_info) then
        % For simple_mode_constraints, local variables must all be bound
        % within the goal.
        !:Constraint = !.Constraint ^ var(Vgp) ^ var(Vout)
    else
        !:Constraint = !.Constraint ^ eq_vars(Vgp, Vout)
    ).

:- pred constrain_non_occurring_vars(can_succeed::in, set_of_progvar::in,
    set_of_progvar::in, goal_id::in,
    mode_constraint::in, mode_constraint::out,
    goal_constraints_info::in, goal_constraints_info::out) is det.

constrain_non_occurring_vars(no, _, _, _, !Constraint, !GCInfo).
constrain_non_occurring_vars(yes, ParentNonLocals, OccurringVars, GoalId,
        !Constraint, !GCInfo) :-
    InstGraph = !.GCInfo ^ inst_graph,
    Generator =
        ( pred(V::out) is nondet :-
            set_of_var.member(ParentNonLocals, U),
            inst_graph.reachable(InstGraph, U, V),
            not set_of_var.member(OccurringVars, V)
        ),
    Accumulator =
        ( pred(V::in, Vs0::in, Vs::out, in, out) is det -->
            get_var(V `at` GoalId, VGP),
            { Vs = Vs0 `insert` VGP }
        ),
    solutions.aggregate2(Generator, Accumulator, empty_vars_set,
        NonOccurringVars, !GCInfo),
    !:Constraint = !.Constraint ^ conj_not_vars(NonOccurringVars).

%   aggregate2((pred(V::out) is nondet :-
%       set.member(U, ParentNonLocals),
%       inst_graph.reachable(InstGraph, U, V),
%       not set.member(V, OccurringVars)
%       ), (pred(V::in, C0::in, C::out, in, out) is det -->
%           get_var(V `at` GoalId, VGP),
%           { C = C0 ^ not_var(VGP) }
%       ), Constraint0, Constraint).

%------------------------------------------------------------------------%

:- pred share_ho_modes(prog_var::in, prog_var::in, ho_modes::in, ho_modes::out,
    mode_constraint_info::in, mode_constraint_info::out) is det.

share_ho_modes(VarA, VarB, HoModes0, HoModes, !MCI) :-
    get_prog_var_level(!.MCI, VarA, A),
    get_prog_var_level(!.MCI, VarB, B),
    ( if map.search(HoModes0, A, AModes) then
        ( if map.search(HoModes0, B, BModes) then
            Modes = list.sort_and_remove_dups(AModes ++ BModes),
            map.det_update(A, Modes, HoModes0, HoModes1),
            map.det_update(B, Modes, HoModes1, HoModes)
        else
            map.det_insert(B, AModes, HoModes0, HoModes)
        )
    else if map.search(HoModes0, B, BModes) then
        map.det_insert(A, BModes, HoModes0, HoModes)
    else
        HoModes = HoModes0
    ).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- pred arg_modes_map(list(prog_var)::in, inst_graph::in, mode_constraint::in,
    mode_constraint_info::in, arg_modes_map::out) is nondet.

arg_modes_map(HeadVars, InstGraph, Constraint0, Info0, ArgModes) :-
    solutions.solutions(inst_graph.reachable_from_list(InstGraph, HeadVars),
        Vars),
    list.map_foldl(
        ( pred(PV::in, (MV - in(PV))::out, in, out) is det -->
            mode_constraint_var(in(PV), MV)
        ), Vars, InVars, Info0, Info1),
    list.map_foldl(
        ( pred(PV::in, (MV - out(PV))::out, in, out) is det -->
            mode_constraint_var(out(PV), MV)
        ), Vars, OutVars, Info0, Info1),
    MVars = list.sort_and_remove_dups(InVars ++ OutVars),
    MVarKeys = assoc_list.keys(MVars),
    Constraint = restrict_filter(
        ( pred(V::in) is semidet :-
            list.member(V, MVarKeys)
        ),
        ensure_normalised(Constraint0)),
    ArgModes0 = map.init - map.init,
    list.foldl2(arg_modes_map_2, MVars, Constraint, _, ArgModes0, ArgModes).

:- pred arg_modes_map_2(pair(mode_constraint_var, rep_var)::in,
    mode_constraint::in, mode_constraint::out,
    arg_modes_map::in, arg_modes_map::out) is nondet.

arg_modes_map_2(MV - RV, Constraint0, Constraint, ArgModes0, ArgModes) :-
    (
        Constraint = var_restrict_true(MV, Constraint0),
        Bool = yes
    ;
        Constraint = var_restrict_false(MV, Constraint0),
        Bool = no
    ),
    Constraint \= zero,
    ArgModes0 = InModes0 - OutModes0,
    (
        RV = in(PV),
        ArgModes = map.det_insert(InModes0, PV, Bool) - OutModes0
    ;
        RV = out(PV),
        ArgModes = InModes0 - map.det_insert(OutModes0, PV, Bool)
    ).

% :- type labelling == map(mode_constraint_var, bool).
%
% :- pred labelling(set(mode_constraint_var)::in, mode_constraint::in,
%       labelling::out) is nondet.
%
% labelling(Vs, Constraint, Map) :-
%   labelling(Vs, Constraint, TrueVars, FalseVars),
%   Map = true_false_sets_to_labelling_map(TrueVars, FalseVars).
%
%   % Return a ``fundamental mode'' (i.e. non-implied mode) for the given
%   % mode constraint. This is calculated by computing a minimal model for
%   % the initial insts of the head variables of the predicate.
% :- pred fundamental_mode(set(mode_constraint_var)::in, mode_constraint::in,
%       mode_constraint::out) is nondet.
%
% fundamental_mode(Vs, Constraint0, Constraint) :-
%   minimal_model(Vs, Constraint0, TrueVars, FalseVars),
%
%   % XXX There's probably a more efficient way to do this.
%   Constraint = Constraint0 * conj_vars(TrueVars) *
%           (~disj_vars(FalseVars)).
%
% :- func true_false_sets_to_labelling_map(set(mode_constraint_var),
%       set(mode_constraint_var)) = labelling.
%
% true_false_sets_to_labelling_map(TrueVars, FalseVars) =
%   list.foldl(func(V, M) = map.det_insert(M, V, no),
%       set.to_sorted_list(FalseVars),
%       list.foldl(func(V, M) = map.det_insert(M, V, yes),
%           set.to_sorted_list(TrueVars), map.init)).
%
%   % implied_mode(L0, L1) is true iff mode L0 is implied by mode L1.
% :- pred implied_mode(labelling::in, labelling::in) is semidet.
%
% implied_mode(L0, L1) :-
%   all [V] ( map.member(L1, V, yes) => map.lookup(L0, V, yes) ).
%
% :- pred split_constraint_into_modes(pred_id::in, list(prog_var)::in,
%   inst_graph::in, mode_constraint::in, list(labelling)::out,
%   mode_constraint_info::in, mode_constraint_info::out) is det.
%
% split_constraint_into_modes(PredId, HeadVars, InstGraph, ModeConstraint0,
%       Labellings) -->
%   { solutions(inst_graph.reachable_from_list(InstGraph, HeadVars),
%       ReachVars) },
%   list.map_foldl((pred(PV::in, MV::out, in, out) is det -->
%       mode_constraint_var(in(PV), MV)
%   ), ReachVars, InVars),
%
%   get_interesting_vars_for_pred(PredId, InterestingVars),
%   { solutions((pred(Labelling::out) is nondet :-
%       fundamental_mode(set.list_to_set(InVars), ModeConstraint0,
%           ModeConstraint1),
%       labelling(InterestingVars, ModeConstraint1, Labelling)
%   ), Labellings) }.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- func get_goal_id(hlds_goal) = goal_id.

get_goal_id(hlds_goal(_, GoalInfo)) =
    goal_info_get_goal_id(GoalInfo).

:- func vars(hlds_goal) = set_of_progvar.

vars(hlds_goal(_, GoalInfo)) = OccurringVars :-
    goal_info_get_occurring_vars(GoalInfo, OccurringVars).

%------------------------------------------------------------------------%

    % A predicate can succeed if at least one of its procedures
    % can succeed.
    %
:- pred pred_can_succeed(pred_info::in) is semidet.

pred_can_succeed(PredInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    some [ProcInfo] (
        map.member(ProcTable, _ProcId, ProcInfo),
        proc_can_succeed(ProcInfo)
    ).

    % A procedure can possibly succeed if it has no declared determinism or
    % it has a declared determinism that allows more than zero solutions.
    % (This is a conservative approximation since we can't use the results
    % of determinism inference -- it hasn't been run yet.)
    %
:- pred proc_can_succeed(proc_info::in) is semidet.

proc_can_succeed(ProcInfo) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    (
        MaybeDet = no
    ;
        MaybeDet = yes(Detism),
        determinism_components(Detism, _, SolnCount),
        SolnCount \= at_most_zero
    ).

%------------------------------------------------------------------------%

% DEBUGGING CODE
%
% :- impure pred conj_to_dot(mode_constraint::in, prog_varset::in,
%   mode_constraint_info::in, io::di, io::uo) is det.
%
% conj_to_dot(MC, VS, CI) -->
%   robdd_to_dot(MC, VS, CI, string.format("conj%d.dot", [i(conjnum)])).
%
% :- impure func conjnum = int.
%
% :- pragma foreign_code("C",
% "
% static MR_Integer conjnum = 0;
% ").
%
% :- pragma foreign_proc("C",
%   conjnum = (N::out),
%   [will_not_call_mercury],
% "
%   N = conjnum++;
% ").

%------------------------------------------------------------------------%
:- end_module check_hlds.mode_constraints.
%------------------------------------------------------------------------%
