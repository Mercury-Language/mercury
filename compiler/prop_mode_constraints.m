%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prop_mode_constraints.m.
% Main author: richardf.
%
% XXX This module essentially serves as interface between the old
% mode_constraints module (by dmo) and the new abstract_mode_constraints
% and build_mode_constraints modules (by richardf). Ultimately its contents
% should probably be moved into those modules respectively.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.prop_mode_constraints.
:- interface.

:- import_module check_hlds.abstract_mode_constraints.
:- import_module check_hlds.build_mode_constraints.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.

:- import_module io.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

    % Maps each predicate to information about its goal ids and its mode
    % constraint.
:- type pred_constraints_map ==
    map(pred_id, {containing_goal_map, mode_constraints}).

%-----------------------------------------------------------------------------%

    % prop_mode_constraints_in_scc(SCC, !ModuleInfo, !VarSet, !VarMap,
    %   !PredConstraintsMap):
    %
    % For each predicate in SCC:
    %
    % - Add producer/consumer constraints to PredConstraintsMap.
    % - Add goal path annotations to its clauses in ModuleInfo.
    % - Add any constraint variables it requires to VarSet and VarMap.
    %
:- pred prop_mode_constraints_in_scc(list(pred_id)::in,
    module_info::in, module_info::out, mc_var_info::in, mc_var_info::out,
    pred_constraints_map::in, pred_constraints_map::out) is det.

    % ensure_unique_arguments(PredId, !ModuleInfo):
    %
    % Creates variables and introduces unifications in predicate PredId where
    % appropriate to ensure that no program variable is used as an argument of
    % more than one predicate (including the head of the caller, PredId).
    %
:- pred ensure_unique_arguments(pred_id::in, module_info::in, module_info::out)
    is det.

    % Check whether a predicate has been imported according to the
    % status_is_imported pred in the hlds_pred module.
    %
:- pred module_info_pred_status_is_imported(module_info::in, pred_id::in)
    is semidet.

    % Write in human readable form to the current output stream
    % the information in the pred_constraints_map, indicating which
    % predicate each set of constraints applies to.
    %
:- pred pretty_print_pred_constraints_map(module_info::in, mc_varset::in,
    pred_constraints_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.make_goal.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

prop_mode_constraints_in_scc(SCC0, !ModuleInfo, !VarInfo, !Constraints) :-
    % Process only predicates from this module.
    list.filter(module_info_pred_status_is_imported(!.ModuleInfo),
        SCC0, _, SCC),

    % Prepare the solver variables for the home path of each predicate
    % of the SCC - needed for calls to predicates in the same SCC
    % that do not have mode declarations.
    add_mc_vars_for_scc_heads(!.ModuleInfo, SCC, !VarInfo),

    % Now go through the SCC and add the constraint
    % variables and then constraints predicate by predicate
    list.foldl3(prop_mode_constraints_in_pred, SCC,
        !ModuleInfo, !VarInfo, !Constraints).

    % prop_mode_constraints_in_pred(PredId, !ModuleInfo, !VarSet, !VarMap,
    %   !Constraints):
    %
    % Performs a number of tasks for predicate PredId:
    %   1) Fill out the goal_id information in the ModuleInfo structure.
    %   2) Add producer/consumer constraint variables for program
    %      variables corresponding to any location at which they are
    %      nonlocal to VarSet and VarMap. (Elsewhere is is clear as to
    %      whether they are produced or consumed.)
    %   3) Add mode declaration constraints to Constraints.
    %   4) Add goal constraints to Constraints.
    %
    % NOTE: This relies on the head variables for any predicate without mode
    % declarations that is called by this one (PredId) to have the constraint
    % variables corresponding to the empty goal path (i.e. the whole body
    % of the predicate) to already be VarMap (and therefore also in VarSet).
    %
:- pred prop_mode_constraints_in_pred(pred_id::in,
    module_info::in, module_info::out, mc_var_info::in, mc_var_info::out,
    pred_constraints_map::in, pred_constraints_map::out) is det.

prop_mode_constraints_in_pred(PredId, !ModuleInfo, !VarInfo, !Constraints) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    do_prop_mode_constraints_in_pred(!.ModuleInfo, PredId, PredInfo0, PredInfo,
        !VarInfo, !Constraints),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

    % The working part of prop_mode_constraints_in_pred/8, with the pred_info
    % unpacked from the ModuleInfo.
    %
:- pred do_prop_mode_constraints_in_pred(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, mc_var_info::in, mc_var_info::out,
    pred_constraints_map::in, pred_constraints_map::out) is det.

do_prop_mode_constraints_in_pred(ModuleInfo, PredId, !PredInfo, !VarInfo,
        !PredConstraintsMap) :-
    % XXX Currently the constraints simply say that if a variable is bound
    % at a disjunct it is bound at the disjunction by making the relevant
    % variables equivalent. Setting GoalPathOptimisation to yes will cause
    % the disjucts to be given the same path as the disjunction, so that
    % the relevant constraint variables will not need to be constrained
    % equivalent - they will be the same variable. It will do the same
    % for other path types with similar equivalence constraints -
    % refer to the goal_path module for a more detailed description.
    % GoalPathOptimisation = no.
    %
    % XXX The above comment is obsolete, in that the current version of the
    % goal_path module leaves the task of making the relevant variables
    % equivalent to THIS module.

    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    fill_goal_id_slots_in_clauses(ModuleInfo, ContainingGoalMap,
        ClausesInfo0, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),

    % If mode inference requested, just add constraints for the clause body,
    % otherwise, process the predicate for each of the procedures.

    ( if pred_info_infer_modes(!.PredInfo) then
        add_clauses_constraints(ModuleInfo, PredId, !.PredInfo, !VarInfo,
            init_pred_p_c_constraints, BodyConstraints)
    else
        prop_mode_constraints_in_mode_declared_pred(ModuleInfo, PredId,
            !.PredInfo, !VarInfo, BodyConstraints)
    ),
    map.set(PredId, {ContainingGoalMap, BodyConstraints},
        !PredConstraintsMap).

    % prop_mode_constraints_in_mode_declared_pred(ModuleInfo, PredId, PredInfo,
    %   !VarInfo, FullConstraints):
    %
    % Uses the clauses and mode declarations in PredInfo (which should be
    % for predicate PredId taken from ModuleInfo) to create producer/consumer
    % constraints for program variables in predicate PredId. VarInfo is updated
    % with any constraint variables used.
    %
:- pred prop_mode_constraints_in_mode_declared_pred(module_info::in,
    pred_id::in, pred_info::in, mc_var_info::in, mc_var_info::out,
    mode_constraints::out) is det.

prop_mode_constraints_in_mode_declared_pred(ModuleInfo, PredId, PredInfo,
        !VarInfo, FullConstraints) :-
    ProcIds = pred_info_all_procids(PredInfo),

    add_clauses_constraints(ModuleInfo, PredId, PredInfo, !VarInfo,
        init_pred_p_c_constraints, BodyConstr),

    % Store procedure specific constraints in the constraints structure
    list.map(pred_info_proc_info(PredInfo), ProcIds, ProcInfos),
    list.foldl2_corresponding(
        prop_mode_constraints_in_mode_declared_proc(ModuleInfo, PredId),
        ProcIds, ProcInfos, !VarInfo, BodyConstr, FullConstraints).

    % prop_mode_constraints_in_mode_declared_proc(ModuleInfo, PredId, ProcId,
    %   ProcInfo, !VarInfo, !PredConstraints):
    %
    % Adds constraints based on the mode declaration in ProcInfo to
    % the PredConstraints structure, associating them specifically with ProcId.
    % Relies on the constraint variables associated with the head variables
    % of PredId at the empty goal path being stored in VarInfo.
    %
:- pred prop_mode_constraints_in_mode_declared_proc(module_info::in,
    pred_id::in, proc_id::in, proc_info::in, mc_var_info::in,
    mc_var_info::out, mode_constraints::in, mode_constraints::out) is det.

prop_mode_constraints_in_mode_declared_proc(ModuleInfo, PredId, ProcId,
        ProcInfo, !VarInfo, !PredConstraints) :-
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_headvars(ProcInfo, Args),
    add_mode_decl_constraints(ModuleInfo, PredId, ProcId, ArgModes, Args,
        !VarInfo, !PredConstraints).

%----------------------------------------------------------------------------%

ensure_unique_arguments(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
    clauses_info_get_var_table(ClausesInfo0, VarTable0),
    split_var_table(VarTable0, VarSet0, VarTypes0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars),

    SeenSoFar = set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),
    get_clause_list_for_replacement(ClausesRep0, Clauses0),
    BodyGoals0 = list.map(func(X) = clause_body(X), Clauses0),
    list.map_foldl3(ensure_unique_arguments_in_goal, BodyGoals0, BodyGoals,
        SeenSoFar, _, VarSet0, VarSet, VarTypes0, VarTypes),

    Clauses = list.map_corresponding(func(C, B) = C ^ clause_body := B,
        Clauses0, BodyGoals),
    set_clause_list(Clauses, ClausesRep),
    some [!ClausesInfo] (
        !:ClausesInfo = ClausesInfo0,
        make_var_table(!.ModuleInfo, VarSet, VarTypes, VarTable),
        clauses_info_set_var_table(VarTable, !ClausesInfo),
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers, !ClausesInfo),
        pred_info_set_clauses_info(!.ClausesInfo, PredInfo0, PredInfo)
    ),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

    % ensure_unique_arguments_in_goal(!Goal, !SeenSoFar, !VarSet, !VarTypes):
    %
    % Create variables and introduce unifications in Goal, where appropriate,
    % to ensure that no program variable in SeenSoFar is used as an argument in
    % a predicate call and that no program variable is used as an argument for
    % more than one predicate call.
    % Created variables are added to VarSet, VarTypes and SeenSoFar. Variables
    % used as arguments in predicate calls are added to SeenSoFar.
    %
:- pred ensure_unique_arguments_in_goal(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

ensure_unique_arguments_in_goal(!Goal, !SeenSoFar, !VarSet, !VarTypes) :-
    some [!GoalExpr, !GoalInfo] (
        !.Goal = hlds_goal(!:GoalExpr, !:GoalInfo),
        (
            !.GoalExpr = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                list.map_foldl3(ensure_unique_arguments_in_goal,
                    Goals0, Goals1, !SeenSoFar, !VarSet, !VarTypes),
                flatten_conjunction(Goals1, Goals),
                !:GoalExpr = conj(plain_conj, Goals)
            ;
                ConjType = parallel_conj,
                list.map_foldl3(ensure_unique_arguments_in_goal,
                    Goals0, Goals, !SeenSoFar, !VarSet, !VarTypes),
                !:GoalExpr = conj(parallel_conj, Goals)
            )
        ;
            !.GoalExpr = plain_call(CalleePredId, CalleeProcId, Args0,
                Builtin, UnifyContext, SymName),
            Context = goal_info_get_context(!.GoalInfo),
            make_unifications(Context, Unifications, Args0, Args, !SeenSoFar,
                !VarSet, !VarTypes),
            (
                % No arguments changed.
                Unifications = []
            ;
                % Some of the argument variables have been replaced.
                % Need to put the call with its new args in a conjunction
                % with the unifications.
                Unifications = [_ | _],
                CallGoalExpr = plain_call(CalleePredId, CalleeProcId, Args,
                    Builtin, UnifyContext, SymName),
                replace_call_with_conjunction(CallGoalExpr, Unifications,
                    Args, !:GoalExpr, !GoalInfo)
            )

        ;
            !.GoalExpr = generic_call(Details, Args0, Modes, MaybeArgRegs,
                Determinism),
            Context = goal_info_get_context(!.GoalInfo),
            make_unifications(Context, Unifications, Args0, Args, !SeenSoFar,
                !VarSet, !VarTypes),
            (
                % No arguments changed.
                Unifications = []
            ;
                % Some of the argument variables have been replaced.
                % Need to put the call with its new args in a conjunction
                % with the unifications.
                Unifications = [_ | _],
                CallGoalExpr = generic_call(Details, Args, Modes, MaybeArgRegs,
                    Determinism),
                replace_call_with_conjunction(CallGoalExpr, Unifications,
                    Args, !:GoalExpr, !GoalInfo)
            )
        ;
            !.GoalExpr = switch(_SwitchVar, _CanFail, _Cases0),
            unexpected($pred, "switch")
        ;
            !.GoalExpr = unify(_, _, _, _, _)
        ;
            !.GoalExpr = disj(Goals0),
            list.map_foldl3(ensure_unique_arguments_in_goal, Goals0, Goals,
                !SeenSoFar, !VarSet, !VarTypes),
            !:GoalExpr = disj(Goals)
        ;
            !.GoalExpr = negation(Goal0),
            ensure_unique_arguments_in_goal(Goal0, Goal, !SeenSoFar, !VarSet,
                !VarTypes),
            !:GoalExpr = negation(Goal)
        ;
            !.GoalExpr = scope(Reason, Goal0),
            % XXX We should special-case the handling of
            % from_ground_term_construct scopes.
            ensure_unique_arguments_in_goal(Goal0, Goal, !SeenSoFar, !VarSet,
                !VarTypes),
            !:GoalExpr = scope(Reason, Goal)
        ;
            !.GoalExpr = if_then_else(ExistVars, Cond0, Then0, Else0),
            ensure_unique_arguments_in_goal(Cond0, Cond, !SeenSoFar, !VarSet,
                !VarTypes),
            ensure_unique_arguments_in_goal(Then0, Then, !SeenSoFar, !VarSet,
                !VarTypes),
            ensure_unique_arguments_in_goal(Else0, Else, !SeenSoFar, !VarSet,
                !VarTypes),
            !:GoalExpr = if_then_else(ExistVars, Cond, Then, Else)
        ;
            !.GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ;
            !.GoalExpr = shorthand(ShortHand0),
            (
                ShortHand0 = atomic_goal(GoalType, Outer, Inner,
                    MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
                ensure_unique_arguments_in_goal(MainGoal0, MainGoal,
                    !SeenSoFar, !VarSet, !VarTypes),
                list.map_foldl3(ensure_unique_arguments_in_goal,
                    OrElseGoals0, OrElseGoals, !SeenSoFar, !VarSet, !VarTypes),
                ShortHand = atomic_goal(GoalType, Outer, Inner,
                    MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
                !:GoalExpr = shorthand(ShortHand)
            ;
                ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
                ensure_unique_arguments_in_goal(SubGoal0, SubGoal, !SeenSoFar,
                    !VarSet, !VarTypes),
                ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
                !:GoalExpr = shorthand(ShortHand)
            ;
                ShortHand0 = bi_implication(_, _),
                unexpected($pred, "bi_implication")
            )
        ),
        !:Goal = hlds_goal(!.GoalExpr, !.GoalInfo)
    ).

    % flatten_conjunction(!Goals) flattens the conjunction Goals - that
    % is, moves the conjuncts from nested conjunctions into Goals.
    %
:- pred flatten_conjunction(list(hlds_goal)::in, list(hlds_goal)::out) is det.

flatten_conjunction(!Goals) :-
    list.foldr(add_to_before_conjunction, !.Goals, [], !:Goals).

    % add_to_before_conjunction(Goal, !Goals) adds Goal to the front of the
    % conjunction Goals. It keeps the conjunction flat, so nested conjunctions
    % are flattened and their conjuncts prepended to Goals.
    %
:- pred add_to_before_conjunction(hlds_goal::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

add_to_before_conjunction(Goal, !Goals) :-
    ( if Goal = hlds_goal(conj(plain_conj, SubGoals), _) then
        !:Goals = SubGoals ++ !.Goals
    else
        !:Goals = [Goal | !.Goals]
    ).

    % make_unifications(Context, MaybeUnifications, Args0, Args, !SeenSoFar,
    %   !VarSet, !VarTypes):
    %
    % If any of the given arguments in Args0 is in SeenSoFar, creates a new
    % argument (in VarSet and VarTypes) to replace it (in Args), and generates
    % a unification between new argument and old (with context Context).
    %
:- pred make_unifications(prog_context::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out,
    set_of_progvar::in, set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_unifications(Context, Unifications, !Args, !SeenSoFar,
        !VarSet, !VarTypes) :-
    list.map_foldl4(make_unification(Context), !Args,
        [], Unifications, !SeenSoFar, !VarSet, !VarTypes).

    % make_unification(Context, Var0, Var, !Unifications, !SeenSoFar,
    %   !VarSet, !VarTypes):
    %
    % If Var0 is in SeenSoFar, creates a new argument Var (in VarSet and
    % VarTypes), and generates a unification between Var0 and Var.
    %
:- pred make_unification(prog_context::in, prog_var::in, prog_var::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_unification(Context, Var0, Var, !Unifications, !SeenSoFar, !VarSet,
        !VarTypes) :-
    ( if set_of_var.contains(!.SeenSoFar, Var0) then
        % Make new variable.
        OldVarName = varset.lookup_name(!.VarSet, Var0),
        lookup_var_type(!.VarTypes, Var0, OldVarType),
        NewVarName = "Arg_" ++ OldVarName,
        varset.new_uniquely_named_var(NewVarName, Var, !VarSet),
        add_var_type(Var, OldVarType, !VarTypes),

        % Make new unification.
        create_atomic_complicated_unification(Var0, rhs_var(Var), Context,
            umc_implicit("Making call args unique for cbma"), [], purity_pure,
            UnificationGoal0),
        UnificationGoal0 =
            hlds_goal(UnificationGoalExpr, UnificationGoalInfo0),
        goal_info_set_nonlocals(set_of_var.list_to_set([Var0, Var]),
            UnificationGoalInfo0, UnificationGoalInfo),
        UnificationGoal =
            hlds_goal(UnificationGoalExpr, UnificationGoalInfo),
        !:Unifications = [UnificationGoal | !.Unifications]
    else
        Var = Var0
    ),
    set_of_var.insert(Var, !SeenSoFar).

    % replace_call_with_conjunction(NewCallGoalExpr, Unifications, NewArgs,
    %   GoalExpr, !GoalInfo):
    %
    % Makes a conjunction out of CallGoalExpr and Unifications - the
    % conjunction becomes GoalExpr and the goal info for the conjunction
    % becomes !:GoalInfo. Goal info for CallGoalExpr and GoalExpr is
    % created with the assumption that GoalExpr replaces a call and that
    % CallGoalExpr is that call with its arguments replaced by NewArgs
    % (where Unifications contains unfications between old arguments
    % and their new replacements).
    %
:- pred replace_call_with_conjunction(hlds_goal_expr::in, list(hlds_goal)::in,
    list(prog_var)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

replace_call_with_conjunction(CallGoalExpr, Unifications, NewArgs, GoalExpr,
        !GoalInfo) :-
    CallGoalInfo0 = !.GoalInfo,
    Context = goal_info_get_context(CallGoalInfo0),
    CallNonlocals0 = goal_info_get_nonlocals(CallGoalInfo0),
    set_of_var.insert_list(NewArgs, CallNonlocals0, CallNonlocals),
    goal_info_set_nonlocals(CallNonlocals, CallGoalInfo0, CallGoalInfo),
    Goals = [hlds_goal(CallGoalExpr, CallGoalInfo) | Unifications],

    % Create the new conjunction
    GoalExpr = conj(plain_conj, Goals),
    goal_info_init(!:GoalInfo),
    goal_info_set_context(Context, !GoalInfo),
    goal_info_set_nonlocals(CallNonlocals0, !GoalInfo).

%----------------------------------------------------------------------------%

module_info_pred_status_is_imported(ModuleInfo, PredId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),

    % The following used because pred_info_is_imported/2 is not
    % as comprehensive as status_is_imported/2.
    pred_info_get_status(PredInfo, Status),
    pred_status_is_imported(Status) = yes.

%----------------------------------------------------------------------------%

    % Print the constraints to the current output stream in human readable
    % format. It titles each pred's constraints with a module qualification
    % based on the default filename for the module followed by the
    % predicate's name.
    %
pretty_print_pred_constraints_map(ModuleInfo, ConstraintVarSet,
        PredConstraintsMap, !IO) :-
    ConstrainedPreds = map.keys(PredConstraintsMap),
    list.foldl(
        pretty_print_pred_constraints(ModuleInfo, ConstraintVarSet,
            PredConstraintsMap),
        ConstrainedPreds, !IO).

    % Print the constraints for the specified predicate from the
    % pred_constraints_map to the current output stream in a human
    % readable format.
    %
:- pred pretty_print_pred_constraints(module_info::in, mc_varset::in,
    pred_constraints_map::in, pred_id::in, io::di, io::uo) is det.

pretty_print_pred_constraints(ModuleInfo, ConstraintVarSet,
        PredConstraintsMap, PredId, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),

    % Start with a blank line.
    write_error_pieces_plain(Globals, [fixed("")], !IO),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    write_error_pieces_plain(Globals, [words("Constraints for")] ++
        describe_one_pred_info_name(should_module_qualify, PredInfo) ++
        [suffix(":")], !IO),

    map.lookup(PredConstraintsMap, PredId, {_, PredConstraints}),
    AllProcAnnConstraints = allproc_annotated_constraints(PredConstraints),
    dump_constraints_and_annotations(Globals, ConstraintVarSet,
        AllProcAnnConstraints, !IO),
    list.foldl(
        pretty_print_proc_constraints(ModuleInfo, ConstraintVarSet,
            PredConstraints, PredId),
        pred_info_all_procids(PredInfo), !IO).

    % Puts the constraints specific to the procedure indicated from
    % the pred_p_c_constraints to the current output stream in human
    % readable format.
    %
:- pred pretty_print_proc_constraints(module_info::in, mc_varset::in,
    pred_p_c_constraints::in, pred_id::in, proc_id::in, io::di, io::uo) is det.

pretty_print_proc_constraints(ModuleInfo, ConstraintVarSet, PredConstraints,
        PredId, ProcId, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),

    % Start with a blank line.
    write_error_pieces_plain(Globals, [fixed("")], !IO),

    write_error_pieces_plain(Globals, describe_one_proc_name(ModuleInfo,
        should_module_qualify, proc(PredId, ProcId)) ++ [suffix(":")], !IO),
    ProcSpecAnnConstraints =
        proc_specific_annotated_constraints(ProcId, PredConstraints),
    dump_constraints_and_annotations(Globals, ConstraintVarSet,
        ProcSpecAnnConstraints, !IO).

%----------------------------------------------------------------------------%
:- end_module check_hlds.prop_mode_constraints.
%----------------------------------------------------------------------------%
