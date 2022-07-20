%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module check_hlds.clause_to_proc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%-----------------------------------------------------------------------------%

    % In the hlds, we initially record the clauses for a predicate in the
    % clauses_info data structure, which is part of the pred_info data
    % structure. But once the clauses have been type-checked, we want to have
    % a separate copy of each clause for each different mode of the predicate,
    % since we may end up reordering the clauses differently in different
    % modes. Here we copy the clauses from the clause_info data structure
    % into the proc_info data structure. Each clause is marked with a list
    % of the modes for which it applies, so that there can be different code
    % to implement different modes of a predicate (e.g. sort). For each mode
    % of the predicate, we select the clauses for that mode, disjoin them
    % together, and save this in the proc_info.
    %
:- pred copy_clauses_to_proc_for_all_valid_procs(
    module_info::in, module_info::out) is det.
:- pred copy_clauses_to_procs_for_pred_in_module_info(pred_id::in,
    module_info::in, module_info::out) is det.
:- pred copy_clauses_to_nonmethod_procs_for_preds_in_module_info(
    list(pred_id)::in, module_info::in, module_info::out) is det.

:- pred copy_clauses_to_proc_in_proc_info(pred_info::in, proc_id::in,
    proc_info::in, proc_info::out) is det.

:- pred should_copy_clauses_to_procs(pred_info::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.

%-----------------------------------------------------------------------------%

copy_clauses_to_proc_for_all_valid_procs(!ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.keys(PredIdTable0, PredIds),
    list.foldl(copy_pred_clauses_to_procs_in_pred_id_table, PredIds,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

copy_clauses_to_procs_for_pred_in_module_info(PredId, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    copy_pred_clauses_to_procs_in_pred_id_table(PredId,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred copy_pred_clauses_to_procs_in_pred_id_table(pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

copy_pred_clauses_to_procs_in_pred_id_table(PredId, !PredIdTable) :-
    map.lookup(!.PredIdTable, PredId, PredInfo0),
    copy_clauses_to_procs_in_pred_info(PredId, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredIdTable).

:- pred copy_clauses_to_procs_in_pred_info(pred_id::in,
    pred_info::in, pred_info::out) is det.

copy_clauses_to_procs_in_pred_info(PredId, !PredInfo) :-
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo),
    pred_info_get_proc_table(!.PredInfo, ProcMap0),
    map.map_values(
        copy_clauses_to_maybe_imported_proc_in_proc_info(!.PredInfo,
            ClausesInfo, PredId),
        ProcMap0, ProcMap),
    pred_info_set_proc_table(ProcMap, !PredInfo).

:- pred copy_clauses_to_maybe_imported_proc_in_proc_info(pred_info::in,
    clauses_info::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out) is det.

copy_clauses_to_maybe_imported_proc_in_proc_info(PredInfo, ClausesInfo,
        _PredId, ProcId, !ProcInfo) :-
    ( if
        (
            pred_info_is_imported(PredInfo)
        ;
            pred_info_is_pseudo_imported(PredInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        )
    then
        % We need to set these fields in the proc_info here, because
        % some parts of the compiler (e.g. unused_args.m) depend on
        % these fields being valid even for imported procedures.

        % XXX ARGVEC - when the proc_info uses the proc_arg_vector,
        % just pass the headvar vector directly to the proc_info.
        clauses_info_get_headvars(ClausesInfo, HeadVars),
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        clauses_info_get_var_table(ClausesInfo, VarTable),
        clauses_info_get_rtti_varmaps(ClausesInfo, RttiVarMaps),
        proc_info_set_headvars(HeadVarList, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
    else
        copy_clauses_to_proc_in_proc_info(PredInfo, ProcId, !ProcInfo)
    ).

%-----------------------------------------------------------------------------%

copy_clauses_to_nonmethod_procs_for_preds_in_module_info(PredIds,
        !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    list.foldl(copy_pred_clauses_to_nonmethod_procs_in_pred_id_table, PredIds,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

    % For each mode of the given predicate, copy the clauses relevant
    % to the mode and the current backend to the proc_info.
    %
:- pred copy_pred_clauses_to_nonmethod_procs_in_pred_id_table(pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

copy_pred_clauses_to_nonmethod_procs_in_pred_id_table(PredId, !PredIdTable) :-
    map.lookup(!.PredIdTable, PredId, PredInfo0),
    ( if should_copy_clauses_to_procs(PredInfo0) then
        copy_clauses_to_procs_in_pred_info(PredId, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredIdTable)
    else
        true
    ).

%-----------------------------------------------------------------------------%

copy_clauses_to_proc_in_proc_info(PredInfo, ProcId, !ProcInfo) :-
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    ClausesInfo = clauses_info(_, _, VarTable0, RttiVarMaps, _, HeadVars,
        ClausesRep0, _ItemNumbers, _HaveForeignClauses, _HadSyntaxError),
    % The "replacement" is the replacement of the pred_info's clauses_rep
    % with the goal in the proc_info; the clauses_rep won't be needed again.
    get_clause_list_for_replacement(ClausesRep0, Clauses),
    select_matching_clauses(PredInfo, ProcId, Clauses, MatchingClauses),
    get_clause_disjuncts_and_warnings(MatchingClauses, ClausesDisjuncts,
        StateVarWarnings),
    (
        StateVarWarnings = [_ | _],
        proc_info_set_statevar_warnings(StateVarWarnings, !ProcInfo)
    ;
        StateVarWarnings = []
        % Do not allocate a new proc_info if we do not need to.
    ),
    (
        ClausesDisjuncts = [SingleGoal],
        SingleGoal = hlds_goal(SingleExpr, _),
        (
            SingleExpr = call_foreign_proc(_, _, _, Args, ExtraArgs,
                MaybeTraceRuntimeCond, _),
            % Use the original variable names for the headvars of foreign_proc
            % clauses, not the introduced `HeadVar__n' names.
            list.foldl(set_arg_names, Args, VarTable0, VarTable),
            expect(unify(ExtraArgs, []), $pred, "extra_args"),
            expect(unify(MaybeTraceRuntimeCond, no), $pred,
                "trace runtime cond")
        ;
            ( SingleExpr = plain_call(_, _, _, _, _, _)
            ; SingleExpr = generic_call(_, _, _, _, _)
            ; SingleExpr = unify(_, _, _, _, _)
            ; SingleExpr = conj(_, _)
            ; SingleExpr = disj(_)
            ; SingleExpr = switch(_, _, _)
            ; SingleExpr = if_then_else(_,_,  _, _)
            ; SingleExpr = negation(_)
            ; SingleExpr = scope(_, _)
            ; SingleExpr = shorthand(_)
            ),
            VarTable = VarTable0
        ),
        Goal = SingleGoal
    ;
        % We use the context of the first clause, unless there were
        % no clauses at all, in which case we use the context of the
        % mode declaration.
        (
            ClausesDisjuncts = [FirstGoal, _ | _],
            FirstGoal = hlds_goal(_, FirstGoalInfo),
            Context = goal_info_get_context(FirstGoalInfo)
        ;
            ClausesDisjuncts = [],
            proc_info_get_context(!.ProcInfo, Context)
        ),

        VarTable = VarTable0,

        % Convert the list of clauses into a disjunction,
        % and construct a goal_info for the disjunction.

        % The nonlocal vars are just the head variables.
        NonLocalVars =
            set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),

        % The disjunction is impure/semipure if any of the disjuncts
        % is impure/semipure.
        accumulate_disjunction_purity(ClausesDisjuncts,
            purity_pure, DisjunctionPurity),

        % The InstMapDelta and Detism are just placeholders; they will be
        % overridden by the actual computed values later.
        instmap_delta_init_unreachable(InstMapDelta),
        Detism = detism_erroneous,

        goal_info_init(NonLocalVars, InstMapDelta, Detism,
            DisjunctionPurity, Context, GoalInfo),
        Goal = hlds_goal(disj(ClausesDisjuncts), GoalInfo)
    ),
    % XXX ARGVEC - when the proc_info is converted to use proc_arg_vectors
    % we should just pass the headvar vector in directly.
    HeadVarList = proc_arg_vector_to_list(HeadVars),
    proc_info_set_body_vt(VarTable, HeadVarList, Goal, RttiVarMaps, !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred select_matching_clauses(pred_info::in, proc_id::in,
    list(clause)::in, list(clause)::out) is det.

select_matching_clauses(PredInfo, ProcId, Clauses, MatchingClauses) :-
    pred_info_get_origin(PredInfo, Origin),
    % To allow us to process even *very* long lists of clauses without
    % running out of stack, we have to keep select_matching_clauses_loop
    % tail recursive. We do this by making it add each matching clause
    % it processes to the *front* of the list of so-far-detected-to-be-matching
    % clauses, which computes the list of matching clauses in reverse.
    RevMatchingClauses0 = [],
    ( if Origin = origin_special_pred(spec_pred_unify, _TypeCtor) then
        ( if hlds_pred.in_in_unification_proc_id(ProcId) then
            MaybeInInMode = in_in_mode
        else
            MaybeInInMode = not_in_in_mode
        ),
        select_matching_unify_clauses_acc(MaybeInInMode, ProcId, Clauses,
            RevMatchingClauses0, RevMatchingClauses)
    else
        select_matching_nonunify_clauses_acc(ProcId, Clauses,
            RevMatchingClauses0, RevMatchingClauses)
    ),
    list.reverse(RevMatchingClauses, MatchingClauses).

:- type maybe_in_in_mode
    --->    not_in_in_mode
    ;       in_in_mode.

:- pred select_matching_unify_clauses_acc(maybe_in_in_mode::in, proc_id::in,
    list(clause)::in, list(clause)::in, list(clause)::out) is det.

select_matching_unify_clauses_acc(_, _, [], !RevMatchingClauses).
select_matching_unify_clauses_acc(MaybeInInMode, ProcId, [Clause | Clauses],
        !RevMatchingClauses) :-
    ApplicableProcIds = Clause ^ clause_applicable_procs,
    (
        ApplicableProcIds = all_modes,
        !:RevMatchingClauses = [Clause | !.RevMatchingClauses]
    ;
        ApplicableProcIds = selected_modes(_),
        unexpected($pred, "non unify mode")
    ;
        ApplicableProcIds = unify_in_in_modes,
        (
            MaybeInInMode = not_in_in_mode
        ;
            MaybeInInMode = in_in_mode,
            !:RevMatchingClauses = [Clause | !.RevMatchingClauses]
        )
    ;
        ApplicableProcIds = unify_non_in_in_modes,
        (
            MaybeInInMode = not_in_in_mode,
            !:RevMatchingClauses = [Clause | !.RevMatchingClauses]
        ;
            MaybeInInMode = in_in_mode
        )
    ),
    select_matching_unify_clauses_acc(MaybeInInMode, ProcId, Clauses,
        !RevMatchingClauses).

:- pred select_matching_nonunify_clauses_acc(proc_id::in, list(clause)::in,
    list(clause)::in, list(clause)::out) is det.

select_matching_nonunify_clauses_acc(_, [], !RevMatchingClauses).
select_matching_nonunify_clauses_acc(ProcId, [Clause | Clauses],
        !RevMatchingClauses) :-
    ApplicableProcIds = Clause ^ clause_applicable_procs,
    (
        ApplicableProcIds = all_modes,
        !:RevMatchingClauses = [Clause | !.RevMatchingClauses]
    ;
        ApplicableProcIds = selected_modes(ProcIds),
        ( if list.member(ProcId, ProcIds) then
            !:RevMatchingClauses = [Clause | !.RevMatchingClauses]
        else
            true
        )
    ;
        ( ApplicableProcIds = unify_in_in_modes
        ; ApplicableProcIds = unify_non_in_in_modes
        ),
        unexpected($pred, "unify mode")
    ),
    select_matching_nonunify_clauses_acc(ProcId, Clauses, !RevMatchingClauses).

%-----------------------------------------------------------------------------%

:- pred get_clause_disjuncts_and_warnings(list(clause)::in,
    list(hlds_goal)::out, list(error_spec)::out) is det.

get_clause_disjuncts_and_warnings([], [], []).
get_clause_disjuncts_and_warnings([Clause | Clauses], Disjuncts, Warnings) :-
    Goal = Clause ^ clause_body,
    goal_to_disj_list(Goal, FirstDisjuncts),
    FirstWarnings = Clause ^ clause_statevar_warnings,
    get_clause_disjuncts_and_warnings(Clauses, LaterDisjuncts, LaterWarnings),
    Disjuncts = FirstDisjuncts ++ LaterDisjuncts,
    Warnings = FirstWarnings ++ LaterWarnings.

%-----------------------------------------------------------------------------%

:- pred set_arg_names(foreign_arg::in, var_table::in, var_table::out)
    is det.

set_arg_names(Arg, !VarTable) :-
    Var = foreign_arg_var(Arg),
    MaybeNameMode = foreign_arg_maybe_name_mode(Arg),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, _)),
        update_var_name(Var, Name, !VarTable)
    ;
        MaybeNameMode = no
    ).

%-----------------------------------------------------------------------------%

:- pred accumulate_disjunction_purity(list(hlds_goal)::in,
    purity::in, purity::out) is det.

accumulate_disjunction_purity([], !Purity).
accumulate_disjunction_purity([Disjunct | Disjuncts], !Purity) :-
    DisjunctPurity = goal_get_purity(Disjunct),
    !:Purity = worst_purity(!.Purity, DisjunctPurity),
    accumulate_disjunction_purity(Disjuncts, !Purity).

%-----------------------------------------------------------------------------%

should_copy_clauses_to_procs(PredInfo) :-
    % Don't process typeclass methods, because their proc_infos
    % are generated already mode-correct.
    pred_info_get_markers(PredInfo, PredMarkers),
    not check_marker(PredMarkers, marker_class_method).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.clause_to_proc.
%-----------------------------------------------------------------------------%
