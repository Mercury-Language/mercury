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
:- import_module hlds.hlds_clauses.
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
:- pred copy_module_clauses_to_procs(list(pred_id)::in,
    module_info::in, module_info::out) is det.
:- pred copy_clauses_to_proc(proc_id::in, clauses_info::in,
    proc_info::in, proc_info::out) is det.

:- pred should_copy_clauses_to_procs(pred_info::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.

:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

copy_module_clauses_to_procs(PredIds, !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    list.foldl(copy_pred_clauses_to_procs_if_needed, PredIds,
        PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

    % For each mode of the given predicate, copy the clauses relevant
    % to the mode and the current backend to the proc_info.
    %
    % This is not the only predicate in the compiler that does this task;
    % the other is polymorphism.process_proc.
    %
:- pred copy_pred_clauses_to_procs_if_needed(pred_id::in,
    pred_table::in, pred_table::out) is det.

copy_pred_clauses_to_procs_if_needed(PredId, !PredTable) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    ( if should_copy_clauses_to_procs(PredInfo0) then
        copy_clauses_to_procs(PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredTable)
    else
        true
    ).

should_copy_clauses_to_procs(PredInfo) :-
    % Don't process typeclass methods, because their proc_infos
    % are generated already mode-correct.
    pred_info_get_markers(PredInfo, PredMarkers),
    not check_marker(PredMarkers, marker_class_method).

:- pred copy_clauses_to_procs(pred_info::in, pred_info::out) is det.

copy_clauses_to_procs(!PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, ProcMap0),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo),
    ProcIds = pred_info_all_non_imported_procids(!.PredInfo),
    copy_clauses_to_procs_2(ProcIds, ClausesInfo, ProcMap0, ProcMap),
    pred_info_set_proc_table(ProcMap, !PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id)::in, clauses_info::in,
    proc_table::in, proc_table::out) is det.

copy_clauses_to_procs_2([], _, !ProcMap).
copy_clauses_to_procs_2([ProcId | ProcIds], ClausesInfo, !ProcMap) :-
    map.lookup(!.ProcMap, ProcId, ProcInfo0),
    copy_clauses_to_proc(ProcId, ClausesInfo, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcMap),
    copy_clauses_to_procs_2(ProcIds, ClausesInfo, !ProcMap).

%-----------------------------------------------------------------------------%

copy_clauses_to_proc(ProcId, ClausesInfo, !ProcInfo) :-
    ClausesInfo = clauses_info(VarSet0, _, _, VarTypes, HeadVars, ClausesRep0,
        _ItemNumbers, RttiInfo, _HaveForeignClauses, _HadSyntaxError),
    % The "replacement" is the replacement of the pred_info's clauses_rep
    % with the goal in the proc_info; the clauses_rep won't be needed again.
    get_clause_list_for_replacement(ClausesRep0, Clauses),
    select_matching_clauses(Clauses, ProcId, MatchingClauses),
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
            VarSet = list.foldl(set_arg_names, Args, VarSet0),
            expect(unify(ExtraArgs, []), $module, $pred, "extra_args"),
            expect(unify(MaybeTraceRuntimeCond, no), $module, $pred,
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
            VarSet = VarSet0
        ),
        Goal = SingleGoal
    ;
        % We use the context of the first clause, unless there weren't
        % any clauses at all, in which case we use the context of the
        % mode declaration.
        (
            ClausesDisjuncts = [FirstGoal, _ | _],
            FirstGoal = hlds_goal(_, FirstGoalInfo),
            Context = goal_info_get_context(FirstGoalInfo)
        ;
            ClausesDisjuncts = [],
            proc_info_get_context(!.ProcInfo, Context)
        ),

        % Convert the list of clauses into a disjunction,
        % and construct a goal_info for the disjunction.

        VarSet = VarSet0,
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),

        % The non-local vars are just the head variables.
        NonLocalVars =
            set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),
        goal_info_set_nonlocals(NonLocalVars, GoalInfo1, GoalInfo2),

        % The disjunction is impure/semipure if any of the disjuncts
        % is impure/semipure.
        ( if contains_nonpure_goal(ClausesDisjuncts) then
            PurityList = list.map(goal_get_purity, ClausesDisjuncts),
            Purity = list.foldl(worst_purity, PurityList, purity_pure),
            goal_info_set_purity(Purity, GoalInfo2, GoalInfo)
        else
            GoalInfo2 = GoalInfo
        ),

        Goal = hlds_goal(disj(ClausesDisjuncts), GoalInfo)
    ),
    % XXX ARGVEC - when the proc_info is converted to use proc_arg_vectors
    % we should just pass the headvar vector in directly.
    HeadVarList = proc_arg_vector_to_list(HeadVars),
    proc_info_set_body(VarSet, VarTypes, HeadVarList, Goal, RttiInfo,
        !ProcInfo).

:- pred contains_nonpure_goal(list(hlds_goal)::in) is semidet.

contains_nonpure_goal([Goal | Goals]) :-
    (
        goal_get_purity(Goal) \= purity_pure
    ;
        contains_nonpure_goal(Goals)
    ).

:- func set_arg_names(foreign_arg, prog_varset) = prog_varset.

set_arg_names(Arg, !.Vars) = !:Vars :-
    Var = foreign_arg_var(Arg),
    MaybeNameMode = foreign_arg_maybe_name_mode(Arg),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, _)),
        varset.name_var(Var, Name, !Vars)
    ;
        MaybeNameMode = no
    ).

:- pred select_matching_clauses(list(clause)::in, proc_id::in,
    list(clause)::out) is det.

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
    select_matching_clauses(Clauses, ProcId, MatchingClausesTail),
    ApplicableProcIds = Clause ^ clause_applicable_procs,
    (
        ApplicableProcIds = all_modes,
        MatchingClauses = [Clause | MatchingClausesTail]
    ;
        ApplicableProcIds = selected_modes(ProcIds),
        ( if list.member(ProcId, ProcIds) then
            MatchingClauses = [Clause | MatchingClausesTail]
        else
            MatchingClauses = MatchingClausesTail
        )
    ).

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
:- end_module check_hlds.clause_to_proc.
%-----------------------------------------------------------------------------%
