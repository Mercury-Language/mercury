%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module check_hlds__clause_to_proc.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module list.
:- import_module std_util.

    % In the hlds, we initially record the clauses for a predicate
    % in the clauses_info data structure which is part of the
    % pred_info data structure.  But once the clauses have been
    % type-checked, we want to have a separate copy of each clause
    % for each different mode of the predicate, since we may
    % end up reordering the clauses differently in different modes.
    % Here we copy the clauses from the clause_info data structure
    % into the proc_info data structure.  Each clause is marked
    % with a list of the modes for which it applies, so that
    % there can be different code to implement different modes
    % of a predicate (e.g. sort).  For each mode of the predicate,
    % we select the clauses for that mode, disjoin them together,
    % and save this in the proc_info.
    %
:- pred copy_module_clauses_to_procs(list(pred_id)::in,
    module_info::in, module_info::out) is det.

:- pred copy_clauses_to_proc(proc_id::in, clauses_info::in,
    proc_info::in, proc_info::out) is det.

    % Before copying the clauses to the procs, we need to add
    % a default mode of `:- mode foo(in, in, ..., in) = out is det.'
    % for functions that don't have an explicit mode declaration.
    %
:- pred maybe_add_default_func_modes(list(pred_id)::in,
    pred_table::in, pred_table::out) is det.

:- pred maybe_add_default_func_mode(pred_info::in, pred_info::out,
    maybe(proc_id)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__purity.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__make_hlds.
:- import_module libs__globals.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module varset.

maybe_add_default_func_modes([], Preds, Preds).
maybe_add_default_func_modes([PredId | PredIds], Preds0, Preds) :-
    map__lookup(Preds0, PredId, PredInfo0),
    maybe_add_default_func_mode(PredInfo0, PredInfo, _),
    map__det_update(Preds0, PredId, PredInfo, Preds1),
    maybe_add_default_func_modes(PredIds, Preds1, Preds).

maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProcId) :-
    pred_info_procedures(PredInfo0, Procs0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    (
        %
        % Is this a function with no modes?
        %
        PredOrFunc = function,
        map__is_empty(Procs0)
    ->
        %
        % If so, add a default mode of
        %
        %   :- mode foo(in, in, ..., in) = out is det.
        %
        % for this function.  (N.B. functions which can
        % fail must be explicitly declared as semidet.)
        %
        PredArity = pred_info_orig_arity(PredInfo0),
        FuncArity = PredArity - 1,
        in_mode(InMode),
        out_mode(OutMode),
        list__duplicate(FuncArity, InMode, FuncArgModes),
        FuncRetMode = OutMode,
        list__append(FuncArgModes, [FuncRetMode], PredArgModes),
        Determinism = det,
        pred_info_context(PredInfo0, Context),
        MaybePredArgLives = no,
        varset__init(InstVarSet),
            % No inst_vars in default func mode.
        add_new_proc(InstVarSet, PredArity, PredArgModes,
            yes(PredArgModes), MaybePredArgLives, yes(Determinism),
            Context, address_is_not_taken, PredInfo0, PredInfo, ProcId),
        MaybeProcId = yes(ProcId)
    ;
        PredInfo = PredInfo0,
        MaybeProcId = no
    ).

copy_module_clauses_to_procs(PredIds, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    list__foldl(copy_pred_clauses_to_procs, PredIds, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

    % For each mode of the given predicate, copy the clauses relevant
    % to the mode and the current backend to the proc_info.
    %
    % This is not the only predicate in the compiler that does this task;
    % the other is polymorphism__process_proc.
    %
:- pred copy_pred_clauses_to_procs(pred_id::in,
    pred_table::in, pred_table::out) is det.

copy_pred_clauses_to_procs(PredId, !PredTable) :-
    map__lookup(!.PredTable, PredId, PredInfo0),
    (
        % Don't process typeclass methods, because their proc_infos
        % are generated already mode-correct.
        pred_info_get_markers(PredInfo0, PredMarkers),
        check_marker(PredMarkers, class_method)
    ->
        true
    ;
        copy_clauses_to_procs(PredInfo0, PredInfo),
        map__det_update(!.PredTable, PredId, PredInfo, !:PredTable)
    ).

:- pred copy_clauses_to_procs(pred_info::in, pred_info::out) is det.

copy_clauses_to_procs(!PredInfo) :-
    pred_info_procedures(!.PredInfo, Procs0),
    pred_info_clauses_info(!.PredInfo, ClausesInfo),
    ProcIds = pred_info_all_non_imported_procids(!.PredInfo),
    copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
    pred_info_set_procedures(Procs, !PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id)::in, clauses_info::in,
    proc_table::in, proc_table::out) is det.

copy_clauses_to_procs_2([], _, !Procs).
copy_clauses_to_procs_2([ProcId | ProcIds], ClausesInfo, !Procs) :-
    map__lookup(!.Procs, ProcId, Proc0),
    copy_clauses_to_proc(ProcId, ClausesInfo, Proc0, Proc),
    map__det_update(!.Procs, ProcId, Proc, !:Procs),
    copy_clauses_to_procs_2(ProcIds, ClausesInfo, !Procs).

copy_clauses_to_proc(ProcId, ClausesInfo, !Proc) :-
    ClausesInfo = clauses_info(VarSet0, _, _, VarTypes, HeadVars,
        ClausesRep, RttiInfo, _),
    get_clause_list(ClausesRep, Clauses),
    select_matching_clauses(Clauses, ProcId, MatchingClauses),
    get_clause_goals(MatchingClauses, GoalList),
    ( GoalList = [SingleGoal] ->
        SingleGoal = SingleExpr - _,
        ( SingleExpr = foreign_proc(_, _, _, Args, ExtraArgs, _) ->
            %
            % Use the original variable names for the headvars
            % of foreign_proc clauses, not the introduced
            % `HeadVar__n' names.
            %
            VarSet = list__foldl(set_arg_names, Args, VarSet0),
            require(unify(ExtraArgs, []), "copy_clauses_to_proc: extra_args")
        ;
            VarSet = VarSet0
        ),
        Goal = SingleGoal
    ;
        VarSet = VarSet0,

        %
        % Convert the list of clauses into a disjunction,
        % and construct a goal_info for the disjunction.
        %

        %
        % We use the context of the first clause, unless
        % there weren't any clauses at all, in which case
        % we use the context of the mode declaration.
        %
        goal_info_init(GoalInfo0),
        ( GoalList = [FirstGoal | _] ->
            FirstGoal = _ - FirstGoalInfo,
            goal_info_get_context(FirstGoalInfo, Context)
        ;
            proc_info_context(!.Proc, Context)
        ),
        goal_info_set_context(GoalInfo0, Context, GoalInfo1),

        %
        % The non-local vars are just the head variables.
        %
        set__list_to_set(HeadVars, NonLocalVars),
        goal_info_set_nonlocals(GoalInfo1, NonLocalVars, GoalInfo2),

        %
        % The disjunction is impure/semipure if any of the disjuncts
        % is impure/semipure.
        %
        ( contains_nonpure_goal(GoalList) ->
            list__map(get_purity, GoalList, PurityList),
            Purity = list__foldl(worst_purity, PurityList, (pure)),
            add_goal_info_purity_feature(GoalInfo2, Purity, GoalInfo)
        ;
            GoalInfo2 = GoalInfo
        ),

        Goal = disj(GoalList) - GoalInfo
    ),
    proc_info_set_body(VarSet, VarTypes, HeadVars, Goal, RttiInfo, !Proc).

:- pred contains_nonpure_goal(list(hlds_goal)::in) is semidet.

contains_nonpure_goal([Goal | Goals]) :-
    (
        Goal = _ - GoalInfo,
        \+ goal_info_is_pure(GoalInfo)
    ;
        contains_nonpure_goal(Goals)
    ).

:- func set_arg_names(foreign_arg, prog_varset) = prog_varset.

set_arg_names(foreign_arg(Arg, MaybeNameMode, _), Vars0) = Vars :-
    (
        MaybeNameMode = yes(Name - _),
        varset__name_var(Vars0, Arg, Name, Vars)
    ;
        MaybeNameMode = no,
        Vars = Vars0
    ).

:- pred get_purity(hlds_goal::in, purity::out) is det.

get_purity(_Goal - GoalInfo, Purity) :-
    infer_goal_info_purity(GoalInfo, Purity).

:- pred select_matching_clauses(list(clause)::in, proc_id::in,
    list(clause)::out) is det.

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
    Clause = clause(ProcIds, _, _, _),
    % an empty list here means that the clause applies to all procs
    ( ProcIds = [] ->
        MatchingClauses = [Clause | MatchingClauses1]
    ; list__member(ProcId, ProcIds) ->
        MatchingClauses = [Clause | MatchingClauses1]
    ;
        MatchingClauses = MatchingClauses1
    ),
    select_matching_clauses(Clauses, ProcId, MatchingClauses1).

:- pred get_clause_goals(list(clause)::in, list(hlds_goal)::out) is det.

get_clause_goals([], []).
get_clause_goals([Clause | Clauses], Goals) :-
    get_clause_goals(Clauses, Goals1),
    Clause = clause(_, Goal, _, _),
    goal_to_disj_list(Goal, GoalList),
    list__append(GoalList, Goals1, Goals).
