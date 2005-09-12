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

    % After copying the clauses to the procs, we need to transform the
    % procedures to introduce any required exists_casts..
    %
:- pred introduce_exists_casts(list(pred_id)::in, module_info::in,
    module_info::out) is det.

    % This version is used by polymorphism.m.
    %
:- pred introduce_exists_casts_proc(module_info::in, pred_info::in,
	proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module check_hlds__purity.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__make_hlds.
:- import_module libs__globals.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
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
        do_copy_clauses_to_procs(PredInfo0)
    ->
        copy_clauses_to_procs(PredInfo0, PredInfo),
        map__det_update(!.PredTable, PredId, PredInfo, !:PredTable)
    ;
        true
    ).

:- pred do_copy_clauses_to_procs(pred_info::in) is semidet.

do_copy_clauses_to_procs(PredInfo) :-
    % Don't process typeclass methods, because their proc_infos
    % are generated already mode-correct.
    pred_info_get_markers(PredInfo, PredMarkers),
    \+ check_marker(PredMarkers, class_method).

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
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),

        %
        % The non-local vars are just the head variables.
        %
        set__list_to_set(HeadVars, NonLocalVars),
        goal_info_set_nonlocals(NonLocalVars, GoalInfo1, GoalInfo2),

        %
        % The disjunction is impure/semipure if any of the disjuncts
        % is impure/semipure.
        %
        ( contains_nonpure_goal(GoalList) ->
            list__map(get_purity, GoalList, PurityList),
            Purity = list__foldl(worst_purity, PurityList, (pure)),
            add_goal_info_purity_feature(Purity, GoalInfo2, GoalInfo)
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

%-----------------------------------------------------------------------------%

introduce_exists_casts(PredIds, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    list__foldl(introduce_exists_casts_pred(!.ModuleInfo), PredIds,
        PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

:- pred introduce_exists_casts_pred(module_info::in, pred_id::in,
    pred_table::in, pred_table::out) is det.

introduce_exists_casts_pred(ModuleInfo, PredId, !PredTable) :-
    map__lookup(!.PredTable, PredId, PredInfo0),
    (
        % Optimise the common case.
        pred_info_get_existq_tvar_binding(PredInfo0, Subn),
        \+ map__is_empty(Subn),

        % Only process preds for which we copied clauses to procs.
        do_copy_clauses_to_procs(PredInfo0)
    ->
        pred_info_procedures(PredInfo0, Procs0),
        ProcIds = pred_info_all_non_imported_procids(PredInfo0),
        introduce_exists_casts_procs(ModuleInfo, PredInfo0, ProcIds,
            Procs0, Procs),
        pred_info_set_procedures(Procs, PredInfo0, PredInfo),
        svmap__det_update(PredId, PredInfo, !PredTable)
    ;
        true
    ).

:- pred introduce_exists_casts_procs(module_info::in, pred_info::in,
    list(proc_id)::in, proc_table::in, proc_table::out) is det.

introduce_exists_casts_procs(_, _, [], !Procs).
introduce_exists_casts_procs(ModuleInfo, PredInfo, [ProcId | ProcIds],
        !Procs) :-
    map__lookup(!.Procs, ProcId, ProcInfo0),
    introduce_exists_casts_proc(ModuleInfo, PredInfo, ProcInfo0, ProcInfo),
    svmap__det_update(ProcId, ProcInfo, !Procs),
    introduce_exists_casts_procs(ModuleInfo, PredInfo, ProcIds, !Procs).

introduce_exists_casts_proc(ModuleInfo, PredInfo, !ProcInfo) :-
    pred_info_arg_types(PredInfo, ArgTypes),
    pred_info_get_existq_tvar_binding(PredInfo, Subn),
    OrigArity = pred_info_orig_arity(PredInfo),
    NumExtraHeadVars = list__length(ArgTypes) - OrigArity,

    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_headvars(!.ProcInfo, HeadVars0),
    proc_info_goal(!.ProcInfo, Body0),
    proc_info_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    proc_info_argmodes(!.ProcInfo, ArgModes),

    (
        list__split_list(NumExtraHeadVars, ArgTypes, ExtraArgTypes0,
            OrigArgTypes0),
        list__split_list(NumExtraHeadVars, HeadVars0, ExtraHeadVars0,
            OrigHeadVars0),
        list__split_list(NumExtraHeadVars, ArgModes, ExtraArgModes0,
            OrigArgModes0)
    ->
        ExtraArgTypes = ExtraArgTypes0,
        OrigArgTypes = OrigArgTypes0,
        ExtraHeadVars1 = ExtraHeadVars0,
        OrigHeadVars1 = OrigHeadVars0,
        ExtraArgModes = ExtraArgModes0,
        OrigArgModes = OrigArgModes0
    ;
        unexpected(this_file, "introduce_exists_casts_proc: split_list failed")
    ),

    % Add exists_casts for any head vars which are existentially typed,
    % and for which the type is statically bound inside the procedure.  Subn
    % represents which existential types are bound.
    introduce_exists_casts_for_head(ModuleInfo, Subn, OrigArgTypes,
        OrigArgModes, OrigHeadVars1, OrigHeadVars, VarSet0, VarSet1,
        VarTypes0, VarTypes1, [], ExistsCastHeadGoals),

    % Add exists_casts for any existential type_infos or typeclass_infos.
    % We determine which of these are existential by looking at the mode.
    %
    % Currently we pass in PredTypesMap so that the external type of type_infos
    % and typeclass_infos can be looked up.  When the arguments of these two
    % types are removed, we will no longer need to do this.
    %
    map__from_corresponding_lists(ExtraHeadVars1, ExtraArgTypes,
        ExternalTypes),
    introduce_exists_casts_extra(ModuleInfo, ExternalTypes, Subn,
        ExtraArgModes, ExtraHeadVars1, ExtraHeadVars, VarSet1, VarSet,
        VarTypes1, VarTypes, RttiVarMaps0, RttiVarMaps, ExistsCastExtraGoals),

    Body0 = _ - GoalInfo0,
    goal_to_conj_list(Body0, Goals0),
    Goals = Goals0 ++ ExistsCastHeadGoals ++ ExistsCastExtraGoals,
    HeadVars = ExtraHeadVars ++ OrigHeadVars,
    set__list_to_set(HeadVars, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    Body = conj(Goals) - GoalInfo,
    proc_info_set_body(VarSet, VarTypes, HeadVars, Body, RttiVarMaps,
        !ProcInfo).

:- pred introduce_exists_casts_for_head(module_info::in, tsubst::in,
    list(type)::in, list(mode)::in, list(prog_var)::in,
    list(prog_var)::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(hlds_goal)::in, list(hlds_goal)::out)
    is det.

introduce_exists_casts_for_head(ModuleInfo, Subn, ArgTypes, ArgModes,
        !HeadVars, !VarSet, !VarTypes, !ExtraGoals) :-
    (
        ArgTypes = [],
        ArgModes = [],
        !.HeadVars = []
    ->
        true
    ;
        ArgTypes = [ArgType | ArgTypesRest],
        ArgModes = [ArgMode | ArgModesRest],
        !.HeadVars = [HeadVar0 | HeadVarsRest0]
    ->
        introduce_exists_casts_for_head(ModuleInfo, Subn, ArgTypesRest,
            ArgModesRest, HeadVarsRest0, HeadVarsRest, !VarSet, !VarTypes,
            !ExtraGoals),
        introduce_exists_casts_for_arg(ModuleInfo, Subn, ArgType, ArgMode,
            HeadVar0, HeadVar, !VarSet, !VarTypes, !ExtraGoals),
        !:HeadVars = [HeadVar | HeadVarsRest]
    ;
        unexpected(this_file, "introduce_exists_casts_for_head: " ++
            "length mismatch")
    ).

:- pred introduce_exists_casts_for_arg(module_info::in, tsubst::in,
    (type)::in, (mode)::in, prog_var::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

introduce_exists_casts_for_arg(ModuleInfo, Subn, ExternalType, ArgMode,
        HeadVar0, HeadVar, !VarSet, !VarTypes, !ExtraGoals) :-
    apply_rec_subst_to_type(Subn, ExternalType, InternalType),
    (
        % Add an exists_cast for the head variable if its type
        % inside the procedure is different from its type at the
        % interface.
        InternalType \= ExternalType
    ->
        term__context_init(Context),
        svmap__det_update(HeadVar0, InternalType, !VarTypes),
        make_new_exist_cast_var(HeadVar0, HeadVar, !VarSet),
        svmap__det_insert(HeadVar, ExternalType, !VarTypes),
        mode_get_insts(ModuleInfo, ArgMode, _, Inst),
        generate_cast(exists_cast, HeadVar0, HeadVar, Inst, Inst, Context,
            ExtraGoal),
        !:ExtraGoals = [ExtraGoal | !.ExtraGoals]
    ;
        HeadVar = HeadVar0
    ).

:- pred introduce_exists_casts_extra(module_info::in, vartypes::in, tsubst::in,
    list(mode)::in, list(prog_var)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in,  rtti_varmaps::out, list(hlds_goal)::out) is det.

introduce_exists_casts_extra(_, _, _, [], [], [], !VarSet, !VarTypes,
    !RttiVarMaps, []).
introduce_exists_casts_extra(_, _, _, [], [_ | _], _, _, _, _, _, _, _, _) :-
    unexpected(this_file, "introduce_exists_casts_extra: length mismatch").
introduce_exists_casts_extra(_, _, _, [_ | _], [], _, _, _, _, _, _, _, _) :-
    unexpected(this_file, "introduce_exists_casts_extra: length mismatch").
introduce_exists_casts_extra(ModuleInfo, ExternalTypes, Subn,
        [ArgMode | ArgModes], [Var0 | Vars0], [Var | Vars], !VarSet, !VarTypes,
        !RttiVarMaps, ExtraGoals) :-
    introduce_exists_casts_extra(ModuleInfo, ExternalTypes, Subn, ArgModes,
        Vars0, Vars, !VarSet, !VarTypes, !RttiVarMaps, ExtraGoals0),

    (
        mode_is_output(ModuleInfo, ArgMode)
    ->
            % Update the type of this variable.  This only needs to be done
            % because type_info/1 and typeclass_info/1 have types in their
            % respective arguments.
            %
        map__lookup(ExternalTypes, Var0, ExternalType),
        apply_rec_subst_to_type(Subn, ExternalType, InternalType),
        svmap__det_update(Var0, InternalType, !VarTypes),

            % Create the exists_cast goal.
            %
        term__context_init(Context),
        make_new_exist_cast_var(Var0, Var, !VarSet),
        svmap__det_insert(Var, ExternalType, !VarTypes),
        generate_cast(exists_cast, Var0, Var, Context, ExtraGoal),
        ExtraGoals = [ExtraGoal | ExtraGoals0],

            % Update the rtti_varmaps.  The old variable needs to have the
            % substitution applied to its type/constraint.  The new variable
            % needs to be associated with the unsubstituted type/constraint.
            %
        rtti_varmaps_var_info(!.RttiVarMaps, Var0, VarInfo),
        (
            VarInfo = type_info_var(TypeInfoType0),
            apply_rec_subst_to_type(Subn, TypeInfoType0, TypeInfoType),
            rtti_set_type_info_type(Var0, TypeInfoType, !RttiVarMaps),
            rtti_det_insert_type_info_type(Var, TypeInfoType0, !RttiVarMaps)
        ;
            VarInfo = typeclass_info_var(Constraint0),
            apply_rec_subst_to_prog_constraint(Subn, Constraint0, Constraint),
            rtti_set_typeclass_info_var(Constraint, Var0, !RttiVarMaps),
            rtti_det_insert_typeclass_info_var(Constraint0, Var, !RttiVarMaps)
        ;
            VarInfo = non_rtti_var,
            unexpected(this_file, "introduce_exists_casts_extra: " ++
                "rtti_varmaps info not found")
        )
    ;
        Var = Var0,
        ExtraGoals = ExtraGoals0
    ).

:- pred make_new_exist_cast_var(prog_var::in, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

make_new_exist_cast_var(InternalVar, ExternalVar, !VarSet) :-
    svvarset__new_var(ExternalVar, !VarSet),
    varset__lookup_name(!.VarSet, InternalVar, InternalName),
    string__append("ExistQ", InternalName, ExternalName),
    svvarset__name_var(ExternalVar, ExternalName, !VarSet).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "clause_to_proc.m".

%-----------------------------------------------------------------------------%
:- end_module check_hlds.clause_to_proc.
%-----------------------------------------------------------------------------%
