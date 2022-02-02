%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module check_hlds.introduce_exists_casts.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%-----------------------------------------------------------------------------%

    % After copying the clauses to the procs, we need to transform the
    % procedures to introduce any required exists_casts.
    % XXX Replace the above with *proper* documentation.
    %
    % This version is used by modes.m.
    %
:- pred introduce_exists_casts(list(pred_id)::in,
    module_info::in, module_info::out) is det.

    % This version is used by polymorphism.m.
    %
:- pred introduce_exists_casts_poly(pred_id::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

introduce_exists_casts(PredIds, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    list.foldl(maybe_introduce_exists_casts_pred(!.ModuleInfo), PredIds,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred maybe_introduce_exists_casts_pred(module_info::in, pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

maybe_introduce_exists_casts_pred(ModuleInfo, PredId, !PredTable) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    ( if
        % Optimise the common case: predicates with no existentially typed
        % variables.
        pred_info_get_existq_tvar_binding(PredInfo0, Subn),
        not map.is_empty(Subn),

        % Only process preds for which we copied clauses to procs.
        should_copy_clauses_to_procs(PredInfo0)
    then
        pred_info_get_proc_table(PredInfo0, Procs0),
        ProcIds = pred_info_all_non_imported_procids(PredInfo0),
        introduce_exists_casts_procs(ModuleInfo, PredInfo0, ProcIds,
            Procs0, Procs),
        pred_info_set_proc_table(Procs, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredTable)
    else
        true
    ).

:- pred introduce_exists_casts_procs(module_info::in, pred_info::in,
    list(proc_id)::in, proc_table::in, proc_table::out) is det.

introduce_exists_casts_procs(_, _, [], !Procs).
introduce_exists_casts_procs(ModuleInfo, PredInfo, [ProcId | ProcIds],
        !Procs) :-
    map.lookup(!.Procs, ProcId, ProcInfo0),
    introduce_exists_casts_proc(ModuleInfo, PredInfo, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !Procs),
    introduce_exists_casts_procs(ModuleInfo, PredInfo, ProcIds, !Procs).

%-----------------------------------------------------------------------------%

introduce_exists_casts_poly(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcMap0),
    map.map_values_only(introduce_exists_casts_proc(!.ModuleInfo, PredInfo0),
        ProcMap0, ProcMap),
    pred_info_set_proc_table(ProcMap, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred introduce_exists_casts_proc(module_info::in, pred_info::in,
    proc_info::in, proc_info::out) is det.

introduce_exists_casts_proc(ModuleInfo, PredInfo, !ProcInfo) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_existq_tvar_binding(PredInfo, Subn),
    pred_info_get_class_context(PredInfo, PredConstraints),
    OrigArity = pred_info_orig_arity(PredInfo),
    NumExtraHeadVars = list.length(ArgTypes) - OrigArity,

    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_headvars(!.ProcInfo, HeadVars0),
    proc_info_get_goal(!.ProcInfo, Body0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),

    ( if
        list.drop(NumExtraHeadVars, ArgTypes, OrigArgTypes0),
        list.split_list(NumExtraHeadVars, HeadVars0, ExtraHeadVars0,
            OrigHeadVars0),
        list.split_list(NumExtraHeadVars, ArgModes, ExtraArgModes0,
            OrigArgModes0)
    then
        OrigArgTypes = OrigArgTypes0,
        ExtraHeadVars1 = ExtraHeadVars0,
        OrigHeadVars1 = OrigHeadVars0,
        ExtraArgModes = ExtraArgModes0,
        OrigArgModes = OrigArgModes0
    else
        unexpected($pred, "split_list failed")
    ),

    % Add exists_casts for any head vars which are existentially typed,
    % and for which the type is statically bound inside the procedure.
    % Subn represents which existential types are bound.
    introduce_exists_casts_for_head(ModuleInfo, Subn, OrigArgTypes,
        OrigArgModes, OrigHeadVars1, OrigHeadVars, VarSet0, VarSet1,
        VarTypes0, VarTypes1, [], ExistsCastHeadGoals),

    % Add exists_casts for any existential type_infos or typeclass_infos.
    % We determine which of these are existential by looking at the mode.
    %
    ExistConstraints = PredConstraints ^ exist_constraints,
    assoc_list.from_corresponding_lists(ExtraArgModes, ExtraHeadVars1,
        ExtraModesAndVars),
    introduce_exists_casts_extra(ModuleInfo, Subn, ExistConstraints,
        ExtraModesAndVars, ExtraHeadVars, VarSet1, VarSet, VarTypes1, VarTypes,
        RttiVarMaps0, RttiVarMaps, [], ExistsCastExtraGoals),

    Body0 = hlds_goal(_, GoalInfo0),
    goal_to_conj_list(Body0, Goals0),
    Goals = Goals0 ++ ExistsCastHeadGoals ++ ExistsCastExtraGoals,
    HeadVars = ExtraHeadVars ++ OrigHeadVars,
    NonLocals = set_of_var.list_to_set(HeadVars),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    Body = hlds_goal(conj(plain_conj, Goals), GoalInfo),
    proc_info_set_body(VarSet, VarTypes, HeadVars, Body, RttiVarMaps,
        !ProcInfo).

:- pred introduce_exists_casts_for_head(module_info::in, tsubst::in,
    list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
    list(prog_var)::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(hlds_goal)::in, list(hlds_goal)::out)
    is det.

introduce_exists_casts_for_head(ModuleInfo, Subn, ArgTypes, ArgModes,
        !HeadVars, !VarSet, !VarTypes, !ExtraGoals) :-
    ( if
        ArgTypes = [],
        ArgModes = [],
        !.HeadVars = []
    then
        true
    else if
        ArgTypes = [ArgType | ArgTypesRest],
        ArgModes = [ArgMode | ArgModesRest],
        !.HeadVars = [HeadVar0 | HeadVarsRest0]
    then
        introduce_exists_casts_for_head(ModuleInfo, Subn, ArgTypesRest,
            ArgModesRest, HeadVarsRest0, HeadVarsRest, !VarSet, !VarTypes,
            !ExtraGoals),
        introduce_exists_casts_for_arg(ModuleInfo, Subn, ArgType, ArgMode,
            HeadVar0, HeadVar, !VarSet, !VarTypes, !ExtraGoals),
        !:HeadVars = [HeadVar | HeadVarsRest]
    else
        unexpected($pred, "length mismatch")
    ).

:- pred introduce_exists_casts_for_arg(module_info::in, tsubst::in,
    mer_type::in, mer_mode::in, prog_var::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

introduce_exists_casts_for_arg(ModuleInfo, Subn, ExternalType, ArgMode,
        HeadVar0, HeadVar, !VarSet, !VarTypes, !ExtraGoals) :-
    apply_rec_subst_to_type(Subn, ExternalType, InternalType),
    % Add an exists_cast for the head variable if its type
    % inside the procedure is different from its type at the interface.
    ( if InternalType = ExternalType then
        HeadVar = HeadVar0
    else
        term.context_init(Context),
        update_var_type(HeadVar0, InternalType, !VarTypes),
        make_new_exist_cast_var(HeadVar0, HeadVar, !VarSet),
        add_var_type(HeadVar, ExternalType, !VarTypes),
        mode_get_insts(ModuleInfo, ArgMode, _, Inst),
        generate_cast_with_insts(exists_cast, HeadVar0, HeadVar, Inst, Inst,
            Context, ExtraGoal),
        !:ExtraGoals = [ExtraGoal | !.ExtraGoals]
    ).

:- pred introduce_exists_casts_extra(module_info::in, tsubst::in,
    list(prog_constraint)::in, assoc_list(mer_mode, prog_var)::in,
    list(prog_var)::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in,  rtti_varmaps::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

introduce_exists_casts_extra(_, _, ExistConstraints, [], [], !VarSet,
        !VarTypes, !RttiVarMaps, !ExtraGoals) :-
    (
        ExistConstraints = []
    ;
        ExistConstraints = [_ | _],
        unexpected($pred, "length mismatch")
    ).

introduce_exists_casts_extra(ModuleInfo, Subn, ExistConstraints0,
        [ModeAndVar | ModesAndVars], [Var | Vars], !VarSet, !VarTypes,
        !RttiVarMaps, !ExtraGoals) :-
    ModeAndVar = ArgMode - Var0,
    ( if mode_is_output(ModuleInfo, ArgMode) then
        % Create the exists_cast goal.

        term.context_init(Context),
        make_new_exist_cast_var(Var0, Var, !VarSet),
        lookup_var_type(!.VarTypes, Var0, VarType),
        add_var_type(Var, VarType, !VarTypes),
        generate_cast(exists_cast, Var0, Var, Context, ExtraGoal),
        !:ExtraGoals = [ExtraGoal | !.ExtraGoals],

        % Update the rtti_varmaps. The old variable needs to have the
        % substitution applied to its type/constraint. The new variable
        % needs to be associated with the unsubstituted type/constraint.

        rtti_varmaps_var_info(!.RttiVarMaps, Var0, VarInfo),
        (
            VarInfo = type_info_var(TypeInfoType0),
            % For type_infos, the old variable needs to have the substitution
            % applied to its type, and the new variable needs to be associated
            % with the unsubstituted type.
            apply_rec_subst_to_type(Subn, TypeInfoType0, TypeInfoType),
            rtti_set_type_info_type(Var0, TypeInfoType, !RttiVarMaps),
            rtti_det_insert_type_info_type(Var, TypeInfoType0, !RttiVarMaps),
            ExistConstraints = ExistConstraints0
        ;
            VarInfo = typeclass_info_var(_),
            % For typeclass_infos, the constraint associated with the old
            % variable was derived from the constraint map, so all binding
            % and improvement has been applied. The new variable needs to
            % be associated with the corresponding existential head constraint,
            % so we pop one off the front of the list.
            (
                ExistConstraints0 = [ExistConstraint | ExistConstraints]
            ;
                ExistConstraints0 = [],
                unexpected($pred, "missing constraint")
            ),
            rtti_det_insert_typeclass_info_var(ExistConstraint, Var,
                !RttiVarMaps),
            % We also need to ensure that all type variables in the constraint
            % have a location recorded, so we insert a location now if there
            % is not already one.
            ExistConstraint = constraint(_, ConstraintArgs),
            maybe_add_type_info_locns(ConstraintArgs, Var, 1, !RttiVarMaps)
        ;
            VarInfo = non_rtti_var,
            unexpected($pred, "rtti_varmaps info not found")
        )
    else
        Var = Var0,
        ExistConstraints = ExistConstraints0
    ),
    introduce_exists_casts_extra(ModuleInfo, Subn, ExistConstraints,
        ModesAndVars, Vars, !VarSet, !VarTypes, !RttiVarMaps, !ExtraGoals).

:- pred maybe_add_type_info_locns(list(mer_type)::in, prog_var::in, int::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_add_type_info_locns([], _, _, !RttiVarMaps).
maybe_add_type_info_locns([ArgType | ArgTypes], Var, Num, !RttiVarMaps) :-
    ( if
        ArgType = type_variable(TVar, _),
        not rtti_search_type_info_locn(!.RttiVarMaps, TVar, _)
    then
        Locn = typeclass_info(Var, Num),
        rtti_det_insert_type_info_locn(TVar, Locn, !RttiVarMaps)
    else
        true
    ),
    maybe_add_type_info_locns(ArgTypes, Var, Num + 1, !RttiVarMaps).

:- pred make_new_exist_cast_var(prog_var::in, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

make_new_exist_cast_var(InternalVar, ExternalVar, !VarSet) :-
    varset.new_var(ExternalVar, !VarSet),
    varset.lookup_name(!.VarSet, InternalVar, InternalName),
    string.append("ExistQ", InternalName, ExternalName),
    varset.name_var(ExternalVar, ExternalName, !VarSet).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.introduce_exists_casts.
%-----------------------------------------------------------------------------%
