%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_type_class_info.
:- interface.

:- import_module check_hlds.polymorphism_info.
:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.

    % Given the list of constraints for a called predicate, create a list of
    % variables to hold the typeclass_info for those constraints, and create
    % a list of goals to initialize those typeclass_info variables to the
    % appropriate typeclass_info structures for the constraints.
    %
    % Constraints should be renamed-apart and actual-to-formal substituted
    % constraints. Constraints which are already in the rtti_varmaps are
    % assumed to have already had their typeclass_infos initialized; for them,
    % we just return the variable in the rtti_varmaps.
    %
:- pred make_typeclass_info_vars(list(prog_constraint)::in,
    existq_tvars::in, prog_context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

    % Produce the typeclass_infos for the existential class constraints
    % for a call or deconstruction unification.
    %
:- pred make_existq_typeclass_info_vars(list(prog_constraint)::in,
    prog_context::in, list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

    % Usually when we call make_typeclass_info_head_var, we want to record
    % the type_info_locn for each constrained type var so that later goals
    % will know where to get the type_info from. However, when setting up
    % head vars for existential constraints on the predicate/function we
    % are processing, we assume that the type_infos will be produced
    % somewhere else in the goal. In this case, we don't want to record
    % the type_info_locns (if we did, then the code to actually produce the
    % type_info will just try to get it from here, which would be a mode
    % error).
    %
:- type record_type_info_locns
    --->    do_record_type_info_locns
    ;       do_not_record_type_info_locns.

    % Create a head var for each class constraint.
    %
:- pred make_typeclass_info_head_vars(record_type_info_locns::in,
    list(prog_constraint)::in, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

make_typeclass_info_vars(Constraints, ExistQVars, Context,
        TypeClassInfoVarsMCAs, ExtraGoals, !Info) :-
    SeenInstances = [],
    make_typeclass_info_vars_loop(Constraints, SeenInstances, ExistQVars,
        Context, TypeClassInfoVarsMCAs, ExtraGoals, !Info).

    % Accumulator version of the above.
    %
:- pred make_typeclass_info_vars_loop(list(prog_constraint)::in,
    list(prog_constraint)::in, existq_tvars::in, prog_context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_vars_loop([], _Seen, _ExistQVars,
        _Context, [], [], !Info).
make_typeclass_info_vars_loop([Constraint | Constraints], Seen, ExistQVars,
        Context, [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs], ExtraGoals,
        !Info) :-
    make_typeclass_info_var(Constraint, [Constraint | Seen],
        ExistQVars, Context, TypeClassInfoVarMCA, HeadExtraGoals, !Info),
    make_typeclass_info_vars_loop(Constraints, Seen, ExistQVars,
        Context, TypeClassInfoVarsMCAs, TailExtraGoals, !Info),
    ExtraGoals = HeadExtraGoals ++ TailExtraGoals.

:- pred make_typeclass_info_var(prog_constraint::in,
    list(prog_constraint)::in, existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_var(Constraint, Seen, ExistQVars, Context,
        TypeClassInfoVarMCA, Goals, !Info) :-
    ( if
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_search_typeclass_info_var(RttiVarMaps0, Constraint,
            OldTypeClassInfoVar)
    then
        % We already have a typeclass_info for this constraint, either from
        % a parameter to the pred or from an existentially quantified goal
        % that we have already processed.
        TypeClassInfoVar = OldTypeClassInfoVar,
        TypeClassInfoVarMCA = TypeClassInfoVar - no,
        Goals = []
    else if
        % We don't have the typeclass_info, so we must either have a proof
        % that tells us how to make it, or ...
        poly_info_get_proof_map(!.Info, ProofMap),
        map.search(ProofMap, Constraint, Proof)
    then
        make_typeclass_info_from_proof(Constraint, Seen, Proof, ExistQVars,
            Context, TypeClassInfoVarMCA, Goals, !Info)
    else
        % ... it will be produced by an existentially typed goal that
        % we will process later on.
        make_typeclass_info_head_var(do_record_type_info_locns, Constraint,
            TypeClassInfoVar, !Info),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_reuse_typeclass_info_var(TypeClassInfoVar,
            RttiVarMaps0, RttiVarMaps),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - no,
        Goals = []
    ).

:- pred make_typeclass_info_from_proof(prog_constraint::in,
    list(prog_constraint)::in, constraint_proof::in, existq_tvars::in,
    prog_context::in, pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_proof(Constraint, Seen, Proof,
        ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info) :-
    (
        % XXX MR_Dictionary should have MR_Dictionaries for superclass
        % We have to extract the typeclass_info from another one.
        Proof = superclass(SubClassConstraint),
        make_typeclass_info_from_subclass(Constraint, Seen, SubClassConstraint,
            ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info)
    ;
        % We have to construct the typeclass_info using an instance
        % declaration.
        Proof = apply_instance(InstanceNum),
        make_typeclass_info_from_instance(Constraint, Seen, InstanceNum,
            ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info)
    ).

:- pred make_typeclass_info_from_subclass(prog_constraint::in,
    list(prog_constraint)::in, prog_constraint::in, existq_tvars::in,
    prog_context::in, pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_subclass(Constraint, Seen, SubClassConstraint,
        ExistQVars, Context, TypeClassInfoVar - MaybeTCIConstArg, Goals,
        !Info) :-
    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        poly_info_get_selected_pred(SelectedPred, !IO),
        poly_info_get_indent_level(Level, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_set_indent_level(Level + 1, !IO),

            io.format(Stream,
                "%smake_typeclass_info_from_subclass\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%sConstraint: ", [s(IndentStr)], !IO),
            io.write_line(Stream, Constraint, !IO),
            io.format(Stream, "%sSeen: ", [s(IndentStr)], !IO),
            ( if Seen = [Constraint] then
                io.write_string(Stream, "[Constraint]\n", !IO)
            else
                io.write_line(Stream, Seen, !IO)
            ),
            io.format(Stream, "%sSubClassConstraint: ",
                [s(IndentStr)], !IO),
            io.write_line(Stream, SubClassConstraint, !IO),
            io.format(Stream, "%sExistQVars: ", [s(IndentStr)], !IO),
            io.write_line(Stream, ExistQVars, !IO),
            io.nl(Stream, !IO)
        )
    ),

    % Work out where to extract the typeclass info from.
    SubClassConstraint = constraint(SubClassName, SubClassTypes),
    list.length(SubClassTypes, SubClassArity),
    SubClassId = class_id(SubClassName, SubClassArity),

    % Make the typeclass_info for the subclass.
    make_typeclass_info_var(SubClassConstraint, Seen, ExistQVars, Context,
        SubClassVarMCA, SubClassVarGoals, !Info),
    SubClassVarMCA = SubClassVar - SubClassMCA,

    % Look up the definition of the subclass.
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, SubClassId, SubClassDefn),

    % Work out which superclass typeclass_info to take.
    map.from_corresponding_lists(SubClassDefn ^ classdefn_vars, SubClassTypes,
        SubTypeSubst),
    apply_subst_to_prog_constraint_list(SubTypeSubst,
        SubClassDefn ^ classdefn_supers, SuperClasses),
    ( if
        list.index1_of_first_occurrence(SuperClasses, Constraint,
            SuperClassIndexPrime)
    then
        SuperClassIndex = SuperClassIndexPrime
    else
        % We shouldn't have got this far if the constraints were not satisfied.
        unexpected($pred, "constraint not in constraint list")
    ),

    (
        SubClassMCA = yes(SubClassConstArg),
        (
            SubClassConstArg = csa_constant(_, _),
            unexpected($pred, "typeclass infos need a cell")
        ;
            SubClassConstArg = csa_const_struct(SubClassConstNum),
            poly_info_get_const_struct_db(!.Info, ConstStructDb),
            lookup_const_struct_num(ConstStructDb, SubClassConstNum,
                SubClassConstStruct),
            SubClassConstStruct = const_struct(SubClassConsId, SubClassArgs,
                _, _, _),
            ( if
                SubClassConsId = typeclass_info_cell_constructor,
                SubClassArgs = [BTCIArg | OtherArgs],
                BTCIArg = csa_constant(BTCIConsId, _),
                BTCIConsId = base_typeclass_info_const(_, SubClassId,
                    SubInstanceNum, _),
                module_info_get_instance_table(ModuleInfo, InstanceTable),
                map.lookup(InstanceTable, SubClassId, SubInstanceDefns),
                list.index1(SubInstanceDefns, SubInstanceNum, SubInstanceDefn),
                num_extra_instance_args(SubInstanceDefn, NumExtra),
                Index = NumExtra + SuperClassIndex,
                list.det_index1(OtherArgs, Index, SelectedArg),
                SelectedArg = csa_const_struct(SelectedConstNum)
            then
                materialize_typeclass_info_var(Constraint, SelectedConstNum,
                    TypeClassInfoVar, Goals, !Info),
                MaybeTCIConstArg = yes(SelectedArg)
            else
                unexpected($pred, "unexpected typeclass info structure")
            )
        ),

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            poly_info_get_selected_pred(SelectedPred, !IO),
            poly_info_get_indent_level(Level, !IO),
            poly_info_set_indent_level(Level - 1, !IO),
            (
                SelectedPred = is_not_selected_pred
            ;
                SelectedPred = is_selected_pred,
                poly_info_get_debug_stream(!.Info, Stream, !IO),
                IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                io.format(Stream, "%ssubclass constant result ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream,
                    TypeClassInfoVar - MaybeTCIConstArg, !IO),
                io.nl(Stream, !IO)
            )
        )
    ;
        SubClassMCA = no,
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        get_poly_const(SuperClassIndex, IndexVar, IndexGoals, !Info),

        % We extract the superclass typeclass_info by inserting a call
        % to superclass_from_typeclass_info in private_builtin.
        generate_simple_call(ModuleInfo,
            mercury_private_builtin_module, "superclass_from_typeclass_info",
            pf_predicate, only_mode, detism_det, purity_pure,
            [SubClassVar, IndexVar, TypeClassInfoVar], [],
            instmap_delta_bind_no_var, term.context_init, SuperClassGoal),
        Goals = SubClassVarGoals ++ IndexGoals ++ [SuperClassGoal],
        MaybeTCIConstArg = no,

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            poly_info_get_selected_pred(SelectedPred, !IO),
            poly_info_get_indent_level(Level, !IO),
            poly_info_set_indent_level(Level - 1, !IO),
            (
                SelectedPred = is_not_selected_pred
            ;
                SelectedPred = is_selected_pred,
                poly_info_get_debug_stream(!.Info, Stream, !IO),
                IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                io.format(Stream, "%ssubclass computed result ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream,
                    TypeClassInfoVar - MaybeTCIConstArg, !IO),
                io.nl(Stream, !IO)
            )
        )
    ).

:- pred make_typeclass_info_from_instance(prog_constraint::in,
    list(prog_constraint)::in, int::in, existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_from_instance(Constraint, Seen, InstanceNum, ExistQVars,
        Context, TypeClassInfoVarMCA, Goals, !Info) :-
    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        poly_info_get_selected_pred(SelectedPred, !IO),
        poly_info_get_indent_level(Level, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_set_indent_level(Level + 1, !IO),

            io.format(Stream, "%smake_typeclass_info_from_instance\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%sConstraint: ", [s(IndentStr)], !IO),
            io.write_line(Stream, Constraint, !IO),
            io.format(Stream, "%sSeen: ", [s(IndentStr)], !IO),
            ( if Seen = [Constraint] then
                io.write_string(Stream, "[Constraint]\n", !IO)
            else
                io.write_line(Stream, Seen, !IO)
            ),
            io.format(Stream, "%sInstanceNum: %d\n",
                [s(IndentStr), i(InstanceNum)], !IO),
            io.format(Stream, "%sExistQVars: ", [s(IndentStr)], !IO),
            io.write_line(Stream, ExistQVars, !IO),
            io.nl(Stream, !IO)
        )
    ),

    poly_info_get_const_struct_db(!.Info, ConstStructDb0),
    InstanceId = ciid(InstanceNum, Constraint, Seen),
    ( if
        search_for_constant_instance(ConstStructDb0, InstanceId,
            InstanceIdConstNum)
    then
        materialize_typeclass_info_var(Constraint, InstanceIdConstNum,
            TypeClassInfoVar, Goals, !Info),
        TypeClassInfoVarMCA =
            TypeClassInfoVar - yes(csa_const_struct(InstanceIdConstNum)),

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            poly_info_get_selected_pred(SelectedPred, !IO),
            poly_info_get_indent_level(Level, !IO),
            poly_info_set_indent_level(Level - 1, !IO),
            (
                SelectedPred = is_not_selected_pred
            ;
                SelectedPred = is_selected_pred,
                poly_info_get_debug_stream(!.Info, Stream, !IO),
                IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                (
                    Goals = [],
                    ResultStr = "instance doubly cached result "
                ;
                    Goals = [_ | _],
                    ResultStr = "instance cached result "
                ),

                io.format(Stream, "%s%s",
                    [s(IndentStr), s(ResultStr)], !IO),
                io.write_line(Stream, TypeClassInfoVarMCA, !IO),
                io.nl(Stream, !IO)
            )
        )
    else
        do_make_typeclass_info_from_instance(InstanceId, ExistQVars,
            Context, TypeClassInfoVarMCA, Goals, !Info),
        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            poly_info_get_selected_pred(SelectedPred, !IO),
            poly_info_get_indent_level(Level, !IO),
            poly_info_set_indent_level(Level - 1, !IO),
            (
                SelectedPred = is_not_selected_pred
            ;
                SelectedPred = is_selected_pred,
                poly_info_get_debug_stream(!.Info, Stream, !IO),
                IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                io.format(Stream, "%sinstance computed result: ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream, TypeClassInfoVarMCA, !IO),

                poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap),
                poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap),
                poly_info_get_const_struct_var_map(!.Info,
                    ConstStructVarMap),

                io.format(Stream, "%stype_info_var_map: ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream, TypeInfoVarMap, !IO),
                io.format(Stream, "%stypeclass_info_map: ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream, TypeClassInfoMap, !IO),
                io.format(Stream, "%sstricy_var_map: ",
                    [s(IndentStr)], !IO),
                io.write_line(Stream, ConstStructVarMap, !IO),
                io.nl(Stream, !IO)
            )
        )
    ).

:- pred do_make_typeclass_info_from_instance(const_instance_id::in,
    existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

do_make_typeclass_info_from_instance(InstanceId, ExistQVars, Context,
        TypeClassInfoVarMCA, Goals, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    module_info_get_class_table(ModuleInfo, ClassTable),
    poly_info_get_typevarset(!.Info, TypeVarSet),
    poly_info_get_proof_map(!.Info, ProofMap0),

    InstanceId = ciid(InstanceNum, Constraint, Seen),
    Constraint = constraint(ClassName, ConstrainedTypes),

    list.length(ConstrainedTypes, ClassArity),
    ClassId = class_id(ClassName, ClassArity),

    map.lookup(InstanceTable, ClassId, InstanceList),
    list.det_index1(InstanceList, InstanceNum, ProofInstanceDefn),

    ProofInstanceDefn = hlds_instance_defn(_, InstanceTypes, _, _, _,
        InstanceConstraints, _, _, InstanceTVarset, InstanceProofMap),

    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(KindMap),

    type_vars_list(InstanceTypes, InstanceTvars),
    get_unconstrained_tvars(InstanceTvars, InstanceConstraints,
        UnconstrainedTvars),

    % We can ignore the new typevarset because all the type variables
    % in the instance constraints and superclass proofs must appear
    % in the arguments of the instance, and all such variables
    % are bound when we call type_list_subsumes then apply
    % the resulting bindings.
    tvarset_merge_renaming(TypeVarSet, InstanceTVarset, _NewTVarset, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes,
        RenamedInstanceTypes),
    type_list_subsumes_det(RenamedInstanceTypes, ConstrainedTypes,
        InstanceSubst),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        InstanceConstraints, RenamedInstanceConstraints),
    apply_rec_subst_to_prog_constraint_list(InstanceSubst,
        RenamedInstanceConstraints, ActualInstanceConstraints0),
    % XXX document diamond as guess
    % XXX does anyone know what the preceding line means?
    list.delete_elems(ActualInstanceConstraints0, Seen,
        ActualInstanceConstraints),
    apply_variable_renaming_to_constraint_proof_map(Renaming,
        InstanceProofMap, RenamedInstanceProofMap),
    apply_rec_subst_to_constraint_proof_map(InstanceSubst,
        RenamedInstanceProofMap, ActualInstanceProofMap),

    apply_variable_renaming_to_tvar_list(Renaming, UnconstrainedTvars,
        RenamedUnconstrainedTvars),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap,
        RenamedKindMap),
    apply_rec_subst_to_tvar_list(RenamedKindMap, InstanceSubst,
        RenamedUnconstrainedTvars, ActualUnconstrainedTypes),

    map.overlay(ProofMap0, ActualInstanceProofMap, ProofMap),

    get_var_maps_snapshot("make_typeclass_info_from_instance",
        InitialVarMapsSnapshot, !Info),

    % Make the type_infos for the types that are constrained by this.
    % These are packaged in the typeclass_info.
    polymorphism_do_make_type_info_vars(ConstrainedTypes, Context,
        ArgTypeInfoVarsMCAs, TypeInfoGoals, !Info),

    % Make the typeclass_infos for the constraints from the context of the
    % instance decl.
    make_typeclass_info_vars_loop(ActualInstanceConstraints, Seen, ExistQVars,
        Context, ArgTypeClassInfoVarsMCAs, InstanceConstraintGoals, !Info),

    % Make the type_infos for the unconstrained type variables
    % from the head of the instance declaration.
    polymorphism_do_make_type_info_vars(ActualUnconstrainedTypes, Context,
        ArgUnconstrainedTypeInfoVarsMCAs, UnconstrainedTypeInfoGoals, !Info),

    % --------------------- %

    map.lookup(ClassTable, ClassId, ClassDefn),

    get_arg_superclass_vars(ClassDefn, ConstrainedTypes, ProofMap,
        ExistQVars, ArgSuperClassVarsMCAs, SuperClassGoals, !Info),

    PrevGoals = UnconstrainedTypeInfoGoals ++ TypeInfoGoals ++
        InstanceConstraintGoals ++ SuperClassGoals,
    % Lay out the argument variables as expected in the typeclass_info.
    ArgVarsMCAs = ArgUnconstrainedTypeInfoVarsMCAs ++
        ArgTypeClassInfoVarsMCAs ++
        ArgSuperClassVarsMCAs ++ ArgTypeInfoVarsMCAs,
    list.map(make_const_or_var_arg, ArgVarsMCAs, ArgCOVAs),

    Constraint = constraint(ConstraintClassName, ConstraintArgTypes),
    poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap0),
    ( if
        map.search(TypeClassInfoMap0, ConstraintClassName, ClassNameMap0),
        map.search(ClassNameMap0, ConstraintArgTypes, OldEntry0),
        OldEntry0 = typeclass_info_map_entry(_BaseConsId, ArgsMap0),
        map.search(ArgsMap0, ArgCOVAs, OldTypeClassInfoVarMCA0)
    then
        TypeClassInfoVarMCA = OldTypeClassInfoVarMCA0,
        Goals = [],
        set_var_maps_snapshot("make_typeclass_info",
            InitialVarMapsSnapshot, !Info),
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 2, !Info)
    else
        BaseConsId = base_typeclass_info_cons_id(InstanceTable,
            Constraint, InstanceNum, InstanceTypes),
        materialize_base_typeclass_info_var(Constraint, BaseConsId, BaseVar,
            BaseGoals, !Info),
        construct_typeclass_info(Constraint, BaseVar, BaseConsId, ArgVarsMCAs,
            InitialVarMapsSnapshot, TypeClassInfoVar, TypeClassInfoMCA,
            BaseGoals ++ PrevGoals, Goals, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - TypeClassInfoMCA,

        % We must start the search from scratch, since construct_typeclass_info
        % may have reset all the cache maps.
        poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap1),
        ( if
            map.search(TypeClassInfoMap1, ConstraintClassName, ClassNameMap1)
        then
            ( if map.search(ClassNameMap1, ConstraintArgTypes, OldEntry1) then
                OldEntry1 = typeclass_info_map_entry(BaseConsId1, ArgsMap1),
                expect(unify(BaseConsId1, BaseConsId), $pred,
                    "BaseConsId1 != BaseConsId"),
                map.det_insert(ArgCOVAs, TypeClassInfoVarMCA,
                    ArgsMap1, ArgsMap),
                Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
                map.det_update(ConstraintArgTypes, Entry,
                    ClassNameMap1, ClassNameMap),
                map.det_update(ConstraintClassName, ClassNameMap,
                    TypeClassInfoMap1, TypeClassInfoMap)
            else
                ArgsMap = map.singleton(ArgCOVAs, TypeClassInfoVarMCA),
                Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
                map.det_insert(ConstraintArgTypes, Entry,
                    ClassNameMap1, ClassNameMap),
                map.det_update(ConstraintClassName, ClassNameMap,
                    TypeClassInfoMap1, TypeClassInfoMap)
            )
        else
            ArgsMap = map.singleton(ArgCOVAs, TypeClassInfoVarMCA),
            Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
            ClassNameMap = map.singleton(ConstraintArgTypes, Entry),
            map.det_insert(ConstraintClassName, ClassNameMap,
                TypeClassInfoMap1, TypeClassInfoMap)
        ),
        poly_info_set_typeclass_info_map(TypeClassInfoMap, !Info)
    ),

    ( if
        TypeClassInfoVarMCA = _ - yes(TypeClassInfoConstArg),
        TypeClassInfoConstArg = csa_const_struct(TypeClassInfoConstArgNum)
    then
        poly_info_get_const_struct_db(!.Info, ConstStructDb1),
        insert_constant_instance(InstanceId, TypeClassInfoConstArgNum,
            ConstStructDb1, ConstStructDb),
        poly_info_set_const_struct_db(ConstStructDb, !Info)
    else
        true
    ).

:- pred make_const_or_var_arg(pair(prog_var, maybe(const_struct_arg))::in,
    const_or_var_arg::out) is det.

make_const_or_var_arg(Var - MCA, ConstOrVarArg) :-
    (
        MCA = no,
        ConstOrVarArg = cova_var(Var)
    ;
        MCA = yes(ConstArg),
        ConstOrVarArg = cova_const(ConstArg)
    ).

:- pred construct_typeclass_info(prog_constraint::in,
    prog_var::in, cons_id::in,
    assoc_list(prog_var, maybe(const_struct_arg))::in, var_maps::in,
    prog_var::out, maybe(const_struct_arg)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

construct_typeclass_info(Constraint, BaseVar, BaseConsId, ArgVarsMCAs,
        InitialVarMapsSnapshot, TypeClassInfoVar, TypeClassInfoMCA,
        PrevGoals, AllGoals, !Info) :-
    % Build a unification to add the argvars to the base_typeclass_info.
    ConsId = typeclass_info_cell_constructor,

    poly_info_get_const_struct_db(!.Info, ConstStructDb0),
    const_struct_db_get_poly_enabled(ConstStructDb0, ConstStructEnabled),
    ( if
        ConstStructEnabled = enable_const_struct_poly,
        all_are_const_struct_args(ArgVarsMCAs, VarConstArgs)
    then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),

        set_var_maps_snapshot("construct_typeclass_info",
            InitialVarMapsSnapshot, !Info),
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, TypeClassInfoVarType, !Info),

        BaseConstArg = csa_constant(BaseConsId, typeclass_info_type),
        StructArgs = [BaseConstArg | VarConstArgs],
        list.map(get_inst_of_const_struct_arg(ConstStructDb0),
            VarConstArgs, VarInsts),
        list.length(ArgVarsMCAs, NumArgs),
        InstConsId = cell_inst_cons_id(typeclass_info_cell, NumArgs),
        StructInst = bound(shared, inst_test_results_fgtc,
            [bound_functor(InstConsId, VarInsts)]),
        poly_info_get_defined_where(!.Info, DefinedWhere),
        ConstStruct = const_struct(ConsId, StructArgs,
            TypeClassInfoVarType, StructInst, DefinedWhere),
        lookup_insert_const_struct(ConstStruct, ConstNum,
            ConstStructDb0, ConstStructDb),
        poly_info_set_const_struct_db(ConstStructDb, !Info),
        TypeClassInfoConstArg = csa_const_struct(ConstNum),
        TypeClassInfoMCA = yes(TypeClassInfoConstArg),

        % Create the construction unification to initialize the variable.
        ConstConsId = typeclass_info_const(ConstNum),
        Unification = construct(TypeClassInfoVar, ConstConsId, [], [],
            construct_statically(born_static), cell_is_shared,
            no_construct_sub_info),
        Ground = ground(shared, none_or_default_func),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        % XXX The UnifyContext is wrong.
        UnifyContext = unify_context(umc_explicit, []),
        TypeClassInfoRHS = rhs_functor(ConstConsId, is_not_exist_constr, []),
        GoalExpr = unify(TypeClassInfoVar, TypeClassInfoRHS, UnifyMode,
            Unification, UnifyContext),

        % Create a goal_info for the unification.
        goal_info_init(GoalInfo0),
        NonLocals = set_of_var.make_singleton(TypeClassInfoVar),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
        % Note that we could perhaps be more accurate than `ground(shared)',
        % but it shouldn't make any difference.
        TypeClassInfoInst = bound(shared, inst_test_results_fgtc,
            [bound_functor(ConsId, [])]),
        TypeClassInfoVarInst = TypeClassInfoVar - TypeClassInfoInst,
        InstMapDelta = instmap_delta_from_assoc_list([TypeClassInfoVarInst]),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo1, GoalInfo2),
        goal_info_set_determinism(detism_det, GoalInfo2, GoalInfo),

        Goal = hlds_goal(GoalExpr, GoalInfo),
        % XXX reset varset and vartypes
        AllGoals = [Goal]
    else
        TypeClassInfoMCA = no,
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        assoc_list.keys(ArgVarsMCAs, ArgVars),
        AllArgVars = [BaseVar | ArgVars],

        % Create the construction unification to initialize the variable.
        TypeClassInfoRHS =
            rhs_functor(ConsId, is_not_exist_constr, AllArgVars),
        Ground = ground(shared, none_or_default_func),
        ArgMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        list.length(AllArgVars, NumArgs),
        list.duplicate(NumArgs, ArgMode, ArgModes),
        Unification = construct(TypeClassInfoVar, ConsId, AllArgVars, ArgModes,
            construct_dynamically, cell_is_unique, no_construct_sub_info),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        % XXX The UnifyContext is wrong.
        UnifyContext = unify_context(umc_explicit, []),
        GoalExpr = unify(TypeClassInfoVar, TypeClassInfoRHS, UnifyMode,
            Unification, UnifyContext),

        % Create a goal_info for the unification.
        goal_info_init(GoalInfo0),
        set_of_var.list_to_set([TypeClassInfoVar | AllArgVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
        list.duplicate(NumArgs, Ground, ArgInsts),
        % Note that we could perhaps be more accurate than `ground(shared)',
        % but it shouldn't make any difference.
        InstConsId = cell_inst_cons_id(typeclass_info_cell, NumArgs),
        InstResults = inst_test_results(inst_result_is_ground,
            inst_result_does_not_contain_any,
            inst_result_contains_inst_names_known(set.init),
            inst_result_contains_inst_vars_unknown,
            inst_result_contains_types_unknown,
            inst_result_no_type_ctor_propagated),
        % XXX that should be inst_result_contains_types_known(set.init),
        TypeClassInfoInst = bound(unique, InstResults,
            [bound_functor(InstConsId, ArgInsts)]),
        TypeClassInfoVarInst = TypeClassInfoVar - TypeClassInfoInst,
        InstMapDelta = instmap_delta_from_assoc_list([TypeClassInfoVarInst]),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo1, GoalInfo2),
        goal_info_set_determinism(detism_det, GoalInfo2, GoalInfo),

        Goal = hlds_goal(GoalExpr, GoalInfo),
        AllGoals = PrevGoals ++ [Goal]
    ).

%---------------------------------------------------------------------------%

:- pred get_arg_superclass_vars(hlds_class_defn::in, list(mer_type)::in,
    constraint_proof_map::in, existq_tvars::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

get_arg_superclass_vars(ClassDefn, InstanceTypes, SuperClassProofMap,
        ExistQVars, SuperClassTypeClassInfoVarsMCAs, SuperClassGoals, !Info) :-
    poly_info_get_proof_map(!.Info, ProofMap),

    poly_info_get_typevarset(!.Info, TVarSet0),
    SuperClasses0 = ClassDefn ^ classdefn_supers,
    ClassVars0 = ClassDefn ^ classdefn_vars,
    ClassTVarSet = ClassDefn ^ classdefn_tvarset,
    tvarset_merge_renaming(TVarSet0, ClassTVarSet, TVarSet1, Renaming),
    poly_info_set_typevarset(TVarSet1, !Info),

    apply_variable_renaming_to_tvar_list(Renaming, ClassVars0, ClassVars),
    map.from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

    apply_variable_renaming_to_prog_constraint_list(Renaming,
        SuperClasses0, SuperClasses1),
    apply_rec_subst_to_prog_constraint_list(TypeSubst,
        SuperClasses1, SuperClasses),

    poly_info_set_proof_map(SuperClassProofMap, !Info),
    make_typeclass_infos_for_superclasses(SuperClasses, ExistQVars,
        SuperClassTypeClassInfoVarsMCAs, SuperClassGoals, !Info),
    poly_info_set_proof_map(ProofMap, !Info).

:- pred make_typeclass_infos_for_superclasses(list(prog_constraint)::in,
    existq_tvars::in, assoc_list(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_infos_for_superclasses([], _, [], [], !Info).
make_typeclass_infos_for_superclasses([Constraint | Constraints], ExistQVars,
        [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs], Goals, !Info) :-
    term.context_init(Context),
    make_typeclass_info_var(Constraint, [], ExistQVars, Context,
        TypeClassInfoVarMCA, HeadGoals, !Info),
    make_typeclass_infos_for_superclasses(Constraints, ExistQVars,
        TypeClassInfoVarsMCAs, TailGoals, !Info),
    Goals = HeadGoals ++ TailGoals.

%---------------------------------------------------------------------------%

make_existq_typeclass_info_vars(ExistentialConstraints, Context,
        ExtraTypeClassVars, ExtraGoals, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, OldRttiVarMaps),
    make_typeclass_info_head_vars(do_record_type_info_locns,
        ExistentialConstraints, ExtraTypeClassVars, !Info),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var, ExtraTypeClassVars,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),

    constraint_list_get_tvars(ExistentialConstraints, TVars0),
    list.sort_and_remove_dups(TVars0, TVars),
    list.foldl2(
        polymorphism_maybe_extract_type_info(OldRttiVarMaps, Context), TVars,
        [], ExtraGoals, !Info).

    % For code which requires mode reordering, we may have already seen uses
    % of some of the type variables produced by this call. At the point of the
    % use of a type variable that we haven't seen before, we assume that it is
    % unconstrained. If it turns out that the type variable is constrained,
    % and the type_info is contained in a typeclass_info, we need to generate
    % code to extract it here.
    %
:- pred polymorphism_maybe_extract_type_info(rtti_varmaps::in,
    prog_context::in, tvar::in, list(hlds_goal)::in, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_maybe_extract_type_info(OldRttiVarMaps, Context, TVar,
        !ExtraGoals, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    ( if
        rtti_search_type_info_locn(OldRttiVarMaps, TVar,
            type_info(TypeInfoVar0)),
        rtti_search_type_info_locn(RttiVarMaps, TVar,
            typeclass_info(TypeClassInfoVar, Index))
    then
        polymorphism_extract_type_info(TVar, TypeClassInfoVar, Index, Context,
            NewGoals, TypeInfoVar1, !Info),
        make_complicated_unify_assign(TypeInfoVar0, TypeInfoVar1, AssignGoal),
        !:ExtraGoals = NewGoals ++ [AssignGoal | !.ExtraGoals]
    else
        true
    ).

%---------------------------------------------------------------------------%

make_typeclass_info_head_vars(RecordLocns, Constraints, ExtraHeadVars,
        !Info) :-
    list.map_foldl(make_typeclass_info_head_var(RecordLocns),
        Constraints, ExtraHeadVars, !Info).

:- pred make_typeclass_info_head_var(record_type_info_locns::in,
    prog_constraint::in, prog_var::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_head_var(RecordLocns, Constraint, TypeClassInfoVar,
        !Info) :-
    ( if
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_search_typeclass_info_var(RttiVarMaps0, Constraint,
            OldTypeClassInfoVar)
    then
        TypeClassInfoVar = OldTypeClassInfoVar
    else
        % Make a new variable to contain the dictionary for this typeclass
        % constraint.
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        (
            RecordLocns = do_record_type_info_locns,
            record_constraint_type_info_locns(Constraint, TypeClassInfoVar,
                !Info)
        ;
            RecordLocns = do_not_record_type_info_locns
        )
    ).

:- pred record_constraint_type_info_locns(prog_constraint::in, prog_var::in,
    poly_info::in, poly_info::out) is det.

record_constraint_type_info_locns(Constraint, ExtraHeadVar, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo),

    % Work out how many superclasses the class has.
    Constraint = constraint(ClassName, ClassTypes),
    list.length(ClassTypes, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, ClassId, ClassDefn),
    SuperClasses = ClassDefn ^ classdefn_supers,
    list.length(SuperClasses, NumSuperClasses),

    % Find all the type variables in the constraint, and remember what
    % index they appear in the typeclass info.

    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    % The first type_info will be just after the superclass infos.
    record_tci_slots_for_unseen_or_in_type_info_tvars(ExtraHeadVar,
        ClassTypes, NumSuperClasses + 1, RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info).

    % Work out which type variables we haven't seen before, or which we
    % assumed earlier would be produced in a type_info (this can happen for
    % code which needs mode reordering and which calls existentially quantified
    % predicates or deconstructs existentially quantified terms).
    %
    % Then make an entry in the TypeInfo locations map for each new type
    % variable. The type variable can be found at the calculated offset
    % with the new typeclass_info.
    %
:- pred record_tci_slots_for_unseen_or_in_type_info_tvars(prog_var::in,
    list(mer_type)::in, int::in, rtti_varmaps::in, rtti_varmaps::out) is det.

record_tci_slots_for_unseen_or_in_type_info_tvars(_, [], _, !RttiVarMaps).
record_tci_slots_for_unseen_or_in_type_info_tvars(ExtraHeadVar,
        [ClassType | ClassTypes], CurIndex, !RttiVarMaps) :-
    type_vars(ClassType, TypeVars),
    list.filter(is_unseen_or_in_type_info_tvar(!.RttiVarMaps),
        TypeVars, UnSeenOrInTypeInfoTypeVars),
    Location = typeclass_info(ExtraHeadVar, CurIndex),
    InsertIntoRttiVarMap =
        ( pred(TVar::in, R0::in, R::out) is det :-
            rtti_set_type_info_locn(TVar, Location, R0, R)
        ),
    % XXX If ClassType contains more than one type variable, this records
    % Location as applying to ALL OF THEM. This code is inherited from
    % the time when the parameters of typeclasses *had* to be type variables,
    % and has been a bug since we lifted that restriction ages ago.
    list.foldl(InsertIntoRttiVarMap, UnSeenOrInTypeInfoTypeVars, !RttiVarMaps),
    record_tci_slots_for_unseen_or_in_type_info_tvars(ExtraHeadVar,
        ClassTypes, CurIndex + 1, !RttiVarMaps).

:- pred is_unseen_or_in_type_info_tvar(rtti_varmaps::in, tvar::in) is semidet.

is_unseen_or_in_type_info_tvar(RttiVarMaps, TypeVar) :-
    ( if rtti_search_type_info_locn(RttiVarMaps, TypeVar, TypeInfoLocn) then
        TypeInfoLocn = type_info(_)
    else
        true
    ).

:- type tci_var_kind
    --->    base_typeclass_info_kind
    ;       typeclass_info_kind.

:- pred new_typeclass_info_var(prog_constraint::in, tci_var_kind::in,
    prog_var::out, mer_type::out, poly_info::in, poly_info::out) is det.

new_typeclass_info_var(Constraint, VarKind, Var, VarType, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    Constraint = constraint(ClassName, _),
    ClassNameString = unqualify_name(ClassName),

    % Introduce new variable.
    varset.new_var(Var, VarSet0, VarSet1),
    (
        VarKind = base_typeclass_info_kind,
        Name = "BaseTypeClassInfo_for_" ++ ClassNameString
    ;
        VarKind = typeclass_info_kind,
        Name = "TypeClassInfo_for_" ++ ClassNameString
    ),
    varset.name_var(Var, Name, VarSet1, VarSet),
    VarType = typeclass_info_type,
    add_var_type(Var, VarType, VarTypes0, VarTypes),
    rtti_det_insert_typeclass_info_var(Constraint, Var,
        RttiVarMaps0, RttiVarMaps),

    poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info).

%---------------------------------------------------------------------------%

:- pred materialize_base_typeclass_info_var(prog_constraint::in, cons_id::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

materialize_base_typeclass_info_var(Constraint, ConsId, Var, Goals, !Info) :-
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    ConstArg = csa_constant(ConsId, typeclass_info_type),
    ( if map.search(ConstStructVarMap0, ConstArg, OldVar) then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        Var = OldVar,
        Goals = []
    else
        new_typeclass_info_var(Constraint, base_typeclass_info_kind, Var,
            _VarType, !Info),

        % Create the construction unification to initialize the variable.
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        Unification = construct(Var, ConsId, [], [],
            construct_dynamically, cell_is_shared, no_construct_sub_info),
        Ground = ground(shared, none_or_default_func),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        % XXX The UnifyContext is wrong.
        UnifyContext = unify_context(umc_explicit, []),
        Unify = unify(Var, RHS, UnifyMode, Unification, UnifyContext),

        % Create the unification goal.
        NonLocals = set_of_var.make_singleton(Var),
        InstmapDelta = instmap_delta_bind_var(Var),
        goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
            GoalInfo),
        Goal = hlds_goal(Unify, GoalInfo),
        Goals = [Goal]
    ).

:- pred materialize_typeclass_info_var(prog_constraint::in, int::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

materialize_typeclass_info_var(Constraint, InstanceIdConstNum, Var, Goals,
        !Info) :-
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    InstanceIdConstArg = csa_const_struct(InstanceIdConstNum),
    ( if map.search(ConstStructVarMap0, InstanceIdConstArg, OldVar) then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        Var = OldVar,
        Goals = []
    else
        new_typeclass_info_var(Constraint, typeclass_info_kind, Var, _VarType,
            !Info),
        map.det_insert(InstanceIdConstArg, Var,
            ConstStructVarMap0, ConstStructVarMap),
        poly_info_set_const_struct_var_map(ConstStructVarMap, !Info),

        % Create the construction unification to initialize the variable.
        ConsId = typeclass_info_const(InstanceIdConstNum),
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        Unification = construct(Var, ConsId, [], [],
            construct_statically(born_static), cell_is_shared,
            no_construct_sub_info),
        Ground = ground(shared, none_or_default_func),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        % XXX The UnifyContext is wrong.
        UnifyContext = unify_context(umc_explicit, []),
        GoalExpr = unify(Var, RHS, UnifyMode, Unification, UnifyContext),

        % Create a goal_info for the unification.
        NonLocals = set_of_var.make_singleton(Var),
        InstmapDelta = instmap_delta_bind_var(Var),
        goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
            GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Goals = [Goal]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_type_class_info.
%---------------------------------------------------------------------------%
