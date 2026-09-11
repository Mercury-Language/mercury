%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2021-2023, 2025-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_type_class_info.
:- interface.

:- import_module check_hlds.polymorphism_info.
:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_goal.
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
:- pred make_existq_typeclass_info_vars(prog_context::in,
    list(prog_constraint)::in, list(prog_var)::out, list(hlds_goal)::out,
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

:- implementation.

:- import_module check_hlds.polymorphism_type_info.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.type_rename.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module deconstruct.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module pretty_printer.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

make_typeclass_info_vars(Constraints, ExistQVars, Context,
        TypeClassInfoVarsMCAs, ExtraGoals, !Info) :-
    SeenInstances = [],
    make_typeclass_info_vars_loop(ExistQVars, Context, SeenInstances,
        Constraints, TypeClassInfoVarsMCAs, ExtraGoals, !Info).

    % Accumulator version of the above.
    %
:- pred make_typeclass_info_vars_loop(existq_tvars::in, prog_context::in,
    list(prog_constraint)::in, list(prog_constraint)::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_vars_loop(_Context,  _ExistQVars, _Seen,
        [], [], [], !Info).
make_typeclass_info_vars_loop(ExistQVars, Context, Seen,
        [Constraint | Constraints],
        [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs], ExtraGoals, !Info) :-
    make_typeclass_info_var(ExistQVars, Context, [Constraint | Seen],
        Constraint, TypeClassInfoVarMCA, HeadExtraGoals, !Info),
    make_typeclass_info_vars_loop(ExistQVars, Context, Seen,
        Constraints, TypeClassInfoVarsMCAs, TailExtraGoals, !Info),
    ExtraGoals = HeadExtraGoals ++ TailExtraGoals.

%---------------------------------------------------------------------------%

:- pred make_typeclass_info_var(existq_tvars::in, prog_context::in,
    list(prog_constraint)::in, prog_constraint::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_var(ExistQVars, Context, Seen, Constraint,
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
        Goals = [],
        record_constructed_typeclass_info_var("rtti_varmaps",
            do_not_dump_all_tables, 0, Constraint,
            TypeClassInfoVar, no, no, !Info)
    else if
        % We don't have the typeclass_info, so we must either have a proof
        % that tells us how to make it, or ...
        poly_info_get_proof_map(!.Info, ProofMap),
        map.search(ProofMap, Constraint, Proof)
    then
        make_typeclass_info_from_proof(ExistQVars, Context, Seen,
            Constraint, Proof, TypeClassInfoVarMCA, Goals, !Info)
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
        Goals = [],
        record_constructed_typeclass_info_var("for later",
            do_not_dump_all_tables, 0, Constraint,
            TypeClassInfoVar, no, no, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred make_typeclass_info_from_proof(existq_tvars::in, prog_context::in,
    list(prog_constraint)::in, prog_constraint::in, constraint_proof::in,
    pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_proof(ExistQVars, Context, Seen,
        Constraint, Proof, TypeClassInfoVarMCA, Goals, !Info) :-
    (
        % XXX MR_Dictionary should have MR_Dictionaries for superclass
        % We have to extract the typeclass_info from another one.
        Proof = superclass(SubClassConstraint),
        get_or_make_typeclass_info_from_proof_subclass(ExistQVars, Context,
            Seen, Constraint, SubClassConstraint, TypeClassInfoVarMCA,
            Goals, !Info)
    ;
        % We have to construct the typeclass_info using an instance
        % declaration.
        Proof = apply_instance(InstanceNum),
        get_or_make_typeclass_info_from_proof_instance(ExistQVars, Context,
            Seen, Constraint, InstanceNum, TypeClassInfoVarMCA, Goals, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred get_or_make_typeclass_info_from_proof_subclass(existq_tvars::in,
    prog_context::in, list(prog_constraint)::in,
    prog_constraint::in, prog_constraint::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

get_or_make_typeclass_info_from_proof_subclass(ExistQVars, Context, Seen,
        Constraint, SubClassConstraint, TypeClassInfoVarMCA, Goals, !Info) :-
    trace [
        compile_time(flag("debug_poly_caches")),
        run_time(env("DEBUG_POLY_CACHES")),
        io(!IO)]
    (
        poly_info_get_selected_pred(SelectedPred, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            poly_info_get_indent_level(Level, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_set_indent_level(Level + 1, !IO),
            poly_info_get_typevarset(!.Info, TVarSet),
            ConstraintStr = trace_constraint_to_string(TVarSet, Constraint),
            SubClassConstraintStr =
                trace_constraint_to_string(TVarSet, SubClassConstraint),

            io.format(Stream, "%smake_typeclass_info_from_subclass\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%sConstraint: %s\n",
                [s(IndentStr), s(ConstraintStr)], !IO),
            ( if Seen = [Constraint] then
                io.format(Stream, "%sSeen: only Constraint\n",
                    [s(IndentStr)], !IO)
            else
                SeenStrs = list.map(trace_constraint_to_string(TVarSet), Seen),
                SeenStr = string.join_list(", ", SeenStrs),
                io.format(Stream, "%sSeen: %s\n",
                    [s(IndentStr), s(SeenStr)], !IO)
            ),
            io.format(Stream, "%sSubClassConstraint: %s\n",
                [s(IndentStr), s(SubClassConstraintStr)], !IO),
            io.format(Stream, "%sExistQVars: ", [s(IndentStr)], !IO),
            io.write_line(Stream, ExistQVars, !IO),
            io.nl(Stream, !IO),
            io.flush_output(Stream, !IO)
        )
    ),

    % Work out where to extract the typeclass info from.
    SubClassConstraint = constraint(SubClassName, SubClassTypes),
    list.length(SubClassTypes, SubClassArity),
    SubClassId = class_id(SubClassName, SubClassArity),

    % Make the typeclass_info for the subclass.
    make_typeclass_info_var(ExistQVars, Context, Seen, SubClassConstraint,
        SubClassVarMCA, SubClassVarGoals, !Info),
    SubClassVarMCA = SubClassVar - SubClassMCA,

    % Look up the definition of the subclass.
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, SubClassId, SubClassDefn),

    % Work out which superclass typeclass_info to take.
    SubClassTParams = SubClassDefn ^ classdefn_tparams,
    map.from_corresponding_lists(SubClassTParams, SubClassTypes, SubTypeSubst),
    apply_subst_to_prog_constraints(SubTypeSubst,
        SubClassDefn ^ classdefn_supers, SuperClasses),
    % We shouldn't have got this far if Constraint is not in SuperClasses.
    SuperClassIndex =
        list.det_index1_of_first_occurrence(SuperClasses, Constraint),

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
            SubClassConstStruct =
                const_struct(SubClassConsId, SubClassArgs, _, _, _),
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
                    TypeClassInfoVar, MaybeConsId, Goals, !Info),
                TypeClassInfoVarMCA = TypeClassInfoVar - yes(SelectedArg),
                record_constructed_typeclass_info_var("subclass constant",
                    do_not_dump_all_tables, -1, Constraint,
                    TypeClassInfoVar, yes(SelectedArg), MaybeConsId, !Info)
            else
                unexpected($pred, "unexpected typeclass info structure")
            )
        )
    ;
        SubClassMCA = no,
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - no,
        get_poly_const(SuperClassIndex, IndexVar, IndexGoals, !Info),

        % We extract the superclass typeclass_info by inserting a call
        % to superclass_from_typeclass_info in private_builtin.
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_private_builtin_module, "superclass_from_typeclass_info",
            [], [SubClassVar, IndexVar, TypeClassInfoVar],
            instmap_delta_bind_no_var, only_mode, detism_det, purity_pure, [],
            term_context.dummy_context, SuperClassGoal),
        Goals = SubClassVarGoals ++ IndexGoals ++ [SuperClassGoal],
        record_constructed_typeclass_info_var("subclass computed",
            do_not_dump_all_tables, -1, Constraint,
            TypeClassInfoVar, no, no, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred get_or_make_typeclass_info_from_proof_instance(existq_tvars::in,
    prog_context::in, list(prog_constraint)::in,
    prog_constraint::in, instance_id::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

get_or_make_typeclass_info_from_proof_instance(ExistQVars, Context, Seen,
        Constraint, InstanceId, TypeClassInfoVarMCA, Goals, !Info) :-
    InstanceId = instance_id(InstanceNum),
    trace [
        compile_time(flag("debug_poly_caches")),
        run_time(env("DEBUG_POLY_CACHES")),
        io(!IO)]
    (
        poly_info_get_selected_pred(SelectedPred, !IO),
        poly_info_get_indent_level(Level, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(!.Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_set_indent_level(Level + 1, !IO),
            poly_info_get_typevarset(!.Info, TVarSet),
            ConstraintStr = trace_constraint_to_string(TVarSet, Constraint),

            io.format(Stream,
                "%sget_or_make_typeclass_info_from_proof_instance\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%sConstraint: %s\n",
                [s(IndentStr), s(ConstraintStr)], !IO),
            ( if Seen = [Constraint] then
                io.format(Stream, "%sSeen: only Constraint\n",
                    [s(IndentStr)], !IO)
            else
                SeenStrs = list.map(trace_constraint_to_string(TVarSet), Seen),
                SeenStr = string.join_list(", ", SeenStrs),
                io.format(Stream, "%sSeen: %s\n",
                    [s(IndentStr), s(SeenStr)], !IO)
            ),
            list.sort(Seen, SortedSeen),
            list.sort_and_remove_dups(Seen, SortedSeenNoDups),
            ( if SortedSeen = SortedSeenNoDups then
                true
            else
                io.format(Stream, "%sSeen CONTAINS DUPLICATES\n",
                    [s(IndentStr)], !IO)
            ),
            io.format(Stream, "%sInstanceId: %d\n",
                [s(IndentStr), i(InstanceNum)], !IO),
            io.format(Stream, "%sExistQVars: ", [s(IndentStr)], !IO),
            io.write_line(Stream, ExistQVars, !IO),
            io.nl(Stream, !IO),
            io.flush_output(Stream, !IO)
        )
    ),

    poly_info_get_const_struct_db(!.Info, ConstStructDb0),
    ConstInstanceId = ciid(InstanceNum, Constraint, Seen),
    ( if
        search_for_constant_instance(ConstStructDb0, ConstInstanceId,
            InstanceIdConstNum)
    then
        materialize_typeclass_info_var(Constraint, InstanceIdConstNum,
            TypeClassInfoVar, MaybeConsId, Goals, !Info),
        CSA = csa_const_struct(InstanceIdConstNum),
        TypeClassInfoVarMCA = TypeClassInfoVar - yes(CSA),
        (
            Goals = [],
            ResultStr = "instance doubly cached result"
        ;
            Goals = [_ | _],
            ResultStr = "instance cached result"
        ),
        record_constructed_typeclass_info_var(ResultStr,
            do_not_dump_all_tables, -1, Constraint,
            TypeClassInfoVar, yes(CSA), MaybeConsId, !Info)
    else
        make_typeclass_info_from_proof_instance(ExistQVars, Context,
            ConstInstanceId, TypeClassInfoVarMCA, BaseConsId, Goals, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - MaybeCSA,
        record_constructed_typeclass_info_var("instance computed",
            dump_all_tables, -1, Constraint,
            TypeClassInfoVar, MaybeCSA, yes(BaseConsId), !Info)
    ).

:- pred make_typeclass_info_from_proof_instance(existq_tvars::in,
    prog_context::in, const_instance_id::in,
    pair(prog_var, maybe(const_struct_arg))::out, cons_id::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_proof_instance(ExistQVars, Context,
        ConstInstanceId, TypeClassInfoVarMCA, BaseConsId, Goals, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    poly_info_get_typevarset(!.Info, TypeVarSet),
    poly_info_get_proof_map(!.Info, ProofMap0),

    ConstInstanceId = ciid(InstanceNum, Constraint, Seen),
    Constraint = constraint(ClassName, ConstrainedTypes),

    list.length(ConstrainedTypes, ClassArity),
    ClassId = class_id(ClassName, ClassArity),

    map.lookup(InstanceTable, ClassId, InstanceList),
    list.det_index1(InstanceList, InstanceNum, ProofInstanceDefn),

    ProofInstanceDefn = hlds_instance_defn(_, _, InstanceTVarset,
        _, InstanceTypes, InstanceConstraints, _, InstanceProofMap, _, _, _),

    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(KindMap),

    type_vars_in_types(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
        UnconstrainedTVars),

    % We can ignore the new typevarset because all the type variables
    % in the instance constraints and superclass proofs must appear in
    % the arguments of the instance, and all such variables are bound
    % when we call type_list_subsumes then apply the resulting bindings.
    tvarset_merge_renaming(TypeVarSet, InstanceTVarset, _NewTVarset, Renaming),
    apply_renaming_to_types(Renaming, InstanceTypes, RenamedInstanceTypes),
    type_list_subsumes_det(RenamedInstanceTypes, ConstrainedTypes,
        InstanceSubst),
    apply_renaming_to_prog_constraints(Renaming,
        InstanceConstraints, RenamedInstanceConstraints),
    apply_rec_subst_to_prog_constraints(InstanceSubst,
        RenamedInstanceConstraints, ActualInstanceConstraints0),
    % XXX document diamond as guess
    % XXX does anyone know what the preceding line means?
    list.delete_elems(ActualInstanceConstraints0, Seen,
        ActualInstanceConstraints),
    apply_renaming_to_constraint_proof_map(Renaming,
        InstanceProofMap, RenamedInstanceProofMap),
    apply_rec_subst_to_constraint_proof_map(InstanceSubst,
        RenamedInstanceProofMap, ActualInstanceProofMap),

    apply_renaming_to_tvars(Renaming,
        UnconstrainedTVars, RenamedUnconstrainedTVars),
    apply_renaming_to_tvar_kind_map(Renaming, KindMap, RenamedKindMap),
    apply_rec_subst_to_tvars(RenamedKindMap, InstanceSubst,
        RenamedUnconstrainedTVars, ActualUnconstrainedTypes),

    map.overlay(ProofMap0, ActualInstanceProofMap, ProofMap),

    get_var_maps_snapshot("make_typeclass_info_from_proof_instance",
        InitialVarMapsSnapshot, !Info),

    % Make the type_infos for the types that are constrained by this.
    % These are packaged in the typeclass_info.
    polymorphism_do_make_type_info_vars(ConstrainedTypes, Context,
        ArgTypeInfoVarsMCAs, TypeInfoGoals, !Info),

    % Make the typeclass_infos for the constraints from the context of the
    % instance decl.
    make_typeclass_info_vars_loop(ExistQVars, Context, Seen,
        ActualInstanceConstraints, ArgTypeClassInfoVarsMCAs,
        InstanceConstraintGoals, !Info),

    % Make the type_infos for the unconstrained type variables
    % from the head of the instance declaration.
    polymorphism_do_make_type_info_vars(ActualUnconstrainedTypes, Context,
        ArgUnconstrainedTypeInfoVarsMCAs, UnconstrainedTypeInfoGoals, !Info),

    %---------------------%

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
        OldEntry0 = typeclass_info_map_entry(BaseConsIdPrime, ArgsMap0),
        map.search(ArgsMap0, ArgCOVAs, OldTypeClassInfoVarMCA0)
    then
        TypeClassInfoVarMCA = OldTypeClassInfoVarMCA0,
        BaseConsId = BaseConsIdPrime,
        Goals = [],
        set_var_maps_snapshot("make_typeclass_info",
            InitialVarMapsSnapshot, !Info),
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 2, !Info)
    else
        get_base_typeclass_info_cons_id(!.Info, InstanceTable, Constraint,
            instance_id(InstanceNum), InstanceTypes, BaseConsId),
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
        insert_constant_instance(ConstInstanceId, TypeClassInfoConstArgNum,
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

%---------------------------------------------------------------------------%

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
    ClassTParams0 = ClassDefn ^ classdefn_tparams,
    SuperClasses0 = ClassDefn ^ classdefn_supers,
    ClassTVarSet = ClassDefn ^ classdefn_tvarset,
    tvarset_merge_renaming(TVarSet0, ClassTVarSet, TVarSet1, Renaming),
    poly_info_set_typevarset(TVarSet1, !Info),

    apply_renaming_to_tvars(Renaming, ClassTParams0, ClassTParams),
    map.from_corresponding_lists(ClassTParams, InstanceTypes, TypeSubst),

    apply_renaming_to_prog_constraints(Renaming,
        SuperClasses0, SuperClasses1),
    apply_rec_subst_to_prog_constraints(TypeSubst,
        SuperClasses1, SuperClasses),

    poly_info_set_proof_map(SuperClassProofMap, !Info),
    make_typeclass_infos_for_superclasses(ExistQVars, SuperClasses,
        SuperClassTypeClassInfoVarsMCAs, SuperClassGoals, !Info),
    poly_info_set_proof_map(ProofMap, !Info).

:- pred make_typeclass_infos_for_superclasses(existq_tvars::in,
    list(prog_constraint)::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_infos_for_superclasses(_, [], [], [], !Info).
make_typeclass_infos_for_superclasses(ExistQVars, [Constraint | Constraints],
        [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs], Goals, !Info) :-
    Context = term_context.dummy_context,
    make_typeclass_info_var(ExistQVars, Context, [],
        Constraint, TypeClassInfoVarMCA, HeadGoals, !Info),
    make_typeclass_infos_for_superclasses(ExistQVars,
        Constraints, TypeClassInfoVarsMCAs, TailGoals, !Info),
    Goals = HeadGoals ++ TailGoals.

%---------------------------------------------------------------------------%

make_existq_typeclass_info_vars(Context, ExistentialConstraints,
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
    type_vars_in_type(ClassType, TypeVars),
    list.filter(is_unseen_or_in_type_info_tvar(!.RttiVarMaps),
        TypeVars, UnSeenOrInTypeInfoTypeVars),
    Location = typeclass_info(ExtraHeadVar, CurIndex),
    InsertIntoRttiVarMap =
        ( pred(TVar::in, R0::in, R::out) is det :-
            rtti_set_type_info_locn(TVar, Location, R0, R)
        ),
    % XXX If ClassType contains more than one type variable, this records
    % Location as applying to ALL OF THEM. This code is inherited from
    % the time when the parameters of typeclasses in typeclass constraints
    % *had* to be type variables, and has been a bug since we lifted
    % that restriction ages ago.
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

%---------------------------------------------------------------------------%

:- type tci_var_kind
    --->    base_typeclass_info_kind
    ;       typeclass_info_kind.

:- pred new_typeclass_info_var(prog_constraint::in, tci_var_kind::in,
    prog_var::out, mer_type::out, poly_info::in, poly_info::out) is det.

new_typeclass_info_var(Constraint, VarKind, Var, VarType, !Info) :-
    poly_info_get_var_table(!.Info, VarTable0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    Constraint = constraint(ClassName, _),
    ClassNameString = unqualify_name(ClassName),

    % Introduce new variable.
    (
        VarKind = base_typeclass_info_kind,
        VarName = "BaseTypeClassInfo_for_" ++ ClassNameString
    ;
        VarKind = typeclass_info_kind,
        VarName = "TypeClassInfo_for_" ++ ClassNameString
    ),
    VarType = typeclass_info_type,
    VarEntry = vte(VarName, VarType, is_not_dummy_type),
    add_var_entry(VarEntry, Var, VarTable0, VarTable),
    rtti_det_insert_typeclass_info_var(Constraint, Var,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_var_table_rtti(VarTable, RttiVarMaps, !Info).

%---------------------------------------------------------------------------%

:- pred materialize_base_typeclass_info_var(prog_constraint::in, cons_id::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

materialize_base_typeclass_info_var(Constraint, ConsId, Var, Goals, !Info) :-
    % NOTE We used to search for ConsId in the const_struct_var_map, but
    % this was useless, because we never PUT base_typeclass_infos into
    % the const_struct_var_map.
    %
    % Even if we COULD get base_typeclass_infos from there, there would be
    % no point, because base_typeclass_infos are constants, which means that
    %
    % - we do not avoid any memory traffic by reusing earlier constructed
    %   base_typeclass_infos, and
    %
    % - reusing an old base_typeclass_info constructed can be SLOWER than
    %   using a newly constructed one, if there is a call or other construct
    %   that requires flushing the stack between the program points
    %   of the construction and the reuse.

    % Create the construction unification to initialize the variable.
    new_typeclass_info_var(Constraint, base_typeclass_info_kind, Var,
        _VarType, !Info),
    RHS = rhs_functor(ConsId, is_not_exist_constr, []),
    Unification = construct(Var, ConsId, [], [],
        construct_dynamically, cell_is_shared, no_construct_sub_info),
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    % XXX The UnifyContext is wrong.
    UnifyContext = unify_context(umc_explicit, []),
    Unify = unify(Var, RHS, UnifyMode, Unification, UnifyContext),

    % Create the rest of the unification goal.
    NonLocals = set_of_var.make_singleton(Var),
    InstmapDelta = instmap_delta_bind_var(Var),
    goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
        GoalInfo),
    Goal = hlds_goal(Unify, GoalInfo),
    Goals = [Goal].

:- pred materialize_typeclass_info_var(prog_constraint::in, int::in,
    prog_var::out, maybe(cons_id)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

materialize_typeclass_info_var(Constraint, InstanceIdConstNum,
        Var, MaybeConsId, Goals, !Info) :-
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    InstanceIdConstArg = csa_const_struct(InstanceIdConstNum),
    ( if map.search(ConstStructVarMap0, InstanceIdConstArg, OldVar) then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        Var = OldVar,
        MaybeConsId = no,
        Goals = []
    else
        new_typeclass_info_var(Constraint, typeclass_info_kind, Var, _VarType,
            !Info),
        map.det_insert(InstanceIdConstArg, Var,
            ConstStructVarMap0, ConstStructVarMap),
        poly_info_set_const_struct_var_map(ConstStructVarMap, !Info),

        % Create the construction unification to initialize the variable.
        ConsId = typeclass_info_const(InstanceIdConstNum),
        MaybeConsId = yes(ConsId),
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        Ground = ground(shared, none_or_default_func),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
        ConstructHow = construct_statically(born_static),
        Unification = construct(Var, ConsId, [], [], ConstructHow,
            cell_is_shared, no_construct_sub_info),
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

    % Given a type_ctor, return the cons_id that represents its type_ctor_info.
    %
:- pred get_base_typeclass_info_cons_id(poly_info::in, instance_table::in,
    prog_constraint::in, instance_id::in, list(mer_type)::in,
    cons_id::out) is det.

get_base_typeclass_info_cons_id(Info, InstanceTable, Constraint, InstanceId,
        InstanceTypes, ConsId) :-
    Constraint = constraint(ClassSymName, ConstraintArgTypes),
    ClassId = class_id(ClassSymName, list.length(ConstraintArgTypes)),
    map.lookup(InstanceTable, ClassId, InstanceList),
    InstanceId = instance_id(InstanceNum),
    list.det_index1(InstanceList, InstanceNum, InstanceDefn),
    InstanceModuleName = InstanceDefn ^ instdefn_module,
    % NOTE The InstanceString ignores all parts of the InstanceTypes
    % except for the top type_ctor of each type.
    make_instance_string(InstanceTypes, InstanceString),
    ConsId = base_typeclass_info_const(InstanceModuleName, ClassId,
        InstanceNum, InstanceString),
    trace [
        compile_time(flag("debug_poly_caches")),
        run_time(env("DEBUG_POLY_CACHES")),
        io(!IO)]
    (
        poly_info_get_selected_pred(SelectedPred, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(Info, Stream, !IO),
            poly_info_get_indent_level(Level, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),
            poly_info_get_typevarset(Info, TVarSet),
            ConstraintStr = trace_constraint_to_string(TVarSet, Constraint),
            ConsIdStr = unqual_cons_id_and_arity_to_string(ConsId),
            io.format(Stream, "%sget_base_typeclass_info_cons_id:\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%s    %s\n",
                [s(IndentStr), s(ConstraintStr)], !IO),
            io.format(Stream, "%s    %s\n\n",
                [s(IndentStr), s(ConsIdStr)], !IO),
            io.flush_output(Stream, !IO)
        )
    ).

%---------------------------------------------------------------------------%
% The rest of this file contains code that is needed only for debugging.
%---------------------------------------------------------------------------%

:- type maybe_dump_all_tables
    --->    do_not_dump_all_tables
    ;       dump_all_tables.

:- pred record_constructed_typeclass_info_var(string::in,
    maybe_dump_all_tables::in, int::in, prog_constraint::in,
    prog_var::in, maybe(const_struct_arg)::in, maybe(cons_id)::in,
    poly_info::in, poly_info::out) is det.

record_constructed_typeclass_info_var(Where, MaybeDumpAll, LevelStep,
        Constraint, TypeClassInfoVar, MaybeCSA, MaybeConsId, Info, Info) :-
    trace [
        compile_time(flag("debug_poly_caches")),
        run_time(env("DEBUG_POLY_CACHES")),
        io(!IO)]
    (
        poly_info_get_selected_pred(SelectedPred, !IO),
        poly_info_get_indent_level(Level, !IO),
        poly_info_set_indent_level(Level + LevelStep, !IO),
        (
            SelectedPred = is_not_selected_pred
        ;
            SelectedPred = is_selected_pred,
            poly_info_get_debug_stream(Info, Stream, !IO),
            IndentStr = string.duplicate_char(' ', Level * 4),

            poly_info_get_typevarset(Info, TVarSet),
            poly_info_get_var_table(Info, VarTable),
            ConstraintStr = trace_constraint_to_string(TVarSet, Constraint),
            VarStr = mercury_var_to_string(VarTable, print_name_and_num,
                TypeClassInfoVar),
            (
                MaybeCSA = yes(ConstStructArg),
                CSAStr = const_struct_arg_to_string(TVarSet, ConstStructArg)
            ;
                MaybeCSA = no,
                CSAStr = "no const_struct_arg"
            ),
            (
                MaybeConsId = yes(ConsId),
                ConsIdStr = unqual_cons_id_and_arity_to_string(ConsId)
            ;
                MaybeConsId = no,
                ConsIdStr = "no cons_id"
            ),
            io.format(Stream, "%sFOR CONSTRAINT %s,\n%s%s returns %s\n",
                [s(IndentStr), s(ConstraintStr),
                s(IndentStr), s(Where), s(VarStr)], !IO),
            io.format(Stream, "%s    %s\n%s    %s\n\n",
                [s(IndentStr), s(CSAStr), s(IndentStr), s(ConsIdStr)], !IO),
            (
                MaybeDumpAll = do_not_dump_all_tables
            ;
                MaybeDumpAll = dump_all_tables,
                write_type_info_var_map(Stream, Info, IndentStr, !IO),
                write_typeclass_info_map(Stream, Info, IndentStr, !IO),
                write_constr_struct_var_map(Stream, Info, IndentStr, !IO)
            ),
            io.nl(Stream, !IO),
            io.flush_output(Stream, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred write_type_info_var_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_type_info_var_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_type_info_var_map(Info, TypeInfoVarMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%stype_info_var_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_type_info_var_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeInfoVarMap, !IO).

:- pred write_type_info_var_map_entry(io.text_output_stream::in, tvarset::in,
    var_table::in, string::in, type_ctor::in, type_info_var_map_entry::in,
    io::di, io::uo) is det.

write_type_info_var_map_entry(Stream, TVarSet, VarTable, IndentStr,
        TypeCtor, TypeInfoVarMapEntry, !IO) :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    TypeCtorName = unqualify_name(TypeCtorSymName),
    io.format(Stream, "%s%s/%d:\n",
        [s(IndentStr), s(TypeCtorName), i(TypeCtorArity)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_type_info_var_map_entry_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeInfoVarMapEntry, !IO).

:- pred write_type_info_var_map_entry_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(mer_type)::in, pair(prog_var, maybe(const_struct_arg))::in,
    io::di, io::uo) is det.

write_type_info_var_map_entry_entry(Stream, TVarSet, VarTable, IndentStr,
        Types, VarMaybeCSA, !IO) :-
    TypeStrs = list.map(trace_type_to_string(TVarSet), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    VarCSAStr =
        var_and_maybe_const_struct_arg_to_string(VarTable, VarMaybeCSA),
    io.format(Stream, "%s[%s] -> %s\n",
        [s(IndentStr), s(TypesStr), s(VarCSAStr)], !IO).

%---------------------------------------------------------------------------%

:- pred write_typeclass_info_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_typeclass_info_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_typeclass_info_map(Info, TypeClassInfoMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%stypeclass_info_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_top_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeClassInfoMap, !IO).

:- pred write_typeclass_info_top_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    class_name::in, typeclass_info_class_map::in, io::di, io::uo) is det.

write_typeclass_info_top_map_entry(Stream, TVarSet, VarTable, IndentStr,
        ClassSymName, TypeClassInfoClassMap, !IO) :-
    ClassName = unqualify_name(ClassSymName),
    io.format(Stream, "%sclass name %s\n", [s(IndentStr), s(ClassName)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_class_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        TypeClassInfoClassMap, !IO).

:- pred write_typeclass_info_class_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(mer_type)::in, typeclass_info_map_entry::in, io::di, io::uo) is det.

write_typeclass_info_class_map_entry(Stream, TVarSet, VarTable, IndentStr,
        Types, Entry, !IO) :-
    TypeStrs = list.map(trace_type_to_string(TVarSet), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    Entry = typeclass_info_map_entry(ConsId, CVAMap),
    ConsIdStr = unqual_cons_id_and_arity_to_string(ConsId),
    io.format(Stream, "%stypes [%s]:\n", [s(IndentStr), s(TypesStr)], !IO),
    io.format(Stream, "%s%s:\n", [s(IndentStr), s(ConsIdStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_typeclass_info_cva_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        CVAMap, !IO).

:- pred write_typeclass_info_cva_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in,
    list(const_or_var_arg)::in, pair(prog_var, maybe(const_struct_arg))::in,
    io::di, io::uo) is det.

write_typeclass_info_cva_map_entry(Stream, TVarSet, VarTable, IndentStr,
        ConstOrVarArgs, VarMaybeCSA, !IO) :-
    COVAStrs = list.map(const_or_var_arg_to_string(TVarSet, VarTable),
        ConstOrVarArgs),
    COVAsStr = string.join_list(", ", COVAStrs),
    VarMaybeCSAStr =
        var_and_maybe_const_struct_arg_to_string(VarTable, VarMaybeCSA),
    io.format(Stream, "%s%s ->\n%s    %s\n",
        [s(IndentStr), s(COVAsStr), s(IndentStr), s(VarMaybeCSAStr)], !IO).

%---------------------------------------------------------------------------%

:- pred write_constr_struct_var_map(io.text_output_stream::in,
    poly_info::in, string::in, io::di, io::uo) is det.

write_constr_struct_var_map(Stream, Info, IndentStr, !IO) :-
    poly_info_get_const_struct_var_map(Info, ConstStructVarMap),
    poly_info_get_typevarset(Info, TVarSet),
    poly_info_get_var_table(Info, VarTable),
    io.format(Stream, "%sconst_struct_var_map\n", [s(IndentStr)], !IO),
    NextIndentStr = IndentStr ++ "    ",
    map.foldl(
        write_constr_struct_var_map_entry(Stream, TVarSet, VarTable,
            NextIndentStr),
        ConstStructVarMap, !IO).

:- pred write_constr_struct_var_map_entry(io.text_output_stream::in,
    tvarset::in, var_table::in, string::in, const_struct_arg::in, prog_var::in,
    io::di, io::uo) is det.

write_constr_struct_var_map_entry(Stream, TVarSet, VarTable, IndentStr,
        CSA, Var, !IO) :-
    CSAStr = const_struct_arg_to_string(TVarSet, CSA),
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    io.format(Stream, "%s%s ->\n%s    %s\n",
        [s(IndentStr), s(CSAStr), s(IndentStr), s(VarStr)], !IO).

%---------------------------------------------------------------------------%

:- func var_and_maybe_const_struct_arg_to_string(var_table,
    pair(prog_var, maybe(const_struct_arg))) = string.

var_and_maybe_const_struct_arg_to_string(VarTable, Var - MaybeCSA) = Str :-
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    string.format("%s - %s", [s(VarStr), s(string(MaybeCSA))], Str).

:- func const_or_var_arg_to_string(tvarset, var_table, const_or_var_arg)
    = string.

const_or_var_arg_to_string(TVarSet, VarTable, ConstOrVarArg) = Str :-
    (
        ConstOrVarArg = cova_const(ConstStructArg),
        Str = const_struct_arg_to_string(TVarSet, ConstStructArg)
    ;
        ConstOrVarArg = cova_var(Var),
        Str = mercury_var_to_string(VarTable, print_name_and_num, Var)
    ).

:- func const_struct_arg_to_string(tvarset, const_struct_arg) = string.

const_struct_arg_to_string(TVarSet, ConstStructArg) = Str :-
    (
        ConstStructArg = csa_const_struct(N),
        string.format("struct #%d", [i(N)], Str)
    ;
        ConstStructArg = csa_constant(ConsId, Type),
        ConsIdStr = unqual_cons_id_and_arity_to_string(ConsId),
        TypeStr = trace_type_to_string(TVarSet, Type),
        string.format("constant(%s %s)", [s(ConsIdStr), s(TypeStr)], Str)
    ).

%---------------------------------------------------------------------------%

:- func trace_constraint_to_string(tvarset, prog_constraint) = string.

trace_constraint_to_string(TVarSet, Constraint0) = Str :-
    strip_module_names_from_constraint(strip_all_module_names,
        set_default_func, Constraint0, Constraint),
    Constraint = constraint(ClassSymName, ArgTypes),
    ClassName = unqualify_name(ClassSymName),
    ArgTypeStrs = list.map(trace_type_to_string(TVarSet), ArgTypes),
    ArgTypesStr = string.join_list(", ", ArgTypeStrs),
    string.format("%s(%s)", [s(ClassName), s(ArgTypesStr)], Str).

%---------------------------------------------------------------------------%

:- func trace_type_to_string(tvarset, mer_type) = string.

trace_type_to_string(TVarSet, Type0) = Str :-
    strip_module_names_from_type(strip_all_module_names, set_default_func,
        Type0, Type),
    Str = mercury_type_to_string(TVarSet, print_name_and_num, Type).

%---------------------------------------------------------------------------%

:- pred format_for_trace(string::in, T::in, string::out,
    io::di, io::uo) is det.
:- pragma consider_used(pred(format_for_trace/5)).

format_for_trace(IndentStr, Item, ItemDocStr, !IO) :-
    get_default_formatter_map(FormatterMap, !IO),
    MaxLen = 78 - string.count_code_points(IndentStr),
    Params = pp_params(MaxLen, 99999, linear(99999)),
    ItemDoc = pretty_printer.format(Item),
    doc_to_string(canonicalize, FormatterMap, Params, ItemDoc, ItemDocStr0),
    add_prefix_to_every_line(IndentStr, ItemDocStr0, ItemDocStr).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_type_class_info.
%---------------------------------------------------------------------------%
