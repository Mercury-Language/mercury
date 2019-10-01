%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_class.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred add_typeclass_defn(sec_item(item_typeclass_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_instance_defn(ims_item(item_instance_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Given the definition for a predicate or function from a type class
    % instance declaration, produce the clauses_info for that definition.
    %
:- pred do_produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, arity::in, list(mer_type)::in,
    pred_markers::in, term.context::in, instance_status::in, clauses_info::out,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.add_pred.
:- import_module hlds.default_func_mode.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module varset.

add_typeclass_defn(SectionItem, !ModuleInfo, !Specs) :-
    SectionItem = sec_item(SectionInfo, ItemTypeClassInfo),
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    item_mercury_status_to_typeclass_status(ItemMercuryStatus,
        TypeClassStatus0),

    ItemTypeClassInfo = item_typeclass_info(ClassName, ClassParamVars,
        Constraints, FunDeps, Interface, VarSet, Context, _SeqNum),
    module_info_get_class_table(!.ModuleInfo, ClassTable0),
    list.length(ClassParamVars, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    (
        Interface = class_interface_abstract,
        typeclass_make_status_abstract(TypeClassStatus0, TypeClassStatus1)
    ;
        Interface = class_interface_concrete(_),
        TypeClassStatus1 = TypeClassStatus0
    ),
    HLDSFunDeps = list.map(make_hlds_fundep(ClassParamVars), FunDeps),
    ( if map.search(ClassTable0, ClassId, OldDefn) then
        OldDefn = hlds_class_defn(OldTypeClassStatus, OldConstraints,
            OldFunDeps, _OldAncestors, OldClassParamVars, _OldKinds,
            OldInterface, OldClassMethodPredProcIds0, OldVarSet, OldContext,
            _BadClassDefn0),
        % The typeclass is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        typeclass_combine_status(TypeClassStatus1, OldTypeClassStatus,
            TypeClassStatus),
        (
            OldInterface = class_interface_concrete(_),
            OldClassMethodPredProcIds = OldClassMethodPredProcIds0,
            ClassInterface = OldInterface
        ;
            OldInterface = class_interface_abstract,
            OldClassMethodPredProcIds = [],
            ClassInterface = Interface
        ),
        ( if
            % Check that the superclass constraints are identical.
            not constraints_are_identical(OldClassParamVars, OldVarSet,
                OldConstraints, ClassParamVars, VarSet, Constraints)
        then
            % Always report the error, even in `.opt' files.
            Extras = [words("The superclass constraints do not match."), nl],
            report_multiple_def_error(ClassName, ClassArity, "typeclass",
                Context, OldContext, Extras, !Specs),
            HasIncompatibility = yes(OldDefn)
        else if
            % Check that the functional dependencies are identical.
            not class_fundeps_are_identical(OldFunDeps, HLDSFunDeps)
        then
            % Always report the error, even in `.opt' files.
            Extras = [words("The functional dependencies do not match."), nl],
            report_multiple_def_error(ClassName, ClassArity, "typeclass",
                Context, OldContext, Extras, !Specs),
            HasIncompatibility = yes(OldDefn)
        else if
            Interface = class_interface_concrete(_),
            OldInterface = class_interface_concrete(_)
        then
            ( if TypeClassStatus = typeclass_status(status_opt_imported) then
                true
            else
                Extras = [],
                report_multiple_def_error(ClassName, ClassArity, "typeclass",
                    Context, OldContext, Extras, !Specs)
            ),
            HasIncompatibility = yes(OldDefn)
        else
            HasIncompatibility = no
        )
    else
        HasIncompatibility = no,
        OldClassMethodPredProcIds = [],
        ClassInterface = Interface,
        TypeClassStatus = TypeClassStatus1,

        % When we find the class declaration, make an entry
        % for the instances.
        module_info_get_instance_table(!.ModuleInfo, Instances0),
        map.det_insert(ClassId, [], Instances0, Instances),
        module_info_set_instance_table(Instances, !ModuleInfo)
    ),
    (
        HasIncompatibility = yes(BaseDefn),
        (
            Interface = class_interface_abstract
        ;
            Interface = class_interface_concrete(_),
            % Record the presence of a bad concrete definition,
            % so check_typeclass.m can avoid generating a misleading
            % error message about the class having no definition.
            BadDefn = BaseDefn ^ classdefn_maybe_bad := has_bad_class_defn,
            map.det_update(ClassId, BadDefn, ClassTable0, ClassTable),
            module_info_set_class_table(ClassTable, !ModuleInfo)
        )
    ;
        HasIncompatibility = no,
        (
            Interface = class_interface_concrete(ClassDecls),
            module_add_class_interface(ClassName, ClassParamVars,
                TypeClassStatus, yes(ItemMercuryStatus), NeedQual,
                ClassDecls, ClassMethodPredProcIds, !ModuleInfo, !Specs)
        ;
            Interface = class_interface_abstract,
            ClassMethodPredProcIds = OldClassMethodPredProcIds
        ),

        % Ancestors is not set until check_typeclass.
        Ancestors = [],
        % XXX kind inference:
        % We set all the kinds to `star' at the moment. This should be
        % done differently when we have a proper kind system.
        Kinds = map.init,
        ClassDefn = hlds_class_defn(TypeClassStatus, Constraints, HLDSFunDeps,
            Ancestors, ClassParamVars, Kinds, ClassInterface,
            ClassMethodPredProcIds, VarSet, Context, has_no_bad_class_defn),
        map.set(ClassId, ClassDefn, ClassTable0, ClassTable),
        module_info_set_class_table(ClassTable, !ModuleInfo)
    ).

:- func make_hlds_fundep(list(tvar), prog_fundep) = hlds_class_fundep.

make_hlds_fundep(TVars, fundep(Domain0, Range0)) = fundep(Domain, Range) :-
    Domain = make_hlds_fundep_2(TVars, Domain0),
    Range = make_hlds_fundep_2(TVars, Range0).

:- func make_hlds_fundep_2(list(tvar), list(tvar)) = set(hlds_class_argpos).

make_hlds_fundep_2(TVars, List) = list.foldl(Func, List, set.init) :-
    Func = (func(TVar, Set0) = set.insert(Set0, N) :-
        N = get_list_index(TVars, 1, TVar)
    ).

:- func get_list_index(list(T), hlds_class_argpos, T) = hlds_class_argpos.

get_list_index([], _, _) = _ :-
    unexpected($pred, "element not found").
get_list_index([E | Es], N, X) =
    ( if X = E then
        N
    else
        get_list_index(Es, N + 1, X)
    ).

:- pred constraints_are_identical(list(tvar)::in, tvarset::in,
    list(prog_constraint)::in, list(tvar)::in, tvarset::in,
    list(prog_constraint)::in) is semidet.

constraints_are_identical(OldVars0, OldVarSet, OldConstraints0,
        Vars, VarSet, Constraints) :-
    tvarset_merge_renaming(VarSet, OldVarSet, _, Renaming),
    apply_variable_renaming_to_prog_constraint_list(Renaming, OldConstraints0,
        OldConstraints1),
    apply_variable_renaming_to_tvar_list(Renaming, OldVars0,  OldVars),

    map.from_corresponding_lists(OldVars, Vars, VarRenaming),
    apply_variable_renaming_to_prog_constraint_list(VarRenaming,
        OldConstraints1, OldConstraints),
    OldConstraints = Constraints.

:- pred class_fundeps_are_identical(hlds_class_fundeps::in,
    hlds_class_fundeps::in) is semidet.

class_fundeps_are_identical(OldFunDeps0, FunDeps0) :-
    % Allow for the functional dependencies to be in a different order.
    sort_and_remove_dups(OldFunDeps0, OldFunDeps),
    sort_and_remove_dups(FunDeps0, FunDeps),
    % The list elements we are comparing are sets; we rely on the fact that
    % sets have a canonical representation.
    OldFunDeps = FunDeps.

:- pred module_add_class_interface(sym_name::in, list(tvar)::in,
    typeclass_status::in, maybe(item_mercury_status)::in,
    need_qualifier::in, list(class_decl)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_class_interface(ClassName, ClassParamVars, TypeClassStatus,
        MaybeItemMercuryStatus, NeedQual, ClassDecls, !:PredProcIds,
        !ModuleInfo, !Specs) :-
    classify_class_decls(ClassDecls, ClassPredOrFuncInfos, ClassModeInfos),

    % We collect !:PredProcIds, the pred_proc_ids of the methods
    % of the typeclass, in three stages.
    %
    % - The first stage: add_class_pred_or_func_decl will add to it
    %   the pred_proc_ids of methods that have predmode declarations.
    %
    % - The second stage: add_class_mode_decl will add to it
    %   the pred_proc_ids of methods that have separate mode declarations.
    %
    % - The third stage: handle_no_mode_decl will add to it
    %   the pred_proc_ids of the default modes of any function methods
    %   that have no mode declaration, either separately or as part of the
    %   function declaration.
    %
    % At the end, we sort the results, on pred_id and then proc_id.
    % check_typeclass.m assumes this order when it is generating the
    % corresponding list of pred_proc_ids for instance definitions.
    !:PredProcIds = [],

    % XXX STATUS
    TypeClassStatus = typeclass_status(OldImportStatus),
    PredStatus = pred_status(OldImportStatus),
    ItemNumber = -1,
    list.foldl3(
        add_class_pred_or_func_decl(ClassName, ClassParamVars,
            ItemNumber, PredStatus, NeedQual),
        ClassPredOrFuncInfos, !PredProcIds, !ModuleInfo, !Specs),

    % Add the mode declarations. Since we have already added the
    % predicate/function declarations, there should already be an entry
    % in the predicate table corresponding to the mode we are about to add.
    % If not, report an error.
    list.foldl3(
        add_class_mode_decl(ItemNumber, MaybeItemMercuryStatus, PredStatus),
        ClassModeInfos, !PredProcIds, !ModuleInfo, !Specs),
    list.foldl3(handle_no_mode_decl,
        ClassPredOrFuncInfos, !PredProcIds, !ModuleInfo, !Specs),
    list.sort(!PredProcIds).

:- pred classify_class_decls(list(class_decl)::in,
    list(class_pred_or_func_info)::out, list(class_mode_info)::out) is det.

classify_class_decls([], [], []).
classify_class_decls([Decl | Decls], !:PredOrFuncInfos, !:ModeInfos) :-
    classify_class_decls(Decls, !:PredOrFuncInfos, !:ModeInfos),
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        !:PredOrFuncInfos = [PredOrFuncInfo | !.PredOrFuncInfos]
    ;
        Decl = class_decl_mode(ModeInfo),
        !:ModeInfos = [ModeInfo | !.ModeInfos]
    ).

:- pred add_class_pred_or_func_decl(sym_name::in, list(tvar)::in,
    int::in, pred_status::in, need_qualifier::in, class_pred_or_func_info::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_decl(ClassName, ClassParamVars, ItemNumber,
        PredStatus, NeedQual,
        PredOrFuncInfo, !PredProcIds, !ModuleInfo, !Specs) :-
    PredOrFuncInfo = class_pred_or_func_info(PredName, PredOrFunc,
        ArgTypesAndModes, WithType, WithInst, MaybeDetism,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints0, Context),
    % XXX kind inference:
    % We set the kinds to `star' at the moment. This will be different
    % when we have a kind system.
    var_list_to_type_list(map.init, ClassParamVars, ClassParamTypes),
    ImplicitConstraint = constraint(ClassName, ClassParamTypes),
    Constraints0 = constraints(UnivConstraints0, ExistConstraints),
    UnivConstraints = [ImplicitConstraint | UnivConstraints0],
    Constraints = constraints(UnivConstraints, ExistConstraints),
    Attrs = item_compiler_attributes(compiler_origin_class_method),
    MaybeAttrs = item_origin_compiler(Attrs),
    PredDecl = item_pred_decl_info(PredName, PredOrFunc,
        ArgTypesAndModes, WithType, WithInst, MaybeDetism, MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, ItemNumber),
    module_add_pred_decl(PredStatus, NeedQual, PredDecl, MaybePredProcId,
        !ModuleInfo, !Specs),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        !:PredProcIds = [PredProcId | !.PredProcIds]
    ).

:- pred add_class_mode_decl(int::in, maybe(item_mercury_status)::in,
    pred_status::in, class_mode_info::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_mode_decl(ItemNumber, MaybeItemMercuryStatus, PredStatus,
        ModeInfo, !PredProcIds, !ModuleInfo, !Specs) :-
    ModeInfo = class_mode_info(PredName, MaybePredOrFunc, Modes,
        _WithInst, MaybeDetism, VarSet, Context),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    PredArity = list.length(Modes) : int,
    (
        MaybePredOrFunc = no,
        % The only way this could have happened now is if a `with_inst`
        % annotation was not expanded.
        unexpected($pred, "unexpanded `with_inst` annotation")
    ;
        MaybePredOrFunc = yes(PredOrFunc)
    ),
    predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
        PredOrFunc, PredName, PredArity, PredIds),
    (
        PredIds = [],
        missing_pred_or_func_method_error(PredName, PredArity, PredOrFunc,
            Context, !Specs)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_markers(PredInfo, PredMarkers),
            ( if check_marker(PredMarkers, marker_class_method) then
                module_add_mode(Context, ItemNumber,
                    MaybeItemMercuryStatus, PredStatus,
                    PredOrFunc, PredName, VarSet, Modes, MaybeDetism,
                    is_a_class_method, PredProcId, !ModuleInfo, !Specs),
                !:PredProcIds = [PredProcId | !.PredProcIds]
            else
                % XXX It may also be worth reporting that although there
                % wasn't a matching class method, there was a matching
                % predicate/function.
                missing_pred_or_func_method_error(PredName, PredArity,
                    PredOrFunc, Context, !Specs)
            )
        ;
            TailPredIds = [_ | _],
            % This shouldn't happen.
            unexpected($pred, "multiple preds matching method mode")
        )
    ).

    % If a method has no mode declaration, then
    % - add a default mode, if the method is a function;
    % - report an error, if the method is a predicate.
    %
:- pred handle_no_mode_decl(class_pred_or_func_info::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_no_mode_decl(PredOrFuncInfo, !PredProcIds, !ModuleInfo, !Specs) :-
    PredOrFuncInfo = class_pred_or_func_info(QualPredOrFuncName, PorF,
        TypesAndModes, _, _, _, _, _, _, _, _, _),
    (
        QualPredOrFuncName = qualified(ModuleName, PredOrFuncName)
    ;
        QualPredOrFuncName = unqualified(_),
        % The class interface should be fully module qualified
        % by the parser at the time it is read in.
        unexpected($pred, "unqualified")
    ),
    list.length(TypesAndModes, PredArity),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pf_m_n_a(PredTable, is_fully_qualified,
        PorF, ModuleName, PredOrFuncName, PredArity, PredIds),
    (
        PredIds = [PredId],
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        (
            PorF = pf_function,
            maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProcId),
            (
                MaybeProcId = no
            ;
                MaybeProcId = yes(ProcId),
                !:PredProcIds = [proc(PredId, ProcId) | !.PredProcIds],
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            )
        ;
            PorF = pf_predicate,
            pred_info_get_proc_table(PredInfo0, Procs),
            ( if map.is_empty(Procs) then
                pred_method_with_no_modes_error(PredInfo0, !Specs)
            else
                true
            )
        )
    ;
        ( PredIds = []
        ; PredIds = [_, _ | _]
        ),
        unexpected($pred, "number of preds != 1")
    ).

%-----------------------------------------------------------------------------%

add_instance_defn(StatusItem, !ModuleInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemInstanceInfo),
    ItemInstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody0, VarSet, InstanceModuleName,
        Context, _SeqNum),
    item_mercury_status_to_instance_status(ItemMercuryStatus, InstanceStatus0),
    (
        InstanceBody0 = instance_body_abstract,
        % XXX This can make the status abstract_imported even if the instance
        % is NOT imported.
        % When this is fixed, please undo the workaround for this bug
        % in instance_used_modules in unused_imports.m.
        instance_make_status_abstract(InstanceStatus0, InstanceStatus)
    ;
        InstanceBody0 = instance_body_concrete(_),
        InstanceStatus = InstanceStatus0
    ),

    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_instance_table(!.ModuleInfo, InstanceTable0),
    list.length(Types, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    expand_bang_state_pairs_in_instance_body(InstanceBody0, InstanceBody),
    ( if map.search(Classes, ClassId, _) then
        MaybeClassInterface = no,
        map.init(ProofMap),
        NewInstanceDefn = hlds_instance_defn(InstanceModuleName,
            Types, OriginalTypes, InstanceStatus, Context, Constraints,
            InstanceBody, MaybeClassInterface, VarSet, ProofMap),
        map.lookup(InstanceTable0, ClassId, OldInstanceDefns),
        check_for_overlapping_instances(NewInstanceDefn, OldInstanceDefns,
            ClassId, !Specs),
        check_instance_compatibility(NewInstanceDefn, OldInstanceDefns,
            ClassId, !Specs),
        map.det_update(ClassId, [NewInstanceDefn | OldInstanceDefns],
            InstanceTable0, InstanceTable),
        module_info_set_instance_table(InstanceTable, !ModuleInfo)
    else
        undefined_type_class_error(ClassName, ClassArity, Context,
            "instance declaration", !Specs)
    ).

:- pred check_for_overlapping_instances(hlds_instance_defn::in,
    list(hlds_instance_defn)::in, class_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_overlapping_instances(NewInstanceDefn, OtherInstanceDefns,
        ClassId, !Specs) :-
    NewInstanceDefn = hlds_instance_defn(_, NewTypes, _, _, NewContext,
        _, NewInstanceBody, _, NewTVarSet, _),
    ( if
        NewInstanceBody = instance_body_concrete(_) % XXX
    then
        report_any_overlapping_instance_declarations(ClassId,
            NewTypes, NewTVarSet, NewContext, OtherInstanceDefns, !Specs)
    else
        true
    ).

:- pred report_any_overlapping_instance_declarations(class_id::in,
    list(mer_type)::in, tvarset::in, prog_context::in,
    list(hlds_instance_defn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_overlapping_instance_declarations(_, _, _, _, [], !Specs).
report_any_overlapping_instance_declarations(ClassId,
        NewTypes, NewTVarSet, NewContext,
        [OtherInstanceDefn | OtherInstanceDefns], !Specs) :-
    OtherInstanceDefn = hlds_instance_defn(_, OtherTypes, _, _, OtherContext,
        _, OtherInstanceBody, _, OtherTVarSet, _),
    ( if
        OtherInstanceBody = instance_body_concrete(_), % XXX
        tvarset_merge_renaming(NewTVarSet, OtherTVarSet, _MergedTVarSet,
            Renaming),
        apply_variable_renaming_to_type_list(Renaming, OtherTypes,
            NewOtherTypes),
        type_list_subsumes(NewTypes, NewOtherTypes, _)
    then
        ClassId = class_id(ClassName, ClassArity),
        % XXX STATUS Multiply defined if type_list_subsumes in BOTH directions.
        NewPieces = [words("Error: multiply defined (or overlapping)"),
            words("instance declarations for class"),
            qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
            suffix("."), nl],
        NewMsg = simple_msg(NewContext, [always(NewPieces)]),
        OtherPieces = [words("Previous instance declaration was here.")],
        OtherMsg = error_msg(yes(OtherContext), treat_as_first, 0,
            [always(OtherPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [NewMsg, OtherMsg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    % Maybe we shouldn't recurse if we generated an error above,
    % but triply-defined instances are so rare that it doesn't much matter,
    % and in some even more rare cases, the extra info may be useful.
    report_any_overlapping_instance_declarations(ClassId,
        NewTypes, NewTVarSet, NewContext, OtherInstanceDefns, !Specs).

    % If two instance declarations are about the same type, then one must be
    % the abstract declaration and the other the concrete definition.
    % The two must be compatible, i.e. their constraints must be identical.
    % If they are not, then generate an error message.
    %
:- pred check_instance_compatibility(hlds_instance_defn::in,
    list(hlds_instance_defn)::in, class_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_compatibility(InstanceDefn, InstanceDefns, ClassId, !Specs) :-
    list.filter(same_type_hlds_instance_defn(InstanceDefn),
        InstanceDefns, EquivInstanceDefns),
    list.foldl(check_instance_constraints(InstanceDefn, ClassId),
        EquivInstanceDefns, !Specs).

:- pred check_instance_constraints(hlds_instance_defn::in,
    class_id::in, hlds_instance_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_constraints(InstanceDefnA, ClassId, InstanceDefnB, !Specs) :-
    type_vars_list(InstanceDefnA ^ instdefn_types, TVarsA),
    TVarSetA = InstanceDefnA ^ instdefn_tvarset,
    ConstraintsA = InstanceDefnA ^ instdefn_constraints,

    type_vars_list(InstanceDefnB ^ instdefn_types, TVarsB),
    TVarSetB = InstanceDefnB ^ instdefn_tvarset,
    ConstraintsB = InstanceDefnB ^ instdefn_constraints,

    ( if
        constraints_are_identical(TVarsA, TVarSetA, ConstraintsA,
            TVarsB, TVarSetB, ConstraintsB)
    then
        true
    else
        ClassId = class_id(ClassName, ClassArity),
        ClassSNA = sym_name_arity(ClassName, ClassArity),

        ContextA = InstanceDefnA ^ instdefn_context,
        ContextB = InstanceDefnB ^ instdefn_context,
        % The flattening of source item blocks by modules.m puts
        % all items in a given section together. Since the original
        % source code may have had the contents of the different sections
        % intermingled, this may change the relative order of items.
        % Put them back in the original order for this error message.
        compare(CmpRes, ContextA, ContextB),
        (
            ( CmpRes = (<)
            ; CmpRes = (=)
            ),
            FirstContext = ContextA,
            SecondContext = ContextB
        ;
            CmpRes = (>),
            FirstContext = ContextB,
            SecondContext = ContextA
        ),

        % Since the first declaration by itself is fine, the first sign
        % of a problem appears at the second declaration. This is why
        % we start the report of the problem with *its* context.
        SecondDeclPieces = [words("In instance declaration for class"),
            qual_sym_name_and_arity(ClassSNA), suffix(":"), nl,
            words("the instance constraints here"),
            words("are incompatible with ..."), nl],
        SecondDeclMsg = simple_msg(SecondContext, [always(SecondDeclPieces)]),

        FirstDeclPieces = [words("... the instance constraints here."), nl],
        FirstDeclMsg = simple_msg(FirstContext, [always(FirstDeclPieces)]),

        Spec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [SecondDeclMsg, FirstDeclMsg]),
        !:Specs = [Spec | !.Specs]
    ).

    % Do two hlds_instance_defn refer to the same type?
    % e.g. "instance tc(f(T))" compares equal to "instance tc(f(U))"
    %
    % Note we don't check that the constraints of the declarations are the
    % same.
    %
:- pred same_type_hlds_instance_defn(hlds_instance_defn::in,
    hlds_instance_defn::in) is semidet.

same_type_hlds_instance_defn(InstanceDefnA, InstanceDefnB) :-
    TypesA = InstanceDefnA ^ instdefn_types,
    TypesB0 = InstanceDefnB ^ instdefn_types,

    VarSetA = InstanceDefnA ^ instdefn_tvarset,
    VarSetB = InstanceDefnB ^ instdefn_tvarset,

    % Rename the two lists of types apart.
    tvarset_merge_renaming(VarSetA, VarSetB, _NewVarSet, RenameApart),
    apply_variable_renaming_to_type_list(RenameApart, TypesB0, TypesB1),

    type_vars_list(TypesA, TVarsA),
    type_vars_list(TypesB1, TVarsB),

    % If the lengths are different they can't be the same type.
    list.length(TVarsA, NumTVars),
    list.length(TVarsB, NumTVars),

    map.from_corresponding_lists(TVarsB, TVarsA, Renaming),
    apply_variable_renaming_to_type_list(Renaming, TypesB1, TypesB),

    TypesA = TypesB.

do_produce_instance_method_clauses(InstanceProcDefn, PredOrFunc, PredArity,
        ArgTypes, Markers, Context, InstanceStatus, ClausesInfo,
        !TVarSet, !ModuleInfo, !QualInfo, !Specs) :-
    (
        % Handle the `pred(<MethodName>/<Arity>) is <ImplName>' syntax.
        InstanceProcDefn = instance_proc_def_name(InstancePredName),
        % Add the body of the introduced pred.
        % First the goal info, ...
        set_of_var.list_to_set(HeadVars, NonLocals),
        ( if check_marker(Markers, marker_is_impure) then
            Purity = purity_impure
        else if check_marker(Markers, marker_is_semipure) then
            Purity = purity_semipure
        else
            Purity = purity_pure
        ),
        instmap_delta_init_unreachable(DummyInstMapDelta),
        DummyDetism = detism_erroneous,
        goal_info_init(NonLocals, DummyInstMapDelta, DummyDetism, Purity,
            Context, GoalInfo),
        % ... and then the goal itself.
        varset.init(VarSet0),
        make_n_fresh_vars("HeadVar__", PredArity, HeadVars, VarSet0, VarSet),
        construct_pred_or_func_call(invalid_pred_id, PredOrFunc,
            InstancePredName, HeadVars, GoalInfo, IntroducedGoal, !QualInfo),
        IntroducedClause = clause(all_modes, IntroducedGoal, impl_lang_mercury,
            Context, []),

        map.init(TVarNameMap),
        vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
        HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
        set_clause_list([IntroducedClause], ClausesRep),
        rtti_varmaps_init(RttiVarMaps),
        ClausesInfo = clauses_info(VarSet, TVarNameMap, VarTypes, VarTypes,
            HeadVarVec, ClausesRep, init_clause_item_numbers_comp_gen,
            RttiVarMaps, no_foreign_lang_clauses, no_clause_syntax_errors)
    ;
        % Handle the arbitrary clauses syntax.
        InstanceProcDefn = instance_proc_def_clauses(InstanceClauses),
        clauses_info_init(PredOrFunc, PredArity,
            init_clause_item_numbers_comp_gen, ClausesInfo0),
        list.foldl5(
            produce_instance_method_clause(PredOrFunc, Context,
                InstanceStatus),
            InstanceClauses, !TVarSet, !ModuleInfo, !QualInfo,
            ClausesInfo0, ClausesInfo, !Specs)
    ).

:- pred produce_instance_method_clause(pred_or_func::in,
    prog_context::in, instance_status::in, item_clause_info::in,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, clauses_info::in, clauses_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

produce_instance_method_clause(PredOrFunc, Context, InstanceStatus,
        InstanceClause, TVarSet0, TVarSet, !ModuleInfo, !QualInfo,
        !ClausesInfo, !Specs) :-
    InstanceClause = item_clause_info(PredName, ClausePredOrFunc, HeadTerms0,
        _Origin, CVarSet, MaybeBodyGoal, _ClauseContext, _SeqNum),
    % XXX Can this ever fail? If yes, we should generate an error message
    % instead of aborting.
    expect(unify(PredOrFunc, ClausePredOrFunc), $pred, "PredOrFunc mismatch"),
    ( if
        illegal_state_var_func_result(PredOrFunc, HeadTerms0, StateVar,
            StateVarContext)
    then
        TVarSet = TVarSet0,
        report_illegal_func_svar_result(StateVarContext, CVarSet, StateVar,
            !Specs),
        !:Specs = get_any_errors1(MaybeBodyGoal) ++ !.Specs
    else
        (
            MaybeBodyGoal = error1(BodyGoalSpecs),
            TVarSet = TVarSet0,
            !:Specs = BodyGoalSpecs ++ !.Specs
        ;
            MaybeBodyGoal = ok1(BodyGoal),
            expand_bang_state_pairs_in_terms(HeadTerms0, HeadTerms),
            PredArity = list.length(HeadTerms),
            adjust_func_arity(PredOrFunc, Arity, PredArity),

            % AllProcIds is only used when the predicate has foreign procs,
            % which the instance method pred should not have, so this
            % dummy value should be ok.
            AllProcIds = [],
            GoalType = goal_type_none,    % goal is not a promise
            % XXX STATUS
            InstanceStatus = instance_status(OldImportStatus),
            PredStatus = pred_status(OldImportStatus),
            clauses_info_add_clause(all_modes, AllProcIds, CVarSet, TVarSet0,
                HeadTerms, BodyGoal, Context, no, PredStatus, PredOrFunc,
                Arity, GoalType, Goal, VarSet, TVarSet, Warnings,
                !ClausesInfo, !ModuleInfo, !QualInfo, !Specs),

            SimpleCallId = simple_call_id(PredOrFunc, PredName, Arity),

            % Warn about singleton variables.
            warn_singletons(!.ModuleInfo, SimpleCallId, VarSet, Goal, !Specs),

            % Warn about variables with overlapping scopes.
            add_quant_warnings(SimpleCallId, VarSet, Warnings, !Specs)
        )
    ).

:- pred pred_method_with_no_modes_error(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_method_with_no_modes_error(PredInfo, !Specs) :-
    pred_info_get_context(PredInfo, Context),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),

    Pieces = [words("Error: no mode declaration"),
        words("for type class method predicate"),
        qual_sym_name_and_arity(
            sym_name_arity(qualified(ModuleName, PredName), Arity)),
        suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred undefined_type_class_error(sym_name::in, arity::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

undefined_type_class_error(ClassName, ClassArity, Context, Description,
        !Specs) :-
    Pieces = [words("Error:"), words(Description), words("for"),
        qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
        words("without corresponding"), decl("typeclass"),
        words("declaration."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred missing_pred_or_func_method_error(sym_name::in, arity::in,
    pred_or_func::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

missing_pred_or_func_method_error(MethodName, MethodArity, PredOrFunc,
        Context, !Specs) :-
    Pieces = [words("Error: mode declaration for type class method"),
        qual_sym_name_and_arity(sym_name_arity(MethodName, MethodArity)),
        words("without corresponding"), p_or_f(PredOrFunc),
        words("method declaration."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_class.
%-----------------------------------------------------------------------------%
