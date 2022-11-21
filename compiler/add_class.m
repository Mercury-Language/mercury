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
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred add_typeclass_defns(sec_list(item_typeclass_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_instance_defns(ims_list(item_instance_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.                    % XXX Temporary import.
:- import_module check_hlds.check_typeclass.    % XXX Temporary import.
:- import_module hlds.add_pred.
:- import_module hlds.default_func_mode.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds_error.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_typeclass_defns([], !ModuleInfo, !Specs).
add_typeclass_defns([SecSubList | SecSubLists], !ModuleInfo, !Specs) :-
    SecSubList = sec_sub_list(SectionInfo, Items),
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    item_mercury_status_to_typeclass_status(ItemMercuryStatus,
        TypeClassStatus0),
    list.foldl2(
        add_typeclass_defn(ItemMercuryStatus, TypeClassStatus0, NeedQual),
        Items, !ModuleInfo, !Specs),
    add_typeclass_defns(SecSubLists, !ModuleInfo, !Specs).

add_instance_defns([], !ModuleInfo, !Specs).
add_instance_defns([ImsSubList | ImsSubLists], !ModuleInfo, !Specs) :-
    ImsSubList = ims_sub_list(ItemMercuryStatus, Items),
    item_mercury_status_to_instance_status(ItemMercuryStatus, InstanceStatus),
    list.foldl2(add_instance_defn(InstanceStatus), Items,
        !ModuleInfo, !Specs),
    add_instance_defns(ImsSubLists, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_typeclass_defn(item_mercury_status::in,
    typeclass_status::in, need_qualifier::in, item_typeclass_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_typeclass_defn(ItemMercuryStatus, TypeClassStatus0, NeedQual,
        ItemTypeClassInfo, !ModuleInfo, !Specs) :-
    ItemTypeClassInfo = item_typeclass_info(ClassName, ClassParamTVars,
        Constraints, FunDeps, Interface, VarSet, Context, _SeqNum),
    module_info_get_class_table(!.ModuleInfo, ClassTable0),
    list.length(ClassParamTVars, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    (
        Interface = class_interface_abstract,
        typeclass_make_status_abstract(TypeClassStatus0, TypeClassStatus1)
    ;
        Interface = class_interface_concrete(_),
        TypeClassStatus1 = TypeClassStatus0
    ),
    HLDSFunDeps = list.map(make_hlds_fundep(ClassParamTVars), FunDeps),
    ( if map.search(ClassTable0, ClassId, OldDefn) then
        OldDefn = hlds_class_defn(OldTypeClassStatus, OldVarSet, _OldKinds,
            OldClassParamTVars, OldConstraints, OldFunDeps, _OldAncestors,
            OldInterface, OldClassMethodPredProcIds0, OldContext,
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
        % Check that the superclass constraints are identical.
        ( if
            constraints_are_identical(OldClassParamTVars, OldVarSet,
                OldConstraints, ClassParamTVars, VarSet, Constraints)
        then
            SuperClassMismatchPieces = []
        else
            SuperClassMismatchPieces =
                [words("The superclass constraints do not match."), nl]
        ),
        % Check that the functional dependencies are identical.
        ( if class_fundeps_are_identical(OldFunDeps, HLDSFunDeps) then
            FunDepsMismatchPieces = []
        else
            FunDepsMismatchPieces =
                [words("The functional dependencies do not match."), nl]
        ),
        MismatchPieces = SuperClassMismatchPieces ++ FunDepsMismatchPieces,
        (
            MismatchPieces = [],
            ( if
                Interface = class_interface_concrete(_),
                OldInterface = class_interface_concrete(_)
            then
                TypeClassStatus = typeclass_status(OldImportStatus),
                % This is a duplicate, but an identical duplicate.
                ( if OldImportStatus = status_opt_imported then
                    true
                else
                    report_multiply_defined("typeclass", ClassName,
                        user_arity(ClassArity), Context, OldContext,
                        [], !Specs)
                ),
                HasIncompatibility = yes(OldDefn)
            else
                HasIncompatibility = no
            )
        ;
            MismatchPieces = [_ | _],
            % This is a duplicate typeclass declaration that specifies
            % different superclasses and/or functional dependencies than
            % the original. Always report such errors, even in `.opt' files.
            report_multiply_defined("typeclass", ClassName,
                user_arity(ClassArity), Context, OldContext,
                MismatchPieces, !Specs),
            HasIncompatibility = yes(OldDefn)
        )
    else
        HasIncompatibility = no,
        OldClassMethodPredProcIds = [],
        ClassInterface = Interface,
        TypeClassStatus = TypeClassStatus1,

        % When we find the class declaration, initialize the list of its
        % instances, so that code processing the instances can just do
        % map.lookups in the instance table once a search in the class table
        % for the class_id has succeeded.
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
            module_declare_class_method_preds(ClassName, ClassParamTVars,
                TypeClassStatus, ItemMercuryStatus, NeedQual,
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
        ClassDefn = hlds_class_defn(TypeClassStatus, VarSet, Kinds,
            ClassParamTVars, Constraints, HLDSFunDeps, Ancestors,
            ClassInterface, ClassMethodPredProcIds, Context,
            has_no_bad_class_defn),
        map.set(ClassId, ClassDefn, ClassTable0, ClassTable),
        module_info_set_class_table(ClassTable, !ModuleInfo)
    ).

:- func make_hlds_fundep(list(tvar), prog_fundep) = hlds_class_fundep.

make_hlds_fundep(TVars, ProgFunDeps) = HLDSFunDeps :-
    ProgFunDeps = fundep(ProgDomain, ProgRange),
    convert_vars_to_arg_posns(TVars, ProgDomain, HLDSDomain),
    convert_vars_to_arg_posns(TVars, ProgRange, HLDSRange),
    HLDSFunDeps = fundep(HLDSDomain, HLDSRange).

:- pred convert_vars_to_arg_posns(list(tvar)::in, list(tvar)::in,
    set(hlds_class_argpos)::out) is det.

convert_vars_to_arg_posns(ProgTVars, HLDSTVars, ArgPosnsSet) :-
    ArgPosns =
        list.map(list.det_index1_of_first_occurrence(ProgTVars), HLDSTVars),
    set.list_to_set(ArgPosns, ArgPosnsSet).

:- pred class_fundeps_are_identical(hlds_class_fundeps::in,
    hlds_class_fundeps::in) is semidet.

class_fundeps_are_identical(OldFunDeps, FunDeps) :-
    % Allow for the functional dependencies to be in a different order.
    sort_and_remove_dups(OldFunDeps, SortedOldFunDeps),
    sort_and_remove_dups(FunDeps, SortedFunDeps),
    % The list elements we are comparing are sets; we rely on the fact that
    % sets have a canonical representation.
    SortedOldFunDeps = SortedFunDeps.

    % Add the item_pred_decl_infos and item_mode_decl_infos in the given
    % list of class_decls to the ModuleInfo, and record their identities
    % in the list of MethodInfos. The methods in MethodInfos will be in
    % the same order as the corresponding item_pred_decl_infos in ClassDecls.
    %
:- pred module_declare_class_method_preds(sym_name::in, list(tvar)::in,
    typeclass_status::in, item_mercury_status::in,
    need_qualifier::in, list(class_decl)::in, list(method_info)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_declare_class_method_preds(ClassName, ClassParamVars, TypeClassStatus,
        ItemMercuryStatus, NeedQual, ClassDecls, MethodInfos,
        !ModuleInfo, !Specs) :-
    % We process ClassDecls in three stages.
    %
    % - In the first stage, classify_class_decls classifies each class_decl
    %   as either pred/func declaration or as a mode declaration. It returns
    %   the pred/func declarations in order, and puts the mode declarations
    %   into a per-pred/func map (ClassModeInfoMap).
    %
    % - In the second stage, add_class_pred_or_func_decl adds to the HLDS,
    %   for each class method, both the item_pred_decl_info of the method,
    %   and any item_mode_decl_infos for it. It also adds the default mode
    %   declarations for functions that have no explicit mode declaration,
    %   and generates an error message for predicates that have no explicit
    %   mode declaration. It deletes from ClassModeInfoMap all the mode
    %   declarations that it has added to the HLDS.
    %
    % - In the third stage, we call report_mode_decls_for_undeclared_method
    %   on all the mode declarations left over from ClassModeInfoMap,
    %   since these represent mode declarations for nonexistent class methods.
    %
    % If ClassDecls declares N predicates and/or functions, then the
    % MethodInfos list we return will contain N contiguous subsequences,
    % each corresponding to one of these predicates or functions.
    % The order of these subsequences will match the order of the
    % predicate and/or function declarations in ClassDecls.
    %
    % Each subsequence will consist of one method_info for each mode
    % declaration in for its method predicate or function ClassDecls,
    % and the order of these method_infos will match the order of those
    % mode declarations in ClassDecls.

    % Stage 1.
    classify_class_decls(ClassDecls, cord.init, ClassPredOrFuncInfosCord,
        map.init, ClassModeInfoMap),
    ClassPredOrFuncInfos = cord.list(ClassPredOrFuncInfosCord),

    % XXX STATUS
    TypeClassStatus = typeclass_status(OldImportStatus),
    PredStatus = pred_status(OldImportStatus),
    % Stage 2.
    list.foldl5(
        add_class_pred_or_func_and_mode_decls(ClassName, ClassParamVars,
            ItemMercuryStatus, PredStatus, NeedQual),
        ClassPredOrFuncInfos, 1, _, cord.init, MethodInfosCord,
        ClassModeInfoMap, UnhandledClassModeInfoMap, !ModuleInfo, !Specs),
    MethodInfos = cord.list(MethodInfosCord),

    % Stage 3.
    map.foldl(report_mode_decls_for_undeclared_method,
        UnhandledClassModeInfoMap, !Specs).

:- pred classify_class_decls(list(class_decl)::in,
    cord(class_pred_or_func_info)::in,
    cord(class_pred_or_func_info)::out,
    map(pred_pf_name_arity, cord(class_mode_info))::in,
    map(pred_pf_name_arity, cord(class_mode_info))::out) is det.

classify_class_decls([], !PredOrFuncInfos, !ModeDeclMap).
classify_class_decls([Decl | Decls], !PredOrFuncInfos, !ModeDeclMap) :-
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        cord.snoc(PredOrFuncInfo, !PredOrFuncInfos)
    ;
        Decl = class_decl_mode(ModeInfo),
        ModeInfo = class_mode_info(PredSymName, MaybePredOrFunc, Modes,
            _WithInst, _MaybeDetism, _InstVarSet, _Context),
        (
            MaybePredOrFunc = no,
            % The only way this could have happened now is if a `with_inst`
            % annotation was not expanded.
            unexpected($pred, "unexpanded `with_inst` annotation")
        ;
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        PredFormArity = arg_list_arity(Modes),
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        MethodPredName =
            pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
        ( if map.search(!.ModeDeclMap, MethodPredName, ProcIdCord0) then
            cord.snoc(ModeInfo, ProcIdCord0, ProcIdCord),
            map.det_update(MethodPredName, ProcIdCord, !ModeDeclMap)
        else
            map.det_insert(MethodPredName, cord.singleton(ModeInfo),
                !ModeDeclMap)
        )
    ),
    classify_class_decls(Decls, !PredOrFuncInfos, !ModeDeclMap).

:- pred add_class_pred_or_func_and_mode_decls(sym_name::in, list(tvar)::in,
    item_mercury_status::in, pred_status::in, need_qualifier::in,
    class_pred_or_func_info::in, int::in, int::out,
    cord(method_info)::in, cord(method_info)::out,
    map(pred_pf_name_arity, cord(class_mode_info))::in,
    map(pred_pf_name_arity, cord(class_mode_info))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_and_mode_decls(ClassName, ClassParamVars,
        ItemMercuryStatus, PredStatus, NeedQual, PredOrFuncInfo,
        !MethodProcNum, !MethodInfosCord, !ModeDeclMap, !ModuleInfo, !Specs) :-
    PredOrFuncInfo = class_pred_or_func_info(PredSymName, PredOrFunc,
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
    ClassId = class_id(ClassName, list.length(ClassParamTypes)),
    PredFormArity = arg_list_arity(ArgTypesAndModes),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    MethodPredName = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
    Origin = compiler_origin_class_method(ClassId, MethodPredName),
    Attrs = item_compiler_attributes(Origin),
    MaybeAttrs = item_origin_compiler(Attrs),
    SeqNum = item_no_seq_num,
    PredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        ArgTypesAndModes, WithType, WithInst, MaybeDetism, MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual, PredDecl,
        MaybePredMaybeProcId, !ModuleInfo, !Specs),
    (
        MaybePredMaybeProcId = no
        % We could not add PredDecl to !ModuleInfo, but module_add_pred_decl
        % will have generated an error message to report the reason.
    ;
        MaybePredMaybeProcId = yes(PredId - MaybeProcId),
        ( if map.remove(MethodPredName, MethodModeDeclsCord, !ModeDeclMap) then
            MethodModeDecls = cord.list(MethodModeDeclsCord)
        else
            MethodModeDecls = []
        ),
        (
            MaybeProcId = no,
            (
                MethodModeDecls = [],
                % If a method has no mode declaration, then
                % - if the method is a function: add the default mode.
                % - if the method is a predicate: report an error.
                module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
                (
                    PredOrFunc = pf_function,
                    maybe_add_default_func_mode(!.ModuleInfo,
                        PredInfo0, PredInfo, MaybeFuncProcId),
                    (
                        MaybeFuncProcId = no,
                        % PredInfo0 was created fresh just above,
                        % without any modes defined.
                        unexpected($pred,
                            "maybe_add_default_func_mode did not add proc")
                    ;
                        MaybeFuncProcId = yes(FuncProcId),
                        module_info_set_pred_info(PredId, PredInfo,
                            !ModuleInfo),
                        PredProcId = proc(PredId, FuncProcId),
                        MethodInfo = method_info(
                            method_proc_num(!.MethodProcNum), MethodPredName,
                            PredProcId, PredProcId),
                        !:MethodProcNum = !.MethodProcNum + 1,
                        cord.snoc(MethodInfo, !MethodInfosCord)
                    )
                ;
                    PredOrFunc = pf_predicate,
                    pred_method_with_no_modes_error(PredInfo0, !Specs)
                )
            ;
                MethodModeDecls = [_ | _],
                list.foldl4(
                    add_class_mode_decl(ItemMercuryStatus, PredStatus,
                        MethodPredName, PredId),
                    MethodModeDecls,
                    !MethodProcNum, !MethodInfosCord, !ModuleInfo, !Specs)
            )
        ;
            MaybeProcId = yes(ProcId),
            % The mode declaration has already been added to the pred_info
            % of the class method in !.ModuleInfo.
            PredProcId = proc(PredId, ProcId),
            MethodInfo = method_info(
                method_proc_num(!.MethodProcNum), MethodPredName,
                PredProcId, PredProcId),
            !:MethodProcNum = !.MethodProcNum + 1,
            cord.snoc(MethodInfo, !MethodInfosCord),
            (
                MethodModeDecls = []
                % The mode is part of the pred_info in !.ModuleInfo, and
                % we added its method_info to !:MethodInfosCord above.
            ;
                MethodModeDecls = [_ | _],
                % Every mode in MethodModeDecls is disallowed by the
                % predmode declaration embedded in PredDecl. We keep
                % the predmode declaration, and discard MethodModeDecls
                % after reporting them.
                ReportBadModeDecl =
                    ( func(MMD) = Spec :-
                        MMD = class_mode_info(_, _, _, _, _, _, MMDContext),
                        Spec = report_mode_decl_after_predmode(MethodPredName,
                            MMDContext)
                    ),
                BadModeDeclSpecs =
                    list.map(ReportBadModeDecl, MethodModeDecls),
                !:Specs = BadModeDeclSpecs ++ !.Specs
            )
        )
    ).

:- pred add_class_mode_decl(item_mercury_status::in, pred_status::in,
    pred_pf_name_arity::in, pred_id::in, class_mode_info::in,
    int::in, int::out, cord(method_info)::in, cord(method_info)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_mode_decl(ItemMercuryStatus, PredStatus, MethodPredName, PredId,
        ModeInfo, !MethodProcNum, !MethodInfosCord, !ModuleInfo, !Specs) :-
    MethodPredName = pred_pf_name_arity(PredOrFunc, PredSymName, _UserArity),
    ModeInfo = class_mode_info(_PredSymName, _MaybePredOrFunc, Modes,
        _WithInst, MaybeDetism, InstVarSet, Context),
    WithInst = maybe.no,
    SeqNum = item_no_seq_num,
    ItemModeDecl = item_mode_decl_info(PredSymName, yes(PredOrFunc), Modes,
        WithInst, MaybeDetism, InstVarSet, Context, SeqNum),
    module_add_mode_decl(not_part_of_predmode, is_a_class_method,
        ItemMercuryStatus, PredStatus, ItemModeDecl, PredProcId,
        !ModuleInfo, !Specs),
    PredProcId = proc(PredPredId, _ProcId),
    expect(unify(PredId, PredPredId), $pred, "pred_id mismatch"),
    MethodInfo = method_info(method_proc_num(!.MethodProcNum),
        MethodPredName, PredProcId, PredProcId),
    !:MethodProcNum = !.MethodProcNum + 1,
    cord.snoc(MethodInfo, !MethodInfosCord).

%-----------------------------------------------------------------------------%

:- pred add_instance_defn(instance_status::in, item_instance_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_instance_defn(InstanceStatus0, ItemInstanceInfo, !ModuleInfo, !Specs) :-
    ItemInstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody0, TVarSet, InstanceModuleName,
        Context, _SeqNum),
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract,
        % XXX This can make the status abstract_imported even if the instance
        % is NOT imported.
        % When this is fixed, please undo the workaround for this bug
        % in instance_used_modules in unused_imports.m.
        instance_make_status_abstract(InstanceStatus0, InstanceStatus)
    ;
        InstanceBody0 = instance_body_concrete(InstanceMethods0),
        list.map(expand_bang_state_pairs_in_instance_method,
            InstanceMethods0, InstanceMethods),
        InstanceBody = instance_body_concrete(InstanceMethods),
        InstanceStatus = InstanceStatus0
    ),

    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_instance_table(!.ModuleInfo, InstanceTable0),
    list.length(Types, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    ( if map.search(Classes, ClassId, _) then
        % The MaybeSubsumedContext is set later, by check_typeclass.m.
        MaybeSubsumedContext = maybe.no,
        MaybeMethodInfos = maybe.no,
        map.init(ProofMap),
        NewInstanceDefn = hlds_instance_defn(InstanceModuleName,
            InstanceStatus, TVarSet, OriginalTypes, Types,
            Constraints, MaybeSubsumedContext, ProofMap,
            InstanceBody, MaybeMethodInfos, Context),
        map.lookup(InstanceTable0, ClassId, OldInstanceDefns),
        map.det_update(ClassId, [NewInstanceDefn | OldInstanceDefns],
            InstanceTable0, InstanceTable),
        module_info_set_instance_table(InstanceTable, !ModuleInfo)
    else
        report_instance_for_undefined_typeclass(ClassId, Context, !Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred pred_method_with_no_modes_error(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_method_with_no_modes_error(PredInfo, !Specs) :-
    pred_info_get_pf_sym_name_arity(PredInfo, PFSNA),
    pred_info_get_context(PredInfo, Context),
    Pieces = [words("Error: no mode declaration for method"),
        unqual_pf_sym_name_pred_form_arity(PFSNA), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_instance_for_undefined_typeclass(class_id::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_instance_for_undefined_typeclass(ClassId, Context, !Specs) :-
    Pieces = [words("Error:"), decl("instance"), words("declaration"),
        words("for"), qual_class_id(ClassId), words("without corresponding"),
        decl("typeclass"), words("declaration."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_mode_decls_for_undeclared_method(pred_pf_name_arity::in,
    cord(class_mode_info)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_mode_decls_for_undeclared_method(MethodPredName, ModeInfosCord,
        !Specs) :-
    ModeInfos = cord.list(ModeInfosCord),
    list.foldl(report_mode_decl_for_undeclared_method(MethodPredName),
        ModeInfos, !Specs).

:- pred report_mode_decl_for_undeclared_method(pred_pf_name_arity::in,
    class_mode_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_mode_decl_for_undeclared_method(MethodPredName, ModeInfo, !Specs) :-
    MethodPredName = pred_pf_name_arity(PredOrFunc, _, _),
    ModeInfo = class_mode_info(_, _, _, _, _, _, Context),
    Pieces = [words("Error: mode declaration for type class method"),
        unqual_pf_sym_name_user_arity(MethodPredName),
        words("without corresponding"), p_or_f(PredOrFunc),
        words("declaration."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_class.
%-----------------------------------------------------------------------------%
