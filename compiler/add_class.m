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
:- import_module parse_tree.error_util.
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
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module maybe.
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
        % Check that the superclass constraints are identical.
        ( if
            constraints_are_identical(OldClassParamVars, OldVarSet,
                OldConstraints, ClassParamVars, VarSet, Constraints)
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
            module_declare_class_method_preds(ClassName, ClassParamVars,
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
        ClassDefn = hlds_class_defn(TypeClassStatus, Constraints, HLDSFunDeps,
            Ancestors, ClassParamVars, Kinds, ClassInterface,
            ClassMethodPredProcIds, VarSet, Context, has_no_bad_class_defn),
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

:- pred module_declare_class_method_preds(sym_name::in, list(tvar)::in,
    typeclass_status::in, item_mercury_status::in,
    need_qualifier::in, list(class_decl)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_declare_class_method_preds(ClassName, ClassParamVars, TypeClassStatus,
        ItemMercuryStatus, NeedQual, ClassDecls, !:PredProcIds,
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
    %
    % XXX zs: This method of communicating "which pred_proc_id is for which
    % method" to check_typeclass.m seems very fragile to me. It seems to me
    % that a map from pred_pf_name_arity to pred_proc_id would be both
    % simpler and more reliable.
    !:PredProcIds = [],

    % XXX STATUS
    TypeClassStatus = typeclass_status(OldImportStatus),
    PredStatus = pred_status(OldImportStatus),
    list.foldl3(
        add_class_pred_or_func_decl(ClassName, ClassParamVars,
            ItemMercuryStatus, PredStatus, NeedQual),
        ClassPredOrFuncInfos, !PredProcIds, !ModuleInfo, !Specs),

    % Add the mode declarations. Since we have already added the
    % predicate/function declarations, there should already be an entry
    % in the predicate table corresponding to the mode we are about to add.
    % If not, report an error.
    list.foldl3(
        add_class_mode_decl(ItemMercuryStatus, PredStatus),
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
    item_mercury_status::in, pred_status::in, need_qualifier::in,
    class_pred_or_func_info::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_decl(ClassName, ClassParamVars,
        ItemMercuryStatus, PredStatus, NeedQual, PredOrFuncInfo,
        !PredProcIds, !ModuleInfo, !Specs) :-
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
    ClassId = class_id(ClassName, list.length(ClassParamTypes)),
    PredFormArity = arg_list_arity(ArgTypesAndModes),
    MethodId = pf_sym_name_arity(PredOrFunc, PredName, PredFormArity),
    Origin = compiler_origin_class_method(ClassId, MethodId),
    Attrs = item_compiler_attributes(Origin),
    MaybeAttrs = item_origin_compiler(Attrs),
    SeqNum = item_no_seq_num,
    PredDecl = item_pred_decl_info(PredName, PredOrFunc,
        ArgTypesAndModes, WithType, WithInst, MaybeDetism, MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum),
    module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual, PredDecl,
        MaybePredProcId, !ModuleInfo, !Specs),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        !:PredProcIds = [PredProcId | !.PredProcIds]
    ).

:- pred add_class_mode_decl(item_mercury_status::in, pred_status::in,
    class_mode_info::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_mode_decl(ItemMercuryStatus, PredStatus, ModeInfo,
        !PredProcIds, !ModuleInfo, !Specs) :-
    ModeInfo = class_mode_info(PredSymName, MaybePredOrFunc, Modes,
        _WithInst, MaybeDetism, InstVarSet, Context),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    PredFormArity = arg_list_arity(Modes),
    (
        MaybePredOrFunc = no,
        % The only way this could have happened now is if a `with_inst`
        % annotation was not expanded.
        unexpected($pred, "unexpanded `with_inst` annotation")
    ;
        MaybePredOrFunc = yes(PredOrFunc)
    ),
    predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
        PredOrFunc, PredSymName, PredFormArity, PredIds),
    (
        PredIds = [],
        missing_pred_or_func_method_error(PredOrFunc, PredSymName,
            PredFormArity, Context, !Specs)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_markers(PredInfo, PredMarkers),
            ( if check_marker(PredMarkers, marker_class_method) then
                WithInst = maybe.no,
                SeqNum = item_no_seq_num,
                ItemModeDecl = item_mode_decl_info(PredSymName,
                    MaybePredOrFunc, Modes, WithInst, MaybeDetism, InstVarSet,
                    Context, SeqNum),
                module_add_mode_decl(not_part_of_predmode, is_a_class_method,
                    ItemMercuryStatus, PredStatus, ItemModeDecl, PredProcId,
                    !ModuleInfo, !Specs),
                !:PredProcIds = [PredProcId | !.PredProcIds]
            else
                % XXX It may also be worth reporting that although there
                % wasn't a matching class method, there was a matching
                % predicate/function.
                missing_pred_or_func_method_error(PredOrFunc, PredSymName,
                    PredFormArity, Context, !Specs)
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
    PredFormArity = arg_list_arity(TypesAndModes),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pf_m_n_a(PredTable, is_fully_qualified,
        PorF, ModuleName, PredOrFuncName, PredFormArity, PredIds),
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

:- pred add_instance_defn(instance_status::in, item_instance_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_instance_defn(InstanceStatus0, ItemInstanceInfo, !ModuleInfo, !Specs) :-
    ItemInstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody0, VarSet, InstanceModuleName,
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
        MaybeMethodPPIds = maybe.no,
        map.init(ProofMap),
        NewInstanceDefn = hlds_instance_defn(InstanceModuleName,
            Types, OriginalTypes, InstanceStatus,
            Context, MaybeSubsumedContext, Constraints,
            InstanceBody, MaybeMethodPPIds, VarSet, ProofMap),
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
    pred_info_get_context(PredInfo, Context),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    SNA = sym_name_arity(qualified(ModuleName, PredName), Arity),
    Pieces = [words("Error: no mode declaration for"),
        words("type class method predicate"), qual_sym_name_arity(SNA),
        suffix("."), nl],
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

:- pred missing_pred_or_func_method_error(pred_or_func::in, sym_name::in,
    pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

missing_pred_or_func_method_error(PredOrFunc, SymName, PredFormArity,
        Context, !Specs) :-
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Error: mode declaration for type class method"),
        qual_sym_name_arity(SNA), words("without corresponding"),
        p_or_f(PredOrFunc), words("method declaration."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_class.
%-----------------------------------------------------------------------------%
