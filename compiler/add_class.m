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

:- import_module check_hlds.
:- import_module check_hlds.clause_to_proc.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
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
    module_info_get_class_table(!.ModuleInfo, Classes0),
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
    ( if map.search(Classes0, ClassId, OldDefn) then
        OldDefn = hlds_class_defn(OldTypeClassStatus, OldConstraints,
            OldFunDeps, _OldAncestors, OldClassParamVars, _OldKinds,
            OldInterface, OldMethods, OldVarSet, OldContext),
        % The typeclass is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        typeclass_combine_status(TypeClassStatus1, OldTypeClassStatus,
            TypeClassStatus),
        (
            OldInterface = class_interface_concrete(_),
            ClassMethods0 = OldMethods,
            ClassInterface = OldInterface
        ;
            OldInterface = class_interface_abstract,
            ClassMethods0 = [],
            ClassInterface = Interface
        ),
        ( if
            % Check that the superclass constraints are identical.
            not constraints_are_identical(OldClassParamVars, OldVarSet,
                OldConstraints, ClassParamVars, VarSet, Constraints)
        then
            % Always report the error, even in `.opt' files.
            Extras = [words("The superclass constraints do not match."), nl],
            multiple_def_error(is_not_opt_imported, ClassName, ClassArity,
                "typeclass", Context, OldContext, Extras, !Specs),
            ErrorOrPrevDef = bool.yes
        else if
            not class_fundeps_are_identical(OldFunDeps, HLDSFunDeps)
        then
            % Always report the error, even in `.opt' files.
            Extras = [words("The functional dependencies do not match."), nl],
            multiple_def_error(is_not_opt_imported, ClassName, ClassArity,
                "typeclass", Context, OldContext, Extras, !Specs),
            ErrorOrPrevDef = bool.yes
        else if
            Interface = class_interface_concrete(_),
            OldInterface = class_interface_concrete(_)
        then
            ( if TypeClassStatus = typeclass_status(status_opt_imported) then
                true
            else
                Extras = [],
                multiple_def_error(is_not_opt_imported, ClassName, ClassArity,
                    "typeclass", Context, OldContext, Extras, !Specs)
            ),
            ErrorOrPrevDef = bool.yes
        else
            ErrorOrPrevDef = bool.no
        ),
        IsNewDefn = bool.no
    else
        IsNewDefn = bool.yes,
        ErrorOrPrevDef = bool.no,
        ClassMethods0 = [],
        ClassInterface = Interface,
        TypeClassStatus = TypeClassStatus1
    ),
    (
        ErrorOrPrevDef = yes
    ;
        ErrorOrPrevDef = no,
        (
            Interface = class_interface_concrete(Methods),
            module_add_class_interface(ClassName, ClassParamVars,
                TypeClassStatus, yes(ItemMercuryStatus), NeedQual,
                Methods, PredProcIds0, !ModuleInfo, !Specs),
            % Get rid of the `no's from the list of maybes, and convert
            % from pairs to hlds_class_procs.
            IsYes =
                ( pred(Maybe::in, PredProcId::out) is semidet :-
                    Maybe = yes(PredId - ProcId),
                    PredProcId = hlds_class_proc(PredId, ProcId)
                ),
            list.filter_map(IsYes, PredProcIds0, PredProcIds1),

            % The list must be sorted on pred_id and then proc_id --
            % check_typeclass.m assumes this when it is generating the
            % corresponding list of pred_proc_ids for instance definitions.
            list.sort(PredProcIds1, ClassMethods)
        ;
            Interface = class_interface_abstract,
            ClassMethods = ClassMethods0
        ),

        % Ancestors is not set until check_typeclass.
        Ancestors = [],
        % XXX kind inference:
        % We set all the kinds to `star' at the moment. This should be
        % done differently when we have a proper kind system.
        Kinds = map.init,
        ClassDefn = hlds_class_defn(TypeClassStatus, Constraints, HLDSFunDeps,
            Ancestors, ClassParamVars, Kinds, ClassInterface, ClassMethods,
            VarSet, Context),
        map.set(ClassId, ClassDefn, Classes0, Classes),
        module_info_set_class_table(Classes, !ModuleInfo),

        (
            IsNewDefn = yes,
            % When we find the class declaration, make an entry
            % for the instances.
            module_info_get_instance_table(!.ModuleInfo, Instances0),
            map.det_insert(ClassId, [], Instances0, Instances),
            module_info_set_instance_table(Instances, !ModuleInfo)
        ;
            IsNewDefn = no
        )
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
    unexpected($module, $pred, "element not found").
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
    need_qualifier::in, list(class_method)::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_class_interface(ClassName, ClassParamVars, TypeClassStatus,
        MaybeItemMercuryStatus, NeedQual, Methods, PredProcIds,
        !ModuleInfo, !Specs) :-
    list.filter(is_class_method_mode_item, Methods, ModeMethods,
        PredOrFuncMethods),
    some [!PPIds] (
        add_class_pred_or_func_methods(ClassName, ClassParamVars,
            MaybeItemMercuryStatus, TypeClassStatus, NeedQual,
            PredOrFuncMethods, !:PPIds, !ModuleInfo, !Specs),

        % Add the pred_or_func_mode decls. Since we have already added the
        % predicate/function method decls, there should already be an entry in
        % the predicate table corresponding to the mode item we are about to
        % add. If not, report an error.
        ItemNumber = -1,
        list.foldl3(
            add_class_pred_or_func_mode_method(ClassName, ClassParamVars,
                ItemNumber, MaybeItemMercuryStatus, TypeClassStatus, NeedQual),
            ModeMethods, !PPIds, !ModuleInfo, !Specs),
        check_method_modes(Methods, !.PPIds, PredProcIds, !ModuleInfo, !Specs)
    ).

:- pred is_class_method_mode_item(class_method::in) is semidet.

is_class_method_mode_item(Method) :-
    Method = method_pred_or_func_mode(_, _, _, _, _, _, _).

:- pred add_class_pred_or_func_mode_method(sym_name::in, list(tvar)::in,
    int::in, maybe(item_mercury_status)::in, typeclass_status::in,
    need_qualifier::in, class_method::in,
    list(maybe(pair(pred_id, proc_id)))::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_mode_method(ClassName, ClassParamVars,
        ItemNumber, MaybeItemMercuryStatus, TypeClassStatus, NeedQual, Method,
        !PredProcIds, !ModuleInfo, !Specs) :-
    (
        Method = method_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _),
        unexpected($module, $pred, "pred_or_func method item")
    ;
        Method = method_pred_or_func_mode(PredName, MaybePredOrFunc, Modes,
            _WithInst, _MaybeDetism, _VarSet, Context)
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    PredArity = list.length(Modes) : int,
    (
        MaybePredOrFunc = no,
        % The only way this could have happened now is if a `with_inst`
        % annotation was not expanded.
        unexpected($module, $pred, "unexpanded `with_inst` annotation")
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
                module_add_class_method(ClassName, ClassParamVars,
                    ItemNumber, MaybeItemMercuryStatus, TypeClassStatus,
                    NeedQual, Method, PredProcId, !ModuleInfo, !Specs),
                list.cons(PredProcId, !PredProcIds)
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
            unexpected($module, $pred, "multiple preds matching method mode")
        )
    ).

:- pred add_class_pred_or_func_methods(sym_name::in, list(tvar)::in,
    maybe(item_mercury_status)::in, typeclass_status::in, need_qualifier::in,
    list(class_method)::in, list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_methods(_, _, _, _, _, [], [], !ModuleInfo, !Specs).
add_class_pred_or_func_methods(ClassName, ClassParamVars,
        MaybeItemMercuryStatus, TypeClassStatus, NeedQual, [Method | Methods],
        [MaybePredProcId | MaybePredProcIds], !ModuleInfo, !Specs) :-
    ItemNumber = -1,
    module_add_class_method(ClassName, ClassParamVars,
        ItemNumber, MaybeItemMercuryStatus, TypeClassStatus, NeedQual, Method,
        MaybePredProcId, !ModuleInfo, !Specs),
    add_class_pred_or_func_methods(ClassName, ClassParamVars,
        MaybeItemMercuryStatus, TypeClassStatus, NeedQual, Methods,
        MaybePredProcIds, !ModuleInfo, !Specs).

:- pred module_add_class_method(sym_name::in, list(tvar)::in,
    int::in, maybe(item_mercury_status)::in, typeclass_status::in,
    need_qualifier::in, class_method::in, maybe(pair(pred_id, proc_id))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_class_method(ClassName, ClassParamVars,
        ItemNumber, MaybeItemMercuryStatus, TypeClassStatus, NeedQual, Method,
        MaybePredIdProcId, !ModuleInfo, !Specs) :-
    % XXX STATUS
    TypeClassStatus = typeclass_status(OldImportStatus),
    PredStatus = pred_status(OldImportStatus),
    (
        Method = method_pred_or_func(PredName, PredOrFunc, TypesAndModes,
            WithType, WithInst, MaybeDetism, TypeVarSet, InstVarSet,
            ExistQVars, Purity, Constraints0, Context),
        % Any WithType and WithInst annotations should have been expanded
        % and the type and/or inst put into TypesAndModes by equiv_type.m.
        expect(unify(WithType, no), $module, $pred, "WithType != no"),
        expect(unify(WithInst, no), $module, $pred, "WithInst != no"),
        % XXX This setting of Origin looks suspicious.
        Origin = origin_user(PredName),
        % XXX kind inference:
        % We set the kinds to `star' at the moment. This will be different
        % when we have a kind system.
        prog_type.var_list_to_type_list(map.init, ClassParamVars,
            ClassParamTypes),
        Constraints0 = constraints(UnivConstraints0, ExistConstraints),
        UnivConstraints = [constraint(ClassName, ClassParamTypes)
            | UnivConstraints0],
        Constraints = constraints(UnivConstraints, ExistConstraints),
        init_markers(Markers0),
        add_marker(marker_class_method, Markers0, Markers),
        module_add_pred_or_func(Origin, Context, ItemNumber,
            MaybeItemMercuryStatus, PredStatus, NeedQual,
            PredOrFunc, PredName, TypeVarSet, InstVarSet, ExistQVars,
            TypesAndModes, Constraints, MaybeDetism, Purity, Markers,
            MaybePredIdProcId, !ModuleInfo, !Specs)
    ;
        Method = method_pred_or_func_mode(PredName, MaybePredOrFunc, Modes,
            _WithInst, MaybeDet, VarSet, Context),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            module_add_mode(Context, ItemNumber,
                MaybeItemMercuryStatus, PredStatus,
                PredOrFunc, PredName, VarSet, Modes, MaybeDet,
                is_a_class_method, PredIdProcId, !ModuleInfo, !Specs),
            MaybePredIdProcId = yes(PredIdProcId)
        ;
            MaybePredOrFunc = no,
            % equiv_type.m should have either set the
            % pred_or_func or removed the item from the list.
            unexpected($module, $pred, "no pred_or_func on mode declaration")
        )
    ).

    % Go through the list of class methods, looking for
    % - functions without mode declarations: add a default mode
    % - predicates without mode declarations: report an error
    % - mode declarations with no determinism: report an error
    %
:- pred check_method_modes(class_methods::in,
    list(maybe(pair(pred_id, proc_id)))::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_method_modes([], !PredProcIds, !ModuleInfo, !Specs).
check_method_modes([Method | Methods], !PredProcIds, !ModuleInfo, !Specs) :-
    (
        Method = method_pred_or_func(QualPredOrFuncName, PorF, TypesAndModes,
            _, _, _, _, _, _, _, _, _),
        (
            QualPredOrFuncName = qualified(ModuleName, PredOrFuncName)
        ;
            QualPredOrFuncName = unqualified(_),
            % The class interface should be fully module qualified
            % by the parser at the time it is read in.
            unexpected($module, $pred, "unqualified func")
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
                maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProc),
                (
                    MaybeProc = no
                ;
                    MaybeProc = yes(ProcId),
                    NewPredProc = yes(PredId - ProcId),
                    !:PredProcIds = [NewPredProc | !.PredProcIds],
                    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
                ) ;
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
            unexpected($module, $pred, "number of preds != 1")
        )
    ;
        Method = method_pred_or_func_mode(_, _, _, _, _, _, _)
    ),
    check_method_modes(Methods, !PredProcIds, !ModuleInfo, !Specs).

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
        ContextA = InstanceDefnA ^ instdefn_context,

        PiecesA = [words("In instance declaration for class "),
            qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)), nl,
            words("instance constraints are incompatible with")],
        MsgA = simple_msg(ContextA, [always(PiecesA)]),

        ContextB = InstanceDefnB ^ instdefn_context,
        PiecesB = [words("instance constraints here.")],
        MsgB = simple_msg(ContextB, [always(PiecesB)]),

        Spec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [MsgA, MsgB]),
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
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),
        set_of_var.list_to_set(HeadVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo1, GoalInfo2),
        ( if check_marker(Markers, marker_is_impure) then
            goal_info_set_purity(purity_impure, GoalInfo2, GoalInfo)
        else if check_marker(Markers, marker_is_semipure) then
            goal_info_set_purity(purity_semipure, GoalInfo2, GoalInfo)
        else
            GoalInfo = GoalInfo2
        ),
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
        HasForeignClauses = no,
        HadSyntaxErrors = no,
        ClausesInfo = clauses_info(VarSet, TVarNameMap, VarTypes, VarTypes,
            HeadVarVec, ClausesRep, init_clause_item_numbers_comp_gen,
            RttiVarMaps, HasForeignClauses, HadSyntaxErrors)
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
    expect(unify(PredOrFunc, ClausePredOrFunc), $module, $pred,
        "PredOrFunc mismatch"),
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
