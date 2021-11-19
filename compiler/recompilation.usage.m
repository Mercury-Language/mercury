%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2012 University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompilation.usage.m.
% Main author: stayl.
%
% Write the file recording which imported items were used by a compilation.
%
%---------------------------------------------------------------------------%

:- module recompilation.usage.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module parse_tree.
:- import_module parse_tree.module_imports.
:- import_module recompilation.used_file.

:- pred construct_used_file_contents(module_info::in, recompilation_info::in,
    maybe_top_module::in, module_timestamp_map::in,
    used_file_contents::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

construct_used_file_contents(ModuleInfo, RecompInfo, MaybeTopModule,
        TimestampMap, Contents) :-
    % Go over the set of imported items found to be used and
    % find the transitive closure of the imported items they use.

    % We need to make sure each visible module has an entry in the `.used'
    % file, even if nothing was used from it. This will cause
    % recompilation_check.m to check for new items causing ambiguity
    % when the interface of the module changes.
    module_info_get_visible_modules(ModuleInfo, AllVisibleModules),
    module_info_get_name(ModuleInfo, ModuleName),
    set.delete(ModuleName, AllVisibleModules, ImportedVisibleModules),

    map.init(ImportedItems0),
    set.foldl(insert_into_imported_items_map, ImportedVisibleModules,
        ImportedItems0, ImportedItems1),

    queue.init(ItemsToProcess0),
    map.init(ModuleUsedClasses),
    set.init(UsedClasses0),

    UsedItems = RecompInfo ^ recomp_used_items,
    UsedItems = used_items(TypeNames, TypeDefns, Insts, Modes, Classes,
        _, _, _),
    map.init(ResolvedCtors),
    map.init(ResolvedPreds),
    map.init(ResolvedFuncs),
    ResolvedUsedItems0 = resolved_used_items(TypeNames, TypeDefns,
        Insts, Modes, Classes, ResolvedCtors, ResolvedPreds, ResolvedFuncs),

    Dependencies = RecompInfo ^ recomp_dependencies,
    Info0 = recompilation_usage_info(ModuleInfo, ItemsToProcess0,
        ImportedItems1, ModuleUsedClasses, Dependencies,
        ResolvedUsedItems0, UsedClasses0),

    find_all_used_imported_items(UsedItems, Info0, Info),

    ImportedItems = Info ^ imported_items,
    ModuleInstances = Info ^ module_instances,
    UsedTypeClasses = Info ^ used_typeclasses,
    ResolvedUsedItems = Info ^ resolved_used_items,

    ModuleItemVersionNumbersMap = RecompInfo ^ recomp_version_numbers,
    Contents = used_file_contents(ModuleName, MaybeTopModule, TimestampMap,
        ModuleItemVersionNumbersMap,
        ResolvedUsedItems, UsedTypeClasses, ImportedItems, ModuleInstances).

%---------------------------------------------------------------------------%

:- pred insert_into_imported_items_map(module_name::in,
    imported_items::in, imported_items::out) is det.

insert_into_imported_items_map(VisibleModule, !ImportedItemsMap) :-
    ModuleItems = module_imported_items(set.init, set.init, set.init,
        set.init, set.init, set.init, set.init,set.init),
    % Use map.set rather than map.det_insert as this routine may be called
    % multiple times with the same VisibleModule, for example if the module
    % is both imported and an ancestor module.
    map.set(VisibleModule, ModuleItems, !ImportedItemsMap).

%---------------------------------------------------------------------------%

:- type recompilation_usage_info
    --->    recompilation_usage_info(
                module_info         :: module_info,
                item_queue          :: queue(item_id),
                imported_items      :: imported_items,
                module_instances    :: map(module_name, set(item_name)),
                                    % For each module, the used typeclasses for
                                    % which the module contains an instance.
                dependencies        :: map(item_id, set(item_id)),
                resolved_used_items :: resolved_used_items,
                used_typeclasses    :: set(item_name)
            ).

%---------------------------------------------------------------------------%

:- pred find_all_used_imported_items(used_items::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_all_used_imported_items(UsedItems, !Info) :-
    % Find items used by imported instances for local classes.
    ModuleInfo = !.Info ^ module_info,
    module_info_get_instance_table(ModuleInfo, Instances),
    map.foldl(find_items_used_by_instances, Instances, !Info),

    UsedItems = used_items(TypeNames, TypeDefns, Insts, Modes, Classes,
        Functors, Predicates, Functions),
    find_items_used_by_simple_item_set(type_name_item, TypeNames, !Info),
    find_items_used_by_simple_item_set(type_defn_item, TypeDefns, !Info),
    find_items_used_by_simple_item_set(inst_item, Insts, !Info),
    find_items_used_by_simple_item_set(mode_item, Modes, !Info),
    find_items_used_by_simple_item_set(typeclass_item, Classes, !Info),
    find_items_used_by_preds(pf_predicate, Predicates, !Info),
    find_items_used_by_preds(pf_function, Functions, !Info),
    find_items_used_by_functors(Functors, !Info),

    process_imported_item_to_fixpoint(!Info).

:- pred process_imported_item_to_fixpoint(
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

process_imported_item_to_fixpoint(!Info) :-
    Queue0 = !.Info ^ item_queue,
    !Info ^ item_queue := queue.init,
    process_imported_items_in_queue(Queue0, !Info),
    Queue = !.Info ^ item_queue,
    ( if queue.is_empty(Queue) then
        true
    else
        disable_warning [suspicious_recursion] (
            process_imported_item_to_fixpoint(!Info)
        )
    ).

:- pred process_imported_items_in_queue(queue(item_id)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

process_imported_items_in_queue(!.Queue, !Info) :-
    ( if queue.get(Item, !Queue) then
        Item = item_id(ItemType, ItemId),
        find_items_used_by_item(ItemType, ItemId, !Info),
        disable_warning [suspicious_recursion] (
            process_imported_items_in_queue(!.Queue, !Info)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred record_used_pred_or_func(pred_or_func::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_used_pred_or_func(PredOrFunc, Id, !Info) :-
    Id = item_name(SymName, Arity),
    UsedItems0 = !.Info ^ resolved_used_items,
    (
        PredOrFunc = pf_predicate,
        IdSet0 = UsedItems0 ^ rui_predicates,
        record_resolved_item(SymName, Arity,
            do_record_used_pred_or_func(PredOrFunc),
            IdSet0, IdSet, !Info),
        UsedItems = UsedItems0 ^ rui_predicates := IdSet
    ;
        PredOrFunc = pf_function,
        IdSet0 = UsedItems0 ^ rui_functions,
        record_resolved_item(SymName, Arity,
            do_record_used_pred_or_func(PredOrFunc),
            IdSet0, IdSet, !Info),
        UsedItems = UsedItems0 ^ rui_functions := IdSet
    ),
    !Info ^ resolved_used_items := UsedItems.

:- pred do_record_used_pred_or_func(pred_or_func::in,
    module_qualifier::in, sym_name::in, arity::in, bool::out,
    resolved_pred_or_func_map::in, resolved_pred_or_func_map::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

do_record_used_pred_or_func(PredOrFunc, ModuleQualifier,
        SymName, Arity, Recorded, !MatchingNames, !Info) :-
    ModuleInfo = !.Info ^ module_info,
    module_info_get_predicate_table(ModuleInfo, PredTable),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    predicate_table_lookup_pf_sym_arity(PredTable, may_be_partially_qualified,
        PredOrFunc, SymName, OrigArity, MatchingPredIds),
    (
        MatchingPredIds = [_ | _],
        Recorded = yes,
        PredModules = set.list_to_set(list.map(
            ( func(PredId) = PredId - PredModule :-
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                PredModule = pred_info_module(PredInfo)
            ),
            MatchingPredIds)),
        map.det_insert(ModuleQualifier, PredModules, !MatchingNames),
        NameArity = name_arity(unqualify_name(SymName), Arity),
        set.fold(find_items_used_by_pred(PredOrFunc, NameArity),
            PredModules, !Info)
    ;
        MatchingPredIds = [],
        Recorded = no
    ).

%---------------------------------------------------------------------------%

:- pred record_used_functor(pair(sym_name, arity)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_used_functor(SymName - Arity, !Info) :-
    UsedItems0 = !.Info ^ resolved_used_items,
    IdSet0 = UsedItems0 ^ rui_functors,
    record_resolved_item(SymName, Arity, do_record_used_functor,
        IdSet0, IdSet, !Info),
    UsedItems = UsedItems0 ^ rui_functors := IdSet,
    !Info ^ resolved_used_items := UsedItems.

:- pred do_record_used_functor(module_qualifier::in,
    sym_name::in, arity::in, bool::out, resolved_functor_map::in,
    resolved_functor_map::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

do_record_used_functor(ModuleQualifier, SymName, Arity, Recorded,
        !ResolvedCtorMap, !Info) :-
    ModuleInfo = !.Info ^ module_info,

    find_matching_functors(ModuleInfo, SymName, Arity, MatchingCtors),
    Name = unqualify_name(SymName),
    set.fold(find_items_used_by_functor(Name, Arity), MatchingCtors, !Info),

    ( if set.is_empty(MatchingCtors) then
        Recorded = no
    else
        Recorded = yes,
        map.det_insert(ModuleQualifier, MatchingCtors, !ResolvedCtorMap)
    ).

:- pred find_matching_functors(module_info::in,
    sym_name::in, arity::in, set(resolved_functor)::out) is det.

find_matching_functors(ModuleInfo, SymName, Arity, ResolvedConstructors) :-
    % Is it a constructor.
    module_info_get_cons_table(ModuleInfo, Ctors),
    ConsId = cons(SymName, Arity, cons_id_dummy_type_ctor),
    ( if search_cons_table(Ctors, ConsId, ConsDefns0) then
        ConsDefns1 = ConsDefns0
    else
        ConsDefns1 = []
    ),
    ( if
        remove_new_prefix(SymName, SymNameMinusNew),
        ConsIdMinusNew = cons(SymNameMinusNew, Arity, cons_id_dummy_type_ctor),
        search_cons_table(Ctors, ConsIdMinusNew, ConsDefns2)
    then
        ConsDefns = ConsDefns1 ++ ConsDefns2
    else
        ConsDefns = ConsDefns1
    ),
    MatchingConstructorRFs =
        list.map(
            ( func(ConsDefn) = Ctor :-
                ConsDefn ^ cons_type_ctor = TypeCtor,
                Ctor = resolved_functor_data_constructor(TypeCtor)
            ),
            ConsDefns),

    % Is it a higher-order term or function call.
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_lookup_sym(PredicateTable,
        may_be_partially_qualified, SymName, PredIds),
    list.filter_map(
        can_resolve_pred_or_func(ModuleInfo, SymName, Arity),
        PredIds, MatchingPredRFs),

    % Is it a field access function.
    ( if
        is_field_access_function_name(ModuleInfo, SymName, Arity,
            _, FieldName),
        module_info_get_ctor_field_table(ModuleInfo, CtorFields),
        map.search(CtorFields, FieldName, FieldDefns)
    then
        MatchingFieldAccessRFs = list.map(
            ( func(FieldDefn) = FieldAccessRF :-
                FieldDefn =
                    hlds_ctor_field_defn(_, _, TypeCtor, FieldConsId, _),
                ( if FieldConsId = cons(ConsName, ConsArity, _) then
                    ConsCtor = cons_ctor(ConsName, ConsArity, TypeCtor),
                    FieldAccessRF = resolved_functor_field_access_func(ConsCtor)
                else
                    unexpected($pred, "weird cons_id in hlds_field_defn")
                )
            ), FieldDefns)
    else
        MatchingFieldAccessRFs = []
    ),
    ResolvedConstructors = set.list_to_set(list.condense(
        [MatchingConstructorRFs, MatchingPredRFs, MatchingFieldAccessRFs])).

:- pred can_resolve_pred_or_func(module_info::in, sym_name::in, arity::in,
    pred_id::in, resolved_functor::out) is semidet.

can_resolve_pred_or_func(ModuleInfo, _SymName, Arity, PredId, ResolvedCtor) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
    adjust_func_arity(PredOrFunc, OrigArity, PredArity),
    (
        PredOrFunc = pf_predicate,
        OrigArity >= Arity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified predicate.
        PredExistQVars = []
    ;
        PredOrFunc = pf_function,
        OrigArity >= Arity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; OrigArity = Arity
        )
    ),
    ResolvedCtor = resolved_functor_pred_or_func(PredId, PredOrFunc,
        PredModule, pred_form_arity(OrigArity)).

%---------------------------------------------------------------------------%

:- type record_resolved_item(T) ==
    pred(module_qualifier, sym_name, arity, bool,
        resolved_item_map(T), resolved_item_map(T),
        recompilation_usage_info, recompilation_usage_info).
:- inst record_resolved_item ==
    (pred(in, in, in, out, in, out, in, out) is det).

:- pred record_resolved_item(sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item),
    resolved_item_set(T)::in, resolved_item_set(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item(SymName, Arity, RecordItem, !IdSet, !Info) :-
    UnqualifiedName = unqualify_name(SymName),
    ModuleQualifier = find_module_qualifier(SymName),
    ( if map.search(!.IdSet, UnqualifiedName, MatchingNames0) then
        MatchingNames1 = MatchingNames0
    else
        MatchingNames1 = []
    ),
    record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem,
        Recorded, MatchingNames1, MatchingNames, !Info),
    (
        Recorded = yes,
        map.set(UnqualifiedName, MatchingNames, !IdSet)
    ;
        Recorded = no
    ).

:- pred record_resolved_item_2(module_qualifier::in, sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item), bool::out,
    resolved_item_list(T)::in, resolved_item_list(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem, Recorded,
        !List, !Info) :-
    !.List = [],
    map.init(Map0),
    record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
        Recorded, Map0, Map, !Info),
    (
        Recorded = yes,
        !:List = [Arity - Map]
    ;
        Recorded = no
    ).
record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem, Recorded,
        !List, !Info) :-
    !.List = [ThisArity - ArityMap0 | ListRest0],
    ( if Arity < ThisArity then
        map.init(NewArityMap0),
        record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, NewArityMap0, NewArityMap, !Info),
        (
            Recorded = yes,
            !:List = [Arity - NewArityMap | !.List]
        ;
            Recorded = no
        )
    else if Arity = ThisArity then
        record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, ArityMap0, ArityMap, !Info),
        (
            Recorded = yes,
            !:List = [Arity - ArityMap | ListRest0]
        ;
            Recorded = no
        )
    else
        record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, ListRest0, ListRest, !Info),
        (
            Recorded = yes,
            !:List = [ThisArity - ArityMap0 | ListRest]
        ;
            Recorded = no
        )
    ).

:- pred record_resolved_item_3(module_qualifier::in, sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item), bool::out,
    resolved_item_map(T)::in, resolved_item_map(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem, Recorded,
        !ResolvedMap, !Info) :-
    ( if map.contains(!.ResolvedMap, ModuleQualifier) then
        Recorded = no
    else
        RecordItem(ModuleQualifier, SymName, Arity, Recorded,
            !ResolvedMap, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred find_items_used_by_item(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_item(ItemType, ItemName, !Info) :-
    (
        ItemType = type_name_item,
        ModuleInfo = !.Info ^ module_info,
        module_info_get_type_table(ModuleInfo, TypeTable),
        TypeCtor = item_name_to_type_ctor(ItemName),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        ( if TypeBody = hlds_eqv_type(Type) then
            % If we use an equivalence type we also use the type
            % it is equivalent to.
            find_items_used_by_type(Type, !Info)
        else
            true
        )
    ;
        ItemType = type_defn_item,
        ModuleInfo = !.Info ^ module_info,
        module_info_get_type_table(ModuleInfo, TypeTable),
        TypeCtor = item_name_to_type_ctor(ItemName),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        find_items_used_by_type_body(TypeBody, !Info)
    ;
        ItemType = inst_item,
        ModuleInfo = !.Info ^ module_info,
        module_info_get_inst_table(ModuleInfo, Insts),
        inst_table_get_user_insts(Insts, UserInstTable),
        InstCtor = item_name_to_inst_ctor(ItemName),
        map.lookup(UserInstTable, InstCtor, InstDefn),
        find_items_used_by_inst_defn(InstDefn, !Info)
    ;
        ItemType = mode_item,
        ModuleInfo = !.Info ^ module_info,
        module_info_get_mode_table(ModuleInfo, Modes),
        mode_table_get_mode_defns(Modes, ModeDefns),
        ModeCtor = item_name_to_mode_ctor(ItemName),
        map.lookup(ModeDefns, ModeCtor, ModeDefn),
        find_items_used_by_mode_defn(ModeDefn, !Info)
    ;
        ItemType = typeclass_item,
        ItemName = item_name(ClassName, ClassArity),
        ClassId = class_id(ClassName, ClassArity),
        ModuleInfo = !.Info ^ module_info,
        module_info_get_class_table(ModuleInfo, Classes),
        map.lookup(Classes, ClassId, ClassDefn),
        Constraints = ClassDefn ^ classdefn_supers,
        ClassInterface = ClassDefn ^ classdefn_interface,
        find_items_used_by_class_constraints(Constraints, !Info),
        (
            ClassInterface = class_interface_abstract
        ;
            ClassInterface = class_interface_concrete(ClassDecls),
            list.foldl(find_items_used_by_class_decl, ClassDecls, !Info)
        ),
        module_info_get_instance_table(ModuleInfo, Instances),
        ( if map.search(Instances, ClassId, InstanceDefns) then
            list.foldl(find_items_used_by_instance(ItemName), InstanceDefns,
                !Info)
        else
            true
        )
    ;
        ItemType = predicate_item,
        record_used_pred_or_func(pf_predicate, ItemName, !Info)
    ;
        ItemType = function_item,
        record_used_pred_or_func(pf_function, ItemName, !Info)
    ;
        ItemType = functor_item,
        unexpected($pred, "functor")
    ;
        ( ItemType = mutable_item
        ; ItemType = foreign_proc_item
        )
        % XXX What should be done here???
        % Mutables are expanded into other item types which track the
        % types, insts, preds, and funcs used.
    ).

:- pred find_items_used_by_instances(class_id::in,
    list(hlds_instance_defn)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_instances(ClassId, InstanceDefns, !Info) :-
    ClassId = class_id(Name, Arity),
    ClassIdItem = item_name(Name, Arity),
    ( if item_is_local(!.Info, ClassIdItem) then
        record_expanded_items_used_by_item(typeclass_item, ClassIdItem, !Info),
        list.foldl(find_items_used_by_instance(ClassIdItem), InstanceDefns,
            !Info)
    else
        true
    ).

:- pred find_items_used_by_instance(item_name::in, hlds_instance_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_instance(ClassId, Defn, !Info) :-
    % XXX Should we process OriginalArgTypes as we do ArgTypes?
    Defn = hlds_instance_defn(InstanceModuleName, ArgTypes, _OriginalArgTypes,
        _, _, Constraints, _, _, _, _),
    % XXX Handle interface (currently not needed because the interfaces
    % for imported instances are only needed with --intermodule-optimization,
    % which isn't handled here yet).
    ModuleInfo = !.Info ^ module_info,
    ( if module_info_get_name(ModuleInfo, InstanceModuleName) then
        true
    else
        find_items_used_by_class_constraints(Constraints, !Info),
        find_items_used_by_types(ArgTypes, !Info),
        ModuleInstances0 = !.Info ^ module_instances,
        ( if
            map.search(ModuleInstances0, InstanceModuleName, ClassIdsPrime)
        then
            ClassIds1 = ClassIdsPrime
        else
            set.init(ClassIds1)
        ),
        set.insert(ClassId, ClassIds1, ClassIds),
        map.set(InstanceModuleName, ClassIds,
            ModuleInstances0, ModuleInstances),
        !Info ^ module_instances := ModuleInstances
    ).

:- pred find_items_used_by_class_decl(class_decl::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_decl(Decl, !Info) :-
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(_, _, ArgTypesAndModes,
            _, _, _, _, _, _, _, Constraints, _),
        find_items_used_by_class_context(Constraints, !Info),
        list.foldl(find_items_used_by_type_and_mode, ArgTypesAndModes, !Info)
    ;
        Decl = class_decl_mode(ModeInfo),
        ModeInfo = class_mode_info(_, _, Modes, _, _, _, _),
        find_items_used_by_modes(Modes, !Info)
    ).

:- pred find_items_used_by_type_and_mode(type_and_mode::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_and_mode(TypeAndMode, !Info) :-
    (
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode = type_and_mode(Type, Mode),
        find_items_used_by_mode(Mode, !Info)
    ),
    find_items_used_by_type(Type, !Info).

:- pred find_items_used_by_type_body(hlds_type_body::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_body(TypeBody, !Info) :-
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(Ctors, MaybeSuperType, _, _, _),
        (
            MaybeSuperType = subtype_of(SuperType),
            find_items_used_by_type(SuperType, !Info)
        ;
            MaybeSuperType = not_a_subtype
        ),
        list.foldl(find_items_used_by_ctor, one_or_more_to_list(Ctors), !Info)
    ;
        TypeBody = hlds_eqv_type(EqvType),
        find_items_used_by_type(EqvType, !Info)
    ;
        ( TypeBody = hlds_abstract_type(_)
        ; TypeBody = hlds_foreign_type(_)
        )
    ;
        TypeBody = hlds_solver_type(_)
        % rafe: XXX Should we trace the representation type?
    ).

:- pred find_items_used_by_ctor(constructor::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_ctor(Ctor, !Info) :-
    Ctor = ctor(_, MaybeExistConstraints, _, CtorArgs, _, _),
    (
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(_, Constraints, _, _),
        find_items_used_by_class_constraints(Constraints, !Info)
    ),
    list.foldl(find_items_used_by_ctor_arg, CtorArgs, !Info).

:- pred find_items_used_by_ctor_arg(constructor_arg::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_ctor_arg(CtorArg, !Info) :-
    ArgType = CtorArg ^ arg_type,
    find_items_used_by_type(ArgType, !Info).

:- pred find_items_used_by_mode_defn(hlds_mode_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_mode_defn(Defn, !Info) :-
    Defn = hlds_mode_defn(_, _, hlds_mode_body(Mode), _, _),
    find_items_used_by_mode(Mode, !Info).

:- pred find_items_used_by_inst_defn(hlds_inst_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst_defn(Defn, !Info) :-
    Defn = hlds_inst_defn(_, _, InstBody, IFTC, _, _),
    InstBody = eqv_inst(Inst),
    find_items_used_by_inst(Inst, !Info),
    (
        IFTC = iftc_applicable_declared(ForTypeCtor),
        find_items_used_by_type_ctor(ForTypeCtor, !Info)
    ;
        IFTC = iftc_applicable_known(MatchingTypeCtors),
        list.foldl(find_items_used_by_type_ctor, MatchingTypeCtors, !Info)
    ;
        ( IFTC = iftc_applicable_not_known
        ; IFTC = iftc_applicable_error
        ; IFTC = iftc_not_applicable
        )
    ).

:- pred find_items_used_by_preds(pred_or_func::in, simple_item_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds(PredOrFunc, Set, !Info) :-
    map.foldl(find_items_used_by_preds_2(PredOrFunc), Set, !Info).

:- pred find_items_used_by_preds_2(pred_or_func::in,
    name_arity::in, map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds_2(PredOrFunc, NameArity, MatchingPredMap, !Info) :-
    NameArity = name_arity(Name, Arity),
    map.foldl(find_items_used_by_preds_3(
        PredOrFunc, Name, Arity), MatchingPredMap, !Info).

:- pred find_items_used_by_preds_3(pred_or_func::in,
    string::in, arity::in, module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds_3(PredOrFunc, Name, Arity, ModuleQualifier, _,
        !Info) :-
    SymName = module_qualify_name(ModuleQualifier, Name),
    record_used_pred_or_func(PredOrFunc, item_name(SymName, Arity), !Info).

:- pred find_items_used_by_pred(pred_or_func::in,
    name_arity::in, pair(pred_id, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_pred(PredOrFunc, NameArity, PredId - PredModule, !Info) :-
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    ModuleInfo = !.Info ^ module_info,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    NameArity = name_arity(Name, Arity),
    ( if
        ItemName = item_name(qualified(PredModule, Name), Arity),
        (
            item_is_recorded_used(!.Info, ItemType, ItemName)
        ;
            item_is_local(!.Info, ItemName)
        )
    then
        % We have already recorded the items used by this predicate.
        true
    else if
        % Items used by class methods are recorded when processing
        % the typeclass declaration. Make sure that is done.
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_class_method)
    then
        % The typeclass for which the predicate is a method is the first
        % of the universal class constraints in the pred_info.
        pred_info_get_class_context(PredInfo, MethodClassContext),
        MethodClassContext = constraints(MethodUnivConstraints, _),
        (
            MethodUnivConstraints = [MethodUnivConstraint | _],
            MethodUnivConstraint = constraint(ClassName, ClassArgTypes),
            ClassArity = list.length(ClassArgTypes)
        ;
            MethodUnivConstraints = [],
            unexpected($pred, "class method with no class constraints")
        ),
        maybe_record_item_to_process(typeclass_item,
            item_name(ClassName, ClassArity), !Info)
    else
        ItemName = item_name(qualified(PredModule, Name), Arity),
        record_expanded_items_used_by_item(ItemType, ItemName, !Info),
        record_imported_item(ItemType, ItemName, !Info),
        pred_info_get_arg_types(PredInfo, ArgTypes),
        find_items_used_by_types(ArgTypes, !Info),
        pred_info_get_proc_table(PredInfo, Procs),
        map.foldl(find_items_used_by_proc_arg_modes, Procs, !Info),
        pred_info_get_class_context(PredInfo, ClassContext),
        find_items_used_by_class_context(ClassContext, !Info),

        % Record items used by `:- pragma type_spec' declarations.
        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, _, _, PragmaMap),
        ( if map.search(PragmaMap, PredId, TypeSpecPragmas) then
            list.foldl(find_items_used_by_type_spec, TypeSpecPragmas, !Info)
        else
            true
        )
    ).

:- pred find_items_used_by_proc_arg_modes(proc_id::in, proc_info::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_proc_arg_modes(_ProcId, ProcInfo, !Info) :-
    proc_info_get_argmodes(ProcInfo, ArgModes),
    find_items_used_by_modes(ArgModes, !Info).

:- pred find_items_used_by_type_spec(pragma_info_type_spec::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_spec(TypeSpecInfo, !Info) :-
    TypeSpecInfo = pragma_info_type_spec(PFUMM, _, _, Subst, _, _),
    (
        ( PFUMM = pfumm_predicate(ModesOrArity)
        ; PFUMM = pfumm_function(ModesOrArity)
        ),
        (
            ModesOrArity = moa_modes(Modes),
            find_items_used_by_modes(Modes, !Info)
        ;
            ModesOrArity = moa_arity(_Arity)
        )
    ;
        PFUMM = pfumm_unknown(_Arity)
    ),
    assoc_list.values(Subst, SubstTypes),
    find_items_used_by_types(SubstTypes, !Info).

:- pred find_items_used_by_functors(simple_item_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors(Set, !Info) :-
    map.foldl(find_items_used_by_functors_2, Set, !Info).

:- pred find_items_used_by_functors_2(name_arity::in,
    map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors_2(NameArity, MatchingCtorMap, !Info) :-
    NameArity = name_arity(Name, Arity),
    map.foldl(find_items_used_by_functors_3(Name, Arity), MatchingCtorMap,
        !Info).

:- pred find_items_used_by_functors_3(string::in, arity::in,
    module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors_3(Name, Arity, Qualifier, _, !Info) :-
    SymName = module_qualify_name(Qualifier, Name),
    record_used_functor(SymName - Arity, !Info).

:- pred find_items_used_by_functor(string::in, arity::in, resolved_functor::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functor(Name, _Arity, ResolverFunctor, !Info) :-
    (
        ResolverFunctor = resolved_functor_pred_or_func(PredId, PredOrFunc,
            PredModule, pred_form_arity(PredFormArity)),
        NameArity = name_arity(Name, PredFormArity),
        find_items_used_by_pred(PredOrFunc, NameArity, PredId - PredModule,
            !Info)
    ;
        (
            ResolverFunctor = resolved_functor_data_constructor(TypeCtor)
        ;
            ResolverFunctor = resolved_functor_field_access_func(ConsCtor),
            ConsCtor = cons_ctor(_ConsName, _ConsArity, TypeCtor)
        ),
        ItemName = type_ctor_to_item_name(TypeCtor),
        maybe_record_item_to_process(type_defn_item, ItemName, !Info)
    ).

:- pred find_items_used_by_simple_item_set(item_type::in, simple_item_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set(ItemType, Set, !Info) :-
    map.foldl(find_items_used_by_simple_item_set_2(ItemType), Set, !Info).

:- pred find_items_used_by_simple_item_set_2(item_type::in,
    name_arity::in, map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set_2(ItemType, NameArity, MatchingIdMap,
        !Info) :-
    NameArity = name_arity(Name, Arity),
    map.foldl(find_items_used_by_simple_item_set_3(ItemType, Name, Arity),
        MatchingIdMap, !Info).

:- pred find_items_used_by_simple_item_set_3(item_type::in,
    string::in, arity::in, module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set_3(ItemType, Name, Arity, _, Module,
        !Info) :-
    maybe_record_item_to_process(ItemType,
        item_name(qualified(Module, Name), Arity), !Info).

:- pred find_items_used_by_types(list(mer_type)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_types(Types, !Info) :-
    list.foldl(find_items_used_by_type, Types, !Info).

:- pred find_items_used_by_type(mer_type::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type(Type, !Info) :-
    ( if type_to_ctor_and_args(Type, TypeCtor, TypeArgs) then
        find_items_used_by_type_ctor(TypeCtor, !Info),
        find_items_used_by_types(TypeArgs, !Info)
    else
        true
    ).

:- pred find_items_used_by_type_ctor(type_ctor::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_ctor(TypeCtor, !Info) :-
    ( if
        % Unqualified type constructor names are builtins.
        TypeCtor = type_ctor(qualified(_, _), _),
        not type_ctor_is_higher_order(TypeCtor, _, _, _)
    then
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        maybe_record_item_to_process(type_name_item, TypeCtorItem, !Info)
    else
        true
    ).

:- pred find_items_used_by_modes(list(mer_mode)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_modes(Modes, !Info) :-
    list.foldl(find_items_used_by_mode, Modes, !Info).

:- pred find_items_used_by_mode(mer_mode::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_mode(from_to_mode(Inst1, Inst2), !Info) :-
    find_items_used_by_inst(Inst1, !Info),
    find_items_used_by_inst(Inst2, !Info).
find_items_used_by_mode(user_defined_mode(ModeName, ArgInsts), !Info) :-
    list.length(ArgInsts, ModeArity),
    maybe_record_item_to_process(mode_item, item_name(ModeName, ModeArity),
        !Info),
    find_items_used_by_insts(ArgInsts, !Info).

:- pred find_items_used_by_insts(list(mer_inst)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_insts(Modes, !Info) :-
    list.foldl(find_items_used_by_inst, Modes, !Info).

:- pred find_items_used_by_inst(mer_inst::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst(Inst, !Info) :-
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = inst_var(_)
        )
    ;
        ( Inst = any(_, HOInstInfo)
        ; Inst = ground(_, HOInstInfo)
        ),
        (
            HOInstInfo = higher_order(pred_inst_info(_, Modes, _, _)),
            find_items_used_by_modes(Modes, !Info)
        ;
            HOInstInfo = none_or_default_func
        )
    ;
        Inst = bound(_, _, BoundInsts),
        list.foldl(find_items_used_by_bound_inst, BoundInsts, !Info)
    ;
        Inst = constrained_inst_vars(_, SubInst),
        find_items_used_by_inst(SubInst, !Info)
    ;
        Inst = defined_inst(InstName),
        find_items_used_by_inst_name(InstName, !Info)
    ;
        Inst = abstract_inst(Name, ArgInsts),
        list.length(ArgInsts, Arity),
        maybe_record_item_to_process(inst_item, item_name(Name, Arity), !Info),
        find_items_used_by_insts(ArgInsts, !Info)
    ).

:- pred find_items_used_by_bound_inst(bound_inst::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_bound_inst(BoundInst, !Info) :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    ( if ConsId = cons(Name, Arity, _) then
        record_used_functor(Name - Arity, !Info)
    else
        true
    ),
    find_items_used_by_insts(ArgInsts, !Info).

:- pred find_items_used_by_inst_name(inst_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst_name(InstName, !Info) :-
    (
        InstName = user_inst(Name, ArgInsts),
        list.length(ArgInsts, Arity),
        maybe_record_item_to_process(inst_item, item_name(Name, Arity), !Info),
        find_items_used_by_insts(ArgInsts, !Info)
    ;
        ( InstName = merge_inst(InstA, InstB)
        ; InstName = unify_inst(_, _, InstA, InstB)
        ),
        find_items_used_by_inst(InstA, !Info),
        find_items_used_by_inst(InstB, !Info)
    ;
        ( InstName = ground_inst(SubInstName, _, _, _)
        ; InstName = any_inst(SubInstName, _, _, _)
        ; InstName = shared_inst(SubInstName)
        ; InstName = mostly_uniq_inst(SubInstName)
        ),
        find_items_used_by_inst_name(SubInstName, !Info)
    ;
        InstName = typed_ground(_, Type),
        find_items_used_by_type(Type, !Info)
    ;
        InstName = typed_inst(Type, SubInstName),
        find_items_used_by_type(Type, !Info),
        find_items_used_by_inst_name(SubInstName, !Info)
    ).

:- pred find_items_used_by_class_context(prog_constraints::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_context(Constraints, !Info) :-
    Constraints = constraints(UnivConstraints, ExistConstraints),
    find_items_used_by_class_constraints(UnivConstraints, !Info),
    find_items_used_by_class_constraints(ExistConstraints, !Info).

:- pred find_items_used_by_class_constraints(list(prog_constraint)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_constraints(Constraints, !Info) :-
    list.foldl(find_items_used_by_class_constraint, Constraints, !Info).

:- pred find_items_used_by_class_constraint(prog_constraint::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_constraint(Constraint, !Info) :-
    Constraint = constraint(ClassName, ArgTypes),
    ClassArity = list.length(ArgTypes),
    maybe_record_item_to_process(typeclass_item,
        item_name(ClassName, ClassArity), !Info),
    find_items_used_by_types(ArgTypes, !Info).

:- pred maybe_record_item_to_process(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

maybe_record_item_to_process(ItemType, ItemName, !Info) :-
    ( if ItemType = typeclass_item then
        Classes0 = !.Info ^ used_typeclasses,
        set.insert(ItemName, Classes0, Classes),
        !Info ^ used_typeclasses := Classes
    else
        true
    ),

    ( if item_is_recorded_used(!.Info, ItemType, ItemName) then
        % This item has already been recorded.
        true
    else if item_is_local(!.Info, ItemName) then
        % Ignore local items. The items used by them have already been recorded
        % by module_qual.m.
        true
    else
        Queue0 = !.Info ^ item_queue,
        queue.put(item_id(ItemType, ItemName), Queue0, Queue),
        !Info ^ item_queue := Queue,

        record_imported_item(ItemType, ItemName, !Info),
        record_expanded_items_used_by_item(ItemType, ItemName, !Info)
    ).

:- pred item_is_recorded_used(recompilation_usage_info::in,
    item_type::in, item_name::in) is semidet.

item_is_recorded_used(Info, ItemType, ItemName) :-
    ImportedItems = Info ^ imported_items,
    ItemName = item_name(qualified(ModuleName, Name), Arity),
    map.search(ImportedItems, ModuleName, ModuleImportedItems),
    get_module_imported_items(ModuleImportedItems, ItemType, ModuleItemIdSet),
    set.member(name_arity(Name, Arity), ModuleItemIdSet).

:- pred item_is_local(recompilation_usage_info::in, item_name::in) is semidet.

item_is_local(Info, ItemName) :-
    ItemName = item_name(qualified(ModuleName, _), _),
    module_info_get_name(Info ^ module_info, ModuleName).

:- pred record_imported_item(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_imported_item(ItemType, ItemName, !Info) :-
    ItemName = item_name(SymName, Arity),
    (
        SymName = qualified(Module0, Name0),
        Module = Module0,
        Name = Name0
    ;
        SymName = unqualified(_),
        unexpected($pred, "unqualified item")
    ),

    ImportedItems0 = !.Info ^ imported_items,
    ( if map.search(ImportedItems0, Module, ModuleItems0) then
        ModuleItems1 = ModuleItems0
    else
        ModuleItems1 = init_module_imported_items
    ),
    get_module_imported_items(ModuleItems1, ItemType, ModuleItemIds0),
    set.insert(name_arity(Name, Arity), ModuleItemIds0, ModuleItemIds),
    set_module_imported_items(ItemType, ModuleItemIds,
        ModuleItems1, ModuleItems),
    map.set(Module, ModuleItems, ImportedItems0, ImportedItems),
    !Info ^ imported_items := ImportedItems.

    % Uses of equivalence types have been expanded away by equiv_type.m.
    % equiv_type.m records which equivalence types were used by each
    % imported item.
    %
:- pred record_expanded_items_used_by_item(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_expanded_items_used_by_item(ItemType, NameArity, !Info) :-
    Dependencies = !.Info ^ dependencies,
    ( if
        map.search(Dependencies, item_id(ItemType, NameArity), EquivTypes)
    then
        list.foldl(record_expanded_items_used_by_item_2,
            set.to_sorted_list(EquivTypes), !Info)
    else
        true
    ).

:- pred record_expanded_items_used_by_item_2(item_id::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_expanded_items_used_by_item_2(Item, !Info) :-
    Item = item_id(DepItemType, DepItemId),
    maybe_record_item_to_process(DepItemType, DepItemId, !Info).

%---------------------------------------------------------------------------%

:- func init_module_imported_items = module_imported_items.

init_module_imported_items =
    module_imported_items(set.init, set.init, set.init, set.init, set.init,
        set.init, set.init, set.init).

:- pred get_module_imported_items(module_imported_items::in,
    item_type::in, imported_item_set::out) is det.

get_module_imported_items(MII, type_name_item, MII ^ mii_type_names).
get_module_imported_items(MII, type_defn_item, MII ^ mii_type_defns).
get_module_imported_items(MII, inst_item, MII ^ mii_insts).
get_module_imported_items(MII, mode_item, MII ^ mii_modes).
get_module_imported_items(MII, typeclass_item, MII ^ mii_typeclasses).
get_module_imported_items(MII, functor_item, MII ^ mii_functors).
get_module_imported_items(MII, predicate_item, MII ^ mii_predicates).
get_module_imported_items(MII, function_item, MII ^ mii_functions).
get_module_imported_items(_MII, mutable_item, _) :-
    unexpected($pred, "mutable_item").
get_module_imported_items(_MII, foreign_proc_item, _) :-
    unexpected($pred, "foreign_proc_item").

:- pred set_module_imported_items(item_type::in, imported_item_set::in,
    module_imported_items::in, module_imported_items::out) is det.

set_module_imported_items(type_name_item, Set, !MII) :-
    !MII ^ mii_type_names := Set.
set_module_imported_items(type_defn_item, Set, !MII) :-
    !MII ^ mii_type_defns := Set.
set_module_imported_items(inst_item, Set, !MII) :-
    !MII ^ mii_insts := Set.
set_module_imported_items(mode_item, Set, !MII) :-
    !MII ^ mii_modes := Set.
set_module_imported_items(typeclass_item, Set, !MII) :-
    !MII ^ mii_typeclasses := Set.
set_module_imported_items(functor_item, Set, !MII) :-
    !MII ^ mii_functors := Set.
set_module_imported_items(predicate_item, Set, !MII) :-
    !MII ^ mii_predicates := Set.
set_module_imported_items(function_item, Set, !MII) :-
    !MII ^ mii_functions := Set.
set_module_imported_items(mutable_item, _Set, !MII) :-
    unexpected($pred, "mutable_item").
set_module_imported_items(foreign_proc_item, _Set, !MII) :-
    unexpected($pred, "foreign_proc_item").

%---------------------------------------------------------------------------%
:- end_module recompilation.usage.
%---------------------------------------------------------------------------%
