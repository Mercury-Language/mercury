%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: recompilation_version.m.
% Main author: stayl.
%
% Compute version numbers for program items in interface files.
%
%-----------------------------------------------------------------------------%

:- module recompilation.version.
:- interface.

:- import_module libs.timestamp.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

    % compute_version_numbers(SourceFileModTime, NewItems, MaybeOldItems,
    %   VersionNumbers).
    %
:- pred compute_version_numbers(timestamp::in, list(item)::in,
    maybe(list(item))::in, version_numbers::out) is det.

:- pred write_version_numbers(version_numbers::in, io::di, io::uo) is det.

:- pred parse_version_numbers(term::in, maybe1(version_numbers)::out) is det.

    % The version number for the format of the version numbers
    % written to the interface files.
    %
:- func version_numbers_version_number = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

compute_version_numbers(SourceFileTime,
        Items, MaybeOldItems,
        version_numbers(ItemVersionNumbers, InstanceVersionNumbers)) :-
    gather_items(section_implementation, Items, GatheredItems, InstanceItems),
    (
        MaybeOldItems = yes(OldItems0),
        OldItems0 = [FirstItem, VersionNumberItem | OldItems],
        FirstItem = item_module_defn(FirstItemModuleDefn),
        FirstItemModuleDefn = item_module_defn_info(md_interface, _, _),
        VersionNumberItem = item_module_defn(VersionNumberItemModuleDefn),
        VersionNumberItemModuleDefn = item_module_defn_info(
            md_version_numbers(_, OldVersionNumbers), _, _)
    ->
        OldVersionNumbers = version_numbers(OldItemVersionNumbers,
            OldInstanceVersionNumbers),
        gather_items(section_implementation, OldItems,
            GatheredOldItems, OldInstanceItems)
    ;
        % There were no old version numbers, so every item
        % gets the same timestamp as the source module.
        OldItemVersionNumbers = init_item_id_set(map.init),
        GatheredOldItems = init_item_id_set(map.init),
        map.init(OldInstanceItems),
        map.init(OldInstanceVersionNumbers)
    ),

    compute_item_version_numbers(SourceFileTime,
        GatheredItems, GatheredOldItems,
        OldItemVersionNumbers, ItemVersionNumbers),

    compute_instance_version_numbers(SourceFileTime,
        InstanceItems, OldInstanceItems,
        OldInstanceVersionNumbers, InstanceVersionNumbers).

:- pred compute_item_version_numbers(timestamp::in,
    gathered_items::in, gathered_items::in,
    item_version_numbers::in, item_version_numbers::out) is det.

compute_item_version_numbers(SourceFileTime,
        GatheredItems, GatheredOldItems,
        OldVersionNumbers, VersionNumbers) :-
    VersionNumbers = map_ids(compute_item_version_numbers_2(SourceFileTime,
        GatheredOldItems, OldVersionNumbers),
        GatheredItems, map.init).

:- func compute_item_version_numbers_2(timestamp, gathered_items,
    item_version_numbers, item_type,
    map(pair(string, arity), assoc_list(section, item)))
    = map(pair(string, arity), timestamp).

compute_item_version_numbers_2(SourceFileTime, GatheredOldItems,
        OldVersionNumbers, ItemType, Items0) =
    map.map_values(compute_item_version_numbers_3(SourceFileTime,
        GatheredOldItems, OldVersionNumbers, ItemType), Items0).

:- func compute_item_version_numbers_3(timestamp, gathered_items,
    item_version_numbers, item_type, pair(string, arity),
    assoc_list(section, item)) = timestamp.

compute_item_version_numbers_3(SourceFileTime, GatheredOldItems,
        OldVersionNumbers, ItemType, NameArity, Items) =
    (
        OldIds = extract_ids(GatheredOldItems, ItemType),
        map.search(OldIds, NameArity, OldItems),
        items_are_unchanged(OldItems, Items),
        map.search(extract_ids(OldVersionNumbers, ItemType), NameArity,
            OldVersionNumber)
    ->
        OldVersionNumber
    ;
        SourceFileTime
    ).

:- pred compute_instance_version_numbers(timestamp::in,
    instance_item_map::in, instance_item_map::in,
    instance_version_numbers::in, instance_version_numbers::out) is det.

compute_instance_version_numbers(SourceFileTime,
        InstanceItems, OldInstanceItems,
        OldInstanceVersionNumbers, InstanceVersionNumbers) :-
    InstanceVersionNumbers = map.map_values(
        (func(ClassId, Items) = VersionNumber :-
            (
                map.search(OldInstanceItems, ClassId, OldItems),
                items_are_unchanged(OldItems, Items),
                map.search(OldInstanceVersionNumbers, ClassId,
                    OldVersionNumber)
            ->
                VersionNumber = OldVersionNumber
            ;
                VersionNumber = SourceFileTime
            )
        ),
        InstanceItems
    ).

:- pred gather_items(section::in, list(item)::in,
    gathered_items::out, instance_item_map::out) is det.

gather_items(Section, Items, GatheredItems, Instances) :-
    list.reverse(Items, RevItems),
    Info0 = gathered_item_info(init_item_id_set(map.init), [], [], map.init),
    list.foldl2(gather_items_2, RevItems, Section, _, Info0, Info1),

    % Items which could appear in _OtherItems (those which aren't gathered
    % into the list for another type of item) can't appear in the interface
    % section. Those other items (e.g. assertions) will need to be handled here
    % when smart recompilation is made to work with
    % `--intermodule-optimization'.
    Info1 = gathered_item_info(GatheredItems1, PragmaItems,
        _OtherItems, Instances),
    list.reverse(PragmaItems, RevPragmaItems),
    list.foldl(distribute_pragma_items, RevPragmaItems,
        GatheredItems1, GatheredItems).

:- pred distribute_pragma_items(
    {maybe_pred_or_func_id, item, section}::in,
    gathered_items::in, gathered_items::out) is det.

distribute_pragma_items({ItemId, Item, Section}, !GatheredItems) :-
    ItemId = MaybePredOrFunc - SymName / Arity,

    % For predicates defined using `with_type` annotations we don't know
    % the actual arity, so always we need to add entries for pragmas, even if
    % the pragma doesn't match any recorded predicate. For pragmas which don't
    % include enough information to work out whether they apply to a predicate
    % or a function this will result in an extra entry in the version numbers.
    % Pragmas in the interface aren't common so this won't be too much of
    % a problem.
    AddIfNotExisting = yes,
    ItemName = item_name(SymName, Arity),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        add_gathered_item(Item, item_id(ItemType, ItemName),
            Section, AddIfNotExisting, !GatheredItems)
    ;
        MaybePredOrFunc = no,
        add_gathered_item(Item, item_id(predicate_item, ItemName),
            Section, AddIfNotExisting, !GatheredItems),
        add_gathered_item(Item, item_id(function_item, ItemName),
            Section, AddIfNotExisting, !GatheredItems)
    ),

    % Pragmas can apply to typeclass methods.
    map.map_values_only(distribute_pragma_items_class_items(MaybePredOrFunc,
        SymName, Arity, Item, Section),
        extract_ids(!.GatheredItems, typeclass_item), GatheredTypeClasses),
    !:GatheredItems = update_ids(!.GatheredItems, typeclass_item,
        GatheredTypeClasses).

:- pred distribute_pragma_items_class_items(maybe(pred_or_func)::in,
    sym_name::in, arity::in, item::in, section::in,
    assoc_list(section, item)::in, assoc_list(section, item)::out) is det.

distribute_pragma_items_class_items(MaybePredOrFunc, SymName, Arity,
        Item, Section, !ClassItems) :-
    (
        % Does this pragma match any of the methods of this class.
        list.member(_ - ClassItem, !.ClassItems),
        ClassItem = item_typeclass(ClassItemTypeClass),
        Interface = ClassItemTypeClass ^ tc_class_methods,
        Interface = class_interface_concrete(Methods),
        list.member(Method, Methods),
        Method = method_pred_or_func(_, _, _, MethodPredOrFunc, SymName,
            TypesAndModes, WithType, _, _, _, _, _, _),
        ( MaybePredOrFunc = yes(MethodPredOrFunc)
        ; MaybePredOrFunc = no
        ),
        (
            WithType = no,
            adjust_func_arity(MethodPredOrFunc, Arity,
                list.length(TypesAndModes))
        ;
            % We don't know the actual arity, so just match on the name
            % and pred_or_func.
            WithType = yes(_)
        )
    ->
        % XXX O(N^2), but shouldn't happen too often.
        !:ClassItems = !.ClassItems ++ [Section - Item]
    ;
        true
    ).

:- type gathered_item_info
    --->    gathered_item_info(
                gathered_items  :: gathered_items,
                pragma_items    :: list({maybe_pred_or_func_id,
                                    item, section}),
                other_items     :: list(item),
                instances       :: instance_item_map
            ).

:- type instance_item_map ==
    map(item_name, assoc_list(section, item)).

    % The constructors set should always be empty.
:- type gathered_items == item_id_set(gathered_item_map).
:- type gathered_item_map == map(pair(string, arity),
    assoc_list(section, item)).

:- pred gather_items_2(item::in, section::in, section::out,
    gathered_item_info::in, gathered_item_info::out) is det.

gather_items_2(Item, !Section, !Info) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(md_interface, _, _)
    ->
        !:Section = section_interface
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(md_implementation, _, _)
    ->
        !:Section = section_implementation
    ;
        Item = item_type_defn(ItemTypeDefn)
    ->
        ItemTypeDefn = item_type_defn_info(VarSet, Name, Args, Body, Cond,
            Context, SeqNum),
        (
            Body = parse_tree_abstract_type(_),
            NameItem = Item,
            % The body of an abstract type can be recorded as used when
            % generating a call to the automatically generated unification
            % procedure.
            BodyItem = Item
        ;
            Body = parse_tree_du_type(_, _, _),
            % XXX does the abstract_details matter here?
            AbstractDetails = abstract_type_general,
            NameItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_abstract_type(AbstractDetails), Cond, Context,
                SeqNum),
            NameItem = item_type_defn(NameItemTypeDefn),
            BodyItem = Item
        ;
            Body = parse_tree_eqv_type(_),
            % When we use an equivalence type we always use the body.
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_solver_type(_, _),
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_foreign_type(_, _, _),
            NameItem = Item,
            BodyItem = Item
        ),
        TypeCtorItem = item_name(Name, list.length(Args)),
        GatheredItems0 = !.Info ^ gathered_items,
        add_gathered_item(NameItem, item_id(type_abstract_item, TypeCtorItem),
            !.Section, yes, GatheredItems0, GatheredItems1),
        add_gathered_item(BodyItem, item_id(type_body_item, TypeCtorItem),
            !.Section, yes, GatheredItems1, GatheredItems),
        !Info ^ gathered_items := GatheredItems
    ;
        Item = item_instance(ItemInstance)
    ->
        ItemInstance =
            item_instance_info(_, ClassName, ClassArgs, _, _, _, _, _, _),
        Instances0 = !.Info ^ instances,
        ClassArity = list.length(ClassArgs),
        ClassItemName = item_name(ClassName, ClassArity),
        NewInstanceItem = !.Section - Item,
        ( map.search(Instances0, ClassItemName, OldInstanceItems) ->
            NewInstanceItems = [NewInstanceItem | OldInstanceItems],
            map.det_update(ClassItemName, NewInstanceItems,
                Instances0, Instances)
        ;
            map.det_insert(ClassItemName, [NewInstanceItem],
                Instances0, Instances)
        ),
        !Info ^ instances := Instances
    ;
        % For predicates or functions defined using `with_inst` annotations
        % the pred_or_func and arity here won't be correct, but equiv_type.m
        % will record the dependency on the version number with the `incorrect'
        % pred_or_func and arity, so this will work.
        Item = item_mode_decl(ItemModeDecl),
        ItemModeDecl = item_mode_decl_info(_, MaybePredOrFunc, SymName, Modes,
            WithInst, _, _, _, _),
        MaybePredOrFunc = no,
        WithInst = yes(_)
    ->
        GatheredItems0 = !.Info ^ gathered_items,
        ItemName = item_name(SymName, list.length(Modes)),
        add_gathered_item(Item, item_id(predicate_item, ItemName),
            !.Section, yes, GatheredItems0, GatheredItems1),
        add_gathered_item(Item, item_id(function_item, ItemName),
            !.Section, yes, GatheredItems1, GatheredItems),
        !Info ^ gathered_items := GatheredItems
    ;
        item_to_item_id(Item, ItemId)
    ->
        GatheredItems0 = !.Info ^ gathered_items,
        add_gathered_item(Item, ItemId, !.Section, yes,
            GatheredItems0, GatheredItems),
        !Info ^ gathered_items := GatheredItems
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, PragmaType, _, _),
        is_pred_pragma(PragmaType, yes(PredOrFuncId))
    ->
        PragmaItems = !.Info ^ pragma_items,
        !Info ^ pragma_items := [{PredOrFuncId, Item, !.Section} | PragmaItems]
    ;
        OtherItems = !.Info ^ other_items,
        !Info ^ other_items := [Item | OtherItems]
    ).

:- pred add_gathered_item(item::in, item_id::in, section::in, bool::in,
    gathered_items::in, gathered_items::out) is det.

add_gathered_item(Item, ItemId, Section, AddIfNotExisting, !GatheredItems) :-
    ItemId = item_id(ItemType, ItemName),
    ItemName = item_name(SymName, Arity),
    Name = unqualify_name(SymName),
    IdMap0 = extract_ids(!.GatheredItems, ItemType),
    NameArity = Name - Arity,
    ( map.search(IdMap0, NameArity, MatchingItems0) ->
        MatchingItems = MatchingItems0
    ;
        MatchingItems = []
    ),
    (
        MatchingItems = [],
        AddIfNotExisting = no
    ->
        true
    ;
        add_gathered_item_2(Item, ItemType, NameArity, Section,
            MatchingItems, !GatheredItems)
    ).

:- pred add_gathered_item_2(item::in, item_type::in, pair(string, arity)::in,
    section::in, assoc_list(section, item)::in,
    gathered_items::in, gathered_items::out) is det.

add_gathered_item_2(Item, ItemType, NameArity, Section, MatchingItems0,
        !GatheredItems) :-
    % mercury_to_mercury.m splits combined pred and mode declarations.
    % That needs to be done here as well the item list read from the interface
    % file will match the item list generated here.
    (
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(Origin, TVarSet, InstVarSet,
            ExistQVars, PredOrFunc, PredName, TypesAndModes,
            WithType, WithInst, Det, Cond, Purity, ClassContext,
            Context, SeqNum),
        split_types_and_modes(TypesAndModes, Types, MaybeModes),
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    ->
        TypesWithoutModes = list.map((func(Type) = type_only(Type)), Types),
        varset.init(EmptyInstVarSet),
        PredItemPredDecl = item_pred_decl_info(Origin, TVarSet,
            EmptyInstVarSet, ExistQVars, PredOrFunc, PredName,
            TypesWithoutModes, WithType, no, no, Cond, Purity, ClassContext,
            Context, SeqNum),
        PredItem = item_pred_decl(PredItemPredDecl),
        (
            WithInst = yes(_),
            % MaybePredOrFunc needs to be `no' here because when the item
            % is read from the interface file we won't know whether it is
            % a predicate or a function mode.
            MaybePredOrFunc = no
        ;
            WithInst = no,
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        ModeItemModeDecl = item_mode_decl_info(InstVarSet, MaybePredOrFunc,
            PredName, Modes, WithInst, Det, Cond, Context, SeqNum),
        ModeItem = item_mode_decl(ModeItemModeDecl),
        MatchingItems = [Section - PredItem, Section - ModeItem
            | MatchingItems0]
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass ^ tc_class_methods = class_interface_concrete(Methods0)
    ->
        MethodsList = list.map(split_class_method_types_and_modes, Methods0),
        list.condense(MethodsList, Methods),
        NewItemTypeClass = ItemTypeClass ^ tc_class_methods
            := class_interface_concrete(Methods),
        NewItem = item_typeclass(NewItemTypeClass),
        MatchingItems = [Section - NewItem | MatchingItems0]
    ;
        MatchingItems = [Section - Item | MatchingItems0]
    ),

    IdMap0 = extract_ids(!.GatheredItems, ItemType),
    map.set(NameArity, MatchingItems, IdMap0, IdMap),
    !:GatheredItems = update_ids(!.GatheredItems, ItemType, IdMap).

:- func split_class_method_types_and_modes(class_method) = class_methods.

split_class_method_types_and_modes(Method0) = Methods :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    Method0 = method_pred_or_func(TVarSet, InstVarSet, ExistQVars, PredOrFunc,
        SymName, TypesAndModes, WithType, WithInst, MaybeDet, Cond,
        Purity, ClassContext, _),
    (
        split_types_and_modes(TypesAndModes, Types, MaybeModes),
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    ->
        TypesWithoutModes = list.map((func(Type) = type_only(Type)), Types),
        (
            WithInst = yes(_),
            % MaybePredOrFunc needs to be `no' here because when the item
            % is read from the interface file we won't know whether it is
            % a predicate or a function mode.
            MaybePredOrFunc = no
        ;
            WithInst = no,
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        PredOrFuncModeItem = method_pred_or_func_mode(InstVarSet,
            MaybePredOrFunc, SymName, Modes, WithInst, MaybeDet, Cond,
            term.context_init),
        PredOrFuncModeItems = [PredOrFuncModeItem]
    ;
        TypesWithoutModes = TypesAndModes,
        PredOrFuncModeItems = []
    ),
    varset.init(EmptyInstVarSet),
    PredOrFuncItem = method_pred_or_func(TVarSet, EmptyInstVarSet, ExistQVars,
        PredOrFunc, SymName, TypesWithoutModes, WithType, no, no, Cond, Purity,
        ClassContext, term.context_init),
    Methods = [PredOrFuncItem | PredOrFuncModeItems].
split_class_method_types_and_modes(Method0) = [Method] :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    Method0 = method_pred_or_func_mode(A, B, C, D, E, F, G, _),
    Method = method_pred_or_func_mode(A, B, C, D, E, F, G, term.context_init).

:- pred item_to_item_id(item::in, item_id::out) is semidet.

item_to_item_id(Item, ItemId) :-
    item_to_item_id_2(Item, yes(ItemId)).

:- pred item_to_item_id_2(item::in, maybe(item_id)::out) is det.

item_to_item_id_2(Item, MaybeItemId) :-
    (
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_clause(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        ),
        MaybeItemId = no
    ;
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(_, Name, Params, _, _, _, _),
        list.length(Params, Arity),
        ItemId = item_id(type_abstract_item, item_name(Name, Arity)),
        MaybeItemId = yes(ItemId)
    ;
        Item = item_inst_defn(ItemInstDefn),
        ItemInstDefn = item_inst_defn_info(_, Name, Params, _, _, _, _),
        list.length(Params, Arity),
        ItemId = item_id(inst_item, item_name(Name, Arity)),
        MaybeItemId = yes(ItemId)
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(_, Name, Params, _, _, _, _),
        list.length(Params, Arity),
        ItemId = item_id(mode_item, item_name(Name, Arity)),
        MaybeItemId = yes(ItemId)
    ;
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(_, _, _, _, PredOrFunc, SymName,
            TypesAndModes, WithType, _, _, _, _, _, _, _),
        % For predicates or functions defined using `with_type` annotations
        % the arity here won't be correct, but equiv_type.m will record
        % the dependency on the version number with the `incorrect' arity,
        % so this will work.
        (
            WithType = no,
            adjust_func_arity(PredOrFunc, Arity, list.length(TypesAndModes))
        ;
            WithType = yes(_),
            Arity = list.length(TypesAndModes)
        ),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        ItemId = item_id(ItemType, item_name(SymName, Arity)),
        MaybeItemId = yes(ItemId)
    ;
        Item = item_mode_decl(ItemModeDecl),
        ItemModeDecl = item_mode_decl_info(_, MaybePredOrFunc, SymName, Modes,
            _, _, _, _, _),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
            ItemType = pred_or_func_to_item_type(PredOrFunc),
            ItemId = item_id(ItemType, item_name(SymName, Arity)),
            MaybeItemId = yes(ItemId)
        ;
            MaybePredOrFunc = no,
            % We need to handle these separately because a `:- mode'
            % declaration with a `with_inst` annotation could be for
            % a predicate or a function.
            MaybeItemId = no
        )
    ;
        Item = item_pragma(_),
        % We need to handle these separately because some pragmas
        % may affect a predicate and a function.
        MaybeItemId = no
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(_, _, ClassName, ClassVars,
            _, _, _, _),
        list.length(ClassVars, ClassArity),
        ItemId = item_id(typeclass_item, item_name(ClassName, ClassArity)),
        MaybeItemId = yes(ItemId)
    ;
        Item = item_instance(_),
        % Instances are handled separately (unlike other items, the module
        % qualifier on an instance declaration is the module containing
        % the class, not the module containing the instance).
        MaybeItemId = no
    ).

:- type maybe_pred_or_func_id == pair(maybe(pred_or_func), sym_name_and_arity).

:- pred is_pred_pragma(pragma_type::in, maybe(maybe_pred_or_func_id)::out)
    is det.

is_pred_pragma(PragmaType, MaybePredOrFuncId) :-
    (
        ( PragmaType = pragma_foreign_decl(_)
        ; PragmaType = pragma_foreign_import_module(_)
        ; PragmaType = pragma_foreign_code(_)
        ; PragmaType = pragma_foreign_export_enum(_)
        ; PragmaType = pragma_foreign_enum(_)
        ; PragmaType = pragma_source_file(_)
        ; PragmaType = pragma_reserve_tag(_)
        ; PragmaType = pragma_oisu(_)               % XXX
        ; PragmaType = pragma_require_feature_set(_)
        ),
        MaybePredOrFuncId = no
    ;
        ( PragmaType = pragma_inline(PredNameArity)
        ; PragmaType = pragma_no_inline(PredNameArity)
        ; PragmaType = pragma_obsolete(PredNameArity)
        ; PragmaType = pragma_no_detism_warning(PredNameArity)
        ; PragmaType = pragma_promise_pure(PredNameArity)
        ; PragmaType = pragma_promise_semipure(PredNameArity)
        ; PragmaType = pragma_promise_eqv_clauses(PredNameArity)
        ; PragmaType = pragma_terminates(PredNameArity)
        ; PragmaType = pragma_does_not_terminate(PredNameArity)
        ; PragmaType = pragma_check_termination(PredNameArity)
        ; PragmaType = pragma_mode_check_clauses(PredNameArity)
        ),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - Name / Arity)
    ;
        PragmaType = pragma_fact_table(FTInfo),
        FTInfo = pragma_info_fact_table(PredNameArity, _),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - Name / Arity)
    ;
        PragmaType = pragma_type_spec(TypeSpecInfo),
        TypeSpecInfo = pragma_info_type_spec(Name, _, Arity, MaybePredOrFunc,
            _, _, _, _),
        MaybePredOrFuncId = yes(MaybePredOrFunc - Name / Arity)
    ;
        PragmaType = pragma_tabled(TabledInfo),
        TabledInfo = pragma_info_tabled(_, PredNameArityMPF, _, _),
        PredNameArityMPF = pred_name_arity_mpf(Name, Arity, MaybePredOrFunc),
        MaybePredOrFuncId = yes(MaybePredOrFunc - Name / Arity)
    ;
        (
            PragmaType = pragma_unused_args(UnusedArgsInfo),
            UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, _)
        ;
            PragmaType = pragma_exceptions(ExceptionsInfo),
            ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, _)
        ;
            PragmaType = pragma_trailing_info(TrailingInfo),
            TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn, _)
        ;
            PragmaType = pragma_mm_tabling_info(MMTablingOnfo),
            MMTablingOnfo = pragma_info_mm_tabling_info(PredNameArityPFMn, _)
        ),
        PredNameArityPFMn = pred_name_arity_pf_mn(Name, Arity, PredOrFunc, _),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - Name / Arity)
    ;
        PragmaType = pragma_foreign_proc(FPInfo),
        FPInfo = pragma_info_foreign_proc(_, Name, PredOrFunc, Args, _, _, _),
        adjust_func_arity(PredOrFunc, Arity, list.length(Args)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - Name / Arity)
    ;
        (
            PragmaType = pragma_foreign_proc_export(FPEInfo),
            FPEInfo = pragma_info_foreign_proc_export(_, PredNameModesPF, _)
        ;
            PragmaType = pragma_termination_info(TermInfo),
            TermInfo = pragma_info_termination_info(PredNameModesPF, _, _)
        ;
            PragmaType = pragma_termination2_info(Term2Info),
            Term2Info = pragma_info_termination2_info(PredNameModesPF, _, _, _)
        ;
            PragmaType = pragma_structure_sharing(SharingInfo),
            SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
                _, _, _)
        ;
            PragmaType = pragma_structure_reuse(ReuseInfo),
            ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
                _, _, _)
        ),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - Name / Arity)
    ).

    % XXX This is a bit brittle (need to be careful with term.contexts).
    % For example, it won't work for clauses.
    % It will never succeed when it shouldn't, so it will never cause
    % a necessary recompilation to be missed.
    %
:- pred items_are_unchanged(assoc_list(section, item)::in,
    assoc_list(section, item)::in) is semidet.

items_are_unchanged([], []).
items_are_unchanged([Section - Item1 | Items1], [Section - Item2 | Items2]) :-
    yes = item_is_unchanged(Item1, Item2),
    items_are_unchanged(Items1, Items2).

    % In most places here, we don't need to compare the varsets.
    % What matters is that the variable numbers in the arguments
    % and body are the same, the names are usually irrelevant.
    %
    % The only places where the names of variables affect the compilation
    % of the program are in explicit type qualifications and
    % `:- pragma type_spec' declarations. Explicit type qualifications
    % do not need to be considered here. This module only deals with items
    % in interface files (we don't yet write type qualifications to `.opt'
    % files). Variables in type qualifications are only matched with
    % the head type variables of the predicate by make_hlds.m.
    % For `:- pragma type_spec' declarations to work we need to consider
    % a predicate or function declaration to be changed if the names
    % of any of the type variables are changed.
    %
    % It's important not to compare the varsets for type and instance
    % declarations because the declarations we get here may be abstract
    % declarations produced from concrete declarations for use in an
    % interface file. The varsets may contain variables from the discarded
    % bodies which will not be present in the items read in from the
    % interface files for comparison.
    %
    % This code assumes that the variables in the head of a type or instance
    % declaration are added to the varset before those from the body, so that
    % the variable numbers in the head of the declaration match those from
    % an abstract declaration read from an interface file.
    %
:- func item_is_unchanged(item, item) = bool.

item_is_unchanged(Item1, Item2) = Unchanged :-
    (
        Item1 = item_module_start(ItemModuleStart1),
        ItemModuleStart1 = item_module_start_info(ModuleName, _, _),
        (
            Item2 = item_module_start(ItemModuleStart2),
            ItemModuleStart2 = item_module_start_info(ModuleName, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_module_end(ItemModuleEnd1),
        ItemModuleEnd1 = item_module_end_info(ModuleName, _, _),
        (
            Item2 = item_module_end(ItemModuleEnd2),
            ItemModuleEnd2 = item_module_end_info(ModuleName, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_module_defn(ItemModuleDefn1),
        ItemModuleDefn1 = item_module_defn_info(ModuleDefn, _, _),
        (
            Item2 = item_module_defn(ItemModuleDefn2),
            ItemModuleDefn2 = item_module_defn_info(ModuleDefn, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_clause(ItemClause1),
        ItemClause1 = item_clause_info(_, _, PorF, SymName, Args, Goal, _, _),
        % XXX Need to compare the goals properly in clauses and assertions.
        % That's not necessary at the moment because smart recompilation
        % doesn't work with inter-module optimization yet.
        (
            Item2 = item_clause(ItemClause2),
            ItemClause2 =
                item_clause_info(_, _, PorF, SymName, Args, Goal, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_type_defn(ItemTypeDefn1),
        ItemTypeDefn1 = item_type_defn_info(_, Name, Args, Defn, Cond, _, _),
        (
            Item2 = item_type_defn(ItemTypeDefn2),
            ItemTypeDefn2 =
                item_type_defn_info(_, Name, Args, Defn, Cond, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_inst_defn(ItemInstDefn1),
        ItemInstDefn1 = item_inst_defn_info(_, Name, Args, Defn, Cond, _, _),
        (
            Item2 = item_inst_defn(ItemInstDefn2),
            ItemInstDefn2 =
                item_inst_defn_info(_, Name, Args, Defn, Cond, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_mode_defn(ItemModeDefn1),
        ItemModeDefn1 = item_mode_defn_info(_, Name, Args, Defn, Cond, _, _),
        (
            Item2 = item_mode_defn(ItemModeDefn2),
            ItemModeDefn2 =
                item_mode_defn_info(_, Name, Args, Defn, Cond, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_pred_decl(ItemPredDecl1),
        ItemPredDecl1 = item_pred_decl_info(_, TVarSet1, _, ExistQVars1,
            PredOrFunc, Name, TypesAndModes1, WithType1, _,
            Det1, Cond, Purity, Constraints1, _, _),
        (
            Item2 = item_pred_decl(ItemPredDecl2),
            ItemPredDecl2 = item_pred_decl_info(_, TVarSet2, _, ExistQVars2,
                PredOrFunc, Name, TypesAndModes2, WithType2,
                _, Det2, Cond, Purity, Constraints2, _, _),

            % For predicates, ignore the determinism -- the modes and
            % determinism should have been split into a separate declaration.
            % This case can only happen if this was not a combined predicate
            % and mode declaration (XXX We should warn about this somewhere).
            % For functions a determinism declaration but no modes implies
            % the default modes. The default modes are added later by
            % make_hlds.m, so they won't have been split into a separate
            % declaration here.
            (
                PredOrFunc = pf_function,
                Det1 = Det2
            ;
                PredOrFunc = pf_predicate
            ),

            pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
                TypesAndModes1, WithType1, Constraints1, TVarSet2,
                ExistQVars2, TypesAndModes2, WithType2, Constraints2)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_mode_decl(ItemModeDecl1),
        ItemModeDecl1 = item_mode_decl_info(InstVarSet1, PredOrFunc, Name,
            Modes1, WithInst1, Det, Cond, _, _),
        (
            Item2 = item_mode_decl(ItemModeDecl2),
            ItemModeDecl2 = item_mode_decl_info(InstVarSet2, PredOrFunc,
                Name, Modes2, WithInst2, Det, Cond, _, _),
            pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
                InstVarSet2, Modes2, WithInst2)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_pragma(ItemPragma1),
        ItemPragma1 = item_pragma_info(_, PragmaType1, _, _),
        % We do need to compare the variable names in `:- pragma type_spec'
        % declarations because the names of the variables are used to find
        % the corresponding variables in the predicate or function
        % type declaration.
        (
            Item2 = item_pragma(ItemPragma2),
            ItemPragma2 = item_pragma_info(_, PragmaType2, _, _)
        ->
            (
                PragmaType1 = pragma_type_spec(TypeSpecInfo1),
                PragmaType2 = pragma_type_spec(TypeSpecInfo2),
                TypeSpecInfo1 = pragma_info_type_spec(Name, SpecName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubst1, TVarSet1, _),
                TypeSpecInfo2 = pragma_info_type_spec(Name, SpecName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubst2, TVarSet2, _)
            ->
                assoc_list.keys_and_values(TypeSubst1, TVars1, Types1),
                assoc_list.keys_and_values(TypeSubst2, TVars2, Types2),
                % XXX kind inference:
                % we assume vars have kind `star'.
                KindMap = map.init,
                prog_type.var_list_to_type_list(KindMap, TVars1, TVarTypes1),
                prog_type.var_list_to_type_list(KindMap, TVars2, TVarTypes2),
                (
                    type_list_is_unchanged(
                        TVarSet1, TVarTypes1 ++ Types1,
                        TVarSet2, TVarTypes2 ++ Types2,
                        _, _, _)
                ->
                    Unchanged = yes
                ;
                    Unchanged = no
                )
            ;
                Unchanged = ( PragmaType1 = PragmaType2 -> yes ; no )
            )
        ;
            Unchanged = no
        )
    ;
        Item1 = item_promise(ItemPromiseInfo1),
        ItemPromiseInfo1 = item_promise_info(PromiseType, Goal, _,
            UnivVars, _, _),
        (
            Item2 = item_promise(ItemPromiseInfo2),
            ItemPromiseInfo2 = item_promise_info(PromiseType, Goal, _,
                UnivVars, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_initialise(ItemInitialise1),
        ItemInitialise1 = item_initialise_info(A, B, C, _, _),
        (
            Item2 = item_initialise(ItemInitialise2),
            ItemInitialise2 = item_initialise_info(A, B, C, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_finalise(ItemFinalise1),
        ItemFinalise1 = item_finalise_info(A, B, C, _, _),
        (
            Item2 = item_finalise(ItemFinalise2),
            ItemFinalise2 = item_finalise_info(A, B, C, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_mutable(ItemMutable1),
        ItemMutable1 = item_mutable_info(A, B, C, D, E, F, _, _),
        (
            Item2 = item_mutable(ItemMutable2),
            ItemMutable2 = item_mutable_info(A, B, C, D, E, F, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_typeclass(ItemTypeClass1),
        ItemTypeClass1 = item_typeclass_info(Constraints, FunDeps, Name,
            Vars, Interface1, _, _, _),
        (
            Item2 = item_typeclass(ItemTypeClass2),
            ItemTypeClass2 = item_typeclass_info(Constraints, FunDeps, Name,
                Vars, Interface2, _, _, _),
            class_interface_is_unchanged(Interface1, Interface2)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_instance(ItemInstance1),
        ItemInstance1 = item_instance_info(Constraints, Name,
            Types, OriginalTypes, Body, _, Module, _, _),
        (
            Item2 = item_instance(ItemInstance2),
            ItemInstance2 = item_instance_info(Constraints, Name,
                Types, OriginalTypes, Body, _, Module, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ;
        Item1 = item_nothing(ItemNothing1),
        ItemNothing1 = item_nothing_info(A, _, _),
        (
            Item2 = item_nothing(ItemNothing2),
            ItemNothing2 = item_nothing_info(A, _, _)
        ->
            Unchanged = yes
        ;
            Unchanged = no
        )
    ).

    % Apply a substitution to the existq_tvars, types_and_modes, and
    % prog_constraints so that the type variables from both declarations
    % being checked are contained in the same tvarset, then check that
    % they are identical.
    %
    % We can't just assume that the varsets will be identical for
    % identical declarations because mercury_to_mercury.m splits
    % combined type and mode declarations into separate declarations.
    % When they are read back in the variable numbers will be different
    % because parser stores the type and inst variables for a combined
    % declaration in a single varset (it doesn't know which are which).
    %
:- pred pred_or_func_type_is_unchanged(tvarset::in, existq_tvars::in,
    list(type_and_mode)::in, maybe(mer_type)::in, prog_constraints::in,
    tvarset::in, existq_tvars::in, list(type_and_mode)::in,
    maybe(mer_type)::in, prog_constraints::in) is semidet.

pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1, TypesAndModes1,
        MaybeWithType1, Constraints1, TVarSet2, ExistQVars2,
        TypesAndModes2, MaybeWithType2, Constraints2) :-
    GetArgTypes =
        (func(TypeAndMode0) = Type :-
            (
                TypeAndMode0 = type_only(Type)
            ;
                % This should have been split out into a separate
                % mode declaration by gather_items.
                TypeAndMode0 = type_and_mode(_, _),
                unexpected($module, $pred, "type_and_mode")
            )
        ),
    Types1 = list.map(GetArgTypes, TypesAndModes1),
    Types2 = list.map(GetArgTypes, TypesAndModes2),
    (
        MaybeWithType1 = yes(WithType1),
        MaybeWithType2 = yes(WithType2),
        AllTypes1 = [WithType1 | Types1],
        AllTypes2 = [WithType2 | Types2]
    ;
        MaybeWithType1 = no,
        MaybeWithType2 = no,
        AllTypes1 = Types1,
        AllTypes2 = Types2
    ),

    type_list_is_unchanged(TVarSet1, AllTypes1, TVarSet2, AllTypes2,
        _TVarSet, Renaming, Types2ToTypes1Subst),

    % Check that the existentially quantified variables are equivalent.
    %
    % XXX kind inference: we assume all tvars have kind `star'.

    map.init(KindMap2),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap2,
        RenamedKindMap2),
    apply_variable_renaming_to_tvar_list(Renaming, ExistQVars2,
        RenamedExistQVars2),
    apply_rec_subst_to_tvar_list(RenamedKindMap2, Types2ToTypes1Subst,
        RenamedExistQVars2, SubstExistQTypes2),
    ( prog_type.type_list_to_var_list(SubstExistQTypes2, SubstExistQVars2) ->
        ExistQVars1 = SubstExistQVars2
    ;
        unexpected($module, $pred, "non-var")
    ),

    % Check that the class constraints are identical.
    apply_variable_renaming_to_prog_constraints(Renaming,
        Constraints2, RenamedConstraints2),
    apply_rec_subst_to_prog_constraints(Types2ToTypes1Subst,
        RenamedConstraints2, SubstConstraints2),
    Constraints1 = SubstConstraints2.

:- pred type_list_is_unchanged(tvarset::in, list(mer_type)::in,
    tvarset::in, list(mer_type)::in, tvarset::out,
    tvar_renaming::out, tsubst::out) is semidet.

type_list_is_unchanged(TVarSet1, Types1, TVarSet2, Types2,
        TVarSet, Renaming, Types2ToTypes1Subst) :-
    tvarset_merge_renaming(TVarSet1, TVarSet2, TVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, Types2, SubstTypes2),

    % Check that the types are equivalent.
    type_list_subsumes(SubstTypes2, Types1, Types2ToTypes1Subst),
    type_list_subsumes(Types1, SubstTypes2, _),

    % Check that the corresponding variables have the same names. This is
    % necessary because `:- pragma type_spec' declarations depend on the names
    % of the variables, so for example if two variable names are swapped,
    % the same `:- pragma type_spec' declaration will cause a different
    % specialized version to be created.

    ( all [VarInItem1, VarInItem2]
        (
            map.member(Types2ToTypes1Subst, VarInItem2, SubstTerm),
            % Note that since the type comes from a substitution,
            % it will not contain a kind annotation.
            SubstTerm = type_variable(VarInItem1, _)
        )
    =>
        (
            varset.lookup_name(TVarSet, VarInItem1, VarName1),
            varset.lookup_name(TVarSet, VarInItem2, VarName2),
            (
                VarName1 = VarName2
            ;
                % Variables written to interface files are always named,
                % even if the variable in the source code was not, so we can't
                % just use varset.search_name to check whether the variables
                % are named.
                VarIsNotNamed =
                    (pred(VarName::in) is semidet :-
                        string.append("V_", VarNum, VarName),
                        string.to_int(VarNum, _)
                    ),
                VarIsNotNamed(VarName1),
                VarIsNotNamed(VarName2)
            )
        )
    ).

:- pred pred_or_func_mode_is_unchanged(inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in, inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in) is semidet.

pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, MaybeWithInst1,
        InstVarSet2, Modes2, MaybeWithInst2) :-
    varset.coerce(InstVarSet1, VarSet1),
    varset.coerce(InstVarSet2, VarSet2),

    % Apply the substitution to the modes so that the inst variables
    % from both declarations being checked are contained in the same
    % inst_varset, then check that they are identical.
    varset.merge_renaming(VarSet1, VarSet2, _, InstRenaming),

    % Treat modes as terms here to use term.list_subsumes, which does just
    % what we want here.
    ModeTerms1 = list.map(mode_to_term(output_mercury), Modes1),
    ModeTerms2 = list.map(mode_to_term(output_mercury), Modes2),
    (
        MaybeWithInst1 = yes(Inst1),
        MaybeWithInst2 = yes(Inst2),
        WithInstTerm1 = mode_to_term(output_mercury, free -> Inst1),
        WithInstTerm2 = mode_to_term(output_mercury, free -> Inst2),
        AllModeTerms1 = [WithInstTerm1 | ModeTerms1],
        AllModeTerms2 = [WithInstTerm2 | ModeTerms2]
    ;
        MaybeWithInst1 = no,
        MaybeWithInst2 = no,
        AllModeTerms1 = ModeTerms1,
        AllModeTerms2 = ModeTerms2
    ),

    term.apply_renaming_to_list(AllModeTerms2, InstRenaming,
        SubstAllModeTerms2),
    term.list_subsumes(AllModeTerms1, SubstAllModeTerms2, _),
    term.list_subsumes(SubstAllModeTerms2, AllModeTerms1, _).

    % Combined typeclass method type and mode declarations are split as for
    % ordinary predicate declarations, so the varsets won't necessarily match
    % up if a typeclass declaration is read back from an interface file.
    %
:- pred class_interface_is_unchanged(class_interface::in, class_interface::in)
    is semidet.

class_interface_is_unchanged(Interface0, Interface) :-
    (
        Interface0 = class_interface_abstract,
        Interface = class_interface_abstract
    ;
        Interface0 = class_interface_concrete(Methods1),
        class_methods_are_unchanged(Methods1, Methods2),
        Interface = class_interface_concrete(Methods2)
    ).

:- pred class_methods_are_unchanged(class_methods::in, class_methods::in)
    is semidet.

class_methods_are_unchanged([], []).
class_methods_are_unchanged([Method1 | Methods1], [Method2 | Methods2]) :-
    (
        Method1 = method_pred_or_func(TVarSet1, _, ExistQVars1, PredOrFunc,
            Name, TypesAndModes1, WithType1, _,
            Detism, Cond, Purity, Constraints1, _),
        Method2 = method_pred_or_func(TVarSet2, _, ExistQVars2, PredOrFunc,
            Name, TypesAndModes2, WithType2, _,
            Detism, Cond, Purity, Constraints2, _),
        pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
            TypesAndModes1, WithType1, Constraints1,
            TVarSet2, ExistQVars2, TypesAndModes2, WithType2,
            Constraints2)
    ;
        Method1 = method_pred_or_func_mode(InstVarSet1, PredOrFunc, Name,
            Modes1, WithInst1, Det, Cond, _),
        Method2 = method_pred_or_func_mode(InstVarSet2, PredOrFunc, Name,
            Modes2, WithInst2, Det, Cond, _),
        pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
            InstVarSet2, Modes2, WithInst2)
    ),
    class_methods_are_unchanged(Methods1, Methods2).

%-----------------------------------------------------------------------------%

write_version_numbers(AllVersionNumbers, !IO) :-
    AllVersionNumbers = version_numbers(VersionNumbers,
        InstanceVersionNumbers),
    VersionNumbersList = list.filter_map(
        (func(ItemType) = (ItemType - ItemVersions) is semidet :-
            ItemVersions = extract_ids(VersionNumbers, ItemType),
            \+ map.is_empty(ItemVersions)
        ),
        [type_abstract_item, type_body_item, mode_item, inst_item,
            predicate_item, function_item, typeclass_item]),
    io.write_string("{\n\t", !IO),
    io.write_list(VersionNumbersList, ",\n\t",
        write_item_type_and_versions, !IO),
    ( map.is_empty(InstanceVersionNumbers) ->
        true
    ;
        (
            VersionNumbersList = []
        ;
            VersionNumbersList = [_ | _],
            io.write_string(",\n\t", !IO)
        ),
        io.write_string("instance(", !IO),
        map.to_assoc_list(InstanceVersionNumbers, InstanceAL),
        io.write_list(InstanceAL, ",\n\n\t",
            write_symname_arity_version_number, !IO),
        io.write_string(")\n\t", !IO)
    ),
    io.write_string("\n}", !IO).

:- pred write_item_type_and_versions(
    pair(item_type, map(pair(string, int), version_number))::in,
    io::di, io::uo) is det.

write_item_type_and_versions(ItemType - ItemVersions, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io.write_string(ItemTypeStr, !IO),
    io.write_string("(\n\t\t", !IO),
    map.to_assoc_list(ItemVersions, ItemVersionsList),
    io.write_list(ItemVersionsList, ",\n\t\t",
        write_name_arity_version_number, !IO),
    io.write_string("\n\t)", !IO).

:- pred write_name_arity_version_number(
    pair(pair(string, int), version_number)::in, io::di, io::uo) is det.

write_name_arity_version_number(NameArity - VersionNumber, !IO) :-
    NameArity = Name - Arity,
    mercury_output_bracketed_sym_name_ngt(unqualified(Name),
        next_to_graphic_token, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

:- pred write_symname_arity_version_number(
    pair(item_name, version_number)::in, io::di, io::uo) is det.

write_symname_arity_version_number(ItemName - VersionNumber, !IO) :-
    ItemName = item_name(SymName, Arity),
    mercury_output_bracketed_sym_name_ngt(SymName, next_to_graphic_token, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

%-----------------------------------------------------------------------------%

version_numbers_version_number = 1.

%-----------------------------------------------------------------------------%

parse_version_numbers(VersionNumbersTerm, Result) :-
    (
        VersionNumbersTerm = term.functor(term.atom("{}"),
            VersionNumbersTermList0, _)
    ->
        VersionNumbersTermList = VersionNumbersTermList0
    ;
        VersionNumbersTermList = [VersionNumbersTerm]
    ),
    map_parser(parse_item_type_version_numbers, VersionNumbersTermList,
        Result0),
    (
        Result0 = ok1(List),
        VersionNumbers0 = version_numbers(init_item_id_set(map.init),
            map.init),
        VersionNumbers = list.foldl(
            (func(VNResult, version_numbers(VNs0, Instances0)) =
                    version_numbers(VNs, Instances) :-
                (
                    VNResult = items(ItemType, ItemVNs),
                    VNs = update_ids(VNs0, ItemType, ItemVNs),
                    Instances = Instances0
                ;
                    VNResult = instances(Instances),
                    VNs = VNs0
                )
            ), List, VersionNumbers0),
        Result = ok1(VersionNumbers)
    ;
        Result0 = error1(Errors),
        Result = error1(Errors)
    ).

:- type item_version_numbers_result
    --->    items(item_type, version_number_map)
    ;       instances(instance_version_numbers).

:- pred parse_item_type_version_numbers(term::in,
    maybe1(item_version_numbers_result)::out) is det.

parse_item_type_version_numbers(Term, Result) :-
    (
        Term = term.functor(term.atom(ItemTypeStr), ItemsVNsTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    ->
        ParseName =
            (pred(NameTerm::in, Name::out) is semidet :-
                NameTerm = term.functor(term.atom(Name), [], _)
            ),
        map_parser(parse_key_version_number(ParseName), ItemsVNsTerms,
            Result0),
        (
            Result0 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(items(ItemType, VNsMap))
        ;
            Result0 = error1(Specs),
            Result = error1(Specs)
        )
    ;
        Term = term.functor(term.atom("instance"), InstanceVNsTerms, _)
    ->
        map_parser(parse_item_version_number(try_parse_sym_name_and_no_args),
            InstanceVNsTerms, Result1),
        (
            Result1 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(instances(VNsMap))
        ;
            Result1 = error1(Specs),
            Result = error1(Specs)
        )
    ;
        % XXX This is an uninformative error message.
        Pieces = [words("Invalid item type version numbers."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- pred parse_key_version_number(
    pred(term, string)::(pred(in, out) is semidet), term::in,
    maybe1(pair(pair(string, arity), version_number))::out) is det.

parse_key_version_number(ParseName, Term, Result) :-
    (
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, Name),
        ArityTerm = term.functor(term.integer(Arity), _, _),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    ->
        Result = ok1((Name - Arity) - VersionNumber)
    ;
        Pieces = [words("Error in item version number."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- pred parse_item_version_number(
    pred(term, sym_name)::(pred(in, out) is semidet), term::in,
    maybe1(pair(item_name, version_number))::out) is det.

parse_item_version_number(ParseName, Term, Result) :-
    (
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, SymName),
        ArityTerm = term.functor(term.integer(Arity), _, _),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    ->
        Result = ok1(item_name(SymName, Arity) - VersionNumber)
    ;
        Pieces = [words("Error in item version number."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
:- end_module recompilation.version.
%-----------------------------------------------------------------------------%
