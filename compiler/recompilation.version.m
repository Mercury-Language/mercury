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

:- import_module libs.
:- import_module libs.timestamp.
:- import_module parse_tree.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

    % compute_version_numbers(SourceFileModTime, CurParseTreeInt,
    %   MaybeOldParseTreeInt, VersionNumbers).
    %
:- pred compute_version_numbers(timestamp::in, parse_tree_int::in,
    maybe(parse_tree_int)::in, version_numbers::out) is det.

:- pred write_version_numbers(version_numbers::in, io::di, io::uo) is det.

    % The version number for the format of the version numbers
    % written to the interface files.
    %
:- func version_numbers_version_number = int.

    % Parse a term that maps item ids to timestamps. These terms
    % look like this:
    %
    % {
    %     type(
    %         state_mc/0 - "2015-10-16 08:51:02",
    %         state_no/0 - "2015-10-16 08:51:02",
    %         transition/0 - "2015-10-16 08:51:02",
    %         transitions/0 - "2015-10-16 08:51:02"
    %     ),
    %     type_body(
    %         state_mc/0 - "2015-10-16 08:51:02",
    %         state_no/0 - "2015-10-16 08:51:02",
    %         transition/0 - "2015-10-16 08:51:02",
    %         transitions/0 - "2015-10-16 08:51:02"
    %     ),
    %     inst(
    %         atom_transition/0 - "2015-10-16 08:51:02",
    %         atom_transitions/0 - "2015-10-16 08:51:02",
    %         null_transition/0 - "2015-10-16 08:51:02",
    %         null_transition_free_state_mc/0 - "2015-10-16 08:51:02",
    %         null_transitions/0 - "2015-10-16 08:51:02"
    %     )
    % }
    %
:- pred parse_version_numbers(term::in, maybe1(version_numbers)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.parse_tree_to_term.

:- import_module assoc_list.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

compute_version_numbers(SourceFileTime, CurParseTreeInt, MaybeOldParseTreeInt,
        NewVersionNumbers) :-
    CurParseTreeInt = parse_tree_int(_ModuleName, _IntFileKind,
        _ModuleNameContext, _CurMaybeVersionNumbers,
        _CurIntIncls, _CurImpIncls, _CurIntAvails, _CurImpAvails,
        _CurIntFIMs, _CurImpFIMs, CurIntItems, CurImpItems),
    gather_items(CurIntItems, CurImpItems, CurGatheredItems, CurInstanceItems),
    ( if
        MaybeOldParseTreeInt = yes(OldParseTreeInt),
        OldParseTreeInt = parse_tree_int(_, _, _, OldMaybeVersionNumbers,
            _OldIntIncls, _OldImpIncls, _OldIntAvails, _OldImpAvails,
            _OldIntFIMs, _OldImpFIMs, OldIntItems, OldImpItems),
        OldMaybeVersionNumbers = version_numbers(OldVersionNumbers)
    then
        OldVersionNumbers = version_numbers(OldItemVersionNumbers,
            OldInstanceVersionNumbers),
        gather_items(OldIntItems, OldImpItems,
            OldGatheredItems, OldInstanceItems)
    else
        % There were no old version numbers, so every item gets
        % the same timestamp as the source module.
        % XXX ITEM_LIST In which case, the call to compute_item_version_numbers
        % below is mostly a waste of time, since we could get the same job done
        % more quickly without doing a lot of lookups in empty maps.
        OldItemVersionNumbers = init_item_id_set(map.init),
        OldGatheredItems = init_item_id_set(map.init),
        map.init(OldInstanceItems),
        map.init(OldInstanceVersionNumbers)
    ),
    compute_item_version_numbers(SourceFileTime,
        CurGatheredItems, OldGatheredItems,
        OldItemVersionNumbers, NewItemVersionNumbers),
    compute_instance_version_numbers(SourceFileTime,
        CurInstanceItems, OldInstanceItems,
        OldInstanceVersionNumbers, NewInstanceVersionNumbers),
    NewVersionNumbers =
        version_numbers(NewItemVersionNumbers, NewInstanceVersionNumbers).

:- pred compute_item_version_numbers(timestamp::in,
    gathered_items::in, gathered_items::in,
    item_version_numbers::in, item_version_numbers::out) is det.

compute_item_version_numbers(SourceFileTime,
        CurGatheredItems, OldGatheredItems,
        OldItemVersionNumbers, NewItemVersionNumbers) :-
    Func = compute_item_version_numbers_2(SourceFileTime,
        OldGatheredItems, OldItemVersionNumbers),
    NewItemVersionNumbers = map_ids(Func, CurGatheredItems, map.init).

:- func compute_item_version_numbers_2(timestamp, gathered_items,
    item_version_numbers, item_type,
    map(pair(string, arity), assoc_list(module_section, item)))
    = map(pair(string, arity), timestamp).

compute_item_version_numbers_2(SourceFileTime, OldGatheredItems,
        OldItemVersionNumbers, ItemType, CurGatheredItems) =
    map.map_values(
        compute_item_version_numbers_3(SourceFileTime,
            OldGatheredItems, OldItemVersionNumbers, ItemType),
        CurGatheredItems).

:- func compute_item_version_numbers_3(timestamp, gathered_items,
    item_version_numbers, item_type, pair(string, arity),
    assoc_list(module_section, item)) = timestamp.

compute_item_version_numbers_3(SourceFileTime, OldGatheredItems,
        OldItemVersionNumbers, ItemType, NameArity, CurItems) = TimeStamp :-
    OldIds = extract_ids(OldGatheredItems, ItemType),
    OldItemTypeVersionNumbers = extract_ids(OldItemVersionNumbers, ItemType),
    ( if
        map.search(OldIds, NameArity, OldItems),
        % We call order_items on the items in both the interface and the
        % implementation of the current parse tree, but doing the same for
        % the previous parse tree would be overkill. However, for some "item"
        % types, such as predicate_item, OldItems and CurItems may contain
        % more than one prog_item.item. We don't want this artificially-created
        % difference in the ORDER of those items to count as a difference
        % that requires a recompilation.
        list.sort(OldItems, SortedOldItems),
        list.sort(CurItems, SortedCurItems),
        are_items_changed(SortedOldItems, SortedCurItems, unchanged),
        map.search(OldItemTypeVersionNumbers, NameArity, OldItemVersionNumber)
    then
        TimeStamp = OldItemVersionNumber
    else
        TimeStamp = SourceFileTime
    ).

:- pred compute_instance_version_numbers(timestamp::in,
    instance_item_map::in, instance_item_map::in,
    instance_version_numbers::in, instance_version_numbers::out) is det.

compute_instance_version_numbers(SourceFileTime,
        CurInstanceItemMap, OldInstanceItemMap,
        OldInstanceVersionNumbers, NewInstanceVersionNumbers) :-
    NewInstanceVersionNumbers = map.map_values(
        ( func(ClassId, Items) = InstanceVersionNumber :-
            ( if
                map.search(OldInstanceItemMap, ClassId, OldItems),
                are_items_changed(OldItems, Items, unchanged),
                map.search(OldInstanceVersionNumbers, ClassId,
                    OldInstanceVersionNumber)
            then
                InstanceVersionNumber = OldInstanceVersionNumber
            else
                InstanceVersionNumber = SourceFileTime
            )
        ),
        CurInstanceItemMap
    ).

%-----------------------------------------------------------------------------%

:- pred gather_items(list(item)::in, list(item)::in,
    gathered_items::out, instance_item_map::out) is det.

gather_items(IntItems, ImpItems, GatheredItems, Instances) :-
    GatheredItems0 = init_item_id_set(map.init),
    Info0 = gathered_item_info(GatheredItems0, cord.init, map.init),
    gather_in_section(ms_interface, IntItems, Info0, Info1),
    gather_in_section(ms_implementation, ImpItems, Info1, Info),
    % Items which could appear in _OtherItems (those which aren't gathered
    % into the list for another type of item) can't appear in the interface
    % section. Those other items (e.g. assertions) will need to be handled here
    % when smart recompilation is made to work with
    % `--intermodule-optimization'.
    Info = gathered_item_info(GatheredItems1, PragmaItemsCord, Instances),
    PragmaItems = cord.list(PragmaItemsCord),
    list.foldl(distribute_pragma_items, PragmaItems,
        GatheredItems1, GatheredItems).

%-----------------------------------------------------------------------------%

:- type gathered_item_info
    --->    gathered_item_info(
                gii_gathered_items  :: gathered_items,
                gii_pragma_items    :: cord({maybe_pred_or_func_id,
                                            item, module_section}),
                gii_instances       :: instance_item_map
            ).

:- type instance_item_map ==
    map(item_name, assoc_list(module_section, item)).

    % The constructors set should always be empty.
:- type gathered_items == item_id_set(gathered_item_map).
:- type gathered_item_map == map(pair(string, arity),
    assoc_list(module_section, item)).

:- pred gather_in_section(module_section::in, list(item)::in,
    gathered_item_info::in, gathered_item_info::out) is det.

gather_in_section(_Section, [], !Info).
gather_in_section(Section, [Item | Items], !Info) :-
    gather_in_item(Section, Item, !Info),
    gather_in_section(Section, Items, !Info).

:- pred gather_in_item(module_section::in, item::in,
    gathered_item_info::in, gathered_item_info::out) is det.

gather_in_item(Section, Item, !Info) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, Args, Body,
            VarSet, Context, SeqNum),
        (
            Body = parse_tree_abstract_type(_),
            NameItem = Item,
            % The body of an abstract type can be recorded as used when
            % generating a call to the automatically generated unification
            % procedure.
            BodyItem = Item
        ;
            Body = parse_tree_du_type(_),
            % XXX does the abstract_details matter here?
            % XXX TYPE_REPN zs: yes, it should, when it changes in a way that
            % affects decisions about the representations of other types
            % that include the abstract type. That means that *assuming*
            % this value for AbstractDetails is a BUG.
            AbstractDetails = abstract_type_general,
            AbstractBody = parse_tree_abstract_type(AbstractDetails),
            NameItemTypeDefn = item_type_defn_info(Name, Args, AbstractBody,
                VarSet, Context, SeqNum),
            NameItem = item_type_defn(NameItemTypeDefn),
            BodyItem = Item
        ;
            Body = parse_tree_eqv_type(_),
            % When we use an equivalence type we always use the body.
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_solver_type(_),
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_foreign_type(_),
            NameItem = Item,
            BodyItem = Item
        ),
        TypeCtorItem = item_name(Name, list.length(Args)),
        GatheredItems0 = !.Info ^ gii_gathered_items,
        add_gathered_item(NameItem, item_id(type_abstract_item, TypeCtorItem),
            Section, GatheredItems0, GatheredItems1),
        add_gathered_item(BodyItem, item_id(type_body_item, TypeCtorItem),
            Section, GatheredItems1, GatheredItems),
        !Info ^ gii_gathered_items := GatheredItems
    ;
        % For predicates or functions defined using `with_inst` annotations
        % the pred_or_func and arity here won't be correct, but equiv_type.m
        % will record the dependency on the version number with the `incorrect'
        % pred_or_func and arity, so this will work.
        Item = item_mode_decl(ItemModeDecl),
        ItemModeDecl = item_mode_decl_info(SymName, MaybePredOrFunc, Modes,
            WithInst, _, _, _, _),
        ( if
            MaybePredOrFunc = no,
            WithInst = yes(_)
        then
            GatheredItems0 = !.Info ^ gii_gathered_items,
            ItemName = item_name(SymName, list.length(Modes)),
            add_gathered_item(Item, item_id(predicate_item, ItemName),
                Section, GatheredItems0, GatheredItems1),
            add_gathered_item(Item, item_id(function_item, ItemName),
                Section, GatheredItems1, GatheredItems),
            !Info ^ gii_gathered_items := GatheredItems
        else
            (
                MaybePredOrFunc = yes(PredOrFunc),
                adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
                ItemType = pred_or_func_to_item_type(PredOrFunc),
                ItemId = item_id(ItemType, item_name(SymName, Arity)),
                GatheredItems0 = !.Info ^ gii_gathered_items,
                add_gathered_item(Item, ItemId, Section,
                    GatheredItems0, GatheredItems),
                !Info ^ gii_gathered_items := GatheredItems
            ;
                MaybePredOrFunc = no
                % We don't have an item_id, so we cannot gather the item.
                % XXX Does this lead to missing some needed recompilations?
            )
        )
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(ClassName, ClassArgs,
            _, _, _, _, _, _, _),
        Instances0 = !.Info ^ gii_instances,
        ClassArity = list.length(ClassArgs),
        ClassItemName = item_name(ClassName, ClassArity),
        NewInstanceItem = Section - Item,
        ( if map.search(Instances0, ClassItemName, OldInstanceItems) then
            NewInstanceItems = [NewInstanceItem | OldInstanceItems],
            map.det_update(ClassItemName, NewInstanceItems,
                Instances0, Instances)
        else
            map.det_insert(ClassItemName, [NewInstanceItem],
                Instances0, Instances)
        ),
        !Info ^ gii_instances := Instances
    ;
        ( Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        )
        % Do nothing.
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        ItemDeclPragma = item_pragma_info(DeclPragma, _, _),
        ( if is_pred_decl_pragma(DeclPragma, yes(PredOrFuncId)) then
            PragmaItems0 = !.Info ^ gii_pragma_items,
            PragmaItems = cord.snoc(PragmaItems0,
                {PredOrFuncId, Item, Section}),
            !Info ^ gii_pragma_items := PragmaItems
        else
            true
        )
    ;
        Item = item_impl_pragma(ItemImplPragma),
        ItemImplPragma = item_pragma_info(ImplPragma, _, _),
        ( if is_pred_impl_pragma(ImplPragma, yes(PredOrFuncId)) then
            PragmaItems0 = !.Info ^ gii_pragma_items,
            PragmaItems = cord.snoc(PragmaItems0,
                {PredOrFuncId, Item, Section}),
            !Info ^ gii_pragma_items := PragmaItems
        else
            true
        )
    ;
        Item = item_generated_pragma(ItemGenPragma),
        ItemGenPragma = item_pragma_info(GenPragma, _, _),
        ( if is_pred_gen_pragma(GenPragma, yes(PredOrFuncId)) then
            PragmaItems0 = !.Info ^ gii_pragma_items,
            PragmaItems = cord.snoc(PragmaItems0,
                {PredOrFuncId, Item, Section}),
            !Info ^ gii_pragma_items := PragmaItems
        else
            true
        )
    ;
        (
            Item = item_inst_defn(ItemInstDefn),
            ItemInstDefn = item_inst_defn_info(Name, Params, _, _, _, _, _),
            list.length(Params, Arity),
            ItemId = item_id(inst_item, item_name(Name, Arity))
        ;
            Item = item_mode_defn(ItemModeDefn),
            ItemModeDefn = item_mode_defn_info(Name, Params, _, _, _, _),
            list.length(Params, Arity),
            ItemId = item_id(mode_item, item_name(Name, Arity))
        ;
            Item = item_pred_decl(ItemPredDecl),
            ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc,
                TypesAndModes, WithType, _, _, _, _, _, _, _, _, _, _),
            % For predicates or functions defined using `with_type` annotations
            % the arity here won't be correct, but equiv_type.m will record
            % the dependency on the version number with the `incorrect' arity,
            % so this will work.
            (
                WithType = no,
                adjust_func_arity(PredOrFunc, Arity,
                    list.length(TypesAndModes))
            ;
                WithType = yes(_),
                Arity = list.length(TypesAndModes)
            ),
            ItemType = pred_or_func_to_item_type(PredOrFunc),
            ItemId = item_id(ItemType, item_name(SymName, Arity))
        ;
            Item = item_typeclass(ItemTypeClass),
            ItemTypeClass = item_typeclass_info(ClassName, ClassVars, _, _,
                _, _, _, _),
            list.length(ClassVars, ClassArity),
            ItemId = item_id(typeclass_item, item_name(ClassName, ClassArity))
        ),
        GatheredItems0 = !.Info ^ gii_gathered_items,
        add_gathered_item(Item, ItemId, Section,
            GatheredItems0, GatheredItems),
        !Info ^ gii_gathered_items := GatheredItems
    ;
        Item = item_promise(_)
        % Do nothing; don't gather the item.
        % XXX This is likely to be a bug. If the old version of an interface
        % file makes a promise that the new version of that interface file
        % doesn't, then any importing module whose compilation made use
        % of that promise *needs* to be recompiled, but we don't detect
        % that need. The only reason why we haven't come across this bug
        % in real life is that (a) promises are very rare, and (b) when
        % they *do* occur, they tend to be very stable.
    ;
        Item = item_type_repn(ItemTypeRepn),
        ItemTypeRepn =
            item_type_repn_info(TypeCtorSymName, TypeCtorArgs, _, _, _, _),
        list.length(TypeCtorArgs, TypeCtorArity),
        TypeCtorItem = item_name(TypeCtorSymName, TypeCtorArity),
        GatheredItems0 = !.Info ^ gii_gathered_items,
        add_gathered_item(Item, item_id(type_body_item, TypeCtorItem),
            Section, GatheredItems0, GatheredItems),
        !Info ^ gii_gathered_items := GatheredItems
    ;
        ( Item = item_clause(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        % Such items should not appear in interfaces.
        unexpected($pred, "unexpected item in interface")
    ).

:- pred add_gathered_item(item::in, item_id::in, module_section::in,
    gathered_items::in, gathered_items::out) is det.

add_gathered_item(Item, ItemId, Section, !GatheredItems) :-
    ItemId = item_id(ItemType, ItemName),
    ItemName = item_name(SymName, Arity),
    Name = unqualify_name(SymName),
    NameArity = Name - Arity,
    % mercury_to_mercury.m splits combined pred and mode declarations.
    % XXX ITEM_LIST Maybe that should be fixed, instead of this workaround.
    % That needs to be done here as well the item list read from the interface
    % file will match the item list generated here.
    ( if
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info( PredName, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            Origin, TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
            Context, SeqNum),
        split_types_and_modes(TypesAndModes, Types, MaybeModes),
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    then
        TypesWithoutModes = list.map((func(Type) = type_only(Type)), Types),
        varset.init(EmptyInstVarSet),
        PredItemPredDecl = item_pred_decl_info(PredName, PredOrFunc,
            TypesWithoutModes, WithType, no, no, Origin,
            TypeVarSet, EmptyInstVarSet, ExistQVars, Purity, Constraints,
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
        ModeItemModeDecl = item_mode_decl_info(PredName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, Context, SeqNum),
        ModeItem = item_mode_decl(ModeItemModeDecl),
        AddedItems = [Section - PredItem, Section - ModeItem]
    else if
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass ^ tc_class_methods = class_interface_concrete(Decls0)
    then
        DeclsList = list.map(split_class_method_types_and_modes, Decls0),
        list.condense(DeclsList, Decls),
        NewItemTypeClass = ItemTypeClass ^ tc_class_methods
            := class_interface_concrete(Decls),
        NewItem = item_typeclass(NewItemTypeClass),
        AddedItems = [Section - NewItem]
    else
        AddedItems = [Section - Item]
    ),
    IdMap0 = extract_ids(!.GatheredItems, ItemType),
    ( if map.search(IdMap0, NameArity, OldItems) then
        map.det_update(NameArity, AddedItems ++ OldItems, IdMap0, IdMap)
    else
        map.det_insert(NameArity, AddedItems, IdMap0, IdMap)
    ),
    update_ids(ItemType, IdMap, !GatheredItems).

:- func split_class_method_types_and_modes(class_decl) = list(class_decl).

split_class_method_types_and_modes(Decl0) = Decls :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(SymName, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints, _Context),
        ( if
            split_types_and_modes(TypesAndModes, Types, MaybeModes),
            MaybeModes = yes(Modes),
            ( Modes = [_ | _]
            ; WithInst = yes(_)
            )
        then
            TypesWithoutModes =
                list.map((func(Type) = type_only(Type)), Types),
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
            ModeInfo = class_mode_info(SymName, MaybePredOrFunc,
                Modes, WithInst, MaybeDetism,
                InstVarSet, term.context_init),
            ModeDecl = class_decl_mode(ModeInfo),
            ModeDecls = [ModeDecl]
        else
            TypesWithoutModes = TypesAndModes,
            ModeDecls = []
        ),
        varset.init(EmptyInstVarSet),
        PredOrFuncInfo = class_pred_or_func_info(SymName, PredOrFunc,
            TypesWithoutModes, WithType, no, no, TypeVarSet, EmptyInstVarSet,
            ExistQVars, Purity, Constraints, term.context_init),
        PredOrFuncDecl = class_decl_pred_or_func(PredOrFuncInfo),
        Decls = [PredOrFuncDecl | ModeDecls]
    ;
        Decl0 = class_decl_mode(ModeInfo0),
        ModeInfo0 = class_mode_info(SymName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, _Context),
        ModeInfo = class_mode_info(SymName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, term.context_init),
        Decl = class_decl_mode(ModeInfo),
        Decls = [Decl]
    ).

%-----------------------------------------------------------------------------%

:- pred distribute_pragma_items(
    {maybe_pred_or_func_id, item, module_section}::in,
    gathered_items::in, gathered_items::out) is det.

distribute_pragma_items({ItemId, Item, Section}, !GatheredItems) :-
    ItemId = MaybePredOrFunc - sym_name_arity(SymName, Arity),

    % For predicates defined using `with_type` annotations we don't know
    % the actual arity, so always we need to add entries for pragmas, even if
    % the pragma doesn't match any recorded predicate. For pragmas which don't
    % include enough information to work out whether they apply to a predicate
    % or a function, this will result in an extra entry in the version numbers.
    % Pragmas in the interface aren't common so this won't be too much of
    % a problem.
    ItemName = item_name(SymName, Arity),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        add_gathered_item(Item, item_id(ItemType, ItemName),
            Section, !GatheredItems)
    ;
        MaybePredOrFunc = no,
        add_gathered_item(Item, item_id(predicate_item, ItemName),
            Section, !GatheredItems),
        add_gathered_item(Item, item_id(function_item, ItemName),
            Section, !GatheredItems)
    ),

    % Pragmas can apply to typeclass methods.
    map.map_values_only(distribute_pragma_items_class_items(MaybePredOrFunc,
        SymName, Arity, Item, Section),
        extract_ids(!.GatheredItems, typeclass_item), GatheredTypeClasses),
    update_ids(typeclass_item, GatheredTypeClasses, !GatheredItems).

:- pred distribute_pragma_items_class_items(maybe(pred_or_func)::in,
    sym_name::in, arity::in, item::in, module_section::in,
    assoc_list(module_section, item)::in,
    assoc_list(module_section, item)::out) is det.

distribute_pragma_items_class_items(MaybePredOrFunc, SymName, Arity,
        Item, Section, !ClassItems) :-
    ( if
        % Does this pragma match any of the methods of this class.
        list.member(_ - ClassItem, !.ClassItems),
        ClassItem = item_typeclass(ClassItemTypeClass),
        ClassItemTypeClass ^ tc_class_methods =
            class_interface_concrete(Decls),
        list.member(Decl, Decls),
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(SymName, MethodPredOrFunc,
            TypesAndModes, WithType, _, _, _, _, _, _, _, _),
        ( MaybePredOrFunc = yes(MethodPredOrFunc)
        ; MaybePredOrFunc = no
        ),
        (
            WithType = no,
            adjust_func_arity(MethodPredOrFunc, Arity,
                list.length(TypesAndModes))
        ;
            WithType = yes(_)
            % We don't know the actual arity, so just match on the name
            % and pred_or_func.
        )
    then
        % XXX O(N^2), but shouldn't happen too often.
        !:ClassItems = !.ClassItems ++ [Section - Item]
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- type maybe_pred_or_func_id == pair(maybe(pred_or_func), sym_name_arity).

:- pred is_pred_decl_pragma(decl_pragma::in,
    maybe(maybe_pred_or_func_id)::out) is det.

is_pred_decl_pragma(DeclPragma, MaybePredOrFuncId) :-
    (
        DeclPragma = decl_pragma_type_spec(TypeSpecInfo),
        TypeSpecInfo = pragma_info_type_spec(Name, _, Arity, MaybePredOrFunc,
            _, _, _, _),
        MaybePredOrFuncId = yes(MaybePredOrFunc - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsoleteProcInfo),
        ObsoleteProcInfo = pragma_info_obsolete_proc(PredNameModesPF, _),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_obsolete_pred(ObsoletePredInfo),
        ObsoletePredInfo = pragma_info_obsolete_pred(PredNameArity, _),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_oisu(_),              % XXX
        MaybePredOrFuncId = no
    ;
        ( DeclPragma = decl_pragma_terminates(PredNameArity)
        ; DeclPragma = decl_pragma_does_not_terminate(PredNameArity)
        ; DeclPragma = decl_pragma_check_termination(PredNameArity)
        ),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - sym_name_arity(Name, Arity))
    ;
        (
            DeclPragma = decl_pragma_termination_info(TermInfo),
            TermInfo = pragma_info_termination_info(PredNameModesPF, _, _)
        ;
            DeclPragma = decl_pragma_termination2_info(Term2Info),
            Term2Info = pragma_info_termination2_info(PredNameModesPF, _, _, _)
        ;
            DeclPragma = decl_pragma_structure_sharing(SharingInfo),
            SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
                _, _, _, _, _)
        ;
            DeclPragma = decl_pragma_structure_reuse(ReuseInfo),
            ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
                _, _, _, _, _)
        ),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ).

:- pred is_pred_impl_pragma(impl_pragma::in,
    maybe(maybe_pred_or_func_id)::out) is det.

is_pred_impl_pragma(ImplPragma, MaybePredOrFuncId) :-
    (
        ( ImplPragma = impl_pragma_foreign_decl(_)
        ; ImplPragma = impl_pragma_foreign_code(_)
        ; ImplPragma = impl_pragma_require_feature_set(_)
        ; ImplPragma = impl_pragma_require_tail_rec(_)
        ),
        MaybePredOrFuncId = no
    ;
        ( ImplPragma = impl_pragma_inline(PredNameArity)
        ; ImplPragma = impl_pragma_no_inline(PredNameArity)
        ; ImplPragma = impl_pragma_consider_used(PredNameArity)
        ; ImplPragma = impl_pragma_no_detism_warning(PredNameArity)
        ; ImplPragma = impl_pragma_promise_pure(PredNameArity)
        ; ImplPragma = impl_pragma_promise_semipure(PredNameArity)
        ; ImplPragma = impl_pragma_promise_eqv_clauses(PredNameArity)
        ; ImplPragma = impl_pragma_mode_check_clauses(PredNameArity)
        ),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - sym_name_arity(Name, Arity))
    ;
        ImplPragma = impl_pragma_fact_table(FTInfo),
        FTInfo = pragma_info_fact_table(PredNameArity, _),
        PredNameArity = pred_name_arity(Name, Arity),
        MaybePredOrFuncId = yes(no - sym_name_arity(Name, Arity))
    ;
        ImplPragma = impl_pragma_tabled(TabledInfo),
        TabledInfo = pragma_info_tabled(_, PredNameArityMPF, _, _),
        PredNameArityMPF = pred_name_arity_mpf(Name, Arity, MaybePredOrFunc),
        MaybePredOrFuncId = yes(MaybePredOrFunc - sym_name_arity(Name, Arity))
    ;
        ImplPragma = impl_pragma_foreign_proc(FPInfo),
        FPInfo = pragma_info_foreign_proc(_, Name, PredOrFunc, Args, _, _, _),
        adjust_func_arity(PredOrFunc, Arity, list.length(Args)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ;
        ImplPragma = impl_pragma_external_proc(ExternalInfo),
        ExternalInfo = pragma_info_external_proc(Name, Arity, PredOrFunc, _),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ;
        ImplPragma = impl_pragma_foreign_proc_export(FPEInfo),
        FPEInfo = pragma_info_foreign_proc_export(_, _, PredNameModesPF, _),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ).

:- pred is_pred_gen_pragma(generated_pragma::in,
    maybe(maybe_pred_or_func_id)::out) is det.

is_pred_gen_pragma(GenPragma, MaybePredOrFuncId) :-
    (
        GenPragma = gen_pragma_unused_args(UnusedArgsInfo),
        UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, _)
    ;
        GenPragma = gen_pragma_exceptions(ExceptionsInfo),
        ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, _)
    ;
        GenPragma = gen_pragma_trailing_info(TrailingInfo),
        TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn, _)
    ;
        GenPragma = gen_pragma_mm_tabling_info(MMTablingOnfo),
        MMTablingOnfo = pragma_info_mm_tabling_info(PredNameArityPFMn, _)
    ),
    PredNameArityPFMn = pred_name_arity_pf_mn(Name, Arity, PredOrFunc, _),
    MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity)).

%-----------------------------------------------------------------------------%
%
% Check whether various things are unchanged.
%
% XXX This code is a bit brittle, because in some places the things being
% compared include term.contexts, which can change even if nothing we care
% about has been modified. For example, it won't work for clauses, which
% have lots of contexts inside them.
%
% However, the important thing is that these predicates will never succeed
% when they shouldn't, so they should never cause a necessary recompilation
% to be missed.
%

:- type maybe_changed
    --->    unchanged
    ;       changed.

    % XXX This predicate is unused, which is likely to be a bug.
    %
:- pred is_item_include_changed(item_include::in, item_include::in,
    maybe_changed::out) is det.
:- pragma consider_used(is_item_include_changed/3).

is_item_include_changed(ItemInclude1, ItemInclude2, Changed) :-
    ItemInclude1 = item_include(ModuleName1, _, _),
    ItemInclude2 = item_include(ModuleName2, _, _),
    ( if ModuleName1 = ModuleName2 then
        Changed = unchanged
    else
        Changed = changed
    ).

    % XXX This predicate is unused, which is likely to be a bug.
    %
:- pred is_item_avail_changed(item_avail::in, item_avail::in,
    maybe_changed::out) is det.
:- pragma consider_used(is_item_avail_changed/3).

is_item_avail_changed(Avail1, Avail2, Changed) :-
    (
        Avail1 = avail_import(avail_import_info(ModuleName1, _, _)),
        ( if
            Avail2 = avail_import(avail_import_info(ModuleName2, _, _)),
            ModuleName1 = ModuleName2
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Avail1 = avail_use(avail_use_info(ModuleName1, _, _)),
        ( if
            Avail2 = avail_use(avail_use_info(ModuleName2, _, _)),
            ModuleName1 = ModuleName2
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ).

:- pred are_items_changed(assoc_list(module_section, item)::in,
    assoc_list(module_section, item)::in, maybe_changed::out) is det.

are_items_changed([], [], unchanged).
are_items_changed([], [_ | _], changed).
are_items_changed([_ | _], [], changed).
are_items_changed([Section1 - Item1 | Items1], [Section2 - Item2 | Items2],
        Changed) :-
    ( if Section1 = Section2 then
        is_item_changed(Item1, Item2, ItemChanged),
        (
            ItemChanged = changed,
            Changed = changed
        ;
            ItemChanged = unchanged,
            are_items_changed(Items1, Items2, Changed)
        )
    else
        Changed = changed
    ).

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
    % It is important not to compare the varsets for type and instance
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
:- pred is_item_changed(item::in, item::in, maybe_changed::out) is det.

is_item_changed(Item1, Item2, Changed) :-
    (
        Item1 = item_clause(ItemClause1),
        ItemClause1 = item_clause_info(_, _, PorF, SymName, Args, Goal, _, _),
        % XXX Need to compare the goals properly in clauses and assertions.
        % That is not necessary at the moment because smart recompilation
        % doesn't work with inter-module optimization yet.
        ( if
            Item2 = item_clause(ItemClause2),
            ItemClause2 =
                item_clause_info(_, _, PorF, SymName, Args, Goal, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_type_defn(ItemTypeDefn1),
        ItemTypeDefn1 = item_type_defn_info(_, Name, Args, Defn, _, _),
        ( if
            Item2 = item_type_defn(ItemTypeDefn2),
            ItemTypeDefn2 = item_type_defn_info(_, Name, Args, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_inst_defn(ItemInstDefn1),
        ItemInstDefn1 = item_inst_defn_info(_, Name, Args,
            MaybeForTypeCtor, Defn, _, _),
        ( if
            Item2 = item_inst_defn(ItemInstDefn2),
            ItemInstDefn2 = item_inst_defn_info(_, Name, Args,
                MaybeForTypeCtor, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mode_defn(ItemModeDefn1),
        ItemModeDefn1 = item_mode_defn_info(_, Name, Args, Defn, _, _),
        ( if
            Item2 = item_mode_defn(ItemModeDefn2),
            ItemModeDefn2 = item_mode_defn_info(_, Name, Args, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_pred_decl(ItemPredDecl1),
        ItemPredDecl1 = item_pred_decl_info(Name, PredOrFunc,
            TypesAndModes1, WithType1, _, Det1, _, TVarSet1, _,
            ExistQVars1, Purity, Constraints1, _, _),
        ( if
            Item2 = item_pred_decl(ItemPredDecl2),
            ItemPredDecl2 = item_pred_decl_info(Name, PredOrFunc,
                TypesAndModes2, WithType2, _, Det2, _, TVarSet2, _,
                ExistQVars2, Purity, Constraints2, _, _),

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
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mode_decl(ItemModeDecl1),
        ItemModeDecl1 = item_mode_decl_info(Name, PredOrFunc, Modes1,
            WithInst1, Det, InstVarSet1, _, _),
        ( if
            Item2 = item_mode_decl(ItemModeDecl2),
            ItemModeDecl2 = item_mode_decl_info(Name, PredOrFunc, Modes2,
                WithInst2, Det, InstVarSet2, _, _),
            pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
                InstVarSet2, Modes2, WithInst2)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_foreign_enum(ItemForeignEnum1),
        ItemForeignEnum1 = item_foreign_enum_info(Lang, TypeCtor, Values, _, _),
        ( if
            Item2 = item_foreign_enum(ItemForeignEnum2),
            ItemForeignEnum2 = item_foreign_enum_info(Lang, TypeCtor, Values,
                _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_foreign_export_enum(ItemForeignEnum1),
        ItemForeignEnum1 = item_foreign_export_enum_info(Lang,
            TypeCtor, Attrs, Overrides, _, _),
        ( if
            Item2 = item_foreign_export_enum(ItemForeignEnum2),
            ItemForeignEnum2 = item_foreign_export_enum_info(Lang,
                TypeCtor, Attrs, Overrides, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_decl_pragma(ItemDeclPragma1),
        ItemDeclPragma1 = item_pragma_info(DeclPragma1, _, _),
        % We do need to compare the variable names in `:- pragma type_spec'
        % declarations because the names of the variables are used to find
        % the corresponding variables in the predicate or function
        % type declaration.
        ( if
            Item2 = item_decl_pragma(ItemDeclPragma2),
            ItemDeclPragma2 = item_pragma_info(DeclPragma2, _, _)
        then
            ( if
                DeclPragma1 = decl_pragma_type_spec(TypeSpecInfo1),
                DeclPragma2 = decl_pragma_type_spec(TypeSpecInfo2),
                TypeSpecInfo1 = pragma_info_type_spec(Name, SpecName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubst1, TVarSet1, _),
                TypeSpecInfo2 = pragma_info_type_spec(Name, SpecName, Arity,
                    MaybePredOrFunc, MaybeModes, TypeSubst2, TVarSet2, _)
            then
                assoc_list.keys_and_values(TypeSubst1, TVars1, Types1),
                assoc_list.keys_and_values(TypeSubst2, TVars2, Types2),
                % XXX kind inference:
                % we assume vars have kind `star'.
                KindMap = map.init,
                prog_type.var_list_to_type_list(KindMap, TVars1, TVarTypes1),
                prog_type.var_list_to_type_list(KindMap, TVars2, TVarTypes2),
                ( if
                    type_list_is_unchanged(
                        TVarSet1, TVarTypes1 ++ Types1,
                        TVarSet2, TVarTypes2 ++ Types2,
                        _, _, _)
                then
                    Changed = unchanged
                else
                    Changed = changed
                )
            else
                ( if DeclPragma1 = DeclPragma2 then
                    Changed = unchanged
                else
                    Changed = changed
                )
            )
        else
            Changed = changed
        )
    ;
        Item1 = item_impl_pragma(ItemImplPragma1),
        ItemImplPragma1 = item_pragma_info(ImplPragma, _, _),
        ( if
            Item2 = item_impl_pragma(ItemImplPragma2),
            ItemImplPragma2 = item_pragma_info(ImplPragma, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_generated_pragma(ItemGenPragma1),
        ItemGenPragma1 = item_pragma_info(GenPragma, _, _),
        ( if
            Item2 = item_generated_pragma(ItemGenPragma2),
            ItemGenPragma2 = item_pragma_info(GenPragma, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_promise(ItemPromiseInfo1),
        ItemPromiseInfo1 = item_promise_info(PromiseType, Goal, _,
            UnivVars, _, _),
        ( if
            Item2 = item_promise(ItemPromiseInfo2),
            ItemPromiseInfo2 = item_promise_info(PromiseType, Goal, _,
                UnivVars, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_initialise(ItemInitialise1),
        ItemInitialise1 = item_initialise_info(A, B, C, _, _),
        ( if
            Item2 = item_initialise(ItemInitialise2),
            ItemInitialise2 = item_initialise_info(A, B, C, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_finalise(ItemFinalise1),
        ItemFinalise1 = item_finalise_info(A, B, C, _, _),
        ( if
            Item2 = item_finalise(ItemFinalise2),
            ItemFinalise2 = item_finalise_info(A, B, C, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mutable(ItemMutable1),
        ItemMutable1 = item_mutable_info(A, _, B, _, C, D, E, F, _, _),
        ( if
            Item2 = item_mutable(ItemMutable2),
            ItemMutable2 = item_mutable_info(A, _, B, _, C, D, E, F, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_typeclass(ItemTypeClass1),
        ItemTypeClass1 = item_typeclass_info(Constraints, FunDeps, Name,
            Vars, Interface1, _, _, _),
        ( if
            Item2 = item_typeclass(ItemTypeClass2),
            ItemTypeClass2 = item_typeclass_info(Constraints, FunDeps, Name,
                Vars, Interface2, _, _, _),
            class_interface_is_unchanged(Interface1, Interface2)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_instance(ItemInstance1),
        ItemInstance1 = item_instance_info(Constraints, Name,
            Types, OriginalTypes, Body, _, Module, _, _),
        ( if
            Item2 = item_instance(ItemInstance2),
            ItemInstance2 = item_instance_info(Constraints, Name,
                Types, OriginalTypes, Body, _, Module, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_type_repn(_),
        % Type representation items record information derived from
        % *other items*. They cannot change unless those other items change.
        Changed = unchanged
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
        ( func(TypeAndMode0) = Type :-
            (
                TypeAndMode0 = type_only(Type)
            ;
                % This should have been split out into a separate
                % mode declaration by gather_items.
                TypeAndMode0 = type_and_mode(_, _),
                unexpected($pred, "type_and_mode")
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
    ( if
        prog_type.type_list_to_var_list(SubstExistQTypes2, SubstExistQVars2)
    then
        ExistQVars1 = SubstExistQVars2
    else
        unexpected($pred, "non-var")
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
                    ( pred(VarName::in) is semidet :-
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
        WithInstTerm1 = mode_to_term(output_mercury,
            from_to_mode(free, Inst1)),
        WithInstTerm2 = mode_to_term(output_mercury,
            from_to_mode(free, Inst2)),
        AllModeTerms1 = [WithInstTerm1 | ModeTerms1],
        AllModeTerms2 = [WithInstTerm2 | ModeTerms2]
    ;
        MaybeWithInst1 = no,
        MaybeWithInst2 = no,
        AllModeTerms1 = ModeTerms1,
        AllModeTerms2 = ModeTerms2
    ),

    term.apply_renaming_in_terms(InstRenaming,
        AllModeTerms2, SubstAllModeTerms2),
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

:- pred class_methods_are_unchanged(list(class_decl)::in, list(class_decl)::in)
    is semidet.

class_methods_are_unchanged([], []).
class_methods_are_unchanged([Decl1 | Decls1], [Decl2 | Decls2]) :-
    (
        Decl1 = class_decl_pred_or_func(PredOrFuncInfo1),
        Decl2 = class_decl_pred_or_func(PredOrFuncInfo2),
        PredOrFuncInfo1 = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes1, WithType1, _, Detism, TVarSet1, _, ExistQVars1,
            Purity, Constraints1, _),
        PredOrFuncInfo2 = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes2, WithType2, _, Detism, TVarSet2, _, ExistQVars2,
            Purity, Constraints2, _),
        pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
            TypesAndModes1, WithType1, Constraints1,
            TVarSet2, ExistQVars2,
            TypesAndModes2, WithType2, Constraints2)
    ;
        Decl1 = class_decl_mode(ModeInfo1),
        Decl2 = class_decl_mode(ModeInfo2),
        ModeInfo1 = class_mode_info(Name, PredOrFunc, Modes1,
            WithInst1, Det, InstVarSet1, _),
        ModeInfo2 = class_mode_info(Name, PredOrFunc, Modes2,
            WithInst2, Det, InstVarSet2, _),
        pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
            InstVarSet2, Modes2, WithInst2)
    ),
    class_methods_are_unchanged(Decls1, Decls2).

%-----------------------------------------------------------------------------%

write_version_numbers(AllVersionNumbers, !IO) :-
    AllVersionNumbers = version_numbers(VersionNumbers,
        InstanceVersionNumbers),
    VersionNumbersList = list.filter_map(
        ( func(ItemType) = (ItemType - ItemVersions) is semidet :-
            ItemVersions = extract_ids(VersionNumbers, ItemType),
            not map.is_empty(ItemVersions)
        ),
        [type_abstract_item, type_body_item, mode_item, inst_item,
            predicate_item, function_item, typeclass_item]),
    io.write_string("{\n\t", !IO),
    io.write_list(VersionNumbersList, ",\n\t",
        write_item_type_and_versions, !IO),
    ( if map.is_empty(InstanceVersionNumbers) then
        true
    else
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
    mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
        unqualified(Name), !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

:- pred write_symname_arity_version_number(
    pair(item_name, version_number)::in, io::di, io::uo) is det.

write_symname_arity_version_number(ItemName - VersionNumber, !IO) :-
    ItemName = item_name(SymName, Arity),
    mercury_output_bracketed_sym_name_ngt(next_to_graphic_token, SymName, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

%-----------------------------------------------------------------------------%

version_numbers_version_number = 1.

%-----------------------------------------------------------------------------%

parse_version_numbers(VersionNumbersTerm, Result) :-
    ( if
        VersionNumbersTerm = term.functor(term.atom("{}"),
            VersionNumbersTermList0, _)
    then
        VersionNumbersTermList = VersionNumbersTermList0
    else
        VersionNumbersTermList = [VersionNumbersTerm]
    ),
    map_parser(parse_item_type_version_numbers, VersionNumbersTermList,
        Result0),
    (
        Result0 = ok1(List),
        VersionNumbers0 = version_numbers(init_item_id_set(map.init),
            map.init),
        VersionNumbers = list.foldl(
            ( func(VNResult, version_numbers(VNs0, Instances0)) =
                    version_numbers(VNs, Instances) :-
                (
                    VNResult = items(ItemType, ItemVNs),
                    update_ids(ItemType, ItemVNs, VNs0, VNs),
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
    ( if
        Term = term.functor(term.atom(ItemTypeStr), ItemsVNsTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    then
        ParseName =
            ( pred(NameTerm::in, Name::out) is semidet :-
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
    else if
        Term = term.functor(term.atom("instance"), InstanceVNsTerms, _)
    then
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
    else
        % XXX This is an uninformative error message.
        Pieces = [words("Invalid item type version numbers."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

:- pred parse_key_version_number(
    pred(term, string)::(pred(in, out) is semidet), term::in,
    maybe1(pair(pair(string, arity), version_number))::out) is det.

parse_key_version_number(ParseName, Term, Result) :-
    ( if
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, Name),
        decimal_term_to_int(ArityTerm, Arity),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    then
        Result = ok1((Name - Arity) - VersionNumber)
    else
        Pieces = [words("Error in item version number."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

:- pred parse_item_version_number(
    pred(term, sym_name)::(pred(in, out) is semidet), term::in,
    maybe1(pair(item_name, version_number))::out) is det.

parse_item_version_number(ParseName, Term, Result) :-
    ( if
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, SymName),
        decimal_term_to_int(ArityTerm, Arity),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    then
        Result = ok1(item_name(SymName, Arity) - VersionNumber)
    else
        Pieces = [words("Error in item version number."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
:- end_module recompilation.version.
%-----------------------------------------------------------------------------%
