%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: equiv_type.m.
% Main author: fjh.
%
% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % expand_eqv_types(ModuleName, !Items, !EventSpecMap,
    %   CircularTypes, EqvMap, !MaybeRecompInfo, Specs):
    %
    % First it builds up a map from type_ctor to the equivalent type.
    % Then it traverses through the list of items and event specs,
    % expanding all types. This has the effect of eliminating all the
    % equivalence types from the source code.
    %
    % `with_type` and `with_inst` annotations on predicate and
    % function type declarations are also expaneded.
    %
    % Error messages are generated for any circular equivalence types
    % and invalid `with_type` and `with_inst` annotations.
    %
    % For items not defined in the current module, the items expanded
    % while processing each item are recorded in the recompilation_info,
    % for use by smart recompilation.
    %
:- pred expand_eqv_types(module_name::in, list(item)::in, list(item)::out,
    event_spec_map::in, event_spec_map::out,
    eqv_map::out, used_modules::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    list(error_spec)::out) is det.

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_type(eqv_map::in, mer_type::in, mer_type::out,
    bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_type_list(eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_prog_constraints(eqv_map::in,
    prog_constraints::in, prog_constraints::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_prog_constraint(eqv_map::in,
    prog_constraint::in, prog_constraint::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_ctors(eqv_map::in,
    list(constructor)::in, list(constructor)::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- type eqv_type_body
    --->    eqv_type_body(
                tvarset,
                list(type_param),
                mer_type
            ).

:- type eqv_map == map(type_ctor, eqv_type_body).

% XXX Should we make equiv_type_info abstract?
:- type equiv_type_info == maybe(expanded_item_set).
:- type expanded_item_set.

    % For smart recompilation we need to record which items were expanded
    % in each declaration. Any items which depend on that declaration also
    % depend on the expanded items.
    %
:- pred maybe_start_recording_expanded_items(module_name::in, sym_name::in,
    maybe(recompilation_info)::in, equiv_type_info::out) is det.

    % Record all the expanded items in the recompilation_info.
    %
:- pred finish_recording_expanded_items(item_id::in, equiv_type_info::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type eqv_type_location
    --->    eqv_type_out_of_module
    ;       eqv_type_in_interface
    ;       eqv_type_in_implementation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % First we build up a mapping which records the equivalence type
    % definitions. Then we go through the item list and replace them.
    %
expand_eqv_types(ModuleName, Items0, Items, EventSpecMap0, EventSpecMap,
        EqvMap, !:UsedModules, !RecompInfo, !:Specs) :-
    map.init(EqvMap0),
    map.init(EqvInstMap0),
    build_eqv_map(Items0, EqvMap0, EqvMap, EqvInstMap0, EqvInstMap),
    !:UsedModules = used_modules_init,
    !:Specs = [],
    replace_in_item_list(ModuleName, eqv_type_out_of_module, Items0,
        EqvMap, EqvInstMap, [], RevItems, !RecompInfo, !UsedModules, !Specs),
    list.reverse(RevItems, Items),
    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    replace_in_event_spec_list(EventSpecList0, EventSpecList,
        EqvMap, EqvInstMap, !RecompInfo, !UsedModules, !Specs),
    map.from_sorted_assoc_list(EventSpecList, EventSpecMap).

    % We need to expand equivalence insts in
    % `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body
    --->    eqv_inst_body(
                inst_varset,
                list(inst_var),
                mer_inst
            ).

:- type eqv_inst_map == map(inst_id, eqv_inst_body).

:- type pred_or_func_decl_type
    --->    type_decl
    ;       mode_decl.

:- pred build_eqv_map(list(item)::in,
    eqv_map::in, eqv_map::out, eqv_inst_map::in, eqv_inst_map::out) is det.

build_eqv_map([], !EqvMap, !EqvInstMap).
build_eqv_map([Item | Items], !EqvMap, !EqvInstMap) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _Context, _SeqNum),
        ModuleDefn = md_abstract_imported
    ->
        skip_abstract_imported_items(Items, AfterSkipItems),
        build_eqv_map(AfterSkipItems, !EqvMap, !EqvInstMap)
    ;
        (
            Item = item_type_defn(ItemTypeDefn),
            ItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_eqv_type(Body), _, _, _SeqNum)
        ->
            list.length(Args, Arity),
            TypeCtor = type_ctor(Name, Arity),
            map.set(TypeCtor, eqv_type_body(VarSet, Args, Body), !EqvMap)
        ;
            Item = item_inst_defn(ItemInstDefn),
            ItemInstDefn = item_inst_defn_info(VarSet, Name, Args,
                eqv_inst(Body), _, _, _SeqNum)
        ->
            list.length(Args, Arity),
            InstId = inst_id(Name, Arity),
            map.set(InstId, eqv_inst_body(VarSet, Args, Body), !EqvInstMap)
        ;
            true
        ),
        build_eqv_map(Items, !EqvMap, !EqvInstMap)
    ).

:- pred skip_abstract_imported_items(list(item)::in, list(item)::out) is det.

skip_abstract_imported_items([], []).
skip_abstract_imported_items([Item0 | Items0], Items) :-
    (
        ( Item0 = item_module_start(_)
        ; Item0 = item_module_end(_)
        ),
        Items = Items0
    ;
        Item0 = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            is_section_defn(ModuleDefn) = yes,
            ModuleDefn \= md_abstract_imported
        ->
            Items = Items0
        ;
            skip_abstract_imported_items(Items0, Items)
        )
    ;
        ( Item0 = item_clause(_)
        ; Item0 = item_type_defn(_)
        ; Item0 = item_inst_defn(_)
        ; Item0 = item_mode_defn(_)
        ; Item0 = item_pred_decl(_)
        ; Item0 = item_mode_decl(_)
        ; Item0 = item_pragma(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_typeclass(_)
        ; Item0 = item_instance(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_mutable(_)
        ; Item0 = item_nothing(_)
        ),
        skip_abstract_imported_items(Items0, Items)
    ).

:- func is_section_defn(module_defn) = bool.

is_section_defn(md_interface) = yes.
is_section_defn(md_implementation) = yes.
is_section_defn(md_implementation_but_exported_to_submodules) = yes.
is_section_defn(md_imported(_)) = yes.
is_section_defn(md_used(_)) = yes.
is_section_defn(md_abstract_imported) = yes.
is_section_defn(md_opt_imported) = yes.
is_section_defn(md_transitively_imported) = yes.
is_section_defn(md_external(_, _)) = no.
is_section_defn(md_export(_)) = no.
is_section_defn(md_import(_)) = no.
is_section_defn(md_use(_)) = no.
is_section_defn(md_include_module(_)) = no.
is_section_defn(md_version_numbers(_, _)) = no.

    % The following predicate replace_in_item_list performs substitution
    % of equivalence types on a list of items. Similarly the replace_in_<foo>
    % predicates that follow perform substitution of equivalence types
    % on <foo>s.
    %
:- pred replace_in_item_list(module_name::in, eqv_type_location::in,
    list(item)::in, eqv_map::in, eqv_inst_map::in,
    list(item)::in, list(item)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_item_list(_, _, [], _, _, !Items, !RecompInfo, !UsedModules,
        !Specs).
replace_in_item_list(ModuleName, Location0, [Item0 | Items0],
        EqvMap, EqvInstMap, !ReplItems, !RecompInfo, !UsedModules, !Specs) :-
    (
        Item0 = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _SeqNum),
        (
            ModuleDefn = md_interface,
            Location = eqv_type_in_interface
        ;
            ( ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ),
            Location = eqv_type_in_implementation
        ;

            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            % XXX I'm not sure what this is so it may not signify
            % that we've finished processing the module.
            ; ModuleDefn = md_export(_)
            ),
            Location = eqv_type_out_of_module
        ;
            ( ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            Location = Location0
        )
    ;
        ( Item0 = item_module_start(_)
        ; Item0 = item_module_end(_)
        ),
        unexpected($module, $pred, "module start or end")
    ;
        ( Item0 = item_clause(_)
        ; Item0 = item_type_defn(_)
        ; Item0 = item_inst_defn(_)
        ; Item0 = item_mode_defn(_)
        ; Item0 = item_pred_decl(_)
        ; Item0 = item_mode_decl(_)
        ; Item0 = item_pragma(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_typeclass(_)
        ; Item0 = item_instance(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_mutable(_)
        ; Item0 = item_nothing(_)
        ),
        Location = Location0
    ),
    replace_in_item(ModuleName, Location, EqvMap, EqvInstMap,
        Item0, Item, !RecompInfo, !UsedModules, ItemSpecs),
    % Discard the item if there were any errors.
    (
        ItemSpecs = [],
        !:ReplItems = [Item | !.ReplItems]
    ;
        ItemSpecs = [_ | _]
    ),
    !:Specs = ItemSpecs ++ !.Specs,
    replace_in_item_list(ModuleName, Location, Items0,
        EqvMap, EqvInstMap, !ReplItems, !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_item(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,  item::in,item::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_item(ModuleName, Location, EqvMap, EqvInstMap,
        Item0, Item, !RecompInfo, !UsedModules, Specs) :-
    (
        Item0 = item_type_defn(ItemTypeDefn0),
        replace_in_type_defn_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemTypeDefn0, ItemTypeDefn, !RecompInfo, !UsedModules, Specs),
        Item = item_type_defn(ItemTypeDefn)
    ;
        Item0 = item_pred_decl(ItemPredDecl0),
        replace_in_pred_decl_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemPredDecl0, ItemPredDecl, !RecompInfo, !UsedModules, Specs),
        Item = item_pred_decl(ItemPredDecl)
    ;
        Item0 = item_mode_decl(ItemModeDecl0),
        replace_in_mode_decl_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemModeDecl0, ItemModeDecl, !RecompInfo, !UsedModules, Specs),
        Item = item_mode_decl(ItemModeDecl)
    ;
        Item0 = item_pragma(ItemPragma0),
        replace_in_pragma_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemPragma0, ItemPragma, !RecompInfo, !UsedModules, Specs),
        Item = item_pragma(ItemPragma)
    ;
        Item0 = item_typeclass(ItemTypeClass0),
        replace_in_typeclass_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemTypeClass0, ItemTypeClass, !RecompInfo, !UsedModules, Specs),
        Item = item_typeclass(ItemTypeClass)
    ;
        Item0 = item_instance(ItemInstance0),
        replace_in_instance_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemInstance0, ItemInstance, !RecompInfo, !UsedModules, Specs),
        Item = item_instance(ItemInstance)
    ;
        Item0 = item_mutable(ItemMutable0),
        replace_in_mutable_info(ModuleName, Location, EqvMap, EqvInstMap,
            ItemMutable0, ItemMutable, !RecompInfo, !UsedModules, Specs),
        Item = item_mutable(ItemMutable)
    ;
        Item0 = item_mode_defn(_),
        % XXX zs: This seems to be a bug. Mode definitions contain insts,
        % we should see if they need expansion.
        Item = Item0,
        Specs = []
    ;
        ( Item0 = item_module_start(_)
        ; Item0 = item_module_end(_)
        ; Item0 = item_module_defn(_)
        ; Item0 = item_clause(_)
        ; Item0 = item_inst_defn(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_nothing(_)
        ),
        Item = Item0,
        Specs = []
    ).

:- pred replace_in_type_defn_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_type_defn_info::in, item_type_defn_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_info(ModuleName, Location, EqvMap, EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_type_defn_info(VarSet0, SymName, TArgs, TypeDefn0, Cond,
        Context, SeqNum),
    list.length(TArgs, Arity),
    maybe_start_recording_expanded_items(ModuleName, SymName, !.RecompInfo,
        UsedTypeCtors0),
    replace_in_type_defn(Location, EqvMap, EqvInstMap,
        type_ctor(SymName, Arity), TypeDefn0, TypeDefn, ContainsCirc,
        VarSet0, VarSet, UsedTypeCtors0, UsedTypeCtors, !UsedModules),
    (
        ContainsCirc = yes,
        ( TypeDefn0 = parse_tree_eqv_type(_) ->
            Pieces = [words("Error: circular equivalence type"),
                sym_name_and_arity(SymName / length(TArgs)), suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_expand_types, [Msg]),
            Specs = [Spec]
        ;
            unexpected($module, $pred, "invalid item")
        )
    ;
        ContainsCirc = no,
        Specs = []
    ),
    ItemId = item_id(type_body_item, item_name(SymName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_type_defn_info(VarSet, SymName, TArgs, TypeDefn, Cond,
        Context, SeqNum).

:- pred replace_in_pred_decl_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_pred_decl_info::in, item_pred_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_decl_info(ModuleName, Location, EqvMap, EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_pred_decl_info(Origin, TypeVarSet0, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes0, MaybeWithType0,
        MaybeWithInst0, Det0, Cond, Purity, ClassContext0, Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_type(Location, PredName, PredOrFunc, Context, EqvMap,
        EqvInstMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        Det0, Det, ExpandedItems0, ExpandedItems, !UsedModules, Specs),
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    list.length(TypesAndModes, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_pred_decl_info(Origin, TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeWithType,
        MaybeWithInst, Det, Cond, Purity, ClassContext, Context, SeqNum).

:- pred replace_in_mode_decl_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_mode_decl_info::in, item_mode_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mode_decl_info(ModuleName, Location, _EqvMap, EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_mode_decl_info(InstVarSet, MaybePredOrFunc0, PredName,
        Modes0, WithInst0, Det0, Cond, Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_mode(Location, PredName, length(Modes0), Context,
        mode_decl, EqvInstMap, ExtraModes, MaybePredOrFunc0, MaybePredOrFunc,
        WithInst0, WithInst, Det0, Det,
        ExpandedItems0, ExpandedItems, !UsedModules, Specs),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        list.length(Modes, Arity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
        finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo)
    ;
        MaybePredOrFunc = no
    ),
    Info = item_mode_decl_info(InstVarSet, MaybePredOrFunc, PredName,
        Modes, WithInst, Det, Cond, Context, SeqNum).

:- pred replace_in_typeclass_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_typeclass_info::in, item_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_typeclass_info(ModuleName, Location, EqvMap, EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_typeclass_info(Constraints0, FunDeps, ClassName, Vars,
        ClassInterface0, VarSet0, Context, SeqNum),
    list.length(Vars, Arity),
    maybe_start_recording_expanded_items(ModuleName, ClassName, !.RecompInfo,
        ExpandedItems0),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, VarSet0, VarSet,
        ExpandedItems0, ExpandedItems1, !UsedModules),
    (
        ClassInterface0 = class_interface_abstract,
        ClassInterface = class_interface_abstract,
        ExpandedItems = ExpandedItems1,
        Specs = []
    ;
        ClassInterface0 = class_interface_concrete(Methods0),
        replace_in_class_interface(Location, Methods0, EqvMap, EqvInstMap,
            Methods, ExpandedItems1, ExpandedItems, !UsedModules, [], Specs),
        ClassInterface = class_interface_concrete(Methods)
    ),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_typeclass_info(Constraints, FunDeps, ClassName, Vars,
        ClassInterface, VarSet, Context, SeqNum).

:- pred replace_in_instance_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_instance_info::in, item_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_instance_info(ModuleName, Location, EqvMap, _EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    Info0 = item_instance_info(Constraints0, ClassName, Types0, OriginalTypes,
        InstanceBody, VarSet0, ContainingModuleName, Context, SeqNum),
    (
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    ->
        UsedTypeCtors0 = no
    ;
        UsedTypeCtors0 = yes(ModuleName - set.init)
    ),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, VarSet0, VarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(Location, EqvMap, Types0, Types, _, _,
        VarSet1, VarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    list.length(Types0, Arity),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_instance_info(Constraints, ClassName, Types, OriginalTypes,
        InstanceBody, VarSet, ContainingModuleName, Context, SeqNum).

:- pred replace_in_pragma_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_pragma_info::in, item_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pragma_info(ModuleName, Location, EqvMap, _EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    Info0 = item_pragma_info(Origin, Pragma0, Context, SeqNum),
    (
        Pragma0 = pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(PredName, NewName, Arity,
            PorF, Modes, Subst0, VarSet0, ItemIds0),
        (
            ( !.RecompInfo = no
            ; PredName = qualified(ModuleName, _)
            )
        ->
            ExpandedItems0 = no
        ;
            ExpandedItems0 = yes(ModuleName - ItemIds0)
        ),
        replace_in_subst(Location, EqvMap, Subst0, Subst, VarSet0, VarSet,
            ExpandedItems0, ExpandedItems, !UsedModules),
        (
            ExpandedItems = no,
            ItemIds = ItemIds0
        ;
            ExpandedItems = yes(_ - ItemIds)
        ),
        TypeSpecInfo = pragma_info_type_spec(PredName, NewName, Arity,
            PorF, Modes, Subst, VarSet, ItemIds),
        Pragma = pragma_type_spec(TypeSpecInfo)
    ;
        Pragma0 = pragma_foreign_proc(FPInfo0),
        FPInfo0 = pragma_info_foreign_proc(Attrs0, PName, PredOrFunc,
            ProcVars, ProcVarset, ProcInstVarset, ProcImpl),
        some [!EquivTypeInfo] (
            maybe_start_recording_expanded_items(ModuleName, PName,
                !.RecompInfo, !:EquivTypeInfo),
            UserSharing0 = get_user_annotated_sharing(Attrs0),
            (
                UserSharing0 = user_sharing(Sharing0, MaybeTypes0),
                MaybeTypes0 = yes(user_type_info(Types0, TVarset0))
            ->
                replace_in_type_list_location(Location,
                    EqvMap, Types0, Types, _AnythingChanged,
                    TVarset0, TVarset, !EquivTypeInfo, !UsedModules),
                replace_in_structure_sharing_domain(Location,
                    EqvMap, Sharing0, Sharing,
                    TVarset0, !EquivTypeInfo, !UsedModules),
                MaybeTypes = yes(user_type_info(Types, TVarset)),
                UserSharing = user_sharing(Sharing, MaybeTypes),
                set_user_annotated_sharing(UserSharing, Attrs0, Attrs)
            ;
                Attrs = Attrs0
            ),
            ItemId = item_id(foreign_proc_item, item_name(PName,
                list.length(ProcVars))),
            finish_recording_expanded_items(ItemId, !.EquivTypeInfo,
                !RecompInfo)
        ),
        FPInfo = pragma_info_foreign_proc(Attrs, PName, PredOrFunc,
            ProcVars, ProcVarset, ProcInstVarset, ProcImpl),
        Pragma = pragma_foreign_proc(FPInfo)
    ;
        ( Pragma0 = pragma_check_termination(_)
        ; Pragma0 = pragma_does_not_terminate(_)
        ; Pragma0 = pragma_exceptions(_)
        ; Pragma0 = pragma_fact_table(_)
        ; Pragma0 = pragma_foreign_code(_)
        ; Pragma0 = pragma_foreign_decl(_)
        ; Pragma0 = pragma_foreign_enum(_)
        ; Pragma0 = pragma_foreign_proc_export(_)
        ; Pragma0 = pragma_foreign_export_enum(_)
        ; Pragma0 = pragma_foreign_import_module(_)
        ; Pragma0 = pragma_inline(_)
        ; Pragma0 = pragma_mm_tabling_info(_)
        ; Pragma0 = pragma_mode_check_clauses(_)
        ; Pragma0 = pragma_no_inline(_)
        ; Pragma0 = pragma_obsolete(_)
        ; Pragma0 = pragma_no_detism_warning(_)
        ; Pragma0 = pragma_promise_eqv_clauses(_)
        ; Pragma0 = pragma_promise_pure(_)
        ; Pragma0 = pragma_promise_semipure(_)
        ; Pragma0 = pragma_require_feature_set(_)
        ; Pragma0 = pragma_reserve_tag(_)
        ; Pragma0 = pragma_source_file(_)
        ; Pragma0 = pragma_structure_reuse(_)
        ; Pragma0 = pragma_structure_sharing(_)
        ; Pragma0 = pragma_oisu(_)
        ; Pragma0 = pragma_tabled(_)
        ; Pragma0 = pragma_terminates(_)
        ; Pragma0 = pragma_termination2_info(_)
        ; Pragma0 = pragma_termination_info(_)
        ; Pragma0 = pragma_trailing_info(_)
        ; Pragma0 = prog_item.pragma_unused_args(_)
        ),
        Pragma = Pragma0
    ),
    Info = item_pragma_info(Origin, Pragma, Context, SeqNum).

:- pred replace_in_mutable_info(module_name::in, eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_mutable_info::in, item_mutable_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mutable_info(ModuleName, Location, EqvMap, EqvInstMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    MutName = Info0 ^ mut_name,
    QualName = qualified(ModuleName, MutName),
    maybe_start_recording_expanded_items(ModuleName, QualName, !.RecompInfo,
        ExpandedItems0),
    replace_in_mutable_defn(Location, EqvMap, EqvInstMap, Info0, Info,
        ExpandedItems0, ExpandedItems, !UsedModules),
    ItemId = item_id(mutable_item, item_name(QualName, 0)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo).

:- pred replace_in_mutable_defn(eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    item_mutable_info::in, item_mutable_info::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_mutable_defn(Location, EqvMap, EqvInstMap, Info0, Info,
        !ExpandedItems, !UsedModules) :-
    Info0 = item_mutable_info(MutName, Type0, InitValue, Inst0, Attrs, Varset,
        Context, SeqNum),
    TVarSet0 = varset.init,
    replace_in_type_location(Location, EqvMap, Type0, Type, _TypeChanged,
        TVarSet0, _TVarSet, !ExpandedItems, !UsedModules),
    replace_in_inst(Location, Inst0, EqvInstMap, Inst, !ExpandedItems,
        !UsedModules),
    Info = item_mutable_info(MutName, Type, InitValue, Inst, Attrs, Varset,
        Context, SeqNum).

:- pred replace_in_event_spec_list(
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    eqv_map::in, eqv_inst_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_spec_list([], [], _, _, !RecompInfo, !UsedModules, !Specs).
replace_in_event_spec_list(
        [Name - EventSpec0 | NameSpecs0], [Name - EventSpec | NameSpecs],
        EqvMap, EqvInstMap, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_spec(EventSpec0, EventSpec,
        EqvMap, EqvInstMap, !RecompInfo, !UsedModules, !Specs),
    replace_in_event_spec_list(NameSpecs0, NameSpecs,
        EqvMap, EqvInstMap, !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_spec(event_spec::in, event_spec::out,
    eqv_map::in, eqv_inst_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_spec(EventSpec0, EventSpec, EqvMap, EqvInstMap,
        !RecompInfo, !UsedModules, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SyntAttrNumOrder),
    replace_in_event_attrs(Attrs0, Attrs, EqvMap, EqvInstMap,
        !RecompInfo, !UsedModules, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SyntAttrNumOrder).

:- pred replace_in_event_attrs(
    list(event_attribute)::in, list(event_attribute)::out,
    eqv_map::in, eqv_inst_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attrs([], [], _EqvMap, _EqvInstMap,
        !RecompInfo, !UsedModules, !Specs).
replace_in_event_attrs([Attr0 | Attrs0], [Attr | Attrs], EqvMap, EqvInstMap,
        !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_attr(Attr0, Attr, EqvMap, EqvInstMap,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_event_attrs(Attrs0, Attrs, EqvMap, EqvInstMap,
        !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_attr(event_attribute::in, event_attribute::out,
    eqv_map::in, eqv_inst_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attr(Attr0, Attr, EqvMap, _EqvInstMap,
        !RecompInfo, !UsedModules, !Specs) :-
    % We construct the attributes' modes ourselves in event_spec.m; they should
    % not contain type names.
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode,
        MaybeSynthCall),
    TVarSet0 = varset.init,
    replace_in_type_location(eqv_type_out_of_module, EqvMap,
        AttrType0, AttrType, _Changed, TVarSet0, _TVarSet, no, _EquivTypeInfo,
        !UsedModules),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

:- pred replace_in_type_defn(eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in, type_ctor::in,
    type_defn::in, type_defn::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_defn(Location, EqvMap, EqvInstMap, TypeCtor,
        TypeDefn0, TypeDefn, ContainsCirc, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    (
        TypeDefn0 = parse_tree_eqv_type(TypeBody0),
        replace_in_type_location_2(Location, EqvMap, [TypeCtor],
            TypeBody0, TypeBody, _, ContainsCirc, !VarSet, !EquivTypeInfo,
            !UsedModules),
        TypeDefn = parse_tree_eqv_type(TypeBody)
    ;
        TypeDefn0 = parse_tree_du_type(TypeBody0, EqPred, DirectArgFunctors),
        replace_in_ctors_location(Location, EqvMap, TypeBody0, TypeBody,
            !VarSet, !EquivTypeInfo, !UsedModules),
        ContainsCirc = no,
        TypeDefn = parse_tree_du_type(TypeBody, EqPred, DirectArgFunctors)
    ;
        TypeDefn0 = parse_tree_solver_type(SolverDetails0, MaybeUserEqComp),
        SolverDetails0 = solver_type_details(RepresentationType0, InitPred,
            GroundInst, AnyInst, MutableInfos0),
        replace_in_type_location_2(Location, EqvMap, [TypeCtor],
            RepresentationType0, RepresentationType,
            _, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules),
        replace_in_constraint_store(Location, EqvMap, EqvInstMap,
            MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules),
        SolverDetails = solver_type_details(RepresentationType, InitPred,
            GroundInst, AnyInst, MutableInfos),
        TypeDefn = parse_tree_solver_type(SolverDetails, MaybeUserEqComp)
    ;
        ( TypeDefn0 = parse_tree_abstract_type(_)
        ; TypeDefn0 = parse_tree_foreign_type(_, _, _)
        ),
        TypeDefn = TypeDefn0,
        ContainsCirc = no
    ).

:- pred replace_in_constraint_store(eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_constraint_store(_, _, _, [], [], !EquivTypeInfo, !UsedModules).
replace_in_constraint_store(Location, EqvMap, EqvInstMap,
        [MutableInfo0 | MutableInfos0], [MutableInfo | MutableInfos],
        !EquivTypeInfo, !UsedModules) :-
    replace_in_mutable_defn(Location, EqvMap, EqvInstMap,
        MutableInfo0, MutableInfo, !EquivTypeInfo, !UsedModules),
    replace_in_constraint_store(Location, EqvMap, EqvInstMap,
        MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_prog_constraints(EqvMap, Cs0, Cs, !VarSet, !EquivTypeInfo) :-
    replace_in_prog_constraints_location(eqv_type_out_of_module, EqvMap,
        Cs0, Cs, !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraints_location(eqv_type_location::in,
    eqv_map::in, prog_constraints::in, prog_constraints::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraints_location(Location, EqvMap, Cs0, Cs, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    Cs0 = constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraint_list(Location, EqvMap, UnivCs0, UnivCs,
        !VarSet, !EquivTypeInfo, !UsedModules),
    replace_in_prog_constraint_list(Location, EqvMap, ExistCs0, ExistCs,
        !VarSet, !EquivTypeInfo, !UsedModules),
    Cs = constraints(UnivCs, ExistCs).

:- pred replace_in_prog_constraint_list(eqv_type_location::in, eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_list(Location, EqvMap, !Cs, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    list.map_foldl3(replace_in_prog_constraint_location(Location, EqvMap),
        !Cs, !VarSet, !EquivTypeInfo, !UsedModules).

replace_in_prog_constraint(EqvMap, Constraint0, Constraint, !VarSet,
        !EquivTypeInfo) :-
    replace_in_prog_constraint_location(eqv_type_out_of_module, EqvMap,
        Constraint0, Constraint, !VarSet, !EquivTypeInfo,
        used_modules_init, _).

:- pred replace_in_prog_constraint_location(eqv_type_location::in, eqv_map::in,
    prog_constraint::in, prog_constraint::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(Location, EqvMap, Constraint0, Constraint,
        !VarSet, !EquivTypeInfo, !UsedModules) :-
    Constraint0 = constraint(ClassName, Ts0),
    replace_in_type_list_location_circ(Location, EqvMap, Ts0, Ts, _, _,
        !VarSet, !EquivTypeInfo, !UsedModules),
    Constraint = constraint(ClassName, Ts).

%-----------------------------------------------------------------------------%

:- pred replace_in_class_interface(eqv_type_location::in, class_methods::in,
    eqv_map::in, eqv_inst_map::in, class_methods::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_interface(Location, ClassInterface0, EqvMap, EqvInstMap,
        ClassInterface, !EquivTypeInfo, !UsedModules, !Specs) :-
    list.map_foldl3(replace_in_class_method(Location, EqvMap, EqvInstMap),
        ClassInterface0, ClassInterface, !EquivTypeInfo, !UsedModules, !Specs).

:- pred replace_in_class_method(eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in, class_method::in, class_method::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_method(Location, EqvMap, EqvInstMap, Method0, Method,
        !EquivTypeInfo, !UsedModules, !Specs) :-
    (
        Method0 = method_pred_or_func(TypeVarSet0, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes0, WithType0, WithInst0,
            Det0, Cond, Purity, ClassContext0, Context),
        replace_in_pred_type(Location, PredName, PredOrFunc, Context, EqvMap,
            EqvInstMap, ClassContext0, ClassContext,
            TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
            WithType0, WithType, WithInst0, WithInst, Det0, Det,
            !EquivTypeInfo, !UsedModules, NewSpecs),
        !:Specs = NewSpecs ++ !.Specs,
        Method = method_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes, WithType, WithInst,
            Det, Cond, Purity, ClassContext, Context)
    ;
        Method0 = method_pred_or_func_mode(InstVarSet, MaybePredOrFunc0,
            PredName, Modes0, WithInst0, Det0, Cond, Context),
        replace_in_pred_mode(Location, PredName, length(Modes0), Context,
            mode_decl, EqvInstMap, ExtraModes,
            MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst,
            Det0, Det, !EquivTypeInfo, !UsedModules, NewSpecs),
        (
            ExtraModes = [],
            Modes = Modes0
        ;
            ExtraModes = [_ | _],
            Modes = Modes0 ++ ExtraModes
        ),
        !:Specs = NewSpecs ++ !.Specs,
        Method = method_pred_or_func_mode(InstVarSet, MaybePredOrFunc,
            PredName, Modes, WithInst, Det, Cond, Context)
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_subst(eqv_type_location::in, eqv_map::in,
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(_Location, _EqvMap, [], [], !VarSet, !EquivTypeInfo,
        !UsedModules).
replace_in_subst(Location, EqvMap, [Var - Type0 | Subst0],
        [Var - Type | Subst], !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet,
    !EquivTypeInfo,
        !UsedModules),
    replace_in_subst(Location, EqvMap, Subst0, Subst, !VarSet, !EquivTypeInfo,
        !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_ctors(EqvMap, !Ctors, !VarSet, !EquivTypeInfo) :-
    replace_in_ctors_location(eqv_type_out_of_module, EqvMap, !Ctors, !VarSet,
        !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_ctors_location(eqv_type_location::in, eqv_map::in,
    list(constructor)::in, list(constructor)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctors_location(Location, EqvMap, !Ctors, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    list.map_foldl3(replace_in_ctor(Location, EqvMap), !Ctors, !VarSet,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_ctor(eqv_type_location::in, eqv_map::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(Location, EqvMap,
        ctor(ExistQVars, Constraints0, TName, Targs0, Ctxt),
        ctor(ExistQVars, Constraints, TName, Targs, Ctxt),
        !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_ctor_arg_list(Location,
        EqvMap, Targs0, Targs, _, !VarSet, !EquivTypeInfo, !UsedModules),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, !VarSet, !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_type_list(EqvMap, !Ts, Changed, !VarSet, !EquivTypeInfo) :-
    replace_in_type_list_location(eqv_type_out_of_module, EqvMap, !Ts, Changed,
        !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_list_location(eqv_type_location::in, eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(Location, EqvMap, !Ts, Changed, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(Location, EqvMap, [], !Ts, Changed,
        no, _, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ(eqv_type_location::in, eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ(Location, EqvMap, !Ts,
        Changed, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(Location, EqvMap, [], !Ts,
        Changed, no, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ_2(eqv_type_location::in,
    eqv_map::in, list(type_ctor)::in, list(mer_type)::in, list(mer_type)::out,
    bool::out, bool::in, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ_2(_Location, _EqvMap, _Seen, [], [], no,
        !ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules).
replace_in_type_list_location_circ_2(Location, EqvMap, Seen,
        List0 @ [T0 | Ts0], List, Changed, !Circ, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, Seen, T0, T, Changed0,
        ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules),
    !:Circ = ContainsCirc `or` !.Circ,
    replace_in_type_list_location_circ_2(Location, EqvMap, Seen, Ts0, Ts,
        Changed1, !Circ, !VarSet, !EquivTypeInfo, !UsedModules),
    (
        ( Changed0 = yes
        ; Changed1 = yes
        )
    ->
        Changed = yes,
        List = [T | Ts]
    ;
        Changed = no,
        List = List0
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_ctor_arg_list(eqv_type_location::in, eqv_map::in,
    list(constructor_arg)::in, list(constructor_arg)::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(Location,
        EqvMap, !As, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_ctor_arg_list_2(Location, EqvMap, [], !As, no, ContainsCirc,
        !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_ctor_arg_list_2(eqv_type_location::in,
    eqv_map::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    bool::in, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_2(_Location, _EqvMap, _Seen, [], [],
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules).
replace_in_ctor_arg_list_2(Location, EqvMap, Seen,
        [Arg0 | Args0], [Arg | Args],
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules) :-
    Arg0 = ctor_arg(Name, Type0, Width, Context),
    replace_in_type_location_2(Location, EqvMap, Seen, Type0, Type, _,
        ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules),
    (
        Width = full_word
    ;
        Width = double_word
    ;
        ( Width = partial_word_first(_)
        ; Width = partial_word_shifted(_, _)
        ),
        ( Type = Type0 ->
            true
        ;
            unexpected($module, $pred, "changed type of packed argument")
        )
    ),
    Arg = ctor_arg(Name, Type, Width, Context),
    !:Circ = !.Circ `or` ContainsCirc,
    replace_in_ctor_arg_list_2(Location, EqvMap, Seen, Args0, Args,
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_type(EqvMap, Type0, Type, Changed, !VarSet, !EquivTypeInfo) :-
    replace_in_type_location(eqv_type_out_of_module, EqvMap, Type0, Type,
        Changed, !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_location(eqv_type_location::in, eqv_map::in,
    mer_type::in, mer_type::out, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_location(Location, EqvMap, Type0, Type, Changed, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, [], Type0, Type, Changed, _,
        !VarSet, !EquivTypeInfo, !UsedModules).

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_location_2(eqv_type_location::in, eqv_map::in,
    list(type_ctor)::in, mer_type::in, mer_type::out, bool::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_location_2(Location, EqvMap, TypeCtorsAlreadyExpanded,
        Type0, Type, Changed, Circ, !VarSet, !EquivTypeInfo, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = no,
        Circ = no
    ;
        Type0 = defined_type(SymName, TArgs0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, TArgs0, TArgs, ArgsChanged, no, Circ0,
            !VarSet, !EquivTypeInfo, !UsedModules),
        Arity = list.length(TArgs),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(Location, EqvMap, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, TArgs, Kind, Type, ArgsChanged, Changed,
            Circ0, Circ, !VarSet, !EquivTypeInfo, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = no,
        Circ = no
    ;
        Type0 = higher_order_type(Args0, MaybeRet0, Purity, EvalMethod),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, ArgsChanged, no, ArgsCirc,
            !VarSet, !EquivTypeInfo, !UsedModules),
        (
            MaybeRet0 = yes(Ret0),
            replace_in_type_location_2(Location, EqvMap,
                TypeCtorsAlreadyExpanded, Ret0, Ret, RetChanged, RetCirc,
                !VarSet, !EquivTypeInfo, !UsedModules),
            MaybeRet = yes(Ret),
            Changed = bool.or(ArgsChanged, RetChanged),
            Circ = bool.or(ArgsCirc, RetCirc)
        ;
            MaybeRet0 = no,
            MaybeRet = no,
            Changed = ArgsChanged,
            Circ = ArgsCirc
        ),
        (
            Changed = yes,
            Type = higher_order_type(Args, MaybeRet, Purity, EvalMethod)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = tuple_type(Args0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, no, Circ,
            !VarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = yes,
            Type = tuple_type(Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, no, Circ,
            !VarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = yes,
            Type = apply_n_type(Var, Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        replace_in_type_location_2(Location, EqvMap, TypeCtorsAlreadyExpanded,
            RawType0, RawType, Changed, Circ, !VarSet, !EquivTypeInfo,
            !UsedModules),
        (
            Changed = yes,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(eqv_type_location::in, eqv_map::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, bool::in, bool::out, bool::in, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(Location, EqvMap, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, TArgs, Kind, Type, !Changed, !Circ, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    ( list.member(TypeCtor, TypeCtorsAlreadyExpanded) ->
        AlreadyExpanded = yes
    ;
        AlreadyExpanded = no
    ),
    (
        map.search(EqvMap, TypeCtor, eqv_type_body(EqvVarSet, Args0, Body0)),

        % Don't merge in the variable names from the type declaration to avoid
        % creating multiple variables with the same name so that
        % `varset.create_name_var_map' can be used on the resulting tvarset.
        % make_hlds uses `varset.create_name_var_map' to match up type
        % variables in `:- pragma type_spec' declarations and explicit type
        % qualifications with the type variables in the predicate's
        % declaration.

        tvarset_merge_renaming_without_names(!.VarSet, EqvVarSet, !:VarSet,
            Renaming),
        !.Circ = no,
        AlreadyExpanded = no
    ->
        type_ctor_used_modules(Location, TypeCtor, !UsedModules),

        !:Changed = yes,
        map.apply_to_list(Args0, Renaming, Args),
        apply_variable_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        record_expanded_item(item_id(type_abstract_item, TypeCtorItem),
            !EquivTypeInfo),
        map.from_corresponding_lists(Args, TArgs, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_location_2(Location, EqvMap,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !VarSet, !EquivTypeInfo, !UsedModules)
    ;
        (
            !.Changed = yes,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, TArgs, Kind)
        ;
            !.Changed = no,
            Type = Type0
        ),
        bool.or(AlreadyExpanded, !Circ)
    ).

:- pred type_ctor_used_modules(eqv_type_location::in, type_ctor::in,
    used_modules::in, used_modules::out) is det.

type_ctor_used_modules(eqv_type_out_of_module, _, !UsedModules).
type_ctor_used_modules(eqv_type_in_interface, type_ctor(Name, _),
        !UsedModules) :-
    add_sym_name_module(visibility_public, Name, !UsedModules).
type_ctor_used_modules(eqv_type_in_implementation, type_ctor(Name, _),
        !UsedModules) :-
    add_sym_name_module(visibility_private, Name, !UsedModules).

:- pred replace_in_inst(eqv_type_location::in,
    mer_inst::in, eqv_inst_map::in, mer_inst::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(Location, Inst0, EqvInstMap, Inst,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_inst_location(Location, Inst0, EqvInstMap, set.init, Inst,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_inst_location(eqv_type_location::in, mer_inst::in,
    eqv_inst_map::in, set(inst_id)::in, mer_inst::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(Location, Inst0, EqvInstMap, ExpandedInstIds, Inst,
        !EquivTypeInfo, !UsedModules) :-
    % XXX Need to record the used modules
    ( Inst0 = defined_inst(user_inst(SymName, ArgInsts)) ->
        InstId = inst_id(SymName, length(ArgInsts)),
        (
            set.member(InstId, ExpandedInstIds)
        ->
            Inst = Inst0
        ;
            map.search(EqvInstMap, InstId, EqvInstBody),
            EqvInstBody = eqv_inst_body(_, EqvInstParams, EqvInst)
        ->
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstIdItem = inst_id_to_item_name(InstId),
            record_expanded_item(item_id(inst_item, InstIdItem),
                !EquivTypeInfo),
            replace_in_inst_location(Location, Inst1, EqvInstMap,
                set.insert(ExpandedInstIds, InstId), Inst,
                !EquivTypeInfo, !UsedModules)
        ;
            Inst = Inst0
        )
    ;
        Inst = Inst0
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_pred_type(eqv_type_location::in, sym_name::in,
    pred_or_func::in, prog_context::in, eqv_map::in, eqv_inst_map::in,
    prog_constraints::in, prog_constraints::out,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_type(Location, PredName, PredOrFunc, Context,
        EqvMap, EqvInstMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, !TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        !Det, !EquivTypeInfo, !UsedModules, !:Specs) :-
    replace_in_prog_constraints_location(Location, EqvMap,
        ClassContext0, ClassContext, !TypeVarSet,
        !EquivTypeInfo, !UsedModules),
    replace_in_tms(Location, EqvMap, TypesAndModes0, TypesAndModes1,
        !TypeVarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_location(Location, EqvMap, WithType0, WithType, _,
            !TypeVarSet, !EquivTypeInfo, !UsedModules),
        (
            type_is_higher_order_details(WithType, _Purity, PredOrFunc,
                _EvalMethod, ExtraTypesPrime)
        ->
            ExtraTypes = ExtraTypesPrime,
            !:Specs = []
        ;
            ExtraTypes = [],
            Pieces1 = [words("In type declaration for"),
                p_or_f(PredOrFunc), sym_name(PredName), suffix(":"), nl,
                words("error: expected higher order"), p_or_f(PredOrFunc),
                words("type after `with_type`."), nl],
            Msg1 = simple_msg(Context, [always(Pieces1)]),
            Spec1 = error_spec(severity_error, phase_expand_types, [Msg1]),
            !:Specs = [Spec1]
        )
    ;
        MaybeWithType0 = no,
        ExtraTypes = [],
        !:Specs = []
    ),

    replace_in_pred_mode(Location, PredName, length(TypesAndModes0),
        Context, type_decl, EqvInstMap, ExtraModes, yes(PredOrFunc), _,
        MaybeWithInst0, _, !Det, !EquivTypeInfo, !UsedModules, ModeSpecs),
    !:Specs = !.Specs ++ ModeSpecs,

    (
        !.Specs = [_ | _],
        ExtraTypesAndModes = []
    ;
        !.Specs = [],
        (
            ExtraModes = [],
            ExtraTypesAndModes = list.map((func(Type) = type_only(Type)),
                ExtraTypes)
        ;
            ExtraModes = [_ | _],
            ( length(ExtraTypes) `with_type` int = length(ExtraModes) ->
                assoc_list.from_corresponding_lists(ExtraTypes, ExtraModes,
                    ExtraTypesModes),
                ExtraTypesAndModes = list.map(
                    (func(Type - Mode) = type_and_mode(Type, Mode)),
                    ExtraTypesModes)
            ;
                Pieces2 = [words("In type declaration for"),
                    p_or_f(PredOrFunc), sym_name(PredName), suffix(":"), nl,
                    words("error: the `with_type` and `with_inst`"),
                    words("annotations are incompatible."), nl],
                Msg2 = simple_msg(Context, [always(Pieces2)]),
                Spec2 = error_spec(severity_error, phase_expand_types, [Msg2]),
                !:Specs = [Spec2 | !.Specs],
                ExtraTypesAndModes = []
            )
        )
    ),
    (
        !.Specs = [],
        MaybeWithType = no,
        MaybeWithInst = no
    ;
        !.Specs = [_ | _],
        % Leave the `with_type` and `with_inst` fields so that make_hlds knows
        % to discard this declaration.
        MaybeWithType = MaybeWithType0,
        MaybeWithInst = MaybeWithInst0
    ),
    (
        ExtraTypesAndModes = [],
        TypesAndModes = TypesAndModes1
    ;
        ExtraTypesAndModes = [_ | _],
        OrigItemId = item_id(pred_or_func_to_item_type(PredOrFunc),
            item_name(PredName, list.length(TypesAndModes0))),
        record_expanded_item(OrigItemId, !EquivTypeInfo),
        TypesAndModes = TypesAndModes1 ++ ExtraTypesAndModes
    ).

:- pred replace_in_pred_mode(eqv_type_location::in, sym_name::in, arity::in,
    prog_context::in, pred_or_func_decl_type::in, eqv_inst_map::in,
    list(mer_mode)::out,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_mode(Location, PredName, OrigArity, Context, DeclType,
        EqvInstMap, ExtraModes, MaybePredOrFunc0, MaybePredOrFunc,
        MaybeWithInst0, MaybeWithInst, !MaybeDet, !EquivTypeInfo, !UsedModules,
        Specs) :-
    (
        MaybeWithInst0 = yes(WithInst0),
        replace_in_inst(Location, WithInst0, EqvInstMap, WithInst,
            !EquivTypeInfo, !UsedModules),
        (
            WithInst = ground(_, GroundInstInfo),
            GroundInstInfo = higher_order(HOInst),
            HOInst = pred_inst_info(PredOrFunc, ExtraModes0, _, DetPrime),
            ( MaybePredOrFunc0 = no
            ; MaybePredOrFunc0 = yes(PredOrFunc)
            )
        ->
            !:MaybeDet = yes(DetPrime),
            MaybeWithInst = no,
            MaybePredOrFunc = yes(PredOrFunc),
            ExtraModes = ExtraModes0,
            (
                MaybePredOrFunc0 = no,
                RecordedPredOrFunc = pf_predicate
            ;
                MaybePredOrFunc0 = yes(RecordedPredOrFunc)
            ),
            OrigItemId = item_id(pred_or_func_to_item_type(RecordedPredOrFunc),
                item_name(PredName, OrigArity)),
            record_expanded_item(OrigItemId, !EquivTypeInfo),
            Specs = []
        ;
            ExtraModes = [],
            MaybePredOrFunc = MaybePredOrFunc0,
            % Leave the `with_inst` fields so that make_hlds
            % knows to discard this declaration.
            MaybeWithInst = MaybeWithInst0,
            ( DeclType = type_decl, DeclStr = "declaration"
            ; DeclType = mode_decl, DeclStr = "mode declaration"
            ),
            (
                MaybePredOrFunc = no,
                PredOrFuncPieces = []
            ;
                MaybePredOrFunc = yes(PredOrFunc),
                PredOrFuncPieces = [p_or_f(PredOrFunc)]
            ),
            Pieces = [words("In"), words(DeclStr), words("for")] ++
                PredOrFuncPieces ++ [sym_name(PredName), suffix(":"), nl,
                words("error: expected higher order ")] ++ PredOrFuncPieces ++
                [words("inst after `with_inst`."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_expand_types, [Msg]),
            Specs = [Spec]
        )
    ;
        MaybeWithInst0 = no,
        MaybeWithInst = MaybeWithInst0,
        MaybePredOrFunc = MaybePredOrFunc0,
        ExtraModes = [],
        Specs = []
    ).

:- pred replace_in_tms(eqv_type_location::in, eqv_map::in,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_tms(Location, EqvMap, !TMs, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    list.map_foldl3(replace_in_tm(Location, EqvMap), !TMs,
        !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_tm(eqv_type_location::in, eqv_map::in,
    type_and_mode::in, type_and_mode::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_tm(Location, EqvMap, type_only(Type0),
        type_only(Type), !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet,
        !EquivTypeInfo, !UsedModules).

replace_in_tm(Location, EqvMap, type_and_mode(Type0, Mode),
        type_and_mode(Type, Mode), !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet,
        !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%
%
:- pred replace_in_structure_sharing_domain(eqv_type_location::in, eqv_map::in,
    structure_sharing_domain::in, structure_sharing_domain::out,
    tvarset::in, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_domain(_, _, X @ structure_sharing_bottom,
    X, _TVarset, !EquivTypeInfo, !UsedModules).
replace_in_structure_sharing_domain(_, _, X @ structure_sharing_top(_),
    X, _TVarset, !EquivTypeInfo, !UsedModules).
replace_in_structure_sharing_domain(Location, EqvMap,
        structure_sharing_real(SharingPairs0),
        structure_sharing_real(SharingPairs),
        TVarset, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl2(
        replace_in_structure_sharing_pair(Location, EqvMap, TVarset),
        SharingPairs0, SharingPairs, !EquivTypeInfo, !UsedModules).

:- pred replace_in_structure_sharing_pair(eqv_type_location::in,
    eqv_map::in, tvarset::in,
    structure_sharing_pair::in, structure_sharing_pair::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(Location, EqvMap, TVarset, Data10 - Data20,
        Data1 - Data2, !EquivTypeInfo, !UsedModules) :-
    replace_in_datastruct(Location, EqvMap, TVarset, Data10, Data1,
        !EquivTypeInfo, !UsedModules),
    replace_in_datastruct(Location, EqvMap, TVarset, Data20, Data2,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_datastruct(eqv_type_location::in,
    eqv_map::in, tvarset::in, datastruct::in,
    datastruct::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(Location, EqvMap, TVarset, Data0, Data, !EquivTypeInfo,
        !UsedModules) :-
    Sel0 = Data0 ^ sc_selector,
    list.map_foldl2(replace_in_unit_selector(Location, EqvMap, TVarset),
        Sel0, Sel, !EquivTypeInfo, !UsedModules),
    Data = Data0 ^ sc_selector := Sel.

:- pred replace_in_unit_selector(eqv_type_location::in,
    eqv_map::in, tvarset::in, unit_selector::in,
    unit_selector::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_unit_selector(_, _, _, X @ termsel(_, _), X, !EquivTypeInfo, !UMs).
replace_in_unit_selector(Location, EqvMap, TVarset,
        typesel(Type0), typesel(Type), !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, TVarset, _,
        !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%

:- type expanded_item_set == pair(module_name, set(item_id)).

maybe_start_recording_expanded_items(_, _, no, no).
maybe_start_recording_expanded_items(ModuleName, SymName, yes(_), MaybeInfo) :-
    ( SymName = qualified(ModuleName, _) ->
        MaybeInfo = no
    ;
        MaybeInfo = yes(ModuleName - set.init)
    ).

:- pred record_expanded_item(item_id::in,
    equiv_type_info::in, equiv_type_info::out) is det.

record_expanded_item(Item, !EquivTypeInfo) :-
    map_maybe(record_expanded_item_2(Item), !EquivTypeInfo).

:- pred record_expanded_item_2(item_id::in,
    expanded_item_set::in, expanded_item_set::out) is det.

record_expanded_item_2(ItemId, ExpandedItemSet0, ExpandedItemSet) :-
    ExpandedItemSet0 = ModuleName - Items0,
    ItemId = item_id(_, ItemName),
    ( ItemName = item_name(qualified(ModuleName, _), _) ->
        % We don't need to record local types.
        ExpandedItemSet = ExpandedItemSet0
    ;
        set.insert(ItemId, Items0, Items),
        ExpandedItemSet = ModuleName - Items
    ).

finish_recording_expanded_items(_, no, no, no).
finish_recording_expanded_items(_, no, yes(Info), yes(Info)).
finish_recording_expanded_items(_, yes(_), no, _) :-
    unexpected($module, $pred, "items but no info").
finish_recording_expanded_items(Item, yes(_ - ExpandedItems),
        yes(Info0), yes(Info)) :-
    recompilation.record_expanded_items(Item, ExpandedItems, Info0, Info).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%-----------------------------------------------------------------------------%
