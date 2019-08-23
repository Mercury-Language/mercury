%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: equiv_type.m.
% Main author: fjh.
%
% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.
%
%---------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % expand_eqv_types_insts(!AugCompUnit, !EventSpecMap,
    %   CircularTypes, TypeEqvMap, !MaybeRecompInfo, Specs):
    %
    % This predicate finds all type and inst declarations that define a type
    % or inst to be equivalent to another type or inst. It builds up two maps
    % of such declarations, and then traverses through all the items in the
    % given item blocks and through all the given event specs, expanding all
    % type and inst synonyms, which has the effect of eliminating all the
    % equivalence types and insts from the source code. We return the
    % equivalence map for types (our callers don't need the corresponding map
    % for insts).
    %
    % It also expands `with_type` and `with_inst` annotations on predicate and
    % function type declarations.
    %
    % It generates error messages for any circular equivalence types and insts
    % and for invalid `with_type` and `with_inst` annotations.
    %
    % For items not defined in the current module, the items expanded
    % while processing each item are recorded in the recompilation_info,
    % for use by smart recompilation.
    %
:- pred expand_eqv_types_insts(
    aug_compilation_unit::in, aug_compilation_unit::out,
    event_spec_map::in, event_spec_map::out,
    type_eqv_map::out, used_modules::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    list(error_spec)::out) is det.

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_type(type_eqv_map::in, mer_type::in, mer_type::out,
    bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_type_list(type_eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_prog_constraints(type_eqv_map::in,
    prog_constraints::in, prog_constraints::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_prog_constraint_list(type_eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out)
    is det.

:- pred replace_in_ctors(type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- type eqv_type_body
    --->    eqv_type_body(
                tvarset,
                list(type_param),
                mer_type
            ).

:- type type_eqv_map == map(type_ctor, eqv_type_body).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

expand_eqv_types_insts(AugCompUnit0, AugCompUnit, EventSpecMap0, EventSpecMap,
        TypeEqvMap, !:UsedModules, !RecompInfo, !:Specs) :-
    AugCompUnit0 = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, SrcItemBlocks0,
        DirectIntItemBlocks0, IndirectIntItemBlocks0,
        OptItemBlocks0, IntForOptItemBlocks0),
    % First we build up a mapping which records the equivalence type
    % definitions, ...
    some [!TypeEqvMap, !InstEqvMap] (
        map.init(!:TypeEqvMap),
        map.init(!:InstEqvMap),
        build_eqv_maps_in_item_blocks(SrcItemBlocks0,
            !TypeEqvMap, !InstEqvMap),
        list.filter(non_abstract_imported_int_item_block,
            DirectIntItemBlocks0, NonAbstractDirectIntItemBlocks0),
        build_eqv_maps_in_item_blocks(NonAbstractDirectIntItemBlocks0,
            !TypeEqvMap, !InstEqvMap),
        list.filter(non_abstract_imported_int_item_block,
            IndirectIntItemBlocks0, NonAbstractIndirectIntItemBlocks0),
        build_eqv_maps_in_item_blocks(NonAbstractIndirectIntItemBlocks0,
            !TypeEqvMap, !InstEqvMap),
        build_eqv_maps_in_item_blocks(IntForOptItemBlocks0,
            !TypeEqvMap, !InstEqvMap),
        build_eqv_maps_in_item_blocks(OptItemBlocks0,
            !TypeEqvMap, !InstEqvMap),
        TypeEqvMap = !.TypeEqvMap,
        InstEqvMap = !.InstEqvMap
    ),

    % .. and then we go through all the items in the relevant blocks
    % and in all the event specs, and replace all occurrences of
    % equivalence types and insts in them.
    !:UsedModules = used_modules_init,
    !:Specs = [],
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        src_maybe_record_sym_name, SrcItemBlocks0, [], RevSrcItemBlocks,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        int_maybe_record_sym_name, DirectIntItemBlocks0,
        [], RevDirectIntItemBlocks,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        int_maybe_record_sym_name, IndirectIntItemBlocks0,
        [], RevIndirectIntItemBlocks,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        int_for_opt_maybe_record_sym_name, IntForOptItemBlocks0,
        [], RevIntForOptItemBlocks,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        opt_maybe_record_sym_name, OptItemBlocks0, [], RevOptItemBlocks,
        !RecompInfo, !UsedModules, !Specs),

    list.reverse(RevSrcItemBlocks, SrcItemBlocks),
    list.reverse(RevDirectIntItemBlocks, DirectIntItemBlocks),
    list.reverse(RevIndirectIntItemBlocks, IndirectIntItemBlocks),
    list.reverse(RevOptItemBlocks, OptItemBlocks),
    list.reverse(RevIntForOptItemBlocks, IntForOptItemBlocks),
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, SrcItemBlocks,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),

    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    replace_in_event_specs(EventSpecList0, EventSpecList,
        TypeEqvMap, InstEqvMap, !RecompInfo, !UsedModules, !Specs),
    map.from_sorted_assoc_list(EventSpecList, EventSpecMap).

%---------------------------------------------------------------------------%

    % We need to expand equivalence insts in
    % `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body
    --->    eqv_inst_body(
                inst_varset,
                list(inst_var),
                mer_inst
            ).

:- type inst_eqv_map == map(inst_id, eqv_inst_body).

:- type pred_or_func_decl_type
    --->    type_decl
    ;       mode_decl.

:- pred non_abstract_imported_int_item_block(
    item_block(int_module_section)::in) is semidet.

non_abstract_imported_int_item_block(ItemBlock) :-
    ItemBlock = item_block(_, Section, _, _, _, _),
    require_complete_switch [Section]
    (
        Section = ims_imported_or_used(_, _, _, _)
    ;
        Section = ims_abstract_imported(_, _),
        fail
    ).

:- pred build_eqv_maps_in_item_blocks(list(item_block(MS))::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_item_blocks([], !TypeEqvMap, !InstEqvMap).
build_eqv_maps_in_item_blocks([ItemBlock | ItemBlocks],
        !TypeEqvMap, !InstEqvMap) :-
    ItemBlock = item_block(_, _, _, _, _, Items),
    build_eqv_maps_in_items(Items, !TypeEqvMap, !InstEqvMap),
    build_eqv_maps_in_item_blocks(ItemBlocks, !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_items(list(item)::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_items([], !TypeEqvMap, !InstEqvMap).
build_eqv_maps_in_items([Item | Items], !TypeEqvMap, !InstEqvMap) :-
    build_eqv_maps_in_item(Item, !TypeEqvMap, !InstEqvMap),
    build_eqv_maps_in_items(Items, !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_item(item::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_item(Item, !TypeEqvMap, !InstEqvMap) :-
    ( if
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, TypeParams, TypeDefn, VarSet,
            _Context, _SeqNum),
        TypeDefn = parse_tree_eqv_type(type_details_eqv(EqvType))
   then
        list.length(TypeParams, Arity),
        TypeCtor = type_ctor(Name, Arity),
        map.set(TypeCtor, eqv_type_body(VarSet, TypeParams, EqvType),
            !TypeEqvMap)
    else if
        Item = item_inst_defn(ItemInstDefn),
        InstDefn = nonabstract_inst_defn(eqv_inst(EqvInst)),
        ItemInstDefn = item_inst_defn_info(Name, InstParams, _IFTC,
            InstDefn, VarSet, _Context, _SeqNum)
    then
        list.length(InstParams, Arity),
        InstId = inst_id(Name, Arity),
        map.set(InstId, eqv_inst_body(VarSet, InstParams, EqvInst),
            !InstEqvMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- type maybe_record_sym_name_use
    --->    dont_record_sym_name_use
    ;       record_sym_name_use(item_visibility).

:- type section_maybe_record_sym_name_use(MS) ==
    (func(MS) = maybe_record_sym_name_use).

:- func src_maybe_record_sym_name(src_module_section) =
    maybe_record_sym_name_use.
:- func int_maybe_record_sym_name(int_module_section) =
    maybe_record_sym_name_use.
:- func opt_maybe_record_sym_name(opt_module_section) =
    maybe_record_sym_name_use.
:- func int_for_opt_maybe_record_sym_name(int_for_opt_module_section) =
    maybe_record_sym_name_use.

src_maybe_record_sym_name(sms_interface) =
    record_sym_name_use(visibility_public).
src_maybe_record_sym_name(sms_implementation) =
    record_sym_name_use(visibility_private).
src_maybe_record_sym_name(sms_impl_but_exported_to_submodules) =
    record_sym_name_use(visibility_private).

int_maybe_record_sym_name(_) = dont_record_sym_name_use.
opt_maybe_record_sym_name(_) = dont_record_sym_name_use.
int_for_opt_maybe_record_sym_name(_) = dont_record_sym_name_use.

%---------------------------------------------------------------------------%

    % The following predicate replace_in_item_blocks performs substitution
    % of equivalence types on a list of item_blocks. Similarly, the other
    % replace_in_<foo> predicates that follow perform substitution of
    % equivalence types on <foo>s.
    %
:- pred replace_in_item_blocks(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    section_maybe_record_sym_name_use(MS)::in, list(item_block(MS))::in,
    list(item_block(MS))::in, list(item_block(MS))::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_item_blocks(_, _, _, _, [],
        !RevReplItemBlocks, !RecompInfo, !UsedModules, !Specs).
replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap, SectionVisibility,
        [ItemBlock0 | ItemBlocks0],
        !RevReplItemBlocks, !RecompInfo, !UsedModules, !Specs) :-
    ItemBlock0 = item_block(BlockModuleName, Section,
        Incls, Avails, FIMs, Items0),
    MaybeRecord = SectionVisibility(Section),
    replace_in_items(ModuleName, TypeEqvMap, InstEqvMap, MaybeRecord,
        Items0, [], RevReplItems, !RecompInfo, !UsedModules, !Specs),
    list.reverse(RevReplItems, ReplItems),
    ReplItemBlock = item_block(BlockModuleName, Section,
        Incls, Avails, FIMs, ReplItems),
    !:RevReplItemBlocks = [ReplItemBlock | !.RevReplItemBlocks],
    replace_in_item_blocks(ModuleName, TypeEqvMap, InstEqvMap,
        SectionVisibility, ItemBlocks0,
        !RevReplItemBlocks, !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_items(module_name::in, type_eqv_map::in, inst_eqv_map::in,
    maybe_record_sym_name_use::in, list(item)::in,
    list(item)::in, list(item)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_items(_, _, _, _,
        [], !RevReplItems, !RecompInfo, !UsedModules, !Specs).
replace_in_items(ModuleName, TypeEqvMap, InstEqvMap, MaybeRecord,
        [Item0 | Items0], !RevReplItems, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_item(ModuleName, TypeEqvMap, InstEqvMap, MaybeRecord,
        Item0, Item, !RecompInfo, !UsedModules, ItemSpecs),
    % Discard the item if there were any errors.
    (
        ItemSpecs = [],
        !:RevReplItems = [Item | !.RevReplItems]
    ;
        ItemSpecs = [_ | _],
        !:Specs = ItemSpecs ++ !.Specs
    ),
    replace_in_items(ModuleName, TypeEqvMap, InstEqvMap, MaybeRecord,
        Items0, !RevReplItems, !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_item(module_name::in, type_eqv_map::in, inst_eqv_map::in,
    maybe_record_sym_name_use::in, item::in, item::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_item(ModuleName, TypeEqvMap, InstEqvMap, MaybeRecord,
        Item0, Item, !RecompInfo, !UsedModules, Specs) :-
    (
        Item0 = item_type_defn(ItemTypeDefn0),
        replace_in_type_defn_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemTypeDefn0, ItemTypeDefn, !RecompInfo, !UsedModules, Specs),
        Item = item_type_defn(ItemTypeDefn)
    ;
        Item0 = item_inst_defn(_ItemInstDefn0),
        % If inst i1's body contains inst i2, and i2 has been defined
        % to be equivalent to some other inst i3, then we *could* replace
        % i2 with i3 in i1's body. Instead of doing this once for this
        % user-defined inst, we do it on every use of this inst. This is
        % significantly less efficient, but if there is any error that
        % involves this inst, the error message we generate will refer to
        % the inst by the name the user gave it. If the user e.g. wrote
        % an inst i1 in a mode declaration, but an error message about
        % that mode declaration referred to the expanded form of i1,
        % this would be confusing to many programmers. Most likely,
        % it would also be harder to read, since inst names are almost always
        % shorter than the insts they are defined to be equivalent to.
        %
        % XXX IFTC
        % If inst i1 is for type t2, and t2 has been defined to be
        % equivalent to type t3, then we SHOULD record that i1 is really
        % for t3. However, while t2 is required to be just a type_ctor
        % and arity, t3 may be more complex. The obvious thing to do would be
        % to record that i1 is for t3's top type_ctor and its arity. Whether
        % that is good enough depends on what *exactly* we will do with the
        % "inst for type ctor" information. We don't yet know the answer
        % to that question.
        % XXX This should allow us to fix Mantis bug #89.
        Item = Item0,
        Specs = []
    ;
        Item0 = item_pred_decl(ItemPredDecl0),
        replace_in_pred_decl_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemPredDecl0, ItemPredDecl, !RecompInfo, !UsedModules, Specs),
        Item = item_pred_decl(ItemPredDecl)
    ;
        Item0 = item_mode_decl(ItemModeDecl0),
        replace_in_mode_decl_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemModeDecl0, ItemModeDecl, !RecompInfo, !UsedModules, Specs),
        Item = item_mode_decl(ItemModeDecl)
    ;
        Item0 = item_pragma(ItemPragma0),
        replace_in_pragma_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemPragma0, ItemPragma, !RecompInfo, !UsedModules, Specs),
        Item = item_pragma(ItemPragma)
    ;
        Item0 = item_typeclass(ItemTypeClass0),
        replace_in_typeclass_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemTypeClass0, ItemTypeClass, !RecompInfo, !UsedModules, Specs),
        Item = item_typeclass(ItemTypeClass)
    ;
        Item0 = item_instance(ItemInstance0),
        replace_in_instance_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemInstance0, ItemInstance, !RecompInfo, !UsedModules, Specs),
        Item = item_instance(ItemInstance)
    ;
        Item0 = item_mutable(ItemMutable0),
        replace_in_mutable_info(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap,
            ItemMutable0, ItemMutable, !RecompInfo, !UsedModules, Specs),
        Item = item_mutable(ItemMutable)
    ;
        Item0 = item_mode_defn(_),
        % XXX zs: This seems to be a bug. Mode definitions contain insts,
        % we should see if they need expansion.
        Item = Item0,
        Specs = []
    ;
        ( Item0 = item_clause(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ),
        Item = Item0,
        Specs = []
    ;
        Item0 = item_type_repn(_),
        % XXX TYPE_REPN Implement this.
        unexpected($pred, "item_type_repn nyi")
    ).

:- pred replace_in_type_defn_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_type_defn_info::in, item_type_defn_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_type_defn_info(SymName, ArgTypeVars, TypeDefn0, VarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    maybe_start_recording_expanded_items(ModuleName, SymName, !.RecompInfo,
        UsedTypeCtors0),
    replace_in_type_defn(MaybeRecord, TypeEqvMap, InstEqvMap,
        type_ctor(SymName, Arity), TypeDefn0, TypeDefn, ContainsCirc,
        VarSet0, VarSet, UsedTypeCtors0, UsedTypeCtors, !UsedModules),
    (
        ContainsCirc = yes,
        ( if TypeDefn0 = parse_tree_eqv_type(_) then
            list.length(ArgTypeVars, NumArgTypeVars),
            Pieces = [words("Error: circular equivalence type"),
                qual_sym_name_and_arity(
                    sym_name_arity(SymName, NumArgTypeVars)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_expand_types, [Msg]),
            Specs = [Spec]
        else
            unexpected($pred, "invalid item")
        )
    ;
        ContainsCirc = no,
        Specs = []
    ),
    ItemId = item_id(type_body_item, item_name(SymName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_type_defn_info(SymName, ArgTypeVars, TypeDefn, VarSet,
        Context, SeqNum).

:- pred replace_in_pred_decl_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_pred_decl_info::in, item_pred_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_decl_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_pred_decl_info(PredName, PredOrFunc, TypesAndModes0,
        MaybeWithType0, MaybeWithInst0, MaybeDetism0, Origin,
        TypeVarSet0, InstVarSet, ExistQVars, Purity, ClassContext0,
        Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
        TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        MaybeDetism0, MaybeDetism, ExpandedItems0, ExpandedItems,
        !UsedModules, Specs),
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    list.length(TypesAndModes, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_pred_decl_info(PredName, PredOrFunc, TypesAndModes,
        MaybeWithType, MaybeWithInst, MaybeDetism, Origin,
        TypeVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, SeqNum).

:- pred replace_in_mode_decl_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_mode_decl_info::in, item_mode_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mode_decl_info(ModuleName, MaybeRecord, _TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_mode_decl_info(PredName, MaybePredOrFunc0, Modes0,
        WithInst0, MaybeDetism0, InstVarSet, Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_mode(MaybeRecord, InstEqvMap, PredName,
        list.length(Modes0), Context, mode_decl, ExtraModes,
        MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst,
        MaybeDetism0, MaybeDetism, ExpandedItems0, ExpandedItems,
        !UsedModules, Specs),
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
    Info = item_mode_decl_info(PredName, MaybePredOrFunc, Modes,
        WithInst, MaybeDetism, InstVarSet, Context, SeqNum).

:- pred replace_in_typeclass_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_typeclass_info::in, item_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_typeclass_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_typeclass_info(ClassName, Vars, Constraints0, FunDeps,
        ClassInterface0, VarSet0, Context, SeqNum),
    list.length(Vars, Arity),
    maybe_start_recording_expanded_items(ModuleName, ClassName, !.RecompInfo,
        ExpandedItems0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, VarSet0, VarSet,
        ExpandedItems0, ExpandedItems1, !UsedModules),
    (
        ClassInterface0 = class_interface_abstract,
        ClassInterface = class_interface_abstract,
        ExpandedItems = ExpandedItems1,
        Specs = []
    ;
        ClassInterface0 = class_interface_concrete(Methods0),
        replace_in_class_interface(MaybeRecord, TypeEqvMap, InstEqvMap,
            Methods0, Methods, ExpandedItems1, ExpandedItems,
            !UsedModules, [], Specs),
        ClassInterface = class_interface_concrete(Methods)
    ),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_typeclass_info(ClassName, Vars, Constraints, FunDeps,
        ClassInterface, VarSet, Context, SeqNum).

:- pred replace_in_instance_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_instance_info::in, item_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_instance_info(ModuleName, MaybeRecord, TypeEqvMap, _InstEqvMap,
        InstanceInfo0, InstanceInfo, !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody, VarSet0, ContainingModuleName,
        Context, SeqNum),
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        UsedTypeCtors0 = no
    else
        UsedTypeCtors0 = yes(eqv_expanded_item_set(ModuleName, set.init))
    ),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, VarSet0, VarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, Types0, Types,
        _, _, VarSet1, VarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    list.length(Types0, Arity),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, VarSet, ContainingModuleName,
        Context, SeqNum).

:- pred replace_in_pragma_info(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_pragma_info::in, item_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pragma_info(ModuleName, MaybeRecord, TypeEqvMap, _InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    Info0 = item_pragma_info(Pragma0, Origin, Context, SeqNum),
    (
        Pragma0 = pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(PredName, NewName, Arity,
            PorF, Modes, Subst0, VarSet0, ItemIds0),
        ( if
            ( !.RecompInfo = no
            ; PredName = qualified(ModuleName, _)
            )
        then
            ExpandedItems0 = no
        else
            ExpandedItems0 = yes(eqv_expanded_item_set(ModuleName, ItemIds0))
        ),
        replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst,
            VarSet0, VarSet, ExpandedItems0, ExpandedItems, !UsedModules),
        (
            ExpandedItems = no,
            ItemIds = ItemIds0
        ;
            ExpandedItems = yes(eqv_expanded_item_set(_, ItemIds))
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
            ( if
                UserSharing0 = user_sharing(Sharing0, MaybeTypes0),
                MaybeTypes0 = yes(user_type_info(Types0, TVarSet0))
            then
                replace_in_type_list_location(MaybeRecord,
                    TypeEqvMap, Types0, Types, _AnythingChanged,
                    TVarSet0, TVarSet, !EquivTypeInfo, !UsedModules),
                replace_in_structure_sharing_domain(MaybeRecord, TypeEqvMap,
                    TVarSet0, Sharing0, Sharing, !EquivTypeInfo, !UsedModules),
                MaybeTypes = yes(user_type_info(Types, TVarSet)),
                UserSharing = user_sharing(Sharing, MaybeTypes),
                set_user_annotated_sharing(UserSharing, Attrs0, Attrs)
            else
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
        ; Pragma0 = pragma_consider_used(_)
        ; Pragma0 = pragma_does_not_terminate(_)
        ; Pragma0 = pragma_exceptions(_)
        ; Pragma0 = pragma_external_proc(_)
        ; Pragma0 = pragma_fact_table(_)
        ; Pragma0 = pragma_foreign_code(_)
        ; Pragma0 = pragma_foreign_decl(_)
        ; Pragma0 = pragma_foreign_enum(_)
        ; Pragma0 = pragma_foreign_proc_export(_)
        ; Pragma0 = pragma_foreign_export_enum(_)
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
        ; Pragma0 = pragma_structure_reuse(_)
        ; Pragma0 = pragma_structure_sharing(_)
        ; Pragma0 = pragma_oisu(_)
        ; Pragma0 = pragma_tabled(_)
        ; Pragma0 = pragma_terminates(_)
        ; Pragma0 = pragma_termination2_info(_)
        ; Pragma0 = pragma_termination_info(_)
        ; Pragma0 = pragma_trailing_info(_)
        ; Pragma0 = pragma_unused_args(_)
        ; Pragma0 = pragma_require_tail_recursion(_)
        ),
        Pragma = Pragma0
    ),
    Info = item_pragma_info(Pragma, Origin, Context, SeqNum).

:- pred replace_in_mutable_info(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_mutable_info::in, item_mutable_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mutable_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    MutName = Info0 ^ mut_name,
    QualName = qualified(ModuleName, MutName),
    maybe_start_recording_expanded_items(ModuleName, QualName, !.RecompInfo,
        ExpandedItems0),
    replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap, Info0, Info,
        ExpandedItems0, ExpandedItems, !UsedModules),
    ItemId = item_id(mutable_item, item_name(QualName, 0)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo).

:- pred replace_in_mutable_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_mutable_info::in, item_mutable_info::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap, Info0, Info,
        !ExpandedItems, !UsedModules) :-
    Info0 = item_mutable_info(MutName, OrigType, Type0, OrigInst, Inst0,
        InitValue, Attrs, Varset, Context, SeqNum),
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap, Type0, Type,
        _TypeChanged, TVarSet0, _TVarSet, !ExpandedItems, !UsedModules),
    replace_in_inst(MaybeRecord, InstEqvMap, Inst0, Inst, !ExpandedItems,
        !UsedModules),
    Info = item_mutable_info(MutName, OrigType, Type, OrigInst, Inst,
        InitValue, Attrs, Varset, Context, SeqNum).

:- pred replace_in_event_specs(
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    type_eqv_map::in, inst_eqv_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_specs([], [], _, _, !RecompInfo, !UsedModules, !Specs).
replace_in_event_specs(
        [Name - EventSpec0 | NameSpecs0], [Name - EventSpec | NameSpecs],
        TypeEqvMap, InstEqvMap, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_spec(EventSpec0, EventSpec,
        TypeEqvMap, InstEqvMap, !RecompInfo, !UsedModules, !Specs),
    replace_in_event_specs(NameSpecs0, NameSpecs,
        TypeEqvMap, InstEqvMap, !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_spec(event_spec::in, event_spec::out,
    type_eqv_map::in, inst_eqv_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_spec(EventSpec0, EventSpec, TypeEqvMap, InstEqvMap,
        !RecompInfo, !UsedModules, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SyntAttrNumOrder),
    replace_in_event_attrs(Attrs0, Attrs, TypeEqvMap, InstEqvMap,
        !RecompInfo, !UsedModules, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SyntAttrNumOrder).

:- pred replace_in_event_attrs(
    list(event_attribute)::in, list(event_attribute)::out,
    type_eqv_map::in, inst_eqv_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attrs([], [], _TypeEqvMap, _InstEqvMap,
        !RecompInfo, !UsedModules, !Specs).
replace_in_event_attrs([Attr0 | Attrs0], [Attr | Attrs],
        TypeEqvMap, InstEqvMap, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_attr(Attr0, Attr, TypeEqvMap, InstEqvMap,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_event_attrs(Attrs0, Attrs, TypeEqvMap, InstEqvMap,
        !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_attr(event_attribute::in, event_attribute::out,
    type_eqv_map::in, inst_eqv_map::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attr(Attr0, Attr, TypeEqvMap, _InstEqvMap,
        !RecompInfo, !UsedModules, !Specs) :-
    % We construct the attributes' modes ourselves in event_spec.m; they should
    % not contain type names.
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode,
        MaybeSynthCall),
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use(dont_record_sym_name_use, TypeEqvMap,
        AttrType0, AttrType, _Changed, TVarSet0, _TVarSet, no, _EquivTypeInfo,
        !UsedModules),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

:- pred replace_in_type_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in,
    type_defn::in, type_defn::out, bool::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_defn(MaybeRecord, TypeEqvMap, InstEqvMap, TypeCtor,
        TypeDefn0, TypeDefn, ContainsCirc, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    (
        TypeDefn0 = parse_tree_eqv_type(DetailsEqv0),
        DetailsEqv0 = type_details_eqv(TypeBody0),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
            TypeBody0, TypeBody, _, ContainsCirc, !VarSet, !EquivTypeInfo,
            !UsedModules),
        DetailsEqv = type_details_eqv(TypeBody),
        TypeDefn = parse_tree_eqv_type(DetailsEqv)
    ;
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        DetailsDu0 = type_details_du(Ctors0, EqPred, DirectArgFunctors),
        replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors,
            !VarSet, !EquivTypeInfo, !UsedModules),
        ContainsCirc = no,
        DetailsDu = type_details_du(Ctors, EqPred, DirectArgFunctors),
        TypeDefn = parse_tree_du_type(DetailsDu)
    ;
        TypeDefn0 = parse_tree_solver_type(DetailsSolver0),
        DetailsSolver0 = type_details_solver(SolverDetails0, MaybeUserEqComp),
        SolverDetails0 = solver_type_details(RepresentationType0,
            GroundInst, AnyInst, MutableInfos0),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
            RepresentationType0, RepresentationType,
            _, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules),
        replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
            MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules),
        SolverDetails = solver_type_details(RepresentationType,
            GroundInst, AnyInst, MutableInfos),
        DetailsSolver = type_details_solver(SolverDetails, MaybeUserEqComp),
        TypeDefn = parse_tree_solver_type(DetailsSolver)
    ;
        ( TypeDefn0 = parse_tree_abstract_type(_)
        ; TypeDefn0 = parse_tree_foreign_type(_)
        ),
        TypeDefn = TypeDefn0,
        ContainsCirc = no
    ).

%---------------------------------------------------------------------------%

replace_in_type(TypeEqvMap, Type0, Type, Changed, !VarSet, !EquivTypeInfo) :-
    replace_in_type_maybe_record_use(dont_record_sym_name_use,
        TypeEqvMap, Type0, Type, Changed, !VarSet,
        !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_maybe_record_use(maybe_record_sym_name_use::in,
    type_eqv_map::in, mer_type::in, mer_type::out, bool::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        Type0, Type, Changed, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [],
        Type0, Type, Changed, _, !VarSet, !EquivTypeInfo, !UsedModules).

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_maybe_record_use_2(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in, mer_type::in, mer_type::out,
    bool::out, bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
        TypeCtorsAlreadyExpanded, Type0, Type, Changed, Circ,
        !VarSet, !EquivTypeInfo, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = no,
        Circ = no
    ;
        Type0 = defined_type(SymName, TArgs0, Kind),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, TArgs0, TArgs, ArgsChanged, no, Circ0,
            !VarSet, !EquivTypeInfo, !UsedModules),
        Arity = list.length(TArgs),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(MaybeRecord, TypeEqvMap, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, TArgs, Kind, Type, ArgsChanged, Changed,
            Circ0, Circ, !VarSet, !EquivTypeInfo, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = no,
        Circ = no
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, no, Circ,
            !VarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = yes,
            Type = higher_order_type(PorF, Args, HOInstInfo, Purity,
                EvalMethod)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = tuple_type(Args0, Kind),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
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
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
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
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, RawType0, RawType, Changed, Circ,
            !VarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = yes,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(maybe_record_sym_name_use::in, type_eqv_map::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, bool::in, bool::out, bool::in, bool::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(MaybeRecord, TypeEqvMap, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, TArgs, Kind, Type, !Changed, !Circ, !VarSet, !EquivTypeInfo,
        !UsedModules) :-
    ( if list.member(TypeCtor, TypeCtorsAlreadyExpanded) then
        AlreadyExpanded = yes
    else
        AlreadyExpanded = no
    ),
    ( if
        map.search(TypeEqvMap, TypeCtor, EqvTypeBody),
        EqvTypeBody = eqv_type_body(EqvVarSet, Args0, Body0),

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
    then
        maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor,
            !UsedModules),

        !:Changed = yes,
        map.apply_to_list(Args0, Renaming, Args),
        apply_variable_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        record_expanded_item(item_id(type_abstract_item, TypeCtorItem),
            !EquivTypeInfo),
        map.from_corresponding_lists(Args, TArgs, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !VarSet, !EquivTypeInfo, !UsedModules)
    else
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

%---------------------------------------------------------------------------%

replace_in_type_list(TypeEqvMap, !Typess, Changed, !VarSet, !EquivTypeInfo) :-
    replace_in_type_list_location(dont_record_sym_name_use, TypeEqvMap,
        !Typess, Changed, !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(MaybeRecord, TypeEqvMap, !Types,
        Changed, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, [], !Types,
        Changed, no, _, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out,
    bool::out, bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, !Ts,
        Changed, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, [], !Ts,
        Changed, no, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ_2(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in,
    list(mer_type)::in, list(mer_type)::out,
    bool::out, bool::in, bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ_2(_MaybeRecord, _TypeEqvMap, _Seen,
        [], [], no, !ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules).
replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        List0 @ [Type0 | Types0], List, Changed, !Circ, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, Seen,
        Type0, Type, Changed0, ContainsCirc, !VarSet, !EquivTypeInfo,
        !UsedModules),
    !:Circ = bool.or(!.Circ, ContainsCirc),
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        Types0, Types, Changed1, !Circ, !VarSet, !EquivTypeInfo, !UsedModules),
    ( if
        ( Changed0 = yes
        ; Changed1 = yes
        )
    then
        Changed = yes,
        List = [Type | Types]
    else
        Changed = no,
        List = List0
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_ctor_arg_list(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(constructor_arg)::in, list(constructor_arg)::out,
    bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(MaybeRecord, TypeEqvMap, !Args,
        ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, [], !Args,
        no, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_ctor_arg_list_loop(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    bool::in, bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_loop(_MaybeRecord, _TypeEqvMap, _Seen, [], [],
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules).
replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, Seen,
        [Arg0 | Args0], [Arg | Args],
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules) :-
    Arg0 = ctor_arg(Name, Type0, Context),
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, Seen,
        Type0, Type, _, ContainsCirc, !VarSet, !EquivTypeInfo, !UsedModules),
    Arg = ctor_arg(Name, Type, Context),
    !:Circ = bool.or(!.Circ, ContainsCirc),
    replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, Seen, Args0, Args,
        !Circ, !VarSet, !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

:- pred replace_in_constraint_store(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_constraint_store(_, _, _, [], [], !EquivTypeInfo, !UsedModules).
replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
        [MutableInfo0 | MutableInfos0], [MutableInfo | MutableInfos],
        !EquivTypeInfo, !UsedModules) :-
    replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap,
        MutableInfo0, MutableInfo, !EquivTypeInfo, !UsedModules),
    replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
        MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

replace_in_prog_constraints(TypeEqvMap, Cs0, Cs, !VarSet, !EquivTypeInfo) :-
    replace_in_prog_constraints_location(dont_record_sym_name_use, TypeEqvMap,
        Cs0, Cs, !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraints_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, prog_constraints::in, prog_constraints::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraints_location(MaybeRecord, TypeEqvMap, Cs0, Cs, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    Cs0 = constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        UnivCs0, UnivCs, !VarSet, !EquivTypeInfo, !UsedModules),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        ExistCs0, ExistCs, !VarSet, !EquivTypeInfo, !UsedModules),
    Cs = constraints(UnivCs, ExistCs).

replace_in_prog_constraint_list(TypeEqvMap,
        !Constraints, !VarSet, !EquivTypeInfo) :-
    replace_in_prog_constraint_list_location(dont_record_sym_name_use,
        TypeEqvMap, !Constraints,
        !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraint_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        !Constraints, !VarSet, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl3(
        replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap),
        !Constraints, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_prog_constraint_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, prog_constraint::in, prog_constraint::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap,
        Constraint0, Constraint, !VarSet, !EquivTypeInfo, !UsedModules) :-
    Constraint0 = constraint(ClassName, ArgTypes0),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap,
        ArgTypes0, ArgTypes, _, _, !VarSet, !EquivTypeInfo, !UsedModules),
    Constraint = constraint(ClassName, ArgTypes).

%---------------------------------------------------------------------------%

:- pred replace_in_class_interface(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    list(class_decl)::in, list(class_decl)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_interface(MaybeRecord, TypeEqvMap, InstEqvMap,
        ClassInterface0, ClassInterface, !EquivTypeInfo, !UsedModules,
        !Specs) :-
    list.map_foldl3(
        replace_in_class_decl(MaybeRecord, TypeEqvMap, InstEqvMap),
        ClassInterface0, ClassInterface, !EquivTypeInfo, !UsedModules, !Specs).

:- pred replace_in_class_decl(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, class_decl::in, class_decl::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_decl(MaybeRecord, TypeEqvMap, InstEqvMap, Decl0, Decl,
        !EquivTypeInfo, !UsedModules, !Specs) :-
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(PredName, PredOrFunc,
            TypesAndModes0, WithType0, WithInst0, MaybeDetism0,
            TypeVarSet0, InstVarSet, ExistQVars, Purity,
            ClassContext0, Context),
        replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
            TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
            TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
            WithType0, WithType, WithInst0, WithInst,
            MaybeDetism0, MaybeDetism, !EquivTypeInfo, !UsedModules, NewSpecs),
        !:Specs = NewSpecs ++ !.Specs,
        PredOrFuncInfo = class_pred_or_func_info(PredName, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity,
            ClassContext, Context),
        Decl = class_decl_pred_or_func(PredOrFuncInfo)
    ;
        Decl0 = class_decl_mode(ModeInfo0),
        ModeInfo0 = class_mode_info(PredName, MaybePredOrFunc0, Modes0,
            WithInst0, MaybeDetism0, InstVarSet, Context),
        replace_in_pred_mode(MaybeRecord, InstEqvMap,
            PredName, list.length(Modes0), Context, mode_decl, ExtraModes,
            MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst,
            MaybeDetism0, MaybeDetism, !EquivTypeInfo, !UsedModules, NewSpecs),
        (
            ExtraModes = [],
            Modes = Modes0
        ;
            ExtraModes = [_ | _],
            Modes = Modes0 ++ ExtraModes
        ),
        !:Specs = NewSpecs ++ !.Specs,
        ModeInfo = class_mode_info(PredName, MaybePredOrFunc, Modes,
            WithInst, MaybeDetism, InstVarSet, Context),
        Decl = class_decl_mode(ModeInfo)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_subst(maybe_record_sym_name_use::in, type_eqv_map::in,
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(_MaybeRecord, _TypeEqvMap, [], [], !VarSet, !EquivTypeInfo,
        !UsedModules).
replace_in_subst(MaybeRecord, TypeEqvMap, [Var - Type0 | Subst0],
        [Var - Type | Subst], !VarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        Type0, Type, _, !VarSet, !EquivTypeInfo, !UsedModules),
    replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst, !VarSet,
        !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

replace_in_ctors(TypeEqvMap, !Ctors, !VarSet, !EquivTypeInfo) :-
    replace_in_ctors_location(dont_record_sym_name_use, TypeEqvMap,
        !Ctors, !VarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_ctors_location(maybe_record_sym_name_use::in,
    type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors, !VarSet,
        !EquivTypeInfo, !UsedModules) :-
    Ctors0 = one_or_more(HeadCtor0, TailCtors0),
    replace_in_ctor(MaybeRecord, TypeEqvMap, HeadCtor0, HeadCtor,
        !VarSet, !EquivTypeInfo, !UsedModules),
    list.map_foldl3(replace_in_ctor(MaybeRecord, TypeEqvMap),
        TailCtors0, TailCtors,
        !VarSet, !EquivTypeInfo, !UsedModules),
    Ctors = one_or_more(HeadCtor, TailCtors).

:- pred replace_in_ctor(maybe_record_sym_name_use::in, type_eqv_map::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(MaybeRecord, TypeEqvMap, Ctor0, Ctor,
        !VarSet, !EquivTypeInfo, !UsedModules) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, CtorName, CtorArgs0, Arity,
        Ctxt),
    replace_in_ctor_arg_list(MaybeRecord, TypeEqvMap,
        CtorArgs0, CtorArgs, _, !VarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        ExistConstraints0 = cons_exist_constraints(ExistQVars, Constraints0,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
            Constraints0, Constraints, !VarSet, !EquivTypeInfo, !UsedModules),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorName, CtorArgs, Arity,
        Ctxt).

%---------------------------------------------------------------------------%

:- pred replace_in_inst(maybe_record_sym_name_use::in, inst_eqv_map::in,
    mer_inst::in, mer_inst::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(MaybeRecord, InstEqvMap, Inst0, Inst,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_inst_location(MaybeRecord, InstEqvMap, set.init, Inst0, Inst,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_inst_location(maybe_record_sym_name_use::in,
    inst_eqv_map::in, set(inst_id)::in, mer_inst::in, mer_inst::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(MaybeRecord, InstEqvMap, ExpandedInstIds, Inst0, Inst,
        !EquivTypeInfo, !UsedModules) :-
    % XXX Need to record the used modules
    ( if Inst0 = defined_inst(user_inst(SymName, ArgInsts)) then
        InstId = inst_id(SymName, length(ArgInsts)),
        ( if
            set.member(InstId, ExpandedInstIds)
        then
            Inst = Inst0
        else if
            map.search(InstEqvMap, InstId, EqvInstBody),
            EqvInstBody = eqv_inst_body(_, EqvInstParams, EqvInst)
        then
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstIdItem = inst_id_to_item_name(InstId),
            record_expanded_item(item_id(inst_item, InstIdItem),
                !EquivTypeInfo),
            replace_in_inst_location(MaybeRecord, InstEqvMap,
                set.insert(ExpandedInstIds, InstId), Inst1, Inst,
                !EquivTypeInfo, !UsedModules)
        else
            Inst = Inst0
        )
    else
        Inst = Inst0
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_pred_type(maybe_record_sym_name_use::in, sym_name::in,
    pred_or_func::in, prog_context::in, type_eqv_map::in, inst_eqv_map::in,
    prog_constraints::in, prog_constraints::out,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
        TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, !TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        !MaybeDetism, !EquivTypeInfo, !UsedModules, !:Specs) :-
    replace_in_prog_constraints_location(MaybeRecord, TypeEqvMap,
        ClassContext0, ClassContext, !TypeVarSet,
        !EquivTypeInfo, !UsedModules),
    replace_in_types_and_modes(MaybeRecord, TypeEqvMap,
        TypesAndModes0, TypesAndModes1,
        !TypeVarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            WithType0, WithType, _, !TypeVarSet, !EquivTypeInfo, !UsedModules),
        ( if
            type_is_higher_order_details(WithType, _Purity, PredOrFunc,
                _EvalMethod, ExtraTypesPrime)
        then
            ExtraTypes = ExtraTypesPrime,
            !:Specs = []
        else
            ExtraTypes = [],
            Pieces1 = [words("In type declaration for"),
                p_or_f(PredOrFunc), qual_sym_name(PredName), suffix(":"), nl,
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

    replace_in_pred_mode(MaybeRecord, InstEqvMap,
        PredName, list.length(TypesAndModes0), Context, type_decl,
        ExtraModes, yes(PredOrFunc), _, MaybeWithInst0, _, !MaybeDetism,
        !EquivTypeInfo, !UsedModules, ModeSpecs),
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
            pair_extra_types_and_modes(ExtraTypes, ExtraModes,
                ExtraTypesAndModes, LeftOverExtraTypes, LeftOverExtraModes),
            (
                LeftOverExtraTypes = [],
                LeftOverExtraModes = []
            ;
                LeftOverExtraTypes = [],
                LeftOverExtraModes = [_ | _],
                list.length(ExtraTypes, NumExtraTypes),
                list.length(ExtraModes, NumExtraModes),
                Pieces2 = [words("In type declaration for"),
                    p_or_f(PredOrFunc), qual_sym_name(PredName),
                    suffix(":"), nl,
                    words("error: the `with_type` and `with_inst`"),
                    words("annotations are incompatible;"),
                    words("they specify"), int_fixed(NumExtraModes),
                    words(choose_number(ExtraModes, "mode", "modes")),
                    words("but only"), int_fixed(NumExtraTypes),
                    words(choose_number(ExtraTypes, "type.", "types.")), nl],
                Msg2 = simple_msg(Context, [always(Pieces2)]),
                Spec2 = error_spec(severity_error, phase_expand_types, [Msg2]),
                !:Specs = [Spec2 | !.Specs]
            ;
                LeftOverExtraTypes = [_ | _],
                LeftOverExtraModes = [],
                list.length(ExtraTypes, NumExtraTypes),
                list.length(ExtraModes, NumExtraModes),
                Pieces2 = [words("In type declaration for"),
                    p_or_f(PredOrFunc), qual_sym_name(PredName),
                    suffix(":"), nl,
                    words("error: the `with_type` and `with_inst`"),
                    words("annotations are incompatible;"),
                    words("they specify"), int_fixed(NumExtraTypes),
                    words(choose_number(ExtraTypes, "type", "types")),
                    words("but only"), int_fixed(NumExtraModes),
                    words(choose_number(ExtraModes, "mode.", "modes.")), nl],
                Msg2 = simple_msg(Context, [always(Pieces2)]),
                Spec2 = error_spec(severity_error, phase_expand_types, [Msg2]),
                !:Specs = [Spec2 | !.Specs]
            ;
                LeftOverExtraTypes = [_ | _],
                LeftOverExtraModes = [_ | _],
                % pair_extra_types_and_modes should have paired these up.
                unexpected($pred, "both types and modes left over")
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

:- pred pair_extra_types_and_modes(list(mer_type)::in, list(mer_mode)::in,
    list(type_and_mode)::out, list(mer_type)::out, list(mer_mode)::out) is det.

pair_extra_types_and_modes([], [], [], [], []).
pair_extra_types_and_modes(LeftOverTypes @ [_ | _], [], [], LeftOverTypes, []).
pair_extra_types_and_modes([], LeftOverModes @ [_ | _], [], [], LeftOverModes).
pair_extra_types_and_modes([Type | Types], [Mode | Modes],
        [type_and_mode(Type, Mode) | TypesAndModes],
        LeftOverTypes, LeftOverModes) :-
    pair_extra_types_and_modes(Types, Modes, TypesAndModes,
        LeftOverTypes, LeftOverModes).

:- pred replace_in_pred_mode(maybe_record_sym_name_use::in, inst_eqv_map::in,
    sym_name::in, arity::in, prog_context::in, pred_or_func_decl_type::in,
    list(mer_mode)::out,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_mode(MaybeRecord, InstEqvMap, PredName, OrigArity, Context,
        DeclType, ExtraModes, MaybePredOrFunc0, MaybePredOrFunc,
        MaybeWithInst0, MaybeWithInst, !MaybeDetism,
        !EquivTypeInfo, !UsedModules, Specs) :-
    (
        MaybeWithInst0 = yes(WithInst0),
        replace_in_inst(MaybeRecord, InstEqvMap, WithInst0, WithInst,
            !EquivTypeInfo, !UsedModules),
        ( if
            WithInst = ground(_, GroundInstInfo),
            GroundInstInfo = higher_order(HOInst),
            HOInst = pred_inst_info(PredOrFunc, ExtraModes0, _, DetPrime),
            ( MaybePredOrFunc0 = no
            ; MaybePredOrFunc0 = yes(PredOrFunc)
            )
        then
            !:MaybeDetism = yes(DetPrime),
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
        else
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
                PredOrFuncPieces ++ [qual_sym_name(PredName), suffix(":"), nl,
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

:- pred replace_in_types_and_modes(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_types_and_modes(MaybeRecord, TypeEqvMap,
        !TypeAndModes, !VarSet, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl3(replace_in_type_and_mode(MaybeRecord, TypeEqvMap),
        !TypeAndModes, !VarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_and_mode(maybe_record_sym_name_use::in,
    type_eqv_map::in, type_and_mode::in, type_and_mode::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_and_mode(MaybeRecord, TypeEqvMap, TypeAndMode0, TypeAndMode,
        !VarSet, !EquivTypeInfo, !UsedModules) :-
    (
        TypeAndMode0 = type_only(Type0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, !VarSet, !EquivTypeInfo, !UsedModules),
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode0 = type_and_mode(Type0, Mode),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, !VarSet, !EquivTypeInfo, !UsedModules),
        TypeAndMode = type_and_mode(Type, Mode)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_structure_sharing_domain(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in,
    structure_sharing_domain::in, structure_sharing_domain::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_domain(MaybeRecord, TypeEqvMap, TVarSet,
        SharingDomain0, SharingDomain, !EquivTypeInfo, !UsedModules) :-
    (
        ( SharingDomain0 = structure_sharing_bottom
        ; SharingDomain0 = structure_sharing_top(_)
        ),
        SharingDomain = SharingDomain0
    ;
        SharingDomain0 = structure_sharing_real(SharingPairs0),
        list.map_foldl2(
            replace_in_structure_sharing_pair(MaybeRecord, TypeEqvMap,
                TVarSet),
            SharingPairs0, SharingPairs, !EquivTypeInfo, !UsedModules),
        SharingDomain = structure_sharing_real(SharingPairs)
    ).

:- pred replace_in_structure_sharing_pair(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in,
    structure_sharing_pair::in, structure_sharing_pair::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(MaybeRecord, TypeEqvMap, TVarSet,
        SSA0 - SSB0, SSA - SSB, !EquivTypeInfo, !UsedModules) :-
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSA0, SSA,
        !EquivTypeInfo, !UsedModules),
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSB0, SSB,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_datastruct(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, datastruct::in,
    datastruct::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, DS0, DS,
        !EquivTypeInfo, !UsedModules) :-
    DS0 = selected_cel(Var, Sel0),
    list.map_foldl2(replace_in_unit_selector(MaybeRecord, TypeEqvMap, TVarSet),
        Sel0, Sel, !EquivTypeInfo, !UsedModules),
    DS = selected_cel(Var, Sel).

:- pred replace_in_unit_selector(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, unit_selector::in,
    unit_selector::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_unit_selector(MaybeRecord, TypeEqvMap, TVarSet, Sel0, Sel,
        !EquivTypeInfo, !UsedModules) :-
    (
        Sel0 = termsel(_, _),
        Sel = Sel0
    ;
        Sel0 = typesel(Type0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, TVarSet, _, !EquivTypeInfo, !UsedModules),
        Sel = typesel(Type)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_record_type_ctor_sym_name_use(maybe_record_sym_name_use::in,
    type_ctor::in, used_modules::in, used_modules::out) is det.

maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor, !UsedModules) :-
    (
        MaybeRecord = dont_record_sym_name_use
    ;
        MaybeRecord = record_sym_name_use(Visibility),
        TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
        record_sym_name_module_as_used(Visibility, TypeCtorSymName,
            !UsedModules)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%---------------------------------------------------------------------------%
