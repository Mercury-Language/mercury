%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module separates the different kinds of items in an augmented
% compilation unit.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

    % Items in the blocks of the parse tree are stored in the order in which
    % they appear in the files they were read in from; source files, interface
    % files and/or optimization files. Take these conmingled items and separate
    % them out by kind, i.e. return a separate list for each kind of item.
    %
    % How we add an item to the HLDS depends on what kind of section
    % it occurs in. In all cases, we need to know the status of the item
    % (e.g. whether it is defined in the current module, whether it is
    % imported or exported etc), and in some cases, we also need to know
    % whether appearances of the thing it defines elsewhere in the code
    % must be module qualified or not. We therefore pair each item with either
    % its item_mercury_status (the ims_lists below) or with its
    % item_mercury_status and need_qualifier flag (the sec_lists below).
    %
:- pred separate_items_in_aug_comp_unit(aug_compilation_unit::in,
    ims_list(list(item_avail))::out,
    sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::out,
    ims_list(item_inst_defn_info)::out,
    ims_list(item_mode_defn_info)::out,
    sec_list(item_pred_decl_info)::out,
    ims_list(item_mode_decl_info)::out,
    ims_list(item_promise_info)::out,
    sec_list(item_typeclass_info)::out,
    ims_list(item_instance_info)::out,
    ims_list(item_initialise_info)::out,
    ims_list(item_finalise_info)::out,
    sec_list(item_mutable_info)::out,
    list(item_foreign_import_module_info)::out,
    list(item_type_repn_info)::out,
    list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::out,
    ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_2)),
    ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_3)),
    ims_list(item_clause_info)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

separate_items_in_aug_comp_unit(AugCompUnit, ItemAvailLists,
        ItemTypeDefnsAbstract, ItemTypeDefnsMercury, ItemTypeDefnsForeign,
        ItemInstDefns, ItemModeDefns, ItemPredDecls, ItemModeDecls,
        ItemPromises, ItemTypeclasses, ItemInstances,
        ItemInitialises, ItemFinalises, ItemMutables,
        ItemFIMs, ItemTypeRepns, ItemForeignEnums, ItemForeignExportEnums,
        ItemPragmas2, ItemPragmas3, ItemClauses) :-
    AugCompUnit = aug_compilation_unit(_ModuleName, _ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),

    % To ensure tail recursion, we accumulate each list of items
    % in reverse order.

    some [!RevItemAvailLists,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses]
    (
        !:RevItemAvailLists = [],
        !:RevItemTypeDefnsAbstract = [],
        !:RevItemTypeDefnsMercury = [],
        !:RevItemTypeDefnsForeign = [],
        !:RevItemInstDefns = [],
        !:RevItemModeDefns = [],
        !:RevItemPredDecls = [],
        !:RevItemModeDecls = [],
        !:RevItemPromises = [],
        !:RevItemTypeclasses = [],
        !:RevItemInstances = [],
        !:RevItemInitialises = [],
        !:RevItemFinalises = [],
        !:RevItemMutables = [],
        !:RevItemFIMs = [],
        !:RevItemTypeRepns = [],
        !:RevItemForeignEnums = [],
        !:RevItemForeignExportEnums = [],
        !:RevItemPragmas2 = [],
        !:RevItemPragmas3 = [],
        !:RevItemClauses = [],

        separate_items_in_blocks(SrcItemBlocks,
            src_module_section_status,
            !RevItemAvailLists, !RevItemTypeDefnsAbstract,
            !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
            !RevItemInstDefns, !RevItemModeDefns,
            !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
            !RevItemTypeclasses, !RevItemInstances,
            !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
            !RevItemFIMs, !RevItemTypeRepns,
            !RevItemForeignEnums, !RevItemForeignExportEnums,
            !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),
        separate_items_in_blocks(DirectIntItemBlocks,
            int_module_section_status,
            !RevItemAvailLists, !RevItemTypeDefnsAbstract,
            !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
            !RevItemInstDefns, !RevItemModeDefns,
            !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
            !RevItemTypeclasses, !RevItemInstances,
            !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
            !RevItemFIMs, !RevItemTypeRepns,
            !RevItemForeignEnums, !RevItemForeignExportEnums,
            !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),
        separate_items_in_blocks(IndirectIntItemBlocks,
            int_module_section_status,
            !RevItemAvailLists, !RevItemTypeDefnsAbstract,
            !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
            !RevItemInstDefns, !RevItemModeDefns,
            !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
            !RevItemTypeclasses, !RevItemInstances,
            !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
            !RevItemFIMs, !RevItemTypeRepns,
            !RevItemForeignEnums, !RevItemForeignExportEnums,
            !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),
        separate_items_in_blocks(IntForOptItemBlocks,
            int_for_opt_module_section_status,
            !RevItemAvailLists, !RevItemTypeDefnsAbstract,
            !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
            !RevItemInstDefns, !RevItemModeDefns,
            !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
            !RevItemTypeclasses, !RevItemInstances,
            !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
            !RevItemFIMs, !RevItemTypeRepns,
            !RevItemForeignEnums, !RevItemForeignExportEnums,
            !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),
        separate_items_in_blocks(OptItemBlocks,
            opt_module_section_status,
            !RevItemAvailLists, !RevItemTypeDefnsAbstract,
            !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
            !RevItemInstDefns, !RevItemModeDefns,
            !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
            !RevItemTypeclasses, !RevItemInstances,
            !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
            !RevItemFIMs, !RevItemTypeRepns,
            !RevItemForeignEnums, !RevItemForeignExportEnums,
            !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),

        list.reverse(!.RevItemAvailLists, ItemAvailLists),
        list.reverse(!.RevItemTypeDefnsAbstract, ItemTypeDefnsAbstract),
        list.reverse(!.RevItemTypeDefnsMercury, ItemTypeDefnsMercury),
        list.reverse(!.RevItemTypeDefnsForeign, ItemTypeDefnsForeign),
        list.reverse(!.RevItemInstDefns, ItemInstDefns),
        list.reverse(!.RevItemModeDefns, ItemModeDefns),
        list.reverse(!.RevItemPredDecls, ItemPredDecls),
        list.reverse(!.RevItemModeDecls, ItemModeDecls),
        list.reverse(!.RevItemPromises, ItemPromises),
        list.reverse(!.RevItemTypeclasses, ItemTypeclasses),
        list.reverse(!.RevItemInstances, ItemInstances),
        list.reverse(!.RevItemInitialises, ItemInitialises),
        list.reverse(!.RevItemFinalises, ItemFinalises),
        list.reverse(!.RevItemMutables, ItemMutables),
        list.reverse(!.RevItemFIMs, ItemFIMs),
        list.reverse(!.RevItemTypeRepns, ItemTypeRepns),
        ItemForeignEnums = inst_preserving_reverse(!.RevItemForeignEnums),
        ItemForeignExportEnums =
            inst_preserving_reverse(!.RevItemForeignExportEnums),
        ItemPragmas2 = inst_preserving_reverse(!.RevItemPragmas2),
        ItemPragmas3 = inst_preserving_reverse(!.RevItemPragmas3),
        list.reverse(!.RevItemClauses, ItemClauses)
    ).

%---------------------------------------------------------------------------%

    % Return the status and qualifier need appropriate for items
    % in the given kind of block.
    %
:- pred src_module_section_status(src_module_section::in,
    sec_info::out) is det.
:- pred int_module_section_status(int_module_section::in,
    sec_info::out) is det.
:- pred opt_module_section_status(opt_module_section::in,
    sec_info::out) is det.
:- pred int_for_opt_module_section_status(int_for_opt_module_section::in,
    sec_info::out) is det.

src_module_section_status(SrcSection, SectionInfo) :-
    (
        SrcSection = sms_interface,
        Status = item_defined_in_this_module(item_export_anywhere),
        NeedQual = may_be_unqualified
    ;
        SrcSection = sms_implementation,
        Status = item_defined_in_this_module(item_export_nowhere),
        NeedQual = may_be_unqualified
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        Status = item_defined_in_this_module(item_export_only_submodules),
        NeedQual = may_be_unqualified
    ),
    SectionInfo = sec_info(Status, NeedQual).

int_module_section_status(IntSection, SectionInfo) :-
    (
        IntSection = ims_imported_or_used(_ModuleName, _IntFileKind,
            ImportLocn, ImportedOrUsed),
        Status =
            item_defined_in_other_module(item_import_int_concrete(ImportLocn)),
        (
            ( ImportedOrUsed = iou_imported
            ; ImportedOrUsed = iou_used_and_imported
            ),
            NeedQual = may_be_unqualified
        ;
            ImportedOrUsed = iou_used,
            NeedQual = must_be_qualified
        )
    ;
        IntSection = ims_abstract_imported(_ModuleName, _IntFileKind),
        Status = item_defined_in_other_module(item_import_int_abstract),
        NeedQual = must_be_qualified
    ),
    SectionInfo = sec_info(Status, NeedQual).

opt_module_section_status(OptSection, SectionInfo) :-
    (
        OptSection = oms_opt_imported(_ModuleName, _OptFileKind),
        Status = item_defined_in_other_module(item_import_opt_int),
        NeedQual = must_be_qualified
    ),
    SectionInfo = sec_info(Status, NeedQual).

int_for_opt_module_section_status(IntForOptSection, SectionInfo) :-
    (
        IntForOptSection = ioms_opt_imported(_ModuleName, _OptFileKind),
        Status = item_defined_in_other_module(item_import_opt_int),
        NeedQual = must_be_qualified
    ),
    SectionInfo = sec_info(Status, NeedQual).

%---------------------------------------------------------------------------%

:- pred separate_items_in_blocks(list(item_block(MS))::in,
    pred(MS, sec_info)::in(pred(in, out) is det),
    ims_list(list(item_avail))::in, ims_list(list(item_avail))::out,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    ims_list(item_inst_defn_info)::in, ims_list(item_inst_defn_info)::out,
    ims_list(item_mode_defn_info)::in, ims_list(item_mode_defn_info)::out,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    ims_list(item_mode_decl_info)::in, ims_list(item_mode_decl_info)::out,
    ims_list(item_promise_info)::in, ims_list(item_promise_info)::out,
    sec_list(item_typeclass_info)::in, sec_list(item_typeclass_info)::out,
    ims_list(item_instance_info)::in, ims_list(item_instance_info)::out,
    ims_list(item_initialise_info)::in, ims_list(item_initialise_info)::out,
    ims_list(item_finalise_info)::in, ims_list(item_finalise_info)::out,
    sec_list(item_mutable_info)::in, sec_list(item_mutable_info)::out,
    list(item_foreign_import_module_info)::in,
        list(item_foreign_import_module_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::in,
        list(item_foreign_export_enum_info)::out,
    ims_list(item_pragma_info)::in(list_skel(ims_pragma_pass_2)),
        ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_2)),
    ims_list(item_pragma_info)::in(list_skel(ims_pragma_pass_3)),
        ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_3)),
    ims_list(item_clause_info)::in, ims_list(item_clause_info)::out) is det.

separate_items_in_blocks([], _MakeSectionInfo,
        !RevItemAvailLists,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses).
separate_items_in_blocks([ItemBlock | ItemBlocks], MakeSectionInfo,
        !RevItemAvailLists,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses) :-
    ItemBlock = item_block(_, Section, _, _Incls, Avails, Items),
    MakeSectionInfo(Section, SectionInfo),
    SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
    AvailSectionItem = ims_item(ItemMercuryStatus, Avails),
    !:RevItemAvailLists = [AvailSectionItem | !.RevItemAvailLists],
    separate_items(Items, SectionInfo,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses),
    separate_items_in_blocks(ItemBlocks, MakeSectionInfo,
        !RevItemAvailLists, !RevItemTypeDefnsAbstract,
        !RevItemTypeDefnsMercury, !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses).

:- pred separate_items(list(item)::in, sec_info::in,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::in, sec_list(item_type_defn_info)::out,
    ims_list(item_inst_defn_info)::in, ims_list(item_inst_defn_info)::out,
    ims_list(item_mode_defn_info)::in, ims_list(item_mode_defn_info)::out,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    ims_list(item_mode_decl_info)::in, ims_list(item_mode_decl_info)::out,
    ims_list(item_promise_info)::in, ims_list(item_promise_info)::out,
    sec_list(item_typeclass_info)::in, sec_list(item_typeclass_info)::out,
    ims_list(item_instance_info)::in, ims_list(item_instance_info)::out,
    ims_list(item_initialise_info)::in, ims_list(item_initialise_info)::out,
    ims_list(item_finalise_info)::in, ims_list(item_finalise_info)::out,
    sec_list(item_mutable_info)::in, sec_list(item_mutable_info)::out,
    list(item_foreign_import_module_info)::in,
        list(item_foreign_import_module_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::in,
        list(item_foreign_export_enum_info)::out,
    ims_list(item_pragma_info)::in(list_skel(ims_pragma_pass_2)),
        ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_2)),
    ims_list(item_pragma_info)::in(list_skel(ims_pragma_pass_3)),
        ims_list(item_pragma_info)::out(list_skel(ims_pragma_pass_3)),
    ims_list(item_clause_info)::in, ims_list(item_clause_info)::out) is det.

separate_items([], _SectionInfo,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses).
separate_items([Item | Items], SectionInfo,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses) :-
    (
        Item = item_clause(ItemClauseInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        ClauseStatusItem = ims_item(ItemMercuryStatus, ItemClauseInfo),
        !:RevItemClauses = [ClauseStatusItem |  !.RevItemClauses]
    ;
        Item = item_type_defn(ItemTypeDefnInfo),
        TypeDefnSectionItem = sec_item(SectionInfo, ItemTypeDefnInfo),
        ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
        (
            TypeDefn = parse_tree_abstract_type(_),
            !:RevItemTypeDefnsAbstract =
                [TypeDefnSectionItem | !.RevItemTypeDefnsAbstract]
        ;
            ( TypeDefn = parse_tree_du_type(_)
            ; TypeDefn = parse_tree_eqv_type(_)
            ; TypeDefn = parse_tree_solver_type(_)
            ),
            !:RevItemTypeDefnsMercury =
                [TypeDefnSectionItem | !.RevItemTypeDefnsMercury]
        ;
            TypeDefn = parse_tree_foreign_type(_),
            !:RevItemTypeDefnsForeign =
                [TypeDefnSectionItem | !.RevItemTypeDefnsForeign]
        )
    ;
        Item = item_inst_defn(ItemInstDefnInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        InstDefnStatusItem = ims_item(ItemMercuryStatus, ItemInstDefnInfo),
        !:RevItemInstDefns = [InstDefnStatusItem | !.RevItemInstDefns]
    ;
        Item = item_mode_defn(ItemModeDefnInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        ModeDefnStatusItem = ims_item(ItemMercuryStatus, ItemModeDefnInfo),
        !:RevItemModeDefns = [ModeDefnStatusItem | !.RevItemModeDefns]
    ;
        Item = item_pred_decl(ItemPredDeclInfo),
        PredDeclSectionItem = sec_item(SectionInfo, ItemPredDeclInfo),
        !:RevItemPredDecls = [PredDeclSectionItem | !.RevItemPredDecls]
    ;
        Item = item_mode_decl(ItemModeDeclInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        ModeDeclStatusItem = ims_item(ItemMercuryStatus, ItemModeDeclInfo),
        !:RevItemModeDecls = [ModeDeclStatusItem | !.RevItemModeDecls]
    ;
        Item = item_promise(ItemPromiseInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        PromiseStatusItem = ims_item(ItemMercuryStatus, ItemPromiseInfo),
        !:RevItemPromises = [PromiseStatusItem | !.RevItemPromises]
    ;
        Item = item_typeclass(ItemTypeclassInfo),
        TypeclassSectionItem = sec_item(SectionInfo, ItemTypeclassInfo),
        !:RevItemTypeclasses = [TypeclassSectionItem | !.RevItemTypeclasses]
    ;
        Item = item_instance(ItemInstanceInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        InstanceStatusItem = ims_item(ItemMercuryStatus, ItemInstanceInfo),
        !:RevItemInstances = [InstanceStatusItem | !.RevItemInstances]
    ;
        Item = item_initialise(ItemInitialiseInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        InitialiseStatusItem = ims_item(ItemMercuryStatus, ItemInitialiseInfo),
        !:RevItemInitialises = [InitialiseStatusItem | !.RevItemInitialises]
    ;
        Item = item_finalise(ItemFinaliseInfo),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        FinaliseStatusItem = ims_item(ItemMercuryStatus, ItemFinaliseInfo),
        !:RevItemFinalises = [FinaliseStatusItem | !.RevItemFinalises]
    ;
        Item = item_mutable(ItemMutableInfo),
        MutableSectionItem = sec_item(SectionInfo, ItemMutableInfo),
        !:RevItemMutables = [MutableSectionItem | !.RevItemMutables]
    ;
        Item = item_foreign_import_module(ItemFIM),
        !:RevItemFIMs = [ItemFIM | !.RevItemFIMs]
    ;
        Item = item_type_repn(ItemTypeRepnInfo),
        !:RevItemTypeRepns = [ItemTypeRepnInfo | !.RevItemTypeRepns]
    ;
        Item = item_pragma(ItemPragmaInfo0),
        % Note that the distinction between ItemPragmaInfo0 and ItemPragmaInfo
        % is there only to make up for the lack of alias tracking in our
        % current mode checker. It does not know what kind of pragma
        % is inside ItemPragmaInfo0, but (after it knows the value of
        % PragmaType) it *will* know what kind of pragma is inside
        % ItemPragmaInfo.
        ItemPragmaInfo0 = item_pragma_info(PragmaType, MaybeAttrs,
            Context, SeqNum),
        SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
        (
            PragmaType = pragma_foreign_enum(PragmaForeignEnum),
            expect(unify(MaybeAttrs, item_origin_user), $pred,
                "foreign_enum MaybeAttrs != item_origin_user"),
            ItemForeignEnumInfo = item_foreign_enum_info(PragmaForeignEnum,
                ItemMercuryStatus, Context, SeqNum),
            !:RevItemForeignEnums =
                [ItemForeignEnumInfo | !.RevItemForeignEnums]
        ;
            PragmaType = pragma_foreign_export_enum(PragmaForeignExportEnum),
            expect(unify(MaybeAttrs, item_origin_user), $pred,
                "foreign_export_enum MaybeAttrs != item_origin_user"),
            ItemForeignExportEnumInfo =
                item_foreign_export_enum_info(PragmaForeignExportEnum,
                    ItemMercuryStatus, Context, SeqNum),
            !:RevItemForeignExportEnums =
                [ItemForeignExportEnumInfo | !.RevItemForeignExportEnums]
        ;
            ( PragmaType = pragma_foreign_decl(_)
            ; PragmaType = pragma_foreign_code(_)
            ; PragmaType = pragma_external_proc(_)

            ; PragmaType = pragma_inline(_)
            ; PragmaType = pragma_no_inline(_)
            ; PragmaType = pragma_consider_used(_)

            ; PragmaType = pragma_unused_args(_)
            ; PragmaType = pragma_exceptions(_)
            ; PragmaType = pragma_trailing_info(_)
            ; PragmaType = pragma_mm_tabling_info(_)
            ; PragmaType = pragma_obsolete(_)
            ; PragmaType = pragma_no_detism_warning(_)
            ; PragmaType = pragma_require_tail_recursion(_)

            ; PragmaType = pragma_promise_eqv_clauses(_)
            ; PragmaType = pragma_promise_pure(_)
            ; PragmaType = pragma_promise_semipure(_)

            ; PragmaType = pragma_terminates(_)
            ; PragmaType = pragma_does_not_terminate(_)
            ; PragmaType = pragma_check_termination(_)
            ; PragmaType = pragma_mode_check_clauses(_)

            ; PragmaType = pragma_require_feature_set(_)
            ),
            ItemPragmaInfo = item_pragma_info(PragmaType, MaybeAttrs,
                Context, SeqNum),
            Pragma2StatusItem = ims_item(ItemMercuryStatus, ItemPragmaInfo),
            !:RevItemPragmas2 = [Pragma2StatusItem | !.RevItemPragmas2]
        ;
            ( PragmaType = pragma_foreign_proc(_)
            ; PragmaType = pragma_type_spec(_)
            ; PragmaType = pragma_tabled(_)
            ; PragmaType = pragma_fact_table(_)

            ; PragmaType = pragma_oisu(_)

            ; PragmaType = pragma_foreign_proc_export(_)
            ; PragmaType = pragma_termination_info(_)
            ; PragmaType = pragma_termination2_info(_)

            ; PragmaType = pragma_structure_sharing(_)
            ; PragmaType = pragma_structure_reuse(_)
            ),
            ItemPragmaInfo = item_pragma_info(PragmaType, MaybeAttrs,
                Context, SeqNum),
            Pragma3StatusItem = ims_item(ItemMercuryStatus, ItemPragmaInfo),
            !:RevItemPragmas3 = [Pragma3StatusItem | !.RevItemPragmas3]
        )
    ),
    separate_items(Items, SectionInfo,
        !RevItemTypeDefnsAbstract, !RevItemTypeDefnsMercury,
        !RevItemTypeDefnsForeign,
        !RevItemInstDefns, !RevItemModeDefns,
        !RevItemPredDecls, !RevItemModeDecls, !RevItemPromises,
        !RevItemTypeclasses, !RevItemInstances,
        !RevItemInitialises, !RevItemFinalises, !RevItemMutables,
        !RevItemFIMs, !RevItemTypeRepns,
        !RevItemForeignEnums, !RevItemForeignExportEnums,
        !RevItemPragmas2, !RevItemPragmas3, !RevItemClauses).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
%---------------------------------------------------------------------------%
