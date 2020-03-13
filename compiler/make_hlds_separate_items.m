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
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type ims_tuple_list(T) == list(ims_tuple_item(T)).
:- type ims_tuple_item(T) == {item_mercury_status, T}.

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
    % XXX CLEANUP We should return several kinds of items as maps,
    % not as lists. Specifically,
    %
    % - we should return type definitions as a type_ctor_checked_map,
    %   or at least as a type_ctor_defn_map, in both cases with additional
    %   section information in the values;
    % - we should return inst definitions as an inst_ctor_defn_map
    %   extended with status information;
    % - we should return mode definitions as a mode_ctor_defn_map
    %   extended with status information;
    % - we should return type representation info as a map from type_ctor
    %   to item_type_repn_info.
    %
    % Several of the parse trees kinds that we process here already have
    % this information in the map form. We flatten these maps to lists
    % only to minimize the complexity of the diff that radically rewrites
    % this module and the associated code.
    %
    % XXX CLEANUP We currently convert values of type read_whyN
    % to various item_block kinds, and then transform those item_block
    % kinds to item_mercury_status values. We should do this in one step,
    % but we keep the two step process to make the review of this diff easier.
    %
:- pred separate_items_in_aug_comp_unit(aug_compilation_unit::in,
    ims_list(list(item_avail))::out,
    list(item_fim)::out,
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
    list(item_type_repn_info)::out,
    ims_tuple_list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::out,
    ims_list(item_decl_pragma_info)::out,
    ims_list(item_impl_pragma_info)::out,
    list(item_generated_pragma_info)::out,
    ims_list(item_clause_info)::out,
    set(pf_sym_name_and_arity)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.convert_parse_tree. % XXX Undesirable dependency.
:- import_module parse_tree.file_kind.          % XXX Undesirable dependency.
:- import_module parse_tree.item_util.

%---------------------------------------------------------------------------%

    % Return the status and qualifier need appropriate for items
    % in the given kind of block.
    %
:- pred src_module_section_status(src_module_section::in,
    sec_info::out) is det.
:- pred int_module_section_status(int_module_section::in,
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

int_for_opt_module_section_status(IntForOptSection, SectionInfo) :-
    (
        IntForOptSection = ioms_opt_imported(_ModuleName, _OptFileKind),
        Status = item_defined_in_other_module(item_import_opt_int),
        NeedQual = must_be_qualified
    ),
    SectionInfo = sec_info(Status, NeedQual).

%---------------------------------------------------------------------------%

:- type item_accumulator
    --->    item_accumulator(
                ia_avails           :: ims_list(list(item_avail)),
                ia_fims             :: list(item_fim),
                ia_type_defns_abs   :: sec_list(item_type_defn_info),
                ia_type_defns_mer   :: sec_list(item_type_defn_info),
                ia_type_defns_for   :: sec_list(item_type_defn_info),
                ia_inst_defns       :: ims_list(item_inst_defn_info),
                ia_mode_defns       :: ims_list(item_mode_defn_info),
                ia_typeclasses      :: sec_list(item_typeclass_info),
                ia_instances        :: ims_list(item_instance_info),
                ia_pred_decls       :: sec_list(item_pred_decl_info),
                ia_mode_decls       :: ims_list(item_mode_decl_info),
                ia_clauses          :: ims_list(item_clause_info),
                ia_foreign_enums    :: ims_tuple_list(item_foreign_enum_info),
                ia_fees             :: list(item_foreign_export_enum_info),
                ia_decl_pragmas     :: ims_list(item_decl_pragma_info),
                ia_impl_pragmas     :: ims_list(item_impl_pragma_info),
                ia_gen_pragmas      :: list(item_generated_pragma_info),
                ia_promises         :: ims_list(item_promise_info),
                ia_initialises      :: ims_list(item_initialise_info),
                ia_finalises        :: ims_list(item_finalise_info),
                ia_mutables         :: sec_list(item_mutable_info),
                ia_type_repns       :: list(item_type_repn_info)
            ).

separate_items_in_aug_comp_unit(AugCompUnit, Avails, FIMs,
        TypeDefnsAbstract, TypeDefnsMercury, TypeDefnsForeign,
        InstDefns, ModeDefns, PredDecls, ModeDecls,
        Promises, Typeclasses, Instances,
        Initialises, Finalises, Mutables,
        TypeRepns, ForeignEnums, ForeignExportEnums,
        PragmasDecl, PragmasImpl, PragmasGen, Clauses, IntBadPreds) :-
    AugCompUnit = aug_compilation_unit(_ModuleName, _ModuleNameContext,
        _ModuleVersionNumbers, ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs),

    % We start with an empty list for each kind of item.
    % Then for each file we have read in, in reverse order,
    % we prepend all the items of that kind in that file to the list.
    % The final result should be a list of all the items of that kind
    % in the original forward order.
    %
    % For the item kinds that we wrap in a sec_item or ims_item
    % (which is most kinds of items), we use acc_sec_list and acc_ims_list
    % to do the wrapping up and the prepending at the same time, which
    % keeps the amount of memory we allocate linear in the number of items
    % while preserving tail recursion.
    %
    % An even better approach could be to wrap up with the section_info
    % or the item_mercury_status not a single item of a given kind,
    % but a list of items of a given kind, as we already do for item_avails.
    % This would have three advantages.
    %
    % - First, it would allow us to avoid the traversal that we currently do
    %   to do the wrapping.
    % - Second, it would reduce the amount of memory we need to allocate.
    % - Third, it would naturally break up potentially long lists into chunks,
    %   which should reduce any problems with non-tail-recursive traversals.
    %
    % The one drawback is minor: the need for an inner loop for processing
    % each kind of item.
    %
    % XXX ITEM_LIST PlainOpts and TransOpts have separate lists of
    % the different kinds of pragmas they may contain, but they have to
    % wrap them up as decl_pragmas or generated_pragmas before adding them
    % to the accumulator. This should not be necessary.
    some [!Acc] (
        !:Acc = item_accumulator([], [], [], [], [], [], [], [], [],
            [], [], [], [], [], [], [], [], [], [], [], [], []),
        map.foldl_values(acc_int_for_opt_spec, IntForOptSpecs, !Acc),
        map.foldl_values(acc_parse_tree_trans_opt, TransOpts, !Acc),
        map.foldl_values(acc_parse_tree_plain_opt, PlainOpts, !Acc),
        map.foldl_values(acc_indirect_int_spec, IndirectIntSpecs, !Acc),
        map.foldl_values(acc_direct_int_spec, DirectIntSpecs, !Acc),
        map.foldl_values(acc_ancestor_int_spec, AncestorIntSpecs, !Acc),
        acc_parse_tree_module_src(ParseTreeModuleSrc, !Acc),
        !.Acc = item_accumulator(Avails, FIMs,
            TypeDefnsAbstract, TypeDefnsMercury, TypeDefnsForeign,
            InstDefns, ModeDefns, Typeclasses, Instances,
            PredDecls, ModeDecls, Clauses, ForeignEnums, ForeignExportEnums,
            PragmasDecl, PragmasImpl, PragmasGen, Promises,
            Initialises, Finalises, Mutables, TypeRepns)
    ),
    IntBadPreds = ParseTreeModuleSrc ^ ptms_int_bad_clauses.

%---------------------------------------------------------------------------%

:- pred acc_ancestor_int_spec(ancestor_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_ancestor_int_spec(AncestorIntSpec, !Acc) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
    acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc).

:- pred acc_direct_int_spec(direct_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_direct_int_spec(DirectIntSpec, !Acc) :-
    (
        DirectIntSpec = direct_int1(ParseTreeInt1, ReadWhy1),
        acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc)
    ;
        DirectIntSpec = direct_int3(ParseTreeInt3, ReadWhy3),
        acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc)
    ).

:- pred acc_indirect_int_spec(indirect_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_indirect_int_spec(IndirectIntSpec, !Acc) :-
    (
        IndirectIntSpec = indirect_int2(ParseTreeInt2, ReadWhy2),
        acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc)
    ;
        IndirectIntSpec = indirect_int3(ParseTreeInt3, ReadWhy3),
        acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc)
    ).

:- pred acc_int_for_opt_spec(int_for_opt_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_int_for_opt_spec(IntForOptSpec, !Acc) :-
    (
        IntForOptSpec = for_opt_int0(ParseTreeInt0, ReadWhy0),
        acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc)
    ;
        IntForOptSpec = for_opt_int1(ParseTreeInt1, ReadWhy1),
        acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc)
    ;
        IntForOptSpec = for_opt_int2(ParseTreeInt2, ReadWhy2),
        acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc)
    ).

%---------------------------------------------------------------------------%

:- pred acc_parse_tree_module_src(parse_tree_module_src::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_module_src(ParseTreeModuleSrc, !Acc) :-
    % XXX CLEANUP *If* we could reply on _MaybeImplicitFIMLangs
    % having been filled in by now, which unfortunately we can't,
    % we could include the FIMs it corresponds to in AccFIMs,
    % which should allow us to delete some code elsewhere in the compiler
    % that now has the job of computing the set of FIMs implicitly needed
    % by the code of this module.
    ParseTreeModuleSrc = parse_tree_module_src(_ModuleName, _ModuleNameContext,
        _IntInclMap, _ImpInclMap, InclMap,
        _IntImportMap, _IntUseMap, _ImpImportMap, _ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, _MaybeImplicitFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsFor,
        IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls,
        IntForeignExportEnums, IntDeclPragmas, IntPromises, _IntBadPreds,

        SubTypeDefnsAbs, SubTypeDefnsMer, SubTypeDefnsFor,
        SubInstDefns, SubModeDefns, SubTypeClasses, SubInstances,
        SubPredDecls, SubModeDecls, ImpClauses,
        SubForeignEnums, ImpForeignExportEnums,
        SubDeclPragmas, ImpImplPragmas, SubPromises,
        ImpInitialises, ImpFinalises, SubMutables),

    src_module_section_status(sms_interface, IntSectionInfo),
    src_module_section_status(sms_implementation, ImpSectionInfo),
    IntSectionInfo = sec_info(IntItemMercuryStatus, _IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, _ImpNeedQual),
    ( if map.is_empty(InclMap) then
        % There are no submodules to export stuff to.
        SubSectionInfo = ImpSectionInfo,
        SubItemMercuryStatus = ImpItemMercuryStatus
    else
        src_module_section_status(sms_impl_but_exported_to_submodules,
            SubSectionInfo),
        SubSectionInfo = sec_info(SubItemMercuryStatus, _SubNeedQual)
    ),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccClauses0 = !.Acc ^ ia_clauses,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccForeignExportEnums0 = !.Acc ^ ia_fees,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccImplPragmas0 = !.Acc ^ ia_impl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,
    AccInitialises0 = !.Acc ^ ia_initialises,
    AccFinalises0 = !.Acc ^ ia_finalises,
    AccMutables0 = !.Acc ^ ia_mutables,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails0, AccAvails1),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, map.keys(IntFIMSpecMap)),
    ImpFIMs = list.map(fim_spec_to_item, map.keys(ImpFIMSpecMap)),
    AccFIMs = IntFIMs ++ ImpFIMs ++ AccFIMs0,
    acc_sec_list(SubSectionInfo, SubTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(SubSectionInfo, SubTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(SubSectionInfo, SubTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    acc_ims_list(SubItemMercuryStatus, SubInstDefns,
        AccInstDefns0, AccInstDefns1),
    acc_ims_list(IntItemMercuryStatus, IntInstDefns,
        AccInstDefns1, AccInstDefns),
    acc_ims_list(SubItemMercuryStatus, SubModeDefns,
        AccModeDefns0, AccModeDefns1),
    acc_ims_list(IntItemMercuryStatus, IntModeDefns,
        AccModeDefns1, AccModeDefns),
    acc_sec_list(SubSectionInfo, SubTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(SubItemMercuryStatus, SubInstances,
        AccInstances0, AccInstances1),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances1, AccInstances),
    acc_sec_list(SubSectionInfo, SubPredDecls, AccPredDecls0, AccPredDecls1),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls1, AccPredDecls),
    acc_ims_list(SubItemMercuryStatus, SubModeDecls,
        AccModeDecls0, AccModeDecls1),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls1, AccModeDecls),
    acc_ims_list(ImpItemMercuryStatus, ImpClauses,
        AccClauses0, AccClauses),
    acc_ims_tuple_list(SubItemMercuryStatus, SubForeignEnums,
        AccForeignEnums0, AccForeignEnums),
    AccForeignExportEnums =
        IntForeignExportEnums ++ ImpForeignExportEnums ++
        AccForeignExportEnums0,
    acc_ims_list(SubItemMercuryStatus, SubDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas1),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas1, AccDeclPragmas),
    acc_ims_list(SubItemMercuryStatus, ImpImplPragmas,
        AccImplPragmas0, AccImplPragmas),
    acc_ims_list(SubItemMercuryStatus, SubPromises,
        AccPromises0, AccPromises1),
    acc_ims_list(IntItemMercuryStatus, IntPromises,
        AccPromises1, AccPromises),
    acc_ims_list(ImpItemMercuryStatus, ImpInitialises,
        AccInitialises0, AccInitialises),
    acc_ims_list(ImpItemMercuryStatus, ImpFinalises,
        AccFinalises0, AccFinalises),
    acc_sec_list(SubSectionInfo, SubMutables, AccMutables0, AccMutables),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_clauses := AccClauses,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_fees := AccForeignExportEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_impl_pragmas := AccImplPragmas,
    !Acc ^ ia_promises := AccPromises,
    !Acc ^ ia_initialises := AccInitialises,
    !Acc ^ ia_finalises := AccFinalises,
    !Acc ^ ia_mutables := AccMutables.

%---------------------%

:- pred acc_parse_tree_int0(parse_tree_int0::in,
    read_why_int0::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc) :-
    % XXX ITEM_LIST Compute {Int,Imp}Section and {Int,Imp}ItemMercuryStatus
    % directly from ReadWhy0.
    (
        ReadWhy0 = rwi0_section,
        MakeIntSection =
            make_ims_imported(import_locn_ancestor_int0_interface),
        MakeImpSection =
            make_ims_imported(import_locn_ancestor_int0_implementation),
        IntSection = MakeIntSection(ModuleName, ifk_int0),
        ImpSection = MakeImpSection(ModuleName, ifk_int0),
        int_module_section_status(IntSection, IntSectionInfo),
        int_module_section_status(ImpSection, ImpSectionInfo)
    ;
        ReadWhy0 = rwi0_opt,
        MakeIntSection = make_ioms_opt_imported,
        MakeImpSection = make_ioms_opt_imported,
        IntOptSection = MakeIntSection(ModuleName, ifk_int0),
        ImpOptSection = MakeImpSection(ModuleName, ifk_int0),
        int_for_opt_module_section_status(IntOptSection, IntSectionInfo),
        int_for_opt_module_section_status(ImpOptSection, ImpSectionInfo)
    ),
    IntSectionInfo = sec_info(IntItemMercuryStatus, _IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, _ImpNeedQual),

    ParseTreeInt0 = parse_tree_int0(ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntImportMap, _IntUseMap, _ImpImportMap, _ImpUseMap, ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpDeclPragmas, ImpPromises),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails0, AccAvails1),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = IntFIMs ++ ImpFIMs ++ AccFIMs0,
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    IntInstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    ImpInstDefns = inst_ctor_defn_map_to_inst_defns(ImpInstDefnMap),
    acc_ims_list(ImpItemMercuryStatus, ImpInstDefns,
        AccInstDefns0, AccInstDefns1),
    acc_ims_list(IntItemMercuryStatus, IntInstDefns,
        AccInstDefns1, AccInstDefns),
    IntModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    ImpModeDefns = mode_ctor_defn_map_to_mode_defns(ImpModeDefnMap),
    acc_ims_list(ImpItemMercuryStatus, ImpModeDefns,
        AccModeDefns0, AccModeDefns1),
    acc_ims_list(IntItemMercuryStatus, IntModeDefns,
        AccModeDefns1, AccModeDefns),
    acc_sec_list(ImpSectionInfo, ImpTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(ImpItemMercuryStatus, ImpInstances,
        AccInstances0, AccInstances1),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances1, AccInstances),
    acc_sec_list(ImpSectionInfo, ImpPredDecls, AccPredDecls0, AccPredDecls1),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls1, AccPredDecls),
    acc_ims_list(ImpItemMercuryStatus, ImpModeDecls,
        AccModeDecls0, AccModeDecls1),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls1, AccModeDecls),
    acc_ims_tuple_list(ImpItemMercuryStatus,
        cjcse_map_to_list(ImpForeignEnumMap),
        AccForeignEnums0, AccForeignEnums1),
    acc_ims_tuple_list(IntItemMercuryStatus,
        cjcse_map_to_list(IntForeignEnumMap),
        AccForeignEnums1, AccForeignEnums),
    acc_ims_list(ImpItemMercuryStatus, ImpDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas1),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas1, AccDeclPragmas),
    acc_ims_list(ImpItemMercuryStatus, ImpPromises,
        AccPromises0, AccPromises1),
    acc_ims_list(IntItemMercuryStatus, IntPromises,
        AccPromises1, AccPromises),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_promises := AccPromises.

%---------------------%

:- pred acc_parse_tree_int1(parse_tree_int1::in,
    read_why_int1::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc) :-
    % XXX ITEM_LIST Compute {Int,Imp}Section and {Int,Imp}ItemMercuryStatus
    % directly from ReadWhy1.
    (
        (
            ReadWhy1 = rwi1_int_import,
            MakeIntSection = make_ims_imported(import_locn_interface)
        ;
            ReadWhy1 = rwi1_int_use,
            MakeIntSection = make_ims_used(import_locn_interface)
        ;
            ReadWhy1 = rwi1_imp_import,
            MakeIntSection = make_ims_imported(import_locn_implementation)
        ;
            ReadWhy1 = rwi1_imp_use,
            MakeIntSection = make_ims_used(import_locn_implementation)
        ;
            ReadWhy1 = rwi1_int_use_imp_import,
            MakeIntSection = make_ims_used_and_imported(import_locn_interface)
        ),
        MakeImpSection = make_ims_abstract_imported,
        IntSection = MakeIntSection(ModuleName, ifk_int1),
        ImpSection = MakeImpSection(ModuleName, ifk_int1),
        int_module_section_status(IntSection, IntSectionInfo),
        int_module_section_status(ImpSection, ImpSectionInfo)
    ;
        ReadWhy1 = rwi1_opt,
        MakeIntSection = make_ioms_opt_imported,
        MakeImpSection = make_ioms_opt_imported,
        IntOptSection = MakeIntSection(ModuleName, ifk_int1),
        ImpOptSection = MakeImpSection(ModuleName, ifk_int1),
        int_for_opt_module_section_status(IntOptSection, IntSectionInfo),
        int_for_opt_module_section_status(ImpOptSection, ImpSectionInfo)
    ),
    IntSectionInfo = sec_info(IntItemMercuryStatus, _IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, _ImpNeedQual),

    ParseTreeInt1 = parse_tree_int1(ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntUseMap, _ImpUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails0, AccAvails1),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = IntFIMs ++ ImpFIMs ++ AccFIMs0,
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(ImpSectionInfo, ImpTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls0, AccPredDecls),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls0, AccModeDecls),
    acc_ims_tuple_list(ImpItemMercuryStatus,
        cjcse_map_to_list(ImpForeignEnumMap),
        AccForeignEnums0, AccForeignEnums1),
    acc_ims_tuple_list(IntItemMercuryStatus,
        cjcse_map_to_list(IntForeignEnumMap),
        AccForeignEnums1, AccForeignEnums),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    acc_ims_list(IntItemMercuryStatus, IntPromises, AccPromises0, AccPromises),
    TypeRepns = type_ctor_repn_map_to_type_repns(IntTypeRepnMap),
    AccTypeRepns = TypeRepns ++ AccTypeRepns0,

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_promises := AccPromises,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_int2(parse_tree_int2::in,
    read_why_int2::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc) :-
    % XXX ITEM_LIST Compute {Int,Imp}Section and {Int,Imp}ItemMercuryStatus
    % directly from ReadWhy2.
    (
        (
            ReadWhy2 = rwi2_int_use,
            MakeIntSection = make_ims_used(import_locn_interface)
        ;
            ReadWhy2 = rwi2_imp_use,
            MakeIntSection = make_ims_used(import_locn_implementation)
        ;
            ReadWhy2 = rwi2_abstract,
            MakeIntSection = make_ims_abstract_imported
        ),
        MakeImpSection = make_ims_abstract_imported,
        IntSection = MakeIntSection(ModuleName, ifk_int2),
        ImpSection = MakeImpSection(ModuleName, ifk_int2),
        int_module_section_status(IntSection, IntSectionInfo),
        int_module_section_status(ImpSection, ImpSectionInfo)
    ;
        ReadWhy2 = rwi2_opt,
        MakeIntSection = make_ioms_opt_imported,
        MakeImpSection = make_ioms_opt_imported,
        IntOptSection = MakeIntSection(ModuleName, ifk_int2),
        ImpOptSection = MakeImpSection(ModuleName, ifk_int2),
        int_for_opt_module_section_status(IntOptSection, IntSectionInfo),
        int_for_opt_module_section_status(ImpOptSection, ImpSectionInfo)
    ),
    IntSectionInfo = sec_info(IntItemMercuryStatus, _IntNeedQual),

    ParseTreeInt2 = parse_tree_int2(ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _InclMap,
        _IntUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    expect(unify(ImpAvails, []), $pred, "ImpAvails != []"),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = IntFIMs ++ ImpFIMs ++ AccFIMs0,
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    TypeRepns = type_ctor_repn_map_to_type_repns(IntTypeRepnMap),
    AccTypeRepns = TypeRepns ++ AccTypeRepns0,

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_int3(parse_tree_int3::in,
    read_why_int3::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc) :-
    % XXX ITEM_LIST Compute IntSection and IntItemMercuryStatus
    % directly from ReadWhy3.
    (
        ReadWhy3 = rwi3_direct_ancestor_import,
        MakeIntSection = make_ims_imported(import_locn_import_by_ancestor)
    ;
        ReadWhy3 = rwi3_direct_int_import,
        MakeIntSection = make_ims_imported(import_locn_interface)
    ;
        ReadWhy3 = rwi3_direct_imp_import,
        MakeIntSection = make_ims_imported(import_locn_implementation)
    ;
        ReadWhy3 = rwi3_direct_ancestor_use,
        MakeIntSection = make_ims_used(import_locn_import_by_ancestor)
    ;
        ReadWhy3 = rwi3_direct_int_use,
        MakeIntSection = make_ims_used(import_locn_interface)
    ;
        ReadWhy3 = rwi3_direct_imp_use,
        MakeIntSection = make_ims_used(import_locn_implementation)
    ;
        ReadWhy3 = rwi3_direct_int_use_imp_import,
        MakeIntSection = make_ims_used_and_imported(import_locn_interface)
    ;
        ReadWhy3 = rwi3_indirect_int_use,
        MakeIntSection = make_ims_used(import_locn_interface)
    ;
        ReadWhy3 = rwi3_indirect_imp_use,
        MakeIntSection = make_ims_used(import_locn_implementation)
    ),
    IntSection = MakeIntSection(ModuleName, ifk_int3),
    int_module_section_status(IntSection, IntSectionInfo),
    IntSectionInfo = sec_info(IntItemMercuryStatus, _NeedQual),

    ParseTreeInt3 = parse_tree_int3(ModuleName, _ModuleNameContext,
        _IntInclMap, _InclMap, _IntImportMap, ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),

    AccAvails0 = !.Acc ^ ia_avails,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    expect(unify(ImpAvails, []), $pred, "ImpAvails != []"),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails),
    % XXX IntTypeDefnMap has the different kind of type definitions
    % already separated. However, taking advantage of that fact
    % to optimize the separation would be unwise, since the *proper* fix
    % is to merge IntTypeDefnMap into a single type_ctor_checked_map
    % extended with section information.
    TypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    separate_type_defns_abs_mer_for(TypeDefns,
        [], TypeDefnsAbs, [], TypeDefnsMer, [], TypeDefnsFor),
    acc_sec_list(IntSectionInfo, TypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, TypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, TypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    TypeRepns = type_ctor_repn_map_to_type_repns(IntTypeRepnMap),
    AccTypeRepns = TypeRepns ++ AccTypeRepns0,

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_plain_opt(parse_tree_plain_opt::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_plain_opt(ParseTreePlainOpt, !Acc) :-
    ItemMercuryStatus = item_defined_in_other_module(item_import_opt_int),
    SectionInfo = sec_info(ItemMercuryStatus, must_be_qualified),

    ParseTreePlainOpt = parse_tree_plain_opt(_ModuleName, _ModuleNameContext,
        UseMap, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        MarkerPragmas, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccClauses0 = !.Acc ^ ia_clauses,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccImplPragmas0 = !.Acc ^ ia_impl_pragmas,
    AccGenPragmas0 = !.Acc ^ ia_gen_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,

    OptAvails = use_map_to_item_avails(UseMap),
    acc_ims_avails(ItemMercuryStatus, OptAvails, AccAvails0, AccAvails),
    OptFIMs = list.map(fim_spec_to_item, set.to_sorted_list(FIMSpecs)),
    AccFIMs = OptFIMs ++ AccFIMs0,
    separate_type_defns_abs_mer_for(TypeDefns,
        [], TypeDefnsAbs, [], TypeDefnsMer, [], TypeDefnsFor),
    acc_sec_list(SectionInfo, TypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs),
    acc_sec_list(SectionInfo, TypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer),
    acc_sec_list(SectionInfo, TypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor),
    acc_ims_list(ItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    acc_ims_list(ItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(SectionInfo, TypeClasses, AccTypeClasses0, AccTypeClasses),
    acc_ims_list(ItemMercuryStatus, Instances, AccInstances0, AccInstances),
    acc_sec_list(SectionInfo, PredDecls, AccPredDecls0, AccPredDecls),
    acc_ims_list(ItemMercuryStatus, ModeDecls, AccModeDecls0, AccModeDecls),
    acc_ims_list(ItemMercuryStatus, Clauses, AccClauses0, AccClauses),
    acc_ims_tuple_list(ItemMercuryStatus, ForeignEnums,
        AccForeignEnums0, AccForeignEnums),
    acc_pred_marker_pragmas(MarkerPragmas,
        [], DeclMarkerPragmas, [], ImplMarkerPragmas),
    OptDeclPragmas =
        list.map(wrap_type_spec_pragma, TypeSpecs) ++
        list.map(wrap_termination_pragma, TermInfos) ++
        list.map(wrap_termination2_pragma, Term2Infos) ++
        list.map(wrap_struct_sharing_pragma, Sharings) ++
        list.map(wrap_struct_reuse_pragma, Reuses) ++
        DeclMarkerPragmas,
    acc_ims_list(ItemMercuryStatus, OptDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    OptImplPragmas =
        list.map(wrap_foreign_proc, ForeignProcs) ++
        ImplMarkerPragmas,
    acc_ims_list(ItemMercuryStatus, OptImplPragmas,
        AccImplPragmas0, AccImplPragmas),
    OptGenPragmas =
        list.map(wrap_unused_args_pragma, UnusedArgs) ++
        list.map(wrap_exceptions_pragma, Exceptions) ++
        list.map(wrap_trailing_pragma, Trailings) ++
        list.map(wrap_mm_tabling_pragma, MMTablings),
    AccGenPragmas = OptGenPragmas ++ AccGenPragmas0,
    acc_ims_list(ItemMercuryStatus, Promises, AccPromises0, AccPromises),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_clauses := AccClauses,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_impl_pragmas := AccImplPragmas,
    !Acc ^ ia_gen_pragmas := AccGenPragmas,
    !Acc ^ ia_promises := AccPromises.

%---------------------%

:- pred acc_parse_tree_trans_opt(parse_tree_trans_opt::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_trans_opt(ParseTreeTransOpt, !Acc) :-
    ParseTreeTransOpt = parse_tree_trans_opt(_ModuleName, _ModuleNameContext,
        TermInfos, Term2Infos, Exceptions, Trailings, MMTablings,
        Sharings, Reuses),
    ItemMercuryStatus = item_defined_in_other_module(item_import_opt_int),

    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccGenPragmas0 = !.Acc ^ ia_gen_pragmas,

    OptDeclPragmas =
        list.map(wrap_termination_pragma, TermInfos) ++
        list.map(wrap_termination2_pragma, Term2Infos) ++
        list.map(wrap_struct_sharing_pragma, Sharings) ++
        list.map(wrap_struct_reuse_pragma, Reuses),
    acc_ims_list(ItemMercuryStatus, OptDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    OptGenPragmas =
        list.map(wrap_exceptions_pragma, Exceptions) ++
        list.map(wrap_trailing_pragma, Trailings) ++
        list.map(wrap_mm_tabling_pragma, MMTablings),
    AccGenPragmas = OptGenPragmas ++ AccGenPragmas0,

    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_gen_pragmas := AccGenPragmas.

%---------------------------------------------------------------------------%

:- pred acc_ims_avails(item_mercury_status::in, list(item_avail)::in,
    ims_list(list(item_avail))::in, ims_list(list(item_avail))::out) is det.

acc_ims_avails(ItemMercuryStatus, Avails, !AccAvails) :-
    (
        Avails = []
    ;
        Avails = [_ | _],
        !:AccAvails = [ims_item(ItemMercuryStatus, Avails) | !.AccAvails]
    ).

%---------------------------------------------------------------------------%

:- pred separate_type_defns_abs_mer_for(list(item_type_defn_info)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

separate_type_defns_abs_mer_for([], !Abs, !Mer, !For).
separate_type_defns_abs_mer_for([Item | Items], !Abs, !Mer, !For) :-
    separate_type_defns_abs_mer_for(Items, !Abs, !Mer, !For),
    Item = item_type_defn_info(_, _, TypeDefn, _, _, _),
    (
        TypeDefn = parse_tree_abstract_type(_),
        !:Abs = [Item | !.Abs]
    ;
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        ),
        !:Mer = [Item | !.Mer]
    ;
        TypeDefn = parse_tree_foreign_type(_),
        !:For = [Item | !.For]
    ).

%---------------------------------------------------------------------------%

:- pred acc_pred_marker_pragmas(list(item_pred_marker)::in,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out) is det.

acc_pred_marker_pragmas([], !DeclPragmas, !ImplPragmas).
acc_pred_marker_pragmas([ItemMarker | ItemMarkers],
        !DeclPragmas, !ImplPragmas) :-
    acc_pred_marker_pragmas(ItemMarkers, !DeclPragmas, !ImplPragmas),
    Item = wrap_marker_pragma_item(ItemMarker),
    (
        Item = item_decl_pragma(DeclPragma),
        !:DeclPragmas = [DeclPragma | !.DeclPragmas]
    ;
        Item = item_impl_pragma(ImplPragma),
        !:ImplPragmas = [ImplPragma | !.ImplPragmas]
    ).

%---------------------------------------------------------------------------%

    % These functions differ from similar functions in item_util.m in that
    % they yield an item_decl_pragma_info or item_generated_pragma_info;
    % they do not convert the resulting pragma into a general item.

:- func wrap_type_spec_pragma(item_type_spec) = item_decl_pragma_info.
:- func wrap_termination_pragma(item_termination) = item_decl_pragma_info.
:- func wrap_termination2_pragma(item_termination2) = item_decl_pragma_info.
:- func wrap_struct_sharing_pragma(item_struct_sharing)
    = item_decl_pragma_info.
:- func wrap_struct_reuse_pragma(item_struct_reuse) = item_decl_pragma_info.

wrap_type_spec_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_type_spec(Info), Context, SeqNum).

wrap_termination_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_termination_info(Info),
        Context, SeqNum).

wrap_termination2_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_termination2_info(Info),
        Context, SeqNum).

wrap_struct_sharing_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_structure_sharing(Info),
        Context, SeqNum).

wrap_struct_reuse_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_structure_reuse(Info),
        Context, SeqNum).

:- func wrap_foreign_proc(item_foreign_proc) = item_impl_pragma_info.

wrap_foreign_proc(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(impl_pragma_foreign_proc(Info), Context, SeqNum).

:- func wrap_unused_args_pragma(item_unused_args) = item_generated_pragma_info.
:- func wrap_exceptions_pragma(item_exceptions) = item_generated_pragma_info.
:- func wrap_trailing_pragma(item_trailing) = item_generated_pragma_info.
:- func wrap_mm_tabling_pragma(item_mm_tabling) = item_generated_pragma_info.

wrap_unused_args_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_unused_args(Info), Context, SeqNum).

wrap_exceptions_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_exceptions(Info), Context, SeqNum).

wrap_trailing_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_trailing_info(Info), Context, SeqNum).

wrap_mm_tabling_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_mm_tabling_info(Info), Context, SeqNum).

%---------------------------------------------------------------------------%

:- pred acc_sec_list(sec_info::in, list(T)::in,
    list(sec_item(T))::in, list(sec_item(T))::out) is det.

acc_sec_list(SectionInfo, Items, !SecItems) :-
    list.reverse(Items, RevItems),
    acc_sec_list_loop(SectionInfo, RevItems, !SecItems).

:- pred acc_sec_list_loop(sec_info::in, list(T)::in,
    list(sec_item(T))::in, list(sec_item(T))::out) is det.

acc_sec_list_loop(_SectionInfo, [], !SecItems).
acc_sec_list_loop(SectionInfo, [RevItem | RevItems], !SecItems) :-
    !:SecItems = [sec_item(SectionInfo, RevItem) | !.SecItems],
    acc_sec_list_loop(SectionInfo, RevItems, !SecItems).

%---------------------%

:- pred acc_ims_list(item_mercury_status::in, list(T)::in,
    list(ims_item(T))::in, list(ims_item(T))::out) is det.

acc_ims_list(ItemMercuryStatus, Items, !ImsItems) :-
    list.reverse(Items, RevItems),
    acc_ims_list_loop(ItemMercuryStatus, RevItems, !ImsItems).

:- pred acc_ims_list_loop(item_mercury_status::in, list(T)::in,
    list(ims_item(T))::in, list(ims_item(T))::out) is det.

acc_ims_list_loop(_ItemMercuryStatus, [], !ImsItems).
acc_ims_list_loop(ItemMercuryStatus, [RevItem | RevItems], !ImsItems) :-
    !:ImsItems = [ims_item(ItemMercuryStatus, RevItem) | !.ImsItems],
    acc_ims_list_loop(ItemMercuryStatus, RevItems, !ImsItems).

%---------------------%

:- pred acc_ims_tuple_list(item_mercury_status::in, list(T)::in,
    list(ims_tuple_item(T))::in, list(ims_tuple_item(T))::out) is det.

acc_ims_tuple_list(ItemMercuryStatus, Items, !ImsItems) :-
    list.reverse(Items, RevItems),
    acc_ims_tuple_list_loop(ItemMercuryStatus, RevItems, !ImsItems).

:- pred acc_ims_tuple_list_loop(item_mercury_status::in, list(T)::in,
    list(ims_tuple_item(T))::in, list(ims_tuple_item(T))::out) is det.

acc_ims_tuple_list_loop(_ItemMercuryStatus, [], !ImsItems).
acc_ims_tuple_list_loop(ItemMercuryStatus, [RevItem | RevItems], !ImsItems) :-
    !:ImsItems = [{ItemMercuryStatus, RevItem} | !.ImsItems],
    acc_ims_tuple_list_loop(ItemMercuryStatus, RevItems, !ImsItems).

%---------------------------------------------------------------------------%

:- func cjcse_map_to_list(map(_K, c_java_csharp_erlang(list(V)))) = list(V).

cjcse_map_to_list(Map) = List :-
    RevList0 = [],
    map.foldl_values(acc_cjcse_map_to_list, Map, RevList0, RevList),
    list.reverse(RevList, List).

:- pred acc_cjcse_map_to_list(c_java_csharp_erlang(list(V))::in,
    list(V)::in, list(V)::out) is det.

acc_cjcse_map_to_list(CJCsE, !RevList) :-
    CJCsE = c_java_csharp_erlang(Cs, Javas, Csharps, Erlangs),
    !:RevList = Cs ++ Javas ++ Csharps ++ Erlangs ++ !.RevList.

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
%---------------------------------------------------------------------------%
