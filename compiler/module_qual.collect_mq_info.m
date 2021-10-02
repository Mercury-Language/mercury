%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module implements the first pass of module_qual.m; it records
% what entities are available from which modules and with what permissions.
%

:- module parse_tree.module_qual.collect_mq_info.
:- interface.

:- import_module parse_tree.prog_item.

%---------------------------------------------------------------------------%

:- type mq_section
    --->    mq_section_exported
    ;       mq_section_local
    ;       mq_section_imported(import_locn)
    ;       mq_section_abstract_imported.

:- type section_mq_info(MS) == (pred(MS, mq_section, module_permissions)).
:- inst section_mq_info     == (pred(in, out, out) is det).

:- pred src_section_mq_info(src_module_section::in,
    mq_section::out, module_permissions::out) is det.

:- pred int_section_mq_info(int_module_section::in,
    mq_section::out, module_permissions::out) is det.

%---------------------------------------------------------------------------%

:- type int3_role
    --->    int3_as_src
    ;       int3_as_direct_int(read_why_int3).

    % Pass over the given parse tree collecting all defined module, type,
    % inst, mode and class ids, together with their permissions.
    %
:- pred collect_mq_info_in_parse_tree_module_src(parse_tree_module_src::in,
    mq_info::in, mq_info::out) is det.
:- pred collect_mq_info_in_ancestor_int_spec(ancestor_int_spec::in,
    mq_info::in, mq_info::out) is det.
:- pred collect_mq_info_in_direct_int1_spec(direct_int1_spec::in,
    mq_info::in, mq_info::out) is det.
:- pred collect_mq_info_in_direct_int3_spec(direct_int3_spec::in,
    mq_info::in, mq_info::out) is det.

:- pred collect_mq_info_in_parse_tree_int0(read_why_int0::in,
    parse_tree_int0::in, mq_info::in, mq_info::out) is det.
:- pred collect_mq_info_in_parse_tree_int3(int3_role::in,
    parse_tree_int3::in, mq_info::in, mq_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.parse_sym_name.

:- import_module list.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

src_section_mq_info(SrcSection, MQSection, Permissions) :-
    (
        SrcSection = sms_interface,
        MQSection = mq_section_exported,
        PermInInt = may_use_in_int(may_be_unqualified)
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        MQSection = mq_section_local,
        PermInInt = may_not_use_in_int
    ),
    PermInImp = may_use_in_imp(may_be_unqualified),
    Permissions = module_permissions(PermInInt, PermInImp).

int_section_mq_info(IntSection, MQSection, Permissions) :-
    (
        IntSection = ims_imported_or_used(_ModuleName, _IntFileKind,
            Locn, ImportOrUse),
        MQSection = mq_section_imported(Locn),
        (
            (
                ImportOrUse = iou_imported,
                NeedQual = may_be_unqualified
            ;
                ImportOrUse = iou_used,
                NeedQual = must_be_qualified
            ),
            (
                % XXX Whether this module's interface can use an mq_id
                % that was imported by an ancestor should depend on whether
                % the ancestor imported that mq_id in its INTERFACE or not.
                % Since we don't know where that import was, this is a
                % conservative approximation.
                ( Locn = import_locn_interface
                ; Locn = import_locn_import_by_ancestor
                ; Locn = import_locn_ancestor_int0_interface
                ; Locn = import_locn_ancestor_int0_implementation
                ),
                PermInInt = may_use_in_int(NeedQual)
            ;
                Locn = import_locn_implementation,
                PermInInt = may_not_use_in_int
            ),
            PermInImp = may_use_in_imp(NeedQual)
        ;
            ImportOrUse = iou_used_and_imported,
            PermInInt = may_use_in_int(must_be_qualified),
            PermInImp = may_use_in_imp(may_be_unqualified)
        )
    ;
        IntSection = ims_abstract_imported(_ModuleName, _IntFileKind),
        MQSection = mq_section_abstract_imported,
        PermInInt = may_not_use_in_int,
        PermInImp = may_use_in_imp(must_be_qualified)
    ),
    Permissions = module_permissions(PermInInt, PermInImp).

%---------------------------------------------------------------------------%

collect_mq_info_in_parse_tree_module_src(ParseTreeModuleSrc, !Info) :-
    IntPermInInt = may_use_in_int(may_be_unqualified),
    ImpPermInInt = may_not_use_in_int,
    PermInImp = may_use_in_imp(may_be_unqualified),
    IntPermissions = module_permissions(IntPermInInt, PermInImp),
    ImpPermissions = module_permissions(ImpPermInInt, PermInImp),

    ParseTreeModuleSrc = parse_tree_module_src(_ModuleName, _ModuleNameContext,
        _IntInclMap, _ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, _ImportUseMap,
        _IntFIMSpecMap, _ImpFIMSpecMap, _IntSelfFIMLangs, _ImpSelfFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsForeign,
        IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        _IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, IntPromises, _IntBadPreds,

        ImpTypeDefnsAbs, ImpTypeDefnsMer, ImpTypeDefnsForeign,
        ImpInstDefns, ImpModeDefns, ImpTypeClasses, ImpInstances,
        _ImpPredDecls, _ImpModeDecls, _ImpClauses,
        _ImpForeignEnums, _ImpForeignExportEnums,
        _ImpDeclPragmas, _ImpImplPragmas, ImpPromises,
        _ImpInitialises, _ImpFinalises, _ImpMutables),

    mq_info_get_modules(!.Info, Modules0),
    map.foldl(collect_mq_info_in_included_module_info(IntPermissions),
        InclMap, Modules0, Modules),
    mq_info_set_modules(Modules, !Info),

    mq_info_get_imported_modules(!.Info, ImportedModules0),
    mq_info_get_as_yet_unused_interface_modules(!.Info, UnusedIntModules0),
    map.foldl2(collect_mq_info_in_src_avail_map_entry(ms_interface),
        IntImportMap, ImportedModules0, ImportedModules1,
        UnusedIntModules0, UnusedIntModules1),
    map.foldl2(collect_mq_info_in_src_avail_map_entry(ms_interface),
        IntUseMap, ImportedModules1, ImportedModules2,
        UnusedIntModules1, UnusedIntModules2),
    map.foldl2(collect_mq_info_in_src_avail_map_entry(ms_implementation),
        ImpImportMap, ImportedModules2, ImportedModules3,
        UnusedIntModules2, UnusedIntModules3),
    map.foldl2(collect_mq_info_in_src_avail_map_entry(ms_implementation),
        ImpUseMap, ImportedModules3, ImportedModules,
        UnusedIntModules3, UnusedIntModules),
    mq_info_set_imported_modules(ImportedModules, !Info),
    mq_info_set_as_yet_unused_interface_modules(UnusedIntModules, !Info),

    mq_info_get_types(!.Info, Types0),
    IntTypeDefns = IntTypeDefnsAbs ++ IntTypeDefnsMer ++ IntTypeDefnsForeign,
    ImpTypeDefns = ImpTypeDefnsAbs ++ ImpTypeDefnsMer ++ ImpTypeDefnsForeign,
    list.foldl(id_set_insert(IntPermissions),
        list.map(item_type_defn_info_to_mq_id, IntTypeDefns), Types0, Types1),
    list.foldl(id_set_insert(ImpPermissions),
        list.map(item_type_defn_info_to_mq_id, ImpTypeDefns), Types1, Types),
    mq_info_set_types(Types, !Info),

    mq_info_get_insts(!.Info, Insts0),
    list.foldl(id_set_insert(IntPermissions),
        list.map(item_inst_defn_info_to_mq_id, IntInstDefns), Insts0, Insts1),
    list.foldl(id_set_insert(ImpPermissions),
        list.map(item_inst_defn_info_to_mq_id, ImpInstDefns), Insts1, Insts),
    mq_info_set_insts(Insts, !Info),

    mq_info_get_modes(!.Info, Modes0),
    list.foldl(id_set_insert(IntPermissions),
        list.map(item_mode_defn_info_to_mq_id, IntModeDefns), Modes0, Modes1),
    list.foldl(id_set_insert(ImpPermissions),
        list.map(item_mode_defn_info_to_mq_id, ImpModeDefns), Modes1, Modes),
    mq_info_set_modes(Modes, !Info),

    list.foldl(collect_mq_info_in_item_typeclass(IntPermissions),
        IntTypeClasses, !Info),
    list.foldl(collect_mq_info_in_item_typeclass(ImpPermissions),
        ImpTypeClasses, !Info),
    list.foldl(collect_mq_info_in_item_instance, IntInstances, !Info),
    list.foldl(collect_mq_info_in_item_instance, ImpInstances, !Info),
    list.foldl(collect_mq_info_in_item_promise(mq_used_in_interface),
        IntPromises, !Info),
    list.foldl(collect_mq_info_in_item_promise(mq_not_used_in_interface),
        ImpPromises, !Info).

:- pred collect_mq_info_in_src_avail_map_entry(module_section::in,
    module_name::in, one_or_more(prog_context)::in,
    set(module_name)::in, set(module_name)::out,
    module_names_contexts::in, module_names_contexts::out) is det.

collect_mq_info_in_src_avail_map_entry(Section, ModuleName, Contexts,
        !ImportedModules, !UnusedIntModules) :-
    set.insert(ModuleName, !ImportedModules),
    (
        Section = ms_interface,
        % Most of the time, ModuleName does not occur in !.UnusedIntModules.
        % We therefore try the insertion first, and only if the insertion
        % fails do we look up and update the existing entry (OldContexts)
        % that caused that failure.
        ( if map.insert(ModuleName, Contexts, !UnusedIntModules) then
            true
        else
            map.lookup(!.UnusedIntModules, ModuleName, OldContexts),
            NewContexts = OldContexts ++ Contexts,
            map.det_update(ModuleName, NewContexts, !UnusedIntModules)
        )
    ;
        Section = ms_implementation
    ).

%---------------------------------------------------------------------------%

collect_mq_info_in_ancestor_int_spec(AncestorIntSpec, !Info) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
    collect_mq_info_in_parse_tree_int0(ReadWhy0, ParseTreeInt0, !Info).

collect_mq_info_in_direct_int1_spec(DirectInt1Spec, !Info) :-
    DirectInt1Spec = direct_int1(ParseTreeInt1, ReadWhy1),
    collect_mq_info_in_parse_tree_int1(ReadWhy1, ParseTreeInt1, !Info).

collect_mq_info_in_direct_int3_spec(DirectInt3Spec, !Info) :-
    DirectInt3Spec = direct_int3(ParseTreeInt3, ReadWhy3),
    Role = int3_as_direct_int(ReadWhy3),
    collect_mq_info_in_parse_tree_int3(Role, ParseTreeInt3, !Info).

%---------------------------------------------------------------------------%

collect_mq_info_in_parse_tree_int0(ReadWhy0, ParseTreeInt0, !Info) :-
    trace [compile_time(flag("debug_collect_mq_info")), io(!IO)] (
        get_mq_debug_output_stream(!.Info, DebugStream, !IO),
        io.format(DebugStream, "collect_mq_info_in_parse_tree_int0: %s ",
            [s(sym_name_to_string(ParseTreeInt0 ^ pti0_module_name))], !IO),
        io.write_line(DebugStream, ReadWhy0, !IO)
    ),

    (
        ReadWhy0 = rwi0_section,
        % XXX Whether this module's interface can use an mq_id
        % that was imported by an ancestor should depend on whether
        % the ancestor imported that mq_id in its INTERFACE or not.
        % Since we don't know where that import was, this is a
        % conservative approximation.
        IntPermInInt = may_use_in_int(may_be_unqualified),
        IntPermInImp = may_use_in_imp(may_be_unqualified),
        ImpPermInInt = may_use_in_int(may_be_unqualified),
        ImpPermInImp = may_use_in_imp(may_be_unqualified),

        IntPermissions = module_permissions(IntPermInInt, IntPermInImp),
        ImpPermissions = module_permissions(ImpPermInInt, ImpPermInImp)
    ;
        ReadWhy0 = rwi0_opt,
        % Since we do not collect module qual info for int_for_opt_specs,
        % we should never encounter this value of ReadWhy0.
        unexpected($pred, "rwi0_opt")
    ),

    ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, _ImportUseMap,
        _IntFIMSpecs, _ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, _IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, _ImpPredDecls, _ImpModeDecls,
        _ImpForeignEnumMap, _ImpDeclPragmas, ImpPromises),

    mq_info_get_modules(!.Info, Modules0),
    map.foldl(collect_mq_info_in_included_module_info(IntPermissions),
        InclMap, Modules0, Modules),
    mq_info_set_modules(Modules, !Info),

    mq_info_get_imported_modules(!.Info, ImportedModules0),
    list.foldl(collect_mq_info_in_int0_import_or_use, map.keys(IntImportMap),
        ImportedModules0, ImportedModules1),
    list.foldl(collect_mq_info_in_int0_import_or_use, map.keys(IntUseMap),
        ImportedModules1, ImportedModules2),
    list.foldl(collect_mq_info_in_int0_import_or_use, map.keys(ImpImportMap),
        ImportedModules2, ImportedModules3),
    list.foldl(collect_mq_info_in_int0_import_or_use, map.keys(ImpUseMap),
        ImportedModules3, ImportedModules),
    mq_info_set_imported_modules(ImportedModules, !Info),

    mq_info_get_types(!.Info, Types0),
    IntTypeIds = list.map(type_ctor_to_mq_id, map.keys(IntTypeDefnMap)),
    ImpTypeIds = list.map(type_ctor_to_mq_id, map.keys(ImpTypeDefnMap)),
    list.foldl(id_set_insert(IntPermissions), IntTypeIds, Types0, Types1),
    list.foldl(id_set_insert(ImpPermissions), ImpTypeIds, Types1, Types),
    mq_info_set_types(Types, !Info),

    mq_info_get_insts(!.Info, Insts0),
    IntInstIds = list.map(inst_ctor_to_mq_id, map.keys(IntInstDefnMap)),
    ImpInstIds = list.map(inst_ctor_to_mq_id, map.keys(ImpInstDefnMap)),
    list.foldl(id_set_insert(IntPermissions), IntInstIds, Insts0, Insts1),
    list.foldl(id_set_insert(ImpPermissions), ImpInstIds, Insts1, Insts),
    mq_info_set_insts(Insts, !Info),

    mq_info_get_modes(!.Info, Modes0),
    IntModeIds = list.map(mode_ctor_to_mq_id, map.keys(IntModeDefnMap)),
    ImpModeIds = list.map(mode_ctor_to_mq_id, map.keys(ImpModeDefnMap)),
    list.foldl(id_set_insert(IntPermissions), IntModeIds, Modes0, Modes1),
    list.foldl(id_set_insert(ImpPermissions), ImpModeIds, Modes1, Modes),
    mq_info_set_modes(Modes, !Info),

    list.foldl(collect_mq_info_in_item_typeclass(IntPermissions),
        IntTypeClasses, !Info),
    list.foldl(collect_mq_info_in_item_typeclass(ImpPermissions),
        ImpTypeClasses, !Info),
    list.foldl(collect_mq_info_in_item_instance, IntInstances, !Info),
    list.foldl(collect_mq_info_in_item_instance, ImpInstances, !Info),
    list.foldl(collect_mq_info_in_item_promise(mq_used_in_interface),
        IntPromises, !Info),
    list.foldl(collect_mq_info_in_item_promise(mq_not_used_in_interface),
        ImpPromises, !Info).

%---------------------------------------------------------------------------%

:- pred collect_mq_info_in_parse_tree_int1(read_why_int1::in,
    parse_tree_int1::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_parse_tree_int1(ReadWhy1, ParseTreeInt1, !Info) :-
    trace [compile_time(flag("debug_collect_mq_info")), io(!IO)] (
        get_mq_debug_output_stream(!.Info, DebugStream, !IO),
        io.format(DebugStream, "collect_mq_info_in_parse_tree_int1: %s ",
            [s(sym_name_to_string(ParseTreeInt1 ^ pti1_module_name))], !IO),
        io.write_line(DebugStream, ReadWhy1, !IO)
    ),

    (
        ReadWhy1 = rwi1_int_import,
        IntPermInInt = may_use_in_int(may_be_unqualified),
        IntPermInImp = may_use_in_imp(may_be_unqualified)
    ;
        ReadWhy1 = rwi1_imp_import,
        IntPermInInt = may_not_use_in_int,
        IntPermInImp = may_use_in_imp(may_be_unqualified)
    ;
        ReadWhy1 = rwi1_int_use,
        IntPermInInt = may_use_in_int(must_be_qualified),
        IntPermInImp = may_use_in_imp(must_be_qualified)
    ;
        ReadWhy1 = rwi1_imp_use,
        IntPermInInt = may_not_use_in_int,
        IntPermInImp = may_use_in_imp(must_be_qualified)
    ;
        ReadWhy1 = rwi1_int_use_imp_import,
        IntPermInInt = may_use_in_int(must_be_qualified),
        IntPermInImp = may_use_in_imp(may_be_unqualified)
    ;
        ReadWhy1 = rwi1_opt,
        % Since we do not collect module qual info for int_for_opt_specs,
        % we should never encounter this value of ReadWhy1.
        unexpected($pred, "rwi1_opt")
    ;
        ReadWhy1 = rwi1_type_repn,
        % Since we do not collect module qual info for type_repn_specs,
        % we should never encounter this value of ReadWhy1.
        unexpected($pred, "rwi1_opt")
    ),
    IntPermissions = module_permissions(IntPermInInt, IntPermInImp),
    % The implementation section of a .int1 file is abstract imported,
    % which means we have no permission for any item defined there.
    % We therefore do not need any ImpPermissions.

    ParseTreeInt1 = parse_tree_int1(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, InclMap,
        _IntUseMap, _ImpUseMap, _ImportUseMap, _IntFIMSpecs, _ImpFIMSpecs,
        TypeCheckedMap, InstCheckedMap, ModeCheckedMap,
        IntTypeClasses, IntInstances, _IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, IntPromises, _IntTypeRepnMap,
        _ImpTypeClasses),

    mq_info_get_modules(!.Info, Modules0),
    map.foldl(collect_mq_info_in_included_module_info(IntPermissions),
        InclMap, Modules0, Modules),
    mq_info_set_modules(Modules, !Info),

    mq_info_get_types(!.Info, Types0),
    map.foldl(collect_mq_info_in_int_type_defn(IntPermissions),
        TypeCheckedMap, Types0, Types),
    mq_info_set_types(Types, !Info),

    mq_info_get_insts(!.Info, Insts0),
    map.foldl(collect_mq_info_in_int_inst_defn(IntPermissions),
        InstCheckedMap, Insts0, Insts),
    mq_info_set_insts(Insts, !Info),

    mq_info_get_modes(!.Info, Modes0),
    map.foldl(collect_mq_info_in_int_mode_defn(IntPermissions),
        ModeCheckedMap, Modes0, Modes),
    mq_info_set_modes(Modes, !Info),

    list.foldl(collect_mq_info_in_item_typeclass(IntPermissions),
        IntTypeClasses, !Info),
    list.foldl(collect_mq_info_in_item_instance, IntInstances, !Info),
    list.foldl(collect_mq_info_in_item_promise(mq_used_in_interface),
        IntPromises, !Info).

:- pred collect_mq_info_in_int_type_defn(module_permissions::in,
    type_ctor::in, type_ctor_checked_defn::in,
    type_id_set::in, type_id_set::out) is det.

collect_mq_info_in_int_type_defn(IntPermissions, TypeCtor, CheckedDefn,
        !Types) :-
    (
        CheckedDefn = checked_defn_solver(SolverDefn, _),
        (
            SolverDefn = solver_type_abstract(AbsStatus, _),
            (
                AbsStatus = abstract_solver_type_exported,
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                AbsStatus = abstract_solver_type_private
            )
        ;
            SolverDefn = solver_type_full(MaybeAbsDefn, _),
            (
                MaybeAbsDefn = yes(_),
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                MaybeAbsDefn = no
            )
        )
    ;
        CheckedDefn = checked_defn_std(StdDefn, _),
        (
            StdDefn = std_mer_type_eqv(EqvStatus, _),
            (
                ( EqvStatus = std_eqv_type_mer_exported
                ; EqvStatus = std_eqv_type_abstract_exported
                ),
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                EqvStatus = std_eqv_type_all_private
            )
        ;
            StdDefn = std_mer_type_subtype(SubStatus, _),
            (
                ( SubStatus = std_sub_type_mer_exported
                ; SubStatus = std_sub_type_abstract_exported
                ),
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                SubStatus = std_sub_type_all_private
            )
        ;
            (
                StdDefn =
                    std_mer_type_du_all_plain_constants(DuStatus, _, _, _, _)
            ;
                StdDefn =
                    std_mer_type_du_not_all_plain_constants(DuStatus, _, _)
            ),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                DuStatus = std_du_type_all_private
            )
        ;
            StdDefn = std_mer_type_abstract(AbsStatus, _, _),
            (
                ( AbsStatus = std_abs_type_ft_exported
                ; AbsStatus = std_abs_type_abstract_exported
                ),
                id_set_insert(IntPermissions,  type_ctor_to_mq_id(TypeCtor),
                    !Types)
            ;
                AbsStatus = std_abs_type_all_private
            )
        )
    ).

:- pred collect_mq_info_in_int_inst_defn(module_permissions::in,
    inst_ctor::in, inst_ctor_checked_defn::in,
    inst_id_set::in, inst_id_set::out) is det.

collect_mq_info_in_int_inst_defn(IntPermissions, InstCtor, CheckedDefn,
        !Insts) :-
    CheckedDefn = checked_defn_inst(StdInstDefn, _),
    StdInstDefn = std_inst_defn(Status, _),
    (
        ( Status = std_inst_exported
        ; Status = std_inst_abstract_exported
        ),
        id_set_insert(IntPermissions,  inst_ctor_to_mq_id(InstCtor), !Insts)
    ;
        Status = std_inst_all_private
    ).

:- pred collect_mq_info_in_int_mode_defn(module_permissions::in,
    mode_ctor::in, mode_ctor_checked_defn::in,
    mode_id_set::in, mode_id_set::out) is det.

collect_mq_info_in_int_mode_defn(IntPermissions, ModeCtor, CheckedDefn,
        !Modes) :-
    CheckedDefn = checked_defn_mode(StdModeDefn, _),
    StdModeDefn = std_mode_defn(Status, _),
    (
        ( Status = std_mode_exported
        ; Status = std_mode_abstract_exported
        ),
        id_set_insert(IntPermissions,  mode_ctor_to_mq_id(ModeCtor), !Modes)
    ;
        Status = std_mode_all_private
    ).

%---------------------------------------------------------------------------%

collect_mq_info_in_parse_tree_int3(Role, ParseTreeInt3, !Info) :-
    trace [compile_time(flag("debug_collect_mq_info")), io(!IO)] (
        get_mq_debug_output_stream(!.Info, DebugStream, !IO),
        io.format(DebugStream, "collect_mq_info_in_parse_tree_int3: %s ",
            [s(sym_name_to_string(ParseTreeInt3 ^ pti3_module_name))], !IO),
        io.write_line(DebugStream, Role, !IO)
    ),

    (
        Role = int3_as_src,
        PermInInt = may_use_in_int(may_be_unqualified),
        PermInImp = may_use_in_imp(may_be_unqualified)
    ;
        Role = int3_as_direct_int(ReadWhy3),
        (
            ( ReadWhy3 = rwi3_direct_ancestor_import
            ; ReadWhy3 = rwi3_direct_int_import
            ),
            PermInInt = may_use_in_int(may_be_unqualified),
            PermInImp = may_use_in_imp(may_be_unqualified)
        ;
            ( ReadWhy3 = rwi3_direct_ancestor_use
            ; ReadWhy3 = rwi3_direct_int_use
            ; ReadWhy3 = rwi3_indirect_int_use
            ),
            PermInInt = may_use_in_int(must_be_qualified),
            PermInImp = may_use_in_imp(must_be_qualified)
        ;
            ReadWhy3 = rwi3_direct_imp_import,
            PermInInt = may_not_use_in_int,
            PermInImp = may_use_in_imp(may_be_unqualified)
        ;
            ( ReadWhy3 = rwi3_direct_imp_use
            ; ReadWhy3 = rwi3_indirect_imp_use
            ),
            PermInInt = may_not_use_in_int,
            PermInImp = may_use_in_imp(must_be_qualified)
        ;
            ReadWhy3 = rwi3_direct_int_use_imp_import,
            PermInInt = may_use_in_int(must_be_qualified),
            PermInImp = may_use_in_imp(may_be_unqualified)
        )
    ),
    Permissions = module_permissions(PermInInt, PermInImp),

    ParseTreeInt3 = parse_tree_int3(_ModuleName, _ModuleNameContext,
        _IntInclMap, InclMap, IntImportMap, _ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, _IntTypeRepns),

    mq_info_get_modules(!.Info, Modules0),
    map.foldl(collect_mq_info_in_included_module_info(Permissions),
        InclMap, Modules0, Modules),
    mq_info_set_modules(Modules, !Info),

    mq_info_get_imported_modules(!.Info, ImportedModules0),
    list.foldl(collect_mq_info_in_int3_import, map.keys(IntImportMap),
        ImportedModules0, ImportedModules),
    mq_info_set_imported_modules(ImportedModules, !Info),

    mq_info_get_types(!.Info, Types0),
    TypeIds = list.map(type_ctor_to_mq_id, map.keys(IntTypeDefnMap)),
    list.foldl(id_set_insert(Permissions), TypeIds, Types0, Types),
    mq_info_set_types(Types, !Info),

    mq_info_get_insts(!.Info, Insts0),
    InstIds = list.map(inst_ctor_to_mq_id, map.keys(IntInstDefnMap)),
    list.foldl(id_set_insert(Permissions), InstIds, Insts0, Insts),
    mq_info_set_insts(Insts, !Info),

    mq_info_get_modes(!.Info, Modes0),
    ModeIds = list.map(mode_ctor_to_mq_id, map.keys(IntModeDefnMap)),
    list.foldl(id_set_insert(Permissions), ModeIds, Modes0, Modes),
    mq_info_set_modes(Modes, !Info),

    list.foldl(collect_mq_info_in_item_typeclass(Permissions), IntTypeClasses,
        !Info),
    list.foldl(collect_mq_info_in_item_instance, IntInstances, !Info).

%---------------------------------------------------------------------------%

:- func type_ctor_to_mq_id(type_ctor) = mq_id.

type_ctor_to_mq_id(TypeCtor) = Id :-
    TypeCtor = type_ctor(SymName, Arity),
    Id = mq_id(SymName, Arity).

:- func inst_ctor_to_mq_id(inst_ctor) = mq_id.

inst_ctor_to_mq_id(InstCtor) = Id :-
    InstCtor = inst_ctor(SymName, Arity),
    Id = mq_id(SymName, Arity).

:- func mode_ctor_to_mq_id(mode_ctor) = mq_id.

mode_ctor_to_mq_id(ModeCtor) = Id :-
    ModeCtor = mode_ctor(SymName, Arity),
    Id = mq_id(SymName, Arity).

%---------------------------------------------------------------------------%

:- pred collect_mq_info_in_included_module_info(module_permissions::in,
    module_name::in, include_module_info::in,
    module_id_set::in, module_id_set::out) is det.

collect_mq_info_in_included_module_info(IntPermissions, ModuleName, InclInfo,
        !Modules) :-
    InclInfo = include_module_info(Section, _Context),
    (
        Section = ms_interface,
        Arity = 0,
        id_set_insert(IntPermissions, mq_id(ModuleName, Arity), !Modules)
    ;
        Section = ms_implementation,
        Arity = 0,
        id_set_insert(IntPermissions, mq_id(ModuleName, Arity), !Modules)
    ).

:- pred collect_mq_info_in_int0_import_or_use(module_name::in,
    set(module_name)::in, set(module_name)::out) is det.

collect_mq_info_in_int0_import_or_use(ModuleName, !ImportedModules) :-
    set.insert(ModuleName, !ImportedModules).

:- pred collect_mq_info_in_int3_import(module_name::in,
    set(module_name)::in, set(module_name)::out) is det.

collect_mq_info_in_int3_import(ModuleName, !ImportedModules) :-
    set.insert(ModuleName, !ImportedModules).

:- func item_type_defn_info_to_mq_id(item_type_defn_info) = mq_id.

item_type_defn_info_to_mq_id(ItemTypeDefn) = MQId :-
    ItemTypeDefn = item_type_defn_info(SymName, Params, _, _, _, _),
    MQId = mq_id(SymName, list.length(Params)).

:- func item_inst_defn_info_to_mq_id(item_inst_defn_info) = mq_id.

item_inst_defn_info_to_mq_id(ItemInstDefn) = MQId :-
    ItemInstDefn = item_inst_defn_info(SymName, Params, _, _, _, _, _),
    MQId = mq_id(SymName, list.length(Params)).

:- func item_mode_defn_info_to_mq_id(item_mode_defn_info) = mq_id.

item_mode_defn_info_to_mq_id(ItemModeDefn) = MQId :-
    ItemModeDefn = item_mode_defn_info(SymName, Params, _, _, _, _),
    MQId = mq_id(SymName, list.length(Params)).

:- pred collect_mq_info_in_item_typeclass(module_permissions::in,
    item_typeclass_info::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_item_typeclass(Permissions, ItemTypeClass, !Info) :-
    ItemTypeClass = item_typeclass_info(SymName, Params, _, _, _, _, _, _),
    list.length(Params, Arity),
    mq_info_get_classes(!.Info, Classes0),
    id_set_insert(Permissions, mq_id(SymName, Arity), Classes0, Classes),
    mq_info_set_classes(Classes, !Info).

:- pred collect_mq_info_in_item_instance(item_instance_info::in,
    mq_info::in, mq_info::out) is det.

collect_mq_info_in_item_instance(ItemInstance, !Info) :-
    InstanceModule = ItemInstance ^ ci_module_containing_instance,
    mq_info_get_imported_instance_modules(!.Info, ImportedInstanceModules0),
    set.insert(InstanceModule,
        ImportedInstanceModules0, ImportedInstanceModules),
    mq_info_set_imported_instance_modules(ImportedInstanceModules, !Info).

:- pred collect_mq_info_in_item_promise(mq_in_interface::in,
    item_promise_info::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_item_promise(InInt, ItemPromise, !Info) :-
    ItemPromise = item_promise_info(_PromiseType, Goal, _ProgVarSet,
        _UnivVars, _Context, _SeqNum),
    collect_used_modules_in_promise_goal(Goal,
        set.init, UsedModuleNames, no, FoundUnqual),
    (
        FoundUnqual = no,
        set.fold(mq_info_set_module_used(InInt), UsedModuleNames, !Info)
    ;
        % Any unqualified symbol in the promise might come from *any* of
        % the imported modules. There is no way for us to tell which ones,
        % so we conservatively assume that it uses *all* of them.
        FoundUnqual = yes,
        map.init(UnusedModules),
        mq_info_set_as_yet_unused_interface_modules(UnusedModules, !Info)
    ).

%---------------------%

    % Scan Goal. Add the set of module names found in the qualified symbols
    % in Goal to !UsedModuleNames. If there exists a single unqualified symbol,
    % in Goal, set !Success to no.
    %
:- pred collect_used_modules_in_promise_goal(goal::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_goal(Goal, !UsedModuleNames, !FoundUnqual) :-
    (
        ( Goal = conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = par_conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = disj_expr(_, SubGoalA, SubGoalB)
        ; Goal = implies_expr(_, SubGoalA, SubGoalB)
        ; Goal = equivalent_expr(_, SubGoalA, SubGoalB)
        ),
        collect_used_modules_in_promise_goal(SubGoalA,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(SubGoalB,
            !UsedModuleNames, !FoundUnqual)
    ;
        ( Goal = true_expr(_)
        ; Goal = fail_expr(_)
        )
    ;
        ( Goal = not_expr(_, SubGoal)
        ; Goal = quant_expr(_, _, _, _, SubGoal)
        ; Goal = promise_purity_expr(_, _, SubGoal)
        ; Goal = promise_equivalent_solutions_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_sets_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_arbitrary_expr(_, _, _, _, _,
            SubGoal)
        ; Goal = require_detism_expr(_, _, SubGoal)
        ; Goal = require_complete_switch_expr(_, _, SubGoal)
        ; Goal = require_switch_arms_detism_expr(_, _, _, SubGoal)
        ; Goal = trace_expr(_, _, _, _, _, SubGoal)
        ; Goal = disable_warnings_expr(_, _, _, SubGoal)
        ),
        collect_used_modules_in_promise_goal(SubGoal,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = try_expr(_, _, SubGoal, ThenGoal, MaybeElseGoal, Catches,
            MaybeCatchAny),
        collect_used_modules_in_promise_goal(SubGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ThenGoal,
            !UsedModuleNames, !FoundUnqual),
        (
            MaybeElseGoal = no
        ;
            MaybeElseGoal = yes(ElseGoal),
            collect_used_modules_in_promise_goal(ElseGoal,
                !UsedModuleNames, !FoundUnqual)
        ),
        list.foldl2(collect_used_modules_in_promise_catch, Catches,
            !UsedModuleNames, !FoundUnqual),
        (
            MaybeCatchAny = no
        ;
            MaybeCatchAny = yes(catch_any_expr(_, CatchAnyGoal)),
            collect_used_modules_in_promise_goal(CatchAnyGoal,
                !UsedModuleNames, !FoundUnqual)
        )
    ;
        Goal = atomic_expr(_, _, _, _, MainGoal, OrElseGoals),
        collect_used_modules_in_promise_goal(MainGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goals(OrElseGoals,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = if_then_else_expr(_, _, _, CondGoal, ThenGoal, ElseGoal),
        collect_used_modules_in_promise_goal(CondGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ThenGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ElseGoal,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = event_expr(_, _Name, ArgTerms0),
        list.map(term.coerce, ArgTerms0, ArgTerms),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    ;
        Goal = call_expr(_, SymName, ArgTerms0, _Purity),
        (
            SymName = qualified(ModuleName, _),
            set.insert(ModuleName, !UsedModuleNames)
        ;
            SymName = unqualified(_),
            !:FoundUnqual = yes
        ),
        list.map(term.coerce, ArgTerms0, ArgTerms),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    ;
        Goal = unify_expr(_, LHS0, RHS0, _Purity),
        term.coerce(LHS0, LHS),
        term.coerce(RHS0, RHS),
        collect_used_modules_in_term(LHS, !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_term(RHS, !UsedModuleNames, !FoundUnqual)
    ).

    % Performs collect_used_modules_in_promise_goal on a list of goals.
    %
:- pred collect_used_modules_in_promise_goals(list(goal)::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_goals([], !UsedModuleNames, !FoundUnqual).
collect_used_modules_in_promise_goals([Goal | Goals],
        !UsedModuleNames, !FoundUnqual) :-
    collect_used_modules_in_promise_goal(Goal,
        !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_promise_goals(Goals,
        !UsedModuleNames, !FoundUnqual).

:- pred collect_used_modules_in_promise_catch(catch_expr::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_catch(CatchExpr,
        !UsedModuleNames, !FoundUnqual) :-
    CatchExpr = catch_expr(Pattern0, Goal),
    term.coerce(Pattern0, Pattern),
    collect_used_modules_in_term(Pattern, !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_promise_goal(Goal, !UsedModuleNames, !FoundUnqual).

    % Add all the module names in qualified sym_names in Term to
    % !UsedModuleNames, and set !FoundUnqual to true if any of the sym_names
    % in Term is unqualified.
    %
:- pred collect_used_modules_in_term(term::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_term(Term, !UsedModuleNames, !FoundUnqual) :-
    ( if try_parse_sym_name_and_args(Term, SymName, ArgTerms) then
        (
            SymName = qualified(ModuleName, _),
            set.insert(ModuleName, !UsedModuleNames)
        ;
            SymName = unqualified(_),
            !:FoundUnqual = yes
        ),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    else
        true
    ).

:- pred collect_used_modules_in_terms(list(term)::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_terms([], !UsedModuleNames, !FoundUnqual).
collect_used_modules_in_terms([Term | Terms],
        !UsedModuleNames, !FoundUnqual) :-
    collect_used_modules_in_term(Term, !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_terms(Terms, !UsedModuleNames, !FoundUnqual).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.collect_mq_info.
%---------------------------------------------------------------------------%
