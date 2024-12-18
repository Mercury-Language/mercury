%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019-2021, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: convert_parse_tree.m.
% Main author: zs.
%
% This module provides ways to convert between parse_tree_int on the one hand
% and parse_tree_int0, parse_tree_int1, parse_tree_int2 and parse_tree_int3
% on the other hand. The former is a generic data structure for representing
% interface files, while the latter are specialized versions of it that
% encode the different structural invariants on each kind of interface file
% in the type.
%
%---------------------------------------------------------------------------%

:- module parse_tree.convert_parse_tree.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.

:- import_module list.

%---------------------------------------------------------------------------%

    % The generic representation of all the different kinds of interface files.
    % The parser reads in each .intN file in this format, and then immediately
    % converts it to its int-file-kind representation using the
    % check_convert_parse_tree_int_to_intN predicates just below.
    % The rest of the compiler uses the parse_tree_intN representation.
    %
:- type parse_tree_int
    --->    parse_tree_int(
                pti_module_name             :: module_name,
                pti_int_file_kind           :: int_file_kind,

                % The context of the `:- module' declaration.
                pti_module_name_context     :: prog_context,

                % For .int0, .int and .int2; not for .int3.
                pti_maybe_version_numbers   :: maybe_version_numbers,

                % `:- include_module' declarations in the interface and
                % in the implementation.
                pti_int_includes            :: list(item_include),
                pti_imp_includes            :: list(item_include),

                % `:- import_module' and `:- use_module' declarations
                % in the interface and in the implementation.
                pti_int_avails              :: list(item_avail),
                pti_imp_avails              :: list(item_avail),

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti_int_fims                :: list(item_fim),
                pti_imp_fims                :: list(item_fim),

                % Items in the interface and in the implementation.
                pti_int_items               :: list(item),
                pti_imp_items               :: list(item)
            ).

    % Convert from the generic interface file parse tree to the
    % interface-file-kind specific parse trees. These conversions go
    % from less restrictive to more restrictive, so they can discover
    % problems, which they report as error messages.
    %
:- pred check_convert_parse_tree_int_to_int0(
    parse_tree_int::in, parse_tree_int0::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred check_convert_parse_tree_int_to_int1(
    parse_tree_int::in, parse_tree_int1::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred check_convert_parse_tree_int_to_int2(
    parse_tree_int::in, parse_tree_int2::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred check_convert_parse_tree_int_to_int3(
    parse_tree_int::in, parse_tree_int3::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- type parse_tree_opt
    --->    parse_tree_opt(
                pto_module_name             :: module_name,
                pto_opt_file_kind           :: opt_file_kind,

                % The context of the `:- module' declaration.
                pto_module_name_context     :: prog_context,

                % `:- use_module' (not `:- import_module') declarations.
                pto_uses                    :: list(avail_use_info),

                pto_fims                    :: list(item_fim),
                pto_items                   :: list(item)
            ).

    % Convert from the generic optimization file parse tree to the
    % optimization-file-kind specific parse trees. These conversions go
    % from less restrictive to more restrictive, so they can discover
    % problems, which they report as error messages.
    %
:- pred check_convert_parse_tree_opt_to_plain_opt(
    parse_tree_opt::in, parse_tree_plain_opt::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred check_convert_parse_tree_opt_to_trans_opt(
    parse_tree_opt::in, parse_tree_trans_opt::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- type raw_compilation_unit
    --->    raw_compilation_unit(
                % The name of the module.
                rcu_module_name                 :: module_name,

                % The context of the `:- module' declaration.
                rcu_module_name_context         :: prog_context,

                % The items in the module.
                rcu_raw_item_blocks             :: list(raw_item_block)
            ).

    % We used to have several kinds of item blocks, but raw item blocks
    % are the only ones we still use.
:- type raw_item_block
    --->    item_block(
                module_name,
                module_section,
                list(item_include),
                list(item_avail),
                list(item_fim),
                list(item)
            ).

:- pred check_convert_raw_comp_unit_to_module_src(globals::in,
    raw_compilation_unit::in, parse_tree_module_src::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Functions for gathering up lists of particular kinds of items
    % and gathering all items for a given type_ctor (or its inst
    % or mode equivalent) together. These are a key component
    % of the less to more restrictive conversions above, but they are
    % also useful in their own right.
    %
:- func type_ctor_defn_items_to_map(list(item_type_defn_info))
    = type_ctor_defn_map.
:- func inst_ctor_defn_items_to_map(list(item_inst_defn_info))
    = inst_ctor_defn_map.
:- func mode_ctor_defn_items_to_map(list(item_mode_defn_info))
    = mode_ctor_defn_map.
:- func type_ctor_repn_items_to_map(list(item_type_repn_info))
    = type_ctor_repn_map.
:- func type_ctor_foreign_enum_items_to_map(list(item_foreign_enum_info))
    = type_ctor_foreign_enum_map.

%---------------------------------------------------------------------------%

:- func type_ctor_defn_map_to_type_defns(type_ctor_defn_map)
    = list(item_type_defn_info).
:- func inst_ctor_defn_map_to_inst_defns(inst_ctor_defn_map)
    = list(item_inst_defn_info).
:- func mode_ctor_defn_map_to_mode_defns(mode_ctor_defn_map)
    = list(item_mode_defn_info).
:- func type_ctor_repn_map_to_type_repns(type_ctor_repn_map)
    = list(item_type_repn_info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.check_type_inst_mode_defns.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int0(ParseTreeInt, ParseTreeInt0, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),
    expect(unify(IntFileKind, ifk_int0), $pred,
        "trying to convert non-ifk_int0 parse_tree_int to parse_tree_int0"),

    classify_include_modules(IntIncls, ImpIncls, InclMap, !Specs),
    classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        SectionImportUseMap, !Specs),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int0_items_int_or_imp(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], _IntForeignEnums, [], IntDeclPragmas0, [], IntDeclMarkers0,
        [], IntPromises0, !Specs),
    % XXX ITEM_LIST Should we report any misplaced foreign enums in
    % _IntForeignEnums now, or wait until code generation? For now,
    % we do the latter.
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    list.sort(IntDeclPragmas0, IntDeclPragmas),
    list.sort(IntDeclMarkers0, IntDeclMarkers),
    list.sort(IntPromises0, IntPromises),

    classify_int0_items_int_or_imp(ImpItems, [], ImpTypeDefns,
        [], ImpInstDefns, [], ImpModeDefns,
        [], ImpTypeClasses0, [], ImpInstances0,
        [], ImpPredDecls0, [], RevImpModeDecls,
        [], ImpForeignEnums, [], ImpDeclPragmas0, [], ImpDeclMarkers0,
        [], ImpPromises0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns),
    ImpInstDefnMap = inst_ctor_defn_items_to_map(ImpInstDefns),
    ImpModeDefnMap = mode_ctor_defn_items_to_map(ImpModeDefns),
    list.sort(ImpTypeClasses0, ImpTypeClasses),
    list.sort(ImpInstances0, ImpInstances),
    list.sort(ImpPredDecls0, ImpPredDecls),
    list.reverse(RevImpModeDecls, ImpModeDecls),
    ImpForeignEnumMap = type_ctor_foreign_enum_items_to_map(ImpForeignEnums),
    list.sort(ImpDeclPragmas0, ImpDeclPragmas),
    list.sort(ImpDeclMarkers0, ImpDeclMarkers),
    list.sort(ImpPromises0, ImpPromises),

    create_type_ctor_checked_map(do_not_insist_on_defn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        TypeCtorCheckedMap, !Specs),
    create_inst_ctor_checked_map(do_not_insist_on_defn,
        IntInstDefnMap, ImpInstDefnMap, InstCtorCheckedMap, !Specs),
    create_mode_ctor_checked_map(do_not_insist_on_defn,
        IntModeDefnMap, ImpModeDefnMap, ModeCtorCheckedMap, !Specs),

    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, InclMap,
        SectionImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpDeclMarkers, ImpPromises).

:- pred classify_int0_items_int_or_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_abstract_instance_info)::in,
        list(item_abstract_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int0_items_int_or_imp([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers, !Promises, !Specs).
classify_int0_items_int_or_imp([Item | Items], !TypeDefns,
        !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers, !Promises, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(ClassName, Types, OrigTypes,
            Constraints, Body, TVarSet, Module, Context, SeqNum),
        (
            Body = instance_body_abstract,
            ItemAbstractInstance = item_instance_info(ClassName,
                Types, OrigTypes, Constraints, instance_body_abstract,
                TVarSet, Module, Context, SeqNum),
            !:Instances = [ItemAbstractInstance | !.Instances]
        ;
            Body = instance_body_concrete(_),
            Pieces = [words("A .int0 file may not contain"),
                words("concrete instance declarations."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_pred_decl(ItemPredDecl),
        !:PredDecls = [ItemPredDecl | !.PredDecls]
    ;
        Item = item_mode_decl(ItemModeDecl),
        !:RevModeDecls = [ItemModeDecl | !.RevModeDecls]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        !:ForeignEnums = [ItemForeignEnum | !.ForeignEnums]
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        !:DeclPragmas = [ItemDeclPragma | !.DeclPragmas]
    ;
        Item = item_decl_marker(ItemDeclMarker),
        !:DeclMarkers = [ItemDeclMarker | !.DeclMarkers]
    ;
        Item = item_promise(ItemPromise),
        !:Promises = [ItemPromise | !.Promises]
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int0 file may not contain")] ++
            items_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int0_items_int_or_imp(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers, !Promises, !Specs).

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int1(ParseTreeInt, ParseTreeInt1, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),
    expect(unify(IntFileKind, ifk_int1), $pred,
        "trying to convert non-ifk_int1 parse_tree_int to parse_tree_int1"),

    classify_include_modules(IntIncls, ImpIncls, InclMap, !Specs),
    classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        SectionImportUseMap, !Specs),
    map.foldl2(restrict_to_section_use_map_entry(".int"),
        SectionImportUseMap, map.init, SectionUseMap, !Specs),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int1_items_int(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], _IntForeignEnums, [], IntDeclPragmas0, [], IntDeclMarkers,
        [], IntPromises0, [], IntTypeRepns, !Specs),
    % XXX ITEM_LIST Should we report any misplaced foreign enums in
    % _IntForeignEnums now, or wait until code generation? For now,
    % we do the latter.
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    list.sort(IntDeclPragmas0, IntDeclPragmas),
    list.sort(IntPromises0, IntPromises),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns),

    classify_int1_items_imp(ImpItems, [], ImpTypeDefns0,
        [], ImpForeignEnums0, [], ImpTypeClasses0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns0),
    ImpForeignEnumMap = type_ctor_foreign_enum_items_to_map(ImpForeignEnums0),
    list.sort(ImpTypeClasses0, ImpTypeClasses),

    create_type_ctor_checked_map(do_not_insist_on_defn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        IntTypeCheckedMap, !Specs),
    map.init(ImpInstDefnMap),
    create_inst_ctor_checked_map(do_not_insist_on_defn,
        IntInstDefnMap, ImpInstDefnMap, IntInstCheckedMap, !Specs),
    map.init(ImpModeDefnMap),
    create_mode_ctor_checked_map(do_not_insist_on_defn,
        IntModeDefnMap, ImpModeDefnMap, IntModeCheckedMap, !Specs),

    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, InclMap, SectionUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeCheckedMap, IntInstCheckedMap, IntModeCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises, IntTypeRepnMap,
        ImpTypeClasses).

:- pred classify_int1_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_abstract_instance_info)::in,
        list(item_abstract_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers,
        !Promises, !TypeRepns, !Specs).
classify_int1_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers,
        !Promises, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(ClassName, Types, OrigTypes,
            Constraints, Body, TVarSet, Module, Context, SeqNum),
        (
            Body = instance_body_abstract,
            ItemAbstractInstance = item_instance_info(ClassName,
                Types, OrigTypes, Constraints, instance_body_abstract,
                TVarSet, Module, Context, SeqNum),
            !:Instances = [ItemAbstractInstance | !.Instances]
        ;
            Body = instance_body_concrete(_),
            Pieces = [words("A .int file may not contain"),
                words("concrete instance declarations"),
                words("in its interface section."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        Item = item_pred_decl(ItemPredDecl),
        !:PredDecls = [ItemPredDecl | !.PredDecls]
    ;
        Item = item_mode_decl(ItemModeDecl),
        !:ModeDecls = [ItemModeDecl | !.ModeDecls]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        !:ForeignEnums = [ItemForeignEnum | !.ForeignEnums]
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        !:DeclPragmas = [ItemDeclPragma | !.DeclPragmas]
    ;
        Item = item_decl_marker(ItemDeclMarker),
        !:DeclMarkers = [ItemDeclMarker | !.DeclMarkers]
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(PromiseType, _, _, _, Context, _),
        (
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            !:Promises = [ItemPromise | !.Promises]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("A .int file may not contain")] ++
                items_desc_pieces(Item) ++
                [words("in its interface section."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int file may not contain")] ++
            items_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int1_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !DeclPragmas, !DeclMarkers,
        !Promises, !TypeRepns, !Specs).

:- pred classify_int1_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_abstract_typeclass_info)::in,
        list(item_abstract_typeclass_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_imp([], !TypeDefns, !ForeignEnums, !TypeClasses, !Specs).
classify_int1_items_imp([Item | Items], !TypeDefns, !ForeignEnums,
        !TypeClasses, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(ClassName, Params,
            Supers, Fundeps, Interface, TVarSet, Context, SeqNum),
        (
            Interface = class_interface_abstract,
            AbstractItemTypeClass = item_typeclass_info(ClassName, Params,
                Supers, Fundeps, class_interface_abstract, TVarSet,
                Context, SeqNum),
            !:TypeClasses = [AbstractItemTypeClass | !.TypeClasses]
        ;
            Interface = class_interface_concrete(_),
            Pieces = [words("A .int file may not contain"),
                words("concrete typeclass declarations."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        !:ForeignEnums = [ItemForeignEnum | !.ForeignEnums]
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int file may not contain")] ++
            items_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int1_items_imp(Items, !TypeDefns, !ForeignEnums,
        !TypeClasses, !Specs).

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int2(ParseTreeInt, ParseTreeInt2, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int2), $pred,
        "trying to convert non-ifk_int2 parse_tree_int to parse_tree_int2"),

    (
        ImpIncls = []
    ;
        ImpIncls = [FirstImpIncl | _],
        ImpInclPieces = [words("A .int2 file may not contain"),
            decl("include_module"), words("declarations"),
            words("in its implementation section."), nl],
        ImpInclSpec = spec($pred, severity_error, phase_t2pt,
            FirstImpIncl ^ incl_context, ImpInclPieces),
        !:Specs = [ImpInclSpec | !.Specs]
    ),
    classify_include_modules(IntIncls, [], InclMap, !Specs),
    map.foldl(add_only_int_include, InclMap, map.init, IntInclMap),

    classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        SectionImportUseMap, !Specs),
    map.foldl2(restrict_to_section_use_map_entry(".int2"),
        SectionImportUseMap, map.init, SectionUseMap, !Specs),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int2_items_int(IntItems, [], IntTypeDefns0,
        [], IntInstDefns0, [], IntModeDefns0,
        [], IntTypeClasses0, [], IntInstances0, [], IntTypeRepns0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns0),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns0),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns0),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns0),

    classify_int2_items_imp(ImpItems, [], ImpTypeDefns0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns0),

    map.init(ImpForeignEnumMap),
    create_type_ctor_checked_map(do_not_insist_on_defn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        IntTypeCheckedMap, !Specs),
    map.init(ImpInstDefnMap),
    create_inst_ctor_checked_map(do_not_insist_on_defn,
        IntInstDefnMap, ImpInstDefnMap, IntInstCheckedMap, !Specs),
    map.init(ImpModeDefnMap),
    create_mode_ctor_checked_map(do_not_insist_on_defn,
        IntModeDefnMap, ImpModeDefnMap, IntModeCheckedMap, !Specs),

    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclMap,
        SectionUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeCheckedMap, IntInstCheckedMap, IntModeCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred classify_int2_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_abstract_instance_info)::in,
        list(item_abstract_instance_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int2_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).
classify_int2_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(ClassName, Types, OrigTypes,
            Constraints, Body, TVarSet, Module, Context, SeqNum),
        (
            Body = instance_body_abstract,
            ItemAbstractInstance = item_instance_info(ClassName,
                Types, OrigTypes, Constraints, instance_body_abstract,
                TVarSet, Module, Context, SeqNum),
            !:Instances = [ItemAbstractInstance | !.Instances]
        ;
            Body = instance_body_concrete(_),
            Pieces = [words("A .int2 file may not contain"),
                words("concrete instance declarations"),
                words("in its interface section."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            items_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int2_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).

:- pred classify_int2_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int2_items_imp([], !TypeDefns, !Specs).
classify_int2_items_imp([Item | Items], !TypeDefns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            items_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int2_items_imp(Items, !TypeDefns, !Specs).

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int3(ParseTreeInt, ParseTreeInt3, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int3), $pred,
        "trying to convert non-ifk_int3 parse_tree_int to parse_tree_int3"),

    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(_),
        VNPieces = [words("A .int3 file may not contain"),
            words("version number information."), nl],
        % MaybeVersionNumbers itself contains no context information.
        VNSpec = spec($pred, severity_error, phase_t2pt,
            ModuleNameContext, VNPieces),
        !:Specs = [VNSpec | !.Specs]
    ),

    classify_include_modules(IntIncls, [], InclMap, !Specs),
    map.foldl(add_only_int_include, InclMap, map.init, IntInclMap),

    classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        SectionImportUseMap, !Specs),
    map.foldl2(restrict_to_int_import_map_entry(".int3"),
        SectionImportUseMap, map.init, IntImportMap, !Specs),

    (
        IntFIMs = []
    ;
        IntFIMs = [FirstIntFIM | _],
        IntFIMPieces = [words("A .int3 file may not contain"),
            pragma_decl("foreign_import_module"), words("declarations."), nl],
        IntFIMSpec = spec($pred, severity_error, phase_t2pt,
            FirstIntFIM ^ fim_context, IntFIMPieces),
        !:Specs = [IntFIMSpec | !.Specs]
    ),

    classify_int3_items_int(IntItems, [], IntTypeDefns0,
        [], IntInstDefns0, [], IntModeDefns0,
        [], IntTypeClasses0, [], IntInstances0, [], IntTypeRepns0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns0),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns0),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns0),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns0),

    map.init(ImpTypeDefnMap),
    map.init(ImpForeignEnumMap),
    create_type_ctor_checked_map(do_not_insist_on_defn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        IntTypeCheckedMap, !Specs),
    map.init(ImpInstDefnMap),
    create_inst_ctor_checked_map(do_not_insist_on_defn,
        IntInstDefnMap, ImpInstDefnMap, IntInstCheckedMap, !Specs),
    map.init(ImpModeDefnMap),
    create_mode_ctor_checked_map(do_not_insist_on_defn,
        IntModeDefnMap, ImpModeDefnMap, IntModeCheckedMap, !Specs),

    some [!ImpContexts]
    (
        !:ImpContexts = [],
        (
            ImpIncls = []
        ;
            ImpIncls = [HeadIncl | _],
            !:ImpContexts = [HeadIncl ^ incl_context | !.ImpContexts]
        ),
        (
            ImpAvails = []
        ;
            ImpAvails = [HeadAvail | _],
            !:ImpContexts = [get_avail_context(HeadAvail) | !.ImpContexts]
        ),
        (
            ImpFIMs = []
        ;
            ImpFIMs = [HeadFIM | _],
            !:ImpContexts = [HeadFIM ^ fim_context | !.ImpContexts]
        ),
        (
            ImpItems = []
        ;
            ImpItems = [HeadImpItem | _],
            !:ImpContexts = [get_item_context(HeadImpItem) | !.ImpContexts]
        ),
        list.sort(!ImpContexts),
        (
            !.ImpContexts = []
        ;
            !.ImpContexts = [FirstImpContext | _],
            ImpItemPieces = [words("A .int3 file must not have"),
                words("an implementation section."), nl],
            ImpItemSpec = spec($pred, severity_error, phase_t2pt,
                FirstImpContext, ImpItemPieces),
            !:Specs = [ImpItemSpec | !.Specs]
        )
    ),
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclMap, IntImportMap,
        IntTypeCheckedMap, IntInstCheckedMap, IntModeCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred classify_int3_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_abstract_typeclass_info)::in,
        list(item_abstract_typeclass_info)::out,
    list(item_abstract_instance_info)::in,
        list(item_abstract_instance_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int3_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).
classify_int3_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(ClassName, Params,
            Supers, Fundeps, Interface, TVarSet, Context, SeqNum),
        (
            Interface = class_interface_abstract,
            (
                Supers = [],
                (
                    Fundeps = [],
                    ItemAbstractTypeClass = item_typeclass_info(ClassName,
                        Params, [], [], class_interface_abstract,
                        TVarSet, Context, SeqNum),
                    !:TypeClasses = [ItemAbstractTypeClass | !.TypeClasses]
                ;
                    Fundeps = [_ | _],
                    Pieces = [words("A typeclass declaration in a .int3 file"),
                        words("may not list any functional dependencies."),
                        nl],
                    Spec = spec($pred, severity_error, phase_t2pt,
                        Context, Pieces),
                    !:Specs = [Spec | !.Specs]
                )
            ;
                Supers = [_ | _],
                (
                    Fundeps = [],
                    FunDepPieces = [words("or any functional dependencies")]
                ;
                    Fundeps = [_ | _],
                    FunDepPieces = []
                ),
                Pieces = [words("A typeclass declaration in a .int3 file"),
                    words("may not list any superclasses")] ++ FunDepPieces ++
                    [suffix("."), nl],
                Spec = spec($pred, severity_error, phase_t2pt,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            Interface = class_interface_concrete(_),
            Pieces = [words("A .int3 file may not contain"),
                words("concrete typeclass declarations."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(ClassName, Types, OrigTypes,
            Constraints, Body, TVarSet, Module, Context, SeqNum),
        (
            Body = instance_body_abstract,
            ItemAbstractInstance = item_instance_info(ClassName,
                Types, OrigTypes, Constraints, instance_body_abstract,
                TVarSet, Module, Context, SeqNum),
            !:Instances = [ItemAbstractInstance | !.Instances]
        ;
            Body = instance_body_concrete(_),
            Pieces = [words("A .int3 file may not contain"),
                words("concrete instance declarations."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int3 file may not contain")] ++
            items_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int3_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).

%---------------------------------------------------------------------------%

check_convert_parse_tree_opt_to_plain_opt(ParseTreeOpt, ParseTreePlainOpt,
        !Specs) :-
    ParseTreeOpt = parse_tree_opt(ModuleName, OptFileKind, ModuleNameContext,
        Uses, FIMs, Items),
    expect(unify(OptFileKind, ofk_opt), $pred,
        "trying to convert non-ofk_plain_opt parse_tree_opt " ++
        "to parse_tree_plain_opt"),

    accumulate_uses_maps(Uses, one_or_more_map.init, UseMap),
    set.list_to_set(list.map(fim_item_to_spec, FIMs), FIMSpecs),
    classify_plain_opt_items(Items, [], TypeDefns0, [], ForeignEnums0,
        [], InstDefns0, [], ModeDefns0, [], TypeClasses0, [], Instances0,
        [], PredDecls0, [], RevModeDecls, [], RevClauses0, [], RevForeignProcs,
        [], Promises0, [], DeclMarkers0, [], ImplMarkers0,
        [], TypeSpecs0, [], UnusedArgs0, [], TermInfos0, [], Term2Infos0,
        [], Exceptions0, [], Trailings0, [], MMTablings0,
        [], Sharings0, [], Reuses0, !Specs),
    list.sort(TypeDefns0, TypeDefns),
    list.sort(ForeignEnums0, ForeignEnums),
    list.sort(InstDefns0, InstDefns),
    list.sort(ModeDefns0, ModeDefns),
    list.sort(TypeClasses0, TypeClasses),
    list.sort(Instances0, Instances),
    list.sort(PredDecls0, PredDecls),
    list.reverse(RevModeDecls, ModeDecls),
    list.reverse(RevClauses0, Clauses0),
    list.reverse(RevForeignProcs, ForeignProcs),
    list.sort(Promises0, Promises),
    list.sort(DeclMarkers0, DeclMarkers),
    list.sort(ImplMarkers0, ImplMarkers),
    list.sort(TypeSpecs0, TypeSpecs),
    list.sort(UnusedArgs0, UnusedArgs),
    list.sort(TermInfos0, TermInfos),
    list.sort(Term2Infos0, Term2Infos),
    list.sort(Exceptions0, Exceptions),
    list.sort(Trailings0, Trailings),
    list.sort(MMTablings0, MMTablings),
    list.sort(Sharings0, Sharings),
    list.sort(Reuses0, Reuses),

    list.map(undo_default_names_in_clause, Clauses0, Clauses),
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, ModuleNameContext,
        UseMap, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        DeclMarkers, ImplMarkers, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses).

:- pred classify_plain_opt_items(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_clause_info)::in, list(item_clause_info)::out,
    list(item_foreign_proc_info)::in, list(item_foreign_proc_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_decl_marker_info_opt)::in, list(item_decl_marker_info_opt)::out,
    list(item_impl_marker_info_opt)::in, list(item_impl_marker_info_opt)::out,
    list(decl_pragma_type_spec_info)::in,
        list(decl_pragma_type_spec_info)::out,
    list(gen_pragma_unused_args_info)::in,
        list(gen_pragma_unused_args_info)::out,
    list(decl_pragma_termination_info)::in,
        list(decl_pragma_termination_info)::out,
    list(decl_pragma_termination2_info)::in,
        list(decl_pragma_termination2_info)::out,
    list(gen_pragma_exceptions_info)::in,
        list(gen_pragma_exceptions_info)::out,
    list(gen_pragma_trailing_info)::in, list(gen_pragma_trailing_info)::out,
    list(gen_pragma_mm_tabling_info)::in,
        list(gen_pragma_mm_tabling_info)::out,
    list(decl_pragma_struct_sharing_info)::in,
        list(decl_pragma_struct_sharing_info)::out,
    list(decl_pragma_struct_reuse_info)::in,
        list(decl_pragma_struct_reuse_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_plain_opt_items([], !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs, !Promises,
        !DeclMarkers, !ImplMarkers, !TypeSpecs,
        !UnusedArgs, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).
classify_plain_opt_items([Item | Items], !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs, !Promises,
        !DeclMarkers, !ImplMarkers, !TypeSpecs,
        !UnusedArgs, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        !:ForeignEnums = [ItemForeignEnum | !.ForeignEnums]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_pred_decl(ItemPredDecl),
        !:PredDecls = [ItemPredDecl | !.PredDecls]
    ;
        Item = item_mode_decl(ItemModeDecl),
        !:RevModeDecls = [ItemModeDecl | !.RevModeDecls]
    ;
        Item = item_clause(ItemClause),
        !:RevClauses = [ItemClause | !.RevClauses]
    ;
        Item = item_foreign_proc(ItemForeignProc),
        !:RevForeignProcs = [ItemForeignProc | !.RevForeignProcs]
    ;
        Item = item_promise(ItemPromise),
        !:Promises = [ItemPromise | !.Promises]
    ;
        Item = item_decl_pragma(DeclPragma),
        (
            DeclPragma = decl_pragma_type_spec(TypeSpec),
            !:TypeSpecs = [TypeSpec | !.TypeSpecs]
        ;
            DeclPragma = decl_pragma_termination(Term),
            !:TermInfos = [Term | !.TermInfos]
        ;
            DeclPragma = decl_pragma_termination2(Term2),
            !:Term2Infos = [Term2 | !.Term2Infos]
        ;
            DeclPragma = decl_pragma_struct_sharing(Sharing),
            !:Sharings = [Sharing | !.Sharings]
        ;
            DeclPragma = decl_pragma_struct_reuse(Reuse),
            !:Reuses = [Reuse | !.Reuses]
        ;
            ( DeclPragma = decl_pragma_obsolete_pred(_)
            ; DeclPragma = decl_pragma_obsolete_proc(_)
            ; DeclPragma = decl_pragma_format_call(_)
            ; DeclPragma = decl_pragma_type_spec_constr(_)
            ; DeclPragma = decl_pragma_oisu(_)
            ),
            Pieces = [words("A .opt file may not contain")] ++
                items_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_item_context(Item), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_decl_marker(DeclMarker),
        DeclMarker = item_decl_marker_info(Marker, SymNameArityMaybePF,
            Context, SeqNum),
        (
            ( Marker = dpmk_terminates
            ; Marker = dpmk_does_not_terminate
            ),
            SymNameArityMaybePF =
                pred_pfu_name_arity(PFU, SymName, Arity),
            (
                ( PFU = pfu_predicate
                ; PFU = pfu_function
                )
            ;
                PFU = pfu_unknown,
                % When we create .opt files, we always specify PredOrFunc.
                unexpected($pred, "PFU = pfu_unknown")
            ),
            SubSymNameArityMaybePF = pred_pfu_name_arity(PFU, SymName, Arity),
            SubDeclMarker = item_decl_marker_info(Marker,
                SubSymNameArityMaybePF, Context, SeqNum),
            !:DeclMarkers = [coerce(SubDeclMarker) | !.DeclMarkers]
        ;
            Marker = dpmk_check_termination,
            Pieces = [words("A .opt file may not contain")] ++
                items_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_item_context(Item), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_impl_pragma(_),
        Pieces = [words("A .opt file may not contain")] ++
            items_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        Item = item_impl_marker(ImplMarker),
        ImplMarker = item_impl_marker_info(Marker, SymNameArityMaybePF,
            Context, SeqNum),
        (
            ( Marker = ipmk_inline
            ; Marker = ipmk_no_inline
            ; Marker = ipmk_promise_eqv_clauses
            ; Marker = ipmk_promise_pure
            ; Marker = ipmk_promise_semipure
            ; Marker = ipmk_mode_check_clauses
            ),
            SymNameArityMaybePF = pred_pfu_name_arity(PFU, SymName, Arity),
            (
                ( PFU = pfu_predicate
                ; PFU = pfu_function
                )
            ;
                PFU = pfu_unknown,
                % When we create .opt files, we always specify PredOrFunc.
                unexpected($pred, "PFU = pfu_unknown")
            ),
            SubSymNameArityMaybePF = pred_pfu_name_arity(PFU, SymName, Arity),
            SubImplMarker = item_impl_marker_info(Marker,
                SubSymNameArityMaybePF, Context, SeqNum),
            !:ImplMarkers = [coerce(SubImplMarker) | !.ImplMarkers]
        ;
            ( Marker = ipmk_consider_used
            ; Marker = ipmk_no_detism_warning
            ),
            Pieces = [words("A .opt file may not contain")] ++
                items_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_item_context(Item), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_generated_pragma(GenPragma),
        (
            GenPragma = gen_pragma_unused_args(UnusedArgs),
            !:UnusedArgs = [UnusedArgs | !.UnusedArgs]
        ;
            GenPragma = gen_pragma_exceptions(Exception),
            !:Exceptions = [Exception | !.Exceptions]
        ;
            GenPragma = gen_pragma_trailing(Trailing),
            !:Trailings = [Trailing | !.Trailings]
        ;
            GenPragma = gen_pragma_mm_tabling(MMTabling),
            !:MMTablings = [MMTabling | !.MMTablings]
        )
    ;
        ( Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .opt file may not contain")] ++
            items_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_plain_opt_items(Items, !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs, !Promises,
        !DeclMarkers, !ImplMarkers, !TypeSpecs,
        !UnusedArgs, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).

    % When the compiler writes out a clause to an optimization file,
    % it must give every variable in that clause visible representation,
    % even if that variable *had* no name in the memory representation
    % of the clause. The name written out will be the default name of the
    % variable, as given by varset.lookup_name, which will have the form
    % V_<N>, where <N> is the variable's number.
    %
    % When the clause thus written out is later read in, the compiler will
    % record e.g. "V_3" as the name of a variable. However, in HLDS dumps,
    % and probably in some other contexts, where it is important to be able
    % to distinguish two different variables even if they have the same name
    % (as in e.g. two variables that started out with the same name but then
    % got automatically renamed apart), the compiler will write this variable
    % out as e.g. "V_V_3_17". In this form, the middle "V_3" is the variable
    % name that the compiler believes was given by the user, the "_17" is
    % the variable's actual number, and the initial "V_" is a "stuffing" prefix
    % added by mercury_convert_var_name to every variable name whose name
    % starts with "V_", to allow them to be distinguished from the default
    % names given by varset.lookup_name to unnamed variables.
    %
    % However, for clauses read in from compiler-generated files,
    % variable names such as "V_3" are *not* given by the programmer,
    % so for them, all this effor is wasted. Worse, names such as "V_V_3_17"
    % are harder to read and remember than the printed default names
    % of actually unnamed variables.
    %
    % Therefore, before we put a clause into the parse tree of a .opt file,
    % delete the name of every variable whose name has the form "V_<N>".
    %
:- pred undo_default_names_in_clause(
    item_clause_info::in, item_clause_info::out) is det.

undo_default_names_in_clause(Clause0, Clause) :-
    VarSet0 = Clause0 ^ cl_varset,
    varset.undo_default_names(VarSet0, VarSet),
    Clause = Clause0 ^ cl_varset := VarSet.

%---------------------------------------------------------------------------%

check_convert_parse_tree_opt_to_trans_opt(ParseTreeOpt, ParseTreeTransOpt,
        !Specs) :-
    ParseTreeOpt = parse_tree_opt(ModuleName, OptFileKind, ModuleNameContext,
        Uses, FIMs, Items),
    expect(unify(OptFileKind, ofk_trans_opt), $pred,
        "trying to convert non-ofk_trans_opt parse_tree_opt " ++
        "to parse_tree_trans_opt"),

    (
        Uses = []
    ;
        Uses = [FirstUse | _],
        UsePieces = [words("A .trans_opt file may not contain"),
            decl("use_module"), words("declarations."), nl],
        UseSpec = spec($pred, severity_error, phase_t2pt,
            FirstUse ^ aui_context, UsePieces),
        !:Specs = [UseSpec | !.Specs]
    ),
    (
        FIMs = []
    ;
        FIMs = [FirstFIM | _],
        FIMPieces = [words("A .trans_opt file may not contain"),
            pragma_decl("foreign_import_module"), words("declarations."), nl],
        FIMSpec = spec($pred, severity_error, phase_t2pt,
            FirstFIM ^ fim_context, FIMPieces),
        !:Specs = [FIMSpec | !.Specs]
    ),

    classify_trans_opt_items(Items, [], TermInfos0, [], Term2Infos0,
        [], Exceptions0, [], Trailings0, [], MMTablings0,
        [], Sharings0, [], Reuses0, !Specs),
    list.sort(TermInfos0, TermInfos),
    list.sort(Term2Infos0, Term2Infos),
    list.sort(Exceptions0, Exceptions),
    list.sort(Trailings0, Trailings),
    list.sort(MMTablings0, MMTablings),
    list.sort(Sharings0, Sharings),
    list.sort(Reuses0, Reuses),

    ParseTreeTransOpt = parse_tree_trans_opt(ModuleName, ModuleNameContext,
        TermInfos, Term2Infos, Exceptions, Trailings, MMTablings,
        Sharings, Reuses).

:- pred classify_trans_opt_items(list(item)::in,
    list(decl_pragma_termination_info)::in,
        list(decl_pragma_termination_info)::out,
    list(decl_pragma_termination2_info)::in,
        list(decl_pragma_termination2_info)::out,
    list(gen_pragma_exceptions_info)::in,
        list(gen_pragma_exceptions_info)::out,
    list(gen_pragma_trailing_info)::in, list(gen_pragma_trailing_info)::out,
    list(gen_pragma_mm_tabling_info)::in,
        list(gen_pragma_mm_tabling_info)::out,
    list(decl_pragma_struct_sharing_info)::in,
        list(decl_pragma_struct_sharing_info)::out,
    list(decl_pragma_struct_reuse_info)::in,
        list(decl_pragma_struct_reuse_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_trans_opt_items([], !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).
classify_trans_opt_items([Item | Items], !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs) :-
    (
        Item = item_decl_pragma(DeclPragma),
        (
            DeclPragma = decl_pragma_termination(Term),
            !:TermInfos = [Term | !.TermInfos]
        ;
            DeclPragma = decl_pragma_termination2(Term2),
            !:Term2Infos = [Term2 | !.Term2Infos]
        ;
            DeclPragma = decl_pragma_struct_sharing(Sharing),
            !:Sharings = [Sharing | !.Sharings]
        ;
            DeclPragma = decl_pragma_struct_reuse(Reuse),
            !:Reuses = [Reuse | !.Reuses]
        ;
            ( DeclPragma = decl_pragma_obsolete_pred(_)
            ; DeclPragma = decl_pragma_obsolete_proc(_)
            ; DeclPragma = decl_pragma_format_call(_)
            ; DeclPragma = decl_pragma_type_spec(_)
            ; DeclPragma = decl_pragma_type_spec_constr(_)
            ; DeclPragma = decl_pragma_oisu(_)
            ),
            Pieces = [words("A .trans_opt file may not contain")] ++
                items_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_item_context(Item), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Item = item_generated_pragma(GenPragma),
        (
            GenPragma = gen_pragma_exceptions(Exception),
            !:Exceptions = [Exception | !.Exceptions]
        ;
            GenPragma = gen_pragma_trailing(Trailing),
            !:Trailings = [Trailing | !.Trailings]
        ;
            GenPragma = gen_pragma_mm_tabling(MMTabling),
            !:MMTablings = [MMTabling | !.MMTablings]
        ;
            GenPragma = gen_pragma_unused_args(_),
            Pieces = [words("A .trans_opt file may not contain")] ++
                items_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_item_context(Item), Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_promise(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_proc(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_impl_marker(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .trans_opt file may not contain")] ++
            items_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_item_context(Item), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    classify_trans_opt_items(Items, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).

%---------------------------------------------------------------------------%

check_convert_raw_comp_unit_to_module_src(Globals, RawCompUnit,
        ParseTreeModuleSrc, !Specs) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        ItemBlocks),

    map.init(IntFIMSpecMap0),
    map.init(ImpFIMSpecMap0),

    IntImplicitAvailNeeds0 = init_implicit_avail_needs,
    ImpImplicitAvailNeeds0 = init_implicit_avail_needs,

    classify_src_items_in_blocks(ItemBlocks,
        [], IntIncls, [], IntAvails, IntFIMSpecMap0, IntFIMSpecMap,

        [], RevIntTypeDefns, [], RevIntInstDefns, [], RevIntModeDefns,
        [], RevIntTypeClasses, [], RevIntInstances0,
        [], RevIntPredDecls, [], RevIntModeDecls,
        [], RevIntDeclPragmas, [], RevIntDeclMarkers,
        [], RevIntImplPragmas, [], RevIntImplMarkers,
        set.init, IntBadClausePreds, [], RevIntPromises,
        [], RevIntInitialises, [], RevIntFinalises, [], RevIntMutables,
        IntImplicitAvailNeeds0, IntImplicitAvailNeeds,
        set.init, IntSelfFIMLangs,

        [], ImpIncls, [], ImpAvails, ImpFIMSpecMap0, ImpFIMSpecMap1,
        [], RevImpTypeDefns, [], RevImpInstDefns, [], RevImpModeDefns,
        [], RevImpTypeClasses, [], RevImpInstances0,
        [], RevImpPredDecls, [], RevImpModeDecls,
        [], RevImpClauses, [], RevImpForeignProcs,
        [], RevImpForeignEnums, [], RevImpForeignExportEnums,
        [], RevImpDeclPragmas, [], RevImpDeclMarkers,
        [], RevImpImplPragmas, [], RevImpImplMarkers,
        [], RevImpPromises,
        [], RevImpInitialises0, [], RevImpFinalises0, [], RevImpMutables0,
        ImpImplicitAvailNeeds0, ImpImplicitAvailNeeds,
        set.init, ImpSelfFIMLangs,

        !Specs),

    classify_include_modules(IntIncls, ImpIncls, InclMap, !Specs),

    list.reverse(RevIntTypeDefns, IntTypeDefns),
    list.reverse(RevIntInstDefns, IntInstDefns),
    list.reverse(RevIntModeDefns, IntModeDefns),
    list.reverse(RevIntTypeClasses, IntTypeClasses),
    list.reverse(RevIntInstances0, IntInstances0),
    list.reverse(RevIntPredDecls, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    list.reverse(RevIntDeclPragmas, IntDeclPragmas),
    list.reverse(RevIntDeclMarkers, IntDeclMarkers),
    list.reverse(RevIntImplPragmas, IntImplPragmas),
    list.reverse(RevIntImplMarkers, IntImplMarkers),
    list.reverse(RevIntPromises, IntPromises),
    list.reverse(RevIntInitialises, IntInitialises),
    list.reverse(RevIntFinalises, IntFinalises),
    list.reverse(RevIntMutables, IntMutables),

    list.reverse(RevImpTypeDefns, ImpTypeDefns),
    list.reverse(RevImpInstDefns, ImpInstDefns),
    list.reverse(RevImpModeDefns, ImpModeDefns),
    list.reverse(RevImpTypeClasses, ImpTypeClasses),
    list.reverse(RevImpInstances0, ImpInstances0),
    list.reverse(RevImpPredDecls, ImpPredDecls),
    list.reverse(RevImpModeDecls, ImpModeDecls),
    list.reverse(RevImpClauses, ImpClauses),
    list.reverse(RevImpForeignProcs, ImpForeignProcs),
    list.reverse(RevImpForeignEnums, ImpForeignEnums),
    list.reverse(RevImpForeignExportEnums, ImpForeignExportEnums),
    list.reverse(RevImpDeclPragmas, ImpDeclPragmas),
    list.reverse(RevImpDeclMarkers, ImpDeclMarkers),
    list.reverse(RevImpImplPragmas, ImpImplPragmas0),
    list.reverse(RevImpImplMarkers, ImpImplMarkers0),
    list.reverse(RevImpPromises, ImpPromises),
    list.reverse(RevImpInitialises0, ImpInitialises0),
    list.reverse(RevImpFinalises0, ImpFinalises0),
    list.reverse(RevImpMutables0, ImpMutables0),

    ( if map.is_empty(InclMap) then
        IntInstances = IntInstances0,
        ImpInstances = ImpInstances0
    else
        split_concrete_int_instances(IntInstances0,
            IntInstances, MovedImpInstances),
        ImpInstances = MovedImpInstances ++ ImpInstances0
    ),

    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns),
    ImpForeignEnumMap = type_ctor_foreign_enum_items_to_map(ImpForeignEnums),
    create_type_ctor_checked_map(do_insist_on_defn,
        IntTypeDefnMap, ImpTypeDefnMap, ImpForeignEnumMap,
        TypeCtorCheckedMap, [], TypeSpecs),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    ImpInstDefnMap = inst_ctor_defn_items_to_map(ImpInstDefns),
    create_inst_ctor_checked_map(do_insist_on_defn,
        IntInstDefnMap, ImpInstDefnMap, InstCtorCheckedMap,
        [], InstSpecs),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    ImpModeDefnMap = mode_ctor_defn_items_to_map(ImpModeDefns),
    create_mode_ctor_checked_map(do_insist_on_defn,
        IntModeDefnMap, ImpModeDefnMap, ModeCtorCheckedMap,
        InstSpecs, InstModeSpecs),

    % classify_src_items_in_blocks has already generated an error message
    % for each impl pragma in the interface section. However, we then treat
    % these misplaced pragmas as if they were in the implementation section.
    % This preserves old behavior. By allowing predicates to be marked as
    % external even when the external pragma is misplaced, this old behavior
    % prevents a compiler abort on the invalid/type_spec test case.
    ImpImplPragmas = IntImplPragmas ++ ImpImplPragmas0,
    ImpImplMarkers = IntImplMarkers ++ ImpImplMarkers0,

    % By implicitly moving initialise, finalise and mutable declarations
    % from the interface (where they should not be) to the implementation
    % section *after* generating an error message for their inappropriate
    % placement, we allow the compiler to test them for further errors.
    % Reporting such further errors together with the bad placement
    % should allow programmers to fix both problems at once.
    ImpInitialises = IntInitialises ++ ImpInitialises0,
    ImpFinalises = IntFinalises ++ ImpFinalises0,
    ImpMutables = IntMutables ++ ImpMutables0,

    classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        SectionImportUseMap, !Specs),
    import_and_or_use_map_section_to_maybe_implicit(SectionImportUseMap,
        ImportUseMap0),
    extend_import_and_or_use_map_with_implicits(Globals,
        IntImplicitAvailNeeds, ImpImplicitAvailNeeds,
        ImportUseMap0, ImportUseMap),

    set.intersect(
        map.keys_as_set(IntFIMSpecMap),
        map.keys_as_set(ImpFIMSpecMap1),
        IntImpFIMSpecs),
    set.foldl2(report_int_imp_fim(IntFIMSpecMap), IntImpFIMSpecs,
        ImpFIMSpecMap1, ImpFIMSpecMap, !Specs),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises, IntBadClausePreds,

        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpClauses, ImpForeignProcs, ImpForeignExportEnums,
        ImpDeclPragmas, ImpDeclMarkers, ImpImplPragmas, ImpImplMarkers,
        ImpPromises, ImpInitialises, ImpFinalises, ImpMutables).

    % Given a list of instances in the interface section of a
    % parse_tree_module_src that has submodules, return the instances
    % that should remain in the interface section of the module
    % (which will be the abtract versions of the original instances)
    % and the instances that should be added to the implementation section
    % (the original int instances, if concrete).
    %
    % XXX CLEANUP This preserves old behavior. I (zs) would find it
    % nice to know just *why* this needs to be done.
    %
:- pred split_concrete_int_instances(list(item_instance_info)::in,
    list(item_instance_info)::out, list(item_instance_info)::out) is det.

split_concrete_int_instances(IntInstances0, IntInstances, ImpInstances) :-
    split_concrete_int_instances_acc(IntInstances0,
        cord.init, IntInstanceCord, cord.init, ImpInstanceCord),
    IntInstances = cord.list(IntInstanceCord),
    ImpInstances = cord.list(ImpInstanceCord).

:- pred split_concrete_int_instances_acc(list(item_instance_info)::in,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out) is det.

split_concrete_int_instances_acc([],
        !IntInstanceCord, !ImpInstanceCord).
split_concrete_int_instances_acc([IntInstance | IntInstances],
        !IntInstanceCord, !ImpInstanceCord) :-
    Body = IntInstance ^ ci_method_instances,
    (
        Body = instance_body_concrete(_),
        AbstractIntInstance =
            IntInstance ^ ci_method_instances := instance_body_abstract,
        cord.snoc(AbstractIntInstance, !IntInstanceCord),
        cord.snoc(IntInstance, !ImpInstanceCord)
    ;
        Body = instance_body_abstract,
        % Do not put another copy of this item into !ImpInstanceCord.
        cord.snoc(IntInstance, !IntInstanceCord)
    ),
    split_concrete_int_instances_acc(IntInstances,
        !IntInstanceCord, !ImpInstanceCord).

%---------------------------------------------------------------------------%

type_ctor_defn_items_to_map(TypeDefnInfos) = TypeDefnMap :-
    list.foldl(add_type_defn_to_map, TypeDefnInfos, map.init, TypeDefnMap).

:- pred add_type_defn_to_map(item_type_defn_info::in,
    type_ctor_defn_map::in, type_ctor_defn_map::out) is det.

add_type_defn_to_map(TypeDefnInfo, !TypeDefnMap) :-
    TypeDefnInfo = item_type_defn_info(SymName, Params, TypeDefn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    some [!AbsSolverDefns, !SolverDefns,
        !AbsStdDefns, !EqvDefns, !DuDefns, !SubDefns,
        !ForeignDefnsC, !ForeignDefnsJava, !ForeignDefnsCsharp]
    (
        ( if map.search(!.TypeDefnMap, TypeCtor, AllDefns0) then
            AllDefns0 = type_ctor_all_defns(
                !:AbsSolverDefns, !:SolverDefns,
                !:AbsStdDefns, !:EqvDefns, !:DuDefns, !:SubDefns,
                c_java_csharp(!:ForeignDefnsC, !:ForeignDefnsJava,
                    !:ForeignDefnsCsharp))
        else
            !:AbsSolverDefns = [],
            !:SolverDefns = [],

            !:AbsStdDefns = [],
            !:EqvDefns = [],
            !:DuDefns = [],
            !:SubDefns = [],
            !:ForeignDefnsC = [],
            !:ForeignDefnsJava = [],
            !:ForeignDefnsCsharp = []
        ),
        (
            TypeDefn = parse_tree_abstract_type(DetailsAbs),
            AbsDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsAbs,
            (
                DetailsAbs = abstract_solver_type,
                !:AbsSolverDefns = !.AbsSolverDefns ++ [AbsDefnInfo]
            ;
                ( DetailsAbs = abstract_type_general
                ; DetailsAbs = abstract_type_fits_in_n_bits(_)
                ; DetailsAbs = abstract_dummy_type
                ; DetailsAbs = abstract_notag_type
                ; DetailsAbs = abstract_subtype(_)
                ),
                !:AbsStdDefns = !.AbsStdDefns ++ [AbsDefnInfo]
            )
        ;
            TypeDefn = parse_tree_solver_type(DetailsSolver),
            SolverDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsSolver,
            !:SolverDefns = !.SolverDefns ++ [SolverDefnInfo]
        ;
            TypeDefn = parse_tree_eqv_type(DetailsEqv),
            EqvDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsEqv,
            !:EqvDefns = !.EqvDefns ++ [EqvDefnInfo]
        ;
            TypeDefn = parse_tree_du_type(DetailsDu),
            DuDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsDu,
            !:DuDefns = !.DuDefns ++ [DuDefnInfo]
        ;
            TypeDefn = parse_tree_sub_type(DetailsSub),
            SubDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsSub,
            !:SubDefns = !.SubDefns ++ [SubDefnInfo]
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            ForeignDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsForeign,
            DetailsForeign = type_details_foreign(LangType, _, _),
            (
                LangType = c(_),
                !:ForeignDefnsC = !.ForeignDefnsC ++ [ForeignDefnInfo]
            ;
                LangType = java(_),
                !:ForeignDefnsJava = !.ForeignDefnsJava ++ [ForeignDefnInfo]
            ;
                LangType = csharp(_),
                !:ForeignDefnsCsharp = !.ForeignDefnsCsharp ++
                    [ForeignDefnInfo]
            )
        ),
        AllDefns = type_ctor_all_defns(!.AbsSolverDefns, !.SolverDefns,
            !.AbsStdDefns, !.EqvDefns, !.DuDefns, !.SubDefns,
            c_java_csharp(!.ForeignDefnsC, !.ForeignDefnsJava,
                !.ForeignDefnsCsharp))
    ),
    map.set(TypeCtor, AllDefns, !TypeDefnMap).

inst_ctor_defn_items_to_map(InstDefnInfos) = InstDefnMap :-
    list.foldl(add_inst_defn_to_map, InstDefnInfos, map.init, InstDefnMap).

:- pred add_inst_defn_to_map(item_inst_defn_info::in,
    inst_ctor_defn_map::in, inst_ctor_defn_map::out) is det.

add_inst_defn_to_map(InstDefnInfo, !InstDefnMap) :-
    InstDefnInfo = item_inst_defn_info(SymName, Params, _MaybeForTypeCtor,
        MaybeAbstractInstDefn, _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    InstCtor = inst_ctor(SymName, Arity),
    ( if map.search(!.InstDefnMap, InstCtor, AllDefns0) then
        AllDefns0 = inst_ctor_all_defns(AbstractDefns0, EqvDefns0),
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefn = InstDefnInfo ^ id_inst_defn := no_inst_defn,
            AbstractDefns = [AbstractDefn | AbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns0)
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
            EqvDefn = InstDefnInfo ^ id_inst_defn := InstDefn,
            EqvDefns = [EqvDefn | EqvDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns0, EqvDefns)
        ),
        map.det_update(InstCtor, AllDefns, !InstDefnMap)
    else
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefn = InstDefnInfo ^ id_inst_defn := no_inst_defn,
            AllDefns = inst_ctor_all_defns([AbstractDefn], [])
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
            EqvDefn = InstDefnInfo ^ id_inst_defn := InstDefn,
            AllDefns = inst_ctor_all_defns([], [EqvDefn])
        ),
        map.det_insert(InstCtor, AllDefns, !InstDefnMap)
    ).

mode_ctor_defn_items_to_map(ModeDefnInfos) = ModeDefnMap :-
    list.foldl(add_mode_defn_to_map, ModeDefnInfos, map.init, ModeDefnMap).

:- pred add_mode_defn_to_map(item_mode_defn_info::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::out) is det.

add_mode_defn_to_map(ModeDefnInfo, !ModeDefnMap) :-
    ModeDefnInfo = item_mode_defn_info(SymName, Params, MaybeAbstractModeDefn,
        _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    ModeCtor = mode_ctor(SymName, Arity),
    ( if map.search(!.ModeDefnMap, ModeCtor, AllDefns0) then
        AllDefns0 = mode_ctor_all_defns(AbstractDefns0, EqvDefns0),
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefn = ModeDefnInfo ^ md_mode_defn := no_mode_defn,
            AbstractDefns = [AbstractDefn | AbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns0)
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
            EqvDefn = ModeDefnInfo ^ md_mode_defn := ModeDefn,
            EqvDefns = [EqvDefn | EqvDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns0, EqvDefns)
        ),
        map.det_update(ModeCtor, AllDefns, !ModeDefnMap)
    else
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefn = ModeDefnInfo ^ md_mode_defn := no_mode_defn,
            AllDefns = mode_ctor_all_defns([AbstractDefn], [])
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
            EqvDefn = ModeDefnInfo ^ md_mode_defn := ModeDefn,
            AllDefns = mode_ctor_all_defns([], [EqvDefn])
        ),
        map.det_insert(ModeCtor, AllDefns, !ModeDefnMap)
    ).

type_ctor_repn_items_to_map(TypeRepnInfos) = TypeRepnMap :-
    list.foldl(add_type_repn_to_map, TypeRepnInfos, map.init, TypeRepnMap).

:- pred add_type_repn_to_map(item_type_repn_info::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_type_repn_to_map(TypeRepnInfo, !TypeRepnMap) :-
    TypeRepnInfo = item_type_repn_info(SymName, Params, _TypeRepn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    % This could be a map.det_insert, because
    %
    % - we ensure the invariant that an interface file contains at most one
    %   type_repn item for a given type_ctor, and
    %
    % - source files should not contain any type_ctor items, because they are
    %   not a publicly documented part of the language.
    %
    % However, until we have code to filter out all type_repn items from
    % source files, they may contain duplicates, so we keep this as map.set
    % for now.
    map.set(TypeCtor, TypeRepnInfo, !TypeRepnMap).

type_ctor_foreign_enum_items_to_map(ForeignEnums) = ForeignEnumMap :-
    list.foldl(add_foreign_enum_item_to_map, ForeignEnums,
        map.init, ForeignEnumMap).

:- pred add_foreign_enum_item_to_map(item_foreign_enum_info::in,
    type_ctor_foreign_enum_map::in, type_ctor_foreign_enum_map::out) is det.

add_foreign_enum_item_to_map(ForeignEnumInfo, !ForeignEnumMap) :-
    ForeignEnumInfo = item_foreign_enum_info(Lang, TypeCtor, _Values, _, _),
    some [!ForeignEnumsC, !ForeignEnumsJava, !ForeignEnumsCsharp]
    (
        ( if map.search(!.ForeignEnumMap, TypeCtor, AllEnums0) then
            AllEnums0 = c_java_csharp(!:ForeignEnumsC,
                !:ForeignEnumsJava, !:ForeignEnumsCsharp)
        else
            !:ForeignEnumsC = [],
            !:ForeignEnumsJava = [],
            !:ForeignEnumsCsharp = []
        ),
        (
            Lang = lang_c,
            !:ForeignEnumsC = !.ForeignEnumsC ++ [ForeignEnumInfo]
        ;
            Lang = lang_java,
            !:ForeignEnumsJava = !.ForeignEnumsJava ++ [ForeignEnumInfo]
        ;
            Lang = lang_csharp,
            !:ForeignEnumsCsharp = !.ForeignEnumsCsharp ++ [ForeignEnumInfo]
        ),
        AllEnums = c_java_csharp(!.ForeignEnumsC,
            !.ForeignEnumsJava, !.ForeignEnumsCsharp),
        map.set(TypeCtor, AllEnums, !ForeignEnumMap)
    ).

%---------------------------------------------------------------------------%

type_ctor_defn_map_to_type_defns(TypeCtorDefnMap) = TypeDefns :-
    map.foldl_values(accumulate_type_ctor_defns, TypeCtorDefnMap,
        cord.init, TypeDefnsCord),
    TypeDefns = cord.list(TypeDefnsCord).

:- pred accumulate_type_ctor_defns(type_ctor_all_defns::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

accumulate_type_ctor_defns(CtorAllDefns, !TypeDefns) :-
    CtorAllDefns = type_ctor_all_defns(AbstractSolverDefns, SolverDefns,
        AbstractStdDefns, EqvDefns, DuDefns, SubDefns, CJCsEDefns),
    CJCsEDefns = c_java_csharp(ForeignDefnsC, ForeignDefnsJava,
        ForeignDefnsCsharp),
    !:TypeDefns = !.TypeDefns ++ cord.from_list(
        list.map(wrap_abstract_type_defn, at_most_one(AbstractSolverDefns)) ++
        list.map(wrap_solver_type_defn, SolverDefns) ++
        list.map(wrap_abstract_type_defn, at_most_one(AbstractStdDefns)) ++
        list.map(wrap_eqv_type_defn, EqvDefns) ++
        list.map(wrap_du_type_defn, DuDefns) ++
        list.map(wrap_sub_type_defn, SubDefns) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsC) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsJava) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsCsharp)).

:- func at_most_one(list(T)) = list(T).

at_most_one([]) = [].
at_most_one([X | _Xs]) = [X].

%---------------------%

inst_ctor_defn_map_to_inst_defns(InstCtorDefnMap) = InstDefns :-
    map.foldl_values(accumulate_inst_ctor_defns, InstCtorDefnMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord).

:- pred accumulate_inst_ctor_defns(inst_ctor_all_defns::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

accumulate_inst_ctor_defns(CtorAllDefns, !InstDefns) :-
    CtorAllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns),
    !:InstDefns = !.InstDefns ++
        cord.from_list(list.map(wrap_abstract_inst_defn, AbstractDefns)) ++
        cord.from_list(list.map(wrap_eqv_inst_defn, EqvDefns)).

%---------------------%

mode_ctor_defn_map_to_mode_defns(ModeCtorDefnMap) = ModeDefns :-
    map.foldl_values(accumulate_mode_ctor_defns, ModeCtorDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord).

:- pred accumulate_mode_ctor_defns(mode_ctor_all_defns::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

accumulate_mode_ctor_defns(CtorAllDefns, !ModeDefns) :-
    CtorAllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns),
    !:ModeDefns = !.ModeDefns ++
        cord.from_list(list.map(wrap_abstract_mode_defn, AbstractDefns)) ++
        cord.from_list(list.map(wrap_eqv_mode_defn, EqvDefns)).

%---------------------%

type_ctor_repn_map_to_type_repns(TypeCtorRepnMap) = TypeRepns :-
    map.foldl_values(accumulate_type_ctor_repns, TypeCtorRepnMap,
        cord.init, TypeRepnsCord),
    TypeRepns = cord.list(TypeRepnsCord).

:- pred accumulate_type_ctor_repns(item_type_repn_info::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

accumulate_type_ctor_repns(TypeRepn, !TypeRepns) :-
    !:TypeRepns = cord.snoc(!.TypeRepns, TypeRepn).

%---------------------------------------------------------------------------%

:- pred report_int_imp_fim(map(fim_spec, prog_context)::in,
    fim_spec::in,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_int_imp_fim(IntFIMSpecMap, FIMSpec, !ImpFIMSpecMap, !Specs) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    map.det_remove(FIMSpec, ImpContext, !ImpFIMSpecMap),
    map.lookup(IntFIMSpecMap, FIMSpec, IntContext),
    ImpPieces = [words("Warning: this"), pragma_decl("foreign_import_module"),
        words("pragma for"), qual_sym_name(ModuleName),
        words("and"), words(foreign_language_string(Lang)),
        words("in the implementation section is")] ++
        color_as_incorrect([words("redundant,")]) ++
        [words("because there is a"), pragma_decl("foreign_import_module"),
        words("pragma for the same module/language combination"),
        words("in the interface section."), nl],
    IntPieces = [words("The"), pragma_decl("foreign_import_module"),
        words("pragma in the interface section is here."), nl],
    ImpMsg = msg(ImpContext, ImpPieces),
    IntMsg = msg(IntContext, IntPieces),
    Spec = conditional_spec($pred, warn_simple_code, yes, severity_warning,
        phase_pt2h, [ImpMsg, IntMsg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred classify_src_items_in_blocks(list(raw_item_block)::in,
    list(item_include)::in, list(item_include)::out,
    list(item_avail)::in, list(item_avail)::out,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out,
    list(item_impl_marker_info)::in, list(item_impl_marker_info)::out,
    set(pred_pf_name_arity)::in, set(pred_pf_name_arity)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_initialise_info)::in, list(item_initialise_info)::out,
    list(item_finalise_info)::in, list(item_finalise_info)::out,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    implicit_avail_needs::in, implicit_avail_needs::out,
    set(foreign_language)::in, set(foreign_language)::out,
    list(item_include)::in, list(item_include)::out,
    list(item_avail)::in, list(item_avail)::out,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_clause_info)::in, list(item_clause_info)::out,
    list(item_foreign_proc_info)::in, list(item_foreign_proc_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::in,
        list(item_foreign_export_enum_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out,
    list(item_impl_marker_info)::in, list(item_impl_marker_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_initialise_info)::in, list(item_initialise_info)::out,
    list(item_finalise_info)::in, list(item_finalise_info)::out,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    implicit_avail_needs::in, implicit_avail_needs::out,
    set(foreign_language)::in, set(foreign_language)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_src_items_in_blocks([],
        !IntIncls, !IntAvails, !IntFIMSpecMap,
        !RevIntTypeDefns, !RevIntInstDefns, !RevIntModeDefns,
        !RevIntTypeClasses, !RevIntInstances,
        !RevIntPredDecls, !RevIntModeDecls,
        !RevIntDeclPragmas, !RevIntDeclMarkers,
        !RevIntImplPragmas, !RevIntImplMarkers,
        !IntBadClausePreds, !RevIntPromises,
        !RevIntInitialises, !RevIntFinalises, !RevIntMutables,
        !IntImplicitAvailNeeds, !IntSelfFIMLangs,
        !ImpIncls, !ImpAvails, !ImpFIMSpecMap,
        !RevImpTypeDefns, !RevImpInstDefns, !RevImpModeDefns,
        !RevImpTypeClasses, !RevImpInstances,
        !RevImpPredDecls, !RevImpModeDecls, !RevImpClauses,
        !RevImpForeignProcs, !RevImpForeignEnums, !RevImpForeignExportEnums,
        !RevImpDeclPragmas, !RevImpDeclMarkers,
        !RevImpImplPragmas, !RevImpImplMarkers,
        !RevImpPromises, !RevImpInitialises, !RevImpFinalises, !RevImpMutables,
        !ImpImplicitAvailNeeds, !ImpSelfFIMLangs, !Specs).
classify_src_items_in_blocks([ItemBlock | ItemBlocks],
        !IntIncls, !IntAvails, !IntFIMSpecMap,
        !RevIntTypeDefns, !RevIntInstDefns, !RevIntModeDefns,
        !RevIntTypeClasses, !RevIntInstances,
        !RevIntPredDecls, !RevIntModeDecls,
        !RevIntDeclPragmas, !RevIntDeclMarkers,
        !RevIntImplPragmas, !RevIntImplMarkers,
        !IntBadClausePreds, !RevIntPromises,
        !RevIntInitialises, !RevIntFinalises, !RevIntMutables,
        !IntImplicitAvailNeeds, !IntSelfFIMLangs,
        !ImpIncls, !ImpAvails, !ImpFIMSpecMap,
        !RevImpTypeDefns, !RevImpInstDefns, !RevImpModeDefns,
        !RevImpTypeClasses, !RevImpInstances,
        !RevImpPredDecls, !RevImpModeDecls, !RevImpClauses,
        !RevImpForeignProcs, !RevImpForeignEnums, !RevImpForeignExportEnums,
        !RevImpDeclPragmas, !RevImpDeclMarkers,
        !RevImpImplPragmas, !RevImpImplMarkers,
        !RevImpPromises, !RevImpInitialises, !RevImpFinalises, !RevImpMutables,
        !ImpImplicitAvailNeeds, !ImpSelfFIMLangs, !Specs) :-
    ItemBlock = item_block(_, Section, Incls, Avails, FIMs, Items),
    (
        Section = ms_interface,
        !:IntIncls = !.IntIncls ++ Incls,
        !:IntAvails = !.IntAvails ++ Avails,
        list.foldl2(classify_foreign_import_module, FIMs, !IntFIMSpecMap,
            !Specs),
        classify_src_items_int(Items,
            !RevIntTypeDefns, !RevIntInstDefns, !RevIntModeDefns,
            !RevIntTypeClasses, !RevIntInstances,
            !RevIntPredDecls, !RevIntModeDecls,
            !RevIntDeclPragmas, !RevIntDeclMarkers,
            !RevIntImplPragmas, !RevIntImplMarkers,
            !IntBadClausePreds, !RevIntPromises,
            !RevIntInitialises, !RevIntFinalises, !RevIntMutables,
            !IntImplicitAvailNeeds, !IntSelfFIMLangs, !Specs)
    ;
        Section = ms_implementation,
        !:ImpIncls = !.ImpIncls ++ Incls,
        !:ImpAvails = !.ImpAvails ++ Avails,
        list.foldl2(classify_foreign_import_module, FIMs, !ImpFIMSpecMap,
            !Specs),
        classify_src_items_imp(Items,
            !RevImpTypeDefns, !RevImpInstDefns, !RevImpModeDefns,
            !RevImpTypeClasses, !RevImpInstances,
            !RevImpPredDecls, !RevImpModeDecls, !RevImpClauses,
            !RevImpForeignProcs, !RevImpForeignEnums,
            !RevImpForeignExportEnums,
            !RevImpDeclPragmas, !RevImpDeclMarkers,
            !RevImpImplPragmas, !RevImpImplMarkers,
            !RevImpPromises, !RevImpInitialises, !RevImpFinalises,
            !RevImpMutables, !ImpImplicitAvailNeeds, !ImpSelfFIMLangs, !Specs)
    ),
    classify_src_items_in_blocks(ItemBlocks,
        !IntIncls, !IntAvails, !IntFIMSpecMap,
        !RevIntTypeDefns, !RevIntInstDefns, !RevIntModeDefns,
        !RevIntTypeClasses, !RevIntInstances,
        !RevIntPredDecls, !RevIntModeDecls,
        !RevIntDeclPragmas, !RevIntDeclMarkers,
        !RevIntImplPragmas, !RevIntImplMarkers,
        !IntBadClausePreds, !RevIntPromises,
        !RevIntInitialises, !RevIntFinalises, !RevIntMutables,
        !IntImplicitAvailNeeds, !IntSelfFIMLangs,
        !ImpIncls, !ImpAvails, !ImpFIMSpecMap,
        !RevImpTypeDefns, !RevImpInstDefns, !RevImpModeDefns,
        !RevImpTypeClasses, !RevImpInstances,
        !RevImpPredDecls, !RevImpModeDecls, !RevImpClauses,
        !RevImpForeignProcs, !RevImpForeignEnums, !RevImpForeignExportEnums,
        !RevImpDeclPragmas, !RevImpDeclMarkers,
        !RevImpImplPragmas, !RevImpImplMarkers,
        !RevImpPromises, !RevImpInitialises, !RevImpFinalises, !RevImpMutables,
        !ImpImplicitAvailNeeds, !ImpSelfFIMLangs, !Specs).

:- pred classify_foreign_import_module(item_fim::in,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_foreign_import_module(ItemFIM, !FIMSpecMap, !Specs) :-
    ItemFIM = item_fim(Lang, ModuleName, Context, _SeqNum),
    FIMSpec = fim_spec(Lang, ModuleName),
    ( if map.search(!.FIMSpecMap, FIMSpec, PrevContext) then
        MainPieces = [words("Warning:")] ++
            color_as_incorrect([words("duplicate")]) ++
            [pragma_decl("foreign_import_module"),
            words("pragma for")] ++
            color_as_subject([qual_sym_name(ModuleName)]) ++
            [words("and")] ++
            color_as_subject([words(foreign_language_string(Lang)),
                suffix(".")]) ++
            [nl],
        MainMsg = msg(Context, MainPieces),
        PrevPieces = [words("The previous"),
            pragma_decl("foreign_import_module"),
            words("pragma for the same module/language combination"),
            words("is here."), nl],
        PrevMsg = msg(PrevContext, PrevPieces),
        Spec = conditional_spec($pred, warn_simple_code, yes,
            severity_warning, phase_pt2h, [MainMsg, PrevMsg]),
        !:Specs = [Spec | !.Specs]
    else
        map.det_insert(FIMSpec, Context, !FIMSpecMap)
    ).

:- pred classify_src_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out,
    list(item_impl_marker_info)::in, list(item_impl_marker_info)::out,
    set(pred_pf_name_arity)::in, set(pred_pf_name_arity)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_initialise_info)::in, list(item_initialise_info)::out,
    list(item_finalise_info)::in, list(item_finalise_info)::out,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    implicit_avail_needs::in, implicit_avail_needs::out,
    set(foreign_language)::in, set(foreign_language)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_src_items_int([],
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances, !RevPredDecls, !RevModeDecls,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !BadClausePreds, !RevPromises,
        !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs).
classify_src_items_int([Item | Items],
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances, !RevPredDecls, !RevModeDecls,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !BadClausePreds, !RevPromises,
        !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        !:RevTypeDefns = [ItemTypeDefnInfo | !.RevTypeDefns],
        ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
        (
            ( TypeDefn = parse_tree_abstract_type(_)
            ; TypeDefn = parse_tree_du_type(_)
            ; TypeDefn = parse_tree_sub_type(_)
            ; TypeDefn = parse_tree_eqv_type(_)
            )
        ;
            TypeDefn = parse_tree_solver_type(DetailsSolver),
            % XXX IMPLICIT None of the implicit avail needs this call looks for
            % has any business occurring in a solver type.
            acc_implicit_avail_needs_solver_type(DetailsSolver,
                !ImplicitAvailNeeds)
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _),
            set.insert(foreign_type_language(ForeignType), !SelfFIMLangs)
        )
    ;
        Item = item_inst_defn(ItemInstDefnInfo),
        !:RevInstDefns = [ItemInstDefnInfo | !.RevInstDefns]
    ;
        Item = item_mode_defn(ItemModeDefnInfo),
        !:RevModeDefns = [ItemModeDefnInfo | !.RevModeDefns]
    ;
        Item = item_typeclass(ItemTypeclassInfo),
        !:RevTypeClasses = [ItemTypeclassInfo | !.RevTypeClasses]
    ;
        Item = item_instance(ItemInstanceInfo),
        InstanceBody = ItemInstanceInfo ^ ci_method_instances,
        (
            InstanceBody = instance_body_abstract
        ;
            InstanceBody = instance_body_concrete(InstanceMethods),
            list.foldl(acc_implicit_avail_needs_in_instance_method,
                InstanceMethods, !ImplicitAvailNeeds),

            AlwaysPieces =
                [words("Error: non-abstract instance declaration")] ++
                color_as_incorrect([words("in module interface.")]) ++
                [nl],
            VerbosePieces = [words("If you intend to export this instance,"),
                words("then move this declaration"),
                words("to the implementation section,"),
                words("replacing it in the interface section"),
                words("with its abstract version, which omits"),
                words("the"), quote("where [...]"), words("part."), nl],
            Msg = simple_msg(ItemInstanceInfo ^ ci_context,
                [always(AlwaysPieces),
                verbose_only(verbose_once, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
            !:Specs = [Spec | !.Specs]
        ),
        !:RevInstances = [ItemInstanceInfo | !.RevInstances]
    ;
        Item = item_pred_decl(ItemPredDeclInfo),
        !:RevPredDecls = [ItemPredDeclInfo | !.RevPredDecls]
    ;
        Item = item_mode_decl(ItemModeDeclInfo),
        !:RevModeDecls = [ItemModeDeclInfo | !.RevModeDecls]
    ;
        Item = item_clause(ItemClauseInfo),
        ItemClauseInfo = item_clause_info(PredOrFunc, PredSymName, ArgTerms,
            _VarSet, _Body, Context, _SeqNum),
        PredFormArity = arg_list_arity(ArgTerms),
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        PredPfNameArity =
            pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
        Pieces = [words("Error:")] ++
            color_as_subject([words("clauses,")]) ++
            [words("such as this one for"),
            % There is no point printing out the qualified name,
            % since the module name is implicit in the context.
            unqual_pf_sym_name_user_arity(PredPfNameArity), suffix(","),
            words("are")] ++
            color_as_incorrect([words("not allowed in module interfaces.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs],
        set.insert(PredPfNameArity, !BadClausePreds)
    ;
        Item = item_foreign_proc(ItemForeignProcInfo),
        error_item_is_exported(Item, !Specs),
        ItemForeignProcInfo = item_foreign_proc_info(_, SymName, PredOrFunc,
            Vars, _, _, _, _, _),
        list.length(Vars, Arity),
        user_arity_pred_form_arity(PredOrFunc, UserArity,
            pred_form_arity(Arity)),
        PredPfNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
        set.insert(PredPfNameArity, !BadClausePreds)
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        !:RevDeclPragmas = [ItemDeclPragma | !.RevDeclPragmas]
    ;
        Item = item_decl_marker(ItemDeclMarker),
        !:RevDeclMarkers = [ItemDeclMarker | !.RevDeclMarkers]
    ;
        Item = item_impl_pragma(ItemImplPragma),
        error_item_is_exported(Item, !Specs),
        !:RevImplPragmas = [ItemImplPragma | !.RevImplPragmas],
        (
            ItemImplPragma = impl_pragma_external_proc(ExternalProc),
            ExternalProc =
                impl_pragma_external_proc_info(PredPfNameArity, _, _, _),
            set.insert(PredPfNameArity, !BadClausePreds)
        ;
            ItemImplPragma = impl_pragma_fact_table(FactTable),
            FactTable = impl_pragma_fact_table_info(PredSpec, _FileName, _, _),
            PredSpec = pred_pfu_name_arity(PFU, SymName, UserArity),
            (
                PFU = pfu_predicate,
                PredPfNameArity =
                    pred_pf_name_arity(pf_predicate, SymName, UserArity),
                set.insert(PredPfNameArity, !BadClausePreds)
            ;
                PFU = pfu_function,
                PredPfNameArity =
                    pred_pf_name_arity(pf_function, SymName, UserArity),
                set.insert(PredPfNameArity, !BadClausePreds)
            ;
                PFU = pfu_unknown
                % If a predicate named e.g. foo/N has a fact table pragma
                % for it, but due to a bug the pragma is in the interface
                % section, then generating an error message about the
                % absence of clauses for predicate foo/N will be misleading.
                % However, we cannot add foo/N to !BadClausePreds without
                % knowing whether the pragma is for the predicate foo/N
                % or the function foo/N, and if we got here, then we do not
                % know. If the module we are compiling declares both
                % a predicate foo/N and a function foo/N, has no clauses
                % for either of them in the implementation, but has an
                % (invalid) fact_table pragma for just one of them in the
                % interface, we have no way of generating an error message
                % about the missing clauses for *just* the other; we have
                % generate that error message either for both, or for neither.
                % By doing nothing here, we choose generating a message
                % for both. We know one will be misleading, we just don't know
                % which one ;-(
            )
        ;
            ( ItemImplPragma = impl_pragma_foreign_decl(_)
            ; ItemImplPragma = impl_pragma_foreign_code(_)
            ; ItemImplPragma = impl_pragma_fproc_export(_)
            ; ItemImplPragma = impl_pragma_tabled(_)
            ; ItemImplPragma = impl_pragma_req_tail_rec(_)
            ; ItemImplPragma = impl_pragma_req_feature_set(_)
            )
        )
    ;
        Item = item_impl_marker(ItemImplMarker),
        error_item_is_exported(Item, !Specs),
        !:RevImplMarkers = [ItemImplMarker | !.RevImplMarkers]
    ;
        Item = item_generated_pragma(_),
        report_forbidden_item_in_src(Item, !Specs)
    ;
        Item = item_promise(ItemPromiseInfo),
        % XXX IMPLICIT None of the implicit avail needs this call looks for
        % has any business occurring in a promise.
        acc_implicit_avail_needs_in_promise(ItemPromiseInfo,
            !ImplicitAvailNeeds),
        !:RevPromises = [ItemPromiseInfo | !.RevPromises]
    ;
        ( Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ),
        error_item_is_exported(Item, !Specs)
    ;
        Item = item_initialise(ItemInitialiseInfo),
        error_item_is_exported(Item, !Specs),
        !:RevInitialises = [ItemInitialiseInfo | !.RevInitialises]
    ;
        Item = item_finalise(ItemFinaliseInfo),
        error_item_is_exported(Item, !Specs),
        !:RevFinalises = [ItemFinaliseInfo | !.RevFinalises]
    ;
        Item = item_mutable(ItemMutableInfo),
        error_item_is_exported(Item, !Specs),
        !:RevMutables = [ItemMutableInfo | !.RevMutables]
    ;
        Item = item_type_repn(_),
        report_forbidden_item_in_src(Item, !Specs)
    ),
    classify_src_items_int(Items,
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances, !RevPredDecls, !RevModeDecls,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !BadClausePreds, !RevPromises,
        !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs).

:- pred classify_src_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_clause_info)::in, list(item_clause_info)::out,
    list(item_foreign_proc_info)::in, list(item_foreign_proc_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::in,
        list(item_foreign_export_enum_info)::out,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_decl_marker_info)::in, list(item_decl_marker_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out,
    list(item_impl_marker_info)::in, list(item_impl_marker_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_initialise_info)::in, list(item_initialise_info)::out,
    list(item_finalise_info)::in, list(item_finalise_info)::out,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    implicit_avail_needs::in, implicit_avail_needs::out,
    set(foreign_language)::in, set(foreign_language)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_src_items_imp([],
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances,
        !RevPredDecls, !RevModeDecls, !RevClauses,
        !RevForeignProcs, !RevForeignEnums, !RevForeignExportEnums,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !RevPromises, !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs).
classify_src_items_imp([Item | Items],
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances,
        !RevPredDecls, !RevModeDecls, !RevClauses,
        !RevForeignProcs, !RevForeignEnums, !RevForeignExportEnums,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !RevPromises, !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        !:RevTypeDefns = [ItemTypeDefnInfo | !.RevTypeDefns],
        ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
        (
            ( TypeDefn = parse_tree_abstract_type(_)
            ; TypeDefn = parse_tree_du_type(_)
            ; TypeDefn = parse_tree_sub_type(_)
            ; TypeDefn = parse_tree_eqv_type(_)
            )
        ;
            TypeDefn = parse_tree_solver_type(DetailsSolver),
            % XXX IMPLICIT None of the implicit avail needs this call looks for
            % has any business occurring in a solver type.
            acc_implicit_avail_needs_solver_type(DetailsSolver,
                !ImplicitAvailNeeds)
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _),
            set.insert(foreign_type_language(ForeignType), !SelfFIMLangs)
        )
    ;
        Item = item_inst_defn(ItemInstDefnInfo),
        !:RevInstDefns = [ItemInstDefnInfo | !.RevInstDefns]
    ;
        Item = item_mode_defn(ItemModeDefnInfo),
        !:RevModeDefns = [ItemModeDefnInfo | !.RevModeDefns]
    ;
        Item = item_typeclass(ItemTypeclassInfo),
        !:RevTypeClasses = [ItemTypeclassInfo | !.RevTypeClasses]
    ;
        Item = item_instance(ItemInstanceInfo),
        acc_implicit_avail_needs_in_instance(ItemInstanceInfo,
            !ImplicitAvailNeeds),
        !:RevInstances = [ItemInstanceInfo | !.RevInstances]
    ;
        Item = item_pred_decl(ItemPredDeclInfo),
        !:RevPredDecls = [ItemPredDeclInfo | !.RevPredDecls]
    ;
        Item = item_mode_decl(ItemModeDeclInfo),
        !:RevModeDecls = [ItemModeDeclInfo | !.RevModeDecls]
    ;
        Item = item_clause(ItemClauseInfo),
        acc_implicit_avail_needs_in_clause(ItemClauseInfo,
            !ImplicitAvailNeeds),
        !:RevClauses = [ItemClauseInfo |  !.RevClauses]
    ;
        Item = item_foreign_proc(ItemForeignProcInfo),
        ItemForeignProcInfo =
            item_foreign_proc_info(Attrs, _, _, _, _, _, _, _, _),
        set.insert(get_foreign_language(Attrs), !SelfFIMLangs),
        !:RevForeignProcs = [ItemForeignProcInfo | !.RevForeignProcs]
    ;
        Item = item_foreign_enum(ItemForeignEnumInfo),
        ItemForeignEnumInfo = item_foreign_enum_info(Lang, _, _, _, _),
        set.insert(Lang, !SelfFIMLangs),
        !:RevForeignEnums = [ItemForeignEnumInfo | !.RevForeignEnums]
    ;
        Item = item_foreign_export_enum(ItemFEEInfo),
        !:RevForeignExportEnums = [ItemFEEInfo | !.RevForeignExportEnums]
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        !:RevDeclPragmas = [ItemDeclPragma | !.RevDeclPragmas]
    ;
        Item = item_decl_marker(ItemDeclMarker),
        !:RevDeclMarkers = [ItemDeclMarker | !.RevDeclMarkers]
    ;
        Item = item_impl_pragma(ItemImplPragma),
        !:RevImplPragmas = [ItemImplPragma | !.RevImplPragmas],
        (
            (
                ItemImplPragma = impl_pragma_foreign_code(FCInfo),
                FCInfo = impl_pragma_foreign_code_info(Lang, _, _, _)
            ;
                ItemImplPragma = impl_pragma_foreign_decl(FDInfo),
                FDInfo = impl_pragma_foreign_decl_info(Lang, _, _, _, _)
            ),
            set.insert(Lang, !SelfFIMLangs)
        ;
            ItemImplPragma = impl_pragma_fproc_export(FPEInfo),
            FPEInfo = impl_pragma_fproc_export_info(_, Lang, _, _, _, _, _),
            set.insert(Lang, !SelfFIMLangs)
        ;
            ItemImplPragma = impl_pragma_tabled(TableInfo),
            TableInfo = impl_pragma_tabled_info(_, _, MaybeAttributes, _, _),
            !ImplicitAvailNeeds ^ ian_tabling := do_need_tabling,
            (
                MaybeAttributes = no
            ;
                MaybeAttributes = yes(Attributes),
                StatsAttr = Attributes ^ table_attr_statistics,
                (
                    StatsAttr = table_gather_statistics,
                    !ImplicitAvailNeeds ^ ian_tabling_statistics
                        := do_need_tabling_statistics
                ;
                    StatsAttr = table_do_not_gather_statistics
                )
            )
        ;
            ( ItemImplPragma = impl_pragma_external_proc(_)
            ; ItemImplPragma = impl_pragma_fact_table(_)
            ; ItemImplPragma = impl_pragma_req_tail_rec(_)
            ; ItemImplPragma = impl_pragma_req_feature_set(_)
            )
        )
    ;
        Item = item_impl_marker(ItemImplMarker),
        !:RevImplMarkers = [ItemImplMarker | !.RevImplMarkers]
    ;
        Item = item_generated_pragma(_),
        report_forbidden_item_in_src(Item, !Specs)
    ;
        Item = item_promise(ItemPromiseInfo),
        % XXX IMPLICIT None of the implicit avail needs this call looks for
        % has any business occurring in a promise.
        acc_implicit_avail_needs_in_promise(ItemPromiseInfo,
            !ImplicitAvailNeeds),
        !:RevPromises = [ItemPromiseInfo | !.RevPromises]
    ;
        Item = item_initialise(ItemInitialiseInfo),
        !:RevInitialises = [ItemInitialiseInfo | !.RevInitialises]
    ;
        Item = item_finalise(ItemFinaliseInfo),
        !:RevFinalises = [ItemFinaliseInfo | !.RevFinalises]
    ;
        Item = item_mutable(ItemMutableInfo),
        set.insert_list(all_foreign_languages, !SelfFIMLangs),
        acc_implicit_avail_needs_in_mutable(ItemMutableInfo,
            !ImplicitAvailNeeds),
        !:RevMutables = [ItemMutableInfo | !.RevMutables]
    ;
        Item = item_type_repn(_),
        report_forbidden_item_in_src(Item, !Specs)
    ),
    classify_src_items_imp(Items,
        !RevTypeDefns, !RevInstDefns, !RevModeDefns,
        !RevTypeClasses, !RevInstances,
        !RevPredDecls, !RevModeDecls, !RevClauses,
        !RevForeignProcs, !RevForeignEnums, !RevForeignExportEnums,
        !RevDeclPragmas, !RevDeclMarkers,
        !RevImplPragmas, !RevImplMarkers,
        !RevPromises, !RevInitialises, !RevFinalises, !RevMutables,
        !ImplicitAvailNeeds, !SelfFIMLangs, !Specs).

:- pred acc_implicit_avail_needs_solver_type(type_details_solver::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_solver_type(DetailsSolver, !ImplicitAvailNeeds) :-
    DetailsSolver = type_details_solver(SolverTypeDetails,
        _MaybeUnifyComparePredNames),
    SolverTypeDetails = solver_type_details(_RepresentationType,
        _GroundInst, _AnyInst, MutableItems),
    list.foldl(acc_implicit_avail_needs_in_mutable, MutableItems,
        !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_promise(item_promise_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_promise(ItemPromiseInfo, !ImplicitAvailNeeds) :-
    ItemPromiseInfo = item_promise_info(_PromiseType, Goal, _VarSet,
        _UnivQuantVars, _Context, _SeqNum),
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds).

%---------------------------------------------------------------------------%

:- pred error_item_is_exported(item::in,
    list(error_spec)::in, list(error_spec)::out) is det.

error_item_is_exported(Item, !Specs) :-
    error_is_exported(get_item_context(Item), items_desc_pieces(Item), !Specs).

    % Emit an error reporting that something should not have occurred in
    % a module interface.
    %
:- pred error_is_exported(prog_context::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

error_is_exported(Context, DescPieces, !Specs) :-
    Pieces = [words("Error:")] ++
        color_as_subject(DescPieces) ++
        [words("are")] ++
        color_as_incorrect([words("not allowed in module interfaces.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred accumulate_uses_maps(list(avail_use_info)::in,
    module_names_contexts::in, module_names_contexts::out) is det.

accumulate_uses_maps([], !UsesMap).
accumulate_uses_maps([Use | Uses], !UseMap) :-
    Use = avail_use_info(ModuleName, Context, _),
    one_or_more_map.add(ModuleName, Context, !UseMap),
    accumulate_uses_maps(Uses, !UseMap).

%---------------------------------------------------------------------------%

:- pred restrict_to_section_use_map_entry(string::in,
    module_name::in, section_import_and_or_use::in,
    section_use_map::in, section_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

restrict_to_section_use_map_entry(Extension, ModuleName, SectionImportAndOrUse,
        !SectionUseMap, !Specs) :-
    (
        ( SectionImportAndOrUse = int_import(Context)
        ; SectionImportAndOrUse = imp_import(Context)
        ; SectionImportAndOrUse = int_use_imp_import(_, Context)
        ),
        report_forbidden_avail(Extension, "import_module", no, Context, !Specs)
    ;
        (
            SectionImportAndOrUse = int_use(Context),
            SectionUse = int_use(Context)
        ;
            SectionImportAndOrUse = imp_use(Context),
            SectionUse = imp_use(Context)
        ),
        map.det_insert(ModuleName, SectionUse, !SectionUseMap)
    ).

:- pred restrict_to_int_import_map_entry(string::in,
    module_name::in, section_import_and_or_use::in,
    int_import_map::in, int_import_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

restrict_to_int_import_map_entry(Extension, ModuleName, SectionImportAndOrUse,
        !IntImportMap, !Specs) :-
    (
        SectionImportAndOrUse = int_import(Context),
        IntImport = int_import(Context),
        map.det_insert(ModuleName, IntImport, !IntImportMap)
    ;
        SectionImportAndOrUse = imp_import(Context),
        report_forbidden_avail(Extension, "import_module",
            yes("implementation"), Context, !Specs)
    ;
        ( SectionImportAndOrUse = int_use(Context)
        ; SectionImportAndOrUse = imp_use(Context)
        ),
        report_forbidden_avail(Extension, "use_module", no, Context, !Specs)
    ;
        SectionImportAndOrUse = int_use_imp_import(IntContext, ImpContext),
        report_forbidden_avail(Extension, "use_module",
            no, IntContext, !Specs),
        report_forbidden_avail(Extension, "import_module",
            yes("implementation"), ImpContext, !Specs)
    ).

%---------------------%

:- pred report_forbidden_avail(string::in, string::in, maybe(string)::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_forbidden_avail(Extension, Decl, MaybeSection, Context, !Specs) :-
    (
        MaybeSection = no,
        Pieces = [words("A"), words(Extension), words("file may not contain"),
            words("any"), decl(Decl), words("declarations."), nl]
    ;
        MaybeSection = yes(Section),
        Pieces = [words("A"), words(Extension), words("file may not contain"),
            words("any"), decl(Decl), words("declarations"),
            words("in its"), words(Section), words("section."), nl]
    ),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_forbidden_item_in_src(item::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_forbidden_item_in_src(Item, !Specs) :-
    Pieces = [words("A Mercury source file")] ++
        color_as_incorrect([words("may not contain")] ++
            items_desc_pieces(Item) ++ [suffix(".")]) ++
        [nl],
    Context = get_item_context(Item),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.convert_parse_tree.
%---------------------------------------------------------------------------%
