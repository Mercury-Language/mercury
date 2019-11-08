%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
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

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

    % Convert from the interface-file-kind specific parse trees
    % to the generic interface file parse tree. These conversions go
    % from more restrictive to less restrictive, so they always
    % succeed without any problems.
    %
:- func convert_parse_tree_int0_to_int(parse_tree_int0) = parse_tree_int.
:- func convert_parse_tree_int1_to_int(parse_tree_int1) = parse_tree_int.
:- func convert_parse_tree_int2_to_int(parse_tree_int2) = parse_tree_int.
:- func convert_parse_tree_int3_to_int(parse_tree_int3) = parse_tree_int.

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

    % Convert from the optimization-file-kind specific parse trees
    % to the generic optimization file parse tree. These conversions go
    % from more restrictive to less restrictive, so they always
    % succeed without any problems.
    %
:- func convert_parse_tree_plain_opt_to_opt(parse_tree_plain_opt)
    = parse_tree_opt.
:- func convert_parse_tree_trans_opt_to_opt(parse_tree_trans_opt)
    = parse_tree_opt.

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

:- func wrap_include(module_name) = item_include.
:- func wrap_import_avail(module_name) = item_avail.
:- func wrap_use_avail(module_name) = item_avail.
:- func wrap_import(module_name) = avail_import_info.
:- func wrap_use(module_name) = avail_use_info.

:- func wrap_type_defn_item(item_type_defn_info) = item.
:- func wrap_inst_defn_item(item_inst_defn_info) = item.
:- func wrap_mode_defn_item(item_mode_defn_info) = item.
:- func wrap_typeclass_item(item_typeclass_info) = item.
:- func wrap_instance_item(item_instance_info) = item.
:- func wrap_pred_decl_item(item_pred_decl_info) = item.
:- func wrap_mode_decl_item(item_mode_decl_info) = item.
:- func wrap_foreign_enum_item(item_foreign_enum_info) = item.
:- func wrap_pragma_item(item_pragma_info) = item.
:- func wrap_promise_item(item_promise_info) = item.
:- func wrap_type_repn_item(item_type_repn_info) = item.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module recompilation.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module parse_tree.check_parse_tree_type_defns.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

convert_parse_tree_int0_to_int(ParseTreeInt0) = ParseTreeInt :-
    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntImportedModuleNames, IntUsedModuleNames,
        ImpImportedModuleNames, ImpUsedModuleNames,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpPragmas, ImpPromises),
    IntIncls = list.map(wrap_include, set.to_sorted_list(IntInclModuleNames)),
    ImpIncls = list.map(wrap_include, set.to_sorted_list(ImpInclModuleNames)),
    IntImportAvails = list.map(wrap_import_avail,
        set.to_sorted_list(IntImportedModuleNames)),
    IntUseAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    ImpImportAvails = list.map(wrap_import_avail,
        set.to_sorted_list(ImpImportedModuleNames)),
    ImpUseAvails = list.map(wrap_use_avail,
        set.to_sorted_list(ImpUsedModuleNames)),
    IntAvails = IntImportAvails ++ IntUseAvails,
    ImpAvails = ImpImportAvails ++ ImpUseAvails,

    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),
    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        list.map(wrap_pred_decl_item, IntPredDecls) ++
        list.map(wrap_mode_decl_item, IntModeDecls) ++
        type_ctor_foreign_enum_map_to_items(IntForeignEnumMap) ++
        list.map(wrap_pragma_item, IntPragmas) ++
        list.map(wrap_promise_item, IntPromises),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap) ++
        inst_ctor_defn_map_to_items(ImpInstDefnMap) ++
        mode_ctor_defn_map_to_items(ImpModeDefnMap) ++
        list.map(wrap_typeclass_item, ImpTypeClasses) ++
        list.map(wrap_instance_item, ImpInstances) ++
        list.map(wrap_pred_decl_item, ImpPredDecls) ++
        list.map(wrap_mode_decl_item, ImpModeDecls) ++
        type_ctor_foreign_enum_map_to_items(ImpForeignEnumMap) ++
        list.map(wrap_pragma_item, ImpPragmas) ++
        list.map(wrap_promise_item, ImpPromises),

    ParseTreeInt = parse_tree_int(ModuleName, ifk_int0, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems).

convert_parse_tree_int1_to_int(ParseTreeInt1) = ParseTreeInt :-
    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntUsedModuleNames, ImpUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses),

    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    ImpIncls = list.map(wrap_include,
        set.to_sorted_list(ImpInclModuleNames)),
    IntAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    ImpAvails = list.map(wrap_use_avail,
        set.to_sorted_list(ImpUsedModuleNames)),
    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),

    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        list.map(wrap_pred_decl_item, IntPredDecls) ++
        list.map(wrap_mode_decl_item, IntModeDecls) ++
        type_ctor_foreign_enum_map_to_items(IntForeignEnumMap) ++
        list.map(wrap_pragma_item, IntPragmas) ++
        list.map(wrap_promise_item, IntPromises) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap) ++
        type_ctor_foreign_enum_map_to_items(ImpForeignEnumMap) ++
        list.map(wrap_typeclass_item, ImpTypeClasses),

    ParseTreeInt = parse_tree_int(ModuleName, ifk_int1, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems).

convert_parse_tree_int2_to_int(ParseTreeInt2) = ParseTreeInt :-
    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        MaybeVersionNumbers,
        IntInclModuleNames, IntUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap),

    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    IntAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),

    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap),
    ParseTreeInt = parse_tree_int(ModuleName, ifk_int2, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [],
        IntFIMs, ImpFIMs, IntItems, ImpItems).

convert_parse_tree_int3_to_int(ParseTreeInt3) = ParseTreeInt :-
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),

    MaybeVersionNumbers = no_version_numbers,
    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    IntAvails = list.map(wrap_import_avail,
        set.to_sorted_list(IntImportModuleNames)),
    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ParseTreeInt = parse_tree_int(ModuleName, ifk_int3, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [],
        [], [], IntItems, []).

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int0(ParseTreeInt, ParseTreeInt0, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int0), $pred,
        "trying to convert non-ifk_int0 parse_tree_int to parse_tree_int0"),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    list.foldl(add_included_module_name, ImpIncls,
        set.init, ImpInclModuleNames),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    avail_imports_uses(ImpAvails, ImpImports, ImpUses),
    IntImportedModules = list.map(get_import_module_name, IntImports),
    ImpImportedModules = list.map(get_import_module_name, ImpImports),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    ImpUsedModules = list.map(get_use_module_name, ImpUses),
    set.list_to_set(IntImportedModules, IntImportedModuleNames),
    set.list_to_set(ImpImportedModules, ImpImportedModuleNames),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    set.list_to_set(ImpUsedModules, ImpUsedModuleNames),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int0_items_int_or_imp(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], IntForeignEnums, [], IntPragmas0, [], IntPromises0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    IntForeignEnumMap = type_ctor_foreign_enum_items_to_map(IntForeignEnums),
    list.sort(IntPragmas0, IntPragmas),
    list.sort(IntPromises0, IntPromises),

    classify_int0_items_int_or_imp(ImpItems, [], ImpTypeDefns,
        [], ImpInstDefns, [], ImpModeDefns,
        [], ImpTypeClasses0, [], ImpInstances0,
        [], ImpPredDecls0, [], RevImpModeDecls,
        [], ImpForeignEnums, [], ImpPragmas0, [], ImpPromises0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns),
    ImpInstDefnMap = inst_ctor_defn_items_to_map(ImpInstDefns),
    ImpModeDefnMap = mode_ctor_defn_items_to_map(ImpModeDefns),
    list.sort(ImpTypeClasses0, ImpTypeClasses),
    list.sort(ImpInstances0, ImpInstances),
    list.sort(ImpPredDecls0, ImpPredDecls),
    list.reverse(RevImpModeDecls, ImpModeDecls),
    ImpForeignEnumMap = type_ctor_foreign_enum_items_to_map(ImpForeignEnums),
    list.sort(ImpPragmas0, ImpPragmas),
    list.sort(ImpPromises0, ImpPromises),

    % We want only the error messages.
    create_type_ctor_checked_map(do_not_insist_on_defn, ModuleName,
        IntTypeDefnMap, ImpTypeDefnMap, IntForeignEnumMap, ImpForeignEnumMap,
        _TypeDefnCheckedMap, !Specs),

    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntImportedModuleNames, IntUsedModuleNames,
        ImpImportedModuleNames, ImpUsedModuleNames,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpPragmas, ImpPromises).

:- pred classify_int0_items_int_or_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_pragma_info)::in, list(item_pragma_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int0_items_int_or_imp([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs).
classify_int0_items_int_or_imp([Item | Items], !TypeDefns,
        !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs) :-
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
        !:Instances = [ItemInstance | !.Instances]
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
        Item = item_pragma(ItemPragma),
        !:Pragmas = [ItemPragma | !.Pragmas]
    ;
        Item = item_promise(ItemPromise),
        !:Promises = [ItemPromise | !.Promises]
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int0 file may not contain")] ++
            item_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int0_items_int_or_imp(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs).

%---------------------------------------------------------------------------%

check_convert_parse_tree_int_to_int1(ParseTreeInt, ParseTreeInt1, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int1), $pred,
        "trying to convert non-ifk_int1 parse_tree_int to parse_tree_int1"),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    list.foldl(add_included_module_name, ImpIncls,
        set.init, ImpInclModuleNames),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    avail_imports_uses(ImpAvails, ImpImports, ImpUses),
    IntIntImportContexts = list.map(get_import_context,
        IntImports ++ ImpImports),
    (
        IntIntImportContexts = []
    ;
        IntIntImportContexts = [FirstImportContext | _],
        IntImportPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("declarations."), nl],
        IntImportSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstImportContext, [always(IntImportPieces)])]),
        !:Specs = [IntImportSpec | !.Specs]
    ),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    ImpUsedModules = list.map(get_use_module_name, ImpUses),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    set.list_to_set(ImpUsedModules, ImpUsedModuleNames),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int1_items_int(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], IntForeignEnums, [], IntPragmas0, [], IntPromises0,
        [], IntTypeRepns, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    IntForeignEnumMap = type_ctor_foreign_enum_items_to_map(IntForeignEnums),
    list.sort(IntPragmas0, IntPragmas),
    list.sort(IntPromises0, IntPromises),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns),

    classify_int1_items_imp(ImpItems, [], ImpTypeDefns0,
        [], ImpForeignEnums0, [], ImpTypeClasses0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns0),
    ImpForeignEnumMap = type_ctor_foreign_enum_items_to_map(ImpForeignEnums0),
    list.sort(ImpTypeClasses0, ImpTypeClasses),

    % We want only the error messages.
    create_type_ctor_checked_map(do_not_insist_on_defn, ModuleName,
        IntTypeDefnMap, ImpTypeDefnMap, IntForeignEnumMap, ImpForeignEnumMap,
        _TypeDefnCheckedMap, !Specs),

    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntUsedModuleNames, ImpUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses).

:- pred classify_int1_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_pragma_info)::in, list(item_pragma_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs).
classify_int1_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs) :-
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
        !:Instances = [ItemInstance | !.Instances]
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
        Item = item_pragma(ItemPragma),
        !:Pragmas = [ItemPragma | !.Pragmas]
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
            Pieces = [words("A .int1 file may not contain")] ++
                item_desc_pieces(Item) ++
                [words("in its interface section."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int1 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int1_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs).

:- pred classify_int1_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::in, list(item_foreign_enum_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_imp([], !TypeDefns, !ForeignEnums, !TypeClasses, !Specs).
classify_int1_items_imp([Item | Items], !TypeDefns, !ForeignEnums,
        !TypeClasses, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        !:ForeignEnums = [ItemForeignEnum | !.ForeignEnums]
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
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

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    (
        ImpIncls = []
    ;
        ImpIncls = [FirstImpIncl | _],
        ImpInclPieces = [words("A .int2 file may not contain any"),
            decl("include_module"), words("declarations"),
            words("in its implementation section."), nl],
        ImpInclSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstImpIncl ^ incl_context,
                [always(ImpInclPieces)])]),
        !:Specs = [ImpInclSpec | !.Specs]
    ),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    (
        IntImports = []
    ;
        IntImports = [FirstIntImport | _],
        IntImportPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("declarations."), nl],
        IntImportSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntImport ^ aii_context,
                [always(IntImportPieces)])]),
        !:Specs = [IntImportSpec | !.Specs]
    ),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    (
        ImpAvails = []
    ;
        ImpAvails = [FirstImpAvail | _],
        ImpAvailPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("or"), decl("use_module"),
            words("declarations in its implementation section."), nl],
        ImpAvailSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_avail_context(FirstImpAvail),
                [always(ImpAvailPieces)])]),
        !:Specs = [ImpAvailSpec | !.Specs]
    ),

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

    map.init(IntForeignEnumMap),
    map.init(ImpForeignEnumMap),
    % We want only the error messages.
    create_type_ctor_checked_map(do_not_insist_on_defn, ModuleName,
        IntTypeDefnMap, ImpTypeDefnMap, IntForeignEnumMap, ImpForeignEnumMap,
        _TypeDefnCheckedMap, !Specs),

    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        MaybeVersionNumbers,
        IntInclModuleNames, IntUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap).

:- pred classify_int2_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
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
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
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
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
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
        VNSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(ModuleNameContext, [always(VNPieces)])]),
        !:Specs = [VNSpec | !.Specs]
    ),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    avail_imports_uses(IntAvails, IntImports, IntUses),
    IntImportModules = list.map(get_import_module_name, IntImports),
    set.list_to_set(IntImportModules, IntImportModuleNames),
    (
        IntUses = []
    ;
        IntUses = [FirstIntUse | _],
        IntUsePieces = [words("A .int3 file may not contain any"),
            decl("use_module"), words("declarations."), nl],
        IntUseSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntUse ^ aui_context, [always(IntUsePieces)])]),
        !:Specs = [IntUseSpec | !.Specs]
    ),

    (
        IntFIMs = []
    ;
        IntFIMs = [FirstIntFIM | _],
        IntFIMPieces = [words("A .int3 file may not contain any"),
            pragma_decl("foreign_import_module"), words("declarations."), nl],
        IntFIMSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntFIM ^ fim_context, [always(IntFIMPieces)])]),
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
    map.init(IntForeignEnumMap),
    map.init(ImpForeignEnumMap),
    % We want only the error messages.
    create_type_ctor_checked_map(do_not_insist_on_defn, ModuleName,
        IntTypeDefnMap, ImpTypeDefnMap, IntForeignEnumMap, ImpForeignEnumMap,
        _TypeDefnCheckedMap, !Specs),

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
            ImpItemSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(FirstImpContext, [always(ImpItemPieces)])]),
            !:Specs = [ImpItemSpec | !.Specs]
        )
    ),
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred classify_int3_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
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
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int3 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int3_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).

%---------------------------------------------------------------------------%

convert_parse_tree_plain_opt_to_opt(ParseTreePlainOpt) = ParseTreeOpt :-
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, ModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs,
        MarkerPragmas, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),

    Uses = list.map(wrap_use, set.to_sorted_list(UsedModuleNames)),
    FIMs = list.map(fim_spec_to_item, set.to_sorted_list(FIMSpecs)),
    Items =
        list.map(wrap_type_defn_item, TypeDefns) ++
        list.map(wrap_foreign_enum_item, ForeignEnums) ++
        list.map(wrap_inst_defn_item, InstDefns) ++
        list.map(wrap_mode_defn_item, ModeDefns) ++
        list.map(wrap_typeclass_item, TypeClasses) ++
        list.map(wrap_instance_item, Instances) ++
        list.map(wrap_pred_decl_item, PredDecls) ++
        list.map(wrap_mode_decl_item, ModeDecls) ++
        list.map(wrap_clause, Clauses) ++
        list.map(wrap_foreign_proc, ForeignProcs) ++
        list.map(wrap_marker_pragma_item, MarkerPragmas) ++
        list.map(wrap_type_spec_pragma_item, TypeSpecs) ++
        list.map(wrap_unused_args_pragma_item, UnusedArgs) ++
        list.map(wrap_termination_pragma_item, TermInfos) ++
        list.map(wrap_termination2_pragma_item, Term2Infos) ++
        list.map(wrap_exceptions_pragma_item, Exceptions) ++
        list.map(wrap_trailing_pragma_item, Trailings) ++
        list.map(wrap_mm_tabling_pragma_item, MMTablings) ++
        list.map(wrap_struct_sharing_pragma_item, Sharings) ++
        list.map(wrap_struct_reuse_pragma_item, Reuses),

    ParseTreeOpt = parse_tree_opt(ModuleName, ofk_opt, ModuleNameContext,
        Uses, FIMs, Items).

convert_parse_tree_trans_opt_to_opt(ParseTreeTransOpt) = ParseTreeOpt :-
    ParseTreeTransOpt = parse_tree_trans_opt(ModuleName, ModuleNameContext,
        TermInfos, Term2Infos, Exceptions, Trailings, MMTablings,
        Sharings, Reuses),

    Items =
        list.map(wrap_termination_pragma_item, TermInfos) ++
        list.map(wrap_termination2_pragma_item, Term2Infos) ++
        list.map(wrap_exceptions_pragma_item, Exceptions) ++
        list.map(wrap_trailing_pragma_item, Trailings) ++
        list.map(wrap_mm_tabling_pragma_item, MMTablings) ++
        list.map(wrap_struct_sharing_pragma_item, Sharings) ++
        list.map(wrap_struct_reuse_pragma_item, Reuses),

    ParseTreeOpt = parse_tree_opt(ModuleName, ofk_trans_opt, ModuleNameContext,
        [], [], Items).

%---------------------------------------------------------------------------%

check_convert_parse_tree_opt_to_plain_opt(ParseTreeOpt, ParseTreePlainOpt,
        !Specs) :-
    ParseTreeOpt = parse_tree_opt(ModuleName, OptFileKind, ModuleNameContext,
        Uses, FIMs, Items),
    expect(unify(OptFileKind, ofk_opt), $pred,
        "trying to convert non-ofk_plain_opt parse_tree_opt " ++
        "to parse_tree_plain_opt"),

    set.list_to_set(list.map(get_use_module_name, Uses), UsedModuleNames),
    set.list_to_set(list.map(fim_item_to_spec, FIMs), FIMSpecs),
    classify_plain_opt_items(Items, [], TypeDefns0, [], ForeignEnums0,
        [], InstDefns0, [], ModeDefns0, [], TypeClasses0, [], Instances0,
        [], PredDecls0, [], RevModeDecls, [], RevClauses, [], RevForeignProcs,
        [], PredMarkers0, [], TypeSpecs0, [], UnusedArgs0,
        [], TermInfos0, [], Term2Infos0,
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
    list.reverse(RevClauses, Clauses),
    list.reverse(RevForeignProcs, ForeignProcs),
    list.sort(PredMarkers0, PredMarkers),
    list.sort(TypeSpecs0, TypeSpecs),
    list.sort(UnusedArgs0, UnusedArgs),
    list.sort(TermInfos0, TermInfos),
    list.sort(Term2Infos0, Term2Infos),
    list.sort(Exceptions0, Exceptions),
    list.sort(Trailings0, Trailings),
    list.sort(MMTablings0, MMTablings),
    list.sort(Sharings0, Sharings),
    list.sort(Reuses0, Reuses),

    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, ModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs,
        PredMarkers, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
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
    list(item_foreign_proc)::in, list(item_foreign_proc)::out,
    list(item_pred_marker)::in, list(item_pred_marker)::out,
    list(item_type_spec)::in, list(item_type_spec)::out,
    list(item_unused_args)::in, list(item_unused_args)::out,
    list(item_termination)::in, list(item_termination)::out,
    list(item_termination2)::in, list(item_termination2)::out,
    list(item_exceptions)::in, list(item_exceptions)::out,
    list(item_trailing)::in, list(item_trailing)::out,
    list(item_mm_tabling)::in, list(item_mm_tabling)::out,
    list(item_struct_sharing)::in, list(item_struct_sharing)::out,
    list(item_struct_reuse)::in, list(item_struct_reuse)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_plain_opt_items([], !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs,
        !PredMarkers, !TypeSpecs, !UnusedArgs, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).
classify_plain_opt_items([Item | Items], !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs,
        !PredMarkers, !TypeSpecs, !UnusedArgs, !TermInfos, !Term2Infos,
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
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(PragmaType, Context, SeqNum),
        (
            (
                PragmaType = pragma_inline(SymNameArity),
                Kind = pmpk_inline
            ;
                PragmaType = pragma_no_inline(SymNameArity),
                Kind = pmpk_noinline
            ;
                PragmaType = pragma_promise_eqv_clauses(SymNameArity),
                Kind = pmpk_promise_eqv_clauses
            ;
                PragmaType = pragma_promise_pure(SymNameArity),
                Kind = pmpk_promise_pure
            ;
                PragmaType = pragma_promise_semipure(SymNameArity),
                Kind = pmpk_promise_semipure
            ;
                PragmaType = pragma_terminates(SymNameArity),
                Kind = pmpk_terminates
            ;
                PragmaType = pragma_does_not_terminate(SymNameArity),
                Kind = pmpk_does_not_terminate
            ;
                PragmaType = pragma_mode_check_clauses(SymNameArity),
                Kind = pmpk_mode_check_clauses
            ),
            PredMarker = pragma_info_pred_marker(SymNameArity, Kind),
            ItemPredMarker = item_pragma_info(PredMarker, Context, SeqNum),
            !:PredMarkers = [ItemPredMarker | !.PredMarkers]
        ;
            PragmaType = pragma_type_spec(TypeSpec),
            ItemTypeSpec = item_pragma_info(TypeSpec, Context, SeqNum),
            !:TypeSpecs = [ItemTypeSpec | !.TypeSpecs]
        ;
            PragmaType = pragma_unused_args(UnusedArgs),
            ItemUnusedArgs = item_pragma_info(UnusedArgs, Context, SeqNum),
            !:UnusedArgs = [ItemUnusedArgs | !.UnusedArgs]
        ;
            PragmaType = pragma_termination_info(Term),
            ItemTerm = item_pragma_info(Term, Context, SeqNum),
            !:TermInfos = [ItemTerm | !.TermInfos]
        ;
            PragmaType = pragma_termination2_info(Term2),
            ItemTerm2 = item_pragma_info(Term2, Context, SeqNum),
            !:Term2Infos = [ItemTerm2 | !.Term2Infos]
        ;
            PragmaType = pragma_exceptions(Exception),
            ItemException = item_pragma_info(Exception, Context, SeqNum),
            !:Exceptions = [ItemException | !.Exceptions]
        ;
            PragmaType = pragma_trailing_info(Trailing),
            ItemTrailing = item_pragma_info(Trailing, Context, SeqNum),
            !:Trailings = [ItemTrailing | !.Trailings]
        ;
            PragmaType = pragma_mm_tabling_info(MMTabling),
            ItemMMTabling = item_pragma_info(MMTabling, Context, SeqNum),
            !:MMTablings = [ItemMMTabling | !.MMTablings]
        ;
            PragmaType = pragma_structure_sharing(Sharing),
            ItemSharing = item_pragma_info(Sharing, Context, SeqNum),
            !:Sharings = [ItemSharing | !.Sharings]
        ;
            PragmaType = pragma_structure_reuse(Reuse),
            ItemReuse = item_pragma_info(Reuse, Context, SeqNum),
            !:Reuses = [ItemReuse | !.Reuses]
        ;
            PragmaType = pragma_foreign_proc(ForeignProc),
            ItemForeignProc = item_pragma_info(ForeignProc, Context, SeqNum),
            !:RevForeignProcs = [ItemForeignProc | !.RevForeignProcs]
        ;
            ( PragmaType = pragma_foreign_decl(_)
            ; PragmaType = pragma_foreign_code(_)
            ; PragmaType = pragma_foreign_proc_export(_)
            ; PragmaType = pragma_external_proc(_)
            ; PragmaType = pragma_consider_used(_)
            ; PragmaType = pragma_no_detism_warning(_)
            ; PragmaType = pragma_require_tail_recursion(_)
            ; PragmaType = pragma_tabled(_)
            ; PragmaType = pragma_fact_table(_)
            ; PragmaType = pragma_oisu(_)
            ; PragmaType = pragma_check_termination(_)
            ; PragmaType = pragma_obsolete_pred(_)
            ; PragmaType = pragma_obsolete_proc(_)
            ; PragmaType = pragma_require_feature_set(_)
            ),
            Pieces = [words("A .opt file may not contain")] ++
                item_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_item_context(Item), [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_foreign_export_enum(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .opt file may not contain")] ++
            item_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_plain_opt_items(Items, !TypeDefns, !ForeignEnums,
        !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !RevModeDecls, !RevClauses, !RevForeignProcs,
        !PredMarkers, !TypeSpecs, !UnusedArgs, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).

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
        UsePieces = [words("A .trans_opt file may not contain any"),
            decl("use_module"), words("declarations."), nl],
        UseSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstUse ^ aui_context, [always(UsePieces)])]),
        !:Specs = [UseSpec | !.Specs]
    ),
    (
        FIMs = []
    ;
        FIMs = [FirstFIM | _],
        FIMPieces = [words("A .trans_opt file may not contain any"),
            pragma_decl("foreign_import_module"), words("declarations."), nl],
        FIMSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstFIM ^ fim_context, [always(FIMPieces)])]),
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
    list(item_termination)::in, list(item_termination)::out,
    list(item_termination2)::in, list(item_termination2)::out,
    list(item_exceptions)::in, list(item_exceptions)::out,
    list(item_trailing)::in, list(item_trailing)::out,
    list(item_mm_tabling)::in, list(item_mm_tabling)::out,
    list(item_struct_sharing)::in, list(item_struct_sharing)::out,
    list(item_struct_reuse)::in, list(item_struct_reuse)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_trans_opt_items([], !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).
classify_trans_opt_items([Item | Items], !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(PragmaType, Context, SeqNum),
        (
            PragmaType = pragma_termination_info(Term),
            ItemTerm = item_pragma_info(Term, Context, SeqNum),
            !:TermInfos = [ItemTerm | !.TermInfos]
        ;
            PragmaType = pragma_termination2_info(Term2),
            ItemTerm2 = item_pragma_info(Term2, Context, SeqNum),
            !:Term2Infos = [ItemTerm2 | !.Term2Infos]
        ;
            PragmaType = pragma_exceptions(Exception),
            ItemException = item_pragma_info(Exception, Context, SeqNum),
            !:Exceptions = [ItemException | !.Exceptions]
        ;
            PragmaType = pragma_trailing_info(Trailing),
            ItemTrailing = item_pragma_info(Trailing, Context, SeqNum),
            !:Trailings = [ItemTrailing | !.Trailings]
        ;
            PragmaType = pragma_mm_tabling_info(MMTabling),
            ItemMMTabling = item_pragma_info(MMTabling, Context, SeqNum),
            !:MMTablings = [ItemMMTabling | !.MMTablings]
        ;
            PragmaType = pragma_structure_sharing(Sharing),
            ItemSharing = item_pragma_info(Sharing, Context, SeqNum),
            !:Sharings = [ItemSharing | !.Sharings]
        ;
            PragmaType = pragma_structure_reuse(Reuse),
            ItemReuse = item_pragma_info(Reuse, Context, SeqNum),
            !:Reuses = [ItemReuse | !.Reuses]
        ;
            ( PragmaType = pragma_foreign_decl(_)
            ; PragmaType = pragma_foreign_code(_)
            ; PragmaType = pragma_foreign_proc(_)
            ; PragmaType = pragma_foreign_proc_export(_)
            ; PragmaType = pragma_external_proc(_)
            ; PragmaType = pragma_type_spec(_)
            ; PragmaType = pragma_inline(_)
            ; PragmaType = pragma_no_inline(_)
            ; PragmaType = pragma_consider_used(_)
            ; PragmaType = pragma_unused_args(_)
            ; PragmaType = pragma_no_detism_warning(_)
            ; PragmaType = pragma_require_tail_recursion(_)
            ; PragmaType = pragma_tabled(_)
            ; PragmaType = pragma_fact_table(_)
            ; PragmaType = pragma_oisu(_)
            ; PragmaType = pragma_promise_eqv_clauses(_)
            ; PragmaType = pragma_promise_pure(_)
            ; PragmaType = pragma_promise_semipure(_)
            ; PragmaType = pragma_terminates(_)
            ; PragmaType = pragma_does_not_terminate(_)
            ; PragmaType = pragma_check_termination(_)
            ; PragmaType = pragma_mode_check_clauses(_)
            ; PragmaType = pragma_obsolete_pred(_)
            ; PragmaType = pragma_obsolete_proc(_)
            ; PragmaType = pragma_require_feature_set(_)
            ),
            Pieces = [words("A .trans_opt file may not contain")] ++
                item_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_item_context(Item), [always(Pieces)])]),
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
        ; Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .trans_opt file may not contain")] ++
            item_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_trans_opt_items(Items, !TermInfos, !Term2Infos,
        !Exceptions, !Trailings, !MMTablings, !Sharings, !Reuses, !Specs).

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
    some [!AbstractSolverDefns, !SolverDefns,
        !AbstractStdDefns, !EqvDefns, !DuDefns,
        !ForeignDefnsC, !ForeignDefnsJava,
        !ForeignDefnsCsharp, !ForeignDefnsErlang]
    (
        ( if map.search(!.TypeDefnMap, TypeCtor, AllDefns0) then
            AllDefns0 = type_ctor_all_defns(
                !:AbstractSolverDefns, !:SolverDefns,
                !:AbstractStdDefns, !:EqvDefns, !:DuDefns,
                c_java_csharp_erlang(!:ForeignDefnsC, !:ForeignDefnsJava,
                    !:ForeignDefnsCsharp, !:ForeignDefnsErlang))
        else
            !:AbstractSolverDefns = [],
            !:SolverDefns = [],

            !:AbstractStdDefns = [],
            !:EqvDefns = [],
            !:DuDefns = [],
            !:ForeignDefnsC = [],
            !:ForeignDefnsJava = [],
            !:ForeignDefnsCsharp = [],
            !:ForeignDefnsErlang = []
        ),
        (
            TypeDefn = parse_tree_abstract_type(DetailsAbstract),
            AbstractDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsAbstract,
            (
                DetailsAbstract = abstract_solver_type,
                !:AbstractSolverDefns = !.AbstractSolverDefns ++
                    [AbstractDefnInfo]
            ;
                ( DetailsAbstract = abstract_type_general
                ; DetailsAbstract = abstract_type_fits_in_n_bits(_)
                ; DetailsAbstract = abstract_dummy_type
                ; DetailsAbstract = abstract_notag_type
                ),
                !:AbstractStdDefns = !.AbstractStdDefns ++ [AbstractDefnInfo]
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
            ;
                LangType = erlang(_),
                !:ForeignDefnsErlang = !.ForeignDefnsErlang ++
                    [ForeignDefnInfo]
            )
        ),
        AllDefns = type_ctor_all_defns(!.AbstractSolverDefns, !.SolverDefns,
            !.AbstractStdDefns, !.EqvDefns, !.DuDefns,
            c_java_csharp_erlang(!.ForeignDefnsC, !.ForeignDefnsJava,
                !.ForeignDefnsCsharp, !.ForeignDefnsErlang))
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
    SNA = sym_name_arity(SymName, Arity),
    ( if map.search(!.InstDefnMap, SNA, AllDefns0) then
        AllDefns0 = inst_ctor_all_defns(AbstractDefns0, NonAbstractDefns0),
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefns = [InstDefnInfo | AbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns, NonAbstractDefns0)
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(_),
            NonAbstractDefns = [InstDefnInfo | NonAbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns0, NonAbstractDefns)
        ),
        map.det_update(SNA, AllDefns, !InstDefnMap)
    else
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AllDefns = inst_ctor_all_defns([InstDefnInfo], [])
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(_),
            AllDefns = inst_ctor_all_defns([], [InstDefnInfo])
        ),
        map.det_insert(SNA, AllDefns, !InstDefnMap)
    ).

mode_ctor_defn_items_to_map(ModeDefnInfos) = ModeDefnMap :-
    list.foldl(add_mode_defn_to_map, ModeDefnInfos, map.init, ModeDefnMap).

:- pred add_mode_defn_to_map(item_mode_defn_info::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::out) is det.

add_mode_defn_to_map(ModeDefnInfo, !ModeDefnMap) :-
    ModeDefnInfo = item_mode_defn_info(SymName, Params, MaybeAbstractModeDefn,
        _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    SNA = sym_name_arity(SymName, Arity),
    ( if map.search(!.ModeDefnMap, SNA, AllDefns0) then
        AllDefns0 = mode_ctor_all_defns(AbstractDefns0, NonAbstractDefns0),
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefns = [ModeDefnInfo | AbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns, NonAbstractDefns0)
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(_),
            NonAbstractDefns = [ModeDefnInfo | NonAbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns0, NonAbstractDefns)
        ),
        map.det_update(SNA, AllDefns, !ModeDefnMap)
    else
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AllDefns = mode_ctor_all_defns([ModeDefnInfo], [])
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(_),
            AllDefns = mode_ctor_all_defns([], [ModeDefnInfo])
        ),
        map.det_insert(SNA, AllDefns, !ModeDefnMap)
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
    some [!ForeignEnumsC, !ForeignEnumsJava,
        !ForeignEnumsCsharp, !ForeignEnumsErlang]
    (
        ( if map.search(!.ForeignEnumMap, TypeCtor, AllEnums0) then
            AllEnums0 = c_java_csharp_erlang(!:ForeignEnumsC,
                !:ForeignEnumsJava, !:ForeignEnumsCsharp, !:ForeignEnumsErlang)
        else
            !:ForeignEnumsC = [],
            !:ForeignEnumsJava = [],
            !:ForeignEnumsCsharp = [],
            !:ForeignEnumsErlang = []
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
        ;
            Lang = lang_erlang,
            !:ForeignEnumsErlang = !.ForeignEnumsErlang ++ [ForeignEnumInfo]
        ),
        AllEnums = c_java_csharp_erlang(!.ForeignEnumsC,
            !.ForeignEnumsJava, !.ForeignEnumsCsharp, !.ForeignEnumsErlang),
        map.set(TypeCtor, AllEnums, !ForeignEnumMap)
    ).

%---------------------------------------------------------------------------%

:- func type_ctor_defn_map_to_items(type_ctor_defn_map) = list(item).

type_ctor_defn_map_to_items(TypeCtorDefnMap) = Items :-
    map.foldl_values(accumulate_type_ctor_defns, TypeCtorDefnMap,
        cord.init, TypeDefnsCord),
    TypeDefns = cord.list(TypeDefnsCord),
    Items = list.map(wrap_type_defn_item, TypeDefns).

:- pred accumulate_type_ctor_defns(type_ctor_all_defns::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

accumulate_type_ctor_defns(CtorAllDefns, !TypeDefns) :-
    CtorAllDefns = type_ctor_all_defns(AbstractSolverDefns, SolverDefns,
        AbstractStdDefns, EqvDefns, DuDefns, CJCsEDefns),
    CJCsEDefns = c_java_csharp_erlang(ForeignDefnsC, ForeignDefnsJava,
        ForeignDefnsCsharp, ForeignDefnsErlang),
    !:TypeDefns = !.TypeDefns ++ cord.from_list(
        list.map(wrap_abstract_type_defn, at_most_one(AbstractSolverDefns)) ++
        list.map(wrap_solver_type_defn, SolverDefns) ++
        list.map(wrap_abstract_type_defn, at_most_one(AbstractStdDefns)) ++
        list.map(wrap_eqv_type_defn, EqvDefns) ++
        list.map(wrap_du_type_defn, DuDefns) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsC) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsJava) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsCsharp) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsErlang)).

:- func at_most_one(list(T)) = list(T).

at_most_one([]) = [].
at_most_one([X | _Xs]) = [X].

:- func wrap_abstract_type_defn(item_type_defn_info_abstract)
    = item_type_defn_info.

wrap_abstract_type_defn(AbstractDefnInfo) = TypeDefnInfo :-
    AbstractDefn = AbstractDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = AbstractDefnInfo ^ td_ctor_defn
        := parse_tree_abstract_type(AbstractDefn).

:- func wrap_solver_type_defn(item_type_defn_info_solver)
    = item_type_defn_info.

wrap_solver_type_defn(SolverDefnInfo) = TypeDefnInfo :-
    SolverDefn = SolverDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = SolverDefnInfo ^ td_ctor_defn
        := parse_tree_solver_type(SolverDefn).

:- func wrap_eqv_type_defn(item_type_defn_info_eqv)
    = item_type_defn_info.

wrap_eqv_type_defn(EqvDefnInfo) = TypeDefnInfo :-
    EqvDefn = EqvDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = EqvDefnInfo ^ td_ctor_defn
        := parse_tree_eqv_type(EqvDefn).

:- func wrap_du_type_defn(item_type_defn_info_du)
    = item_type_defn_info.

wrap_du_type_defn(DuDefnInfo) = TypeDefnInfo :-
    DuDefn = DuDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = DuDefnInfo ^ td_ctor_defn
        := parse_tree_du_type(DuDefn).

:- func wrap_foreign_type_defn(item_type_defn_info_foreign)
    = item_type_defn_info.

wrap_foreign_type_defn(ForeignDefnInfo) = TypeDefnInfo :-
    ForeignDefn = ForeignDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = ForeignDefnInfo ^ td_ctor_defn
        := parse_tree_foreign_type(ForeignDefn).

%---------------------%

:- func inst_ctor_defn_map_to_items(inst_ctor_defn_map) = list(item).

inst_ctor_defn_map_to_items(InstCtorDefnMap) = Items :-
    map.foldl_values(accumulate_inst_ctor_defns, InstCtorDefnMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord),
    Items = list.map(wrap_inst_defn_item, InstDefns).

:- pred accumulate_inst_ctor_defns(inst_ctor_all_defns::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

accumulate_inst_ctor_defns(CtorAllDefns, !InstDefns) :-
    CtorAllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns),
    !:InstDefns = !.InstDefns ++
        cord.from_list(AbstractDefns) ++
        cord.from_list(EqvDefns).

:- func mode_ctor_defn_map_to_items(mode_ctor_defn_map) = list(item).

mode_ctor_defn_map_to_items(ModeCtorDefnMap) = Items :-
    map.foldl_values(accumulate_mode_ctor_defns, ModeCtorDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord),
    Items = list.map(wrap_mode_defn_item, ModeDefns).

:- pred accumulate_mode_ctor_defns(mode_ctor_all_defns::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

accumulate_mode_ctor_defns(CtorAllDefns, !ModeDefns) :-
    CtorAllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns),
    !:ModeDefns = !.ModeDefns ++
        cord.from_list(AbstractDefns) ++
        cord.from_list(EqvDefns).

:- func type_ctor_repn_map_to_items(type_ctor_repn_map) = list(item).

type_ctor_repn_map_to_items(TypeCtorRepnMap) = Items :-
    map.foldl_values(accumulate_type_ctor_repns, TypeCtorRepnMap,
        cord.init, TypeRepnsCord),
    TypeRepns = cord.list(TypeRepnsCord),
    Items = list.map(wrap_type_repn_item, TypeRepns).

:- pred accumulate_type_ctor_repns(item_type_repn_info::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

accumulate_type_ctor_repns(TypeRepn, !TypeRepns) :-
    !:TypeRepns = cord.snoc(!.TypeRepns, TypeRepn).

:- func type_ctor_foreign_enum_map_to_items(type_ctor_foreign_enum_map)
    = list(item).

type_ctor_foreign_enum_map_to_items(ForeignEnumMap) = Items :-
    map.foldl_values(accumulate_foreign_enum_items, ForeignEnumMap,
        cord.init, ForeignEnumItemsCord),
    ForeignEnumItems = cord.list(ForeignEnumItemsCord),
    Items = list.map(wrap_foreign_enum_item, ForeignEnumItems).

:- pred accumulate_foreign_enum_items(c_j_cs_e_enums::in,
    cord(item_foreign_enum_info)::in, cord(item_foreign_enum_info)::out)
    is det.

accumulate_foreign_enum_items(AllEnums, !ForeignEnums) :-
    AllEnums = c_java_csharp_erlang(ForeignEnumsC, ForeignEnumsJava,
        ForeignEnumsCsharp, ForeignEnumsErlang),
    !:ForeignEnums = !.ForeignEnums ++
        cord.from_list(ForeignEnumsC) ++
        cord.from_list(ForeignEnumsJava) ++
        cord.from_list(ForeignEnumsCsharp) ++
        cord.from_list(ForeignEnumsErlang).

%---------------------------------------------------------------------------%

wrap_include(ModuleName) = Include :-
    Include = item_include(ModuleName, term.context_init, -1).

wrap_import_avail(ModuleName) = Avail :-
    ImportInfo = avail_import_info(ModuleName, term.context_init, -1),
    Avail = avail_import(ImportInfo).

wrap_use_avail(ModuleName) = Avail :-
    UseInfo = avail_use_info(ModuleName, term.context_init, -1),
    Avail = avail_use(UseInfo).

wrap_import(ModuleName) = ImportInfo :-
    ImportInfo = avail_import_info(ModuleName, term.context_init, -1).

wrap_use(ModuleName) = UseInfo :-
    UseInfo = avail_use_info(ModuleName, term.context_init, -1).

wrap_type_defn_item(X) = item_type_defn(X).
wrap_inst_defn_item(X) = item_inst_defn(X).
wrap_mode_defn_item(X) = item_mode_defn(X).
wrap_typeclass_item(X) = item_typeclass(X).
wrap_instance_item(X) = item_instance(X).
wrap_pred_decl_item(X) = item_pred_decl(X).
wrap_mode_decl_item(X) = item_mode_decl(X).
wrap_foreign_enum_item(X) = item_foreign_enum(X).
wrap_pragma_item(X) = item_pragma(X).
wrap_promise_item(X) = item_promise(X).
wrap_type_repn_item(X) = item_type_repn(X).

:- func wrap_clause(item_clause_info) = item.
:- func wrap_foreign_proc(item_foreign_proc) = item.
:- func wrap_marker_pragma_item(item_pred_marker) = item.
:- func wrap_type_spec_pragma_item(item_type_spec) = item.
:- func wrap_unused_args_pragma_item(item_unused_args) = item.
:- func wrap_termination_pragma_item(item_termination) = item.
:- func wrap_termination2_pragma_item(item_termination2) = item.
:- func wrap_exceptions_pragma_item(item_exceptions) = item.
:- func wrap_trailing_pragma_item(item_trailing) = item.
:- func wrap_mm_tabling_pragma_item(item_mm_tabling) = item.
:- func wrap_struct_sharing_pragma_item(item_struct_sharing) = item.
:- func wrap_struct_reuse_pragma_item(item_struct_reuse) = item.

wrap_clause(X) = item_clause(X).

wrap_foreign_proc(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_foreign_proc(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_marker_pragma_item(X) = Item :-
    X = item_pragma_info(MarkerInfo, Context, SeqNum),
    MarkerInfo = pragma_info_pred_marker(SymNameArity, Kind),
    (
        Kind = pmpk_inline,
        PragmaInfo = pragma_inline(SymNameArity)
    ;
        Kind = pmpk_noinline,
        PragmaInfo = pragma_no_inline(SymNameArity)
    ;
        Kind = pmpk_promise_pure,
        PragmaInfo = pragma_promise_pure(SymNameArity)
    ;
        Kind = pmpk_promise_semipure,
        PragmaInfo = pragma_promise_semipure(SymNameArity)
    ;
        Kind = pmpk_promise_eqv_clauses,
        PragmaInfo = pragma_promise_eqv_clauses(SymNameArity)
    ;
        Kind = pmpk_terminates,
        PragmaInfo = pragma_terminates(SymNameArity)
    ;
        Kind = pmpk_does_not_terminate,
        PragmaInfo = pragma_does_not_terminate(SymNameArity)
    ;
        Kind = pmpk_mode_check_clauses,
        PragmaInfo = pragma_mode_check_clauses(SymNameArity)
    ),
    Pragma = item_pragma_info(PragmaInfo, Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_type_spec_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_type_spec(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_unused_args_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_unused_args(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_termination_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_termination_info(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_termination2_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_termination2_info(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_exceptions_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_exceptions(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_trailing_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_trailing_info(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_mm_tabling_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_mm_tabling_info(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_struct_sharing_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_structure_sharing(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

wrap_struct_reuse_pragma_item(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Pragma = item_pragma_info(pragma_structure_reuse(Info), Context, SeqNum),
    Item = item_pragma(Pragma).

%---------------------------------------------------------------------------%
:- end_module parse_tree.convert_parse_tree.
%---------------------------------------------------------------------------%
