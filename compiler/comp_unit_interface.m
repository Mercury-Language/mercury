%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: comp_unit_interface.m.
% Authors: fjh (original version), zs (current version).
%
% Given the raw compilation unit of a module, extract the part of that module
% that will go into the .int file of the module.
%
%---------------------------------------------------------------------------%

:- module parse_tree.comp_unit_interface.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.
    % XXX document me better
    %
    % XXX Why do we report errors NOW, as opposed to when we generate code?
    %
:- pred generate_short_interface_int3(globals::in, raw_compilation_unit::in,
    parse_tree_int3::out, parse_tree_int::out) is det.

    % Generate the .int2 using the same approach as we use for .int3 files.
    % THIS PREDICATE IS ONLY FOR EXPERIMENTAL PURPOSES.
    %
:- pred generate_interface_int2_via_int3(globals::in, aug_compilation_unit::in,
    parse_tree_int::out) is det.

%---------------------------------------------------------------------------%

    % generate_private_interface_int0(AugCompUnit, ParseTreeInt0):
    %
    % Generate the private interface of a module (its .int0 file), which
    % makes available some not-generally-available items to the other modules
    % nested inside it.
    %
:- pred generate_private_interface_int0(aug_compilation_unit::in,
    parse_tree_int::out) is det.

%---------------------------------------------------------------------------%

    % generate_pre_grab_pre_qual_interface_for_int1_int2(RawCompUnit,
    %   InterfaceRawCompUnit):
    %
    % Prepare for the generation of .int and .int2 files by generating
    % the part of the raw compilation unit that needs to be module qualified
    % before the invocation of generate_interfaces_int1_int2.
    %
    % We return interface sections almost intact, changing them only by
    % making instance declarations abstract. We delete most kinds of items
    % from implementation sections, keeping only
    %
    % - Module includes.
    %
    % - Module imports and uses.
    %
    % - Type definitions, in a possibly changed form. Specifically,
    %   we replace the definitions (a) solver types and (b) noncanonical
    %   du and foreign types with their abstract forms. We leave the
    %   definitions of all other types (canonical du and foreign types,
    %   equivalence types, and already abtract types) unchanged.
    %
    % - Typeclass declarations in their abstract from.
    %
    % - Foreign_enum pragmas.
    %
    % - Foreign_import_module declarations.
    %
    % XXX ITEM_LIST Document why we do all this *before* module qualification.
    %
    % XXX ITEM_LIST The original comment on this predicate,
    % when it was conjoined with the code of get_interface above, was:
    % "Given the raw compilation unit of a module, extract and return
    % the part of that module that will go into the .int file of the module.
    % This will typically mostly be the interface section of the module,
    % but it may also contain parts of the implementation section as well.
    % Both parts may be somewhat modified; for example, we may remove
    % the bodies of instance definitions in an interface section,
    % but put the original, non-abstract instance definition in the
    % implementation section."
    %
:- pred generate_pre_grab_pre_qual_interface_for_int1_int2(
    raw_compilation_unit::in, raw_compilation_unit::out) is det.

    % Generate the contents for the .int and .int2 files.
    %
:- pred generate_interfaces_int1_int2(globals::in, aug_compilation_unit::in,
    parse_tree_int::out, parse_tree_int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% XXX ITEM_LIST
% The predicates in rest of the interface should not be needed at all.
%

    % This predicate is exported for use by modules.m.
    %
    % XXX ITEM_LIST When the predicate above is deleted, this function
    % should not be needed in this module anymore either, and so it should be
    % moved elsewhere.
    %
:- func make_foreign_import(module_name, foreign_language) = item_fim.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.decide_type_repn.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_short_interface_int3(Globals, RawCompUnit,
        ParseTreeInt3, ParseTreeInt) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, RawItemBlocks),
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        set.init, IntInclModuleNames, set.init, IntImportModuleNames0,
        cord.init, IntTypeDefnsCord,
        cord.init, IntInstDefnsCord, cord.init, IntModeDefnsCord,
        cord.init, IntTypeClassesCord, cord.init, IntInstancesCord,
        cord.init, OrigIntTypeDefnsCord, cord.init, OrigImpTypeDefnsCord,
        map.init, ForeignEnumTypeCtors, do_not_need_imports, NeedImports),
    (
        NeedImports = do_not_need_imports,
        IntImportModuleNames = set.init
    ;
        NeedImports = do_need_imports,
        IntImportModuleNames = IntImportModuleNames0
    ),
    IntTypeDefns = cord.list(IntTypeDefnsCord),
    IntInstDefns = cord.list(IntInstDefnsCord),
    IntModeDefns = cord.list(IntModeDefnsCord),
    IntTypeClasses = cord.list(IntTypeClassesCord),
    IntInstances = cord.list(IntInstancesCord),
    globals.lookup_bool_option(Globals, experiment1, Experiment1),
    (
        Experiment1 = no,
        TypeRepnInfos = []
    ;
        Experiment1 = yes,
        OrigIntTypeDefns = cord.list(OrigIntTypeDefnsCord),
        OrigImpTypeDefns = cord.list(OrigImpTypeDefnsCord),
        decide_repns_for_simple_types(ModuleName,
            OrigIntTypeDefns, OrigImpTypeDefns, ForeignEnumTypeCtors,
            IntTypeRepnInfos, _NonIntTypeRepnInfos),
        TypeRepnInfos = IntTypeRepnInfos
    ),
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefns, IntInstDefns, IntModeDefns,
        IntTypeClasses, IntInstances, TypeRepnInfos),
    ParseTreeInt0 = convert_parse_tree_int3_to_parse_tree_int(ParseTreeInt3),
    % Any Specs this can generat would be better reported when the module
    % is being compiled to target language code.
    module_qualify_parse_tree_int3(Globals, ParseTreeInt0, ParseTreeInt,
        [], _Specs).

:- type need_imports
    --->    do_not_need_imports
    ;       do_need_imports.

:- pred get_short_interface_int3_from_item_blocks(list(raw_item_block)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out,
    need_imports::in, need_imports::out) is det.

get_short_interface_int3_from_item_blocks([],
        !IntIncls, !IntImports, !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns, !OrigImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports).
get_short_interface_int3_from_item_blocks([RawItemBlock | RawItemBlocks],
        !IntIncls, !IntImports, !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns, !OrigImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports) :-
    RawItemBlock = item_block(_, Section, Incls, Avails, _FIMs, Items),
    (
        Section = ms_interface,
        list.foldl(add_included_module_name, Incls, !IntIncls),
        list.filter_map(avail_is_import, Avails, Imports),
        ImportModuleNames = list.map(avail_import_info_module_name, Imports),
        set.insert_list(ImportModuleNames, !IntImports),
        get_short_interface_int3_from_items_int(Items,
            !IntTypeDefns, !IntInstDefns, !IntModeDefns,
            !IntTypeClasses, !IntInstances, !OrigIntTypeDefns,
            !ForeignEnumTypeCtors, !NeedImports)
    ;
        Section = ms_implementation,
        get_short_interface_int3_from_items_imp(Items,
            !OrigImpTypeDefns, !ForeignEnumTypeCtors)
    ),
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        !IntIncls, !IntImports, !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns, !OrigImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports).

:- pred get_short_interface_int3_from_items_int(list(item)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out,
    need_imports::in, need_imports::out) is det.

get_short_interface_int3_from_items_int([],
        !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports).
get_short_interface_int3_from_items_int([Item | Items],
        !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        cord.snoc(ItemTypeDefnInfo, !OrigIntTypeDefns),
        % XXX TYPE_REPN do this in decide_type_repn.m?
        make_type_defn_abstract_type_for_int3(ItemTypeDefnInfo,
            AbstractOrForeignItemTypeDefnInfo),
        cord.snoc(AbstractOrForeignItemTypeDefnInfo, !IntTypeDefns)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        ItemTypeClassInfo = item_typeclass_info(ClassName, ParamsTVars,
            _Constraints, _FunDeps, _Methods, TVarSet, Context, SeqNum),
        AbstractItemTypeClassInfo = item_typeclass_info(ClassName, ParamsTVars,
            [], [], class_interface_abstract, TVarSet, Context, SeqNum),
        cord.snoc(AbstractItemTypeClassInfo, !IntTypeClasses)
    ;
        Item = item_instance(ItemInstanceInfo),
        AbstractItemInstanceInfo = ItemInstanceInfo ^ ci_method_instances
            := instance_body_abstract,
        cord.snoc(AbstractItemInstanceInfo, !IntInstances),
        % We may need the imported modules to module qualify the names
        % of the type constructors in the instance's member types.
        !:NeedImports = do_need_imports
    ;
        Item = item_inst_defn(ItemInstInfo),
        AbstractItemInstInfo =
            ItemInstInfo ^ id_inst_defn := abstract_inst_defn,
        cord.snoc(AbstractItemInstInfo, !IntInstDefns)
    ;
        Item = item_mode_defn(ItemModeInfo),
        AbstractItemModeInfo =
            ItemModeInfo ^ md_mode_defn := abstract_mode_defn,
        cord.snoc(AbstractItemModeInfo, !IntModeDefns)
    ;
        Item = item_pragma(ItemPragma),
        maybe_record_foreign_enum(ItemPragma, !ForeignEnumTypeCtors)
    ;
        ( Item = item_clause(_)
        ; Item = item_mutable(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        )
    ),
    get_short_interface_int3_from_items_int(Items,
        !IntTypeDefns, !IntInstDefns, !IntModeDefns,
        !IntTypeClasses, !IntInstances, !OrigIntTypeDefns,
        !ForeignEnumTypeCtors, !NeedImports).

:- pred make_type_defn_abstract_type_for_int3(item_type_defn_info::in,
    item_type_defn_info::out) is det.

make_type_defn_abstract_type_for_int3(ItemTypeDefn,
        AbstractOrForeignItemTypeDefnInfo) :-
    TypeDefn = ItemTypeDefn ^ td_ctor_defn,
    (
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu =
            type_details_du(Ctors, MaybeCanonical, _MaybeDirectArgCtors),
        ( if du_type_is_enum(DetailsDu, NumBits) then
            AbstractDetails = abstract_type_fits_in_n_bits(NumBits)
        else if du_type_is_notag(Ctors, MaybeCanonical) then
            AbstractDetails = abstract_notag_type
        else if du_type_is_dummy(DetailsDu) then
            AbstractDetails = abstract_dummy_type
        else
            AbstractDetails = abstract_type_general
        ),
        AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn ^ td_ctor_defn
            := parse_tree_abstract_type(AbstractDetails)
    ;
        TypeDefn = parse_tree_abstract_type(_AbstractDetails),
        AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn
    ;
        TypeDefn = parse_tree_solver_type(_),
        % rafe: XXX we need to also export the details of the forwarding type
        % for the representation and the forwarding pred for initialization.
        AbstractDetails = abstract_solver_type,
        AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn ^ td_ctor_defn
            := parse_tree_abstract_type(AbstractDetails)
    ;
        TypeDefn = parse_tree_eqv_type(_),
        % XXX Is this right for solver types?
        % XXX TYPE_REPN Is this right for types that are eqv to enums,
        % or to known size ints/uints?
        AbstractDetails = abstract_type_general,
        AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn ^ td_ctor_defn
            := parse_tree_abstract_type(AbstractDetails)
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, MaybeCanonical,
            Assertions),
        % We always need the definitions of foreign types
        % to handle inter-language interfacing correctly.
        % XXX Even in .int3 files?
        % However, we want to abstract away any unify and compare predicates.
        (
            MaybeCanonical = canon,
            AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn
        ;
            MaybeCanonical = noncanon(_NonCanonical),
            AbstractMaybeCanonical =
                noncanon(noncanon_abstract(non_solver_type)),
            AbstractDetailsForeign = type_details_foreign(ForeignType,
                AbstractMaybeCanonical, Assertions),
            AbstractForeignTypeDefn =
                parse_tree_foreign_type(AbstractDetailsForeign),
            AbstractOrForeignItemTypeDefnInfo = ItemTypeDefn ^ td_ctor_defn
                := AbstractForeignTypeDefn
        )
    ).

:- pred get_short_interface_int3_from_items_imp(list(item)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out) is det.

get_short_interface_int3_from_items_imp([],
        !ImpTypeDefns, !ForeignEnumTypeCtors).
get_short_interface_int3_from_items_imp([Item | Items],
        !ImpTypeDefns, !ForeignEnumTypeCtors) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        cord.snoc(ItemTypeDefnInfo, !ImpTypeDefns)
    ;
        Item = item_pragma(ItemPragma),
        maybe_record_foreign_enum(ItemPragma, !ForeignEnumTypeCtors)
    ;
        ( Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_clause(_)
        ; Item = item_mutable(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        )
    ),
    get_short_interface_int3_from_items_imp(Items,
        !ImpTypeDefns, !ForeignEnumTypeCtors).

:- pred maybe_record_foreign_enum(item_pragma_info::in,
    foreign_enum_map::in, foreign_enum_map::out) is det.

maybe_record_foreign_enum(ItemPragma, !ForeignEnumTypeCtors) :-
    ItemPragma = item_pragma_info(Pragma, _, _, _),
    ( if Pragma = pragma_foreign_enum(PragmaInfoForeignEnum) then
        record_foreign_enum(PragmaInfoForeignEnum, !ForeignEnumTypeCtors)
    else
        true
    ).

:- pred record_foreign_enum(pragma_info_foreign_enum::in,
    foreign_enum_map::in, foreign_enum_map::out) is det.

record_foreign_enum(PragmaInfoForeignEnum, !ForeignEnumTypeCtors) :-
    PragmaInfoForeignEnum =
        pragma_info_foreign_enum(Lang, TypeCtor, OoMValues),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeName = unqualify_name(TypeSymName),
    UnqualTypeCtor = unqual_type_ctor(TypeName, TypeArity),
    ( if map.search(!.ForeignEnumTypeCtors, UnqualTypeCtor, LVs0) then
        map.det_update(UnqualTypeCtor, [Lang - OoMValues | LVs0],
            !ForeignEnumTypeCtors)
    else
        map.det_insert(UnqualTypeCtor, [Lang - OoMValues],
            !ForeignEnumTypeCtors)
    ).

:- pred record_foreign_enum_spec(foreign_enum_spec::in,
    foreign_enum_map::in, foreign_enum_map::out) is det.

record_foreign_enum_spec(ForeignEnumSpec, !ForeignEnumTypeCtors) :-
    ForeignEnumSpec = foreign_enum_spec(PragmaInfoForeignEnum, _MaybeAttrs),
    PragmaInfoForeignEnum =
        pragma_info_foreign_enum(Lang, TypeCtor, OoMValues),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeName = unqualify_name(TypeSymName),
    UnqualTypeCtor = unqual_type_ctor(TypeName, TypeArity),
    ( if map.search(!.ForeignEnumTypeCtors, UnqualTypeCtor, LVs0) then
        map.det_update(UnqualTypeCtor, [Lang - OoMValues | LVs0],
            !ForeignEnumTypeCtors)
    else
        map.det_insert(UnqualTypeCtor, [Lang - OoMValues],
            !ForeignEnumTypeCtors)
    ).

%---------------------------------------------------------------------------%

generate_interface_int2_via_int3(Globals, AugCompUnit, ParseTreeInt23) :-
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),
    list.map(src_to_raw_item_block, SrcItemBlocks, RawItemBlocks),
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        set.init, IntInclModuleNames, set.init, IntImportModuleNames0,
        cord.init, IntTypeDefnsCord,
        cord.init, IntInstDefnsCord, cord.init, IntModeDefnsCord,
        cord.init, IntTypeClassesCord, cord.init, IntInstancesCord,
        cord.init, OrigIntTypeDefnsCord, cord.init, OrigImpTypeDefnsCord,
        map.init, ForeignEnumTypeCtors, do_not_need_imports, NeedImports),
    (
        NeedImports = do_not_need_imports,
        IntImportModuleNames = set.init
    ;
        NeedImports = do_need_imports,
        IntImportModuleNames = IntImportModuleNames0
    ),
    IntTypeDefns = cord.list(IntTypeDefnsCord),
    IntInstDefns = cord.list(IntInstDefnsCord),
    IntModeDefns = cord.list(IntModeDefnsCord),
    IntTypeClasses = cord.list(IntTypeClassesCord),
    IntInstances = cord.list(IntInstancesCord),
    globals.lookup_bool_option(Globals, experiment1, Experiment1),
    (
        Experiment1 = no,
        TypeRepnInfos = []
    ;
        Experiment1 = yes,
        OrigIntTypeDefns = cord.list(OrigIntTypeDefnsCord),
        OrigImpTypeDefns = cord.list(OrigImpTypeDefnsCord),
        decide_repns_for_simple_types(ModuleName,
            OrigIntTypeDefns, OrigImpTypeDefns, ForeignEnumTypeCtors,
            IntTypeRepnInfos, _NonIntTypeRepnInfos),
        TypeRepnInfos = IntTypeRepnInfos
    ),
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefns, IntInstDefns, IntModeDefns,
        IntTypeClasses, IntInstances, TypeRepnInfos),
    ParseTreeInt23Prime =
        convert_parse_tree_int3_to_parse_tree_int(ParseTreeInt3),
    ParseTreeInt23 = ParseTreeInt23Prime ^ pti_int_file_kind := ifk_int2.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_private_interface_int0(AugCompUnit, ParseTreeInt0) :-
    AugCompUnit = aug_compilation_unit(ModuleName,
        ModuleNameContext, ModuleVersionNumbers, SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),
    set.init(IntIncls0),
    set.init(ImpIncls0),
    map.init(IntAvailMap0),
    map.init(ImpAvailMap0),
    set.init(IntFIMSpecs0),
    set.init(ImpFIMSpecs0),
    get_private_interface_int0_from_item_blocks(ModuleName, SrcItemBlocks,
        IntIncls0, IntIncls, ImpIncls0, ImpIncls,
        IntAvailMap0, IntAvailMap, ImpAvailMap0, ImpAvailMap,
        IntFIMSpecs0, IntFIMSpecs, ImpFIMSpecs0, ImpFIMSpecs,
        cord.init, IntTypeDefnsCord, cord.init, ImpTypeDefnsCord,
        cord.init, IntInstDefnsCord, cord.init, ImpInstDefnsCord,
        cord.init, IntModeDefnsCord, cord.init, ImpModeDefnsCord,
        cord.init, IntTypeClassesCord, cord.init, ImpTypeClassesCord,
        cord.init, IntInstancesCord, cord.init, ImpInstancesCord,
        cord.init, IntPredDeclsCord, cord.init, ImpPredDeclsCord,
        cord.init, IntModeDeclsCord, cord.init, ImpModeDeclsCord,
        cord.init, IntPragmasCord, cord.init, ImpPragmasCord,
        cord.init, IntPromisesCord, cord.init, ImpPromisesCord),
    ( if map.search(ModuleVersionNumbers, ModuleName, VersionNumbers) then
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    else
        MaybeVersionNumbers = no_version_numbers
    ),
    IntTypeDefns = cord.list(IntTypeDefnsCord),
    ImpTypeDefns = cord.list(ImpTypeDefnsCord),
    IntInstDefns = cord.list(IntInstDefnsCord),
    ImpInstDefns = cord.list(ImpInstDefnsCord),
    IntModeDefns = cord.list(IntModeDefnsCord),
    ImpModeDefns = cord.list(ImpModeDefnsCord),
    IntTypeClasses = cord.list(IntTypeClassesCord),
    ImpTypeClasses = cord.list(ImpTypeClassesCord),
    IntInstances = cord.list(IntInstancesCord),
    ImpInstances = cord.list(ImpInstancesCord),
    IntPredDecls = cord.list(IntPredDeclsCord),
    ImpPredDecls = cord.list(ImpPredDeclsCord),
    IntModeDecls = cord.list(IntModeDeclsCord),
    ImpModeDecls = cord.list(ImpModeDeclsCord),
    IntPragmas = cord.list(IntPragmasCord),
    ImpPragmas = cord.list(ImpPragmasCord),
    IntPromises = cord.list(IntPromisesCord),
    ImpPromises = cord.list(ImpPromisesCord),
    ParseTreeInt0Prime = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls,
        IntAvailMap, ImpAvailMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls, IntPragmas, IntPromises,
        ImpTypeDefns, ImpInstDefns, ImpModeDefns, ImpTypeClasses, ImpInstances,
        ImpPredDecls, ImpModeDecls, ImpPragmas, ImpPromises),
    ParseTreeInt0 =
        convert_parse_tree_int0_to_parse_tree_int(ParseTreeInt0Prime).

    % get_private_interface_int0_from_item_blocks processes each item
    % in the item blocks of a module, as part of the process of creating
    % .int0 files.
    %
    % The `.int0' file contains items which are available to any module in the
    % interface section, and items which are only available to submodules in
    % the implementation section. The term "private interface" is ambiguous:
    % sometimes it refers to the `.int0' file which, as just explained,
    % contains the public interface as well. The term "private interface
    % proper" may be used to refer to the information in the implementation
    % section of the `.int0' file.
    %
    % (Historically, the `.int0' file did not distinguish between the public
    % and private interfaces.)
    %
    % This predicate has several jobs.
    %
    % - It removes items that do not belong in the private interface,
    %   in either sense. This includes clauses, pragmas that function as
    %   clauses, and initialise and finalise declarations, since effectively
    %   they also represent code.
    %
    % - It expands any mutable declarations into the pred and mode declarations
    %   for their access predicates, since only these components of a
    %   mutable declaration should be written to a private interface file.
    %
    % - It makes any instance declarations abstract.
    %
    % - It separates the includes, avails and items into those that appear
    %   in the interface section and in the implementation section
    %   respectively. (There is an exception to this, marked with
    %   an XXX below).
    %
:- pred get_private_interface_int0_from_item_blocks(module_name::in,
    list(src_item_block)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    map(module_name, import_or_use)::in, map(module_name, import_or_use)::out,
    map(module_name, import_or_use)::in, map(module_name, import_or_use)::out,
    set(fim_spec)::in, set(fim_spec)::out,
    set(fim_spec)::in, set(fim_spec)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_pragma_info)::in, cord(item_pragma_info)::out,
    cord(item_pragma_info)::in, cord(item_pragma_info)::out,
    cord(item_promise_info)::in, cord(item_promise_info)::out,
    cord(item_promise_info)::in, cord(item_promise_info)::out) is det.

get_private_interface_int0_from_item_blocks(_ModuleName, [],
        !IntIncls, !ImpIncls, !IntAvailMap, !ImpAvailMap,
        !IntFIMSpecs, !ImpFIMSpecs, !IntTypeDefns, !ImpTypeDefns,
        !IntInstDefns, !ImpInstDefns, !IntModeDefns, !ImpModeDefns,
        !IntTypeClasses, !ImpTypeClasses, !IntInstances,!ImpInstances,
        !IntPredDecls, !ImpPredDecls, !IntModeDecls, !ImpModeDecls,
        !IntPragmas, !ImpPragmas, !IntPromises, !ImpPromises).
get_private_interface_int0_from_item_blocks(ModuleName,
        [ItemBlock | ItemBlocks],
        !IntIncls, !ImpIncls, !IntAvailMap, !ImpAvailMap,
        !IntFIMSpecs, !ImpFIMSpecs, !IntTypeDefns, !ImpTypeDefns,
        !IntInstDefns, !ImpInstDefns, !IntModeDefns, !ImpModeDefns,
        !IntTypeClasses, !ImpTypeClasses, !IntInstances,!ImpInstances,
        !IntPredDecls, !ImpPredDecls, !IntModeDecls, !ImpModeDecls,
        !IntPragmas, !ImpPragmas, !IntPromises, !ImpPromises) :-
    ItemBlock = item_block(_, SrcSection, Incls, Avails, FIMs, Items),
    (
        SrcSection = sms_interface,
        list.foldl(add_included_module_name, Incls, !IntIncls),
        list.foldl(record_avail_in_module_map, Avails, !IntAvailMap),
        list.foldl(add_fim_to_specs, FIMs, !IntFIMSpecs),
        get_private_interface_int0_from_items(ModuleName, Items,
            !IntTypeDefns, !IntInstDefns, !IntModeDefns,
            !IntTypeClasses, !IntInstances, !IntPredDecls, !IntModeDecls,
            !IntPragmas, !IntPromises)
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        % XXX ITEM_LIST Our parent calls grab_unqual_imported_modules,
        % which has traditionally NOT changed sms_implementation
        % to sms_impl_but_exported_to_submodules even in the presence
        % of submodules, but future factorizations of common code
        % may change that.
        list.foldl(add_included_module_name, Incls, !ImpIncls),
        list.foldl(record_avail_in_module_map, Avails, !ImpAvailMap),
        list.foldl(add_fim_to_specs, FIMs, !ImpFIMSpecs),
        get_private_interface_int0_from_items(ModuleName, Items,
            !ImpTypeDefns, !ImpInstDefns, !ImpModeDefns,
            !ImpTypeClasses, !ImpInstances, !ImpPredDecls, !ImpModeDecls,
            !ImpPragmas, !ImpPromises)
    ),
    get_private_interface_int0_from_item_blocks(ModuleName, ItemBlocks,
        !IntIncls, !ImpIncls, !IntAvailMap, !ImpAvailMap,
        !IntFIMSpecs, !ImpFIMSpecs, !IntTypeDefns, !ImpTypeDefns,
        !IntInstDefns, !ImpInstDefns, !IntModeDefns, !ImpModeDefns,
        !IntTypeClasses, !ImpTypeClasses, !IntInstances,!ImpInstances,
        !IntPredDecls, !ImpPredDecls, !IntModeDecls, !ImpModeDecls,
        !IntPragmas, !ImpPragmas, !IntPromises, !ImpPromises).

:- pred record_avail_in_module_map(item_avail::in,
    map(module_name, import_or_use)::in, map(module_name, import_or_use)::out)
    is det.

record_avail_in_module_map(Avail, !AvailMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, _Ctxt, _SeqNum)),
        Kind = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, _Ctxt, _SeqNum)),
        Kind = use_decl
    ),
    map.search_insert(ModuleName, Kind, MaybeOldKind, !AvailMap),
    (
        MaybeOldKind = no
    ;
        MaybeOldKind = yes(OldKind),
        ( if OldKind = use_decl, Kind = import_decl then
            map.det_update(ModuleName, Kind, !AvailMap)
        else
            true
        )
    ).

:- pred get_private_interface_int0_from_items(module_name::in, list(item)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_pragma_info)::in, cord(item_pragma_info)::out,
    cord(item_promise_info)::in, cord(item_promise_info)::out) is det.

get_private_interface_int0_from_items(_ModuleName, [],
        !TypeDefns, !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !ModeDecls, !Pragmas, !Promises).
get_private_interface_int0_from_items(ModuleName, [Item | Items],
        !TypeDefns, !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !ModeDecls, !Pragmas, !Promises) :-
    % XXX ITEM_LIST The action here for types, insts, modes, pred decls,
    % mode decls, typeclasses and promises follows what this predicate
    % used to do before the item list change.
    (
        ( Item = item_clause(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        )
        % Don't include in either section of the private interface.
    ;
        Item = item_type_repn(_)
        % process_item_for_private_interface should be invoked only on items
        % from the *source file*, not on items read from any interface file.
        % Any occurrence of item_type_repns in the source file should have been
        % caught and reported by the parsing code. If through some bug
        % they make it here, neither reporting them again nor aborting the
        % compiler is likely to be helpful.
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no
        ;
            AllowedInInterface = yes,
            cord.snoc(ItemPragma, !Pragmas)
        )
    ;
        Item = item_type_defn(TypeDefn),
        cord.snoc(TypeDefn, !TypeDefns)
    ;
        Item = item_inst_defn(InstDefn),
        cord.snoc(InstDefn, !InstDefns)
    ;
        Item = item_mode_defn(ModeDefn),
        cord.snoc(ModeDefn, !ModeDefns)
    ;
        Item = item_pred_decl(PredDecl),
        cord.snoc(PredDecl, !PredDecls)
    ;
        Item = item_mode_decl(ModeDecl),
        cord.snoc(ModeDecl, !ModeDecls)
    ;
        Item = item_typeclass(TypeClass),
        cord.snoc(TypeClass, !TypeClasses)
    ;
        Item = item_promise(Promise),
        cord.snoc(Promise, !Promises)
    ;
        Item = item_instance(Instance),
        AbstractInstance =
            Instance ^ ci_method_instances := instance_body_abstract,
        cord.snoc(AbstractInstance, !Instances)
    ;
        Item = item_mutable(ItemMutable),
        ItemMutable = item_mutable_info(MutableName,
            _OrigType, Type, _OrigInst, Inst, _Value, _Varset, MutAttrs,
            Context, _SeqNum),
        compute_needed_public_mutable_aux_preds(MutAttrs, PublicAuxPreds),
        list.map(
            make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst,
                Context),
            PublicAuxPreds, PublicAuxPredDecls),
        !:PredDecls = !.PredDecls ++ cord.from_list(PublicAuxPredDecls)
    ),
    get_private_interface_int0_from_items(ModuleName, Items,
        !TypeDefns, !InstDefns, !ModeDefns, !TypeClasses, !Instances,
        !PredDecls, !ModeDecls, !Pragmas, !Promises).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_pre_grab_pre_qual_interface_for_int1_int2(RawCompUnit,
        InterfaceRawCompUnit) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    generate_pre_grab_pre_qual_item_blocks(RawItemBlocks,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord,
        cord.init, IntFIMsCord, cord.init, ImpFIMsCord,
        cord.init, IntItemsCord, cord.init, ImpItemsCord),
    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails = cord.list(ImpAvailsCord),
    IntFIMs = cord.list(IntFIMsCord),
    ImpFIMs = cord.list(ImpFIMsCord),
    IntItems = cord.list(IntItemsCord),
    ImpItems = cord.list(ImpItemsCord),
    int_imp_items_to_item_blocks(ModuleName, ms_interface, ms_implementation,
        IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems, InterfaceItemBlocks),
    InterfaceRawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        InterfaceItemBlocks).

%---------------------------------------------------------------------------%

:- pred generate_pre_grab_pre_qual_item_blocks(list(raw_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_fim)::in, cord(item_fim)::out,
    cord(item_fim)::in, cord(item_fim)::out,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_item_blocks([],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntFIMsCord, !ImpFIMsCord, !IntItemsCord, !ImpItemsCord).
generate_pre_grab_pre_qual_item_blocks([RawItemBlock | RawItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntFIMsCord, !ImpFIMsCord, !IntItemsCord, !ImpItemsCord) :-
    RawItemBlock = item_block(_, Section, Incls, Avails, FIMs, Items),
    (
        Section = ms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        !:IntFIMsCord = !.IntFIMsCord ++ cord.from_list(FIMs),
        generate_pre_grab_pre_qual_items_int(Items, !IntItemsCord)
    ;
        Section = ms_implementation,
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        !:ImpFIMsCord = !.ImpFIMsCord ++ cord.from_list(FIMs),
        generate_pre_grab_pre_qual_items_imp(Items, !ImpItemsCord)
    ),
    generate_pre_grab_pre_qual_item_blocks(RawItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntFIMsCord, !ImpFIMsCord, !IntItemsCord, !ImpItemsCord).

:- pred generate_pre_grab_pre_qual_items_int(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_items_int([], !IntItemsCord).
generate_pre_grab_pre_qual_items_int([Item | Items], !IntItemsCord) :-
    ( if Item = item_instance(ItemInstance) then
        AbstractItemInstance = ItemInstance ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstance),
        cord.snoc(AbstractItem, !IntItemsCord)
    else
        cord.snoc(Item, !IntItemsCord)
    ),
    generate_pre_grab_pre_qual_items_int(Items, !IntItemsCord).

:- pred generate_pre_grab_pre_qual_items_imp(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_items_imp([], !ImpItemsCord).
generate_pre_grab_pre_qual_items_imp([Item | Items], !ImpItemsCord) :-
    % `:- typeclass' declarations may be referred to by the constructors
    % in type declarations. Since these constructors are abstractly
    % exported, we won't need the local instance declarations.
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        make_canon_make_du_and_solver_types_abstract(ItemTypeDefnInfo,
            MaybeAbstractItemTypeDefnInfo),
        AbstractItem = item_type_defn(MaybeAbstractItemTypeDefnInfo),
        cord.snoc(AbstractItem, !ImpItemsCord)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        cord.snoc(AbstractItem, !ImpItemsCord)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        ( if Pragma = pragma_foreign_enum(_) then
            cord.snoc(Item, !ImpItemsCord)
        else
            true
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        )
    ;
        Item = item_type_repn(_),
        % XXX TYPE_REPN Implement this.
        unexpected($pred, "item_type_repn")
    ),
    generate_pre_grab_pre_qual_items_imp(Items, !ImpItemsCord).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_interfaces_int1_int2(Globals, AugCompUnit,
        ParseTreeInt1, ParseTreeInt2) :-
    generate_interface_int1(AugCompUnit, IntInclModuleNames, IntImportsUses,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        ImpTypeDefns, IntTypesMap, ImpTypesMap, IntPragmas, ImpForeignEnums,
        ParseTreeInt1A),
    generate_interface_int2(Globals, AugCompUnit,
        IntInclModuleNames, IntImportsUses,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        ImpTypeDefns, IntTypesMap, ImpTypesMap, IntPragmas, ImpForeignEnums,
        ParseTreeInt2A),
    ParseTreeInt1 = convert_parse_tree_int1_to_parse_tree_int(ParseTreeInt1A),
    ParseTreeInt2 = convert_parse_tree_int2_to_parse_tree_int(ParseTreeInt2A).

:- pred generate_interface_int1(aug_compilation_unit::in,
    set(module_name)::out, set(module_name)::out,
    set(fim_spec)::out, set(fim_spec)::out,
    list(item_type_defn_info)::out,
    list(item_inst_defn_info)::out, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::out, list(item_instance_info)::out,
    list(item_type_defn_info)::out, type_defn_map::out, type_defn_map::out,
    list(item_pragma_info)::out, list(foreign_enum_spec)::out,
    parse_tree_int1::out) is det.

generate_interface_int1(AugCompUnit, IntIncls, IntImportsUses,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns,
        IntTypeClasses, IntInstances,
        ImpTypeDefns, IntTypesMap, ImpTypesMap, IntPragmas, ImpForeignEnums0,
        ParseTreeInt1) :-
    % We return some of our intermediate results to our caller, for use
    % in constructing the .int2 file.
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),

    % Separate out the contents of the interface section(s) from the
    % contents of the implementation section(s). Separate out the
    % foreign enum pragmas and foreign_import_module items in the
    % implementation section, for possible selective reinclusion later.
    % Likewise, remove type definitions from the implementation section
    % after recording them in ImpTypesMap. Record the type definitions
    % in the interface section as well, in IntTypesMap. Record the set of
    % modules that we need access to due to references in typeclass
    % definition items.
    set.init(IntImportsUses0),
    set.init(ImpImportsUses0),
    set.init(IntExplicitFIMSpecs0),
    set.init(ImpExplicitFIMSpecs0),
    set.init(IntImplicitFIMLangs0),
    set.init(ImpImplicitFIMLangs0),
    multi_map.init(IntTypesMap0),
    multi_map.init(ImpTypesMap0),

    % XXX Why are we ignoring _IntModulesNeededByTypeClassDefns?
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        set.init, IntIncls, set.init, ImpIncls,
        IntImportsUses0, IntImportsUses, ImpImportsUses0, ImpImportsUses1,
        IntExplicitFIMSpecs0, IntExplicitFIMSpecs,
        ImpExplicitFIMSpecs0, ImpExplicitFIMSpecs,
        IntImplicitFIMLangs0, IntImplicitFIMLangs,
        ImpImplicitFIMLangs0, ImpImplicitFIMLangs1,
        IntTypesMap0, IntTypesMap, cord.init, IntTypeDefnsCord,
        cord.init, IntInstDefnsCord, cord.init, IntModeDefnsCord,
        cord.init, IntTypeClassesCord, cord.init, IntInstancesCord,
        cord.init, IntPredDeclsCord, cord.init, IntModeDeclsCord,
        cord.init, IntPragmasCord, cord.init, IntPromisesCord,
        set.init, _IntModulesNeededByTypeClassDefns,
        ImpTypesMap0, ImpTypesMap,
        cord.init, ImpTypeClassesCord, cord.init, ImpForeignEnumsCord,
        set.init, ImpModulesNeededByTypeClassDefns),

    IntTypeDefns = cord.list(IntTypeDefnsCord),
    IntInstDefns = cord.list(IntInstDefnsCord),
    IntModeDefns = cord.list(IntModeDefnsCord),
    IntTypeClasses = cord.list(IntTypeClassesCord),
    IntInstances = cord.list(IntInstancesCord),
    IntPredDecls = cord.list(IntPredDeclsCord),
    IntModeDecls = cord.list(IntModeDeclsCord),
    IntPragmas = cord.list(IntPragmasCord),
    IntPromises = cord.list(IntPromisesCord),

    ImpTypeClasses = cord.list(ImpTypeClassesCord),
    ImpForeignEnums0 = cord.list(ImpForeignEnumsCord),

    BothTypesMap = multi_map.merge(IntTypesMap, ImpTypesMap),
    % Compute the set of type_ctors whose definitions in the implementation
    % section we need to preserve, possibly in abstract form (that is
    % figured out below).
    %
    % Also, work out which modules we will need access to due to the
    % definitions of equivalence types, foreign types, dummy, enum and other
    % du types types whose definitions we are keeping in the implementation
    % section.
    get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
        BothTypesMap, NeededImpTypeCtors, ImpModulesNeededByTypeDefns),
    set.union(ImpModulesNeededByTypeClassDefns, ImpModulesNeededByTypeDefns,
        ImpNeededModules),

    % XXX ITEM_LIST We should put a use_module decl into the interface
    % of the .int file ONLY IF the module is actually used in the interface.
    %
    % We already *do* generate warnings for any modules we import or use
    % in the interface that are not required in the interface, and programmers
    % do tend to delete such unnecessary imports from the interface,
    % so fixing this overestimation is not all that urgent.
    %
    % Since everything we put into a .int file should be fully module
    % qualified, we convert all import_modules into use_modules.
    ( if set.is_empty(ImpNeededModules) then
        % This gets the same result as the else case, only more quickly.
        set.init(ImpImportsUses)
    else
        ImpImportsUses2 = set.intersect(ImpImportsUses1, ImpNeededModules),
        ImpImportsUses = set.difference(ImpImportsUses2, IntImportsUses)
    ),

    % Compute the list of type definitions we deleted from ImpItems0
    % that we want to add back to the implementation section,
    % possibly in their abstract form.
    map.foldl2(
        maybe_add_maybe_abstract_type_defns(BothTypesMap, IntTypesMap,
            NeededImpTypeCtors),
        ImpTypesMap, [], ImpTypeDefns,
        ImpImplicitFIMLangs1, ImpImplicitFIMLangs2),

    % Figure out which of the foreign enum items we deleted from ImpItems0
    % we want to add back to the implementation section.
    % Record the needs of these foreign enum items for
    % foreign_import_module items.
    list.foldl2(add_foreign_enum_spec_if_needed(IntTypesMap),
        ImpForeignEnums0, [], ImpForeignEnums,
        ImpImplicitFIMLangs2, ImpImplicitFIMLangs),

    set.foldl(add_self_fim(ModuleName), IntImplicitFIMLangs,
        IntExplicitFIMSpecs, IntFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ImpImplicitFIMLangs,
        ImpExplicitFIMSpecs, ImpFIMSpecs0),
    set.difference(ImpFIMSpecs0, IntFIMSpecs, ImpFIMSpecs),

    DummyMaybeVersionNumbers = no_version_numbers,
    % XXX TODO
    IntTypeRepns = [],
    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        DummyMaybeVersionNumbers, IntIncls, ImpIncls,
        IntImportsUses, ImpImportsUses, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns,
        IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls,
        IntPragmas, IntPromises,
        IntTypeRepns,
        ImpTypeDefns, ImpForeignEnums, ImpTypeClasses).

%---------------------%

:- pred add_self_fim(module_name::in, foreign_language::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

add_self_fim(ModuleName, Lang, !FIMSpecs) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    set.insert(FIMSpec, !FIMSpecs).

%---------------------%

:- type type_defn_map == multi_map(type_ctor, item_type_defn_info).
:- type type_defn_pair == pair(type_ctor, item_type_defn_info).

    % get_interface_int1_item_blocks_loop(SrcItemBlocks,
    %   !IntIncls, !ImpIncls, !IntImportsUses, !ImpImportsUses,
    %   !IntExplicitFIMSpecs, !ImpExplicitFIMSpecs,
    %   !IntImplicitFIMLangs, !ImpImplicitFIMLangs,
    %   !IntItemsCord, !IntTypesMap, !IntTypeDefnsCord,
    %   !IntInstDefnsCord, !IntModeDefnsCord,
    %   !IntTypeClassesCord, !IntInstancesCord,
    %   !IntPredDeclsCord, !IntModeDeclsCord,
    %   !IntPragmasCord, !IntPromisesCord, !IntModulesNeededByTypeClassDefns,
    %   !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
    %   !ImpModulesNeededByTypeClassDefns):
    %
    % Do the bulk of the work needed to generate a module's .int file
    % from the module's augmented compilation unit.
    %
    % The input is the list of source item blocks in the module's
    % augmented compilation unit, *after* they have been processed
    %
    % - by generate_pre_grab_pre_qual_item_blocks_int1_int2, which deletes
    %   most kinds of items from implementation sections, and
    %
    % - by having everything fully module qualified (in the absence of any
    %   qualification errors).
    %
    % Our outputs are as follows.
    %
    % !:IntIncls and !:ImpIncls are the module names included in the
    % interface and implementation sections respectively.
    %
    % !:IntImportsUses and !:ImpImportsUses are the modules imported and/or
    % used in the interface and implementation sections respectively.
    %
    % !:IntExplicitFIMSpecs and !:ImpExplicitFIMSpecs are the
    % foreign_import_modules explicitly includes in the
    % the interface and implementation sections respectively.
    % !:IntImplicitFIMLangs and !:ImpImplicitFIMLangs are the languages
    % for which we need to generate an implicit foreign_import_module
    % for the current module, as needed by the interface and implementation
    % items we return in the following arguments.
    %
    % !:IntItems is a list of all the items in the interface. Our caller
    % should use it solely to check whether the interface is empty.
    % XXX Why do we need that check when creating .int files?
    %
    % The arguments from !:IntTypeDefnsCord to !:IntPromisesCord
    % contain the parts of the interface section of the source module
    % that we want to include in the .int file. Likewise,
    % the arguments !:ImpTypeClassesCord and !:ImpForeignEnumsCord
    % contain parts of the implementation section of the source module
    % that we want to include in the .int file. However, until we switch over
    % to using type_repn items, we also need to include in the .int file
    % a subset of the type definitions from the implementation section.
    %
    % We return !:IntTypesMap and !:ImpTypesMap, which contain
    % the type definitions in the interface and implementation sections
    % respectively, to our caller as the raw material for it to use in
    % computing that subset. They both map the type constructors
    % of the types defined in the interface and implementation sections
    % respectively to the list of definitions of that type. (We need a list
    % for two reasons: because a type may be defined both in Mercury and
    % in several target languages, and because of the potential presence
    % of erroneous double definitions.)
    %
    % !:{Int,Imp}ModulesNeededByTypeClassDefns are the sets of the modules
    % needed by any superclass constraints on any typeclass declaration
    % in the interface and the implementation sections. A module may be needed
    % because it defines the typeclass in the superclass constraint,
    % or because it defines a type constructor mentioned anywhere
    % in the superclass constraint.
    %
:- pred get_interface_int1_item_blocks_loop(list(src_item_block)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(fim_spec)::in, set(fim_spec)::out,
    set(fim_spec)::in, set(fim_spec)::out,
    set(foreign_language)::in, set(foreign_language)::out,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_pragma_info)::in, cord(item_pragma_info)::out,
    cord(item_promise_info)::in, cord(item_promise_info)::out,
    set(module_name)::in, set(module_name)::out,
    type_defn_map::in, type_defn_map::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(foreign_enum_spec)::in, cord(foreign_enum_spec)::out,
    set(module_name)::in, set(module_name)::out) is det.

get_interface_int1_item_blocks_loop([],
        !IntIncls, !ImpIncls, !IntImportsUses, !ImpImportsUses,
        !IntExplicitFIMSpecs, !ImpExplicitFIMSpecs,
        !IntImplicitFIMLangs, !ImpImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord,
        !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord, !IntModulesNeededByTypeClassDefns,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns).
get_interface_int1_item_blocks_loop([SrcItemBlock | SrcItemBlocks],
        !IntIncls, !ImpIncls, !IntImportsUses, !ImpImportsUses,
        !IntExplicitFIMSpecs, !ImpExplicitFIMSpecs,
        !IntImplicitFIMLangs, !ImpImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord,
        !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord, !IntModulesNeededByTypeClassDefns,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns) :-
    SrcItemBlock = item_block(_, SrcSection, Incls, Avails, FIMs, Items),
    (
        SrcSection = sms_interface,
        list.foldl(add_included_module_name, Incls, !IntIncls),
        get_interface_int1_avails_loop(Avails, !IntImportsUses),
        list.foldl(add_fim_to_specs, FIMs, !IntExplicitFIMSpecs),
        get_interface_int1_items_loop_int(Items, !IntImplicitFIMLangs,
            !IntTypesMap, !IntTypeDefnsCord,
            !IntInstDefnsCord, !IntModeDefnsCord,
            !IntTypeClassesCord, !IntInstancesCord,
            !IntPredDeclsCord, !IntModeDeclsCord,
            !IntPragmasCord, !IntPromisesCord,
            !IntModulesNeededByTypeClassDefns)
    ;
        SrcSection = sms_implementation,
        list.foldl(add_included_module_name, Incls, !ImpIncls),
        get_interface_int1_avails_loop(Avails, !ImpImportsUses),
        list.foldl(add_fim_to_specs, FIMs, !ImpExplicitFIMSpecs),
        get_interface_int1_items_loop_imp(Items, !ImpImplicitFIMLangs,
            !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
            !ImpModulesNeededByTypeClassDefns)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        % The code that transforms sms_implementation to this
        % should not have been run.
        unexpected($pred, "sms_impl_but_exported_to_submodules")
    ),
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        !IntIncls, !ImpIncls, !IntImportsUses, !ImpImportsUses,
        !IntExplicitFIMSpecs, !ImpExplicitFIMSpecs,
        !IntImplicitFIMLangs, !ImpImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord,
        !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord, !IntModulesNeededByTypeClassDefns,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns).

:- pred get_interface_int1_avails_loop(list(item_avail)::in,
    set(module_name)::in, set(module_name)::out) is det.

get_interface_int1_avails_loop([], !ImportsUses).
get_interface_int1_avails_loop([Avail | Avails], !ImportsUses) :-
    ( Avail = avail_import(avail_import_info(ModuleName, _Ctxt, _SeqNum))
    ; Avail = avail_use(avail_use_info(ModuleName, _Ctxt, _SeqNum))
    ),
    set.insert(ModuleName, !ImportsUses),
    get_interface_int1_avails_loop(Avails, !ImportsUses).

:- pred get_interface_int1_items_loop_int(list(item)::in,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_pragma_info)::in, cord(item_pragma_info)::out,
    cord(item_promise_info)::in, cord(item_promise_info)::out,
    set(module_name)::in, set(module_name)::out) is det.

get_interface_int1_items_loop_int([], !IntImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord, !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord,
        !IntModulesNeededByTypeClassDefns).
get_interface_int1_items_loop_int([Item | Items], !IntImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord, !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord,
        !IntModulesNeededByTypeClassDefns) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn =
            item_type_defn_info(Name, TypeParams, TypeDefn, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !IntTypesMap),
        cord.snoc(ItemTypeDefn, !IntTypeDefnsCord),
        ( if
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _)
        then
            Lang = foreign_type_language(ForeignType),
            set.insert(Lang, !IntImplicitFIMLangs)
        else
            true
        )
    ;
        Item = item_typeclass(ItemTypeClass),
        cord.snoc(ItemTypeClass, !IntTypeClassesCord),
        % The superclass constraints on the typeclass being declared
        % may refer to typeclasses that this module has imported.
        Constraints = ItemTypeClass ^ tc_superclasses,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !IntModulesNeededByTypeClassDefns)
    ;
        Item = item_clause(_)
        % A clause in the interface is a bug, but it should be reported
        % when we try to generate code for the module.
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _MaybeAttrs, _Context, _SeqNum),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no
            % A clause-like pragma in the interface is a bug, but it should be
            % reported when we try to generate code for the module.
        ;
            AllowedInInterface = yes,
            cord.snoc(ItemPragma, !IntPragmasCord),
            Langs = pragma_needs_foreign_imports(Pragma),
            ( if Pragma = pragma_foreign_enum(_) then
                set.insert_list(Langs, !IntImplicitFIMLangs)
            else
                expect(unify(Langs, []), $pred,
                    "interface pragma other than foreign_enum needs Langs")
            )
        )
    ;
        Item = item_promise(ItemPromise),
        PromiseType = ItemPromise ^ prom_type,
        (
            PromiseType = promise_type_true
            % Ignore the item, which deletes it.
            % XXX ITEM_LIST We should reconsider this decision,
            % since it limits optimization opportunities in other modules.
        ;
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            cord.snoc(ItemPromise, !IntPromisesCord)
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        cord.snoc(ItemInstDefn, !IntInstDefnsCord)
    ;
        Item = item_mode_defn(ItemModeDefn),
        cord.snoc(ItemModeDefn, !IntModeDefnsCord)
    ;
        Item = item_pred_decl(ItemPredDecl),
        cord.snoc(ItemPredDecl, !IntPredDeclsCord)
    ;
        Item = item_mode_decl(ItemModeDecl),
        cord.snoc(ItemModeDecl, !IntModeDeclsCord)
    ;
        Item = item_instance(ItemInstance),
        cord.snoc(ItemInstance, !IntInstancesCord)
    ;
        ( Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        )
        % The presence of these items in the interface section is an error.
        % We delay reporting this bug until we generate target code
        % for this module. (Aborting would not be a good idea; it would
        % lead to the failure of tests/valid/mutable_interface_in_main.)
        % I (zs) think they should be silently deleted here;
        % the error should be reported when we try to generate code
        % for this module.
    ;
        Item = item_type_repn(_),
        % We shouldn't yet have invoked any code that creates
        % type_repn items.
        unexpected($pred, "item_type_repn")
    ),
    get_interface_int1_items_loop_int(Items, !IntImplicitFIMLangs,
        !IntTypesMap, !IntTypeDefnsCord, !IntInstDefnsCord, !IntModeDefnsCord,
        !IntTypeClassesCord, !IntInstancesCord,
        !IntPredDeclsCord, !IntModeDeclsCord,
        !IntPragmasCord, !IntPromisesCord,
        !IntModulesNeededByTypeClassDefns).

:- pred get_interface_int1_items_loop_imp(list(item)::in,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out,
    cord(foreign_enum_spec)::in, cord(foreign_enum_spec)::out,
    set(module_name)::in, set(module_name)::out) is det.

get_interface_int1_items_loop_imp([], !ImpImplicitFIMLangs,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns).
get_interface_int1_items_loop_imp([Item | Items], !ImpImplicitFIMLangs,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, TypeParams, _, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !ImpTypesMap)
        % We don't add this to an item cord yet -- we may be removing it.
        % If we *do* want the items for a given type_ctor, we will create
        % new copies of the items from the type_ctor's entry in ImpTypesMap.
    ;
        Item = item_typeclass(ItemTypeClass),
        cord.snoc(ItemTypeClass, !ImpTypeClassesCord),
        % The superclass constraints on the typeclass being declared
        % may refer to typeclasses that this module has imported.
        Constraints = ItemTypeClass ^ tc_superclasses,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !ImpModulesNeededByTypeClassDefns)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, MaybeAttrs, _Context, _SeqNum),
        ( if Pragma = pragma_foreign_enum(FEInfo) then
            FESpec = foreign_enum_spec(FEInfo, MaybeAttrs),
            cord.snoc(FESpec, !ImpForeignEnumsCord),
            FEInfo = pragma_info_foreign_enum(FELang, _, _),
            set.insert(FELang, !ImpImplicitFIMLangs)
        else
            unexpected($pred, "non-foreign-enum pragma")
        )
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_instance(_)
        ; Item = item_clause(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        unexpected($pred,
            "generate_pre_grab_pre_qual_items_imp should've deleted imp item")
    ),
    get_interface_int1_items_loop_imp(Items, !ImpImplicitFIMLangs,
        !ImpTypesMap, !ImpTypeClassesCord, !ImpForeignEnumsCord,
        !ImpModulesNeededByTypeClassDefns).

:- pred add_included_module_name(item_include::in,
    set(module_name)::in, set(module_name)::out) is det.

add_included_module_name(Incl, !ModuleNames) :-
    set.insert(item_include_module_name(Incl), !ModuleNames).

:- pred add_fim_to_specs(item_fim::in, 
    set(fim_spec)::in, set(fim_spec)::out) is det.

add_fim_to_specs(FIM, !FIMSpecs) :-
    FIM = item_fim(Lang, ModuleName, _Context, _SeqNum),
    FIMSpec = fim_spec(Lang, ModuleName),
    set.insert(FIMSpec, !FIMSpecs).

%---------------------------------------------------------------------------%

:- pred accumulate_modules_from_constraint(prog_constraint::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_from_constraint(Constraint, !Modules) :-
    Constraint = constraint(ClassName, ArgTypes),
    (
        ClassName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        ClassName = unqualified(_),
        unexpected($pred, "unknown typeclass in constraint")
    ),
    accumulate_modules_from_types(ArgTypes, !Modules).

:- pred accumulate_modules_from_types(list(mer_type)::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_from_types([], !Modules).
accumulate_modules_from_types([Type | Types], !Modules) :-
    accumulate_modules_from_type(Type, !Modules),
    accumulate_modules_from_types(Types, !Modules).

:- pred accumulate_modules_from_type(mer_type::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_from_type(Type, !Modules) :-
    (
        % Do nothing for these types - they cannot affect the set of
        % implementation imports in an interface file.
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(TypeName, ArgTypes, _),
        det_sym_name_get_module_name(TypeName, ModuleName),
        set.insert(ModuleName, !Modules),
        accumulate_modules_from_types(ArgTypes, !Modules)
    ;
        Type = kinded_type(KindedType, _),
        accumulate_modules_from_type(KindedType, !Modules)
    ;
        ( Type = tuple_type(ArgTypes, _)
        ; Type = apply_n_type(_, ArgTypes, _)
        ; Type = higher_order_type(_, ArgTypes, _HOInstInfo, _, _)
        ),
        % XXX ITEM_LIST accumulate modules from _HOInstInfo
        accumulate_modules_from_types(ArgTypes, !Modules)
    ).

%---------------------------------------------------------------------------%

    % get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
    %   BothTypesMap, NeededTypeCtors, ModulesNeededByTypeDefns):
    %
    % Compute NeededTypeCtors, the set of type constructors whose definitions
    % we need to keep in the implementation section of the .int file
    % (in their original or abstract form), and ModulesNeededByTypeDefns,
    % the set of modules whose :- import_module and :- use_module declarations
    % we need to keep because they define type_ctors used in these kept
    % type definitions.
    %
    % We do this using two passes.
    %
    % In the first pass, we process every type with a definition in the
    % implementation.
    %
    % - If that definition is equivalence type definition, and there is
    %   any definition of that same type_ctor in the interface (presumably
    %   but necessarily as an abstract type), then include the type_ctor
    %   in AbsExpEqvLhsTypeCtors. We include these type_ctors in
    %   NeededImpTypeCtors because on 32-bit platforms, if type t1 is
    %   defined to be equivalent to a 64 bit float, then we need to take
    %   this into account when deciding the representation of types
    %   with t1 fields even if type t1 is abstract exported.
    %   XXX TYPE_REPN We should convey this info in type_repn items,
    %   not type_defn items, since the latter can be used for purposes
    %   other than type representation.
    %
    % - We handle foreign type definitions the same way as equivalence type
    %   definitions, just in case the foreign type is also bigger than a word.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %   XXX TYPE_REPN Shouldn't boxing make the size of the foreign type
    %   immaterial?
    %
    % - If the definition defines an enum type, and there is a definition
    %   of the same type_ctor in the interface, we include the type_ctor in
    %   AbsExpEnumTypeCtors. This is so that when we abstract export
    %   the type_ctor, we can record that its size is less than one word.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %
    % - If the definition defines a dummy type, we include the type_ctor in
    %   DirectDummyTypeCtors. XXX ITEM_LIST Presumably (by me -zs) this is
    %   so that when we abstract export them, we can record that it needs
    %   no storage. XXX However, we currently include dummy types in the
    %   implementation section of the .int file unchanged, and we do so
    %   even if the type is not mentioned in the interface section at all.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %
    % The first pass ignores all other type definitions.
    %
    % The second pass processes the type_ctors in AbsExpEqvLhsTypeCtors,
    % i.e. the abstract exported type_ctors which have an equivalence type
    % or foreign type definition in the implementation section. Its job
    % is to compute three sets.
    %
    % - The first set is AbsExpEqvRhsTypeCtors, the set of type_ctors
    %   that occur in any (partial or full) expansion of an equivalence type
    %   in AbsExpEqvLhsTypeCtors. This means that if e.g. type t2 is abstract
    %   exported and its definition in the implementation section is
    %
    %       :- type t2 == t3(t4, t5).
    %       :- type t3(A, B) ---> ... a discriminated union definition ...
    %       :- type t4 --->       ... a discriminated union definition ...
    %       :- type t5 == t6.
    %       :- type t6 --->       ... a discriminated union definition ...
    %
    %   then we return {t2, t3, t4, t5, t6} as AbsExpEqvRhsTypeCtors.
    %
    % - The second set is DuArgTypeCtors, the set of type_ctors that occur
    %   on the right hand side (i.e. among the field argument types) of
    %   a discriminated union definition of a type_ctor that is in
    %   AbsExpEqvLhsTypeCtors, which should happen only that type_ctor
    %   also has foreign language definitions (since we put a type_ctor
    %   into AbsExpEqvLhsTypeCtors only if it has either an equivalence
    %   or a foreign language definition). If these type_ctors are not
    %   otherwise included in the .int file, this will cause our caller
    %   to include an abstract declaration of these type_ctors in the
    %   .int file, to disambiguate the references to these types
    %   in the full (in the sense of non-abstractified) du Mercury definitions
    %   we include in the .int file next to the foreign language definitions.
    %
    % - The third set we return is ModulesNeededByTypeDefns, which consists
    %   of the names of the modules that define the type_ctors in the first
    %   two sets.
    %
    % XXX ITEM_LIST The comment lines starting with a double percent
    % are the comment on the original version of this predicate.
    %
    %% Figure out the set of abstract equivalence type constructors (i.e.
    %% the types that are exported as abstract types and which are defined
    %% in the implementation section as equivalence types or as foreign types).
    %% Return in NeededTypeCtors the smallest set containing those
    %% constructors, and the set of private type constructors referred to
    %% by the right hand side of any type in NeededTypeCtors.
    %%
    %% XXX Return in DirectDummyTypeCtors the set of dummy type constructors.
    %%
    %% Given a du type definition in the implementation section, we should
    %% include it in AbsImpExpLhsTypeCtors if the type constructor is abstract
    %% exported and the implementation section also contains a foreign_type
    %% definition of the type constructor.
    %%
    %% Given a enumeration type definition in the implementation section, we
    %% should include it in AbsImpExpEnumTypeCtors if the type constructor is
    %% abstract exported.
    %%
    %% Return in NeededModules the set of modules that define the type
    %% constructors in NeededTypeCtors.
    %
:- pred get_requirements_of_imp_exported_types(type_defn_map::in,
    type_defn_map::in, type_defn_map::in,
    set(type_ctor)::out, set(module_name)::out) is det.

get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
        BothTypesMap, NeededImpTypeCtors, ModulesNeededByTypeDefns) :-
    map.foldl3(
        accumulate_abs_imp_exported_type_lhs(IntTypesMap, BothTypesMap),
        ImpTypesMap, set.init, AbsExpEqvLhsTypeCtors,
        set.init, AbsExpEnumTypeCtors, set.init, DirectDummyTypeCtors),
    set.fold3(accumulate_abs_imp_exported_type_rhs(ImpTypesMap),
        AbsExpEqvLhsTypeCtors,
        set.init, AbsExpEqvRhsTypeCtors, set.init, DuArgTypeCtors,
        set.init, ModulesNeededByTypeDefns),
    NeededImpTypeCtors = set.union_list([AbsExpEqvLhsTypeCtors,
        AbsExpEqvRhsTypeCtors, AbsExpEnumTypeCtors, DirectDummyTypeCtors,
        DuArgTypeCtors]).

:- pred accumulate_abs_imp_exported_type_lhs(type_defn_map::in,
    type_defn_map::in, type_ctor::in, list(item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfos, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DirectDummyTypeCtors) :-
    (
        ImpItemTypeDefnInfos = [],
        % The list of values for a key in a multi_map should never be empty.
        unexpected($pred, "ImpItemTypeDefnInfos = []")
    ;
        ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo],
        % Don't construct a closure when a type_ctor has only one definition
        % in the implementation section, since this the common case.
        accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap, BothTypesMap,
            TypeCtor, ImpItemTypeDefnInfo,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors,
            !DirectDummyTypeCtors)
    ;
        ImpItemTypeDefnInfos = [_, _ | _],
        % A type may have multiple definitions in the implementation section
        % because it may be defined both in Mercury and in a foreign language.
        % A type with multiple definitions doesn't typically include
        % an equivalence type among those definitions, but we have to be
        % prepared for such an eventuality anyway.
        list.foldl3(
            accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap,
                BothTypesMap, TypeCtor),
            ImpItemTypeDefnInfos,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors,
            !DirectDummyTypeCtors)
    ).

:- pred accumulate_abs_imp_exported_type_lhs_in_defn(type_defn_map::in,
    type_defn_map::in, type_ctor::in, item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfo, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DirectDummyTypeCtors) :-
    ImpItemTypeDefnInfo = item_type_defn_info(_, _, ImpTypeDefn, _, _, _),
    (
        ImpTypeDefn = parse_tree_eqv_type(_),
        ( if map.search(IntTypesMap, TypeCtor, _) then
            set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors)
        else
            true
        )
    ;
        ImpTypeDefn = parse_tree_foreign_type(_),
        ( if map.search(IntTypesMap, TypeCtor, _) then
            % XXX ITEM_LIST This looks like a lost opportunity to me (zs),
            % because the only foreign types that *need* the same treatment
            % as equivalence types are foreign types that are bigger than
            % one word in size. The ones that have can_pass_as_mercury_type
            % as an attribute are supposed to fit into one word (though
            % that assertion may be valid for some platforms only) and thus
            % *could* be left out of !AbsExpEqvLhsTypeCtors.
            %
            % However, before making such a change, consider everything
            % in the discussion on this topic on m-rev on 2019 feb 18-19.
            set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors)
        else
            true
        )
    ;
        ImpTypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeEqCmp, MaybeDirectArgCtors),
        ( if
            map.search(IntTypesMap, TypeCtor, _),
            du_type_is_enum(DetailsDu, _NumBits)
        then
            set.insert(TypeCtor, !AbsExpEnumTypeCtors)
        else if
            % XXX ITEM_LIST Why don't we insist that TypeCtor occurs
            % in IntTypesMap?
            % XXX ITEM_LIST If a type has one function symbol with arity one
            % and the argument type is equivalent to a dummy type that is
            % defined in another module, we will NOT include TypeCtor in
            % !DirectDummyTypeCtors, since we won't know enough about
            % the contents of the other module.
            constructor_list_represents_dummy_argument_type(BothTypesMap,
                OoMCtors, MaybeEqCmp, MaybeDirectArgCtors)
        then
            set.insert(TypeCtor, !DirectDummyTypeCtors)
        else
            true
        )
    ;
        ( ImpTypeDefn = parse_tree_abstract_type(_)
        ; ImpTypeDefn = parse_tree_solver_type(_)
        )
    ).

:- pred accumulate_abs_imp_exported_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_imp_exported_type_rhs(ImpTypesMap, TypeCtor,
        !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns) :-
    ( if map.search(ImpTypesMap, TypeCtor, ImpTypeDefns) then
        list.foldl3(
            accumulate_abs_eqv_type_rhs_in_defn(ImpTypesMap),
            ImpTypeDefns,
            !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns)
    else
        % TypeCtor is not defined in the implementation section
        % of this module.
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_in_defn(type_defn_map::in,
    item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_in_defn(ImpTypesMap, ImpItemTypeDefnInfo,
        !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns) :-
    ImpItemTypeDefnInfo = item_type_defn_info(_, _, ImpTypeDefn, _, _, _),
    (
        ImpTypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(RhsType),
        type_to_user_type_ctor_set(RhsType, set.init, RhsTypeCtors),

        % Logically, we want to invoke the call to set.union and the
        % calls to set.foldl/foldl3 below on all RhsTypeCtors. However, for
        % any type_ctor in RhsTypeCtors that is in !.AbsExpEqvRhsTypeCtors,
        % we have alteady done so, and since all three operations are
        % idempotent, there is no point in invoking them again.
        set.difference(RhsTypeCtors, !.AbsExpEqvRhsTypeCtors, NewRhsTypeCtors),
        set.union(NewRhsTypeCtors, !AbsExpEqvRhsTypeCtors),
        set.fold(accumulate_modules_used_by_type_ctor, NewRhsTypeCtors,
            !ModulesNeededByTypeDefns),
        % XXX ITEM_LIST I (zs) *think* that the reason why we ignore the
        % result of the second accumulator (!DuArgTypeCtors) in this call
        % is because the appearance of a type_ctor in RhsTypeCtors
        % on the right hand side of an equivalence type definition
        % will (by itself) only generate an abstract definition for that
        % type_ctor in the .int file, so other modules need not know about
        % any type_ctors just because they appear on the right hand side
        % of *its* definition. However, I am far from sure.
        set.fold3(accumulate_abs_imp_exported_type_rhs(ImpTypesMap),
            NewRhsTypeCtors,
            !AbsExpEqvRhsTypeCtors, set.init, _, !ModulesNeededByTypeDefns)
    ;
        ImpTypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, _, _),
        % There must exist a foreign type alternative to this type.
        % XXX ITEM_LIST I (zs) would like to see a proof argument for that,
        % since I don't think it is true. Unfortunately, we cannot check it
        % locally.

        % As the du type will be exported, we require all the type_ctors
        % inside all the argument types of all the data constructors, and the
        % modules that define them.
        ctors_to_user_type_ctor_set(one_or_more_to_list(OoMCtors),
            set.init, RhsTypeCtors),
        set.union(RhsTypeCtors, !DuArgTypeCtors),
        set.fold(accumulate_modules_used_by_type_ctor, RhsTypeCtors,
            !ModulesNeededByTypeDefns)
    ;
        ( ImpTypeDefn = parse_tree_abstract_type(_)
        ; ImpTypeDefn = parse_tree_solver_type(_)
        ; ImpTypeDefn = parse_tree_foreign_type(_)
        )
    ).

%---------------------%

:- pred accumulate_modules_used_by_type_ctor(type_ctor::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_used_by_type_ctor(TypeCtor, !Modules) :-
    TypeCtor = type_ctor(SymName, _Arity),
    (
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        SymName = unqualified(_),
        % Our ancestor generate_interfaces_int1_int2 should be invoked
        % only *after* the module qualification of the augmented compilation
        % unit whose contents we are now processing.
        unexpected($pred, "unqualified type encountered")
    ).

%---------------------%

    % Given a type, return the set of user-defined type constructors
    % occurring in it. We do not gather the type constructors of
    % builtin types, higher-order types and typle types, because
    % are always available without any module needing to be imported,
    % which is what our caller uses our results for.
    %
:- pred type_to_user_type_ctor_set(mer_type::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

type_to_user_type_ctor_set(Type, !TypeCtors) :-
    ( if type_to_ctor_and_args(Type, TypeCtor, ArgTypes) then
        TypeCtor = type_ctor(SymName, _Arity),
        ( if
            ( is_builtin_type_sym_name(SymName)
            ; type_ctor_is_higher_order(TypeCtor, _, _, _)
            ; type_ctor_is_tuple(TypeCtor)
            )
        then
            true
        else
            set.insert(TypeCtor, !TypeCtors)
        ),
        list.foldl(type_to_user_type_ctor_set, ArgTypes, !TypeCtors)
    else
        true
    ).

:- pred ctors_to_user_type_ctor_set(list(constructor)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctors_to_user_type_ctor_set([], !TypeCtors).
ctors_to_user_type_ctor_set([Ctor | Ctors], !TypeCtors) :-
    Ctor = ctor(_, _, _, CtorArgs, _, _),
    ctor_args_to_user_type_ctor_set(CtorArgs, !TypeCtors),
    ctors_to_user_type_ctor_set(Ctors, !TypeCtors).

:- pred ctor_args_to_user_type_ctor_set(list(constructor_arg)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctor_args_to_user_type_ctor_set([], !TypeCtors).
ctor_args_to_user_type_ctor_set([Arg | Args], !TypeCtors) :-
    Arg = ctor_arg(_, Type, _),
    type_to_user_type_ctor_set(Type, !TypeCtors),
    ctor_args_to_user_type_ctor_set(Args, !TypeCtors).

%---------------------%

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % See the documentation for `type_util.check_dummy_type' for the definition
    % of a dummy type.
    %
    % NOTE: changes here may require changes to `type_util.check_dummy_type'.
    %
:- pred constructor_list_represents_dummy_argument_type(type_defn_map::in,
    one_or_more(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in) is semidet.

constructor_list_represents_dummy_argument_type(TypeDefnMap,
        OoMCtors, MaybeCanonical, MaybeDirectArgCtors) :-
    constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
        OoMCtors, MaybeCanonical, MaybeDirectArgCtors, []).

:- pred constructor_list_represents_dummy_argument_type_2(type_defn_map::in,
    one_or_more(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in, list(mer_type)::in) is semidet.

constructor_list_represents_dummy_argument_type_2(TypeDefnMap, OoMCtors,
        canon, no, CoveredTypes) :-
    OoMCtors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _Name, CtorArgs, _Arity,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    (
        % A single zero-arity constructor.
        CtorArgs = []
    ;
        % A constructor with a single dummy argument.
        CtorArgs = [ctor_arg(_, ArgType, _)],
        ctor_arg_is_dummy_type(TypeDefnMap, ArgType, CoveredTypes) = yes
    ).

:- func ctor_arg_is_dummy_type(type_defn_map, mer_type, list(mer_type)) = bool.

ctor_arg_is_dummy_type(TypeDefnMap, Type, CoveredTypes0) = IsDummyType :-
    (
        Type = defined_type(SymName, TypeArgs, _Kind),
        ( if list.member(Type, CoveredTypes0) then
            % The type is circular.
            IsDummyType = no
        else
            Arity = list.length(TypeArgs),
            TypeCtor = type_ctor(SymName, Arity),
            ( if
                (
                    is_type_ctor_a_builtin_dummy(TypeCtor)
                        = is_builtin_dummy_type_ctor
                ;
                    % Can we find a definition of the type that tells us
                    % it is a dummy type?
                    multi_map.search(TypeDefnMap, TypeCtor, ItemTypeDefnInfos),
                    list.member(ItemTypeDefnInfo, ItemTypeDefnInfos),
                    TypeDefn = ItemTypeDefnInfo ^ td_ctor_defn,
                    TypeDefn = parse_tree_du_type(DetailsDu),
                    DetailsDu = type_details_du(OoMCtors, MaybeEqCmp,
                        MaybeDirectArgCtors),
                    constructor_list_represents_dummy_argument_type_2(
                        TypeDefnMap, OoMCtors, MaybeEqCmp, MaybeDirectArgCtors,
                        [Type | CoveredTypes0])
                )
            then
                IsDummyType = yes
            else
                IsDummyType = no
            )
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ),
        IsDummyType = no
    ;
        Type = kinded_type(_, _),
        unexpected($pred, "kinded_type")
    ).

%---------------------%

    % Given a type constructor's type definitions from the implementation
    % section, as recorded in ImpTypesMap, include their abstract versions
    % in !ImpTypeDefnItems, the list of type definition items scheduled to be
    % added back to the implementation section, *provided* that
    %
    % - the type constructor is in NeededTypeCtors, and
    %
    % - *either* the type has no declaration or definition in the interface,
    %   *or* at least one of the type definitions in the implementation section
    %   contains more information than a pure abstract type declaration
    %   (such as may be found in the interface section) would.
    %
    % By "pure abstract" type declarations, we mean abstract type
    % declarations that give no further implementation. This means that
    % `type_is_abstract_enum' declarations are not *pure* abstract.
    % XXX ITEM_LIST I (zs) believe that the intention behind this proviso
    % was to allow items representing the following scenario to be left
    % alone:
    %
    % :- interface.
    % :- type t1.
    % ...
    % :- implementation.
    % :- type t1 where type_is_abstract_enum(...).
    %
    % XXX ITEM_LIST Just because *one* definition in the implementation has
    % more info than a pure abstract type declaration *should not* result in
    % us adding back to the implementation section any other type definitions
    % that *do* represent nothing more than a pure abstract type declaration.
    % Note that this distinction should matter only for types whose set of
    % definitions are erroneous, such a type that is defined both as
    % an equivalence type and as a du type.
    %
:- pred maybe_add_maybe_abstract_type_defns(
    type_defn_map::in, type_defn_map::in, set(type_ctor)::in,
    type_ctor::in, list(item_type_defn_info)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

maybe_add_maybe_abstract_type_defns(BothTypesMap, IntTypesMap,
        NeededTypeCtors, TypeCtor, ImpItemTypeDefnInfos,
        !ImpTypeDefns, !ImpImplicitFIMLangs) :-
    ( if
        set.member(TypeCtor, NeededTypeCtors),
        make_imp_types_abstract(BothTypesMap,
            ImpItemTypeDefnInfos, AbstractImpItemTypeDefnInfos),
        not (
            multi_map.contains(IntTypesMap, TypeCtor),
            list.all_true(is_pure_abstract_type_defn,
                AbstractImpItemTypeDefnInfos)
        )
    then
        add_type_defn_items(AbstractImpItemTypeDefnInfos,
            !ImpTypeDefns, !ImpImplicitFIMLangs)
    else
        true
    ).

:- pred is_pure_abstract_type_defn(item_type_defn_info::in) is semidet.

is_pure_abstract_type_defn(ImpItemTypeDefnInfo) :-
    ImpItemTypeDefnInfo ^ td_ctor_defn = parse_tree_abstract_type(Details),
    % XXX ITEM_LIST This test may do the wrong thing for
    % abstract_{dummy,notag,solver}_types, once we start generating them.
    Details \= abstract_type_fits_in_n_bits(_).

:- pred make_imp_types_abstract(type_defn_map::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

make_imp_types_abstract(BothTypesMap, !ImpItemTypeDefnInfos) :-
    (
        !.ImpItemTypeDefnInfos = [],
        % The list of values for a key in a multi_map should never be empty.
        unexpected($pred, "!.ImpItemTypeDefnInfos = []")
    ;
        !.ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo0],
        make_imp_type_abstract(BothTypesMap,
            ImpItemTypeDefnInfo0, ImpItemTypeDefnInfo),
        !:ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo]
    ;
        !.ImpItemTypeDefnInfos = [_, _ | _]
        % This type constructor has two or more definitions, which is
        % an error, but it should be reported somewhere else.
        % XXX This is not true. It is perfectly ok for a type constructor
        % to have one Mercury definition as a du type and several foreign
        % language definitions. For these, we probably *should* process
        % the du definition as above.
        % XXX TYPE_REPN In such cases, we should consider replacing
        % the foreign definitions with a new kind of internal-use-only item
        % that records the presence of foreign type definitions for the type,
        % and lists, for each foreign language with a definition, the
        % assertions from that definition, but no more.
    ).

:- pred make_imp_type_abstract(type_defn_map::in,
    item_type_defn_info::in, item_type_defn_info::out) is det.

make_imp_type_abstract(BothTypesMap, !ImpItemTypeDefnInfo) :-
    % XXX TYPE_REPN We should record the aspects of the type definition
    % that are relevant to type representation (such as "is dummy",
    % "fits in n bits", "is equivalent to ...") in a type_repn item,
    % and then make the type definition abstract.
    !.ImpItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn0, _, _, _),
    (
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        DetailsDu0 = type_details_du(OoMCtors, MaybeEqCmp,
            MaybeDirectArgCtors),
        ( if
            constructor_list_represents_dummy_argument_type(BothTypesMap,
                OoMCtors, MaybeEqCmp, MaybeDirectArgCtors)
        then
            % Leave dummy types alone.
            true
        else
            ( if du_type_is_enum(DetailsDu0, NumBits) then
                DetailsAbs = abstract_type_fits_in_n_bits(NumBits)
            else
                DetailsAbs = abstract_type_general
            ),
            TypeDefn = parse_tree_abstract_type(DetailsAbs),
            !ImpItemTypeDefnInfo ^ td_ctor_defn := TypeDefn
        )
    ;
        TypeDefn0 = parse_tree_eqv_type(_)
        % XXX TYPE_REPN We currently leave the type definition alone.
        % However, in the future we should test whether the type
        % equivalence is to a type that requires special treatment,
        % either with respect to type representation (because it is smaller
        % than a word, because it is bigger than a word, or because it is
        % guaranteed to be an aligned pointer) or because it needs to be
        % passed in an FP register.
        %
        % If the type does require special treatment, we should generate
        % an item that specifies that treatment, and no more.
        % If the type does not require special treatment, we should
        % generate an item that specifies the absence of a need for
        % special treatment: a simple abstract type definition
        % should suffice.
    ;
        TypeDefn0 = parse_tree_foreign_type(_)
    ;
        TypeDefn0 = parse_tree_abstract_type(_)
        % This type is already abstract.
    ;
        TypeDefn0 = parse_tree_solver_type(_),
        % generate_pre_grab_pre_qual_items_imp should have already made
        % this type abstract.
        unexpected($pred, "solver type should have been made abstract")
    ).

:- pred add_type_defn_items(list(item_type_defn_info)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

add_type_defn_items([], !RevImpTypeDefns, !ImpImplicitFIMLangs).
add_type_defn_items([ImpTypeDefn | ImpTypeDefns],
        !RevImpTypeDefns, !ImpImplicitFIMLangs) :-
    !:RevImpTypeDefns = [ImpTypeDefn | !.RevImpTypeDefns],
    ImpTypeDefn = item_type_defn_info(_, _, TypeDefn, _, _, _),
    ( if
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, _, _)
    then
        set.insert(foreign_type_language(ForeignType), !ImpImplicitFIMLangs)
    else
        true
    ),
    add_type_defn_items(ImpTypeDefns, !RevImpTypeDefns, !ImpImplicitFIMLangs).

%---------------------%

:- pred add_foreign_enum_spec_if_needed(type_defn_map::in,
    foreign_enum_spec::in,
    list(foreign_enum_spec)::in, list(foreign_enum_spec)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

add_foreign_enum_spec_if_needed(IntTypesMap, ForeignEnumSpec,
        !ImpForeignEnumSpecs, !ImpImplicitFIMLangs) :-
    ForeignEnumSpec = foreign_enum_spec(FEInfo, _MaybeAttrs),
    FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, _Values),
    ( if
        map.search(IntTypesMap, TypeCtor, Defns),
        some_type_defn_is_non_abstract(Defns)
    then
        !:ImpForeignEnumSpecs = [ForeignEnumSpec | !.ImpForeignEnumSpecs],
        set.insert(Lang, !ImpImplicitFIMLangs)
    else
        true
    ).

:- pred some_type_defn_is_non_abstract(list(item_type_defn_info)::in)
    is semidet.

some_type_defn_is_non_abstract([Defn | Defns]) :-
    Defn = item_type_defn_info(_, _, Body, _, _, _),
    ( if Body = parse_tree_abstract_type(_) then
        some_type_defn_is_non_abstract(Defns)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % generate_interface_int2(AugCompUnit,
    %   IntInclModuleNames, IntImportsUses,
    %   IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
    %   IntTypeDefnItems, IntInstDefns, IntModeDefns,
    %   IntTypeClasses, IntInstances, ImpTypeDefnItems, ParseTreeInt2):
    %
    % The input arguments should be the relevant parts of the .int1 file
    % computed by our parent.
    %
:- pred generate_interface_int2(globals::in, aug_compilation_unit::in,
    set(module_name)::in, set(module_name)::in,
    set(fim_spec)::in, set(fim_spec)::in,
    list(item_type_defn_info)::in,
    list(item_inst_defn_info)::in, list(item_mode_defn_info)::in,
    list(item_typeclass_info)::in, list(item_instance_info)::in,
    list(item_type_defn_info)::in, type_defn_map::in, type_defn_map::in,
    list(item_pragma_info)::in, list(foreign_enum_spec)::in,
    parse_tree_int2::out) is det.

generate_interface_int2(Globals, AugCompUnit,
        IntInclModuleNames, IntImportsUses,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        IntTypeDefns, IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        ImpTypeDefns, IntTypesMap, ImpTypesMap, IntPragmas, ImpForeignEnums,
        ParseTreeInt2) :-
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        _ModuleVersionNumbers, _SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),

    some [!UnqualSymNames, !UsedModuleNames] (
        !:UnqualSymNames = no_unqual_symnames,
        set.init(!:UsedModuleNames),

        get_int2_items_from_int1_int_type_defn(IntTypeDefns,
            !UnqualSymNames, !UsedModuleNames,
            cord.init, ShortIntTypeDefnsCord,
            set.init, ShortIntImplicitFIMLangs),
        get_int2_items_from_int1_int_inst_defn(IntInstDefns,
            !UnqualSymNames, !UsedModuleNames),
        get_int2_items_from_int1_int_mode_defn(IntModeDefns,
            !UnqualSymNames, !UsedModuleNames),
        get_int2_items_from_int1_int_typeclass(IntTypeClasses,
            !UnqualSymNames, !UsedModuleNames,
            cord.init, ShortIntTypeClassesCord),
        get_int2_items_from_int1_int_instance(IntInstances,
            !UnqualSymNames, !UsedModuleNames,
            cord.init, ShortIntInstancesCord),

        ShortIntTypeDefns = cord.list(ShortIntTypeDefnsCord),
        ShortIntInstDefns = IntInstDefns,
        ShortIntModeDefns = IntModeDefns,
        ShortIntTypeClasses = cord.list(ShortIntTypeClassesCord),
        ShortIntInstances = cord.list(ShortIntInstancesCord),

        UnqualSymNames = !.UnqualSymNames,
        UsedModuleNames = !.UsedModuleNames
    ),

    get_int2_items_from_int1_imp_types(ImpTypeDefns,
        set.init, ShortImpImplicitFIMLangs),

    globals.lookup_bool_option(Globals, experiment1, Experiment1),
    (
        Experiment1 = no,
        ShortIntTypeRepns = []
    ;
        Experiment1 = yes,
        % XXX We should pass to decide_repns_for_simple_types not just
        % the type definitions in this module, but also all the type_REPNs
        % we have read in from the .int3 files of the imported modules.
        % That would allow decide_repns_for_simple_types to take into
        % account that an imported type (such as bool) is subword sized,
        % and that therefore some types that have fields of that type
        % may themselves be subword sized, if all their arguments are subword
        % sized and there are few enough of them. (Note that will in general
        % require fully expanding the relevant type equivalence chains.)
        map.foldl_values(gather_type_defn_items, IntTypesMap,
            [], OrigIntTypeDefns),
        map.foldl_values(gather_type_defn_items, ImpTypesMap,
            [], OrigImpTypeDefns),
        list.foldl(maybe_record_foreign_enum, IntPragmas,
            map.init, ForeignEnumTypeCtors0),
        list.foldl(record_foreign_enum_spec, ImpForeignEnums,
            ForeignEnumTypeCtors0, ForeignEnumTypeCtors),
        decide_repns_for_simple_types(ModuleName,
            OrigIntTypeDefns, OrigImpTypeDefns, ForeignEnumTypeCtors,
            ShortIntTypeRepns, _ShortImpTypeRepns)
    ),

    (
        UnqualSymNames = no_unqual_symnames,
        % UsedModuleNames may contain references to implicitly imported
        % builtin modules, which we do not want to *explicitly* import.
        % Intersecting it with IntImportsUses deletes these.
        set.intersect(UsedModuleNames, IntImportsUses, ShortIntUsedModuleNames)
    ;
        UnqualSymNames = some_unqual_symnames,
        % Since some item did not get fully qualified, the module has an error.
        % If we deleted any element of IntImportsUses, a compiler invocation
        % that read the .int2 file we are generating could print
        % an error message that points the blame at that modification,
        % rather than at the contents of the .m file we were given.
        ShortIntUsedModuleNames = IntImportsUses
    ),

    % If there is nothing involving a foreign language in the interface,
    % then we do not need either explicit or implicit FIMs for that
    % language in the interface.
    set.filter(fim_spec_is_for_needed_language(ShortIntImplicitFIMLangs),
        IntExplicitFIMSpecs, ShortIntExplicitFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ShortIntImplicitFIMLangs,
        ShortIntExplicitFIMSpecs, ShortIntFIMSpecs),

    % The same is true for the implementation section, with two
    % differences. One is that the implementation section may need
    % a language that the interface does not, and there is an
    % explicit FIM for this language that we did not include
    % in the interface, we must include it in the implementation.
    % Second, we don't want to include a FIM in *both* the interface
    % and the implementation.
    set.union(IntExplicitFIMSpecs, ImpExplicitFIMSpecs, ExplicitFIMSpecs),
    set.filter(fim_spec_is_for_needed_language(ShortImpImplicitFIMLangs),
        ExplicitFIMSpecs, ShortImpExplicitFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ShortImpImplicitFIMLangs,
        ShortImpExplicitFIMSpecs, ShortImpFIMSpecs0),
    set.difference(ShortImpFIMSpecs0, ShortIntFIMSpecs, ShortImpFIMSpecs),

    DummyMaybeVersionNumbers = no_version_numbers,

    % For now, we need the implementation sections of .int2 files to contain
    % all the information that other modules reading that .int file will need
    % to correctly decide the representation of the types exported by this
    % module.
    %
    % The computation we use to decide which types' type_defn items
    % need to stay in the implementation section of the .int file,
    % and in what form, computes exactly this information. Therefore
    % we need only the copy the type_defn items that this previous
    % computation has given us.
    %
    % XXX TYPE_REPN In the future, these type_defn items (which include
    % some for types that *shouldn't* be exported from the module)
    % should be replaced by type_repn items (for only the types which
    % *are* exported from the module).
    %
    % The implementation section of .int2 files needs no other items,
    % and when we switch to using type_repn items to decide type
    % representations, the implementation sections of .int2 files
    % should be empty (as are the implementation sections of .int3 files).
    %
    ShortImpTypeDefns = ImpTypeDefns,
    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        DummyMaybeVersionNumbers,
        IntInclModuleNames, ShortIntUsedModuleNames,
        ShortIntFIMSpecs, ShortImpFIMSpecs,
        ShortIntTypeDefns, ShortIntInstDefns, ShortIntModeDefns,
        ShortIntTypeClasses, ShortIntInstances, ShortIntTypeRepns,
        ShortImpTypeDefns).

%---------------------%

:- pred gather_type_defn_items(list(item_type_defn_info)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

gather_type_defn_items(TypeDefns, !TypeDefns) :-
    !:TypeDefns = TypeDefns ++ !.TypeDefns.

%---------------------%

:- pred fim_spec_is_for_needed_language(set(foreign_language)::in,
    fim_spec::in) is semidet.

fim_spec_is_for_needed_language(NeededLangs, FIMSpec) :-
    FIMSpec = fim_spec(Lang, _ModuleName),
    set.contains(NeededLangs, Lang).

%---------------------%

:- pred get_int2_items_from_int1_int_type_defn(list(item_type_defn_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

get_int2_items_from_int1_int_type_defn([],
        !MaybeUnqual, !ModuleNames, !IntTypeDefnsCord, !IntImplicitFIMLangs).
get_int2_items_from_int1_int_type_defn([TypeDefnInfo | TypeDefnInfos],
        !MaybeUnqual, !ModuleNames, !IntTypeDefnsCord, !IntImplicitFIMLangs) :-
    % generate_pre_grab_pre_qual_interface_for_int1_int2 had invoked
    % make_canon_make_du_and_solver_types_abstract on type_defn items
    % in the implementation section of the module. We now do the same
    % for type_defn items in the interface section.
    make_canon_make_du_and_solver_types_abstract(TypeDefnInfo,
        MaybeAbstractTypeDefnInfo),
    MaybeAbstractTypeDefnInfo = item_type_defn_info(_TypeSymName,
        _TypeParams, TypeDefn, _TVarSet, _Context, _SeqNum),
    (
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        )
        % We just made du and solver types abstract, and abstract types
        % were abstract already, so they cannot refer to other modules.
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        % Foreign types can never refer to Mercury code in other
        % modules (though they can refer to target language code
        % in other modules).
        DetailsForeign = type_details_foreign(ForeignType, _, _),
        Lang = foreign_type_language(ForeignType),
        set.insert(Lang, !IntImplicitFIMLangs)
    ;
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(EqvType),
        accumulate_modules_in_type(EqvType, !MaybeUnqual, !ModuleNames)
    ),
    cord.snoc(MaybeAbstractTypeDefnInfo, !IntTypeDefnsCord),
    get_int2_items_from_int1_int_type_defn(TypeDefnInfos,
        !MaybeUnqual, !ModuleNames, !IntTypeDefnsCord, !IntImplicitFIMLangs).

:- pred get_int2_items_from_int1_int_inst_defn(list(item_inst_defn_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

get_int2_items_from_int1_int_inst_defn([],
        !MaybeUnqual, !ModuleNames).
get_int2_items_from_int1_int_inst_defn([InstDefnInfo | InstDefnInfos],
        !MaybeUnqual, !ModuleNames) :-
    InstDefnInfo = item_inst_defn_info(_SymName, _InstArgVars,
        MaybeForTypeCtor, MaybeAbstractInstDefn, _InstVarSet,
        _Context, _SeqNum),
    (
        MaybeForTypeCtor = no
    ;
        MaybeForTypeCtor = yes(TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, _TypectorArity),
        accumulate_module(TypeCtorSymName, !MaybeUnqual, !ModuleNames)
    ),
    (
        MaybeAbstractInstDefn = abstract_inst_defn
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
        InstDefn = eqv_inst(Inst),
        accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames)
    ),
    get_int2_items_from_int1_int_inst_defn(InstDefnInfos,
        !MaybeUnqual, !ModuleNames).

:- pred get_int2_items_from_int1_int_mode_defn(list(item_mode_defn_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

get_int2_items_from_int1_int_mode_defn([],
        !MaybeUnqual, !ModuleNames).
get_int2_items_from_int1_int_mode_defn([ModeDefnInfo | ModeDefnInfos],
        !MaybeUnqual, !ModuleNames) :-
    ModeDefnInfo = item_mode_defn_info(_SymName, _InstArgVars,
        MaybeAbstractModeDefn, _InstVarSet, _Context, _SeqNum),
    (
        MaybeAbstractModeDefn = abstract_mode_defn
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
        ModeDefn = eqv_mode(Mode),
        accumulate_modules_in_mode(Mode, !MaybeUnqual, !ModuleNames)
    ),
    get_int2_items_from_int1_int_mode_defn(ModeDefnInfos,
        !MaybeUnqual, !ModuleNames).

:- pred get_int2_items_from_int1_int_typeclass(list(item_typeclass_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out) is det.

get_int2_items_from_int1_int_typeclass([],
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord).
get_int2_items_from_int1_int_typeclass([TypeClassInfo | TypeClassInfos],
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord) :-
    TypeClassInfo = item_typeclass_info(ClassSymName, TypeParams,
        SuperclassConstraints, FunDeps, _Methods0, TVarSet, Context, SeqNum),
    accumulate_modules_in_constraints(SuperclassConstraints,
        !MaybeUnqual, !ModuleNames),
    Methods = class_interface_abstract,
    AbstractTypeClassInfo = item_typeclass_info(ClassSymName, TypeParams,
        SuperclassConstraints, FunDeps, Methods, TVarSet, Context, SeqNum),
    cord.snoc(AbstractTypeClassInfo, !IntTypeClassesCord),
    get_int2_items_from_int1_int_typeclass(TypeClassInfos,
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord).

:- pred get_int2_items_from_int1_int_instance(list(item_instance_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_instance_info)::in, cord(item_instance_info)::out) is det.

get_int2_items_from_int1_int_instance([],
        !MaybeUnqual, !ModuleNames, !IntInstancesCord).
get_int2_items_from_int1_int_instance([InstanceInfo | InstanceInfos],
        !MaybeUnqual, !ModuleNames, !IntInstancesCord) :-
    InstanceInfo = item_instance_info(ClassSymName,
        ArgTypes, OrigArgTypes, ClassConstraints, _InstanceBody0,
        TVarSet, ContainingModuleName, Context, SeqNum),
    accumulate_module(ClassSymName, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(OrigArgTypes, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_constraints(ClassConstraints,
        !MaybeUnqual, !ModuleNames),
    InstanceBody = instance_body_abstract,
    AbstractInstanceInfo = item_instance_info(ClassSymName,
        ArgTypes, OrigArgTypes, ClassConstraints, InstanceBody,
        TVarSet, ContainingModuleName, Context, SeqNum),
    cord.snoc(AbstractInstanceInfo, !IntInstancesCord),
    get_int2_items_from_int1_int_instance(InstanceInfos,
        !MaybeUnqual, !ModuleNames, !IntInstancesCord).

%---------------------%

:- pred get_int2_items_from_int1_imp_types(list(item_type_defn_info)::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

get_int2_items_from_int1_imp_types([], !ImpImplicitFIMLangs).
get_int2_items_from_int1_imp_types([ImpTypeDefn | ImpTypeDefns],
        !ImpImplicitFIMLangs) :-
    TypeDefn = ImpTypeDefn ^ td_ctor_defn,
    ( if TypeDefn = parse_tree_foreign_type(DetailsForeign) then
        DetailsForeign = type_details_foreign(ForeignType, _, _),
        Lang = foreign_type_language(ForeignType),
        set.insert(Lang, !ImpImplicitFIMLangs)
    else
        true
    ),
    get_int2_items_from_int1_imp_types(ImpTypeDefns, !ImpImplicitFIMLangs).

%---------------------------------------------------------------------------%

    % XXX TYPE_REPN Consider the relationship between this predicate and
    % make_impl_type_abstract in write_module_interface_files.m. Unlike this
    % predicate, that one has access to the definitions of the types
    % in this module, so it knows whether e.g. an equivalence type definition
    % makes the defined type equivalent to a type that needs special treatment
    % by the algorithm that decides data representations.
    %
:- pred make_canon_make_du_and_solver_types_abstract(
    item_type_defn_info::in, item_type_defn_info::out) is det.

make_canon_make_du_and_solver_types_abstract(ItemTypeDefn,
        MaybeAbstractItemTypeDefn) :-
    TypeDefn = ItemTypeDefn ^ td_ctor_defn,
    (
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(_Ctors, MaybeCanonical,
            _MaybeDirectArgCtors),
        % For the `.int2' files, we need the full definitions of
        % discriminated union types. Even if the functors for a type
        % are not used within a module, we may need to know them for
        % comparing insts, e.g. for comparing `ground' and `bound(...)'.
        % XXX ITEM_LIST: zs: That may be so, but writing out the type
        % definition unchanged, without something on it that says
        % "use these functors *only* for these purposes",
        % is a bug in my opinion.
        % XXX ITEM_LIST: And most types do NOT have any insts defined for them.
        % We could collect (a) the set of type constructors mentioned
        % explicitly in insts as being for that type, and (b) the set of
        % function symbol/arity pairs that occur in bound insts, and then
        % make the type definition totally abstract unless the type constructor
        % either is in set (a) or a member of Ctors is in set (b).
        (
            MaybeCanonical = canon,
            MaybeAbstractItemTypeDefn = ItemTypeDefn
        ;
            MaybeCanonical = noncanon(_NonCanonical),
            AbstractDetailsDu = DetailsDu ^ du_canonical :=
                noncanon(noncanon_abstract(non_solver_type)),
            AbstractTypeDefn = parse_tree_du_type(AbstractDetailsDu),
            MaybeAbstractItemTypeDefn = ItemTypeDefn ^ td_ctor_defn :=
                AbstractTypeDefn
        )
    ;
        TypeDefn = parse_tree_abstract_type(_AbstractDetails),
        MaybeAbstractItemTypeDefn = ItemTypeDefn
    ;
        TypeDefn = parse_tree_solver_type(_),
        % rafe: XXX we need to also export the details of the
        % forwarding type for the representation and the forwarding
        % pred for initialization.
        AbstractDetails = abstract_solver_type,
        MaybeAbstractItemTypeDefn = ItemTypeDefn ^ td_ctor_defn :=
            parse_tree_abstract_type(AbstractDetails)
    ;
        TypeDefn = parse_tree_eqv_type(_),
        % For the `.int2' files, we need the full definitions of
        % equivalence types. They are needed to ensure that
        % non-abstract equivalence types always get fully expanded
        % before code generation, even in modules that only indirectly
        % import the definition of the equivalence type.
        % XXX TYPE_REPN: *After* we have generated a type_repn item
        % including this information, we should be able to make
        % MaybeAbstractItemTypeDefn actually abstract.
        MaybeAbstractItemTypeDefn = ItemTypeDefn
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(_ForeignType, MaybeCanonical,
            _Assertions),
        % We always need the definitions of foreign types
        % to handle inter-language interfacing correctly.
        % However, we want to abstract away any unify and compare predicates.
        (
            MaybeCanonical = canon,
            MaybeAbstractItemTypeDefn = ItemTypeDefn
        ;
            MaybeCanonical = noncanon(_NonCanonical),
            AbstractDetailsForeign = DetailsForeign ^ foreign_canonical :=
                noncanon(noncanon_abstract(non_solver_type)),
            AbstractTypeDefn = parse_tree_foreign_type(AbstractDetailsForeign),
            MaybeAbstractItemTypeDefn = ItemTypeDefn ^ td_ctor_defn :=
                AbstractTypeDefn
        )
    ).

%---------------------------------------------------------------------------%

:- type maybe_unqual_symnames
    --->    no_unqual_symnames
    ;       some_unqual_symnames.

:- pred accumulate_module(sym_name::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_module(SymName, !MaybeUnqual, !ModuleNames) :-
    (
        SymName = unqualified(_),
        !:MaybeUnqual = some_unqual_symnames
    ;
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !ModuleNames)
    ).

%---------------------%

:- pred accumulate_modules_in_constraint(prog_constraint::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_constraint(Constraint, !MaybeUnqual, !ModuleNames) :-
    Constraint = constraint(ClassSymName, ArgTypes),
    accumulate_module(ClassSymName, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames).

%---------------------%

:- pred accumulate_modules_in_type(mer_type::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames) :-
    (
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames)
    ;
        ( Type = tuple_type(ArgTypes, _Kind)
        ; Type = apply_n_type(_TVar, ArgTypes, _Kind)
        ; Type = higher_order_type(_PredOrFunc, ArgTypes,
            _HOInstInfo, _Purity, _EvalMethod)
        ),
        accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames)
    ;
        Type = kinded_type(ArgType, _Kind),
        accumulate_modules_in_type(ArgType, !MaybeUnqual, !ModuleNames)
    ).

%---------------------%

:- pred accumulate_modules_in_inst(mer_inst::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames) :-
    (
        ( Inst = free
        ; Inst = not_reached
        ; Inst = ground(_Uniq, _HOInstInfo)
        ; Inst = inst_var(_InstVar)
        ; Inst = any(_Uniq, _HOInstInfo)
        )
    ;
        Inst = free(Type),
        accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames)
    ;
        Inst = bound(_Uniq, _InstTestsResults, BoundInsts),
        accumulate_modules_in_bound_insts(BoundInsts,
            !MaybeUnqual, !ModuleNames)
    ;
        Inst = constrained_inst_vars(_InstVars, ArgInst),
        accumulate_modules_in_inst(ArgInst, !MaybeUnqual, !ModuleNames)
    ;
        Inst = defined_inst(InstName),
        accumulate_modules_in_inst_name(InstName, !MaybeUnqual, !ModuleNames)
    ;
        Inst = abstract_inst(SymName, ArgInsts),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames)
    ).

:- pred accumulate_modules_in_inst_name(inst_name::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_inst_name(InstName, !MaybeUnqual, !ModuleNames) :-
    (
        InstName = user_inst(SymName, ArgInsts),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames)
    ;
        ( InstName = unify_inst(_IsLive, _IsReal, ArgInstA, ArgInstB)
        ; InstName = merge_inst(ArgInstA, ArgInstB)
        ),
        accumulate_modules_in_insts([ArgInstA, ArgInstB],
            !MaybeUnqual, !ModuleNames)
    ;
        ( InstName = ground_inst(ArgInstName, _Uniq, _IsLive, _IsReal)
        ; InstName = any_inst(ArgInstName, _Uniq, _IsLive, _IsReal)
        ; InstName = shared_inst(ArgInstName)
        ; InstName = mostly_uniq_inst(ArgInstName)
        ),
        accumulate_modules_in_inst_name(ArgInstName,
            !MaybeUnqual, !ModuleNames)
    ;
        InstName = typed_ground(_Uniq, Type),
        accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames)
    ;
        InstName = typed_inst(Type, ArgInstName),
        accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_inst_name(ArgInstName,
            !MaybeUnqual, !ModuleNames)
    ).

:- pred accumulate_modules_in_bound_inst(bound_inst::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_bound_inst(BoundInst, !MaybeUnqual, !ModuleNames) :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    ( if ConsId = cons(SymName, _ConsArity, TypeCtor) then
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        TypeCtor = type_ctor(TypeCtorSymName, _Arity),
        accumulate_module(TypeCtorSymName, !MaybeUnqual, !ModuleNames)
    else
        true
    ),
    accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames).

%---------------------%

:- pred accumulate_modules_in_mode(mer_mode::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_mode(Mode, !MaybeUnqual, !ModuleNames) :-
    (
        Mode = from_to_mode(InstA, InstB),
        accumulate_modules_in_inst(InstA, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_inst(InstB, !MaybeUnqual, !ModuleNames)
    ;
        Mode = user_defined_mode(SymName, ArgInsts),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames)
    ).

%---------------------%

:- pred accumulate_modules_in_constraints(list(prog_constraint)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_constraints([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_constraints([Constraint | Constraints],
        !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_constraint(Constraint, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_constraints(Constraints, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_types(list(mer_type)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_types([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_types([Type | Types], !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(Types, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_bound_insts(list(bound_inst)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_bound_insts([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_bound_insts([BoundInst | BoundInsts],
        !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_bound_inst(BoundInst, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_bound_insts(BoundInsts, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_insts(list(mer_inst)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_insts([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_insts([Inst | Insts], !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_insts(Insts, !MaybeUnqual, !ModuleNames).

%---------------------------------------------------------------------------%
% The rest of this module should not be needed.
%---------------------------------------------------------------------------%

make_foreign_import(ModuleName, Lang) = FIM :-
    FIM = item_fim(Lang, ModuleName, term.context_init, -1).

%---------------------------------------------------------------------------%
:- end_module parse_tree.comp_unit_interface.
%---------------------------------------------------------------------------%
