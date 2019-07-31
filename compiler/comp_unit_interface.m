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
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.
    % XXX document me better
    %
    % XXX Why do we report errors NOW, as opposed to when we generate code?
    %
:- pred generate_short_interface_int3(globals::in, raw_compilation_unit::in,
    parse_tree_int::out, list(error_spec)::out) is det.

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
    % - Type definitions, in a possibly changed form. Specifically,
    %   we replace the definitions (a) solver types and (b) noncanonical
    %   du and foreign types with their abstract forms. We leave the
    %   definitions of all other types (canonical du and foreign types,
    %   equivalence types, and already abtract types) unchanged.
    %
    % - Typeclass declarations in their abstract from.
    %
    % - Foreign_import_module declarations.
    %
    % - Foreign_enum pragmas.
    %
    % If any item in the final raw compilation unit needs a
    % foreign_import_module declaration for the current module
    % for the any language, we add such an item.
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
    parse_tree_int::out, parse_tree_int::out, list(error_spec)::out) is det.

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
:- func make_foreign_import(module_name, foreign_language) = item.

%---------------------------------------------------------------------------%

    % This predicate is exported for use by module_imports.m.
    %
    % XXX ITEM_LIST They shouldn't be needed; the representation of the
    % compilation unit should have all this information separate from
    % the items.
    %
:- pred get_foreign_self_imports_from_item_blocks(list(item_block(MS))::in,
    list(foreign_language)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.check_raw_comp_unit.
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
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_short_interface_int3(Globals, RawCompUnit, ParseTreeInt, !:Specs) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, RawItemBlocks),
    !:Specs = [],
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        cord.init, IntInclsCord, cord.init, IntAvailsCord0,
        cord.init, IntItemsCord0,
        cord.init, IntTypeDefnsCord, cord.init, ImpTypeDefnsCord,
        map.init, ForeignEnumTypeCtors, do_not_need_avails, NeedAvails,
        !Specs),
    IntIncls = cord.list(IntInclsCord),
    (
        NeedAvails = do_not_need_avails,
        IntAvails = []
    ;
        NeedAvails = do_need_avails,
        IntAvails = cord.list(IntAvailsCord0)
    ),
    IntItems0 = cord.list(IntItemsCord0),
    globals.lookup_string_option(Globals, experiment, Experiment),
    ( if Experiment = "type_repn_int3" then
        IntTypeDefns = cord.list(IntTypeDefnsCord),
        ImpTypeDefns = cord.list(ImpTypeDefnsCord),
        decide_repns_for_simple_types(ModuleName, IntTypeDefns, ImpTypeDefns,
            ForeignEnumTypeCtors, IntTypeRepnItems, _NonIntTypeRepnItems),
        IntItems = IntItems0 ++ IntTypeRepnItems
    else
        IntItems = IntItems0
    ),
    MaybeVersionNumbers = no,
    ParseTreeInt0 = parse_tree_int(ModuleName, ifk_int3, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [], IntItems, []),
    module_qualify_parse_tree_int3(Globals, ParseTreeInt0, ParseTreeInt,
        !Specs).

:- type need_avails
    --->    do_not_need_avails
    ;       do_need_avails.

:- pred get_short_interface_int3_from_item_blocks(list(raw_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out,
    need_avails::in, need_avails::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_from_item_blocks([],
        !IntIncls, !IntAvails, !IntItems, !IntTypeDefns, !ImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs).
get_short_interface_int3_from_item_blocks([RawItemBlock | RawItemBlocks],
        !IntIncls, !IntAvails, !IntItems, !IntTypeDefns, !ImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs) :-
    RawItemBlock = item_block(_, Section, Incls, Avails, Items),
    (
        Section = ms_interface,
        !:IntIncls = !.IntIncls ++ cord.from_list(Incls),
        !:IntAvails = !.IntAvails ++
            cord.from_list(list.filter(avail_is_import, Avails)),
        get_short_interface_int3_from_items(Items, !IntItems, !IntTypeDefns,
            !ForeignEnumTypeCtors, !NeedAvails, !Specs)
    ;
        Section = ms_implementation,
        gather_imp_type_defns(Items, !ImpTypeDefns, !ForeignEnumTypeCtors)
    ),
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        !IntIncls, !IntAvails, !IntItems, !IntTypeDefns, !ImpTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs).

:- pred get_short_interface_int3_from_items(list(item)::in,
    cord(item)::in, cord(item)::out,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out,
    need_avails::in, need_avails::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_from_items([], !IntItems, !IntTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs).
get_short_interface_int3_from_items([Item | Items], !IntItems, !IntTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        cord.snoc(ItemTypeDefnInfo, !IntTypeDefns),
        % XXX TYPE_REPN do this in decide_type_repn.m?
        make_type_defn_abstract_type_for_int3(ItemTypeDefnInfo,
            AbstractOrForeignItemTypeDefnInfo),
        AbstractOrForeignItem =
            item_type_defn(AbstractOrForeignItemTypeDefnInfo),
        cord.snoc(AbstractOrForeignItem, !IntItems)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        ItemTypeClassInfo = item_typeclass_info(ClassName, ParamsTVars,
            _Constraints, _FunDeps, _Methods, TVarSet, Context, SeqNum),
        AbstractItemTypeClassInfo = item_typeclass_info(ClassName, ParamsTVars,
            [], [], class_interface_abstract, TVarSet, Context, SeqNum),
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        cord.snoc(AbstractItem, !IntItems)
    ;
        Item = item_instance(ItemInstanceInfo),
        AbstractItemInstanceInfo = ItemInstanceInfo ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstanceInfo),
        cord.snoc(AbstractItem, !IntItems),
        % We may need the imported modules to module qualify the names
        % of the type constructors in the instance's member types.
        !:NeedAvails = do_need_avails
    ;
        Item = item_inst_defn(ItemInstInfo),
        AbstractItemInstInfo =
            ItemInstInfo ^ id_inst_defn := abstract_inst_defn,
        AbstractItem = item_inst_defn(AbstractItemInstInfo),
        cord.snoc(AbstractItem, !IntItems)
    ;
        Item = item_mode_defn(ItemModeInfo),
        AbstractItemModeInfo =
            ItemModeInfo ^ md_mode_defn := abstract_mode_defn,
        AbstractItem = item_mode_defn(AbstractItemModeInfo),
        cord.snoc(AbstractItem, !IntItems)
    ;
        Item = item_clause(ItemClause),
        Context = ItemClause ^ cl_context,
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs]
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, Context, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs]
        ;
            AllowedInInterface = yes
        ),
        maybe_record_foreign_enum(Pragma, !ForeignEnumTypeCtors)
    ;
        ( Item = item_foreign_import_module(_)
        ; Item = item_mutable(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        )
    ),
    get_short_interface_int3_from_items(Items, !IntItems, !IntTypeDefns,
        !ForeignEnumTypeCtors, !NeedAvails, !Specs).

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

:- pred gather_imp_type_defns(list(item)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    foreign_enum_map::in, foreign_enum_map::out) is det.

gather_imp_type_defns([], !ImpTypeDefns, !ForeignEnumTypeCtors).
gather_imp_type_defns([Item | Items], !ImpTypeDefns, !ForeignEnumTypeCtors) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        cord.snoc(ItemTypeDefnInfo, !ImpTypeDefns)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        maybe_record_foreign_enum(Pragma, !ForeignEnumTypeCtors)
    ;
        ( Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_import_module(_)
        ; Item = item_mutable(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        )
    ),
    gather_imp_type_defns(Items, !ImpTypeDefns, !ForeignEnumTypeCtors).

:- pred maybe_record_foreign_enum(pragma_type::in,
    foreign_enum_map::in, foreign_enum_map::out) is det.

maybe_record_foreign_enum(Pragma, !ForeignEnumTypeCtors) :-
    ( if Pragma = pragma_foreign_enum(PragmaInfoForeignEnum) then
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
        )
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_private_interface_int0(AugCompUnit, ParseTreeInt) :-
    AugCompUnit = aug_compilation_unit(ModuleName,
        ModuleNameContext, ModuleVersionNumbers, SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),
    get_private_interface_int0_from_item_blocks(ModuleName, SrcItemBlocks,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord,
        cord.init, IntItemsCord, cord.init, ImpItemsCord),
    ( if map.search(ModuleVersionNumbers, ModuleName, VersionNumbers) then
        MaybeVersionNumbers = yes(VersionNumbers)
    else
        MaybeVersionNumbers = no
    ),
    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails = cord.list(ImpAvailsCord),
    IntItems = cord.list(IntItemsCord),
    ImpItems = cord.list(ImpItemsCord),
    ParseTreeInt = parse_tree_int(ModuleName, ifk_int0,
        ModuleNameContext, MaybeVersionNumbers, IntIncls, ImpIncls,
        IntAvails, ImpAvails, IntItems, ImpItems).

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
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    cord(item)::in, cord(item)::out) is det.

get_private_interface_int0_from_item_blocks(_ModuleName, [],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).
get_private_interface_int0_from_item_blocks(ModuleName,
        [ItemBlock | ItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord) :-
    ItemBlock = item_block(_, SrcSection, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        get_private_interface_int0_from_items(ModuleName, Items,
            !IntItemsCord)
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        % XXX ITEM_LIST Our parent calls grab_unqual_imported_modules,
        % which has traditionally NOT changed sms_implementation
        % to sms_impl_but_exported_to_submodules even in the presence
        % of submodules, but future factorizations of common code
        % may change that.
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        get_private_interface_int0_from_items(ModuleName, Items,
            !ImpItemsCord)
    ),
    get_private_interface_int0_from_item_blocks(ModuleName, ItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).

:- pred get_private_interface_int0_from_items(module_name::in,
    list(item)::in, cord(item)::in, cord(item)::out) is det.

get_private_interface_int0_from_items(_ModuleName, [],
        !SectionItemsCord).
get_private_interface_int0_from_items(ModuleName, [Item | Items],
        !SectionItemsCord) :-
    get_private_interface_int0_from_item(ModuleName, Item,
        !SectionItemsCord),
    get_private_interface_int0_from_items(ModuleName, Items,
        !SectionItemsCord).

:- pred get_private_interface_int0_from_item(module_name::in,
    item::in, cord(item)::in, cord(item)::out) is det.

get_private_interface_int0_from_item(ModuleName, Item, !SectionItemsCord) :-
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
            cord.snoc(Item, !SectionItemsCord)
        )
    ;
        % XXX ITEM_LIST The action here follows what this predicate used
        % to do before the item list change.
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_foreign_import_module(_)
        ),
        cord.snoc(Item, !SectionItemsCord)
    ;
        Item = item_instance(InstanceInfo),
        AbstractInstanceInfo =
            InstanceInfo ^ ci_method_instances := instance_body_abstract,
        AbstractItem = item_instance(AbstractInstanceInfo),
        cord.snoc(AbstractItem, !SectionItemsCord)
    ;
        Item = item_mutable(ItemMutable),
        ItemMutable = item_mutable_info(MutableName,
            _OrigType, Type, _OrigInst, Inst, _Value, _Varset, Attrs,
            Context, _SeqNum),
        ConstantInterface = mutable_var_constant(Attrs),
        (
            ConstantInterface = mutable_constant,
            ConstantGetPredDecl = constant_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantSetPredDecl = constant_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantGetPredDeclItem = item_pred_decl(ConstantGetPredDecl),
            ConstantSetPredDeclItem = item_pred_decl(ConstantSetPredDecl),
            cord.snoc(ConstantGetPredDeclItem, !SectionItemsCord),
            cord.snoc(ConstantSetPredDeclItem, !SectionItemsCord)
        ;
            ConstantInterface = mutable_not_constant,
            StdGetPredDecl = std_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdSetPredDecl = std_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdGetPredDeclItem = item_pred_decl(StdGetPredDecl),
            StdSetPredDeclItem = item_pred_decl(StdSetPredDecl),
            cord.snoc(StdGetPredDeclItem, !SectionItemsCord),
            cord.snoc(StdSetPredDeclItem, !SectionItemsCord),

            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = mutable_attach_to_io_state,
                IOGetPredDecl = io_get_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOSetPredDecl = io_set_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOGetPredDeclItem = item_pred_decl(IOGetPredDecl),
                IOSetPredDeclItem = item_pred_decl(IOSetPredDecl),
                cord.snoc(IOGetPredDeclItem, !SectionItemsCord),
                cord.snoc(IOSetPredDeclItem, !SectionItemsCord)
            ;
                IOStateInterface = mutable_dont_attach_to_io_state
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_pre_grab_pre_qual_interface_for_int1_int2(RawCompUnit,
        InterfaceRawCompUnit) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    generate_pre_grab_pre_qual_item_blocks(RawItemBlocks,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord,
        cord.init, IntItemsCord, cord.init, ImpItemsCord),
    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails = cord.list(ImpAvailsCord),
    IntItems = cord.list(IntItemsCord),
    ImpItems = cord.list(ImpItemsCord),
    int_imp_items_to_item_blocks(ModuleName, ms_interface, ms_implementation,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems,
        InterfaceItemBlocks),
    InterfaceRawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        InterfaceItemBlocks).

%---------------------------------------------------------------------------%

:- pred generate_pre_grab_pre_qual_item_blocks(list(raw_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_item_blocks([],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).
generate_pre_grab_pre_qual_item_blocks([RawItemBlock | RawItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord) :-
    RawItemBlock = item_block(_, Section, Incls, Avails, Items),
    (
        Section = ms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        generate_pre_grab_pre_qual_items_int(Items, !IntItemsCord)
    ;
        Section = ms_implementation,
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        generate_pre_grab_pre_qual_items_imp(Items, !ImpItemsCord)
    ),
    generate_pre_grab_pre_qual_item_blocks(RawItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord).

:- pred generate_pre_grab_pre_qual_items_int(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_items_int([], !ItemsCord).
generate_pre_grab_pre_qual_items_int([Item | Items], !ItemsCord) :-
    ( if Item = item_instance(ItemInstance) then
        AbstractItemInstance = ItemInstance ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstance),
        cord.snoc(AbstractItem, !ItemsCord)
    else
        cord.snoc(Item, !ItemsCord)
    ),
    generate_pre_grab_pre_qual_items_int(Items, !ItemsCord).

:- pred generate_pre_grab_pre_qual_items_imp(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_items_imp([], !ItemsCord).
generate_pre_grab_pre_qual_items_imp([Item | Items], !ItemsCord) :-
    % `:- typeclass' declarations may be referred to by the constructors
    % in type declarations. Since these constructors are abstractly
    % exported, we won't need the local instance declarations.
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        maybe_make_abstract_type_defn_for_int2(ItemTypeDefnInfo,
            MaybeAbstractItemTypeDefnInfo),
        AbstractItem = item_type_defn(MaybeAbstractItemTypeDefnInfo),
        cord.snoc(AbstractItem, !ItemsCord)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        cord.snoc(AbstractItem, !ItemsCord)
    ;
        Item = item_foreign_import_module(_),
        cord.snoc(Item, !ItemsCord)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        ( if Pragma = pragma_foreign_enum(_) then
            cord.snoc(Item, !ItemsCord)
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
    generate_pre_grab_pre_qual_items_imp(Items, !ItemsCord).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_interfaces_int1_int2(Globals, AugCompUnit,
        ParseTreeInt1, ParseTreeInt2, !:Specs) :-
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
    !:Specs = [],
    map.init(IntFIMsMap0),
    map.init(ImpFIMsMap0),
    set.init(IntSelfFIMs0),
    set.init(ImpSelfFIMs0),
    multi_map.init(IntTypesMap0),
    multi_map.init(ImpTypesMap0),
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord0,
        cord.init, IntItemsCord0, cord.init, ImpItemsCord0,
        cord.init, ImpForeignEnumsCord,
        IntFIMsMap0, IntFIMsMap1, ImpFIMsMap0, ImpFIMsMap1,
        IntSelfFIMs0, IntSelfFIMs, ImpSelfFIMs0, ImpSelfFIMs1,
        IntTypesMap0, IntTypesMap, ImpTypesMap0, ImpTypesMap,
        set.init, ModulesNeededByTypeClassDefns, !Specs),

    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails0 = cord.list(ImpAvailsCord0),
    IntItems0 = cord.list(IntItemsCord0),
    ImpItems0 = cord.list(ImpItemsCord0),
    ImpForeignEnums = cord.list(ImpForeignEnumsCord),

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
        BothTypesMap, NeededImpTypeCtors, ModulesNeededByTypeDefns),
    set.union(ModulesNeededByTypeClassDefns, ModulesNeededByTypeDefns,
        NeededModules),

    % Compute the list of type definitions we deleted from ImpItems0
    % that we want to add back to the implementation section,
    % possibly in their abstract form.
    map.foldl2(
        maybe_add_maybe_abstract_type_defn_items(BothTypesMap, IntTypesMap,
            NeededImpTypeCtors),
        ImpTypesMap, [], ImpTypeDefnItems, ImpSelfFIMs1, ImpSelfFIMs2),

    % Figure out which of the foreign enum items we deleted from ImpItems0
    % we want to add back to the implementation section.
    % Record the needs of these foreign enum items for
    % foreign_import_module items.
    list.foldl2(add_foreign_enum_item_if_needed(IntTypesMap), ImpForeignEnums,
        [], ImpForeignEnumItems, ImpSelfFIMs2, ImpSelfFIMs),

    ImpItems1 = ImpTypeDefnItems ++ ImpForeignEnumItems ++ ImpItems0,

    add_self_fims(ModuleName, IntSelfFIMs, IntFIMsMap1, IntFIMsMap),
    add_self_fims(ModuleName, ImpSelfFIMs, ImpFIMsMap1, ImpFIMsMap2),
    map.map_values(subtract_int_fims_map(IntFIMsMap), ImpFIMsMap2, ImpFIMsMap),

    map.foldl(acccumulate_foreign_import_items, IntFIMsMap, [], IntFIMItems),
    map.foldl(acccumulate_foreign_import_items, ImpFIMsMap, [], ImpFIMItems),

    IntItems = IntFIMItems ++ IntItems0,
    ImpItems = ImpFIMItems ++ ImpItems1,

    % Find out whether anything we are about to put into the implementation
    % section needs any ":- import_module"s or ":- use_module"s at all.
    % XXX ITEM_LIST Given that we have already computed NeededModules,
    % why do we need to compute this cruder measure as well?
    find_need_imports(ImpItems,
        NeedImports, set.init, _NeedForeignImportLangs),
    (
        NeedImports = need_imports,
        strip_unneeded_imp_avails(NeededModules, ImpAvails0, ImpAvails)
    ;
        NeedImports = dont_need_imports,
        ImpAvails = []
    ),

    ToCheckIntItemBlock = item_block(ModuleName, ms_interface,
        IntIncls, IntAvails, IntItems),
    check_interface_item_blocks_for_no_exports(Globals,
        ModuleName, ModuleNameContext, [ToCheckIntItemBlock], !Specs),

    % XXX ITEM_LIST See the comment about version numbers in our caller.
    DummyMaybeVersionNumbers = no,
    ParseTreeInt1 = parse_tree_int(ModuleName, ifk_int,
        ModuleNameContext, DummyMaybeVersionNumbers,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems),

    %------%

    % The rest of this predicate body constructs the .int2 file.

    % We start from the versions of the .int item lists from *before*
    % we added any foreign_import_module items to them, since for the
    % .int2 file, we want to decide whether we want to keep the
    % foreign_import_module in each section on somewhat different
    % criteria than those used for .int files.
    %
    % XXX I (zs) don't really understand the motivation behind the
    % rules here. And I don't understand why these rules treat
    % the interface section the same as the implementation section.
    get_int2_items_from_int1(IntItems0, ShortIntItems0),
    get_int2_items_from_int1(ImpItems1, ShortImpItems0),
    find_need_imports(ShortIntItems0,
        ShortIntNeedImports, set.init, ShortIntNeedForeignImportLangs),
    find_need_imports(ShortImpItems0,
        ShortImpNeedImports, set.init, ShortImpNeedForeignImportLangs),
    (
        ShortIntNeedImports = need_imports,
        ShortIntAvails = IntAvails
    ;
        ShortIntNeedImports = dont_need_imports,
        ShortIntAvails = []
    ),
    (
        ShortImpNeedImports = need_imports,
        ShortImpAvails = ImpAvails
    ;
        ShortImpNeedImports = dont_need_imports,
        ShortImpAvails = []
    ),
    ( if set.is_non_empty(ShortIntNeedForeignImportLangs) then
        ShortIntItems = IntFIMItems ++ ShortIntItems0
    else
        ShortIntItems = ShortIntItems0
    ),
    ( if set.is_non_empty(ShortImpNeedForeignImportLangs) then
        ShortImpItems = ImpFIMItems ++ ShortImpItems0
    else
        ShortImpItems = ShortImpItems0
    ),
    ParseTreeInt2 = parse_tree_int(ModuleName, ifk_int2,
        ModuleNameContext, DummyMaybeVersionNumbers,
        IntIncls, ImpIncls, ShortIntAvails, ShortImpAvails,
        ShortIntItems, ShortImpItems).

%---------------------%

:- pred add_self_fims(module_name::in, set(foreign_language)::in,
    map(module_name, set(foreign_language))::in,
    map(module_name, set(foreign_language))::out) is det.

add_self_fims(ModuleName, SelfFIMs, !FIMsMap) :-
    ( if map.search(!.FIMsMap, ModuleName, Langs0) then
        set.union(SelfFIMs, Langs0, Langs),
        map.det_update(ModuleName, Langs, !FIMsMap)
    else
        map.det_insert(ModuleName, SelfFIMs, !FIMsMap)
    ).

:- pred subtract_int_fims_map(map(module_name, set(foreign_language))::in,
    module_name::in, set(foreign_language)::in, set(foreign_language)::out)
    is det.

subtract_int_fims_map(IntFIMsMap, ModuleName, ImpLangs0, ImpLangs) :-
    ( if map.search(IntFIMsMap, ModuleName, IntLangs) then
        set.difference(ImpLangs0, IntLangs, ImpLangs)
    else
        ImpLangs = ImpLangs0
    ).

%---------------------%

:- pred acccumulate_foreign_import_items(module_name::in,
    set(foreign_language)::in,
    list(item)::in, list(item)::out) is det.

acccumulate_foreign_import_items(ModuleName, Langs, !Items) :-
    set.fold(acccumulate_foreign_import_item(ModuleName), Langs, !Items).

:- pred acccumulate_foreign_import_item(module_name::in, foreign_language::in,
    list(item)::in, list(item)::out) is det.

acccumulate_foreign_import_item(ModuleName, Lang, !Items) :-
    FIMInfo = item_foreign_import_module_info(Lang, ModuleName,
        term.context_init, -1),
    Item = item_foreign_import_module(FIMInfo),
    !:Items = [Item | !.Items].

%---------------------%

:- type type_defn_map == multi_map(type_ctor, item_type_defn_info).
:- type type_defn_pair == pair(type_ctor, item_type_defn_info).

    % get_interface_int1_item_blocks_loop(SrcItemBlocks,
    %   !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
    %   !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
    %   !IntFIMsMap, !ImpFIMsMap, !IntTypesMap, !ImpTypesMap,
    %   !NeededModules, !Specs).
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
    % !:IntInclsCord, !:ImpInclsCord, !:IntAvailsCord, and !:ImpAvailsCord are
    % the includes and avails in the interface and implementation sections
    % respectively.
    %
    % !:IntItems is a filtered version of !.IntItems. Specifically,
    % we delete any promise_type_true promise items, and we both delete,
    % and generate error messages for, any clauses and any pragmas that
    % should not appear in a module's interface, We also filter out
    % any foreign_import_modules and return them separarely in !:IntFIMsCord.
    %
    % !:ImpItems is also a filtered version of !.ImpItems. Our caller
    % ensures that !.ImpItems can have only four kinds of items, which
    % we handle as follows.
    %
    % - We delete type definitions. If one of these type definitions later
    %   turns out to be needed, it can be reconstructed from !:ImpTypesMap.
    % - We keep typeclass declarations.
    % - We filter out any foreign enum pragmas, and return them separately
    %   in !:ImpForeignEnumsCord.
    % - We filter out any foreign_import_modules, and return them separately
    %   in !:ImpFIMsCord.
    %
    % We return foreign enums and foreign_import_modules separately
    % because our caller wants to process them separately.
    %
    % !:IntTypesMap and !:ImpTypesMap each map the type constructors
    % of the types defined in the interface and implementation sections
    % respectively to the list of definitions of that type. (We need a list
    % for two reasons: because a type may be defined both in Mercury and
    % in several target languages, and because of the potential presence
    % of erroneous double definitions.)
    %
    % !:ModulesNeededByTypeClassDefns is the list of the modules needed by
    % any superclass constraints on any typeclass declaration in either
    % the interface or the implementation section. A module may be needed
    % because it defines the typeclass in the superclass constraint,
    % or because it defines a type constructor mentioned anywhere
    % in the superclass constraint.
    %
:- pred get_interface_int1_item_blocks_loop(list(src_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out,
    cord(foreign_enum_reconstructor)::in,
    cord(foreign_enum_reconstructor)::out,
    map(module_name, set(foreign_language))::in,
    map(module_name, set(foreign_language))::out,
    map(module_name, set(foreign_language))::in,
    map(module_name, set(foreign_language))::out,
    set(foreign_language)::in, set(foreign_language)::out,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_item_blocks_loop([],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsMap, !ImpFIMsMap, !IntSelfFIMs, !ImpSelfFIMs,
        !IntTypesMap, !ImpTypesMap, !ModulesNeededByTypeClassDefns, !Specs).
get_interface_int1_item_blocks_loop([SrcItemBlock | SrcItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsMap, !ImpFIMsMap, !IntSelfFIMs, !ImpSelfFIMs,
        !IntTypesMap, !ImpTypesMap, !ModulesNeededByTypeClassDefns, !Specs) :-
    SrcItemBlock = item_block(_, SrcSection, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        get_interface_int1_items_loop_int(Items, !IntItemsCord,
            !IntFIMsMap, !IntSelfFIMs, !IntTypesMap,
            !ModulesNeededByTypeClassDefns, !Specs)
    ;
        SrcSection = sms_implementation,
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        get_interface_int1_items_loop_imp(Items, !ImpItemsCord,
            !ImpForeignEnumsCord, !ImpFIMsMap, !ImpSelfFIMs, !ImpTypesMap,
            !ModulesNeededByTypeClassDefns, !Specs)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        % The code that transforms sms_implementation to this
        % should not have been run.
        unexpected($pred, "sms_impl_but_exported_to_submodules")
    ),
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsMap, !ImpFIMsMap, !IntSelfFIMs, !ImpSelfFIMs,
        !IntTypesMap, !ImpTypesMap, !ModulesNeededByTypeClassDefns, !Specs).

:- pred get_interface_int1_items_loop_int(list(item)::in,
    cord(item)::in, cord(item)::out,
    map(module_name, set(foreign_language))::in,
    map(module_name, set(foreign_language))::out,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_items_loop_int([], !IntItemsCord,
        !IntFIMsMap, !IntSelfFIMs, !IntTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs).
get_interface_int1_items_loop_int([Item | Items], !IntItemsCord,
        !IntFIMsMap, !IntSelfFIMs, !IntTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn =
            item_type_defn_info(Name, TypeParams, TypeDefn, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !IntTypesMap),
        cord.snoc(Item, !IntItemsCord),
        ( if
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _)
        then
            set.insert(foreign_type_language(ForeignType), !IntSelfFIMs)
        else
            true
        )
    ;
        Item = item_typeclass(ItemTypeClass),
        cord.snoc(Item, !IntItemsCord),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !ModulesNeededByTypeClassDefns)
    ;
        Item = item_clause(ItemClause),
        Context = ItemClause ^ cl_context,
        % XXX ITEM_LIST We should delay reporting this bug until
        % we generate target code for this module.
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs]
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _MaybeAttrs, Context, _SeqNum),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            % XXX ITEM_LIST We should delay reporting this bug until
            % we generate target code for this module.
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs]
        ;
            AllowedInInterface = yes,
            cord.snoc(Item, !IntItemsCord),
            Langs = pragma_needs_foreign_imports(Pragma),
            ( if Pragma = pragma_foreign_enum(_) then
                set.insert_list(Langs, !IntSelfFIMs)
            else
                expect(unify(Langs, []), $pred,
                    "interface pragma other than foreign_enum needs Langs ")
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
            cord.snoc(Item, !IntItemsCord)
        )
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_instance(_)
        ),
        cord.snoc(Item, !IntItemsCord)
    ;
        Item = item_foreign_import_module(FIMInfo),
        FIMInfo = item_foreign_import_module_info(Lang, ModuleName,
            _Context, _SeqNum),
        ( if map.search(!.IntFIMsMap, ModuleName, Langs0) then
            set.insert(Lang, Langs0, Langs),
            map.det_update(ModuleName, Langs, !IntFIMsMap)
        else
            Langs = set.make_singleton_set(Lang),
            map.det_insert(ModuleName, Langs, !IntFIMsMap)
        )
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
    get_interface_int1_items_loop_int(Items, !IntItemsCord,
        !IntFIMsMap, !IntSelfFIMs, !IntTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs).

:- pred get_interface_int1_items_loop_imp(list(item)::in,
    cord(item)::in, cord(item)::out,
    cord(foreign_enum_reconstructor)::in,
    cord(foreign_enum_reconstructor)::out,
    map(module_name, set(foreign_language))::in,
    map(module_name, set(foreign_language))::out,
    set(foreign_language)::in, set(foreign_language)::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_items_loop_imp([], !ImpItemsCord,
        !ImpForeignEnumsCord, !ImpFIMsMap, !ImpSelfFIMs, !ImpTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs).
get_interface_int1_items_loop_imp([Item | Items], !ImpItemsCord,
        !ImpForeignEnumsCord, !ImpFIMsMap, !ImpSelfFIMs, !ImpTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, TypeParams, _, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !ImpTypesMap)
        % We don't add this to !ImpItemsCord yet -- we may be removing it.
        % If we *do* want the items for a given type_ctor, we will create
        % new copies of the items from the type_ctor's entry in ImpTypesMap.
    ;
        Item = item_typeclass(ItemTypeClass),
        cord.snoc(Item, !ImpItemsCord),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !ModulesNeededByTypeClassDefns)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, MaybeAttrs, _Context, _SeqNum),
        ( if Pragma = pragma_foreign_enum(FEInfo) then
            Reconstructor = foreign_enum_reconstructor(FEInfo, MaybeAttrs),
            cord.snoc(Reconstructor, !ImpForeignEnumsCord),
            FEInfo = pragma_info_foreign_enum(FELang, _, _),
            set.insert(FELang, !ImpSelfFIMs)
        else
            unexpected($pred, "non-foreign-enum pragma")
        )
    ;
        Item = item_foreign_import_module(FIMInfo),
        FIMInfo = item_foreign_import_module_info(Lang, ModuleName,
            _Context, _SeqNum),
        ( if map.search(!.ImpFIMsMap, ModuleName, Langs0) then
            set.insert(Lang, Langs0, Langs),
            map.det_update(ModuleName, Langs, !ImpFIMsMap)
        else
            Langs = set.make_singleton_set(Lang),
            map.det_insert(ModuleName, Langs, !ImpFIMsMap)
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
    get_interface_int1_items_loop_imp(Items, !ImpItemsCord,
        !ImpForeignEnumsCord, !ImpFIMsMap, !ImpSelfFIMs, !ImpTypesMap,
        !ModulesNeededByTypeClassDefns, !Specs).

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
:- pred maybe_add_maybe_abstract_type_defn_items(
    type_defn_map::in, type_defn_map::in, set(type_ctor)::in,
    type_ctor::in, list(item_type_defn_info)::in,
    list(item)::in, list(item)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

maybe_add_maybe_abstract_type_defn_items(BothTypesMap, IntTypesMap,
        NeededTypeCtors, TypeCtor, ImpItemTypeDefnInfos,
        !ImpTypeDefnItems, !ImpSelfFIMs) :-
    ( if
        set.member(TypeCtor, NeededTypeCtors),
        make_imp_type_abstract(BothTypesMap,
            ImpItemTypeDefnInfos, AbstractImpItemTypeDefnInfos),
        not (
            multi_map.contains(IntTypesMap, TypeCtor),
            list.all_true(is_pure_abstract_type_defn,
                AbstractImpItemTypeDefnInfos)
        )
    then
        add_type_defn_items(AbstractImpItemTypeDefnInfos,
            !ImpTypeDefnItems, !ImpSelfFIMs)
    else
        true
    ).

:- pred is_pure_abstract_type_defn(item_type_defn_info::in) is semidet.

is_pure_abstract_type_defn(ImpItemTypeDefnInfo) :-
    ImpItemTypeDefnInfo ^ td_ctor_defn = parse_tree_abstract_type(Details),
    % XXX ITEM_LIST This test may do the wrong thing for
    % abstract_{dummy,notag,solver}_types, once we start generating them.
    Details \= abstract_type_fits_in_n_bits(_).

:- pred make_imp_type_abstract(type_defn_map::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

make_imp_type_abstract(BothTypesMap, !ImpItemTypeDefnInfos) :-
    % XXX TYPE_REPN We should record the aspects of the type definition
    % that are relevant to type representation (such as "is dummy",
    % "fits in n bits", "is equivalent to ...") in a type_repn item,
    % and then make the type definition abstract.
    (
        !.ImpItemTypeDefnInfos = [],
        % The list of values for a key in a multi_map should never be empty.
        unexpected($pred, "!.ImpItemTypeDefnInfos = []")
    ;
        !.ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo0],
        ImpItemTypeDefnInfo0 = item_type_defn_info(_, _, TypeDefn0, _, _, _),
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
                ImpItemTypeDefnInfo = ImpItemTypeDefnInfo0 ^ td_ctor_defn
                    := TypeDefn,
                !:ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo]
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
        )
    ;
        !.ImpItemTypeDefnInfos = [_, _ | _],
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
        true
    ).

:- pred add_type_defn_items(list(item_type_defn_info)::in,
    list(item)::in, list(item)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

add_type_defn_items([], !ImpTypeDefnItems, !ImpSelfFIMs).
add_type_defn_items([ImpItemTypeDefnInfo | ImpItemTypeDefnInfos],
        !ImpTypeDefnItems, !ImpSelfFIMs) :-
    ImpTypeDefnItem = item_type_defn(ImpItemTypeDefnInfo),
    !:ImpTypeDefnItems = [ImpTypeDefnItem | !.ImpTypeDefnItems],
    ImpItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
    ( if
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, _, _)
    then
        set.insert(foreign_type_language(ForeignType), !ImpSelfFIMs)
    else
        true
    ),
    add_type_defn_items(ImpItemTypeDefnInfos, !ImpTypeDefnItems, !ImpSelfFIMs).

%---------------------%

:- type maybe_need_imports
    --->    dont_need_imports
    ;       need_imports.

:- type maybe_need_foreign_imports
    --->    dont_need_foreign_imports
    ;       need_foreign_imports.

:- pred find_need_imports(list(item)::in,
    maybe_need_imports::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

find_need_imports(MaybeForeignItems, !:NeedImports, !NeedForeignImportLangs) :-
    !:NeedImports = dont_need_imports,
    find_need_imports_acc(MaybeForeignItems,
        !NeedImports, !NeedForeignImportLangs).

:- pred find_need_imports_acc(list(item)::in,
    maybe_need_imports::in, maybe_need_imports::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

find_need_imports_acc([], !NeedImports, !NeedForeignImports).
find_need_imports_acc([Item | Items], !NeedImports, !NeedForeignImportLangs) :-
    % XXX ITEM_LIST Should do with one call and one switch.
    ItemNeedsImports = item_needs_imports(Item),
    ItemNeedsForeignImportLangs = item_needs_foreign_imports(Item),
    (
        ItemNeedsImports = yes,
        !:NeedImports = need_imports
    ;
        ItemNeedsImports = no
    ),
    set.insert_list(ItemNeedsForeignImportLangs, !NeedForeignImportLangs),
    find_need_imports_acc(Items, !NeedImports, !NeedForeignImportLangs).

%---------------------%

    % strip_unneeded_imp_avails(NeededModules, !Avails):
    %
    % Remove all import_module and use_module declarations for modules
    % that are not in NeededModules.
    %
:- pred strip_unneeded_imp_avails(set(module_name)::in,
    list(item_avail)::in, list(item_avail)::out) is det.

strip_unneeded_imp_avails(NeededModules, !Avails) :-
    list.filter(is_needed_avail(NeededModules), !Avails).

:- pred is_needed_avail(set(module_name)::in, item_avail::in) is semidet.

is_needed_avail(NeededModules, Avail) :-
    ModuleName = item_avail_module_name(Avail),
    set.member(ModuleName, NeededModules).

%---------------------%

:- type foreign_enum_reconstructor
    --->    foreign_enum_reconstructor(
                pragma_info_foreign_enum,
                item_maybe_attrs
            ).

:- pred add_foreign_enum_item_if_needed(type_defn_map::in,
    foreign_enum_reconstructor::in, list(item)::in, list(item)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

add_foreign_enum_item_if_needed(IntTypesMap, ForeignEnumReconstuctor,
        !Items, !NeedForeignImportLangs) :-
    ForeignEnumReconstuctor = foreign_enum_reconstructor(FEInfo,
        MaybeAttrs),
    FEInfo = pragma_info_foreign_enum(_Lang, TypeCtor, _Values),
    ( if
        map.search(IntTypesMap, TypeCtor, Defns),
        some_type_defn_is_non_abstract(Defns)
    then
        Pragma = pragma_foreign_enum(FEInfo),
        ItemPragmaInfo = item_pragma_info(Pragma,  MaybeAttrs,
            term.context_init, -1),
        Item = item_pragma(ItemPragmaInfo),
        !:Items = [Item | !.Items],
        set.insert_list(pragma_needs_foreign_imports(Pragma),
            !NeedForeignImportLangs)
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

:- pred get_int2_items_from_int1(list(item)::in, list(item)::out) is det.

get_int2_items_from_int1(Int1Items, Int2Items) :-
    get_int2_items_from_int1_acc(Int1Items, cord.init, Int2ItemsCord),
    Int2Items = cord.list(Int2ItemsCord).

:- pred get_int2_items_from_int1_acc(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

get_int2_items_from_int1_acc([], !ItemsCord).
get_int2_items_from_int1_acc([Item | Items], !ItemsCord) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        maybe_make_abstract_type_defn_for_int2(ItemTypeDefnInfo,
            MaybeAbstractItemTypeDefnInfo),
        MaybeAbstractItem = item_type_defn(MaybeAbstractItemTypeDefnInfo),
        cord.snoc(MaybeAbstractItem, !ItemsCord)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        cord.snoc(AbstractItem, !ItemsCord)
    ;
        Item = item_instance(ItemInstanceInfo),
        AbstractItemInstanceInfo = ItemInstanceInfo ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstanceInfo),
        cord.snoc(AbstractItem, !ItemsCord)
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ),
        cord.snoc(Item, !ItemsCord)
    ;
        ( Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        )
        % Do not include Item in !ItemsCord.
        % XXX TYPE_REPN Is this the right thing to do for item_type_repn?
    ;
        ( Item = item_foreign_import_module(_)
        ; Item = item_type_repn(_)
        ),
        % We should have filtered out foreign_import_module and nothing
        % items before we get here, and we are not yet generating type_repn
        % items at all.
        unexpected($pred, "item_foreign_import_module/type_repn/nothing")
    ),
    get_int2_items_from_int1_acc(Items, !ItemsCord).

%---------------------------------------------------------------------------%

    % XXX make_abstract_defn should be merged with make_abstract_unify_compare
    % and made det, returning the unchanged item if it does not need to be made
    % abstract (so we can use det switches instead semidet tests in the code).
    % XXX This seems to have been done already.
    %
    % XXX TYPE_REPN The operation of both of those predicates should be changed
    % to remove representation from type_defn items and to put it into separate
    % type_repn items instead.
    %
    % XXX TYPE_REPN Consider the relationship between this predicate and
    % make_impl_type_abstract in write_module_interface_files.m. Unlike this
    % predicate, that one has access to the definitions of the types
    % in this module, so it knows whether e.g. an equivalence type definition
    % makes the defined type equivalent to a type that needs special treatment
    % by the algorithm that decides data representations.
    %
    % XXX Needs a better name.
    %
:- pred maybe_make_abstract_type_defn_for_int2(
    item_type_defn_info::in, item_type_defn_info::out) is det.

maybe_make_abstract_type_defn_for_int2(ItemTypeDefn,
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

:- func clause_in_interface_warning(string, prog_context) = error_spec.

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%---------------------------------------------------------------------------%
% The rest of this module should not be needed.
%---------------------------------------------------------------------------%

make_foreign_import(ModuleName, Lang) = Item :-
    ItemFIM = item_foreign_import_module_info(Lang, ModuleName,
        term.context_init, -1),
    Item = item_foreign_import_module(ItemFIM).

%---------------------------------------------------------------------------%

get_foreign_self_imports_from_item_blocks(ItemBlocks, Langs) :-
    list.foldl(accumulate_foreign_import_langs_in_item_block, ItemBlocks,
        set.init, LangSet),
    set.to_sorted_list(LangSet, Langs).

:- pred accumulate_foreign_import_langs_in_item_block(item_block(MS)::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_foreign_import_langs_in_item_block(ItemBlock, !LangSet) :-
    ItemBlock = item_block(_, _, _, _, Items),
    list.foldl(accumulate_foreign_import_langs_in_item, Items, !LangSet).

:- pred accumulate_foreign_import_langs_in_item(item::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_foreign_import_langs_in_item(Item, !LangSet) :-
    Langs = item_needs_foreign_imports(Item),
    set.insert_list(Langs, !LangSet).

%---------------------------------------------------------------------------%
:- end_module parse_tree.comp_unit_interface.
%---------------------------------------------------------------------------%
