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
:- import_module parse_tree.prog_data.
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

    % generate_pre_grab_pre_qual_item_blocks_int1_int2(RawCompUnit,
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

    % XXX ITEM_LIST This predicate should not need to be exported.
    %
:- func clause_in_interface_warning(string, prog_context) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The predicates in rest of the interface should not be needed at all.
%

    % get_interface(RawCompUnit, InterfaceRawCompUnit):
    %
    % We return interface sections almost intact, changing them only by
    % making instance declarations abstract. We delete implementation
    % sections entirely.
    %
    % XXX ITEM_LIST This predicate is called from module_imports.m.
    % The caller is part of the code that implements --generate-dependencies.
    % The code in write_module_interface_files.m that generates .int and
    % .int2 files calls generate_pre_grab_pre_qual_item_blocks.
    % Since the contents of the .d and .dep files should be derived
    % from what goes into .int and .int2 files, the fact that it uses
    % a different predicate, and therefore a different algorithm,
    % looks like a bug. This is almost certainly the reason why
    % the contents put into .d files by mmc --generate-dependencies
    % gets overwritten by *different* contents after we start generating
    % interface files.
    %
    % XXX ITEM_LIST Change the outputs to two lists of includes, two lists
    % of avails, and two lists of items, for the interface and
    % implementation sections respectively.
    %
:- pred get_interface(raw_compilation_unit::in, raw_compilation_unit::out)
    is det.

    % As above, but also return ...
    % XXX ITEM_LIST document EXACTLY what the second list of item blocks is.
    %
:- pred get_int_and_imp(raw_compilation_unit::in,
    list(raw_item_block)::out, list(raw_item_block)::out) is det.

%---------------------------------------------------------------------------%

    % This predicate is exported for use by modules.m.
    %
    % XXX ITEM_LIST They shouldn't be needed; the representation of the
    % compilation unit should have all this information separate from
    % the items.
    %
:- pred add_needed_foreign_import_module_items_to_item_blocks(module_name::in,
    MS::in, list(item_block(MS))::in, list(item_block(MS))::out) is det.

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

:- import_module parse_tree.check_raw_comp_unit.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_qual.
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
        cord.init, IntItemsCord, do_not_need_avails, NeedAvails, !Specs),
    IntIncls = cord.list(IntInclsCord),
    (
        NeedAvails = do_not_need_avails,
        IntAvails = []
    ;
        NeedAvails = do_need_avails,
        IntAvails = cord.list(IntAvailsCord0)
    ),
    IntItems = cord.list(IntItemsCord),
    MaybeVersionNumbers = no,
    ParseTreeInt0 = parse_tree_int(ModuleName, ifk_int3, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [], IntItems, []),
    module_qualify_parse_tree_int(Globals, ParseTreeInt0, ParseTreeInt,
        !Specs).

:- type need_avails
    --->    do_not_need_avails
    ;       do_need_avails.

:- pred get_short_interface_int3_from_item_blocks(list(raw_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out, need_avails::in, need_avails::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_from_item_blocks([],
        !IntIncls, !IntAvails, !IntItems, !NeedAvails, !Specs).
get_short_interface_int3_from_item_blocks([RawItemBlock | RawItemBlocks],
        !IntIncls, !IntAvails, !IntItems, !NeedAvails, !Specs) :-
    RawItemBlock = item_block(Section, _SectionContext, Incls, Avails, Items),
    (
        Section = ms_interface,
        !:IntIncls = !.IntIncls ++ cord.from_list(Incls),
        !:IntAvails = !.IntAvails ++ cord.from_list(Avails),
        get_short_interface_int3_from_items(Items, !IntItems, !NeedAvails,
            !Specs)
    ;
        Section = ms_implementation
    ),
    get_short_interface_int3_from_item_blocks(RawItemBlocks,
        !IntIncls, !IntAvails, !IntItems, !NeedAvails, !Specs).

:- pred get_short_interface_int3_from_items(list(item)::in,
    cord(item)::in, cord(item)::out, need_avails::in, need_avails::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_from_items([], !IntItems, !NeedAvails,
        !Specs).
get_short_interface_int3_from_items([Item | Items], !IntItems, !NeedAvails,
        !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        make_type_defn_abstract_type_for_int3(ItemTypeDefnInfo,
            AbstractOrForeignItemTypeDefnInfo),
        AbstractOrForeignItem =
            item_type_defn(AbstractOrForeignItemTypeDefnInfo),
        !:IntItems = cord.snoc(!.IntItems, AbstractOrForeignItem)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        !:IntItems = cord.snoc(!.IntItems, AbstractItem),
        !:NeedAvails = do_need_avails
    ;
        Item = item_instance(ItemInstanceInfo),
        AbstractItemInstanceInfo = ItemInstanceInfo ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstanceInfo),
        !:IntItems = cord.snoc(!.IntItems, AbstractItem),
        !:NeedAvails = do_need_avails
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ),
        !:IntItems = cord.snoc(!.IntItems, Item),
        !:NeedAvails = do_need_avails
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
        )
    ;
        ( Item = item_foreign_import_module(_)
        ; Item = item_mutable(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        )
    ),
    get_short_interface_int3_from_items(Items, !IntItems, !NeedAvails,
        !Specs).

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
    ItemBlock = item_block(SrcSection, _, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        % XXX ITEM_LIST Document why we need to add ImportAvails
        % to !ImpAvailsCord.
        list.filter(avail_is_import, Avails, ImportAvails),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(ImportAvails),
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
            !:SectionItemsCord = cord.snoc(!.SectionItemsCord, Item)
        )
    ;
        % XXX ITEM_LIST The action here follows what this predicate used
        % to do before the item list change. I (zs) don't think that
        % it does the right thing for item_nothing, but then again
        % I don't think that such items actually reach here ...
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_foreign_import_module(_)
        ; Item = item_nothing(_)
        ),
        !:SectionItemsCord = cord.snoc(!.SectionItemsCord, Item)
    ;
        Item = item_instance(InstanceInfo),
        AbstractInstanceInfo = make_instance_abstract(InstanceInfo),
        AbstractItem = item_instance(AbstractInstanceInfo),
        !:SectionItemsCord = cord.snoc(!.SectionItemsCord, AbstractItem)
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
            !:SectionItemsCord =
                cord.snoc(!.SectionItemsCord, ConstantGetPredDeclItem),
            !:SectionItemsCord =
                cord.snoc(!.SectionItemsCord, ConstantSetPredDeclItem)
        ;
            ConstantInterface = mutable_not_constant,
            StdGetPredDecl = std_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdSetPredDecl = std_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdGetPredDeclItem = item_pred_decl(StdGetPredDecl),
            StdSetPredDeclItem = item_pred_decl(StdSetPredDecl),
            !:SectionItemsCord =
                cord.snoc(!.SectionItemsCord, StdGetPredDeclItem),
            !:SectionItemsCord =
                cord.snoc(!.SectionItemsCord, StdSetPredDeclItem),

            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = mutable_attach_to_io_state,
                IOGetPredDecl = io_get_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOSetPredDecl = io_set_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                IOGetPredDeclItem = item_pred_decl(IOGetPredDecl),
                IOSetPredDeclItem = item_pred_decl(IOSetPredDecl),
                !:SectionItemsCord =
                    cord.snoc(!.SectionItemsCord, IOGetPredDeclItem),
                !:SectionItemsCord =
                    cord.snoc(!.SectionItemsCord, IOSetPredDeclItem)
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
    IntItems0 = cord.list(IntItemsCord),
    ImpItems = cord.list(ImpItemsCord),
    list.foldl(accumulate_foreign_import_langs_in_item, IntItems0,
        set.init, LangSet0),
    list.foldl(accumulate_foreign_import_langs_in_item, ImpItems,
        LangSet0, LangSet),
    Langs = set.to_sorted_list(LangSet),
    (
        Langs = [],
        IntItems = IntItems0
    ;
        Langs = [_ | _],
        % XXX ITEM_LIST Adding the foreign_import_module items to the
        % interface section is a guess.
        % XXX ITEM_LIST (It seems to be a wrong guess; the compiler bootstraps
        % even if we add them to implementation section.)
        list.foldl(accumulate_foreign_import(ModuleName), Langs,
            IntItems0, IntItems)
    ),
    int_imp_items_to_item_blocks(ModuleNameContext,
        ms_interface, ms_implementation,
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
    RawItemBlock = item_block(Section, _SectionContext, Incls, Avails, Items),
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
        !:ItemsCord = cord.snoc(!.ItemsCord, AbstractItem)
    else
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ),
    generate_pre_grab_pre_qual_items_int(Items, !ItemsCord).

:- pred generate_pre_grab_pre_qual_items_imp(list(item)::in,
    cord(item)::in, cord(item)::out) is det.

generate_pre_grab_pre_qual_items_imp([], !ItemsCord).
generate_pre_grab_pre_qual_items_imp([Item | Items], !ItemsCord) :-
    MaybeIncludeItem = include_in_int_file_implementation(Item),
    (
        MaybeIncludeItem = yes(IncludeItem),
        !:ItemsCord = cord.snoc(!.ItemsCord, IncludeItem)
    ;
        MaybeIncludeItem = no
    ),
    generate_pre_grab_pre_qual_items_imp(Items, !ItemsCord).

:- func include_in_int_file_implementation(item) = maybe(item).

include_in_int_file_implementation(Item) = MaybeIFileItem :-
    (
        % `:- typeclass declarations' may be referred to by the constructors
        % in type declarations. Since these constructors are abstractly
        % exported, we won't need the local instance declarations.
        Item = item_type_defn(ItemTypeDefnInfo),
        maybe_make_abstract_type_defn_for_int2(ItemTypeDefnInfo,
            MaybeAbstractItemTypeDefnInfo),
        AbstractItem = item_type_defn(MaybeAbstractItemTypeDefnInfo),
        MaybeIFileItem = yes(AbstractItem)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        MaybeIFileItem = yes(AbstractItem)
    ;
        Item = item_foreign_import_module(_),
        MaybeIFileItem = yes(Item)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        ( if Pragma = pragma_foreign_enum(_) then
            MaybeIFileItem = yes(Item)
        else
            MaybeIFileItem = no
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
        ; Item = item_nothing(_)
        ),
        MaybeIFileItem = no
    ;
        Item = item_type_repn(_),
        % XXX TYPE_REPN Implement this.
        unexpected($pred, "item_type_repn")
    ).

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
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        cord.init, IntInclsCord, cord.init, ImpInclsCord,
        cord.init, IntAvailsCord, cord.init, ImpAvailsCord0,
        cord.init, IntItemsCord0, cord.init, ImpItemsCord0,
        cord.init, ImpForeignEnumsCord,
        cord.init, IntFIMsCord, cord.init, ImpFIMsCord,
        multi_map.init, IntTypesMap, multi_map.init, ImpTypesMap,
        set.init, ModulesNeededByTypeClassDefns, !Specs),

    IntIncls = cord.list(IntInclsCord),
    ImpIncls = cord.list(ImpInclsCord),
    IntAvails = cord.list(IntAvailsCord),
    ImpAvails0 = cord.list(ImpAvailsCord0),
    IntItems0 = cord.list(IntItemsCord0),
    ImpItems0 = cord.list(ImpItemsCord0),
    ImpForeignEnums = cord.list(ImpForeignEnumsCord),
    IntFIMs = cord.list(IntFIMsCord),
    ImpFIMs = cord.list(ImpFIMsCord),

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

    % Compute the list of type definitions we deleted from the implementation
    % section we want to add back, possibly in their abstract form.
    map.foldl(
        maybe_add_maybe_abstract_type_defn_items(BothTypesMap, IntTypesMap,
            NeededImpTypeCtors),
        ImpTypesMap, [], ImpTypeDefnItems),

    % Figure out which of the foreign enum items we deleted from the
    % implementation section above we want to add back.
    % Record the needs of these foreign enum items for
    % foreign_import_module items.
    list.foldl2(add_foreign_enum_item_if_needed(IntTypesMap), ImpForeignEnums,
        [], ImpForeignEnumItems, set.init, NeedForeignImportLangs0),

    list.foldl(add_foreign_import_item, IntFIMs, IntItems0, IntItems),

    ImpItems1 = ImpForeignEnumItems ++ ImpItems0,
    ImpItems2 = ImpTypeDefnItems ++ ImpItems1,
    % Find out whether anything we are about to put into the implementation
    % section needs any ":- import_module"s or ":- use_module"s at all.
    % XXX ITEM_LIST Given that we have already computed NeededModules,
    % why do we need to compute this cruder measure as well?
    %
    % Record the needs of the ImpTypeDefnItems items for
    % foreign_import_module items.
    find_need_imports(ImpTypeDefnItems, ImpItems1,
        NeedImports, NeedForeignImportLangs0, NeedForeignImportLangs),
    (
        NeedImports = need_imports,
        strip_unneeded_imp_avails(NeededModules, ImpAvails0, ImpAvails)
    ;
        NeedImports = dont_need_imports,
        ImpAvails = []
    ),
    % XXX ITEM_LIST Keeping foreign_import_modules items for *all*
    % foreign languages if we need them for *any* language
    % seems overkill to me (-zs). Similarly, just because we need
    % a foreign_import_module item for *one* module does not mean
    % we need to keep them for *all* modules. True, get_interface
    % *adds* foreign_import_module items only for the current module,
    % but the source code may have them for other modules as well.
    ( if set.is_non_empty(NeedForeignImportLangs) then
        list.foldl(add_foreign_import_item, ImpFIMs, ImpItems2, ImpItems)
    else
        ImpItems = ImpItems2
    ),

    ToCheckIntItemBlock = item_block(ms_interface,
        ModuleNameContext, IntIncls, IntAvails, IntItems),
    check_interface_item_blocks_for_no_exports(Globals,
        ModuleName, ModuleNameContext, [ToCheckIntItemBlock], !Specs),

    % XXX ITEM_LIST See the comment about version numbers in our caller.
    DummyMaybeVersionNumbers = no,
    ParseTreeInt1 = parse_tree_int(ModuleName, ifk_int,
        ModuleNameContext, DummyMaybeVersionNumbers,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems),

    %------%

    % The rest of this predicate body constructs the .int2 file

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
    get_int2_items_from_int1(ImpItems2, ShortImpItems0),
    find_need_imports(ShortIntItems0, [],
        ShortIntNeedImports, set.init, ShortIntNeedForeignImportLangs),
    find_need_imports(ShortImpItems0, [],
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
        list.foldl(add_foreign_import_item, IntFIMs,
            ShortIntItems0, ShortIntItems)
    else
        ShortIntItems = ShortIntItems0
    ),
    ( if set.is_non_empty(ShortImpNeedForeignImportLangs) then
        list.foldl(add_foreign_import_item, ImpFIMs,
            ShortImpItems0, ShortImpItems)
    else
        ShortImpItems = ShortImpItems0
    ),
    ParseTreeInt2 = parse_tree_int(ModuleName, ifk_int2,
        ModuleNameContext, DummyMaybeVersionNumbers,
        IntIncls, ImpIncls, ShortIntAvails, ShortImpAvails,
        ShortIntItems, ShortImpItems).

%---------------------%

:- type type_defn_map == multi_map(type_ctor, item_type_defn_info).
:- type type_defn_pair == pair(type_ctor, item_type_defn_info).

    % get_interface_int1_item_blocks_loop(SrcItemBlocks,
    %   !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
    %   !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord, !ImpFIMsCord,
    %   !IntTypesMap, !ImpTypesMap, !NeededModules, !Specs).
    %
    % Do the bulk of the work needed to generate a module's .int file
    % from the module's augmented compilation unit.
    %
    % The input is the list of source item blocks in the module's
    % augmented compilation unit, *after* they have been processed
    %
    % - by get_interface(include_impl_types, ...), which deletes
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
    % !:NeededModules is the list of the modules needed by any superclass
    % constraints on any typeclass declaration in either the interface or the
    % implementation section. A module may be needed because it defines
    % the typeclass in the superclass constraint, or because it defines
    % a type constructor mentioned anywhere in the superclass constraint.
    %
:- pred get_interface_int1_item_blocks_loop(list(src_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out, cord(item)::in, cord(item)::out,
    cord(foreign_enum_reconstructor)::in,
    cord(foreign_enum_reconstructor)::out,
    cord(item_foreign_import_module_info)::in,
    cord(item_foreign_import_module_info)::out,
    cord(item_foreign_import_module_info)::in,
    cord(item_foreign_import_module_info)::out,
    type_defn_map::in, type_defn_map::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_item_blocks_loop([],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsCord, !ImpFIMsCord,
        !IntTypesMap, !ImpTypesMap, !NeededModules, !Specs).
get_interface_int1_item_blocks_loop([SrcItemBlock | SrcItemBlocks],
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsCord, !ImpFIMsCord,
        !IntTypesMap, !ImpTypesMap, !NeededModules, !Specs) :-
    SrcItemBlock = item_block(SrcSection, _Context, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        !:IntInclsCord = !.IntInclsCord ++ cord.from_list(Incls),
        !:IntAvailsCord = !.IntAvailsCord ++ cord.from_list(Avails),
        get_interface_int1_items_loop_int(Items, !IntItemsCord,
            !IntFIMsCord, !IntTypesMap, !NeededModules, !Specs)
    ;
        SrcSection = sms_implementation,
        !:ImpInclsCord = !.ImpInclsCord ++ cord.from_list(Incls),
        !:ImpAvailsCord = !.ImpAvailsCord ++ cord.from_list(Avails),
        get_interface_int1_items_loop_imp(Items, !ImpItemsCord,
            !ImpForeignEnumsCord, !ImpFIMsCord,
            !ImpTypesMap, !NeededModules, !Specs)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        % The code that transforms sms_implementation to this
        % should not have been run.
        unexpected($pred, "sms_impl_but_exported_to_submodules")
    ),
    get_interface_int1_item_blocks_loop(SrcItemBlocks,
        !IntInclsCord, !ImpInclsCord, !IntAvailsCord, !ImpAvailsCord,
        !IntItemsCord, !ImpItemsCord, !ImpForeignEnumsCord,
        !IntFIMsCord, !ImpFIMsCord,
        !IntTypesMap, !ImpTypesMap, !NeededModules, !Specs).

:- pred get_interface_int1_items_loop_int(list(item)::in,
    cord(item)::in, cord(item)::out,
    cord(item_foreign_import_module_info)::in,
    cord(item_foreign_import_module_info)::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_items_loop_int([], !ItemsCord, !FIMsCord,
        !TypesMap, !NeededModules, !Specs).
get_interface_int1_items_loop_int([Item | Items], !ItemsCord, !FIMsCord,
        !TypesMap, !NeededModules, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, TypeParams, _, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !TypesMap),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ;
        Item = item_typeclass(ItemTypeClass),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !NeededModules)
    ;
        Item = item_clause(ItemClause),
        Context = ItemClause ^ cl_context,
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs]
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _MaybeAttrs, Context, _SeqNum),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs]
        ;
            AllowedInInterface = yes,
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
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
            !:ItemsCord = cord.snoc(!.ItemsCord, Item)
        )
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_instance(_)
        ),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
    ;
        Item = item_foreign_import_module(FIMInfo),
        !:FIMsCord = cord.snoc(!.FIMsCord, FIMInfo)
    ;
        ( Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        % XXX ITEM_LIST The presence of these items in the interface section
        % is an error. I (zs) think they should be silently deleted here;
        % the error should be reported when we try to generate code
        % for this module.
        unexpected($pred, "item_initialise/finalise/mutable")
    ;
        Item = item_type_repn(_),
        % We shouldn't yet have invoked any code that creates
        % type_repn items.
        unexpected($pred, "item_type_repn")
    ;
        Item = item_nothing(_)
        % Ignore the item, which deletes it.
    ),
    get_interface_int1_items_loop_int(Items, !ItemsCord, !FIMsCord,
        !TypesMap, !NeededModules, !Specs).

:- pred get_interface_int1_items_loop_imp(list(item)::in,
    cord(item)::in, cord(item)::out,
    cord(foreign_enum_reconstructor)::in,
    cord(foreign_enum_reconstructor)::out,
    cord(item_foreign_import_module_info)::in,
    cord(item_foreign_import_module_info)::out,
    type_defn_map::in, type_defn_map::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_interface_int1_items_loop_imp([], !ItemsCord,
        !ForeignEnumsCord, !FIMsCord, !TypesMap, !NeededModules, !Specs).
get_interface_int1_items_loop_imp([Item | Items], !ItemsCord,
        !ForeignEnumsCord, !FIMsCord, !TypesMap, !NeededModules, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(Name, TypeParams, _, _, _, _),
        TypeCtor = type_ctor(Name, list.length(TypeParams)),
        multi_map.set(TypeCtor, ItemTypeDefn, !TypesMap)
        % We don't add this to !ItemsCord yet -- we may be removing it.
        % If we *do* want the items for a given type_ctor, we will create
        % new copies of the items from the type_ctor's entry in TypesMap.
    ;
        Item = item_typeclass(ItemTypeClass),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(accumulate_modules_from_constraint, Constraints,
            !NeededModules)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, MaybeAttrs, Context, SeqNum),
        ( if Pragma = pragma_foreign_enum(FEInfo) then
            Reconstructor = foreign_enum_reconstructor(FEInfo, MaybeAttrs,
                Context, SeqNum),
            !:ForeignEnumsCord = cord.snoc(!.ForeignEnumsCord, Reconstructor)
        else
            unexpected($pred, "non-foreign-enum pragma")
        )
    ;
        Item = item_foreign_import_module(FIMInfo),
        !:FIMsCord = cord.snoc(!.FIMsCord, FIMInfo)
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
        ; Item = item_nothing(_)
        ),
        unexpected($pred, "imp item that should be deleted by get_interface")
    ),
    get_interface_int1_items_loop_imp(Items, !ItemsCord,
        !ForeignEnumsCord, !FIMsCord, !TypesMap, !NeededModules, !Specs).

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
    %   defined to be equivalent to a 64 bit float, then code that handles
    %   values of type t1 needs to know this even if type t1 is abstract
    %   exported.
    %
    % - We handle foreign type definitions the same way as equivalence type
    %   definitions, just in case the foreign type is also bigger than a word.
    %
    % - If the definition defines an enum type, and there is a definition
    %   of the same type_ctor in the interface, we include the type_ctor in
    %   AbsExpEnumTypeCtors. This is so that when we abstract export
    %   the type_ctor, we can record that its size is less than one word.
    %
    % - If the definition defines a dummy type, we include the type_ctor in
    %   DummyTypeCtors. XXX ITEM_LIST Presumably (by me -zs) this is so that
    %   when we abstract export them, we can record that it needs no storage.
    %   However, we currently include dummy types in the implementation section
    %   of the .int file unchanged, and we do so even if the type is not
    %   mentioned in the interface section at all.
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
    %% XXX Return in DummyTypeCtors the set of dummy type constructors.
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
        set.init, AbsExpEnumTypeCtors, set.init, DummyTypeCtors),
    set.fold3(accumulate_abs_imp_exported_type_rhs(ImpTypesMap),
        AbsExpEqvLhsTypeCtors,
        set.init, AbsExpEqvRhsTypeCtors, set.init, DuArgTypeCtors,
        set.init, ModulesNeededByTypeDefns),
    NeededImpTypeCtors = set.union_list([AbsExpEqvLhsTypeCtors,
        AbsExpEqvRhsTypeCtors, AbsExpEnumTypeCtors, DummyTypeCtors,
        DuArgTypeCtors]).

:- pred accumulate_abs_imp_exported_type_lhs(type_defn_map::in,
    type_defn_map::in, type_ctor::in, list(item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfos, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DummyTypeCtors) :-
    (
        ImpItemTypeDefnInfos = [],
        % The list of values for a key in a multi_map should never be empty.
        unexpected($pred, "ImpItemTypeDefnInfos = []")
    ;
        ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo],
        % Don't construct a closure when a type_ctor has only definition
        % in the implementation section, since this the common case.
        accumulate_abs_imp_exported_type_lhs_2(IntTypesMap, BothTypesMap,
            TypeCtor, ImpItemTypeDefnInfo,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors, !DummyTypeCtors)
    ;
        ImpItemTypeDefnInfos = [_, _ | _],
        % A type may have multiple definitions in the implementation section
        % because it may be defined both in Mercury and in a foreign language.
        % A type with multiple definitions doesn't typically include
        % an equivalence type among those definitions, but we have to be
        % prepared for such an eventuality anyway.
        list.foldl3(
            accumulate_abs_imp_exported_type_lhs_2(IntTypesMap, BothTypesMap,
                TypeCtor),
            ImpItemTypeDefnInfos,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors, !DummyTypeCtors)
    ).

:- pred accumulate_abs_imp_exported_type_lhs_2(type_defn_map::in,
    type_defn_map::in, type_ctor::in, item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs_2(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfo, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DummyTypeCtors) :-
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
        DetailsDu = type_details_du(Ctors, MaybeEqCmp, MaybeDirectArgCtors),
        ( if
            map.search(IntTypesMap, TypeCtor, _),
            du_type_is_enum(DetailsDu, _NumBits)
        then
            set.insert(TypeCtor, !AbsExpEnumTypeCtors)
        else if
            % XXX ITEM_LIST We don't we insist that TypeCtor occurs
            % in IntTypesMap?
            % XXX ITEM_LIST If a type has one function symbol with arity one
            % and the argument type is equivalent to a dummy type that is
            % defined in another module, we will NOT include TypeCtor in
            % !DummyTypeCtors, since we won't know enough about the contents
            % of the other module.
            constructor_list_represents_dummy_argument_type(BothTypesMap,
                Ctors, MaybeEqCmp, MaybeDirectArgCtors)
        then
            set.insert(TypeCtor, !DummyTypeCtors)
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
        list.foldl3(accumulate_abs_eqv_type_rhs_2(ImpTypesMap), ImpTypeDefns,
            !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns)
    else
        % TypeCtor is not defined in the implementation section
        % of this module.
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_2(type_defn_map::in,
    item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_2(ImpTypesMap, ImpItemTypeDefnInfo,
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
        DetailsDu = type_details_du(Ctors, _, _),
        % There must exist a foreign type alternative to this type.
        % XXX ITEM_LIST I (zs) would like to see a proof argument for that,
        % since I don't think it is true. Unfortunately, we cannot check it
        % locally.

        % As the du type will be exported, we require all the type_ctors
        % inside all the argument types of all the data constructors, and the
        % modules that define them.
        ctors_to_user_type_ctor_set(Ctors, set.init, RhsTypeCtors),
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
        % Our ancestor generate_interface_int1_int2 should be invoked
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
    list(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in) is semidet.

constructor_list_represents_dummy_argument_type(TypeDefnMap,
        Ctors, MaybeCanonical, MaybeDirectArgCtors) :-
    constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
        Ctors, MaybeCanonical, MaybeDirectArgCtors, []).

:- pred constructor_list_represents_dummy_argument_type_2(type_defn_map::in,
    list(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_and_arity))::in, list(mer_type)::in) is semidet.

constructor_list_represents_dummy_argument_type_2(TypeDefnMap, [Ctor],
        canon, no, CoveredTypes) :-
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
                    DetailsDu = type_details_du(Ctors, MaybeEqCmp,
                        MaybeDirectArgCtors),
                    constructor_list_represents_dummy_argument_type_2(
                        TypeDefnMap, Ctors, MaybeEqCmp, MaybeDirectArgCtors,
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
    list(item)::in, list(item)::out) is det.

maybe_add_maybe_abstract_type_defn_items(BothTypesMap, IntTypesMap,
        NeededTypeCtors, TypeCtor, ImpItemTypeDefnInfos, !ImpTypeDefnItems) :-
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
        add_type_defn_items(AbstractImpItemTypeDefnInfos, !ImpTypeDefnItems)
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
    (
        !.ImpItemTypeDefnInfos = [],
        % The list of values for a key in a multi_map should never be empty.
        unexpected($pred, "!.ImpItemTypeDefnInfos = []")
    ;
        !.ImpItemTypeDefnInfos = [ImpItemTypeDefnInfo0],
        ImpItemTypeDefnInfo0 = item_type_defn_info(_, _, TypeDefn0, _, _, _),
        (
            TypeDefn0 = parse_tree_du_type(DetailsDu0),
            DetailsDu0 = type_details_du(Ctors, MaybeEqCmp,
                MaybeDirectArgCtors),
            ( if
                constructor_list_represents_dummy_argument_type(BothTypesMap,
                    Ctors, MaybeEqCmp, MaybeDirectArgCtors)
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
            TypeDefn0 = parse_tree_solver_type(_)
            % get_interface has already made this type abstract.
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
    list(item)::in, list(item)::out) is det.

add_type_defn_items([], !ImpTypeDefnItems).
add_type_defn_items([ImpItemTypeDefnInfo | ImpItemTypeDefnInfos],
        !ImpTypeDefnItems) :-
    ImpTypeDefnItem = item_type_defn(ImpItemTypeDefnInfo),
    !:ImpTypeDefnItems = [ImpTypeDefnItem | !.ImpTypeDefnItems],
    add_type_defn_items(ImpItemTypeDefnInfos, !ImpTypeDefnItems).

%---------------------%

:- type maybe_need_imports
    --->    dont_need_imports
    ;       need_imports.

:- type maybe_need_foreign_imports
    --->    dont_need_foreign_imports
    ;       need_foreign_imports.

:- pred find_need_imports(list(item)::in, list(item)::in,
    maybe_need_imports::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

find_need_imports(MaybeForeignItems, NotForeignItems,
        !:NeedImports, !NeedForeignImportLangs) :-
    !:NeedImports = dont_need_imports,
    find_need_imports_acc(MaybeForeignItems,
        !NeedImports, !NeedForeignImportLangs),
    % Some items in MaybeForeignItems may add new languages to
    % !NeedForeignImportLangs. The items in NotForeignItems shouldn't.
    NeedForeignImportLangs1 = !.NeedForeignImportLangs,
    find_need_imports_acc(NotForeignItems,
        !NeedImports, !NeedForeignImportLangs),
    NeedForeignImportLangs = !.NeedForeignImportLangs,
    expect(set.equal(NeedForeignImportLangs1, NeedForeignImportLangs), $pred,
        "NeedForeignImportLangs1 != NeedForeignImportLangs").

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
                item_maybe_attrs,
                % XXX ITEM_LIST The context and the sequence number
                % don't get written to interface files, so we need
                % the last two fields ONLY for the sanity check
                % that compares the output of get_interface_int1
                % with the output of the old code that used to do
                % the same job. When that sanity check is deleted,
                % these two fields should be deleted as well.
                prog_context,
                int
            ).

:- pred add_foreign_enum_item_if_needed(type_defn_map::in,
    foreign_enum_reconstructor::in, list(item)::in, list(item)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

add_foreign_enum_item_if_needed(IntTypesMap, ForeignEnumReconstuctor,
        !Items, !NeedForeignImportLangs) :-
    ForeignEnumReconstuctor = foreign_enum_reconstructor(FEInfo,
        MaybeAttrs, Context, SeqNum),
    FEInfo = pragma_info_foreign_enum(_Lang, TypeCtor, _Values),
    ( if
        map.search(IntTypesMap, TypeCtor, Defns),
        not (
            Defns = [Defn],
            Defn = item_type_defn_info(_, _, Body, _, _, _),
            Body = parse_tree_abstract_type(_)
        )
    then
        Pragma = pragma_foreign_enum(FEInfo),
        ItemPragmaInfo = item_pragma_info(Pragma,  MaybeAttrs,
            Context, SeqNum),
        Item = item_pragma(ItemPragmaInfo),
        !:Items = [Item | !.Items],
        set.insert_list(pragma_needs_foreign_imports(Pragma),
            !NeedForeignImportLangs)
    else
        true
    ).

%---------------------%

:- pred add_foreign_import_item(item_foreign_import_module_info::in,
    list(item)::in, list(item)::out) is det.

add_foreign_import_item(FIMInfo, !Items) :-
    Item = item_foreign_import_module(FIMInfo),
    !:Items = [Item | !.Items].

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
        !:ItemsCord = cord.snoc(!.ItemsCord, MaybeAbstractItem)
    ;
        Item = item_typeclass(ItemTypeClassInfo),
        AbstractItemTypeClassInfo = ItemTypeClassInfo ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        !:ItemsCord = cord.snoc(!.ItemsCord, AbstractItem)
    ;
        Item = item_instance(ItemInstanceInfo),
        AbstractItemInstanceInfo = ItemInstanceInfo ^ ci_method_instances
            := instance_body_abstract,
        AbstractItem = item_instance(AbstractItemInstanceInfo),
        !:ItemsCord = cord.snoc(!.ItemsCord, AbstractItem)
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ),
        !:ItemsCord = cord.snoc(!.ItemsCord, Item)
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
        ; Item = item_nothing(_)
        ),
        % We should have filtered out foreign_import_module and nothing
        % items before we get here, and we are not yet generating type_repn
        % items at all.
        unexpected($pred, "item_foreign_import_module/type_repn/nothing")
    ),
    get_int2_items_from_int1_acc(Items, !ItemsCord).

%---------------------------------------------------------------------------%

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%---------------------------------------------------------------------------%
% The rest of this module should not be needed.
%---------------------------------------------------------------------------%

get_interface(RawCompUnit, InterfaceRawCompUnit) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, RawItemBlocks),
    % XXX ITEM_LIST Don't compute _NonIFileItemBlocksCord
    % just to throw it away. If we mode-specialize
    % get_ifile_and_noifile_in_raw_item_blocks_acc for
    % dont_gather_noifile_items, will the compiler optimize away
    % the arguments for NoIFileItemBlocks?
    get_ifile_and_noifile_in_raw_item_blocks_acc(dont_gather_noifile_items,
        RawItemBlocks,
        cord.init, IFileItemBlocksCord, cord.init, _NoIFileItemBlocksCord),
    IFileItemBlocks0 = cord.list(IFileItemBlocksCord),
    % XXX ITEM_LIST The ms_interface is a guess.
    % XXX ITEM_LIST (It seems to be a wrong guess; the compiler bootstraps
    % even if we pass ms_implementation here.)
    add_needed_foreign_import_module_items_to_item_blocks(ModuleName,
        ms_interface, IFileItemBlocks0, IFileItemBlocks),
    InterfaceRawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, IFileItemBlocks).

get_int_and_imp(RawCompUnit, IFileItemBlocks, NoIFileItemBlocks) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, _ModuleNameContext, RawItemBlocks),
    get_ifile_and_noifile_in_raw_item_blocks_acc(gather_noifile_items,
        RawItemBlocks,
        cord.init, IFileItemBlocksCord, cord.init, NoIFileItemBlocksCord),
    IFileItemBlocks0 = cord.list(IFileItemBlocksCord),
    NoIFileItemBlocks = cord.list(NoIFileItemBlocksCord),
    % XXX ITEM_LIST The ms_interface is a guess.
    add_needed_foreign_import_module_items_to_item_blocks(ModuleName,
        ms_interface, IFileItemBlocks0, IFileItemBlocks).

%---------------------------------------------------------------------------%

:- type maybe_gather_noifile_items
    --->    dont_gather_noifile_items
    ;       gather_noifile_items.

:- mode gather_noifile_items == in(bound(gather_noifile_items)).
:- mode dont_gather_noifile_items == in(bound(dont_gather_noifile_items)).

:- pred get_ifile_and_noifile_in_raw_item_blocks_acc(
    maybe_gather_noifile_items, list(raw_item_block),
    cord(raw_item_block), cord(raw_item_block),
    cord(raw_item_block), cord(raw_item_block)).
:- mode get_ifile_and_noifile_in_raw_item_blocks_acc(
    dont_gather_noifile_items, in, in, out, in, out) is det.
:- mode get_ifile_and_noifile_in_raw_item_blocks_acc(
    gather_noifile_items, in, in, out, in, out) is det.

get_ifile_and_noifile_in_raw_item_blocks_acc(_,
        [], !IFileItemBlocksCord, !NoIFileItemBlocksCord).
get_ifile_and_noifile_in_raw_item_blocks_acc(GatherNoIFileItems,
        [RawItemBlock | RawItemBlocks],
        !IFileItemBlocksCord, !NoIFileItemBlocksCord) :-
    RawItemBlock = item_block(Section, SectionContext, Incls, Avails, Items),
    (
        Section = ms_interface,
        IFileIncls = Incls,
        IFileAvails = Avails,
        NoIFileIncls = [],
        NoIFileAvails = []
    ;
        Section = ms_implementation,
        IFileIncls = [],
        IFileAvails = [],
        (
            GatherNoIFileItems = dont_gather_noifile_items,
            NoIFileIncls = [],
            NoIFileAvails = []
        ;
            GatherNoIFileItems = gather_noifile_items,
            NoIFileIncls = Incls,
            NoIFileAvails = Avails
        )
    ),
    get_ifile_and_noifile_in_items_acc(GatherNoIFileItems, Section,
        Items, cord.init, IFileItemsCord, cord.init, NoIFileItemsCord),
    IFileItems = cord.list(IFileItemsCord),
    NoIFileItems = cord.list(NoIFileItemsCord),
    ( if
        IFileIncls = [],
        IFileAvails = [],
        IFileItems = []
    then
        true
    else
        IFileItemBlock = item_block(Section, SectionContext,
            IFileIncls, IFileAvails, IFileItems),
        !:IFileItemBlocksCord =
            cord.snoc(!.IFileItemBlocksCord, IFileItemBlock)
    ),
    ( if
        NoIFileIncls = [],
        NoIFileAvails = [],
        NoIFileItems = []
    then
        true
    else
        NoIFileItemBlock = item_block(Section, SectionContext,
            NoIFileIncls, NoIFileAvails, NoIFileItems),
        !:NoIFileItemBlocksCord =
            cord.snoc(!.NoIFileItemBlocksCord, NoIFileItemBlock)
    ),
    get_ifile_and_noifile_in_raw_item_blocks_acc(GatherNoIFileItems,
        RawItemBlocks, !IFileItemBlocksCord, !NoIFileItemBlocksCord).

:- pred get_ifile_and_noifile_in_items_acc(maybe_gather_noifile_items,
    module_section, list(item),
    cord(item), cord(item), cord(item), cord(item)).
:- mode get_ifile_and_noifile_in_items_acc(
    dont_gather_noifile_items, in, in, in, out, in, out) is det.
:- mode get_ifile_and_noifile_in_items_acc(
    gather_noifile_items, in, in, in, out, in, out) is det.

get_ifile_and_noifile_in_items_acc(_, _,
        [], !IFileItemsCord, !NoIFileItemsCord).
get_ifile_and_noifile_in_items_acc(GatherNoIFileItems, Section,
        [Item | Items], !IFileItemsCord, !NoIFileItemsCord) :-
    (
        Section = ms_interface,
        ( if Item = item_instance(ItemInstance) then
            % Include the abstract version of the instance in the
            % interface, ...
            AbstractItemInstance = make_instance_abstract(ItemInstance),
            AbstractItem = item_instance(AbstractItemInstance),
            !:IFileItemsCord = cord.snoc(!.IFileItemsCord, AbstractItem),

            % ... and the concrete version in the implementation.
            (
                GatherNoIFileItems = dont_gather_noifile_items
            ;
                GatherNoIFileItems = gather_noifile_items,
                !:NoIFileItemsCord = cord.snoc(!.NoIFileItemsCord, Item)
            )
        else
            !:IFileItemsCord = cord.snoc(!.IFileItemsCord, Item)
        )
    ;
        Section = ms_implementation,
        (
            GatherNoIFileItems = dont_gather_noifile_items
        ;
            GatherNoIFileItems = gather_noifile_items,
            !:NoIFileItemsCord = cord.snoc(!.NoIFileItemsCord, Item)
        )
    ),
    get_ifile_and_noifile_in_items_acc(GatherNoIFileItems, Section,
        Items, !IFileItemsCord, !NoIFileItemsCord).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_needed_foreign_import_module_items_to_item_blocks(ModuleName, Section,
        ItemBlocks0, ItemBlocks) :-
    list.foldl(accumulate_foreign_import_langs_in_item_block, ItemBlocks0,
        set.init, LangSet),
    set.to_sorted_list(LangSet, Langs),
    (
        Langs = [],
        ItemBlocks = ItemBlocks0
    ;
        Langs = [_ | _],
        ImportItems = list.map(make_foreign_import(ModuleName), Langs),
        ImportItemBlock = item_block(Section, term.context_init,
            [], [], ImportItems),
        ItemBlocks = [ImportItemBlock | ItemBlocks0]
    ).

:- pred accumulate_foreign_import(module_name::in, foreign_language::in,
    list(item)::in, list(item)::out) is det.

accumulate_foreign_import(ModuleName, Lang, !Items) :-
    !:Items = [make_foreign_import(ModuleName, Lang) | !.Items].

:- func make_foreign_import(module_name, foreign_language) = item.

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
