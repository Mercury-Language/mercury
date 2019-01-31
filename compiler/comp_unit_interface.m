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

:- type maybe_include_impl_types
    --->    dont_include_impl_types
    ;       include_impl_types.

    % Given the raw compilation unit of a module, extract and return
    % the part of that module that will go into the .int file of the module.
    % This will typically mostly be the interface section of the module,
    % but it may also contain parts of the implementation section as well.
    % Both parts may be somewhat modified; for example, we may remove
    % the bodies of instance definitions in an interface section,
    % but put the original, non-abstract instance definition in the
    % implementation section.
    %
:- pred get_interface(maybe_include_impl_types::in,
    raw_compilation_unit::in, raw_compilation_unit::out) is det.

    % As above, but also return ...
    % XXX ITEM_LIST document EXACTLY what the second list of item blocks is.
    %
:- pred get_int_and_impl(maybe_include_impl_types::in,
    raw_compilation_unit::in,
    list(raw_item_block)::out, list(raw_item_block)::out) is det.

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

:- func clause_in_interface_warning(string, prog_context) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_kind.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_interface(IncludeImplTypes, RawCompUnit, InterfaceRawCompUnit) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, RawItemBlocks),
    % XXX ITEM_LIST Don't compute _NonIFileItemBlocksCord
    % just to throw it away. If we mode-specialize
    % get_ifile_and_noifile_in_raw_item_blocks_acc for
    % dont_gather_noifile_items, will the compiler optimize away
    % the arguments for NoIFileItemBlocks?
    get_ifile_and_noifile_in_raw_item_blocks_acc(IncludeImplTypes,
        dont_gather_noifile_items, RawItemBlocks,
        cord.init, IFileItemBlocksCord, cord.init, _NoIFileItemBlocksCord),
    IFileItemBlocks0 = cord.list(IFileItemBlocksCord),
    % XXX ITEM_LIST The ms_interface is a guess.
    add_needed_foreign_import_module_items_to_item_blocks(ModuleName,
        ms_interface, IFileItemBlocks0, IFileItemBlocks),
    InterfaceRawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, IFileItemBlocks).

get_int_and_impl(IncludeImplTypes, RawCompUnit,
        IFileItemBlocks, NoIFileItemBlocks) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, _ModuleNameContext, RawItemBlocks),
    get_ifile_and_noifile_in_raw_item_blocks_acc(IncludeImplTypes,
        gather_noifile_items, RawItemBlocks,
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

:- pred get_ifile_and_noifile_in_raw_item_blocks_acc(maybe_include_impl_types,
    maybe_gather_noifile_items, list(raw_item_block),
    cord(raw_item_block), cord(raw_item_block),
    cord(raw_item_block), cord(raw_item_block)).
:- mode get_ifile_and_noifile_in_raw_item_blocks_acc(in,
    dont_gather_noifile_items, in, in, out, in, out) is det.
:- mode get_ifile_and_noifile_in_raw_item_blocks_acc(in,
    gather_noifile_items, in, in, out, in, out) is det.

get_ifile_and_noifile_in_raw_item_blocks_acc(_, _,
        [], !IFileItemBlocksCord, !NoIFileItemBlocksCord).
get_ifile_and_noifile_in_raw_item_blocks_acc(IncludeImplTypes,
        GatherNoIFileItems, [RawItemBlock | RawItemBlocks],
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
        (
            IncludeImplTypes = dont_include_impl_types,
            IFileIncls = [],
            IFileAvails = []
        ;
            IncludeImplTypes = include_impl_types,
            IFileIncls = Incls,
            IFileAvails = Avails
        ),
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
    get_ifile_and_noifile_in_items_acc(IncludeImplTypes, GatherNoIFileItems,
        Section, Items,
        cord.init, IFileItemsCord, cord.init, NoIFileItemsCord),
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
    get_ifile_and_noifile_in_raw_item_blocks_acc(IncludeImplTypes,
        GatherNoIFileItems, RawItemBlocks,
        !IFileItemBlocksCord, !NoIFileItemBlocksCord).

:- pred get_ifile_and_noifile_in_items_acc(maybe_include_impl_types,
    maybe_gather_noifile_items, module_section, list(item),
    cord(item), cord(item), cord(item), cord(item)).
:- mode get_ifile_and_noifile_in_items_acc(in,
    dont_gather_noifile_items, in, in, in, out, in, out) is det.
:- mode get_ifile_and_noifile_in_items_acc(in,
    gather_noifile_items, in, in, in, out, in, out) is det.

get_ifile_and_noifile_in_items_acc(_, _, _,
        [], !IFileItemsCord, !NoIFileItemsCord).
get_ifile_and_noifile_in_items_acc(IncludeImplTypes, GatherNoIFileItems,
        Section, [Item | Items], !IFileItemsCord, !NoIFileItemsCord) :-
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
        ),
        % XXX ITEM_LIST Unify include_in_int_file_implementation with
        % include_in_short_interface.
        ( if
            IncludeImplTypes = include_impl_types,
            include_in_int_file_implementation(Item) = yes(IFileItem)
        then
            !:IFileItemsCord = cord.snoc(!.IFileItemsCord, IFileItem)
        else
            true
        )
    ),
    get_ifile_and_noifile_in_items_acc(IncludeImplTypes, GatherNoIFileItems,
        Section, Items, !IFileItemsCord, !NoIFileItemsCord).

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
        make_abstract_typeclass(ItemTypeClassInfo, AbstractItemTypeClassInfo),
        AbstractItem = item_typeclass(AbstractItemTypeClassInfo),
        MaybeIFileItem = yes(AbstractItem)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        (
            Pragma = pragma_foreign_enum(_),
            MaybeIFileItem = yes(Item)
        ;
            % XXX I am not sure about the proper value of MaybeIFileItem
            % for some of these. -zs
            ( Pragma = pragma_foreign_decl(_)
            ; Pragma = pragma_foreign_code(_)
            ; Pragma = pragma_foreign_proc(_)
            ; Pragma = pragma_foreign_proc_export(_)
            ; Pragma = pragma_foreign_export_enum(_)
            ; Pragma = pragma_external_proc(_)
            ; Pragma = pragma_type_spec(_)
            ; Pragma = pragma_inline(_)
            ; Pragma = pragma_no_inline(_)
            ; Pragma = pragma_consider_used(_)
            ; Pragma = pragma_unused_args(_)
            ; Pragma = pragma_exceptions(_)
            ; Pragma = pragma_trailing_info(_)
            ; Pragma = pragma_mm_tabling_info(_)
            ; Pragma = pragma_obsolete(_)
            ; Pragma = pragma_no_detism_warning(_)
            ; Pragma = pragma_require_tail_recursion(_)
            ; Pragma = pragma_oisu(_)
            ; Pragma = pragma_tabled(_)
            ; Pragma = pragma_fact_table(_)
            ; Pragma = pragma_promise_eqv_clauses(_)
            ; Pragma = pragma_promise_pure(_)
            ; Pragma = pragma_promise_semipure(_)
            ; Pragma = pragma_termination_info(_)
            ; Pragma = pragma_termination2_info(_)
            ; Pragma = pragma_terminates(_)
            ; Pragma = pragma_does_not_terminate(_)
            ; Pragma = pragma_check_termination(_)
            ; Pragma = pragma_mode_check_clauses(_)
            ; Pragma = pragma_structure_sharing(_)
            ; Pragma = pragma_structure_reuse(_)
            ; Pragma = pragma_require_feature_set(_)
            ),
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
        Item = item_foreign_import_module(_),
        MaybeIFileItem = yes(Item)
    ;
        Item = item_type_repn(_),
        % XXX TYPE_REPN Implement this.
        unexpected($pred, "item_type_repn")
    ).

%---------------------------------------------------------------------------%

generate_short_interface_int3(Globals, RawCompUnit, ParseTreeInt, !:Specs) :-
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleNameContext, RawItemBlocks),
    !:Specs = [],
    get_short_interface_int3_for_item_blocks(RawItemBlocks,
        cord.init, IntInclsCord, cord.init, IntAvailsCord0,
        cord.init, IntItemsCord0, cord.init, StdFIMItemsCord,
        do_not_need_avails, NeedAvails, need_fims(set.init), NeedFIMs,
        implicit_langs(set.init), ImplicitLangs, !Specs),
    IntIncls = cord.list(IntInclsCord),
    (
        NeedAvails = do_not_need_avails,
        IntAvails = []
    ;
        NeedAvails = do_need_avails,
        IntAvails = cord.list(IntAvailsCord0)
    ),
    IntItems0 = cord.list(IntItemsCord0),
    NeedFIMs = need_fims(NeedFIMLangs),
    ( if set.is_empty(NeedFIMLangs) then
        IntItems = IntItems0
    else
        % The StdFIMItems come from the source module, while ImplicitFIMItems
        % are created here by us based on the needs on *all* the items
        % in the source module.
        %
        % XXX This code preserves old behavior. I (zs) do not understand
        % why we want that behavior.
        %
        % First, why are we including in the .int3 file foreign_import_module
        % items that refer to languages that the rest of the .int3 file does
        % not refer to? In other words, why aren't we filtering StdFIMItems
        % and ImplicitFIMItems and keeping only the ones whose languages
        % are in NeedFIMLangs?
        %
        % Second, why do .int3 file need foreign_import_module items at all?
        StdFIMItems = cord.list(StdFIMItemsCord),
        ImplicitLangs = implicit_langs(ImplicitLangsSet),
        ImplicitFIMItems = list.map(make_foreign_import(ModuleName),
            set.to_sorted_list(ImplicitLangsSet)),
        IntItems = IntItems0 ++ StdFIMItems ++ ImplicitFIMItems
    ),
    MaybeVersionNumbers = no,
    ParseTreeInt0 = parse_tree_int(ModuleName, ifk_int3, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [], IntItems, []),
    module_qualify_parse_tree_int(Globals, ParseTreeInt0, ParseTreeInt,
        !Specs).

:- type need_avails
    --->    do_not_need_avails
    ;       do_need_avails.

:- type need_fims
    --->    need_fims(set(foreign_language)).

:- type implicit_langs
    --->    implicit_langs(set(foreign_language)).

:- pred get_short_interface_int3_for_item_blocks(list(raw_item_block)::in,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    cord(item)::in, cord(item)::out,
    need_avails::in, need_avails::out,
    need_fims::in, need_fims::out,
    implicit_langs::in, implicit_langs::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_for_item_blocks([],
        !IntIncls, !IntAvails, !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs).
get_short_interface_int3_for_item_blocks([RawItemBlock | RawItemBlocks],
        !IntIncls, !IntAvails, !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs) :-
    RawItemBlock = item_block(Section, _SectionContext, Incls, Avails, Items),
    (
        Section = ms_interface,
        !:IntIncls = !.IntIncls ++ cord.from_list(Incls),
        !:IntAvails = !.IntAvails ++ cord.from_list(Avails),
        get_short_interface_int3_for_items(Items, !IntItems, !StdFIMItems,
            !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs)
    ;
        Section = ms_implementation
    ),
    get_short_interface_int3_for_item_blocks(RawItemBlocks,
        !IntIncls, !IntAvails, !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs).

:- pred get_short_interface_int3_for_items(list(item)::in,
    cord(item)::in, cord(item)::out,
    cord(item)::in, cord(item)::out,
    need_avails::in, need_avails::out,
    need_fims::in, need_fims::out,
    implicit_langs::in, implicit_langs::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_short_interface_int3_for_items([], !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs).
get_short_interface_int3_for_items([Item | Items], !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        make_type_defn_abstract_type_for_int3(ItemTypeDefnInfo,
            AbstractOrForeignItemTypeDefnInfo,
            !NeedAvails, !NeedFIMs, !ImplicitLangs),
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
        Item = item_foreign_import_module(FIMInfo),
        FIMInfo = item_foreign_import_module_info(Lang, Module, _, _),
        StdFIMInfo = item_foreign_import_module_info(Lang, Module,
            term.context_init, -1),
        StdItem = item_foreign_import_module(StdFIMInfo),
        !:StdFIMItems = cord.snoc(!.StdFIMItems, StdItem)
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
        Langs = pragma_needs_foreign_imports(Pragma),
        !.ImplicitLangs = implicit_langs(ImplicitLangsSet0),
        set.insert_list(Langs, ImplicitLangsSet0, ImplicitLangsSet),
        !:ImplicitLangs = implicit_langs(ImplicitLangsSet)
    ;
        Item = item_mutable(_),
        Langs = all_foreign_languages,
        !.ImplicitLangs = implicit_langs(ImplicitLangsSet0),
        set.insert_list(Langs, ImplicitLangsSet0, ImplicitLangsSet),
        !:ImplicitLangs = implicit_langs(ImplicitLangsSet)
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        )
    ),
    get_short_interface_int3_for_items(Items, !IntItems, !StdFIMItems,
        !NeedAvails, !NeedFIMs, !ImplicitLangs, !Specs).

:- pred make_type_defn_abstract_type_for_int3(item_type_defn_info::in,
    item_type_defn_info::out,
    need_avails::in, need_avails::out, need_fims::in, need_fims::out,
    implicit_langs::in, implicit_langs::out) is det.

make_type_defn_abstract_type_for_int3(ItemTypeDefn,
        AbstractOrForeignItemTypeDefnInfo,
        !NeedAvails, !NeedFIMs, !ImplicitLangs) :-
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
        ),
        % XXX ITEM_LIST This preserves old behavior, but I (zs) do not see
        % why this is needed: foreign type definitions do not contain
        % any sym_names with whose module qualification any imported modules
        % could help.
        !:NeedAvails = do_need_avails,

        Lang = foreign_type_language(ForeignType),
        !.NeedFIMs = need_fims(NeedFIMLangs0),
        set.insert(Lang, NeedFIMLangs0, NeedFIMLangs),
        !:NeedFIMs = need_fims(NeedFIMLangs),

        !.ImplicitLangs = implicit_langs(ImplicitLangsSet0),
        set.insert(Lang, ImplicitLangsSet0, ImplicitLangsSet),
        !:ImplicitLangs = implicit_langs(ImplicitLangsSet)
    ).

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

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%---------------------------------------------------------------------------%
:- end_module parse_tree.comp_unit_interface.
%---------------------------------------------------------------------------%
