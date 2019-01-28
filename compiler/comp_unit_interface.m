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

:- import_module parse_tree.item_util.

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
        maybe_make_abstract_type_defn(sifk_int2,
            ItemTypeDefnInfo, MaybeAbstractItemTypeDefnInfo),
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

%---------------------%

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

%---------------------%

:- func make_foreign_import(module_name, foreign_language) = item.

make_foreign_import(ModuleName, Lang) = Item :-
    ItemFIM = item_foreign_import_module_info(Lang, ModuleName,
        term.context_init, -1),
    Item = item_foreign_import_module(ItemFIM).

%---------------------------------------------------------------------------%
:- end_module parse_tree.comp_unit_interface.
%---------------------------------------------------------------------------%
