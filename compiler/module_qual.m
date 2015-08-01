%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_qual.m.
% Main authors: stayl, fjh.
%
% Module qualifies types, insts and modes within declaration items.
% The head of all declarations should be module qualified in prog_io.m.
% This module qualifies the bodies of the declarations. Checks for
% undefined types, insts and modes. Uses two passes over the item list,
% one to collect all type, mode and inst ids and a second to do the
% qualification and report errors. If the --warn-interface-imports
% option is set, warns about modules imported in the interface that do
% not need to be in the interface. The modes of lambda expressions are
% qualified in modes.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_qual.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.status.
:- import_module recompilation.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % module_qualify_aug_comp_unit(Globals, AugCompUnit0, AugCompUnit,
    %   EventSpecMap0, EventSpecMap, MaybeContext, EventSpecFileName,
    %   MQ_Info, UndefTypes, UndefModes, !Specs):
    %
    % AugCompUnit is AugCompUnit0 with all items module qualified
    % as much as possible; likewise for EventSpecMap0 and EventSpecMap.
    %
    % Errors in EventSpecMap0 will be reported as being for EventSpecFileName.
    %
:- pred module_qualify_aug_comp_unit(globals::in,
    aug_compilation_unit::in, aug_compilation_unit::out,
    event_spec_map::in, event_spec_map::out, string::in, mq_info::out,
    bool::out, bool::out, list(error_spec)::in, list(error_spec)::out) is det.

    % module_qualify_parse_tree_int(Globals, ParseTreeInt0, ParseTreeInt,
    %   !Specs):
    %
    % ParseTreeInt is ParseTreeInt0 with all items module qualified
    % as much as possible.
    %
:- pred module_qualify_parse_tree_int(globals::in,
    parse_tree_int::in, parse_tree_int::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds to qualify the mode of a lambda expression.
    %
:- pred qualify_lambda_mode_list(list(mer_mode)::in,
    list(mer_mode)::out, prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds.m to qualify the modes in a
    % clause mode annotation.
    %
:- pred qualify_clause_mode_list(list(mer_mode)::in,
    list(mer_mode)::out, prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds to qualify an explicit type qualification.
    %
:- pred qualify_type_qualification(mer_type::in, mer_type::out,
    prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % The type mq_info holds information needed for doing module qualification.
    %
:- type mq_info.

:- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_need_qual_flag(mq_info::in, need_qualifier::out) is det.
:- pred mq_info_get_partial_qualifier_info(mq_info::in,
    partial_qualifier_info::out) is det.
:- pred mq_info_get_recompilation_info(mq_info::in,
    maybe(recompilation_info)::out) is det.

:- pred mq_info_set_need_qual_flag(need_qualifier::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_recompilation_info(maybe(recompilation_info)::in,
    mq_info::in, mq_info::out) is det.

    % The type partial_qualifier_info holds info need for computing which
    % partial quantifiers are visible -- see get_partial_qualifiers/3.
    %
:- type partial_qualifier_info.

    % Suppose we are processing a definition which defines the symbol
    % foo.bar.baz.quux/1. Then we insert the following symbols
    % into the symbol table:
    %   - if the current value of the NeedQual flag at this point
    %       is `may_be_unqualified',
    %       i.e. module `foo.bar.baz' was imported
    %       then we insert the fully unqualified symbol quux/1;
    %   - if module `foo.bar.baz' occurs in the "imported" section,
    %       i.e. if module `foo.bar' was imported,
    %       then we insert the partially qualified symbol baz.quux/1;
    %   - if module `foo.bar' occurs in the "imported" section,
    %       i.e. if module `foo' was imported,
    %       then we insert the partially qualified symbol bar.baz.quux/1;
    %   - we always insert the fully qualified symbol foo.bar.baz.quux/1.
    %
    % The predicate `get_partial_qualifiers' returns all of the
    % partial qualifiers for which we need to insert definitions,
    % i.e. all the ones which are visible. For example,
    % given as input `foo.bar.baz', it returns a list containing
    %   (1) `baz', iff `foo.bar' is imported, and
    %   (2) `bar.baz', iff `foo' is imported.
    % Note that the caller will still need to handle the fully-qualified
    % and fully-unqualified versions separately.
    %
:- pred get_partial_qualifiers(module_name::in, partial_qualifier_info::in,
    list(module_name)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

module_qualify_aug_comp_unit(Globals, AugCompUnit0, AugCompUnit,
        EventSpecMap0, EventSpecMap, EventSpecFileName,
        !:Info, UndefTypes, UndefModes, !Specs) :-
    AugCompUnit0 = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, SrcItemBlocks0,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),
    init_mq_info(Globals, ModuleName, SrcItemBlocks0,
        DirectIntItemBlocks ++ IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks, yes, !:Info),
    collect_mq_info_in_item_blocks(src_section_mq_info, SrcItemBlocks0,
        !Info),
    collect_mq_info_in_item_blocks(int_section_mq_info, DirectIntItemBlocks,
        !Info),
    % XXX ITEM_LIST asymmetry: collect from 2, qualify 1
    module_qualify_items_in_src_item_blocks(SrcItemBlocks0, SrcItemBlocks,
        !Info, !Specs),
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, SrcItemBlocks,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),

    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    do_module_qualify_event_specs(EventSpecFileName,
        EventSpecList0, EventSpecList, !Info, !Specs),
    map.from_assoc_list(EventSpecList, EventSpecMap),
    mq_info_get_type_error_flag(!.Info, UndefTypes),
    mq_info_get_mode_error_flag(!.Info, UndefModes),

    % Warn about any unused module imports in the interface.
    % There is a special case involving type class instances that
    % we need to handle here. Consider:
    %
    %   :- module foo.
    %   :- interface.
    %
    %   :- import_module bar.
    %   :- typeclass tc1(T) <= tc2(T).
    %   :- instance tc1(unit).
    %
    % where module bar exports the instance tc2(unit). We must import
    % the module bar in the interface of the module foo in order for
    % the superclass constraint on the instance tc1(unit) to be satisfied.
    % However, at this stage of compilation we do not know that the
    % instance tc2(unit) needs to be visible. (Knowing this would require
    % a more extensive analysis of type classes and instances to be done
    % in this module.)
    %
    % In order to prevent the import of the module bar being erroneously
    % reported as unused, we make the conservative assumption that any
    % imported module that exports a type class instance is used in
    % the interface of the importing module, except if the importing
    % module itself exports _no_ type class instances.
    %
    mq_info_get_unused_interface_modules(!.Info, UnusedImports0),
    mq_info_get_exported_instances_flag(!.Info, ModuleExportsInstances),
    (
        ModuleExportsInstances = yes,
        mq_info_get_imported_instance_modules(!.Info, InstanceImports),
        set.difference(UnusedImports0, InstanceImports, UnusedImports1)
    ;
        ModuleExportsInstances = no,
        UnusedImports1 = UnusedImports0
    ),
    set.to_sorted_list(UnusedImports1, UnusedImports),
    % XXX ITEM_LIST Currently we report all modules whose imports
    % are unnecessarily in the interface using the context of the
    % `:- module' declaration of the module. We should use the contexts
    % of the actual imports themselves.
    maybe_warn_unused_interface_imports(ModuleName, ModuleNameContext,
        UnusedImports, !Specs).

module_qualify_parse_tree_int(Globals, ParseTreeInt0, ParseTreeInt, !Specs) :-
    ParseTreeInt0 = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntItems0, ImpItems0),
    IntSrcItemBlocks0 =
        [item_block(sms_interface, term.context_init, IntItems0)],
    ImpSrcItemBlocks0 =
        [item_block(sms_implementation, term.context_init, ImpItems0)],
    % XXX ITEM_LIST The completely separate treatment of the interface
    % and implementation part of an interface file preserves old behavior;
    % write_short_interface_file in write_module_interface_files.m used
    % to call module_qualify_items separately on the interface and
    % implementation items. I (zs) this is likely to be a bug, in that
    % the module qualification of the items in the implementation section
    % doesn't see the declarations in the interface section.
    % XXX ITEM_LIST Check whether we can get a nonempty list of implicit
    % dependencies in either call to collect_mq_info_in_aug_item_blocks below.
    DummyItemBlocks = []:list(src_item_block),
    some [!IntInfo] (
        init_mq_info(Globals, ModuleName, IntSrcItemBlocks0,
            DummyItemBlocks, DummyItemBlocks, DummyItemBlocks, no, !:IntInfo),
        collect_mq_info_in_item_blocks(src_section_mq_info, IntSrcItemBlocks0,
            !IntInfo),
        module_qualify_items_loop(IntItems0, IntItems, !.IntInfo, _, !Specs)
    ),
    some [!ImpInfo] (
        init_mq_info(Globals, ModuleName, ImpSrcItemBlocks0,
            DummyItemBlocks, DummyItemBlocks, DummyItemBlocks, no, !:ImpInfo),
        collect_mq_info_in_item_blocks(src_section_mq_info, ImpSrcItemBlocks0,
            !ImpInfo),
        module_qualify_items_loop(ImpItems0, ImpItems, !.ImpInfo, _, !Specs)
    ),
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntItems, ImpItems).

%-----------------------------------------------------------------------------%

qualify_lambda_mode_list(Modes0, Modes, Context, !Info, !Specs) :-
    ErrorContext = mqec_lambda_expr(Context),
    qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs).

qualify_clause_mode_list(Modes0, Modes, Context, !Info, !Specs) :-
    ErrorContext = mqec_clause_mode_annotation(Context),
    qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs).

qualify_type_qualification(Type0, Type, Context, !Info, !Specs) :-
    ErrorContext = mqec_type_qual(Context),
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs).

:- type mq_info
    --->    mq_info(
                % XXX ITEM_LIST Reorder to put related fields together.

                % Modules which have been imported or used, i.e. the ones
                % for which there was a `:- import_module' or `:- use_module'
                % declaration in this module.
                mqi_imported_modules            :: set(module_name),

                % Modules which have been imported or used in the interface.
                mqi_interface_visible_modules   :: set(module_name),

                % Sets of all modules, types, insts, modes, and typeclasses
                % visible in this module. Impl_types is the set of all types
                % visible from the implementation of the module.
                mqi_modules                     :: module_id_set,
                mqi_types                       :: type_id_set,
                mqi_impl_types                  :: type_id_set,
                mqi_insts                       :: inst_id_set,
                mqi_modes                       :: mode_id_set,
                mqi_classes                     :: class_id_set,

                % Modules imported in the interface that are not definitely
                % needed in the interface.
                mqi_unused_interface_modules    :: set(module_name),

                % Modules from which `:- instance' declarations have
                % been imported.
                mqi_imported_instance_modules   :: set(module_name),

                % Does this module export any type class instances?
                mqi_exported_instances_flag      :: bool,

                % XXX ITEM_LIST Should be passed separately from the mq_info.
                % Import status of the current item.
                mqi_import_status               :: mq_import_status,

                % The number of errors found.
                mqi_num_errors                  :: int,

                % Are there any undefined types or typeclasses.
                mqi_type_error_flag             :: bool,

                % Are there any undefined insts or modes.
                mqi_mode_error_flag             :: bool,

                % Do we want to report errors.
                mqi_report_error_flag           :: bool,

                % The name of the current module.
                mqi_this_module                 :: module_name,

                % XXX ITEM_LIST Should be passed separately from the mq_info.
                % Must uses of the current item be explicitly module qualified.
                mqi_need_qual_flag              :: need_qualifier,

                mqi_maybe_recompilation_info    :: maybe(recompilation_info)
            ).

:- type partial_qualifier_info
    --->    partial_qualifier_info(module_id_set).

mq_info_get_partial_qualifier_info(MQInfo, QualifierInfo) :-
    mq_info_get_modules(MQInfo, ModuleIdSet),
    QualifierInfo = partial_qualifier_info(ModuleIdSet).

    % We only need to keep track of what is exported and what isn't,
    % so we use a simpler data type here than hlds_pred.import_status.
    %
:- type mq_import_status
    --->    mq_status_exported
    ;       mq_status_local
    ;       mq_status_imported(import_locn)
    ;       mq_status_abstract_imported.

%-----------------------------------------------------------------------------%

:- type section_mq_info(MS) == (pred(MS, mq_import_status, need_qualifier)).
:- inst section_mq_info     == (pred(in, out, out) is det).

:- pred src_section_mq_info(src_module_section::in,
    mq_import_status::out, need_qualifier::out) is det.

src_section_mq_info(sms_interface,
    mq_status_exported, may_be_unqualified).
src_section_mq_info(sms_implementation,
    mq_status_local, may_be_unqualified).
src_section_mq_info(sms_impl_but_exported_to_submodules,
    mq_status_local, may_be_unqualified).

:- pred int_section_mq_info(int_module_section::in,
    mq_import_status::out, need_qualifier::out) is det.

int_section_mq_info(ims_imported(_ModuleName, _IntFileKind, Locn),
    mq_status_imported(Locn), may_be_unqualified).
int_section_mq_info(ims_used(_ModuleName, _IntFileKind, Locn),
    mq_status_imported(Locn), must_be_qualified).
int_section_mq_info(ims_abstract_imported(_ModuleName, _IntFileKind),
    mq_status_abstract_imported, must_be_qualified).

%-----------------------------------------------------------------------------%

    % Pass over the item_block list collecting all defined module, type, mode
    % and inst ids, all module synonym definitions, and the names of all
    % modules imported in the interface.
    %
:- pred collect_mq_info_in_item_blocks(
    section_mq_info(MS)::in(section_mq_info), list(item_block(MS))::in,
    mq_info::in, mq_info::out) is det.

collect_mq_info_in_item_blocks(_, [], !Info).
collect_mq_info_in_item_blocks(SectionInfo, [ItemBlock | ItemBlocks],
        !Info) :-
    ItemBlock = item_block(Section, _Context, Items),
    SectionInfo(Section, MQStatus, NeedQualFlag),
    mq_info_set_import_status(MQStatus, !Info),
    mq_info_set_need_qual_flag(NeedQualFlag, !Info),
    collect_mq_info_in_items(Items, !Info),
    collect_mq_info_in_item_blocks(SectionInfo, ItemBlocks, !Info).

    % Pass over the item list collecting all defined module, type, mode and
    % inst ids, all module synonym definitions, and the names of all
    % modules imported in the interface.
    %
:- pred collect_mq_info_in_items(list(item)::in,
    mq_info::in, mq_info::out) is det.

collect_mq_info_in_items([], !Info).
collect_mq_info_in_items([Item | Items], !Info) :-
    collect_mq_info_in_item(Item, !Info),
    collect_mq_info_in_items(Items, !Info).

:- pred collect_mq_info_in_item(item::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_item(Item, !Info) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ModuleDefn = md_include_module(IncludedModuleName),
            add_module_defn(IncludedModuleName, !Info)
        ;
            ( ModuleDefn = md_import(ImportedModuleName)
            ; ModuleDefn = md_use(ImportedModuleName)
            ),
            maybe_add_import(ImportedModuleName, !Info)
        )
    ;
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(SymName, Params, _, _, _, _),
        mq_info_get_import_status(!.Info, ImportStatus),
        ( if ImportStatus = mq_status_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_types(!.Info, Types0),
            mq_info_get_impl_types(!.Info, ImplTypes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Types0, Types),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity),
                ImplTypes0, ImplTypes),
            mq_info_set_types(Types, !Info),
            mq_info_set_impl_types(ImplTypes, !Info)
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        ItemInstDefn = item_inst_defn_info(SymName, Params, _, _, _, _),
        mq_info_get_import_status(!.Info, ImportStatus),
        ( if ImportStatus = mq_status_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_insts(!.Info, Insts0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Insts0, Insts),
            mq_info_set_insts(Insts, !Info)
        )
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(SymName, Params, _, _, _, _),
        mq_info_get_import_status(!.Info, ImportStatus),
        ( if ImportStatus = mq_status_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_modes(!.Info, Modes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Modes0, Modes),
            mq_info_set_modes(Modes, !Info)
        )
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(_PromiseType, Goal, _ProgVarSet,
            _UnivVars, _Context, _SeqNum),
        process_assert(Goal, SymNames, Success),
        (
            Success = yes,
            list.foldl(collect_mq_info_qualified_symname, SymNames, !Info)
        ;
            % Any unqualified symbol in the promise might come from *any* of
            % the imported modules. There's no way for us to tell which ones.
            % So we conservatively assume that it uses all of them.
            Success = no,
            set.init(UnusedInterfaceModules),
            mq_info_set_unused_interface_modules(UnusedInterfaceModules, !Info)
        )
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(SymName, Params, _, _, _, _, _, _),
        mq_info_get_import_status(!.Info, ImportStatus),
        ( if ImportStatus = mq_status_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_classes(!.Info, Classes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity),
                Classes0, Classes),
            mq_info_set_classes(Classes, !Info)
        )
    ;
        Item = item_instance(ItemInstance),
        mq_info_get_import_status(!.Info, ImportStatus),
        ( if ImportStatus = mq_status_imported(_) then
            InstanceModule = ItemInstance ^ ci_module_containing_instance,
            mq_info_get_imported_instance_modules(!.Info,
                ImportedInstanceModules0),
            set.insert(InstanceModule,
                ImportedInstanceModules0, ImportedInstanceModules),
            mq_info_set_imported_instance_modules(ImportedInstanceModules,
                !Info)
        else
            true
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        )
        % Do nothing.
    ).

:- pred collect_mq_info_qualified_symname(sym_name::in,
    mq_info::in, mq_info::out) is det.

collect_mq_info_qualified_symname(SymName, !Info) :-
    (
        SymName = qualified(ModuleName, _),
        mq_info_set_module_used(ModuleName, !Info)
    ;
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified")
    ).

    % For submodule definitions (whether nested or separate,
    % i.e. either `:- module foo.' or `:- include_module foo.'),
    % add the module id to the module_id_set.
    %
    % We don't actually handle nested submodules here. Nested submodules
    % were separated out and replaced with the internal representation of
    % the `:- include_module' declaration that would correspond to it
    % by the code that created the module's raw_compilation_unit, which
    % was later transformed into the aug_compilation_unit whose items
    % we process here.
    %
:- pred add_module_defn(module_name::in,
    mq_info::in, mq_info::out) is det.

add_module_defn(ModuleName, !Info) :-
    mq_info_get_modules(!.Info, Modules0),
    mq_info_get_need_qual_flag(!.Info, NeedQualifier),
    Arity = 0,
    id_set_insert(NeedQualifier, mq_id(ModuleName, Arity), Modules0, Modules),
    mq_info_set_modules(Modules, !Info).

    % For import declarations (`:- import_module' or `:- use_module'),
    % if we are currently in the interface section, then add the
    % imported modules to the unused_interface_modules list.
    %
    % XXX ITEM_LIST Why do we base this decision on the status we get
    % from the mq_info, instead of directly on the current section's
    % section kind?
    %
:- pred maybe_add_import(module_name::in, mq_info::in, mq_info::out)
    is det.

maybe_add_import(ImportedModuleName, !Info) :-
    mq_info_get_import_status(!.Info, Status),
    % XXX ITEM_LIST Consider rewriting this to use a switch on Status.

    % Modules imported from the the proper private interface of ancestors of
    % the current module are treated as if they were directly imported
    % by the current module.
    ( if
        ( Status = mq_status_local
        ; Status = mq_status_exported
        ; Status = mq_status_imported(
            import_locn_ancestor_private_interface_proper)
        )
    then
        mq_info_get_imported_modules(!.Info, Modules0),
        set.insert(ImportedModuleName, Modules0, Modules),
        mq_info_set_imported_modules(Modules, !Info)
    else
        true
    ),

    % We check that all modules imported in the interface are
    % used in the interface.
    ( if Status = mq_status_exported then
        mq_info_get_unused_interface_modules(!.Info, UnusedIntModules0),
        set.insert(ImportedModuleName, UnusedIntModules0, UnusedIntModules),
        mq_info_set_unused_interface_modules(UnusedIntModules, !Info)
    else
        true
    ),

    % Only modules imported in the interface or in the private
    % interface of ancestor modules may be used in the interface.
    ( if
        ( Status = mq_status_exported
        ; Status = mq_status_imported(
            import_locn_ancestor_private_interface_proper)
        )
    then
        mq_info_get_interface_visible_modules(!.Info, IntModules0),
        set.insert(ImportedModuleName, IntModules0, IntModules),
        mq_info_set_interface_visible_modules(IntModules, !Info)
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % Scan Goal, building the list of qualified symbols, SymNames.
    % If there exists a single unqualified symbol in Goal, set Success to no.
    %
:- pred process_assert(goal::in, list(sym_name)::out, bool::out) is det.

process_assert(Goal, SymNames, Success) :-
    % AAA Some more stuff to do accumulator introduction on, it
    % would be better to rewrite using maybes and then to declare
    % the maybe_and predicate to be associative.
    % NB. accumulator introduction doesn't work on this case yet.
    %
    (
        ( Goal = conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = par_conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = disj_expr(_, SubGoalA, SubGoalB)
        ; Goal = implies_expr(_, SubGoalA, SubGoalB)
        ; Goal = equivalent_expr(_, SubGoalA, SubGoalB)
        ),
        process_assert(SubGoalA, SymNamesA, SuccessA),
        process_assert(SubGoalB, SymNamesB, SuccessB),
        SymNames = SymNamesA ++ SymNamesB,
        bool.and(SuccessA, SuccessB, Success)
    ;
        ( Goal = true_expr(_)
        ; Goal = fail_expr(_)
        ),
        SymNames = [],
        Success = yes
    ;
        ( Goal = not_expr(_, SubGoal)
        ; Goal = some_expr(_, _, SubGoal)
        ; Goal = some_state_vars_expr(_, _, SubGoal)
        ; Goal = all_expr(_, _, SubGoal)
        ; Goal = all_state_vars_expr(_, _, SubGoal)
        ; Goal = promise_purity_expr(_, _, SubGoal)
        ; Goal = promise_equivalent_solutions_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_sets_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_arbitrary_expr(_, _, _, _, _,
            SubGoal)
        ; Goal = require_detism_expr(_, _, SubGoal)
        ; Goal = require_complete_switch_expr(_, _, SubGoal)
        ; Goal = require_switch_arms_detism_expr(_, _, _, SubGoal)
        ; Goal = trace_expr(_, _, _, _, _, SubGoal)
        ),
        process_assert(SubGoal, SymNames, Success)
    ;
        Goal = try_expr(_, _, SubGoal, Then, MaybeElse, Catches,
            MaybeCatchAny),
        process_assert(SubGoal, SymNamesGoal, SuccessGoal),
        process_assert(Then, SymNamesThen, SuccessThen),
        maybe_process_assert(MaybeElse, SymNamesElse, SuccessElse),
        list.map2(process_assert_catch, Catches,
            SymNamesCatches, SuccessCatches),
        (
            MaybeCatchAny = yes(catch_any_expr(_, CatchAnyGoal)),
            process_assert(CatchAnyGoal, SymNamesCatchAny, SuccessCatchAny)
        ;
            MaybeCatchAny = no,
            SymNamesCatchAny = [],
            SuccessCatchAny = no
        ),
        SymNamesLists = [SymNamesGoal, SymNamesThen, SymNamesElse,
            SymNamesCatchAny | SymNamesCatches],
        list.condense(SymNamesLists, SymNames),
        SuccessLists = [SuccessGoal, SuccessThen, SuccessElse, SuccessCatchAny
            | SuccessCatches],
        bool.and_list(SuccessLists, Success)
    ;
        Goal = atomic_expr(_, _, _, _, MainGoal, OrElseGoals),
        process_assert(MainGoal, SymNamesMainGoal, SuccessMainGoal),
        process_assert_list(OrElseGoals,
            SymNamesOrElseGoals, SuccessOrElseGoals),
        SymNames = SymNamesMainGoal ++ SymNamesOrElseGoals,
        bool.and(SuccessMainGoal, SuccessOrElseGoals, Success)
    ;
        Goal = if_then_else_expr(_, _, _, GoalCond, GoalThen, GoalElse),
        process_assert(GoalCond, SymNamesCond, SuccessCond),
        process_assert(GoalThen, SymNamesThen, SuccessThen),
        process_assert(GoalElse, SymNamesElse, SuccessElse),
        SymNames = SymNamesCond ++ SymNamesThen ++ SymNamesElse,
        bool.and(SuccessCond, SuccessThen, Success0),
        bool.and(Success0, SuccessElse, Success)
    ;
        Goal = event_expr(_, _Name, Args0),
        list.map(term.coerce, Args0, Args),
        ( if term_qualified_symbols_list(Args, SymNamesPrime) then
            SymNames = SymNamesPrime,
            Success = yes
        else
            SymNames = [],
            Success = no
        )
    ;
        Goal = call_expr(_, SymName, Args0, _Purity),
        (
            SymName = qualified(_, _),
            list.map(term.coerce, Args0, Args),
            ( if term_qualified_symbols_list(Args, SymNames0) then
                SymNames = [SymName | SymNames0],
                Success = yes
            else
                SymNames = [],
                Success = no
            )
        ;
            SymName = unqualified(_),
            SymNames = [],
            Success = no
        )
    ;
        Goal = unify_expr(_, LHS0, RHS0, _Purity),
        term.coerce(LHS0, LHS),
        term.coerce(RHS0, RHS),
        ( if
            term_qualified_symbols(LHS, SymNamesL),
            term_qualified_symbols(RHS, SymNamesR)
        then
            list.append(SymNamesL, SymNamesR, SymNames),
            Success = yes
        else
            SymNames = [],
            Success = no
        )
    ).

:- pred maybe_process_assert(maybe(goal)::in, list(sym_name)::out, bool::out)
    is det.

maybe_process_assert(no, [], yes).
maybe_process_assert(yes(Goal), Symbols, Success) :-
    process_assert(Goal, Symbols, Success).

:- pred process_assert_catch(catch_expr::in, list(sym_name)::out, bool::out)
    is det.

process_assert_catch(catch_expr(Pattern0, Goal), SymNames, Success) :-
    term.coerce(Pattern0, Pattern),
    ( if
        term_qualified_symbols(Pattern, SymNamesPattern),
        process_assert(Goal, SymNamesGoal, ProcessAssertSuccess),
        ProcessAssertSuccess = yes
    then
        list.append(SymNamesPattern, SymNamesGoal, SymNames),
        Success = yes
    else
        SymNames = [],
        Success = no
    ).

    % Performs process_assert on a list of goals.
    %
:- pred process_assert_list(list(goal)::in, list(sym_name)::out,
    bool::out) is det.

process_assert_list(Goals, SymNames, Success) :-
    (
        Goals = [],
        SymNames = [],
        Success = yes
    ;
        Goals = [HeadGoal | TailGoals],
        process_assert(HeadGoal, SymNamesHead, SuccessHead),
        process_assert_list(TailGoals, SymNamesTail, SuccessTail),
        list.append(SymNamesHead, SymNamesTail, SymNames),
        bool.and(SuccessHead, SuccessTail, Success)
    ).

    % Given a term, T, return the list of all the sym_names, S, in the term.
    % The predicate fails if any sub-term of T is unqualified.
    %
:- pred term_qualified_symbols(term::in, list(sym_name)::out) is semidet.

term_qualified_symbols(Term, Symbols) :-
    ( if try_parse_sym_name_and_args(Term, SymName, Args) then
        SymName = qualified(_, _),
        term_qualified_symbols_list(Args, Symbols0),
        Symbols = [SymName | Symbols0]
    else
        Symbols = []
    ).

:- pred term_qualified_symbols_list(list(term)::in, list(sym_name)::out)
    is semidet.

term_qualified_symbols_list([], []).
term_qualified_symbols_list([Term | Terms], Symbols) :-
    term_qualified_symbols(Term, TermSymbols),
    term_qualified_symbols_list(Terms, Symbols0),
    list.append(Symbols0, TermSymbols, Symbols).

%-----------------------------------------------------------------------------%

    % Iterate over the items in the given blocks, module qualifying
    % all declarations. Stop when we reach a section that is imported,
    % since imported declarations should already be module qualified.
    % (An imported block should never be followed by an unimported block.)
    %
:- pred module_qualify_items_in_src_item_blocks(
    list(src_item_block)::in, list(src_item_block)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_items_in_src_item_blocks([], [], !Info, !Specs).
module_qualify_items_in_src_item_blocks([SrcItemBlock0 | SrcItemBlocks0],
        [SrcItemBlock | SrcItemBlocks], !Info, !Specs) :-
    SrcItemBlock0 = item_block(SrcSection, Context, Items0),
    (
        SrcSection = sms_interface,
        mq_info_set_import_status(mq_status_exported, !Info)
    ;
        SrcSection = sms_implementation,
        mq_info_set_import_status(mq_status_local, !Info)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        mq_info_set_import_status(mq_status_local, !Info)
    ),
    module_qualify_items_loop(Items0, Items, !Info, !Specs),
    SrcItemBlock = item_block(SrcSection, Context, Items),
    module_qualify_items_in_src_item_blocks(SrcItemBlocks0, SrcItemBlocks,
        !Info, !Specs).

:- pred module_qualify_items_loop(list(item)::in, list(item)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_items_loop([], [], !Info, !Specs).
module_qualify_items_loop([Item0 | Items0], [Item | Items], !Info, !Specs) :-
    module_qualify_item(Item0, Item, !Info, !Specs),
    module_qualify_items_loop(Items0, Items, !Info, !Specs).

    % Call predicates to qualify a single item.
    %
:- pred module_qualify_item(item::in, item::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item(Item0, Item, !Info, !Specs) :-
    (
        ( Item0 = item_clause(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_nothing(_)
        ),
        Item = Item0
    ;
        Item0 = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ( ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            )
        ;
            ModuleDefn = md_include_module(_),
            % The submodule might make use of *any* of the imported modules.
            % There is no way for us to tell which ones.
            % So we conservatively assume that it uses all of them.
            % XXX ITEM_LIST Fix this too-conservative behavior.
            % If a submodule needs a module that the parent doesn't,
            % it should import it for itself. Anything else may lead to
            % unnecessary recompilations.
            set.init(UnusedInterfaceModules),
            mq_info_set_unused_interface_modules(UnusedInterfaceModules, !Info)
        ),
        Item = Item0
    ;
        Item0 = item_type_defn(ItemTypeDefn0),
        ItemTypeDefn0 = item_type_defn_info(SymName, Params, TypeDefn0,
            TVarSet, Context, SeqNum),
        list.length(Params, Arity),
        TypeCtor = type_ctor(SymName, Arity),
        qualify_type_defn(TypeDefn0, TypeDefn, Context, TypeCtor,
            !Info, !Specs),
        ItemTypeDefn = item_type_defn_info(SymName, Params, TypeDefn,
            TVarSet, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn)
    ;
        Item0 = item_inst_defn(ItemInstDefn0),
        ItemInstDefn0 = item_inst_defn_info(SymName, Params, InstDefn0,
            InstVarSet, Context, SeqNum),
        list.length(Params, Arity),
        ErrorContext = mqec_inst(Context, mq_id(SymName, Arity)),
        qualify_inst_defn(InstDefn0, InstDefn, ErrorContext, !Info, !Specs),
        ItemInstDefn = item_inst_defn_info(SymName, Params, InstDefn,
            InstVarSet, Context, SeqNum),
        Item = item_inst_defn(ItemInstDefn)
    ;
        Item0 = item_mode_defn(ItemModeDefn0),
        ItemModeDefn0 = item_mode_defn_info(SymName, Params, ModeDefn0,
            InstVarSet, Context, SeqNum),
        list.length(Params, Arity),
        ErrorContext = mqec_mode(Context, mq_id(SymName, Arity)),
        qualify_mode_defn(ModeDefn0, ModeDefn, ErrorContext, !Info, !Specs),
        ItemModeDefn = item_mode_defn_info(SymName, Params, ModeDefn,
            InstVarSet, Context, SeqNum),
        Item = item_mode_defn(ItemModeDefn)
    ;
        Item0 = item_pred_decl(ItemPredDecl0),
        ItemPredDecl0 = item_pred_decl_info(SymName, PredOrFunc,
            TypesAndModes0, MaybeWithType0, MaybeWithInst0, MaybeDetism,
            Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints0, Context, SeqNum),
        list.length(TypesAndModes0, Arity),
        ErrorContext = mqec_pred_or_func(Context, PredOrFunc,
            mq_id(SymName, Arity)),
        qualify_types_and_modes(TypesAndModes0, TypesAndModes, ErrorContext,
            !Info, !Specs),
        ConstraintErrorContext = mqcec_pred_decl(Context, PredOrFunc,
            SymName, Arity),
        qualify_prog_constraints(Constraints0, Constraints,
            ConstraintErrorContext, !Info, !Specs),
        (
            MaybeWithType0 = yes(WithType0),
            % XXX We could pass a more specific error context.
            qualify_type(WithType0, WithType, ErrorContext, !Info, !Specs),
            MaybeWithType = yes(WithType)
        ;
            MaybeWithType0 = no,
            MaybeWithType = no
        ),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(WithInst0, WithInst, ErrorContext, !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc,
            TypesAndModes, MaybeWithType, MaybeWithInst, MaybeDetism,
            Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints, Context, SeqNum),
        Item = item_pred_decl(ItemPredDecl)
    ;
        Item0 = item_mode_decl(ItemModeDecl0),
        ItemModeDecl0 = item_mode_decl_info(SymName, PredOrFunc, Modes0,
            MaybeWithInst0, MaybeDetism, InstVarSet, Context, SeqNum),
        list.length(Modes0, Arity),
        ErrorContext = mqec_pred_or_func_mode(Context, PredOrFunc,
            mq_id(SymName, Arity)),
        qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(WithInst0, WithInst, ErrorContext, !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        ItemModeDecl = item_mode_decl_info(SymName, PredOrFunc, Modes,
            MaybeWithInst, MaybeDetism, InstVarSet, Context, SeqNum),
        Item = item_mode_decl(ItemModeDecl)
    ;
        Item0 = item_pragma(ItemPragma0),
        ItemPragma0 = item_pragma_info(Pragma0, Origin, Context, SeqNum),
        qualify_pragma(Pragma0, Pragma, Context, !Info, !Specs),
        ItemPragma = item_pragma_info(Pragma, Origin, Context, SeqNum),
        Item = item_pragma(ItemPragma)
    ;
        Item0 = item_typeclass(ItemTypeClass0),
        ItemTypeClass0 = item_typeclass_info(Name, Vars, Constraints0, FunDeps,
            Interface0, VarSet, Context, SeqNum),
        list.length(Vars, Arity),
        ConstraintErrorContext = mqcec_class_defn(Context, Name, Arity),
        qualify_prog_constraint_list(Constraints0, Constraints,
            ConstraintErrorContext, !Info, !Specs),
        (
            Interface0 = class_interface_abstract,
            Interface = class_interface_abstract
        ;
            Interface0 = class_interface_concrete(Methods0),
            ErrorContext = mqec_class(Context, mq_id(Name, Arity)),
            qualify_class_methods(Methods0, Methods, ErrorContext,
                !Info, !Specs),
            Interface = class_interface_concrete(Methods)
        ),
        ItemTypeClass = item_typeclass_info(Name, Vars, Constraints, FunDeps,
            Interface, VarSet, Context, SeqNum),
        Item = item_typeclass(ItemTypeClass)
    ;
        Item0 = item_instance(ItemInstance0),
        ItemInstance0 = item_instance_info(Name0, Types0, OriginalTypes0,
            Constraints0, Body0, VarSet, ModName, Context, SeqNum),
        list.length(Types0, Arity),
        Id0 = mq_id(Name0, Arity),
        ErrorContext = mqec_instance(Context, Id0),

        ( if mq_info_get_import_status(!.Info, mq_status_exported) then
            mq_info_set_exported_instances_flag(yes, !Info)
        else
            true
        ),

        % We don't qualify the implementation yet, since that requires
        % us to resolve overloading.
        ConstraintErrorContext = mqcec_instance_defn(Context, Name0,
            OriginalTypes0),
        qualify_prog_constraint_list(Constraints0, Constraints,
            ConstraintErrorContext, !Info, !Specs),
        qualify_class_name(Id0, Id, ErrorContext, !Info, !Specs),
        Id = mq_id(Name, _),
        % XXX We don't want to keep the errors from the expansion of both
        % forms of the instance types, since printing two error messages about
        % one instance definition that make apparently contradictory
        % assumptions about whether the instance types are equiv-type-expanded
        % or not wouldd be confusing. However, I (zs) cannot think of any
        % compelling reason right now for preferring the error messages
        % from one version of the types over the other.
        qualify_type_list(Types0, Types, ErrorContext, !Info, !Specs),
        qualify_type_list(OriginalTypes0, OriginalTypes, ErrorContext,
            !Info, !.Specs, _),
        qualify_instance_body(Name, Body0, Body),
        ItemInstance = item_instance_info(Name, Types, OriginalTypes,
            Constraints, Body, VarSet, ModName, Context, SeqNum),
        Item = item_instance(ItemInstance)
    ;
        Item0 = item_mutable(ItemMutable0),
        do_module_qualify_mutable(ItemMutable0, ItemMutable, !Info, !Specs),
        Item = item_mutable(ItemMutable)
    ).

%-----------------------------------------------------------------------------%

:- pred do_module_qualify_mutable(
    item_mutable_info::in, item_mutable_info::out,
    mq_info::in, mq_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

do_module_qualify_mutable(ItemMutable0, ItemMutable, !Info, !Specs) :-
    ItemMutable0 = item_mutable_info(Name, Type0, InitTerm, Inst0,
        Attrs, Varset, Context, SeqNum),
    ErrorContext = mqec_mutable(Context, Name),
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
    qualify_inst(Inst0, Inst, ErrorContext, !Info, !Specs),
    ItemMutable = item_mutable_info(Name, Type, InitTerm, Inst,
        Attrs, Varset, Context, SeqNum).

:- pred do_module_qualify_event_specs(string::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_specs(_, [], [], !Info, !Specs).
do_module_qualify_event_specs(FileName,
        [Name - Spec0 | NameSpecs0], [Name - Spec | NameSpecs],
        !Info, !Specs) :-
    do_module_qualify_event_spec(FileName, Spec0, Spec, !Info, !Specs),
    do_module_qualify_event_specs(FileName, NameSpecs0, NameSpecs,
        !Info, !Specs).

:- pred do_module_qualify_event_spec(string::in,
    event_spec::in, event_spec::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_spec(FileName, EventSpec0, EventSpec, !Info, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SynthAttrNumOrder),
    list.map_foldl2(
        do_module_qualify_event_attr(EventName, FileName, EventLineNumber),
        Attrs0, Attrs, !Info, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SynthAttrNumOrder).

:- pred do_module_qualify_event_attr(string::in, string::in, int::in,
    event_attribute::in, event_attribute::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_attr(EventName, FileName, LineNumber, Attr0, Attr,
        !Info, !Specs) :-
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode0,
        MaybeSynthCall),
    Context = context(FileName, LineNumber),
    ErrorContext = mqec_event_spec_attr(Context, EventName, AttrName),
    qualify_type(AttrType0, AttrType, ErrorContext, !Info, !Specs),
    qualify_mode(AttrMode0, AttrMode, ErrorContext, !Info, !Specs),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

    % Qualify the constructors or other types in a type definition.
    %
:- pred qualify_type_defn(type_defn::in, type_defn::out,
    prog_context::in, type_ctor::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn(TypeDefn0, TypeDefn, Context, TypeCtor, !Info, !Specs) :-
    (
        TypeDefn0 = parse_tree_du_type(Ctors0, MaybeUserEqComp0,
            MaybeDirectArgCtors0),
        qualify_constructors(Ctors0, Ctors, TypeCtor, !Info, !Specs),
        % User-defined equality pred names will be converted into predicate
        % calls and then module-qualified after type analysis (during mode
        % analysis). That way, they get full type overloading resolution, etc.
        % Thus we don't module-qualify them here.
        MaybeUserEqComp = MaybeUserEqComp0,
        MaybeDirectArgCtors = MaybeDirectArgCtors0,
        TypeDefn = parse_tree_du_type(Ctors, MaybeUserEqComp,
            MaybeDirectArgCtors)
    ;
        TypeDefn0 = parse_tree_eqv_type(Type0),
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
        TypeDefn = parse_tree_eqv_type(Type)
    ;
        TypeDefn0 = parse_tree_abstract_type(_),
        TypeDefn = TypeDefn0
    ;
        TypeDefn0 = parse_tree_foreign_type(_, _, _),
        TypeDefn = TypeDefn0
    ;
        TypeDefn0 = parse_tree_solver_type(SolverTypeDetails0,
            MaybeUserEqComp),
        SolverTypeDetails0 = solver_type_details(RepnType0, InitPred,
            GroundInst0, AnyInst0, MutableItems0),
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        qualify_type(RepnType0, RepnType, ErrorContext, !Info, !Specs),
        qualify_inst(GroundInst0, GroundInst, ErrorContext, !Info, !Specs),
        qualify_inst(AnyInst0, AnyInst, ErrorContext, !Info, !Specs),
        qualify_constraint_stores(MutableItems0, MutableItems, !Info, !Specs),
        SolverTypeDetails  = solver_type_details(RepnType, InitPred,
            GroundInst, AnyInst, MutableItems),
        TypeDefn = parse_tree_solver_type(SolverTypeDetails,
            MaybeUserEqComp)
    ).

:- pred qualify_constraint_stores(
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constraint_stores([], [], !Info, !Specs).
qualify_constraint_stores([Mutable0 | Mutables0], [Mutable | Mutables],
        !Info, !Specs) :-
    do_module_qualify_mutable(Mutable0, Mutable, !Info, !Specs),
    qualify_constraint_stores(Mutables0, Mutables, !Info, !Specs).

:- pred qualify_constructors(list(constructor)::in, list(constructor)::out,
    type_ctor::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructors([], [], _ContainingTypeCtor, !Info, !Specs).
qualify_constructors([Ctor0 | Ctors0], [Ctor | Ctors], ContainingTypeCtor,
        !Info, !Specs) :-
    qualify_constructor(Ctor0, Ctor, ContainingTypeCtor, !Info, !Specs),
    qualify_constructors(Ctors0, Ctors, ContainingTypeCtor, !Info, !Specs).

:- pred qualify_constructor(constructor::in, constructor::out,
    type_ctor::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor(Ctor0, Ctor, ContainingTypeCtor, !Info, !Specs) :-
    Ctor0 = ctor(ExistQVars, Constraints0, FunctionSymbolSymName, Args0,
        Arity, Context),
    FunctionSymbolName = unqualify_name(FunctionSymbolSymName),
    ConstraintErrorContext = mqcec_type_defn_constructor(Context,
        ContainingTypeCtor, FunctionSymbolName),
    qualify_prog_constraint_list(Constraints0, Constraints,
        ConstraintErrorContext, !Info, !Specs),
    qualify_constructor_args(Args0, Args, ContainingTypeCtor,
        FunctionSymbolName, 0, !Info, !Specs),
    Ctor = ctor(ExistQVars, Constraints, FunctionSymbolSymName, Args,
        Arity, Context).

:- pred qualify_constructor_args(
    list(constructor_arg)::in, list(constructor_arg)::out,
    type_ctor::in, string::in, int::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor_args([], [], _, _, _, !Info, !Specs).
qualify_constructor_args([Arg0 | Args0], [Arg | Args],
        ContainingTypeCtor, FunctionSymbol, LastArgNum,!Info, !Specs) :-
    CurArgNum = LastArgNum + 1,
    qualify_constructor_arg(Arg0, Arg,
        ContainingTypeCtor, FunctionSymbol, CurArgNum, !Info, !Specs),
    qualify_constructor_args(Args0, Args,
        ContainingTypeCtor, FunctionSymbol, CurArgNum, !Info, !Specs).

:- pred qualify_constructor_arg(constructor_arg::in, constructor_arg::out,
    type_ctor::in, string::in, int::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor_arg(Arg0, Arg, ContainingTypeCtor, FunctionSymbol, ArgNum,
        !Info, !Specs) :-
    Arg0 = ctor_arg(MaybeFieldName, Type0, Width, Context),
    ErrorContext = mqec_constructor_arg(Context, ContainingTypeCtor,
        FunctionSymbol, ArgNum, MaybeFieldName),
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
    Arg = ctor_arg(MaybeFieldName, Type, Width, Context).

    % Qualify the inst parameters of an inst definition.
    %
:- pred qualify_inst_defn(inst_defn::in, inst_defn::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_defn(InstDefn0, InstDefn, ErrorContext, !Info, !Specs) :-
    (
        InstDefn0 = eqv_inst(Inst0),
        qualify_inst(Inst0, Inst, ErrorContext, !Info, !Specs),
        InstDefn = eqv_inst(Inst)
    ;
        InstDefn0 = abstract_inst,
        InstDefn = abstract_inst
    ).

    % Qualify the mode parameter of an equivalence mode definition.
    %
:- pred qualify_mode_defn(mode_defn::in, mode_defn::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode_defn(eqv_mode(Mode0), eqv_mode(Mode), ErrorContext,
        !Info, !Specs) :-
    qualify_mode(Mode0, Mode, ErrorContext, !Info, !Specs).

    % Qualify a list of items of the form Type::Mode, as in a
    % predicate declaration.
    %
:- pred qualify_types_and_modes(
    list(type_and_mode)::in, list(type_and_mode)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_types_and_modes([], [], _ErrorContext, !Info, !Specs).
qualify_types_and_modes([TypeAndMode0 | TypesAndModes0],
        [TypeAndMode | TypesAndModes], ErrorContext, !Info, !Specs) :-
    qualify_type_and_mode(TypeAndMode0, TypeAndMode, ErrorContext,
        !Info, !Specs),
    qualify_types_and_modes(TypesAndModes0, TypesAndModes, ErrorContext,
        !Info, !Specs).

:- pred qualify_type_and_mode(type_and_mode::in, type_and_mode::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_and_mode(TypeAndMode0, TypeAndMode, ErrorContext,
        !Info, !Specs) :-
    (
        TypeAndMode0 = type_only(Type0),
        qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode0 = type_and_mode(Type0, Mode0),
        qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
        qualify_mode(Mode0, Mode, ErrorContext, !Info, !Specs),
        TypeAndMode = type_and_mode(Type, Mode)
    ).

:- pred qualify_mode_list(list(mer_mode)::in, list(mer_mode)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode_list([], [], _ErrorContext, !Info, !Specs).
qualify_mode_list([Mode0 | Modes0], [Mode | Modes], ErrorContext,
        !Info, !Specs) :-
    qualify_mode(Mode0, Mode, ErrorContext, !Info, !Specs),
    qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs).

:- pred qualify_mode(mer_mode::in, mer_mode::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode(Mode0, Mode, ErrorContext, !Info, !Specs) :-
    (
        Mode0 = (InstA0 -> InstB0),
        qualify_inst(InstA0, InstA, ErrorContext, !Info, !Specs),
        qualify_inst(InstB0, InstB, ErrorContext, !Info, !Specs),
        Mode = (InstA -> InstB)
    ;
        Mode0 = user_defined_mode(SymName0, Insts0),
        qualify_inst_list(Insts0, Insts, ErrorContext, !Info, !Specs),
        list.length(Insts, Arity),
        mq_info_get_modes(!.Info, Modes),
        find_unique_match_symname(mq_id(SymName0, Arity), SymName,
            Modes, mode_id, ErrorContext, !Info, !Specs),
        Mode = user_defined_mode(SymName, Insts)
    ).

:- pred qualify_inst_list(list(mer_inst)::in, list(mer_inst)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_list([], [], _ErrorContext, !Info, !Specs).
qualify_inst_list([Inst0 | Insts0], [Inst | Insts], ErrorContext,
        !Info, !Specs) :-
    qualify_inst(Inst0, Inst, ErrorContext, !Info, !Specs),
    qualify_inst_list(Insts0, Insts, ErrorContext, !Info, !Specs).

    % Qualify a single inst.
    %
:- pred qualify_inst(mer_inst::in, mer_inst::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst(Inst0, Inst, ErrorContext, !Info, !Specs) :-
    (
        Inst0 = any(Uniq, HOInstInfo0),
        qualify_ho_inst_info(HOInstInfo0, HOInstInfo, ErrorContext,
            !Info, !Specs),
        Inst = any(Uniq, HOInstInfo)
    ;
        ( Inst0 = free
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ),
        Inst = Inst0
    ;
        Inst0 = free(_),
        unexpected($module, $pred, "compiler generated inst not expected")
    ;
        Inst0 = bound(Uniq, InstResults0, BoundInsts0),
        (
            ( InstResults0 = inst_test_results_fgtc
            ; InstResults0 = inst_test_no_results
            )
        ;
            InstResults0 = inst_test_results(_, _, _, _, _, _),
            unexpected($module, $pred, "compiler generated inst not expected")
        ),
        % XXX We could pass a more specific error context.
        qualify_bound_insts(BoundInsts0, BoundInsts, ErrorContext,
            !Info, !Specs),
        Inst = bound(Uniq, InstResults0, BoundInsts)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        % XXX We could pass a more specific error context.
        qualify_ho_inst_info(HOInstInfo0, HOInstInfo, ErrorContext,
            !Info, !Specs),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        % XXX We could pass a more specific error context.
        qualify_inst(SubInst0, SubInst, ErrorContext, !Info, !Specs),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = defined_inst(InstName0),
        % XXX We could pass a more specific error context.
        qualify_inst_name(InstName0, InstName, ErrorContext, !Info, !Specs),
        Inst = defined_inst(InstName)
    ;
        Inst0 = abstract_inst(Name, Args0),
        % XXX We could pass a more specific error context.
        qualify_inst_list(Args0, Args, ErrorContext, !Info, !Specs),
        Inst = abstract_inst(Name, Args)
    ).

:- pred qualify_ho_inst_info(ho_inst_info::in, ho_inst_info::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_ho_inst_info(HOInstInfo0, HOInstInfo, ErrorContext, !Info, !Specs) :-
    (
        HOInstInfo0 = higher_order(pred_inst_info(PredOrFunc, Modes0,
            MaybeArgRegs, Detism)),
        qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
        HOInstInfo = higher_order(pred_inst_info(PredOrFunc, Modes,
            MaybeArgRegs, Detism))
    ;
        HOInstInfo0 = none,
        HOInstInfo = none
    ).

    % Find the unique inst_id that matches this inst, and qualify
    % the argument insts.
    %
:- pred qualify_inst_name(inst_name::in, inst_name::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_name(InstName0, InstName, ErrorContext, !Info, !Specs) :-
    (
        InstName0 = user_inst(SymName0, Insts0),
        qualify_inst_list(Insts0, Insts, ErrorContext, !Info, !Specs),
        ( if
            % Check for a variable inst constructor.
            SymName0 = unqualified("")
        then
            report_invalid_user_inst(SymName0, Insts, ErrorContext, !Specs),
            mq_info_set_error_flag(inst_id, !Info),
            SymName = SymName0
        else
            list.length(Insts0, Arity),
            mq_info_get_insts(!.Info, InstIds),
            find_unique_match_symname(mq_id(SymName0, Arity), SymName,
                InstIds, inst_id, ErrorContext, !Info, !Specs)
        ),
        InstName = user_inst(SymName, Insts)
    ;
        ( InstName0 = unify_inst(_, _, _, _)
        ; InstName0 = merge_inst(_, _)
        ; InstName0 = ground_inst(_, _, _, _)
        ; InstName0 = any_inst(_, _, _, _)
        ; InstName0 = shared_inst(_)
        ; InstName0 = mostly_uniq_inst(_)
        ; InstName0 = typed_ground(_, _)
        ; InstName0 = typed_inst(_, _)
        ),
        unexpected($module, $pred, "unexpected compiler generated inst_name")
    ).

:- pred qualify_bound_insts(list(bound_inst)::in, list(bound_inst)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_bound_insts([], [], _ErrorContext, !Info, !Specs).
qualify_bound_insts([BoundInst0 | BoundInsts0], [BoundInst | BoundInsts],
        ErrorContext, !Info, !Specs) :-
    qualify_bound_inst(BoundInst0, BoundInst, ErrorContext, !Info, !Specs),
    qualify_bound_insts(BoundInsts0, BoundInsts, ErrorContext, !Info, !Specs).

    % Qualify an inst of the form bound(functor(...)).
    %
:- pred qualify_bound_inst(bound_inst::in, bound_inst::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_bound_inst(BoundInst0, BoundInst, ErrorContext, !Info, !Specs) :-
    BoundInst0 = bound_functor(ConsId, Insts0),
    (
        ConsId = cons(Name, Arity, _),
        Id = item_name(Name, Arity),
        update_recompilation_info(
            recompilation.record_used_item(functor_item, Id, Id), !Info)
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
        ; ConsId = int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ),
    qualify_inst_list(Insts0, Insts, ErrorContext, !Info, !Specs),
    BoundInst = bound_functor(ConsId, Insts).

:- pred qualify_type_list(list(mer_type)::in, list(mer_type)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_list([], [], _ErrorContext, !Info, !Specs).
qualify_type_list([Type0 | Types0], [Type | Types], ErrorContext,
        !Info, !Specs) :-
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
    qualify_type_list(Types0, Types, ErrorContext, !Info, !Specs).

:- pred qualify_maybe_type(maybe(mer_type)::in, maybe(mer_type)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_maybe_type(no, no, _ErrorContext, !Info, !Specs).
qualify_maybe_type(yes(Type0), yes(Type), ErrorContext, !Info, !Specs) :-
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs).

    % Qualify a type and its argument types.
    %
:- pred qualify_type(mer_type::in, mer_type::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type(Type0, Type, ErrorContext, !Info, !Specs) :-
    (
        Type0 = type_variable(_Var, _Kind),
        Type = Type0
    ;
        Type0 = defined_type(SymName0, Args0, Kind),
        Arity = list.length(Args0),
        TypeCtorId0 = mq_id(SymName0, Arity),
        mq_info_get_types(!.Info, Types),
        find_unique_match_symname(TypeCtorId0, SymName, Types, type_id,
            ErrorContext, !Info, !Specs),
        % XXX We could pass a more specific error context.
        qualify_type_list(Args0, Args, ErrorContext, !Info, !Specs),
        Type = defined_type(SymName, Args, Kind)
    ;
        Type0 = builtin_type(BuiltinType),
        % The types `int', `float', and `string' are builtin types,
        % defined by the compiler, but arguably they ought to be defined
        % in int.m, float.m, and string.m, and so if someone uses the type
        % `int' in the interface, then we don't want to warn about
        % `import_module int' in the interface. We don't do the same for
        % `character', since the corresponding library module `char'
        % will be flagged as used in the interface if the type `char' is used.
        (
            BuiltinType = builtin_type_int,
            mq_info_set_module_used(unqualified("int"), !Info)
        ;
            BuiltinType = builtin_type_float,
            mq_info_set_module_used(unqualified("float"), !Info)
        ;
            BuiltinType = builtin_type_string,
            mq_info_set_module_used(unqualified("string"), !Info)
        ;
            BuiltinType = builtin_type_char
        ),
        Type = Type0
    ;
        Type0 = higher_order_type(Args0, MaybeRet0, Purity, EvalMethod),
        % XXX We could pass a more specific error context.
        qualify_type_list(Args0, Args, ErrorContext, !Info, !Specs),
        % XXX We could pass a more specific error context.
        qualify_maybe_type(MaybeRet0, MaybeRet, ErrorContext, !Info, !Specs),
        Type = higher_order_type(Args, MaybeRet, Purity, EvalMethod)
    ;
        Type0 = tuple_type(Args0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type_list(Args0, Args, ErrorContext, !Info, !Specs),
        Type = tuple_type(Args, Kind)
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type_list(Args0, Args, ErrorContext, !Info, !Specs),
        Type = apply_n_type(Var, Args, Kind)
    ;
        Type0 = kinded_type(SubType0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type(SubType0, SubType, ErrorContext, !Info, !Specs),
        Type = kinded_type(SubType, Kind)
    ).

:- pred qualify_type_ctor(type_ctor::in, type_ctor::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_ctor(TypeCtor0, TypeCtor, ErrorContext, !Info, !Specs) :-
    TypeCtor0 = type_ctor(SymName0, Arity),
    ( if is_builtin_atomic_type(TypeCtor0) then
        SymName = SymName0
    else
        TypeCtorId0 = mq_id(SymName0, Arity),
        mq_info_get_types(!.Info, Types),
        % XXX We could pass a more specific error context.
        find_unique_match_symname(TypeCtorId0, SymName, Types, type_id,
            ErrorContext, !Info, !Specs)
    ),
    TypeCtor = type_ctor(SymName, Arity).

:- pred qualify_pragma(pragma_type::in, pragma_type::out,
    prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma(Pragma0, Pragma, Context, !Info, !Specs) :-
    (
        ( Pragma0 = pragma_foreign_decl(_)
        ; Pragma0 = pragma_foreign_code(_)
        ; Pragma0 = pragma_foreign_import_module(_)
        ; Pragma0 = pragma_external_proc(_)
          % The predicate name in the pragma_external_proc is constructed
          % already qualified.
        ; Pragma0 = pragma_inline(_)
        ; Pragma0 = pragma_no_inline(_)
        ; Pragma0 = pragma_obsolete(_)
        ; Pragma0 = pragma_no_detism_warning(_)
        ; Pragma0 = pragma_unused_args(_)
        ; Pragma0 = pragma_exceptions(_)
        ; Pragma0 = pragma_trailing_info(_)
        ; Pragma0 = pragma_mm_tabling_info(_)
        ; Pragma0 = pragma_fact_table(_)
        ; Pragma0 = pragma_promise_pure(_)
        ; Pragma0 = pragma_promise_semipure(_)
        ; Pragma0 = pragma_promise_eqv_clauses(_)
        ; Pragma0 = pragma_terminates(_)
        ; Pragma0 = pragma_does_not_terminate(_)
        ; Pragma0 = pragma_check_termination(_)
        ; Pragma0 = pragma_mode_check_clauses(_)
        ; Pragma0 = pragma_require_feature_set(_)
        ),
        Pragma = Pragma0
    ;
        Pragma0 = pragma_reserve_tag(_TypeCtor0),
        % XXX We should be module qualifying TypeCtor0 here,
        % not in add_pragma.m. However, the code in add_pragma.m
        % does generate better error messages than qualify_type_ctor does;
        % this implies we should fix qualify_type_ctor.
        Pragma = Pragma0
    ;
        Pragma0 = pragma_foreign_export_enum(FEEInfo0),
        FEEInfo0 = pragma_info_foreign_export_enum(Lang, TypeCtor0,
            Attributes, Overrides),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_type_ctor(TypeCtor0, TypeCtor, ErrorContext, !Info, !Specs),
        FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
            Attributes, Overrides),
        Pragma = pragma_foreign_export_enum(FEEInfo)
    ;
        Pragma0 = pragma_foreign_enum(FEInfo0),
        FEInfo0 = pragma_info_foreign_enum(Lang, TypeCtor0, Values),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_type_ctor(TypeCtor0, TypeCtor, ErrorContext, !Info, !Specs),
        FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, Values),
        Pragma = pragma_foreign_enum(FEInfo)
    ;
        Pragma0 = pragma_foreign_proc(FPInfo0),
        FPInfo0 = pragma_info_foreign_proc(Attrs0, Name, PredOrFunc,
            Vars0, Varset, InstVarset, Impl),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_pragma_vars(Vars0, Vars, ErrorContext, !Info, !Specs),
        UserSharing0 = get_user_annotated_sharing(Attrs0),
        qualify_user_sharing(UserSharing0, UserSharing, ErrorContext,
            !Info, !Specs),
        set_user_annotated_sharing(UserSharing, Attrs0, Attrs),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, PredOrFunc,
            Vars, Varset, InstVarset, Impl),
        Pragma = pragma_foreign_proc(FPInfo)
    ;
        Pragma0 = pragma_oisu(OISUInfo0),
        OISUInfo0 = pragma_info_oisu(TypeCtor0, CreatorPreds,
            MutatorPreds, DestructorPreds),
        % XXX Preds
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_type_ctor(TypeCtor0, TypeCtor, ErrorContext, !Info, !Specs),
        OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds,
            MutatorPreds, DestructorPreds),
        Pragma = pragma_oisu(OISUInfo)
    ;
        Pragma0 = pragma_tabled(TabledInfo0),
        TabledInfo0 = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes0, Attrs),
        (
            MModes0 = yes(Modes0),
            ErrorContext = mqec_pragma(Context, Pragma0),
            qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
            MModes = yes(Modes)
        ;
            MModes0 = no,
            MModes = no
        ),
        TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes, Attrs),
        Pragma = pragma_tabled(TabledInfo)
    ;
        Pragma0 = pragma_foreign_proc_export(FPEInfo0),
        FPEInfo0 = pragma_info_foreign_proc_export(Lang, PredNameModesPF0,
            CFunc),
        PredNameModesPF0 = pred_name_modes_pf(Name, Modes0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        FPEInfo = pragma_info_foreign_proc_export(Lang, PredNameModesPF,
            CFunc),
        Pragma = pragma_foreign_proc_export(FPEInfo)
    ;
        Pragma0 = pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(PredName, SpecializedPredName,
            Arity, PredOrFunc, MaybeModes0, Subst0, TVarSet, Items),
        ErrorContext = mqec_pragma(Context, Pragma0),
        (
            MaybeModes0 = yes(Modes0),
            qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
            MaybeModes = yes(Modes)
        ;
            MaybeModes0 = no,
            MaybeModes = no
        ),
        qualify_type_spec_subst(Subst0, Subst, ErrorContext, !Info, !Specs),
        TypeSpecInfo = pragma_info_type_spec(PredName, SpecializedPredName,
            Arity, PredOrFunc, MaybeModes, Subst, TVarSet, Items),
        Pragma = pragma_type_spec(TypeSpecInfo)
    ;
        Pragma0 = pragma_termination_info(TermInfo0),
        TermInfo0 = pragma_info_termination_info(PredNameModesPF0, Args, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(ModeList0, ModeList, ErrorContext, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        TermInfo = pragma_info_termination_info(PredNameModesPF, Args, Term),
        Pragma = pragma_termination_info(TermInfo)
    ;
        Pragma0 = pragma_structure_sharing(SharingInfo0),
        SharingInfo0 = pragma_info_structure_sharing(PredNameModesPF0,
            Vars, Types, Sharing),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(ModeList0, ModeList, ErrorContext, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            Vars, Types, Sharing),
        Pragma = pragma_structure_sharing(SharingInfo)
    ;
        Pragma0 = pragma_structure_reuse(ReuseInfo0),
        ReuseInfo0 = pragma_info_structure_reuse(PredNameModesPF0,
            Vars, Types, ReuseTuples),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(ModeList0, ModeList, ErrorContext, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            Vars, Types, ReuseTuples),
        Pragma = pragma_structure_reuse(ReuseInfo)
    ;
        Pragma0 = pragma_termination2_info(Term2Info0),
        Term2Info0 = pragma_info_termination2_info(PredNameModesPF0,
            SuccessArgs, FailureArgs, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(ModeList0, ModeList, ErrorContext, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgs, FailureArgs, Term),
        Pragma = pragma_termination2_info(Term2Info)
    ).

:- pred qualify_pragma_vars(list(pragma_var)::in, list(pragma_var)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma_vars([], [], _ErrorContext, !Info, !Specs).
qualify_pragma_vars([PragmaVar0 | PragmaVars0], [PragmaVar | PragmaVars],
        ErrorContext, !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_pragma_var(PragmaVar0, PragmaVar, ErrorContext, !Info, !Specs),
    qualify_pragma_vars(PragmaVars0, PragmaVars, ErrorContext, !Info, !Specs).

:- pred qualify_pragma_var(pragma_var::in, pragma_var::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma_var(PragmaVar0, PragmaVar, ErrorContext, !Info, !Specs) :-
    PragmaVar0 = pragma_var(Var, Name, Mode0, Box),
    % XXX We could pass a more specific error context.
    qualify_mode(Mode0, Mode, ErrorContext, !Info, !Specs),
    PragmaVar = pragma_var(Var, Name, Mode, Box).

:- pred qualify_type_spec_subst(
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_spec_subst([], [], _ErrorContext, !Info, !Specs).
qualify_type_spec_subst([Var - Type0 |  Subst0], [Var - Type | Subst],
        ErrorContext, !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_type(Type0, Type, ErrorContext, !Info, !Specs),
    qualify_type_spec_subst(Subst0, Subst, ErrorContext, !Info, !Specs).

:- pred qualify_prog_constraints(
    prog_constraints::in, prog_constraints::out,
    mq_constraint_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraints(Constraints0, Constraints, ConstraintErrorContext,
        !Info, !Specs) :-
    Constraints0 = constraints(UnivCs0, ExistCs0),
    % XXX We could pass a more specific error context.
    qualify_prog_constraint_list(UnivCs0, UnivCs, ConstraintErrorContext,
        !Info, !Specs),
    qualify_prog_constraint_list(ExistCs0, ExistCs, ConstraintErrorContext,
        !Info, !Specs),
    Constraints = constraints(UnivCs, ExistCs).

:- pred qualify_prog_constraint_list(
    list(prog_constraint)::in, list(prog_constraint)::out,
    mq_constraint_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint_list([], [], _ConstraintErrorContext, !Info, !Specs).
qualify_prog_constraint_list([Constraint0 | Constraints0],
        [Constraint | Constraints], ConstraintErrorContext, !Info, !Specs) :-
    qualify_prog_constraint(Constraint0, Constraint,
        ConstraintErrorContext, !Info, !Specs),
    qualify_prog_constraint_list(Constraints0, Constraints,
        ConstraintErrorContext, !Info, !Specs).

:- pred qualify_prog_constraint(prog_constraint::in, prog_constraint::out,
    mq_constraint_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint(Constraint0, Constraint, ContainingErrorContext,
        !Info, !Specs) :-
    Constraint0 = constraint(ClassName0, Types0),
    list.length(Types0, Arity),
    OutsideContext = mqec_typeclass_constraint_name(ContainingErrorContext),
    qualify_class_name(mq_id(ClassName0, Arity), mq_id(ClassName, _),
        OutsideContext, !Info, !Specs),
    ErrorContext = mqec_typeclass_constraint(ClassName0, Arity,
        ContainingErrorContext),
    qualify_type_list(Types0, Types, ErrorContext, !Info, !Specs),
    Constraint = constraint(ClassName, Types).

:- pred qualify_class_name(mq_id::in, mq_id::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_name(Class0, Class, ErrorContext, !Info, !Specs) :-
    mq_info_get_classes(!.Info, ClassIdSet),
    find_unique_match(Class0, Class, ClassIdSet, class_id, ErrorContext,
        !Info, !Specs).

:- pred qualify_class_methods(list(class_method)::in, list(class_method)::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_methods([], [], _ErrorContext, !Info, !Specs).
qualify_class_methods([Method0 | Methods0], [Method | Methods], ErrorContext,
        !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_class_method(Method0, Method, ErrorContext, !Info, !Specs),
    qualify_class_methods(Methods0, Methods, ErrorContext, !Info, !Specs).

:- pred qualify_class_method(class_method::in, class_method::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_method(Method0, Method, ErrorContext, !Info, !Specs) :-
    % There is no need to qualify the method name, since that is done
    % when the item is parsed.
    (
        Method0 = method_pred_or_func(Name, PredOrFunc, TypesAndModes0,
            MaybeWithType0, MaybeWithInst0, MaybeDetism,
            TypeVarset, InstVarset, ExistQVars,
            Purity, Constraints0, Context),
        % XXX We could pass a more specific error context.
        qualify_types_and_modes(TypesAndModes0, TypesAndModes, ErrorContext,
            !Info, !Specs),
        ConstraintErrorContext = mqcec_class_method(Context, PredOrFunc,
            unqualify_name(Name)),
        qualify_prog_constraints(Constraints0, Constraints,
            ConstraintErrorContext, !Info, !Specs),
        (
            MaybeWithType0 = yes(WithType0),
            % XXX We could pass a more specific error context.
            qualify_type(WithType0, WithType, ErrorContext, !Info, !Specs),
            MaybeWithType = yes(WithType)
        ;
            MaybeWithType0 = no,
            MaybeWithType = no
        ),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(WithInst0, WithInst, ErrorContext, !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        Method = method_pred_or_func(Name, PredOrFunc, TypesAndModes,
            MaybeWithType, MaybeWithInst, MaybeDetism,
            TypeVarset, InstVarset, ExistQVars,
            Purity, Constraints, Context)
    ;
        Method0 = method_pred_or_func_mode(PredOrFunc, Name, Modes0,
            MaybeWithInst0, MaybeDetism, Varset, Context),
        qualify_mode_list(Modes0, Modes, ErrorContext, !Info, !Specs),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(WithInst0, WithInst, ErrorContext, !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        Method = method_pred_or_func_mode(PredOrFunc, Name, Modes,
            MaybeWithInst, MaybeDetism, Varset, Context)
    ).

:- pred qualify_instance_body(sym_name::in,
    instance_body::in, instance_body::out) is det.

qualify_instance_body(ClassName, InstanceBody0, InstanceBody) :-
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract
    ;
        InstanceBody0 = instance_body_concrete(Methods0),
        (
            ClassName = unqualified(_),
            Methods = Methods0
        ;
            ClassName = qualified(_, _),
            sym_name_get_module_name_default(ClassName, unqualified(""),
                Module),
            % XXX InstanceProcDef may contain a list of clauses.
            % Why aren't those clauses module qualified?
            Qualify = (pred(InstanceMethod0::in, InstanceMethod::out) is det :-
                InstanceMethod0 = instance_method(PredOrFunc, Method0,
                    InstanceProcDef, Arity, DeclContext),
                add_module_qualifier(Module, Method0, Method),
                InstanceMethod = instance_method(PredOrFunc, Method,
                    InstanceProcDef, Arity, DeclContext)
            ),
            list.map(Qualify, Methods0, Methods)
        ),
        InstanceBody = instance_body_concrete(Methods)
    ).

:- pred add_module_qualifier(sym_name::in, sym_name::in, sym_name::out) is det.

add_module_qualifier(DefaultModule, SymName0, SymName) :-
    (
        SymName0 = unqualified(Name),
        SymName = qualified(DefaultModule, Name)
    ;
        SymName0 = qualified(SymModule, SubSymName),
        ( if partial_sym_name_matches_full(SymModule, DefaultModule) then
            SymName = qualified(DefaultModule, SubSymName)
        else
            % This case is an error. The user must have written something like
            %   :- instance foo.bar(some_type) where [
            %       pred(baz.p/1) is q
            %   ].
            % where the module qualifier on the pred or func in the instance
            % (baz) does not match the qualifier for the class name (foo).
            %
            % We don't report the error here, we just leave the original
            % module qualifier intact so that the error can be reported
            % later on.
            SymName = SymName0
        )
    ).

    % Find the unique match in the current name space for a given mq_id
    % from a list of ids. If none exists, either because no match
    % was found or multiple matches were found, report an error.
    % This predicate assumes that type_ids, inst_ids, mode_ids and
    % class_ids have the same representation.
    %
:- pred find_unique_match(mq_id::in, mq_id::out, id_set::in, id_type::in,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

find_unique_match(Id0, Id, Ids, TypeOfId, ErrorContext, !Info, !Specs) :-
    % Find all IDs which match the current id.
    Id0 = mq_id(SymName0, Arity),
    mq_info_get_modules(!.Info, Modules),
    id_set_search_sym_arity(Ids, Modules, SymName0, Arity, MatchingModules),

    ( if mq_info_get_import_status(!.Info, mq_status_exported) then
        % Items in the interface may only refer to modules
        % imported in the interface.
        mq_info_get_interface_visible_modules(!.Info, InterfaceImports),
        set.intersect(InterfaceImports, MatchingModules,
            ApplicableMatchingModules)
    else
        ApplicableMatchingModules = MatchingModules
    ),

    set.to_sorted_list(ApplicableMatchingModules,
        ApplicableMatchingModulesList),
    (
        ApplicableMatchingModulesList = [],
        % No matches for this id.
        Id = Id0,
        mq_info_get_report_error_flag(!.Info, ReportErrors),
        (
            ReportErrors = yes,
            id_set_search_sym(Ids, Modules, SymName0, PossibleArities),
            report_undefined_mq_id(!.Info, ErrorContext, Id0, TypeOfId,
                MatchingModules, PossibleArities, !Specs),
            mq_info_set_error_flag(TypeOfId, !Info)
        ;
            ReportErrors = no
        )
    ;
        ApplicableMatchingModulesList = [Module],
        % A unique match for this ID.
        IdName = unqualify_name(SymName0),
        Id = mq_id(qualified(Module, IdName), Arity),
        mq_info_set_module_used(Module, !Info),
        ItemType = convert_simple_item_type(TypeOfId),
        ItemName0 = item_name(SymName0, Arity),
        ItemName = item_name(qualified(Module, IdName), Arity),
        update_recompilation_info(
            recompilation.record_used_item(ItemType, ItemName0, ItemName),
            !Info)
    ;
        ApplicableMatchingModulesList = [_, _ | _],
        % There are multiple matches.
        Id = Id0,
        mq_info_get_report_error_flag(!.Info, ReportErrors),
        (
            ReportErrors = yes,
            report_ambiguous_match(ErrorContext, Id0, TypeOfId,
                ApplicableMatchingModulesList, !Specs),
            mq_info_set_error_flag(TypeOfId, !Info)
        ;
            ReportErrors = no
        )
    ).

    % A version of find_unique_match for use in the common (universal?)
    % situation where module qualification won't alter the arity of the
    % original mq_id.
    %
:- pred find_unique_match_symname(mq_id::in, sym_name::out,
    id_set::in, id_type::in, mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

find_unique_match_symname(Id0, SymName, Ids, TypeOfId, ErrorContext,
        !Info, !Specs) :-
    find_unique_match(Id0, Id, Ids, TypeOfId, ErrorContext, !Info, !Specs),
    Id = mq_id(SymName, _).

:- pred update_recompilation_info(
    pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
    mq_info::in, mq_info::out) is det.

update_recompilation_info(Pred, !Info) :-
    mq_info_get_recompilation_info(!.Info, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        Pred(RecompInfo0, RecompInfo),
        mq_info_set_recompilation_info(yes(RecompInfo), !Info)
    ;
        MaybeRecompInfo0 = no
    ).

:- func convert_simple_item_type(id_type) = item_type.

convert_simple_item_type(type_id) = type_abstract_item.
convert_simple_item_type(mode_id) = mode_item.
convert_simple_item_type(inst_id) = inst_item.
convert_simple_item_type(class_id) = typeclass_item.

:- pred qualify_user_sharing(
    user_annotated_sharing::in, user_annotated_sharing::out,
    mq_error_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_user_sharing(UserSharing0, UserSharing, ErrorContext, !Info, !Specs) :-
    (
        UserSharing0 = no_user_annotated_sharing,
        UserSharing = UserSharing0
    ;
        UserSharing0 = user_sharing(Sharing, MaybeTypes0),
        (
            MaybeTypes0 = yes(user_type_info(Types0, TVarset)),
            qualify_type_list(Types0, Types, ErrorContext, !Info, !Specs),
            MaybeTypes = yes(user_type_info(Types, TVarset)),
            UserSharing = user_sharing(Sharing, MaybeTypes)
        ;
            MaybeTypes0 = no,
            UserSharing = UserSharing0
        )
    ).

%-----------------------------------------------------------------------------%

:- type id_type
    --->    type_id
    ;       mode_id
    ;       inst_id
    ;       class_id.

:- type mq_id
    --->    mq_id(sym_name, int).

:- type mq_constraint_error_context
    --->    mqcec_class_defn(prog_context,
                % The name of the type class beging defined, and its arity.
                class_name,
                int
            )
    ;       mqcec_class_method(prog_context,
                % The identity of the class method the constraint is on:
                % whether it is predicate or function, and its name.
                % Its arity would be nice, but it is tricky to calculate
                % in the presence of with_type.
                pred_or_func,
                string
            )
    ;       mqcec_instance_defn(prog_context,
                % The name of the class the instance is for, and the
                % instance type vector.
                class_name,
                list(mer_type)
            )
    ;       mqcec_type_defn_constructor(prog_context,
                % The name of the type whose definition the constraint is in.
                type_ctor,

                % The function symbol the constraint is on.
                string
            )
    ;       mqcec_pred_decl(prog_context,
                % The identity of the entity the constraint is on:
                % whether it is predicate or function, and its name.
                % Its arity would be nice, but it is tricky to calculate
                % in the presence of with_type.
                pred_or_func,
                sym_name,
                int
            ).

:- type mq_error_context
    --->    mqec_type_defn(prog_context,
                % The name of the type constructor whose definition we are in.
                type_ctor
            )
    ;       mqec_constructor_arg(prog_context,
                % The name of the type constructor whose definition we are in.
                type_ctor,

                % The name of the function symbol.
                string,

                % The argument number of the type.
                int,

                % The name of the field, if it has one.
                maybe(ctor_field_name)
            )
    ;       mqec_typeclass_constraint_name(
                % The context the constraint is in.
                mq_constraint_error_context
            )
    ;       mqec_typeclass_constraint(
                % The name and arity of the typeclass the constraint is for.
                sym_name,
                int,

                % The context the constraint is in.
                mq_constraint_error_context
            )
    ;       mqec_inst(prog_context,
                % The name of the inst.
                mq_id
            )
    ;       mqec_mode(prog_context,
                % The name of the mode.
                mq_id
            )
    ;       mqec_pred_or_func(prog_context,
                % Whether it is a predicate or function declaration, ...
                pred_or_func,

                % and its name.
                mq_id
            )
    ;       mqec_pred_or_func_mode(prog_context,
                maybe(pred_or_func),
                mq_id
            )
    ;       mqec_pragma(prog_context,
                pragma_type
            )
    ;       mqec_lambda_expr(prog_context)
    ;       mqec_clause_mode_annotation(prog_context)
    ;       mqec_type_qual(prog_context)
    ;       mqec_class(prog_context,
                mq_id
            )
    ;       mqec_instance(prog_context,
                mq_id
            )
    ;       mqec_mutable(prog_context,
                string
            )
    ;       mqec_event_spec_attr(prog_context,
                % The event name.
                string,

                % The attribute name.
                string
            ).

:- func id_to_sym_name_and_arity(mq_id) = sym_name_and_arity.

id_to_sym_name_and_arity(mq_id(SymName, Arity)) = SymName / Arity.

    % Report an undefined type, inst or mode.
    %
:- pred report_undefined_mq_id(mq_info::in, mq_error_context::in,
    mq_id::in, id_type::in, set(module_name)::in, set(int)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_undefined_mq_id(Info, ErrorContext, Id, IdType,
        MatchingModulesSet, PossibleAritiesSet, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, ErrorContextPieces),
    id_type_to_string(IdType, IdStr),
    Pieces1 = [words("In")] ++ ErrorContextPieces ++
        [suffix(":"), nl, words("error: undefined"), fixed(IdStr),
        sym_name_and_arity(id_to_sym_name_and_arity(Id)),
        suffix("."), nl],
    ( if
        % If it is a qualified symbol, then check whether the specified module
        % has been imported.

        Id = mq_id(qualified(ModuleName, _), _Arity),
        mq_info_get_imported_modules(Info, ImportedModules),
        \+ set.member(ModuleName, ImportedModules),
        \+ ModuleName = Info ^ mqi_this_module
    then
        Pieces2 = [words("(The module"), sym_name(ModuleName),
            words("has not been imported.)"), nl]
    else
        set.to_sorted_list(MatchingModulesSet, MatchingModules),
        (
            MatchingModules = [],
            Pieces2 = []
        ;
            MatchingModules = [_ | MatchingModulesTail],
            (
                MatchingModulesTail = [],
                ModuleWord = "module",
                HasWord = "has"
            ;
                MatchingModulesTail = [_ | _],
                ModuleWord = "modules",
                HasWord = "have"
            ),
            MatchingSymNames = list.map(wrap_module_name, MatchingModules),
            Pieces2 = [words("(The"), fixed(ModuleWord)] ++
                component_list_to_pieces(MatchingSymNames) ++
                [fixed(HasWord),
                    words("not been imported in the interface.)"), nl]
        )
    ),
    set.to_sorted_list(PossibleAritiesSet, PossibleArities),
    ( if
        PossibleArities = [_ | _],
        Pieces2 = []
    then
        Id = mq_id(SymName, _),
        id_types_to_string(IdType, IdsStr),
        IsAre = choose_number(PossibleArities, "is a", "are"),
        KindKinds = choose_number(PossibleArities, IdStr, IdsStr),
        ArityArities = choose_number(PossibleArities, "arity", "arities"),
        list.map(string.int_to_string, PossibleArities, PossibleArityStrs),
        PossibleAritiesPieces = list_to_pieces(PossibleArityStrs),
        Pieces3 = [words("(There"), words(IsAre), words(KindKinds),
            words("named"), quote(unqualify_name(SymName)),
            words("with"), words(ArityArities)] ++
            PossibleAritiesPieces ++ [suffix(".)"), nl]
    else
        Pieces3 = []
    ),
    Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2 ++ Pieces3)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

    % Report an error where a type, inst, mode or typeclass had
    % multiple possible matches.
    %
:- pred report_ambiguous_match(mq_error_context::in, mq_id::in, id_type::in,
    list(module_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_ambiguous_match(ErrorContext, Id, IdType, Modules, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, ErrorContextPieces),
    id_type_to_string(IdType, IdStr),
    ModuleNames = list.map(wrap_module_name, Modules),
    MainPieces = [words("In")] ++ ErrorContextPieces ++
        [words("ambiguity error: multiple possible matches for"),
        fixed(IdStr), wrap_id(Id), suffix("."), nl,
        words("The possible matches are in modules")] ++ ModuleNames ++
        [suffix("."), nl],
    VerbosePieces = [words("An explicit module qualifier"),
        words("may be necessary."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred mq_constraint_error_context_to_pieces(mq_constraint_error_context::in,
    prog_context::out, string::out, list(format_component)::out) is det.

mq_constraint_error_context_to_pieces(ConstraintErrorContext,
        Context, Start, Pieces) :-
    (
        ConstraintErrorContext = mqcec_class_defn(Context, ClassName, Arity),
        Start = "in",
        Pieces = [words("definition of type class"),
            sym_name_and_arity(ClassName / Arity)]
    ;
        ConstraintErrorContext = mqcec_class_method(Context,
            PredOrFunc, MethodName),
        Start = "on",
        Pieces = [words("class method"),
            p_or_f(PredOrFunc), quote(MethodName)]
    ;
        ConstraintErrorContext = mqcec_instance_defn(Context,
            ClassName, ArgTypes),
        Start = "on",
        Pieces = [words("instance definition for"),
            sym_name_and_arity(ClassName / list.length(ArgTypes))]
    ;
        ConstraintErrorContext = mqcec_type_defn_constructor(Context,
            TypeCtor, FunctionSymbol),
        Start = "on",
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        Pieces = [words("function symbol"), quote(FunctionSymbol),
            words("for type constructor"),
            sym_name_and_arity(TypeCtorSymName / TypeCtorArity)]
    ;
        ConstraintErrorContext = mqcec_pred_decl(Context,
            PredOrFunc, SymName, OrigArity),
        Start = "on",
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        Pieces = [words("declaration of "),
            fixed(pred_or_func_to_full_str(PredOrFunc)),
            sym_name_and_arity(SymName / Arity)]
    ).

:- pred mq_error_context_to_pieces(mq_error_context::in,
    prog_context::out, list(format_component)::out) is det.

mq_error_context_to_pieces(ErrorContext, Context,Pieces) :-
    (
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        Pieces = [words("definition of type"), wrap_type_ctor(TypeCtor)]
    ;
        ErrorContext = mqec_constructor_arg(Context, ContainingTypeCtor,
            FunctionSymbol, ArgNum, MaybeCtorFieldName),
        (
            MaybeCtorFieldName = no,
            FieldNamePieces = []
        ;
            MaybeCtorFieldName = yes(CtorFieldName),
            CtorFieldName = ctor_field_name(FieldSymName, _FieldContext),
            FieldNamePieces = [words("(field name"),
                quote(unqualify_name(FieldSymName)), suffix(")")]
        ),
        Pieces = [words("the"), nth_fixed(ArgNum), words("argument of"),
            words("function symbol"), quote(FunctionSymbol)] ++
            FieldNamePieces ++
            [words("of the type"), wrap_type_ctor(ContainingTypeCtor)]
    ;
        ErrorContext = mqec_typeclass_constraint_name(ConstraintErrorContext),
        mq_constraint_error_context_to_pieces(ConstraintErrorContext,
            Context, _Start, Pieces)
    ;
        ErrorContext = mqec_typeclass_constraint(ClassName, Arity,
            ConstraintErrorContext),
        mq_constraint_error_context_to_pieces(ConstraintErrorContext,
            Context, Start, ConstraintErrorContextPieces),
        Pieces = [words("type class constraint for "),
            sym_name_and_arity(ClassName / Arity), words(Start) |
            ConstraintErrorContextPieces]
    ;
        ErrorContext = mqec_mode(Context, Id),
        Pieces = [words("definition of mode"), wrap_id(Id)]
    ;
        ErrorContext = mqec_inst(Context, Id),
        Pieces = [words("definition of inst"), wrap_id(Id)]
    ;
        ErrorContext = mqec_pred_or_func(Context, PredOrFunc, Id),
        Id = mq_id(SymName, OrigArity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        Pieces = [words("declaration of "),
            fixed(pred_or_func_to_full_str(PredOrFunc)),
            sym_name_and_arity(SymName / Arity)]
    ;
        ErrorContext = mqec_pred_or_func_mode(Context, MaybePredOrFunc, Id),
        Id = mq_id(SymName, OrigArity),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, OrigArity, Arity),
            Pieces = [words("mode declaration for"),
                fixed(pred_or_func_to_full_str(PredOrFunc)),
                sym_name_and_arity(SymName / Arity)]
        ;
            MaybePredOrFunc = no,
            Pieces = [words("mode declaration for"),
                sym_name_and_arity(SymName / OrigArity)]
        )
    ;
        ErrorContext = mqec_lambda_expr(Context),
        Pieces = [words("mode declaration for lambda expression")]
    ;
        ErrorContext = mqec_clause_mode_annotation(Context),
        Pieces = [words("clause mode annotation")]
    ;
        ErrorContext = mqec_pragma(Context, Pragma),
        (
            Pragma = pragma_foreign_decl(_),
            PragmaName = "foreign_decl"
        ;
            Pragma = pragma_foreign_code(_),
            PragmaName = "foreign_code"
        ;
            Pragma = pragma_foreign_proc(_),
            PragmaName = "foreign_proc"
        ;
            Pragma = pragma_foreign_import_module(_),
            PragmaName = "foreign_import_module"
        ;
            Pragma = pragma_foreign_proc_export(_),
            PragmaName = "foreign_proc_export"
        ;
            Pragma = pragma_foreign_export_enum(_),
            PragmaName = "foreign_export_enum"
        ;
            Pragma = pragma_foreign_enum(_),
            PragmaName = "foreign_enum"
        ;
            Pragma = pragma_external_proc(_),
            PragmaName = "external_proc"
        ;
            Pragma = pragma_type_spec(_),
            PragmaName = "type_spec"
        ;
            Pragma = pragma_inline(_),
            PragmaName = "inline"
        ;
            Pragma = pragma_no_inline(_),
            PragmaName = "no_inline"
        ;
            Pragma = pragma_unused_args(_),
            PragmaName = "unused_args"
        ;
            Pragma = pragma_exceptions(_),
            PragmaName = "exceptions"
        ;
            Pragma = pragma_trailing_info(_),
            PragmaName = "trailing_info"
        ;
            Pragma = pragma_mm_tabling_info(_),
            PragmaName = "mm_tabling_info"
        ;
            Pragma = pragma_obsolete(_),
            PragmaName = "obsolete"
        ;
            Pragma = pragma_no_detism_warning(_),
            PragmaName = "no_detism_warning"
        ;
            Pragma = pragma_tabled(_),
            PragmaName = "tabled"
        ;
            Pragma = pragma_fact_table(_),
            PragmaName = "fact_table"
        ;
            Pragma = pragma_reserve_tag(_),
            PragmaName = "reserve_tag"
        ;
            Pragma = pragma_oisu(_),
            PragmaName = "oisu"
        ;
            Pragma = pragma_promise_eqv_clauses(_),
            PragmaName = "promise_equivalent_clauses"
        ;
            Pragma = pragma_promise_pure(_),
            PragmaName = "promise_pure"
        ;
            Pragma = pragma_promise_semipure(_),
            PragmaName = "promise_semipure"
        ;
            Pragma = pragma_termination_info(_),
            PragmaName = "termination_info"
        ;
            Pragma = pragma_termination2_info(_),
            PragmaName = "termination2_info"
        ;
            Pragma = pragma_terminates(_),
            PragmaName = "terminates"
        ;
            Pragma = pragma_does_not_terminate(_),
            PragmaName = "does_not_terminate"
        ;
            Pragma = pragma_check_termination(_),
            PragmaName = "check_termination"
        ;
            Pragma = pragma_mode_check_clauses(_),
            PragmaName = "mode_check_clauses"
        ;
            Pragma = pragma_structure_sharing(_),
            PragmaName = "structure_sharing"
        ;
            Pragma = pragma_structure_reuse(_),
            PragmaName = "structure_reuse"
        ;
            Pragma = pragma_require_feature_set(_),
            PragmaName = "require_feature_set"
        ),
        Pieces = [words("pragma"), words(PragmaName)]
    ;
        ErrorContext = mqec_type_qual(Context),
        Pieces = [words("explicit type qualification")]
    ;
        ErrorContext = mqec_class(Context, Id),
        Pieces = [words("declaration of typeclass"), wrap_id(Id)]
    ;
        ErrorContext = mqec_instance(Context, Id),
        Pieces = [words("declaration of instance of typeclass"), wrap_id(Id)]
    ;
        ErrorContext = mqec_mutable(Context, Name),
        Pieces = [words("declaration for mutable "), quote(Name)]
    ;
        ErrorContext = mqec_event_spec_attr(Context, EventName, AttrName),
        Pieces = [words("attribute"), quote(AttrName),
            words("for"), quote(EventName)]
    ).

:- pred id_type_to_string(id_type::in, string::out) is det.

id_type_to_string(type_id, "type").
id_type_to_string(mode_id, "mode").
id_type_to_string(inst_id, "inst").
id_type_to_string(class_id, "typeclass").

:- pred id_types_to_string(id_type::in, string::out) is det.

id_types_to_string(type_id, "types").
id_types_to_string(mode_id, "modes").
id_types_to_string(inst_id, "insts").
id_types_to_string(class_id, "typeclasses").

    % Warn about modules imported in the interface when they do not need to be.
    %
:- pred maybe_warn_unused_interface_imports(module_name::in, prog_context::in,
    list(module_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_unused_interface_imports(ModuleName, Context, UnusedImports,
        !Specs) :-
    (
        UnusedImports = []
    ;
        UnusedImports = [_ | _],
        UnusedSymNames = list.map(wrap_module_name, UnusedImports),
        Pieces = [words("In module"), sym_name(ModuleName), suffix(":"), nl,
            words("warning:"),
            words(choose_number(UnusedImports, "module", "modules"))] ++
            component_list_to_pieces(UnusedSymNames) ++
            [words(choose_number(UnusedImports, "is", "are")),
            words("imported in the interface,"),
            words("but"), words(choose_number(UnusedImports, "is", "are")),
            words("not used in the interface.")],
        Msg = simple_msg(Context,
            [option_is_set(warn_interface_imports, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_interface_imports, yes,
            severity_warning, no),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- func wrap_module_name(module_name) = format_component.

wrap_module_name(SymName) = sym_name(SymName).

:- func wrap_type_ctor(type_ctor) = format_component.

wrap_type_ctor(type_ctor(SymName, Arity)) =
    sym_name_and_arity(SymName / Arity).

:- func wrap_id(mq_id) = format_component.

wrap_id(mq_id(SymName, Arity)) = sym_name_and_arity(SymName / Arity).

    % Output an error message about an ill-formed user_inst.
    %
:- pred report_invalid_user_inst(sym_name::in, list(mer_inst)::in,
    mq_error_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_invalid_user_inst(_SymName, _Insts, ErrorContext, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, ErrorContextPieces),
    Pieces = [words("In")] ++ ErrorContextPieces ++ [suffix(":"), nl,
        words("error: variable used as inst constructor."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

    % is_builtin_atomic_type(TypeCtor):
    %
    % Succeeds iff 'TypeCtor' is the type_ctor of a builtin atomic type.
    %
:- pred is_builtin_atomic_type(type_ctor::in) is semidet.

is_builtin_atomic_type(type_ctor(unqualified("int"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("float"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("string"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("character"), 0)).

%-----------------------------------------------------------------------------%
%
% Access and initialisation predicates.
%

:- pred init_mq_info(globals::in, module_name::in,
    list(item_block(MS1))::in,
    list(item_block(MS2))::in,
    list(item_block(MS3))::in,
    list(item_block(MS4))::in,
    bool::in, mq_info::out) is det.

init_mq_info(Globals, ModuleName, ItemBlocksA, ItemBlocksB, ItemBlocksC,
        ItemBlocksD, ReportErrors, Info) :-
    set.init(InterfaceModules0),
    set.init(InstanceModules),
    id_set_init(ModuleIdSet),
    id_set_init(TypeIdSet),
    id_set_init(ImplTypeIdSet),
    id_set_init(InstIdSet),
    id_set_init(ModeIdSet),
    id_set_init(ClassIdSet),
    get_implicit_dependencies_in_item_blocks(Globals, ItemBlocksA,
        ImportDepsA, UseDepsA),
    get_implicit_dependencies_in_item_blocks(Globals, ItemBlocksB,
        ImportDepsB, UseDepsB),
    get_implicit_dependencies_in_item_blocks(Globals, ItemBlocksC,
        ImportDepsC, UseDepsC),
    get_implicit_dependencies_in_item_blocks(Globals, ItemBlocksD,
        ImportDepsD, UseDepsD),
    ImportDeps = ImportDepsA ++ ImportDepsB ++ ImportDepsC ++ ImportDepsD,
    UseDeps = UseDepsA ++ UseDepsB ++ UseDepsC ++ UseDepsD,
    set.list_to_set(ImportDeps ++ UseDeps, ImportedModules),

    % Ancestor modules are visible without being explicitly imported.
    set.insert_list([ModuleName | get_ancestors(ModuleName)],
        ImportedModules, InterfaceVisibleModules),

    globals.lookup_bool_option(Globals, smart_recompilation,
        SmartRecompilation),
    (
        SmartRecompilation = no,
        MaybeRecompInfo = no
    ;
        SmartRecompilation = yes,
        MaybeRecompInfo = yes(init_recompilation_info(ModuleName))
    ),
    ExportedInstancesFlag = no,
    Info = mq_info(ImportedModules, InterfaceVisibleModules,
        ModuleIdSet, TypeIdSet, ImplTypeIdSet,
        InstIdSet, ModeIdSet, ClassIdSet,
        InterfaceModules0, InstanceModules, ExportedInstancesFlag,
        mq_status_local, 0, no, no, ReportErrors, ModuleName,
        may_be_unqualified, MaybeRecompInfo).

:- pred mq_info_get_imported_modules(mq_info::in, set(module_name)::out)
    is det.
:- pred mq_info_get_interface_visible_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_modules(mq_info::in, module_id_set::out) is det.
:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_impl_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_classes(mq_info::in, class_id_set::out) is det.
:- pred mq_info_get_unused_interface_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_imported_instance_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_exported_instances_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_import_status(mq_info::in, mq_import_status::out) is det.
% :- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
% :- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_report_error_flag(mq_info::in, bool::out) is det.

mq_info_get_imported_modules(Info, X) :-
    X = Info ^ mqi_imported_modules.
mq_info_get_interface_visible_modules(Info, X) :-
    X = Info ^ mqi_interface_visible_modules.
mq_info_get_modules(Info, X) :-
    X = Info ^ mqi_modules.
mq_info_get_types(Info, X) :-
    X = Info ^ mqi_types.
mq_info_get_impl_types(Info, X) :-
    X = Info ^ mqi_impl_types.
mq_info_get_insts(Info, X) :-
    X = Info ^ mqi_insts.
mq_info_get_modes(Info, X) :-
    X = Info ^ mqi_modes.
mq_info_get_classes(Info, X) :-
    X = Info ^ mqi_classes.
mq_info_get_unused_interface_modules(Info, X) :-
    X = Info ^ mqi_unused_interface_modules.
mq_info_get_imported_instance_modules(Info, X) :-
    X = Info ^ mqi_imported_instance_modules.
mq_info_get_exported_instances_flag(Info, X) :-
    X = Info ^ mqi_exported_instances_flag.
mq_info_get_import_status(Info, X) :-
    X = Info ^ mqi_import_status.
mq_info_get_type_error_flag(Info, X) :-
    X = Info ^ mqi_type_error_flag.
mq_info_get_mode_error_flag(Info, X) :-
    X = Info ^ mqi_mode_error_flag.
mq_info_get_report_error_flag(Info, X) :-
    X = Info ^ mqi_report_error_flag.
mq_info_get_need_qual_flag(Info, X) :-
    X = Info ^ mqi_need_qual_flag.
mq_info_get_recompilation_info(Info, X) :-
    X = Info ^ mqi_maybe_recompilation_info.

:- pred mq_info_set_imported_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_interface_visible_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_modules(module_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_types(type_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_impl_types(type_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_insts(inst_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_modes(mode_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_classes(class_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_unused_interface_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_imported_instance_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_exported_instances_flag(bool::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_import_status(mq_import_status::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_type_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_mode_error_flag(mq_info::in, mq_info::out) is det.

mq_info_set_imported_modules(X, !Info) :-
    !Info ^ mqi_imported_modules := X.
mq_info_set_interface_visible_modules(X, !Info) :-
    !Info ^ mqi_interface_visible_modules := X.
mq_info_set_modules(X, !Info) :-
    !Info ^ mqi_modules := X.
mq_info_set_types(X, !Info) :-
    !Info ^ mqi_types := X.
mq_info_set_impl_types(X, !Info) :-
    !Info ^ mqi_impl_types := X.
mq_info_set_insts(X, !Info) :-
    !Info ^ mqi_insts := X.
mq_info_set_modes(X, !Info) :-
    !Info ^ mqi_modes := X.
mq_info_set_classes(X, !Info) :-
    !Info ^ mqi_classes := X.
mq_info_set_unused_interface_modules(X, !Info) :-
    !Info ^ mqi_unused_interface_modules := X.
mq_info_set_imported_instance_modules(X, !Info) :-
    !Info ^ mqi_imported_instance_modules := X.
mq_info_set_exported_instances_flag(X, !Info) :-
    !Info ^ mqi_exported_instances_flag := X.
mq_info_set_import_status(X, !Info) :-
    !Info ^ mqi_import_status := X.
mq_info_set_type_error_flag(!Info) :-
    X = yes,
    !Info ^ mqi_type_error_flag := X.
mq_info_set_mode_error_flag(!Info) :-
    X = yes,
    !Info ^ mqi_mode_error_flag := X.
mq_info_set_need_qual_flag(X, !Info) :-
    !Info ^ mqi_need_qual_flag := X.
mq_info_set_recompilation_info(X, !Info) :-
    !Info ^ mqi_maybe_recompilation_info := X.

:- pred mq_info_set_error_flag(id_type::in, mq_info::in, mq_info::out) is det.

mq_info_set_error_flag(IdType, !Info) :-
    mq_info_set_error_flag_2(IdType, !Info).

:- pred mq_info_set_error_flag_2(id_type::in, mq_info::in, mq_info::out)
    is det.

mq_info_set_error_flag_2(type_id, !Info) :-
    mq_info_set_type_error_flag(!Info).
mq_info_set_error_flag_2(mode_id, !Info) :-
    mq_info_set_mode_error_flag(!Info).
mq_info_set_error_flag_2(inst_id, !Info) :-
    mq_info_set_mode_error_flag(!Info).
mq_info_set_error_flag_2(class_id, !Info) :-
    mq_info_set_type_error_flag(!Info).

    % If the current item is in the interface, remove its module name
    % from the list of modules not used in the interface (and if the
    % module name is itself module-qualified, recursively mark its
    % parent module as used).
    %
:- pred mq_info_set_module_used(module_name::in, mq_info::in, mq_info::out)
    is det.

mq_info_set_module_used(Module, !Info) :-
    ( if mq_info_get_import_status(!.Info, mq_status_exported) then
        mq_info_get_unused_interface_modules(!.Info, Modules0),
        set.delete(Module, Modules0, Modules),
        mq_info_set_unused_interface_modules(Modules, !Info),
        (
            Module = qualified(ParentModule, _),
            mq_info_set_module_used(ParentModule, !Info)
        ;
            Module = unqualified(_)
        )
    else
        true
    ).

%----------------------------------------------------------------------------%
%
% Define a type for representing sets of ids during module qualification,
% to allow efficient retrieval of all the modules which define an id
% with a certain name and arity.
%

:- type id_set == map(string, map(arity, symname_arity_modules)).

    % The first set of module_names can be used without module qualifiers;
    % items from the second set can only be used with module qualifiers.
    % Items from modules imported with a :- use_module declaration
    % and from `.opt' and `.trans_opt' files should go into the second set.
:- type symname_arity_modules
    --->    symname_arity_modules(
                mm_may_be_unqualified   :: set(module_name),
                mm_must_be_qualified    :: set(module_name)
            ).

:- type type_id_set == id_set.
:- type mode_id_set == id_set.
:- type inst_id_set == id_set.
:- type class_id_set == id_set.
    % Modules don't have an arity, but for simplicity we use the same
    % data structure here, assigning arity zero to all module names.
:- type module_id_set == id_set.

:- pred id_set_init(id_set::out) is det.

id_set_init(IdSet) :-
    map.init(IdSet).

    % Insert an mq_id into an id_set, aborting with an error if the
    % mq_id is not module qualified.
    %
:- pred id_set_insert(need_qualifier::in, mq_id::in, id_set::in, id_set::out)
    is det.

id_set_insert(NeedQualifier, MQId,!IdSet) :-
    MQId = mq_id(SymName, Arity),
    (
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified id")
    ;
        SymName = qualified(Module, Name),
        ( if map.search(!.IdSet, Name, SubMap0) then
            ( if map.search(SubMap0, Arity, SymNameArityModules0) then
                insert_into_symname_arity_modules(NeedQualifier, Module,
                    SymNameArityModules0, SymNameArityModules),
                map.det_update(Arity, SymNameArityModules, SubMap0, SubMap),
                map.det_update(Name, SubMap, !IdSet)
            else
                init_symname_arity_modules(NeedQualifier, Module,
                    SymNameArityModules),
                map.det_insert(Arity, SymNameArityModules, SubMap0, SubMap),
                map.det_update(Name, SubMap, !IdSet)
            )
        else
            init_symname_arity_modules(NeedQualifier, Module,
                SymNameArityModules),
            SubMap = map.singleton(Arity, SymNameArityModules),
            map.det_insert(Name, SubMap, !IdSet)
        )
    ).

:- pred init_symname_arity_modules(need_qualifier::in, module_name::in,
    symname_arity_modules::out) is det.

init_symname_arity_modules(NeedQualifier, Module, SymNameArityModules) :-
    (
        NeedQualifier = may_be_unqualified,
        ImportModules = set.make_singleton_set(Module),
        set.init(UseModules)
    ;
        NeedQualifier = must_be_qualified,
        set.init(ImportModules),
        UseModules = set.make_singleton_set(Module)
    ),
    SymNameArityModules = symname_arity_modules(ImportModules, UseModules).

:- pred insert_into_symname_arity_modules(need_qualifier::in, module_name::in,
    symname_arity_modules::in, symname_arity_modules::out) is det.

insert_into_symname_arity_modules(NeedQualifier, Module,
        SymNameArityModules0, SymNameArityModules) :-
    SymNameArityModules0 = symname_arity_modules(ImportModules0, UseModules0),
    (
        NeedQualifier = may_be_unqualified,
        set.insert(Module, ImportModules0, ImportModules),
        UseModules = UseModules0
    ;
        NeedQualifier = must_be_qualified,
        ImportModules = ImportModules0,
        set.insert(Module, UseModules0, UseModules)
    ),
    SymNameArityModules = symname_arity_modules(ImportModules, UseModules).

:- pred id_set_search_sym_arity(id_set::in, module_id_set::in,
    sym_name::in, int::in, set(module_name)::out) is det.

id_set_search_sym_arity(IdSet, ModuleIdSet, SymName, Arity, MatchingModules) :-
    UnqualName = unqualify_name(SymName),
    ( if
        map.search(IdSet, UnqualName, SubMap),
        map.search(SubMap, Arity, SymNameArityModules)
    then
        find_matches_in_symname_arity_modules(SymName, ModuleIdSet,
            SymNameArityModules, MatchingModules)
    else
        set.init(MatchingModules)
    ).

:- pred id_set_search_sym(id_set::in, module_id_set::in, sym_name::in,
    set(int)::out) is det.

id_set_search_sym(IdSet, ModuleIdSet, SymName, PossibleArities) :-
    UnqualName = unqualify_name(SymName),
    ( if
        map.search(IdSet, UnqualName, SubMap)
    then
        map.to_assoc_list(SubMap, SubMapPairs),
        find_matching_arities(SymName, ModuleIdSet, SubMapPairs,
            set.init, PossibleArities)
    else
        set.init(PossibleArities)
    ).

:- pred find_matching_arities(sym_name::in, module_id_set::in,
    assoc_list(int, symname_arity_modules)::in, set(int)::in, set(int)::out)
    is det.

find_matching_arities(_SymName, _ModuleIdSet, [], !PossibleArities).
find_matching_arities(SymName, ModuleIdSet, [Pair | Pairs],
        !PossibleArities) :-
    Pair = Arity - SymNameArityModules,
    find_matches_in_symname_arity_modules(SymName, ModuleIdSet,
        SymNameArityModules, MatchingModules),
    ( if set.is_empty(MatchingModules) then
        true
    else
        set.insert(Arity, !PossibleArities)
    ),
    find_matching_arities(SymName, ModuleIdSet, Pairs, !PossibleArities).

:- pred find_matches_in_symname_arity_modules(sym_name::in, module_id_set::in,
    symname_arity_modules::in, set(module_name)::out) is det.

find_matches_in_symname_arity_modules(SymName, ModuleIdSet,
        SymNameArityModules, MatchingModules) :-
    SymNameArityModules = symname_arity_modules(ImportModules, UseModules),
    (
        SymName = unqualified(_),
        MatchingModules = ImportModules
    ;
        SymName = qualified(Module, _),

        % Compute the set of modules that this module specifier
        % could possibly refer to.
        %
        % Do a recursive search to find nested modules that match
        % the specified module name.
        ModuleArity = 0,
        id_set_search_sym_arity(ModuleIdSet, ModuleIdSet, Module, ModuleArity,
            MatchingParentModules),
        UnqualModule = unqualify_name(Module),
        AppendModuleName = (pred(X::in, Y::out) is det :-
            Y = qualified(X, UnqualModule)
        ),
        set.map(AppendModuleName,
            MatchingParentModules, MatchingNestedModules),

        % Add the specified module name itself, in case it refers to
        % a top-level (unnested) module name, since top-level modules
        % don't get inserted into the module_id_set.
        set.insert(Module, MatchingNestedModules, AllMatchingModules),

        set.union(ImportModules, UseModules, DefiningModules),
        set.intersect(AllMatchingModules, DefiningModules, MatchingModules)
    ).

%-----------------------------------------------------------------------------%

get_partial_qualifiers(ModuleName, PartialQualInfo, PartialQualifiers) :-
    PartialQualInfo = partial_qualifier_info(ModuleIdSet),
    (
        ModuleName = unqualified(_),
        PartialQualifiers = []
    ;
        ModuleName = qualified(Parent, Child),
        get_partial_qualifiers_2(Parent, unqualified(Child),
            ModuleIdSet, [], PartialQualifiers)
    ).

:- pred get_partial_qualifiers_2(module_name::in, module_name::in,
    module_id_set::in, list(module_name)::in, list(module_name)::out)
    is det.

get_partial_qualifiers_2(ImplicitPart, ExplicitPart, ModuleIdSet,
        !Qualifiers) :-
    % If the ImplicitPart module was imported, rather than just being used,
    % then insert the ExplicitPart module into the list of valid partial
    % qualifiers.

    ( if
        parent_module_is_imported(ImplicitPart, ExplicitPart, ModuleIdSet)
    then
        !:Qualifiers = [ExplicitPart | !.Qualifiers]
    else
        true
    ),
    % Recursively try to add the other possible partial qualifiers.
    (
        ImplicitPart = qualified(Parent, Child),
        NextImplicitPart = Parent,
        NextExplicitPart = add_outermost_qualifier(Child, ExplicitPart),
        get_partial_qualifiers_2(NextImplicitPart, NextExplicitPart,
            ModuleIdSet, !Qualifiers)
    ;
        ImplicitPart = unqualified(_)
    ).

    % Check whether the parent module was imported, given the name of a
    % child (or grandchild, etc.) module occurring in that parent module.
    %
:- pred parent_module_is_imported(module_name::in, module_name::in,
    module_id_set::in) is semidet.

parent_module_is_imported(ParentModule, ChildModule, ModuleIdSet) :-
    % Find the module name at the start of the ChildModule;
    % this sub-module will be a direct sub-module of ParentModule
    DirectSubModuleName = get_first_module_name(ChildModule),

    % Check that the ParentModule was imported.
    % We do this by looking up the definitions for the direct sub-module
    % and checking that the one in ParentModule came from an
    % imported module.
    Arity = 0,
    map.search(ModuleIdSet, DirectSubModuleName, SubMap),
    map.search(SubMap, Arity, SymNameArityModules),
    SymNameArityModules = symname_arity_modules(ImportModules, _UseModules),
    set.member(ParentModule, ImportModules).

    % Given a module name, possibly module-qualified, return the name
    % of the first module in the qualifier list. For example, given
    % `foo.bar.baz', this returns `foo', and given just `baz',
    % it returns `baz'.
    %
:- func get_first_module_name(module_name) = string.

get_first_module_name(unqualified(ModuleName)) = ModuleName.
get_first_module_name(qualified(Parent, _)) = get_first_module_name(Parent).

%----------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.
%----------------------------------------------------------------------------%
