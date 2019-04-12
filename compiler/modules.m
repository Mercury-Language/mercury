%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: modules.m.
% Main author: fjh.
%
% Given a module_and_imports structure initialized for a raw_comp_unit,
% this module has the task of figuring out which interface files the
% raw_comp_unit needs either directly or indirectly, and reading them in,
% adding them to the module_and_imports structure. If intermodule optimization
% is enabled, then calls to grab_opt_files and maybe grab_trans_optfiles
% will figure out what .opt and .trans_opt files the compilation unit can use,
% again either directly or indirectly, and add those to the module_and_imports
% structure. When all this is done, the module_and_imports structure
% will contain an augmented version of the original compilation unit.
%
% The roles of the interface files (.int0, .int3, .int2 and .int) that
% this module reads in are documented (to the extent that they are documented
% anywhere) in the modules that creates them, which are comp_unit_interface.m
% and write_module_interface_files.m.
%
% XXX The file notes/interface_files.html contains (a start on) some
% more comprehensive documentation.
%
%---------------------------------------------------------------------------%

:- module parse_tree.modules.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % grab_imported_modules_augment(Globals, SourceFileName,
    %   SourceFileModuleName, ModuleTimestamp, NestedSubModules, RawCompUnit,
    %   HaveReadModuleMaps, ModuleAndImports, !IO):
    %
    % Given the raw CompUnit, one of the modules stored in SourceFileName,
    % read in the private interface files (.int0) for all the parent modules,
    % the long interface files (.int) for all the imported modules, and the
    % short interface files (.in2) for all the indirectly imported modules.
    % Return the `module_and_imports' structure containing all the information
    % gathered this way, from which we will compute the augmented version
    % of RawCompUnit.
    % XXX ITEM_LIST Move the actual computation of the AugCompUnit together
    % with this code, preferably in a new module, perhaps named something like
    % "augment_comp_unit.m".
    %
    % SourceFileModuleName is the top-level module name in SourceFileName.
    % ModuleTimestamp is the timestamp of the SourceFileName. NestedSubModules
    % is the list of the names of the nested submodules in SourceFileName
    % if RawCompUnit is the toplevel module in SourceFileName (i.e. if it is
    % the compilation unit of SourceFileModuleName). XXX ITEM_LIST document
    % exactly what NestedSubModules is if RawCompUnit is NOT the toplevel
    % module in SourceFileName. HaveReadModuleMaps contains the interface
    % files read during recompilation checking.
    %
    % Used when augmenting a module, which we do when asked to do
    % the tasks described by op_mode_augment. Most of the time, this is
    % generating target language code, but sometimes it may be e.g.
    % generating .opt and .trans_opt files.
    %
:- pred grab_imported_modules_augment(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, set(module_name)::in,
    raw_compilation_unit::in, have_read_module_maps::in,
    module_and_imports::out, io::di, io::uo) is det.

    % grab_unqual_imported_modules(Globals,
    %   SourceFileName, SourceFileModuleName, RawCompUnit, ModuleAndImports,
    %   !IO):
    %
    % Similar to grab_imported_modules, but only reads in the unqualified
    % short interfaces (.int3s), and the .int0 files for parent modules,
    % instead of reading the long interfaces and qualified short interfaces
    % (.int and int2s). Does not set the `PublicChildren', `FactDeps'
    % `ForeignIncludeFiles' fields of the module_and_imports structure.
    %
    % Used when generating .int0 files, and when generating .int/.int2 files.
    %
:- pred grab_unqual_imported_modules_make_int(globals::in,
    file_name::in, module_name::in,
    raw_compilation_unit::in, module_and_imports::out, io::di, io::uo) is det.

    % Add the items from the .opt files of imported modules to
    % the items for this module.
    %
:- pred grab_opt_files(globals::in,
    module_and_imports::in, module_and_imports::out, bool::out,
    io::di, io::uo) is det.

    % grab_trans_optfiles(Globals, ModuleList, !ModuleAndImports, Error, !IO):
    %
    % Add the items from each of the modules in ModuleList.trans_opt to
    % the items in ModuleAndImports.
    %
:- pred grab_trans_opt_files(globals::in, list(module_name)::in,
    module_and_imports::in, module_and_imports::out, bool::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.comp_unit_interface.    % undesirable dependency
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module cord.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module term.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

grab_imported_modules_augment(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, NestedChildren, RawCompUnit, HaveReadModuleMaps,
        !:ModuleAndImports, !IO) :-
    % The predicates grab_imported_modules and grab_unqual_imported_modules
    % have quite similar tasks. Please keep the corresponding parts of these
    % two predicates in sync.
    %
    % XXX ITEM_LIST Why aren't we updating !HaveReadModuleMaps?
    some [!Specs,
        !IntIndirectImported, !ImpIndirectImported,
        !IntImpIndirectImported, !ImpImpIndirectImported]
    (
        WhichGrab = grab_imported(MaybeTimestamp, NestedChildren),
        make_initial_module_and_imports(SourceFileName, SourceFileModuleName,
            WhichGrab, RawCompUnit, SrcItemBlocks, !:ModuleAndImports),

        % Find the modules named in import_module and use_module decls.
        RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
            RawItemBlocks),
        get_dependencies_int_imp_in_raw_item_blocks(RawItemBlocks,
            IntImportedMap, IntUsedMap, ImpImportedMap, ImpUsedMap),
        set.sorted_list_to_set(map.keys(IntImportedMap), IntImported0),
        set.sorted_list_to_set(map.keys(IntUsedMap), IntUsed0),
        set.sorted_list_to_set(map.keys(ImpImportedMap), ImpImported0),
        set.sorted_list_to_set(map.keys(ImpUsedMap), ImpUsed0),

        HaveReadModuleMapInt = HaveReadModuleMaps ^ hrmm_int,

        Ancestors = set.list_to_set(get_ancestors(ModuleName)),

        !:Specs = [],
        warn_if_import_for_self_or_ancestor(ModuleName, RawItemBlocks,
            Ancestors, IntImported0, !Specs),
        warn_if_import_for_self_or_ancestor(ModuleName, RawItemBlocks,
            Ancestors, IntUsed0, !Specs),
        warn_if_import_for_self_or_ancestor(ModuleName, RawItemBlocks,
            Ancestors, ImpImported0, !Specs),
        warn_if_import_for_self_or_ancestor(ModuleName, RawItemBlocks,
            Ancestors, ImpUsed0, !Specs),

        warn_if_duplicate_use_import_decls(ModuleName, ModuleNameContext,
            IntImported0, IntImported1, IntUsed0, IntUsed1,
            ImpImported0, ImpImported, ImpUsed0, ImpUsed,
            IntUsedImpImported, !Specs),

        % Add `builtin' and `private_builtin', and any other builtin modules
        % needed by any of the items, to the imported modules.
        % XXX Why are these added to the interface, and not the implementation
        % dependencies?
        get_implicit_dependencies_in_item_blocks(Globals, SrcItemBlocks,
            ImplicitIntImportedMap, ImplicitIntUsedMap),
        set.sorted_list_to_set(map.keys(ImplicitIntImportedMap),
            ImplicitIntImported),
        set.sorted_list_to_set(map.keys(ImplicitIntUsedMap),
            ImplicitIntUsed),
        set.union(ImplicitIntImported, IntImported1, IntImported2),
        set.union(ImplicitIntUsed, IntUsed1, IntUsed2),

        % Get the .int0 files of the ancestor modules.
        %
        % Uses of the items declared in ancestor modules do not need
        % module qualifiers. Modules imported by ancestors are considered
        % to be visible in the current module.
        % XXX grab_unqual_imported_modules treats ParentImported and ParentUsed
        % slightly differently from !.IntImported and !.IntUsed.
        process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt,
            "ancestors",
            make_ims_imported(import_locn_interface),
            make_ims_imported(import_locn_ancestor_private_interface_proper),
            module_and_imports_add_direct_int_item_blocks,
            Ancestors, IntImported2, IntImported, IntUsed2, IntUsed,
            !ModuleAndImports, !IO),

        % Get the .int files of the modules imported using `import_module'.
        set.init(!:IntIndirectImported),
        set.init(!:ImpIndirectImported),
        set.init(!:IntImpIndirectImported),
        set.init(!:ImpImpIndirectImported),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "int_imported", pik_direct(int123_1, may_be_unqualified),
            make_ims_imported(import_locn_interface),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            IntImported, !IntIndirectImported, !IntImpIndirectImported,
            !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "imp_imported", pik_direct(int123_1, may_be_unqualified),
            make_ims_imported(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ImpImported, !ImpIndirectImported, !ImpImpIndirectImported,
            !ModuleAndImports, !IO),

        % Get the .int files of the modules imported using `use_module'.
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "int_used", pik_direct(int123_1, must_be_qualified),
            make_ims_used(import_locn_interface),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            IntUsed, !IntIndirectImported, !IntImpIndirectImported,
            !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "imp_used", pik_direct(int123_1, must_be_qualified),
            make_ims_used(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ImpUsed, !ImpIndirectImported, !ImpImpIndirectImported,
            !ModuleAndImports, !IO),

        % Get the .int files of the modules imported using `use_module'
        % in the interface and `import_module' in the implementation.
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "int_used_imp_imported",
            pik_direct(int123_1, may_be_unqualified),
            make_ims_used_and_imported(import_locn_interface),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            IntUsedImpImported, !IntIndirectImported, !IntImpIndirectImported,
            !ModuleAndImports, !IO),

        % Get the .int2 files of the modules imported in .int files.
        process_module_indirect_interfaces_transitively(Globals,
            HaveReadModuleMapInt,
            "int_indirect_imported", pik_indirect(int123_2),
            make_ims_used(import_locn_interface),
            make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.IntIndirectImported,
            !IntImpIndirectImported, !ModuleAndImports, !IO),
        process_module_indirect_interfaces_transitively(Globals,
            HaveReadModuleMapInt,
            "imp_indirect_imported", pik_indirect(int123_2),
            make_ims_used(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.ImpIndirectImported,
            !ImpImpIndirectImported, !ModuleAndImports, !IO),

        % Get the .int2 files of the modules indirectly imported
        % the implementation sections of .int/.int2 files.
        % XXX Shouldn't these be .int3 files, as implied by the following
        % old comment?
        % Process the short interfaces for modules imported in the
        % implementation of indirectly imported modules. The items in these
        % modules shouldn't be visible to typechecking -- they are used for
        % fully expanding equivalence types after the semantic checking passes.
        process_module_indirect_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMapInt,
            "int_imp_indirect_imported", pik_indirect(int123_2),
            make_ims_abstract_imported, make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.IntImpIndirectImported, !ModuleAndImports, !IO),
        process_module_indirect_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMapInt,
            "imp_imp_indirect_imported", pik_indirect(int123_2),
            make_ims_abstract_imported, make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.ImpImpIndirectImported, !ModuleAndImports, !IO),

        module_and_imports_get_aug_comp_unit(!.ModuleAndImports, AugCompUnit,
            _, _),
        AllImportedOrUsed = set.union_list([IntImported, IntUsed,
            ImpImported, ImpUsed, IntUsedImpImported]),
        check_imports_accessibility(AugCompUnit, AllImportedOrUsed, !Specs),
        module_and_imports_add_specs(!.Specs, !ModuleAndImports)
    ).

grab_unqual_imported_modules_make_int(Globals, SourceFileName,
        SourceFileModuleName, RawCompUnit, !:ModuleAndImports, !IO) :-
    % The predicates grab_imported_modules and grab_unqual_imported_modules
    % have quite similar tasks. Please keep the corresponding parts of these
    % two predicates in sync.
    %
    % XXX ITEM_LIST Why aren't we updating !HaveReadModuleMaps?

    some [!IntIndirectImported, !ImpIndirectImported]
    (
        WhichGrab = grab_unqual_imported,
        % XXX _SrcItemBlocks
        make_initial_module_and_imports(SourceFileName, SourceFileModuleName,
            WhichGrab, RawCompUnit, _SrcItemBlocks, !:ModuleAndImports),

        % Find the modules named in import_module and use_module decls.
        RawCompUnit = raw_compilation_unit(ModuleName, _ModuleNameContext,
            RawItemBlocks),
        get_dependencies_int_imp_in_raw_item_blocks(RawItemBlocks,
            IntImportedMap, IntUsedMap, ImpImportedMap, ImpUsedMap),
        set.sorted_list_to_set(map.keys(IntImportedMap), IntImported0),
        set.sorted_list_to_set(map.keys(IntUsedMap), IntUsed0),
        set.sorted_list_to_set(map.keys(ImpImportedMap), ImpImported),
        set.sorted_list_to_set(map.keys(ImpUsedMap), ImpUsed),

        map.init(HaveReadModuleMapInt),

        % Add `builtin' and `private_builtin', and any other builtin modules
        % needed by any of the items, to the imported modules.
        % XXX Why are these added to the interface, and not the implementation
        % dependencies?
        get_implicit_dependencies_in_item_blocks(Globals, RawItemBlocks,
            ImplicitIntImportedMap, ImplicitIntUsedMap),
        set.sorted_list_to_set(map.keys(ImplicitIntImportedMap),
            ImplicitIntImported),
        set.sorted_list_to_set(map.keys(ImplicitIntUsedMap),
            ImplicitIntUsed),
        set.union(ImplicitIntImported, IntImported0, IntImported),
        set.union(ImplicitIntUsed, IntUsed0, IntUsed),

        Ancestors = set.list_to_set(get_ancestors(ModuleName)),

        % Get the .int0 files of the ancestor modules.
        process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt,
            "unqual_ancestors",
            make_ims_imported(import_locn_interface),
            make_ims_imported(import_locn_ancestor_private_interface_proper),
            module_and_imports_add_direct_int_item_blocks,
            Ancestors, set.init, ParentImported, set.init, ParentUsed,
            !ModuleAndImports, !IO),

        % Get the .int3 files of the modules imported using `import_module'.
        set.init(!:IntIndirectImported),
        set.init(!:ImpIndirectImported),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_parent_imported", pik_direct(int123_3, may_be_unqualified),
            make_ims_imported(import_locn_import_by_ancestor),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ParentImported,
            !IntIndirectImported, set.init, _, !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_int_imported", pik_direct(int123_3, may_be_unqualified),
            make_ims_imported(import_locn_interface),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            IntImported,
            !IntIndirectImported, set.init, _, !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_imp_imported", pik_direct(int123_3, may_be_unqualified),
            make_ims_imported(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ImpImported,
            !ImpIndirectImported, set.init, _, !ModuleAndImports, !IO),

        % Get the .int3 files of the modules imported using `use_module'.
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_parent_used", pik_direct(int123_3, may_be_unqualified),
            make_ims_imported(import_locn_import_by_ancestor),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ParentUsed,
            !IntIndirectImported, set.init, _, !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_int_used", pik_direct(int123_3, must_be_qualified),
            make_ims_used(import_locn_interface), make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            IntUsed,
            !IntIndirectImported, set.init, _, !ModuleAndImports, !IO),
        process_module_int123_files(Globals, HaveReadModuleMapInt,
            "unqual_imp_used", pik_direct(int123_3, must_be_qualified),
            make_ims_used(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_direct_int_item_blocks,
            ImpUsed,
            !ImpIndirectImported, set.init, _, !ModuleAndImports, !IO),

        % Get the .int3 files of the modules imported in .int3 files.
        process_module_indirect_interfaces_transitively(Globals,
            HaveReadModuleMapInt,
            "unqual_int_indirect_imported", pik_indirect(int123_3),
            make_ims_used(import_locn_interface), make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.IntIndirectImported, set.init, _, !ModuleAndImports, !IO),
        process_module_indirect_interfaces_transitively(Globals,
            HaveReadModuleMapInt,
            "unqual_imp_indirect_imported", pik_indirect(int123_3),
            make_ims_used(import_locn_implementation),
            make_ims_abstract_imported,
            module_and_imports_add_indirect_int_item_blocks,
            !.ImpIndirectImported, set.init, _, !ModuleAndImports, !IO)
    ).

%---------------------------------------------------------------------------%

:- type which_grab
    --->    grab_imported(maybe(timestamp), set(module_name))
    ;       grab_unqual_imported.

:- pred make_initial_module_and_imports(file_name::in, module_name::in,
    which_grab::in, raw_compilation_unit::in,
    list(src_item_block)::out, module_and_imports::out) is det.

make_initial_module_and_imports(SourceFileName, SourceFileModuleName,
        WhichGrab, RawCompUnit, SrcItemBlocks, ModuleAndImports) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    % XXX Why do we compute NestedChildren, FactDeps, ForeignIncludeFiles,
    % SrcItemBlocks and PublicChildren differently in these two cases?
    % XXX And why do we return SrcItemBlocks, instead of SrcItemBlocksWithFIMs?
    (
        WhichGrab = grab_imported(MaybeTimestamp, NestedChildren),
        (
            MaybeTimestamp = yes(Timestamp),
            MaybeTimestampMap = yes(map.singleton(ModuleName,
                module_timestamp(fk_src, Timestamp, may_be_unqualified)))
        ;
            MaybeTimestamp = no,
            MaybeTimestampMap = no
        ),

        get_src_item_blocks_public_children(RawCompUnit,
            SrcItemBlocks, PublicChildren),

        % XXX ITEM_LIST Store the FactDeps and ForeignIncludeFiles
        % in the raw_comp_unit.
        get_fact_table_dependencies_in_item_blocks(RawItemBlocks, FactDeps),
        get_foreign_include_files_in_item_blocks(RawItemBlocks,
            ForeignIncludeFiles)
    ;
        WhichGrab = grab_unqual_imported,
        set.init(NestedChildren),
        MaybeTimestampMap = no,

        raw_item_blocks_to_src(RawItemBlocks, SrcItemBlocks),
        map.init(PublicChildren),

        FactDeps = [],
        ForeignIncludeFiles = cord.init
    ),

    % Construct the initial module import structure.
    % XXX ITEM_LIST sms_interface is a guess. The original code (whose
    % behavior the current code is trying to emulate) simply added
    % the generated items to a raw item list, seemingly without caring
    % about what section those items would end up (it certainly did not
    % look for any section markers).
    add_needed_foreign_import_module_items_to_item_blocks(ModuleName,
        ModuleName, sms_interface, SrcItemBlocks, SrcItemBlocksWithFIMs),

    InitSpecs = [],
    make_module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, ModuleNameContext, SrcItemBlocksWithFIMs, InitSpecs,
        PublicChildren, NestedChildren, FactDeps, ForeignIncludeFiles,
        MaybeTimestampMap, ModuleAndImports).

%---------------------------------------------------------------------------%

:- pred get_src_item_blocks_public_children(raw_compilation_unit::in,
    list(src_item_block)::out, multi_map(module_name, prog_context)::out)
    is det.

get_src_item_blocks_public_children(RawCompUnit,
        SrcItemBlocks, PublicChildren) :-
    RawCompUnit = raw_compilation_unit(_, _, RawItemBlocks),
    get_included_modules_in_item_blocks(RawItemBlocks, Children),
    % If this module has any separately-compiled submodules, then we need
    % to make everything in the implementation of this module exported to
    % submodules. We do that by splitting out the implementation declarations
    % and putting them in a special `sms_impl_but_exported_to_submodules'
    % section.
    ( if map.is_empty(Children) then
        raw_item_blocks_to_src(RawItemBlocks, SrcItemBlocks),
        map.init(PublicChildren)
    else
        get_int_and_imp(RawCompUnit, IFileItemBlocks, NoIFileItemBlocks),
        raw_item_blocks_to_src(IFileItemBlocks, IFileSrcItemBlocks),
        raw_item_blocks_to_split_src(NoIFileItemBlocks, NoIFileSrcItemBlocks),
        SrcItemBlocks = IFileSrcItemBlocks ++ NoIFileSrcItemBlocks,
        get_included_modules_in_item_blocks(IFileItemBlocks,
            PublicChildren)
    ).

:- pred raw_item_blocks_to_src(list(item_block(module_section))::in,
    list(item_block(src_module_section))::out) is det.

raw_item_blocks_to_src([], []).
raw_item_blocks_to_src([RawItemBlock | RawItemBlocks],
        [SrcItemBlock | SrcItemBlocks]) :-
    RawItemBlock = item_block(ModuleName, Section, SectionContext,
        Incls, Avails, Items),
    (
        Section = ms_interface,
        SrcSection = sms_interface
    ;
        Section = ms_implementation,
        SrcSection = sms_implementation
    ),
    SrcItemBlock = item_block(ModuleName, SrcSection, SectionContext,
        Incls, Avails, Items),
    raw_item_blocks_to_src(RawItemBlocks, SrcItemBlocks).

:- pred raw_item_blocks_to_split_src(list(raw_item_block)::in,
    list(src_item_block)::out) is det.

raw_item_blocks_to_split_src([], []).
raw_item_blocks_to_split_src([RawItemBlock | RawItemBlocks],
        !:SrcItemBlocks) :-
    raw_item_blocks_to_split_src(RawItemBlocks, !:SrcItemBlocks),
    RawItemBlock = item_block(ModuleName, _Section, SectionContext,
        Incls, Avails, Items),
    % _Section can sometimes (rarely) be ms_interface. This can happen
    % when an instance declaration occurs in the interface section of a module.
    % The abstract version of the declaration gets put into the interface,
    % but the full version gets put into the noifile item blocks, with
    % the original (i.e. ms_interface) section marker.
    % XXX ITEM_LIST Fix that section marker.
    split_items_into_clauses_and_decls(Items,
        [], RevClauses, [], RevImpDecls),
    ( if
        RevClauses = []
    then
        true
    else
        list.reverse(RevClauses, Clauses),
        ClauseItemBlock = item_block(ModuleName, sms_implementation,
            SectionContext, [], [], Clauses),
        !:SrcItemBlocks = [ClauseItemBlock | !.SrcItemBlocks]
    ),
    ( if
        Incls = [],
        Avails = [],
        RevImpDecls = []
    then
        true
    else
        list.reverse(RevImpDecls, ImpDecls),
        ImpDeclItemBlock = item_block(ModuleName,
            sms_impl_but_exported_to_submodules, SectionContext,
            Incls, Avails, ImpDecls),
        !:SrcItemBlocks = [ImpDeclItemBlock | !.SrcItemBlocks]
    ).

:- pred split_items_into_clauses_and_decls(list(item)::in,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

split_items_into_clauses_and_decls([], !RevClauses, !RevImpDecls).
split_items_into_clauses_and_decls([Item | Items],
        !RevClauses, !RevImpDecls) :-
    (
        ( Item = item_clause(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        !:RevClauses = [Item | !.RevClauses]
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            !:RevClauses = [Item | !.RevClauses]
        ;
            AllowedInInterface = yes,
            !:RevImpDecls = [Item | !.RevImpDecls]
        )
    ;
        % XXX ITEM_LIST I (zs) think that item_nothings should not be put
        % anywhere.
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_mutable(_)
        ; Item = item_foreign_import_module(_)
        ; Item = item_type_repn(_)
        ; Item = item_nothing(_)
        ),
        !:RevImpDecls = [Item | !.RevImpDecls]
    ),
    split_items_into_clauses_and_decls(Items, !RevClauses, !RevImpDecls).

%---------------------------------------------------------------------------%

    % Warn if a module imports itself, or an ancestor.
    %
:- pred warn_if_import_for_self_or_ancestor(module_name::in,
    list(raw_item_block)::in, set(module_name)::in, set(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_import_for_self_or_ancestor(ModuleName, RawItemBlocks,
        Ancestors, ImportedOrUsed, !Specs) :-
    set.intersect(Ancestors, ImportedOrUsed, ImportedOrUsedAncestors),
    set.fold(find_and_warn_import_for_ancestor(ModuleName, RawItemBlocks),
        ImportedOrUsedAncestors, !Specs),
    ( if set.member(ModuleName, ImportedOrUsed) then
        find_and_warn_import_for_self(ModuleName, RawItemBlocks, !Specs)
    else
        true
    ).

%---------------------%

:- pred find_and_warn_import_for_self(module_name::in,
    list(raw_item_block)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

find_and_warn_import_for_self(ModuleName, RawItemBlocks, !Specs) :-
    find_avail_contexts_for_module_in_item_blocks(RawItemBlocks,
        ModuleName, [], AvailContexts),
    list.foldl(warn_import_for_self(ModuleName), AvailContexts, !Specs).

:- pred find_and_warn_import_for_ancestor(module_name::in,
    list(raw_item_block)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

find_and_warn_import_for_ancestor(ModuleName, RawItemBlocks,
        AncestorModuleName, !Specs) :-
    find_avail_contexts_for_module_in_item_blocks(RawItemBlocks,
        AncestorModuleName, [], AvailContexts),
    list.foldl(warn_import_for_ancestor(ModuleName, AncestorModuleName),
        AvailContexts, !Specs).

%---------------------%

    % Return the set of contexts in which the given item blocks import or use
    % the named module.
    %
    % The order in which we return the contexts doesn't matter, because
    % the error specs we generate for the returned contexts will be sorted,
    % and any duplicates removed, before they are printed.
    %
:- pred find_avail_contexts_for_module_in_item_blocks(list(raw_item_block)::in,
    module_name::in, list(prog_context)::in, list(prog_context)::out) is det.

find_avail_contexts_for_module_in_item_blocks([], _, !AvailContexts).
find_avail_contexts_for_module_in_item_blocks([ItemBlock | ItemBlocks],
        ModuleName, !AvailContexts) :-
    ItemBlock = item_block(_, _, _, _Includes, Avails, _Items),
    find_avail_contexts_for_module_in_avails(Avails,
        ModuleName, !AvailContexts),
    find_avail_contexts_for_module_in_item_blocks(ItemBlocks,
        ModuleName, !AvailContexts).

:- pred find_avail_contexts_for_module_in_avails(list(item_avail)::in,
    module_name::in, list(prog_context)::in, list(prog_context)::out) is det.

find_avail_contexts_for_module_in_avails([], _, !AvailContexts).
find_avail_contexts_for_module_in_avails([Avail | Avails],
        ModuleName, !AvailContexts) :-
    (
        Avail = avail_import(Import),
        Import = avail_import_info(AvailModuleName, Context, _SeqNum)
    ;
        Avail = avail_use(Use),
        Use = avail_use_info(AvailModuleName, Context, _SeqNum)
    ),
    ( if ModuleName = AvailModuleName then
        !:AvailContexts = [Context | !.AvailContexts]
    else
        true
    ),
    find_avail_contexts_for_module_in_avails(Avails,
        ModuleName, !AvailContexts).

%---------------------%

:- pred warn_import_for_self(module_name::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_import_for_self(ModuleName, Context, !Specs) :-
    Pieces = [words("Warning: module"), qual_sym_name(ModuleName),
        words("imports itself!"), nl],
    Msg = simple_msg(Context,
        [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
    Severity = severity_conditional(warn_simple_code, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred warn_import_for_ancestor(module_name::in, module_name::in,
    prog_context::in,list(error_spec)::in, list(error_spec)::out) is det.

warn_import_for_ancestor(ModuleName, AncestorName, Context, !Specs) :-
    MainPieces = [words("Module"), qual_sym_name(ModuleName),
        words("imports its own ancestor, module"),
        qual_sym_name(AncestorName), words(".")],
    VerbosePieces = [words("Every submodule"),
        words("implicitly imports its ancestors."),
        words("There is no need to explicitly import them.")],
    Msg = simple_msg(Context,
        [option_is_set(warn_simple_code, yes,
            [always(MainPieces),
            verbose_only(verbose_once, VerbosePieces)])]),
    Severity = severity_conditional(warn_simple_code, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

    % This predicate ensures that every import_module declaration is checked
    % against every use_module declaration, except for the case where
    % the interface has `:- use_module foo.' and the implementation
    % `:- import_module foo.'. Return the set of modules that have a
    % `:- use_module foo' in the interface and an `:- import_module foo'
    % in the implementation.
    %
:- pred warn_if_duplicate_use_import_decls(module_name::in, prog_context::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_duplicate_use_import_decls(ModuleName, Context,
        !IntImported, !IntUsed, !ImpImported, !ImpUsed, IntUsedImpImported,
        !Specs) :-
    do_warn_if_duplicate_use_import_decls(ModuleName, Context,
        !IntImported, !IntUsed, !Specs),
    do_warn_if_duplicate_use_import_decls(ModuleName, Context,
        !IntImported, !ImpUsed, !Specs),
    do_warn_if_duplicate_use_import_decls(ModuleName, Context,
        !ImpImported, !ImpUsed, !Specs),
    IntUsedImpImported = set.intersect(!.ImpImported, !.IntUsed),
    ( if set.is_empty(IntUsedImpImported) then
        % This is the usual case; optimize it.
        true
    else
        !:IntUsed = set.difference(!.IntUsed, IntUsedImpImported),
        !:ImpImported = set.difference(!.ImpImported, IntUsedImpImported)
    ).

    % Report warnings for modules imported using both `:- use_module'
    % and `:- import_module'. Remove the unnecessary `:- use_module'
    % declarations.
    %
:- pred do_warn_if_duplicate_use_import_decls(module_name::in,
    prog_context::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_warn_if_duplicate_use_import_decls(_ModuleName, Context,
        !Imported, !Used, !Specs) :-
    set.intersect(!.Imported, !.Used, ImportedAndUsed),
    ( if set.is_empty(ImportedAndUsed) then
        true
    else
        set.to_sorted_list(ImportedAndUsed, ImportedAndUsedList),
        Pieces = [words("Warning:"),
            words(choose_number(ImportedAndUsedList, "module", "modules"))] ++
            component_list_to_pieces("and",
                list.map(wrap_symname, ImportedAndUsedList)) ++
            [words(choose_number(ImportedAndUsedList, "is", "are")),
            words("imported using both"), decl("import_module"),
            words("and"), decl("use_module"), words("declarations."), nl],
        Msg = simple_msg(Context,
            [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],

        % Treat the modules with both types of import as if they
        % were imported using `:- import_module.'
        set.difference(!.Used, ImportedAndUsed, !:Used)
    ).

:- func wrap_symname(module_name) = format_component.

wrap_symname(ModuleName) = qual_sym_name(ModuleName).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% XXX ITEM_LIST Document what the process_xxx_interface predicates do
% more precisely, and document exactly WHY they do each of their actions.
% I (zs) think it likely that some of the interface files we now read in
% are read in unnecessarily.

:- type int_section_maker(MS) ==
    (func(module_name, int_file_kind) = MS).

:- type section_appender(MS) ==
    (pred(list(item_block(MS)), module_and_imports, module_and_imports)).
:- inst section_appender ==
    (pred(in, in, out) is det).

%---------------------------------------------------------------------------%

    % process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt,
    %   Why, NewIntSection, NewImpSection, SectionAppend, Ancestors,
    %   !DirectImports, !DirectUses, !ModuleAndImports, !IO):
    %
    % Read the complete private interfaces (.int0 files) for all the modules
    % in Ancestors. For each ancestor read, append any imports/uses of modules
    % to the !DirectImports or !DirectUses.
    %
    % Append all the item blocks in the read-in files to !ModuleAndImports,
    % putting all the ms_interface blocks in the int_module_section kind
    % generated by NewIntSection, and putting all the ms_implementation blocks
    % in the int_module_section section kind generated by NewImpSection.
    %
:- pred process_int0_files_of_ancestor_modules(globals::in,
    have_read_module_int_map::in, string::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender),
    set(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt, Why,
        NewIntSection, NewImpSection, SectionAppend, Ancestors,
        !DirectImports, !DirectUses, !ModuleAndImports, !IO) :-
    ( if set.remove_least(FirstAncestor, Ancestors, LaterAncestors) then
        ModuleName = !.ModuleAndImports ^ mai_module_name,
        expect_not(unify(FirstAncestor, ModuleName), $pred,
            "module is its own ancestor?"),
        ModAncestors0 = !.ModuleAndImports ^ mai_parent_deps,
        ( if set.member(FirstAncestor, ModAncestors0) then
            % We have already read it.
            maybe_log_augment_decision(Why, pik_int0, FirstAncestor,
                no, !IO)
        else
            maybe_log_augment_decision(Why, pik_int0, FirstAncestor,
                yes, !IO),
            process_module_int0_file(Globals, HaveReadModuleMapInt,
                FirstAncestor, NewIntSection, NewImpSection, SectionAppend,
                !DirectImports, !DirectUses, !ModuleAndImports, !IO)
        ),
        process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt,
            Why, NewIntSection, NewImpSection, SectionAppend, LaterAncestors,
            !DirectImports, !DirectUses, !ModuleAndImports, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % process_module_indirect_interfaces_and_impls_transitively(Globals,
    %   HaveReadModuleMapInt, Why, PIKind,
    %   NewIntSection, NewImpSection, SectionAppend,
    %   Modules, !ModuleAndImports, !IO):
    %
    % Read the interfaces specified by PIKind for modules in Modules
    % (unless they have already been read in) and any modules that
    % those modules import (transitively) in the interface or implementation.
    %
    % Append all the item blocks in the read-in files to !ModuleAndImports,
    % putting all the ms_interface blocks in the int_module_section kind
    % generated by NewIntSection, and putting all the ms_implementation blocks
    % in the int_module_section kind generated by NewImpSection.
    %
:- pred process_module_indirect_interfaces_and_impls_transitively(globals::in,
    have_read_module_int_map::in, string::in, process_interface_kind::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender),
    set(module_name)::in,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_indirect_interfaces_and_impls_transitively(Globals,
        HaveReadModuleMapInt, Why, PIKind,
        NewIntSection, NewImpSection, SectionAppend,
        Modules, !ModuleAndImports, !IO) :-
    process_module_indirect_interfaces_transitively(Globals,
        HaveReadModuleMapInt, Why, PIKind,
        NewIntSection, NewImpSection, SectionAppend,
        Modules, set.init, ImpIndirectImports, !ModuleAndImports, !IO),
    ( if set.is_empty(ImpIndirectImports) then
        true
    else
        process_module_indirect_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMapInt, Why, PIKind,
            NewIntSection, NewImpSection, SectionAppend,
            ImpIndirectImports, !ModuleAndImports, !IO)
    ).

    % process_module_indirect_interfaces_transitively(Globals,
    %   HaveReadModuleMapInt, Why, PIKind,
    %   NewIntSection, NewImpSection, SectionAppend,
    %   Modules, !ImpIndirectImports, !ModuleAndImports):
    %
    % Read the interfaces specified by PIKind for modules in Modules
    % (unless they have already been read in) and any modules that
    % those modules import (transitively) in the interface.
    %
    % Append all the item blocks in the read-in files to !ModuleAndImports,
    % putting all the ms_interface blocks in the int_module_section kind
    % generated by NewIntSection, and putting all the ms_implementation blocks
    % in the int_module_section kind generated by NewImpSection.
    %
:- pred process_module_indirect_interfaces_transitively(globals::in,
    have_read_module_int_map::in, string::in, process_interface_kind::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender),
    set(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_indirect_interfaces_transitively(Globals, HaveReadModuleMapInt,
        Why, PIKind, NewIntSection, NewImpSection, SectionAppend,
        Modules, !ImpIndirectImports, !ModuleAndImports, !IO) :-
    process_module_int123_files(Globals, HaveReadModuleMapInt, Why, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Modules,
        set.init, IndirectImports, !ImpIndirectImports,
        !ModuleAndImports, !IO),
    ( if set.is_empty(IndirectImports) then
        true
    else
        process_module_indirect_interfaces_transitively(Globals,
            HaveReadModuleMapInt, Why, PIKind,
            NewIntSection, NewImpSection, SectionAppend,
            IndirectImports, !ImpIndirectImports, !ModuleAndImports, !IO)
    ).

    % process_module_int123_files(Globals, HaveReadModuleMapInt, PIKind,
    %   NewIntSection, NewImpSection, SectionAppend,
    %   Modules, !IntImportsUses, !ImpImportsUses, !ModuleAndImports, !IO):
    %
    % Read the interfaces specified by PIKind for modules
    % in Modules (unless they have already been read in).
    % Append the modules imported and/or used by the interface of Modules
    % to !IntImportsUses.
    % Append the modules imported and/or used by the implementation of Modules
    % to !ImpImportsUses.
    %
    % Append all the item blocks in the read-in files to !ModuleAndImports,
    % putting all the ms_interface blocks in the int_module_section kind
    % generated by NewIntSection, and putting all the ms_implementation blocks
    % in the int_module_section kind generated by NewImpSection.
    %
:- pred process_module_int123_files(globals::in, have_read_module_int_map::in,
    string::in, process_interface_kind::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender), set(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_int123_files(Globals, HaveReadModuleMapInt, Why, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Modules,
        !IntIndirectImports, !ImpIndirectImports, !ModuleAndImports, !IO) :-
    ( if set.remove_least(FirstModule, Modules, LaterModules) then
        ( if
            % Have we already processed FirstModule.IntFileKind?
            ( FirstModule = !.ModuleAndImports ^ mai_module_name
            ; set.member(FirstModule, !.ModuleAndImports ^ mai_parent_deps)
            ; map.search(!.ModuleAndImports ^ mai_int_deps, FirstModule, _)
            ; map.search(!.ModuleAndImports ^ mai_imp_deps, FirstModule, _)
            ;
                PIKind = pik_indirect(_),
                set.member(FirstModule, !.ModuleAndImports ^ mai_indirect_deps)
            )
        then
            maybe_log_augment_decision(Why, PIKind, FirstModule, no, !IO)
        else
            maybe_log_augment_decision(Why, PIKind, FirstModule, yes, !IO),
            process_module_int123_file(Globals, HaveReadModuleMapInt,
                PIKind, NewIntSection, NewImpSection, SectionAppend,
                FirstModule, !IntIndirectImports, !ImpIndirectImports,
                !ModuleAndImports, !IO)
        ),
        process_module_int123_files(Globals, HaveReadModuleMapInt, Why,
            PIKind, NewIntSection, NewImpSection, SectionAppend,
            LaterModules, !IntIndirectImports, !ImpIndirectImports,
            !ModuleAndImports, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred process_module_int0_file(globals::in,
    have_read_module_int_map::in, module_name::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender),
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_int0_file(Globals, HaveReadModuleMapInt,
        Module, NewIntSection, NewImpSection, SectionAppend,
        !DirectImports, !DirectUses, !ModuleAndImports, !IO) :-
    PIKind = pik_int0,
    process_module_interface_general(Globals, HaveReadModuleMapInt, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Module,
        _IntAvails, _ImpAvails, ItemBlocks, !ModuleAndImports, !IO),
    get_dependencies_in_item_blocks(ItemBlocks,
        AncDirectImportsMap, AncDirectUsesMap),
    set.sorted_list_to_set(map.keys(AncDirectImportsMap), AncDirectImports),
    set.sorted_list_to_set(map.keys(AncDirectUsesMap), AncDirectUses),
    set.union(AncDirectImports, !DirectImports),
    set.union(AncDirectUses, !DirectUses).

:- pred process_module_int123_file(globals::in,
    have_read_module_int_map::in, process_interface_kind::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender), module_name::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_int123_file(Globals, HaveReadModuleMapInt, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Module,
        !IntImportsUses, !ImpImportsUses, !ModuleAndImports, !IO) :-
    process_module_interface_general(Globals, HaveReadModuleMapInt, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Module,
        IntAvails, ImpAvails, _ItemBlocks, !ModuleAndImports, !IO),
    get_dependencies_in_avails(IntAvails, IntImportsMap, IntUsesMap),
    get_dependencies_in_avails(ImpAvails, ImpImportsMap, ImpUsesMap),
    set.sorted_list_to_set(map.keys(IntImportsMap), IntImports),
    set.sorted_list_to_set(map.keys(IntUsesMap), IntUses),
    set.sorted_list_to_set(map.keys(ImpImportsMap), ImpImports),
    set.sorted_list_to_set(map.keys(ImpUsesMap), ImpUses),
    !:IntImportsUses = set.union_list([!.IntImportsUses, IntImports, IntUses]),
    !:ImpImportsUses = set.union_list([!.ImpImportsUses, ImpImports, ImpUses]).

:- type int123
    --->    int123_1
    ;       int123_2
    ;       int123_3.

:- type process_interface_kind
    --->    pik_int0
    ;       pik_direct(int123, need_qualifier)
    ;       pik_indirect(int123).   % implicitly must_be_qualified

:- pred process_module_interface_general(globals::in,
    have_read_module_int_map::in, process_interface_kind::in,
    int_section_maker(MS)::in, int_section_maker(MS)::in,
    section_appender(MS)::in(section_appender), module_name::in,
    list(item_avail)::out, list(item_avail)::out, list(item_block(MS))::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

process_module_interface_general(Globals, HaveReadModuleMapInt, PIKind,
        NewIntSection, NewImpSection, SectionAppend, Module,
        IntAvails, ImpAvails, ItemBlocks, !ModuleAndImports, !IO) :-
    (
        PIKind = pik_int0,
        IntFileKind = ifk_int0
    ;
        ( PIKind = pik_direct(Int123, _)
        ; PIKind = pik_indirect(Int123)
        ),
        ( Int123 = int123_1, IntFileKind = ifk_int
        ; Int123 = int123_2, IntFileKind = ifk_int2
        ; Int123 = int123_3, IntFileKind = ifk_int3
        )
    ),
    IFKStr = int_file_kind_to_extension(IntFileKind),
    MsgPrefix = "Reading " ++ IFKStr ++ " interface for module",

    maybe_return_timestamp(!.ModuleAndImports ^ mai_maybe_timestamp_map,
        ReturnTimestamp),
    maybe_read_module_int(Globals, HaveReadModuleMapInt, MsgPrefix, do_search,
        Module, IntFileKind, _FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTree, Specs, Errors, !IO),

    ParseTree = parse_tree_int(ModuleName, IntKind,
        Context, MaybeVersionNumbers,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems),
    module_and_imports_maybe_add_module_version_numbers(
        ModuleName, MaybeVersionNumbers, !ModuleAndImports),
    int_imp_items_to_item_blocks(Module, Context,
        NewIntSection(Module, IntKind), NewImpSection(Module, IntKind),
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntItems, ImpItems,
        ItemBlocks),

    SectionAppend(ItemBlocks, !ModuleAndImports),
    module_and_imports_add_specs_errors(Specs, Errors, !ModuleAndImports),

    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(Statistics, !IO),

    % XXX To me (zs), the differences here seem accidental rather than
    % deliberate.
    (
        PIKind = pik_int0,
        % XXX Why do we ignore Errors here for the timestamp (only)?
        maybe_record_timestamp(Module, ifk_int0, may_be_unqualified,
            MaybeTimestamp, !ModuleAndImports),
        set.intersect(Errors, fatal_read_module_errors, FatalErrors),
        ( if set.is_empty(FatalErrors) then
            ModAncestors0 = !.ModuleAndImports ^ mai_parent_deps,
            set.insert(Module, ModAncestors0, ModAncestors),
            !ModuleAndImports ^ mai_parent_deps := ModAncestors
        else
            true
        )
    ;
        PIKind = pik_indirect(_),
        % XXX Why do we ignore Errors here for (a) the timestamp,
        % and (b) for the update of !ModuleAndImports?
        maybe_record_timestamp(Module, IntFileKind, must_be_qualified,
            MaybeTimestamp, !ModuleAndImports),
        ModIndirectImports0 = !.ModuleAndImports ^ mai_indirect_deps,
        set.insert(Module, ModIndirectImports0, ModIndirectImports),
        !ModuleAndImports ^ mai_indirect_deps := ModIndirectImports
    ;
        PIKind = pik_direct(_, NeedQual),
        set.intersect(Errors, fatal_read_module_errors, FatalIntErrors),
        ( if set.is_empty(FatalIntErrors) then
            maybe_record_timestamp(Module, IntFileKind, NeedQual,
                MaybeTimestamp, !ModuleAndImports),
            ModImpImports0 = !.ModuleAndImports ^ mai_imp_deps,
            % Our caller cannot give us a useful nondummy context.
            multi_map.add(Module, term.context_init,
                ModImpImports0, ModImpImports),
            !ModuleAndImports ^ mai_imp_deps := ModImpImports
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

:- pred maybe_log_augment_decision(string::in, process_interface_kind::in,
    module_name::in, bool::in, io::di, io::uo) is det.
% Inlining calls to this predicate effectively optimizes it away
% if the trace condition is not met, as it usually won't be.
:- pragma inline(maybe_log_augment_decision/6).

maybe_log_augment_decision(Why, PIKind, ModuleName, Read, !IO) :-
    trace [compile_time(flag("log_augment_decisions")), io(!TIO)] (
        ModuleNameStr = sym_name_to_string(ModuleName),
        (
            PIKind = pik_int0,
            ExtensionStr = ".int0"
        ;
            PIKind = pik_direct(IntFileKind, NeedQual),
            KindStr = int123_str(IntFileKind),
            (
                NeedQual = must_be_qualified,
                ExtensionStr = "direct must_be_qualified " ++ KindStr
            ;
                NeedQual = may_be_unqualified,
                ExtensionStr = "direct may_be_unqualied " ++ KindStr
            )
        ;
            PIKind = pik_indirect(IntFileKind),
            ExtensionStr = "indirect " ++ int123_str(IntFileKind)
        ),
        (
            Read = no,
            ReadStr = "decided not to read"
        ;
            Read = yes,
            ReadStr = "decided to read"
        ),
        io.format("%s, %s, %s: %s\n",
            [s(Why), s(ModuleNameStr), s(ExtensionStr), s(ReadStr)], !TIO)
    ).

:- func int123_str(int123) = string.

int123_str(int123_1) = ".int".
int123_str(int123_2) = ".int2".
int123_str(int123_3) = ".int3".

%---------------------------------------------------------------------------%

:- pred maybe_return_timestamp(maybe(T)::in, maybe_return_timestamp::out)
    is det.

maybe_return_timestamp(yes(_), do_return_timestamp).
maybe_return_timestamp(no, dont_return_timestamp).

:- pred maybe_record_timestamp(module_name::in, int_file_kind::in,
    need_qualifier::in, maybe(timestamp)::in,
    module_and_imports::in, module_and_imports::out) is det.

maybe_record_timestamp(ModuleName, IntFileKind, NeedQual, MaybeTimestamp,
        !ModuleAndImports) :-
    (
        !.ModuleAndImports ^ mai_maybe_timestamp_map = yes(TimestampMap0),
        (
            MaybeTimestamp = yes(Timestamp),
            FileKind = fk_int(IntFileKind),
            TimestampInfo = module_timestamp(FileKind, Timestamp, NeedQual),
            map.set(ModuleName, TimestampInfo, TimestampMap0, TimestampMap),
            !ModuleAndImports ^ mai_maybe_timestamp_map := yes(TimestampMap)
        ;
            MaybeTimestamp = no
        )
    ;
        !.ModuleAndImports ^ mai_maybe_timestamp_map = no
    ).

%---------------------------------------------------------------------------%

    % check_imports_accessibility(AugItemBlocks, ImportedModules, !Specs):
    %
    % By the time we are called, we should have read in all the appropriate
    % interface files, including, for every imported/used module, at least
    % the short interface for that module's parent module, which will contain
    % the `include_module' declarations for any exported submodules
    % of the parent. So the set of accessible submodules can be determined
    % by looking at every include_module declaration in AugItemBlocks.
    %
    % We then go through all of the imported/used modules, looking for
    % (and reporting) two different but related kinds of errors.
    %
    % The first is when we see a reference to module x.y.z, but module
    % x.y does not include a submodule named z.
    %
    % The second is when module m.n has an import_module or use_module
    % declaration for module x.y.z, but there is some ancestor of x.y.z
    % (either x or x.y) that neither m.n nor its ancestor m imports or uses.
    %
    % A general principle we follow here is that we look for and report
    % these errors only for source files, and only when generating code them.
    % We do not expect automatically generated interface and optimization
    % files to be free of these kinds of errors, because if any such errors
    % are present in the source files from which they are generated,
    % those exact errors will be present in the interface and optimization
    % files as well. Reporting such errors when generating the interface
    % or optimization files disrupts the usual edit-compile-fix cycle,
    % because when e.g. generating interface files, the compiler puts
    % any error messages on standard output, not the module's .err file.
    % It is also unnecessary, since the error *will* be caught before
    % an executable can be produced.
    %
    % XXX ITEM_LIST The ImportedModules that our caller gives us
    % will consist of:
    %
    % - the modules imported or used in SrcItemBlocks,
    % - the modules imported or used in the .int3 files of the ancestors
    %   of *this* module, and
    % - any implicit dependencies on standard library modules, including
    %   the private and public builtin modules, the modules implementing
    %   the operations that we replace calls to e.g. io.format with, etc.
    %
    % XXX ITEM_LIST We should either record in an updated AugCompUnit
    % the set of imported modules that are inaccessible, or remove their
    % imports from it, so that
    %
    % - when we report e.g. an undefined type, we don't tell the user that
    %   the module that defines the type hasn't been imported, when in fact
    %   it *was* imported, but the import was disregarded because the module
    %   is inaccessible due to the missing import of an ancestor; and
    %
    % - we don't generate "unused module" warnings for them when
    %   --warn-unused-imports is enabled.
    %
:- pred check_imports_accessibility(aug_compilation_unit::in,
    set(module_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_imports_accessibility(AugCompUnit, _ImportedModules, !Specs) :-
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),
    IntItemBlocks = DirectIntItemBlocks ++ IndirectIntItemBlocks,
    record_includes_imports_uses(ModuleName, SrcItemBlocks, IntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks, ReadModules, InclMap,
        SrcIntImportUseMap, SrcImpImportUseMap, AncestorImportUseMap),

    % The current module is not an import, but this is the obvious place
    % to check whether its purported parent module (if any) actually
    % includes it.
    report_any_missing_includes(ReadModules, InclMap,
        ModuleName, [ModuleNameContext], !Specs),
    map.foldl(report_any_missing_includes_for_imports(ReadModules, InclMap),
        SrcIntImportUseMap, !Specs),
    map.foldl(report_any_missing_includes_for_imports(ReadModules, InclMap),
        SrcImpImportUseMap, !Specs),

    % When checking whether avail declarations (i.e. import_module
    % and use_module declarations) in the interface section
    % have an accessible avail declaration for their ancestor modules,
    % the places where those declarations may occur include not just
    % the interface of the module itself, but also the contents of
    % the .int0 interface files of ancestor modules.
    map.union(append_one_or_more, SrcIntImportUseMap, AncestorImportUseMap,
        SrcIntAncImportUseMap),
    map.foldl(
        find_any_missing_ancestor_imports(ModuleName, poa_parent,
            SrcIntAncImportUseMap),
        SrcIntImportUseMap, map.init, SrcIntMissingAncestorMap),

    % When checking whether avail declarations in the implementation section
    % have an accessible avail declaration for their ancestor modules,
    % the places where those declarations may occur include not just
    % the implementation section of the module itself, but also every place
    % that the interface of the module has access to.
    map.union(append_one_or_more, SrcIntAncImportUseMap, SrcImpImportUseMap,
        SrcIntImpImportUseMap),
    map.foldl(
        find_any_missing_ancestor_imports(ModuleName, poa_parent,
            SrcIntImpImportUseMap),
        SrcImpImportUseMap, map.init, SrcImpMissingAncestorMap0),

    % If we generate a message about a missing import (or use) for a module
    % in the interface section, do not generate another message for it
    % also missing in the implementation section, because adding an
    % import_module or use_module declaration for it to the interface
    % will also cure the problem in the implementation section.
    map.keys(SrcIntMissingAncestorMap, SrcIntMissingAncestors),
    map.delete_list(SrcIntMissingAncestors,
        SrcImpMissingAncestorMap0, SrcImpMissingAncestorMap),

    map.foldl(
        report_missing_ancestor(ModuleName,
            missing_in_src_int(SrcImpImportUseMap)),
        SrcIntMissingAncestorMap, !Specs),
    map.foldl(
        report_missing_ancestor(ModuleName, missing_in_src_imp),
        SrcImpMissingAncestorMap, !Specs).

:- pred append_one_or_more(one_or_more(T)::in, one_or_more(T)::in,
    one_or_more(T)::out) is det.

append_one_or_more(A, B, AB) :-
    A = one_or_more(HeadA, TailA),
    B = one_or_more(HeadB, TailB),
    AB = one_or_more(HeadA, TailA ++ [HeadB | TailB]).

%---------------------%
%
% The module_inclusion_map and module_import_or_use_map are computed by
% record_includes_imports_uses, for use by find_any_missing_ancestor_imports.
% For their documentation, see those predicates below.
%

:- type maybe_abstract_section
    --->    non_abstract_section
    ;       abstract_section.

:- type include_context
    --->    include_context(maybe_abstract_section, term.context).

:- type module_inclusion_map ==
    map(module_name, one_or_more(include_context)).

:- type import_or_use_context
    --->    import_or_use_context(import_or_use, term.context).

:- type module_import_or_use_map ==
    map(module_name, one_or_more(import_or_use_context)).

    % record_includes_imports_uses(ModuleName, SrcItemBlocks, IntItemBlocks,
    %   OptItemBlocks, IntForOptItemBlocks, ReadModules, InclMap,
    %   SrcIntImportUseMap, SrcImpImportUseMap, AncestorImportUseMap):
    %
    % Scan all the given item blocks from the compilation unit of ModuleName,
    % computing several outputs.
    %
    % - ReadModules will be the set of module names from whose files
    %   (source files, interface files, optimization files) the item blocks
    %   originate.
    % - InclMap will map the name of each module that is named in an
    %   include_module declaration in any item block to the context
    %   of that declaration.
    % - SrcIntImportUseMap will map the module names that occur in
    %   import_module or use_module declarations in the interface sections
    %   of the source file of ModuleName itself to the context(s)
    %   of those declarations.
    % - SrcImpImportUseMap is the same, but for the implementation section.
    % - AncestorImportUseMap is the same, but for import_module and use_module
    %   declarations read from (the .int0 interface files of) the ancestors
    %   of ModuleName.
    %
    % NOTE By making the value in both the module_inclusion_map and the
    % module_import_or_use_map a (nonempty) list, we can represent situations
    % in which a module includes, imports or uses another module
    % more than once. This is an error, and we could and probably should
    % diagnose it here, but doing so would require disabling the code
    % we have elsewhere in the compiler that does that job. If we did that,
    % we could replace the nonempty lists of contexts with just one context,
    % and a message for every other context.
    %
    % XXX ITEM_LIST We could store the results of this call in both raw and
    % augmented compilation units. (The raw version would of course be computed
    % from raw_item_blocks.)
    %
:- pred record_includes_imports_uses(module_name::in,
    list(src_item_block)::in, list(int_item_block)::in,
    list(opt_item_block)::in, list(int_for_opt_item_block)::in,
    set(module_name)::out, module_inclusion_map::out,
    module_import_or_use_map::out, module_import_or_use_map::out,
    module_import_or_use_map::out) is det.

record_includes_imports_uses(ModuleName, SrcItemBlocks, IntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks, !:ReadModules, !:InclMap,
        !:SrcIntImportUseMap, !:SrcImpImportUseMap, !:AncestorImportUseMap) :-
    set.init(!:ReadModules),
    map.init(!:InclMap),
    map.init(!:SrcIntImportUseMap),
    map.init(!:SrcImpImportUseMap),
    map.init(!:AncestorImportUseMap),
    Ancestors = get_ancestors_set(ModuleName),
    record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        SrcItemBlocks, src_section_visibility, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
    record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        IntItemBlocks, int_section_visibility, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
    record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        OptItemBlocks, opt_section_visibility, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
    record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        IntForOptItemBlocks, int_for_opt_section_visibility,
        !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- type which_map
    --->    src_int
    ;       src_imp
    ;       non_src_non_abstract
    ;       non_src_abstract.

:- type section_visibility(MS) == (func(MS) = which_map).

:- func src_section_visibility(src_module_section) = which_map.
:- func int_section_visibility(int_module_section) = which_map.
:- func opt_section_visibility(opt_module_section) = which_map.
:- func int_for_opt_section_visibility(int_for_opt_module_section) = which_map.

src_section_visibility(sms_interface) = src_int.
src_section_visibility(sms_implementation) = src_imp.
src_section_visibility(sms_impl_but_exported_to_submodules) = src_imp.

int_section_visibility(ims_imported_or_used(_, _, _, _)) =
    non_src_non_abstract.
int_section_visibility(ims_abstract_imported(_, _)) =
    non_src_abstract.

opt_section_visibility(oms_opt_imported(_, _)) = non_src_non_abstract.

int_for_opt_section_visibility(ioms_opt_imported(_, _)) = non_src_non_abstract.

:- pred record_includes_imports_uses_in_item_blocks_acc(set(module_name)::in,
    list(item_block(MS))::in, section_visibility(MS)::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_item_blocks_acc(_,
        [], _, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).
record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        [ItemBlock | ItemBlocks], SectionVisibility, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ItemBlock = item_block(ModuleName, Section, _, Incls, Avails, _Items),
    set.insert(ModuleName, !ReadModules),
    WhichMap = SectionVisibility(Section),
    (
        WhichMap = src_int,
        record_includes_acc(non_abstract_section, Incls, !InclMap),
        record_avails_acc(Avails, !SrcIntImportUseMap)
    ;
        WhichMap = src_imp,
        record_includes_acc(non_abstract_section, Incls, !InclMap),
        record_avails_acc(Avails, !SrcImpImportUseMap)
    ;
        (
            WhichMap = non_src_non_abstract,
            record_includes_acc(non_abstract_section, Incls, !InclMap)
        ;
            WhichMap = non_src_abstract,
            record_includes_acc(abstract_section, Incls, !InclMap)
        ),
        ( if set.contains(Ancestors, ModuleName) then
            record_avails_acc(Avails, !AncestorImportUseMap)
        else
            true
        )
    ),
    record_includes_imports_uses_in_item_blocks_acc(Ancestors,
        ItemBlocks, SectionVisibility, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_acc(maybe_abstract_section::in, list(item_include)::in,
    module_inclusion_map::in, module_inclusion_map::out) is det.

record_includes_acc(_, [], !InclMap).
record_includes_acc(Section, [Include | Includes], !InclMap) :-
    Include = item_include(ModuleName, Context, _SeqNum),
    IncludeContext = include_context(Section, Context),
    ( if map.search(!.InclMap, ModuleName, OneOrMore0) then
        OneOrMore0 = one_or_more(HeadContext, TailContexts),
        OneOrMore = one_or_more(IncludeContext, [HeadContext | TailContexts]),
        map.det_update(ModuleName, OneOrMore, !InclMap)
    else
        OneOrMore = one_or_more(IncludeContext, []),
        map.det_insert(ModuleName, OneOrMore, !InclMap)
    ),
    record_includes_acc(Section, Includes, !InclMap).

:- pred record_avails_acc(list(item_avail)::in,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_avails_acc([], !ImportUseMap).
record_avails_acc([Avail | Avails], !ImportUseMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = use_decl
    ),
    IoUC = import_or_use_context(ImportOrUse, Context),
    ( if map.search(!.ImportUseMap, ModuleName, OneOrMore0) then
        OneOrMore0 = one_or_more(HeadIoUC, TailIoUCs),
        OneOrMore = one_or_more(IoUC, [HeadIoUC | TailIoUCs]),
        map.det_update(ModuleName, OneOrMore, !ImportUseMap)
    else
        OneOrMore = one_or_more(IoUC, []),
        map.det_insert(ModuleName, OneOrMore, !ImportUseMap)
    ),
    record_avails_acc(Avails, !ImportUseMap).

%---------------------%

:- type parent_or_ancestor
    --->    poa_parent
    ;       poa_ancestor.

:- type import_and_or_use
    --->    import_only
    ;       use_only
    ;       import_and_use.

:- type missing_ancestor_info
    --->    missing_ancestor_info(
                mai_modules         :: set(module_name),
                mai_max_depth       :: parent_or_ancestor,
                mai_import_use      :: import_and_or_use,
                mai_least_context   :: term.context
            ).

:- type missing_ancestor_map == map(module_name, missing_ancestor_info).

    % find_any_missing_ancestor_imports(CurrentModule, ParentOrAncestor,
    %   ImportUseMap, ImportedModule, IoUCs, !MissingAncestorMap):
    %
    % If there are any ancestors of ImportedModule for which there is
    % neither an explicit import_module or use_module declaration in
    % ImportUseMap, nor an implicit declaration by virtue of that ancestor
    % module being an ancestor of CurrentModule as well, then record
    % the fact that we are missing an import or use of that ancestor.
    %
    % We don't generate an error message right here, so that if several
    % imported modules are missing the same ancestor, we can generate
    % just one message for that missing ancestor.
    %
    % The other inputs allow us to record information that will make
    % the eventual error message more informative.
    %
:- pred find_any_missing_ancestor_imports(module_name::in,
    parent_or_ancestor::in, module_import_or_use_map::in,
    module_name::in, one_or_more(import_or_use_context)::in,
    missing_ancestor_map::in, missing_ancestor_map::out) is det.

find_any_missing_ancestor_imports(CurrentModule, ParentOrAncestor,
        ImportUseMap, ImportedModule, IoUCs, !MissingAncestorMap) :-
    (
        ImportedModule = qualified(ParentModule, _SubModule),
        ( if
            (
                % Does CurrentModule import ParentModule explicitly?
                map.search(ImportUseMap, ParentModule, _ParentIoUCs)
            ;
                % Is ParentModule the same as CurrentModule, or a parent
                % or an ancestor of CurrentModule? If yes, then CurrentModule
                % imports it implicitly.
                is_submodule(CurrentModule, ParentModule)
            )
        then
            true
        else
            IoUCs = one_or_more(HeadIoUC, TailIoUCs),
            ( if
                map.search(!.MissingAncestorMap, ParentModule,
                    MissingAncestorInfo0)
            then
                MissingAncestorInfo0 = missing_ancestor_info(ChildModules0,
                    PoA0, ImportAndOrUse0, LeastContext0),
                set.insert(ImportedModule, ChildModules0, ChildModules),
                ( if
                    PoA0 = poa_parent,
                    ParentOrAncestor = poa_ancestor
                then
                    PoA = poa_ancestor
                else
                    PoA = PoA0
                ),
                update_iu_and_least_context(HeadIoUC,
                    ImportAndOrUse0, ImportAndOrUse1,
                    LeastContext0, LeastContext1),
                list.foldl2(update_iu_and_least_context, TailIoUCs,
                    ImportAndOrUse1, ImportAndOrUse,
                    LeastContext1, LeastContext),
                MissingAncestorInfo = missing_ancestor_info(ChildModules,
                    PoA, ImportAndOrUse, LeastContext),
                map.det_update(ParentModule, MissingAncestorInfo,
                    !MissingAncestorMap)
            else
                ChildModules = set.make_singleton_set(ImportedModule),
                HeadIoUC = import_or_use_context(HeadImportOrUse, HeadContext),
                (
                    HeadImportOrUse = import_decl,
                    ImportAndOrUse0 = import_only
                ;
                    HeadImportOrUse = use_decl,
                    ImportAndOrUse0 = use_only
                ),
                list.foldl2(update_iu_and_least_context, TailIoUCs,
                    ImportAndOrUse0, ImportAndOrUse,
                    HeadContext, LeastContext),
                MissingAncestorInfo = missing_ancestor_info(ChildModules,
                    ParentOrAncestor, ImportAndOrUse, LeastContext),
                map.det_insert(ParentModule, MissingAncestorInfo,
                    !MissingAncestorMap),
                find_any_missing_ancestor_imports(CurrentModule, poa_ancestor,
                    ImportUseMap, ParentModule, IoUCs, !MissingAncestorMap)
            )
        )
    ;
        ImportedModule = unqualified(_)
        % For modules without parent modules, accessibility is moot.
    ).

:- pred update_iu_and_least_context(import_or_use_context::in,
    import_and_or_use::in, import_and_or_use::out,
    term.context::in, term.context::out) is det.

update_iu_and_least_context(IoUC, !ImportAndOrUse, !LeastContext) :-
    IoUC = import_or_use_context(ImportOrUse, Context),
    (
        ImportOrUse = import_decl,
        (
            !.ImportAndOrUse = import_only
        ;
            ( !.ImportAndOrUse = use_only
            ; !.ImportAndOrUse = import_and_use
            ),
            !:ImportAndOrUse = import_and_use
        )
    ;
        ImportOrUse = use_decl,
        (
            !.ImportAndOrUse = use_only
        ;
            ( !.ImportAndOrUse = import_only
            ; !.ImportAndOrUse = import_and_use
            ),
            !:ImportAndOrUse = import_and_use
        )
    ),
    ( if
        compare((<), Context, !.LeastContext),
        Context \= term.context_init
    then
        !:LeastContext = Context
    else
        true
    ).

:- type missing_where
    --->    missing_in_src_int(module_import_or_use_map)
    ;       missing_in_src_imp
    ;       missing_in_non_src.

:- pred report_missing_ancestor(module_name::in,
    missing_where::in, module_name::in, missing_ancestor_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_missing_ancestor(ModuleName, MissingWhere,
        MissingModuleName, SrcIntInfo, !Specs) :-
    SrcIntInfo = missing_ancestor_info(DescendantModuleNamesSet, MaxDepth,
        ImportAndOrUse, LeastContext),
    set.to_sorted_list(DescendantModuleNamesSet, DescendantModuleNames),
    ( MaxDepth = poa_parent, ChildOrDescendant = "child"
    ; MaxDepth = poa_ancestor, ChildOrDescendant = "descendant"
    ),
    (
        ImportAndOrUse = import_only,
        DeclPieces = [decl("import_module")]
    ;
        ImportAndOrUse = use_only,
        DeclPieces = [decl("use_module")]
    ;
        ImportAndOrUse = import_and_use,
        DeclPieces = [decl("import_module"), words("and"), decl("use_module")]
    ),
    (
        MissingWhere = missing_in_src_int(_),
        InTheInterface = [words("in the interface")]
    ;
        ( MissingWhere = missing_in_src_imp
        ; MissingWhere = missing_in_non_src
        ),
        InTheInterface = []
    ),
    DescendantPieces = list.map(wrap_module_name, DescendantModuleNames),
    ModuleS = choose_number(DescendantModuleNames, "module", "modules"),
    DeclarationS = choose_number(DescendantModuleNames,
        "declaration", "declarations"),
    MainPieces = [words("In module"), qual_sym_name(ModuleName),
        suffix(":"), words("error:"), nl,
        words("the absence of an"), decl("import_module"), words("or"),
        decl("use_module"), words("declaration for"),
        qual_sym_name(MissingModuleName)] ++ InTheInterface ++
        [words("prevents access to the")] ++
        DeclPieces ++ [words(DeclarationS)] ++ InTheInterface ++
        [words("for its"), words(ChildOrDescendant), words(ModuleS)] ++
        component_list_to_pieces("and", DescendantPieces) ++
        [suffix("."), nl],
    MainMsg = simple_msg(LeastContext, [always(MainPieces)]),
    ( if
        MissingWhere = missing_in_src_int(SrcImpImportUseMap),
        map.search(SrcImpImportUseMap, MissingModuleName, IoUCs)
    then
        % XXX _TailIoUCs
        IoUCs = one_or_more(HeadIoUC, _TailIoUCs),
        HeadIoUC = import_or_use_context(ImportOrUse, ImpContext),
        ( ImportOrUse = import_decl, ImportOrUseDecl = "import_module"
        ; ImportOrUse = use_decl, ImportOrUseDecl = "use_module"
        ),
        ImpPieces = [words("Adding such a declaration would obsolete"),
            words("this"), decl(ImportOrUseDecl), words("declaration"),
            words("in the implementation section."), nl],
        ImpMsg = simple_msg(ImpContext, [always(ImpPieces)]),
        Msgs = [MainMsg, ImpMsg]
    else
        Msgs = [MainMsg]
    ),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, Msgs),
    !:Specs = [Spec | !.Specs].

:- func wrap_module_name(sym_name) = format_component.

wrap_module_name(Module) = qual_sym_name(Module).

%---------------------%

:- pred report_any_missing_includes_for_imports(set(module_name)::in,
    module_inclusion_map::in,
    module_name::in, one_or_more(import_or_use_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_missing_includes_for_imports(ReadModules, InclMap,
        ModuleName, IoUCs, !Specs) :-
    IoUCs = one_or_more(HeadIoUC, TailIoUCs),
    Contexts = list.map(project_out_import_or_use, [HeadIoUC | TailIoUCs]),
    report_any_missing_includes(ReadModules, InclMap,
        ModuleName, Contexts, !Specs).

    % report_any_missing_includes(ReadModules, InclMap, Module, Contexts,
    %   !Specs):
    %
    % If Module is a submodule of ParentModule but we haven't seen
    % an include_module declaration for Module in ParentModule even though
    % we should have seen it is exists (because we have read an interface
    % file for ParentModule, which should contain all its include_module
    % declarations), then add an error message reporting this fact to !Specs.
    %
:- pred report_any_missing_includes(set(module_name)::in,
    module_inclusion_map::in, module_name::in, list(term.context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_missing_includes(ReadModules, InclMap, Module, Contexts, !Specs) :-
    (
        Module = qualified(ParentModule, SubModule),
        ( if map.search(InclMap, Module, IncludeContexts) then
            % Module *has* its include in ParentModule, ...
            IncludeContexts =
                one_or_more(HeadIncludeContext, TailIncludeContexts),
            IncludeContextsList = [HeadIncludeContext | TailIncludeContexts],
            ( if any_true(is_non_abstract_include, IncludeContextsList) then
                % ... and it is visible here.
                true
            else
                % ... and it is NOT visible here.
                list.foldl(report_abstract_include(ParentModule, SubModule),
                    Contexts, !Specs)
            )
        else
            % We have not seen Module's include in ParentModule.
            ( if set.contains(ReadModules, ParentModule) then
                % We have read item blocks from ParentModule, and they
                % *should* have included its include_module declarations.
                list.foldl(report_missing_include(ParentModule, SubModule),
                    Contexts, !Specs)
            else
                % We have read not any item blocks from ParentModule.
                % For all we know, ParentModule *may* contain an include
                % for Module; we just don't know. Reporting an error
                % would be misleading.
                %
                % If we had imported ParentModule, we would have read
                % item blocks from one of its interface files. We will
                % report the missing import. If the include is truly missing
                % in ParentModule, we will discover and report that fact
                % when the missing import of ParentModule in the *current*
                % module is fixed by the programmer.
                true
            )
        ),
        report_any_missing_includes(ReadModules, InclMap,
            ParentModule, Contexts, !Specs)
    ;
        Module = unqualified(_)
        % For modules without parent modules, accessibility is moot.
    ).

:- pred report_abstract_include(module_name::in, string::in, term.context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_abstract_include(ParentModule, SubModule, Context, !Specs) :-
    Pieces = [words("Error:"),
        words("module"), qual_sym_name(ParentModule),
        words("has a submodule named"), quote(SubModule), suffix(","),
        words("but it is visible only to its other submodules."), nl],
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
        [simple_msg(Context, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

:- pred report_missing_include(module_name::in, string::in, term.context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_missing_include(ParentModule, SubModule, Context, !Specs) :-
    Pieces = [words("Error:"),
        words("module"), qual_sym_name(ParentModule),
        words("does not have a submodule named"), quote(SubModule),
        suffix("."), nl],
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
        [simple_msg(Context, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs].

:- pred is_non_abstract_include(include_context::in) is semidet.

is_non_abstract_include(IncludeContext) :-
    IncludeContext = include_context(MaybeAbstractInclude, _Context),
    MaybeAbstractInclude = non_abstract_section.

:- func project_out_import_or_use(import_or_use_context) = term.context.

project_out_import_or_use(import_or_use_context(_, Context)) = Context.

%---------------------------------------------------------------------------%

grab_opt_files(Globals, !ModuleAndImports, FoundError, !IO) :-
    % Read in the .opt files for imported and ancestor modules.
    ModuleName = !.ModuleAndImports ^ mai_module_name,
    Ancestors0 = !.ModuleAndImports ^ mai_parent_deps,
    IntDepsMap0 = !.ModuleAndImports ^ mai_int_deps,
    ImpDepsMap0 = !.ModuleAndImports ^ mai_imp_deps,
    set.sorted_list_to_set(map.keys(IntDepsMap0), IntDeps0),
    set.sorted_list_to_set(map.keys(ImpDepsMap0), ImpDeps0),
    OptFiles = set.union_list([Ancestors0, IntDeps0, ImpDeps0]),
    globals.lookup_bool_option(Globals, read_opt_files_transitively,
        Transitive),
    set.insert(ModuleName, OptFiles, ModulesProcessed),
    read_optimization_interfaces(Globals, Transitive,
        set.to_sorted_list(OptFiles), ModulesProcessed,
        cord.empty, OptItemBlocksCord, [], OptSpecs, no, OptError, !IO),
    OptItemBlocks = cord.list(OptItemBlocksCord),

    module_and_imports_add_opt_item_blocks(OptItemBlocks, !ModuleAndImports),
    module_and_imports_add_specs(OptSpecs, !ModuleAndImports),

    % Get the :- pragma unused_args(...) declarations created when writing
    % the .opt file for the current module. These are needed because we can
    % probably remove more arguments with intermod_unused_args, but the
    % interface for other modules must remain the same.
    %
    % Similarly for the  :- pragma structure_reuse(...) declarations. With more
    % information available when making the target code than when writing the
    % `.opt' file, it can turn out that a procedure which seemed to have
    % condition reuse actually has none. But we have to maintain the interface
    % for modules that use the conditional reuse information from the `.opt'
    % file.
    globals.lookup_bool_option(Globals, intermod_unused_args, UnusedArgs),
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        StructureReuse),
    ( if
        ( UnusedArgs = yes
        ; StructureReuse = yes
        )
    then
        read_optimization_interfaces(Globals, no, [ModuleName], set.init,
            cord.empty, LocalItemBlocksCord, [], LocalSpecs, no, UA_SR_Error,
            !IO),
        LocalItemBlocks = cord.list(LocalItemBlocksCord),
        keep_only_unused_and_reuse_pragmas_in_blocks(UnusedArgs,
            StructureReuse, LocalItemBlocks, FilteredItemBlocks),
        module_and_imports_add_opt_item_blocks(FilteredItemBlocks,
            !ModuleAndImports),
        module_and_imports_add_specs(LocalSpecs, !ModuleAndImports)
    else
        UA_SR_Error = no
    ),

    % Read .int0 files required by the `.opt' files.
    map.init(HaveReadModuleMapInt),
    OptFileAncestors = set.power_union(set.map(get_ancestors_set, OptFiles)),
    Int0Files = set.delete(OptFileAncestors, ModuleName),
    process_int0_files_of_ancestor_modules(Globals, HaveReadModuleMapInt,
        "opt_int0s", make_ioms_opt_imported, make_ioms_opt_imported,
        module_and_imports_add_int_for_opt_item_blocks,
        Int0Files, set.init, AncestorImports1, set.init, AncestorImports2,
        !ModuleAndImports, !IO),

    % Figure out which .int files are needed by the .opt files
    get_dependencies_in_item_blocks(OptItemBlocks,
        NewImportDepsMap0, NewUseDepsMap0),
    get_implicit_dependencies_in_item_blocks(Globals, OptItemBlocks,
        NewImplicitImportDepsMap0, NewImplicitUseDepsMap0),
    set.sorted_list_to_set(map.keys(NewUseDepsMap0), NewUseDeps0),
    set.sorted_list_to_set(map.keys(NewImportDepsMap0), NewImportDeps0),
    set.sorted_list_to_set(map.keys(NewImplicitImportDepsMap0),
        NewImplicitImportDeps0),
    set.sorted_list_to_set(map.keys(NewImplicitUseDepsMap0),
        NewImplicitUseDeps0),
    NewDeps = set.union_list(
        [NewImportDeps0, NewUseDeps0,
        NewImplicitImportDeps0, NewImplicitUseDeps0,
        AncestorImports1, AncestorImports2]),

    % Read in the .int, and .int2 files needed by the .opt files.
    process_module_int123_files(Globals, HaveReadModuleMapInt,
        "opt_new_deps", pik_direct(int123_1, must_be_qualified),
        make_ioms_opt_imported, make_ioms_opt_imported,
        module_and_imports_add_int_for_opt_item_blocks,
        NewDeps, set.init, NewIndirectDeps, set.init, NewImplIndirectDeps,
        !ModuleAndImports, !IO),
    process_module_indirect_interfaces_and_impls_transitively(Globals,
        HaveReadModuleMapInt, "opt_new_indirect_deps", pik_indirect(int123_2),
        make_ioms_opt_imported, make_ioms_opt_imported,
        module_and_imports_add_int_for_opt_item_blocks,
        set.union(NewIndirectDeps, NewImplIndirectDeps),
        !ModuleAndImports, !IO),

    % Figure out whether anything went wrong.
    % XXX We should try to put all the relevant error indications into
    % !ModuleAndImports, and let our caller figure out what to do with them.
    module_and_imports_get_errors(!.ModuleAndImports, ModuleErrors),
    ( if
        ( set.is_non_empty(ModuleErrors)
        ; OptError = yes
        ; UA_SR_Error = yes
        )
    then
        FoundError = yes
    else
        FoundError = no
    ).

:- pred keep_only_unused_and_reuse_pragmas_in_blocks(bool::in, bool::in,
    list(item_block(MS))::in, list(item_block(MS))::out) is det.

keep_only_unused_and_reuse_pragmas_in_blocks(_, _, [], []).
keep_only_unused_and_reuse_pragmas_in_blocks(UnusedArgs, StructureReuse,
        [ItemBlock0 | ItemBlocks0], [ItemBlock | ItemBlocks]) :-
    ItemBlock0 = item_block(ModuleName, Section, Context,
        _Incls0, _Imports0, Items0),
    Incls = [],
    Imports = [],
    keep_only_unused_and_reuse_pragmas_acc(UnusedArgs, StructureReuse,
        Items0, cord.init, ItemCord),
    Items = cord.list(ItemCord),
    ItemBlock = item_block(ModuleName, Section, Context,
        Incls, Imports, Items),
    keep_only_unused_and_reuse_pragmas_in_blocks(UnusedArgs, StructureReuse,
        ItemBlocks0, ItemBlocks).

:- pred keep_only_unused_and_reuse_pragmas_acc(bool::in, bool::in,
    list(item)::in, cord(item)::in, cord(item)::out) is det.

keep_only_unused_and_reuse_pragmas_acc(_, _, [], !ItemCord).
keep_only_unused_and_reuse_pragmas_acc(UnusedArgs, StructureReuse,
        [Item0 | Items0], !ItemCord) :-
    ( if
        Item0 = item_pragma(ItemPragma0),
        ItemPragma0 = item_pragma_info(Pragma0, _, _, _),
        (
            UnusedArgs = yes,
            Pragma0 = pragma_unused_args(_)
        ;
            StructureReuse = yes,
            Pragma0 = pragma_structure_reuse(_)
        )
    then
        !:ItemCord = cord.snoc(!.ItemCord, Item0)
    else
        true
    ),
    keep_only_unused_and_reuse_pragmas_acc(UnusedArgs, StructureReuse, Items0,
        !ItemCord).

:- pred read_optimization_interfaces(globals::in, bool::in,
    list(module_name)::in, set(module_name)::in,
    cord(opt_item_block)::in, cord(opt_item_block)::out,
    list(error_spec)::in, list(error_spec)::out,
    bool::in, bool::out, io::di, io::uo) is det.

read_optimization_interfaces(_, _, [], _, !ItemBlocksCord,
        !Specs, !Error, !IO).
read_optimization_interfaces(Globals, Transitive,
        [ModuleToRead | ModulesToRead], ModulesProcessed0,
        !OptItemBlocksCord, !Specs, !Error, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
    maybe_write_string(VeryVerbose,
        "% Reading optimization interface for module", !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    ModuleToReadString = sym_name_to_string(ModuleToRead),
    maybe_write_string(VeryVerbose, ModuleToReadString, !IO),
    maybe_write_string(VeryVerbose, "'...\n", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    module_name_to_search_file_name(Globals, ".opt", ModuleToRead, FileName,
        !IO),
    actually_read_module_opt(ofk_opt, Globals, FileName, ModuleToRead, [],
        ParseTreeOpt, OptSpecs, OptError, !IO),
    ParseTreeOpt = parse_tree_opt(OptModuleName, OptFileKind,
        OptModuleContext, OptUses, OptItems),
    OptSection = oms_opt_imported(OptModuleName, OptFileKind),
    OptAvails = list.map(wrap_avail_use, OptUses),
    OptItemBlock = item_block(OptModuleName, OptSection, OptModuleContext,
        [], OptAvails, OptItems),
    !:OptItemBlocksCord = cord.snoc(!.OptItemBlocksCord, OptItemBlock),
    update_opt_error_status(Globals, opt_file, FileName, OptSpecs, OptError,
        !Specs, !Error),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
    maybe_write_string(VeryVerbose, "% done.\n", !IO),

    (
        Transitive = yes,
        NewUseDeps0 = set.list_to_set(
            list.map(avail_use_info_module_name, OptUses)),
        get_implicit_dependencies_in_items(Globals, OptItems,
            NewImplicitImportDepsMap0, NewImplicitUseDepsMap0),
        set.sorted_list_to_set(map.keys(NewImplicitImportDepsMap0),
            NewImplicitImportDeps0),
        set.sorted_list_to_set(map.keys(NewImplicitUseDepsMap0),
            NewImplicitUseDeps0),
        NewDepsSet0 = set.union_list([NewUseDeps0,
            NewImplicitImportDeps0, NewImplicitUseDeps0]),
        set.difference(NewDepsSet0, ModulesProcessed0, NewDepsSet),
        set.union(ModulesProcessed0, NewDepsSet, ModulesProcessed),
        set.to_sorted_list(NewDepsSet, NewDeps)
    ;
        Transitive = no,
        ModulesProcessed = ModulesProcessed0,
        NewDeps = []
    ),
    read_optimization_interfaces(Globals, Transitive,
        NewDeps ++ ModulesToRead, ModulesProcessed,
        !OptItemBlocksCord, !Specs, !Error, !IO).

%---------------------------------------------------------------------------%

grab_trans_opt_files(Globals, TransOptDeps, !Module, FoundError, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Reading .trans_opt files..\n", !IO),
    maybe_flush_output(Verbose, !IO),

    read_trans_opt_files(Globals, TransOptDeps,
        cord.empty, OptItemBlocksCord, [], OptSpecs, no, FoundError, !IO),

    OptItemBlocks = cord.list(OptItemBlocksCord),
    module_and_imports_add_opt_item_blocks(OptItemBlocks, !Module),
    module_and_imports_add_specs(OptSpecs, !Module),
    % XXX why ignore any existing errors?
    module_and_imports_set_errors(set.init, !Module),

    maybe_write_string(Verbose, "% Done.\n", !IO).

:- pred read_trans_opt_files(globals::in, list(module_name)::in,
    cord(opt_item_block)::in, cord(opt_item_block)::out,
    list(error_spec)::in, list(error_spec)::out,
    bool::in, bool::out, io::di, io::uo) is det.

read_trans_opt_files(_, [], !OptItemBlocks, !Specs, !Error, !IO).
read_trans_opt_files(Globals, [Import | Imports], !OptItemBlocks,
        !Specs, !Error, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
    maybe_write_string(VeryVerbose,
        "% Reading transitive optimization interface for module", !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    ImportString = sym_name_to_string(Import),
    maybe_write_string(VeryVerbose, ImportString, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    module_name_to_search_file_name(Globals, ".trans_opt", Import, FileName,
        !IO),
    actually_read_module_opt(ofk_trans_opt, Globals, FileName, Import, [],
        ParseTreeOpt, OptSpecs, OptError, !IO),
    maybe_write_string(VeryVerbose, " done.\n", !IO),
    !:Specs = OptSpecs ++ !.Specs,
    update_opt_error_status(Globals, trans_opt_file, FileName,
        OptSpecs, OptError, !Specs, !Error),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),

    ParseTreeOpt = parse_tree_opt(OptModuleName, _OptFileKind, OptContext,
        OptUses, OptItems),
    OptSection = oms_opt_imported(OptModuleName, ofk_trans_opt),
    OptAvails = list.map(wrap_avail_use, OptUses),
    OptItemBlock = item_block(OptModuleName, OptSection, OptContext,
        [], OptAvails, OptItems),
    !:OptItemBlocks = cord.snoc(!.OptItemBlocks, OptItemBlock),
    read_trans_opt_files(Globals, Imports, !OptItemBlocks,
        !Specs, !Error, !IO).

%---------------------------------------------------------------------------%

:- type opt_file_type
    --->    opt_file
    ;       trans_opt_file.

    % update_opt_error_status(Globals, OptFileType, FileName,
    %   ModuleSpecs, !Specs, ModuleErrors, !Error):
    %
    % Work out whether any fatal errors have occurred while reading
    % `.opt' or `.trans_opt' files, updating !Errors if there were
    % fatal errors.
    %
    % A missing `.opt' or `.trans_opt' file is only a fatal error if
    % `--halt-at-warn' was passed the compiler together with
    % `--warn-missing-opt-files' or `--warn-missing-trans-opt-files'
    % respectively.
    %
    % Syntax errors in these files are always fatal.
    %
:- pred update_opt_error_status(globals::in, opt_file_type::in, string::in,
    list(error_spec)::in, read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out, bool::in, bool::out) is det.

update_opt_error_status(_Globals, FileType, FileName,
        ModuleSpecs, ModuleErrors, !Specs, !Error) :-
    ( if set.is_empty(ModuleErrors) then
        % OptSpecs contains no errors. I (zs) don't know whether it could
        % contain any warnings or informational messages, but if it could,
        % we should add those error_specs to !Specs. Not doing so preserves
        % old behavior.
        true
    else if set.contains(ModuleErrors, rme_could_not_open_file) then
        % We get here if we couldn't find and/or open the file.
        % ModuleSpecs will already contain an error_severity error_spec
        % about that, with more details than the message we generate below,
        % but the test case hard_coded/intermod_unused_args insists on
        % there being no error, only a warning, and on the text below.
        % That is why we do not add ModuleSpecs to !Specs here.
        %
        % I (zs) don't know whether we should add a version of ModuleSpecs
        % with downgraded severity to !Specs instead of the Spec we generate
        % below.
        (
            FileType = opt_file,
            WarningOption = warn_missing_opt_files
        ;
            FileType = trans_opt_file,
            WarningOption = warn_missing_trans_opt_files
        ),
        Severity =
            severity_conditional(WarningOption, yes, severity_warning, no),
        Pieces = [option_is_set(WarningOption, yes,
            [always([words("Warning: cannot open"), quote(FileName),
                suffix("."), nl])])],
        Msg = error_msg(no, treat_as_first, 0, Pieces),
        Spec = error_spec(Severity, phase_read_files, [Msg]),
        !:Specs = [Spec | !.Specs]
        % NOTE: We do NOT update !Error, since a missing optimization
        % interface file is not necessarily an error.
    else
        % ModuleErrors may or may not contain fatal errors other than
        % rme_could_not_open_file, but we do not care.
        !:Specs = ModuleSpecs ++ !.Specs,
        !:Error = yes
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.modules.
%---------------------------------------------------------------------------%
