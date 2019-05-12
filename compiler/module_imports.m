%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_imports.m.
% Main author: fjh.
%
% This module contains the main data structure we use while augmenting
% a raw compilation unit. It records all the things that are imported,
% directly or indirectly, by the original raw compilation unit.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_imports.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module cord.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % When doing smart recompilation, we record, for each module,
    % which of its versions (source .m file, generated .int0/.int3/.int2/.int
    % file, or generated .opt/.transopt file) we read, and the modification
    % time of the file.
    %
    % We also record whether we expected the file we read to be
    % fully module qualified or not.
    %
    % XXX The handling of the map looks wrong to me (zs),
    % for two separate reasons.
    %
    % The first reason is that the need_qualifier field is set by
    % the code that *wants to read* a file, and is not a field
    % whose value should be filled in by *reading* the file.
    % (The timestamp field is properly filled in by reading
    % the file.)
    %
    % The second reason is that when we read in e.g. mod1.int, we simply
    % overwrite any existing entry in the module_timestamp_map for mod1.
    % I see no documented argument anywhere for any of the following
    % propositions, which could each make the above the right thing to do.
    %
    % - Proposition 1: when we add an entry for a module, the map
    %   cannot contain any previous entry for that module.
    %
    % - Proposition 2a: when we add an entry for a module, the map
    %   *can* contain a previous entry for the module, but for
    %   a file of that module than contains at most as much information
    %   as the previously-read-in file (as e.g. a .int2 file cannot
    %   contain more information than a .int file for the same
    %   module).
    %
    % - Proposition 2b: when we add an entry for a module, the map
    %   *can* contain a previous entry for the module, the need_qualifier
    %   field in the new entry is at least as restrictive as in
    %   the old entry. (This means that once we required the file
    %   we read for a module to be fully module qualified, we shouldn't
    %   later forget about that requirement.
    %
:- type module_timestamp_map == map(module_name, module_timestamp).
:- type module_timestamp
    --->    module_timestamp(
                mts_file_kind       :: file_kind,
                mts_timestamp       :: timestamp,
                mts_need_qualifier  :: need_qualifier
            ).

%---------------------------------------------------------------------------%

    % The `module_and_imports' structure holds information about
    % a module and the modules that it imports. We build this structure up
    % as we go along.
    %
:- type module_and_imports.

%---------------------------------------------------------------------------%
%
% The predicates that create module_and_imports structures.
%

    % This predicate is used by
    %
    %   deps_map.m
    %   generate_dep_d_files.m
    %   make.module_dep_file.m
    %
    % for building dependency maps between modules. The module_and_imports
    % structures it builds are not fully complete; only the fields
    % needed for that task are filled in.
    %
:- pred parse_tree_src_to_module_and_imports_list(globals::in, file_name::in,
    parse_tree_src::in, read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out,
    list(raw_compilation_unit)::out, list(module_and_imports)::out) is det.

    % This predicate is used by deps_map.m for building dependency maps
    % between modules, in case the reading of a module's source file
    % encounters a fatal error.
    %
:- pred parse_tree_int_to_module_and_imports(globals::in, file_name::in,
    parse_tree_int::in, read_module_errors::in,
    module_and_imports::out) is det.

:- pred rebuild_module_and_imports_for_dep_file(globals::in,
    module_and_imports::in, module_and_imports::out) is det.

    % make_module_and_imports(Globals, SourceFileName, SourceFileModuleName,
    %  ModuleName, ModuleNameContext, SrcItemBlocks0,
    %  PublicChildren, NestedChildren, FactDeps, ForeignIncludeFiles,
    %  ForeignExportLangs, HasMain, MaybeTimestampMap,
    %  ModuleAndImports):
    %
    % Construct a module_and_imports structure another way.
    % While the code that gets invoked when we make dependencies
    % calls init_module_and_imports, the code that gets invoked
    % when we generate interface files or target code uses this
    % predicate. This difference is (or at least should be) unnecessary;
    % we should build module_and_imports structures the same way
    % for both tasks.
    %
    % XXX ITEM_LIST This predicate is used by code in modules.m to create
    % a module_and_imports structure in what seems (to me, zs) to be
    % a partially filled in state.
    %
:- pred make_module_and_imports(globals::in, file_name::in,
    module_name::in, module_name::in, prog_context::in,
    list(src_item_block)::in, module_names_contexts::in,
    set(module_name)::in, list(string)::in, foreign_include_file_infos::in,
    set(foreign_language)::in, has_main::in,
    maybe(module_timestamp_map)::in, module_and_imports::out) is det.

    % Construct a module_and_imports structure for inclusion in
    % a module dependencies structure, for make.module_dep_file.m.
    % This structure is only partially filled in. I (zs) don't know
    % what rationale governs what fields need to be filled in, and when.
    %
:- pred make_module_dep_module_and_imports(string::in, string::in,
    module_name::in, module_name::in,
    list(module_name)::in, list(module_name)::in,
    list(module_name)::in, list(module_name)::in,
    list(module_name)::in, list(string)::in,
    list(foreign_import_module_info)::in, list(foreign_include_file_info)::in,
    contains_foreign_code::in, contains_foreign_export::in,
    has_main::in, module_and_imports::out) is det.

%---------------------------------------------------------------------------%
%
% Getter and setter predicates for the module_and_imports structure.
%

:- pred module_and_imports_get_source_file_name(module_and_imports::in,
    file_name::out) is det.
:- pred module_and_imports_get_source_file_dir(module_and_imports::in,
    dir_name::out) is det.
:- pred module_and_imports_get_source_file_module_name(module_and_imports::in,
    module_name::out) is det.
:- pred module_and_imports_get_module_name(module_and_imports::in,
    module_name::out) is det.
:- pred module_and_imports_get_module_name_context(module_and_imports::in,
    prog_context::out) is det.
:- pred module_and_imports_get_ancestors(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_children_map(module_and_imports::in,
    module_names_contexts::out) is det.
:- pred module_and_imports_get_public_children_map(module_and_imports::in,
    module_names_contexts::out) is det.
:- pred module_and_imports_get_nested_children(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_int_deps_map(module_and_imports::in,
    module_names_contexts::out) is det.
:- pred module_and_imports_get_imp_deps_map(module_and_imports::in,
    module_names_contexts::out) is det.
:- pred module_and_imports_get_indirect_deps(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_fact_table_deps(module_and_imports::in,
    list(string)::out) is det.
:- pred module_and_imports_get_foreign_import_modules(module_and_imports::in,
    foreign_import_modules::out) is det.
:- pred module_and_imports_get_foreign_include_files(module_and_imports::in,
    foreign_include_file_infos::out) is det.
:- pred module_and_imports_get_contains_foreign_code(module_and_imports::in,
    contains_foreign_code::out) is det.
:- pred module_and_imports_get_contains_foreign_export(module_and_imports::in,
    contains_foreign_export::out) is det.
:- pred module_and_imports_get_has_main(module_and_imports::in,
    has_main::out) is det.
:- pred module_and_imports_get_src_blocks(module_and_imports::in,
    list(src_item_block)::out) is det.
:- pred module_and_imports_get_direct_int_blocks_cord(module_and_imports::in,
    cord(int_item_block)::out) is det.
:- pred module_and_imports_get_indirect_int_blocks_cord(module_and_imports::in,
    cord(int_item_block)::out) is det.
:- pred module_and_imports_get_opt_blocks_cord(module_and_imports::in,
    cord(opt_item_block)::out) is det.
:- pred module_and_imports_get_int_for_opt_blocks_cord(module_and_imports::in,
    cord(int_for_opt_item_block)::out) is det.
:- pred module_and_imports_get_maybe_timestamp_map(module_and_imports::in,
    maybe(module_timestamp_map)::out) is det.
:- pred module_and_imports_get_errors(module_and_imports::in,
    read_module_errors::out) is det.

    % XXX It should NOT be necessary to set the ancestors
    % after the module_and_imports structure is initially created.
:- pred module_and_imports_set_ancestors(set(module_name)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_int_deps_map(module_names_contexts::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_imp_deps_map(module_names_contexts::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_indirect_deps(set(module_name)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_foreign_import_modules(
    foreign_import_modules::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_maybe_timestamp_map(
    maybe(module_timestamp_map)::in,
    module_and_imports::in, module_and_imports::out) is det.
    % XXX It should NOT be necessary to set the read_module_errors field;
    % the predicates below that only *add* to the set of errors
    % should be sufficient.
:- pred module_and_imports_set_errors(read_module_errors::in,
    module_and_imports::in, module_and_imports::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates for getting information from module_and_imports structures.
%

:- pred module_and_imports_get_children(module_and_imports::in,
    list(module_name)::out) is det.
:- pred module_and_imports_get_children_set(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_int_deps(module_and_imports::in,
    list(module_name)::out) is det.
:- pred module_and_imports_get_int_deps_set(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_imp_deps(module_and_imports::in,
    list(module_name)::out) is det.
:- pred module_and_imports_get_imp_deps_set(module_and_imports::in,
    set(module_name)::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates for adding information to module_and_imports structures.
%

:- pred module_and_imports_add_ancestor(module_name::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_imp_dep(module_name::in, prog_context::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_indirect_dep(module_name::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_direct_int_item_blocks(
    list(int_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_indirect_int_item_blocks(
    list(int_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_opt_item_blocks(
    list(opt_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_int_for_opt_item_blocks(
    list(int_for_opt_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_maybe_add_module_version_numbers(
    module_name::in, maybe(version_numbers)::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_specs(list(error_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_interface_error(read_module_errors::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_specs_errors(
    list(error_spec)::in, read_module_errors::in,
    module_and_imports::in, module_and_imports::out) is det.

%---------------------------------------------------------------------------%
%
% The predicates that return the contents of module_and_imports structures.
%

    % Return the parts of the given module_and_imports structure
    % that we need to put into an automatically generated .d file.
    %
:- pred module_and_imports_d_file(module_and_imports::in,
    file_name::out, module_name::out, module_name::out,
    set(module_name)::out, module_names_contexts::out, set(module_name)::out,
    module_names_contexts::out, module_names_contexts::out,
    set(module_name)::out, list(string)::out,
    foreign_import_modules::out, foreign_include_file_infos::out,
    contains_foreign_code::out, list(src_item_block)::out,
    cord(int_item_block)::out, cord(int_item_block)::out,
    cord(opt_item_block)::out, cord(int_for_opt_item_block)::out) is det.

    % Return the results recorded in the module_and_imports structure.
    %
    % There is no predicate to return *just* the items, since that would
    % allow callers to forget to retrieve and print the error messages.
    %
:- pred module_and_imports_get_aug_comp_unit(module_and_imports::in,
    aug_compilation_unit::out, list(error_spec)::out, read_module_errors::out)
    is det.

%---------------------------------------------------------------------------%

:- pred write_mai_stats(io.output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.split_parse_tree_src.

:- import_module dir.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

    % When generating the dependencies (for `--generate-dependencies'), the
    % two fields that hold the direct imports do not include the imports via
    % ancestors when the module is first read in; the ancestor imports are
    % added later, once all the modules have been read in. Similarly the
    % indirect imports field is initially set to the empty list and filled
    % in later.
    %
    % When compiling or when making interface files, the same sort of thing
    % applies: initially all the fields containing module names except the
    % public children field are set to contain no modules, and then
    % we add ancestor modules and imported modules to their respective fields
    % as we process the interface files for those imported or ancestor modules.
    %
    % The mai_int_deps and mai_imp_deps fields record, for each imported
    % module name, the list of contexts in which those imports occur.
    % (Most modules are imported just once, but you may import a module
    % more than once.) The contexts are used when printing error messages
    % about unexpected module names,which most of the time are about
    % unexpected module *qualifications* of those module names.
    % When "mmc --generate-dependencies" first finds an import of module a.c,
    % and then later finds that c.m contains module b.c, not module a.c,
    % and the name a.c happens to b correct (which is usually the case after
    % moving module c from package a to package b), the programmer will
    % probably want to know *where* he/she needs to change a.c to b.c.
    %
    % Since ":- include_module" declarations act similarly in establishing
    % such expectations, the mai_children and mai_public_children fields
    % also record their contexts. Every child module should have exactly one
    % include_module declaration for it, but we record a *list* of contexts
    % for each child module anyway, to allow them to be treated the same way
    % as the mai_int_deps and mai_imp_deps fields.
    %
    % For some imports (e.g. implicit imports), there is no valid context
    % we can record. For those, we record term.context_init instead.
    % (We can't record no context at all, since multi_map makes no distinction
    % between a key in the map that has a empty list of associated values,
    % and a key that is not in the map at all. We *do* want to keep a record
    % of every imported module, even if we have no context for the import.)
    %
:- type module_and_imports
    --->    module_and_imports(
                % The name of the source file and directory
                % containing the module source.
                mai_source_file_name            :: file_name,
                mai_module_dir                  :: dir_name,

                % The name of the top-level module in the above source file.
                mai_source_file_module_name     :: module_name,

                % The module that we are compiling. This may be
                % mai_source_file_module_name, or it may be one of its
                % descendant modules (child modules, grandchild modules, etc).
                mai_module_name                 :: module_name,

                % The context of the module declaration of mai_module_name.
                mai_module_name_context         :: prog_context,

                % The set of mai_module_name's ancestor modules.
                mai_ancestors                   :: set(module_name),

                mai_children                    :: module_names_contexts,

                % The set of its public children, i.e. child modules that
                % it includes in the interface section.
                mai_public_children             :: module_names_contexts,

                % The modules included in the same source file. This field
                % is only set for the top-level module in each file.
                mai_nested_children             :: set(module_name),

                % The set of modules it directly imports in the interface
                % (imports via ancestors count as direct).
                mai_int_deps_map                :: module_names_contexts,

                % The set of modules it directly imports in the
                % implementation.
                mai_imp_deps_map                :: module_names_contexts,

                % The set of modules it indirectly imports.
                mai_indirect_deps               :: set(module_name),

                % The list of filenames for fact tables in this module.
                mai_fact_table_deps             :: list(string),

                % The information from `:- pragma foreign_import_module'
                % declarations.
                mai_foreign_import_modules      :: foreign_import_modules,

                % The list of filenames referenced by `:- pragma foreign_decl'
                % or `:- pragma foreign_code' declarations.
                mai_foreign_include_files       :: foreign_include_file_infos,

                % Whether or not the module contains foreign code, and if yes,
                % which languages they use.
                mai_contains_foreign_code       :: contains_foreign_code,

                % Does the module contain any `:- pragma foreign_export'
                % declarations?
                mai_contains_foreign_export     :: contains_foreign_export,

                % Does this module contain main/2?
                mai_has_main                    :: has_main,

                % The contents of the module and its imports.
                mai_src_blocks                  :: list(src_item_block),
                mai_direct_int_blocks_cord      :: cord(int_item_block),
                mai_indirect_int_blocks_cord    :: cord(int_item_block),
                mai_opt_blocks_cord             :: cord(opt_item_block),
                mai_int_for_opt_blocks_cord     :: cord(
                                                    int_for_opt_item_block),

                mai_version_numbers_map         :: module_version_numbers_map,

                % If we are doing smart recompilation, we need to keep
                % the timestamps of the modules read in.
                mai_maybe_timestamp_map         :: maybe(module_timestamp_map),

                % Whether an error has been encountered when reading in
                % this module.
                mai_specs                       :: list(error_spec),
                mai_errors                      :: read_module_errors,

                mai_construction_method         :: mai_construction_method
            ).

:- type mai_construction_method
    --->    mcm_init
    ;       mcm_make
    ;       mcm_read.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

parse_tree_src_to_module_and_imports_list(Globals, SourceFileName,
        ParseTreeSrc, ReadModuleErrors, !Specs,
        RawCompUnits, ModuleAndImportsList) :-
    split_into_compilation_units_perform_checks(ParseTreeSrc, RawCompUnits,
        !Specs),
    ParseTreeSrc = parse_tree_src(TopModuleName, _, _),
    CompUnitModuleNames = set.list_to_set(
        list.map(raw_compilation_unit_project_name, RawCompUnits)),
    MAISpecs0 = [],
    list.map(
        init_module_and_imports(Globals, SourceFileName, TopModuleName,
            CompUnitModuleNames, MAISpecs0, ReadModuleErrors),
        RawCompUnits, ModuleAndImportsList).

parse_tree_int_to_module_and_imports(Globals, IntFileName,
        ParseTreeInt, ReadModuleErrors, ModuleAndImports) :-
    ParseTreeInt = parse_tree_int(ModuleName, _, ModuleContext,
        _MaybeVersionNumbers, IntIncl, ImpIncls, IntAvails, ImpAvails,
        IntItems, ImpItems),
    int_imp_items_to_item_blocks(ModuleName,
        ms_interface, ms_implementation, IntIncl, ImpIncls,
        IntAvails, ImpAvails, IntItems, ImpItems, RawItemBlocks),
    RawCompUnit =
        raw_compilation_unit(ModuleName, ModuleContext, RawItemBlocks),

    CompUnitModuleNames = set.make_singleton_set(ModuleName),
    MAISpecs0 = [],
    init_module_and_imports(Globals, IntFileName, ModuleName,
        CompUnitModuleNames, MAISpecs0, ReadModuleErrors,
        RawCompUnit, ModuleAndImports).

rebuild_module_and_imports_for_dep_file(Globals,
        ModuleAndImports0, ModuleAndImports) :-
    % Make sure all the required fields are filled in.
    % XXX ITEM_LIST Why build a NEW ModuleAndImports? Wouldn't modifying
    % ModuleAndImports0 be easier and clearer?
    module_and_imports_get_aug_comp_unit(ModuleAndImports0, AugCompUnit,
        Specs, _Errors),
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        _DirectIntItemBlocksCord, _IndirectIntItemBlocksCord,
        _OptItemBlocksCord, _IntForOptItemBlocksCord),
    convert_back_to_raw_item_blocks(SrcItemBlocks, RawItemBlocks),
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    module_and_imports_get_source_file_name(ModuleAndImports0,
        SourceFileName),
    module_and_imports_get_source_file_module_name(ModuleAndImports0,
        SourceFileModuleName),
    module_and_imports_get_nested_children(ModuleAndImports0,
        NestedChildren),
    set.init(ReadModuleErrors0),
    init_module_and_imports(Globals, SourceFileName, SourceFileModuleName,
        NestedChildren, Specs, ReadModuleErrors0, RawCompUnit,
        ModuleAndImports).

:- pred convert_back_to_raw_item_blocks(list(src_item_block)::in,
    list(raw_item_block)::out) is det.

convert_back_to_raw_item_blocks([], []).
convert_back_to_raw_item_blocks([SrcItemBlock | SrcItemBlocks],
        [RawItemBlock | RawItemBlocks]) :-
    SrcItemBlock = item_block(ModuleName, SrcSection, Incls, Avails, Items),
    (
        SrcSection = sms_interface,
        RawSection = ms_interface
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        RawSection = ms_implementation
    ),
    RawItemBlock = item_block(ModuleName, RawSection, Incls, Avails, Items),
    convert_back_to_raw_item_blocks(SrcItemBlocks, RawItemBlocks).

%---------------------------------------------------------------------------%

    % init_module_and_imports(Globals, FileName, SourceFileModuleName,
    %   NestedModuleNames, Specs, Errors, RawCompUnit, ModuleAndImports):
    %
    % Initialize a module_and_imports structure.
    %
    % We do this just after we have read in a raw compulation unit.
    % Later code, mostly in modules.m but in some other modules as well,
    % then calls the module_and_imports_{add,set}_* predicates above
    % to record more information (mostly from read-in interface files)
    % to the module_and_imports structure. When all such modifications
    % are done, the module_and_imports_get_aug_comp_unit predicate
    % will extract the augmented compilation unit from the updated
    % module_and_imports structure.
    %
:- pred init_module_and_imports(globals::in, file_name::in, module_name::in,
    set(module_name)::in, list(error_spec)::in, read_module_errors::in,
    raw_compilation_unit::in, module_and_imports::out) is det.

init_module_and_imports(Globals, FileName, SourceFileModuleName,
        NestedModuleNames, Specs, Errors, RawCompUnit, ModuleAndImports) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    Ancestors = get_ancestors(ModuleName),

    % NOTE This if-then-else looks strange, but it works. It works for
    % two different reasons in our callers' two different use cases.
    %
    % Use case 1: our callers parse_tree_src_to_module_and_imports_list
    % and parse_tree_int_to_module_and_imports pass to us in the
    % NestedModuleNames argument the set of the names of all the modules
    % that are nested within a single source file.
    % If the module we are currently processing is the top module in that
    % source file, then all the other modules in NestedModuleNames are
    % its (direct or indirect) submodules. If it is not the top module,
    % then this field is *supposed* to be empty.
    %
    % Use case 2: our caller rebuild_module_and_imports_for_dep_file
    % passes to as NestedModuleNames the value of the same field
    % of the original module_and_imports structure being rebuilt.
    % This works because the operation of this if-then-else
    % is idempotent.
    ( if ModuleName = SourceFileModuleName then
        set.delete(ModuleName, NestedModuleNames, NestedDeps)
    else
        set.init(NestedDeps)
    ),

    % We don't fill in the indirect dependencies yet.
    set.init(IndirectDeps),

    % NOTE There are two predicates that build an initial module_and_imports
    % structure. One is this predicate, init_module_and_imports, which is
    % called by compiler invocations that want to find out the dependencies
    % between modules.  The other is make_module_and_imports, which is
    % called by compiler invocations that want to generate target language
    % code.
    %
    % These two predicates fill in the module_and_imports structure
    % differently. Some fields, such as HasMain, are not needed during
    % code generation, and thus are not filled in meaningfully
    % by make_module_and_imports; some, such as SrcItemBlocks, are needed
    % *only* during code generation, and are thus not filled in
    % meaningfully by init_module_and_imports. This should be OK,
    % though there should be a mechanism to catch accesses to
    % not-meaningfully-filled-in fields.
    %
    % XXX However, the two predicates used to use different algorithms
    % to fill in some of the remaining fields as well. These differences
    % are almost certainly bugs, caused by the opacity of this code.
    % We want to move towards filling in these fields using the *same* code.
    % Unless there is a specific reason against it, the code we want to base
    % the common code on is the code used by (the callers of)
    % make_module_and_imports. This is because the code used by the
    % make_module_and_imports approach is more likely to be correct.
    % The reason for that is that errors during code generation are
    % much more likely to be noticed than errors in the computation
    % of dependencies (even mmc does not *force* e.g. an interface file to be
    % up to date, that interface file may *happen* be up-to-date anyway).
    %
    % We now compute the values of most fields using the approach used
    % by make_module_and_imports, XXX BUT we exempt the values of
    % the LangSet, ForeignImports, and ContainsForeignExport fields
    % from this. For them, we keep using only the old code in
    % init_module_and_imports, because the new code, which uses
    % the infrastructure used by grab_*modules_* in modules.m,
    % the main callers of make_module_and_imports, fills them
    % quite differently. These differences, which should be looked at
    % one by one, include the following, and maybe others:
    % for items involving foreign languages:
    %   get_foreign_code_indicators_from_item_blocks gathers
    %       two sets of languages
    %   get_implicits_foreigns_fact_tables_acc gathers one set
    % for some kinds of items involving foreign languages:
    %   get_foreign_code_indicators_from_item_blocks cares about
    %       the set of languages supported on the current backend
    %   get_implicits_foreigns_fact_tables_acc does not care
    % for foreign_import_module:
    %   get_foreign_code_indicators_from_item_blocks does something
    %   get_implicits_foreigns_fact_tables_acc does nothing
    % for pragma/foreign_export_enum,
    %   get_foreign_code_indicators_from_item_blocks records presence
    %   get_implicits_foreigns_fact_tables_acc does not

    % XXX START OF THE REMAINS OF THE OLD CODE
    % Figure out whether the items contain foreign code.
    get_foreign_code_indicators_from_item_blocks(Globals, RawItemBlocks,
        LangSet, ForeignImports0, ForeignIncludeFilesCord,
        ContainsForeignExport),

    % If this module contains `:- pragma foreign_export' or
    % `:- pragma foreign_type' declarations, importing modules may need
    % to import its `.mh' file.
    get_foreign_self_imports_from_item_blocks(RawItemBlocks, SelfImportLangs),
    list.foldl(
        ( pred(Lang::in, FIM0::in, FIM::out) is det :-
            add_foreign_import_module(Lang, ModuleName, FIM0, FIM)
        ), SelfImportLangs, ForeignImports0, ForeignImports),
    % XXX END OF THE REMAINS OF THE OLD CODE

    get_raw_components(RawItemBlocks, IntIncls, ImpIncls,
        IntAvails, ImpAvails, IntItems, ImpItems),
    list.foldl(get_included_modules_in_item_include_acc, IntIncls,
        multi_map.init, PublicChildrenMap),
    list.foldl(get_included_modules_in_item_include_acc, ImpIncls,
        PublicChildrenMap, ChildrenMap),
    get_imports_uses_maps(IntAvails, IntImportsMap0, IntUsesMap0),
    get_imports_uses_maps(ImpAvails, ImpImportsMap0, ImpUsesMap0),

    get_implicits_foreigns_fact_tables(IntItems, ImpItems,
        IntImplicitImportNeeds, IntImpImplicitImportNeeds, Contents),
    Contents = item_contents(NewForeignInclFilesCord, FactTables, _NewLangs,
        NewForeignExportLangs, HasMain),
    set.to_sorted_list(FactTables, SortedFactTables),
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    set.intersect(set.list_to_set(BackendLangs), NewForeignExportLangs,
        NewBackendFELangs),
    ( if set.is_empty(NewBackendFELangs) then
        NewContainsForeignExport = contains_no_foreign_export
    else
        NewContainsForeignExport = contains_foreign_export
    ),
    expect(unify(ContainsForeignExport, NewContainsForeignExport), $pred,
        "bad ContainsForeignExport"),

    compute_implicit_import_needs(Globals, IntImplicitImportNeeds,
        ImplicitIntImports, ImplicitIntUses),
    compute_implicit_import_needs(Globals, IntImpImplicitImportNeeds,
        ImplicitIntImpImports, ImplicitIntImpUses),
    set.difference(ImplicitIntImpImports, ImplicitIntImports,
        ImplicitImpImports),
    set.difference(ImplicitIntImpUses, ImplicitIntUses,
        ImplicitImpUses),

    set.fold(multi_map.reverse_set(term.context_init),
        ImplicitIntImports, IntImportsMap0, IntImportsMap),
    set.fold(multi_map.reverse_set(term.context_init),
        ImplicitIntUses, IntUsesMap0, IntUsesMap),
    set.fold(multi_map.reverse_set(term.context_init),
        ImplicitImpImports, ImpImportsMap0, ImpImportsMap),
    set.fold(multi_map.reverse_set(term.context_init),
        ImplicitImpUses, ImpUsesMap0, ImpUsesMap),

    multi_map.merge(IntImportsMap, IntUsesMap, IntDepsMap),
    multi_map.merge(ImpImportsMap, ImpUsesMap, ImpDepsMap),
    multi_map.merge(IntDepsMap, ImpDepsMap, IntImpDepsMap),

    ForeignIncludeFiles = cord.list(ForeignIncludeFilesCord),
    NewForeignInclFiles = cord.list(NewForeignInclFilesCord),
    expect(unify(ForeignIncludeFiles, NewForeignInclFiles), $pred,
        "bad ForeignIncludeFiles"),

    ( if set.is_empty(LangSet) then
        ContainsForeignCode = contains_no_foreign_code
    else
        ContainsForeignCode = contains_foreign_code(LangSet)
    ),

    % XXX ITEM_LIST SrcItemBlocks and the item block fields are NOT
    % stored here, per the documentation above.
    SrcItemBlocks = [],
    DirectIntBlocksCord = cord.init,
    IndirectIntBlocksCord = cord.init,
    OptBlocksCords = cord.init,
    IntForOptBlocksCords = cord.init,

    map.init(VersionNumbers),
    MaybeTimestampMap = no,
    ModuleAndImports = module_and_imports(FileName, dir.this_directory,
        SourceFileModuleName, ModuleName, ModuleNameContext,
        set.list_to_set(Ancestors), ChildrenMap, PublicChildrenMap,
        NestedDeps, IntDepsMap, IntImpDepsMap, IndirectDeps,
        SortedFactTables, ForeignImports, ForeignIncludeFilesCord,
        ContainsForeignCode, ContainsForeignExport, HasMain,
        SrcItemBlocks, DirectIntBlocksCord, IndirectIntBlocksCord,
        OptBlocksCords, IntForOptBlocksCords,
        VersionNumbers, MaybeTimestampMap, Specs, Errors, mcm_init).

%---------------------------------------------------------------------------%

make_module_and_imports(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, ModuleNameContext, SrcItemBlocks,
        PublicChildrenMap, NestedChildren, FactDeps, ForeignIncludeFiles,
        ForeignExportLangs, HasMain,
        MaybeTimestampMap, ModuleAndImports) :-
    set.init(Ancestors),
    map.init(IntDeps),
    map.init(ImpDeps),
    set.init(IndirectDeps),
    % XXX Since PublicChildrenMap should be a subset of ChildrenMap,
    % this looks like bug.
    map.init(ChildrenMap),
    ForeignImports = init_foreign_import_modules,
    map.init(VersionNumbers),
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    set.intersect(set.list_to_set(BackendLangs), ForeignExportLangs,
        BackendFELangs),
    ( if set.is_empty(BackendFELangs) then
        ContainsForeignExport = contains_no_foreign_export
    else
        ContainsForeignExport = contains_foreign_export
    ),
    Specs = [],
    set.init(Errors),
    ModuleAndImports = module_and_imports(SourceFileName, dir.this_directory,
        SourceFileModuleName, ModuleName, ModuleNameContext,
        Ancestors, ChildrenMap, PublicChildrenMap, NestedChildren,
        IntDeps, ImpDeps, IndirectDeps, FactDeps,
        ForeignImports, ForeignIncludeFiles,
        contains_foreign_code_unknown, ContainsForeignExport, HasMain,
        SrcItemBlocks, cord.init, cord.init, cord.init, cord.init,
        VersionNumbers, MaybeTimestampMap, Specs, Errors, mcm_make).

%---------------------------------------------------------------------------%

make_module_dep_module_and_imports(SourceFileName, ModuleDir,
        SourceFileModuleName, ModuleName,
        Ancestors, Children, NestedChildren, IntDeps, ImpDeps, FactDeps,
        ForeignImports, ForeignIncludes,
        ContainsForeignCode, ContainsForeignExport, HasMain,
        ModuleAndImports) :-
    ModuleNameContext = term.context_init,
    AddDummyContext = ( func(MN) = MN - [term.context_init] ),
    multi_map.from_assoc_list(list.map(AddDummyContext, IntDeps),
        IntDepsContexts),
    multi_map.from_assoc_list(list.map(AddDummyContext, ImpDeps),
        ImpDepsContexts),
    set.init(IndirectDeps),
    multi_map.from_assoc_list(list.map(AddDummyContext, Children),
        ChildrenContexts),
    % XXX We do not know which child modules are public, so saying
    % none of them are is likely to be a bug.
    multi_map.init(PublicChildrenContexts),
    list.foldl(add_foreign_import_module_info, ForeignImports,
        init_foreign_import_modules, ForeignImportModules),
    SrcItemBlocks = [],
    DirectIntItemBlocksCord = cord.empty,
    IndirectIntItemBlocksCord = cord.empty,
    OptItemBlocksCord = cord.empty,
    IntForOptItemBlocksCord = cord.empty,
    map.init(ModuleVersionNumbers),
    Specs = [],
    set.init(Errors),
    MaybeTimestamps = no,
    ModuleAndImports = module_and_imports(SourceFileName, ModuleDir,
        SourceFileModuleName, ModuleName, ModuleNameContext,
        set.list_to_set(Ancestors), ChildrenContexts, PublicChildrenContexts,
        set.list_to_set(NestedChildren),
        IntDepsContexts, ImpDepsContexts, IndirectDeps,
        FactDeps,
        ForeignImportModules, cord.from_list(ForeignIncludes),
        ContainsForeignCode, ContainsForeignExport, HasMain,
        SrcItemBlocks, DirectIntItemBlocksCord, IndirectIntItemBlocksCord,
        OptItemBlocksCord, IntForOptItemBlocksCord,
        ModuleVersionNumbers, MaybeTimestamps, Specs, Errors, mcm_read).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred module_and_imports_get_version_numbers_map(module_and_imports::in,
    module_version_numbers_map::out) is det.
:- pred module_and_imports_get_specs(module_and_imports::in,
    list(error_spec)::out) is det.

module_and_imports_get_source_file_name(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_source_file_name := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_source_file_name := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_source_file_name := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_source_file_name
    ).
module_and_imports_get_source_file_dir(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_module_dir := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_module_dir := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_module_dir := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_module_dir
    ).
module_and_imports_get_source_file_module_name(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_source_file_module_name := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_source_file_module_name := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_source_file_module_name := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_source_file_module_name
    ).
module_and_imports_get_module_name(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_module_name := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_module_name := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_module_name := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_module_name
    ).
module_and_imports_get_module_name_context(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_module_name_context := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_module_name_context := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_module_name_context := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_module_name_context
    ).
module_and_imports_get_ancestors(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_ancestors := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_ancestors := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_ancestors := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_ancestors
    ).
module_and_imports_get_children_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_children := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_children := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_children := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_children
    ).
module_and_imports_get_public_children_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_public_children := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_public_children := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_public_children := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_public_children
    ).
module_and_imports_get_nested_children(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_nested_children := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_nested_children := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_nested_children := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_nested_children
    ).
module_and_imports_get_int_deps_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_int_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_int_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_int_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_int_deps_map
    ).
module_and_imports_get_imp_deps_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_imp_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_imp_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_imp_deps_map := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_imp_deps_map
    ).
module_and_imports_get_indirect_deps(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_indirect_deps := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_indirect_deps := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_indirect_deps := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_indirect_deps
    ).
module_and_imports_get_fact_table_deps(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_fact_table_deps := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_fact_table_deps := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_fact_table_deps := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_fact_table_deps
    ).
module_and_imports_get_foreign_import_modules(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_foreign_import_modules := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_foreign_import_modules := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_foreign_import_modules := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_foreign_import_modules
    ).
module_and_imports_get_foreign_include_files(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_foreign_include_files := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_foreign_include_files := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_foreign_include_files := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_foreign_include_files
    ).
module_and_imports_get_contains_foreign_code(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_contains_foreign_code := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_contains_foreign_code := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_contains_foreign_code := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_contains_foreign_code
    ).
module_and_imports_get_contains_foreign_export(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_contains_foreign_export := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_contains_foreign_export := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_contains_foreign_export := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_contains_foreign_export
    ).
module_and_imports_get_has_main(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_has_main := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_has_main := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_has_main := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_has_main
    ).
module_and_imports_get_src_blocks(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_src_blocks := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_src_blocks := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_src_blocks := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_src_blocks
    ).
module_and_imports_get_direct_int_blocks_cord(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_direct_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_direct_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_direct_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_direct_int_blocks_cord
    ).
module_and_imports_get_indirect_int_blocks_cord(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_indirect_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_indirect_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_indirect_int_blocks := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_indirect_int_blocks_cord
    ).
module_and_imports_get_opt_blocks_cord(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_opt_blocks_cord
    ).
module_and_imports_get_int_for_opt_blocks_cord(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_int_for_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_int_for_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_int_for_opt_blocks := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_int_for_opt_blocks_cord
    ).
module_and_imports_get_version_numbers_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_version_numbers_map := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_version_numbers_map := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_version_numbers_map := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_version_numbers_map
    ).
module_and_imports_get_maybe_timestamp_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_maybe_timestamp_map := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_maybe_timestamp_map := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_maybe_timestamp_map := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_maybe_timestamp_map
    ).
module_and_imports_get_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_specs := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_specs := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_specs := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_specs
    ).
module_and_imports_get_errors(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Method = ModuleAndImports ^ mai_construction_method,
            (
                Method = mcm_init,
                Fields0 = Accesses0 ^ mfk_init,
                Fields = Fields0 ^ mf_errors := accessed,
                Accesses = Accesses0 ^ mfk_init := Fields
            ;
                Method = mcm_make,
                Fields0 = Accesses0 ^ mfk_make,
                Fields = Fields0 ^ mf_errors := accessed,
                Accesses = Accesses0 ^ mfk_make := Fields
            ;
                Method = mcm_read,
                Fields0 = Accesses0 ^ mfk_read,
                Fields = Fields0 ^ mf_errors := accessed,
                Accesses = Accesses0 ^ mfk_read := Fields
            ),
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_errors
    ).

:- pred module_and_imports_set_direct_int_blocks_cord(
    cord(int_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_indirect_int_blocks_cord(
    cord(int_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_opt_blocks_cord(
    cord(opt_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_int_for_opt_blocks_cord(
    cord(int_for_opt_item_block)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_version_numbers_map(
    module_version_numbers_map::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_specs(list(error_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.

module_and_imports_set_ancestors(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_ancestors := X.
module_and_imports_set_int_deps_map(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_int_deps_map := X.
module_and_imports_set_imp_deps_map(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_imp_deps_map := X.
module_and_imports_set_indirect_deps(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_indirect_deps := X.
module_and_imports_set_direct_int_blocks_cord(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_direct_int_blocks_cord := X.
module_and_imports_set_indirect_int_blocks_cord(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_indirect_int_blocks_cord := X.
module_and_imports_set_opt_blocks_cord(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_opt_blocks_cord := X.
module_and_imports_set_int_for_opt_blocks_cord(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_int_for_opt_blocks_cord := X.
module_and_imports_set_foreign_import_modules(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_foreign_import_modules := X.
module_and_imports_set_version_numbers_map(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_version_numbers_map := X.
module_and_imports_set_maybe_timestamp_map(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_maybe_timestamp_map := X.
module_and_imports_set_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_specs := X.
module_and_imports_set_errors(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_errors := X.

%---------------------------------------------------------------------------%

module_and_imports_get_children(ModuleAndImports, Children) :-
    module_and_imports_get_children_map(ModuleAndImports, ChildrenMap),
    Children = multi_map.keys(ChildrenMap).

module_and_imports_get_children_set(ModuleAndImports, Children) :-
    module_and_imports_get_children_map(ModuleAndImports, ChildrenMap),
    Children = set.sorted_list_to_set(multi_map.keys(ChildrenMap)).

module_and_imports_get_int_deps(ModuleAndImports, IntDeps) :-
    module_and_imports_get_int_deps_map(ModuleAndImports, IntDepsMap),
    IntDeps = multi_map.keys(IntDepsMap).

module_and_imports_get_int_deps_set(ModuleAndImports, IntDeps) :-
    module_and_imports_get_int_deps_map(ModuleAndImports, IntDepsMap),
    IntDeps = set.sorted_list_to_set(multi_map.keys(IntDepsMap)).

module_and_imports_get_imp_deps(ModuleAndImports, ImpDeps) :-
    module_and_imports_get_imp_deps_map(ModuleAndImports, ImpDepsMap),
    ImpDeps = multi_map.keys(ImpDepsMap).

module_and_imports_get_imp_deps_set(ModuleAndImports, ImpDeps) :-
    module_and_imports_get_imp_deps_map(ModuleAndImports, ImpDepsMap),
    ImpDeps = set.sorted_list_to_set(multi_map.keys(ImpDepsMap)).

%---------------------------------------------------------------------------%

module_and_imports_add_ancestor(ModuleName, !ModuleAndImports) :-
    module_and_imports_get_ancestors(!.ModuleAndImports, Ancestors0),
    set.insert(ModuleName, Ancestors0, Ancestors),
    module_and_imports_set_ancestors(Ancestors, !ModuleAndImports).

module_and_imports_add_imp_dep(ModuleName, Context, !ModuleAndImports) :-
    module_and_imports_get_imp_deps_map(!.ModuleAndImports, ImpDepsMap0),
    multi_map.add(ModuleName, Context, ImpDepsMap0, ImpDepsMap),
    module_and_imports_set_imp_deps_map(ImpDepsMap, !ModuleAndImports).

module_and_imports_add_indirect_dep(ModuleName, !ModuleAndImports) :-
    module_and_imports_get_indirect_deps(!.ModuleAndImports, IndirectDeps0),
    set.insert(ModuleName, IndirectDeps0, IndirectDeps),
    module_and_imports_set_indirect_deps(IndirectDeps, !ModuleAndImports).

%---------------------%

module_and_imports_add_direct_int_item_blocks(NewIntItemBlocks,
        !ModuleAndImports) :-
    module_and_imports_get_direct_int_blocks_cord(!.ModuleAndImports,
        IntItemBlocks0),
    IntItemBlocks = IntItemBlocks0 ++ cord.from_list(NewIntItemBlocks),
    module_and_imports_set_direct_int_blocks_cord(IntItemBlocks,
        !ModuleAndImports).

module_and_imports_add_indirect_int_item_blocks(NewIntItemBlocks,
        !ModuleAndImports) :-
    module_and_imports_get_indirect_int_blocks_cord(!.ModuleAndImports,
        IntItemBlocks0),
    IntItemBlocks = IntItemBlocks0 ++ cord.from_list(NewIntItemBlocks),
    module_and_imports_set_indirect_int_blocks_cord(IntItemBlocks,
        !ModuleAndImports).

module_and_imports_add_opt_item_blocks(NewOptItemBlocks, !ModuleAndImports) :-
    module_and_imports_get_opt_blocks_cord(!.ModuleAndImports,
        OptItemBlocks0),
    OptItemBlocks = OptItemBlocks0 ++ cord.from_list(NewOptItemBlocks),
    module_and_imports_set_opt_blocks_cord(OptItemBlocks,
        !ModuleAndImports).

module_and_imports_add_int_for_opt_item_blocks(NewIntItemBlocks,
        !ModuleAndImports) :-
    module_and_imports_get_int_for_opt_blocks_cord(!.ModuleAndImports,
        IntItemBlocks0),
    IntItemBlocks = IntItemBlocks0 ++ cord.from_list(NewIntItemBlocks),
    module_and_imports_set_int_for_opt_blocks_cord(IntItemBlocks,
        !ModuleAndImports).

%---------------------%

module_and_imports_maybe_add_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !ModuleAndImports) :-
    (
        MaybeVersionNumbers = no
    ;
        MaybeVersionNumbers = yes(VersionNumbers),
        module_and_imports_get_version_numbers_map(!.ModuleAndImports,
            ModuleVersionNumbersMap0),
        map.det_insert(ModuleName, VersionNumbers,
            ModuleVersionNumbersMap0, ModuleVersionNumbersMap),
        module_and_imports_set_version_numbers_map(ModuleVersionNumbersMap,
            !ModuleAndImports)
    ).

%---------------------%

module_and_imports_add_specs(NewSpecs, !ModuleAndImports) :-
    module_and_imports_get_specs(!.ModuleAndImports, Specs0),
    Specs = NewSpecs ++ Specs0,
    module_and_imports_set_specs(Specs, !ModuleAndImports).

module_and_imports_add_interface_error(InterfaceErrors, !ModuleAndImports) :-
    module_and_imports_get_errors(!.ModuleAndImports, Errors0),
    set.union(Errors0, InterfaceErrors, Errors),
    module_and_imports_set_errors(Errors, !ModuleAndImports).

module_and_imports_add_specs_errors(NewSpecs, NewErrors, !ModuleAndImports) :-
    module_and_imports_get_specs(!.ModuleAndImports, Specs0),
    module_and_imports_get_errors(!.ModuleAndImports, Errors0),
    Specs = NewSpecs ++ Specs0,
    set.union(Errors0, NewErrors, Errors),
    module_and_imports_set_specs(Specs, !ModuleAndImports),
    module_and_imports_set_errors(Errors, !ModuleAndImports).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

module_and_imports_d_file(ModuleAndImports,
        SourceFileName, SourceFileModuleName, ModuleName,
        Ancestors, PublicChildrenMap, NestedChildren,
        IntDepsMap, ImpDepsMap, IndirectDeps, FactDeps,
        ForeignImportModules, ForeignIncludeFilesCord, ContainsForeignCode,
        SrcItemBlocks, DirectIntItemBlocksCord, IndirectIntItemBlocksCord,
        OptItemBlocksCord, IntForOptItemBlocksCord) :-
    module_and_imports_get_source_file_name(ModuleAndImports,
        SourceFileName),
    module_and_imports_get_source_file_module_name(ModuleAndImports,
        SourceFileModuleName),
    module_and_imports_get_module_name(ModuleAndImports, ModuleName),
    module_and_imports_get_ancestors(ModuleAndImports, Ancestors),
    module_and_imports_get_public_children_map(ModuleAndImports,
        PublicChildrenMap),
    module_and_imports_get_nested_children(ModuleAndImports, NestedChildren),
    module_and_imports_get_int_deps_map(ModuleAndImports, IntDepsMap),
    module_and_imports_get_imp_deps_map(ModuleAndImports, ImpDepsMap),
    module_and_imports_get_indirect_deps(ModuleAndImports, IndirectDeps),
    module_and_imports_get_fact_table_deps(ModuleAndImports, FactDeps),
    module_and_imports_get_foreign_import_modules(ModuleAndImports,
        ForeignImportModules),
    module_and_imports_get_foreign_include_files(ModuleAndImports,
        ForeignIncludeFilesCord),
    module_and_imports_get_contains_foreign_code(ModuleAndImports,
        ContainsForeignCode),
    module_and_imports_get_src_blocks(ModuleAndImports, SrcItemBlocks),
    module_and_imports_get_direct_int_blocks_cord(ModuleAndImports,
        DirectIntItemBlocksCord),
    module_and_imports_get_indirect_int_blocks_cord(ModuleAndImports,
        IndirectIntItemBlocksCord),
    module_and_imports_get_opt_blocks_cord(ModuleAndImports,
        OptItemBlocksCord),
    module_and_imports_get_int_for_opt_blocks_cord(ModuleAndImports,
        IntForOptItemBlocksCord).

module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit,
        Specs, Errors) :-
    module_and_imports_get_module_name(ModuleAndImports, ModuleName),
    module_and_imports_get_module_name_context(ModuleAndImports,
        ModuleNameContext),
    module_and_imports_get_version_numbers_map(ModuleAndImports,
        ModuleVersionNumbers),
    module_and_imports_get_src_blocks(ModuleAndImports, SrcItemBlocks),
    module_and_imports_get_direct_int_blocks_cord(ModuleAndImports,
        DirectIntItemBlocksCord),
    module_and_imports_get_indirect_int_blocks_cord(ModuleAndImports,
        IndirectIntItemBlocksCord),
    module_and_imports_get_opt_blocks_cord(ModuleAndImports,
        OptItemBlocksCord),
    module_and_imports_get_int_for_opt_blocks_cord(ModuleAndImports,
        IntForOptItemBlocksCord),
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, SrcItemBlocks,
        cord.list(DirectIntItemBlocksCord),
        cord.list(IndirectIntItemBlocksCord),
        cord.list(OptItemBlocksCord),
        cord.list(IntForOptItemBlocksCord)),
    module_and_imports_get_specs(ModuleAndImports, Specs),
    module_and_imports_get_errors(ModuleAndImports, Errors).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_accessed
    --->    not_accessed
    ;       accessed.

:- type mai_fields
    --->    mai_fields(
                mf_source_file_name             :: maybe_accessed,
                mf_module_dir                   :: maybe_accessed,
                mf_source_file_module_name      :: maybe_accessed,
                mf_module_name                  :: maybe_accessed,
                mf_module_name_context          :: maybe_accessed,

                mf_ancestors                    :: maybe_accessed,
                mf_children                     :: maybe_accessed,
                mf_public_children              :: maybe_accessed,
                mf_nested_children              :: maybe_accessed,

                mf_int_deps_map                 :: maybe_accessed,
                mf_imp_deps_map                 :: maybe_accessed,
                mf_indirect_deps                :: maybe_accessed,
                mf_fact_table_deps              :: maybe_accessed,

                mf_foreign_import_modules       :: maybe_accessed,
                mf_foreign_include_files        :: maybe_accessed,
                mf_contains_foreign_code        :: maybe_accessed,
                mf_contains_foreign_export      :: maybe_accessed,
                mf_has_main                     :: maybe_accessed,

                mf_src_blocks                   :: maybe_accessed,
                mf_direct_int_blocks            :: maybe_accessed,
                mf_indirect_int_blocks          :: maybe_accessed,
                mf_opt_blocks                   :: maybe_accessed,
                mf_int_for_opt_blocks           :: maybe_accessed,

                mf_version_numbers_map          :: maybe_accessed,
                mf_maybe_timestamp_map          :: maybe_accessed,

                mf_specs                        :: maybe_accessed,
                mf_errors                       :: maybe_accessed
            ).

:- type mai_fields_kinds
    --->    mai_fields_kinds(
                mfk_init                        :: mai_fields,
                mfk_make                        :: mai_fields,
                mfk_read                        :: mai_fields
            ).

:- func init_mai_fields = mai_fields.

init_mai_fields =
    mai_fields(not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed).

:- func init_mai_fields_kinds = mai_fields_kinds.

init_mai_fields_kinds =
    mai_fields_kinds(init_mai_fields, init_mai_fields, init_mai_fields).

:- mutable(accesses, mai_fields_kinds, init_mai_fields_kinds, ground,
    [untrailed]).

write_mai_stats(Stream, !IO) :-
    promise_pure (
        semipure get_accesses(Accesses),
        Accesses = mai_fields_kinds(Init, Make, Read),
        write_mai_fields_stats(Stream, "INIT", Init, !IO),
        write_mai_fields_stats(Stream, "MAKE", Make, !IO),
        write_mai_fields_stats(Stream, "READ", Read, !IO)
    ).

:- pred write_mai_fields_stats(io.output_stream::in, string::in,
    mai_fields::in, io::di, io::uo) is det.

write_mai_fields_stats(Stream, Kind, Fields, !IO) :-
    Fields = mai_fields(SrcFileName, ModuleDir,
        SrcFileModuleName, ModuleName, ModuleNameContext,
        Ancestors, Children, PublicChildren, NestedChildren,
        IntDepsMap, ImpDepsMap, IndirectDeps, FactTableDeps,
        FIMs, ForeignIncludeFiles, HasForeignCode, HasForeignExport, HasMain,
        SrcBlocks, DirectIntBlocks, IndirectIntBlocks,
        OptBlocks, IntForOptBlocks,
        VersionNumbersMap, MaybeTimestamMap, Specs, Errors),
    io.format(Stream,
        "%s %s %s %s %s %s %s %s %s %s %s %s %s %s " ++
        "%s %s %s %s %s %s %s %s %s %s %s %s %s %s\n",
        [s(Kind),
        s(acc_str(SrcFileName)),
        s(acc_str(ModuleDir)),
        s(acc_str(SrcFileModuleName)),
        s(acc_str(ModuleName)),
        s(acc_str(ModuleNameContext)),
        s(acc_str(Ancestors)),
        s(acc_str(Children)),
        s(acc_str(PublicChildren)),
        s(acc_str(NestedChildren)),
        s(acc_str(IntDepsMap)),
        s(acc_str(ImpDepsMap)),
        s(acc_str(IndirectDeps)),
        s(acc_str(FactTableDeps)),
        s(acc_str(FIMs)),
        s(acc_str(ForeignIncludeFiles)),
        s(acc_str(HasForeignCode)),
        s(acc_str(HasForeignExport)),
        s(acc_str(HasMain)),
        s(acc_str(SrcBlocks)),
        s(acc_str(DirectIntBlocks)),
        s(acc_str(IndirectIntBlocks)),
        s(acc_str(OptBlocks)),
        s(acc_str(IntForOptBlocks)),
        s(acc_str(VersionNumbersMap)),
        s(acc_str(MaybeTimestamMap)),
        s(acc_str(Specs)),
        s(acc_str(Errors))],
        !IO).

:- func acc_str(maybe_accessed) = string.

acc_str(not_accessed) = "n".
acc_str(accessed) = "a".

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_imports.
%---------------------------------------------------------------------------%
