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
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
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
    % I see no documented argument anywhere any of the following propositions,
    % which could each make the above the right thing to do.
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

:- type module_names_contexts == multi_map(module_name, prog_context).

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

    % make_module_and_imports(SourceFileName, SourceFileModuleName,
    %  ModuleName, ModuleNameContext, SrcItemBlocks0,
    %  PublicChildren, NestedChildren, FactDeps, ForeignIncludeFiles,
    %  MaybeTimestampMap, ModuleAndImports):
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
    % a partially filled in state. If that perception is correct,
    % then that code should be fixed to follow the standard method
    % of construcing module_and_imports structures.
    %
:- pred make_module_and_imports(file_name::in,
    module_name::in, module_name::in, prog_context::in,
    list(src_item_block)::in, multi_map(module_name, prog_context)::in,
    set(module_name)::in, list(string)::in, foreign_include_file_infos::in,
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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.get_dependencies.

:- import_module dir.
:- import_module pair.
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
                mai_errors                      :: read_module_errors
            ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init_module_and_imports(Globals, FileName, SourceFileModuleName,
        NestedModuleNames, Specs, Errors, RawCompUnit0, ModuleImports) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    Ancestors = get_ancestors(ModuleName),

    get_dependencies_in_item_blocks(RawItemBlocks,
        ImpImportDeps0, ImpUseDeps0),
    get_implicit_dependencies_in_item_blocks(Globals, RawItemBlocks,
        ImplicitImpImportDeps, ImplicitImpUseDeps),
    multi_map.merge(ImplicitImpImportDeps, ImpImportDeps0, ImpImportDeps),
    multi_map.merge(ImplicitImpUseDeps, ImpUseDeps0, ImpUseDeps),
    multi_map.merge(ImpImportDeps, ImpUseDeps, ImpDeps),

    get_interface(RawCompUnit0, RawCompUnit),
    RawCompUnit = raw_compilation_unit(_, _, InterfaceItemBlocks),
    get_dependencies_in_item_blocks(InterfaceItemBlocks,
        IntImportDeps0, IntUseDeps0),
    get_implicit_dependencies_in_item_blocks(Globals, InterfaceItemBlocks,
        ImplicitIntImportDeps, ImplicitIntUseDeps),
    multi_map.merge(ImplicitIntImportDeps, IntImportDeps0, IntImportDeps),
    multi_map.merge(ImplicitIntUseDeps, IntUseDeps0, IntUseDeps),
    multi_map.merge(IntImportDeps, IntUseDeps, IntDeps),

    % We don't fill in the indirect dependencies yet.
    set.init(IndirectDeps),

    get_included_modules_in_item_blocks(RawItemBlocks, Children),
    get_included_modules_in_item_blocks(InterfaceItemBlocks, PublicChildren),

    % XXX ITEM_LIST Document why we do this.
    ( if ModuleName = SourceFileModuleName then
        set.delete(ModuleName, NestedModuleNames, NestedDeps)
    else
        set.init(NestedDeps)
    ),

    get_fact_table_dependencies_in_item_blocks(RawItemBlocks, FactTableDeps),

    % Figure out whether the items contain foreign code.
    get_foreign_code_indicators_from_item_blocks(Globals, RawItemBlocks,
        LangSet, ForeignImports0, ForeignIncludeFiles, ContainsForeignExport),
    ( if set.is_empty(LangSet) then
        ContainsForeignCode = contains_no_foreign_code
    else
        ContainsForeignCode = contains_foreign_code(LangSet)
    ),

    % If this module contains `:- pragma foreign_export' or
    % `:- pragma foreign_type' declarations, importing modules may need
    % to import its `.mh' file.
    get_foreign_self_imports_from_item_blocks(RawItemBlocks, SelfImportLangs),
    list.foldl(
        ( pred(Lang::in, FIM0::in, FIM::out) is det :-
            add_foreign_import_module(Lang, ModuleName, FIM0, FIM)
        ), SelfImportLangs, ForeignImports0, ForeignImports),

    % Work out whether the items contain main/2.
    look_for_main_pred_in_item_blocks(RawItemBlocks, no_main, HasMain),

    map.init(VersionNumbers),
    MaybeTimestampMap = no,
    % XXX ITEM_LIST SrcItemBlocks and the item block fields are NOT
    % stored here, per the documentation above. Maybe they should be.
    ModuleImports = module_and_imports(FileName, dir.this_directory,
        SourceFileModuleName, ModuleName, ModuleNameContext,
        set.list_to_set(Ancestors), Children, PublicChildren, NestedDeps,
        IntDeps, ImpDeps, IndirectDeps, FactTableDeps,
        ForeignImports, ForeignIncludeFiles,
        ContainsForeignCode, ContainsForeignExport, HasMain,
        [], cord.init, cord.init, cord.init, cord.init,
        VersionNumbers, MaybeTimestampMap, Specs, Errors).

:- pred look_for_main_pred_in_item_blocks(list(item_block(MS))::in,
    has_main::in, has_main::out) is det.

look_for_main_pred_in_item_blocks([], !HasMain).
look_for_main_pred_in_item_blocks([ItemBlock | ItemBlocks], !HasMain) :-
    % XXX ITEM_LIST Warn if Section isn't ms_interface or ams_interface.
    ItemBlock = item_block(_, _, _Incls, _Imports, Items),
    look_for_main_pred_in_items(Items, !HasMain),
    look_for_main_pred_in_item_blocks(ItemBlocks, !HasMain).

:- pred look_for_main_pred_in_items(list(item)::in,
    has_main::in, has_main::out) is det.

look_for_main_pred_in_items([], !HasMain).
look_for_main_pred_in_items([Item | Items], !HasMain) :-
    ( if
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(Name, pf_predicate, ArgTypes,
            _, WithType, _, _, _, _, _, _, _, _, _),
        unqualify_name(Name) = "main",
        % XXX We should allow `main/2' to be declared using `with_type`,
        % but equivalences haven't been expanded at this point.
        % The `has_main' field is only used for some special case handling
        % of the module containing main for the IL backend (we generate
        % a `.exe' file rather than a `.dll' file). This would arguably
        % be better done by generating a `.dll' file as normal, and a
        % separate `.exe' file containing initialization code and a call
        % to `main/2', as we do with the `_init.c' file in the C backend.
        ArgTypes = [_, _],
        WithType = no
    then
        % XXX ITEM_LIST Should we warn if !.HasMain = has_main?
        % If not, then we should stop recursing right here.
        !:HasMain = has_main
    else
        true
    ),
    look_for_main_pred_in_items(Items, !HasMain).

%---------------------------------------------------------------------------%

make_module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, ModuleNameContext, SrcItemBlocks,
        PublicChildren, NestedChildren, FactDeps, ForeignIncludeFiles,
        MaybeTimestampMap, ModuleAndImports) :-
    set.init(Ancestors),
    map.init(IntDeps),
    map.init(ImpDeps),
    set.init(IndirectDeps),
    % XXX Since PublicChildren should be a subset of Children,
    % this looks like bug.
    map.init(Children),
    ForeignImports = init_foreign_import_modules,
    map.init(VersionNumbers),
    Specs = [],
    set.init(Errors),
    ModuleAndImports = module_and_imports(SourceFileName, dir.this_directory,
        SourceFileModuleName, ModuleName, ModuleNameContext,
        Ancestors, Children, PublicChildren, NestedChildren,
        IntDeps, ImpDeps, IndirectDeps, FactDeps,
        ForeignImports, ForeignIncludeFiles,
        contains_foreign_code_unknown, contains_no_foreign_export, no_main,
        SrcItemBlocks, cord.init, cord.init, cord.init, cord.init,
        VersionNumbers, MaybeTimestampMap, Specs, Errors).

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
        ModuleVersionNumbers, MaybeTimestamps, Specs, Errors).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred module_and_imports_get_version_numbers_map(module_and_imports::in,
    module_version_numbers_map::out) is det.
:- pred module_and_imports_get_specs(module_and_imports::in,
    list(error_spec)::out) is det.

module_and_imports_get_source_file_name(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_source_file_name.
module_and_imports_get_source_file_dir(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_module_dir.
module_and_imports_get_source_file_module_name(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_source_file_module_name.
module_and_imports_get_module_name(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_module_name.
module_and_imports_get_module_name_context(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_module_name_context.
module_and_imports_get_ancestors(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_ancestors.
module_and_imports_get_children_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_children.
module_and_imports_get_public_children_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_public_children.
module_and_imports_get_nested_children(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_nested_children.
module_and_imports_get_int_deps_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_int_deps_map.
module_and_imports_get_imp_deps_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_imp_deps_map.
module_and_imports_get_indirect_deps(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_indirect_deps.
module_and_imports_get_fact_table_deps(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_fact_table_deps.
module_and_imports_get_foreign_import_modules(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_foreign_import_modules.
module_and_imports_get_foreign_include_files(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_foreign_include_files.
module_and_imports_get_contains_foreign_code(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_contains_foreign_code.
module_and_imports_get_contains_foreign_export(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_contains_foreign_export.
module_and_imports_get_has_main(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_has_main.
module_and_imports_get_src_blocks(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_src_blocks.
module_and_imports_get_direct_int_blocks_cord(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_direct_int_blocks_cord.
module_and_imports_get_indirect_int_blocks_cord(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_indirect_int_blocks_cord.
module_and_imports_get_opt_blocks_cord(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_opt_blocks_cord.
module_and_imports_get_int_for_opt_blocks_cord(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_int_for_opt_blocks_cord.
module_and_imports_get_version_numbers_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_version_numbers_map.
module_and_imports_get_maybe_timestamp_map(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_maybe_timestamp_map.
module_and_imports_get_specs(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_specs.
module_and_imports_get_errors(ModuleAndImports, X) :-
    X = ModuleAndImports ^ mai_errors.

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
:- end_module parse_tree.module_imports.
%---------------------------------------------------------------------------%
