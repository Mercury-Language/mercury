%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modules.m.
% Main author: fjh.
%
% This module used to contain all the code for handling module imports
% and exports, for computing module dependencies, and for generating makefile
% fragments to record those dependencies. That made it by far the biggest
% module in the compiler (at one point it contained almost 9000 lines),
% so most of this code was moved out into other modules. What is left
% is a mish-mash set predicates, with significantly lower cohesion than
% we would like. The main tasks they handle are
%
% - separating lists of items into interface items and implementation items
% - checking lists of items for errors of various kinds
% - figuring out which interface files a module depends on, and reading them
% - figuring out the information from which we create dependency files
%   (.dep and .d files) for make
% - figuring out what modules are included in a list of items
% - converting a list of items into a set of nested modules
%
% Both the interface and the implementation of the module are divided
% into six parts representing these tasks. Some of the auxiliary predicates
% are used by more than one task.
%
% Some of the code in this module should not be needed. If the parse tree
% representation of Mercury source files were more structured than just
% a plain item list, I (zs) think that much of the code here could be
% greatly simplified, or even eliminated (since the relevant task could have
% been done during the creation of the parse tree). The plain item list
% representation was chosen by Fergus, on the grounds that this is the
% least transformed representation of the code, and that anything more
% complicated would make it too difficult to build non-compiler tools
% such as prettyprinters. Since we haven't felt the need for such tools
% in more than twenty years, this concern has been shown to be irrelevant.
%
% The roles of the interface files (.int0, .int3, .int2 and .int) that
% this module processes are documented in the module that creates them,
% which is write_module_interface_files.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.modules.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a module (well, a list of items), extract the interface
    % part of that module, i.e. all the items between `:- interface'
    % and `:- implementation'.
    % The bodies of instance definitions are removed because
    % the instance methods have not yet been module qualified.
    %
:- pred get_interface(module_name::in, bool::in,
    list(item)::in, list(item)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a module name and a list of the items in that module,
    % this procedure checks if the module doesn't export anything,
    % and if so, and --warn-nothing-exported is set, it reports
    % a warning.
    %
:- pred check_for_no_exports(globals::in, list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

    % Given a module name and a list of the items in that module's interface,
    % this procedure checks if the module doesn't export anything, and if so,
    % and --warn-nothing-exported is set, it returns a warning.
    %
:- pred check_int_for_no_exports(globals::in, list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % grab_imported_modules(Globals, SourceFileName, SourceFileModuleName,
    %   ModuleName, NestedSubModules, HaveReadModuleMap, ModuleTimestamp,
    %   Items, Module, !IO):
    %
    % Given a source file name and the top-level module name in that file,
    % the current module name, the nested sub-modules in the file if this
    % module is the top-level module, the timestamp of the file SourceFileName
    % and the list of items in the current module, read in the private
    % interface files for all the parent modules, the long interface files
    % for all the imported modules, and the short interface files for all
    % the indirectly imported modules, and return a `module_and_imports'
    % structure containing the relevant information. HaveReadModuleMap contains
    % the interface files read during recompilation checking.
    %
:- pred grab_imported_modules(globals::in, file_name::in,
    module_name::in, module_name::in, list(module_name)::in,
    have_read_module_map::in, maybe(timestamp)::in, list(item)::in,
    module_and_imports::out, io::di, io::uo) is det.

    % grab_unqual_imported_modules(Globals, SourceFileName,
    %   SourceFileModuleName, ModuleName, Items, Module, !IO):
    %
    % Similar to grab_imported_modules, but only reads in the unqualified
    % short interfaces (.int3s), and the .int0 files for parent modules,
    % instead of reading the long interfaces and qualified short interfaces
    % (.int and int2s). Does not set the `PublicChildren', `FactDeps'
    % `ForeignIncludeFiles' fields of the module_and_imports structure.
    %
:- pred grab_unqual_imported_modules(globals::in, file_name::in,
    module_name::in, module_name::in, list(item)::in, module_and_imports::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % process_module_private_interfaces(Globals, Ancestors,
    %   IntStatusItem, ImpStatusItem, !DirectImports, !DirectUses,
    %   !Module, !IO):
    %
    % Read the complete private interfaces for modules in Ancestors, and
    % append any imports/uses in the ancestors to the corresponding previous
    % lists.
    %
:- pred process_module_private_interfaces(globals::in,
    have_read_module_map::in, list(module_name)::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_long_interfaces(Globals, HaveReadModuleMap, NeedQualifier,
    %   Imports, Ext, IntStatusItem, ImpStatusItem,
    %   !IndirectImports, !ImplIndirectImports, !Module, !IO):
    %
    % Read the long interfaces for modules in Imports (unless they've already
    % been read in) from files with filename extension Ext, and append any
    % imports/uses in those modules to the IndirectImports list, and append
    % any imports/uses in the implementation of those modules to the
    % ImplIndirectImports list. Replace the `:- interface' declarations with
    % IntStatusItem, which should set the import_status of the following items.
    % Replace the `:- implementation' declarations with ImpStatusItem, which
    % should set the import_status of the following items.
    %
:- pred process_module_long_interfaces(globals::in, have_read_module_map::in,
    need_qualifier::in, list(module_name)::in, string::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
    %   IndirectImports, Ext, IntStatusItem, ImpStatusItem,
    %   !ImpIndirectImports, !Module):
    %
    % Read the short interfaces for modules in IndirectImports (unless they've
    % already been read in) and any modules that those modules import
    % (transitively) in the interface.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which
    % should set the import_status of the following items. Replace the
    % `:- implementation' declarations with ImpStatusItem, which should set
    % the import_status of the following items.
    %
:- pred process_module_short_interfaces_transitively(globals::in,
    have_read_module_map::in, list(module_name)::in, string::in,
    item::in, item::in, list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_and_impls_transitively(Globals,
    %   HaveReadModuleMap, IndirectImports, Ext, IntStatusItem, ImpStatusItem,
    %   !Module):
    %
    % Read the short interfaces for modules in IndirectImports (unless they've
    % already been read in) and any modules that those modules import
    % (transitively) in the interface or implementation.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which
    % should set the import_status of the following items.
    % Replace the `:- implementation' declarations with ImpStatusItem,
    % which should set the import_status of the following items.
    %
:- pred process_module_short_interfaces_and_impls_transitively(globals::in,
    have_read_module_map::in, list(module_name)::in, string::in,
    item::in, item::in, module_and_imports::in, module_and_imports::out,
    io::di, io::uo) is det.

    % process_module_short_interfaces(Globals, HaveReadModuleMap,
    %   IntStatusItem, ImpStatusItem, Modules, Ext,
    %   !IndirectImports, !ImpIndirectImports, !Module):
    %
    % Read the short interfaces for modules in Modules (unless they've already
    % been read in). Append the modules imported by the interface of Modules to
    % !IndirectImports. Append the modules imported by the implementation of
    % Modules to !ImpIndirectImports.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which should
    % set the import_status of the following items. Replace the
    % `:- implementation' declarations with ImpStatusItem, which should set
    % the import_status of the following items.
    %
:- pred process_module_short_interfaces(globals::in, have_read_module_map::in,
    list(module_name)::in, string::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% These predicates do the top level part of their jobs here in this module,
% and hand over the low level details of writing stuff out to the module
% write_deps_file.m.

    % generate_module_dependencies(Globals, ModuleName, !IO):
    %
    % Generate the per-program makefile dependencies (`.dep') file for a
    % program whose top-level module is `ModuleName'. This involves first
    % transitively reading in all imported or ancestor modules. While we're
    % at it, we also save the per-module makefile dependency (`.d') files
    % for all those modules.
    %
:- pred generate_module_dependencies(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependencies(Globals, FileName, !IO):
    %
    % Same as generate_module_dependencies, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependencies(globals::in, file_name::in,
    io::di, io::uo) is det.

    % generate_module_dependency_file(Globals, ModuleName, !IO):
    %
    % Generate the per module makefile dependency ('.d') file for the
    % given module.
    %
:- pred generate_module_dependency_file(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependency_file(Globals, FileName, !IO):
    %
    % Same as generate_module_dependency_file, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependency_file(globals::in, file_name::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% These predicates are exported for use by module_imports.m.
%
% XXX They shouldn't be needed; the representation of the program should have
% all this information readily accessible.

    % get_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items.
    %
:- pred get_children(list(item)::in, list(module_name)::out) is det.

:- pred get_foreign_self_imports(list(item)::in, list(foreign_language)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type module_list == assoc_list(module_name, list(item)).

    % Given a module (well, a list of items), split it into its constituent
    % sub-modules, in top-down order.
    % Also do some error checking:
    % - report an error if the `implementation' section of a sub-module
    %   is contained inside the `interface' section of its parent module
    % - check for modules declared as both nested and separate sub-modules.
    % - check for non-abstract typeclass instance declarations in module
    %   interfaces.
    %
    % XXX With a more structured parse tree, this predicate should not be
    % needed; its job should be done during parsing.
    %
:- pred split_into_submodules(module_name::in, list(item)::in,
    module_list::out, list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.file_names.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.prog_io_error.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.write_deps_file.

:- import_module cord.
:- import_module digraph.
:- import_module dir.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

get_interface(ModuleName, IncludeImplTypes, Items0, Items) :-
    AddToImpl = (func(_, ImplItems) = ImplItems),
    get_interface_and_implementation_acc(IncludeImplTypes, Items0, no,
        [], RevItems, AddToImpl, unit, _),
    list.reverse(RevItems, Items1),
    maybe_add_foreign_import_module(ModuleName, Items1, Items).

:- pred get_interface_and_implementation(module_name::in, bool::in,
    list(item)::in, list(item)::out, list(item)::out) is det.

get_interface_and_implementation(ModuleName, IncludeImplTypes,
        Items0, InterfaceItems, ImplementationItems) :-
    AddToImpl = (func(ImplItem, ImplItems) = [ImplItem | ImplItems]),
    get_interface_and_implementation_acc(IncludeImplTypes, Items0, no,
        [], RevIntItems, AddToImpl, [], RevImplItems),
    list.reverse(RevIntItems, InterfaceItems0),
    list.reverse(RevImplItems, ImplementationItems),
    maybe_add_foreign_import_module(ModuleName,
        InterfaceItems0, InterfaceItems).

%-----------------------------------------------------------------------------%

:- pred get_interface_and_implementation_acc(bool::in, list(item)::in,
    bool::in, list(item)::in, list(item)::out,
    func(item, T) = T::in, T::in, T::out) is det.

get_interface_and_implementation_acc(_, [], _, !RevIntItems, _, !RevImplItems).
get_interface_and_implementation_acc(IncludeImplTypes, [Item | Rest],
        !.InInterface, !RevIntItems, AddImplItem, !RevImplItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_interface
        ; ModuleDefn = md_implementation
        ; ModuleDefn = md_imported(_)
        ; ModuleDefn = md_used(_)
        )
    ->
        (
            ModuleDefn = md_interface,
            !:RevIntItems = [Item | !.RevIntItems],
            !:InInterface = yes,
            get_interface_and_implementation_acc(IncludeImplTypes, Rest,
                !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
        ;
            ModuleDefn = md_implementation,
            !:RevIntItems = [Item | !.RevIntItems],
            !:InInterface = no,
            get_interface_and_implementation_acc(IncludeImplTypes, Rest,
                !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
        ;
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            )
            % Items after here are not part of this module, which is why
            % we don't have a recursive call here.
        )
    ;
        (
            !.InInterface = yes,
            ( Item = item_instance(ItemInstance) ->
                % Include the abstract version of the instance in the
                % interface, ...
                AbstractItemInstance = make_instance_abstract(ItemInstance),
                AbstractItem = item_instance(AbstractItemInstance),
                !:RevIntItems = [AbstractItem | !.RevIntItems],

                % ... and the concrete version in the implementation.
                !:RevImplItems = AddImplItem(Item, !.RevImplItems)
            ;
                !:RevIntItems = [Item | !.RevIntItems]
            )
        ;
            !.InInterface = no,
            !:RevImplItems = AddImplItem(Item, !.RevImplItems),
            (
                IncludeImplTypes = yes,
                include_in_int_file_implementation(Item) = yes
            ->
                ( make_abstract_defn(Item, int2, AbstractItem) ->
                    ItemToAdd = AbstractItem
                ; make_abstract_unify_compare(Item, int2, AbstractItem) ->
                    ItemToAdd = AbstractItem
                ;
                    ItemToAdd = Item
                ),
                !:RevIntItems = [ItemToAdd | !.RevIntItems]
            ;
                true
            )
        ),
        get_interface_and_implementation_acc(IncludeImplTypes, Rest,
            !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
    ).

:- func include_in_int_file_implementation(item) = bool.

include_in_int_file_implementation(Item) = Include :-
    (
        % `:- typeclass declarations' may be referred to by the constructors
        % in type declarations. Since these constructors are abstractly
        % exported, we won't need the local instance declarations.
        ( Item = item_type_defn(_)
        ; Item = item_typeclass(_)
        ),
        Include = yes
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            % XXX Some of these should yield an exception.
            ( ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ; ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            Include = yes
        ;
            ModuleDefn = md_external(_, _),
            Include = no
        )
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        (
            ( Pragma = pragma_foreign_import_module(_)
            ; Pragma = pragma_foreign_enum(_)
            ),
            Include = yes
        ;
            % XXX I am not sure about the proper value of Include
            % for some of these. -zs
            ( Pragma = pragma_foreign_decl(_)
            ; Pragma = pragma_foreign_code(_)
            ; Pragma = pragma_foreign_proc(_)
            ; Pragma = pragma_foreign_proc_export(_)
            ; Pragma = pragma_foreign_export_enum(_)
            ; Pragma = pragma_type_spec(_)
            ; Pragma = pragma_inline(_)
            ; Pragma = pragma_no_inline(_)
            ; Pragma = pragma_unused_args(_)
            ; Pragma = pragma_exceptions(_)
            ; Pragma = pragma_trailing_info(_)
            ; Pragma = pragma_mm_tabling_info(_)
            ; Pragma = pragma_obsolete(_)
            ; Pragma = pragma_no_detism_warning(_)
            ; Pragma = pragma_source_file(_)
            ; Pragma = pragma_oisu(_)
            ; Pragma = pragma_tabled(_)
            ; Pragma = pragma_fact_table(_)
            ; Pragma = pragma_reserve_tag(_)
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
            Include = no
        )
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
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
        Include = no
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_add_foreign_import_module(module_name::in,
    list(item)::in, list(item)::out) is det.

maybe_add_foreign_import_module(ModuleName, Items0, Items) :-
    get_foreign_self_imports(Items0, Langs),
    ImportItems = list.map(make_foreign_import(ModuleName), Langs),
    Items = ImportItems ++ Items0.

:- func make_foreign_import(module_name, foreign_language) = item.

make_foreign_import(ModuleName, Lang) = Item :-
    Origin = compiler(foreign_imports),
    Info = pragma_info_foreign_import_module(Lang, ModuleName),
    Pragma = pragma_foreign_import_module(Info),
    ItemPragma = item_pragma_info(Origin, Pragma, term.context_init, -1),
    Item = item_pragma(ItemPragma).

get_foreign_self_imports(Items, Langs) :-
    list.foldl(accumulate_item_foreign_import_langs, Items, set.init, LangSet),
    set.to_sorted_list(LangSet, Langs).

:- pred accumulate_item_foreign_import_langs(item::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_item_foreign_import_langs(Item, !LangSet) :-
    Langs = item_needs_foreign_imports(Item),
    set.insert_list(Langs, !LangSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

check_for_no_exports(Globals, Items, ModuleName, !Specs, !IO) :-
    globals.lookup_bool_option(Globals, warn_nothing_exported, ExportWarning),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        get_interface(ModuleName, no, Items, InterfaceItems),
        check_int_for_no_exports(Globals, InterfaceItems, ModuleName,
            !Specs, !IO)
    ).

check_int_for_no_exports(Globals, [], ModuleName, !Specs, !IO) :-
    generate_no_exports_warning(Globals, ModuleName, WarnSpec, !IO),
    !:Specs = [WarnSpec | !.Specs].
check_int_for_no_exports(Globals, [Item | Items], ModuleName, !Specs, !IO) :-
    (
        (
            Item = item_nothing(_)
        ;
            Item = item_module_defn(ItemModuleDefn),
            ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
            ModuleDefn \= md_include_module(_)
        )
    ->
        % Nothing useful - keep searching.
        check_int_for_no_exports(Globals, Items, ModuleName, !Specs, !IO)
    ;
        % We found something useful - don't issue the warning.
        true
    ).

:- pred generate_no_exports_warning(globals::in, module_name::in,
    error_spec::out, io::di, io::uo) is det.

generate_no_exports_warning(Globals, ModuleName, Spec, !IO) :-
    % XXX The FileName should be passed down to here; we shouldn't have to
    % compute it again.
    module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
        FileName, !IO),
    % XXX We should use the module declaration's context, not the arbitrary
    % line number 1.
    Context = context_init(FileName, 1),
    Severity = severity_conditional(warn_nothing_exported, yes,
        severity_warning, no),
    Component = option_is_set(warn_nothing_exported, yes,
        [always([words("Warning: interface for module"),
            sym_name(ModuleName), words("does not export anything.")]),
        verbose_only(
            [words("To be useful, a module should export something."),
            words("A file should contain at least one declaration"),
            words("other than"), decl("import_module"),
            words("in its interface section(s)."),
            words("This would normally be a"),
            decl("pred"), suffix(","), decl("func"), suffix(","),
            decl("type"), suffix(","), decl("inst"), words("or"),
            decl("mode"), words("declaration.")])
        ]),
    Msg = simple_msg(Context, [Component]),
    Spec = error_spec(Severity, phase_term_to_parse_tree, [Msg]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, NestedChildren, HaveReadModuleMap, MaybeTimestamp, Items0,
        !:Module, !IO) :-
    % Find out which modules this one depends on.
    AncestorModules = get_ancestors(ModuleName),
    get_dependencies_int_imp(Items0, IntImportedModules0, IntUsedModules0,
        ImpImportedModules0, ImpUsedModules0),

    ImportedModules0 = IntImportedModules0 ++ ImpImportedModules0,
    UsedModules0 = IntUsedModules0 ++ ImpUsedModules0,

    some [!Specs] (
        !:Specs = [],

        module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
            FileName, !IO),
        warn_if_import_self_or_ancestor(ModuleName, FileName, AncestorModules,
            ImportedModules0, UsedModules0, !Specs),

        warn_if_duplicate_use_import_decls(ModuleName, FileName,
            IntImportedModules0, IntImportedModules1,
            IntUsedModules0, IntUsedModules1,
            ImpImportedModules0, ImpImportedModules,
            ImpUsedModules0, ImpUsedModules, !Specs),

        get_interface_and_implementation(ModuleName, no, Items0,
            InterfaceItems, ImplItems),
        get_children(InterfaceItems, PublicChildren),
        get_fact_table_dependencies(Items0, FactDeps),
        get_foreign_include_files(Items0, ForeignIncludeFiles),
        (
            MaybeTimestamp = yes(Timestamp),
            MaybeTimestamps = yes(map.singleton(ModuleName,
                module_timestamp(".m", Timestamp, may_be_unqualified)))
        ;
            MaybeTimestamp = no,
            MaybeTimestamps = no
        ),
        init_module_and_imports(SourceFileName, SourceFileModuleName,
            ModuleName, Items0, !.Specs, PublicChildren, NestedChildren,
            FactDeps, ForeignIncludeFiles, MaybeTimestamps, !:Module),

        % If this module has any separately-compiled sub-modules, then
        % we need to make everything in the implementation of this module
        % exported_to_submodules. We do that by splitting out the
        % implementation declarations and putting them in a special
        % `implementation_but_exported_to_submodules' section.

        get_children(Items0, Children),
        (
            Children = [],
            Items1 = Items0
        ;
            Children = [_ | _],
            split_clauses_and_decls(ImplItems, Clauses, ImplDecls),
            Items1 =
                [make_pseudo_decl(md_interface) | InterfaceItems] ++
                [make_pseudo_decl(md_implementation_but_exported_to_submodules)
                    | ImplDecls] ++
                [make_pseudo_decl(md_implementation) | Clauses],
            !Module ^ mai_items_cord := cord.from_list(Items1)
        ),

        % Add `builtin' and `private_builtin' to the list of imported modules.

        % Add `builtin' and `private_builtin', and any other builtin modules
        % needed by any of the items, to the imported modules.
        % XXX Why are these added to the interface, and not the implementation
        % dependencies?
        get_implicit_dependencies(Items1, Globals,
            ImplicitIntImportedModules, ImplicitIntUsedModules),
        IntImportedModules2 =
            ImplicitIntImportedModules ++ IntImportedModules1,
        IntUsedModules2 = ImplicitIntUsedModules ++ IntUsedModules1,

        % Process the ancestor modules.
        %
        % Uses of the items declared in ancestor modules do not need
        % module qualifiers. Modules imported by ancestors are considered
        % to be visible in the current module.
        process_module_private_interfaces(Globals, HaveReadModuleMap,
            AncestorModules,
            make_pseudo_decl(md_imported(import_locn_interface)),
            make_pseudo_decl(md_imported(
                import_locn_ancestor_private_interface_proper)),
            IntImportedModules2, IntImportedModules,
            IntUsedModules2, IntUsedModules, !Module, !IO),

        % Process the modules imported using `import_module'.
        % Uses of these items do not need module qualifiers.
        IntIndirectImports0 = [],
        IntImpIndirectImports0 = [],
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            may_be_unqualified, IntImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports0, IntIndirectImports1,
            IntImpIndirectImports0, IntImpIndirectImports1,
            !Module, !IO),

        ImpIndirectImports0 = [],
        ImpImpIndirectImports0 = [],
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            may_be_unqualified, ImpImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports0, ImpIndirectImports1,
            ImpImpIndirectImports0, ImpImpIndirectImports1,
            !Module, !IO),

        % Process the modules imported using `use_module' .
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            must_be_qualified, IntUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports1, IntIndirectImports,
            IntImpIndirectImports1, IntImpIndirectImports2,
            !Module, !IO),
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            must_be_qualified, ImpUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports1, ImpIndirectImports,
            ImpImpIndirectImports1, ImpImpIndirectImports2,
            !Module, !IO),

        % Process the short interfaces for indirectly imported modules.
        % The short interfaces are treated as if they are imported
        % using `use_module'.
        append_pseudo_decl(md_transitively_imported, !Module),
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, IntIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntImpIndirectImports2, IntImpIndirectImports, !Module, !IO),
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, ImpIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpImpIndirectImports2, ImpImpIndirectImports, !Module, !IO),

        % Process the short interfaces for modules imported in the
        % implementation of indirectly imported modules. The items in these
        % modules shouldn't be visible to typechecking -- they are used for
        % fully expanding equivalence types after the semantic checking passes.
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, IntImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, ImpImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),

        module_and_imports_get_results(!.Module, Items, _, _),
        check_imports_accessibility(ModuleName,
            IntImportedModules ++ IntUsedModules ++
            ImpImportedModules ++ ImpUsedModules, Items, [], AccessSpecs),
        module_and_imports_add_specs(AccessSpecs, !Module)
    ).

grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, Items0, !:Module, !IO) :-
    % Find out which modules this one depends on.
    ParentDeps = get_ancestors(ModuleName),
    get_dependencies_int_imp(Items0, IntImportDeps0, IntUseDeps0,
        ImpImportDeps, ImpUseDeps),

    % Construct the initial module import structure.
    init_module_and_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, [], [], [], [], [], no, !:Module),

    % Add `builtin' and `private_builtin', and any other builtin modules
    % needed by any of the items, to the imported modules.
    % XXX Why are these added to the interface, and not the implementation
    % dependencies?
    get_implicit_dependencies(Items0, Globals,
        ImplicitIntImportDeps, ImplicitIntUseDeps),
    IntImportDeps = ImplicitIntImportDeps ++ IntImportDeps0,
    IntUseDeps = ImplicitIntUseDeps ++ IntUseDeps0,

    % Get the .int3s and .int0s that the current module depends on.
    map.init(HaveReadModuleMap),

    % First the .int0s for parent modules.
    process_module_private_interfaces(Globals, HaveReadModuleMap, ParentDeps,
        make_pseudo_decl(md_imported(import_locn_interface)),
        make_pseudo_decl(md_imported(
            import_locn_ancestor_private_interface_proper)),
        [], ParentImportDeps, [], ParentUseDeps, !Module, !IO),

    % Then the .int3s for `:- import'-ed modules.
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ParentImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        [], IntIndirectImportDeps0, [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, IntImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps0, IntIndirectImportDeps1,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ImpImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], ImpIndirectImportDeps0,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decls) the .int3s for `:- use'-ed
    % modules.
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ParentUseDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps1, IntIndirectImportDeps2,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        must_be_qualified, IntUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps2, IntIndirectImportDeps,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        must_be_qualified, ImpUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        ImpIndirectImportDeps0, ImpIndirectImportDeps,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decl) the .int3s for indirectly
    % imported modules.
    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        IntIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        ImpIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    module_and_imports_get_results(!.Module, Items, _, _),
    check_imports_accessibility(ModuleName,
        IntImportDeps ++ IntUseDeps ++ ImpImportDeps ++ ImpUseDeps,
        Items, [], AccessSpecs),
    module_and_imports_add_specs(AccessSpecs, !Module).

%-----------------------------------------------------------------------------%

:- pred split_clauses_and_decls(list(item)::in,
    list(item)::out, list(item)::out) is det.

split_clauses_and_decls([], [], []).
split_clauses_and_decls([Item | Items], !:ClauseItems, !:InterfaceItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_interface
        ; ModuleDefn = md_implementation
        )
    ->
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems)
    ;
        (
            Item = item_clause(_)
        ;
            Item = item_pragma(ItemPragma),
            ItemPragma = item_pragma_info(_, Pragma, _, _),
            pragma_allowed_in_interface(Pragma) = no
        ;
            Item = item_initialise(_)
        ;
            Item = item_finalise(_)
        )
     ->
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems),
        !:ClauseItems = [Item | !.ClauseItems]
    ;
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems),
        !:InterfaceItems = [Item | !.InterfaceItems]
    ).

%-----------------------------------------------------------------------------%

    % Warn if a module imports itself, or an ancestor.
    %
:- pred warn_if_import_self_or_ancestor(module_name::in, string::in,
    list(module_name)::in, list(module_name)::in, list(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_import_self_or_ancestor(ModuleName, FileName, AncestorModules,
        ImportedModules, UsedModules, !Specs) :-
    % ZZZ
    IsImportedAncestor = (pred(Import::out) is nondet :-
        list.member(Import, AncestorModules),
        ( list.member(Import, ImportedModules)
        ; list.member(Import, UsedModules)
        )
    ),
    solutions.aggregate(IsImportedAncestor,
        warn_imported_ancestor(ModuleName, FileName), !Specs),
    (
        ( list.member(ModuleName, ImportedModules)
        ; list.member(ModuleName, UsedModules)
        )
    ->
        term.context_init(FileName, 1, Context),
        SelfPieces = [words("Warning: module"),
            sym_name(ModuleName), words("imports itself!")],
        SelfMsg = simple_msg(Context,
            [option_is_set(warn_simple_code, yes, [always(SelfPieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        SelfSpec = error_spec(Severity, phase_parse_tree_to_hlds, [SelfMsg]),
        !:Specs = [SelfSpec | !.Specs]
    ;
        true
    ).

:- pred warn_imported_ancestor(module_name::in, string::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_imported_ancestor(ModuleName, FileName, AncestorName, !Specs) :-
    term.context_init(FileName, 1, Context),
    MainPieces = [words("Module"), sym_name(ModuleName),
        words("imports its own ancestor, module"),
        sym_name(AncestorName), words(".")],
    VerbosePieces = [words("Every sub-module"),
        words("implicitly imports its ancestors."),
        words("There is no need to explicitly import them.")],
    Msg = simple_msg(Context,
        [option_is_set(warn_simple_code, yes,
            [always(MainPieces), verbose_only(VerbosePieces)])]),
    Severity = severity_conditional(warn_simple_code, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

    % This predicate ensures that every import_module declaration is checked
    % against every use_module declaration, except for the case where
    % the interface has `:- use_module foo.' and the implementation
    % `:- import_module foo.'.
    %
:- pred warn_if_duplicate_use_import_decls(module_name::in, string::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules0, IntImportedModules,
        IntUsedModules0, IntUsedModules,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules0, ImpUsedModules, !Specs) :-

    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules0, IntImportedModules1,
        IntUsedModules0, IntUsedModules, !Specs),
    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules1, IntImportedModules,
        ImpUsedModules0, ImpUsedModules1, !Specs),

    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules1, ImpUsedModules, !Specs).

    % Report warnings for modules imported using both `:- use_module'
    % and `:- import_module'. Remove the unnecessary `:- use_module'
    % declarations.
    %
:- pred do_warn_if_duplicate_use_import_decls(module_name::in, string::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_warn_if_duplicate_use_import_decls(_ModuleName, FileName,
        !ImportedModules, !UsedModules, !Specs) :-
    set.list_to_set(!.ImportedModules, ImportedSet),
    set.list_to_set(!.UsedModules, UsedSet),
    set.intersect(ImportedSet, UsedSet, BothSet),
    ( set.is_empty(BothSet) ->
        true
    ;
        set.to_sorted_list(BothSet, BothList),

        term.context_init(FileName, 1, Context),
        Pieces = [words("Warning:"),
            words(choose_number(BothList, "module", "modules"))] ++
            component_list_to_pieces(list.map(wrap_symname, BothList)) ++
            [words(choose_number(BothList, "is", "are")),
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
        list.delete_elems(!.UsedModules, BothList, !:UsedModules)
    ).

:- func wrap_symname(module_name) = format_component.

wrap_symname(ModuleName) = sym_name(ModuleName).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

process_module_private_interfaces(_, _, [], _, _, !DirectImports,
        !DirectUses, !Module, !IO).
process_module_private_interfaces(Globals, HaveReadModuleMap,
        [Ancestor | Ancestors], IntStatusItem, ImpStatusItem, !DirectImports,
        !DirectUses, !Module, !IO) :-
    ModuleName = !.Module ^ mai_module_name,
    ModAncestors0 = !.Module ^ mai_parent_deps,
    ( Ancestor = ModuleName ->
        unexpected($module, $pred, "module is its own ancestor?")
    ; list.member(Ancestor, ModAncestors0) ->
        % We've already read it.
        process_module_private_interfaces(Globals, HaveReadModuleMap,
            Ancestors, IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Ancestor, ".int0",
            "Reading private interface for module", do_search, ReturnTimestamp,
            PrivateIntItems0, PrivateIntSpecs, PrivateIntErrors,
            _AncestorFileName, MaybeTimestamp, !IO),

        maybe_record_timestamp(Ancestor, ".int0", may_be_unqualified,
            MaybeTimestamp, !Module),

        replace_section_decls(IntStatusItem, ImpStatusItem,
            PrivateIntItems0, PrivateIntItems),

        module_and_imports_add_items(cord.from_list(PrivateIntItems), !Module),
        module_and_imports_add_specs(PrivateIntSpecs, !Module),
        module_and_imports_add_interface_error(PrivateIntErrors, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        set.intersect(PrivateIntErrors, fatal_read_module_errors,
            FatalPrivateIntErrors),
        ( if set.is_empty(FatalPrivateIntErrors) then
            ModAncestors = [Ancestor | ModAncestors0]
        else
            ModAncestors = ModAncestors0
        ),
        get_dependencies(PrivateIntItems, AncDirectImports, AncDirectUses),
        !:DirectImports = !.DirectImports ++ AncDirectImports,
        !:DirectUses = !.DirectUses ++ AncDirectUses,
        !Module ^ mai_parent_deps := ModAncestors,

        process_module_private_interfaces(Globals, HaveReadModuleMap,
            Ancestors, IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ).

%-----------------------------------------------------------------------------%

process_module_long_interfaces(_, _, _, [], _Ext, _, _,
        !IndirectImports, !ImplIndirectImports, !Module, !IO).
process_module_long_interfaces(Globals, HaveReadModuleMap, NeedQualifier,
        [Import | Imports], Ext, IntStatusItem, ImpStatusItem,
        !IndirectImports, !ImplIndirectImports, !Module, !IO) :-
    ModuleName = !.Module ^ mai_module_name,
    ModImplementationImports0 = !.Module ^ mai_impl_deps,
    (
        % Have we already read it?
        ( Import = ModuleName
        ; list.member(Import, !.Module ^ mai_parent_deps)
        ; list.member(Import, !.Module ^ mai_int_deps)
        ; list.member(Import, ModImplementationImports0)
        )
    ->
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            NeedQualifier, Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Import, Ext,
            "Reading interface for module", do_search, ReturnTimestamp,
            LongIntItems0, LongIntSpecs, LongIntErrors, _LongIntFileName,
            MaybeTimestamp, !IO),

        get_dependencies_int_imp(LongIntItems0,
            IndirectImports1, IndirectUses1,
            ImplIndirectImports1, ImplIndirectUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            LongIntItems0, LongIntItems),

        module_and_imports_add_items(cord.from_list(LongIntItems), !Module),
        module_and_imports_add_specs(LongIntSpecs, !Module),
        module_and_imports_add_interface_error(LongIntErrors, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        set.intersect(LongIntErrors, fatal_read_module_errors,
            FatalLongIntErrors),
        ( if set.is_empty(FatalLongIntErrors) then
            maybe_record_timestamp(Import, Ext, NeedQualifier, MaybeTimestamp,
                !Module),
            ModImplementationImports = [Import | ModImplementationImports0]
        else
            ModImplementationImports = ModImplementationImports0
        ),
        !:IndirectImports = !.IndirectImports ++ IndirectImports1
            ++ IndirectUses1,
        !:ImplIndirectImports = !.ImplIndirectImports
            ++ ImplIndirectImports1 ++ ImplIndirectUses1,
        !Module ^ mai_impl_deps := ModImplementationImports,

        process_module_long_interfaces(Globals, HaveReadModuleMap,
            NeedQualifier, Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ).

%-----------------------------------------------------------------------------%

process_module_short_interfaces_and_impls_transitively(Globals,
        HaveReadModuleMap, Imports, Ext, IntStatusItem, ImpStatusItem,
        !Module, !IO) :-
    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        Imports, Ext, IntStatusItem, ImpStatusItem, [], ImpIndirectImports,
        !Module, !IO),
    (
        ImpIndirectImports = []
    ;
        ImpIndirectImports = [_ | _],
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, ImpIndirectImports, Ext,
            IntStatusItem, ImpStatusItem, !Module, !IO)
    ).

process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        Imports, Ext, IntStatusItem, ImpStatusItem, !ImpIndirectImports,
        !Module, !IO) :-
    process_module_short_interfaces(Globals, HaveReadModuleMap, Imports, Ext,
        IntStatusItem, ImpStatusItem, [], IndirectImports,
        !ImpIndirectImports, !Module, !IO),
    (
        IndirectImports = []
    ;
        IndirectImports = [_ | _],
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, IndirectImports, Ext,
            IntStatusItem, ImpStatusItem, !ImpIndirectImports, !Module, !IO)
    ).

process_module_short_interfaces(_, _, [], _, _, _, !IndirectImports,
        !ImpIndirectImports, !Module, !IO).
process_module_short_interfaces(Globals, HaveReadModuleMap, [Import | Imports],
        Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
        !ImpIndirectImports, !Module, !IO) :-
    ModIndirectImports0 = !.Module ^ mai_indirect_deps,
    (
        % check if the imported module has already been imported
        ( Import = !.Module ^ mai_module_name
        ; list.member(Import, !.Module ^ mai_parent_deps)
        ; list.member(Import, !.Module ^ mai_int_deps)
        ; list.member(Import, !.Module ^ mai_impl_deps)
        ; list.member(Import, ModIndirectImports0)
        )
    ->
        process_module_short_interfaces(Globals, HaveReadModuleMap, Imports,
            Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Import, Ext,
            "Reading short interface for module", do_search,
            ReturnTimestamp, ShortIntItems0, ShortIntSpecs, ShortIntError,
            _ImportFileName, MaybeTimestamp, !IO),
        maybe_record_timestamp(Import, Ext, must_be_qualified,
            MaybeTimestamp, !Module),

        get_dependencies_int_imp(ShortIntItems0, IntImports1, IntUses1,
            ImpImports1, ImpUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            ShortIntItems0, ShortIntItems),

        module_and_imports_add_items(cord.from_list(ShortIntItems), !Module),
        module_and_imports_add_specs(ShortIntSpecs, !Module),
        module_and_imports_add_interface_error(ShortIntError, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        ModIndirectImports = [Import | ModIndirectImports0],
        !:IndirectImports = !.IndirectImports ++ IntImports1 ++ IntUses1,
        !:ImpIndirectImports = !.ImpIndirectImports ++ ImpImports1 ++ ImpUses1,
        !Module ^ mai_indirect_deps := ModIndirectImports,

        process_module_short_interfaces(Globals, HaveReadModuleMap, Imports,
            Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred init_module_and_imports(file_name::in,
    module_name::in, module_name::in, list(item)::in, list(error_spec)::in,
    list(module_name)::in, list(module_name)::in, list(string)::in,
    foreign_include_file_info_list::in,
    maybe(module_timestamps)::in, module_and_imports::out) is det.

init_module_and_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, Specs, PublicChildren, NestedChildren, FactDeps,
        ForeignIncludeFiles, MaybeTimestamps, Module) :-
    % XXX The reason why init_module_and_imports is here and not in
    % module_imports.m is this call. This should be fixed, preferably
    % by changing the module_and_imports structure.
    maybe_add_foreign_import_module(ModuleName, Items0, Items),
    ItemsCord = cord.from_list(Items),
    set.init(Errors),
    Module = module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, [], [], [], [], [], PublicChildren,
        NestedChildren, FactDeps, contains_foreign_code_unknown, [],
        ForeignIncludeFiles, contains_no_foreign_export, ItemsCord, Specs,
        Errors, MaybeTimestamps, no_main, dir.this_directory).

%-----------------------------------------------------------------------------%

:- pred maybe_return_timestamp(maybe(T)::in, maybe_return_timestamp::out)
    is det.

maybe_return_timestamp(yes(_), do_return_timestamp).
maybe_return_timestamp(no, do_not_return_timestamp).

:- pred maybe_record_timestamp(module_name::in, string::in, need_qualifier::in,
    maybe(timestamp)::in, module_and_imports::in, module_and_imports::out)
    is det.

maybe_record_timestamp(ModuleName, Suffix, NeedQualifier, MaybeTimestamp,
        !Module) :-
    (
        !.Module ^ mai_maybe_timestamps = yes(Timestamps0),
        (
            MaybeTimestamp = yes(Timestamp),
            TimestampInfo = module_timestamp(Suffix, Timestamp, NeedQualifier),
            map.set(ModuleName, TimestampInfo, Timestamps0, Timestamps),
            !Module ^ mai_maybe_timestamps := yes(Timestamps)
        ;
            MaybeTimestamp = no
        )
    ;
        !.Module ^ mai_maybe_timestamps = no
    ).

%-----------------------------------------------------------------------------%

    % At this point, we have read in all the appropriate interface files,
    % including, for every imported/used module, at least the short
    % interface for that module's parent module, which will contain
    % the `include_module' declarations for any exported sub-modules
    % of the parent. So the accessible sub-modules can be determined
    % by just calling get_accessible_children on the complete item list.
    %
    % We then go through all of the imported/used modules,
    % checking that each one is accessible.
    %
:- pred check_imports_accessibility(module_name::in, list(module_name)::in,
    list(item)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_imports_accessibility(ModuleName, Imports, Items, !Specs) :-
    get_accessible_children(Items, AccessibleSubModules),
    list.foldl(check_module_accessibility(ModuleName,
        AccessibleSubModules, Items), Imports, !Specs).

:- pred check_module_accessibility(module_name::in, list(module_name)::in,
    list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_module_accessibility(ModuleName, AccessibleSubModules, Items,
        ImportedModule, !Specs) :-
    ( ImportedModule = qualified(ParentModule, SubModule) ->
        ( list.member(ImportedModule, AccessibleSubModules) ->
            true
        ;
            % The user attempted to import an inaccessible submodule,
            % so report an error. Unfortunately we didn't get passed the
            % context(s) of the `import_module' or `use_module' declaration(s),
            % so we need to search the item list again to find them.
            FindImports = (pred(Item::in, ImportInfo::out) is semidet :-
                Item = item_module_defn(ItemModuleDefn),
                ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _),
                (
                    ModuleDefn = md_import(ItemModuleSpecs),
                    DeclName = "import_module"
                ;
                    ModuleDefn = md_use(ItemModuleSpecs),
                    DeclName = "use_module"
                ),
                list.member(ImportedModule, ItemModuleSpecs),
                ImportInfo = DeclName - Context
            ),
            list.filter_map(FindImports, Items, ImportInfos),
            (
                ImportInfos = [],
                unexpected($module, $pred, "check_parent_module")
            ;
                ImportInfos = [_ | _],
                list.foldl(
                    report_inaccessible_module_error(ModuleName,
                        ParentModule, SubModule),
                    ImportInfos, !Specs)
            )
        )
    ;
        true
    ).

:- pred report_inaccessible_module_error(module_name::in, module_name::in,
    string::in, pair(string, prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

% The error message should come out like this
% (the second sentence is included only with --verbose-errors):
% very_long_name.m:123: In module `very_long_name':
% very_long_name.m:123:   error in `import_module' declaration:
% very_long_name.m:123:   module `parent_module.sub_module' is inaccessible.
% very_long_name.m:123:   Either there was no prior `import_module' or
% very_long_name.m:123:   `use_module' declaration to import module
% very_long_name.m:123:   `parent_module', or the interface for module
% very_long_name.m:123:   `parent_module' does not contain an `include_module'
% very_long_name.m:123:   declaration for module `sub_module'.

report_inaccessible_module_error(ModuleName, ParentModule, SubModule,
        DeclName - Context, !Specs) :-
    MainPieces = [words("In module"), sym_name(ModuleName), suffix(":"), nl,
        words("error in"), quote(DeclName), words("declaration:"), nl,
        words("module"), sym_name(qualified(ParentModule, SubModule)),
        words("is inaccessible."), nl],
    VerbosePieces = [words("Either there was no prior"),
        quote("import_module"),
            words("or"), quote("use_module"),
            words("declaration to import module"), sym_name(ParentModule),
            suffix(","), words("or the interface for module"),
            sym_name(ParentModule), words("does not contain an"),
            quote("include_module"), words("declaration for module"),
            quote(SubModule), suffix("."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

    % replace_section_decls(IntStatusItem, ImpStatusItem, !Items):
    %
    % Replace all occurrences of `:- interface' with IntStatusItem
    % (this will usually be an item which sets the import status).
    % Replace all occurrences of `:- implementation' with ImpStatusItem.
    %
:- pred replace_section_decls(item::in, item::in,
    list(item)::in, list(item)::out) is det.

replace_section_decls(IntStatusItem, ImpStatusItem, !Items) :-
    list.map(replace_section_decl(IntStatusItem, ImpStatusItem), !Items).

:- pred replace_section_decl(item::in, item::in, item::in, item::out) is det.

replace_section_decl(IntStatusItem, ImpStatusItem, Item0, Item) :-
    (
        Item0 = item_module_defn(ItemModuleDefn0),
        ItemModuleDefn0 = item_module_defn_info(ModuleDefn0, _, _),
        (
            ModuleDefn0 = md_interface,
            ItemPrime = IntStatusItem
        ;
            ModuleDefn0 = md_implementation,
            ItemPrime = ImpStatusItem
        )
    ->
        Item = ItemPrime
    ;
        Item = Item0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_module_dependencies(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_file_dependencies(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_module_dependency_file(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_d_file_only, do_search, ModuleName,
        DepsMap, !IO).

generate_file_dependency_file(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_d_file_only, do_search, ModuleName,
        DepsMap, !IO).

%-----------------------------------------------------------------------------%

:- pred build_deps_map(globals::in, file_name::in,
    module_name::out, deps_map::out, io::di, io::uo) is det.

build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO) :-
    % Read in the top-level file (to figure out its module name).
    read_module_from_file(Globals, FileName, ".m", "Reading file",
        do_not_search, do_not_return_timestamp, Items, Specs0, Error,
        ModuleName, _, !IO),
    split_into_submodules(ModuleName, Items, SubModuleList, Specs0, Specs),
    % XXX _NumErrors
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
    assoc_list.keys(SubModuleList, SubModuleNames),
    SourceFileName = FileName ++ ".m",
    list.map(init_dependencies(SourceFileName, ModuleName, SubModuleNames,
        [], Error, Globals), SubModuleList, ModuleImportsList),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, ModuleImportsList, DepsMap0, DepsMap).

:- type generate_dependencies_mode
    --->    output_d_file_only
    ;       output_all_dependencies.

:- pred generate_dependencies(globals::in, generate_dependencies_mode::in,
    maybe_search::in, module_name::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies(Globals, Mode, Search, ModuleName, DepsMap0, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(Globals, ModuleName, Search, DepsMap0, DepsMap, !IO),

    % Check whether we could read the main `.m' file.

    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, ModuleImports),
    Errors = ModuleImports ^ mai_errors,
    set.intersect(Errors, fatal_read_module_errors, FatalErrors),
    ( if set.is_non_empty(FatalErrors) then
        ModuleString = sym_name_to_string(ModuleName),
        ( if set.contains(FatalErrors, rme_could_not_open_file) then
            string.append_list(["cannot read source file for module `",
                ModuleString, "'."], Message)
        else
            string.append_list(["cannot parse source file for module `",
                ModuleString, "'."], Message)
        ),
        report_error(Message, !IO)
    else
        (
            Mode = output_d_file_only
        ;
            Mode = output_all_dependencies,
            module_and_imports_get_source_file_name(ModuleImports,
                SourceFileName),
            generate_dependencies_write_dv_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO),
            generate_dependencies_write_dep_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO)
        ),

        % Compute the interface deps graph and the implementation deps
        % graph from the deps map.

        digraph.init(IntDepsGraph0),
        digraph.init(ImplDepsGraph0),
        map.values(DepsMap, DepsList),
        deps_list_to_deps_graph(DepsList, DepsMap, IntDepsGraph0, IntDepsGraph,
            ImplDepsGraph0, ImplDepsGraph),
        maybe_output_imports_graph(Globals, ModuleName,
            IntDepsGraph, ImplDepsGraph, !IO),

        % Compute the trans-opt deps ordering, by doing an approximate
        % topological sort of the implementation deps, and then finding
        % the subset of those for which of those we have (or can make)
        % trans-opt files.

        digraph.atsort(ImplDepsGraph, ImplDepsOrdering0),
        maybe_output_module_order(Globals, ModuleName, ImplDepsOrdering0, !IO),
        list.map(set.to_sorted_list, ImplDepsOrdering0, ImplDepsOrdering),
        list.condense(ImplDepsOrdering, TransOptDepsOrdering0),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_opt_deps(Globals, yes, TransOptDepsOrdering0, IntermodDirs,
            ".trans_opt", TransOptDepsOrdering, !IO),

        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            digraph.to_assoc_list(ImplDepsGraph, ImplDepsAL),
            io.print("ImplDepsAL:\n", !TIO),
            io.write_list(ImplDepsAL, "\n", print, !TIO),
            io.nl(!TIO)
        ),

        % Compute the indirect dependencies: they are equal to the composition
        % of the implementation dependencies with the transitive closure of the
        % implementation dependencies. (We used to take the transitive closure
        % of the interface dependencies, but we now include implementation
        % details in the interface files).

        digraph.tc(ImplDepsGraph, TransImplDepsGraph),
        digraph.compose(ImplDepsGraph, TransImplDepsGraph, IndirectDepsGraph),

        % Compute the indirect optimization dependencies: indirect
        % dependencies including those via `.opt' or `.trans_opt' files.
        % Actually we cannot compute that, since we don't know
        % which modules the `.opt' files will import!
        % Instead, we need to make a conservative (over-)approximation,
        % and assume that the each module's `.opt' file might import any
        % of that module's implementation dependencies; in actual fact,
        % it will be some subset of that.

        digraph.tc(ImplDepsGraph, IndirectOptDepsGraph),

        (
            Mode = output_d_file_only,
            DFilesToWrite = [ModuleDep]
        ;
            Mode = output_all_dependencies,
            DFilesToWrite = DepsList
        ),
        generate_dependencies_write_d_files(Globals, DFilesToWrite,
            IntDepsGraph, ImplDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsOrdering, DepsMap, !IO)
    ),

    % For Java, the main target is actually a shell script which will
    % set CLASSPATH appropriately and invoke java on the appropriate
    % .class file. Rather than generating an Mmake rule to build this
    % file when it is needed, we just generate this file "mmake depend"
    % time, since that is simpler and probably more efficient anyway.

    globals.get_target(Globals, Target),
    (
        Target = target_java,
        Mode = output_all_dependencies
    ->
        create_java_shell_script(Globals, ModuleName, _Succeeded, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

get_children(Items, IncludeDeps) :-
    get_children_acc(Items, [], IncludeDeps).

:- pred get_children_acc(list(item)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_children_acc([], !IncludeDeps).
get_children_acc([Item | Items], !IncludeDeps) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ModuleDefn = md_include_module(Modules)
    ->
        !:IncludeDeps = !.IncludeDeps ++ Modules
    ;
        true
    ),
    get_children_acc(Items, !IncludeDeps).

    % get_accessible_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items which are visible in the current module.
    %
:- pred get_accessible_children(list(item)::in, list(module_name)::out) is det.

get_accessible_children(Items, IncludeDeps) :-
    get_accessible_children_acc(yes, Items, [], IncludeDeps).

:- pred get_accessible_children_acc(bool::in, list(item)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_accessible_children_acc(_, [], !IncludeDeps).
get_accessible_children_acc(!.Visible, [Item | Items], !IncludeDeps) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ( ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ),
            !:Visible = no
        ;
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ),
            !:Visible = yes
        ;
            ModuleDefn = md_include_module(Modules),
            (
                !.Visible = yes,
                !:IncludeDeps = !.IncludeDeps ++ Modules
            ;
                !.Visible = no
            )
        ;
            ( ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_version_numbers(_, _)
            )
            % Do nothing.
        )
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        )
    ),
    get_accessible_children_acc(!.Visible, Items, !IncludeDeps).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type submodule_map == map(module_name, list(item)).

split_into_submodules(ModuleName, Items0, ModuleList, !Specs) :-
    InParentInterface = no,
    split_into_submodules_2(ModuleName, Items0, InParentInterface,
        LeftOverItems, ModuleList, !Specs),

    % Check that there are no items after the end_module declaration.
    (
        LeftOverItems = []
    ;
        LeftOverItems = [FirstLeftOverItem | _],
        Context = get_item_context(FirstLeftOverItem),
        report_items_after_end_module(Context, !Specs)
    ),

    % Check for modules declared as both nested and separate sub-modules.
    get_children(Items0, NestedSubmodules),
    assoc_list.keys(ModuleList, SeparateSubModules),
    Duplicates = set.intersect(
        set.list_to_set(NestedSubmodules),
        set.list_to_set(SeparateSubModules)),
    ( set.is_empty(Duplicates) ->
        true
    ;
        report_duplicate_modules(Duplicates, Items0, !Specs)
    ).

:- pred split_into_submodules_2(module_name::in, list(item)::in, bool::in,
    list(item)::out, module_list::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_2(ModuleName, Items0, InParentInterface, Items,
        ModuleList, !Specs) :-
    InInterface0 = no,
    split_into_submodules_3(ModuleName, Items0,
        InParentInterface, InInterface0,
        ThisModuleItems, Items, SubModules, !Specs),
    map.to_assoc_list(SubModules, SubModuleList),
    ModuleList = [ModuleName - ThisModuleItems | SubModuleList].

:- pred split_into_submodules_3(module_name::in, list(item)::in, bool::in,
    bool::in, list(item)::out, list(item)::out,
    map(module_name, list(item))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_3(_ModuleName, [], _, _, [], [], SubModules, !Specs) :-
    map.init(SubModules).
split_into_submodules_3(ModuleName, [Item | Items1],
        InParentInterface, !.InInterface,
        ThisModuleItems, OtherItems, SubModules, !Specs) :-
    (
        % Check for a `module' declaration, which signals the start
        % of a nested module.
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart =
            item_module_start_info(SubModuleName, Context, SeqNum)
    ->
        % Parse the items for the nested submodule.
        split_into_submodules_2(SubModuleName, Items1, !.InInterface,
            Items2, SubModules0, !Specs),

        % Parse the remaining items for this module.
        split_into_submodules_3(ModuleName, Items2,
            InParentInterface, !.InInterface,
            ThisModuleItems0, Items3, SubModules1, !Specs),

        % Combine the submodule declarations from the previous two steps.
        list.foldl(add_submodule, SubModules0, SubModules1, SubModules),

        % Replace the nested submodule with an `include_module' declaration.
        IncludeSubModModuleDefn = md_include_module([SubModuleName]),
        IncludeSubModItemModuleDefn = item_module_defn_info(
            IncludeSubModModuleDefn, Context, SeqNum),
        IncludeSubModItem = item_module_defn(IncludeSubModItemModuleDefn),
        ThisModuleItems = [IncludeSubModItem | ThisModuleItems0],
        OtherItems = Items3
    ;
        % Check for a matching `end_module' declaration.
        Item = item_module_end(ItemModuleEnd),
        ItemModuleEnd = item_module_end_info(EndModuleName, _, _),
        EndModuleName = ModuleName
    ->
        % If so, that's the end of this module.
        ThisModuleItems = [],
        OtherItems = Items1,
        map.init(SubModules)
    ;
        % Otherwise, process the next item in this module.

        % Update the flag which records whether we're currently in the
        % interface section, and report an error if there is an
        % `implementation' section inside an `interface' section.
        ( Item = item_module_defn(ItemModuleDefn) ->
            ItemModuleDefn = item_module_defn_info(ModuleDefn, Context,
                _SeqNum),
            ( ModuleDefn = md_interface ->
                !:InInterface = yes
            ; ModuleDefn = md_implementation ->
                !:InInterface = no,
                (
                    InParentInterface = yes,
                    report_error_implementation_in_interface(ModuleName,
                        Context, !Specs)
                ;
                    InParentInterface = no
                )
            ;
                true
            )
        ;
            true
        ),

        % Check to make sure that a non-abstract instance declaration
        % does not occur in a module interface.
        (
            !.InInterface = yes,
            Item = item_instance(ItemInstance),
            ItemInstance ^ ci_method_instances \= instance_body_abstract
        ->
            InstanceContext = ItemInstance ^ ci_context,
            report_non_abstract_instance_in_interface(InstanceContext, !Specs)
        ;
            true
        ),

        % Parse the remaining items for this module.
        split_into_submodules_3(ModuleName, Items1,
            InParentInterface, !.InInterface,
            ThisModuleItems0, Items2, SubModules, !Specs),

        % Put the current item back onto the front of the item list
        % for this module.
        ThisModuleItems = [Item | ThisModuleItems0],
        OtherItems = Items2
    ).

:- pred add_submodule(pair(module_name, list(item))::in,
    submodule_map::in, submodule_map::out) is det.

add_submodule(ModuleName - ModuleItemList, !SubModules) :-
    % If the same module name occurs twice, then just append the lists of items
    % together. Perhaps we should be a bit more strict about this, for example
    % by only allowing one `:- implementation' section and one `:- interface'
    % section for each module? (That is what the Mercury language reference
    % manual mandates. On the other hand, it also says that top-level modules
    % should only have one `:- interface' and one `:- implementation' section,
    % and we don't enforce that either...)
    ( map.search(!.SubModules, ModuleName, ItemList0) ->
        list.append(ModuleItemList, ItemList0, ItemList),
        map.det_update(ModuleName, ItemList, !SubModules)
    ;
        map.det_insert(ModuleName, ModuleItemList, !SubModules)
    ).

%-----------------------------------------------------------------------------%

:- pred report_error_implementation_in_interface(module_name::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_error_implementation_in_interface(ModuleName, Context, !Specs) :-
    (
        ModuleName = qualified(ParentModule0, ChildModule0),
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        ModuleName = unqualified(_),
        unexpected($module, $pred, "unqualified module name")
    ),
    Pieces = [words("In interface for module"), sym_name(ParentModule),
        suffix(":"), nl, words("in definition of sub-module"),
        quote(ChildModule), suffix(":"), nl,
        words("error:"), decl("implementation"),
        words("declaration for sub-module\n"),
        words("occurs in interface section of parent module.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_duplicate_modules(set(module_name)::in, list(item)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_modules(Duplicates, Items, !Specs) :-
    list.filter_map(is_duplicate_error(Duplicates), Items,
        DuplicateErrorLists),
    list.condense(DuplicateErrorLists, DuplicateErrors),
    list.foldl(report_error_duplicate_module_decl, DuplicateErrors, !Specs).

:- pred is_duplicate_error(set(module_name)::in, item::in,
    list(pair(module_name, prog_context))::out) is semidet.

is_duplicate_error(DuplicatesSet, Item, SubModuleNameContexts) :-
    (
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart = item_module_start_info(SubModuleName, Context, _),
        set.member(SubModuleName, DuplicatesSet),
        SubModuleNameContexts = [SubModuleName - Context]
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _),
        ModuleDefn = md_include_module(SubModuleNames),
        set.list_to_set(SubModuleNames, SubModuleNamesSet),
        set.intersect(SubModuleNamesSet, DuplicatesSet,
            DuplicatedSubModuleNamesSet),
        set.to_sorted_list(DuplicatedSubModuleNamesSet,
            DuplicatedSubModuleNames),
        SubModuleNameContexts =
            list.map(pair_with_context(Context), DuplicatedSubModuleNames)
    ).

:- func pair_with_context(prog_context, module_name) =
    pair(module_name, prog_context).

pair_with_context(Context, ModuleName) = ModuleName - Context.

:- pred report_error_duplicate_module_decl(pair(module_name, prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_error_duplicate_module_decl(ModuleName - Context, !Specs) :-
    (
        ModuleName = qualified(ParentModule0, ChildModule0),
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        ModuleName = unqualified(_),
        unexpected($module, $pred, "unqualified module name")
    ),
    Pieces = [words("In module"), sym_name(ParentModule), suffix(":"), nl,
        words("error: sub-module"), quote(ChildModule), words("declared"),
        words("as both a separate sub-module and a nested sub-module.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_items_after_end_module(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_items_after_end_module(Context, !Specs) :-
    Pieces = [words("Error: item(s) after end_module declaration.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred report_non_abstract_instance_in_interface(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_non_abstract_instance_in_interface(Context, !Specs) :-
    Pieces = [words("Error: non-abstract instance declaration"),
        words("in module interface.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.modules.
%-----------------------------------------------------------------------------%
