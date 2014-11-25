%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_imports.m.
% Main author: fjh.
%
% This module contains the data structure for recording module imports
% and its access predicates.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_imports.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_error.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % When doing smart recompilation record for each module the suffix of
    % the file that was read and the modification time of the file.
    %
:- type module_timestamps == map(module_name, module_timestamp).
:- type module_timestamp
    --->    module_timestamp(
                suffix          :: string,
                timestamp       :: timestamp,
                need_qualifier  :: need_qualifier
            ).

    % The `module_and_imports' structure holds information about
    % a module and the modules that it imports.
    %
    % Note that we build this structure up as we go along.
    % When generating the dependencies (for `--generate-dependencies'), the
    % two fields that hold the direct imports do not include the imports via
    % ancestors when the module is first read in; the ancestor imports are
    % added later, once all the modules have been read in.  Similarly the
    % indirect imports field is initially set to the empty list and filled
    % in later.
    %
    % When compiling or when making interface files, the same sort of thing
    % applies: initially all the list(module_name) fields except the public
    % children field are set to empty lists, and then we add ancestor
    % modules and imported modules to their respective lists as we process
    % the interface files for those imported or ancestor modules.
    %
:- type module_and_imports
    --->    module_and_imports(
                % The source file.
                mai_source_file_name            :: file_name,

                % The name of the top-level module in the source file
                % containing the module that we are compiling.
                mai_source_file_module_name     :: module_name,

                % The module (or sub-module) that we are compiling.
                mai_module_name                 :: module_name,

                % The list of ancestor modules it inherits.
                mai_parent_deps                 :: list(module_name),

                % The list of modules it directly imports in the interface
                % (imports via ancestors count as direct).
                mai_int_deps                    :: list(module_name),

                % The list of modules it directly imports in the
                % implementation.
                mai_impl_deps                   :: list(module_name),

                % The list of modules it indirectly imports.
                mai_indirect_deps               :: list(module_name),

                mai_children                    :: list(module_name),

                % The list of its public children, i.e. child modules that
                % it includes in the interface section.
                mai_public_children             :: list(module_name),

                % The modules included in the same source file. This field
                % is only set for the top-level module in each file.
                mai_nested_children             :: list(module_name),

                % The list of filenames for fact tables in this module.
                mai_fact_table_deps             :: list(string),

                % Whether or not the module contains foreign code, and if yes,
                % which languages they use.
                mai_has_foreign_code            :: contains_foreign_code,

                % The `:- pragma foreign_import_module' declarations.
                mai_foreign_import_modules  :: foreign_import_module_info_list,

                % The list of filenames referenced by `:- pragma foreign_decl'
                % or `:- pragma foreign_code' declarations.
                mai_foreign_include_files   :: foreign_include_file_info_list,

                % Does the module contain any `:- pragma foreign_export'
                % declarations?
                mai_contains_foreign_export     :: contains_foreign_export,

                % The contents of the module and its imports.
                mai_items_cord                  :: cord(item),

                % Whether an error has been encountered when reading in
                % this module.
                mai_specs                       :: list(error_spec),
                mai_errors                      :: read_module_errors,

                % If we are doing smart recompilation, we need to keep
                % the timestamps of the modules read in.
                mai_maybe_timestamps            :: maybe(module_timestamps),

                % Does this module contain main/2?
                mai_has_main                    :: has_main,

                % The directory containing the module source.
                mai_module_dir                  :: dir_name
            ).

:- pred module_and_imports_get_source_file_name(module_and_imports::in,
    file_name::out) is det.
:- pred module_and_imports_get_module_name(module_and_imports::in,
    module_name::out) is det.
:- pred module_and_imports_get_impl_deps(module_and_imports::in,
    list(module_name)::out) is det.

    % Set the interface dependencies.
    %
:- pred module_and_imports_set_int_deps(list(module_name)::in,
    module_and_imports::in, module_and_imports::out) is det.

    % Set the implementation dependencies.
    %
:- pred module_and_imports_set_impl_deps(list(module_name)::in,
    module_and_imports::in, module_and_imports::out) is det.

    % Set the indirect dependencies.
    %
:- pred module_and_imports_set_indirect_deps(list(module_name)::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_set_errors(read_module_errors::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_specs(list(error_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_add_interface_error(read_module_errors::in,
    module_and_imports::in, module_and_imports::out) is det.

    % Add items to the end of the list.
    %
:- pred module_and_imports_add_items(cord(item)::in,
    module_and_imports::in, module_and_imports::out) is det.

    % Return the results recorded in the module_and_imports structure.
    %
    % There is no predicate to return *just* the items, since that would
    % allow callers to forget to retrieve and then print the error
    % specifications.
    %
:- pred module_and_imports_get_results(module_and_imports::in,
    list(item)::out, list(error_spec)::out, read_module_errors::out) is det.

%-----------------------------------------------------------------------------%

    % init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
    %   Specs, Errors, Globals, ModuleName - Items, ModuleImports).
    %
:- pred init_dependencies(file_name::in, module_name::in,
    list(module_name)::in, list(error_spec)::in, read_module_errors::in,
    globals::in, pair(module_name, list(item))::in,
    module_and_imports::out) is det.

%-----------------------------------------------------------------------------%

    % get_dependencies(Items, ImportDeps, UseDeps):
    %
    % Get the list of modules that a list of items (explicitly) depends on.
    % ImportDeps is the list of modules imported using `:- import_module',
    % UseDeps is the list of modules imported using `:- use_module'.
    % N.B. Typically you also need to consider the module's implicit
    % dependencies (see get_implicit_dependencies/3), its parent modules
    % (see get_ancestors/1) and possibly also the module's child modules
    % (see get_children/2). You may also need to consider indirect
    % dependencies.
    %
:- pred get_dependencies(list(item)::in, list(module_name)::out,
    list(module_name)::out) is det.

    % get_dependencies_int_imp(Items, IntImportDeps, IntUseDeps,
    %   ImpImportDeps, ImpUseDeps):
    %
    % Get the list of modules that a list of items (explicitly) depends on.
    %
    % IntImportDeps is the list of modules imported using `:- import_module'
    % in the interface, and ImpImportDeps those modules imported in the
    % implementation. IntUseDeps is the list of modules imported using
    % `:- use_module' in the interface, and ImpUseDeps those modules imported
    % in the implementation.
    %
    % N.B. Typically you also need to consider the module's implicit
    % dependencies (see get_implicit_dependencies/3), its parent modules
    % (see get_ancestors/1) and possibly also the module's child modules
    % (see get_children/2). You may also need to consider indirect
    % dependencies.
    %
    % N.B This predicate assumes that any declarations between the `:- module'
    % and the first `:- interface' or `:- implementation' are in the
    % implementation.
    %
:- pred get_dependencies_int_imp(list(item)::in,
    list(module_name)::out, list(module_name)::out,
    list(module_name)::out, list(module_name)::out) is det.

    % get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps):
    %
    % Get the list of builtin modules (e.g. "public_builtin",
    % "private_builtin") that a list of items may implicitly depend on.
    % ImportDeps is the list of modules which should be automatically
    % implicitly imported as if via `:- import_module', and UseDeps is
    % the list which should be automatically implicitly imported as if via
    % `:- use_module'.
    %
:- pred get_implicit_dependencies(list(item)::in, globals::in,
    list(module_name)::out, list(module_name)::out) is det.

    % Get the fact table dependencies for a module.
    %
:- pred get_fact_table_dependencies(list(item)::in, list(string)::out) is det.

    % Get foreign include_file dependencies for a module.
    % This replicates part of get_item_list_foreign_code.
    %
:- pred get_foreign_include_files(list(item)::in,
    foreign_include_file_info_list::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.modules.    % undesirable dependency

:- import_module bool.
:- import_module dir.
:- import_module require.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

module_and_imports_get_source_file_name(Module, Module ^ mai_source_file_name).
module_and_imports_get_module_name(Module, Module ^ mai_module_name).
module_and_imports_get_impl_deps(Module, Module ^ mai_impl_deps).

module_and_imports_set_int_deps(IntDeps, !Module) :-
    !Module ^ mai_int_deps := IntDeps.
module_and_imports_set_impl_deps(ImplDeps, !Module) :-
    !Module ^ mai_impl_deps := ImplDeps.
module_and_imports_set_indirect_deps(IndirectDeps, !Module) :-
    !Module ^ mai_indirect_deps := IndirectDeps.
module_and_imports_set_errors(Errors, !Module) :-
    !Module ^ mai_errors := Errors.

module_and_imports_add_specs(NewSpecs, !Module) :-
    Specs0 = !.Module ^ mai_specs,
    Specs = NewSpecs ++ Specs0,
    !Module ^ mai_specs := Specs.

module_and_imports_add_items(NewItems, !Module) :-
    Items0 = !.Module ^ mai_items_cord,
    Items = Items0 ++ NewItems,
    !Module ^ mai_items_cord := Items.

module_and_imports_add_interface_error(InterfaceErrors, !Module) :-
    Errors0 = !.Module ^ mai_errors,
    set.union(Errors0, InterfaceErrors, Errors),
    !Module ^ mai_errors := Errors.

module_and_imports_get_results(Module, Items, Specs, Errors) :-
    Items = cord.list(Module ^ mai_items_cord),
    Specs = Module ^ mai_specs,
    Errors = Module ^ mai_errors.

%-----------------------------------------------------------------------------%

init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
        Specs, Errors, Globals, ModuleName - Items, ModuleImports) :-
    ParentDeps = get_ancestors(ModuleName),

    get_dependencies(Items, ImplImportDeps0, ImplUseDeps0),
    get_implicit_dependencies(Items, Globals,
        ImplicitImplImportDeps, ImplicitImplUseDeps),
    ImplImportDeps = ImplicitImplImportDeps ++ ImplImportDeps0,
    ImplUseDeps = ImplicitImplUseDeps ++ ImplUseDeps0,
    ImplementationDeps = ImplImportDeps ++ ImplUseDeps,

    get_interface(ModuleName, no, Items, InterfaceItems),
    get_dependencies(InterfaceItems,
        InterfaceImportDeps0, InterfaceUseDeps0),
    get_implicit_dependencies(InterfaceItems, Globals,
        ImplicitInterfaceImportDeps, ImplicitInterfaceUseDeps),
    InterfaceImportDeps = ImplicitInterfaceImportDeps ++ InterfaceImportDeps0,
    InterfaceUseDeps = ImplicitInterfaceUseDeps ++ InterfaceUseDeps0,
    InterfaceDeps = InterfaceImportDeps ++ InterfaceUseDeps,

    % We don't fill in the indirect dependencies yet.
    IndirectDeps = [],

    get_children(Items, IncludeDeps),
    get_children(InterfaceItems, InterfaceIncludeDeps),

    ( ModuleName = SourceFileModuleName ->
        list.delete_all(NestedModuleNames, ModuleName, NestedDeps)
    ;
        NestedDeps = []
    ),

    get_fact_table_dependencies(Items, FactTableDeps),

    % Figure out whether the items contain foreign code.
    get_item_list_foreign_code(Globals, Items, LangSet,
        ForeignImports0, ForeignIncludeFiles, ContainsForeignExport),
    ( set.is_empty(LangSet) ->
        ContainsForeignCode = contains_no_foreign_code
    ;
        ContainsForeignCode = contains_foreign_code(LangSet)
    ),

    % If this module contains `:- pragma foreign_export' or
    % `:- pragma foreign_type' declarations, importing modules
    % may need to import its `.mh' file.
    get_foreign_self_imports(Items, SelfImportLangs),
    ForeignSelfImports = list.map(
        (func(Lang) = foreign_import_module_info(Lang, ModuleName,
            term.context_init)),
        SelfImportLangs),
    ForeignImports = ForeignSelfImports ++ ForeignImports0,

    % Work out whether the items contain main/2.
    (
        % We use find_first_match, even though we are not interested in the
        % match, because doing a nondet generate-and-test runs us out of nondet
        % stack in the asm_fast.gc.debug.stseg grade.
        ItemDeclaresMain = (pred(Item::in) is semidet :-
            Item = item_pred_decl(ItemPredDecl),
            ItemPredDecl = item_pred_decl_info(_, _, _, _, pf_predicate, Name,
                [_, _], WithType, _, _, _, _, _, _, _),
            unqualify_name(Name) = "main",

            % XXX We should allow `main/2' to be declared using `with_type`,
            % but equivalences haven't been expanded at this point.
            % The `has_main' field is only used for some special case handling
            % of the module containing main for the IL backend (we generate
            % a `.exe' file rather than a `.dll' file). This would arguably
            % be better done by generating a `.dll' file as normal, and a
            % separate `.exe' file containing initialization code and a call
            % to `main/2', as we do with the `_init.c' file in the C backend.
            WithType = no
        ),
        list.find_first_match(ItemDeclaresMain, Items, _FirstItemDeclaringMain)
    ->
        HasMain = has_main
    ;
        HasMain = no_main
    ),

    ModuleImports = module_and_imports(FileName, SourceFileModuleName,
        ModuleName, ParentDeps, InterfaceDeps,
        ImplementationDeps, IndirectDeps, IncludeDeps,
        InterfaceIncludeDeps, NestedDeps, FactTableDeps,
        ContainsForeignCode, ForeignImports, ForeignIncludeFiles,
        ContainsForeignExport,
        cord.empty, Specs, Errors, no, HasMain, dir.this_directory).

%-----------------------------------------------------------------------------%

get_dependencies(Items, ImportDeps, UseDeps) :-
    get_dependencies_int_imp(Items, IntImportDeps, IntUseDeps,
        ImpImportDeps, ImpUseDeps),
    ImportDeps = IntImportDeps ++ ImpImportDeps,
    UseDeps = IntUseDeps ++ ImpUseDeps.

get_dependencies_int_imp(Items, IntImportDeps, IntUseDeps,
        ImpImportDeps, ImpUseDeps) :-
    get_dependencies_implementation(Items,
        set.init, IntImportDepsSet, set.init, IntUseDepsSet,
        set.init, ImpImportDepsSet, set.init, ImpUseDepsSet),
    IntImportDeps = set.to_sorted_list(IntImportDepsSet),
    ImpImportDeps = set.to_sorted_list(ImpImportDepsSet),
    IntUseDeps = set.to_sorted_list(IntUseDepsSet),
    ImpUseDeps = set.to_sorted_list(ImpUseDepsSet).

:- pred get_dependencies_implementation(list(item)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

get_dependencies_implementation([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_implementation([Item | Items],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ( Item = item_module_defn(ItemModuleDefn) ->
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_interface ->
            get_dependencies_interface(Items,
                !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
        ;
            ( ModuleDefn = md_import(Modules) ->
                set.insert_list(Modules, !ImpImportDeps)
            ; ModuleDefn = md_use(Modules) ->
                set.insert_list(Modules, !ImpUseDeps)
            ;
                true
            ),
            get_dependencies_implementation(Items,
                !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
        )
    ;
        get_dependencies_implementation(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

:- pred get_dependencies_interface(list(item)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

get_dependencies_interface([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_interface([Item | Items],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ( Item = item_module_defn(ItemModuleDefn) ->
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_implementation ->
            get_dependencies_implementation(Items,
                !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
        ;
            ( ModuleDefn = md_import(Modules) ->
                set.insert_list(Modules, !IntImportDeps)
            ; ModuleDefn = md_use(Modules) ->
                set.insert_list(Modules, !IntUseDeps)
            ;
                true
            ),
            get_dependencies_interface(Items,
                !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
        )
    ;
        get_dependencies_interface(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

%-----------------------------------------------------------------------------%

get_implicit_dependencies(Items, Globals, !:ImportDeps, !:UseDeps) :-
    !:ImportDeps = [mercury_public_builtin_module],
    !:UseDeps = [mercury_private_builtin_module],
    ImplicitImportNeeds0 = implicit_import_needs(
        dont_need_tabling, dont_need_tabling_statistics,
        dont_need_stm, dont_need_exception, dont_need_format),
    gather_implicit_import_needs_in_items(Items,
        ImplicitImportNeeds0, ImplicitImportNeeds),
    ImplicitImportNeeds = implicit_import_needs(
        ItemsNeedTabling, ItemsNeedTablingStatistics,
        ItemsNeedSTM, ItemsNeedException, ItemsNeedFormat),
    % We should include mercury_table_builtin_module if the Items contain
    % a tabling pragma, or if one of --use-minimal-model (either kind) and
    % --trace-table-io is specified. In the former case, we may also need
    % to import mercury_table_statistics_module.
    (
        ItemsNeedTabling = do_need_tabling,
        !:UseDeps = [mercury_table_builtin_module | !.UseDeps],
        (
            ItemsNeedTablingStatistics = do_need_tabling_statistics,
            !:UseDeps = [mercury_table_statistics_module | !.UseDeps]
        ;
            ItemsNeedTablingStatistics = dont_need_tabling_statistics
        )
    ;
        ItemsNeedTabling = dont_need_tabling,
        expect(unify(ItemsNeedTablingStatistics, dont_need_tabling_statistics),
            $module, $pred, "tabling statistics without tabling"),
        (
            % These forms of tabling cannot ask for statistics.
            (
                globals.lookup_bool_option(Globals,
                    use_minimal_model_stack_copy, yes)
            ;
                globals.lookup_bool_option(Globals,
                    use_minimal_model_own_stacks, yes)
            ;
                globals.lookup_bool_option(Globals, trace_table_io, yes)
            )
        ->
            !:UseDeps = [mercury_table_builtin_module | !.UseDeps]
        ;
            true
        )
    ),
    (
        ItemsNeedSTM = do_need_stm,
        !:UseDeps = [mercury_stm_builtin_module, mercury_exception_module,
            mercury_univ_module | !.UseDeps]
    ;
        ItemsNeedSTM = dont_need_stm
    ),
    (
        ItemsNeedException = do_need_exception,
        !:UseDeps = [mercury_exception_module | !.UseDeps]
    ;
        ItemsNeedException = dont_need_exception
    ),
    (
        ItemsNeedFormat = do_need_format,
        !:UseDeps = [mercury_string_format_module,
            mercury_string_parse_util_module | !.UseDeps]
    ;
        ItemsNeedFormat = dont_need_format
    ),
    globals.lookup_bool_option(Globals, profile_deep, Deep),
    (
        Deep = yes,
        !:UseDeps = [mercury_profiling_builtin_module | !.UseDeps]
    ;
        Deep = no
    ),
    (
        (
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        )
    ->
        !:UseDeps = [mercury_term_size_prof_builtin_module | !.UseDeps]
    ;
        true
    ),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    (
        Target = target_c,
        HighLevelCode = no,
        Parallel = yes
    ->
        !:UseDeps = [mercury_par_builtin_module | !.UseDeps]
    ;
        true
    ),
    globals.lookup_bool_option(Globals, use_regions, UseRegions),
    (
        UseRegions = yes,
        !:UseDeps = [mercury_region_builtin_module | !.UseDeps]
    ;
        UseRegions = no
    ),
    globals.get_ssdb_trace_level(Globals, SSDBTraceLevel),
    globals.lookup_bool_option(Globals, force_disable_ssdebug, DisableSSDB),
    (
        ( SSDBTraceLevel = shallow
        ; SSDBTraceLevel = deep
        ),
        DisableSSDB = no
    ->
        !:UseDeps = [mercury_ssdb_builtin_module | !.UseDeps]
    ;
        true
    ),
    list.sort_and_remove_dups(!ImportDeps),
    list.sort_and_remove_dups(!UseDeps).

:- type maybe_need_tabling
    --->    dont_need_tabling
    ;       do_need_tabling.

:- type maybe_need_tabling_statistics
    --->    dont_need_tabling_statistics
    ;       do_need_tabling_statistics.

:- type maybe_need_stm
    --->    dont_need_stm
    ;       do_need_stm.

:- type maybe_need_exception
    --->    dont_need_exception
    ;       do_need_exception.

:- type maybe_need_format
    --->    dont_need_format
    ;       do_need_format.

    % XXX We currently discover the need to import the modules needed
    % to compile away format strings by traversing all parts of all clauses,
    % and checking every predicate name and functor name to see whether
    % it could refer to any of the predicates recognized by the is_format_call
    % predicate. This is inefficient. It is also a bit unpredictable, since
    % it will lead us to implicitly import those modules even if a call
    % to unqualified("format") eventually turns out to call some other
    % predicate of that name.
    %
    % We should therefore consider ALWAYS implicitly importing the predicates
    % needed by format_call.m.
:- type implicit_import_needs
    --->    implicit_import_needs(
                iin_tabling             :: maybe_need_tabling,
                iin_tabling_statistics  :: maybe_need_tabling_statistics,
                iin_stm                 :: maybe_need_stm,
                iin_exception           :: maybe_need_exception,
                iin_format              :: maybe_need_format
            ).

:- pred gather_implicit_import_needs_in_items(list(item)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_items([], !ImplicitImportNeeds).
gather_implicit_import_needs_in_items([Item | Items], !ImplicitImportNeeds) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        Pragma = pragma_tabled(TableInfo),
        TableInfo = pragma_info_tabled(_, _, _, MaybeAttributes)
    ->
        !ImplicitImportNeeds ^ iin_tabling := do_need_tabling,
        (
            MaybeAttributes = no
        ;
            MaybeAttributes = yes(Attributes),
            StatsAttr = Attributes ^ table_attr_statistics,
            (
                StatsAttr = table_gather_statistics,
                !ImplicitImportNeeds ^ iin_tabling_statistics
                    := do_need_tabling_statistics
            ;
                StatsAttr = table_dont_gather_statistics
            )
        )
    ;
        Item = item_clause(ItemClause)
    ->
        HeadArgs = ItemClause ^ cl_head_args,
        gather_implicit_import_needs_in_terms(HeadArgs, !ImplicitImportNeeds),
        Body = ItemClause ^ cl_body,
        gather_implicit_import_needs_in_goal(Body, !ImplicitImportNeeds)
    ;
        true
    ),
    gather_implicit_import_needs_in_items(Items, !ImplicitImportNeeds).

:- pred gather_implicit_import_needs_in_goal(goal::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_goal(GoalExpr - _Context,
        !ImplicitImportNeeds) :-
    (
        ( GoalExpr = true_expr
        ; GoalExpr = fail_expr
        )
        % Cannot contain anything that requires implicit imports.
    ;
        ( GoalExpr = conj_expr(SubGoalA, SubGoalB)
        ; GoalExpr = par_conj_expr(SubGoalA, SubGoalB)
        ; GoalExpr = disj_expr(SubGoalA, SubGoalB)
        ; GoalExpr = implies_expr(SubGoalA, SubGoalB)
        ; GoalExpr = equivalent_expr(SubGoalA, SubGoalB)
        ),
        gather_implicit_import_needs_in_goal(SubGoalA, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_goal(SubGoalB, !ImplicitImportNeeds)
    ;
        ( GoalExpr = not_expr(SubGoal)
        ; GoalExpr = some_expr(_Vars, SubGoal)
        ; GoalExpr = all_expr(_Vars, SubGoal)
        ; GoalExpr = some_state_vars_expr(_Vars, SubGoal)
        ; GoalExpr = all_state_vars_expr(_Vars, SubGoal)
        ; GoalExpr = promise_purity_expr(_Purity, SubGoal)
        ; GoalExpr = promise_equivalent_solutions_expr(_OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; GoalExpr = promise_equivalent_solution_sets_expr(_OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; GoalExpr = promise_equivalent_solution_arbitrary_expr(_OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; GoalExpr = require_detism_expr(_Detism, SubGoal)
        ; GoalExpr = require_complete_switch_expr(_SwitchVar, SubGoal)
        ; GoalExpr = require_switch_arms_detism_expr(_SwitchVar, _Detism,
            SubGoal)
        ; GoalExpr = trace_expr(_CompCond, _RunCond, _MaybeIO, _Mutables,
            SubGoal)
        ),
        gather_implicit_import_needs_in_goal(SubGoal, !ImplicitImportNeeds)
    ;
        GoalExpr = try_expr(_MaybeIO, SubGoal, Then, MaybeElse,
            Catches, MaybeCatchAny),
        !ImplicitImportNeeds ^ iin_exception := do_need_exception,
        gather_implicit_import_needs_in_goal(SubGoal, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_goal(Then, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_maybe_goal(MaybeElse,
            !ImplicitImportNeeds),
        gather_implicit_import_needs_in_catch_exprs(Catches,
            !ImplicitImportNeeds),
        gather_implicit_import_needs_in_maybe_catch_any_expr(MaybeCatchAny,
            !ImplicitImportNeeds)
    ;
        GoalExpr = if_then_else_expr(_Vars, _StateVars, Cond, Then, Else),
        gather_implicit_import_needs_in_goal(Cond, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_goal(Then, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_goal(Else, !ImplicitImportNeeds)
    ;
        GoalExpr = atomic_expr(_Outer, _Inner, _OutputVars,
            MainGoal, OrElseGoals),
        !ImplicitImportNeeds ^ iin_stm := do_need_stm,
        !ImplicitImportNeeds ^ iin_exception := do_need_exception,
        gather_implicit_import_needs_in_goal(MainGoal, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_goals(OrElseGoals,
            !ImplicitImportNeeds)
    ;
        GoalExpr = call_expr(CalleeSymName, Args, _Purity),
        ( if
            ( CalleeSymName = unqualified("format")
            ; CalleeSymName = qualified(unqualified("string"), "format")
            ; CalleeSymName = qualified(unqualified("io"), "format")
            ; CalleeSymName = qualified(unqualified("stream"), "format")
            ; CalleeSymName = qualified(unqualified("string_writer"), "format")
            ; CalleeSymName = qualified(qualified(unqualified("string_writer"),
                "string_writer"), "format")
            )
        then
            !ImplicitImportNeeds ^ iin_format := do_need_format
        else
            gather_implicit_import_needs_in_terms(Args, !ImplicitImportNeeds)
        )
    ;
        GoalExpr = event_expr(_EventName, EventArgs),
        gather_implicit_import_needs_in_terms(EventArgs, !ImplicitImportNeeds)
    ;
        GoalExpr = unify_expr(TermA, TermB, _Purity),
        gather_implicit_import_needs_in_term(TermA, !ImplicitImportNeeds),
        gather_implicit_import_needs_in_term(TermB, !ImplicitImportNeeds)
    ).

:- pred gather_implicit_import_needs_in_goals(list(goal)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_goals([], !ImplicitImportNeeds).
gather_implicit_import_needs_in_goals([Goal | Goals], !ImplicitImportNeeds) :-
    gather_implicit_import_needs_in_goal(Goal, !ImplicitImportNeeds),
    gather_implicit_import_needs_in_goals(Goals, !ImplicitImportNeeds).

:- pred gather_implicit_import_needs_in_maybe_goal(maybe(goal)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_maybe_goal(no, !ImplicitImportNeeds).
gather_implicit_import_needs_in_maybe_goal(yes(Goal), !ImplicitImportNeeds) :-
    gather_implicit_import_needs_in_goal(Goal, !ImplicitImportNeeds).

:- pred gather_implicit_import_needs_in_catch_exprs(list(catch_expr)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_catch_exprs([], !ImplicitImportNeeds).
gather_implicit_import_needs_in_catch_exprs([CatchExpr | CatchExprs],
        !ImplicitImportNeeds) :-
    CatchExpr = catch_expr(_Pattern, Goal),
    gather_implicit_import_needs_in_goal(Goal, !ImplicitImportNeeds),
    gather_implicit_import_needs_in_catch_exprs(CatchExprs,
        !ImplicitImportNeeds).

:- pred gather_implicit_import_needs_in_maybe_catch_any_expr(
    maybe(catch_any_expr)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_maybe_catch_any_expr(no, !ImplicitImportNeeds).
gather_implicit_import_needs_in_maybe_catch_any_expr(yes(CatchAnyExpr),
        !ImplicitImportNeeds) :-
    CatchAnyExpr = catch_any_expr(_Var, Goal),
    gather_implicit_import_needs_in_goal(Goal, !ImplicitImportNeeds).

:- pred gather_implicit_import_needs_in_term(prog_term::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_term(Term, !ImplicitImportNeeds) :-
    (
        Term = variable(_Var, _Context)
    ;
        Term = functor(Const, ArgTerms, _Context),
        (
            Const = atom(Atom),
            ( if
                ( Atom = "format"
                ; Atom = "string.format"
                ; Atom = "string__format"
                ; Atom = "io.format"
                ; Atom = "io__format"
                ; Atom = "stream.format"
                ; Atom = "stream__format"
                ; Atom = "string_writer.format"
                ; Atom = "string_writer__format"
                ; Atom = "stream.string_writer.format"
                ; Atom = "stream.string_writer__format"
                ; Atom = "stream__string_writer.format"
                ; Atom = "stream__string_writer__format"
                )
            then
                !ImplicitImportNeeds ^ iin_format := do_need_format
            else
                true
            )
        ;
            ( Const = integer(_)
            ; Const = string(_)
            ; Const = float(_)
            ; Const = implementation_defined(_)
            )
        ),
        gather_implicit_import_needs_in_terms(ArgTerms, !ImplicitImportNeeds)
    ).

:- pred gather_implicit_import_needs_in_terms(list(prog_term)::in,
    implicit_import_needs::in, implicit_import_needs::out) is det.

gather_implicit_import_needs_in_terms([], !ImplicitImportNeeds).
gather_implicit_import_needs_in_terms([Term | Terms], !ImplicitImportNeeds) :-
    gather_implicit_import_needs_in_term(Term, !ImplicitImportNeeds),
    gather_implicit_import_needs_in_terms(Terms, !ImplicitImportNeeds).

%-----------------------------------------------------------------------------%

get_fact_table_dependencies(Items, Deps) :-
    get_fact_table_dependencies_2(Items, [], Deps).

:- pred get_fact_table_dependencies_2(list(item)::in, list(string)::in,
    list(string)::out) is det.

get_fact_table_dependencies_2([], !Deps).
get_fact_table_dependencies_2([Item | Items], !Deps) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        Pragma = pragma_fact_table(FTInfo),
        FTInfo = pragma_info_fact_table(_PredNameArity, FileName)
    ->
        !:Deps = [FileName | !.Deps]
    ;
        true
    ),
    get_fact_table_dependencies_2(Items, !Deps).

%-----------------------------------------------------------------------------%

get_foreign_include_files(Items, IncludeFiles) :-
    list.foldl(get_foreign_include_file, Items, [], IncludeFiles).

:- pred get_foreign_include_file(item::in,
    foreign_include_file_info_list::in, foreign_include_file_info_list::out)
    is det.

get_foreign_include_file(Item, !IncludeFiles) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        (
            Pragma = pragma_foreign_decl(FDInfo),
            FDInfo = pragma_info_foreign_decl(Lang, _IsLocal, LiteralOrInclude)
        ;
            Pragma = pragma_foreign_code(FCInfo),
            FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude)
        )
    ->
        (
            LiteralOrInclude = literal(_)
        ;
            LiteralOrInclude = include_file(FileName),
            IncludeFile = foreign_include_file_info(Lang, FileName),
            !:IncludeFiles = [IncludeFile | !.IncludeFiles]
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.module_imports.
%-----------------------------------------------------------------------------%
