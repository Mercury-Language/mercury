%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: deps_map.m.
%
% This module contains a data structure for recording module dependencies
% and its access predicates. The module_deps_graph module contains another
% data structure, used for similar purposes, that is built on top of this one.
% XXX Document the exact relationship between the two.
%
%---------------------------------------------------------------------------%

:- module parse_tree.deps_map.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.

:- import_module io.
:- import_module list.
:- import_module map.

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(module_name, deps).
:- type deps
    --->    deps(have_processed, burdened_module).

:- type have_processed
    --->    not_yet_processed
    ;       already_processed.

%---------------------------------------------------------------------------%

:- type file_or_module
    --->    fm_file(file_name)
    ;       fm_module(module_name).

:- pred generate_deps_map(io.text_output_stream::in, globals::in,
    maybe_search::in, file_or_module::in, module_name::out,
    deps_map::out, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.timestamp.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.warn_unread_modules.

:- import_module cord.
:- import_module maybe.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

generate_deps_map(ProgressStream, Globals, Search, FileOrModule, ModuleName,
        !:DepsMap, !:Specs, !IO) :-
    SeenModules0 = set_tree234.init,
    (
        FileOrModule = fm_module(ModuleName),
        map.init(!:DepsMap),
        !:Specs = []
    ;
        FileOrModule = fm_file(FileName),
        build_initial_deps_map_for_file(ProgressStream, Globals,
            FileName, ModuleName, !:DepsMap, !:Specs, !IO)
    ),
    CmdLineModuleName = ModuleName,
    ModuleExpectationContexts0 = map.singleton(ModuleName, []),
    ReadModules0 = set_tree234.init,
    UnreadModules0 = set_tree234.init,
    generate_deps_map_loop(ProgressStream, Globals, Search, CmdLineModuleName,
        SeenModules0, ModuleExpectationContexts0,
        ReadModules0, ReadModules, UnreadModules0, UnreadModules1,
        !DepsMap, !Specs, !IO),
    % When module mod_a.mod_b is nested inside mod_a.m, the source file
    % containing module mod_a, then it is possible for an attempt to read
    % mod_a.mod_b.m to fail (since module mod_a.mod_b is not there),
    % but for the module to be later found by reading mod_a.m.
    % This would result in mod_a.mod_b being included in both
    % ReadModules and UnreadModules1.
    set_tree234.difference(UnreadModules1, ReadModules, UnreadModules),

    trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
        io(!TIO)]
    (
        io.format(ProgressStream, "generate_dot_dx_files for %s\n",
            [s(sym_name_to_string(ModuleName))], !TIO),

        set_tree234.to_sorted_list(UnreadModules, UnreadModuleList),
        set_tree234.to_sorted_list(ReadModules, ReadModuleList),
        ReadStrs = list.map(sym_name_to_string, ReadModuleList),
        UnreadStrs = list.map(sym_name_to_string, UnreadModuleList),

        io.write_string(ProgressStream, "ReadModules\n", !TIO),
        io.write_line(ProgressStream, ReadStrs, !TIO),
        io.write_string(ProgressStream, "UnreadModules\n", !TIO),
        io.write_line(ProgressStream, UnreadStrs, !TIO)
    ),
    warn_about_any_unread_modules_with_read_ancestors(ReadModules,
        UnreadModules, !Specs).

%---------------------------------------------------------------------------%

:- pred build_initial_deps_map_for_file(io.text_output_stream::in, globals::in,
    file_name::in, module_name::out, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

build_initial_deps_map_for_file(ProgressStream, Globals, FileName, ModuleName,
        DepsMap, Specs, !IO) :-
    % Read in the top-level file (to figure out its module name).
    FileNameDotM = FileName ++ ".m",
    read_module_src_from_file(ProgressStream, Globals, FileName, FileNameDotM,
        rrm_file, do_not_search, always_read_module(do_not_return_timestamp),
        HaveReadModuleSrc, !IO),
    (
        HaveReadModuleSrc = have_module(_FN, ParseTreeSrc, Source),
        Source = was_read(MaybeTimestamp, ReadModuleErrors),
        ParseTreeSrc = parse_tree_src(ModuleName, _, _),
        parse_tree_src_to_burdened_module_list(Globals, FileNameDotM,
            ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
            Specs, BurdenedModules)
    ;
        HaveReadModuleSrc = have_not_read_module(_, ReadModuleErrors),
        get_default_module_name_for_file(FileName, FileNameDotM,
            ModuleName, !IO),
        % XXX Caller should not need this info.
        Specs = get_read_module_specs(ReadModuleErrors),
        BurdenedModules = []
    ),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, BurdenedModules, DepsMap0, DepsMap).

%---------------------------------------------------------------------------%

    % Values of this type map each module name to the list of contexts
    % that mention it, and thus establish an expectation that a module
    % with that name exists.
    %
    % The code uses ModuleExpCs as short for "module expectation contexts".
    %
:- type expectation_contexts_map == map(module_name, expectation_contexts).
:- type expectation_contexts == list(term_context).

:- pred generate_deps_map_loop(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, set_tree234(module_name)::in,
    expectation_contexts_map::in,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    deps_map::in, deps_map::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

generate_deps_map_loop(ProgressStream, Globals, Search, CmdLineModuleName,
        !.SeenModules, !.ModuleExpCs, !ReadModules, !UnreadModules,
        !DepsMap, !Specs, !IO) :-
    ( if map.remove_smallest(Module, ExpectationContexts, !ModuleExpCs) then
        set_tree234.insert(Module, !SeenModules),
        generate_deps_map_step(ProgressStream, Globals, Search,
            CmdLineModuleName, Module, ExpectationContexts, !.SeenModules,
            !ModuleExpCs, !ReadModules, !UnreadModules, !DepsMap, !Specs, !IO),
        generate_deps_map_loop(ProgressStream, Globals, Search,
            CmdLineModuleName, !.SeenModules, !.ModuleExpCs,
            !ReadModules, !UnreadModules, !DepsMap, !Specs, !IO)
    else
        % If we can't remove the smallest, then the set of modules to be
        % processed is empty.
        true
    ).

    % generate_deps_map_step(ProgressStream, Globals, Search,
    %   CmdLineModuleName, Module, ExpectationContexts, SeenModules0,
    %   !ModuleExpCs, !DepsMap, !Specs, !IO):
    %
    % Process Module, which we expect *should* exist due to the code
    % at ExpectationContexts, by finding out which other modules it depends on,
    % and adding them to !ModuleExpCs and !DepsMap to be processed later.
    %
    % SeenModules0 is the set of modules that we have either already processed
    % with generate_deps_map_step, or are currently processing. It is therefore
    % the set of modules that we don't need to add to !ModuleExpCs.
    % We *could* add them to !ModuleExpCs, and we once did, since the
    % presence of Module in !.DepsMap with an already_processed flag
    % will prevent us from processing them again anyway, but the extra
    % recursive calls to generate_deps_map_loop are quite annoying to anyone
    % who wants to step through this code.
    %
    % Benchmarking on a laptop shows that representing SeenModules0
    % using set_tree234s generates a very small speedup, while using
    % plain sets generates a very small slowdown.
    %
:- pred generate_deps_map_step(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, module_name::in,
    expectation_contexts::in, set_tree234(module_name)::in,
    expectation_contexts_map::in, expectation_contexts_map::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    deps_map::in, deps_map::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

generate_deps_map_step(ProgressStream, Globals, Search, CmdLineModuleName,
        Module, ExpectationContexts, SeenModules0, !ModuleExpCs,
        !ReadModules, !UnreadModules, !DepsMap, !Specs, !IO) :-
    % Look up the module's dependencies, and determine whether
    % it has been processed yet.
    lookup_or_find_dependency_info_for_module(ProgressStream, Globals, Search,
        CmdLineModuleName, Module, ExpectationContexts, MaybeDeps0,
        NewBurdenedModules, !DepsMap, !Specs, !IO),
    update_read_unread_modules(ProgressStream, Module,
        MaybeDeps0, NewBurdenedModules, !ReadModules, !UnreadModules),

    % If the module hadn't been processed yet, then add its imports, parents,
    % and public children to the list of dependencies we need to generate,
    % and mark it as having been processed.
    % XXX Why only the *public* children?
    ( if
        MaybeDeps0 = yes(Deps0),
        Deps0 = deps(Done0, BurdenedModule),
        Done0 = not_yet_processed
    then
        Deps = deps(already_processed, BurdenedModule),
        map.det_update(Module, Deps, !DepsMap),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,

        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        AncestorModuleNames = get_ancestors_set(ModuleName),
        ModuleNameContext = ParseTreeModuleSrc ^ ptms_module_name_context,
        set.foldl(add_module_name_and_context(SeenModules0, ModuleNameContext),
            AncestorModuleNames, !ModuleExpCs),

        IntFIMs = ParseTreeModuleSrc ^ ptms_int_fims,
        ImpFIMs = ParseTreeModuleSrc ^ ptms_imp_fims,
        map.foldl(add_fim_module_with_context(SeenModules0),
            IntFIMs, !ModuleExpCs),
        map.foldl(add_fim_module_with_context(SeenModules0),
            ImpFIMs, !ModuleExpCs),

        InclMap = ParseTreeModuleSrc ^ ptms_include_map,
        map.foldl(add_public_include_module_with_context(SeenModules0),
            InclMap, !ModuleExpCs),

        ImportUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
        map.foldl(add_avail_module_with_context(SeenModules0),
            ImportUseMap, !ModuleExpCs)
    else
        % Either MaybeDeps0 = no, or Done0 = already_processed.
        true
    ).

:- pred update_read_unread_modules(io.text_output_stream::in,
    module_name::in, maybe(deps)::in, list(burdened_module)::in,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out) is det.

update_read_unread_modules(ProgressStream, Module,
        MaybeDeps, NewBurdenedModules, !ReadModules, !UnreadModules) :-
    (
        MaybeDeps = yes(_Deps),
        NewReadModules =
            list.map((func(BM) = BM ^ bm_module ^ ptms_module_name),
                NewBurdenedModules),
        set_tree234.insert_list(NewReadModules, !ReadModules),
        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            NewReadModuleStrs = list.map(sym_name_to_string, NewReadModules),
            io.format(ProgressStream, "read %s\n",
                [s(string.join_list(", ", NewReadModuleStrs))], !TIO)
        )
    ;
        MaybeDeps = no,
        set_tree234.insert(Module, !UnreadModules),
        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            io.format(ProgressStream, "unread %s\n",
                [s(sym_name_to_string(Module))], !TIO)
        )
    ).

:- pred add_public_include_module_with_context(set_tree234(module_name)::in,
    module_name::in, include_module_info::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_public_include_module_with_context(SeenModules0, ModuleName, InclInfo,
        !ModuleExpCs) :-
    InclInfo = include_module_info(Section, Context),
    (
        Section = ms_interface,
        add_module_name_and_context(SeenModules0, Context, ModuleName,
            !ModuleExpCs)
    ;
        Section = ms_implementation
    ).

:- pred add_avail_module_with_context(set_tree234(module_name)::in,
    module_name::in, maybe_implicit_import_and_or_use::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_avail_module_with_context(SeenModules0, ModuleName, MaybeImplicit,
        !ModuleExpCs) :-
    (
        MaybeImplicit = explicit_avail(SectionImportUse),
        add_section_import_and_or_use_context(SeenModules0, ModuleName,
            SectionImportUse, !ModuleExpCs)
    ;
        MaybeImplicit = implicit_avail(_, MaybeSectionImportUse),
        (
            MaybeSectionImportUse = no,
            add_module_name_and_context(SeenModules0, dummy_context,
                ModuleName, !ModuleExpCs)
        ;
            MaybeSectionImportUse = yes(SectionImportUse),
            add_section_import_and_or_use_context(SeenModules0, ModuleName,
                SectionImportUse, !ModuleExpCs)
        )
    ).

:- pred add_section_import_and_or_use_context(set_tree234(module_name)::in,
    module_name::in, section_import_and_or_use::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_section_import_and_or_use_context(SeenModules0, ModuleName,
        SectionImportUse, !ModuleExpCs) :-
    (
        ( SectionImportUse = int_import(Context)
        ; SectionImportUse = int_use(Context)
        ; SectionImportUse = imp_import(Context)
        ; SectionImportUse = imp_use(Context)
        ),
        add_module_name_and_context(SeenModules0, Context, ModuleName,
            !ModuleExpCs)
    ;
        SectionImportUse = int_use_imp_import(IntContext, ImpContext),
        add_module_name_and_context(SeenModules0, IntContext, ModuleName,
            !ModuleExpCs),
        add_module_name_and_context(SeenModules0, ImpContext, ModuleName,
            !ModuleExpCs)
    ).

:- pred add_fim_module_with_context(set_tree234(module_name)::in,
    fim_spec::in, term_context::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_fim_module_with_context(SeenModules0, FIMSpec, Context, !ModuleExpCs) :-
    FIMSpec = fim_spec(_Lang, ModuleName),
    add_module_name_and_context(SeenModules0, Context, ModuleName,
        !ModuleExpCs).

:- pred add_module_name_and_context(set_tree234(module_name)::in,
    term_context::in, module_name::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_module_name_and_context(SeenModules0, Context, ModuleName, !ModuleExpCs) :-
    ( if set_tree234.contains(SeenModules0, ModuleName) then
        true
    else
        ( if map.search(!.ModuleExpCs, ModuleName, OldContexts) then
            map.det_update(ModuleName, [Context | OldContexts], !ModuleExpCs)
        else
            map.det_insert(ModuleName, [Context], !ModuleExpCs)
        )
    ).

%---------------------%

    % Look up a module in the dependency map.
    % If we don't know its dependencies, read the module and
    % save the dependencies in the dependency map.
    %
:- pred lookup_or_find_dependency_info_for_module(io.text_output_stream::in,
    globals::in, maybe_search::in, module_name::in, module_name::in,
    expectation_contexts::in, maybe(deps)::out, list(burdened_module)::out,
    deps_map::in, deps_map::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

lookup_or_find_dependency_info_for_module(ProgressStream, Globals, Search,
        CmdLineModuleName, ModuleName, ExpectationContexts, MaybeDeps,
        NewBurdenedModules, !DepsMap, !Specs, !IO) :-
    ( if map.search(!.DepsMap, ModuleName, Deps) then
        MaybeDeps = yes(Deps),
        NewBurdenedModules = []
    else
        read_src_file_for_dependency_info(ProgressStream, Globals, Search,
            CmdLineModuleName, ModuleName, ExpectationContexts,
            NewBurdenedModules, !Specs, !IO),
        (
            NewBurdenedModules = [_ | _],
            list.foldl(insert_into_deps_map, NewBurdenedModules, !DepsMap),
            % We can do a map.lookup here even though a map.search above
            % failed, because ModuleName should be one of the BurdenedModules
            % the call above just added to !DepsMap.
            map.lookup(!.DepsMap, ModuleName, Deps),
            MaybeDeps = yes(Deps)
        ;
            NewBurdenedModules = [],
            MaybeDeps = no
        )
    ).

    % Insert a new entry into the deps_map. If the module already occurred
    % in the deps_map, then we just replace the old entry (presumed to be
    % a dummy entry) with the new one.
    %
    % This can only occur for submodules which have been imported before
    % their parent module was imported: before reading a module and
    % inserting it into the deps map, we check if it was already there,
    % but when we read in the module, we try to insert not just that module
    % but also all the nested submodules inside that module. If a submodule
    % was previously imported, then it may already have an entry in the
    % deps_map. However, unless the submodule is defined both as a separate
    % submodule and also as a nested submodule, the previous entry will be
    % a dummy entry that we inserted after trying to read the source file
    % and failing.
    %
    % Note that the case where a module is defined as both a separate
    % submodule and also as a nested submodule is caught by
    % split_into_compilation_units_perform_checks.
    %
:- pred insert_into_deps_map(burdened_module::in,
    deps_map::in, deps_map::out) is det.

insert_into_deps_map(BurdenedModule, !DepsMap) :-
    ParseTreeModuleSrc = BurdenedModule ^ bm_module,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    Deps = deps(not_yet_processed, BurdenedModule),
    % NOTE Redirecting this call to map.det_insert leads to a clean bootcheck
    % with one exception: it causes the failure of the invalid_make_int/sub_c
    % test case, with this message:
    %
    % Uncaught Mercury exception:
    % Software Error: map.det_insert: key already present
    %   Key Type: mdbcomp.sym_name.sym_name
    %   Key Value: qualified(unqualified("sub_c_helper_1"), "sub1")
    %   Value Type: parse_tree.deps_map.deps
    map.set(ModuleName, Deps, !DepsMap).

    % Read a module to determine the (direct) dependencies of that module
    % and any nested submodules it contains. Return the burdened_module
    % structure for both the named module and each of its nested submodules.
    %
:- pred read_src_file_for_dependency_info(io.text_output_stream::in,
    globals::in, maybe_search::in, module_name::in, module_name::in,
    expectation_contexts::in, list(burdened_module)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_src_file_for_dependency_info(ProgressStream, Globals, Search,
        CmdLineModuleName, ModuleName, ExpectationContexts,
        BurdenedModules, !Specs, !IO) :-
    % XXX If HaveReadModuleSrc contains error messages, any parse tree
    % it may also contain may not be complete, and the rest of this predicate
    % may work on incorrect data.
    read_module_src(ProgressStream, Globals, rrm_get_deps,
        Search, ModuleName, ExpectationContexts,
        always_read_module(do_not_return_timestamp), HaveReadModuleSrc, !IO),
    (
        HaveReadModuleSrc = have_module(SourceFileName, ParseTreeSrc, Source),
        Source = was_read(MaybeTimestamp, ReadModuleErrors),
        parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
            ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
            Specs, BurdenedModules),
        !:Specs = Specs ++ !.Specs
    ;
        HaveReadModuleSrc = have_not_read_module(_SourceFileName,
            ReadModuleErrors),
        BurdenedModules = [],
        ( if ModuleName = CmdLineModuleName then
            % The module we cannot read is named on the command line.
            % The user will definitely be interested in the fact that
            % we cannot read it.
            Specs = get_read_module_specs(ReadModuleErrors),
            !:Specs = Specs ++ !.Specs
        else
            % The module is NOT named on the command line, but merely
            % imported directly or (more likely) indirectly by it.
            % The module may well be in another directory of the project,
            % or it may be in an installed library (such as the Mercury
            % standard library) that its code will be linked with.
            % We therefore do not print any error message.
            true
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.deps_map.
%---------------------------------------------------------------------------%
