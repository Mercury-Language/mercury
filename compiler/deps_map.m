%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
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
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.

:- import_module io.
:- import_module list.
:- import_module map.

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(module_name, deps).
:- type deps
    --->    deps(
                have_processed,
                burdened_module
            ).

:- type have_processed
    --->    not_yet_processed
    ;       already_processed.

%---------------------------------------------------------------------------%

:- type submodule_kind
    --->    toplevel
    ;       nested_submodule
    ;       separate_submodule.

    % Check if a module is a top-level module, a nested submodule,
    % or a separate submodule.
    %
:- func get_submodule_kind(module_name, deps_map) = submodule_kind.

%---------------------------------------------------------------------------%

:- pred generate_deps_map(globals::in, maybe_search::in, module_name::in,
    deps_map::in, deps_map::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

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
    % XXX This shouldn't need to be exported.
    %
:- pred insert_into_deps_map(burdened_module::in,
    deps_map::in, deps_map::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.timestamp.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module cord.
:- import_module maybe.
:- import_module one_or_more.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

get_submodule_kind(ModuleName, DepsMap) = Kind :-
    (
        ModuleName = qualified(Parent, _),
        map.lookup(DepsMap, ModuleName, deps(_, BurdenedModule)),
        map.lookup(DepsMap, Parent, deps(_, ParentBurdenedModule)),
        ModuleBaggage = BurdenedModule ^ bm_baggage,
        ParentBaggage = ParentBurdenedModule ^ bm_baggage,
        ModuleFileName = ModuleBaggage ^ mb_source_file_name,
        ParentFileName = ParentBaggage ^ mb_source_file_name,
        ( if ModuleFileName = ParentFileName then
            Kind = nested_submodule
        else
            Kind = separate_submodule
        )
    ;
        ModuleName = unqualified(_),
        Kind = toplevel
    ).

%---------------------------------------------------------------------------%

generate_deps_map(Globals, Search, ModuleName, !DepsMap, !Specs, !IO) :-
    generate_deps_map_loop(Globals, Search, map.singleton(ModuleName, []),
        !DepsMap, !Specs, !IO).

    % Values of this type map each module name to the list of contexts
    % that mention it, and thus establish an expectation that a module
    % with that name exists.
    %
:- type expectation_contexts_map == map(module_name, expectation_contexts).
:- type expectation_contexts == list(term.context).

:- pred generate_deps_map_loop(globals::in, maybe_search::in,
    expectation_contexts_map::in, deps_map::in, deps_map::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

generate_deps_map_loop(Globals, Search, !.Modules, !DepsMap, !Specs, !IO) :-
    ( if map.remove_smallest(Module, ExpectationContexts, !Modules) then
        generate_deps_map_step(Globals, Search, Module, ExpectationContexts,
            !Modules, !DepsMap, !Specs, !IO),
        generate_deps_map_loop(Globals, Search,
            !.Modules, !DepsMap, !Specs, !IO)
    else
        % If we can't remove the smallest, then the set of modules to be
        % processed is empty.
        true
    ).

:- pred generate_deps_map_step(globals::in, maybe_search::in,
    module_name::in, expectation_contexts::in,
    expectation_contexts_map::in, expectation_contexts_map::out,
    deps_map::in, deps_map::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

generate_deps_map_step(Globals, Search, Module, ExpectationContexts,
        !Modules, !DepsMap, !Specs, !IO) :-
    % Look up the module's dependencies, and determine whether
    % it has been processed yet.
    lookup_or_find_dependencies(Globals, Search, Module, ExpectationContexts,
        MaybeDeps0, !DepsMap, !Specs, !IO),

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
        % We could keep a list of the modules we have already processed
        % and subtract it from the sets of modules we add here, but doing that
        % actually leads to a small slowdown.
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,

        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        AncestorModuleNames = get_ancestors_set(ModuleName),
        ModuleNameContext = ParseTreeModuleSrc ^ ptms_module_name_context,
        set.foldl(add_module_name_and_context(ModuleNameContext),
            AncestorModuleNames, !Modules),

        IntFIMs = ParseTreeModuleSrc ^ ptms_int_fims,
        ImpFIMs = ParseTreeModuleSrc ^ ptms_imp_fims,
        map.foldl(add_fim_module_with_context, IntFIMs, !Modules),
        map.foldl(add_fim_module_with_context, ImpFIMs, !Modules),

        InclMap = ParseTreeModuleSrc ^ ptms_include_map,
        map.foldl(add_public_include_module_with_context, InclMap, !Modules),

        ImportUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
        map.foldl(add_avail_module_with_context, ImportUseMap, !Modules)
    else
        % Either MaybeDeps0 = no, or Done0 = already_processed.
        true
    ).

:- pred add_public_include_module_with_context(module_name::in,
    include_module_info::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_public_include_module_with_context(ModuleName, InclInfo, !Modules) :-
    InclInfo = include_module_info(Section, Context),
    (
        Section = ms_interface,
        ( if map.search(!.Modules, ModuleName, OldContexts) then
            Contexts = [Context | OldContexts],
            map.det_update(ModuleName, Contexts, !Modules)
        else
            map.det_insert(ModuleName, [Context], !Modules)
        )
    ;
        Section = ms_implementation
    ).

:- pred add_section_import_and_or_use_context(module_name::in,
    section_import_and_or_use::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_section_import_and_or_use_context(ModuleName, SectionImportUse,
        !Modules) :-
    (
        ( SectionImportUse = int_import(Context)
        ; SectionImportUse = int_use(Context)
        ; SectionImportUse = imp_import(Context)
        ; SectionImportUse = imp_use(Context)
        ),
        add_module_name_and_context(Context, ModuleName, !Modules)
    ;
        SectionImportUse = int_use_imp_import(IntContext, ImpContext),
        add_module_name_and_context(IntContext, ModuleName, !Modules),
        add_module_name_and_context(ImpContext, ModuleName, !Modules)
    ).

:- pred add_avail_module_with_context(module_name::in,
    maybe_implicit_import_and_or_use::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_avail_module_with_context(ModuleName, MaybeImplicit, !Modules) :-
    (
        MaybeImplicit = explicit_avail(SectionImportUse),
        add_section_import_and_or_use_context(ModuleName, SectionImportUse,
            !Modules)
    ;
        MaybeImplicit = implicit_avail(_, MaybeSectionImportUse),
        (
            MaybeSectionImportUse = no,
            add_module_name_and_context(term.dummy_context_init, ModuleName,
                !Modules)
        ;
            MaybeSectionImportUse = yes(SectionImportUse),
            add_section_import_and_or_use_context(ModuleName, SectionImportUse,
                !Modules)
        )
    ).

:- pred add_fim_module_with_context(fim_spec::in, term.context::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_fim_module_with_context(FIMSpec, Context, !Modules) :-
    FIMSpec = fim_spec(_Lang, ModuleName),
    add_module_name_and_context(Context, ModuleName, !Modules).

:- pred add_module_name_and_context(term.context::in, module_name::in,
    expectation_contexts_map::in, expectation_contexts_map::out) is det.

add_module_name_and_context(Context, ModuleName, !Modules) :-
    ( if map.search(!.Modules, ModuleName, OldContexts) then
        map.det_update(ModuleName, [Context | OldContexts], !Modules)
    else
        map.det_insert(ModuleName, [Context], !Modules)
    ).

%---------------------%

    % Look up a module in the dependency map.
    % If we don't know its dependencies, read the module and
    % save the dependencies in the dependency map.
    %
:- pred lookup_or_find_dependencies(globals::in, maybe_search::in,
    module_name::in, expectation_contexts::in,
    maybe(deps)::out, deps_map::in, deps_map::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

lookup_or_find_dependencies(Globals, Search, ModuleName, ExpectationContexts,
        MaybeDeps, !DepsMap, !Specs, !IO) :-
    ( if map.search(!.DepsMap, ModuleName, Deps) then
        MaybeDeps = yes(Deps)
    else
        read_dependencies(Globals, Search, ModuleName, ExpectationContexts,
            BurdenedModuleList, !Specs, !IO),
        (
            BurdenedModuleList = [_ | _],
            list.foldl(insert_into_deps_map, BurdenedModuleList, !DepsMap),
            map.lookup(!.DepsMap, ModuleName, Deps),
            MaybeDeps = yes(Deps)
        ;
            BurdenedModuleList = [],
            MaybeDeps = no
        )
    ).

insert_into_deps_map(BurdenedModule, !DepsMap) :-
    ParseTreeModuleSrc = BurdenedModule ^ bm_module,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    Deps = deps(not_yet_processed, BurdenedModule),
    map.set(ModuleName, Deps, !DepsMap).

    % Read a module to determine the (direct) dependencies of that module
    % and any nested submodules it contains. Return the burdened_module
    % structure for both the named module and each of its nested submodules.
    %
:- pred read_dependencies(globals::in, maybe_search::in,
    module_name::in, expectation_contexts::in, list(burdened_module)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_dependencies(Globals, Search, ModuleName, ExpectationContexts,
        BurdenedModules, !Specs, !IO) :-
    % XXX If HaveReadModuleSrc contains error messages, any parse tree
    % it may also contain may not be complete, and the rest of this predicate
    % may work on incorrect data.
    read_module_src(Globals, rrm_get_deps(ModuleName), ignore_errors, Search,
        ModuleName, ExpectationContexts,
        always_read_module(dont_return_timestamp), HaveReadModuleSrc, !IO),
    (
        HaveReadModuleSrc = have_read_module(SourceFileName, _MaybeTimestamp,
            ParseTreeSrc, ReadModuleErrors)
    ;
        HaveReadModuleSrc = have_not_read_module(SourceFileName,
            ReadModuleErrors),
        % XXX Creating a dummy parse tree, from which we then construct
        % a single burdened module, which we then add to the deps_map,
        % preserves old behavior. This old behavior has two effects,
        % as far as I can tell (zs, 2022 apr 28).
        %
        % - It includes modules which are imported but whose sources
        %   are not in the current directory among the dependencies
        %   of the main module in the main module's .dv file.
        %
        %   I see three main classes of such modules. The two obvious classes
        %   are Mercury library modules and a project's own modules in other
        %   directories, which may or may not be accessed via libraries.
        %   A third class is nested submodules which are not found because
        %   they are looked up as if they were separate submodules. This is
        %   exemplified by the tests/invalid_make_int/sub_c test case,
        %   where sub_c.m imports sub_a.sub_1, but sub_a.sub_1.m does not
        %   exist, because sub_1 is a nested submodule inside sub_a.m.
        %
        %   It is arguable whether including the first two categories
        %   among the main module's dependencies in the .dv file
        %   is a good or not. With the right view path, they can detect
        %   when the main module needs to be rebuilt due to changes in them,
        %   but specifying that view path to --generate-dependencies, and
        %   having the compiler actually use that view path, would be
        %   a better idea. Including the third is probably a bad idea
        %   from all respects, though (a) distinguishing that category
        %   from the other two is nontrivial, and (b) fixing that bad idea
        %   would require changing the expected output of the sub_c test case.
        %
        %   The commented out code below is a possible replacement of this
        %   switch and the call following it. We could use it if decide
        %   not to put dummy parse_tree_srcs into the deps_map. Switching
        %   to it bootchecks, though only with a different expected output
        %   for the sub_c test case.
        %
        % - Strangely, even the totally empty parse_tree_src we construct
        %   will end up having dependencies, because we implicitly import
        %   both the public and private builtin modules into all modules,
        %   even if they contain nothing that could refer to either builtin
        %   module :-( However, these dependencies should not drag into
        %   the deps_map any module that other, nondummy modules' dependencies
        %   wouldn't drag in anyway.
        ParseTreeSrc = parse_tree_src(ModuleName, term.dummy_context_init,
            cord.init)
    ),
    parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ParseTreeSrc, ReadModuleErrors, Specs, BurdenedModules),
%   (
%       HaveReadModuleSrc = have_read_module(SourceFileName, _MaybeTimestamp,
%           ParseTreeSrc, ReadModuleErrors),
%       parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
%           ParseTreeSrc, ReadModuleErrors, Specs, BurdenedModules)
%   ;
%       HaveReadModuleSrc = have_not_read_module(_, ReadModuleErrors),
%       BurdenedModules = [],
%       Specs = get_read_module_specs(ReadModuleErrors)
%   ),
    !:Specs = Specs ++ !.Specs.

%---------------------------------------------------------------------------%
:- end_module parse_tree.deps_map.
%---------------------------------------------------------------------------%
