%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.dependencies.m.
% Author: stayl.
%
% Code to find the dependencies for a particular target,
% e.g. module.c depends on module.m, import.int, etc.
%
%---------------------------------------------------------------------------%

:- module make.dependencies.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.deps_set.
:- import_module make.make_info.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % find_target_dependencies_of_modules(KeepGoing, Globals, TargetType,
    %     ModuleIndexes, !Succeeded, !Deps, !Info, !IO):
    %
    % The TargetType and ModuleIndexes arguments define a set of make targets.
    % Add to !Deps the dependency_file_indexes of all the files that
    % these make targets depend on, and which therefore have to be built
    % before we can build those make targets.
    %
:- pred find_target_dependencies_of_modules(maybe_keep_going::in, globals::in,
    module_target_type::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module make.deps_cache.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.prog_data_foreign.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

find_target_dependencies_of_modules(_KeepGoing, _Globals, _TargetType,
        [], !Succeeded, !Deps, !Info, !IO).
find_target_dependencies_of_modules(KeepGoing, Globals, TargetType,
        [ModuleIndex | ModuleIndexes], !Succeeded, !Deps, !Info, !IO) :-
    find_target_dependencies_of_module(KeepGoing, Globals,
        TargetType, ModuleIndex, NewSucceeded, !Deps, !Info, !IO),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        find_target_dependencies_of_modules(KeepGoing, Globals, TargetType,
            ModuleIndexes, !Succeeded, !Deps, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

:- pred find_target_dependencies_of_module(maybe_keep_going::in, globals::in,
    module_target_type::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_target_dependencies_of_module(_KeepGoing, Globals,
        TargetType, ModuleIndex, NewSucceeded, !Deps, !Info, !IO) :-
    (
        ( TargetType = module_target_source
        ; TargetType = module_target_track_flags
        ),
        NewSucceeded = succeeded
    ;
        TargetType = module_target_int3,
        % module_target_source of self
        Deps0 = !.Deps,
        Info0 = !.Info,

        DepSpecs = [self(module_target_source)],
        find_dep_specs(Globals, ModuleIndex, DepSpecs, NewSucceededB, NewDepsB,
            Info0, InfoB, !IO),
        deps_set_union(NewDepsB, Deps0, DepsB),

        add_targets_of_modules_as_deps(module_target_source,
            [ModuleIndex], Deps0, DepsA, Info0, InfoA),
        NewSucceededA = succeeded,

        eqv_states("src_tf", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ;
        ( TargetType = module_target_int0
        ; TargetType = module_target_int1
        ; TargetType = module_target_int2
        ),
        interface_file_dependencies(FindDeps, DepSpecs),
        Info0 = !.Info,

        find_dep_specs(Globals, ModuleIndex, DepSpecs, NewSucceededB, NewDepsB,
            Info0, InfoB, !IO),

        FindDeps(Globals, ModuleIndex, NewSucceededA, NewDepsA,
            Info0, InfoA, !IO),

        eqv_states("int012", NewSucceededA, NewSucceededB, NewSucceeded,
            NewDepsA, NewDepsB, NewDeps, InfoA, InfoB, !:Info),
        deps_set_union(NewDeps, !Deps)
    ;
        ( TargetType = module_target_c_code
        ; TargetType = module_target_c_header(_)
        ; TargetType = module_target_csharp_code
        ; TargetType = module_target_java_code
        ; TargetType = module_target_errors
        ),
        compiled_code_dependencies(Globals, FindDeps, DepSpecs),
        Info0 = !.Info,

        find_dep_specs(Globals, ModuleIndex, DepSpecs,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),

        FindDeps(Globals, ModuleIndex,
            NewSucceededA, NewDepsA, Info0, InfoA, !IO),

        eqv_states("target", NewSucceededA, NewSucceededB, NewSucceeded,
            NewDepsA, NewDepsB, NewDeps, InfoA, InfoB, !:Info),
        deps_set_union(NewDeps, !Deps)
    ;
        TargetType = module_target_java_class_code,
        Info0 = !.Info,
        Deps0 = !.Deps,

        DepSpec = self(module_target_java_code),
        find_dep_spec(Globals, ModuleIndex, DepSpec,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),
        deps_set_union(Deps0, NewDepsB, DepsB),

        % module_target_java_code of self
        add_targets_of_modules_as_deps(module_target_java_code,
            [ModuleIndex], Deps0, DepsA, Info0, InfoA),
        NewSucceededA = succeeded,

        eqv_states("class", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ;
        ( TargetType = module_target_foreign_object(PIC, _)
        ; TargetType = module_target_fact_table_object(PIC, _)
        ),
        Info0 = !.Info,
        Deps0 = !.Deps,

        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        DepSpec = self(TargetCodeType),
        find_dep_spec(Globals, ModuleIndex, DepSpec,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),
        deps_set_union(Deps0, NewDepsB, DepsB),

        add_compilation_targets_of_module_as_deps(Globals, PIC, ModuleIndex,
            Deps0, DepsA, Info0, InfoA),
        NewSucceededA = succeeded,

        eqv_states("java", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ;
        TargetType = module_target_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        Deps0 = !.Deps,
        Info0 = !.Info,

        DepSpecSelf = self(TargetCodeType),
        DepSpecMh = foreign_imports(module_target_c_header(header_mh)),
        ( if
            CompilationTarget = target_c,
            HighLevelCode = yes
        then
            DepSpecs = [DepSpecSelf, DepSpecMh,
                direct_imports(module_target_c_header(header_mih)),
                indirect_imports(module_target_c_header(header_mih)),
                ancestors(module_target_c_header(header_mih)),
                intermod_imports(module_target_c_header(header_mih))
            ]
        else
            DepSpecs = [DepSpecSelf, DepSpecMh]
        ),
        find_dep_specs(Globals, ModuleIndex, DepSpecs,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),
        deps_set_union(NewDepsB, Deps0, DepsB),

        % TargetCodeType of self
        add_targets_of_modules_as_deps(TargetCodeType, [ModuleIndex],
            Deps0, Deps1A, Info0, Info1A),
        % For --highlevel-code, the `.c' file will #include the .mih file
        % for all imported modules.
        ( if
            CompilationTarget = target_c,
            HighLevelCode = yes
        then
            HeaderDeps = combine_deps_list([
                module_target_c_header(header_mih) `of` direct_imports,
                module_target_c_header(header_mih) `of` indirect_imports,
                module_target_c_header(header_mih) `of` ancestors,
                module_target_c_header(header_mih) `of` intermod_imports
            ])
        else
            HeaderDeps = no_deps
        ),
        FindDeps = combine_deps_list([
            module_target_c_header(header_mh) `of` foreign_imports,
            HeaderDeps
        ]),
        FindDeps(Globals, ModuleIndex, NewSucceededA, NewDepsA,
            Info1A, InfoA, !IO),
        deps_set_union(NewDepsA, Deps1A, DepsA),

        eqv_states("c_object", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ;
        ( TargetType = module_target_opt
        ; TargetType = module_target_xml_doc
        ),
        Deps0 = !.Deps,
        Info0 = !.Info,

        DepSpecs = [
            self(module_target_source),
            % XXX Should we cache the remaining dep_specs as a whole?
            ancestors(module_target_int0),
            non_intermod_direct_imports(module_target_int1),
            non_intermod_indirect_imports(module_target_int2)
        ],
        find_dep_specs(Globals, ModuleIndex, DepSpecs,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),
        deps_set_union(NewDepsB, Deps0, DepsB),

        % module_target_source of self
        add_targets_of_modules_as_deps(module_target_source, [ModuleIndex],
            Deps0, Deps1A, Info0, Info1A),
        % module_target_int0 of ancestors
        add_targets_of_ancestors_as_deps(module_target_int0, ModuleIndex,
            Deps1A, Deps2A, Info1A, Info2A),
        FindDeps = combine_deps_list([
            module_target_int1 `of` non_intermod_direct_imports,
            module_target_int2 `of` non_intermod_indirect_imports
        ]),
        FindDeps(Globals, ModuleIndex, NewSucceededA, NewDepsA,
            Info2A, InfoA, !IO),
        deps_set_union(NewDepsA, Deps2A, DepsA),

        eqv_states("opt_xml", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ;
        TargetType = module_target_analysis_registry,
        Deps0 = !.Deps,
        Info0 = !.Info,

        DepSpecs = [
            self(module_target_source),
            ancestors(module_target_int0),
            non_intermod_direct_imports(module_target_int1),
            non_intermod_indirect_imports(module_target_int2),
            direct_imports(module_target_opt),
            indirect_imports(module_target_opt),
            intermod_imports(module_target_opt)
        ],
        find_dep_specs(Globals, ModuleIndex, DepSpecs,
            NewSucceededB, NewDepsB, Info0, InfoB, !IO),
        deps_set_union(NewDepsB, Deps0, DepsB),

        % module_target_source of self
        add_targets_of_modules_as_deps(module_target_source, [ModuleIndex],
            Deps0, Deps1A, Info0, Info1A),
        % module_target_int0 of ancestors
        add_targets_of_ancestors_as_deps(module_target_int0, ModuleIndex,
            Deps1A, Deps2A, Info1A, Info2A),
        FindDeps = combine_deps_list([
            module_target_int1 `of` non_intermod_direct_imports,
            module_target_int2 `of` non_intermod_indirect_imports,
            module_target_opt `of` direct_imports,
            module_target_opt `of` indirect_imports,
            module_target_opt `of` intermod_imports
        ]),
        FindDeps(Globals, ModuleIndex, NewSucceededA, NewDepsA,
            Info2A, InfoA, !IO),
        deps_set_union(NewDepsA, Deps2A, DepsA),

        eqv_states("registry", NewSucceededA, NewSucceededB, NewSucceeded,
            DepsA, DepsB, !:Deps, InfoA, InfoB, !:Info)
    ).

:- pred add_compilation_targets_of_module_as_deps(globals::in, pic::in,
    module_index::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

add_compilation_targets_of_module_as_deps(Globals, PIC, ModuleIndex,
        !Deps, !Info) :-
    globals.get_target(Globals, CompilationTarget),
    TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
    % TargetCode of self
    add_targets_of_modules_as_deps(TargetCodeType, [ModuleIndex], !Deps, !Info).

:- func target_to_module_target_code(compilation_target, pic)
    = module_target_type.

target_to_module_target_code(_CompilationTarget, _PIC) = TargetCode :-
    % XXX it looks wrong to be returning module_target_c_code for
    % all compilation targets.
    TargetCode = module_target_c_code.

:- pred interface_file_dependencies(
    find_module_deps(dependency_file_index)::out(find_module_deps),
    list(dep_spec)::out) is det.

interface_file_dependencies(Deps, DepSpecs) :-
    Deps = combine_deps_list([
        module_target_source `of` self,
        module_target_int0 `of` ancestors,
        module_target_int3 `of` direct_imports,
        module_target_int3 `of` indirect_imports
    ]),
    DepSpecs = [
        self(module_target_source),
        ancestors(module_target_int0),
        direct_imports(module_target_int3),
        indirect_imports(module_target_int3)
    ].

:- pred compiled_code_dependencies(globals::in,
    find_module_deps(dependency_file_index)::out(find_module_deps),
    list(dep_spec)::out) is det.

compiled_code_dependencies(Globals, Deps, DepSpecs) :-
    % We build up Deps in stages.

    % Stage 0: dependencies on flags.
    globals.lookup_bool_option(Globals, track_flags, TrackFlags),
    (
        TrackFlags = yes,
        DepsTracks = [module_target_track_flags `of` self],
        DepSpecsTracks = [self(module_target_track_flags)]
    ;
        TrackFlags = no,
        DepsTracks = [],
        DepSpecsTracks = []
    ),

    % Stage 1: dependencies on the source file, and on the fact table files,
    % foreign language files and Mercury interface files it imports.
    DepsSrcInts = [
        module_target_source `of` self,
        fact_table_files `files_of` self,
        foreign_include_files `files_of` self,
        module_target_int1 `of` self,
        module_target_int1 `of` ancestors,
        imports_012
    ],
    DepSpecsSrcInts = [
        self(module_target_source),
        self_fact_table_files,
        self_foreign_include_files,
        self(module_target_int1),
        ancestors(module_target_int1),
        imports_012
    ],

    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    AnyIntermod = bool.or(IntermodOpt, IntermodAnalysis),

    % Stage 2: dependencies on optimization files.
    (
        AnyIntermod = yes,
        DepsOpts = [
            module_target_opt `of` self,
            module_target_opt `of` intermod_imports,
            get_intermod_imports_their_ancestors_and_012
        ],
        DepSpecsOpts = [
            self(module_target_opt),
            intermod_imports(module_target_opt),
            intermod_imports_their_ancestors_and_012
        ]
    ;
        AnyIntermod = no,
        DepsOpts = [],
        DepSpecsOpts = []
    ),

    % Stage 3: dependencies on analysis result files.
    (
        IntermodAnalysis = yes,
        DepsRegistries = [
            module_target_analysis_registry `of` self,
            module_target_analysis_registry `of` direct_imports
        ],
        DepSpecsRegistries = [
            self(module_target_analysis_registry),
            direct_imports(module_target_analysis_registry)
        ]
    ;
        IntermodAnalysis = no,
        DepsRegistries = [],
        DepSpecsRegistries = []
    ),

    DepsAll = inst_preserving_condense(
        [DepsTracks, DepsSrcInts, DepsOpts, DepsRegistries]),
    Deps = combine_deps_list(DepsAll),
    DepSpecs = DepSpecsTracks ++ DepSpecsSrcInts ++
        DepSpecsOpts ++ DepSpecsRegistries.

:- func imports_012 =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

imports_012 =
    cache_computed_module_deps(computed_module_deps_import_012,
        combine_deps_list([
            module_target_int0 `of` ancestors,
            module_target_int1 `of` direct_imports,
            module_target_int2 `of` indirect_imports
        ])
    ).

%---------------------------------------------------------------------------%

:- pred add_targets_of_modules_as_deps(module_target_type::in,
    list(module_index)::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

add_targets_of_modules_as_deps(TargetType, ModuleIndexes, !Deps, !Info) :-
    list.map_foldl(target_of_module_to_dep_file_index(TargetType),
        ModuleIndexes, TargetFileIndexes, !Info),
    % Converting TargetFileIndexes to a set, and then unioning !.Deps
    % with that set should usually be faster than inserting its elements
    % into !.Deps one by one. This is because TargetFileIndexes can be expected
    % to usually be not-seen-before indexes, which means that calling
    % insert_list here would end up repeatedly appending to the end of !.Deps.
    deps_set_union(list_to_deps_set(TargetFileIndexes), !Deps).

:- pred target_of_module_to_dep_file_index(module_target_type::in,
    module_index::in, dependency_file_index::out,
    make_info::in, make_info::out) is det.

target_of_module_to_dep_file_index(TargetType, ModuleIndex, TargetFileIndex,
        !Info) :-
    TargetFile = dfmi_target(ModuleIndex, TargetType),
    dependency_file_to_index(TargetFile, TargetFileIndex, !Info).

%---------------------------------------------------------------------------%

:- pred get_intermod_imports_and_their_ancestors(
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_intermod_imports_and_their_ancestors(Globals, ModuleIndex, Succeeded,
        Result, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    intermod_imports(Globals, ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Result = deps_set_init
    else
        deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
            ancestors, Globals, to_sorted_list(Modules1),
            succeeded, Succeeded2, deps_set_init, Result, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

:- pred get_intermod_imports_their_ancestors_and_012(globals::in,
    module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_intermod_imports_their_ancestors_and_012(Globals, ModuleIndex,
        Succeeded, Result, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    get_intermod_imports_and_their_ancestors(Globals,
        ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Result = deps_set_init
    else
        ModulesList1 = deps_set_to_sorted_list(Modules1),
        deps_set_foldl3_maybe_stop_at_error_find_union_fi(KeepGoing,
            imports_012, Globals, ModulesList1,
            succeeded, Succeeded2, deps_set_init, Result, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

%---------------------------------------------------------------------------%

:- pred direct_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

direct_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    CachedDirectImports0 = make_info_get_cached_direct_imports(!.Info),
    ( if map.search(CachedDirectImports0, ModuleIndex, Result0) then
        Result0 = deps_result(Succeeded, Modules)
    else
        KeepGoing = make_info_get_keep_going(!.Info),
        non_intermod_direct_imports(Globals, ModuleIndex, Succeeded0, Modules0,
            !Info, !IO),
        ( if
            Succeeded0 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed,
            Modules = deps_set_init
        else
            % We also read `.int' files for the modules for which we read
            % `.opt' files, and for the modules imported by those modules.
            intermod_imports(Globals, ModuleIndex, Succeeded1, IntermodModules,
                !Info, !IO),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed,
                Modules = deps_set_init
            else
                deps_set_union(IntermodModules, Modules0, Modules1),
                deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
                    non_intermod_direct_imports, Globals,
                    deps_set_to_sorted_list(IntermodModules),
                    succeeded, Succeeded2, Modules1, Modules2, !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2,
                deps_set_delete(ModuleIndex, Modules2, Modules)
            )
        ),
        Result = deps_result(Succeeded, Modules),
        CachedDirectImports1 = make_info_get_cached_direct_imports(!.Info),
        map.det_insert(ModuleIndex, Result,
            CachedDirectImports1, CachedDirectImports),
        make_info_set_cached_direct_imports(CachedDirectImports, !Info)
    ).

    % Return the modules for which `.int' files are read in a compilation
    % which does not use `--intermodule-optimization'.
    %
:- pred non_intermod_direct_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_direct_imports(Globals, ModuleIndex, Succeeded, Modules,
        !Info, !IO) :-
    CachedNonIntermodDirectImports0 =
        make_info_get_cached_non_intermod_direct_imports(!.Info),
    ( if map.search(CachedNonIntermodDirectImports0, ModuleIndex, Result0) then
        Result0 = deps_result(Succeeded, Modules)
    else
        % XXX MAKE_STREAM
        io.output_stream(ProgressStream, !IO),
        non_intermod_direct_imports_uncached(ProgressStream, Globals,
            ModuleIndex, Succeeded, Modules, !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        CachedNonIntermodDirectImports1 =
            make_info_get_cached_non_intermod_direct_imports(!.Info),
        map.det_insert(ModuleIndex, Result,
            CachedNonIntermodDirectImports1, CachedNonIntermodDirectImports),
        make_info_set_cached_non_intermod_direct_imports(
            CachedNonIntermodDirectImports, !Info)
    ).

:- pred non_intermod_direct_imports_uncached(io.text_output_stream::in,
    globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_direct_imports_uncached(ProgressStream, Globals, ModuleIndex,
        Succeeded, Modules, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),

        % Find the direct imports of this module (modules for which we will
        % read the `.int' files).
        %
        % Note that we need to do this both for the imports of this module
        % and for the imports of its ancestors. This is because if this module
        % is a submodule, then it may depend on things imported only by its
        % ancestors.
        %
        module_dep_info_get_int_deps(ModuleDepInfo, IntDeps),
        module_dep_info_get_imp_deps(ModuleDepInfo, ImpDeps),
        module_names_to_index_set(set.to_sorted_list(IntDeps), DepsInt, !Info),
        module_names_to_index_set(set.to_sorted_list(ImpDeps), DepsImp, !Info),
        deps_set_union(DepsInt, DepsImp, Modules0),
        (
            ModuleName = qualified(ParentModule, _),
            module_name_to_index(ParentModule, ParentIndex, !Info),
            non_intermod_direct_imports(Globals, ParentIndex, Succeeded,
                ParentImports, !Info, !IO),
            deps_set_union(ParentImports, Modules0, Modules)
        ;
            ModuleName = unqualified(_),
            Succeeded = succeeded,
            Modules = Modules0
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Modules = deps_set_init
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.int2' files.
    %
:- pred indirect_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

indirect_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    CachedIndirectImports0 = make_info_get_cached_indirect_imports(!.Info),
    ( if map.search(CachedIndirectImports0, ModuleIndex, CachedResult) then
        CachedResult = deps_result(Succeeded, Modules)
    else
        % XXX MAKE_STREAM
        io.output_stream(ProgressStream, !IO),
        indirect_imports_uncached(ProgressStream, Globals, direct_imports,
            ModuleIndex, Succeeded, Modules, !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        CachedIndirectImports1 = make_info_get_cached_indirect_imports(!.Info),
        map.det_insert(ModuleIndex, Result,
            CachedIndirectImports1, CachedIndirectImports),
        make_info_set_cached_indirect_imports(CachedIndirectImports, !Info)
    ).

    % Return the list of modules for which we should read `.int2' files,
    % ignoring those which need to be read as a result of importing modules
    % imported by a `.opt' file.
    %
:- pred non_intermod_indirect_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_indirect_imports(Globals, ModuleIndex, Succeeded, Modules,
        !Info, !IO) :-
    % XXX MAKE_STREAM
    io.output_stream(ProgressStream, !IO),
    indirect_imports_uncached(ProgressStream, Globals,
        non_intermod_direct_imports, ModuleIndex, Succeeded, Modules,
        !Info, !IO).

:- pred indirect_imports_uncached(io.text_output_stream::in, globals::in,
    find_module_deps(module_index)::in(find_module_deps),
    module_index::in, maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

indirect_imports_uncached(ProgressStream, Globals, FindDirectImports,
        ModuleIndex, Succeeded, IndirectImports, !Info, !IO) :-
    % XXX MDNEW Caller may know DirectImports already.
    FindDirectImports(Globals, ModuleIndex, DirectSucceeded, DirectImports,
        !Info, !IO),
    % XXX The original version of this code by stayl had the line assigning
    % to KeepGoing textually *before* the call to FindDirectImports, but
    % looked up the keep_going in the version of !Info *after* that call.
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        DirectSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        IndirectImports = deps_set_init
    else
        deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
            find_transitive_implementation_imports(ProgressStream), Globals,
            deps_set_to_sorted_list(DirectImports),
            succeeded, IndirectSucceeded,
            deps_set_init, IndirectImports0, !Info, !IO),
        deps_set_delete(ModuleIndex, IndirectImports0, IndirectImports1),
        IndirectImports = deps_set_difference(IndirectImports1, DirectImports),
        Succeeded = DirectSucceeded `and` IndirectSucceeded
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.opt' files.
    %
:- pred intermod_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

intermod_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        globals.lookup_bool_option(Globals, read_opt_files_transitively,
            Transitive),
        (
            Transitive = yes,
            % XXX MAKE_STREAM
            io.output_stream(ProgressStream, !IO),
            find_transitive_implementation_imports(ProgressStream, Globals,
                ModuleIndex, Succeeded, Modules, !Info, !IO)
        ;
            Transitive = no,
            non_intermod_direct_imports(Globals, ModuleIndex, Succeeded,
                Modules, !Info, !IO)
        )
    ;
        AnyIntermod = no,
        Succeeded = succeeded,
        Modules = deps_set_init
    ).

%---------------------------------------------------------------------------%

:- pred foreign_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foreign_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    % The object file depends on the header files for the modules
    % mentioned in `:- pragma foreign_import_module' declarations
    % in the current module and the `.opt' files it imports.

    globals.get_backend_foreign_languages(Globals, Languages),
    LanguagesSet = set.list_to_set(Languages),
    intermod_imports(Globals, ModuleIndex, IntermodSucceeded, IntermodModules,
        !Info, !IO),
    KeepGoing = make_info_get_keep_going(!.Info),
    % XXX MAKE_STREAM
    io.output_stream(ProgressStream, !IO),
    deps_set_insert(ModuleIndex, IntermodModules, IntermodSelfModules),
    deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
        find_module_foreign_imports(ProgressStream, LanguagesSet),
        Globals, to_sorted_list(IntermodSelfModules),
        succeeded, ForeignSucceeded, deps_set_init, Modules, !Info, !IO),
    Succeeded = IntermodSucceeded `and` ForeignSucceeded.

:- pred find_module_foreign_imports(io.text_output_stream::in,
    set(foreign_language)::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_module_foreign_imports(ProgressStream, Languages, Globals, ModuleIndex,
        Succeeded, ForeignModules, !Info, !IO) :-
    % Languages should be constant for the duration of the process,
    % which means that it is unnecessary to include it in the cache key.
    CachedForeignImports0 =
        make_info_get_cached_transitive_foreign_imports(!.Info),
    ( if map.search(CachedForeignImports0, ModuleIndex, CachedResult) then
        CachedResult = deps_result(Succeeded, ForeignModules)
    else
        find_transitive_implementation_imports(ProgressStream, Globals,
            ModuleIndex, Succeeded0, ImportedModules, !Info, !IO),
        (
            Succeeded0 = succeeded,
            KeepGoing = make_info_get_keep_going(!.Info),
            deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
                find_module_foreign_imports_uncached(ProgressStream,
                    Languages),
                Globals, to_sorted_list(insert(ImportedModules, ModuleIndex)),
                succeeded, Succeeded, deps_set_init, ForeignModules,
                !Info, !IO),
            Result = deps_result(Succeeded, ForeignModules),
            CachedForeignImports1 =
                make_info_get_cached_transitive_foreign_imports(!.Info),
            map.det_insert(ModuleIndex, Result,
                CachedForeignImports1, CachedForeignImports),
            make_info_set_cached_transitive_foreign_imports(
                CachedForeignImports, !Info)
        ;
            Succeeded0 = did_not_succeed,
            Succeeded = did_not_succeed,
            ForeignModules = deps_set_init
        )
    ).

:- pred find_module_foreign_imports_uncached(io.text_output_stream::in,
    set(foreign_language)::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_module_foreign_imports_uncached(ProgressStream, Languages, Globals,
        ModuleIndex, Succeeded, ForeignModules, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_fims(ModuleDepInfo, FIMSpecs),
        ForLangsPred =
            ( pred(fim_spec(Lang, Module)::in, Module::out) is semidet :-
                % XXX MDNEW Instead of returning Module,
                % we should add its index to a deps_set (in a fold).
                set.contains(Languages, Lang)
            ),
        set.filter_map(ForLangsPred, FIMSpecs, ForeignModuleNameSet),
        module_names_to_index_set(set.to_sorted_list(ForeignModuleNameSet),
            ForeignModules, !Info),
        Succeeded = succeeded
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        ForeignModules = deps_set_init,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred find_transitive_implementation_imports(io.text_output_stream::in,
    globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_implementation_imports(ProgressStream, Globals, ModuleIndex,
        Succeeded, Modules, !Info, !IO) :-
    % XXX MDNEW process_modules_anywhere allows a .module_dep file
    % from a directory far down a search path to create a reference
    % to a module that exists in the *current* directory, if its name
    % duplicates the name of a module in the .module_dep file's directory.
    % This causes the failure of e.g. the warnings/bug311 test case,
    % with the module in the current directory being time.m.
    find_transitive_module_dependencies(ProgressStream, Globals, all_imports,
        process_modules_anywhere, ModuleIndex, Succeeded, Modules0,
        !Info, !IO),
    deps_set_insert(ModuleIndex, Modules0, Modules),
    trace [
        compile_time(flag("find_trans_impl_imports")),
        run_time(env("FIND_TRANS_IMPL_IMPORTS")),
        io(!TIO)
    ] (
        io.output_stream(OutputStream, !TIO),
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        module_index_set_to_plain_set(!.Info, Modules, ModuleSet),
        ModuleList = set.to_sorted_list(ModuleSet),
        ModuleNlStrs = list.map(
            (func(M) = "    " ++ sym_name_to_string(M) ++ "\n"),
            ModuleList),
        io.format(OutputStream, "trans impl imports for %s:\n",
            [s(IndexModuleNameStr)], !TIO),
        list.foldl(io.write_string(OutputStream), ModuleNlStrs, !TIO),
        io.write_string(OutputStream, "trans impl imports list ends\n\n", !TIO)
    ).

%---------------------------------------------------------------------------%

:- pred fact_table_files(globals::in, module_index::in,
    maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fact_table_files(Globals, ModuleIndex, Succeeded, Files, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    % XXX MAKE_STREAM
    io.output_stream(ProgressStream, !IO),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFiles),
        Files = set.map((func(File) = dep_file(File)), FactTableFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Files = set.init
    ).

%---------------------------------------------------------------------------%

:- pred foreign_include_files(globals::in, module_index::in,
    maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foreign_include_files(Globals, ModuleIndex, Succeeded, Files, !Info, !IO) :-
    globals.get_backend_foreign_languages(Globals, Languages),
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    % XXX MAKE_STREAM
    io.output_stream(ProgressStream, !IO),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_source_file_name(ModuleDepInfo, SourceFileName),
        module_dep_info_get_foreign_include_files(ModuleDepInfo,
            ForeignIncludeFiles),
        FilesList = get_foreign_include_files(set.list_to_set(Languages),
            SourceFileName, set.to_sorted_list(ForeignIncludeFiles)),
        Files = set.list_to_set(FilesList)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Files = set.init
    ).

:- func get_foreign_include_files(set(foreign_language), file_name,
    list(foreign_include_file_info)) = list(dependency_file).

get_foreign_include_files(Languages, SourceFileName, ForeignIncludes)
        = Files :-
    list.filter_map(get_foreign_include_files_2(Languages, SourceFileName),
        ForeignIncludes, Files).

:- pred get_foreign_include_files_2(set(foreign_language)::in, file_name::in,
    foreign_include_file_info::in, dependency_file::out) is semidet.

get_foreign_include_files_2(Languages, SourceFileName, ForeignInclude, File) :-
    ForeignInclude = foreign_include_file_info(Language, IncludeFileName),
    set.member(Language, Languages),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
    File = dep_file(IncludePath).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% NEW DEPENDENCY SPECIFICATION MACHINERY
%
% The code in this section is intended to replace the code in the
% section named OLD DEPENDENCY SPECIFICATION MACHINERY.
%
% The old machinery specified the set of dependency_files that a "mmc --make"
% target depends on using closures containing closures containing closures.
%
% - The top layer of closures contained one of the "combine_deps" and
%   "combine_deps_list" predicates.
%
% - The middle layer of closures contained one of the "of" and "files_of"
%   predicates.
%
% - The bottom layer of closures contained one of the predicates mentioned
%   in the right arguments of the `of` and "files_of" infix operators in the
%   rest of the module.
%
% The new machinery replaces the two top layers of closures with explicit
% data structures. It replaces the closures constructed by "of" and "files_of"
% with values of the dep_spec type, and it replaces combine_deps and
% combine_deps_list closures with lists of dep_specs. The deletion of these
% top two layers of closures then turns the calls to the bottom layers from 
% being higher order calls to being first order calls, which thus deletes
% the bottom layer of closures as well.
%
% This approach is
%
% - much more readable and understandable (because the logic is explicit),
%
% - more debuggable (again, because the tasks are explicit), and
%
% - it is more flexible, in that it does not require the predicates used
%   in each layer to have exactly the same argument list in its usual
%   curried form.
%
% the main motivation for this change is this last advantage, since
% should make it possible to pass a progress stream to every predicate
% that needs it *without* modifying "of", "files_of", "combine_deps", and
% "combine_deps_list", which would also require a bunch of predicates
% that don't do I/O to take that argument.
%

    % The dependency specification type.
    %
    % Values of this type indirectly represent the specification of
    % a set of dependency_files (actually, dependency_file_indexes).
    % The "indirect" part is there because they actually represent 
    % a specification of a task for find_dep_spec, which will compute
    % that set of dependencys when given a dep_spec.
    %
    % XXX MDNEW Resolve ambiguities between these function symbols
    % and the predicates whose names they share by renaming the predicates.
    %
:- type dep_spec
    --->    self(module_target_type)
    ;       self_fact_table_files
    ;       self_foreign_include_files
    ;       ancestors(module_target_type)
    ;       direct_imports(module_target_type)
    ;       indirect_imports(module_target_type)
    ;       non_intermod_direct_imports(module_target_type)
    ;       non_intermod_indirect_imports(module_target_type)
    ;       intermod_imports(module_target_type)
    ;       foreign_imports(module_target_type)

    ;       imports_012
            % XXX MDNEW Get a better name.
    ;       intermod_imports_their_ancestors_and_012.
            % XXX MDNEW See the other XXX MDNEWs below.

:- pred find_dep_specs(globals::in, module_index::in, list(dep_spec)::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_dep_specs(_, _, [], succeeded, deps_set_init, !Info, !IO).
find_dep_specs(Globals, ModuleIndex, [HeadDepSpec | TailDepSpecs],
        Succeeded, DepFileIndexSet, !Info, !IO) :-
    find_dep_spec(Globals, ModuleIndex, HeadDepSpec,
        HeadSucceeded, HeadDepFileIndexSet, !Info, !IO),
    % XXX MDNEW Pass KeepGoing as a separate arg to make clear
    % that its value is constant throughout the entire process.
    ( if
        HeadSucceeded = did_not_succeed,
        make_info_get_keep_going(!.Info) = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        DepFileIndexSet = HeadDepFileIndexSet
    else
        find_dep_specs(Globals, ModuleIndex, TailDepSpecs,
            TailSucceeded, TailDepFileIndexSet, !Info, !IO),
        Succeeded = HeadSucceeded `and` TailSucceeded,
        deps_set_union(HeadDepFileIndexSet, TailDepFileIndexSet,
            DepFileIndexSet)
    ).

:- pred find_dep_spec(globals::in, module_index::in, dep_spec::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_dep_spec(Globals, ModuleIndex, DepSpec, Succeeded, DepFileIndexSet,
        !Info, !IO) :-
    trace [
        compile_time(flag("find_dep_spec")),
        run_time(env("FIND_DEP_SPEC")),
        io(!TIO)
    ] (
        io.output_stream(OutputStream, !TIO),
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        io.format(OutputStream, "starting dep_spec %s for %s\n",
            [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
    ),
    % XXX Some of these alternatives don't need I/O.
    % We can wrap caching code around the code of any set of switch arms.
    (
        DepSpec = self(TargetType),
        Succeeded = succeeded,
        acc_rev_dfmi_target(TargetType, ModuleIndex,
            deps_set_init, DepFileIndexSet, !Info)
    ;
        DepSpec = self_fact_table_files,
        fact_table_files(Globals, ModuleIndex,
            Succeeded, FactTableDepFiles, !Info, !IO),
        % XXX MDNEW Push this call into fact_table_files
        % when the old machinery is deleted.
        dependency_files_to_index_set(set.to_sorted_list(FactTableDepFiles),
            DepFileIndexSet, !Info)
    ;
        DepSpec = self_foreign_include_files,
        foreign_include_files(Globals, ModuleIndex,
            Succeeded, ForeignDepFiles, !Info, !IO),
        % XXX MDNEW Push this call into foreign_include_files
        % when the old machinery is deleted.
        dependency_files_to_index_set(set.to_sorted_list(ForeignDepFiles),
            DepFileIndexSet, !Info)
    ;
        DepSpec = ancestors(TargetType),
        Succeeded = succeeded,
        module_index_to_name(!.Info, ModuleIndex, ModuleName),
        Ancestors = get_ancestors(ModuleName),
        module_names_to_index_set(Ancestors, ModuleIndexSet, !Info),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = direct_imports(TargetType),
        direct_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = indirect_imports(TargetType),
        indirect_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = non_intermod_direct_imports(TargetType),
        non_intermod_direct_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = non_intermod_indirect_imports(TargetType),
        non_intermod_indirect_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = intermod_imports(TargetType),
        intermod_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = foreign_imports(TargetType),
        % XXX not yet used
        foreign_imports(Globals, ModuleIndex,
            Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = imports_012,
        % XXX cache
        SubDepSpecs = [
            ancestors(module_target_int0),
            direct_imports(module_target_int1),
            indirect_imports(module_target_int2)
        ],
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            io.output_stream(OutputStream, !TIO),
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(OutputStream, "dep_spec %s for %s starts\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ),
        find_dep_specs(Globals, ModuleIndex, SubDepSpecs, Succeeded,
            DepFileIndexSet, !Info, !IO),
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            io.output_stream(OutputStream, !TIO),
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(OutputStream, "dep_spec %s for %s ends\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        DepSpec = intermod_imports_their_ancestors_and_012,
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            io.output_stream(OutputStream, !TIO),
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(OutputStream, "dep_spec %s for %s starts\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ),
        new_get_intermod_imports_their_ancestors_and_012(Globals, ModuleIndex,
            Succeeded, DepFileIndexSet, !Info, !IO),
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            io.output_stream(OutputStream, !TIO),
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(OutputStream, "dep_spec %s for %s ends\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ),
    trace [
        compile_time(flag("find_dep_spec")),
        run_time(env("FIND_DEP_SPEC")),
        io(!TIO)
    ] (
        io.output_stream(OutputStream, !TIO),
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        dependency_file_index_set_to_plain_set(!.Info, DepFileIndexSet,
            DepFileSet),
        DepFiles = set.to_sorted_list(DepFileSet),
        (
            DepFiles = [],
            io.format(OutputStream, "dep_spec %s for %s yields no deps\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ;
            DepFiles = [_ | _],
            DepFileNlStrs = list.map(
                dependency_file_to_debug_string("    ", "\n"), DepFiles),
            io.format(OutputStream, "dep_spec %s for %s yields these deps:\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO),
            list.foldl(io.write_string(OutputStream), DepFileNlStrs, !TIO),
            io.write_string(OutputStream, "dep list ends\n\n", !TIO)
        )
    ).

:- pred dfmi_targets(deps_set(module_index)::in, module_target_type::in,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info) :-
    deps_set_foldl2(acc_rev_dfmi_target(TargetType), ModuleIndexSet,
        deps_set_init, DepFileIndexSet, !Info).

% XXX MDNEW This predicate would be better named something like
% get_ancestors_of_intermod_imports.
:- pred new_get_intermod_imports_and_their_ancestors(
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

new_get_intermod_imports_and_their_ancestors(Globals, ModuleIndex, Succeeded,
        ModuleIndexSet, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    intermod_imports(Globals, ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        ModuleIndexSet = deps_set_init
    else
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        list.map_foldl(index_get_ancestors, ModuleList1,
            AncestorModuleIndexSets, !Info),
        ModuleIndexSet = deps_set_union_list(AncestorModuleIndexSets),
        Succeeded = Succeeded1
    ).

% XXX MDNEW This predicate would be better named something like
% get_int012_of_ancestors_of_intermod_imports.
:- pred new_get_intermod_imports_their_ancestors_and_012(
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

new_get_intermod_imports_their_ancestors_and_012(Globals, ModuleIndex,
        Succeeded, DepFileIndexSet, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    new_get_intermod_imports_and_their_ancestors(Globals, ModuleIndex,
        Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        DepFileIndexSet = deps_set_init
    else
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        fold_dep_spec_over_modules_stop_at_error_fi(KeepGoing, Globals,
            imports_012, ModuleList1,
            succeeded, Succeeded2, deps_set_init, DepFileIndexSet, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

:- pred index_get_ancestors(module_index::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

index_get_ancestors(ModuleIndex, AncestorModuleIndexSet, !Info) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    AncestorModuleNames = get_ancestors(ModuleName),
    module_names_to_index_set(AncestorModuleNames, AncestorModuleIndexSet,
        !Info).

%---------------------------------------------------------------------------%

    % Check the equivalence of results generated by the old and new
    % machineries. The results from the old and new machineries
    % are represented by the variables ending in A and B respectively.
    %
    % Note that for some kinds of dep_specs, Deps[AB] represent
    % the files *added* as dependencies by processing that dep_spec,
    % while for the other kinds of dep_specs, Deps[AB] represent
    % the files *we end up with* as dependencies after processing
    % that dep_spec. The difference is that the latter also includes
    % the files that were already known to be dependencies *before*
    % processing the dep_spec.
    %
:- pred eqv_states(string::in,
    maybe_succeeded::in, maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::in,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::in, make_info::out) is det.

eqv_states(Id, SucceededA, SucceededB, Succeeded, DepsA, DepsB, Deps,
        InfoA, InfoB, Info) :-
    some [!Msgs]
    (
        !:Msgs = cord.init,
        ( if SucceededA = SucceededB then
            true
        else
            cord.snoc("succeeded mismatch", !Msgs),
            cord.snoc(string.string(SucceededA), !Msgs),
            cord.snoc(string.string(SucceededB), !Msgs)
        ),

        DepsALA = deps_set_to_sorted_list(DepsA),
        DepsALB = deps_set_to_sorted_list(DepsB),
        ( if DepsALA = DepsALB then
            true
        else
            cord.snoc("deps mismatch", !Msgs),
            report_list_mismatch(DepsALA, DepsALB, !Msgs)
        ),

        OptionArgsA = make_info_get_option_args(InfoA),
        OptionArgsB = make_info_get_option_args(InfoB),
        ( if OptionArgsA = OptionArgsB then
            true
        else
            cord.snoc("option args mismatch", !Msgs),
            report_list_mismatch(OptionArgsA, OptionArgsB, !Msgs)
        ),

        CmdLineTargetSetA = make_info_get_command_line_targets(InfoA),
        CmdLineTargetSetB = make_info_get_command_line_targets(InfoB),
        set.to_sorted_list(CmdLineTargetSetA, CmdLineTargetsA),
        set.to_sorted_list(CmdLineTargetSetB, CmdLineTargetsB),
        ( if CmdLineTargetsA = CmdLineTargetsB then
            true
        else
            cord.snoc("command line targets mismatch", !Msgs),
            report_list_mismatch(CmdLineTargetsA, CmdLineTargetsB, !Msgs)
        ),

        RebuildDepsA = make_info_get_rebuild_module_deps(InfoA),
        RebuildDepsB = make_info_get_rebuild_module_deps(InfoB),
        ( if RebuildDepsA = RebuildDepsB then
            true
        else
            cord.snoc("rebuild deps mismatch", !Msgs),
            cord.snoc(string.string(RebuildDepsA), !Msgs),
            cord.snoc(string.string(RebuildDepsB), !Msgs)
        ),

        ReanalysisPassesA = make_info_get_reanalysis_passes(InfoA),
        ReanalysisPassesB = make_info_get_reanalysis_passes(InfoB),
        ( if ReanalysisPassesA = ReanalysisPassesB then
            true
        else
            cord.snoc("reanalysis passes mismatch", !Msgs),
            cord.snoc(string.string(ReanalysisPassesA), !Msgs),
            cord.snoc(string.string(ReanalysisPassesB), !Msgs)
        ),

        ModuleDepsMapA = make_info_get_module_dependencies(InfoA),
        ModuleDepsMapB = make_info_get_module_dependencies(InfoB),
        map.to_sorted_assoc_list(ModuleDepsMapA, ModuleDepsALA),
        map.to_sorted_assoc_list(ModuleDepsMapB, ModuleDepsALB),
        ( if ModuleDepsALA = ModuleDepsALB then
            true
        else
            cord.snoc("module dependencies mismatch", !Msgs),
            report_assoc_list_mismatch(ModuleDepsALA, ModuleDepsALB, !Msgs)
        ),

        FileTimestampsMapA = make_info_get_file_timestamps(InfoA),
        FileTimestampsMapB = make_info_get_file_timestamps(InfoB),
        map.to_sorted_assoc_list(FileTimestampsMapA, FileTimestampsA),
        map.to_sorted_assoc_list(FileTimestampsMapB, FileTimestampsB),
        ( if FileTimestampsA = FileTimestampsB then
            true
        else
            cord.snoc("file timestamps mismatch", !Msgs),
            report_assoc_list_mismatch(FileTimestampsA, FileTimestampsB, !Msgs)
        ),

        TargetFileTimestampHashA = make_info_get_target_file_timestamps(InfoA),
        TargetFileTimestampHashB = make_info_get_target_file_timestamps(InfoB),
        TargetFileTimestampsA =
            version_hash_table.to_assoc_list(TargetFileTimestampHashA),
        TargetFileTimestampsB =
            version_hash_table.to_assoc_list(TargetFileTimestampHashB),
        list.sort(TargetFileTimestampsA, SortedTargetFileTimestampsA),
        list.sort(TargetFileTimestampsB, SortedTargetFileTimestampsB),
        ( if SortedTargetFileTimestampsA = SortedTargetFileTimestampsB then
            true
        else
            cord.snoc("target file timestamps mismatch", !Msgs),
            report_assoc_list_mismatch(SortedTargetFileTimestampsA,
                SortedTargetFileTimestampsB, !Msgs)
        ),

        ModuleIndexMapA = make_info_get_module_index_map(InfoA),
        ModuleIndexMapB = make_info_get_module_index_map(InfoB),
        ModuleIndexMapA =
            module_index_map(ModuleForwardHashA, ModuleReverseA, ModuleCntA),
        ModuleIndexMapB =
            module_index_map(ModuleForwardHashB, ModuleReverseB, ModuleCntB),
        ( if ModuleCntA = ModuleCntB then
            true
        else
            cord.snoc("module index count mismatch", !Msgs),
            cord.snoc(string.string(ModuleCntA), !Msgs),
            cord.snoc(string.string(ModuleCntB), !Msgs)
        ),
        ModuleForwardALA = version_hash_table.to_assoc_list(ModuleForwardHashA),
        ModuleForwardALB = version_hash_table.to_assoc_list(ModuleForwardHashB),
        list.sort(ModuleForwardALA, SortedModuleForwardALA),
        list.sort(ModuleForwardALB, SortedModuleForwardALB),
        ( if SortedModuleForwardALA = SortedModuleForwardALB then
            true
        else
            cord.snoc("module index forward mismatch", !Msgs),
            report_assoc_list_mismatch(SortedModuleForwardALA,
                SortedModuleForwardALB, !Msgs)
        ),
        ModuleReverseListA = version_array.to_list(ModuleReverseA),
        ModuleReverseListB = version_array.to_list(ModuleReverseB),
        list.sort(ModuleReverseListA, SortedModuleReverseListA),
        list.sort(ModuleReverseListB, SortedModuleReverseListB),
        ( if SortedModuleReverseListA = SortedModuleReverseListB then
            true
        else
            cord.snoc("module index reverse mismatch", !Msgs),
            report_list_mismatch(SortedModuleReverseListA,
                SortedModuleReverseListB, !Msgs)
        ),

        DepIndexMapA = make_info_get_dep_file_index_map(InfoA),
        DepIndexMapB = make_info_get_dep_file_index_map(InfoB),
        DepIndexMapA =
            dependency_file_index_map(DepForwardHashA, DepReverseA,
                DepCntA),
        DepIndexMapB =
            dependency_file_index_map(DepForwardHashB, DepReverseB,
                DepCntB),
        ( if DepCntA = DepCntB then
            true
        else
            cord.snoc("dep file index count mismatch", !Msgs),
            cord.snoc(string.string(DepCntA), !Msgs),
            cord.snoc(string.string(DepCntB), !Msgs)
        ),
        DepForwardALA = version_hash_table.to_assoc_list(DepForwardHashA),
        DepForwardALB = version_hash_table.to_assoc_list(DepForwardHashB),
        list.sort(DepForwardALA, SortedDepForwardALA),
        list.sort(DepForwardALB, SortedDepForwardALB),
        ( if SortedDepForwardALA = SortedDepForwardALB then
            true
        else
            cord.snoc("dep file index forward mismatch", !Msgs),
            report_assoc_list_mismatch(SortedDepForwardALA,
                SortedDepForwardALB, !Msgs)
        ),
        DepReverseListA = version_array.to_list(DepReverseA),
        DepReverseListB = version_array.to_list(DepReverseB),
        list.sort(DepReverseListA, SortedDepReverseListA),
        list.sort(DepReverseListB, SortedDepReverseListB),
        ( if SortedDepReverseListA = SortedDepReverseListB then
            true
        else
            cord.snoc("dep file index reverse mismatch", !Msgs),
            report_list_mismatch(SortedDepReverseListA,
                SortedDepReverseListB, !Msgs)
        ),

        DepStatusHashA = make_info_get_dependency_status(InfoA),
        DepStatusHashB = make_info_get_dependency_status(InfoB),
        DepStatusALA = version_hash_table.to_assoc_list(DepStatusHashA),
        DepStatusALB = version_hash_table.to_assoc_list(DepStatusHashB),
        list.sort(DepStatusALA, SortedDepStatusALA),
        list.sort(DepStatusALB, SortedDepStatusALB),
        ( if SortedDepStatusALA = SortedDepStatusALB then
            true
        else
            cord.snoc("dep status mismatch", !Msgs),
            report_assoc_list_mismatch(SortedDepStatusALA, SortedDepStatusALB,
                !Msgs)
        ),

        ErrorModuleSetA = make_info_get_error_file_modules(InfoA),
        ErrorModuleSetB = make_info_get_error_file_modules(InfoB),
        set.to_sorted_list(ErrorModuleSetA, ErrorModulesA),
        set.to_sorted_list(ErrorModuleSetB, ErrorModulesB),
        ( if ErrorModulesA = ErrorModulesB then
            true
        else
            cord.snoc("error file modules mismatch", !Msgs),
            report_list_mismatch(ErrorModulesA, ErrorModulesB, !Msgs)
        ),

        ImportingModuleA = make_info_get_importing_module(InfoA),
        ImportingModuleB = make_info_get_importing_module(InfoB),
        ( if ImportingModuleA = ImportingModuleB then
            true
        else
            cord.snoc("importing module mismatch", !Msgs),
            cord.snoc(string.string(ImportingModuleA), !Msgs),
            cord.snoc(string.string(ImportingModuleB), !Msgs)
        ),

        ( if cord.is_empty(!.Msgs) then
            Succeeded = SucceededB,
            Deps = DepsB,
            Info = InfoB
        else
            % XXX MDNEW Without enforcing a requirement that the
            % new machinery generate the same updated make_info structure
            % as the old machinery results in a bootcheck that has exactly
            % the same results (248 failed tests) as a compiler using only
            % the old machinery.
            %
            % Enforcing a requirement yields a bootcheck that still builds
            % the right stage 3, but causes some extra test case failures,
            % all of which come from the exception thrown below.
            % These failures are
            %
            % valid_make_int/bug506
            % valid_make_int/test_repn
            % warnings/arg_order_arrangement    ???
            % warnings/format_call_multi
            % warnings/format_call_warning
            % warnings/inst_with_no_type
            % warnings/unused_interface_import
            %
            % Several of these test cases include submodules, but not all.
            trace [
                compile_time(flag("make_dep_compare")),
                run_time(env("MAKE_DEP_COMPARE")),
                io(!TIO)
            ] (
                Msg = "\neqv_state(" ++ Id ++ ")\n" ++
                    string.join_list("\n", cord.list(!.Msgs)) ++ "\n",
                unexpected($pred, Msg)
            ),

            Succeeded = SucceededB,
            Deps = DepsB,
            Info = InfoB
        )
    ).

:- pred report_list_mismatch(list(T)::in, list(T)::in,
    cord(string)::in, cord(string)::out) is det.

report_list_mismatch(ListA, ListB, !Msgs) :-
    cord.snoc("version A:", !Msgs),
    list.foldl(report_list_element, ListA, !Msgs),
    cord.snoc("version B:", !Msgs),
    list.foldl(report_list_element, ListB, !Msgs).

:- pred report_assoc_list_mismatch(assoc_list(K, V)::in, assoc_list(K, V)::in,
    cord(string)::in, cord(string)::out) is det.

report_assoc_list_mismatch(AssocListA, AssocListB, !Msgs) :-
    map_to_diffs(AssocListA, AssocListB, KeysBoth,
        PairListOnlyA, PairListOnlyB),
    cord.snoc("common keys:", !Msgs),
    list.foldl(report_list_element, KeysBoth, !Msgs),
    cord.snoc("version A only:", !Msgs),
    list.foldl(report_assoc_list_element, PairListOnlyA, !Msgs),
    cord.snoc("version B only:", !Msgs),
    list.foldl(report_assoc_list_element, PairListOnlyB, !Msgs).

:- pred report_list_element(T::in,
    cord(string)::in, cord(string)::out) is det.

report_list_element(X, !Msgs) :-
    cord.snoc(string.string(X), !Msgs).

:- pred report_assoc_list_element(pair(K, V)::in,
    cord(string)::in, cord(string)::out) is det.

report_assoc_list_element(K - V, !Msgs) :-
    cord.snoc(string.string(K), !Msgs),
    cord.snoc("    " ++ string.string(V), !Msgs).

:- pred map_to_diffs(assoc_list(K, V)::in, assoc_list(K, V)::in,
    list(K)::out, assoc_list(K, V)::out, assoc_list(K, V)::out) is det.

map_to_diffs(AssocListA, AssocListB, KeysBoth, PairListOnlyA, PairListOnlyB) :-
    PairSetA = set.from_sorted_list(AssocListA),
    PairSetB = set.from_sorted_list(AssocListB),
    set.intersect(PairSetA, PairSetB, PairSetBoth),
    set.to_sorted_list(PairSetBoth, KeySetBoth),
    assoc_list.keys(KeySetBoth, KeysBoth),
    set.difference(PairSetA, PairSetBoth, PairSetOnlyA),
    set.difference(PairSetB, PairSetBoth, PairSetOnlyB),
    set.to_sorted_list(PairSetOnlyA, PairListOnlyA),
    set.to_sorted_list(PairSetOnlyB, PairListOnlyB).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% OLD DEPENDENCY SPECIFICATION MACHINERY
%

    % find_module_deps(Globals, ModuleIndex, Succeeded, Deps, !Info, !IO).
    %
    % The reason we don't return maybe(Deps) is that with `--keep-going'
    % we want to do as much work as possible.
    %
:- type find_module_deps(T) ==
    pred(globals, module_index, maybe_succeeded, deps_set(T),
        make_info, make_info, io, io).
:- inst find_module_deps ==
    (pred(in, in, out, out, in, out, di, uo) is det).

:- type find_module_deps_plain_set(T) ==
    pred(globals, module_index, maybe_succeeded, set(T),
        make_info, make_info, io, io).
:- inst find_module_deps_plain_set ==
    (pred(in, in, out, out, in, out, di, uo) is det).

%---------------------------------------------------------------------------%

:- pred no_deps(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

no_deps(_, _, succeeded, deps_set_init, !Info, !IO).

:- pred self(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

self(_Globals, ModuleIndex, succeeded, make_singleton_set(ModuleIndex),
    !Info, !IO).

:- pred ancestors(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

ancestors(_Globals, ModuleIndex, succeeded, AncestorIndices, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    Ancestors = get_ancestors(ModuleName),
    module_names_to_index_set(Ancestors, AncestorIndices, !Info).

:- pred add_targets_of_ancestors_as_deps(module_target_type::in,
    module_index::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

add_targets_of_ancestors_as_deps(TargetType, ModuleIndex, !Deps, !Info) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    module_names_to_index_set(get_ancestors(ModuleName),
        AncestorModuleIndexSet, !Info),
    add_targets_of_modules_as_deps(TargetType,
        to_sorted_list(AncestorModuleIndexSet), !Deps, !Info).

%---------------------------------------------------------------------------%

    % cache_computed_module_deps(Label, FindDeps) adds caching to FindDeps.
    % Label is used to discriminate cache entries for the same module;
    % it must uniquely identify the set that is computed by FindDeps.
    %
:- pred cache_computed_module_deps(computed_module_deps_label::in,
    find_module_deps(dependency_file_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

cache_computed_module_deps(Label, FindDeps, Globals, ModuleIndex, Succeeded,
        Deps, !Info, !IO) :-
    Cache0 = make_info_get_cached_computed_module_deps(!.Info),
    Key = computed_module_deps_key(ModuleIndex, Label),
    ( if map.search(Cache0, Key, CachedResult) then
        CachedResult = deps_result(Succeeded, Deps)
    else
        FindDeps(Globals, ModuleIndex, Succeeded, Deps, !Info, !IO),
        Cache1 = make_info_get_cached_computed_module_deps(!.Info),
        Result = deps_result(Succeeded, Deps),
        map.det_insert(Key, Result, Cache1, Cache),
        make_info_set_cached_computed_module_deps(Cache, !Info)
    ).

%---------------------------------------------------------------------------%

    % Note that we go to some effort in this module to stop dependency
    % calculation as soon as possible if there are errors.
    % This is important, because the calls to get_module_dependencies from
    % the dependency calculation predicates can result in every module in
    % the program being read.
    %
:- func combine_deps(
    find_module_deps(T)::in(find_module_deps),
    find_module_deps(T)::in(find_module_deps)) =
    (find_module_deps(T)::out(find_module_deps)) is det.

combine_deps(FindDeps1, FindDeps2) =
    combine_deps_2(FindDeps1, FindDeps2).

:- pred combine_deps_2(
    find_module_deps(T)::in(find_module_deps),
    find_module_deps(T)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out, deps_set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

combine_deps_2(FindDeps1, FindDeps2, Globals, ModuleIndex, Succeeded, Deps,
        !Info, !IO) :-
    FindDeps1(Globals, ModuleIndex, Succeeded1, Deps1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        make_info_get_keep_going(!.Info) = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Deps = Deps1
    else
        FindDeps2(Globals, ModuleIndex, Succeeded2, Deps2, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2,
        deps_set_union(Deps1, Deps2, Deps)
    ).

:- func combine_deps_list(list(
    find_module_deps(T))::in(list_skel(find_module_deps))) =
    (find_module_deps(T)::out(find_module_deps)) is det.

combine_deps_list([]) = no_deps.
combine_deps_list([FindDeps]) = FindDeps.
combine_deps_list([FindDeps1, FindDeps2 | FindDepsTail]) =
    combine_deps(FindDeps1, combine_deps_list([FindDeps2 | FindDepsTail])).

%---------------------------------------------------------------------------%

    % TargetType `of` F is function that returns the set of TargetType targets
    % based on the modules generated by F.
    %
    % e.g. module_target_int0 `of` ancestors takes a module and returns the
    % set of .int0 targets for the module's ancestor modules.
    %
:- func of(module_target_type, find_module_deps(module_index)) =
    find_module_deps(dependency_file_index).
:- mode of(in, in(find_module_deps)) = out(find_module_deps) is det.

of(FileType, FindDeps) =
    of_2(FileType, FindDeps).

:- pred of_2(module_target_type::in,
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

of_2(TargetType, FindDeps, Globals, ModuleIndex, Succeeded,
        DepFileIndexSet, !Info, !IO) :-
    FindDeps(Globals, ModuleIndex, Succeeded, ModuleIndexes, !Info, !IO),
    deps_set_foldl2(acc_rev_dfmi_target(TargetType), ModuleIndexes,
        deps_set_init, DepFileIndexSet, !Info).

:- pred acc_rev_dfmi_target(module_target_type::in, module_index::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

acc_rev_dfmi_target(TargetType, ModuleIndex, !DepFileIndexSet, !Info) :-
    TargetFile = dfmi_target(ModuleIndex, TargetType),
    dependency_file_to_index(TargetFile, TargetFileIndex, !Info),
    deps_set_insert(TargetFileIndex, !DepFileIndexSet).

%---------------------------------------------------------------------------%

:- func files_of(find_module_deps_plain_set(dependency_file),
    find_module_deps(module_index)) = find_module_deps(dependency_file_index).
:- mode files_of(in(find_module_deps_plain_set), in(find_module_deps))
    = out(find_module_deps) is det.

files_of(FindFiles, FindDeps) =
    files_of_2(FindFiles, FindDeps).

:- pred files_of_2(
    find_module_deps_plain_set(dependency_file)::
        in(find_module_deps_plain_set),
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

files_of_2(FindFiles, FindDeps, Globals, ModuleIndex, Succeeded, DepIndices,
        !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    FindDeps(Globals, ModuleIndex, Succeeded1, ModuleIndices, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        DepIndices = deps_set_init
    else
        deps_set_foldl3_maybe_stop_at_error_find_plain_union_mi(KeepGoing,
            FindFiles, Globals, to_sorted_list(ModuleIndices),
            succeeded, Succeeded2, set.init, FileNames, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2,
        dependency_files_to_index_set(set.to_sorted_list(FileNames),
            DepIndices, !Info)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% XXX Document me.
%

:- pred deps_set_foldl3_maybe_stop_at_error_find_union_mi(
    maybe_keep_going::in,
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

deps_set_foldl3_maybe_stop_at_error_find_union_mi(_KeepGoing,
        _FindDeps, _Globals, [], !Succeeded, !Deps, !Info, !IO).
deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
        FindDeps, Globals, [MI | MIs], !Succeeded, !Deps, !Info, !IO) :-
    FindDeps(Globals, MI, NewSucceeded, NewDeps, !Info, !IO),
    deps_set_union(NewDeps, !Deps),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        deps_set_foldl3_maybe_stop_at_error_find_union_mi(KeepGoing,
            FindDeps, Globals, MIs, !Succeeded, !Deps, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------%

:- pred deps_set_foldl3_maybe_stop_at_error_find_plain_union_mi(
    maybe_keep_going::in,
    find_module_deps_plain_set(dependency_file)::
        in(find_module_deps_plain_set),
    globals::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    set(dependency_file)::in, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

deps_set_foldl3_maybe_stop_at_error_find_plain_union_mi(_KeepGoing,
        _FindDeps, _Globals, [], !Succeeded, !Deps, !Info, !IO).
deps_set_foldl3_maybe_stop_at_error_find_plain_union_mi(KeepGoing,
        FindDeps, Globals, [MI | MIs], !Succeeded, !Deps, !Info, !IO) :-
    FindDeps(Globals, MI, NewSucceeded, NewDeps, !Info, !IO),
    set.union(NewDeps, !Deps),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        deps_set_foldl3_maybe_stop_at_error_find_plain_union_mi(KeepGoing,
            FindDeps, Globals, MIs, !Succeeded, !Deps, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------%

    % XXX Document me.
    %
    % The difference between this predicate (and its local siblings) and
    % the old deps_set_foldl3_maybe_stop_at_error (now replaced by these
    % predicates) is that the second argument has a more specific job.
    % That job used to be done by a predicate, union_deps, whose documentation
    % used to say this:
    %
    % "Union the output set of dependencies for a given module
    % with the accumulated set. This is used with
    % deps_set_foldl3_maybe_stop_at_error to iterate over a list of
    % module_names to find all target files for those modules."
    %
:- pred deps_set_foldl3_maybe_stop_at_error_find_union_fi(maybe_keep_going::in,
    find_module_deps(dependency_file_index)::in(find_module_deps),
    globals::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

deps_set_foldl3_maybe_stop_at_error_find_union_fi(_KeepGoing,
        _FindDeps, _Globals, [], !Succeeded, !Deps, !Info, !IO).
deps_set_foldl3_maybe_stop_at_error_find_union_fi(KeepGoing,
        FindDeps, Globals, [MI | MIs], !Succeeded, !Deps, !Info, !IO) :-
    FindDeps(Globals, MI, NewSucceeded, NewDeps, !Info, !IO),
    deps_set_union(NewDeps, !Deps),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        deps_set_foldl3_maybe_stop_at_error_find_union_fi(KeepGoing,
            FindDeps, Globals, MIs, !Succeeded, !Deps, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

:- pred fold_dep_spec_over_modules_stop_at_error_fi(maybe_keep_going::in,
    globals::in, dep_spec::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fold_dep_spec_over_modules_stop_at_error_fi(_, _,
        _, [], !Succeeded, !DepFileIndexSet, !Info, !IO).
fold_dep_spec_over_modules_stop_at_error_fi(KeepGoing, Globals,
        DepSpec, [MI | MIs], !Succeeded, !DepFileIndexSet, !Info, !IO) :-
    find_dep_spec(Globals, MI, DepSpec, HeadSucceeded, HeadDepFileIndexSet,
        !Info, !IO),
    deps_set_union(HeadDepFileIndexSet, !DepFileIndexSet),
    ( if
        ( HeadSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` HeadSucceeded,
        fold_dep_spec_over_modules_stop_at_error_fi(KeepGoing, Globals,
            DepSpec, MIs, !Succeeded, !DepFileIndexSet, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func dependency_file_to_debug_string(string, string, dependency_file)
    = string.

dependency_file_to_debug_string(Prefix, Suffix, DepFile) = Str :-
    (
        DepFile = dep_target(TargetFile),
        TargetFile = target_file(ModuleName, TargetType),
        ModuleNameStr = sym_name_to_string(ModuleName),
        TargetTypeStr = string.string(TargetType),
        string.format("dep_target %s of %s",
            [s(TargetTypeStr), s(ModuleNameStr)], Str0)
    ;
        DepFile = dep_file(FileName),
        string.format("dep_file %s", [s(FileName)], Str0)
    ),
    Str = Prefix ++ Str0 ++ Suffix.

%---------------------------------------------------------------------------%
:- end_module make.dependencies.
%---------------------------------------------------------------------------%
