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
% Original author: stayl.
% Author of current version: zs.
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
:- pred find_target_dependencies_of_modules(io.text_output_stream::in,
    maybe_keep_going::in, globals::in,
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

:- import_module bool.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.

%---------------------------------------------------------------------------%

find_target_dependencies_of_modules(_, _, _, _,
        [], !Succeeded, !Deps, !Info, !IO).
find_target_dependencies_of_modules(ProgressStream, KeepGoing, Globals,
        TargetType, [ModuleIndex | ModuleIndexes],
        !Succeeded, !Deps, !Info, !IO) :-
    find_target_dependencies_of_module(ProgressStream, KeepGoing, Globals,
        TargetType, ModuleIndex, NewSucceeded, !Deps, !Info, !IO),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        find_target_dependencies_of_modules(ProgressStream, KeepGoing, Globals,
            TargetType, ModuleIndexes, !Succeeded, !Deps, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

:- pred find_target_dependencies_of_module(io.text_output_stream::in,
    maybe_keep_going::in, globals::in,
    module_target_type::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_target_dependencies_of_module(ProgressStream, KeepGoing, Globals,
        TargetType, ModuleIndex, NewSucceeded, !Deps, !Info, !IO) :-
    (
        ( TargetType = module_target_source
        ; TargetType = module_target_track_flags
        ),
        NewSucceeded = succeeded
    ;
        TargetType = module_target_int3,
        DepSpecs = [self(module_target_source)],
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        ( TargetType = module_target_int0
        ; TargetType = module_target_int1
        ; TargetType = module_target_int2
        ),
        DepSpecs = [
            self(module_target_source),
            ancestors(module_target_int0),
            direct_imports_intermod(module_target_int3),
            indirect_imports_intermod(module_target_int3)
        ],
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        ( TargetType = module_target_c_code
        ; TargetType = module_target_c_header(_)
        ; TargetType = module_target_csharp_code
        ; TargetType = module_target_java_code
        ; TargetType = module_target_errors
        ),
        compiled_code_dependencies(Globals, DepSpecs),
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        TargetType = module_target_java_class_code,
        DepSpec = self(module_target_java_code),
        find_dep_spec(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpec, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        ( TargetType = module_target_foreign_object(PIC, _)
        ; TargetType = module_target_fact_table_object(PIC, _)
        ),
        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        DepSpec = self(TargetCodeType),
        find_dep_spec(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpec, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        TargetType = module_target_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

        MhHeader = module_target_c_header(header_mh),
        MihHeader = module_target_c_header(header_mih),
        DepSpecSelf = self(TargetCodeType),
        DepSpecMh = foreign_imports_intermod_trans(MhHeader),
        ( if
            CompilationTarget = target_c,
            HighLevelCode = yes
        then
            DepSpecs = [DepSpecSelf, DepSpecMh,
                direct_imports_intermod(MihHeader),
                indirect_imports_intermod(MihHeader),
                ancestors(MihHeader),
                intermod_imports(MihHeader)
            ]
        else
            DepSpecs = [DepSpecSelf, DepSpecMh]
        ),
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        ( TargetType = module_target_opt
        ; TargetType = module_target_xml_doc
        ),
        DepSpecs = [
            self(module_target_source),
            % XXX Should we cache the remaining dep_specs as a whole?
            ancestors(module_target_int0),
            direct_imports_non_intermod(module_target_int1),
            indirect_imports_non_intermod(module_target_int2)
        ],
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ;
        TargetType = module_target_analysis_registry,
        DepSpecs = [
            self(module_target_source),
            ancestors(module_target_int0),
            direct_imports_non_intermod(module_target_int1),
            indirect_imports_non_intermod(module_target_int2),
            direct_imports_intermod(module_target_opt),
            indirect_imports_intermod(module_target_opt),
            intermod_imports(module_target_opt)
        ],
        find_dep_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DepSpecs, NewSucceeded, NewDeps, !Info, !IO),
        deps_set_union(NewDeps, !Deps)
    ).

:- func target_to_module_target_code(compilation_target, pic)
    = module_target_type.

target_to_module_target_code(_CompilationTarget, _PIC) = TargetCode :-
    % XXX it looks wrong to be returning module_target_c_code for
    % all compilation targets.
    TargetCode = module_target_c_code.

:- pred compiled_code_dependencies(globals::in, list(dep_spec)::out) is det.

compiled_code_dependencies(Globals, DepSpecs) :-
    % We build up Deps in stages.

    % Stage 0: dependencies on flags.
    globals.lookup_bool_option(Globals, track_flags, TrackFlags),
    (
        TrackFlags = yes,
        DepSpecsTracks = [self(module_target_track_flags)]
    ;
        TrackFlags = no,
        DepSpecsTracks = []
    ),

    % Stage 1: dependencies on the source file, and on the fact table files,
    % foreign language files and Mercury interface files it imports.
    DepSpecsSrcInts = [
        self(module_target_source),
        self_fact_table_files,
        self_foreign_include_files,
        self(module_target_int1),
        % XXX MDNEW The next two dep_specs should be a single
        % combined dep_spec, anc01_dir1_indir2.
        ancestors(module_target_int1),
        anc0_dir1_indir2
    ],

    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    AnyIntermod = bool.or(IntermodOpt, IntermodAnalysis),

    % Stage 2: dependencies on optimization files.
    (
        AnyIntermod = yes,
        DepSpecsOpts = [
            self(module_target_opt),
            % XXX MDNEW Given that we compute the set of intermod imports
            % for this dep_spec ...
            intermod_imports(module_target_opt),
            % ... why do we have to compute it AGAIN, as part of this
            % dep_spec as well?
            %
            % We should replace both of these dep_specs with one that
            % does the job of both but computes the set of intermod_imports
            % modules set just once. This would save even the cost of a
            % cache hit.
            anc0_dir1_indir2_of_ancestors_of_intermod_imports
        ]
    ;
        AnyIntermod = no,
        DepSpecsOpts = []
    ),

    % Stage 3: dependencies on analysis result files.
    (
        IntermodAnalysis = yes,
        DepSpecsRegistries = [
            self(module_target_analysis_registry),
            direct_imports_intermod(module_target_analysis_registry)
        ]
    ;
        IntermodAnalysis = no,
        DepSpecsRegistries = []
    ),

    DepSpecs = DepSpecsTracks ++ DepSpecsSrcInts ++
        DepSpecsOpts ++ DepSpecsRegistries.

%---------------------------------------------------------------------------%

    % The dependency specification type.
    %
    % Values of this type indirectly represent the specification of
    % a set of dependency_files (actually, dependency_file_indexes).
    % The "indirect" part is there because they actually represent
    % a specification of a task for find_dep_spec, which will compute
    % that set of dependency file indexes when given a dep_spec.
    %
:- type dep_spec
    --->    self(module_target_type)
    ;       ancestors(module_target_type)
    ;       direct_imports_non_intermod(module_target_type)
    ;       direct_imports_intermod(module_target_type)
    ;       indirect_imports_non_intermod(module_target_type)
    ;       indirect_imports_intermod(module_target_type)
    ;       intermod_imports(module_target_type)
    ;       foreign_imports_intermod_trans(module_target_type)

    ;       anc0_dir1_indir2
            % Get the .int0 files of ancestors, the .int files of direct
            % imports, and the .int2 files of indirect imports.

    ;       anc0_dir1_indir2_of_ancestors_of_intermod_imports
            % Get the .int0 files of ancestors, the .int files of direct
            % imports, and the .int2 files of indirect imports, but not
            % of the specified module, but of the ancestors of its intermod
            % imports.

    ;       self_fact_table_files
    ;       self_foreign_include_files.

:- pred find_dep_specs(io.text_output_stream::in, maybe_keep_going::in,
    globals::in, module_index::in, list(dep_spec)::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_dep_specs(_, _, _, _, [], succeeded, deps_set_init, !Info, !IO).
find_dep_specs(ProgressStream, KeepGoing, Globals, ModuleIndex,
        [HeadDepSpec | TailDepSpecs], Succeeded, DepFileIndexSet,
        !Info, !IO) :-
    find_dep_spec(ProgressStream, KeepGoing, Globals, ModuleIndex, HeadDepSpec,
        HeadSucceeded, HeadDepFileIndexSet, !Info, !IO),
    ( if
        ( HeadSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        find_dep_specs(ProgressStream, KeepGoing, Globals, ModuleIndex,
            TailDepSpecs, TailSucceeded, TailDepFileIndexSet, !Info, !IO),
        Succeeded = HeadSucceeded `and` TailSucceeded,
        deps_set_union(HeadDepFileIndexSet, TailDepFileIndexSet,
            DepFileIndexSet)
    else
        Succeeded = did_not_succeed,
        DepFileIndexSet = HeadDepFileIndexSet
    ).

:- pred find_dep_spec(io.text_output_stream::in, maybe_keep_going::in,
    globals::in, module_index::in, dep_spec::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_dep_spec(ProgressStream, KeepGoing, Globals, ModuleIndex, DepSpec,
        Succeeded, DepFileIndexSet, !Info, !IO) :-
    trace [
        compile_time(flag("find_dep_spec")),
        run_time(env("FIND_DEP_SPEC")),
        io(!TIO)
    ] (
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        io.format(ProgressStream, "starting dep_spec %s for %s\n",
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
        DepSpec = ancestors(TargetType),
        Succeeded = succeeded,
        module_index_to_name(!.Info, ModuleIndex, ModuleName),
        Ancestors = get_ancestors(ModuleName),
        module_names_to_index_set(Ancestors, ModuleIndexSet, !Info),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = direct_imports_non_intermod(TargetType),
        get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = direct_imports_intermod(TargetType),
        get_direct_imports_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = indirect_imports_non_intermod(TargetType),
        get_indirect_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = indirect_imports_intermod(TargetType),
        get_indirect_imports_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = intermod_imports(TargetType),
        get_intermod_imports(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = foreign_imports_intermod_trans(TargetType),
        get_foreign_imports_intermod_trans(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        DepSpec = anc0_dir1_indir2,
        SubDepSpecs = [
            ancestors(module_target_int0),
            direct_imports_intermod(module_target_int1),
            indirect_imports_intermod(module_target_int2)
        ],
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "dep_spec %s for %s starts\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ),

        ( if search_anc0_dir1_indir2_cache(!.Info, ModuleIndex, Result0) then
            Result0 = deps_result(Succeeded, DepFileIndexSet)
        else
            find_dep_specs(ProgressStream, KeepGoing, Globals,
                ModuleIndex, SubDepSpecs, Succeeded, DepFileIndexSet,
                !Info, !IO),
            Result = deps_result(Succeeded, DepFileIndexSet),
            add_to_anc0_dir1_indir2_cache(ModuleIndex, Result, !Info)
        ),

        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "dep_spec %s for %s ends\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        DepSpec = anc0_dir1_indir2_of_ancestors_of_intermod_imports,
        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "dep_spec %s for %s starts\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ),

        get_anc0_dir1_indir2_of_ancestors_of_intermod_imports(ProgressStream,
            KeepGoing, Globals, ModuleIndex, Succeeded, DepFileIndexSet,
            !Info, !IO),

        trace [
            compile_time(flag("find_dep_spec")),
            run_time(env("FIND_DEP_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "dep_spec %s for %s ends\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        DepSpec = self_fact_table_files,
        get_fact_table_files(ProgressStream, Globals, ModuleIndex,
            Succeeded, FactTableDepFiles, !Info, !IO),
        % XXX MDNEW Push this call into fact_table_files
        % when the old machinery is deleted.
        dependency_files_to_index_set(set.to_sorted_list(FactTableDepFiles),
            DepFileIndexSet, !Info)
    ;
        DepSpec = self_foreign_include_files,
        get_foreign_include_files(ProgressStream, Globals, ModuleIndex,
            Succeeded, ForeignDepFiles, !Info, !IO),
        % XXX MDNEW Push this call into foreign_include_files
        % when the old machinery is deleted.
        dependency_files_to_index_set(set.to_sorted_list(ForeignDepFiles),
            DepFileIndexSet, !Info)
    ),
    trace [
        compile_time(flag("find_dep_spec")),
        run_time(env("FIND_DEP_SPEC")),
        io(!TIO)
    ] (
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        dependency_file_index_set_to_plain_set(!.Info, DepFileIndexSet,
            DepFileSet),
        DepFiles = set.to_sorted_list(DepFileSet),
        (
            DepFiles = [],
            io.format(ProgressStream, "dep_spec %s for %s yields no deps\n\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO)
        ;
            DepFiles = [_ | _],
            DepFileNlStrs = list.map(
                dependency_file_to_debug_string("    ", "\n"), DepFiles),
            io.format(ProgressStream, "dep_spec %s for %s yields these deps:\n",
                [s(string.string(DepSpec)), s(IndexModuleNameStr)], !TIO),
            list.foldl(io.write_string(ProgressStream), DepFileNlStrs, !TIO),
            io.write_string(ProgressStream, "dep list ends\n\n", !TIO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred dfmi_targets(deps_set(module_index)::in, module_target_type::in,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info) :-
    deps_set_foldl2(acc_rev_dfmi_target(TargetType), ModuleIndexSet,
        deps_set_init, DepFileIndexSet, !Info).

:- pred acc_rev_dfmi_target(module_target_type::in, module_index::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

acc_rev_dfmi_target(TargetType, ModuleIndex, !DepFileIndexSet, !Info) :-
    TargetFile = dfmi_target(ModuleIndex, TargetType),
    dependency_file_to_index(TargetFile, TargetFileIndex, !Info),
    deps_set_insert(TargetFileIndex, !DepFileIndexSet).

%---------------------------------------------------------------------------%

    % Return the modules for which `.int' files are read in a compilation
    % which does not use `--intermodule-optimization'.
    %
:- pred get_direct_imports_non_intermod(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    ( if
        search_direct_imports_non_intermod_cache(!.Info, ModuleIndex, Result0)
    then
        Result0 = deps_result(Succeeded, Modules)
    else
        get_direct_imports_non_intermod_uncached(ProgressStream, KeepGoing,
            Globals, ModuleIndex, Succeeded, Modules, !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        add_to_direct_imports_non_intermod_cache(ModuleIndex, Result, !Info)
    ).

:- pred get_direct_imports_non_intermod_uncached(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_direct_imports_non_intermod_uncached(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        % Find the direct imports of this module, i.e. the modules
        % for which we will read the `.int' files.
        %
        % Note that we need to do this both for the imports of this module,
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
            get_direct_imports_non_intermod(ProgressStream, KeepGoing,
                Globals, ParentIndex, Succeeded, ParentImports, !Info, !IO),
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

:- pred get_direct_imports_intermod(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_direct_imports_intermod(ProgressStream, KeepGoing, Globals, ModuleIndex,
        Succeeded, Modules, !Info, !IO) :-
    ( if
        search_direct_imports_intermod_cache(!.Info, ModuleIndex, Result0)
    then
        Result0 = deps_result(Succeeded, Modules)
    else
        get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded0, Modules0, !Info, !IO),
        ( if
            Succeeded0 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed,
            Modules = deps_set_init
        else
            % We also read `.int' files for the modules for which we read
            % `.opt' files, and for the modules imported by those modules.
            get_intermod_imports(ProgressStream, KeepGoing, Globals,
                ModuleIndex, Succeeded1, IntermodModules, !Info, !IO),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed,
                Modules = deps_set_init
            else
                deps_set_union(IntermodModules, Modules0, Modules1),
                fold_find_modules_over_modules(ProgressStream, KeepGoing,
                    Globals, get_direct_imports_non_intermod,
                    deps_set_to_sorted_list(IntermodModules),
                    succeeded, Succeeded2, Modules1, Modules2, !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2,
                deps_set_delete(ModuleIndex, Modules2, Modules)
            )
        ),
        Result = deps_result(Succeeded, Modules),
        add_to_direct_imports_intermod_cache(ModuleIndex, Result, !Info)
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.int2' files,
    % ignoring those which need to be read as a result of importing modules
    % imported by a `.opt' file.
    %
:- pred get_indirect_imports_non_intermod(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_indirect_imports_non_intermod(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, IndirectNonIntermodImportModules,
        !Info, !IO) :-
    get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
        ModuleIndex, DirectSucceeded, DirectImportModules, !Info, !IO),
    get_indirect_imports_uncached(ProgressStream, KeepGoing, Globals,
        ModuleIndex, DirectSucceeded, DirectImportModules,
        Succeeded, IndirectNonIntermodImportModules, !Info, !IO).

    % Return the list of modules for which we should read `.int2' files.
    %
:- pred get_indirect_imports_intermod(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_indirect_imports_intermod(ProgressStream, KeepGoing, Globals, ModuleIndex,
        Succeeded, IndirectIntermodImportModules, !Info, !IO) :-
    ( if
        search_indirect_imports_intermod_cache(!.Info, ModuleIndex, Result0)
    then
        Result0 = deps_result(Succeeded, IndirectIntermodImportModules)
    else
        get_direct_imports_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DirectSucceeded, DirectImportModules, !Info, !IO),
        get_indirect_imports_uncached(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DirectSucceeded, DirectImportModules,
            Succeeded, IndirectIntermodImportModules, !Info, !IO),
        Result = deps_result(Succeeded, IndirectIntermodImportModules),
        add_to_indirect_imports_intermod_cache(ModuleIndex, Result, !Info)
    ).

:- pred get_indirect_imports_uncached(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::in, deps_set(module_index)::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_indirect_imports_uncached(ProgressStream, KeepGoing, Globals, ModuleIndex,
        DirectSucceeded, DirectImports, Succeeded, IndirectImports,
        !Info, !IO) :-
    ( if
        DirectSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        IndirectImports = deps_set_init
    else
        fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals,
            find_transitive_implementation_imports,
            deps_set_to_sorted_list(DirectImports),
            succeeded, IndirectSucceeded, deps_set_init, IndirectImports0,
            !Info, !IO),
        deps_set_delete(ModuleIndex, IndirectImports0, IndirectImports1),
        IndirectImports = deps_set_difference(IndirectImports1, DirectImports),
        Succeeded = DirectSucceeded `and` IndirectSucceeded
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.opt' files.
    %
:- pred get_intermod_imports(io.text_output_stream::in, maybe_keep_going::in,
    globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_intermod_imports(ProgressStream, KeepGoing, Globals, ModuleIndex,
        Succeeded, Modules, !Info, !IO) :-
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        globals.lookup_bool_option(Globals, read_opt_files_transitively,
            Transitive),
        (
            Transitive = yes,
            find_transitive_implementation_imports(ProgressStream, KeepGoing,
                Globals, ModuleIndex, Succeeded, Modules, !Info, !IO)
        ;
            Transitive = no,
            get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
                ModuleIndex, Succeeded, Modules, !Info, !IO)
        )
    ;
        AnyIntermod = no,
        Succeeded = succeeded,
        Modules = deps_set_init
    ).

%---------------------------------------------------------------------------%

:- pred get_foreign_imports_intermod_trans(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_imports_intermod_trans(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    % The object file depends on the header files for the modules
    % mentioned in `:- pragma foreign_import_module' declarations
    % in the current module and the `.opt' files it imports.
    globals.get_backend_foreign_languages(Globals, Languages),
    LanguagesSet = set.list_to_set(Languages),
    get_intermod_imports(ProgressStream, KeepGoing, Globals, ModuleIndex,
        IntermodSucceeded, IntermodModules, !Info, !IO),
    deps_set_insert(ModuleIndex, IntermodModules, IntermodSelfModules),
    fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals,
        get_foreign_imports_non_intermod_trans(LanguagesSet),
        to_sorted_list(IntermodSelfModules),
        succeeded, ForeignSucceeded, deps_set_init, Modules, !Info, !IO),
    Succeeded = IntermodSucceeded `and` ForeignSucceeded.

:- pred get_foreign_imports_non_intermod_trans(set(foreign_language)::in,
    io.text_output_stream::in, maybe_keep_going::in, globals::in,
    module_index::in, maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_imports_non_intermod_trans(LangSet, ProgressStream, KeepGoing,
        Globals, ModuleIndex, Succeeded, ForeignModules, !Info, !IO) :-
    % LangSet should be constant for the duration of the process,
    % which means that it is unnecessary to include it in the cache key.
    ( if
        search_foreign_imports_non_intermod_trans_cache(!.Info, ModuleIndex,
            Result0)
    then
        Result0 = deps_result(Succeeded, ForeignModules)
    else
        find_transitive_implementation_imports(ProgressStream, KeepGoing,
            Globals, ModuleIndex, Succeeded0, ImportedModules, !Info, !IO),
        (
            Succeeded0 = succeeded,
            fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals,
                get_foreign_imports_non_intermod_uncached(LangSet),
                to_sorted_list(insert(ImportedModules, ModuleIndex)),
                succeeded, Succeeded, deps_set_init, ForeignModules,
                !Info, !IO),
            Result = deps_result(Succeeded, ForeignModules),
            add_to_foreign_imports_non_intermod_trans_cache(ModuleIndex,
                Result, !Info)
        ;
            Succeeded0 = did_not_succeed,
            Succeeded = did_not_succeed,
            ForeignModules = deps_set_init
        )
    ).

:- pred get_foreign_imports_non_intermod_uncached(set(foreign_language)::in,
    io.text_output_stream::in, maybe_keep_going::in, globals::in,
    module_index::in, maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_imports_non_intermod_uncached(LangSet, ProgressStream, _KeepGoing,
        Globals, ModuleIndex, Succeeded, ForeignModules, !Info, !IO) :-
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
                set.contains(LangSet, Lang)
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

:- pred get_anc0_dir1_indir2_of_ancestors_of_intermod_imports(
    io.text_output_stream::in, maybe_keep_going::in, globals::in,
    module_index::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_anc0_dir1_indir2_of_ancestors_of_intermod_imports(ProgressStream,
        KeepGoing, Globals, ModuleIndex, Succeeded, DepFileIndexSet,
        !Info, !IO) :-
    get_ancestors_of_intermod_imports(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        DepFileIndexSet = deps_set_init
    else
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        fold_dep_spec_over_modules(ProgressStream, KeepGoing, Globals,
            anc0_dir1_indir2, ModuleList1,
            succeeded, Succeeded2, deps_set_init, DepFileIndexSet, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

:- pred get_ancestors_of_intermod_imports(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_ancestors_of_intermod_imports(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO) :-
    get_intermod_imports(ProgressStream, KeepGoing, Globals, ModuleIndex,
        Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        ModuleIndexSet = deps_set_init
    else
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        list.map_foldl(index_get_ancestors,
            ModuleList1, AncestorModuleIndexSets, !Info),
        ModuleIndexSet = deps_set_union_list(AncestorModuleIndexSets),
        Succeeded = Succeeded1
    ).

:- pred index_get_ancestors(module_index::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

index_get_ancestors(ModuleIndex, AncestorModuleIndexSet, !Info) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    AncestorModuleNames = get_ancestors(ModuleName),
    module_names_to_index_set(AncestorModuleNames, AncestorModuleIndexSet,
        !Info).

%---------------------------------------------------------------------------%

:- pred get_fact_table_files(io.text_output_stream::in, globals::in,
    module_index::in, maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_fact_table_files(ProgressStream, Globals, ModuleIndex,
        Succeeded, Files, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
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

:- pred get_foreign_include_files(io.text_output_stream::in, globals::in,
    module_index::in, maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_include_files(ProgressStream, Globals, ModuleIndex,
        Succeeded, DepFiles, !Info, !IO) :-
    globals.get_backend_foreign_languages(Globals, Languages),
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_source_file_name(ModuleDepInfo, SourceFileName),
        module_dep_info_get_foreign_include_files(ModuleDepInfo,
            ForeignIncludeFiles),
        LangSet = set.list_to_set(Languages),
        dep_files_for_foreign_includes_if_in_langset(LangSet, SourceFileName,
            ForeignIncludeFiles, DepFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        DepFiles = set.init
    ).

:- pred dep_files_for_foreign_includes_if_in_langset(set(foreign_language)::in,
    file_name::in, set(foreign_include_file_info)::in,
    set(dependency_file)::out) is det.

dep_files_for_foreign_includes_if_in_langset(LangSet, SourceFileName,
        ForeignIncludes, DepFiles) :-
    set.filter_map(
        dep_file_for_foreign_include_if_in_langset(LangSet, SourceFileName),
        ForeignIncludes, DepFiles).

:- pred dep_file_for_foreign_include_if_in_langset(set(foreign_language)::in,
    file_name::in, foreign_include_file_info::in, dependency_file::out)
    is semidet.

dep_file_for_foreign_include_if_in_langset(LangSet, SourceFileName,
        ForeignInclude, DepFile) :-
    ForeignInclude = foreign_include_file_info(Lang, IncludeFileName),
    set.member(Lang, LangSet),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
    DepFile = dep_file(IncludePath).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred find_transitive_implementation_imports(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_implementation_imports(ProgressStream, _KeepGoing, Globals,
        ModuleIndex, Succeeded, Modules, !Info, !IO) :-
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
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        module_index_set_to_plain_set(!.Info, Modules, ModuleSet),
        ModuleList = set.to_sorted_list(ModuleSet),
        ModuleNlStrs = list.map(
            (func(M) = "    " ++ sym_name_to_string(M) ++ "\n"),
            ModuleList),
        io.format(ProgressStream, "trans impl imports for %s:\n",
            [s(IndexModuleNameStr)], !TIO),
        list.foldl(io.write_string(ProgressStream), ModuleNlStrs, !TIO),
        io.write_string(ProgressStream,
            "trans impl imports list ends\n\n", !TIO)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % find_module_deps(Globals, ModuleIndex, Succeeded, Deps, !Info, !IO).
    %
    % The reason we don't return maybe(Deps) is that with `--keep-going'
    % we want to do as much work as possible.
    %
:- type find_module_deps(T) ==
    pred(io.text_output_stream, maybe_keep_going, globals, module_index,
        maybe_succeeded, deps_set(T), make_info, make_info, io, io).
:- inst find_module_deps ==
    (pred(in, in, in, in, out, out, in, out, di, uo) is det).

    % fold_find_modules_over_modules(KeepGoing, Globals, FindDeps,
    %   ModuleIndexes, !Succeeded, !ModuleIndexSet, !Info, !IO):
    %
    % Invoke FindDeps on each element of ModuleIndexes, adding
    % the union of the returned module index sets to !ModuleIndexSet.
    %
    % Stop only if an invocation of FindDeps returns did_not_succeed
    % *and* KeepGoing is do_not_keep_going.
    %
:- pred fold_find_modules_over_modules(io.text_output_stream::in,
    maybe_keep_going::in, globals::in,
    find_module_deps(module_index)::in(find_module_deps),
    list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fold_find_modules_over_modules(_, _, _, _,
        [], !Succeeded, !ModuleIndexSet, !Info, !IO).
fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals, FindDeps,
        [MI | MIs], !Succeeded, !ModuleIndexSet, !Info, !IO) :-
    FindDeps(ProgressStream, KeepGoing, Globals, MI,
        HeadSucceeded, HeadModuleIndexSet, !Info, !IO),
    deps_set_union(HeadModuleIndexSet, !ModuleIndexSet),
    ( if
        ( HeadSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` HeadSucceeded,
        fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals,
            FindDeps, MIs, !Succeeded, !ModuleIndexSet, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------%

    % fold_dep_spec_over_modules(ProgressStream, KeepGoing, Globals, DepSpec,
    %   ModuleIndexes, !Succeeded, !DepFileIndexSet, !Info, !IO):
    %
    % Invoke find_dep_spec with DepSpec on each element of ModuleIndexes,
    % adding the union of the resulting dependency file index sets
    % to !DepFileIndexSet.
    %
    % Stop only if an invocation of find_dep_spec returns did_not_succeed
    % *and* KeepGoing is do_not_keep_going.
    %
:- pred fold_dep_spec_over_modules(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, dep_spec::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fold_dep_spec_over_modules(_, _, _, _,
        [], !Succeeded, !DepFileIndexSet, !Info, !IO).
fold_dep_spec_over_modules(ProgressStream, KeepGoing, Globals, DepSpec,
        [MI | MIs], !Succeeded, !DepFileIndexSet, !Info, !IO) :-
    find_dep_spec(ProgressStream, KeepGoing, Globals, MI, DepSpec,
        HeadSucceeded, HeadDepFileIndexSet, !Info, !IO),
    deps_set_union(HeadDepFileIndexSet, !DepFileIndexSet),
    ( if
        ( HeadSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` HeadSucceeded,
        fold_dep_spec_over_modules(ProgressStream, KeepGoing, Globals,
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
