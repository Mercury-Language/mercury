%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.dependencies.m.
% Original author: stayl.
% Author of current version: zs.
%
% This job of this module to find, for a given file T (a target file),
% which *other* files Si you need in order to build T. Effectively,
% given a file name T, it finds all the filename Si that occur on the
% right hand sides of the (implicit) make rules whose left hand side
% contains T:
%
%   T:        S1 S2 ...
%
% Note that we find only the *direct* prereqs of T. This means that
% we return e.g. S1, but not the files needed to build S1 itself.
% (Unless by some very unlikely chance, the files needed to build S1
% are also direct prereqs of T itself.)
%
% We should consider returning effectively a tree, whose nodes are file names,
% where the children of a node are the direct prereqs of the file represented
% by that node, but which have their own prereqs as their children, all the way
% down to the files that have *no* prerequisites. We could construct such
% a tree for every target named on the "mmc --make" command line, yielding
% effectively a forest.
%
% Having the full forest available before we start building the first target
% should allow us to increase parallelism by exploiting our knowledge of
% the structure of Mercury dependencies. We could follow a strategy of
% building files in a bottom up order, like a layercake:
%
% - building all needed .int3 files
% - building all needed .int0 files
% - building all needed .int and .int2 files
% - building all needed .opt files (if enabled)
% - building all needed .trans_opt files (if enabled)
% - building all needed .c/.java/.cs files, depending on the target language
% - building all needed .o/.jar/.dll files, depending on the target language
% - building all needed executables or their equivalents
%
% In most of these stages, once all the files of the previous stages have been
% built, it should be possible to build all files with the given suffix
% in that stage in parallel. The exceptions are the creation of .int0 files
% (where the creation of the .int0 files for e.g. module A and module A.B
% may not be independent), and the creation of .trans_opt files (which must
% respect the chosen order between those files.)
%
% In the absence of this global view, it is possible for e.g. an invocation
% of "mmc --make -j8 prog" the decide to build A.c, B.c, C.c, D.c, E.c, F.c
% and G.c at once, but if all of those modules import module H, then all
% seven build processes may need to stall waiting for the completion of
% the task that builds H.int. This cannot happen with the layercake approach.
%
% The one drawback of the layercake approach is that computing the full
% prereq tree before building the first file may feel like a slow startup
% to the user. This is definitely a much smaller concern now (the 2020s)
% than it was in the 1980s, but whether the startup delay it causes
% is below the perceptibility threshold for typical size projects,
% is something that can be answered only by empirical testing.
%
%---------------------------------------------------------------------------%

:- module make.dependencies.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module make.make_info.
:- import_module parse_tree.
:- import_module parse_tree.module_dep_info.

:- import_module io.
:- import_module set.

%---------------------------------------------------------------------------%

:- type find_prereqs_result
    --->    could_not_find_some_prereqs(set(dependency_file))
            % There were some prerequisites that we could not find.
            % The set specifies the prerequisites we *could* find.
    ;       found_all_prereqs(set(dependency_file)).
            % We found all prerequisites, and here they are.

    % find_direct_prereqs_of_target_file(ProgressStream, Globals,
    %   CompilationTaskType, ModuleDepInfo, TargetFile,
    %   Succeeded, Prereqs, !Info, !IO):
    %
    % The TargetType and ModuleIndexes arguments define a set of make targets.
    % Add to !Deps the dependency_file_indexes of all the files that
    % these make targets depend on, and which therefore have to be built
    % before we can build those make targets.
    %
:- pred find_direct_prereqs_of_target_file(io.text_output_stream::in,
    globals::in, compilation_task_type::in, module_dep_info::in,
    target_file::in, find_prereqs_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.file_util.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module make.deps_cache.
:- import_module make.deps_set.
:- import_module make.file_names.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module list.
:- import_module sparse_bitset.
:- import_module string.

%---------------------------------------------------------------------------%

% XXX MDNEW add loop on top to find indirect prereqs
% That way, complexity is given by
%   #layers in dependency hierarchy * avg width of each layer
% instead of by #paths through the layers,
% but ONLY if we don't repeat this work later, in submakes.
%
% XXX MDNEW Sort the final, overall set of prereqs
% - first on target type (.int3, .int0, .int/.int2, .opt etc)
% - then on the list of module name components
%
% Sorting on target type should put prereqs before the things they are prereqs
% *for* in all cases where the target types differ.
%
% Sorting on the list of module name components should put prereqs before
% the things they are prereqs *for* in all cases where the target types match.
%
% For parallel makes, we would also need to return, for each element of the
% list, the set of earlier elements that are its prereqs.
%
% XXX MDNEW We should never put any targets involving nested submodules into
% sets of depenendency_file_indexes. Those targets will be made as a side
% effect of making the same target type for the top module in the source file.
% However, the prereqs of a nested submodule, such as any fact table files
% it may need, must become the prereqs of the top-level module of their
% source file.

find_direct_prereqs_of_target_file(ProgressStream, Globals,
        CompilationTaskType, ModuleDepInfo, TargetFile, PrereqsResult,
        !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
     (
        CompilationTaskType = process_module(_),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        NestedSubModules =
            get_nested_children_list_of_top_module(MaybeTopModule),
        ModulesToCheck = [ModuleName | NestedSubModules]
    ;
        ( CompilationTaskType = target_code_to_object_code(_)
        ; CompilationTaskType = fact_table_code_to_object_code(_, _)
        ),
        ModulesToCheck = [ModuleName]
    ),
    module_names_to_index_set(ModulesToCheck, ModuleIndexesToCheckSet, !Info),
    ModuleIndexesToCheck = deps_set_to_sorted_list(ModuleIndexesToCheckSet),
    KeepGoing = make_info_get_keep_going(!.Info),
    find_direct_prereqs_of_nested_module_targets(ProgressStream, KeepGoing,
        Globals, TargetType, ModuleIndexesToCheck,
        succeeded, Succeeded, deps_set_init, PrereqIndexes0, !Info, !IO),
    ( if TargetType = module_target_int0 then
        % XXX Simon Taylor's comment, added originally to make.module_target.m
        % on 2002 Apr 23, says:
        %
        %   Avoid circular dependencies (the `.int0' files for the
        %   nested sub-modules depend on this module's `.int0' file).
        %
        % The log message of the commit says:
        %
        %   The `.int0' file for a module depends on the `.int0' file for the
        %   parent module, which caused circular dependencies with nested
        %   submodules, resulting in a compiler abort. The circular
        %   dependencies are now removed from the list of dependencies to make.
        %   Test case: tests/valid/foreign_type_spec.m.
        %
        % That test case, after being moved to valid_seq, was deleted together
        % with the IL backend. It seems to have consisted of a main module
        % and a separate submodule, so the NestedSubModules part of
        % ModulesToCheck would have been irrelevant. And the ModuleName part
        % of ModulesToCheck seems to be redundant: if the *target* is
        % ModuleName.int0, then surely ModuleName.int0 cannot be a prereq?
        %
        % For these reasons, the code here seems to me (zs) to be too crude:
        % it seems to delete far more prereqs than just the ones that may
        % cause the problem that it was added to address.
        ToDelete = make_dependency_list(ModulesToCheck, module_target_int0),
        dependency_files_to_index_set(ToDelete, ToDeleteIndexes, !Info),
        PrereqIndexes = deps_set_difference(PrereqIndexes0, ToDeleteIndexes)
    else
        ToDelete = [],
        PrereqIndexes = PrereqIndexes0
    ),
    dependency_file_index_set_to_plain_set(!.Info, PrereqIndexes, Prereqs),
    (
        Succeeded = did_not_succeed,
        PrereqsResult = could_not_find_some_prereqs(Prereqs)
    ;
        Succeeded = succeeded,
        PrereqsResult = found_all_prereqs(Prereqs)
    ),

    globals.lookup_bool_option(Globals, debug_make, DebugMake),
    (
        DebugMake = no
    ;
        DebugMake = yes,
        % XXX LEGACY
        set.map2_fold(dependency_file_to_file_name(Globals),
            Prereqs, PrereqFileNames, _PrereqFileNamesProposed, !IO),
        WriteFileName =
            ( pred(FN::in, SIO0::di, SIO::uo) is det :-
                io.format(ProgressStream, "\t%s\n", [s(FN)], SIO0, SIO)
            ),
        io.format(ProgressStream, "direct prereqs of %s %s:\n",
            [s(sym_name_to_string(ModuleName)),
            s(string.string(TargetType))], !IO),
        set.foldl(WriteFileName, PrereqFileNames, !IO),
        (
            ToDelete = []
        ;
            ToDelete = [_ | _],
            % XXX LEGACY
            list.map2_foldl(dependency_file_to_file_name(Globals),
                ToDelete, ToDeleteFileNames, _ToDeleteFileNamesProposed, !IO),
            io.write_string(ProgressStream, "after deleting:\n", !IO),
            list.foldl(WriteFileName, ToDeleteFileNames, !IO)
        ),
        io.write_string(ProgressStream, "end direct prereqs\n", !IO)
    ).

:- pred find_direct_prereqs_of_nested_module_targets(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_target_type::in,
    list(module_index)::in, maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_direct_prereqs_of_nested_module_targets(_, _, _, _, [],
        !Succeeded, !Prereqs, !Info, !IO).
find_direct_prereqs_of_nested_module_targets(ProgressStream, KeepGoing,
        Globals, TargetType, [ModuleIndex | ModuleIndexes],
        !Succeeded, !Prereqs, !Info, !IO) :-
    find_direct_prereqs_of_module_target(ProgressStream, KeepGoing,
        Globals, TargetType, ModuleIndex, HeadSucceeded, HeadPrereqs,
        !Info, !IO),
    deps_set_union(HeadPrereqs, !Prereqs),
    should_we_stop_or_continue(KeepGoing, HeadSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        find_direct_prereqs_of_nested_module_targets(ProgressStream, KeepGoing,
            Globals, TargetType, ModuleIndexes, !Succeeded, !Prereqs,
            !Info, !IO)
    ).

:- pred find_direct_prereqs_of_module_target(io.text_output_stream::in,
    maybe_keep_going::in, globals::in,
    module_target_type::in, module_index::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_direct_prereqs_of_module_target(ProgressStream, KeepGoing, Globals,
        TargetType, ModuleIndex, Succeeded, Prereqs, !Info, !IO) :-
    (
        ( TargetType = module_target_source
        ; TargetType = module_target_track_flags
        ),
        Succeeded = succeeded,
        Prereqs = deps_set_init
    ;
        TargetType = module_target_int3,
        PrereqSpecs = [self(module_target_source)],
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ;
        ( TargetType = module_target_int0
        ; TargetType = module_target_int1
        ; TargetType = module_target_int2
        ),
        PrereqSpecs = [
            self(module_target_source),
            ancestors(module_target_int0),
            direct_imports_intermod(module_target_int3),
            indirect_imports_intermod(module_target_int3)
        ],
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ;
        ( TargetType = module_target_c_code
        ; TargetType = module_target_c_header(_)
        ; TargetType = module_target_csharp_code
        ; TargetType = module_target_java_code
        ; TargetType = module_target_errors
        ),
        compiled_code_dependencies(Globals, PrereqSpecs),
        % XXX MDNEW Get intermod, pass as extra arg in the returned deps.
        % XXX MDNEW Same for direct and indirect deps, and ancestors.
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ;
        TargetType = module_target_java_class_code,
        PrereqSpec = self(module_target_java_code),
        find_prereqs_from_spec(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpec, Succeeded, Prereqs, !Info, !IO)
    ;
        TargetType = module_target_fact_table_object(PIC, _),
        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        PrereqSpec = self(TargetCodeType),
        find_prereqs_from_spec(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpec, Succeeded, Prereqs, !Info, !IO)
    ;
        TargetType = module_target_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),
        TargetCodeType = target_to_module_target_code(CompilationTarget, PIC),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

        MhHeader = module_target_c_header(header_mh),
        MihHeader = module_target_c_header(header_mih),
        PrereqSpecSelf = self(TargetCodeType),
        PrereqSpecMh = foreign_imports_intermod_trans(MhHeader),
        ( if
            CompilationTarget = target_c,
            HighLevelCode = yes
        then
            PrereqSpecs = [PrereqSpecSelf, PrereqSpecMh,
                direct_imports_intermod(MihHeader),
                indirect_imports_intermod(MihHeader),
                ancestors(MihHeader),
                intermod_imports(MihHeader)
            ]
        else
            PrereqSpecs = [PrereqSpecSelf, PrereqSpecMh]
        ),
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ;
        ( TargetType = module_target_opt
        ; TargetType = module_target_xml_doc
        ),
        PrereqSpecs = [
            self(module_target_source),
            anc0_dir1_indir2_non_intermod
        ],
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ;
        TargetType = module_target_analysis_registry,
        PrereqSpecs = [
            self(module_target_source),
            anc0_dir1_indir2_non_intermod,
            direct_imports_intermod(module_target_opt),
            indirect_imports_intermod(module_target_opt),
            intermod_imports(module_target_opt)
        ],
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, PrereqSpecs, Prereqs, succeeded, Succeeded,
            !Info, !IO)
    ).

:- func target_to_module_target_code(compilation_target, pic)
    = module_target_type.

target_to_module_target_code(_CompilationTarget, _PIC) = TargetCode :-
    % XXX it looks wrong to be returning module_target_c_code for
    % all compilation targets.
    TargetCode = module_target_c_code.

:- pred compiled_code_dependencies(globals::in, list(prereq_spec)::out) is det.

compiled_code_dependencies(Globals, PrereqSpecs) :-
    % We build up PrereqSpecs in stages.

    % Stage 0: dependencies on flags.
    globals.lookup_bool_option(Globals, make_track_flags, TrackFlags),
    (
        TrackFlags = yes,
        PrereqSpecsTracks = [self(module_target_track_flags)]
    ;
        TrackFlags = no,
        PrereqSpecsTracks = []
    ),

    % Stage 1: dependencies on the source file, and on the fact table files,
    % foreign language files and Mercury interface files it imports.
    PrereqSpecsSrcInts = [
        self(module_target_source),
        self_foreign_incl_fact_table_files,
        self(module_target_int1),
        % XXX MDNEW The next two prereq_specs should be a single
        % combined prereq_spec, anc01_dir1_indir2_intermod.
        ancestors(module_target_int1),
        anc0_dir1_indir2_intermod
    ],

    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    AnyIntermod = bool.or(IntermodOpt, IntermodAnalysis),

    % Stage 2: dependencies on optimization files.
    (
        AnyIntermod = yes,
        PrereqSpecsOpts = [
            self(module_target_opt),
            % XXX MDNEW Given that we compute the set of intermod imports
            % for this prereq_spec ...
            intermod_imports(module_target_opt),
            % ... why do we have to compute it AGAIN, as part of this
            % prereq_spec as well?
            %
            % We should replace both of these prereq_specs with one that
            % does the job of both but computes the set of intermod_imports
            % modules set just once. This would save even the cost of a
            % cache hit.
            anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports
        ]
    ;
        AnyIntermod = no,
        PrereqSpecsOpts = []
    ),

    % Stage 3: dependencies on analysis result files.
    (
        IntermodAnalysis = yes,
        PrereqSpecsRegistries = [
            self(module_target_analysis_registry),
            direct_imports_intermod(module_target_analysis_registry)
        ]
    ;
        IntermodAnalysis = no,
        PrereqSpecsRegistries = []
    ),

    PrereqSpecs = PrereqSpecsTracks ++ PrereqSpecsSrcInts ++
        PrereqSpecsOpts ++ PrereqSpecsRegistries.

%---------------------------------------------------------------------------%

    % The prerequisite specification type.
    %
    % Values of this type indirectly represent the specification of
    % a set of dependency_files (actually, dependency_file_indexes).
    % The "indirect" part is there because they actually represent
    % a specification of a task for find_prereqs_from_spec, which will compute
    % that set of dependency file indexes when given a prereq_spec.
    %
:- type prereq_spec
    --->    self(module_target_type)
    ;       ancestors(module_target_type)
    ;       direct_imports_non_intermod(module_target_type)
    ;       direct_imports_intermod(module_target_type)
    ;       indirect_imports_non_intermod(module_target_type)
    ;       indirect_imports_intermod(module_target_type)
    ;       intermod_imports(module_target_type)
    ;       foreign_imports_intermod_trans(module_target_type)

    ;       anc0_dir1_indir2_non_intermod
    ;       anc0_dir1_indir2_intermod
            % Get the .int0 files of ancestors, the .int files of direct
            % imports, and the .int2 files of indirect imports.

    ;       anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports
            % Get the .int0 files of ancestors, the .int files of direct
            % imports, and the .int2 files of indirect imports, but not
            % of the specified module, but of the ancestors of its intermod
            % imports.

    ;       self_foreign_incl_fact_table_files.
            % Files named in foreign include module and fact table pragmas.

:- pred find_prereqs_from_specs(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in, list(prereq_spec)::in,
    deps_set(dependency_file_index)::out,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_prereqs_from_specs(_, _, _, _, [], deps_set_init, !Succeeded, !Info, !IO).
find_prereqs_from_specs(ProgressStream, KeepGoing, Globals, ModuleIndex,
        [HeadPrereqSpec | TailPrereqSpecs], DepFileIndexSet,
        !Succeeded, !Info, !IO) :-
    find_prereqs_from_spec(ProgressStream, KeepGoing, Globals,
        ModuleIndex, HeadPrereqSpec, HeadSucceeded, HeadDepFileIndexSet,
        !Info, !IO),
    should_we_stop_or_continue(KeepGoing, HeadSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop,
        DepFileIndexSet = HeadDepFileIndexSet
    ;
        StopOrContinue = soc_continue,
        find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
            ModuleIndex, TailPrereqSpecs, TailDepFileIndexSet,
            !Succeeded, !Info, !IO),
        deps_set_union(HeadDepFileIndexSet, TailDepFileIndexSet,
            DepFileIndexSet)
    ).

:- pred find_prereqs_from_spec(io.text_output_stream::in, maybe_keep_going::in,
    globals::in, module_index::in, prereq_spec::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_prereqs_from_spec(ProgressStream, KeepGoing, Globals, ModuleIndex,
        PrereqSpec, Succeeded, DepFileIndexSet, !Info, !IO) :-
    trace [
        compile_time(flag("find_prereqs_from_spec")),
        run_time(env("FIND_PREREQS_FROM_SPEC")),
        io(!TIO)
    ] (
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        io.format(ProgressStream, "starting prereq_spec %s for %s\n",
            [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
    ),
    % XXX Some of these alternatives don't need I/O.
    % We can wrap caching code around the code of any set of switch arms.
    %
    % XXX Are there are any prereq_spec kinds for which we may return
    % did_not_succeed AND a nonempty DepFileIndexSet? If not, then
    % those two parameters are effectively a single value of a maybe type.
    (
        PrereqSpec = self(TargetType),
        Succeeded = succeeded,
        dfmi_target(ModuleIndex, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = ancestors(TargetType),
        Succeeded = succeeded,
        module_index_to_name(!.Info, ModuleIndex, ModuleName),
        Ancestors = get_ancestors(ModuleName),
        module_names_to_index_set(Ancestors, ModuleIndexSet, !Info),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = direct_imports_non_intermod(TargetType),
        get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = direct_imports_intermod(TargetType),
        get_direct_imports_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = indirect_imports_non_intermod(TargetType),
        get_indirect_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = indirect_imports_intermod(TargetType),
        get_indirect_imports_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = intermod_imports(TargetType),
        get_intermod_imports(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = foreign_imports_intermod_trans(TargetType),
        get_foreign_imports_intermod_trans(ProgressStream, KeepGoing, Globals,
            ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO),
        dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info)
    ;
        PrereqSpec = anc0_dir1_indir2_non_intermod,
        SubPrereqSpecs = [
            ancestors(module_target_int0),
            direct_imports_non_intermod(module_target_int1),
            indirect_imports_non_intermod(module_target_int2)
        ],
        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s starts\n\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        ),

% This cache is disabled, because it is ineffective.
%       ( if
%           search_anc0_dir1_indir2_non_intermod_cache(!.Info, ModuleIndex,
%               Result0)
%       then
%           Result0 = deps_result(Succeeded, DepFileIndexSet)
%       else
            find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
                ModuleIndex, SubPrereqSpecs, DepFileIndexSet,
                succeeded, Succeeded, !Info, !IO),
%           Result = deps_result(Succeeded, DepFileIndexSet),
%           add_to_anc0_dir1_indir2_non_intermod_cache(ModuleIndex, Result,
%               !Info)
%       ),

        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s ends\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        PrereqSpec = anc0_dir1_indir2_intermod,
        SubPrereqSpecs = [
            ancestors(module_target_int0),
            direct_imports_intermod(module_target_int1),
            indirect_imports_intermod(module_target_int2)
        ],
        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s starts\n\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        ),

        ( if
            search_anc0_dir1_indir2_intermod_cache(!.Info, ModuleIndex,
                Result0)
        then
            Result0 = deps_result(Succeeded, DepFileIndexSet)
        else
            find_prereqs_from_specs(ProgressStream, KeepGoing, Globals,
                ModuleIndex, SubPrereqSpecs, DepFileIndexSet,
                succeeded, Succeeded, !Info, !IO),
            Result = deps_result(Succeeded, DepFileIndexSet),
            add_to_anc0_dir1_indir2_intermod_cache(ModuleIndex, Result, !Info)
        ),

        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s ends\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        PrereqSpec =
            anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports,
        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s starts\n\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        ),

        get_anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports(
            ProgressStream, KeepGoing, Globals, ModuleIndex,
            Succeeded, DepFileIndexSet, !Info, !IO),

        trace [
            compile_time(flag("find_prereqs_from_spec")),
            run_time(env("FIND_PREREQS_FROM_SPEC")),
            io(!TIO)
        ] (
            module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
            IndexModuleNameStr = sym_name_to_string(IndexModuleName),
            io.format(ProgressStream, "prereq_spec %s for %s ends\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        )
    ;
        PrereqSpec = self_foreign_incl_fact_table_files,
        get_foreign_incl_fact_table_files(ProgressStream, Globals,
            ModuleIndex, Succeeded, DepFileIndexSet, !Info, !IO)
    ),
    trace [
        compile_time(flag("find_prereqs_from_spec")),
        run_time(env("FIND_PREREQS_FROM_SPEC")),
        io(!TIO)
    ] (
        module_index_to_name(!.Info, ModuleIndex, IndexModuleName),
        IndexModuleNameStr = sym_name_to_string(IndexModuleName),
        dependency_file_index_set_to_plain_set(!.Info, DepFileIndexSet,
            DepFileSet),
        DepFiles = set.to_sorted_list(DepFileSet),
        (
            DepFiles = [],
            io.format(ProgressStream,
                "prereq_spec %s for %s yields no prereqs\n\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO)
        ;
            DepFiles = [_ | _],
            DepFileNlStrs = list.map(
                dependency_file_to_debug_string("    ", "\n"), DepFiles),
            io.format(ProgressStream,
                "prereq_spec %s for %s yields these prereqs:\n",
                [s(string.string(PrereqSpec)), s(IndexModuleNameStr)], !TIO),
            list.foldl(io.write_string(ProgressStream), DepFileNlStrs, !TIO),
            io.write_string(ProgressStream, "prereq list ends\n\n", !TIO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred dfmi_target(module_index::in, module_target_type::in,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.
:- pragma inline(pred(dfmi_target/5)).

dfmi_target(ModuleIndex, TargetType, DepFileIndexSet, !Info) :-
    acc_rev_dfmi_target(TargetType, ModuleIndex,
        deps_set_init, DepFileIndexSet, !Info).

:- pred dfmi_targets(deps_set(module_index)::in, module_target_type::in,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.
:- pragma inline(pred(dfmi_targets/5)).

dfmi_targets(ModuleIndexSet, TargetType, DepFileIndexSet, !Info) :-
    deps_set_foldl2(acc_rev_dfmi_target(TargetType), ModuleIndexSet,
        deps_set_init, DepFileIndexSet, !Info).

:- pred acc_rev_dfmi_target(module_target_type::in, module_index::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.
:- pragma inline(pred(acc_rev_dfmi_target/6)).

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
        module_dep_info_get_int_deps(ModuleDepInfo, IntModuleNames),
        module_dep_info_get_imp_deps(ModuleDepInfo, ImpModuleNames),
        module_names_to_index_set(set.to_sorted_list(IntModuleNames),
            IntModules, !Info),
        module_names_to_index_set(set.to_sorted_list(ImpModuleNames),
            ImpModules, !Info),
        deps_set_union(IntModules, ImpModules, Modules0),
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
% This cache is disabled, because it is ineffective.
%   ( if
%       search_indirect_imports_non_intermod_cache(!.Info, ModuleIndex,
%           Result0)
%   then
%       Result0 = deps_result(Succeeded, IndirectNonIntermodImportModules)
%   else
        get_direct_imports_non_intermod(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DirectSucceeded, DirectImportModules, !Info, !IO),
        get_indirect_imports_uncached(ProgressStream, KeepGoing, Globals,
            ModuleIndex, DirectSucceeded, DirectImportModules,
            Succeeded, IndirectNonIntermodImportModules, !Info, !IO).
%       Result = deps_result(Succeeded, IndirectNonIntermodImportModules),
%       add_to_indirect_imports_non_intermod_cache(ModuleIndex, Result, !Info)
%   ).

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
        set.foldl2(acc_module_index_if_for_lang_in_set(LangSet), FIMSpecs,
            deps_set_init, ForeignModules, !Info),
        Succeeded = succeeded
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        ForeignModules = deps_set_init,
        Succeeded = did_not_succeed
    ).

:- pred acc_module_index_if_for_lang_in_set(set(foreign_language)::in,
    fim_spec::in, deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

acc_module_index_if_for_lang_in_set(LangSet, FIMSpec, !DepsSet, !Info) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    ( if set.contains(LangSet, Lang) then
        module_name_to_index(ModuleName, ModuleIndex, !Info),
        deps_set_insert(ModuleIndex, !DepsSet)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred get_anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports(
    io.text_output_stream::in, maybe_keep_going::in, globals::in,
    module_index::in,
    maybe_succeeded::out, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_anc0_dir1_indir2_intermod_of_ancestors_of_intermod_imports(ProgressStream,
        KeepGoing, Globals, ModuleIndex, !:Succeeded, DepFileIndexSet,
        !Info, !IO) :-
    get_ancestors_of_intermod_imports(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    should_we_stop_or_continue(KeepGoing, Succeeded1, StopOrContinue,
        succeeded, !:Succeeded),
    (
        StopOrContinue = soc_stop,
        DepFileIndexSet = deps_set_init
    ;
        StopOrContinue = soc_continue,
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        fold_prereq_spec_over_modules(ProgressStream, KeepGoing, Globals,
            anc0_dir1_indir2_intermod, ModuleList1,
            !Succeeded, deps_set_init, DepFileIndexSet, !Info, !IO)
    ).

:- pred get_ancestors_of_intermod_imports(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_ancestors_of_intermod_imports(ProgressStream, KeepGoing, Globals,
        ModuleIndex, Succeeded, ModuleIndexSet, !Info, !IO) :-
    get_intermod_imports(ProgressStream, KeepGoing, Globals, ModuleIndex,
        Succeeded1, Modules1, !Info, !IO),
    should_we_stop_or_continue(KeepGoing, Succeeded1, StopOrContinue,
        succeeded, Succeeded),
    (
        StopOrContinue = soc_stop,
        ModuleIndexSet = deps_set_init
    ;
        StopOrContinue = soc_continue,
        ModuleList1 = deps_set_to_sorted_list(Modules1),
        list.map_foldl(index_get_ancestors,
            ModuleList1, AncestorModuleIndexSets, !Info),
        ModuleIndexSet = deps_set_union_list(AncestorModuleIndexSets)
    ).

:- pred index_get_ancestors(module_index::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

index_get_ancestors(ModuleIndex, AncestorModuleIndexSet, !Info) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    AncestorModuleNames = get_ancestors(ModuleName),
    module_names_to_index_set(AncestorModuleNames, AncestorModuleIndexSet,
        !Info).

%---------------------------------------------------------------------------%

:- pred get_foreign_incl_fact_table_files(io.text_output_stream::in,
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_incl_fact_table_files(ProgressStream, Globals, ModuleIndex,
        Succeeded, DepFileIndexSet, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFileNames),
        file_names_to_index_set(set.to_sorted_list(FactTableFileNames),
            FactDepFileIndexSet, !Info),

        module_dep_info_get_source_file_name(ModuleDepInfo, SourceFileName),
        module_dep_info_get_foreign_include_files(ModuleDepInfo,
            ForeignIncludeFiles),
        globals.get_backend_foreign_languages(Globals, Languages),
        LangSet = set.list_to_set(Languages),
        set.foldl2(
            acc_dep_file_index_for_foreign_include_if_in_langset(LangSet,
                SourceFileName),
            ForeignIncludeFiles, FactDepFileIndexSet, DepFileIndexSet, !Info)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        DepFileIndexSet = deps_set_init
    ).

:- pred acc_dep_file_index_for_foreign_include_if_in_langset(
    set(foreign_language)::in, file_name::in, foreign_include_file_info::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

acc_dep_file_index_for_foreign_include_if_in_langset(LangSet, SourceFileName,
        ForeignInclude, !DepFileIndexSet, !Info) :-
    ForeignInclude = foreign_include_file_info(Lang, IncludeFileName),
    ( if set.contains(LangSet, Lang) then
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        DepFile = dfmi_file(IncludePath),
        dependency_file_to_index(DepFile, DepFileIndex, !Info),
        deps_set_insert(DepFileIndex, !DepFileIndexSet)
    else
        true
    ).

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

    % find_module_deps(Globals, ModuleIndex, Succeeded, ModuleIndexSet,
    %   !Info, !IO).
    %
    % The reason we don't return maybe(ModuleIndexSet) is that
    % with `--keep-going', we want to do as much work as possible.
    %
:- type find_module_deps(T) ==
    pred(io.text_output_stream, maybe_keep_going, globals, module_index,
        maybe_succeeded, deps_set(T), make_info, make_info, io, io).
:- inst find_module_deps ==
    (pred(in, in, in, in, out, out, in, out, di, uo) is det).

    % fold_find_modules_over_modules(KeepGoing, Globals, FindModules,
    %   ModuleIndexes, !Succeeded, !ModuleIndexSet, !Info, !IO):
    %
    % Invoke FindModules on each element of ModuleIndexes, adding
    % the union of the returned module index sets to !ModuleIndexSet.
    %
    % Stop only if an invocation of FindModules returns did_not_succeed
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
fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals, FindModules,
        [MI | MIs], !Succeeded, !ModuleIndexSet, !Info, !IO) :-
    FindModules(ProgressStream, KeepGoing, Globals, MI,
        HeadSucceeded, HeadModuleIndexSet, !Info, !IO),
    deps_set_union(HeadModuleIndexSet, !ModuleIndexSet),
    should_we_stop_or_continue(KeepGoing, HeadSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        fold_find_modules_over_modules(ProgressStream, KeepGoing, Globals,
            FindModules, MIs, !Succeeded, !ModuleIndexSet, !Info, !IO)
    ).

%---------------------%

    % fold_prereq_spec_over_modules(ProgressStream, KeepGoing, Globals,
    %   PrereqSpec, ModuleIndexes, !Succeeded, !DepFileIndexSet, !Info, !IO):
    %
    % Invoke find_prereqs_from_spec with PrereqSpec on each element of
    % ModuleIndexes, adding the union of the resulting dependency file
    % index sets to !DepFileIndexSet.
    %
    % Stop only if an invocation of find_prereqs_from_spec returns
    % did_not_succeed *and* KeepGoing is do_not_keep_going.
    %
:- pred fold_prereq_spec_over_modules(io.text_output_stream::in,
    maybe_keep_going::in, globals::in, prereq_spec::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fold_prereq_spec_over_modules(_, _, _, _,
        [], !Succeeded, !DepFileIndexSet, !Info, !IO).
fold_prereq_spec_over_modules(ProgressStream, KeepGoing, Globals, PrereqSpec,
        [MI | MIs], !Succeeded, !DepFileIndexSet, !Info, !IO) :-
    find_prereqs_from_spec(ProgressStream, KeepGoing, Globals, MI, PrereqSpec,
        HeadSucceeded, HeadDepFileIndexSet, !Info, !IO),
    deps_set_union(HeadDepFileIndexSet, !DepFileIndexSet),
    should_we_stop_or_continue(KeepGoing, HeadSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        fold_prereq_spec_over_modules(ProgressStream, KeepGoing, Globals,
            PrereqSpec, MIs, !Succeeded, !DepFileIndexSet, !Info, !IO)
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
