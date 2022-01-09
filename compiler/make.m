%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.m.
% Main author: stayl.
%
% A builtin Mercury-specific make replacement.
%
% TODO:
% - distributed builds
%
%-----------------------------------------------------------------------------%

:- module make.
:- interface.

:- include_module make.build.
:- include_module make.options_file.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module make.options_file.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_imports.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % We export this type so that mercury_compile_main.m can tell
    % the code it calls in this package that the call did *not* come from
    % the code of mmc --make itself.
    %
:- type maybe_invoked_by_mmc_make
    --->    not_invoked_by_mmc_make
    ;       invoked_by_mmc_make.

%-----------------------------------------------------------------------------%

:- type make_info.

:- type rebuild_module_deps
    --->    do_rebuild_module_deps
    ;       do_not_rebuild_module_deps.

%-----------------------------------------------------------------------------%

:- pred make_write_module_dep_file(globals::in, burdened_aug_comp_unit::in,
    io::di, io::uo) is det.

:- func make_module_dep_file_extension = other_ext is det.

%-----------------------------------------------------------------------------%

:- pred make_process_compiler_args(globals::in, list(string)::in,
    options_variables::in, list(string)::in, list(file_name)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module make.dependencies.
:- include_module make.deps_set.
:- include_module make.module_dep_file.
:- include_module make.module_target.
:- include_module make.program_target.
:- include_module make.util.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.handle_options.
:- import_module libs.maybe_succeeded.
:- import_module libs.md5.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.dependencies.
:- import_module make.deps_set.
:- import_module make.module_dep_file.
:- import_module make.module_target.
:- import_module make.program_target.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.read_modules.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module solutions.
:- import_module string.
:- import_module version_array.
:- import_module version_hash_table.

%-----------------------------------------------------------------------------%

:- type make_info
    --->    make_info(
                % XXX CLEANUP Add a prefix to each field name
                % to avoid accidental name collisions.

                % For modules whose sources we have read in, the
                % maybe_module_dep_info will contain module_and_imports.
                % For modules for which we have read only the .dep file,
                % maybe_module_dep_info will contain a module_dep_summary.
                %
                % An old comment about the former says:
                %
                % "The items field of each module_and_imports structure should
                % be empty -- we are not trying to cache the items here",
                % but I (zs) don't whether that is actually true.
                mki_module_dependencies :: map(module_name,
                                            maybe_module_dep_info),

                mki_file_timestamps     :: file_timestamps,

                % Cache chosen file names for a module name and extension.
                mki_search_file_name_cache :: map(module_name_ext, file_name),

                % Any flags required to set detected library grades.
                mki_detected_grade_flags :: list(string),

                % The original set of options passed to mmc, not including
                % the targets to be made.
                mki_option_args         :: list(string),

                % The contents of the Mercury.options file.
                mki_options_variables   :: options_variables,

                % The mapping between module_names and indices.
                mki_module_index_map    :: module_index_map,

                % The mapping between dependency_files and indices.
                mki_dep_file_index_map  :: dependency_file_index_map,

                mki_dependency_status   :: version_hash_table(dependency_file,
                                            dependency_status),

                % For each module, the set of modules for which the `.int'
                % files are read, excluding those read as a result of reading
                % `.opt' files. The bool records whether there was an error
                % in the dependencies.
                % XXX Use a better representation for the sets.
                mki_cached_direct_imports :: cached_direct_imports,

                mki_cached_non_intermod_direct_imports
                                        :: cached_direct_imports,

                % The boolean is `yes' if the result is complete.
                % XXX Use a better representation for the sets.
                mki_cached_transitive_dependencies
                                        :: cached_transitive_dependencies,

                mki_cached_foreign_imports :: cached_foreign_imports,

                % Should the `.module_dep' files be rebuilt.
                % Set to `no' for `mmc --make clean'.
                mki_rebuild_module_deps :: rebuild_module_deps,

                mki_keep_going          :: maybe_keep_going,

                % Modules for which we have redirected output
                % to a `.err' file during this invocation of mmc.
                mki_error_file_modules  :: set(module_name),

                % Used for reporting which module imported a nonexistent
                % module.
                mki_importing_module    :: maybe(module_name),

                % Targets specified on the command line.
                mki_command_line_targets
                                        :: set(pair(module_name, target_type)),

                % The remaining number of analysis passes that we will allow
                % on `suboptimal' modules. It starts at the value of
                % `--analysis-repeat' and decrements to zero as analysis passes
                % on `suboptimal' modules are performed. `invalid' modules
                % are not affected as they will always be reanalysed.
                mki_reanalysis_passes   :: int,

                % An inter-process lock to prevent multiple processes
                % interleaving their output to standard output.
                mki_maybe_stdout_lock   :: maybe(stdout_lock),

                % The parse trees of the files we have read so far,
                % so we never have to read and parse each file more than once.
                mki_mi_read_module_maps :: have_read_module_maps
            ).

:- type module_name_ext
    --->    module_name_ext(module_name, ext).

:- type maybe_module_dep_info
    --->    no_module_dep_info
    ;       some_module_dep_info(module_dep_info).

:- type module_index_map
    --->    module_index_map(
                mim_forward_map         :: version_hash_table(module_name,
                                            module_index),
                mim_reverse_map         :: version_array(module_name),
                mim_counter             :: int
            ).

:- type dependency_file_index_map
    --->    dependency_file_index_map(
                dfim_forward_map        :: version_hash_table(dependency_file,
                                            dependency_file_index),
                dfim_reverse_map        :: version_array(dependency_file),
                dfim_counter            :: int
            ).

:- type make_error
    --->    make_error_target(target_file)
    ;       make_error_dependencies(module_name)
    ;       make_error_other(string).

:- type compilation_task_type
    --->    process_module(module_compilation_task_type)

            % The `pic' argument is only used for `--target c'.
    ;       target_code_to_object_code(pic)
    ;       foreign_code_to_object_code(pic, foreign_language)
    ;       fact_table_code_to_object_code(pic, file_name).

:- type module_compilation_task_type
    --->    task_errorcheck
    ;       task_make_int0
    ;       task_make_int12 % makes both .int and .int2
    ;       task_make_int3
    ;       task_make_opt
    ;       task_make_analysis_registry
    ;       task_compile_to_target_code
    ;       task_make_xml_doc.

:- type module_target_type
    --->    module_target_source
    ;       module_target_errors
    ;       module_target_int0
    ;       module_target_int1
    ;       module_target_int2
    ;       module_target_int3
    ;       module_target_opt
    ;       module_target_analysis_registry
    ;       module_target_track_flags
    ;       module_target_c_header(c_header_type)
    ;       module_target_c_code
    ;       module_target_csharp_code
    ;       module_target_java_code
    ;       module_target_java_class_code
    ;       module_target_object_code(pic)
    ;       module_target_foreign_object(pic, foreign_language)
    ;       module_target_fact_table_object(pic, file_name)
    ;       module_target_xml_doc.

:- type c_header_type
    --->    header_mh    % For `:- pragma foreign_export' declarations.
    ;       header_mih.  % Declarations for hlc grades, for compiler use only.

% :- type linked_target_type in compile_target_code.m.

:- type misc_target_type
    --->    misc_target_clean
    ;       misc_target_realclean
    ;       misc_target_build_all(module_target_type)
    ;       misc_target_build_analyses
    ;       misc_target_build_library
    ;       misc_target_install_library
    ;       misc_target_build_xml_docs.

:- type file_timestamps == map(string, maybe_error(timestamp)).

:- type dependency_status
    --->    deps_status_not_considered
    ;       deps_status_being_built
    ;       deps_status_up_to_date
    ;       deps_status_error.

:- type target_file
    --->    target_file(
                target_file_name    :: module_name,
                target_file_type    :: module_target_type
            ).

:- type linked_target_file
    --->    linked_target_file(
                linked_tf_name      :: module_name,
                linked_tf_type      :: linked_target_type
            ).

:- type maybe_keep_going
    --->    do_not_keep_going
    ;       do_keep_going.

%-----------------------------------------------------------------------------%

make_write_module_dep_file(Globals, BurdenedAugCompUnit, !IO) :-
    make.module_dep_file.write_module_dep_file(Globals,
        BurdenedAugCompUnit, !IO).

make_module_dep_file_extension = other_ext(".module_dep").

%-----------------------------------------------------------------------------%

make_process_compiler_args(Globals, DetectedGradeFlags, Variables, OptionArgs,
        Targets0, !IO) :-
    (
        Targets0 = [],
        lookup_main_target(Variables, MaybeTargets, LookupSpecs),
        write_error_specs_ignore(Globals, LookupSpecs, !IO),
        LookupErrors = contains_errors(Globals, LookupSpecs),
        (
            LookupErrors = yes,
            Targets = [],
            Continue0 = no
        ;
            LookupErrors = no,
            (
                MaybeTargets = no,
                Targets = [],
                Continue0 = no
            ;
                MaybeTargets = yes(Targets),
                (
                    Targets = [_ | _],
                    Continue0 = yes
                ;
                    Targets = [],
                    Continue0 = no,
                    io.write_string("** Error: no targets specified " ++
                        "and `MAIN_TARGET' not defined.\n", !IO)
                )
            )
        )
    ;
        Targets0 = [_ | _],
        Continue0 = yes,
        Targets = Targets0
    ),

    % Ensure none of the targets contains the directory_separator. Such targets
    % are not supported by the rest of the code.

    list.filter(
        ( pred(Target::in) is semidet :-
            string.contains_char(Target, dir.directory_separator)
        ), Targets, AbsTargets),
    (
        AbsTargets = [],
        Continue = Continue0
    ;
        AbsTargets = [_ | _],
        Continue = no,
        io.progname_base("mercury_compile", ProgName, !IO),
        AbsTargetSpecs =
            list.map(report_target_with_dir_component(ProgName), AbsTargets),
        io.stderr_stream(StdErr, !IO),
        write_error_specs_ignore(StdErr, Globals, AbsTargetSpecs, !IO)
    ),
    (
        Continue = no,
        io.set_exit_status(1, !IO)
    ;
        Continue = yes,
        globals.lookup_bool_option(Globals, keep_going, KeepGoingBool),
        ( KeepGoingBool = no,  KeepGoing = do_not_keep_going
        ; KeepGoingBool = yes, KeepGoing = do_keep_going
        ),

        ModuleIndexMap = module_index_map(
            version_hash_table.init_default(module_name_hash),
            version_array.empty, 0),
        DepIndexMap = dependency_file_index_map(
            version_hash_table.init_default(dependency_file_hash),
            version_array.empty, 0),
        DepStatusMap = version_hash_table.init_default(dependency_file_hash),

        % Accept and ignore `.depend' targets. `mmc --make' does not need
        % a separate make depend step. The dependencies for each module
        % are regenerated on demand.
        NonDependTargets = list.filter(
            ( pred(Target::in) is semidet :-
                not string.suffix(Target, ".depend")
            ), Targets),
        % Classify the remaining targets.
        list.map(classify_target(Globals), NonDependTargets,
            ClassifiedTargets),

        ShouldRebuildModuleDeps = do_rebuild_module_deps,
        globals.lookup_int_option(Globals, analysis_repeat, AnalysisRepeat),

        map.init(ModuleDependencies),
        map.init(FileTimestamps),
        map.init(SearchFileNameCache),
        set.init(ErrorFileModules),
        MaybeImportingModule = maybe.no,
        MaybeStdoutLock = maybe.no,
        MakeInfo0 = make_info(
            ModuleDependencies,
            FileTimestamps,
            SearchFileNameCache,
            DetectedGradeFlags,
            OptionArgs,
            Variables,
            ModuleIndexMap,
            DepIndexMap,
            DepStatusMap,
            init_cached_direct_imports,
            init_cached_direct_imports,
            init_cached_transitive_dependencies,
            init_cached_foreign_imports,
            ShouldRebuildModuleDeps,
            KeepGoing,
            ErrorFileModules,
            MaybeImportingModule,
            set.list_to_set(ClassifiedTargets),
            AnalysisRepeat,
            MaybeStdoutLock,
            init_have_read_module_maps
        ),

        % Build the targets, stopping on any errors if `--keep-going'
        % was not set.
        foldl2_maybe_stop_at_error(KeepGoing, make_target, Globals,
            ClassifiedTargets, Succeeded, MakeInfo0, _MakeInfo, !IO),
        maybe_set_exit_status(Succeeded, !IO)
    ).

:- func report_target_with_dir_component(string, string) = error_spec.

report_target_with_dir_component(ProgName, Target) = Spec :-
    Pieces = [fixed(ProgName), suffix(":"), fixed(Target), suffix(":"), nl,
        words("Make target must not contain any directory component."), nl],
    Spec = error_spec($pred, severity_error, phase_make_target,
        [error_msg(no, treat_as_first, 0, [always(Pieces)])]).

:- pred make_target(globals::in, pair(module_name, target_type)::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_target(Globals, Target, Succeeded, !Info, !IO) :-
    Target = ModuleName - TargetType,
    globals.lookup_bool_option(Globals, track_flags, TrackFlags),
    (
        TrackFlags = no,
        TrackFlagsSucceeded = succeeded
    ;
        TrackFlags = yes,
        make_track_flags_files(Globals, ModuleName, TrackFlagsSucceeded,
            !Info, !IO)
    ),
    (
        TrackFlagsSucceeded = succeeded,
        (
            TargetType = module_target(ModuleTargetType),
            TargetFile = target_file(ModuleName, ModuleTargetType),
            make_module_target(Globals, dep_target(TargetFile), Succeeded,
                !Info, !IO)
        ;
            TargetType = linked_target(ProgramTargetType),
            LinkedTargetFile = linked_target_file(ModuleName,
                ProgramTargetType),
            make_linked_target(Globals, LinkedTargetFile, Succeeded,
                !Info, !IO)
        ;
            TargetType = misc_target(MiscTargetType),
            make_misc_target(Globals, ModuleName - MiscTargetType, Succeeded,
                !Info, !IO)
        )
    ;
        TrackFlagsSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%-----------------------------------------------------------------------------%

:- type target_type
    --->    module_target(module_target_type)
    ;       linked_target(linked_target_type)
    ;       misc_target(misc_target_type).

:- pred classify_target(globals::in, string::in,
    pair(module_name, target_type)::out) is det.

classify_target(Globals, FileName, ModuleName - TargetType) :-
    ( if
        string.length(FileName, NameLength),
        search_backwards_for_dot(FileName, NameLength, DotLocn),
        string.split(FileName, DotLocn, ModuleNameStr0, Suffix),
        solutions(classify_target_2(Globals, ModuleNameStr0, Suffix),
            TargetFiles),
        TargetFiles = [TargetFile]
    then
        TargetFile = ModuleName - TargetType
    else if
        string.append("lib", ModuleNameStr, FileName)
    then
        TargetType = misc_target(misc_target_build_library),
        file_name_to_module_name(ModuleNameStr, ModuleName)
    else
        ExecutableType = get_executable_type(Globals),
        TargetType = linked_target(ExecutableType),
        file_name_to_module_name(FileName, ModuleName)
    ).

:- pred classify_target_2(globals::in, string::in, string::in,
    pair(module_name, target_type)::out) is nondet.

classify_target_2(Globals, ModuleNameStr0, ExtStr, ModuleName - TargetType) :-
    ( if
        extension_to_target_type(Globals, ExtStr, ModuleTargetType),
        % The .cs extension was used to build all C target files, but .cs is
        % also the file name extension for a C# file. The former use is being
        % migrated over to the .all_cs target but we still accept it for now.
        % NOTE This workaround is still in use as of 2020 may 23, even though
        % it was added in 2010,
        ExtStr \= ".cs"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = module_target(ModuleTargetType)
    else if
        target_extension_synonym(ExtStr, ModuleTargetType)
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = module_target(ModuleTargetType)
    else if
        globals.lookup_string_option(Globals, library_extension, ExtStr),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(static_library)
    else if
        globals.lookup_string_option(Globals, shared_library_extension,
            ExtStr),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(shared_library)
    else if
        globals.lookup_string_option(Globals, executable_file_extension,
            ExtStr)
    then
        ModuleNameStr = ModuleNameStr0,
        ExecutableType = get_executable_type(Globals),
        TargetType = linked_target(ExecutableType)
    else if
        (
            string.append(".all_", Rest, ExtStr),
            string.append(DotlessExtStr1, "s", Rest),
            ExtStr1 = "." ++ DotlessExtStr1
        ;
            % Deprecated.
            string.append(ExtStr1, "s", ExtStr)
        ),
        (
            extension_to_target_type(Globals, ExtStr1, ModuleTargetType)
        ;
            target_extension_synonym(ExtStr1, ModuleTargetType)
        ),
        % Not yet implemented. `build_all' targets are only used by
        % tools/bootcheck, so it doesn't really matter.
        ModuleTargetType \= module_target_c_header(_)
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(ModuleTargetType))
    else if
        ExtStr = ".check"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(module_target_errors))
    else if
        ExtStr = ".analyse"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_analyses)
    else if
        ExtStr = ".clean"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_clean)
    else if
        ExtStr = ".realclean"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_realclean)
    else if
        ExtStr = ".install",
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = misc_target(misc_target_install_library)
    else if
        ExtStr = ".doc"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_xml_docs)
    else
        fail
    ),
    file_name_to_module_name(ModuleNameStr, ModuleName).

:- pred search_backwards_for_dot(string::in, int::in, int::out) is semidet.

search_backwards_for_dot(String, Index, DotIndex) :-
    string.unsafe_prev_index(String, Index, CharIndex, Char),
    ( if Char = ('.') then
        DotIndex = CharIndex
    else
        search_backwards_for_dot(String, CharIndex, DotIndex)
    ).

:- func get_executable_type(globals) = linked_target_type.

get_executable_type(Globals) = ExecutableType :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        ExecutableType = executable
    ;
        CompilationTarget = target_csharp,
        ExecutableType = csharp_executable
    ;
        CompilationTarget = target_java,
        ExecutableType = java_executable
    ).

%-----------------------------------------------------------------------------%

:- type last_hash
    --->    last_hash(
                lh_options  :: list(string),
                lh_hash     :: string
            ).

    % Generate the .track_flags files for local modules reachable from the
    % target module. The files contain hashes of the options which are set for
    % that particular module (deliberately ignoring some options), and are only
    % updated if they have changed since the last --make run. We use hashes as
    % the full option tables are quite large.
    %
:- pred make_track_flags_files(globals::in, module_name::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_track_flags_files(Globals, ModuleName, Succeeded, !Info, !IO) :-
    find_reachable_local_modules(Globals, ModuleName, Succeeded0, Modules,
        !Info, !IO),
    (
        Succeeded0 = succeeded,
        KeepGoing = do_not_keep_going,
        DummyLastHash = last_hash([], ""),
        foldl3_maybe_stop_at_error(KeepGoing, make_track_flags_files_2,
            Globals, set.to_sorted_list(Modules), Succeeded,
            DummyLastHash, _LastHash, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred make_track_flags_files_2(globals::in, module_name::in,
    maybe_succeeded::out, last_hash::in, last_hash::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_track_flags_files_2(Globals, ModuleName, Succeeded,
        !LastHash, !Info, !IO) :-
    lookup_mmc_module_options(!.Info ^ mki_options_variables, ModuleName,
        ModuleOptionArgs, LookupSpecs),
    write_error_specs_ignore(Globals, LookupSpecs, !IO),
    LookupErrors = contains_errors(Globals, LookupSpecs),
    (
        LookupErrors = no,
        DetectedGradeFlags = !.Info ^ mki_detected_grade_flags,
        OptionArgs = !.Info ^ mki_option_args,
        AllOptionArgs = DetectedGradeFlags ++ ModuleOptionArgs ++ OptionArgs,

        % The set of options from one module to the next is usually identical,
        % so we can easily avoid running handle_options and stringifying and
        % hashing the option table, all of which can contribute to an annoying
        % delay when mmc --make starts.
        ( if !.LastHash = last_hash(AllOptionArgs, HashPrime) then
            Hash = HashPrime
        else
            option_table_hash(AllOptionArgs, Hash, !IO),
            !:LastHash = last_hash(AllOptionArgs, Hash)
        ),

        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".track_flags")),
            ModuleName, HashFileName, !IO),
        compare_hash_file(Globals, HashFileName, Hash, Same, !IO),
        (
            Same = yes,
            Succeeded = succeeded
        ;
            Same = no,
            write_hash_file(HashFileName, Hash, Succeeded, !IO)
        )
    ;
        LookupErrors = yes,
        Succeeded = did_not_succeed
    ).

:- pred option_table_hash(list(string)::in, string::out,
    io::di, io::uo) is det.

option_table_hash(AllOptionArgs, Hash, !IO) :-
    % This code is part of the --track-flags implementation. We hash the
    % options in the updated globals because they include module-specific
    % options. The hash is then compared with the hash in a MODULE.track_flags
    % file, which is updated if it differs. The new timestamp will later force
    % the module to be recompiled if necessary, but that's later. We are not
    % compiling the module immediately, so this is the only use we have for
    % AllOptionArgsGlobals here.
    %
    % XXX This algorithm processes every option in the option table, even
    % though it does not include all of them in the hash. Virtually all
    % of these options will have their default values, which makes
    % most of that work effectively wasted.
    %
    % A more elegant approach would be to invoke getopt.record_arguments on
    % AllOptionArgs, and hash the resulting list of option_values. This would
    % require hashing special options (such as -ON) as well as non-special
    % values, but the cost of that would be trivial.
    %
    % This approach would have two principal differences from the current one.
    %
    % - First, the current approach computes a different hash if AllOptionArgs
    %   has not changed, but the default value of an (consequential) option
    %   has changed, or if a new consequential option has been added.
    %   The recompilation that this forces will be needed after some changes
    %   to the option defaults, but not after others.
    %
    %   However, this consideration never applies only to a single module;
    %   if the default set of option values changes, it applies for all
    %   modules. Therefore it would be enough to record a hash of the
    %   default values of all options *once* as a "global" value in the
    %   MODULE.track_flags file, which, if it changes, invalidates *every*
    %   module-specific entry in that file.
    %
    % - Second, it is possible for some changes in AllOptionArgs to yield
    %   the same final AllOptionArgsGlobals, if some option in the old
    %   AllOptionArgs implies the value of some other option, and the new
    %   AllOptionArgs explicitly sets this option to the implied value
    %   (or vice versa). In such cases, the new approach would force a
    %   recompilation, while the old one would not. However, the absence
    %   of a recompilation in such an instance could be more worrysome
    %   than welcome for users who do not know about that option implication,
    %   or who do not appreciate its significance.
    %
    handle_given_options(AllOptionArgs, _, _, OptionsErrors,
        AllOptionArgsGlobals, !IO),
    (
        OptionsErrors = []
    ;
        OptionsErrors = [_ | _],
        unexpected($file, $pred ++ ": " ++
            "handle_options returned with errors")
    ),
    globals.get_options(AllOptionArgsGlobals, OptionTable),
    map.to_sorted_assoc_list(OptionTable, OptionList),
    inconsequential_options(InconsequentialOptions),
    InconsequentialOptionsSet = set_tree234.from_set(InconsequentialOptions),
    list.filter(include_option_in_hash(InconsequentialOptionsSet),
        OptionList, HashOptionList),
    globals.get_opt_tuple(AllOptionArgsGlobals, OptTuple),
    Hash = md5sum(string({HashOptionList, OptTuple})).

:- pred include_option_in_hash(set_tree234(option)::in,
    pair(option, option_data)::in) is semidet.

include_option_in_hash(InconsequentialOptionsSet, Option - OptionData) :-
    require_complete_switch [OptionData]
    (
        ( OptionData = bool(_)
        ; OptionData = int(_)
        ; OptionData = string(_)
        ; OptionData = maybe_int(_)
        ; OptionData = maybe_string(_)
        ; OptionData = accumulating(_)
        ),
        % XXX Reconsider if a lot of these options really should be ignored.
        not set_tree234.contains(InconsequentialOptionsSet, Option)
    ;
        ( OptionData = special
        ; OptionData = bool_special
        ; OptionData = int_special
        ; OptionData = string_special
        ; OptionData = maybe_string_special
        ; OptionData = file_special
        ),
        % There is no point hashing special options as the option_data is
        % always the same.
        fail
    ).

:- pred compare_hash_file(globals::in, string::in, string::in, bool::out,
    io::di, io::uo) is det.

compare_hash_file(Globals, FileName, Hash, Same, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read_line_as_string(Stream, ReadResult, !IO),
        (
            ReadResult = ok(Line),
            ( if Line = Hash then
                Same = yes
            else
                Same = no
            )
        ;
            ReadResult = eof,
            Same = no
        ;
            ReadResult = error(_),
            Same = no
        ),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        % Probably missing file.
        Same = no
    ),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbose = yes,
        io.write_string("% ", !IO),
        io.write_string(FileName, !IO),
        (
            Same = yes,
            io.write_string(" does not need updating.\n", !IO)
        ;
            Same = no,
            io.write_string(" will be UPDATED.\n", !IO)
        )
    ;
        Verbose = no
    ).

:- pred write_hash_file(string::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

write_hash_file(FileName, Hash, Succeeded, !IO) :-
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.write_string(Stream, Hash, !IO),
        io.close_output(Stream, !IO),
        Succeeded = succeeded
    ;
        OpenResult = error(Error),
        io.format("Error creating `%s': %s\n",
            [s(FileName), s(io.error_message(Error))], !IO),
        Succeeded = did_not_succeed
    ).

%-----------------------------------------------------------------------------%
:- end_module make.
%-----------------------------------------------------------------------------%
