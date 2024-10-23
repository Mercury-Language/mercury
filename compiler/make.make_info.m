%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the main data structure used by mmc --make,
% as well as a few utility types.
%
%---------------------------------------------------------------------------%

:- module make.make_info.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.deps_cache.
:- import_module make.deps_set.
:- import_module make.options_file.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.read_modules.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

:- type import_or_include
    --->    ioi_import(module_name)
    ;       ioi_include(module_name).

%---------------------%

:- type maybe_module_dep_info
    --->    no_module_dep_info
    ;       some_module_dep_info(module_dep_info).

%---------------------%

:- type file_timestamp_map ==
    map(string, {list(dir_name), maybe_error(timestamp)}).

:- type target_file_timestamp_map ==
    version_hash_table(target_file, timestamp).

%---------------------%

% NOTE Having version_arrays be indexed by uints, not ints
% that just happen to never be negative, would avoid some casts
% from uint to int when accessing the reverse maps in the next two types.

:- type module_index_map
    --->    module_index_map(
                mim_forward_map         :: version_hash_table(module_name,
                                            module_index),
                mim_reverse_map         :: version_array(module_name),
                mim_counter             :: uint
            ).

%---------------------%

:- type dependency_file
    --->    dep_target(target_file)
            % A target which could be made.

    ;       dep_file(file_name).
            % An ordinary file which `mmc --make' does not know how to rebuild.

    % Like dependency_file but refers to a module by index instead of by name,
    % which is more efficient when the name is not required.
    %
:- type dependency_file_with_module_index
    --->    dfmi_target(module_index, module_target_type)
    ;       dfmi_file(file_name).

:- type dependency_file_index_map
    --->    dependency_file_index_map(
                dfim_forward_map        :: version_hash_table(
                                            dependency_file_with_module_index,
                                            dependency_file_index),
                dfim_reverse_map        :: version_array(
                                            dependency_file_with_module_index),
                dfim_counter            :: uint
            ).

%---------------------%

:- type dependency_status
    --->    deps_status_not_considered
    ;       deps_status_being_built
    ;       deps_status_up_to_date
    ;       deps_status_error.

    % An old, previously inappropriately-placed comment says:
    %
    % There should be a definite improvement if we could replace
    % this hash table indexed by dependency_file terms with an array
    % indexed by the uints inside values of type dependency_file_index.
    %
:- type dep_file_status_map ==
    version_hash_table(dependency_file, dependency_status).

%---------------------%

:- type maybe_rebuild_module_deps
    --->    do_rebuild_module_deps
    ;       do_not_rebuild_module_deps.

%---------------------%

:- type maybe_keep_going
    --->    do_not_keep_going
    ;       do_keep_going.

%---------------------%

:- type target_type
    --->    module_target(module_target_type)
    ;       linked_target(linked_target_type)
    ;       misc_target(misc_target_type).

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
    ;       module_target_fact_table_object(pic, file_name)
    ;       module_target_xml_doc.

:- type c_header_type
    --->    header_mh    % For `:- pragma foreign_export' declarations.
    ;       header_mih.  % Declarations for hlc grades, for compiler use only.

% :- type linked_target_type is in compile_target_code.m.

:- type misc_target_type
    --->    misc_target_clean
    ;       misc_target_realclean
    ;       misc_target_build_all(module_target_type)
    ;       misc_target_build_analyses
    ;       misc_target_build_library
    ;       misc_target_install_library
            % misc_target_install_library installs
            %
            % - non-grade-specific files for the current grade and
            % - grade-specific files for all libgrades,
            %
            % - for the LEGACY directory structure always, and
            % - for the PROPOSED directory structure with --experiment4.
    ;       misc_target_install_library_gs_gas
            % misc_target_install_library_gs_gas installs
            %
            % - grade-specific files for the current grade,
            %
            % - for the PROPOSED directory structure.
    ;       misc_target_build_xml_docs.

:- type compilation_task_type
    --->    process_module(module_compilation_task_type)
            % The `pic' argument is only used for `--target c'.
    ;       target_code_to_object_code(pic)
    ;       fact_table_code_to_object_code(pic, file_name).

:- type module_compilation_task_type
    --->    task_errorcheck
    ;       task_make_int0
    ;       task_make_int12     % makes both .int and .int2
    ;       task_make_int3
    ;       task_make_opt
    ;       task_make_analysis_registry
    ;       task_compile_to_c
    ;       task_compile_to_java
    ;       task_compile_to_csharp
    ;       task_make_xml_doc.

%---------------------------------------------------------------------------%

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

:- type top_target_file
    --->    top_target_file(
                ttf_name            :: module_name,
                ttf_type            :: target_type
            ).

%---------------------------------------------------------------------------%

:- type make_error
    --->    make_error_target(target_file)
    ;       make_error_dependencies(module_name)
    ;       make_error_other(string).

%---------------------------------------------------------------------------%

:- type make_info.

:- func init_make_info(options_variables, list(string), maybe_keep_going,
    list(string), list(string), set(top_target_file), int,
    target_file_timestamp_map, module_index_map, dependency_file_index_map,
    dep_file_status_map) = make_info.

:- func make_info_get_options_variables(make_info) = options_variables.
:- func make_info_get_detected_grade_flags(make_info) = list(string).
:- func make_info_get_keep_going(make_info) = maybe_keep_going.
:- func make_info_get_env_var_args(make_info) = list(string).
:- func make_info_get_option_args(make_info) = list(string).
:- func make_info_get_command_line_targets(make_info) = set(top_target_file).
:- func make_info_get_rebuild_module_deps(make_info) =
    maybe_rebuild_module_deps.
:- func make_info_get_reanalysis_passes(make_info) = int.
:- func make_info_get_maybe_module_dep_info_map(make_info) =
    map(module_name, maybe_module_dep_info).
:- func make_info_get_file_timestamp_map(make_info) = file_timestamp_map.
:- func make_info_get_target_file_timestamp_map(make_info) =
    target_file_timestamp_map.
:- func make_info_get_module_index_map(make_info) = module_index_map.
:- func make_info_get_dep_file_index_map(make_info) =
    dependency_file_index_map.
:- func make_info_get_dep_file_status_map(make_info) = dep_file_status_map.
:- func make_info_get_error_file_modules(make_info) = set(module_name).
:- func make_info_get_importing_module(make_info) = maybe(import_or_include).
:- func make_info_get_maybe_stdout_lock(make_info) = maybe(stdout_lock).
:- func make_info_get_mi_read_module_maps(make_info) = have_parse_tree_maps.
:- func make_info_get_direct_imports_non_intermod_cache(make_info) =
    module_to_module_set_cache.
:- func make_info_get_direct_imports_intermod_cache(make_info) =
    module_to_module_set_cache.
% :- func make_info_get_indirect_imports_non_intermod_cache(make_info) =
%     module_to_module_set_cache.
:- func make_info_get_indirect_imports_intermod_cache(make_info) =
    module_to_module_set_cache.
:- func make_info_get_foreign_imports_non_intermod_trans_cache(make_info) =
    module_to_module_set_cache.
% :- func make_info_get_anc0_dir1_indir2_non_intermod_cache(make_info) =
%     module_to_dep_file_set_cache.
:- func make_info_get_anc0_dir1_indir2_intermod_cache(make_info) =
    module_to_dep_file_set_cache.
:- func make_info_get_trans_deps_cache(make_info) = trans_deps_cache.

:- pred make_info_set_option_args(list(string)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_command_line_targets(set(top_target_file)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_rebuild_module_deps(maybe_rebuild_module_deps::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_reanalysis_passes(int::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_maybe_module_dep_info_map(
    map(module_name, maybe_module_dep_info)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_file_timestamp_map(file_timestamp_map::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_target_file_timestamp_map(target_file_timestamp_map::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_module_index_map(module_index_map::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_dep_file_index_map(dependency_file_index_map::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_dep_file_status_map(dep_file_status_map::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_error_file_modules(set(module_name)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_importing_module(maybe(import_or_include)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_maybe_stdout_lock(maybe(stdout_lock)::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_direct_imports_non_intermod_cache(
    module_to_module_set_cache::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_direct_imports_intermod_cache(
    module_to_module_set_cache::in,
    make_info::in, make_info::out) is det.
% :- pred make_info_set_indirect_imports_non_intermod_cache(
%     module_to_module_set_cache::in,
%     make_info::in, make_info::out) is det.
:- pred make_info_set_indirect_imports_intermod_cache(
    module_to_module_set_cache::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_foreign_imports_non_intermod_trans_cache(
    module_to_module_set_cache::in,
    make_info::in, make_info::out) is det.
% :- pred make_info_set_anc0_dir1_indir2_non_intermod_cache(
%     module_to_dep_file_set_cache::in,
%     make_info::in, make_info::out) is det.
:- pred make_info_set_anc0_dir1_indir2_intermod_cache(
    module_to_dep_file_set_cache::in,
    make_info::in, make_info::out) is det.
:- pred make_info_set_trans_deps_cache(trans_deps_cache::in,
    make_info::in, make_info::out) is det.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

:- type make_info
    --->    make_info(
                % The first few fields are read-only.

                % The contents of the Mercury.options file.
                mki_options_variables   :: options_variables,

                % Any flags required to set detected library grades.
                mki_detected_grade_flags :: list(string),

                % The value of the --keep-going option.
                mki_keep_going          :: maybe_keep_going,

                % A sequence of option values that express the values
                % of environment variables such as MERCURY_COLOR_SCHEME
                % and NO_COLOR.
                mki_env_var_args        :: list(string),

                % The remaining fields are read-write.

                % Initially, the original set of options passed to mmc,
                % not including the targets to be made.
                % Can be modified later,
                mki_option_args         :: list(string),

                % Initially, the targets specified on the command line.
                % Can be modified later,
                mki_command_line_targets :: set(top_target_file),

                % Should the `.module_dep' files be rebuilt?
                % Set to `do_not_rebuild_module_deps' for `mmc --make clean'.
                mki_rebuild_module_deps :: maybe_rebuild_module_deps,

                % The remaining number of analysis passes that we will allow
                % on `suboptimal' modules. It starts at the value of
                % `--analysis-repeat' and decrements to zero as analysis passes
                % on `suboptimal' modules are performed. `invalid' modules
                % are not affected as they will always be reanalysed.
                mki_reanalysis_passes   :: int,

                % For modules whose sources we have read in, the
                % maybe_module_dep_info will contain a burdened_module.
                % For modules for which we have read only the .dep file,
                % maybe_module_dep_info will contain a module_dep_summary.
                %
                % An old comment about the former says:
                %
                % "The items field of each module_and_imports structure should
                % be empty -- we are not trying to cache the items here",
                % but I (zs) don't whether that is actually true.
                % (The burdened_module structure replaced the old
                % module_and_imports structure.)
                mki_maybe_module_dep_info_map :: map(module_name,
                                            maybe_module_dep_info),

                % A map of last known timestamps by file name. This assumes
                % that no external process is updating the same files without
                % our knowledge.
                %
                % If a file is updated or removed, then you need to remove an
                % entry from this map. Also, there may be an entry in the
                % following map for a target_file that needs to be invalidated.
                % If that is difficult, it is simplest to reset the
                % mki_target_file_timestamps map.
                mki_file_timestamp_map  :: file_timestamp_map,

                % A map of last known timestamps for the file that corresponds
                % to a target_file.
                mki_target_file_timestamp_map :: target_file_timestamp_map,

                % The mapping between module_names and indices.
                mki_module_index_map    :: module_index_map,

                % The mapping between dependency_files and indices.
                mki_dep_file_index_map  :: dependency_file_index_map,

                mki_dep_file_status_map :: dep_file_status_map,

                % Modules for which we have redirected output
                % to a `.err' file during this invocation of mmc.
                mki_error_file_modules  :: set(module_name),

                % Used for reporting which module imported or included
                % a nonexistent module.
                %
                % This field is initialized to `no', and is set to `yes'
                % only in do_find_transitive_module_dependencies_uncached,
                % and then only temporarily. The only users of this field
                % are two calls to maybe_write_importing_module in
                % make.module_dep_file.m. We *could* therefore delete
                % this field from the make_info, and pass its contents
                % as separate arguments. However, that would require passing
                % this info through a whole bunch of unrelated predicates.
                % Keeping this field in make_info is the lesser evil.
                mki_importing_module    :: maybe(import_or_include),

                % An inter-process lock to prevent multiple processes
                % interleaving their output to standard output.
                mki_maybe_stdout_lock   :: maybe(stdout_lock),

                % The parse trees of the files we have read so far,
                % so we never have to read and parse each file more than once.
                %
                % XXX This field is not actually used yet.
                mki_mi_read_module_maps :: have_parse_tree_maps,

                % For each module, the set of modules for which the `.int'
                % files are read, excluding those read as a result of reading
                % `.opt' files. The bool records whether there was an error
                % in the dependencies.
                % XXX Use a better representation for the sets.
                % XXX zs: What bool? What sets?
                mki_direct_imports_non_intermod_cache
                                        :: module_to_module_set_cache,
                mki_direct_imports_intermod_cache
                                        :: module_to_module_set_cache,

%               mki_indirect_imports_non_intermod_cache
%                                       :: module_to_module_set_cache,
                mki_indirect_imports_intermod_cache
                                        :: module_to_module_set_cache,

                mki_foreign_imports_non_intermod_trans_cache
                                        :: module_to_module_set_cache,

                % This cache holds dependency sets that are a simple
                % computation (union) on other dependency sets.
%               mki_anc0_dir1_indir2_non_intermod_cache
%                                       :: module_to_dep_file_set_cache,
                mki_anc0_dir1_indir2_intermod_cache
                                        :: module_to_dep_file_set_cache,

                % The boolean is `yes' if the result is complete.
                % XXX Use a better representation for the sets.
                % XXX zs: What sets?
                mki_trans_deps_cache    :: trans_deps_cache
            ).

init_make_info(OptionsVariables, DetectedGradeFlags, KeepGoing,
        EnvVarArgs, OptionArgs, CmdLineTargets, AnalysisRepeat,
        TargetTimestamps, ModuleIndexMap, DepIndexMap, DepStatusMap)
        = MakeInfo :-
    map.init(ModuleDependencies),
    map.init(FileTimestamps),
    ShouldRebuildModuleDeps = do_rebuild_module_deps,
    set.init(ErrorFileModules),
    MaybeImportingModule = maybe.no,
    MaybeStdoutLock = maybe.no,
    MakeInfo = make_info(
        OptionsVariables,
        DetectedGradeFlags,
        KeepGoing,
        EnvVarArgs,
        OptionArgs,
        CmdLineTargets,
        ShouldRebuildModuleDeps,
        AnalysisRepeat,
        ModuleDependencies,
        FileTimestamps,
        TargetTimestamps,
        ModuleIndexMap,
        DepIndexMap,
        DepStatusMap,
        ErrorFileModules,
        MaybeImportingModule,
        MaybeStdoutLock,
        init_have_parse_tree_maps,
        init_module_to_module_set_cache,
        init_module_to_module_set_cache,
%       init_module_to_module_set_cache,
        init_module_to_module_set_cache,
        init_module_to_module_set_cache,
%       init_module_to_dep_file_set_cache,
        init_module_to_dep_file_set_cache,
        init_trans_deps_cache
    ).

make_info_get_options_variables(Info) = X :-
    X = Info ^ mki_options_variables.
make_info_get_detected_grade_flags(Info) = X :-
    X = Info ^ mki_detected_grade_flags.
make_info_get_keep_going(Info) = X :-
    X = Info ^ mki_keep_going.
make_info_get_env_var_args(Info) = X :-
    X = Info ^ mki_env_var_args.
make_info_get_option_args(Info) = X :-
    X = Info ^ mki_option_args.
make_info_get_command_line_targets(Info) = X :-
    X = Info ^ mki_command_line_targets.
make_info_get_rebuild_module_deps(Info) = X :-
    X = Info ^ mki_rebuild_module_deps.
make_info_get_reanalysis_passes(Info) = X :-
    X = Info ^ mki_reanalysis_passes.
make_info_get_maybe_module_dep_info_map(Info) = X :-
    X = Info ^ mki_maybe_module_dep_info_map.
make_info_get_file_timestamp_map(Info) = X :-
    X = Info ^ mki_file_timestamp_map.
make_info_get_target_file_timestamp_map(Info) = X :-
    X = Info ^ mki_target_file_timestamp_map.
make_info_get_module_index_map(Info) = X :-
    X = Info ^ mki_module_index_map.
make_info_get_dep_file_index_map(Info) = X :-
    X = Info ^ mki_dep_file_index_map.
make_info_get_dep_file_status_map(Info) = X :-
    X = Info ^ mki_dep_file_status_map.
make_info_get_error_file_modules(Info) = X :-
    X = Info ^ mki_error_file_modules.
make_info_get_importing_module(Info) = X :-
    X = Info ^ mki_importing_module.
make_info_get_maybe_stdout_lock(Info) = X :-
    X = Info ^ mki_maybe_stdout_lock.
make_info_get_mi_read_module_maps(Info) = X :-
    X = Info ^ mki_mi_read_module_maps.
make_info_get_direct_imports_non_intermod_cache(Info) = X :-
    X = Info ^ mki_direct_imports_non_intermod_cache.
make_info_get_direct_imports_intermod_cache(Info) = X :-
    X = Info ^ mki_direct_imports_intermod_cache.
% make_info_get_indirect_imports_non_intermod_cache(Info) = X :-
%     X = Info ^ mki_indirect_imports_non_intermod_cache.
make_info_get_indirect_imports_intermod_cache(Info) = X :-
    X = Info ^ mki_indirect_imports_intermod_cache.
make_info_get_foreign_imports_non_intermod_trans_cache(Info) = X :-
    X = Info ^ mki_foreign_imports_non_intermod_trans_cache.
% make_info_get_anc0_dir1_indir2_non_intermod_cache(Info) = X :-
%     X = Info ^ mki_anc0_dir1_indir2_non_intermod_cache.
make_info_get_anc0_dir1_indir2_intermod_cache(Info) = X :-
    X = Info ^ mki_anc0_dir1_indir2_intermod_cache.
make_info_get_trans_deps_cache(Info) = X :-
    X = Info ^ mki_trans_deps_cache.

make_info_set_option_args(X, !Info) :-
    !Info ^ mki_option_args := X.
make_info_set_command_line_targets(X, !Info) :-
    !Info ^ mki_command_line_targets := X.
make_info_set_rebuild_module_deps(X, !Info) :-
    !Info ^ mki_rebuild_module_deps := X.
make_info_set_reanalysis_passes(X, !Info) :-
    !Info ^ mki_reanalysis_passes := X.
make_info_set_maybe_module_dep_info_map(X, !Info) :-
    !Info ^ mki_maybe_module_dep_info_map := X.
make_info_set_file_timestamp_map(X, !Info) :-
    !Info ^ mki_file_timestamp_map := X.
make_info_set_target_file_timestamp_map(X, !Info) :-
    !Info ^ mki_target_file_timestamp_map := X.
make_info_set_module_index_map(X, !Info) :-
    !Info ^ mki_module_index_map := X.
make_info_set_dep_file_index_map(X, !Info) :-
    !Info ^ mki_dep_file_index_map := X.
make_info_set_dep_file_status_map(X, !Info) :-
    !Info ^ mki_dep_file_status_map := X.
make_info_set_error_file_modules(X, !Info) :-
    !Info ^ mki_error_file_modules := X.
make_info_set_importing_module(X, !Info) :-
    !Info ^ mki_importing_module := X.
make_info_set_maybe_stdout_lock(X, !Info) :-
    !Info ^ mki_maybe_stdout_lock := X.
make_info_set_direct_imports_non_intermod_cache(X, !Info) :-
    !Info ^ mki_direct_imports_non_intermod_cache := X.
make_info_set_direct_imports_intermod_cache(X, !Info) :-
    !Info ^ mki_direct_imports_intermod_cache := X.
% make_info_set_indirect_imports_non_intermod_cache(X, !Info) :-
%     !Info ^ mki_indirect_imports_non_intermod_cache := X.
make_info_set_indirect_imports_intermod_cache(X, !Info) :-
    !Info ^ mki_indirect_imports_intermod_cache := X.
make_info_set_foreign_imports_non_intermod_trans_cache(X, !Info) :-
    !Info ^ mki_foreign_imports_non_intermod_trans_cache := X.
% make_info_set_anc0_dir1_indir2_non_intermod_cache(X, !Info) :-
%     !Info ^ mki_anc0_dir1_indir2_non_intermod_cache := X.
make_info_set_anc0_dir1_indir2_intermod_cache(X, !Info) :-
    !Info ^ mki_anc0_dir1_indir2_intermod_cache := X.
make_info_set_trans_deps_cache(X, !Info) :-
    !Info ^ mki_trans_deps_cache := X.

%---------------------------------------------------------------------------%
:- end_module make.make_info.
%---------------------------------------------------------------------------%
