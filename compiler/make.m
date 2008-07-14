%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2008 The University of Melbourne.
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

:- include_module make.options_file.
:- include_module make.util.

:- import_module make.options_file.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_io.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % make.process_args(OptionArgs, NonOptionArgs).
    %
:- pred make_process_args(options_variables::in, list(string)::in,
    list(file_name)::in, io::di, io::uo) is det.

:- pred make_write_module_dep_file(module_imports::in, io::di, io::uo) is det.

:- func make_module_dep_file_extension = string.

:- type make_info.

:- type rebuild_module_deps
    --->    do_rebuild_module_deps
    ;       do_not_rebuild_module_deps.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module make.dependencies.
:- include_module make.module_dep_file.
:- include_module make.module_target.
:- include_module make.program_target.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module hlds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.dependencies.
:- import_module make.module_dep_file.
:- import_module make.module_target.
:- import_module make.program_target.
:- import_module make.util.
:- import_module parse_tree.error_util.
:- import_module top_level.                 % XXX unwanted dependency
:- import_module top_level.mercury_compile. % XXX unwanted dependency

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module sparse_bitset.
:- import_module version_array.
:- import_module version_hash_table.

%-----------------------------------------------------------------------------%

:- type make_info
    --->    make_info(
                % The items field of each module_imports structure should be
                % empty -- we're not trying to cache the items here.
                module_dependencies     :: map(module_name,
                                            maybe(module_imports)),

                file_timestamps         :: file_timestamps,

                % Cache chosen file names for a module name and extension.
                search_file_name_cache  :: map(pair(module_name, string),
                                            file_name),

                % The original set of options passed to mmc, not including
                % the targets to be made.
                option_args             :: list(string),

                % The contents of the Mercury.options file.
                options_variables       :: options_variables,

                % The mapping between module_names and indices.
                module_index_map        :: module_index_map,

                % The mapping between dependency_files and indices.
                dep_file_index_map      :: dependency_file_index_map,

                dependency_status       :: version_hash_table(dependency_file,
                                            dependency_status),

                % For each module, the set of modules for which the `.int'
                % files are read, excluding those read as a result of reading
                % `.opt' files. The bool records whether there was an error
                % in the dependencies.
                % XXX Use a better representation for the sets.
                cached_direct_imports   :: cached_direct_imports,

                cached_non_intermod_direct_imports
                                        :: cached_direct_imports,

                % The boolean is `yes' if the result is complete.
                % XXX Use a better representation for the sets.
                cached_transitive_dependencies
                                        :: cached_transitive_dependencies,

                cached_foreign_imports  :: cached_foreign_imports,

                % Should the `.module_dep' files be rebuilt.
                % Set to `no' for `mmc --make clean'.
                rebuild_module_deps     :: rebuild_module_deps,

                keep_going              :: bool,

                % Modules for which we have redirected output
                % to a `.err' file during this invocation of mmc.
                error_file_modules      :: set(module_name),

                % Used for reporting which module imported a nonexistent
                % module.
                importing_module        :: maybe(module_name),

                % Targets specified on the command line.
                command_line_targets    :: set(pair(module_name, target_type)),

                % The remaining number of analysis passes that we will allow on
                % `suboptimal' modules. It starts at the value of
                % `--analysis-repeat' and decrements to zero as analysis passes
                % on `suboptimal' modules are performed. `invalid' modules
                % are not affected as they will always be reanalysed.
                reanalysis_passes       :: int
            ).

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

            % The `pic' argument is only used for
            % `--target c' and `--target asm'.
    ;       target_code_to_object_code(pic)
    ;       foreign_code_to_object_code(pic, foreign_language)
    ;       fact_table_code_to_object_code(pic, file_name).

:- type module_compilation_task_type
    --->    task_errorcheck
    ;       task_make_short_interface
    ;       task_make_interface
    ;       task_make_private_interface
    ;       task_make_optimization_interface
    ;       task_make_analysis_registry
    ;       task_compile_to_target_code
    ;       task_make_xml_doc.

:- type module_target_type
    --->    module_target_source
    ;       module_target_errors
    ;       module_target_private_interface
    ;       module_target_long_interface
    ;       module_target_short_interface
    ;       module_target_unqualified_short_interface
    ;       module_target_intermodule_interface
    ;       module_target_analysis_registry
    ;       module_target_c_header(c_header_type)
    ;       module_target_c_code
    ;       module_target_il_code
    ;       module_target_il_asm
    ;       module_target_java_code
    ;       module_target_erlang_header
    ;       module_target_erlang_code
    ;       module_target_erlang_beam_code
    ;       module_target_asm_code(pic)
    ;       module_target_object_code(pic)
    ;       module_target_foreign_il_asm(foreign_language)
    ;       module_target_foreign_object(pic, foreign_language)
    ;       module_target_fact_table_object(pic, file_name)
    ;       module_target_xml_doc.

:- type c_header_type
    --->    header_mh      % For `:- pragma foreign_export' declarations.
    ;       header_mih.    % Declarations for hlc grades, for compiler use only.

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

%-----------------------------------------------------------------------------%

make_write_module_dep_file(Imports, !IO) :-
    make.module_dep_file.write_module_dep_file(Imports, !IO).

make_module_dep_file_extension = ".module_dep".

make_process_args(Variables, OptionArgs, Targets0, !IO) :-
    (
        Targets0 = [],
        lookup_main_target(Variables, MaybeMAIN_TARGET, !IO),
        (
            MaybeMAIN_TARGET = yes(Targets),
            (
                Targets = [_ | _],
                Continue0 = yes
            ;
                Targets = [],
                Continue0 = no,
                io.write_string("** Error: no targets specified " ++
                    "and `MAIN_TARGET' not defined.\n", !IO)
            )
        ;
            MaybeMAIN_TARGET = no,
            Targets = [],
            Continue0 = no
        )
    ;
        Targets0 = [_ | _],
        Continue0 = yes,
        Targets = Targets0
    ),
    %
    % Ensure none of the targets contains the directory_separator. This is not
    % supported by the rest of the code.
    %
    list.filter(
        (pred(Target::in) is semidet :-
            string.contains_char(Target, dir.directory_separator)
        ), Targets, AbsTargets),
    (
        AbsTargets = [],
        Continue = Continue0
    ;
        AbsTargets = [_ | _],
        Continue = no,
        list.foldl(
            (pred(Target::in, !.I::di, !:I::uo) is det :-
                error_util.write_error_plain_with_progname(Target,
                  "Make target must not contain any directory component.", !I)
            ), AbsTargets, !IO)
    ),
    (
        Continue = no,
        io.set_exit_status(1, !IO)
    ;
        Continue = yes,
        globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
        globals.io_get_globals(Globals, !IO),

        ModuleIndexMap = module_index_map(
            version_hash_table.new_default(module_name_double_hash),
            version_array.empty, 0),
        DepIndexMap = dependency_file_index_map(
            version_hash_table.new_default(dependency_file_double_hash),
            version_array.empty, 0),
        DepStatusMap = version_hash_table.new_default(
            dependency_file_double_hash),

        %
        % Accept and ignore `.depend' targets.  `mmc --make' does not
        % need a separate make depend step. The dependencies for each
        % module are regenerated on demand.
        %
        NonDependTargets = list.filter(
            (pred(Target::in) is semidet :-
                \+ string.suffix(Target, ".depend")
            ), Targets),

        %
        % Classify the remaining targets.
        %
        list.map(classify_target(Globals), NonDependTargets,
            ClassifiedTargets),

        ShouldRebuildModuleDeps = do_rebuild_module_deps,
        globals.io_lookup_int_option(analysis_repeat, AnalysisRepeat, !IO),

        MakeInfo0 = make_info(map.init, map.init, map.init,
            OptionArgs, Variables,
            ModuleIndexMap,
            DepIndexMap,
            DepStatusMap,
            init_cached_direct_imports,
            init_cached_direct_imports,
            init_cached_transitive_dependencies,
            init_cached_foreign_imports,
            ShouldRebuildModuleDeps, KeepGoing,
            set.init, no, set.list_to_set(ClassifiedTargets),
            AnalysisRepeat),

        %
        % Build the targets, stopping on any errors if
        % `--keep-going' was not set.
        %
        foldl2_maybe_stop_at_error(KeepGoing, make_target,
            ClassifiedTargets, Success, MakeInfo0, _MakeInfo, !IO),

        (
            Success = no,
            io.set_exit_status(1, !IO)
        ;
            Success = yes
        )
    ).

:- pred make_target(pair(module_name, target_type)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_target(Target, Success, !Info, !IO) :-
    Target = ModuleName - TargetType,
    (
        TargetType = module_target(ModuleTargetType),
        TargetFile = target_file(ModuleName, ModuleTargetType),
        make_module_target(dep_target(TargetFile), Success,
            !Info, !IO)
    ;
        TargetType = linked_target(ProgramTargetType),
        LinkedTargetFile = linked_target_file(ModuleName, ProgramTargetType),
        make_linked_target(LinkedTargetFile, Success, !Info, !IO)
    ;
        TargetType = misc_target(MiscTargetType),
        make_misc_target(ModuleName - MiscTargetType, Success, !Info, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type target_type
    --->    module_target(module_target_type)
    ;       linked_target(linked_target_type)
    ;       misc_target(misc_target_type).

:- pred classify_target(globals::in, string::in,
    pair(module_name, target_type)::out) is det.

classify_target(Globals, FileName, ModuleName - TargetType) :-
    (
        string.length(FileName, NameLength),
        search_backwards_for_dot(FileName, NameLength - 1, DotLocn),
        string.split(FileName, DotLocn, ModuleNameStr0, Suffix),
        solutions(classify_target_2(Globals, ModuleNameStr0, Suffix),
            TargetFiles),
        TargetFiles = [TargetFile]
    ->
        TargetFile = ModuleName - TargetType
    ;
        string.append("lib", ModuleNameStr, FileName)
    ->
        TargetType = misc_target(misc_target_build_library),
        file_name_to_module_name(ModuleNameStr, ModuleName)
    ;
        TargetType = linked_target(executable),
        file_name_to_module_name(FileName, ModuleName)
    ).

:- pred classify_target_2(globals::in, string::in, string::in,
    pair(module_name, target_type)::out) is nondet.

classify_target_2(Globals, ModuleNameStr0, Suffix, ModuleName - TargetType) :-
    (
        yes(Suffix) = target_extension(Globals, ModuleTargetType)
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = module_target(ModuleTargetType)
    ;
        globals.lookup_string_option(Globals, library_extension, Suffix),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    ->
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(static_library)
    ;
        globals.lookup_string_option(Globals, shared_library_extension,
            Suffix),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    ->
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(shared_library)
    ;
        globals.lookup_string_option(Globals, executable_file_extension,
            Suffix)
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = linked_target(executable)
    ;
        Suffix = ".beams",
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    ->
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(erlang_archive)
    ;
        string.append(Suffix1, "s", Suffix),
        yes(Suffix1) = target_extension(Globals, ModuleTargetType),
        % Not yet implemented. `build_all' targets are only used by
        % tools/bootcheck, so it doesn't really matter.
        ModuleTargetType \= module_target_c_header(_)
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(ModuleTargetType))
    ;
        Suffix = ".check"
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(module_target_errors))
    ;
        Suffix = ".analyse"
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_analyses)
    ;
        Suffix = ".clean"
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_clean)
    ;
        Suffix = ".realclean"
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_realclean)
    ;
        Suffix = ".install",
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    ->
        ModuleNameStr = ModuleNameStr1,
        TargetType = misc_target(misc_target_install_library)
    ;
        Suffix = ".doc"
    ->
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_xml_docs)
    ;
        fail
    ),
    file_name_to_module_name(ModuleNameStr, ModuleName).

:- pred search_backwards_for_dot(string::in, int::in, int::out) is semidet.

search_backwards_for_dot(String, Index, DotIndex) :-
    Index >= 0,
    ( string.index_det(String, Index, '.') ->
        DotIndex = Index
    ;
        search_backwards_for_dot(String, Index - 1, DotIndex)
    ).

%-----------------------------------------------------------------------------%
:- end_module make.
%-----------------------------------------------------------------------------%
