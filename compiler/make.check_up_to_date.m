%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.check_up_to_date.m.
%
% Code to check whether the files that another file depends on are up-to-date.
%
%---------------------------------------------------------------------------%

:- module make.check_up_to_date.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.timestamp.
:- import_module make.make_info.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type dependency_status_result
    --->    dependency_status_result(
                dependency_file,
                maybe(file_name),
                dependency_status
            ).

:- pred get_dependency_file_status(io.text_output_stream::in, globals::in,
    dependency_file::in, dependency_status_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type dependencies_result
    --->    deps_up_to_date
    ;       deps_out_of_date
    ;       deps_error.

    % check_dependencies(ProgressStream, Globals,
    %   TargetFileName, TargetFileTimestamp,
    %   BuildDepsSucceeded, Dependencies, Result, !IO)
    %
    % Check that all the dependency targets are up-to-date.
    %
:- pred check_dependencies(io.text_output_stream::in, globals::in,
    file_name::in, maybe_error(timestamp)::in, maybe_succeeded::in,
    list(dependency_file)::in, dependencies_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % check_dependencies(ProgressStream, Globals,
    %   TargetFileName, TargetFileTimestamp,
    %   BuildDepsSucceeded, Dependencies, Result, !IO)
    %
    % Check that all the dependency files are up-to-date.
    %
:- pred check_dependency_timestamps(io.text_output_stream::in, globals::in,
    file_name::in, maybe_error(timestamp)::in, maybe_succeeded::in,
    list(dependency_status_result)::in, list(maybe_error(timestamp))::in,
    dependencies_result::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module make.file_names.
:- import_module make.get_module_dep_info.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_dep_info.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module pair.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

get_dependency_file_status(ProgressStream, Globals, Dep, Result, !Info, !IO) :-
    (
        Dep = dep_file(TargetFileName),
        DepStatusMap0 = make_info_get_dep_file_status_map(!.Info),
        ( if version_hash_table.search(DepStatusMap0, Dep, StatusPrime) then
            Status = StatusPrime
        else
            get_dependency_timestamp(ProgressStream, Globals,
                Dep, MaybeTimestamp, !Info, !IO),
            (
                MaybeTimestamp = ok(_),
                Status = deps_status_up_to_date
            ;
                MaybeTimestamp = error(Error),
                Status = deps_status_error,
                io.format(ProgressStream, "** Error: %s\n", [s(Error)], !IO)
            ),
            version_hash_table.det_insert(Dep, Status,
                DepStatusMap0, DepStatusMap),
            make_info_set_dep_file_status_map(DepStatusMap, !Info)
        ),
        Result = dependency_status_result(Dep, yes(TargetFileName), Status)
    ;
        Dep = dep_target(Target),
        Target = target_file(ModuleName, FileType),
        (
            ( FileType = module_target_source
            ; FileType = module_target_track_flags
            ),
            % Source files are always up-to-date.
            % .track_flags should already have been made, if required,
            % so are also up-to-date.
            ModuleTarget = module_target(module_target_source),
            TopTargetFile = top_target_file(ModuleName, ModuleTarget),
            module_target_file_to_file_name(Globals, $pred, Target,
                TargetFileName, !IO),
            MaybeTargetFileName = yes(TargetFileName),
            maybe_warn_up_to_date_target_msg(Globals, TopTargetFile,
                TargetFileName, !Info, UpToDateMsg),
            maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
            Status = deps_status_up_to_date
        ;
            ( FileType = module_target_errors
            ; FileType = module_target_int0
            ; FileType = module_target_int1
            ; FileType = module_target_int2
            ; FileType = module_target_int3
            ; FileType = module_target_opt
            ; FileType = module_target_analysis_registry
            ; FileType = module_target_c_header(_)
            ; FileType = module_target_c_code
            ; FileType = module_target_csharp_code
            ; FileType = module_target_java_code
            ; FileType = module_target_java_class_code
            ; FileType = module_target_object_code(_)
            ; FileType = module_target_foreign_object(_, _)
            ; FileType = module_target_fact_table_object(_, _)
            ; FileType = module_target_xml_doc
            ),
            DepStatusMap0 = make_info_get_dep_file_status_map(!.Info),
            % XXX The management of dependency file status map here
            % is incorrect.
            %
            % The code here checks whether Dep is in DepStatusMap0,
            % and if it is not there, it computes its Status, and then
            % inserts that Status into DepStatusMap. So far so good.
            % The problem is that
            %
            % - the code of the else-part calls get_maybe_module_dep_info
            % - which calls maybe_get_maybe_module_dep_info
            % - which calls do_get_maybe_module_dep_info
            % - which calls write_module_dep_files_for_source_file
            % - which calls record_made_target
            % - which calls record_made_target_given_maybe_touched_files
            % - which calls update_target_status
            %
            % which adds an entry to the dependency file status map.
            % This entry CAN be for Dep, and if it is, then the call to
            % version_hash_table.det_insert at the end of the else-part
            % will throw an exception that leads to a compiler abort.
            % The command "mmc --make --options-file xyz after_end_module.int3"
            % in tests/invalid_nodepend exhibits this behavior as of
            % 2023 oct 23, provided the given options file sets things up
            % properly for mmc --make.
            %
            % Unfortunately, the right way to fix this is not clear.
            % For example, replacing the version_hash_table.det_insert below
            % with version_hash_table.set would fix this symptom, but I (zs)
            % think that is unlikely to fix the underlying problem.
            ( if
                version_hash_table.search(DepStatusMap0, Dep, StatusPrime)
            then
                Status = StatusPrime,
                % In this common case, our caller does not need
                % the target file name. Calling get_make_target_file_name
                % to construct the target file name would therefore be
                % an unnecessary cost, and as it happens, it would be
                % an unnecessary LARGE cost in execution time.
                MaybeTargetFileName = no
            else
                module_target_file_to_file_name(Globals, $pred,
                    Target, TargetFileName, !IO),
                MaybeTargetFileName = yes(TargetFileName),
                get_maybe_module_dep_info(ProgressStream, Globals,
                    ModuleName, MaybeModuleDepInfo, !Info, !IO),
                (
                    MaybeModuleDepInfo = no_module_dep_info,
                    Status = deps_status_error
                ;
                    MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
                    module_dep_info_get_source_file_dir(ModuleDepInfo,
                        ModuleDir),
                    ( if ModuleDir = dir.this_directory then
                        Status = deps_status_not_considered
                    else
                        % Targets from libraries are always considered to be
                        % up-to-date if they exist.
                        get_target_timestamp(ProgressStream, Globals,
                            do_search, Target, MaybeTimestamp, !Info, !IO),
                        (
                            MaybeTimestamp = ok(_),
                            Status = deps_status_up_to_date
                        ;
                            MaybeTimestamp = error(Error),
                            Status = deps_status_error,
                            string.format(
                                "** Error: file `%s' not found: %s\n",
                                [s(TargetFileName), s(Error)], ErrorMsg),
                            % XXX MAKE_STREAM
                            % Try to write this with one call to avoid
                            % interleaved output when doing parallel builds.
                            io.write_string(ProgressStream, ErrorMsg, !IO)
                        )
                    )
                ),
                DepStatusMap1 = make_info_get_dep_file_status_map(!.Info),
                version_hash_table.det_insert(Dep, Status,
                    DepStatusMap1, DepStatusMap),
                make_info_set_dep_file_status_map(DepStatusMap, !Info)
            )
        ),
        Result = dependency_status_result(Dep, MaybeTargetFileName, Status)
    ).

    % This type is similar to dependency_status_result, but its second argument
    % has type file_name, not maybe(file_name).
:- type dependency_status_known_file
    --->    dependency_status_known_file(
                dependency_file,
                file_name,
                dependency_status
            ).

:- pred get_dependency_file_names(globals::in,
    list(dependency_status_result)::in,
    list(dependency_status_known_file)::out, io::di, io::uo) is det.

get_dependency_file_names(Globals, Tuples0, Tuples, !IO) :-
    list.map_foldl(get_dependency_file_name(Globals), Tuples0, Tuples, !IO).

:- pred get_dependency_file_name(globals::in, dependency_status_result::in,
    dependency_status_known_file::out, io::di, io::uo) is det.

get_dependency_file_name(Globals, Tuple0, Tuple, !IO) :-
    Tuple0 = dependency_status_result(Dep, MaybeTargetFileName, Status),
    (
        MaybeTargetFileName = yes(TargetFileName)
    ;
        MaybeTargetFileName = no,
        dependency_file_to_file_name(Globals, Dep, TargetFileName, !IO)
    ),
    Tuple = dependency_status_known_file(Dep, TargetFileName, Status).

%---------------------------------------------------------------------------%

check_dependencies(ProgressStream, Globals, TargetFileName, MaybeTimestamp,
        BuildDepsSucceeded, DepFiles, DepsResult, !Info, !IO) :-
    list.map_foldl2(get_dependency_file_status(ProgressStream, Globals),
        DepFiles, DepStatusTuples, !Info, !IO),
    list.filter(
        ( pred(dependency_status_result(_, _, DepStatus)::in) is semidet :-
            DepStatus \= deps_status_up_to_date
        ), DepStatusTuples, UnbuiltDependencyTuples0),
    (
        UnbuiltDependencyTuples0 = [_ | _],
        get_dependency_file_names(Globals,
            UnbuiltDependencyTuples0, UnbuiltDependencyTuples, !IO),
        debug_make_msg(Globals,
            describe_unbuilt_dependencies(TargetFileName,
                UnbuiltDependencyTuples),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        DepsResult = deps_error
    ;
        UnbuiltDependencyTuples0 = [],
        debug_make_msg(Globals,
            string.format("%s: finished dependencies\n", [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        list.map_foldl2(get_dependency_timestamp(ProgressStream, Globals),
            DepFiles, DepTimestamps, !Info, !IO),

        check_dependency_timestamps(ProgressStream, Globals, TargetFileName,
            MaybeTimestamp, BuildDepsSucceeded, DepStatusTuples,
            DepTimestamps, DepsResult, !IO)
    ).

:- pred describe_unbuilt_dependencies(file_name::in,
    list(dependency_status_known_file)::in, string::out) is det.

describe_unbuilt_dependencies(TargetFileName, UnbuiltDependencies,
        UnbuiltDependenciesDesc) :-
    string.format("%s: dependencies could not be built.\n\t",
        [s(TargetFileName)], Header),
    list.map(describe_target_dependency_status, UnbuiltDependencies,
        UnbuiltDependencyDescs),
    string.append_list([Header | UnbuiltDependencyDescs],
        UnbuiltDependenciesDesc).

:- pred describe_target_dependency_status(dependency_status_known_file::in,
    string::out) is det.

describe_target_dependency_status(DepTuple, Desc) :-
    DepTuple = dependency_status_known_file(_, DepTargetFileName, DepStatus),
    (
        DepStatus = deps_status_not_considered,
        DepStatusStr = "deps_status_not_considered"
    ;
        DepStatus = deps_status_being_built,
        DepStatusStr = "deps_status_being_built"
    ;
        DepStatus = deps_status_up_to_date,
        DepStatusStr = "deps_status_up_to_date"
    ;
        DepStatus = deps_status_error,
        DepStatusStr = "deps_status_error"
    ),
    string.format("\t%s - %s\n", [s(DepTargetFileName), s(DepStatusStr)],
        Desc).

    % XXX Move this predicate *below* its only caller.
    %
:- pred check_dependencies_timestamps_missing_deps_msg(file_name::in,
    maybe_succeeded::in, list(dependency_status_known_file)::in,
    list(maybe_error(timestamp))::in, string::out) is det.

check_dependencies_timestamps_missing_deps_msg(TargetFileName,
        BuildDepsSucceeded, DepFileTuples, DepTimestamps, Msg) :-
    assoc_list.from_corresponding_lists(DepFileTuples, DepTimestamps,
        DepTimestampAL),
    list.filter_map(
        ( pred(Pair::in, Tuple::out) is semidet :-
            Pair = Tuple - error(_)
        ), DepTimestampAL, ErrorDepTuples),
    GetFileName = (func(dependency_status_known_file(_, FN, _)) = FN),
    ErrorFileNames = list.map(GetFileName, ErrorDepTuples),
    list.sort(ErrorFileNames, SortedErrorFileNames),
    SortedErrorFileNamesStr = string.join_list(", ", SortedErrorFileNames),
    % This line can get very long.
    string.format("** dependencies for `%s' do not exist: %s\n",
        [s(TargetFileName), s(SortedErrorFileNamesStr)], DoNotExistMsg),
    (
        BuildDepsSucceeded = succeeded,
        Msg = DoNotExistMsg ++
            "** This indicates a bug in `mmc --make'.\n"
    ;
        BuildDepsSucceeded = did_not_succeed,
        Msg = DoNotExistMsg
    ).

check_dependency_timestamps(ProgressStream, Globals, TargetFileName,
        MaybeTimestamp, BuildDepsSucceeded, DepFileTuples0, DepTimestamps,
        DepsResult, !IO) :-
    (
        MaybeTimestamp = error(_),
        DepsResult = deps_out_of_date,
        debug_make_msg(Globals,
            string.format("%s does not exist.\n", [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO)
    ;
        MaybeTimestamp = ok(Timestamp),
        ( if error_in_timestamps(DepTimestamps) then
            DepsResult = deps_error,
            get_dependency_file_names(Globals, DepFileTuples0, DepFileTuples,
                !IO),
            (
                BuildDepsSucceeded = succeeded,
                % Something has gone wrong -- building the target has
                % succeeded, but there are some files missing.
                % Report an error.
                check_dependencies_timestamps_missing_deps_msg(
                    TargetFileName, BuildDepsSucceeded, DepFileTuples,
                    DepTimestamps, MissingDepsMsg),
                io.write_string(ProgressStream, MissingDepsMsg, !IO)
            ;
                BuildDepsSucceeded = did_not_succeed,
                debug_make_msg(Globals,
                    check_dependencies_timestamps_missing_deps_msg(
                        TargetFileName, BuildDepsSucceeded, DepFileTuples,
                        DepTimestamps),
                    MaybeMissingDepsMsg),
                maybe_write_msg(ProgressStream, MaybeMissingDepsMsg, !IO)
            )
        else
            globals.lookup_bool_option(Globals, rebuild, Rebuild),
            (
                Rebuild = yes,
                % With `--rebuild', we always consider the target to be
                % out-of-date, regardless of the timestamps of its
                % dependencies.
                DepsResult = deps_out_of_date
            ;
                Rebuild = no,
                ( if newer_timestamp(DepTimestamps, Timestamp) then
                    get_dependency_file_names(Globals,
                        DepFileTuples0, DepFileTuples, !IO),
                    debug_make_msg(Globals,
                        describe_newer_dependencies(TargetFileName,
                            MaybeTimestamp, DepFileTuples, DepTimestamps),
                        DebugMsg),
                    maybe_write_msg(ProgressStream, DebugMsg, !IO),
                    DepsResult = deps_out_of_date
                else
                    DepsResult = deps_up_to_date
                )
            )
        )
    ).

:- pred error_in_timestamps(list(maybe_error(timestamp))::in) is semidet.

error_in_timestamps([H | T]) :-
    ( H = error(_)
    ; error_in_timestamps(T)
    ).

:- pred newer_timestamp(list(maybe_error(timestamp))::in, timestamp::in)
    is semidet.

newer_timestamp([H | T], Timestamp) :-
    (
        H = ok(DepTimestamp),
        compare((>), DepTimestamp, Timestamp)
    ;
        newer_timestamp(T, Timestamp)
    ).

:- pred describe_newer_dependencies(string::in, maybe_error(timestamp)::in,
    list(dependency_status_known_file)::in,
    list(maybe_error(timestamp))::in, string::out) is det.

describe_newer_dependencies(TargetFileName, MaybeTimestamp,
        DepFileTuples, DepTimestamps, Desc) :-
    string.format("%s [%s]: newer dependencies:\n",
        [s(TargetFileName), s(string(MaybeTimestamp))], Header),
    assoc_list.from_corresponding_lists(DepFileTuples, DepTimestamps,
        DepTimestampAL),
    list.filter(
        ( pred((_DepFileTuple - MaybeDepTimestamp)::in) is semidet :-
            (
                MaybeDepTimestamp = error(_)
            ;
                MaybeDepTimestamp = ok(DepTimestamp),
                MaybeTimestamp = ok(Timestamp),
                compare((>), DepTimestamp, Timestamp)
            )
        ), DepTimestampAL, NewerDepsAL0),
    list.sort(NewerDepsAL0, NewerDepsAL),
    list.map(describe_dependency_file_and_timestamp, NewerDepsAL,
        NewerDepsDescs),
    string.append_list([Header | NewerDepsDescs], Desc).

:- pred describe_dependency_file_and_timestamp(
    pair(dependency_status_known_file, maybe_error(timestamp))::in,
    string::out) is det.

describe_dependency_file_and_timestamp(DepFileTuple - MaybeTimestamp, Desc) :-
    DepFileTuple = dependency_status_known_file(DepFile, DepFileName, _),
    string.format("\t%s %s %s\n",
        [s(string(DepFile)), s(DepFileName), s(string(MaybeTimestamp))], Desc).

%---------------------------------------------------------------------------%
:- end_module make.check_up_to_date.
%---------------------------------------------------------------------------%
