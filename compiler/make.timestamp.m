%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.timestamp.m.
% Authors: stayl, wangp.
%
% Timestamp handling predicates used to implement `mmc --make'.
%
%---------------------------------------------------------------------------%

:- module make.timestamp.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module make.make_info.
:- import_module parse_tree.
:- import_module parse_tree.file_names.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- func init_target_file_timestamps = target_file_timestamps.

    % Find the timestamp updated when a target is produced.
    %
:- pred get_timestamp_file_timestamp(globals::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(io.text_output_stream::in, globals::in,
    dependency_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % get_target_timestamp(Globals, Search, TargetFile, Timestamp)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(io.text_output_stream::in, globals::in,
    maybe_search::in, target_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Find the timestamp of the first file matching the given
    % file name in one of the given directories.
    %
:- pred get_file_timestamp(list(dir_name)::in, file_name::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % If any of the inputs contain an error, return the first error.
    % XXX We should return all errors, not just the first.
    % If none of the inputs contain an error, return the oldest timestamp.
    %
:- pred find_error_or_older_ok_timestamp(maybe_error(timestamp)::in,
    maybe_error(timestamp)::in, maybe_error(timestamp)::out) is det.
:- pred find_error_or_oldest_ok_timestamp(list(maybe_error(timestamp))::in,
    maybe_error(timestamp)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.options.
:- import_module make.file_names.
:- import_module make.hash.
:- import_module make.module_dep_file.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.find_module.
:- import_module parse_tree.module_dep_info.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module dir.
:- import_module map.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

init_target_file_timestamps =
    version_hash_table.unsafe_init_default(target_file_hash).

%---------------------------------------------------------------------------%

get_timestamp_file_timestamp(Globals, target_file(ModuleName, TargetType),
        MaybeTimestamp, !Info, !IO) :-
    ( if timestamp_extension(TargetType, TimestampExt) then
        module_name_to_file_name(Globals, $pred,
            TimestampExt, ModuleName, FileName)
    else
        module_target_to_file_name(Globals, $pred, TargetType,
            ModuleName, FileName, !IO)
    ),
    % We should only ever look for timestamp files in the current directory.
    % Timestamp files are only used when processing a module, and only modules
    % in the current directory are processed.
    SearchDirs = [dir.this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

%---------------------------------------------------------------------------%

get_dependency_timestamp(ProgressStream, Globals, DependencyFile,
        MaybeTimestamp, !Info, !IO) :-
    (
        DependencyFile = dep_file(FileName),
        SearchDirs = [dir.this_directory],
        get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO)
    ;
        DependencyFile = dep_target(Target),
        get_target_timestamp(ProgressStream, Globals, do_search, Target,
            MaybeTimestamp0, !Info, !IO),
        ( if
            Target = target_file(_, module_target_c_header(header_mih)),
            MaybeTimestamp0 = ok(_)
        then
            % Don't rebuild the `.o' file if an irrelevant part of a
            % `.mih' file has changed. If a relevant part of a `.mih'
            % file changed, the interface files of the imported module
            % must have changed in a way that would force the `.c' and
            % `.o' files of the current module to be rebuilt.
            MaybeTimestamp = ok(oldest_timestamp)
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    ).

%---------------------------------------------------------------------------%

get_target_timestamp(ProgressStream, Globals, Search, TargetFile,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(_ModuleName, TargetType),
    ( if TargetType = module_target_analysis_registry then
        ForSearch = maybe_search_to_maybe_for_search(Search),
        get_file_name(ProgressStream, Globals, $pred, ForSearch,
            TargetFile, FileName, !Info, !IO),
        get_target_timestamp_analysis_registry(ProgressStream, Globals,
            Search, TargetFile, FileName, MaybeTimestamp, !Info, !IO)
    else
        % This path is hit very frequently so it is worth caching timestamps
        % by target_file. It avoids having to compute a file name for a
        % target_file first, before looking up the timestamp for that file.
        TargetFileTimestamps0 = make_info_get_target_file_timestamps(!.Info),
        ( if
            version_hash_table.search(TargetFileTimestamps0, TargetFile,
                Timestamp)
        then
            MaybeTimestamp = ok(Timestamp)
        else
            ForSearch = maybe_search_to_maybe_for_search(Search),
            get_file_name(ProgressStream, Globals, $pred, ForSearch,
                TargetFile, FileName, !Info, !IO),
            get_target_timestamp_2(ProgressStream, Globals,
                Search, TargetFile, FileName, MaybeTimestamp, !Info, !IO),
            (
                MaybeTimestamp = ok(Timestamp),
                TargetFileTimestamps1 =
                    make_info_get_target_file_timestamps(!.Info),
                version_hash_table.det_insert(TargetFile, Timestamp,
                    TargetFileTimestamps1, TargetFileTimestamps),
                make_info_set_target_file_timestamps(TargetFileTimestamps,
                    !Info)
            ;
                MaybeTimestamp = error(_)
                % Do not record errors. These would usually be due to files not
                % yet made, and the result would have to be updated once the
                % file is made.
            )
        )
    ).

    % Special treatment for `.analysis' files. If the corresponding
    % `.analysis_status' file says the `.analysis' file is invalid,
    % then we treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(io.text_output_stream::in,
    globals::in, maybe_search::in, target_file::in, file_name::in,
    maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry(ProgressStream, Globals, Search,
        TargetFile, FileName, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, _TargetType),
    FileTimestamps0 = make_info_get_file_timestamps(!.Info),
    ( if map.search(FileTimestamps0, FileName, MaybeTimestamp0) then
        MaybeTimestamp = MaybeTimestamp0
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_2(ProgressStream, Globals, Search, TargetFile,
                FileName, MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            map.det_insert(FileName, MaybeTimestamp,
                FileTimestamps0, FileTimestamps),
            make_info_set_file_timestamps(FileTimestamps, !Info)
        )
    ).

:- pred get_target_timestamp_2(io.text_output_stream::in, globals::in,
    maybe_search::in, target_file::in, file_name::in,
    maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_2(ProgressStream, Globals, Search, TargetFile, FileName,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    (
        Search = do_search,
        get_search_directories(Globals, TargetType, SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    ( if
        MaybeTimestamp0 = error(_),
        ( TargetType = module_target_opt
        ; TargetType = module_target_analysis_registry
        )
    then
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.
        get_maybe_module_dep_info(ProgressStream, Globals,
            ModuleName, MaybeModuleDepInfo, !Info, !IO),
        ( if
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            ModuleDir \= dir.this_directory
        then
            MaybeTimestamp = ok(oldest_timestamp),
            FileTimestamps0 = make_info_get_file_timestamps(!.Info),
            map.set(FileName, MaybeTimestamp, FileTimestamps0, FileTimestamps),
            make_info_set_file_timestamps(FileTimestamps, !Info)
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    else
        MaybeTimestamp = MaybeTimestamp0
    ).

:- pred get_search_directories(globals::in, module_target_type::in,
    list(dir_name)::out) is det.

get_search_directories(Globals, TargetType, SearchDirs) :-
    MaybeOpt = get_search_option_for_file_type(TargetType),
    (
        MaybeOpt = yes(SearchDirOpt),
        globals.lookup_accumulating_option(Globals, SearchDirOpt, SearchDirs0),
        % Make sure the current directory is searched for C headers
        % and libraries.
        ( if list.member(dir.this_directory, SearchDirs0) then
            SearchDirs = SearchDirs0
        else
            SearchDirs = [dir.this_directory | SearchDirs0]
        )
    ;
        MaybeOpt = no,
        SearchDirs = [dir.this_directory]
    ).

:- func get_search_option_for_file_type(module_target_type) = maybe(option).

get_search_option_for_file_type(ModuleTargetType) = MaybeSearchOption :-
    (
        ( ModuleTargetType = module_target_source
        ; ModuleTargetType = module_target_errors
        ; ModuleTargetType = module_target_track_flags
        ; ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_csharp_code
        ; ModuleTargetType = module_target_java_code
        ; ModuleTargetType = module_target_java_class_code
        ; ModuleTargetType = module_target_object_code(_)
        ; ModuleTargetType = module_target_foreign_object(_, _)
        ; ModuleTargetType = module_target_fact_table_object(_, _)
        ; ModuleTargetType = module_target_xml_doc
        ),
        MaybeSearchOption = no
    ;
        ( ModuleTargetType = module_target_int0
        ; ModuleTargetType = module_target_int1
        ; ModuleTargetType = module_target_int2
        ; ModuleTargetType = module_target_int3
        ),
        MaybeSearchOption = yes(search_directories)
    ;
        ( ModuleTargetType = module_target_opt
        ; ModuleTargetType = module_target_analysis_registry
        ),
        MaybeSearchOption = yes(intermod_directories)
    ;
        ModuleTargetType = module_target_c_header(_),
        MaybeSearchOption = yes(c_include_directory)
    ).

%---------------------------------------------------------------------------%

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    FileTimestamps0 = make_info_get_file_timestamps(!.Info),
    ( if map.search(FileTimestamps0, FileName, MaybeTimestamp0) then
        MaybeTimestamp = MaybeTimestamp0
    else
        search_for_file_mod_time(SearchDirs, FileName, SearchResult, !IO),
        (
            SearchResult = ok(TimeT),
            Timestamp = time_t_to_timestamp(TimeT),
            MaybeTimestamp = ok(Timestamp),
            map.det_insert(FileName, MaybeTimestamp,
                FileTimestamps0, FileTimestamps),
            make_info_set_file_timestamps(FileTimestamps, !Info)
        ;
            SearchResult = error(_SearchError),
            % XXX MAKE We should not ignore _SearchError.
            string.format("file `%s' not found", [s(FileName)], NotFoundMsg),
            MaybeTimestamp = error(NotFoundMsg)
        )
    ).

%---------------------------------------------------------------------------%

find_error_or_older_ok_timestamp(MaybeTimestampA, MaybeTimestampB,
        MaybeTimestamp) :-
    (
        MaybeTimestampA = error(_),
        MaybeTimestamp = MaybeTimestampA
    ;
        MaybeTimestampA = ok(TimestampA),
        (
            MaybeTimestampB = error(_),
            MaybeTimestamp = MaybeTimestampB
        ;
            MaybeTimestampB = ok(TimestampB),
            ( if compare((<), TimestampA, TimestampB) then
                Timestamp = TimestampA
            else
                Timestamp = TimestampB
            ),
            MaybeTimestamp = ok(Timestamp)
        )
    ).

%---------------------------------------------------------------------------%

find_error_or_oldest_ok_timestamp(MaybeTimestamps, MaybeTimestamp) :-
    list.foldl(find_error_or_older_ok_timestamp, MaybeTimestamps,
        ok(newest_timestamp), MaybeTimestamp).

%---------------------------------------------------------------------------%
:- end_module make.timestamp.
%---------------------------------------------------------------------------%
