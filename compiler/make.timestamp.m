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
:- import_module parse_tree.find_module.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- func init_target_file_timestamp_map = target_file_timestamp_map.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(io.text_output_stream::in, globals::in,
    dependency_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % get_target_timestamp(ProgressStream, Globals, Search,
    %   TargetFile, Timestamp, !Info, !IO)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(io.text_output_stream::in, globals::in,
    maybe_search::in, target_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % get_file_timestamp(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeTimestamp, !Info, !IO):
    %
    % Find the timestamp of the first file matching the given
    % file name in one of the search directories. We return the list of
    % directories we search in SearchDirs, or, if we got MaybeTimestamp
    % from the cache, we return the list of directories we searched
    % when the cache entry was created.
    %
:- pred get_file_timestamp(search_which_dirs::in,
    file_name::in, list(dir_name)::out, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.options.
:- import_module make.file_names.
:- import_module make.get_module_dep_info.
:- import_module make.hash.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_dep_info.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module dir.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

init_target_file_timestamp_map =
    version_hash_table.unsafe_init_default(target_file_hash).

%---------------------------------------------------------------------------%

get_dependency_timestamp(ProgressStream, Globals, DependencyFile,
        MaybeTimestamp, !Info, !IO) :-
    (
        DependencyFile = dep_file(FileName),
        get_file_timestamp(search_cur_dir, FileName,
            _SearchDirs, MaybeTimestamp, !Info, !IO)
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
    TargetFile = target_file(ModuleName, TargetType),
    ( if TargetType = module_target_analysis_registry then
        ForSearch = maybe_search_to_maybe_for_search(Search),
        % XXX LEGACY
        module_target_to_maybe_for_search_file_name(Globals, $pred,
            ForSearch, TargetType, ModuleName,
            FileName, _FileNameProposed, !IO),
        get_target_timestamp_analysis_registry(ProgressStream, Globals,
            Search, TargetFile, FileName, MaybeTimestamp, !Info, !IO)
    else
        % This path is hit very frequently, so it is worth caching timestamps
        % by target_file. It avoids having to compute a file name for a
        % target_file first, before looking up the timestamp for that file.
        % XXX Wouldn't the search be even faster if, instead of the module's
        % name, our caller gave us its module_index?
        TargetFileTimestampMap0 =
            make_info_get_target_file_timestamp_map(!.Info),
        ( if
            version_hash_table.search(TargetFileTimestampMap0, TargetFile,
                Timestamp)
        then
            trace [compile_time(flag("target_timestamp_cache")), io(!TIO)] (
                verify_cached_target_file_timestamp(ProgressStream, Globals,
                    Search, TargetFile, Timestamp, !.Info, _Info, !TIO)
            ),
            MaybeTimestamp = ok(Timestamp)
        else
            ForSearch = maybe_search_to_maybe_for_search(Search),
            % XXX LEGACY
            module_maybe_nested_target_file_to_file_name(ProgressStream,
                Globals, $pred, ForSearch, TargetFile,
                FileName, _FileNameProposed, !Info, !IO),
            get_target_timestamp_uncached(ProgressStream, Globals,
                Search, TargetFile, FileName, MaybeTimestamp, !Info, !IO),
            (
                MaybeTimestamp = ok(Timestamp),
                TargetFileTimestampMap1 =
                    make_info_get_target_file_timestamp_map(!.Info),
                version_hash_table.det_insert(TargetFile, Timestamp,
                    TargetFileTimestampMap1, TargetFileTimestampMap),
                make_info_set_target_file_timestamp_map(TargetFileTimestampMap,
                    !Info)
            ;
                MaybeTimestamp = error(_)
                % Do not record errors. These would usually be due to files
                % not yet made, and the result would have to be updated
                % once the file *is* made.
            )
        )
    ).

:- pred verify_cached_target_file_timestamp(io.text_output_stream::in,
    globals::in, maybe_search::in, target_file::in, timestamp::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

verify_cached_target_file_timestamp(ProgressStream, Globals, Search,
        TargetFile, CachedTimestamp, !Info, !IO) :-
    ForSearch = maybe_search_to_maybe_for_search(Search),
    % XXX LEGACY
    module_maybe_nested_target_file_to_file_name(ProgressStream,
        Globals, $pred, ForSearch, TargetFile, FileName, _FileNameProposed,
        !Info, !IO),
    get_target_timestamp_uncached(ProgressStream, Globals,
        Search, TargetFile, FileName, MaybeFileTimestamp, !Info, !IO),
    (
        MaybeFileTimestamp = ok(FileTimestamp),
        ( if CachedTimestamp = FileTimestamp then
            true
        else
            string.format(
                "target file timestamp differs: %s (cached) vs %s (actual)",
                [s(timestamp_to_string(CachedTimestamp)),
                s(timestamp_to_string(FileTimestamp))], Msg),
            unexpected($pred, Msg)
        )
    ;
        MaybeFileTimestamp = error(Error),
        string.format(
            "target file timestamp differs: %s (cached) vs %s (actual)",
            [s(timestamp_to_string(CachedTimestamp)), s(Error)], Msg),
        unexpected($pred, Msg)
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
    FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
    ( if map.search(FileTimestampMap0, FileName, MapValue) then
        MapValue = {_SearchDirs, MaybeTimestamp}
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_uncached(ProgressStream, Globals, Search,
                TargetFile, FileName, MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            map.det_insert(FileName, {[], MaybeTimestamp},
                FileTimestampMap0, FileTimestampMap),
            make_info_set_file_timestamp_map(FileTimestampMap, !Info)
        )
    ).

:- pred get_target_timestamp_uncached(io.text_output_stream::in,
    globals::in, maybe_search::in, target_file::in, file_name::in,
    maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_uncached(ProgressStream, Globals, Search,
        TargetFile, FileName, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    (
        Search = do_search,
        search_which_dirs_for_target_type(Globals, TargetType, SearchWhichDirs)
    ;
        Search = do_not_search,
        SearchWhichDirs = search_cur_dir
    ),
    get_file_timestamp(SearchWhichDirs, FileName,
        SearchDirs, MaybeTimestamp0, !Info, !IO),
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
            FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
            map.set(FileName, {SearchDirs, MaybeTimestamp},
                FileTimestampMap0, FileTimestampMap),
            make_info_set_file_timestamp_map(FileTimestampMap, !Info)
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    else
        MaybeTimestamp = MaybeTimestamp0
    ).

:- pred search_which_dirs_for_target_type(globals::in, module_target_type::in,
    search_which_dirs::out) is det.

search_which_dirs_for_target_type(Globals, ModuleTargetType,
        SearchWhichDirs) :-
    globals.get_options(Globals, OptionTable),
    (
        ( ModuleTargetType = module_target_source
        ; ModuleTargetType = module_target_errors
        ; ModuleTargetType = module_target_track_flags
        ; ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_csharp_code
        ; ModuleTargetType = module_target_java_code
        ; ModuleTargetType = module_target_java_class_code
        ; ModuleTargetType = module_target_object_code(_)
        ; ModuleTargetType = module_target_fact_table_object(_, _)
        ; ModuleTargetType = module_target_xml_doc
        ),
        SearchWhichDirs = search_cur_dir
    ;
        ( ModuleTargetType = module_target_int0
        ; ModuleTargetType = module_target_int1
        ; ModuleTargetType = module_target_int2
        ; ModuleTargetType = module_target_int3
        ),
        globals.lookup_accumulating_option(Globals, search_directories,
            SearchDirs0),
        SearchWhichDirs0 = search_normal_dirs(OptionTable),
        ensure_cur_dir_is_searched(SearchDirs0,
            SearchWhichDirs0, SearchWhichDirs)
    ;
        ( ModuleTargetType = module_target_opt
        ; ModuleTargetType = module_target_analysis_registry
        ),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            SearchDirs0),
        SearchWhichDirs0 = search_intermod_dirs(OptionTable),
        ensure_cur_dir_is_searched(SearchDirs0,
            SearchWhichDirs0, SearchWhichDirs)
    ;
        ModuleTargetType = module_target_c_header(_),
        globals.lookup_accumulating_option(Globals, c_include_directories,
            SearchDirs0),
        SearchWhichDirs0 = search_c_include_dirs(OptionTable),
        ensure_cur_dir_is_searched(SearchDirs0,
            SearchWhichDirs0, SearchWhichDirs)
    ).

:- pred ensure_cur_dir_is_searched(list(dir_name)::in,
    search_which_tail_dirs::in, search_which_dirs::out) is det.

ensure_cur_dir_is_searched(SearchDirs0, SearchWhichDirs0, SearchWhichDirs) :-
    CurDir = dir.this_directory,
    ( if list.member(CurDir, SearchDirs0) then
        SearchWhichDirs = coerce(SearchWhichDirs0)
    else
        SearchWhichDirs = search_this_dir_and(CurDir, SearchWhichDirs0)
    ).

%---------------------------------------------------------------------------%

get_file_timestamp(SearchWhichDirs, FileName,
        SearchDirs, MaybeTimestamp, !Info, !IO) :-
    FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
    ( if map.search(FileTimestampMap0, FileName, MapValue) then
        MapValue = {SearchDirs, MaybeTimestamp}
    else
        search_for_file_mod_time(SearchWhichDirs, FileName,
            SearchDirs, SearchResult, !IO),
        (
            SearchResult = ok(TimeT),
            Timestamp = time_t_to_timestamp(TimeT),
            MaybeTimestamp = ok(Timestamp),
            map.det_insert(FileName, {SearchDirs, MaybeTimestamp},
                FileTimestampMap0, FileTimestampMap),
            make_info_set_file_timestamp_map(FileTimestampMap, !Info)
        ;
            SearchResult = error(_SearchError),
            % XXX MAKE We should not ignore _SearchError.
            % XXX SEARCH_ERROR SearchDirs
            string.format("file `%s' not found", [s(FileName)], NotFoundMsg),
            MaybeTimestamp = error(NotFoundMsg)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module make.timestamp.
%---------------------------------------------------------------------------%
