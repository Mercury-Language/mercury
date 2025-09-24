%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023, 2025 The Mercury team.
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
:- import_module parse_tree.find_module.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- func init_target_file_timestamp_map = target_file_timestamp_map.

%---------------------------------------------------------------------------%

    % Find the timestamp for the given target.
    %
:- pred get_target_id_timestamp(io.text_output_stream::in, globals::in,
    target_id::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % get_target_timestamp(ProgressStream, Globals, TargetFile,
    %   MaybeTimestamp, !Info, !IO):
    %
    % Return the timestamp for TargetFile, if it exists.
    %
:- pred get_target_timestamp(io.text_output_stream::in, globals::in,
    target_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % get_target_timestamp_search(ProgressStream, Globals, TargetFile,
    %   MaybeTimestamp, !Info, !IO):
    %
    % Search for TargetFile in the places where its target type indicates
    % it should be searched for, and return its timestamp if it exists.
    %
:- pred get_target_timestamp_search(io.text_output_stream::in, globals::in,
    target_file::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % get_file_timestamp(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeTimestamp, !Info, !IO):
    %
    % Find the timestamp of the first file matching the given
    % file name in one of the search directories. We return the list of
    % directories we search in SearchDirs, or, if we got MaybeTimestamp
    % from the cache, we return the list of directories we searched
    % when the cache entry was created.
    %
:- pred get_file_timestamp(search_auth_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
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

get_target_id_timestamp(ProgressStream, Globals, TargetId,
        MaybeTimestamp, !Info, !IO) :-
    (
        TargetId = non_merc_target(FileName),
        get_file_timestamp(search_auth_cur_dir, FileName,
            _SearchDirs, MaybeTimestamp, !Info, !IO)
    ;
        TargetId = merc_target(Target),
        get_target_timestamp_search(ProgressStream, Globals, Target,
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

get_target_timestamp(ProgressStream, Globals, TargetFile, MaybeTimestamp,
        !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    ( if TargetType = module_target_analysis_registry then
        % XXX LEGACY
        module_target_to_file_name(Globals, $pred,
            TargetType, ModuleName, FileName, _FileNameProposed, !IO),
        get_target_timestamp_analysis_registry(ProgressStream, Globals,
            TargetFile, FileName, MaybeTimestamp, !Info, !IO)
    else
        ( if is_timestamp_in_cache(!.Info, TargetFile, Timestamp) then
            trace [compile_time(flag("target_timestamp_cache")), io(!TIO)] (
                verify_cached_target_file_timestamp(ProgressStream,
                    Globals, TargetFile, Timestamp, !.Info, _Info, !TIO)
            ),
            MaybeTimestamp = ok(Timestamp)
        else
            % XXX LEGACY
            module_maybe_nested_target_file_to_file_name(ProgressStream,
                Globals, $pred, TargetFile,
                FileName, _FileNameProposed, !Info, !IO),
            get_target_timestamp_uncached(ProgressStream, Globals,
                ModuleName, TargetType, FileName, MaybeTimestamp, !Info, !IO),
            record_timestamp_if_ok(TargetFile, MaybeTimestamp, !Info)
        )
    ).

get_target_timestamp_search(ProgressStream, Globals, TargetFile,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    ( if TargetType = module_target_analysis_registry then
        % XXX LEGACY
        module_target_to_search_file_name(Globals, $pred,
            TargetType, ModuleName, SearchAuthDirs,
            FileName, _FileNameProposed, !IO),
        get_target_timestamp_analysis_registry_search(ProgressStream, Globals,
            SearchAuthDirs, TargetFile, FileName, MaybeTimestamp, !Info, !IO)
    else
        ( if is_timestamp_in_cache(!.Info, TargetFile, Timestamp) then
            trace [compile_time(flag("target_timestamp_cache")), io(!TIO)] (
                verify_cached_target_file_timestamp_search(ProgressStream,
                    Globals, TargetFile, Timestamp, !.Info, _Info, !TIO)
            ),
            MaybeTimestamp = ok(Timestamp)
        else
            % XXX LEGACY
            module_maybe_nested_target_file_to_search_file_name(ProgressStream,
                Globals, $pred, TargetFile, SearchAuthDirs,
                FileName, _FileNameProposed, !Info, !IO),
            get_target_timestamp_uncached_search(ProgressStream, Globals,
                ModuleName, TargetType, FileName, SearchAuthDirs,
                MaybeTimestamp, !Info, !IO),
            record_timestamp_if_ok(TargetFile, MaybeTimestamp, !Info)
        )
    ).

%---------------%

    % The code points we are called from are hit very frequently, so it is
    % worth caching timestamps by target_file. It avoids having to compute
    % a file name for a target_file first, before looking up its timestamp.
    % XXX Wouldn't the search be even faster if, instead of the module's
    % name, our caller gave us its module_index?
    %
:- pred is_timestamp_in_cache(make_info::in, target_file::in,
    timestamp::out) is semidet.

is_timestamp_in_cache(Info0, TargetFile, Timestamp) :-
    Cache0 = make_info_get_target_file_timestamp_map(Info0),
    version_hash_table.search(Cache0, TargetFile, Timestamp).

:- pred record_timestamp_if_ok(target_file::in, maybe_error(timestamp)::in,
    make_info::in, make_info::out) is det.

record_timestamp_if_ok(TargetFile, MaybeTimestamp, !Info) :-
    (
        MaybeTimestamp = ok(Timestamp),
        TargetFileTimestampMap0 =
            make_info_get_target_file_timestamp_map(!.Info),
        version_hash_table.det_insert(TargetFile, Timestamp,
            TargetFileTimestampMap0, TargetFileTimestampMap),
        make_info_set_target_file_timestamp_map(TargetFileTimestampMap, !Info)
    ;
        MaybeTimestamp = error(_)
        % Do not record errors. These would usually be due to files
        % not yet made, and the result would have to be updated
        % once the file *is* made.
    ).

%---------------%

:- pred verify_cached_target_file_timestamp(io.text_output_stream::in,
    globals::in, target_file::in, timestamp::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

verify_cached_target_file_timestamp(ProgressStream, Globals,
        TargetFile, CachedTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    % XXX LEGACY
    module_maybe_nested_target_file_to_file_name(ProgressStream,
        Globals, $pred, TargetFile, FileName, _FileNameProposed, !Info, !IO),
    get_target_timestamp_uncached(ProgressStream, Globals,
        ModuleName, TargetType, FileName, MaybeFileTimestamp, !Info, !IO),
    abort_for_any_verification_failure(CachedTimestamp,
        MaybeFileTimestamp, !Info).

:- pred verify_cached_target_file_timestamp_search(io.text_output_stream::in,
    globals::in, target_file::in, timestamp::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

verify_cached_target_file_timestamp_search(ProgressStream, Globals,
        TargetFile, CachedTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    % XXX LEGACY
    module_maybe_nested_target_file_to_search_file_name(ProgressStream,
        Globals, $pred, TargetFile, SearchAuthDirs,
        FileName, _FileNameProposed, !Info, !IO),
    get_target_timestamp_uncached_search(ProgressStream, Globals,
        ModuleName, TargetType, FileName, SearchAuthDirs,
        MaybeFileTimestamp, !Info, !IO),
    abort_for_any_verification_failure(CachedTimestamp,
        MaybeFileTimestamp, !Info).

%---------------%

    % We take a in,out pair of make_info args because we don't want calls
    % to this predicate to be deleted as det code that computes nothing.
    %
:- pred abort_for_any_verification_failure(timestamp::in,
    maybe_error(timestamp)::in, make_info::in, make_info::out) is det.
:- pragma no_inline(pred(abort_for_any_verification_failure/4)).

abort_for_any_verification_failure(CachedTimestamp, MaybeFileTimestamp,
        Info0, Info) :-
    (
        MaybeFileTimestamp = ok(FileTimestamp),
        ( if CachedTimestamp = FileTimestamp then
            Info = Info0
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

%---------------%

    % Special treatment for `.analysis' files. If the corresponding
    % `.analysis_status' file says the `.analysis' file is invalid,
    % then we treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(io.text_output_stream::in,
    globals::in, target_file::in, file_name::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry(ProgressStream, Globals,
        TargetFile, FileName, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
    ( if map.search(FileTimestampMap0, FileName, MapValue) then
        MapValue = {_SearchDirs, MaybeTimestamp}
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_uncached(ProgressStream, Globals,
                ModuleName, TargetType, FileName, MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            map.det_insert(FileName, {[], MaybeTimestamp},
                FileTimestampMap0, FileTimestampMap),
            make_info_set_file_timestamp_map(FileTimestampMap, !Info)
        )
    ).

:- pred get_target_timestamp_analysis_registry_search(
    io.text_output_stream::in, globals::in, search_auth_dirs::in,
    target_file::in, file_name::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry_search(ProgressStream, Globals,
        SearchAuthDirs, TargetFile, FileName, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
    ( if map.search(FileTimestampMap0, FileName, MapValue) then
        MapValue = {_SearchDirs, MaybeTimestamp}
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_uncached_search(ProgressStream, Globals,
                ModuleName, TargetType, FileName, SearchAuthDirs,
                MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            map.det_insert(FileName, {[], MaybeTimestamp},
                FileTimestampMap0, FileTimestampMap),
            make_info_set_file_timestamp_map(FileTimestampMap, !Info)
        )
    ).

%---------------%

:- pred get_target_timestamp_uncached(io.text_output_stream::in,
    globals::in, module_name::in, module_target_type::in, file_name::in,
    maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_uncached(ProgressStream, Globals,
        ModuleName, TargetType, FileName, MaybeTimestamp, !Info, !IO) :-
    SearchAuthDirs = search_auth_cur_dir,
    get_file_timestamp(SearchAuthDirs, FileName,
        SearchDirs, MaybeTimestamp0, !Info, !IO),
    get_target_timestamp_handle_any_errors(ProgressStream, Globals,
        ModuleName, TargetType, FileName, SearchDirs,
        MaybeTimestamp0, MaybeTimestamp, !Info, !IO).

:- pred get_target_timestamp_uncached_search(io.text_output_stream::in,
    globals::in, module_name::in, module_target_type::in, file_name::in,
    search_auth_dirs::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_uncached_search(ProgressStream, Globals,
        ModuleName, TargetType, FileName, SearchAuthDirs, MaybeTimestamp,
        !Info, !IO) :-
    get_file_timestamp(SearchAuthDirs, FileName,
        SearchDirs, MaybeTimestamp0, !Info, !IO),
    get_target_timestamp_handle_any_errors(ProgressStream, Globals,
        ModuleName, TargetType, FileName, SearchDirs,
        MaybeTimestamp0, MaybeTimestamp, !Info, !IO).

    % If a `.opt' file in another directory doesn't exist, it just means
    % that a library wasn't compiled with `--intermodule-optimization'.
    % Similarly for `.analysis' files.
    %
:- pred get_target_timestamp_handle_any_errors(io.text_output_stream::in,
    globals::in, module_name::in, module_target_type::in, file_name::in,
    list(dir_name)::in,
    maybe_error(timestamp)::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_handle_any_errors(ProgressStream, Globals,
        ModuleName, TargetType, FileName, SearchDirs,
        MaybeTimestamp0, MaybeTimestamp, !Info, !IO) :-
    ( if
        MaybeTimestamp0 = error(_),
        ( TargetType = module_target_opt
        ; TargetType = module_target_analysis_registry
        )
    then
        get_maybe_module_dep_info(ProgressStream, Globals,
            ModuleName, MaybeModuleDepInfo, !Info, !IO),
        ( if
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            % NOTE This test can't succeed for the non-search caller.
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

%---------------------------------------------------------------------------%

get_file_timestamp(SearchAuthDirs, FileName,
        SearchDirs, MaybeTimestamp, !Info, !IO) :-
    FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
    ( if map.search(FileTimestampMap0, FileName, MapValue) then
        MapValue = {SearchDirs, MaybeTimestamp}
    else
        search_for_file_mod_time(SearchAuthDirs, FileName,
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
