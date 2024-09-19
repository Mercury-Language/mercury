%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023-2024 The Mercury team.
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

    % Values of this type represent the set of files that building a make
    % target can generate, either from scratch, or by overwriting a
    % previously-generated file. Basically, this is the set of files
    % that would be on left hand side of a make rule:
    %
    % lhs_files:    rhs_files
    %               action
    %
:- type make_lhs_files
    --->    make_lhs_files(
                % The first two fields contain the set of files on the
                % left hand side of the rule. The two fields partition
                % those files based on the answer to the question:
                % does this file have a corresponding date file? More
                % precisely, does date_file_extension succeed for the
                % target file's target type? If it does not, it is part of
                % tf_dateless_target_files; if it does, it is part of
                % tf_dated_target_files.
                mlf_dateless_target_files   :: list(target_file),
                mlf_dated_target_files      :: list(target_file),

                % The names of the date files of the files in the
                % tf_dated_target_files field.
                mlf_date_files              :: list(file_name),

                % The names of any target code files that the make action
                % whose effects we are describing may create or update.
                % Currently, this will be the set of files named in fact_table
                % declarations.
                mlf_foreign_code_files      :: list(file_name)
            ).

:- type maybe_oldest_lhs_file
    --->    some_lhs_file_is_missing
    ;       all_lhs_files_exist_oldest_timestamp(timestamp).

:- type should_rebuild_lhs
    --->    all_lhs_files_up_to_date
    ;       some_lhs_file_needs_rebuilding.

:- type lhs_result
    --->    can_rebuild_lhs(should_rebuild_lhs)
    ;       rhs_error.

%---------------------------------------------------------------------------%

    % must_or_should_we_rebuild_lhs(ProgressStream, Globals,
    %   TargetFile, TargetFileName, MakeLhsFiles, RhsDepFiles, LhsResult,
    %   !Info, !IO):
    %
    % The TargetFile and TargetFileName arguments specify the principal
    % target of a make rule in structured and string form respectively,
    % while MakeLhsFiles and RhsDepFiles specify the full set of the lhs and
    % rhs files respectively.
    %
    % Decide whether we either
    %
    % - *must* execute the action of a mmc --make rule, because one or more
    %   of the lhs files do not exist; or
    %
    % - if all the lhs files, whether we *should* execute the action, because
    %   one or more of the lhs files are out-of-date with respect to the rhs
    %   files.
    %
    % As part of the above process, check whether we have all the info we need
    % about the rhs files to make that decision. If not, then we cannot execute
    % the action, even if we otherwise would like to do so.
    %
    % Return the decision in LhsResult.
    %
:- pred must_or_should_we_rebuild_lhs(io.text_output_stream::in, globals::in,
    target_file::in, file_name::in, make_lhs_files::in,
    list(dependency_file)::in, lhs_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % should_we_rebuild_lhs(ProgressStream, Globals, TargetFileName,
    %   MaybeOldestLhsFile, BuildRhsSucceeded, RhsDepFiles, LhsResult,
    %   !Info, !IO):
    %
    % Decide whether we should execute the make action to (re)build
    % the lhs of a mmc --make rule, returning that decision in LhsResult.
    %
    % MaybeOldestLhsFile specifies whether all the lhs files already exist,
    % and if they do, what the timestamp of the oldest of these is.
    % TargetFileName is the name of the main target on the lhs of the rule.
    %
    % RhsDepFiles lists the files on the rhs of the rule.
    %
    % BuildRhsSucceeded says whether the building of RhsDepFiles has succeeded.
    % XXX This argument should not be needed; if it is did_not_succeed, then
    % this predicate should not be called. The only call site of this predicate
    % in must_or_should_we_rebuild_lhs below *does* always pass succeeded,
    % but this is not necessarily true for the calls in make.program_target.m
    % to this predicate, and to should_we_rebuild_lhs_given_timestamps.
    %
    % XXX This predicate double-checks whether the building of RhsDepFiles
    % succeeded by looking up their dependency statuses. This should NOT be
    % necessary; BuildRhsSucceeded = succeeded *should* imply that all these
    % files have deps_status_up_to_date. This is ensured by code before
    % the one call to should_we_rebuild_lhs_given_timestamps in
    % make.program_target.m, but not (as far as I, zs, can see) in the two
    % calls to should_we_rebuild_lhs, in must_or_should_we_rebuild_lhs below,
    % and in make.program_target.m.
    %
    % XXX The best way to fix this would probably be to replace the calls
    % in make.program_target.m to should_we_rebuild_lhs and to
    % should_we_rebuild_lhs_given_timestamps by a SINGLE call to this
    % predicate.
    %
:- pred should_we_rebuild_lhs(io.text_output_stream::in,
    globals::in, file_name::in, maybe_oldest_lhs_file::in, maybe_succeeded::in,
    list(dependency_file)::in, lhs_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % should_we_rebuild_lhs_given_timestamps(ProgressStream, Globals,
    %   TargetFileName, MaybeOldestLhsFile, BuildRhsSucceeded,
    %   RhsDepStatusTuples, RhsMaybeTimestamps, LhsResult, !IO):
    %
    % A version of the predicate above that
    %
    % - assumes that the failure of the building the rhs files has already
    %   been handled, and which
    %
    % - requires its callers to supply it with the timestamps (if any)
    %   of the rhs files.
    %
    % Exported for make.program_target.m.
    %
:- pred should_we_rebuild_lhs_given_timestamps(io.text_output_stream::in,
    globals::in, file_name::in, maybe_oldest_lhs_file::in, maybe_succeeded::in,
    list(dependency_status_result)::in, list(maybe_error(timestamp))::in,
    lhs_result::out, io::di, io::uo) is det.

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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
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
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

must_or_should_we_rebuild_lhs(ProgressStream, Globals,
        TargetFile, TargetFileName, MakeLhsFiles, RhsDepFiles, LhsResult,
        !Info, !IO) :-
    % Check whether all the lhs files exist, because if some are missing,
    % then we need to execute the action.
    MakeLhsFiles = make_lhs_files(DatelessLhsTargetFiles,
        DatedLhsTargetFiles, LhsDateFileNames, LhsForeignCodeFileNames),
    list.map_foldl2(
        get_target_timestamp(ProgressStream, Globals, do_not_search),
        DatelessLhsTargetFiles, DatelessLhsFileTimestamps, !Info, !IO),
    list.map_foldl2(
        get_target_timestamp(ProgressStream, Globals, do_not_search),
        DatedLhsTargetFiles, DatedLhsFileTimestamps, !Info, !IO),
    ( if
        ( list.member(error(_), DatelessLhsFileTimestamps)
        ; list.member(error(_), DatedLhsFileTimestamps)
        )
    then
        % Some lhs file does not exist.
        % XXX MAKE The one that does not exist may not be TargetFileName.
        debug_make_msg(Globals,
            string.format("%s: target file does not exist\n",
                [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        LhsResult = can_rebuild_lhs(some_lhs_file_needs_rebuilding)
    else
        % All the lhs files exist, so check whether they are all up-to-date.
        ( if
            TargetFile = target_file(ModuleName, TargetType),
            TargetType = module_target_analysis_registry
        then
            should_we_force_reanalysis_of_suboptimal_module(Globals,
                ModuleName, ForceReanalysis, !.Info, !IO)
        else
            ForceReanalysis = no
        ),
        (
            ForceReanalysis = yes,
            LhsResult = can_rebuild_lhs(some_lhs_file_needs_rebuilding)
        ;
            ForceReanalysis = no,
            % Compare the oldest of the timestamps of the lhs files
            % with the timestamps of the rhs.
            GetLocalTimestamps = get_file_timestamp([dir.this_directory]),
            list.map_foldl2(GetLocalTimestamps,
                LhsDateFileNames, LhsDateFileTimestamps, !Info, !IO),
            list.map_foldl2(GetLocalTimestamps,
                LhsForeignCodeFileNames, LhsForeignCodeFileTimestamps,
                !Info, !IO),
            AllLhsTimestamps = DatelessLhsFileTimestamps ++
                LhsDateFileTimestamps ++ LhsForeignCodeFileTimestamps,
            find_oldest_lhs_file(AllLhsTimestamps, MaybeOldestLhsTimestamp),
            % This predicate gets called only if the (re)building
            % of the rhs files succeeded.
            BuildRhsSucceeded = succeeded,
            should_we_rebuild_lhs(ProgressStream, Globals, TargetFileName,
                MaybeOldestLhsTimestamp, BuildRhsSucceeded, RhsDepFiles,
                LhsResult, !Info, !IO)
        )
    ).

%---------------------%

:- pred should_we_force_reanalysis_of_suboptimal_module(globals::in,
    module_name::in, bool::out, make_info::in, io::di, io::uo) is det.

should_we_force_reanalysis_of_suboptimal_module(Globals, ModuleName,
        ForceReanalysis, Info, !IO) :-
    ( if make_info_get_reanalysis_passes(Info) > 0 then
        do_read_module_overall_status(mmc, Globals, ModuleName, AnalysisStatus,
            !IO),
        (
            ( AnalysisStatus = suboptimal
            ; AnalysisStatus = invalid
            ),
            ForceReanalysis = yes
        ;
            AnalysisStatus = optimal,
            ForceReanalysis = no
        )
    else
        ForceReanalysis = no
    ).

%---------------------%

:- pred find_oldest_lhs_file(list(maybe_error(timestamp))::in,
    maybe_oldest_lhs_file::out) is det.

find_oldest_lhs_file(LhsMaybeTimestamps, MaybeOldestLhsTimestamp) :-
    (
        LhsMaybeTimestamps = [],
        unexpected($pred, "LhsMaybeTimestamps = []")
    ;
        LhsMaybeTimestamps = [HeadLhsMaybeTimestamp | TailLhsMaybeTimestamps],
        (
            HeadLhsMaybeTimestamp = error(_),
            MaybeOldestLhsTimestamp = some_lhs_file_is_missing
        ;
            HeadLhsMaybeTimestamp = ok(HeadLhsTimestamp),
            find_oldest_lhs_file_loop(TailLhsMaybeTimestamps, HeadLhsTimestamp,
                MaybeOldestLhsTimestamp)
        )
    ).

:- pred find_oldest_lhs_file_loop(list(maybe_error(timestamp))::in,
    timestamp::in, maybe_oldest_lhs_file::out) is det.

find_oldest_lhs_file_loop(LhsMaybeTimestamps, !.OldestLhsTimestamp,
        MaybeOldestLhsTimestamp) :-
    (
        LhsMaybeTimestamps = [HeadLhsMaybeTimestamp | TailLhsMaybeTimestamps],
        (
            HeadLhsMaybeTimestamp = error(_),
            MaybeOldestLhsTimestamp = some_lhs_file_is_missing
        ;
            HeadLhsMaybeTimestamp = ok(HeadLhsTimestamp),
            ( if compare((<), HeadLhsTimestamp, !.OldestLhsTimestamp) then
                !:OldestLhsTimestamp = HeadLhsTimestamp
            else
                true
            ),
            find_oldest_lhs_file_loop(TailLhsMaybeTimestamps,
                !.OldestLhsTimestamp, MaybeOldestLhsTimestamp)
        )
    ;
        LhsMaybeTimestamps = [],
        MaybeOldestLhsTimestamp =
            all_lhs_files_exist_oldest_timestamp(!.OldestLhsTimestamp)
    ).


%---------------------------------------------------------------------------%

should_we_rebuild_lhs(ProgressStream, Globals, TargetFileName,
        MaybeOldestLhsFile, BuildRhsSucceeded, RhsDepFiles, LhsResult,
        !Info, !IO) :-
    list.map_foldl2(get_dependency_file_status(ProgressStream, Globals),
        RhsDepFiles, RhsDepStatusTuples, !Info, !IO),
    list.filter(
        ( pred(dependency_status_result(_, _, DepStatus)::in) is semidet :-
            DepStatus \= deps_status_up_to_date
        ), RhsDepStatusTuples, UnbuiltRhsDepStatusTuples0),
    (
        UnbuiltRhsDepStatusTuples0 = [_ | _],
        get_dependency_file_names(Globals,
            UnbuiltRhsDepStatusTuples0, UnbuiltRhsDepStatusTuples, !IO),
        debug_make_msg(Globals,
            describe_unbuilt_dependencies(TargetFileName,
                UnbuiltRhsDepStatusTuples),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        LhsResult = rhs_error
    ;
        UnbuiltRhsDepStatusTuples0 = [],
        debug_make_msg(Globals,
            string.format("%s: finished dependencies\n", [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        list.map_foldl2(get_dependency_timestamp(ProgressStream, Globals),
            RhsDepFiles, RhsMaybeTimestamps, !Info, !IO),

        should_we_rebuild_lhs_given_timestamps(ProgressStream, Globals,
            TargetFileName, MaybeOldestLhsFile, BuildRhsSucceeded,
            RhsDepStatusTuples, RhsMaybeTimestamps, LhsResult, !IO)
    ).

should_we_rebuild_lhs_given_timestamps(ProgressStream, Globals, TargetFileName,
        MaybeOldestLhsFile, BuildRhsSucceeded,
        RhsDepStatusTuples, RhsMaybeTimestamps, LhsResult, !IO) :-
    (
        MaybeOldestLhsFile = some_lhs_file_is_missing,
        % The missing file must be rebuilt, even if all other LHS files
        % are up-to-date.
        %
        % XXX However, while TargetFileName will be one of the files
        % on the lhs of the implicit mmc --make rule, it may be a file
        % *other than* TargetFileName that does not exist, so this message
        % *may* be misleading. (Of course, in the common case, the lhs
        % file list will contain just one file, in which case the missing
        % file *has* to be TargetFileName.)
        LhsResult = can_rebuild_lhs(some_lhs_file_needs_rebuilding),
        debug_make_msg(Globals,
            string.format("%s does not exist.\n", [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO)
    ;
        MaybeOldestLhsFile =
            all_lhs_files_exist_oldest_timestamp(OldestLhsFileTimestamp),
        find_timestamps_and_errors(RhsMaybeTimestamps,
            [], RhsTimestamps, not_found_error, FoundError),
        (
            FoundError = found_error,
            LhsResult = rhs_error,
            get_dependency_file_names(Globals, RhsDepStatusTuples,
                FilledInRhsDepStatusTuples, !IO),
            (
                BuildRhsSucceeded = succeeded,
                % Something has gone wrong -- building the target has
                % succeeded, but there are some files missing.
                % Report an error.
                rhs_timestamps_missing_msg(TargetFileName, BuildRhsSucceeded,
                    FilledInRhsDepStatusTuples,
                    RhsMaybeTimestamps, MissingDepsMsg),
                io.write_string(ProgressStream, MissingDepsMsg, !IO)
            ;
                BuildRhsSucceeded = did_not_succeed,
                debug_make_msg(Globals,
                    rhs_timestamps_missing_msg(TargetFileName,
                        BuildRhsSucceeded, FilledInRhsDepStatusTuples,
                        RhsMaybeTimestamps),
                    MaybeMissingDepsMsg),
                maybe_write_msg(ProgressStream, MaybeMissingDepsMsg, !IO)
            )
        ;
            FoundError = not_found_error,
            globals.lookup_bool_option(Globals, rebuild, Rebuild),
            (
                Rebuild = yes,
                % With `--rebuild', we always consider the lhs files to be
                % out-of-date, regardless of their timestamps, or the
                % timestamps of the rhs files.
                ShouldRebuildLhs = some_lhs_file_needs_rebuilding
            ;
                Rebuild = no,
                is_any_rhs_file_newer_than_oldest_lhs(RhsTimestamps,
                    OldestLhsFileTimestamp, ShouldRebuildLhs),
                (
                    ShouldRebuildLhs = some_lhs_file_needs_rebuilding,
                    get_dependency_file_names(Globals,
                        RhsDepStatusTuples, FilledInRhsDepStatusTuples, !IO),
                    debug_make_msg(Globals,
                        describe_newer_dependencies(TargetFileName,
                            OldestLhsFileTimestamp, FilledInRhsDepStatusTuples,
                            RhsTimestamps),
                        DebugMsg),
                    maybe_write_msg(ProgressStream, DebugMsg, !IO)
                ;
                    ShouldRebuildLhs = all_lhs_files_up_to_date
                )
            ),
            LhsResult = can_rebuild_lhs(ShouldRebuildLhs)
        )
    ).

:- type maybe_found_error
    --->    not_found_error
    ;       found_error.

:- pred find_timestamps_and_errors(list(maybe_error(timestamp))::in,
    list(timestamp)::in, list(timestamp)::out,
    maybe_found_error::in, maybe_found_error::out) is det.

find_timestamps_and_errors([], !RhsTimestamps, !FoundError).
find_timestamps_and_errors([RhsMaybeTimestamp | RhsMaybeTimestamps],
        !RhsTimestamps, !FoundError) :-
    (
        RhsMaybeTimestamp = error(_),
        !:FoundError = found_error
    ;
        RhsMaybeTimestamp = ok(RhsTimestamp),
        !:RhsTimestamps = [RhsTimestamp | !.RhsTimestamps]
    ),
    find_timestamps_and_errors(RhsMaybeTimestamps,
        !RhsTimestamps, !FoundError).

:- pred is_any_rhs_file_newer_than_oldest_lhs(list(timestamp)::in,
    timestamp::in, should_rebuild_lhs::out) is det.

is_any_rhs_file_newer_than_oldest_lhs(RhsTimestamps, OldestLhsTimestamp,
        Result) :-
    (
        RhsTimestamps = [HeadRhsTimestamp | TailRhsTimestamps],
        ( if compare((>), HeadRhsTimestamp, OldestLhsTimestamp) then
            % The rhs file that HeadRhsTimestamp belongs to is newer than
            % the oldest file on the lhs.
            Result = some_lhs_file_needs_rebuilding
        else
            % The rhs file that HeadRhsTimestamp belongs to is NOT newer than
            % the oldest file on the lhs; check the other rhs timestamps.
            is_any_rhs_file_newer_than_oldest_lhs(TailRhsTimestamps,
                OldestLhsTimestamp, Result)
        )
    ;
        RhsTimestamps = [],
        % We have checked all of the rhs files' timestamps, and none are
        % newer than the oldest file on the lhs.
        Result = all_lhs_files_up_to_date
    ).

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
            % XXX LEGACY
            module_target_file_to_file_name(Globals, $pred, Target,
                TargetFileName, _TargetFileNameProposed, !IO),
            maybe_warn_up_to_date_target_msg(Globals, TopTargetFile,
                TargetFileName, !Info, UpToDateMsg),
            maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
            MaybeTargetFileName = yes(TargetFileName),
            Status = deps_status_up_to_date,
            Result = dependency_status_result(Dep, MaybeTargetFileName, Status)
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
            ; FileType = module_target_fact_table_object(_, _)
            ; FileType = module_target_xml_doc
            ),
            % We pass Dep, which contains Target, which contains ModuleName,
            % because get_dependency_file_status_main_path needs all of them
            % and this way, it does not have to rebuild Target or Dep.
            get_dependency_file_status_main_path(ProgressStream, Globals,
                Dep, Target, ModuleName, Result, !Info, !IO)
        )
    ).

:- pred get_dependency_file_status_main_path(io.text_output_stream::in,
    globals::in, dependency_file::in, target_file::in, module_name::in,
    dependency_status_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.
:- pragma inline(pred(get_dependency_file_status_main_path/10)).

get_dependency_file_status_main_path(ProgressStream, Globals,
        Dep, Target, ModuleName, Result, !Info, !IO) :-
    DepStatusMap0 = make_info_get_dep_file_status_map(!.Info),
    % XXX The management of dependency file status map here is incorrect.
    %
    % The code here checks whether Dep is in DepStatusMap0, and if it is
    % not there, it computes its Status, and then inserts that Status
    % into DepStatusMap. So far so good. The problem is that
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
    % in tests/invalid_nodepend exhibits this behavior as of 2023 oct 23,
    % provided the given options file sets things up properly for mmc --make.
    %
    % Unfortunately, the right way to fix this is not clear. For example,
    % replacing the version_hash_table.det_insert below with
    % version_hash_table.set would fix this symptom, but I (zs) think
    % that is unlikely to fix the underlying problem.
    ( if version_hash_table.search(DepStatusMap0, Dep, StatusPrime) then
        Status = StatusPrime,
        % In this common case, our caller does not need the target file name.
        % Calling get_make_target_file_name to construct the target file name
        % would therefore be an unnecessary cost, and as it happens,
        % it would be an unnecessary LARGE cost in execution time.
        MaybeTargetFileName = no
    else
        % XXX LEGACY
        module_target_file_to_file_name(Globals, $pred,
            Target, TargetFileName, _TargetFileNameProposed, !IO),
        MaybeTargetFileName = yes(TargetFileName),
        get_maybe_module_dep_info(ProgressStream, Globals, ModuleName,
            MaybeModuleDepInfo, !Info, !IO),
        (
            MaybeModuleDepInfo = no_module_dep_info,
            Status = deps_status_error
        ;
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            ( if ModuleDir = dir.this_directory then
                % XXX What is the reason for returning this value here?
                Status = deps_status_not_considered
            else
                % Targets from libraries are always considered to be
                % up-to-date if they exist.
                % XXX Presumably this code treats any code in another directory
                % as if it were in a library.
                get_target_timestamp(ProgressStream, Globals, do_search,
                    Target, MaybeTimestamp, !Info, !IO),
                (
                    MaybeTimestamp = ok(_),
                    Status = deps_status_up_to_date
                ;
                    MaybeTimestamp = error(Error),
                    Status = deps_status_error,
                    string.format("** Error: file `%s' not found: %s\n",
                        [s(TargetFileName), s(Error)], ErrorMsg),
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
    ),
    Result = dependency_status_result(Dep, MaybeTargetFileName, Status).

%---------------------%

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
        % XXX LEGACY
        dependency_file_to_file_name(Globals, Dep,
            TargetFileName, _TargetFileNameProposed, !IO)
    ),
    Tuple = dependency_status_known_file(Dep, TargetFileName, Status).

%---------------------------------------------------------------------------%
%
% Code to construct messages for all users of mmc --make.
%

:- pred rhs_timestamps_missing_msg(file_name::in,
    maybe_succeeded::in, list(dependency_status_known_file)::in,
    list(maybe_error(timestamp))::in, string::out) is det.

rhs_timestamps_missing_msg(TargetFileName, BuildRhsSucceeded,
        RhsDepStatusTuples, RhsTimestamps, Msg) :-
    assoc_list.from_corresponding_lists(RhsDepStatusTuples, RhsTimestamps,
        RhsTimestampAL),
    list.filter_map(
        ( pred(Pair::in, Tuple::out) is semidet :-
            Pair = Tuple - error(_)
        ), RhsTimestampAL, ErrorRhsStatusTuples),
    GetFileName = (func(dependency_status_known_file(_, FN, _)) = FN),
    ErrorFileNames = list.map(GetFileName, ErrorRhsStatusTuples),
    list.sort(ErrorFileNames, SortedErrorFileNames),
    SortedErrorFileNamesStr = string.join_list(", ", SortedErrorFileNames),
    % This line can get very long.
    string.format("** dependencies for `%s' do not exist: %s\n",
        [s(TargetFileName), s(SortedErrorFileNamesStr)], DoNotExistMsg),
    (
        BuildRhsSucceeded = succeeded,
        Msg = DoNotExistMsg ++
            "** This indicates a bug in `mmc --make'.\n"
    ;
        BuildRhsSucceeded = did_not_succeed,
        Msg = DoNotExistMsg
    ).

%---------------------------------------------------------------------------%
%
% Code to construct messages that can help debug mmc --make.
%

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

%---------------------%

:- pred describe_newer_dependencies(string::in, timestamp::in,
    list(dependency_status_known_file)::in, list(timestamp)::in,
    string::out) is det.

describe_newer_dependencies(TargetFileName, OldestLhsFileTimestamp,
        RhsDepStatusTuples, RhsTimestamps, Desc) :-
    string.format("%s [%s]: newer dependencies:\n",
        [s(TargetFileName), s(string(OldestLhsFileTimestamp))], Header),
    assoc_list.from_corresponding_lists(RhsDepStatusTuples, RhsTimestamps,
        RhsTimestampAL),
    list.filter(
        ( pred((_DepStatusTuple - RhsTimestamp)::in) is semidet :-
            compare((>), RhsTimestamp, OldestLhsFileTimestamp)
        ), RhsTimestampAL, NewerRhsTimestampAL),
    list.sort(NewerRhsTimestampAL, SortedNewerRhsTimestampAL),
    list.map(describe_dependency_file_and_timestamp, SortedNewerRhsTimestampAL,
        NewerDescs),
    string.append_list([Header | NewerDescs], Desc).

:- pred describe_dependency_file_and_timestamp(
    pair(dependency_status_known_file, timestamp)::in, string::out) is det.

describe_dependency_file_and_timestamp(DepStatusTuple - Timestamp, Desc) :-
    DepStatusTuple = dependency_status_known_file(DepFile, DepFileName, _),
    string.format("\t%s %s %s\n",
        [s(string(DepFile)), s(DepFileName), s(string(Timestamp))], Desc).

%---------------------------------------------------------------------------%
:- end_module make.check_up_to_date.
%---------------------------------------------------------------------------%
