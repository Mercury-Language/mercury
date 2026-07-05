%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.analysis.m.
% Main author: stayl.
%
% Build targets which relate to whole programs or libraries.
%
%---------------------------------------------------------------------------%

:- module make.analysis.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type build2 == pred(io.text_output_stream, maybe_succeeded,
    make_info, make_info, io, io).
:- inst build2 == (pred(in, out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After Pred is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_2(io.text_output_stream::in, globals::in,
    build2::in(build2), maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type build3 == pred(io.text_output_stream, maybe_succeeded,
    make_info, make_info, list(error_spec), list(error_spec), io, io).
:- inst build3 == (pred(in, out, in, out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After Pred is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_3(io.text_output_stream::in, globals::in,
    build3::in(build3), maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % The form of the argument list is dictated by the build2 type.
    %
:- pred build_analysis_files(globals::in, module_name::in,
    list(module_name)::in, maybe_succeeded::in,
    io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module make.build.
:- import_module make.find_local_modules.
:- import_module make.int_opt.
:- import_module make.options_file.
:- import_module make.order.
:- import_module make.util.
:- import_module parse_tree.file_names.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module io.file.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

maybe_with_analysis_cache_dir_2(ProgressStream, Globals, Pred, Succeeded,
        !Info, !IO) :-
    should_we_use_analysis_cache_dir(ProgressStream, Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        Pred(ProgressStream, Succeeded, !Info, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigParams = make_info_get_compiler_params(!.Info),
        OrigOptionArgs = OrigParams ^ cp_option_args,
        % Pass the name of the cache directory to child processes.
        NewOptionArgs = OrigOptionArgs ++ [CacheDirOption, CacheDir],
        NewParams = OrigParams ^ cp_option_args := NewOptionArgs,
        make_info_set_compiler_params(NewParams, !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        Pred(ProgressStream, TaskSucceeded, !Info, !IO),
        CleanupPred = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            TaskSucceeded, Succeeded, !Info, !IO),
        remove_cache_dir(ProgressStream, Globals, CacheDir, !Info, !IO),
        make_info_set_compiler_params(OrigParams, !Info)
    ;
        UseAnalysisCacheDir = analysis_cache_dir_create_failed,
        Succeeded = did_not_succeed
    ).

%---------------------%

maybe_with_analysis_cache_dir_3(ProgressStream, Globals, Pred, Succeeded,
        !Info, !Specs, !IO) :-
    should_we_use_analysis_cache_dir(ProgressStream, Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        Pred(ProgressStream, Succeeded, !Info, !Specs, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigParams = make_info_get_compiler_params(!.Info),
        OrigOptionArgs = OrigParams ^ cp_option_args,
        % Pass the name of the cache directory to child processes.
        NewOptionArgs = OrigOptionArgs ++ [CacheDirOption, CacheDir],
        NewParams = OrigParams ^ cp_option_args := NewOptionArgs,
        make_info_set_compiler_params(NewParams, !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        Pred(ProgressStream, TaskSucceeded, !Info, !Specs, !IO),
        CleanupPred = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            TaskSucceeded, Succeeded, !Info, !IO),
        remove_cache_dir(ProgressStream, Globals, CacheDir, !Info, !IO),
        make_info_set_compiler_params(OrigParams, !Info)
    ;
        UseAnalysisCacheDir = analysis_cache_dir_create_failed,
        Succeeded = did_not_succeed
    ).

%---------------------%

:- type maybe_use_analysis_cache_dir
    --->    do_not_use_analysis_cache_dir
    ;       use_analysis_cache_dir(string, string)
    ;       analysis_cache_dir_create_failed.

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files.
    %
    % XXX This oversimplifies the logic in the code.
    %
:- pred should_we_use_analysis_cache_dir(io.text_output_stream::in,
    globals::in, make_info::in, maybe_use_analysis_cache_dir::out,
    io::di, io::uo) is det.

should_we_use_analysis_cache_dir(ProgressStream, Globals, Info,
        UseAnalysisCacheDir, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, analysis_file_cache, Caching),
    globals.lookup_string_option(Globals, analysis_file_cache_dir, CacheDir0),
    CacheDirOption = "--analysis-file-cache-dir",
    ( if
        (
            IntermodAnalysis = no
        ;
            Caching = no
        ;
            % Cache directory given on command line.
            % XXX I (zs) find it strange to take the cache directory's name
            % being specified on the command line as a sign that
            % we should NOT use an analysis cache directory,
            % but that is what we do.
            CacheDir0 \= ""
        ;
            % Analysis file cache directory already set up in a parent call.
            % XXX The comment just above applies here as well.
            Params = make_info_get_compiler_params(Info),
            Params = compiler_params(_, _, OpttonArgs),
            list.member(CacheDirOption, OpttonArgs)
        )
    then
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir
    else
        create_analysis_cache_dir(ProgressStream, Globals, Succeeded,
            CacheDir, !IO),
        (
            Succeeded = succeeded,
            UseAnalysisCacheDir =
                use_analysis_cache_dir(CacheDir, CacheDirOption)
        ;
            Succeeded = did_not_succeed,
            UseAnalysisCacheDir = analysis_cache_dir_create_failed
        )
    ).

%---------------------%

:- pred create_analysis_cache_dir(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, string::out, io::di, io::uo) is det.

create_analysis_cache_dir(ProgressStream, Globals, Succeeded, CacheDir, !IO) :-
    % XXX LEGACY
    analysis_cache_dir_name(Globals, CacheDir, _CacheDirProposed),
    verbose_make_two_part_msg(Globals, "Creating", CacheDir, CreatingMsg),
    maybe_write_msg(ProgressStream, CreatingMsg, !IO),
    dir.make_directory(CacheDir, MakeRes, !IO),
    (
        MakeRes = ok,
        Succeeded = succeeded
    ;
        MakeRes = error(Error),
        io.format(ProgressStream, "Error: making directory %s: %s\n",
            [s(CacheDir), s(io.error_message(Error))], !IO),
        Succeeded = did_not_succeed
    ).

:- pred remove_cache_dir(io.text_output_stream::in, globals::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_cache_dir(ProgressStream, Globals, CacheDir, Info, Info, !IO) :-
    % The unnecessary Info arguments are required by the (current)
    % interface of teardown_checking_for_interrupt.
    verbose_make_two_part_msg(Globals, "Removing", CacheDir, RemovingMsg),
    maybe_write_msg(ProgressStream, RemovingMsg, !IO),
    io.file.remove_file_recursively(CacheDir, _, !IO).

%---------------------------------------------------------------------------%

build_analysis_files(Globals, MainModuleName, AllModules,
        Succeeded0, ProgressStream, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        Succeeded0 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed
    else
        % Ensure all .intN and .opt files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing file later.
        % (Although we can't actually build analysis files in parallel yet.)
        build_int_opt_files(ProgressStream, Globals, build_all_ints_opts,
            AllModules, Succeeded1, !Info, !IO),
        ( if
            Succeeded1 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed
        else
            build_analysis_files_1(ProgressStream, Globals,
                MainModuleName, AllModules, Succeeded, !Info, !IO)
        )
    ).

:- pred build_analysis_files_1(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_1(ProgressStream, Globals, MainModuleName, AllModules,
        Succeeded, !Info, !IO) :-
    get_target_modules(ProgressStream, Globals,
        module_target_analysis_registry, AllModules, TargetModules0,
        !Info, !IO),
    get_bottom_up_ordered_modules(
        make_info_get_maybe_module_dep_info_map(!.Info),
        TargetModules0, TargetModules1),
    % Filter out the non-local modules so we don't try to reanalyse them.
    list.filter(list.contains(AllModules), TargetModules1, TargetModules),
    make_local_module_id_options(ProgressStream, Globals, MainModuleName,
        Succeeded0, LocalModulesOpts, !Info, !IO),
    (
        Succeeded0 = succeeded,
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_analysis_files_2(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, list(string)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_2(ProgressStream, Globals, MainModuleName, TargetModules,
        LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    Registries =
        make_target_id_list(TargetModules, module_target_analysis_registry),
    foldl2_make_module_targets(KeepGoing, LocalModulesOpts, ProgressStream,
        Globals, Registries, Succeeded1, !Info, !IO),
    % Maybe we should have an option to reanalyse cliques before moving
    % upwards in the dependency graph?

    % Find which module analysis files are suboptimal or invalid.
    % If there are any invalid files then we repeat the analysis pass.
    % If there are only suboptimal files then we repeat the analysis up
    % to the number of times given by the user.
    ReanalysisPasses = make_info_get_reanalysis_passes(!.Info),
    ReanalyseSuboptimal = (if ReanalysisPasses > 1 then yes else no),
    modules_needing_reanalysis(ReanalyseSuboptimal, Globals, TargetModules,
        InvalidModules, SuboptimalModules, !IO),
    ( if list.is_not_empty(InvalidModules) then
        maybe_reanalyse_modules_msg(Globals, ReanalysingMsg),
        maybe_write_msg(ProgressStream, ReanalysingMsg, !IO),
        list.foldl(reset_analysis_registry_dependency_status,
            InvalidModules, !Info),
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else if list.is_not_empty(SuboptimalModules) then
        list.foldl(reset_analysis_registry_dependency_status,
            SuboptimalModules, !Info),
        make_info_set_reanalysis_passes(ReanalysisPasses - 1, !Info),
        maybe_reanalyse_modules_msg(Globals, ReanalysingMsg),
        maybe_write_msg(ProgressStream, ReanalysingMsg, !IO),
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else
        Succeeded = Succeeded0 `and` Succeeded1
    ).

%---------------------------------------------------------------------------%

:- pred modules_needing_reanalysis(bool::in, globals::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    io::di, io::uo) is det.

modules_needing_reanalysis(_, _, [], [], [], !IO).
modules_needing_reanalysis(ReanalyseSuboptimal, Globals, [Module | Modules],
        InvalidModules, SuboptimalModules, !IO) :-
    do_read_module_overall_status(mmc, Globals, Module, ModuleStatus, !IO),
    (
        ModuleStatus = optimal,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules, SuboptimalModules, !IO)
    ;
        ModuleStatus = suboptimal,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules, SuboptimalModules0, !IO),
        (
            ReanalyseSuboptimal = yes,
            SuboptimalModules = [Module | SuboptimalModules0]
        ;
            ReanalyseSuboptimal = no,
            SuboptimalModules = SuboptimalModules0
        )
    ;
        ModuleStatus = invalid,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules0, SuboptimalModules, !IO),
        InvalidModules = [Module | InvalidModules0]
    ).

:- pred reset_analysis_registry_dependency_status(module_name::in,
    make_info::in, make_info::out) is det.

reset_analysis_registry_dependency_status(ModuleName, !Info) :-
    TargetFile = target_file(ModuleName, module_target_analysis_registry),
    TargetId = merc_target(TargetFile),
    TargetStatusMap0 = make_info_get_target_status_map(!.Info),
    version_hash_table.set(TargetId, target_status_not_considered,
        TargetStatusMap0, TargetStatusMap),
    make_info_set_target_status_map(TargetStatusMap, !Info).

%---------------------------------------------------------------------------%

    % Find all modules in the current directory which are reachable (by import)
    % from the given module. Return a list of `--local-module-id' options
    % suitable for the command line.
    %
:- pred make_local_module_id_options(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, list(string)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_local_module_id_options(ProgressStream, Globals, ModuleName,
        Succeeded, Options, !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, ModuleName,
        Succeeded, LocalModules, !Info, !IO),
    set.fold(make_local_module_id_option, LocalModules, [], Options).

:- pred make_local_module_id_option(module_name::in, list(string)::in,
    list(string)::out) is det.

make_local_module_id_option(ModuleName, Opts0, Opts) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Opts = ["--local-module-id", ModuleNameStr | Opts0].

%---------------------------------------------------------------------------%
:- end_module make.analysis.
%---------------------------------------------------------------------------%
